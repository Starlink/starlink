*+  SCULIB_CALC_APPARENT - calculate apparent RA, Dec of plate centre and angle
*                          of input coord system N relative to apparent N.
      SUBROUTINE SCULIB_CALC_APPARENT (LONG, LAT, LONG2, LAT2, MAP_X,
     :  MAP_Y, COORD_TYPE, MJD, MJD1, MJD2, RA_APP, DEC_APP, ROTATION, 
     :  STATUS)
*    Description :
*     This routine takes the input coordinates and coordinate system of the
*     map centre and converts them to the apparent coords at the time of the
*     observation. In addition, the angle between the north direction in
*     the input coordinate frame and that in the apparent frame is calculated 
*     (measured anti-clockwise from input north, in radians). See SCU/3.0/JFL/
*     0393.
*
*     RB coords - 
*        Use SLA_FK54Z to convert to RJ.
*        Use SLA_MAP to convert to apparent, giving ra_app, dec_app.
*        Use same method to calculate apparent position of RB N pole, giving
*        ra_N_app, dec_N_app.
*        Then calculate rotation from:-
*
*        dRA =  ra_app - ra_N_app
*
*        sin (rotation) =   sin(dRA) * cos (dec_N_app)
*                           --------------------------
*                                  cos (lat)
* 
*        cos (rotation) = sin (dec_N_app) - sin (dec_app) * sin (lat)
*                         -------------------------------------------
*                                  cos (dec_app) * cos (lat)
*   
*        Since cos (lat) is in the denominator for both sin and cos terms,
*        is always +ve except for +- pi/2 where it goes to zero and blows
*        up the equations, leave it out in the calculations. The ratio
*        of sin and cos will be unaffected.
*
*        If dec_app = pi/2 (i.e. at N pole of apparent system) then
*           rotation = pi - (ra_N_app - ra_app)
*
*     RJ coords -
*        Use SLA_MAP to convert to apparent.
*        Use same method to calculate apparent position of RB N pole.
*        Derive rotation angle in the same way as for RB.
*
*     GA coords -
*        Use SLA_GALEQ to convert to RJ.
*        Use SLA_MAP to convert to apparent, giving ra_app, dec_app.
*        Use same method to calculate apparent position of GA N pole, giving
*        ra_N_app, dec_N_app.
*        Derive rotation angle in the same way as for RB.
*
*     EQ coords -
*        Use SLA_ECLEQ to convert to RJ.
*        Use SLA_MAP to convert to apparent, giving ra_app, dec_app.
*        Use same method to calculate apparent position of EQ N pole, giving
*        ra_N_app, dec_N_app.
*        Derive rotation angle in the same way as for RB.
*
*     HA coords -
*        Apparent RA = LST - LONG
*        Apparent Dec = LAT
*        Rotation = 0.0D0
*
*     RD coords -
*        Apparent RA, Dec set to input values. Rotation = 0.0.
*
*     PLANET coords -
*        If MJD1 = MJD2 then apparent RA, Dec set to input LONG, LAT.
*           Rotation = 0.0.
*        Otherwise apparent RA, Dec interpolated (or extrapolated) between
*           LONG, LAT, MJD1 and LONG2, LAT2, MJD2 according to MJD. 
*           Rotation = 0.
*
*    Invocation :
*     CALL SCULIB_CALC_APPARENT (LONG, LAT, LONG2, LAT2, MAP_X, MAP_Y,
*    :  COORD_TYPE, MJD, MJD1, MJD2, RA_APP, DEC_APP, ROTATION, STATUS)
*    Parameters :
*     LONG                   = DOUBLE PRECISION (Given)
*           longitude of centre in input coord system (radians)
*     LAT                    = DOUBLE PRECISION (Given)
*           latitude of centre in input coord system (radians)
*     LONG2                  = DOUBLE PRECISION (Given)
*           longitude of second centre in PLANET coord system (radians)
*     LAT2                   = DOUBLE PRECISION (Given)
*           latitude of second centre in PLANET coord system (radians)
*     MAP_X                  = DOUBLE PRECISION (Given)
*           x tangent plane offset of point from centre (radians)
*     MAP_Y                  = DOUBLE PRECISION (Given)
*           y tangent plane offset of point from centre (radians)
*     COORD_TYPE             = CHARACTER*(*) (Given)
*           Coord system of input centre, RD, RB, RJ, GA, EQ, PLANET
*     MJD                    = DOUBLE PRECISION (Given)
*           Modified Julian date of observation
*     MJD1                   = DOUBLE PRECISION (Given)
*           Modified Julian date of first centre in PLANET coord system
*     MJD2                   = DOUBLE PRECISION (Given)
*           Modified Julian date of second centre in PLANET coord system
*     RA_APP                 = DOUBLE PRECISION (Returned)
*           Apparent RA of point at date (radians)
*     DEC_APP                = DOUBLE PRECISION (Returned)
*           Apparent Dec
*     ROTATION               = DOUBLE PRECISION (Returned)
*           Angle between apparent north and north of input coord system
*           (radians, measured clockwise from input north)
*     STATUS                 = INTEGER (Given and returned)
*           Global status
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     J.Lightfoot (REVAD::JFL)
*    History :
*     $Id$
*     22-DEC-1992: Original
*      4-MAR-1993: Added GA, EQ, HA coords
*     14-AUG-1993: Moved to SCULIB library
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      DOUBLE PRECISION LONG, LAT, LONG2, LAT2, MAP_X, MAP_Y
      CHARACTER*(*) COORD_TYPE
      DOUBLE PRECISION MJD, MJD1, MJD2
*    Import-Export :
*    Export :
      DOUBLE PRECISION RA_APP, DEC_APP, ROTATION
*    Status :
      INTEGER STATUS
*    External references :
      DOUBLE PRECISION SCULIB_LST             ! function returning LST
*    Global variables :
*    Local Constants :
      DOUBLE PRECISION DPI
      PARAMETER       (DPI = 3.14159265359D0)
      DOUBLE PRECISION DPI2
      PARAMETER       (DPI2 = DPI / 2.0D0)
*    Local variables :
      DOUBLE PRECISION RA_2000, DEC_2000      ! RA, Dec 2000 coords of point
      DOUBLE PRECISION RA_N_2000, DEC_N_2000  ! RA, Dec 2000 coords of N pole
                                              !   of input coord system
      DOUBLE PRECISION RA_N_APP, DEC_N_APP    ! apparent RA, Dec of N pole of
                                              !   input coord system
      DOUBLE PRECISION DRA                    ! 
      DOUBLE PRECISION SIN_ROT, COS_ROT       ! sin and cos of ROTATION, 
*                                             !   multiplied by cos (lat)
*    Internal References :
*    Local data :
*-

      IF (STATUS .NE. SAI__OK) RETURN

      CALL CHR_UCASE (COORD_TYPE)

*  handle each coord type in turn

      IF ((COORD_TYPE .EQ. 'RB') .OR. 
     :    (COORD_TYPE .EQ. 'RJ') .OR.
     :    (COORD_TYPE .EQ. 'GA') .OR.
     :    (COORD_TYPE .EQ. 'EQ')) THEN

         IF (COORD_TYPE .EQ. 'RB') THEN

*  convert map centre and N pole 

            CALL SLA_FK45Z (LONG, LAT, 1950.0D0, RA_2000, DEC_2000)
            CALL SLA_MAP (RA_2000, DEC_2000, 0.0D0, 0.0D0, 0.0D0, 0.0D0,
     :         2000.0D0, MJD, RA_APP, DEC_APP)

            CALL SLA_FK45Z (0.0D0, DPI2, 1950.0D0, RA_N_2000, 
     :         DEC_N_2000)
            CALL SLA_MAP (RA_N_2000, DEC_N_2000, 0.0D0, 0.0D0, 0.0D0, 
     :         0.0D0, 2000.0D0, MJD, RA_N_APP, DEC_N_APP)

         ELSE IF (COORD_TYPE .EQ. 'RJ') THEN
            CALL SLA_MAP (LONG, LAT, 0.0D0, 0.0D0, 0.0D0, 0.0D0,
     :         2000.0D0, MJD, RA_APP, DEC_APP)
            CALL SLA_MAP (0.0D0, DPI2, 0.0D0, 0.0D0, 0.0D0, 0.0D0,
     :         2000.0D0, MJD, RA_N_APP, DEC_N_APP)

         ELSE IF (COORD_TYPE .EQ. 'GA') THEN
            CALL SLA_GALEQ (LONG, LAT, RA_2000, DEC_2000)
            CALL SLA_MAP (RA_2000, DEC_2000, 0.0D0, 0.0D0, 0.0D0, 0.0D0,
     :         2000.0D0, MJD, RA_APP, DEC_APP)

            CALL SLA_GALEQ (0.0D0, DPI2, RA_N_2000, DEC_N_2000)
            CALL SLA_MAP (RA_N_2000, DEC_N_2000, 0.0D0, 0.0D0, 0.0D0, 
     :         0.0D0, 2000.0D0, MJD, RA_N_APP, DEC_N_APP)

         ELSE IF (COORD_TYPE .EQ. 'EQ') THEN
            CALL SLA_ECLEQ (LONG, LAT, RA_2000, DEC_2000)
            CALL SLA_MAP (RA_2000, DEC_2000, 0.0D0, 0.0D0, 0.0D0, 0.0D0,
     :         2000.0D0, MJD, RA_APP, DEC_APP)

            CALL SLA_ECLEQ (0.0D0, DPI2, RA_N_2000, DEC_N_2000)
            CALL SLA_MAP (RA_N_2000, DEC_N_2000, 0.0D0, 0.0D0, 0.0D0, 
     :         0.0D0, 2000.0D0, MJD, RA_N_APP, DEC_N_APP)
         END IF

*  calculate rotation angle

         DRA = RA_APP - RA_N_APP

         IF (COS (DEC_APP) .EQ. 0.0D0) THEN

*  map centre is at a pole of apparent RA,dec system

            ROTATION = DPI - DRA
         ELSE

*  general case

            SIN_ROT = COS (DEC_N_APP) * SIN (DRA)
            COS_ROT = (SIN (DEC_N_APP) - SIN (DEC_APP) * SIN (LAT)) /
     :        COS (DEC_APP)
            ROTATION = ATAN2 (SIN_ROT, COS_ROT)
         END IF

      ELSE IF (COORD_TYPE .EQ. 'HA') THEN
         RA_APP = SCULIB_LST() - LONG
         DEC_APP = LAT
         ROTATION = 0.0D0

      ELSE IF (COORD_TYPE .EQ. 'RD') THEN
         RA_APP = LONG
         DEC_APP = LAT
         ROTATION = 0.0D0

      ELSE IF (COORD_TYPE .EQ. 'PLANET') THEN
         IF (MJD2 .EQ. MJD1) THEN
            RA_APP = LONG
            DEC_APP = LAT
            ROTATION = 0.0D0
         ELSE
            RA_APP = LONG + (LONG2 - LONG) * (MJD - MJD1)/ (MJD2 - MJD1)
            DEC_APP = LAT + (LAT2 - LAT) * (MJD - MJD1) / (MJD2 - MJD1)
            ROTATION = 0.0D0
         END IF

      ELSE
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'SCULIB_CALC_APPARENT: can only '//
     :        'handle PLANET, RJ, RB, RD, GA and EQ coordinates', 
     :        STATUS)
         END IF
      END IF

      END
