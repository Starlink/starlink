      SUBROUTINE SCULIB_CALC_APPARENT (LAT_OBS, LONG, LAT, LONG2, LAT2,
     :     MAP_X, MAP_Y, COORD_TYPE, LST, MJD, MJD1, MJD2, RA_APP,
     :     DEC_APP, ROTATION, STATUS)
*+
*  Name:
*     SCULIB_CALC_APPARENT

*  Purpose:
*     calculate apparent RA, Dec of plate centre and angle
*     of input coord system N relative to apparent N.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_CALC_APPARENT (LAT_OBS, LONG, LAT, LONG2, LAT2, MAP_X, MAP_Y,
*    :  COORD_TYPE, LST, MJD, MJD1, MJD2, RA_APP, DEC_APP, ROTATION, STATUS)


*  Description:
*     This routine takes the input coordinates and coordinate system of the
*     map centre and converts them to the apparent coords at the time of the
*     observation. In addition, the angle between the north direction in
*     the input coordinate frame and that in the apparent frame is calculated
*     (measured anti-clockwise from input north, in radians). See SCU/3.0/JFL/
*     0393.
*
*
*     - AZ coords:
*        Calculate the apparent RA and DEC at the LST when the routine is
*        called by:-
*          sin(dec_app) = sin(lat_obs) * sin(el) + cos(lat_obs) * cos(el) * cos(az)
*
*          sin(H_A) = - sin(az) * cos(el)
*                      .-----------------
*                           cos(dec_app)
*
*          cos(H_A) = sin(el) - sin(dec_app) * sin(lat_obs)
*                    .-------------------------------------
*                        cos(dec_app) * cos(lat_obs)
*
*         Cos(dec_app) is present in the denominator of both the sin(H_A)
*         and cos(H_A) terms and it could cause both to blow up - so
*         it's left out as only the ratio is important
*
*          RA_app = LST - H_A
*
*          sin(rotation) =   sin(az) * cos(lat_obs)
*                           .----------------------
*                               cos(dec_app)
*
*          cos(rotation) = sin(lat_obs) - sin(dec_app) * sin(el)
*                         .-------------------------------------
*                                 cos(dec_app) * cos(el)
*
*        Again, cos(dec_app) appears in the denominator of both sin and cos
*        expressions and is left out of the calculation because on ly the ratio
*        is important.

*     - RB coords:
*        Use SLA_FK54Z to convert to RJ.
*        Use SLA_MAP to convert to apparent, giving ra_app, dec_app.
*        Use same method to calculate apparent position of RB N pole, giving
*        ra_N_app, dec_N_app.
*        Then calculate rotation from:-
*
*        dRA =  ra_app - ra_N_app
*
*        sin (rotation) =   sin(dRA) * cos (dec_N_app)
*                          .--------------------------
*                                  cos (lat)
*
*        cos (rotation) = sin (dec_N_app) - sin (dec_app) * sin (lat)
*                        .-------------------------------------------
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
*     - RJ coords:
*        Use SLA_MAP to convert to apparent.
*        Use same method to calculate apparent position of RB N pole.
*        Derive rotation angle in the same way as for RB.
*
*     - GA coords:
*        Use SLA_GALEQ to convert to RJ.
*        Use SLA_MAP to convert to apparent, giving ra_app, dec_app.
*        Use same method to calculate apparent position of GA N pole, giving
*        ra_N_app, dec_N_app.
*        Derive rotation angle in the same way as for RB.
*
*     - EQ coords:
*        Use SLA_ECLEQ to convert to RJ.
*        Use SLA_MAP to convert to apparent, giving ra_app, dec_app.
*        Use same method to calculate apparent position of EQ N pole, giving
*        ra_N_app, dec_N_app.
*        Derive rotation angle in the same way as for RB.
*
*     - HA coords:
*        Apparent RA = LST - LONG
*        Apparent Dec = LAT
*        Rotation = 0.0D0
*
*     - RD coords:
*        Apparent RA, Dec set to input values. Rotation = 0.0.
*
*     - PLANET coords:
*        If MJD1 = MJD2 then apparent RA, Dec set to input LONG, LAT.
*           Rotation = 0.0.
*        Otherwise apparent RA, Dec interpolated (or extrapolated) between
*           LONG, LAT, MJD1 and LONG2, LAT2, MJD2 according to MJD.
*           Rotation = 0.
*

*  Arguments:
*     LAT_OBS                = DOUBLE PRECISION (Given)
*           latitude of observatory (radians)
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
*           The offset must be in the same coordinate as COORD_TYPE
*     MAP_Y                  = DOUBLE PRECISION (Given)
*           y tangent plane offset of point from centre (radians)
*           The offset must be in the same coordinate as COORD_TYPE
*     COORD_TYPE             = CHARACTER*(*) (Given)
*           Coord system of input centre, RD, RB, RJ, GA, EQ, PLANET
*     LST                    = DOUBLE PRECISION (Given)
*           LST for requested coordinates (for AZ and HA)
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

*  Notes:
*     Does not handle LOCAL_COORDS for MAP_X and MAP_Y
*     (see SCULIB_APARRENT_2_MP for information on how to do this)

*  Authors:
*     J.Lightfoot (jfl@roe.ac.uk)
*     T.Jenness (timj@jach.hawaii.edu)



*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Id$
*     22-DEC-1992: Original
*      4-MAR-1993: Added GA, EQ, HA coords
*     14-AUG-1993: Moved to SCULIB library
*     $Log$
*     Revision 1.12  1999/08/19 03:37:01  timj
*     Header tweaks to ease production of SSN72 documentation.
*
*     Revision 1.11  1999/08/06 02:24:39  timj
*     Tweak headers for use with PROLAT.
*
*     Revision 1.10  1999/08/03 19:34:46  timj
*     Add copyright message to header.
*     Convert old header style to new.
*
*     Revision 1.9  1999/07/14 20:13:26  timj
*     Pass LAT_OBS into SCULIB_CALC_APPARENT rather than having it as
*     a parameter.
*
*     Revision 1.8  1999/07/13 06:27:34  timj
*     Remove spurious check for Az MAP_X offset.
*
*     Revision 1.7  1997/11/19 03:33:19  timj
*     An AZ offset requires a sign change.
*
*     Revision 1.6  1997/11/04 20:40:43  timj
*     Add MAP_X and MAP_Y offsets.
*
*     Revision 1.5  1997/10/30 20:49:29  timj
*     Modernise header.
*
*    endhistory

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      DOUBLE PRECISION LONG
      DOUBLE PRECISION LAT
      DOUBLE PRECISION LONG2
      DOUBLE PRECISION LAT2
      DOUBLE PRECISION LAT_OBS
      DOUBLE PRECISION MAP_X
      DOUBLE PRECISION MAP_Y
      CHARACTER*(*) COORD_TYPE
      DOUBLE PRECISION MJD, MJD1, MJD2

*  Arguments Given and Returned:

*  Arguments Returned:
      DOUBLE PRECISION RA_APP
      DOUBLE PRECISION DEC_APP
      DOUBLE PRECISION ROTATION

*  Status :
      INTEGER STATUS

*  External references :

*  Global variables :

*  Local Constants:
      DOUBLE PRECISION DPI
      PARAMETER       (DPI = 3.14159265359D0)
      DOUBLE PRECISION DPI2
      PARAMETER       (DPI2 = DPI / 2.0D0)

*    Local variables :
      DOUBLE PRECISION COS_HA                 ! cos of hour angle
      DOUBLE PRECISION HA                     ! Hour angle
      DOUBLE PRECISION LST                    ! LST
      DOUBLE PRECISION RA_2000, DEC_2000      ! RA, Dec 2000 coords of point
      DOUBLE PRECISION RA_N_2000, DEC_N_2000  ! RA, Dec 2000 coords of N pole
                                              !   of input coord system
      DOUBLE PRECISION RA_N_APP, DEC_N_APP    ! apparent RA, Dec of N pole of
                                              !   input coord system
      DOUBLE PRECISION DRA                    !
      DOUBLE PRECISION MYLAT                  ! Internal LAT variable
      DOUBLE PRECISION MYLONG                 ! Internal LONG variable
      DOUBLE PRECISION SIN_DEC                ! Sine of apparent dec
      DOUBLE PRECISION SIN_HA                 ! Sine of hour angle
      DOUBLE PRECISION SIN_ROT, COS_ROT       ! sin and cos of ROTATION,
                                              !   multiplied by cos (lat)
      DOUBLE PRECISION SHIFT_X                ! Offset in X
      DOUBLE PRECISION SHIFT_Y                ! Offset in Y

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

      CALL CHR_UCASE (COORD_TYPE)

*     Setup scratch variables.
      MYLONG = LONG
      MYLAT  = LAT

*     Shift map centre by MAP_X and MAP_Y (if non-zero)
      IF (MAP_X .NE. 0.0D0 .OR. MAP_Y .NE. 0.0D0) THEN

         SHIFT_X = MAP_X
         SHIFT_Y = MAP_Y

         CALL SLA_DTP2S(SHIFT_X, SHIFT_Y, LONG, LAT, MYLONG, MYLAT)
      END IF

*  handle each coord type in turn

      IF ((COORD_TYPE .EQ. 'RB') .OR.
     :    (COORD_TYPE .EQ. 'RJ') .OR.
     :    (COORD_TYPE .EQ. 'GA') .OR.
     :    (COORD_TYPE .EQ. 'EQ')) THEN

         IF (COORD_TYPE .EQ. 'RB') THEN

*  convert map centre and N pole
*     Convert to J2000
            CALL SLA_FK45Z (MYLONG, MYLAT, 1950.0D0, RA_2000, DEC_2000)

*     Convert to apparent RA,DEC
            CALL SLA_MAP (RA_2000, DEC_2000, 0.0D0, 0.0D0, 0.0D0, 0.0D0,
     :         2000.0D0, MJD, RA_APP, DEC_APP)

            CALL SLA_FK45Z (0.0D0, DPI2, 1950.0D0, RA_N_2000,
     :         DEC_N_2000)
            CALL SLA_MAP (RA_N_2000, DEC_N_2000, 0.0D0, 0.0D0, 0.0D0,
     :         0.0D0, 2000.0D0, MJD, RA_N_APP, DEC_N_APP)

         ELSE IF (COORD_TYPE .EQ. 'RJ') THEN

*     Convert J2000 to apparent
            CALL SLA_MAP (MYLONG, MYLAT, 0.0D0, 0.0D0, 0.0D0, 0.0D0,
     :         2000.0D0, MJD, RA_APP, DEC_APP)
            CALL SLA_MAP (0.0D0, DPI2, 0.0D0, 0.0D0, 0.0D0, 0.0D0,
     :         2000.0D0, MJD, RA_N_APP, DEC_N_APP)

         ELSE IF (COORD_TYPE .EQ. 'GA') THEN
            CALL SLA_GALEQ (MYLONG, MYLAT, RA_2000, DEC_2000)
            CALL SLA_MAP (RA_2000, DEC_2000, 0.0D0, 0.0D0, 0.0D0, 0.0D0,
     :         2000.0D0, MJD, RA_APP, DEC_APP)

            CALL SLA_GALEQ (0.0D0, DPI2, RA_N_2000, DEC_N_2000)
            CALL SLA_MAP (RA_N_2000, DEC_N_2000, 0.0D0, 0.0D0, 0.0D0,
     :         0.0D0, 2000.0D0, MJD, RA_N_APP, DEC_N_APP)

         ELSE IF (COORD_TYPE .EQ. 'EQ') THEN
            CALL SLA_ECLEQ (MYLONG, MYLAT, MJD, RA_2000, DEC_2000)
            CALL SLA_MAP (RA_2000, DEC_2000, 0.0D0, 0.0D0, 0.0D0, 0.0D0,
     :         2000.0D0, MJD, RA_APP, DEC_APP)

            CALL SLA_ECLEQ (0.0D0, DPI2, MJD, RA_N_2000, DEC_N_2000)
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
            COS_ROT = (SIN (DEC_N_APP) - SIN (DEC_APP) * SIN (MYLAT)) /
     :        COS (DEC_APP)
            ROTATION = ATAN2 (SIN_ROT, COS_ROT)
         END IF

      ELSE IF (COORD_TYPE .EQ. 'AZ') THEN
         SIN_DEC = SIN(LAT_OBS) * SIN(MYLAT) +
     :        COS(LAT_OBS) * COS(MYLAT) * COS(MYLONG)
         DEC_APP = ASIN(SIN_DEC)

         SIN_HA = -SIN(MYLONG) * COS(MYLAT)
         COS_HA = (SIN(MYLAT) - SIN(DEC_APP) * SIN(LAT_OBS)) /
     :        COS(LAT_OBS)

         HA = ATAN2( SIN_HA, COS_HA)
         RA_APP = LST - HA

         SIN_ROT = SIN(MYLONG) * COS(LAT_OBS)
         COS_ROT = (SIN(LAT_OBS) - SIN(DEC_APP) * SIN(MYLAT)) /
     :        COS(MYLAT)

         ROTATION = ATAN2(SIN_ROT, COS_ROT)

      ELSE IF (COORD_TYPE .EQ. 'HA') THEN
         RA_APP = LST - MYLONG
         DEC_APP = MYLAT
         ROTATION = 0.0D0

      ELSE IF (COORD_TYPE .EQ. 'RD') THEN
         RA_APP = MYLONG
         DEC_APP = MYLAT
         ROTATION = 0.0D0

      ELSE IF (COORD_TYPE .EQ. 'PLANET') THEN
         IF (MJD2 .EQ. MJD1) THEN
            RA_APP = MYLONG
            DEC_APP = MYLAT
            ROTATION = 0.0D0
         ELSE
            RA_APP = MYLONG + (LONG2 - MYLONG) * (MJD - MJD1)/
     :           (MJD2 - MJD1)
            DEC_APP = MYLAT + (LAT2 - MYLAT) * (MJD - MJD1) /
     :           (MJD2 - MJD1)
            ROTATION = 0.0D0
         END IF

      ELSE
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'SCULIB_CALC_APPARENT: can only '//
     :        'handle PLANET, RJ, RB, RD, GA, AZ and EQ coordinates',
     :        STATUS)
         END IF
      END IF

      END
