      SUBROUTINE SCULIB_ADD_CHOP(BEAM, RA_CENTRE,
     :     DEC_CENTRE, CHOP_CRD, CHOP_PA, CHOP_FUN, CHOP_THROW,
     :     LST, MJD, LAT_OBS, RA_START, RA_END, DEC_START, 
     :     DEC_END, OUT_RA_CEN, OUT_DEC_CEN,
     :     STATUS)
*+
*  Name:
*     SCULIB_ADD_CHOP

*  Purpose:
*     Add chop throw to apparent RA/Dec

*  Language:
*     Starlink Fortran 77
 
*  Type of Module:
*     SCULIB subroutine
 
*  Invocation:


*  Arguments:
*     SAMPLE_MODE = CHARACTER * (*) (Given)
*          Sample mode (RASTER or JIGGLE) of observation
*     BEAM        = CHARACTER * (*) (Given)
*          Beam name ('M','L' or 'R')
*     RA_CENTRE              = DOUBLE PRECISION (Given)
*        the apparent RA of the `centre' (radians)
*     DEC_CENTRE             = DOUBLE PRECISION (Given)
*        the apparent dec of the `centre' (radians)
*     CHOP_CRD               = CHARACTER *(*) (Given)
*        Coordinate system of CHOP throw (SC,AZ,RB,RJ,GA)
*     CHOP_PA                = REAL (Given)
*        Position angle of chop (in radians)
*     CHOP_FUN               = CHARACTER * (*) (Given)
*        Chop function (CENTER, SQUARE)
*     CHOP_THROW             = REAL (Given)
*        Chop throw in radians
*     LST                    = DOUBLE PRECISION (Given)
*        the local sidereal time (radians)
*     MJD                    = DOUBLE PRECISION (Given)
*        MJD of observation
*     LAT_OBS                = DOUBLE PRECISION (Given)
*        the latitude of the observatory (radians)
*     RA_START               = REAL (Given)
*        RA of start of scan (if SAMPLE_MODE=RASTER)
*     RA_END                 = REAL (Given)
*        RA of end of scan (if SAMPLE_MODE=RASTER)
*     DEC_START              = REAL (Given)
*        DEC of start of scan (if SAMPLE_MODE=RASTER)
*     DEC_END                = REAL (Given)
*        DEC of end of scan (if SAMPLE_MODE=RASTER)
*     OUT_RA_CEN             = DOUBLE (Returned)
*        New position with CHOP
*     OUT_DEC_CEN            = DOUBLE (Returned)
*        New DEC with CHOP
*     STATUS                 = INTEGER (Given and returned)
*        The global status

*  Description :

*  Authors :
*     TIMJ: Tim Jenness (timj@jach.hawaii.edu)
*  History :
*     $Id$
*     $Log$
*     Revision 1.1  1998/04/27 20:58:11  timj
*     Initial revision
*
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-

*    Type Definitions :
      IMPLICIT NONE                            ! No implicit typing

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'

*    Import :
      CHARACTER *(*)   BEAM
      CHARACTER *(*)   CHOP_CRD
      CHARACTER *(*)   CHOP_FUN
      REAL             CHOP_PA
      REAL             CHOP_THROW
      DOUBLE PRECISION DEC_CENTRE
      REAL             DEC_END
      REAL             DEC_START
      DOUBLE PRECISION LAT_OBS
      DOUBLE PRECISION LST
      DOUBLE PRECISION MJD
      DOUBLE PRECISION RA_CENTRE
      REAL             RA_END
      REAL             RA_START

*    Import-Export :
*    Export :
      DOUBLE PRECISION OUT_DEC_CEN
      DOUBLE PRECISION OUT_RA_CEN

*    Status :
      INTEGER          STATUS
*    External references :

*    Global variables :

*    Local Constants :
      DOUBLE PRECISION ARCSEC2RAD         ! arcsec 2 radians conversion
      PARAMETER (ARCSEC2RAD = 4.84813681110D-6)
      DOUBLE PRECISION PI                 !
      PARAMETER (PI = 3.14159265359)

*    Local variables :
      REAL   COS_ANG                      ! Cosine of scan angle
      DOUBLE PRECISION DTEMP              ! Scratch double
      DOUBLE PRECISION MAP_X              ! Cartesian offset of chop
      DOUBLE PRECISION MAP_Y              ! Cartesian offset of chop
      DOUBLE PRECISION MYLAT              ! Intermediate Y coord
      DOUBLE PRECISION MYLONG             ! Intermediate X coord
      REAL   TAN_ANG                      ! Tangent of scan angle
      REAL   SIN_ANG                      ! Sine of scan angle
      
*.

      IF (STATUS .NE. SAI__OK) RETURN

*     First read the beam information

      IF (BEAM .EQ. 'M') THEN
         
*     'M' is already calculated since the chop are centred on M

         OUT_RA_CEN = RA_CENTRE
         OUT_DEC_CEN = DEC_CENTRE

      ELSE IF (BEAM .EQ. 'L' .OR. BEAM .EQ. 'R') THEN

*     If we are using SC sample coords then we can just use the 
*     start and ends of the scan to calculate the chop position

         IF (CHOP_CRD .EQ. 'SC') THEN

            TAN_ANG = (RA_END - RA_START) / (DEC_END - DEC_START)

            COS_ANG = 1 / SQRT(1 + TAN_ANG**2)
            SIN_ANG = TAN_ANG / SQRT(1 + TAN_ANG**2)

            IF (BEAM .EQ. 'L') THEN
               
               OUT_RA_CEN = RA_CENTRE - (CHOP_THROW * COS_ANG)
               OUT_DEC_CEN = DEC_CENTRE - (CHOP_THROW * SIN_ANG)

            ELSE     

               OUT_RA_CEN = RA_CENTRE + (CHOP_THROW * COS_ANG)
               OUT_DEC_CEN = DEC_CENTRE + (CHOP_THROW * SIN_ANG)

            END IF

         ELSE

*     Convert to the LOcal coordinates
*     Should pass in LAT_OBS - otherwise if JCMT moves we will be
*     in trouble :-)

            CALL SCULIB_APPARENT_2_MP(RA_CENTRE,
     :           DEC_CENTRE, CHOP_CRD,
     :           LST, MJD, MYLONG, MYLAT, STATUS)
            
*     Calculate the offsets in the non-rotated frame
*     (since SCULIB_CALC_APPARENT does not take angles
            

            MAP_X = -1.0D0 * DBLE(CHOP_THROW * SIN(CHOP_PA))
            MAP_Y = DBLE(CHOP_THROW * COS(CHOP_PA))

*     Invert the offset if we are in the right beam
            IF (BEAM .EQ. 'R') THEN
               MAP_X = MAP_X * -1.0D0
               MAP_Y = MAP_Y * -1.0D0
            END IF

*     Add on the offset and convert back to apparent RA/Dec

            CALL SCULIB_CALC_APPARENT(MYLONG, MYLAT,
     :           0.0D0, 0.0D0, MAP_X, MAP_Y,
     :           CHOP_CRD, LST, MJD, 0.0D0, 0.0D0,
     :           OUT_RA_CEN, OUT_DEC_CEN,
     :           DTEMP, STATUS)

         END IF


      ELSE

         STATUS = SAI__ERROR
         CALL MSG_SETC('BM', BEAM)
         CALL ERR_REP(' ','SCULIB_ADD_CHOP: ^BM is an invalid ' //
     :        'beam specifier (should be M,L or R)', STATUS)

      END IF


      END
