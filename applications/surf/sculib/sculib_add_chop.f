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
*     CALL SCULIB_ADD_CHOP(BEAM, RA_CENTRE,
*    :     DEC_CENTRE, CHOP_CRD, CHOP_PA, CHOP_FUN, CHOP_THROW,
*    :     LST, MJD, LAT_OBS, RA_START, RA_END, DEC_START, 
*    :     DEC_END, OUT_RA_CEN, OUT_DEC_CEN,
*    :     STATUS)

*  Arguments:
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
*        Chop function (CENTER, SQUARE, TRIPOS)
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
*     This routine takes a chop throw and adds it on the 
*     supplied apparent RA/Dec centre position returning a new
*     apparent RA/Dec. Works with scan map and jiggle map
*     but does not actually know the difference itself (unless
*     using SC chop). 

*  Notes:
*     It is assumed that the chop throw is divided by two
*     prior to calling this routine for scan map mode since the
*     chop is effectively half a chop either side of the middle.
*     For jiggle modes the chop is effectively a full chop throw
*     from the centre since the 3-beam chopping is done by a combination
*     of chopping and nodding. 
*     - This routine does not yet support TRIPOS chopping (I think)
*     - AZ, LO and SC chopping are supported
*     - The negative beam is always the 'left' beam, the +ve beam is the
*       'right'.
*     - This routine does not 'yet' correct for the droopy beam problem
*       in early jiggle data that used RA/Dec chopping
*     

*  Authors :
*     TIMJ: Tim Jenness (timj@jach.hawaii.edu)
*  History :
*     $Id$
*     $Log$
*     Revision 1.2  1999/07/13 06:30:40  timj
*     Major overhaul - should actually work now!
*
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
*      REAL SLA_BEAR

*    Global variables :

*    Local Constants :

*    Local variables :
      REAL   ANG                          ! scan angle (-PI to PI)
      DOUBLE PRECISION DTEMP              ! Scratch double
      REAL   DX                           ! X scan length
      REAL   DY                           ! Y scan length
      DOUBLE PRECISION MAP_X              ! Cartesian offset of chop
      DOUBLE PRECISION MAP_Y              ! Cartesian offset of chop
      DOUBLE PRECISION MYLAT              ! Intermediate Y coord
      DOUBLE PRECISION MYLONG             ! Intermediate X coord
      
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

*     Angles are from the end of the scan to the start so that
*     so that the sense of the positive scan direction can be calculated

            DY = DEC_END - DEC_START
            DX = RA_END - RA_START

*     Angles are relative to the X-axis and go anti-clockwise
*     We are not using the standard 'position angle' definition
*     since there is no real point in doing that

            IF ( ABS(DY) .LT. 1E-10 .AND. ABS(DX) .LT. 1E-10) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP(' ','SCULIB_ADD_CHOP: Start and '//
     :              'end positions of scan were the same so can '//
     :              'not define scan angle', STATUS)

            ELSE
               
*     Calculate the angle using ATAN2 since we want to know
*     which quadrant the angle is in when we come to add it on
*     This assumes cartesian geometry.
*     Use SLA_BEAR to get the bearing along the great circle
*     -- not sure whether this is correct though, need to find 
*     out what the telescope actually does!
               ANG = ATAN2(DY, DX)

*     Calculate the arcsec offset in X and Y
*     This also assumes cartesian geometry but note that
*     SCULIB_PROCESS_BOLS calculates the current RA/Dec centre
*     by assuming cartesian geometry so we will stick with this...
               MAP_X = DBLE( CHOP_THROW * COS(ANG) )
               MAP_Y = DBLE( CHOP_THROW * SIN(ANG) )
               
*     The left beam is always behind the right so have to invert the offset
               IF (BEAM .EQ. 'L') THEN
                  MAP_X = MAP_X * -1.0D0
                  MAP_Y = MAP_Y * -1.0D0
               END IF
               
*     Add on the tangent plane offset (can not simply ADD the 
*     offset assuming cartesian geometry)
               CALL SLA_DTP2S(MAP_X, MAP_Y, RA_CENTRE, DEC_CENTRE,
     :              OUT_RA_CEN, OUT_DEC_CEN)

            END IF

         ELSE

*     Convert to the LOcal coordinates

            CALL SCULIB_APPARENT_2_MP(RA_CENTRE,
     :           DEC_CENTRE, CHOP_CRD,
     :           LST, MJD, LAT_OBS, MYLONG, MYLAT, STATUS)
            
*     Calculate the offsets in the non-rotated frame
*     (since SCULIB_CALC_APPARENT does not take angles)
*     The left (negative) beam is the beam that is aligned
*     with the position angle (and is therefore ahead of the
*     positive beam - this is the opposite way round to a
*     SC chop where the positive beam leads the negative)
*     Also note that the CHOP_PA is a position angle (from north)
*     and not a standard angle from X.
            
            MAP_X = DBLE(CHOP_THROW * SIN(CHOP_PA))
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
