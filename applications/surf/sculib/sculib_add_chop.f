      SUBROUTINE SCULIB_ADD_CHOP(BEAM, RA_REF_CENTRE, DEC_REF_CENTRE,
     :     RA_CENTRE, DEC_CENTRE, CHOP_CRD, CHOP_PA, CHOP_FUN,
     :     CHOP_THROW, LST, MJD, LAT_OBS, RA_START, RA_END, DEC_START,
     :     DEC_END, OUT_RA_CEN, OUT_DEC_CEN, STATUS)
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
*     CALL SCULIB_ADD_CHOP(BEAM, RA_REF_CENTRE, DEC_REF_CENTRE, RA_CENTRE,
*    :     DEC_CENTRE, CHOP_CRD, CHOP_PA, CHOP_FUN, CHOP_THROW,
*    :     LST, MJD, LAT_OBS, RA_START, RA_END, DEC_START,
*    :     DEC_END, OUT_RA_CEN, OUT_DEC_CEN,
*    :     STATUS)

*  Arguments:
*     BEAM        = CHARACTER * (*) (Given)
*          Beam name ('M','L' or 'R')
*     RA_REF_CENTRE = DOUBLE PRECISION (Given)
*          Apparent RA of the map centre. This is the centre that
*          scan map tangent plane offsets are calculated from.
*     DEC_REF_CENTRE = DOUBLE PRECISION (Given)
*          Apparent dec of the map centre. This is the centre that
*          scan map tangent plane offsets are calculated from.
*     RA_CENTRE              = DOUBLE PRECISION (Given)
*        the apparent RA of the `centre' of the array (radians)
*     DEC_CENTRE             = DOUBLE PRECISION (Given)
*        the apparent dec of the `centre' of the array (radians)
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

*  Description:
*     This routine takes a chop throw and adds it on the
*     supplied apparent RA/Dec centre position returning a new
*     apparent RA/Dec. Works with scan map and jiggle map
*     but does not actually know the difference itself (unless
*     using SC chop). For EKH (SC) chopping we have to convert
*     the scan ends to tangent plane offsets from the reference
*     centre before calculating the angle.

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
*     - The chop tracking 'droopy' beam problem should be dealt with
*       in an earlier subroutine such that the chop is converted from
*       LO to AZ before calling this routine
*

*  Authors:
*     TIMJ: Tim Jenness (timj@jach.hawaii.edu)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Id$
*     $Log$
*     Revision 1.6  2005/08/15 07:15:52  timj
*     use standards compliant negation
*
*     Revision 1.5  1999/08/03 19:34:41  timj
*     Add copyright message to header.
*     Convert old header style to new.
*
*     Revision 1.4  1999/07/14 20:13:26  timj
*     Pass LAT_OBS into SCULIB_CALC_APPARENT rather than having it as
*     a parameter.
*
*     Revision 1.3  1999/07/13 20:57:10  timj
*     Correctly calculate SC chop beams by converting scan ends to tangent
*     plane offsets before calculating angle.
*     Convert everything to DOUBLE precision.
*
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

*  Type Definitions:
      IMPLICIT NONE                            ! No implicit typing

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER *(*)   BEAM
      CHARACTER *(*)   CHOP_CRD
      CHARACTER *(*)   CHOP_FUN
      REAL             CHOP_PA
      REAL             CHOP_THROW
      DOUBLE PRECISION DEC_CENTRE
      REAL             DEC_END
      DOUBLE PRECISION DEC_REF_CENTRE
      REAL             DEC_START
      DOUBLE PRECISION LAT_OBS
      DOUBLE PRECISION LST
      DOUBLE PRECISION MJD
      DOUBLE PRECISION RA_CENTRE
      REAL             RA_END
      DOUBLE PRECISION RA_REF_CENTRE
      REAL             RA_START

*  Arguments Given & Returned:

*  Arguments Returned:
      DOUBLE PRECISION OUT_DEC_CEN
      DOUBLE PRECISION OUT_RA_CEN

*  Status:
      INTEGER          STATUS

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      DOUBLE PRECISION ANG                ! scan angle (-PI to PI)
      DOUBLE PRECISION DTEMP              ! Scratch double
      DOUBLE PRECISION DX                 ! X scan length
      DOUBLE PRECISION DY                 ! Y scan length
      DOUBLE PRECISION MAP_X              ! Cartesian offset of chop
      DOUBLE PRECISION MAP_Y              ! Cartesian offset of chop
      DOUBLE PRECISION MYLAT              ! Intermediate Y coord
      DOUBLE PRECISION MYLONG             ! Intermediate X coord
      INTEGER          SLA_STATUS         ! Slalib status
      DOUBLE PRECISION XEND               ! Tangent plane offset of scan end
      DOUBLE PRECISION XSTART             ! tangent plane offset of scan start
      DOUBLE PRECISION YEND               ! Tangent plane offset of scan end
      DOUBLE PRECISION YSTART             ! tangent plane offset of scan start

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

*     Need to convert the start and end apparent RA/Decs to tangent
*     plane offsets relative to the map centre that was used for
*     the centre of the mapped area

*     Scan start
            SLA_STATUS = 0
            CALL SLA_DS2TP( DBLE(RA_START), DBLE(DEC_START),
     :           RA_REF_CENTRE, DEC_REF_CENTRE, XSTART, YSTART,
     :           SLA_STATUS)

            IF (SLA_STATUS .NE. 0 .AND. STATUS .EQ. SAI__OK) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP(' ', 'SCULIB_ADD_CHOP: Error calculating'//
     :              ' tangent plane offset of scan start',
     :              STATUS)
            END IF

*     Scan end
            SLA_STATUS = 0
            CALL SLA_DS2TP( DBLE(RA_END), DBLE(DEC_END), RA_REF_CENTRE,
     :           DEC_REF_CENTRE, XEND, YEND, SLA_STATUS)

            IF (SLA_STATUS .NE. 0 .AND. STATUS .EQ. SAI__OK) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP(' ', 'SCULIB_ADD_CHOP: Error calculating'//
     :              ' tangent plane offset of scan end',
     :              STATUS)
            END IF

*     Angles are from the end of the scan to the start so that
*     so that the sense of the positive scan direction can be calculated

            DX = XEND - XSTART
            DY = YEND - YSTART

*     Angles are relative to the X-axis and go anti-clockwise
*     We are not using the standard 'position angle' definition
*     since there is no real point in doing that

            IF ( ABS(DY) .LT. 1E-10 .AND. ABS(DX) .LT. 1E-10) THEN
               IF (STATUS .EQ. SAI__OK) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP(' ','SCULIB_ADD_CHOP: Start and '//
     :                 'end positions of scan were the same so can '//
     :                 'not define scan angle', STATUS)
               END IF

            ELSE

*     Calculate the angle using ATAN2 since we want to know
*     which quadrant the angle is in when we come to add it on

               ANG = DATAN2(DY, DX)

*     Calculate the arcsec offset in X and Y (these are tangent plane
*     offsets)
               MAP_X = DBLE( CHOP_THROW ) * DCOS( ANG )
               MAP_Y = DBLE( CHOP_THROW ) * DSIN( ANG )

*     The left beam is always behind the right so have to invert the offset
               IF (BEAM .EQ. 'L') THEN
                  MAP_X = -MAP_X
                  MAP_Y = -MAP_Y
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

            MAP_X = DBLE(CHOP_THROW) * DSIN(DBLE(CHOP_PA))
            MAP_Y = DBLE(CHOP_THROW) * DCOS(DBLE(CHOP_PA))

*     Invert the offset if we are in the right beam
            IF (BEAM .EQ. 'R') THEN
               MAP_X = -MAP_X
               MAP_Y = -MAP_Y
            END IF

*     Add on the offset and convert back to apparent RA/Dec

            CALL SCULIB_CALC_APPARENT(LAT_OBS, MYLONG, MYLAT,
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
