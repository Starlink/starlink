      SUBROUTINE IRA_SHIFT( A0, B0, ANGLE, DIST, A1, B1, ENDANG,
     :                      STATUS )
*+
*  Name:
*     IRA_SHIFT

*  Purpose:
*     Find a sky position which is offset along a given position angle.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_SHIFT( A0, B0, ANGLE, DIST, A1, B1, ENDANG, STATUS )

*  Description:
*     This routine finds the sky position which is offset away from a
*     specified "reference" position by a given arc distance, along a
*     line which has a given position angle.  Note, in this context a
*     "line" is actually a great circle on the celestial sphere.  This
*     means for instance, that a line going due east or west from any
*     point, for a distance of PI/2 radians, will always end on the
*     equator. If any of the input values have the Starlink "BAD" value
*     (VAL__BADD) then both output coordinate values will also be bad.
*     The position angle of a great circle varies along its length. The
*     position angle of the requested great circle at the returned
*     position is returned in argument ENDANG.

*  Arguments:
*     A0 = DOUBLE PRECISION (Given)
*        The sky longitude of the reference position, in radians.
*     B0 = DOUBLE PRECISION (Given)
*        The sky latitude of the reference position, in radians.
*     ANGLE = DOUBLE PRECISION (Given)
*        The position angle of the line (actually a great circle) going
*        from the given reference position, to the required position.
*        That is, the angle from north to the required direction, in
*        radians. Positive angles are in the sense of rotation from
*        north to east.
*     DIST = DOUBLE PRECISION (Given)
*        The arc distance to move away from the reference position
*        in the given direction, in radians.
*     A1 = DOUBLE PRECISION (Returned)
*        The sky longitude of the required point, in radians.
*     B1 = DOUBLE PRECISION (Returned)
*        The sky latitude of the required point, in radians.
*     ENDANG = DOUBLE PRECISION (Returned)
*        The position angle of the line (actually great circle) as seen
*        from the the required point. In general, this will not be the
*        same as the value given in ANGLE, especially for large values
*        of DIST, or positions close to the poles.  ENDANG is measured
*        from north to the great circle, in radians.  Positive values
*        are in the sense of rotation from north to east.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-SEP-1990 (DSB):
*        Original version.
*     27-APR-1991 (DSB):
*        Modified for IRA version 2.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRA_ERR'          ! IRA errors

*  Arguments Given:
      DOUBLE PRECISION A0
      DOUBLE PRECISION B0
      DOUBLE PRECISION ANGLE
      DOUBLE PRECISION DIST

*  Arguments Returned:
      DOUBLE PRECISION A1
      DOUBLE PRECISION B1
      DOUBLE PRECISION ENDANG

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION COSDST    ! Cosine of DIST
      DOUBLE PRECISION COSA1     ! Cosine of A1.
      DOUBLE PRECISION COSB1     ! Cosine of B1.
      DOUBLE PRECISION Q1(3)     ! Vector PI/2 away from R4 in meridian
                                 ! of R4.
      DOUBLE PRECISION Q2(3)     ! Vector PI/2 away from R4 on equator.
      DOUBLE PRECISION Q3(3)     ! Vector PI/2 away from R4 on great
                                 ! circle.
      DOUBLE PRECISION R0(3)     ! Reference position vector.
      DOUBLE PRECISION R3(3)     ! Vector PI/2 away from R0 on great
                                 ! circle.
      DOUBLE PRECISION SINDST    ! Sine of DIST
      DOUBLE PRECISION SINA1     ! Sine of A1.
      DOUBLE PRECISION SINB1     ! Sine of B1.
      DOUBLE PRECISION SLA_DRANRM! SLALIB angle range function.
      DOUBLE PRECISION SLA_DVDV  ! SLALIB dot product function.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the input values are not bad.
      IF ( A0 .EQ. VAL__BADD .OR. B0 .EQ. VAL__BADD .OR.
     :     ANGLE .EQ. VAL__BADD .OR. DIST .EQ. VAL__BADD ) THEN
         A1 = VAL__BADD
         B1 = VAL__BADD
         GO TO 999
      END IF

*  Call IRA1_SHCAL to calculate the required vectors R0 (representing
*  the reference point) and R3 (representing the point which is 90
*  degrees away from the reference point, along the required great
*  circle). The XY plane defines zero latitude, Z is in the direction
*  of increasing latitude, X is towards zero longitude, and Y is
*  towards longitude 90 degrees.
      CALL IRA1_SHCAL( A0, B0, ANGLE, R0, R3, STATUS )

*  Call IRA1_SHAPP to use R0 and R3 to calculate the new position.
      CALL IRA1_SHAPP( DIST, R0, R3, A0, A1, B1, STATUS )

*  Ensure that the sky coordinates are in the first order range
*  (longitude between 0 and 2*pi, latitude between +pi/2 and -pi/2 ).
      CALL IRA_NORM( A1, B1, STATUS )

*  Create the vector Q1 representing the point in the meridian of the
*  required point which has latitude 90 degrees greater than the
*  required point.
      SINA1 = SIN( A1 )
      COSA1 = COS( A1 )
      SINB1 = SIN( B1 )
      COSB1 = COS( B1 )

      Q1(1) = -SINB1*COSA1
      Q1(2) = -SINB1*SINA1
      Q1(3) =  COSB1

*  Create the vector Q2 representing the point on the equator (i.e. a
*  latitude of zero), which has a longitude 90 degrees to the west of
*  the required point.
      Q2(1) = -SINA1
      Q2(2) =  COSA1
      Q2(3) =  0.0D0

*  Create the vector Q3 representing the point which is 90 degrees away
*  from the required point, along the required great circle.
      COSDST = COS( DIST )
      SINDST = SIN( DIST )

      Q3(1) = -SINDST*R0(1) + COSDST*R3(1)
      Q3(2) = -SINDST*R0(2) + COSDST*R3(2)
      Q3(3) = -SINDST*R0(3) + COSDST*R3(3)

*  Calculate the position angle of the great circle at the required
*  point.
      ENDANG = ATAN2( SLA_DVDV( Q3, Q2 ), SLA_DVDV( Q3, Q1 ) )

*  Ensure that the end position angle is in the range 0 to 2*pi.
      ENDANG = SLA_DRANRM( ENDANG )

*  Give a contextual message.
 999  IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRA_SHIFT_ERR1',
     :          'IRA_SHIFT: Unable to calculate a shifted sky position',
     :                 STATUS )
      END IF

      END
