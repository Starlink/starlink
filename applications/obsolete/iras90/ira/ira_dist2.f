      SUBROUTINE IRA_DIST2( A0, B0, ANGLE, A1, B1, PARDST, PRPDST,
     :                      STATUS )
*+
*  Name:
*     IRA_DIST2

*  Purpose:
*     Resolve a sky position into distances parallel and perpendicular
*     to a given great circle.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_DIST2( A0, B0, ANGLE, A1, B1, PARDST, PRPDST,
*                     STATUS )

*  Description:
*     The great circle used is the great circle which passes through
*     sky position (A0,B0) (the "reference point") at the position
*     angle given by ANGLE.  The two returned distances PARDST and
*     PRPDST are components of the displacement from (A0,B0) to
*     (A1,B1), both in radians. PARDST ("parallel distance") is the
*     distance which must be moved from (A0,B0) along the great circle
*     (in the direction specified by the position angle, ANGLE) to
*     reach the point of closest approach to (A1,B1). PRPDST
*     ("perpendicular distance") is then the distance from this point
*     of closest approach, to (A1,B1). This second displacement will be
*     an arc of a great circle which crosses the first great circle at
*     right angles at the point of closest approach. Note, performing
*     the shifts in the opposite order (perpendicular then parallel) is
*     NOT the same. The shifts should always be thought of as being
*     FIRST along the great circle specified by the given angle, AND
*     THEN perpendicular to the great circle. This is because the curve
*     with constant PRPDST (for varying PARDST) is not a great circle
*     unless PRPDST is zero.
*
*     PRPDST is positive if rotation from the given position angle to
*     the outlying point (as seen from the reference point) is in the
*     same sense as rotation from north to east.

*  Arguments:
*     A0 = DOUBLE PRECISION (Given)
*        The sky longitude of the reference point, in radians.
*     B0 = DOUBLE PRECISION (Given)
*        The sky latitude of the reference point, in radians.
*     ANGLE = DOUBLE PRECISION (Given)
*        The position angle of the great circle as seen from the
*        reference point. That is, the angle from north to the required
*        direction, in radians. Positive angles are in the sense of
*        rotation from north to east.
*     A1 = DOUBLE PRECISION (Given)
*        The sky longitude of the outlying point, in radians.
*     B1 = DOUBLE PRECISION (Given)
*        The sky latitude of the outlying point, in radians.
*     PARDST = DOUBLE PRECISION (Returned)
*        The arc-distance (in radians) from the reference point, to the
*        point of closest approach on the great circle defined by the
*        reference point and the position angle . If any of the input
*        coordinate values are equal to the Starlink "BAD" value
*        (VAL__BADD), then PRPDST is returned with the BAD value.
*        A positive value is returned if the outlying point is in the
*        direction of the given position angle.
*     PRPDST = DOUBLE PRECISION (Returned)
*        The arc-distance (in radians) of the outlying point, from the
*        point of closest approach on the great circle defined by the
*        reference point and the position angle . If any of the input
*        coordinate values are equal to the Starlink "BAD" value
*        (VAL__BADD), then PRPDST is returned with the BAD value.
*        A positive value is returned if rotation from the position
*        angle specified by ANGLE to the outlying point (as seen from
*        the reference point) is in the same sense as rotation from
*        north to east.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-FEB-1991 (DSB):
*        Original version.
*     26-APR-1991 (DSB):
*        Modified for IRA version 2.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.

*  Arguments Given:
      DOUBLE PRECISION A0
      DOUBLE PRECISION B0
      DOUBLE PRECISION ANGLE
      DOUBLE PRECISION A1
      DOUBLE PRECISION B1

*  Arguments Returned:
      DOUBLE PRECISION PARDST
      DOUBLE PRECISION PRPDST

*  Status:
      INTEGER STATUS             ! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if any of the input coordinates are BAD.
      IF( A0 .EQ. VAL__BADD .OR. B0 .EQ. VAL__BADD .OR.
     :    A1 .EQ. VAL__BADD .OR. B1 .EQ. VAL__BADD ) THEN
         PRPDST= VAL__BADD
         PARDST= VAL__BADD
         GO TO 999
      END IF

*  Call IRA1_IDST2 to do the work.
      CALL IRA1_IDST2( A0, B0, ANGLE, A1, B1, PARDST, PRPDST,
     :                 STATUS )

 999  CONTINUE

      END
