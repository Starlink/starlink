      SUBROUTINE IRA1_SHAPP( DIST, R0, R3, A0, A4, B4, STATUS )
*+
*  Name:
*     IRA1_SHAPP

*  Purpose:
*     Use the vectors calculated by IRA1_SHCAL to find a sky position
*     which is offset along a given position angle.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_SHAPP( DIST, R0, R3, A0, A4, B4, STATUS )

*  Description:
*     This routine uses the vectors R0 and R3 calculated previously by
*     IRA1_SHCAL to find the sky position which is offset away from the
*     "reference" position (see routine IRA_SHIFT) by a given arc
*     distance, along a given great circle.
*
*     No checks are made for BAD (=VAL__BADD) values.

*  Arguments:
*     DIST = DOUBLE PRECISION (Given)
*        The arc distance to move away from the reference position
*        in the given direction, in radians.
*     R0( 3 ) = DOUBLE PRECISION (Given)
*        The 3-vector representing the reference position.
*     R3( 3 ) = DOUBLE PRECISION (Given)
*        The 3-vector representing the point which is 90 degrees away
*        from the reference point, along the required great circle.
*     A0 = DOUBLE PRECISION (Given)
*        The sky longitude of the reference position, in radians.
*     A4 = DOUBLE PRECISION (Returned)
*        The sky longitude of the required point, in radians.
*     B4 = DOUBLE PRECISION (Returned)
*        The sky latitude of the required point, in radians.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-MAY-1991 (DSB):
*        Original version.
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
      DOUBLE PRECISION DIST
      DOUBLE PRECISION R0( 3 )
      DOUBLE PRECISION R3( 3 )
      DOUBLE PRECISION A0

*  Arguments Returned:
      DOUBLE PRECISION A4
      DOUBLE PRECISION B4

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION COSDST    ! Cosine of DIST
      DOUBLE PRECISION R4(3)     ! Required position vector.
      DOUBLE PRECISION SINDST    ! Sine of DIST

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store commonly used values.
      SINDST = SIN( DIST )
      COSDST = COS( DIST )

*  The vector R4 representing the required point is produced as a
*  linear sum of R0 and R3.
      R4(1) = COSDST*R0(1) + SINDST*R3(1)
      R4(2) = COSDST*R0(2) + SINDST*R3(2)
      R4(3) = COSDST*R0(3) + SINDST*R3(3)

*  Create the longitude of the required point. If this point is within
*  an arc-second (about) of a pole it is assigned the same longitude
*  as the reference point.
      IF( ABS( R4(1) ) .GT. 5.0D-6 .OR. ABS( R4(2) ) .GT. 5.0D-6 ) THEN
         A4 = ATAN2( R4(2), R4(1) )

      ELSE
         A4 = A0

      END IF

*  Create the latitude of the required point.
      B4 = ASIN( MAX( -1.0D0, MIN( 1.0D0, R4(3) ) ) )

      END
