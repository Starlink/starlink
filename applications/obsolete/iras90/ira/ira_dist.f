      SUBROUTINE IRA_DIST( A0, B0, A1, B1, DIST, STATUS )
*+
*  Name:
*     IRA_DIST

*  Purpose:
*     Find the arc distance between two sky positions.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_DIST( A0, B0, A1, B1, DIST, STATUS )

*  Description:
*     This routine returns the length of the arc joining the two given
*     sky positions. If any of the input coordinate values are equal to
*     the Starlink "BAD" value (VAL__BADD) then the returned distance
*     will also be equal to the BAD value.

*  Arguments:
*     A0 = DOUBLE PRECISION (Given)
*        The sky longitude of the first position, in radians.
*     B0 = DOUBLE PRECISION (Given)
*        The sky latitude of the first position, in radians.
*     A1 = DOUBLE PRECISION (Given)
*        The sky longitude of the second position, in radians.
*     B1 = DOUBLE PRECISION (Given)
*        The sky latitude of the second position, in radians.
*     DIST = DOUBLE PRECISION (Returned)
*        The length of the arc joining the two positions, in
*        radians. This is always in the range 0 to +PI.
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

*  Arguments Given:
      DOUBLE PRECISION A0
      DOUBLE PRECISION B0
      DOUBLE PRECISION A1
      DOUBLE PRECISION B1

*  Arguments Returned:
      DOUBLE PRECISION DIST

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if any of the input coordinates are BAD.
      IF( A0 .EQ. VAL__BADD .OR. B0 .EQ. VAL__BADD .OR.
     :    A1 .EQ. VAL__BADD .OR. B1 .EQ. VAL__BADD ) THEN
         DIST = VAL__BADD

*  If not, calculate the distance.
      ELSE
         DIST = ACOS( COS( B0 )*COS( B1 )*COS( A0 - A1 )
     :             + SIN( B0 )*SIN( B1 ) )

      END IF

 999  CONTINUE

      END
