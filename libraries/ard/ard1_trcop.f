      SUBROUTINE ARD1_TRCOP( NDIM, CIN, COUT, STATUS )
*+
*  Name:
*     ARD1_TRCOP

*  Purpose:
*     Copy the co-efficients of a linear transformation

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_TRCOP( NDIM, CIN, COUT, STATUS )

*  Description:
*     The co-efficients supplied in CIN are copied to COUT.

*  Arguments:
*     NDIM = INTEGER (Given)
*        The dimensionality of the co-ordinate system.
*     CIN( * ) = REAL (Given)
*        The input co-efficients. The array should hold
*        NDIM*( NDIM + 1 ) values.
*     COUT( * ) = REAL (Returned)
*        The output co-efficients. The array should hold
*        NDIM*( NDIM + 1 ) values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-FEB-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NDIM
      REAL CIN( * )

*  Arguments Returned:
      REAL COUT( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop count

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy the co-efficients.
      DO I = 1, NDIM*( NDIM + 1 )
         COUT( I ) = CIN( I )
      END DO

      END
