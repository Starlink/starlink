      SUBROUTINE IRQ1_INIT( STATUS )
*+
*  Name:
*     IRQ1_INIT

*  Purpose:
*     Initialise the IRQ_ system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ1_INIT( STATUS )

*  Description:
*     A new temporary HDS object is created to hold an array of
*     compiled quality expression structures. All IRQ identifiers are
*     set invalid.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-JUL-1991 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'IRQ_PAR'          ! IRQ constants.

*  Global Variables:
      INCLUDE 'IRQ_COM'          ! IRQ common blocks.
*        QCM_LOC = CHARACTER (Write)
*           An HDS locator to the array of compiled quality expression
*           structures.
*        QCM_STATE = CHARACTER (Write)
*           If QCM_STATE = IRQ__GOING then IRQ has been initialised.
*        QCM_VALID( IRQ__MAXQ ) = LOGICAL (Write)
*           True if the corresponding group identifier is valid (i.e. in
*           use).

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IDQ                ! An IRQ identifier.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a temporary HDS object to hold the structures. This is
*  a one dimensional array of QEXP structures with size given
*  by symbolic constant IRQ__MAXQ.
      CALL IRQ1_TEMP( 'QEXP', 1, IRQ__MAXQ, QCM_LOC, STATUS )

*  Initialise all identifiers to "not in use".
      DO IDQ = 1, IRQ__MAXQ
         QCM_VALID( IDQ ) = .FALSE.
      END DO

*  If all has gone OK, set the common variable QCM_STATE to indicate
*  that the IRQ_ system is initialised.
      IF( STATUS .EQ. SAI__OK ) QCM_STATE = IRQ__GOING

      END
