      SUBROUTINE USR_SPAWN( COMMAND, STATUS )
*+
*  Name:
*     SUBROUTINE USR_SPAWN

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_SPAWN( COMMAND, STATUS )

*  Arguments:
*     COMMAND = CHARACTER* ( * ) (Given)
*        The command to be executed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Purpose:
*     Execute commands in forked shell.

*  Authors:
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     20-JUL-94 (MJC):
*       IUEDR Vn. 3.1-2
*     18-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER*( * ) COMMAND ! Command to be executed.

*  Status:
      INTEGER STATUS          ! Global status.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL SYSTEM( COMMAND )

      END
