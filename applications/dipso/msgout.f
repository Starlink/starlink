      SUBROUTINE MSGOUT( COMM, MESS, BELL, STATUS )
*+
* Name:
*     MSGOUT

*  Purpose:
*     Display a message

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MSGOUT( COMM, MESS, BELL, STATUS )

*  Description:
*     The command name is formatted into the supplied message. Any
*     MSG tokens embedded in the message are expanded. If required a
*     bell is sounded and a newline issued.

*  Arguments:
*     COMM = CHARACTER * ( * ) (Given)
*        The command name.
*     MESS = CHARACTER * ( * ) (Given)
*        The message.
*     BELL = LOGICAL (Given)
*        Should a bell be sounded and a new line be started?
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-AUG-1994 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      LOGICAL BEEP               ! Is a beep allowed?
      COMMON /BEEP/ BEEP

*  Arguments Given:
      CHARACTER * ( * ) COMM
      CHARACTER * ( * ) MESS
      LOGICAL BELL

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Assign the required message to MSG token MESS.
      CALL REPFRM( 'MESS', COMM, MESS, STATUS )

*  Issue the message.
      CALL MSG_OUT( ' ', '^MESS', STATUS )

*  If required, ring the bell and issue a new line.
      IF( BELL .AND. BEEP ) CALL MSG_BELL( STATUS )

      END
