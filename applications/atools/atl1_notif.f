      SUBROUTINE ATL1_NOTIF( MSG, STATUS )
*+
*  Name:
*     ATL1_NOTIF

*  Purpose:
*     Print a message to the screen if ATOOLS_VERBOSE is set.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL1_NOTIF( MSG, STATUS )

*  Description:

*  Arguments:
*     MSG = CHARACTER * ( * ) (Given)
*        The message.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-JUN-2003 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER MSG*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER VERB
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Only display the message if environment variable ATOOLS_VERBOSE is defined.
      CALL PSX_GETENV( 'ATOOLS_VERBOSE', VERB, STATUS )
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
      ELSE
         CALL MSG_OUT( ' ', MSG, STATUS )
      END IF

      END
