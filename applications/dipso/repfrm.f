      SUBROUTINE REPFRM( TOKEN, COMM, MESS, STATUS )
*+
* Name:
*     REPFRM

*  Purpose:
*     Format a message

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL REPFRM( TOKEN, COMM, MESS, STATUS )

*  Description:
*     The command name is formatted into the supplied message. Any
*     MSG tokens embedded in the message are expanded. The complete
*     message is assigned to the name MSG token.

*  Arguments:
*     TOKEN = CHARACTER * ( * ) (Given)
*        The MSG token to which the complete report will be assigned.
*     COMM = CHARACTER * ( * ) (Given)
*        The command name.
*     MESS = CHARACTER * ( * ) (Given)
*        The message.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-SEP-1994 (DSB):
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
      CHARACTER * ( * ) TOKEN
      CHARACTER * ( * ) COMM
      CHARACTER * ( * ) MESS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER
     :        TEXT*255,          ! The complete message text
     :        ITEXT*255          ! Input text

      INTEGER
     :        F,                 ! Index of first non-blank character
     :        L,                 ! Index of last non-blank character
     :        LTEXT              ! Used length of TEXT
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the start and end of the command name.
      CALL CHR_FANDL( COMM, F, L )

*  If a name was supplied, incorporate it into the message. Expand any
*  tokens in the message and store the resulting string in TEXT.
      IF( F .LE. L ) THEN
         ITEXT = '   '//COMM( F : L )//':  '//MESS
         CALL MSG_LOAD( ' ', ITEXT, TEXT, LTEXT, STATUS )
      ELSE
         CALL MSG_LOAD( ' ', MESS, TEXT, LTEXT, STATUS )

      END IF

*  Assign the report to the supplied token.
      CALL MSG_SETC( TOKEN, TEXT )

      END
