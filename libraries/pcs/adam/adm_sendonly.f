      SUBROUTINE ADAM_SENDONLY ( PATH, CONTEXT, NAME, INVAL,
     :                       MESSID, STATUS )
*+
*  Name:
*     ADAM_SENDONLY

*  Purpose:
*     send message to another task.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ADAM_SEND ( PATH, CONTEXT, NAME, INVAL, MESSID, STATUS )

*  Description:
*     Send a message to the task indicated by 'PATH' with given context
*     (get, set, obey, cancel). Return the message identifier in case any
*     replies are expected from the communicating task.

*  Arguments:
*     PATH=INTEGER (given)
*        pointer to the path to the required task
*     CONTEXT=INTEGER (given)
*        parameterised 'get, set, obey, cancel'
*     NAME=CHARACTER*(*) (given)
*        name of required function or parameter
*     INVAL=CHARACTER*(*) (given)
*        value string to be sent
*     MESSID=INTEGER (returned)
*        message number issued by this task

*  Algorithm:
*     Construct the message data structure and use MESSYS_SEND to send
*     the message. This returns the MESSID.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     07.05.1991: original (REVAD::BDK)
*     12.11.1992:  use SAI__OK not ADAM__OK (RLVAD::AJC)
*        8.03.1993: use MESSYS_PAR and MESSYS__MESSAGE
*                   not MESDEFNS and MESSAGE (RLVAD::AJC)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'MESSYS_PAR'

*  Arguments Given:
      INTEGER PATH        !  pointer to the path to the required task
      INTEGER CONTEXT     !  parameterised 'get, set, obey, cancel'
      CHARACTER NAME*(*)  !  name of required function or parameter
      CHARACTER INVAL*(*) !  value string to be sent

*  Arguments Returned:
      INTEGER MESSID      !  message number issued by this task

*  Status:
      INTEGER STATUS
*    Data structures for ADAM:
      INCLUDE 'MESSYS_DD'

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

      MSG_STATUS = SAI__OK
      MSG_FUNCTION = MESSYS__MESSAGE
      MSG_CONTEXT = CONTEXT
      MSG_NAME = NAME
      MSG_LENGTH = MIN ( LEN(INVAL), MSG_VAL_LEN )
      MSG_VAL = INVAL        !  truncated on right if too long

      CALL MESSYS_SEND ( PATH, MSG, MESSID, STATUS )

      END

