*+  ADAM_REPLY - send message without expecting acknowledgment
      SUBROUTINE ADAM_REPLY ( PATH, CONTEXT, NAME, VALUE,
     :                        MESSID, STATUS )
*    Description :
*     Sends a message to the task indicated in the path, but does not expect
*     an acknowledgment and so does not wait before returning.  The values
*     of PATH, CONTEXT, NAME and MESSID will usually have been copied
*     from the corresponding values obtained by a call to RECEIVE or 
*     GETREPLY.
*    Invocation :
*     CALL ADAM_REPLY ( PATH, CONTEXT, NAME, VALUE, MESSID, STATUS )
*    Parameters :
*     PATH=INTEGER (given)
*           pointer to the path back to the originating task
*     CONTEXT=INTEGER (given)
*           parameterised 'get, set, obey, cancel'
*     NAME=CHARACTER*(*) (given)
*           name of required function or parameter
*     VALUE=CHARACTER*(*) (given)
*           value to be sent
*     MESSID=INTEGER (given)
*           message number of original received message
*    Method :
*     Construct a message data structure and call MESSYS_REPLY.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     John Cooke (REVAD::JAC) <date>
*    History :
*     date:  changes (institution::username)
*     3-MAY-1984  first insertion (REVAD::JAC)
*     18-MAY-1984  use messys_reply not messys_send (E:[)
*     18-MAY-1984  "msg_status" added (REVA::ADAM])
*     10-OCT-1984  change "normal" to "adam__ok" (REVA::ADAM)
*     12-NOV-1992  use SAI__OK not ADAM__OK (RLVAD::AJC)
*      8-MAR-1993  use MESSYS_PAR and MESSYS__MESSAGE
*                  not MESDEFNS and MESSAGE (RLVAD::AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'MESSYS_PAR'
*    Import :
      INTEGER PATH        !  pointer to the path back to the originating task
      INTEGER CONTEXT     !  parameterised 'get, set, obey, cancel'
      CHARACTER NAME*(*)  !  name of required function or parameter
      CHARACTER VALUE*(*) !  value to be sent
      INTEGER MESSID      !  message number of original received message
*    Status :
      INTEGER STATUS
*    Data structures for ADAM:
      INCLUDE 'DDMSG'
*-

      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF

      MSG_STATUS = SAI__OK
      MSG_FUNCTION = MESSYS__MESSAGE
      MSG_CONTEXT = CONTEXT
      MSG_NAME = NAME
      MSG_LENGTH = MIN ( LEN(VALUE), MSG_VAL_LEN )
      MSG_VAL = VALUE      !  truncated on right if too long

      CALL MESSYS_REPLY ( PATH, MSG, MESSID, STATUS )

      END

