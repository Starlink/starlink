*+  ADAM_TRIGGER - send triggering message without expecting acknowledgment
      SUBROUTINE ADAM_TRIGGER ( PATH, CONTEXT, NAME, VALUE,
     :                          MESSID, STATUS )
*    Description :
*     Sends a message to the task indicated in the path, but does not expect
*     an acknowledgment and so does not wait before returning.  The values
*     of PATH, CONTEXT, NAME and MESSID will usually have been copied
*     from the corresponding values obtained by a call to RECEIVE or 
*     GETREPLY. This routine is the same as ADAM_REPLY except that it uses
*     message status MESSYS__TRIGGER.
*    Invocation :
*     CALL ADAM_TRIGGER ( PATH, CONTEXT, NAME, VALUE, MESSID, STATUS )
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
*     W.F.Lupton (AAOEPP::WFL)
*    History :
*     01.05.1989: original (AAOEPP::WFL)
*     01.03.1990: correct comments (AAOEPP::WFL)
*     12.11.1992: use SAI__OK not ADAM__OK 
*                 renamed MESERRS to MESSYS_ERR (RLVAD::AJC)
*      8.03.1993: use MESSYS_PAR and MESSYS__MESSAGE
*                 not MESDEFNS and MESSAGE (RLVAD::AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'MESSYS_PAR'
      INCLUDE 'MESSYS_ERR'
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

      MSG_STATUS = MESSYS__TRIGGER
      MSG_FUNCTION = MESSYS__MESSAGE
      MSG_CONTEXT = CONTEXT
      MSG_NAME = NAME
      MSG_LENGTH = MIN ( LEN(VALUE), MSG_VAL_LEN )
      MSG_VAL = VALUE      !  truncated on right if too long

      CALL MESSYS_REPLY ( PATH, MSG, MESSID, STATUS )

      END
