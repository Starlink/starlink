*+  ADAM_SENDONLY - send message to another task 
      SUBROUTINE ADAM_SENDONLY ( PATH, CONTEXT, NAME, INVAL, 
     :                       MESSID, STATUS )
*    Description :
*     Send a message to the task indicated by 'PATH' with given context 
*     (get, set, obey, cancel). Return the message identifier in case any 
*     replies are expected from the communicating task.
*    Invocation :
*     CALL ADAM_SEND ( PATH, CONTEXT, NAME, INVAL, MESSID, STATUS )
*    Parameters :
*     PATH=INTEGER (given)
*           pointer to the path to the required task
*     CONTEXT=INTEGER (given)
*           parameterised 'get, set, obey, cancel'
*     NAME=CHARACTER*(*) (given)
*           name of required function or parameter
*     INVAL=CHARACTER*(*) (given)
*           value string to be sent
*     MESSID=INTEGER (returned)
*           message number issued by this task
*    Method :
*     Construct the message data structure and use MESSYS_SEND to send 
*     the message. This returns the MESSID. 
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     07.05.1991: original (REVAD::BDK)
*     12.11.1992:  use SAI__OK not ADAM__OK (RLVAD::AJC)
*      8.03.1993: use MESSYS_PAR and MESSYS__MESSAGE
*                 not MESDEFNS and MESSAGE (RLVAD::AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'MESSYS_PAR'
*    Import :
      INTEGER PATH        !  pointer to the path to the required task
      INTEGER CONTEXT     !  parameterised 'get, set, obey, cancel'
      CHARACTER NAME*(*)  !  name of required function or parameter
      CHARACTER INVAL*(*) !  value string to be sent
*    Export :
      INTEGER MESSID      !  message number issued by this task
*    Status :
      INTEGER STATUS 
*    Data structures for ADAM:
      INCLUDE 'MESSYS_DD'
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      MSG_STATUS = SAI__OK
      MSG_FUNCTION = MESSYS__MESSAGE
      MSG_CONTEXT = CONTEXT
      MSG_NAME = NAME
      MSG_LENGTH = MIN ( LEN(INVAL), MSG_VAL_LEN )
      MSG_VAL = INVAL        !  truncated on right if too long

      CALL MESSYS_SEND ( PATH, MSG, MESSID, STATUS )

      END

