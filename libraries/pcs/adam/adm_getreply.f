*+  ADAM_GETREPLY - wait for incoming message from specified path & messid
      SUBROUTINE ADAM_GETREPLY ( PATH, MESSID, CONTEXT, NAME, VALUE,
     :                           STATUS )
*    Description :
*     Waits for a message from a particular path and with a particular 
*     ID to arrive at this task, and returns with the message and 
*     associated parameters.
*    Invocation :
*     CALL ADAM_GETREPLY ( PATH, MESSID; CONTEXT, NAME, VALUE, STATUS )
*    Parameters :
*     PATH=INTEGER (given)
*           required path of the received message
*     MESSID=INTEGER (given)
*           required message number of received message
*     CONTEXT=INTEGER (returned)
*           parameterised 'get, set, obey, cancel'
*     NAME=CHARACTER*(*) (returned)
*           name of function or parameter
*     VALUE=CHARACTER*(*) (returned)
*           received value 
*     STATUS=INTEGER
*    Method :
*     Call MESSYS_GETREPLY with infinite timeout and interpret the data 
*     structure returned.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     Charlie Richardson (REVA::DCR) 25-Feb-85
*    History :
*     date:  changes (institution::username)
*     26-FEB-1985  First insertion (REVA::ADAM)
*     06.06.1986:  return values even if bad status from MESSYS 
*                  (REVAD::BDK)
*     12.11.1992:  use SAI__OK not ADAM__OK (RLVAD::AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'MESSYS_PAR'
*    Import :
      INTEGER PATH        !  required path of the received message
      INTEGER MESSID      !  required message number of received message
*    Export :
      INTEGER CONTEXT     !  parameterised 'get, set, obey, cancel'
      CHARACTER NAME*(*)  !  name of function or parameter
      CHARACTER VALUE*(*) !  received value 
*    Status :
      INTEGER STATUS 
*    Local Constants :
      INTEGER TIMEOUT
      PARAMETER ( TIMEOUT = MESSYS__INFINITE )
*    Data structures for ADAM:
      INCLUDE 'MESSYS_DD'
*-

      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF

      CALL MESSYS_GETREPLY ( TIMEOUT, PATH, MESSID, MSG, STATUS )

      CONTEXT = MSG_CONTEXT
      NAME = MSG_NAME

      IF ( MSG_LENGTH .GT. 0 ) THEN
         VALUE = MSG_VAL(1:MSG_LENGTH)
      ELSE
         VALUE = ' '
      ENDIF

      IF ( STATUS .EQ. SAI__OK ) THEN
         STATUS = MSG_STATUS
      ENDIF

      END
