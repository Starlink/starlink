*+  ADAM_ACKNOW - acknowledge a message from another task
      SUBROUTINE ADAM_ACKNOW ( PATH, MESSID, MESSTATUS, CONTEXT,
     :  NAME, VALUE, STATUS )
*    Description :
*     puts the given information into a message and returns it along the 
*     indicated PATH.
*    Invocation :
*     CALL ADAM_ACKNOW ( PATH, MESSID, MESSTATUS, CONTEXT,
*    :  NAME, VALUE, STATUS )
*    Parameters :
*     PATH=INTEGER (given)
*           message path for the REPLY
*     MESSID=INTEGER (given)
*           message id of original incoming message
*     MESSTATUS=INTEGER (given)
*           message status to be set in the reply
*     CONTEXT=INTEGER (given)
*           whether original message was get/set/obey/cancel
*     NAME=CHARACTER*(*) (given)
*           name of the original requested action
*     VALUE=CHARACTER*(*) (given)
*           message string to be sent in reply
*     STATUS=INTEGER
*    Method :
*     The given information is packed into a message structure and sent 
*     along the indicated PATH using MESSYS_REPLY.
*    Deficiencies :
*     This can, in theory, truncate the VALUE string. In practice, this 
*     should not happen. Ignore such an error - it is bad for this 
*     routine to return a bad status.
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     24-NOV-1984  Original version (REVA::ADAM)
*     25.04.1991:  revise INCLUDE files (REVAD::BDK)
*     30.04.1991:  use MESSYS_PAR (REVAD::BDK)
*     25.11.1991:  renamed from DTASK_ACKNOW (REVAD::BDK)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'MESSYS_PAR'

*    Import :
      INTEGER PATH              ! message path for the REPLY

      INTEGER MESSID            ! message id of original incoming message

      INTEGER MESSTATUS         ! message status to be set in the reply

      INTEGER CONTEXT           ! whether original message was 
                                ! get/set/obey/cancel

      CHARACTER*(*) NAME        ! name of the original requested action

      CHARACTER*(*) VALUE       ! message string to be sent in reply

*    Status :
      INTEGER STATUS

*    Data structures for ADAM
      INCLUDE 'MESSYS_DD'
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Load the information into the 'message structure'
*
      MSG_STATUS = MESSTATUS
      MSG_FUNCTION = MESSYS__MESSAGE
      MSG_CONTEXT = CONTEXT
      MSG_NAME = NAME
      MSG_LENGTH = MIN ( LEN(VALUE), MSG_VAL_LEN )
      MSG_VAL = VALUE
*
*   Send the reply
*
      CALL MESSYS_REPLY ( PATH, MSG, MESSID, STATUS )

      END
