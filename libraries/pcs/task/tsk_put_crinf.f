*+  TASK_PUT_CURRINFO - set details of current action
      SUBROUTINE TASK_PUT_CURRINFO ( ACTPTR, CONTEXT, NAMECODE, ANAME,
     :  SEQ, VALUE, REQUEST, STATUS )
*    Description :
*     Sets details of the current action (excluding those that pertain to
*     actions in subsidiary tasks) so that user code can call routines to
*     retrieve the information. This simply involves copying information to
*     COMMON variables.
*    Invocation :
*     CALL TASK_PUT_CURRINFO ( ACTPTR, CONTEXT, NAMECODE, ANAME,
*    :  SEQ, VALUE, REQUEST, STATUS )
*    Parameters :
*     ACTPTR=INTEGER (given)
*           The action pointer
*     CONTEXT=INTEGER (given)
*           The action context (OBEY or CANCEL)
*     NAMECODE=INTEGER (given)
*           The action namecode
*     ANAME=CHARACTER*(*) (given)
*           The action name
*     SEQ=INTEGER (given)
*           The action sequence counter
*     VALUE=CHARACTER*(*) (given)
*           The action value string
*     REQUEST=INTEGER (given)
*           The stored application request
*     STATUS=INTEGER
*    Method :
*     Copy information to COMMON. Set default of -1 (infinite) for delay
*     and blank for output value string.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     W.F.Lupton (AAOEPP::WFL)
*    History :
*     29.04.1989:  original (AAOEPP::WFL)
*     23.04.1991:  rearrange INCLUDE files (REVAD::BDK)
*     06.05.1991:  pass-in and store the action name. Remove ADAMDEFNS 
*                  (REVAD::BDK)
*     12.06.1991:  use CURACTVALUE (REVAD::BDK)
*     22.08.1991:  add REQUEST (REVAD::BDK)
*      4.10.1992:  add PAR_PAR for porting (RLVAD::AJC)
*     24.08.1993:  Use SUBPAR_SYS not PAR_PAR (RLVAD::AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'MESSYS_LEN'
      INCLUDE 'TASK_PAR'

*    Import :
      INTEGER ACTPTR        ! the action pointer
      INTEGER CONTEXT       ! the action context (OBEY or CANCEL)
      INTEGER NAMECODE      ! the action namecode
      CHARACTER*(*) ANAME   ! the action name
      INTEGER SEQ           ! the action sequence counter
      CHARACTER*(*) VALUE   ! the action value string
      INTEGER REQUEST       ! the stored application request

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'TASK_CMN'
*-
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Simply copy the values to COMMON.
*
      CURACTPTR = ACTPTR
      CURACTCONTEXT = CONTEXT
      CURACTNAMECODE = NAMECODE
      CURACTNAME = ANAME
      CURACTSEQ = SEQ
      CURACTVALUE = VALUE
      CURACTREQUEST = REQUEST
      CURACTDELAY = -1

      END
