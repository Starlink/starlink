*+  TASK_GET_CURRINFO - get details of current action
      SUBROUTINE TASK_GET_CURRINFO ( SEQ, VALUE, DELAY, REQUEST, 
     :  STATUS ) 
*    Description :
*     Gets details of the current action that may have been set as a result
*     of user calls in ACT. This simply involves copying information 
*     from COMMON variables.
*    Invocation :
*     CALL TASK_GET_CURRINFO ( SEQ, VALUE, DELAY, REQUEST, STATUS )
*    Parameters :
*     SEQ=INTEGER (returned)
*           the action sequence number
*     VALUE=CHARACTER*(*) (returned)
*           The action value string
*     DELAY=INTEGER (returned)
*           The delay before the next action entry in milliseconds. Depending
*           on the status returned from ACT, this may act as as a time-out
*           period. -1 means infinity.
*     REQUEST=INTEGER (returned)
*           The request returned by the application.
*     STATUS=INTEGER
*    Method :
*     Copy information from COMMON.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     W.F.Lupton (AAOEPP::WFL)
*    History :
*     29.04.1989:  original (AAOEPP::WFL)
*     01.03.1990:  add SEQ argument (AAOEPP::WFL)
*     23.04.1991:  rearrange INCLUDE files (REVAD::BDK)
*     06.05.1991:  remove ADAMDEFNS (REVAD::BDK)
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

*    Export :
      INTEGER SEQ           ! the action sequence number
      CHARACTER*(*) VALUE   ! the action value string
      INTEGER DELAY         ! the delay before the next action entry
      INTEGER REQUEST       ! The request returned by the application.

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'TASK_CMN'
*-
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Simply copy the values from COMMON.
*
      SEQ = CURACTSEQ
      VALUE = CURACTVALUE
      DELAY = CURACTDELAY
      REQUEST = CURACTREQUEST

      END
