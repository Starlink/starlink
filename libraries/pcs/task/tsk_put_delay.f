*+  TASK_PUT_DELAY - set delay before next entry for current action
      SUBROUTINE TASK_PUT_DELAY ( DELAY, STATUS )
*    Description :
*     Sets the delay in milliseconds before the next entry for the current
*     action. This simply involves copying to COMMON.
*    Invocation :
*     CALL TASK_PUT_DELAY ( DELAY, STATUS )
*    Parameters :
*     DELAY=INTEGER (given)
*           The delay before the next action entry in milliseconds. Depending
*           on the status returned from ACT, this may act as as a time-out
*           period. -1 means infinity.
*     STATUS=INTEGER
*    Method :
*     Copy information to COMMON.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     W.F.Lupton (AAOEPP::WFL)
*    History :
*     29.04.1989:  original (AAOEPP::WFL)
*     23.04.1991:  rearrange INCLUDE files (REVAD::BDK)
*     06.05.1991:  remove ADAMDEFNS (REVAD::BDK)
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
      INTEGER DELAY   ! the delay before the next action entry

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'TASK_CMN'
*-
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Simply copy the delay to COMMON.
*
      CURACTDELAY = DELAY

      END
