*+  TASK_PUT_SEQ - set current action sequence counter
      SUBROUTINE TASK_PUT_SEQ ( SEQ, STATUS )
*    Description :
*     Sets the current action sequence counter.
*    Invocation :
*     CALL TASK_PUT_SEQ ( SEQ, STATUS )
*    Parameters :
*     SEQ=INTEGER (given)
*           The current action sequence counter
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
*     01.03.1990:  original (AAOEPP::WFL)
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
      INTEGER SEQ     ! the current action sequence counter

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'TASK_CMN'
*-
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Simply copy the sequence counter to COMMON.
*
      CURACTSEQ = SEQ

      END
