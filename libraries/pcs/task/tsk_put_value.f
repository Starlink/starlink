*+  TASK_PUT_VALUE - set value string for current action
      SUBROUTINE TASK_PUT_VALUE ( VALUE, STATUS )
*    Description :
*     Sets the value string for the current action. This simply involves
*     copying to COMMON.
*    Invocation :
*     CALL TASK_PUT_VALUE ( VALUE, STATUS )
*    Parameters :
*     VALUE=CHARACTER*(*) (given)
*           The action value string
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
*     12.06.1991:  use CURACTVALUE (REVAD::BDK)
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
      CHARACTER*(*) VALUE   ! the action value string

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'TASK_CMN'
*-
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Simply copy the value to COMMON.
*
      CURACTVALUE = VALUE

      END
