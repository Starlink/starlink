*+  TASK_GET_NAME - get current action name
      SUBROUTINE TASK_GET_NAME ( NAME, STATUS )
*    Description :
*     Gets current action name. This simply involves copying the name
*     from COMMON. 
*    Invocation :
*     CALL TASK_GET_NAME ( NAME, STATUS)
*    Parameters :
*     NAME=CHARACTER*(*) (returned)
*           The action name
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
*     23.04.1991:  rearrange INCLUDE files (REVAD::BDK)
*     06.05.1991:  remove ADAMDEFNS, don't call SUBPAR (REVAD::BDK)
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
      CHARACTER*(*) NAME  ! the action name

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'TASK_CMN'

*    Local variables :
*-
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Simply copy the value from COMMON.
*
      NAME = CURACTNAME

      END
