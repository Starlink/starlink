*+  TASK_INIT_MESSINFO - initialise list of active subsidiary actions
      SUBROUTINE TASK_INIT_MESSINFO ( STATUS )
*    Description :
*     Initialises the list of active subsidiary actions. This simply involves
*     clearing the action pointers to zero, since the first zero action
*     pointer indicates the entry past the last used one.
*    Invocation :
*     CALL TASK_INIT_MESSINFO ( STATUS )
*    Parameters :
*     STATUS=INTEGER
*    Method :
*     Clear all action pointers to zero.
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

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'TASK_CMN'

*    Local variables :
      INTEGER I           ! counter
*-
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Cycle through the list clearing all action pointers to zero (end of list).
*
      DO I = 1, TASK__MAXSUB
         MESACTPTR(I) = 0
      ENDDO

      END
