*+  TASK_GET_REASON - get reason for current reschedule
      SUBROUTINE TASK_GET_REASON ( REASON, STATUS )
*    Description :
*     Return an integer whose value signifies the reason for the current 
*     reschedule. Their possible values are:
*       MESSYS__EXTINT
*       MESSYS__RESCHED
*       MESSYS__ASTINT
*       MESSYS__TRIGGER
*       any status from the completion of a subsidiary action
*    Invocation :
*     CALL TASK_GET_REASON ( REASON, STATUS )
*    Parameters :
*     REASON=INTEGER (returned)
*           value indicating reason for reschedule
*     STATUS=INTEGER
*    Method :
*     Return CURMESSTATUS from the common block
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     06.05.1991: original (REVAD::BDK)
*      4.10.1992:  add PAR_PAR for porting (RLVAD::AJC)
*     24.08.1993:  Use SUBPAR_SYS not PAR_PAR (RLVAD::AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'TASK_PAR'
      INCLUDE 'MESSYS_LEN'

*    Export :
      INTEGER REASON   ! value indicating reason for reschedule

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'TASK_CMN'
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      REASON = CURMESSTATUS

      END
