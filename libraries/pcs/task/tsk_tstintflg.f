*+  TASK_TSTINTFLG - test interrupt flag
      SUBROUTINE TASK_TSTINTFLG ( RESULT, STATUS )
*    Description :
*     Tests the flag which is set (in the AST routine) by interrupts.
*     The flag is cleared if it was set.
*     This can be polled by code which is executing under the fixed part
*     of the system to check for interrupts and act accordingly.
*    Invocation :
*     CALL TASK_TSTINTFLG ( RESULT, STATUS )
*    Parameters :
*     RESULT = LOGICAL(EXPORT)
*           status of interrupt flag on calling
*    Method :
*     Uses flag in AST common block.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     John Cooke (REVA::ADAM) 21June84
*    History :
*     date:  changes (institution::username)
*     21-JUN-1984  first insertion (REVA::ADAM)
*     25.04.1991:  revise INCLUDE files (REVAD::BDK)
*     30.04.1991:  revise INCLUDE files (REVAD::BDK)
*     13.05.1991:  move to TASK library (REVAD::BDK)
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
      LOGICAL RESULT
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'TASK_CMN'
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( INTRUPT_FLAG ) THEN
         RESULT = .TRUE.
         INTRUPT_FLAG = .FALSE.
      ELSE
         RESULT = .FALSE.
      ENDIF

      END
