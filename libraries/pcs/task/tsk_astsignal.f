*+  TASK_ASTSIGNAL - signal an AST event to main-line code
      SUBROUTINE TASK_ASTSIGNAL ( NAME, STATUS )
*    Description :
*     This routine should be called inside a user AST routine invoked
*     initially by a d-task action, on completion of the AST routine.
*     It signals to the d-task system that completion has occurred and
*     the required action can now enter the next stage.
*    Invocation :
*     CALL TASK_ASTSIGNAL ( NAME, STATUS )
*    Parameters :
*     NAME = CHARACTER (import)
*           name of action to be staged
*    Method :
*     Disable further AST events. 
*     Signal the MESSYS that an AST interrupt has occurred.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     John Cooke (REVA::ADAM) 21June84
*    History :
*     21-JUN-1984  first insertion (REVA::ADAM)
*     25-JUN-1984  status reporting (REVA::ADAM)
*     25-JUN-1984  srchlst status success is "actactive" (REVA::ADAM)
*     25-JUN-1984  include defns file (REVA::ADAM)
*     25-JUN-1984  statuses - right this time ? (REVA::ADAM)
*     23.06.1985:  total rewrite (REVAD::BDK)
*     09.04.1991:  use MESSYS_ASTMSG (REVAD::BDK)
*     25.04.1991:  revise INCLUDE files (REVAD::BDK)
*     30.04.1991:  revise INCLUDE files (REVAD::BDK)
*     13.05.1991:  move to TASK library, don't disable ASTs (REVAD::BDK)
*     11.06.1991:  remove lib$signal (REVAD::BDK)
*      4.10.1992:  add PAR_PAR for porting (RLVAD::AJC)
*     24.08.1993:  Use SUBPAR_SYS not PAR_PAR (RLVAD::AJC)
*     15.06.2001:  Use AMS (FAMS) _ASTMSG not MESSYS_ASTMSG (AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'TASK_PAR'
      INCLUDE 'MESSYS_LEN'
*    Import :
      CHARACTER NAME*(*)
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'TASK_CMN'
*    Local Constants :
      INTEGER LENGTH
      PARAMETER ( LENGTH = 1 )
      CHARACTER*1 VALUE
      PARAMETER ( VALUE = ' ' )
*    Local variables :
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Set the interrupt flag in case an application wants to test for it.
*
      INTRUPT_FLAG = .TRUE.
*
*   Inform the message system.
*
      CALL FAMS_ASTMSG ( NAME, LENGTH, VALUE, STATUS )

      END
