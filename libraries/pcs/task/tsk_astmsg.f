*+  TASK_ASTMSG - used in user AST routine to signal to dtask
      SUBROUTINE TASK_ASTMSG ( NAME, LENGTH, VALUE, STATUS )
*    Description :
*     This routine should be called inside a user AST routine invoked
*     initially by a d-task action, on completion of the AST routine.
*     It signals to the d-task system that completion has occurred and
*     the required action can now enter the next stage.
*    Invocation :
*     CALL TASK_ASTMSG ( NAME, LENGTH, VALUE, STATUS )
*    Parameters :
*     NAME=CHARACTER (given)
*           name of action to be staged
*     LENGTH=INTEGER (given)
*           number of significant bytes in VALUE
*     VALUE=CHARACTER*(*) (given)
*           a set of bytes to be passed to the main-line code
*     STATUS=INTEGER
*    Method :
*     Signal the MESSYS that an AST interrupt has occurred.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     09.04.1991:  original (REVAD::BDK)
*     25.04.1991:  rearrange INCLUDE files (REVAD::BDK)
*     30.04.1991:  rearrange INCLUDE files (REVAD::BDK)
*     13.05.1991:  move from DTASK library, don't disable ASTs 
*                  (REVAD::BDK)
*     12.06.1991:  remove lib$signal (REVAD::BDK)
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
      CHARACTER*(*) NAME     ! name of action to be staged
      INTEGER LENGTH         ! number of significant bytes in VALUE
      CHARACTER*(*) VALUE    ! a set of bytes to be passed to the
                             ! main-line code 
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'TASK_CMN'
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
