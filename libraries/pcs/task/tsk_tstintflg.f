      SUBROUTINE TASK_TSTINTFLG ( RESULT, STATUS )
*+
*  Name:
*     TASK_TSTINTFLG

*  Purpose:
*     Test interrupt flag

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_TSTINTFLG ( RESULT, STATUS )

*  Description:
*     Tests the flag which is set (in the AST routine) by interrupts.
*     The flag is cleared if it was set.
*     This can be polled by code which is executing under the fixed part
*     of the system to check for interrupts and act accordingly.

*  Arguments:
*     RESULT = LOGICAL(EXPORT)
*           status of interrupt flag on calling

*  Algorithm:
*     Uses flag in AST common block.

*  Authors:
*     John Cooke (REVA::ADAM) 21June84
*     {enter_new_authors_here}

*  History:
*     21-JUN-1984 (REVA::ADAM):
*        First insertion
*     25-APR-1991 (REVAD::BDK):
*        Revise INCLUDE files
*     30-APR-1991 (REVAD::BDK):
*        Revise INCLUDE files
*     13-MAY-1991 (REVAD::BDK):
*        Move to TASK library
*     04-OCT-1992 (RLVAD::AJC):
*        Add PAR_PAR for porting
*     24-AUG-1993 (RLVAD::AJC):
*        Use SUBPAR_SYS not PAR_PAR
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'TASK_PAR'
      INCLUDE 'MESSYS_LEN'
*  Arguments Returned:
      LOGICAL RESULT
*  Status:
      INTEGER STATUS
*  Global Variables:
      INCLUDE 'TASK_CMN'
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( INTRUPT_FLAG ) THEN
         RESULT = .TRUE.
         INTRUPT_FLAG = .FALSE.
      ELSE
         RESULT = .FALSE.
      ENDIF

      END
