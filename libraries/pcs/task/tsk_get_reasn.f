      SUBROUTINE TASK_GET_REASON ( REASON, STATUS )
*+
*  Name:
*     TASK_GET_REASON

*  Purpose:
*     Get reason for current reschedule

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_GET_REASON ( REASON, STATUS )

*  Description:
*     Return an integer whose value signifies the reason for the current 
*     reschedule. Their possible values are:
*       MESSYS__EXTINT
*       MESSYS__RESCHED
*       MESSYS__ASTINT
*       MESSYS__TRIGGER
*       any status from the completion of a subsidiary action

*  Arguments:
*     REASON=INTEGER (returned)
*           value indicating reason for reschedule
*     STATUS=INTEGER

*  Algorithm:
*     Return CURMESSTATUS from the common block

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     06-MAY-1991 (REVAD::BDK):
*        Original
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
      INTEGER REASON   ! value indicating reason for reschedule

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'TASK_CMN'
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      REASON = CURMESSTATUS

      END
