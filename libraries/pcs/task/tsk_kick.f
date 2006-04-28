      SUBROUTINE TASK_KICK ( NAME, LENGTH, VALUE, STATUS )
*+
*  Name:
*     TASK_KICK

*  Purpose:
*     Signal another action to reschedule

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_KICK ( NAME, LENGTH, VALUE, STATUS )

*  Description:
*     This routine should be called by one action in a task to cause
*     another action in that task to be rescheduled.

*  Arguments:
*     NAME=CHARACTER (given)
*           name of action to be rescheduled
*     LENGTH=INTEGER (given)
*           number of significant bytes in VALUE
*     VALUE=CHARACTER*(*) (given)
*           a set of bytes to be passed to the main-line code
*     STATUS=INTEGER

*  Algorithm:
*     Use MESSYS_KICK.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     23-MAY-1991 (REVAD::BDK):
*        Original
*     27-MAY-1991 (REVAD::BDK):
*        Remove LIB$SIGNAL
*     15-JUN-2001 (AJC):
*        Use AMS (FAMS) _KICK not MESSYS_KICK
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER*(*) NAME     ! name of action to be rescheduled
      INTEGER LENGTH         ! number of significant bytes in VALUE
      CHARACTER*(*) VALUE    ! a set of bytes to be passed to the
                             ! main-line code 
*  Status:
      INTEGER STATUS
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Inform the message system.
*
      CALL FAMS_KICK ( NAME, LENGTH, VALUE, STATUS )

      END
