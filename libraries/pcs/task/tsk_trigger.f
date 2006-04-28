      SUBROUTINE TASK_TRIGGER ( NAME, VALUE, STATUS )
*+
*  Name:
*     TASK_TRIGGER

*  Purpose:
*     Trigger a reschedule in the controlling task

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_TRIGGER ( NAME, VALUE, STATUS )

*  Description:
*     Return a trigger message to the task which issued the OBEY to 
*     start the current action in this task. If the other task is 
*     waiting in its fixed-part, this will cause it to reschedule the 
*     action.

*  Arguments:
*     NAME=CHARACTER*(*) (given)
*           name of the action in this task
*     VALUE=CHARACTER*(*) (given)
*           message string to be sent to the other task
*     STATUS=INTEGER

*  Algorithm:
*     Look-up the communications path back to the controlling task and 
*     send it a trigger message.

*  Implementation Deficiencies:
*     Calls the DTASK library, which is a case of wrong layering.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     12-JUN-1991 (REVAD::BDK):
*        Original
*     11-JUN-2001 (AJC):
*        Call AMS (FAMS) directly
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADAM_DEFNS'
      INCLUDE 'MESSYS_PAR'
      INCLUDE 'MESSYS_ERR'

*  Arguments Given:
      CHARACTER*(*) NAME   ! name of the action in this task
      CHARACTER*(*) VALUE  ! message string to be sent to the other task

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER PATH         ! path to other task
      INTEGER MESSID       ! identifier for the current transaction
      INTEGER CONTEXT      ! always OBEY
      INTEGER MESLEN       ! length of VALUE
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      CONTEXT = OBEY
      CALL DTASK_GETPATH ( NAME, PATH, MESSID, STATUS )
      MESLEN = MIN( LEN(VALUE), MESSYS__VAL_LEN )
      CALL FAMS_REPLY( PATH, MESSID, MESSYS__MESSAGE, MESSYS__TRIGGER,
     :  CONTEXT, NAME, MESLEN, VALUE, STATUS )
      END
