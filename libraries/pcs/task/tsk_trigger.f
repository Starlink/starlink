*+  TASK_TRIGGER - trigger a reschedule in the controlling task
      SUBROUTINE TASK_TRIGGER ( NAME, VALUE, STATUS )
*    Description :
*     Return a trigger message to the task which issued the OBEY to 
*     start the current action in this task. If the other task is 
*     waiting in its fixed-part, this will cause it to reschedule the 
*     action.
*    Invocation :
*     CALL TASK_TRIGGER ( NAME, VALUE, STATUS )
*    Parameters :
*     NAME=CHARACTER*(*) (given)
*           name of the action in this task
*     VALUE=CHARACTER*(*) (given)
*           message string to be sent to the other task
*     STATUS=INTEGER
*    Method :
*     Look-up the communications path back to the controlling task and 
*     send it a trigger message.
*    Deficiencies :
*     Calls the DTASK library, which is a case of wrong layering.
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     12.06.1991: original (REVAD::BDK)
*     11.06.2001: call AMS (FAMS) directly (AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADAM_DEFNS'
      INCLUDE 'MESSYS_PAR'
      INCLUDE 'MESSYS_ERR'

*    Import :
      CHARACTER*(*) NAME   ! name of the action in this task
      CHARACTER*(*) VALUE  ! message string to be sent to the other task

*    Status :
      INTEGER STATUS

*    Local variables :
      INTEGER PATH         ! path to other task
      INTEGER MESSID       ! identifier for the current transaction
      INTEGER CONTEXT      ! always OBEY
      INTEGER MESLEN       ! length of VALUE
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      CONTEXT = OBEY
      CALL DTASK_GETPATH ( NAME, PATH, MESSID, STATUS )
      MESLEN = MIN( LEN(VALUE), MESSYS__VAL_LEN )
      CALL FAMS_REPLY( PATH, MESSID, MESSYS__MESSAGE, MESSYS__TRIGGER,
     :  CONTEXT, NAME, MESLEN, VALUE, STATUS )
      END
