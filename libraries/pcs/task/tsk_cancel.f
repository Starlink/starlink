*+  TASK_CANCEL - request a task to cancel an action
      SUBROUTINE TASK_CANCEL ( TASK_NAME, NAME, INVAL, OUTVAL, STATUS )
*    Description :
*     Request the named task to cancel the named action.
*    Invocation :
*     CALL TASK_CANCEL ( TASK_NAME, NAME, INVAL, OUTVAL, STATUS )
*    Parameters :
*     TASK_NAME=CHARACTER*(*) (given)
*           the name of the task
*     NAME=CHARACTER*(*) (given)
*           the name of the action
*     INVAL=CHARACTER*(*) (given)
*           the parameter list to be sent to the task
*     OUTVAL=CHARACTER*(*) (returned)
*           the string returned from the task
*     STATUS=INTEGER
*    Method :
*     Establish the communication path to the named task and send it a 
*     message.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     05.11.1987:  original (REVAD::BDK)
*     08.08.1991:  do message forwarding (REVAD::BDK)
*     11.06.2001:  replace ADAM calls with AMS (AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADAM_DEFNS'
      INCLUDE 'MESSYS_PAR'
*    Import :
      CHARACTER*(*) TASK_NAME     ! the name of the task
      CHARACTER*(*) NAME          ! the name of the action
      CHARACTER*(*) INVAL         ! the parameter list to be sent to the 
                                  ! task
*    Export :
      CHARACTER*(*) OUTVAL        ! the string returned from the task

*    Status :
      INTEGER STATUS

*    Local variables :
      INTEGER PATH                 ! path to task
      INTEGER MESSID               ! message id for the cancel
      INTEGER MESLEN               ! length of INVAL
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL FAMS_PATH ( TASK_NAME, PATH, STATUS )
      MESLEN = MIN( LEN(INVAL), MESSYS__VAL_LEN )
      CALL FAMS_SEND( PATH, MESSYS__MESSAGE, SAI__OK, CANCEL, NAME,
     :  MESLEN, INVAL, MESSID, STATUS )
      CALL TASK_DONE ( MESSYS__INFINITE, PATH, MESSID, OUTVAL, STATUS )

      END
