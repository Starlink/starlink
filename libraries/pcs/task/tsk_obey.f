*+  TASK_OBEY - send an OBEY to a task
      SUBROUTINE TASK_OBEY ( TASK_NAME, NAME, INVAL, OUTVAL, PATH, 
     :  MESSID, STATUS )
*    Description :
*     Send an OBEY command and command-line parameter string to a task.
*    Invocation :
*     CALL TASK_OBEY ( TASK_NAME, NAME, INVAL, OUTVAL, PATH, 
*    :  MESSID, STATUS )
*    Parameters :
*     TASK_NAME=CHARACTER*(*) (given)
*           the name of the task
*     NAME=CHARACTER*(*) (given)
*           the name of the action
*     INVAL=CHARACTER*(*) (given)
*           the command-line parameter values
*     OUTVAL=CHARACTER*(*) (returned)
*           the string returned from the task
*     PATH=INTEGER (returned)
*           the path to the task
*     MESSID=INTEGER (returned)
*           the message identifier for the OBEY
*     STATUS=INTEGER
*    Method :
*     Get a path to the named task and send it a message.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     05.11.1987:  original (REVAD::BDK)
*     06.05.1991:  remove DDMSG (REVAD::BDK)
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
      CHARACTER*(*) TASK_NAME   ! the name of the task

      CHARACTER*(*) NAME        ! the name of the parameter

      CHARACTER*(*) INVAL       ! the command-line parameter values

*    Export :
      CHARACTER*(*) OUTVAL      ! the string returned from the task

      INTEGER PATH              ! the path to the task
      INTEGER MESLEN            ! length of INVAL
      INTEGER MESSID            ! the message identifier for the OBEY

*    Status :
      INTEGER STATUS

*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      OUTVAL = ' '
      CALL FAMS_PATH ( TASK_NAME, PATH, STATUS )
      MESLEN = MIN( LEN(INVAL), MESSYS__VAL_LEN )
      CALL FAMS_SEND( PATH, MESSYS__MESSAGE, SAI__OK, OBEY, NAME,
     :  MESLEN, INVAL, MESSID, STATUS )
      CALL TASK_DONE ( MESSYS__INFINITE, PATH, MESSID, OUTVAL, STATUS )


      END

