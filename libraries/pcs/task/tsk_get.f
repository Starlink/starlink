*+  TASK_GET - get a parameter value from a task
      SUBROUTINE TASK_GET (TASK_NAME, NAME, OUTVAL, STATUS )
*    Description :
*     Get the value of a parameter from a task as a character string.
*    Invocation :
*     CALL TASK_GET (TASK_NAME, NAME, OUTVAL, STATUS )
*    Parameters :
*     TASK_NAME=CHARACTER*(*) (given)
*           the name of the task
*     NAME=CHARACTER*(*) (given)
*           the name of the parameter
*     OUTVAL=CHARACTER*(*) (returned)
*           the returned parameter value
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

*    Export :
      CHARACTER*(*) OUTVAL      ! the returned parameter value

*    Status :
      INTEGER STATUS

*    Local variables :
      INTEGER PATH            ! path to task
      INTEGER MESSID          ! message identifier
      INTEGER MESLEN          ! length of INVAL
      CHARACTER*1 INVAL       ! value string sent
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      INVAL = ' '
      CALL FAMS_PATH ( TASK_NAME, PATH, STATUS )
      MESLEN = MIN( LEN(INVAL), MESSYS__VAL_LEN )
      CALL FAMS_SEND( PATH, MESSYS__MESSAGE, SAI__OK, GET, NAME,
     :  MESLEN, INVAL, MESSID, STATUS )
      CALL TASK_DONE ( MESSYS__INFINITE, PATH, MESSID, OUTVAL, STATUS )


      END

