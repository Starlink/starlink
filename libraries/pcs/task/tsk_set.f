*+  TASK_SET - set a parameter value in a task
      SUBROUTINE TASK_SET (TASK_NAME, NAME, INVAL, STATUS )
*    Description :
*     Set the value of a parameter in a task.
*    Invocation :
*     CALL TASK_SET (TASK_NAME, NAME, INVAL, STATUS )
*    Parameters :
*     TASK_NAME=CHARACTER*(*) (given)
*           the name of the task
*     NAME=CHARACTER*(*) (given)
*           the name of the parameter
*     INVAL=CHARACTER*(*) (returned)
*           the parameter value
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
*      4.10.1992:  add PAR_PAR for porting (RLVAD::AJC)
*     24.08.1993:  Use SUBPAR_SYS not PAR_PAR (RLVAD::AJC)
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

      CHARACTER*(*) INVAL       ! the supplied parameter value

*    Status :
      INTEGER STATUS

*    Local variables :
      INTEGER PATH            ! path to task
      INTEGER MESSID          ! message identifier
      INTEGER MESLEN          ! length of INVAL
      CHARACTER*(MESSYS__VAL_LEN) OUTVAL ! value string sent
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      OUTVAL = ' '
      CALL FAMS_PATH ( TASK_NAME, PATH, STATUS )
      MESLEN = MIN( LEN(INVAL), MESSYS__VAL_LEN )
      CALL FAMS_SEND( PATH, MESSYS__MESSAGE, SAI__OK, SET, NAME,
     :  MESLEN, INVAL, MESSID, STATUS )
      CALL TASK_DONE ( MESSYS__INFINITE, PATH, MESSID, OUTVAL, STATUS )


      END
