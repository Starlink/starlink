*+  TASK_DONE - wait for final acknowledgement from task
      SUBROUTINE TASK_DONE ( TIMEOUT, PATH, MESSID, OUTVAL, STATUS )
*    Description :
*     Wait for a final acknowledgement from a task executing a 
*     GET/SET/OBEY/CANCEL.
*     The routine will return when the required message arrives, or if 
*     it times-out, or if there is an EXTINT event. Requests from the 
*     task for parameter prompts or output of messages associated with 
*     the action are automatically forwarded to the user interface. 
*     Parameter values sent by the user interface are forwarded to the 
*     task.
*    Invocation :
*     CALL TASK_DONE ( TIMEOUT, PATH, MESSID, OUTVAL, STATUS )
*    Parameters :
*     TIMEOUT=INTEGER (given)
*           timeout in millisecs. -1 gives infinite timeout.
*     PATH=INTEGER (given)
*           path to the task
*     MESSID=INTEGER (given)
*           messid for the action.
*     OUTVAL=CHARACTER*(*) (returned)
*           The value string from the task 
*     STATUS=INTEGER
*    Method :
*     Get replies from the task, handling parameter requests until the 
*     final completion message is received.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     05.11.1987:  Original (REVAD::BDK)
*     16.11.1987:  use SUBPAR_WRITE to forward messages (REVAD::BDK)
*     18.04.1989:  return VALUE on success (REVAD::BDK)
*     06.05.1991:  revise INCLUDE files (REVAD::BDK)
*     08.08.1991:  handle GSOC, not just OBEY (REVAD::BDK)
*     25.11.1991:  use ADAM_ACKNOW for SYNC replies (REVAD::BDK)
*      4.10.1992:  add PAR_PAR for porting (RLVAD::AJC)
*     24.08.1993:  Use SUBPAR_SYS not PAR_PAR 
*                  SUBPAR__NAMELEN not PAR__SZNAM (RLVAD::AJC)
*     11.06.2001:  call AMS (FAMS) directly (AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'MESSYS_PAR'
      INCLUDE 'MESSYS_ERR'
      INCLUDE 'DTASK_ERR'
      INCLUDE 'MESSYS_LEN'

*    Import :
      INTEGER TIMEOUT           ! timeout in millisecs. 
                                ! -1 gives infinite timeout.

      INTEGER PATH              ! path to the task

      INTEGER MESSID            ! messid for the action.

*    Export :
      CHARACTER*(*) OUTVAL      ! The value string from the task 

*    Status :
      INTEGER STATUS

*    Local variables :
      INTEGER MSGSTATUS              ! message status returned from task
      INTEGER MESLEN                 ! length of INTVAL
      CHARACTER*(MESSYS__VAL_LEN) INTVAL ! message returned from task
      INTEGER CONTEXT                ! context returned from task
      LOGICAL FINISHED               ! loop controller
      CHARACTER*(SUBPAR__NAMELEN) REPLACT ! action name returned by GETREPLY
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

*
*    Loop picking up parameter requests until a completion status 
*    is returned from the task.
*
      FINISHED = .FALSE.

      DO WHILE ( .NOT. FINISHED )

         CALL FAMS_GETREPLY( TIMEOUT, PATH, MESSID, MSGSTATUS, CONTEXT,
     :     REPLACT, MESLEN, INTVAL, STATUS )

         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( MESLEN .LT. MESSYS__VAL_LEN )
     :        INTVAL(MAX(1,MESLEN+1):) = ' '

            IF ( MSGSTATUS .EQ. MESSYS__PARAMREQ ) THEN
               CALL TASK_ASKPARAM (
     :           PATH, INTVAL, MESSID, STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  FINISHED = .TRUE.
               ENDIF
            ELSE IF ( MSGSTATUS .EQ. MESSYS__INFORM ) THEN
               CALL SUBPAR_WRITE ( INTVAL, STATUS )
               INTVAL = ' '
            ELSE IF ( MSGSTATUS .EQ. MESSYS__SYNC ) THEN
               CALL SUBPAR_SYNC ( STATUS )
               CALL FAMS_REPLY( PATH, MESSID, MESSYS__MESSAGE,
     :           MESSYS__SYNCREP, CONTEXT, REPLACT, 1, ' ', STATUS )
            ELSE
               FINISHED = .TRUE.
               OUTVAL = INTVAL
               STATUS = MSGSTATUS
            END IF

         ELSE
            FINISHED = .TRUE.
            OUTVAL = ' '

         ENDIF

      ENDDO

      END
