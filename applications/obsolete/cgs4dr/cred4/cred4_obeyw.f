*+  CRED4_OBEYW - Instruct a task to obey action and wait for completion
      SUBROUTINE CRED4_OBEYW( TASK_NAME, NAME, INVAL, STATUS )
*    Description :
*     This routine instructs the named task to obey an action,
*     wait for the action to complete, and then reports any
*     failure of that action.
*    Invocation :
*     CALL CRED4_OBEYW( TASK_NAME, NAME, INVAL, STATUS )
*    Parameters :
*     TASK_NAME   = CHARACTER*(*)( READ )
*           The name of the task to perform the action.
*     NAME        = CHARACTER*(*)( READ )
*           The name of the action to be executed.
*     INVAL       = CHARACTER*(*)( READ )
*           Character string value. This should contain an argument to
*           be supplied to the action. Any output is returned here.
*     STATUS   = INTEGER( UPDATE )
*           Global status.
*    Authors :
*     S M Beard  (UK.AC.ROE.STAR::SMB)
*     P N Daly (JACH.HAWAII.EDU::PND)
*    History :
*      9-Aug-1990: Original version.             (SMB)
*     11-Feb-1993: Conform to error strategy     (PND)
*     30-Aug-1994: Port to Unix (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DTASK_ERR'
*    Import :
      CHARACTER*(*)
     :  TASK_NAME,   ! Task name
     :  NAME,        ! Action name
     :  INVAL        ! Character string value
*    Status :
      INTEGER
     :  STATUS       ! Global status
*    Global variables :
      INCLUDE 'CRED4COM.INC'
*    Local variables :
      CHARACTER*( MSG_VAL_LEN ) OUTVAL
      INTEGER
     :  PATH,        ! Path to given task
     :  MESSID,      ! Message ID associated with action
     :  ERR_STATUS,  ! Temporary status for ERR_REP
     :  CHR_STATUS   ! Temporary status for ERR_REP
*-

*   Check for error on entry
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Initiate the action.
      CALL CHR_FILL( ' ', OUTVAL )
      CALL TASK_OBEY( TASK_NAME, NAME, INVAL, OUTVAL, PATH, MESSID, STATUS )

*   Check the action has started successfully.
      IF ( STATUS .EQ. DTASK__ACTSTART ) THEN

*      Reset the status
         CALL ERR_ANNUL( STATUS )

*      Wait indefinitely for completion of the action
         CALL TASK_DONE( -1, PATH, MESSID, OUTVAL, STATUS )

*      Check that the action has completed successfully.
         IF ( STATUS .EQ. DTASK__ACTCOMPLETE ) THEN

*         All is well - reset the status.
            CALL ERR_ANNUL( STATUS )
         ELSE

*          Report the error and return the value string
            CHR_STATUS = SAI__OK
            CALL CHR_COPY( OUTVAL, .FALSE., INVAL, CHR_STATUS )
            ERR_STATUS = STATUS
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'ES', ERR_STATUS )
            CALL MSG_SETC( 'TASK_NAME', TASK_NAME )
            CALL MSG_SETC( 'NAME', NAME )
            CALL ERR_REP( ' ', 'CRED4_OBEYW: '/
     :        /'Failure of ^NAME action '/
     :        /'reported by the ^TASK_NAME task '/
     :        /'(Status = ^ES, message follows)', STATUS )
            CALL MSG_SETC( 'OUTVAL', OUTVAL )
            CALL ERR_REP( ' ', 'CRED4_OBEYW: '/
     :        /'^OUTVAL', STATUS )
         ENDIF
      ELSE

*      Failed to start the action - report an error
         ERR_STATUS = STATUS
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'ES', ERR_STATUS )
         CALL MSG_SETC( 'TASK_NAME', TASK_NAME )
         CALL MSG_SETC( 'NAME', NAME )
         CALL ERR_REP( ' ', 'CRED4_OBEYW: Failed to start ^NAME '/
     :     /'action in ^TASK_NAME task (Status = ^ES)', STATUS )
      ENDIF

      END
