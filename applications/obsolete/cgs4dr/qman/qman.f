*+  QMAN - Main routine for the QMAN I-task
      SUBROUTINE QMAN( STATUS )
*    Description :
*     This is the main routine for the Queue Manager task
*    Invocation :
*     CALL QMAN( STATUS )
*    Authors :
*     P. N. Daly (PND@JACH.HAWAII.EDU)
*    History :
*     27-May-1994: Original version                                   (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'               ! Defines SAI__OK and others
      INCLUDE 'PAR_PAR'               ! Defines SAI__OK and others
      INCLUDE 'ADAMDEFNS'             ! Defines GET, SET, OBEY or CANCEL
      INCLUDE 'ACT_ERR'               ! Defines ACT__something messages
      INCLUDE 'MESSYS_LEN'                 ! Defines MSG_VAL_LEN etc
*    Status :
      INTEGER STATUS                  ! Inherited global ADAM status
*    Global variables :
      INCLUDE 'QMAN_GLOBAL.PAR'       ! QMAN global parameter constants
      INCLUDE 'QMAN_COMMON.BLK'       ! QMAN common block
*    Local variables :
      CHARACTER*( PAR__SZNAM ) NAME   ! The name of action
      INTEGER CONTEXT                 ! The context
*-

*   Return immediately if status bad.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Act according to context
      CALL TASK_GET_CONTEXT( CONTEXT, STATUS )
      IF ( CONTEXT .EQ. OBEY ) THEN

*     Act on action name, first option is to delete a record
        CALL TASK_GET_NAME( NAME, STATUS )

*     Initialise the task
        IF ( NAME .EQ. 'INIT' ) THEN
          CALL QMAN_INIT( STATUS )

*     Delete a record
        ELSE IF ( NAME .EQ. 'DELETE' ) THEN
          CALL QMAN_DELETE( STATUS )

*     Read a record
        ELSE IF ( NAME .EQ. 'READ' ) THEN
          CALL QMAN_READ( STATUS )

*     Lock the database
        ELSE IF ( NAME .EQ. 'LOCK' ) THEN
          CALL QMAN_LOCK( STATUS )

*     Unlock the database
        ELSE IF ( NAME .EQ. 'UNLOCK' ) THEN
          CALL QMAN_UNLOCK( STATUS )

*     Order the queue if possible
        ELSE IF ( NAME .EQ. 'ORDER' ) THEN
          CALL QMAN_ORDER( STATUS )

*     Read an ASCII file into database
        ELSE IF ( NAME .EQ. 'RESTORE' ) THEN
          CALL QMAN_RESTORE( STATUS )

*     Write an ASCII file from database
        ELSE IF ( NAME .EQ. 'SAVE' ) THEN
          CALL QMAN_SAVE( STATUS )

*     Sorts the database into ascending or descending order
        ELSE IF ( NAME .EQ. 'SORT' ) THEN
          CALL QMAN_SORT( STATUS )

*     Write a record
        ELSE IF ( NAME .EQ. 'WRITE' ) THEN
          CALL QMAN_WRITE( STATUS )

*     List the commands in the queue
        ELSE IF ( NAME .EQ. 'LIST' ) THEN
          CALL QMAN_LIST( STATUS )

*     Reset the task by flushing the error status
        ELSE IF ( NAME .EQ. 'RESET' ) THEN
          CALL ERR_ANNUL( STATUS )

*     Status check to see if task is alive
        ELSE IF ( NAME .EQ. 'STATUS' ) THEN
          CALL MSG_BLANK( STATUS )
          CALL MSG_OUT  ( ' ', 'QMAN Task  : Portable-CGS4DR QMAN VPKG_VERS', STATUS )
          CALL MSG_OUT  ( ' ', 'QMAN Task  : The uncached QMAN task is OK', STATUS )
          CALL MSG_BLANK( STATUS )
          IF ( INITIALISED .AND. VERBOSE ) THEN
            CALL MSG_SETC( 'IS', INIT_USER )
            CALL MSG_OUT( ' ', '^IS ', STATUS )
          ENDIF

*     Turn verbose messages on
        ELSE IF ( NAME .EQ. 'VERBOSE' ) THEN
          VERBOSE = .TRUE.
          CALL MSG_OUT( ' ', 'QMAN: Verbose mode enabled', STATUS )

*     Turn verbose messages off
        ELSE IF ( NAME .EQ. 'NOVERBOSE' ) THEN
          VERBOSE = .FALSE.
          CALL MSG_OUT( ' ', 'QMAN: Verbose mode disabled', STATUS )

*     Action not recognised so report error
        ELSE
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ' ,  'QMAN: Action not recognised', STATUS )
        ENDIF

*   The context was CANCEL so abort the action
      ELSE IF ( CONTEXT .EQ. CANCEL ) THEN
        CALL TASK_PUT_REQUEST( ACT__CANCEL, STATUS )

*   The CONTEXT was not recognised
      ELSE
         STATUS = SAI__ERROR
         CALL ERR_REP(' ', 'QMAN: '/
     :      /'Invalid context (neither OBEY nor CANCEL)', STATUS)
      ENDIF

*    Exit subroutine
      END
