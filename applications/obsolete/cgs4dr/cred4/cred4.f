*+  CRED4 - Main routine for the CRED4 I-task
      SUBROUTINE CRED4( STATUS )
*    Description :
*     This is the main routine for the CGS4 data reduction control I-task.
*    Invocation :
*     CALL CRED4( STATUS )
*    Authors :
*     P.N.Daly (JACH::PND)
*    History :
*     30-Aug-1994: Re-written for Unix port (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ACT_ERR'
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'CRED4COM.INC'
*    Local variables :
      CHARACTER*( PAR__SZNAM ) NAME   ! Name of action
      INTEGER CONTEXT                 ! Context ( OBEY or CANCEL )
*-

*   Return immediately if status bad.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Obtain the context, name of the action, sequence number
      CALL TASK_GET_CONTEXT( CONTEXT, STATUS )
      CALL TASK_GET_NAME( NAME, STATUS )

*   Act according to CONTEXT.
      IF ( CONTEXT .EQ. OBEY ) THEN

*     Close the noticeboard and save its contents to the noticeboard file.
        IF ( NAME .EQ. 'CLOSE_NB' ) THEN
           CALL CRED4_CLOSE_NB( STATUS )

*     Close the data reduction queue file.
        ELSE IF ( NAME .EQ. 'CLOSE_QFILE' ) THEN
           CALL CRED4_CLOSE_QFILE( STATUS )

*     Init the data reduction control task
        ELSE IF ( NAME .EQ. 'INIT' ) THEN
           CALL CRED4_INIT( STATUS )

*     Re-init the data reduction control task
        ELSE IF ( NAME .EQ. 'INIT_SYS' ) THEN
           CALL CRED4_INIT_SYS( STATUS )

*     List data reduction parameters contained in noticeboard
        ELSE IF ( NAME .EQ. 'LIST_NB' ) THEN
           CALL CRED4_LIST_NB( STATUS )

*     Open the noticeboard, restoring it from the noticeboard file
        ELSE IF ( NAME .EQ. 'OPEN_NB' ) THEN
           CALL CRED4_OPEN_NB( STATUS )

*     Open the data reduction queue file.
        ELSE IF ( NAME .EQ. 'OPEN_QFILE' ) THEN
           CALL CRED4_OPEN_QFILE( STATUS )

*     Main reduce action
        ELSE IF ( NAME .EQ. 'REDUCE' ) THEN
           CALL CRED4_REDUCE( STATUS )

*     Reset the error status
        ELSE IF ( NAME .EQ. 'RESET' ) THEN
           CALL ERR_ANNUL( STATUS )
           CALL MSG_OUT( ' ', 'Reset CRED4 task and annulled status OK', STATUS )

*     Restore the data reduction config from a file to NBS, PAR and memory
        ELSE IF ( NAME .EQ. 'RESTORE_CONFIG' ) THEN
           CALL CRED4_RESTORE_CONFIG( STATUS )

*     Save the current data reduction configuration to a file,
        ELSE IF ( NAME .EQ. 'SAVE_CONFIG' ) THEN
           CALL CRED4_SAVE_CONFIG( STATUS )

*     Set the VERBOSE flag from a parameter.
        ELSE IF ( NAME .EQ. 'SET_VERBOSE' ) THEN
           CALL PAR_GET0L( 'VERBOSE', VERBOSE, STATUS )
           IF ( VERBOSE ) THEN
              CALL MSG_OUT( ' ', 'Verbose messages switched ON in CRED4 task', STATUS )
           ELSE
              CALL MSG_OUT( ' ', 'Verbose messages switched OFF in CRED4 task', STATUS )
           END IF

*     Quick status check for CRED4 task
        ELSE IF ( NAME .EQ. 'STATUS' ) THEN
           CALL MSG_OUT  ( ' ', 'CRED4 Task : Portable-CGS4DR CRED4 VPKG_VERS', STATUS )
           CALL MSG_OUT  ( ' ', 'CRED4 Task : The uncached CRED4 task is OK', STATUS )
        ELSE

*        The NAME of the action was not recognised.
           STATUS = SAI__ERROR
           CALL ERR_REP( ' ', 'CRED4: Action not recognised', STATUS )
        ENDIF
      ELSE IF ( CONTEXT .EQ. CANCEL ) THEN

*       Cancel the main data reduction action.
         IF ( NAME .EQ. 'REDUCE' ) THEN
            CALL TASK_PUT_REQUEST( ACT__CANCEL, STATUS )
            PAUSE_REDUCTION = .FALSE.
            CALL NBS_PUT_VALUE( REDUCING_ID, 0, VAL__NBI, .FALSE., STATUS )
         ELSE

*         The specified action cannot be cancelled.
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'CRED4: Action cannot be cancelled', STATUS )
         ENDIF
      ELSE

*      The CONTEXT was not recognised (must be OBEY or CANCEL)
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'Invalid context - must be OBEY or CANCEL', STATUS )
      ENDIF

      END
