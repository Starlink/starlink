*+  CRED4_DO_END - Do an END command
      SUBROUTINE CRED4_DO_END( INVAL, STATUS )
*    Invocation :
*     CALL CRED4_DO_END( INVAL, STATUS )
*    Authors :
*     P.N.Daly  (JACH::PND)
*    History :
*      9-Sep-1994: Original Unix version (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
*    Import :
      CHARACTER*(*) INVAL
*    Status :
      INTEGER STATUS
*    External references :
      INTEGER CHR_LEN               ! Character handling routine
*    Global variables :
      INCLUDE 'CRED4COM.INC'
*    Local variables :
      INTEGER ACTBYTES              ! Actual number of bytes returned by NBS
      LOGICAL PAUSE_ON_ERROR        ! T if data reduction should pause
      CHARACTER*64
     :  ROBS,            ! The name of the reduced observation
     :  ROBS_NEW         ! The name of the new reduced observation
      LOGICAL ASK        ! Flag to control prompting for reduction step
      LOGICAL WAIT       ! Flag to control waiting for reduction step to finish
*-

*    Check status on entry.
      IF (STATUS .NE. SAI__OK) RETURN

*    Check file_obs
      IF ( FILE_OBS .EQ. 'ASK' ) THEN
        ASK = .TRUE.
      ELSE
        ASK = .FALSE.
      ENDIF

      WAIT = .TRUE.
      IF ( FILE_OBS .NE. 'NO' ) THEN
        CALL CRED4_FILE_OBSERVATION( INVAL, ASK, WAIT, STATUS )
      ELSE
        CALL MSG_SETC( 'OBS', INVAL )
        CALL MSG_OUT( ' ', 'Observation ^OBS has *NOT* been '/
     :    /'filed !', STATUS )
      ENDIF

*    Check archive_obs
      IF ( ARCHIVE_OBS .EQ. 'ASK' ) THEN
        ASK = .TRUE.
      ELSE
        ASK = .FALSE.
      ENDIF

      WAIT = .TRUE.

      IF ( ARCHIVE_OBS .NE. 'NO' ) THEN
        CALL CRED4_ARCHIVE_OBSERVATION( INVAL, WAIT, STATUS )
      ENDIF

*    Check add_obs
      IF ( ADD_OBS .EQ. 'ASK' ) THEN
        ASK = .TRUE.
      ELSE
        ASK = .FALSE.
      ENDIF

      WAIT = .TRUE.
      POLYFITTED = .FALSE.
      IF ( ADD_OBS .NE. 'NO' ) THEN

        IF ( ( .NOT.ADD_IN_PAIRS ) .AND.
     :       ( PF_POLYFIT.EQ.'OBJ-SKY' ) .AND.
     :       ( INVAL(1:1).EQ.'O' ) ) THEN

          CALL CRED4_OBSTOROBS( INVAL, ROBS, STATUS )
          ROBS_NEW = ROBS(1:CHR_LEN(ROBS)) // '_pf'
          CALL MSG_OUT( ' ', 'Enhanced sky subtraction '/
     :      /'selected to put single OBJECT files into '/
     :      /'reduced groups', STATUS )
          CALL MSG_SETC( 'ROBS', ROBS )
          CALL MSG_OUT( ' ', 'Original reduced observation is ^ROBS', STATUS )
          CALL MSG_SETC( 'ROBSNEW', ROBS_NEW )
          CALL MSG_OUT( ' ', 'New reduced observation is ^ROBSNEW', STATUS )

          ASK  = .FALSE.
          WAIT = .TRUE.
          CALL CRED4_POLYFIT( ROBS, ROBS_NEW, ASK, WAIT, POLYFITTED, STATUS )
        ENDIF

*      Add the observation
        CALL CRED4_ADD_OBSERVATION( INVAL, ASK, WAIT, STATUS )
        IF ( STATUS .EQ. SAI__OK ) REDUCTION_OK = .TRUE.

*      If required, display the partially-built group
        IF ( GROUP_AVAILABLE ) CALL CRED4_SHOW_GRP( INVAL, STATUS )
      ELSE

        IF ( ( PF_POLYFIT .EQ. 'OBJECT' ) .AND.
     :       ( INVAL(1:1) .EQ. 'O' ) ) THEN

          CALL CRED4_OBSTOROBS( INVAL, ROBS, STATUS )
          ROBS_NEW = ROBS(1:CHR_LEN(ROBS)) // '_pf'
          CALL MSG_OUT( ' ',  'Enhanced sky subtraction selected '/
     :       /'for single OBJECT files ', STATUS )
          CALL MSG_SETC( 'ROBS', ROBS )
          CALL MSG_OUT( ' ', 'Original educed observation is ^ROBS', STATUS )
          CALL MSG_SETC( 'ROBSNEW', ROBS_NEW )
          CALL MSG_OUT( ' ', 'New reduced observation is ^ROBSNEW', STATUS )

          ASK  = .FALSE.
          WAIT = .TRUE.
          CALL CRED4_POLYFIT( ROBS, ROBS_NEW, ASK, WAIT, POLYFITTED, STATUS )
          IF ( .NOT. POLYFITTED ) REDUCTION_OK = .FALSE.
        ENDIF
      ENDIF

*    Check the PAUSE_REDUCTION flag in the noticeboard
      CALL NBS_GET_VALUE( PAUSE_REDUCTION_ID, 0, VAL__NBI, PAUSE_REDUCTION, ACTBYTES, STATUS )

*    If the data reduction has failed, and data reduction is not about to
*    pause anyway, give the user the option of pausing the rest of the DR
      IF ( ( .NOT. REDUCTION_OK ) .AND. ( .NOT. PAUSE_REDUCTION ) ) THEN

        CALL PAR_GET0L( 'PAUSE_ON_ERROR', PAUSE_ON_ERROR, STATUS )
        PAUSE_REDUCTION = PAUSE_ON_ERROR
        CALL NBS_PUT_VALUE( PAUSE_REDUCTION_ID, 0, VAL__NBI, PAUSE_REDUCTION, STATUS )
      ENDIF

*    Stop the monitoring of the queue if the data reduction has been paused.
      IF ( PAUSE_REDUCTION ) THEN

        CALL MSG_OUT( ' ', '--- Data reduction paused after END command ---', STATUS )
        CALL NBS_PUT_VALUE( CRED4_BUSY_ID, 0, VAL__NBI, .FALSE., STATUS )
        CALL NBS_PUT_VALUE( RED4_BUSY_ID, 0, VAL__NBI, .FALSE., STATUS )
        CALL NBS_PUT_VALUE( P4_BUSY_ID, 0, VAL__NBI, .FALSE., STATUS )
      ENDIF

*    A new observation will begin, so set some flags
      INTEGRATION_AVAILABLE = .FALSE.
      OBSERVATION_AVAILABLE = .FALSE.
      END
