*+  CRED4_DO_REDUCE - Do a REDUCE command
      SUBROUTINE CRED4_DO_REDUCE( INVAL, STATUS )
*    Invocation :
*     CALL CRED4_DO_REDUCE( INVAL, STATUS )
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
*    Import-Export :
      CHARACTER*(*) INVAL
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'CRED4COM.INC'
*    External references :
      INTEGER CHR_LEN
*    Local variables :
      CHARACTER*12 OSTRUP, OSTRLO, ISTRUP, ISTRLO
      INTEGER ACTBYTES        ! Actual number of bytes
      LOGICAL PAUSE_ON_ERROR  ! T if data reduction should pause
*-

*    Check status on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Check the input
      REDUCTION_OK = .TRUE.

*    Clear the display for new groups
      IF ( NEWGROUP ) THEN
        CALL CRED4_OBEYW( P4_ALIAS, 'CLEAR_PORT', 'PORT=0', STATUS )
        CALL ERR_ANNUL( STATUS )
        NEWGROUP = .FALSE.
      ENDIF

*    Set the search strings
      OSTRUP = 'O' // CGS4_DATE(1:CHR_LEN(CGS4_DATE))
      OSTRLO = 'o' // CGS4_DATE(1:CHR_LEN(CGS4_DATE))
      ISTRUP = 'I' // CGS4_DATE(1:CHR_LEN(CGS4_DATE))
      ISTRLO = 'i' // CGS4_DATE(1:CHR_LEN(CGS4_DATE))

*    Reduce an observation
      IF ( INDEX( INVAL, OSTRUP(1:CHR_LEN(OSTRUP)) ).GT.0 .OR.
     :     INDEX( INVAL, OSTRLO(1:CHR_LEN(OSTRLO)) ).GT.0 ) THEN

        IF ( VERBOSE ) CALL MSG_OUT( ' ', 'CRED4_DO_REDUCE: Reducing an observation', STATUS )
        CALL CRED4_SEQ_OBS( INVAL, STATUS )

*      Check the PAUSE_REDUCTION flag in the noticeboard
        CALL NBS_GET_VALUE( PAUSE_REDUCTION_ID, 0, VAL__NBI, PAUSE_REDUCTION, ACTBYTES, STATUS )

        IF ( ( .NOT. REDUCTION_OK ) .AND. ( .NOT. PAUSE_REDUCTION ) ) THEN
          CALL PAR_GET0L( 'PAUSE_ON_ERROR', PAUSE_ON_ERROR, STATUS )
          PAUSE_REDUCTION = PAUSE_ON_ERROR
          CALL NBS_PUT_VALUE( PAUSE_REDUCTION_ID, 0, VAL__NBI, PAUSE_REDUCTION, STATUS )
        ENDIF

*      Stop the monitoring of the queue if the data reduction has been paused.
        IF ( PAUSE_REDUCTION ) THEN

          CALL MSG_OUT( ' ', '--- Data reduction paused after REDUCE obs command ---', STATUS )
          CALL NBS_PUT_VALUE( CRED4_BUSY_ID, 0, VAL__NBI, .FALSE., STATUS )
          CALL NBS_PUT_VALUE( RED4_BUSY_ID, 0, VAL__NBI, .FALSE., STATUS )
          CALL NBS_PUT_VALUE( P4_BUSY_ID, 0, VAL__NBI, .FALSE., STATUS )
        ENDIF

*    Reduce an integration
      ELSE IF ( INDEX(INVAL,ISTRUP(1:CHR_LEN(ISTRUP))).GT.0 .OR.
     :          INDEX(INVAL,ISTRLO(1:CHR_LEN(ISTRLO))).GT.0 ) THEN

        IF ( VERBOSE ) CALL MSG_OUT( ' ', 'CRED4_DO_REDUCE: Reducing an integration', STATUS )
        CALL CRED4_SEQ_INT( INVAL, STATUS )

*      Check the PAUSE_REDUCTION flag in the noticeboard
        CALL NBS_GET_VALUE( PAUSE_REDUCTION_ID, 0, VAL__NBI, PAUSE_REDUCTION, ACTBYTES, STATUS )

        IF ( ( .NOT. REDUCTION_OK ) .AND. ( .NOT. PAUSE_REDUCTION ) ) THEN
          CALL PAR_GET0L( 'PAUSE_ON_ERROR', PAUSE_ON_ERROR, STATUS )
          PAUSE_REDUCTION = PAUSE_ON_ERROR
          CALL NBS_PUT_VALUE( PAUSE_REDUCTION_ID, 0, VAL__NBI, PAUSE_REDUCTION, STATUS )
        ENDIF

*      Stop the monitoring of the queue if the data reduction has been paused.
        IF ( PAUSE_REDUCTION ) THEN

          CALL MSG_OUT( ' ', '--- Data reduction paused after REDUCE int command ---', STATUS )
          CALL NBS_PUT_VALUE( CRED4_BUSY_ID, 0, VAL__NBI, .FALSE., STATUS )
          CALL NBS_PUT_VALUE( RED4_BUSY_ID, 0, VAL__NBI, .FALSE., STATUS )
          CALL NBS_PUT_VALUE( P4_BUSY_ID, 0, VAL__NBI, .FALSE., STATUS )
        ENDIF
      ENDIF
      END
