*+  CRED4_DO_ENDGROUP - DO an ENDGROUP command
      SUBROUTINE CRED4_DO_ENDGROUP( INVAL, STATUS )
*    Invocation :
*     CALL CRED4_DO_ENDGROUP( INVAL, STATUS )
*    Authors :
*     P.N.Daly  (JACH::PND)
*    History :
*      9-Sep-94: Original Unix version (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
*    Status :
      INTEGER STATUS
*    Import :
      CHARACTER*(*) INVAL
*    Global variables :
      INCLUDE 'CRED4COM.INC'
*    Local variables :
      INTEGER ACTBYTES              ! Actual number of bytes returned by NBS
      LOGICAL PAUSE_ON_ERROR        ! T if data reduction should pause
*-

*    Check status on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Finish processing
      CALL CRED4_END_GRP( INVAL, STATUS )

*    Check the PAUSE_REDUCTION flag in the noticeboard
      CALL NBS_GET_VALUE( PAUSE_REDUCTION_ID, 0, VAL__NBI, PAUSE_REDUCTION, ACTBYTES, STATUS )

*    If the data reduction has failed, and data reduction is not about to
*    pause anyway, give the user the option of pausing the rest of the DR.
      IF ( ( .NOT. REDUCTION_OK ) .AND. ( .NOT. PAUSE_REDUCTION ) ) THEN

        CALL PAR_GET0L( 'PAUSE_ON_ERROR', PAUSE_ON_ERROR, STATUS )
        PAUSE_REDUCTION = PAUSE_ON_ERROR
        CALL NBS_PUT_VALUE( PAUSE_REDUCTION_ID, 0, VAL__NBI, PAUSE_REDUCTION, STATUS )
      ENDIF

*    Stop the monitoring of the queue if the data reduction has been paused.
      IF ( PAUSE_REDUCTION ) THEN

         CALL MSG_OUT( ' ', '--- Data reduction paused after ENDGROUP ---', STATUS )
         CALL NBS_PUT_VALUE( CRED4_BUSY_ID, 0, VAL__NBI, .FALSE., STATUS )
         CALL NBS_PUT_VALUE( RED4_BUSY_ID, 0, VAL__NBI, .FALSE., STATUS )
         CALL NBS_PUT_VALUE( P4_BUSY_ID, 0, VAL__NBI, .FALSE., STATUS )
      END IF

*    Assume that a new group will now begin
      GROUP_AVAILABLE = .FALSE.
      NEWGROUP = .TRUE.
      END
