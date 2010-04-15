*+  CRED4_REDUCE - Monitor data reduction queue file for commands
      SUBROUTINE CRED4_REDUCE( STATUS )
*    Description :
*     This routine monitors the data reduction queue file, reads the
*     instructions in chronological order and interprets them.
*    Invocation :
*     CALL CRED4_REDUCE( STATUS )
*    Authors :
*     P.N.Daly  (JACH::PND)
*    History :
*     30-Aug-1994: Original Unix version (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'ACT_ERR'
      INCLUDE 'DTASK_ERR'
*    Status :
      INTEGER STATUS
*    External references :
      INTEGER CHR_LEN
*    Global variables :
      INCLUDE 'CRED4COM.INC'
*    Local variables :
      INTEGER ACTBYTES
      CHARACTER*( MSG_VAL_LEN ) OUTVAL
      CHARACTER*( MSG_VAL_LEN ) INVAL
*-

*    Check status on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Only read the queue if reducetion is not paused
      CALL NBS_GET_VALUE( PAUSE_REDUCTION_ID, 0, VAL__NBI, PAUSE_REDUCTION, ACTBYTES, STATUS )
      IF ( .NOT. PAUSE_REDUCTION ) THEN

*      Generate the read parameter string
        CALL CHR_FILL( ' ', OUTVAL )

*      Read the oldest command from QMAN
        INVAL = 'READ_MODE="NEWEST" DESTRUCTIVE=TRUE '/
     :    /'PASSWORD="'//QMAN_PWRD(1:CHR_LEN(QMAN_PWRD))//'" '/
     :    /'LOCKWORD="'//QMAN_LWRD(1:CHR_LEN(QMAN_LWRD))//'"'
        CALL TASK_OBEY( QMAN_ALIAS(1:CHR_LEN(QMAN_ALIAS)), 'READ',
     :    INVAL(1:CHR_LEN(INVAL)), OUTVAL, QMAN_PATH,
     :    QMAN_MESSID, STATUS )
        IF ( STATUS .NE. DTASK__ACTSTART ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'CRED4_REDUCE: '/
     :      /'Failed to start READ action in QMAN', STATUS )
        ELSE
          IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Reading from QMAN', STATUS )
        ENDIF

*      Wait (indefinitely) for task to complete
        CALL ERR_ANNUL( STATUS )
        CALL TASK_DONE( -1, QMAN_PATH, QMAN_MESSID, OUTVAL, STATUS )
        IF ( ( STATUS .NE. DTASK__ACTCOMPLETE ) .AND.
     :       ( STATUS .NE. DTASK__ACTINFORM ) ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'CRED4_REDUCE: '/
     :      /'Failed to complete READ action', STATUS )
        ENDIF
        CALL ERR_ANNUL( STATUS )

*      Issue the returned string
        IF ( VERBOSE ) THEN
          CALL MSG_SETC( 'OUT', OUTVAL )
          CALL MSG_OUT( ' ', 'QMAN returned ^OUT', STATUS )
        ENDIF

*      Pass the string to the reduction system
        CALL CRED4_REDUCE_2( OUTVAL, STATUS )

*      Re-schedule (have to flush errors to resort to user and reset status)
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_FLUSH( STATUS )
          CALL PAR_GET0L( 'PAUSE_ON_ERROR', PAUSE_REDUCTION, STATUS )
          CALL NBS_PUT_VALUE( PAUSE_REDUCTION_ID, 0, VAL__NBI, PAUSE_REDUCTION, STATUS )
        ENDIF
      ELSE

*    Report message
        IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Data reduction is paused', STATUS )
      ENDIF

      CALL TASK_PUT_DELAY( RETVAL, STATUS )
      CALL TASK_PUT_REQUEST( ACT__WAIT, STATUS )
      END
