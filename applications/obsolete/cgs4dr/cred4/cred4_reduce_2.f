*+  CRED4_REDUCE_2 - Interprets the incoming DR command
      SUBROUTINE CRED4_REDUCE_2( INVAL, STATUS )
*    Invocation :
*     CALL CRED4_REDUCE_2( INVAL, STATUS )
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
*    Import :
      CHARACTER*(*) INVAL
*    Status :
      INTEGER STATUS
*    External references :
      INTEGER CHR_LEN
*    Global variables :
      INCLUDE 'CRED4COM.INC'        ! CRED4 common block
*    Local Constants :
      INTEGER MAXWRD                ! Maximum no. words allowed in STRING
      PARAMETER ( MAXWRD = 20 )
*    Local variables :
      INTEGER CLEN, I
      INTEGER DUMMY_STATUS
      INTEGER START( MAXWRD )
      INTEGER STOP( MAXWRD )
      INTEGER NUMWRD
      CHARACTER*( STRING_SIZE ) WORDS( MAXWRD )
      CHARACTER*(NBS_CLEN) CDUMMY
*-

*    Check status on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Clean the input string
      CALL CHR_CLEAN( INVAL )

*    Set the "reducing" flag in the noticeboard.
      CALL NBS_PUT_VALUE( REDUCING_ID, 0, VAL__NBI, .TRUE., STATUS )

*    Check that a data reduction sequence has been set up
      IF ( SEQUENCE_SETUP ) THEN

*      Set some flags
        CALL NBS_PUT_VALUE( CRED4_BUSY_ID, 0, VAL__NBI, .TRUE., STATUS )
        CALL NBS_PUT_VALUE( RED4_BUSY_ID, 0, VAL__NBI, .TRUE., STATUS )

*      Obtain the latest data reduction parameters from the noticeboard
        CALL CRED4_READ_NB( STATUS )

*      Initialise the word arrays, then split the string into its constituents
        DO I = 1, MAXWRD, 1
          CALL CHR_FILL( ' ', WORDS(I) )
        ENDDO
        CALL CHR_DCWRD( INVAL, MAXWRD, NUMWRD, START, STOP, WORDS, STATUS )
        CALL CHR_UCASE( WORDS(1) )

        IF ( VERBOSE ) THEN
          CALL MSG_SETC( 'W1', WORDS(1) )
          CALL MSG_OUT( ' ', 'Executing ^W1 command', STATUS )
        ENDIF

*      Do the DRCOMMENT command
        IF ( WORDS(1) .EQ. 'DRCOMMENT' )  THEN
          INVAL = INVAL(10:LEN(INVAL))
          CALL CHR_CLEAN( INVAL )
          CALL MSG_SETC( 'INVAL', INVAL )
          CALL MSG_OUT( ' ', '^INVAL', STATUS )

*      Do the DRCONFIG command
        ELSE IF ( WORDS(1) .EQ. 'DRCONFIG' ) THEN
          IF ( INDEX( WORDS(2), SEPARATOR ) .EQ. 0 )
     :      WORDS(2) = CGS4_CONFIG(1:CHR_LEN(CGS4_CONFIG)) // WORDS(2)(1:CHR_LEN(WORDS(2)))
          CALL CHR_RMBLK( WORDS(2) )
          CALL MSG_SETC( 'CONFIG_FILE', WORDS(2) )
          CALL MSG_OUT(' ', 'Changing data reduction config to ^CONFIG_FILE', STATUS )
          CALL PAR_PUT0C( 'CONFIG_FILE', WORDS(2), STATUS )
          CALL CRED4_RESTORE_CONFIG( STATUS )
          CALL CRED4_RESTORE_PCONFIG( STATUS )

*      Do the DRPAUSE command
        ELSE IF ( WORDS(1) .EQ. 'DRPAUSE' ) THEN
          PAUSE_REDUCTION = .TRUE.
          CALL NBS_PUT_VALUE( PAUSE_REDUCTION_ID, 0, VAL__NBI, PAUSE_REDUCTION, STATUS )
          CALL MSG_OUT( ' ', '--- Data reduction paused ---', STATUS )
          INVAL = INVAL(8:LEN(INVAL))
          CALL CHR_CLEAN( INVAL )
          CALL MSG_SETC( 'INVAL', INVAL )
          CALL MSG_OUT( ' ', 'Reason: ^INVAL', STATUS )

*      Do the DRCONTINUE command
        ELSE IF ( WORDS(1) .EQ. 'DRCONTINUE' ) THEN
          PAUSE_REDUCTION = .FALSE.
          CALL NBS_PUT_VALUE( PAUSE_REDUCTION_ID, 0, VAL__NBI, PAUSE_REDUCTION, STATUS )
          CALL MSG_OUT( ' ', '--- Data reduction resumed ---', STATUS )
          INVAL = INVAL(11:LEN(INVAL))
          CALL CHR_CLEAN( INVAL )
          CALL MSG_SETC( 'INVAL', INVAL )
          CALL MSG_OUT( ' ', 'Reason: ^INVAL', STATUS )

*      Do the DRMASK command
        ELSE IF ( WORDS(1) .EQ. 'DRMASK' ) THEN
          CALL CHR_FILL( ' ', CDUMMY )
          CALL NBS_PUT_CVALUE( MASK_ID, 0, CDUMMY(1:LEN(CDUMMY)), STATUS )
          IF ( INDEX( WORDS(2), SEPARATOR ) .EQ. 0 )
     :      WORDS(2) = PREFIX // 'CGS4_MASKS' // SEPARATOR // WORDS(2)(1:CHR_LEN(WORDS(2)))
          CALL CHR_RMBLK( WORDS(2) )
          CLEN = CHR_LEN( WORDS(2) )
          CALL NBS_PUT_CVALUE( MASK_ID, 0, WORDS(2)(1:CLEN), STATUS )
          CALL PAR_PUT0C( 'MASK', WORDS(2), STATUS )
          CALL MSG_SETC( 'MASK', WORDS(2) )
          CALL MSG_OUT( ' ', 'Bad pixel mask changed to ^MASK', STATUS )

*     Do the DRSKYWT command
        ELSE IF ( WORDS(1) .EQ. 'DRSKYWT' ) THEN
          CALL CHR_CTOR( WORDS(2), SKY_WT, STATUS )
          IF ( STATUS .EQ. SAI__OK ) THEN
            CALL NBS_PUT_VALUE( SKY_WT_ID, 0, VAL__NBR, SKY_WT, STATUS )
            CALL PAR_PUT0R( 'SKY_WT', SKY_WT, STATUS )
            CALL MSG_SETR( 'SKY_WT', SKY_WT )
            CALL MSG_OUT( ' ', 'Sky weighting factor changed to '/
     :         /'^SKY_WT', STATUS )
          ELSE
            CALL MSG_SETC( 'WORD2', WORDS(2) )
            CALL MSG_OUT( ' ', 'Invalid sky weight (^WORD2)', STATUS )
          ENDIF

*      Do the DRVARWT command
        ELSE IF ( WORDS(1) .EQ. 'DRVARWT' ) THEN
          CALL CHR_CTOL( WORDS(2), VARIANCE_WT, STATUS )
          IF ( STATUS .EQ. SAI__OK ) THEN
            CALL NBS_PUT_VALUE( VARIANCE_WT_ID, 0, VAL__NBI, VARIANCE_WT, STATUS )
            CALL PAR_PUT0L( 'VARIANCE_WT', VARIANCE_WT, STATUS )
            IF ( VARIANCE_WT ) THEN
              CALL MSG_OUT( ' ', 'Variance weighting ENABLED', STATUS )
            ELSE
              CALL MSG_OUT( ' ', 'Variance weighting DISABLED', STATUS )
            ENDIF
          ELSE
            CALL MSG_SETC( 'WORD2', WORDS(2) )
            CALL MSG_OUT( ' ', 'Invalid variance weighting flag '/
     :        /'(^WORD2)', STATUS )
          ENDIF

*     Do the DISPLAY command (just pass the whole INVAL string through)
        ELSE IF ( WORDS(1) .EQ. 'DISPLAY' ) THEN
          CALL CRED4_DISPLAY( INVAL(1:CHR_LEN(INVAL)), 'file', .FALSE., .TRUE., STATUS )

*      Do the END command
        ELSE IF ( WORDS(1) .EQ. 'END' ) THEN
          CALL CRED4_CHECK_INPUT( WORDS(2), STATUS )
          CALL CRED4_DO_END( WORDS(2), STATUS )

*      Do the ENDGROUP command
        ELSE IF ( WORDS(1) .EQ. 'ENDGROUP' ) THEN
          CALL CRED4_CHECK_INPUT( WORDS(2), STATUS )
          CALL CRED4_DO_ENDGROUP( WORDS(2), STATUS )

*      Do the REDUCE command
        ELSE IF ( WORDS(1) .EQ. 'REDUCE' ) THEN
           CALL CRED4_CHECK_INPUT( WORDS(2), STATUS )
           CALL CRED4_CLOBBER( WORDS(2), STATUS )
           CALL CRED4_DO_REDUCE( WORDS(2), STATUS )
        ELSE

*      Unknown string, report it in verbose mode
          IF ( VERBOSE ) THEN
            CALL MSG_SETC( 'W1', WORDS(1) )
            CALL MSG_SETC( 'W2', WORDS(2) )
            CALL MSG_OUT( ' ',
     :        'Command not known (^W1 ^W2)', STATUS )
          ENDIF
        ENDIF

*      Wait for pending P4 or RED4 actions in progress to finish
        CALL CRED4_WAIT (STATUS)

*      Ensure that the P4_BUSY flag is now unset (using a dummy status).
        DUMMY_STATUS = SAI__OK
        CALL NBS_PUT_VALUE( P4_BUSY_ID, 0, VAL__NBI, .FALSE., DUMMY_STATUS )
        DUMMY_STATUS = SAI__OK
        CALL NBS_PUT_VALUE( RED4_BUSY_ID, 0, VAL__NBI, .FALSE., DUMMY_STATUS )
        DUMMY_STATUS = SAI__OK
        CALL NBS_PUT_VALUE( CRED4_BUSY_ID, 0, VAL__NBI, .FALSE., DUMMY_STATUS )
      ELSE

        CALL NBS_PUT_VALUE( CRED4_BUSY_ID, 0, VAL__NBI, .FALSE., STATUS )
        CALL NBS_PUT_VALUE( RED4_BUSY_ID, 0, VAL__NBI, .FALSE., STATUS )
        CALL NBS_PUT_VALUE( P4_BUSY_ID, 0, VAL__NBI, .FALSE., STATUS )
        PAUSE_REDUCTION = .TRUE.
        CALL MSG_OUT( ' ', 'Data reduction sequence not set up!', STATUS )
      ENDIF

      END
