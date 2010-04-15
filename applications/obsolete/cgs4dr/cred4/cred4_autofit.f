*+  CRED4_AUTOFIT - Automatically carry out line fit on observation
      SUBROUTINE CRED4_AUTOFIT( OBSERVATION, ASK, WAIT, STATUS )
*    Description :
*     Instructs the RED4 monlith to extract rows from the
*     specified reduced observation and to search for and fit
*     emission lines within those rows with the EMLT function.
*     The ENG4 D-task is instructed to calculate the shift
*     between the brightest lines and convert this into a slit
*     angle.
*    Invocation :
*     CALL CRED4_AUTOFIT( OBSERVATION, ASK, WAIT, STATUS )
*    Authors :
*     P.N.Daly (JACH::PND)
*    History :
*     19-Jan-1995: Original Unix version (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'DTASK_ERR'
*    Import :
      LOGICAL ASK                   ! T if should ask user before proceeding
      LOGICAL WAIT                  ! T if should wait for reduction action
      CHARACTER*(*) OBSERVATION     ! The name of the observation
*    Status :
      INTEGER STATUS
*    External references :
      INTEGER CHR_LEN               ! Character length determining function
*    Global variables :
      INCLUDE 'CRED4COM.INC'        ! CRED4 common block
*    Local variables :
      LOGICAL PROCEED               ! T if line fitting is to proceed
      CHARACTER*( MSG_VAL_LEN )
     :  INVAL,                      ! Command line parameter string
     :  OUTVAL,                     ! Returned string
     :  SPECTRUM,                   ! Output spectrum
     :  SRC_FILE                    ! EMLT source file
      CHARACTER*80 ROBSERVATION     ! Reduced observation name
      CHARACTER*20 OTYPE            ! Observation type
      CHARACTER*4 COMMENT           ! Dummy comment
      INTEGER DROWS                 ! Number of rows on the detector
      INTEGER YS                    ! First row to be extracted
      INTEGER YE                    ! Second row to be extracted
      INTEGER CPOS                  ! Character position
      INTEGER ERR_STATUS            ! Temporary status
*-
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get the task aliases required by this routine
      CALL PAR_GET0C( 'RED4_ALIAS', RED4_ALIAS, STATUS )
      IF ( .NOT. REDUCTION_OK ) RETURN

*    Create a ROBSERVATION name
      CALL CRED4_OBSTOROBS( OBSERVATION, ROBSERVATION, STATUS )

*    Open the observation and check FITS items
      CALL DSA_OPEN( STATUS )
      CALL DSA_NAMED_INPUT( 'OBS_IN', OBSERVATION, STATUS )
      CALL DSA_GET_FITS_I( 'OBS_IN', 'DROWS', 0, DROWS, COMMENT, STATUS )
      CALL DSA_GET_FITS_C( 'OBS_IN', 'OBSTYPE', 0, OTYPE, COMMENT, STATUS )
      CALL CHR_UCASE( OTYPE )
      CALL DSA_CLOSE( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'OBS', OBSERVATION )
         CALL ERR_REP( ' ', 'CRED4_AUTOFIT: Unable to open observation ^OBS', STATUS )
         RETURN
      ENDIF

*    Abort if the observation is a BIAS, DARK or FLAT.
      IF ( OTYPE.EQ.'BIAS' .OR. OTYPE.EQ.'DARK' .OR. OTYPE.EQ.'FLAT' ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC ( 'OBSERVATION', OBSERVATION )
         CALL MSG_SETC( 'OTYPE', OTYPE )
         CALL ERR_REP( ' ', 'CRED4_AUTOFIT: '/
     :     /'Observation ^OBSERVATION is a '/
     :     /'^OTYPE, and is not suitable for line fitting', STATUS )
         RETURN
      ELSE
         CALL MSG_SETC ( 'ROBS', ROBSERVATION )
         CALL MSG_SETC( 'OTYPE', OTYPE )
         CALL MSG_OUT (' ', 'Analysing reduced ^OTYPE observation '/
     :     /'^ROBS with line fitting algorithm', STATUS)
      ENDIF

*    If necessary ask user if they wish to proceed
      IF ( ASK ) THEN
         CALL PAR_CANCL ('PROCEED_ANALYSE', STATUS )
         CALL PAR_GET0L ('PROCEED_ANALYSE', PROCEED, STATUS)
      ELSE
         PROCEED = .TRUE.
      ENDIF

      IF ( PROCEED ) THEN

*       Set the RED4_BUSY flag
         CALL NBS_PUT_VALUE( RED4_BUSY_ID, 0, VAL__NBI, .TRUE., STATUS )

*       If the RED4_ACTIVE flag is true, wait for another action to finish
         IF ( RED4_ACTIVE ) THEN

            CALL TASK_DONE( -1, RED4_PATH, RED4_MESSID, OUTVAL, STATUS )

            RED4_ACTIVE = .FALSE.

            IF ( STATUS .NE. DTASK__ACTCOMPLETE ) THEN
                  IF ( STATUS .EQ. DTASK__ACTINFORM ) THEN
                     ERR_STATUS = STATUS
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'ES', ERR_STATUS )
                     CALL ERR_REP( ' ', 'CRED4_AUTOFIT: '/
     :                  /'First failure reported by RED4 task '/
     :                  /'(Status = ^ES, message follows)', STATUS )
                     CALL MSG_SETC( 'OUTVAL', OUTVAL )
                     CALL ERR_REP( ' ', 'CRED4_AUTOFIT: '/
     :                  /'^OUTVAL', STATUS )
                  ELSE IF ( STATUS .EQ. DSA__ERROR ) THEN
                     STATUS = SAI__ERROR
                     CALL ERR_REP( ' ', 'CRED4_AUTOFIT: '/
     :                  /'First DSA error has occured', STATUS )
                  ELSE
                     ERR_STATUS = STATUS
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'ES', ERR_STATUS )
                     CALL ERR_REP( ' ', 'CRED4_AUTOFIT: '/
     :                  /'Failed to reduce observation'/
     :                  /'(Status = ^ES)', STATUS )
                  ENDIF
               ELSE
                  CALL ERR_ANNUL( STATUS )
               ENDIF
            ENDIF

*       Tell the RED4 task to reset its engineering counters
         CALL CRED4_OBEYW( RED4_ALIAS(1:CHR_LEN(RED4_ALIAS)), 'ENG_RESET', ' ', STATUS )
         IF ( STATUS .NE. SAI__OK ) REDUCTION_OK = .FALSE.

*       Tell the RED4 task to open a log file
         CALL CRED4_OBEYW( RED4_ALIAS(1:CHR_LEN(RED4_ALIAS)), 'OPEN_LOG', ' ', STATUS )
         IF ( STATUS .NE. SAI__OK ) REDUCTION_OK = .FALSE.

*       Tell the RED4 task to log a comment indicating the name of the observation being analysed.
         IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Writing comment to log file', STATUS )
         CPOS = 0
         CALL CHR_PUTC( 'COMMENT="Automatic analysis of observation ', INVAL, CPOS )
         CALL CHR_PUTC( ROBSERVATION(1:CHR_LEN(ROBSERVATION))//'"', INVAL, CPOS )
         CALL CRED4_OBEYW( RED4_ALIAS(1:CHR_LEN(RED4_ALIAS)), 'LOG_COMMENT', INVAL(1:CPOS), STATUS)
         IF ( STATUS .NE. SAI__OK ) REDUCTION_OK = .FALSE.

*      Tell the RED4 task to extract a spectrum from the first row of the observation
         CALL CHR_FILL( ' ', SPECTRUM )
         SPECTRUM = PREFIX // 'CGS4_DATA' // SEPARATOR // 'spectrum1'
         CALL CHR_RMBLK( SPECTRUM )
         YS = MAX( 1, AFIT_ROW1 - (AFIT_NROWS / 2) )
         YE = MIN( DROWS, AFIT_ROW1 + (AFIT_NROWS / 2) )
         CALL MSG_SETI( 'YS', YS )
         CALL MSG_SETI( 'YE', YE )
         CALL MSG_SETC( 'SP', SPECTRUM )
         CALL MSG_OUT( ' ', 'Extracting rows ^YS to ^YE from reduced observation into ^SP', STATUS )
         CPOS = 0
         CALL CHR_PUTC( 'IMAGE="', INVAL, CPOS )
         CALL CHR_PUTC( ROBSERVATION(1:CHR_LEN(ROBSERVATION)), INVAL, CPOS )
         CALL CHR_PUTC( '" SPECTRUM="', INVAL, CPOS )
         CALL CHR_PUTC( SPECTRUM(1:CHR_LEN(SPECTRUM)), INVAL, CPOS )
         CALL CHR_PUTC( '" YSTART=', INVAL, CPOS )
         CALL CHR_PUTI( YS, INVAL, CPOS )
         CALL CHR_PUTC( ' YEND=', INVAL, CPOS )
         CALL CHR_PUTI( YE, INVAL, CPOS )
         CALL CRED4_OBEYW( RED4_ALIAS(1:CHR_LEN(RED4_ALIAS)), 'EXTRACT4', INVAL(1:CPOS), STATUS)
         IF ( STATUS .NE. SAI__OK ) REDUCTION_OK = .FALSE.

*      Tell the RED4 task to search for lines in the given region of the first spectrum.
         CALL MSG_OUT( ' ', 'Searching for lines in first spectrum', STATUS )
         CPOS = 0
         CALL CHR_PUTC( 'SP="', INVAL, CPOS )
         CALL CHR_PUTC( SPECTRUM(1:CHR_LEN(SPECTRUM)), INVAL, CPOS )
         CALL CHR_PUTC( '" LI=0', INVAL, CPOS )
         CALL CHR_PUTC( ' XS=', INVAL, CPOS )
         CALL CHR_PUTR( AFIT_XSTART, INVAL, CPOS )
         CALL CHR_PUTC( ' XE=', INVAL, CPOS )
         CALL CHR_PUTR( AFIT_XEND, INVAL, CPOS )
         CALL CRED4_OBEYW( RED4_ALIAS(1:CHR_LEN(RED4_ALIAS)), 'EMLT', INVAL(1:CPOS), STATUS )
         IF ( STATUS .NE. SAI__OK ) REDUCTION_OK = .FALSE.

*     Tell the RED4 task to copy the contents of the $CGS4_ENG/emlt.lis file
         IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Copying EMLT info to log file', STATUS )
         CALL CHR_FILL( ' ', SRC_FILE )
         SRC_FILE = PREFIX // 'CGS4_ENG' // SEPARATOR // 'emlt.lis'
         CALL CHR_RMBLK( SRC_FILE )
         CPOS = 0
         CALL CHR_PUTC( 'SOURCE_FILE="', INVAL, CPOS )
         CALL CHR_PUTC( SRC_FILE(1:CHR_LEN(SRC_FILE))//'"', INVAL, CPOS )
         CALL CRED4_OBEYW( RED4_ALIAS(1:CHR_LEN(RED4_ALIAS)), 'COPY_TO_LOG', INVAL(1:CPOS), STATUS)
         IF ( STATUS .NE. SAI__OK ) REDUCTION_OK = .FALSE.

*      Tell the RED4 task to read the file $CGS4_ENG/emlt.lis produced by the RED4 EMLT action
         IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Reading EMLT info', STATUS )
         CPOS = 0
         CALL CHR_PUTC( 'XMIN=', INVAL, CPOS )
         CALL CHR_PUTR( AFIT_XSTART, INVAL, CPOS )
         CALL CHR_PUTC( ' XMAX=', INVAL, CPOS )
         CALL CHR_PUTR( AFIT_XEND, INVAL, CPOS )
         CALL CRED4_OBEYW( RED4_ALIAS(1:CHR_LEN(RED4_ALIAS)), 'READ_EMLT', INVAL(1:CPOS), STATUS )
         IF ( STATUS .NE. SAI__OK ) REDUCTION_OK = .FALSE.

*      Tell the ENG4 task to write the results to the log file.
         CALL MSG_OUT( ' ', 'Writing results to log file', STATUS )
         CPOS = 0
         CALL CHR_PUTC( 'YCEN=', INVAL, CPOS )
         CALL CHR_PUTI( AFIT_ROW1, INVAL, CPOS )
         CALL CRED4_OBEYW( RED4_ALIAS(1:CHR_LEN(RED4_ALIAS)), 'LOG_EMLT', INVAL(1:CPOS), STATUS )
         IF ( STATUS .NE. SAI__OK ) REDUCTION_OK = .FALSE.

*       Annul the status so that second line can be found
         CALL ERR_ANNUL( STATUS )

*      Tell the RED4 task to extract a spectrum from the second  row of the observation.
         CALL CHR_FILL( ' ', SPECTRUM )
         SPECTRUM = PREFIX // 'CGS4_DATA' // SEPARATOR // 'spectrum2'
         CALL CHR_RMBLK( SPECTRUM )
         YS = MAX( 1, AFIT_ROW2 - (AFIT_NROWS / 2) )
         YE = MIN( DROWS, AFIT_ROW2 + (AFIT_NROWS / 2) )
         CALL MSG_SETI( 'YS', YS )
         CALL MSG_SETI( 'YE', YE )
         CALL MSG_SETC( 'SP', SPECTRUM )
         CALL MSG_OUT( ' ', 'Extracting rows ^YS to ^YE from observation into ^SP', STATUS )
         CPOS = 0
         CALL CHR_PUTC( 'IMAGE="', INVAL, CPOS )
         CALL CHR_PUTC( ROBSERVATION(1:CHR_LEN(ROBSERVATION)), INVAL, CPOS )
         CALL CHR_PUTC( '" SPECTRUM="', INVAL, CPOS )
         CALL CHR_PUTC( SPECTRUM(1:CHR_LEN(SPECTRUM)), INVAL, CPOS )
         CALL CHR_PUTC( '" YSTART=', INVAL, CPOS )
         CALL CHR_PUTI( YS, INVAL, CPOS )
         CALL CHR_PUTC( ' YEND=', INVAL, CPOS )
         CALL CHR_PUTI( YE, INVAL, CPOS )
         CALL CRED4_OBEYW( RED4_ALIAS(1:CHR_LEN(RED4_ALIAS)), 'EXTRACT4', INVAL(1:CPOS), STATUS )
         IF ( STATUS .NE. SAI__OK ) REDUCTION_OK = .FALSE.

*      Tell the RED4 task to search for lines in the given region of the second spectrum.
         CALL MSG_OUT( ' ', 'Searching for lines in second spectrum', STATUS )
         CPOS = 0
         CALL CHR_PUTC( 'SP="', INVAL, CPOS )
         CALL CHR_PUTC( SPECTRUM(1:CHR_LEN(SPECTRUM)), INVAL, CPOS )
         CALL CHR_PUTC( '" LI=0', INVAL, CPOS )
         CALL CHR_PUTC( ' XS=', INVAL, CPOS )
         CALL CHR_PUTR( AFIT_XSTART, INVAL, CPOS )
         CALL CHR_PUTC( ' XE=', INVAL, CPOS )
         CALL CHR_PUTR( AFIT_XEND, INVAL, CPOS )
         CALL CRED4_OBEYW( RED4_ALIAS(1:CHR_LEN(RED4_ALIAS)), 'EMLT', INVAL(1:CPOS), STATUS )
         IF ( STATUS .NE. SAI__OK ) REDUCTION_OK = .FALSE.

*      Tell the RED4 task to copy the contents of the $CGS4_ENG/emlt.lis file
         IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Copying EMLT info to log file', STATUS )
         CALL CHR_FILL( ' ', SRC_FILE )
         SRC_FILE = PREFIX // 'CGS4_ENG' // SEPARATOR // 'emlt.lis'
         CALL CHR_RMBLK( SRC_FILE )
         CPOS = 0
         CALL CHR_PUTC( 'SOURCE_FILE="', INVAL, CPOS )
         CALL CHR_PUTC( SRC_FILE(1:CHR_LEN(SRC_FILE))//'"', INVAL, CPOS )
         CALL CRED4_OBEYW( RED4_ALIAS(1:CHR_LEN(RED4_ALIAS)), 'COPY_TO_LOG', INVAL(1:CPOS), STATUS )
         IF ( STATUS .NE. SAI__OK ) REDUCTION_OK = .FALSE.

*      Tell the RED4 task to read the file $CGS4_ENG/emlt.lis produced by the RED4 EMLT action
         IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Reading EMLT info', STATUS )
         CPOS = 0
         CALL CHR_PUTC( 'XMIN=', INVAL, CPOS )
         CALL CHR_PUTR( AFIT_XSTART, INVAL, CPOS )
         CALL CHR_PUTC( ' XMAX=', INVAL, CPOS )
         CALL CHR_PUTR( AFIT_XEND, INVAL, CPOS )
         CALL CRED4_OBEYW( RED4_ALIAS(1:CHR_LEN(RED4_ALIAS)), 'READ_EMLT', INVAL(1:CPOS), STATUS )
         IF ( STATUS .NE. SAI__OK ) REDUCTION_OK = .FALSE.

*      Tell the RED4 task to write the results to the log file
         CALL MSG_OUT( ' ', 'Writing results to log file', STATUS )
         CPOS = 0
         CALL CHR_PUTC( 'YCEN=', INVAL, CPOS )
         CALL CHR_PUTI( AFIT_ROW2, INVAL, CPOS )
         CALL CRED4_OBEYW( RED4_ALIAS(1:CHR_LEN(RED4_ALIAS)), 'LOG_EMLT', INVAL(1:CPOS), STATUS )
         IF ( STATUS .NE. SAI__OK ) REDUCTION_OK = .FALSE.

*      Tell RED4 to calculate the slit angle from the above measurements
         CALL MSG_OUT( ' ', 'Calculating slit angle', STATUS )
         CPOS = 0
         CALL CHR_PUTC( 'YCEN_P=', INVAL, CPOS )
         CALL CHR_PUTI( AFIT_ROW1, INVAL, CPOS )
         CALL CHR_PUTC( ' YCEN=', INVAL, CPOS )
         CALL CHR_PUTI( AFIT_ROW2, INVAL, CPOS )
         CALL CRED4_OBEYW( RED4_ALIAS(1:CHR_LEN(RED4_ALIAS)), 'CALC_SLIT_ANGLE', INVAL(1:CPOS), STATUS )
         IF ( STATUS .NE. SAI__OK ) REDUCTION_OK = .FALSE.

*      Tell the RED4 task to report the slit angle
         CPOS = 0
         CALL CHR_PUTC( 'YCEN_P=', INVAL, CPOS )
         CALL CHR_PUTI( AFIT_ROW1, INVAL, CPOS )
         CALL CHR_PUTC( ' YCEN=', INVAL, CPOS )
         CALL CHR_PUTI( AFIT_ROW2, INVAL, CPOS )
         CALL CRED4_OBEYW( RED4_ALIAS(1:CHR_LEN(RED4_ALIAS)), 'REPORT_SLIT', INVAL(1:CPOS), STATUS )
         IF ( STATUS .NE. SAI__OK ) REDUCTION_OK = .FALSE.

*      Tell the RED4 task to write the slit angle to the log file.
         IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Writing slit angle to log file', STATUS )
         CPOS = 0
         CALL CHR_PUTC( 'YCEN_P=', INVAL, CPOS )
         CALL CHR_PUTI( AFIT_ROW1, INVAL, CPOS )
         CALL CHR_PUTC( ' YCEN=', INVAL, CPOS )
         CALL CHR_PUTI( AFIT_ROW2, INVAL, CPOS )
         CALL CRED4_OBEYW( RED4_ALIAS(1:CHR_LEN(RED4_ALIAS)), 'LOG_SLIT', INVAL(1:CPOS), STATUS )
         IF ( STATUS .NE. SAI__OK ) REDUCTION_OK = .FALSE.

*      Tell the RED4 task to read information from the reduced observation.
         IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Reading information from reduced observation file', STATUS )
         CPOS = 0
         CALL CHR_PUTC( 'OBSERVATION="', INVAL, CPOS )
         CALL CHR_PUTC( ROBSERVATION(1:CHR_LEN(ROBSERVATION))//'"', INVAL, CPOS )
         CALL CRED4_OBEYW( RED4_ALIAS(1:CHR_LEN(RED4_ALIAS)), 'READ_OBS', INVAL(1:CPOS), STATUS )
         IF ( STATUS .NE. SAI__OK ) REDUCTION_OK = .FALSE.

*      Tell the RED4 task to write an engineering type 1 record.
         CALL MSG_OUT( ' ', 'Writing engineering type 1 record', STATUS )
         CALL CRED4_OBEYW( RED4_ALIAS(1:CHR_LEN(RED4_ALIAS)), 'LOG_TYPE1', ' ', STATUS )
         IF ( STATUS .NE. SAI__OK ) REDUCTION_OK = .FALSE.

*      Reset the RED4_BUSY flag
         CALL ERR_ANNUL( STATUS )
         CALL NBS_PUT_VALUE( RED4_BUSY_ID, 0, VAL__NBI, .FALSE., STATUS )

*      Delete the old $CGS4_ENG/emlt.lis files and spectrum%.* files
         CALL MSG_OUT( ' ', 'Tidying up old $CGS4_ENG/emlt.lis, $CGS4_DATA/spectrum%.* files', STATUS )
         CALL CRED4_TIDY_FILES( STATUS )
      ENDIF

 500  CONTINUE
      END
