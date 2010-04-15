*+  RED4 - The CGS4 data reduction A-task monolith.
      SUBROUTINE RED4( STATUS )
*    Description :
*     This is the main program for the RED4 A-task monolith.
*    Authors :
*     P.N.Daly    (JACH::PND)
*    History :
*     19-Jan-1995: Original Unix version                        (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_PAR'
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'RED4_COMMON.INC'        ! RED4 common block
      INCLUDE 'RED4_ENG.INC'           ! RED4 engineering common block
*    Local variables :
      CHARACTER*( PAR__SZNAM ) NAME    ! Name of action
*-

*    Check for error on entry
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get the action name and  call appropriate subroutine
      CALL TASK_GET_NAME( NAME, STATUS )

*    Add an integration to the current co-added observation
      IF ( NAME .EQ. 'ADD_INT' ) THEN
         CALL RED4_ADD_INTEGRATION( STATUS )

*    Add an observation to the current group
      ELSE IF ( NAME .EQ. 'ADD_OBS' ) THEN
         CALL RED4_ADD_OBSERVATION( STATUS )

*    Add a pair of OBJ-SKY to current group
      ELSE IF ( NAME .EQ. 'ADD_PAIR' ) THEN
         CALL RED4_ADD_PAIR( STATUS )

*    Merge two spectra according to their axis arrays
      ELSE IF ( NAME .EQ. 'ADJOIN' ) THEN
         CALL RED4_ADJOIN( STATUS )

*    Record an observation in the observation catalogue
      ELSE IF ( NAME .EQ. 'ARCHIVE_OBS' ) THEN
         CALL RED4_ARCHIVE_OBSERVATION( STATUS )

*    Write data quality information from bad pixel mask
      ELSE IF ( NAME .EQ. 'APPLY_MASK' ) THEN
         CALL RED4_APPLY_MASK( STATUS )

*    Calculate a black-body spectrum on a template wavelength axis
      ELSE IF ( NAME .EQ. 'BLACK_BODY' ) THEN
         CALL RED4_BLACK( STATUS )

*    Calculate the slit angle
      ELSE IF ( NAME .EQ. 'CALC_SLIT_ANGLE' ) THEN
         CALL RED4_CALC_SLIT_ANGLE( STATUS )

*    Flag noisy pixels as bad
      ELSE IF ( NAME .EQ. 'CLEAN_OBS' ) THEN
         CALL RED4_CLEAN_OBS( STATUS )

*    Close log file
      ELSE IF ( NAME .EQ. 'CLOSE_LOG' ) THEN
         CALL RED4_CLOSE_LOG( STATUS )

*    Copy info to log file
      ELSE IF ( NAME .EQ. 'COPY_TO_LOG' )  THEN
         CALL RED4_COPY2LOG( STATUS )

*    Create a bad pixel mask from an error array
      ELSE IF ( NAME .EQ. 'CRE_ERROR_MASK' ) THEN
         CALL RED4_CRE_ERROR_MASK( STATUS )

*    Create a bad pixel mask by thresholding
      ELSE IF ( NAME .EQ. 'CRE_THRESH_MASK' ) THEN
         CALL RED4_CRE_THRESH_MASK( STATUS )

*    Create a bad pixel mask by applying a window
      ELSE IF ( NAME .EQ. 'CRE_WINDOW_MASK' ) THEN
         CALL RED4_CRE_WINDOW_MASK( STATUS )

*    Create reduced group container file
      ELSE IF ( NAME .EQ. 'CREATE_GRPRED' ) THEN
         CALL RED4_CREATE_GRPRED( STATUS )

*    Create reduced observation structure
      ELSE IF ( NAME .EQ. 'CREATE_OBSRED' ) THEN
         CALL RED4_CREATE_OBSREDFILE( STATUS )

*    Divide a group by a suitable STANDARD
      ELSE IF ( NAME .EQ. 'DIVIDE_BY_STD' ) THEN
         CALL RED4_DIVIDE_BY_STD( STATUS )

*    Edit an existing bad pixel mask
      ELSE IF ( NAME .EQ. 'EDIT_MASK' ) THEN
         CALL RED4_EDIT_MASK( STATUS )

*    Determine the efficiency at which an observation or group was observed
      ELSE IF ( NAME .EQ. 'EFFICIENCY' ) THEN
         CALL RED4_EFFICIENCY( STATUS )

*    Use the Figaro EMLT function to analyse a spectrum for emission lines
      ELSE IF ( NAME .EQ. 'EMLT' ) THEN
         CALL RED4_EMLT( STATUS )

*    Reset the RED4 engineering parameters
      ELSE IF ( NAME .EQ. 'ENG_RESET' )  THEN
         NMEAS = 0
         EMLT_VALID = .FALSE.
         SLIT_ANGLE_VALID = .FALSE.
         OBSERVATION_VALID = .FALSE.
         CALL ERR_ANNUL( STATUS )

*    Extract a 1-D spectrum from the rows of a 2-D image using CGS3 algorithm
      ELSE IF ( NAME .EQ. 'EXTRACT3' ) THEN
         CALL RED4_EXTRACT3( STATUS )

*    Extract a 1-D spectrum from the rows of a 2-D image using CGS4 algorithm
      ELSE IF ( NAME .EQ. 'EXTRACT4' ) THEN
         CALL RED4_EXTRACT( STATUS )

*    Create a bad pixel mask from a data quality array
      ELSE IF ( NAME .EQ. 'EXTRACT_MASK' ) THEN
         CALL RED4_EXTRACT_MASK( STATUS )

*    Define an observation as a CALIBRATION, and file it in the index file
      ELSE IF ( NAME .EQ. 'FILE_CALIB' ) THEN
         CALL RED4_FILE_CALIBRATION( STATUS )

*    File an observation in the index file
      ELSE IF ( NAME .EQ. 'FILE_OBS' ) THEN
         CALL RED4_FILE_OBSERVATION( STATUS )

*    Define an observation as a STANDARD, and file it in the index file
      ELSE IF ( NAME .EQ. 'FILE_STANDARD' ) THEN
         CALL RED4_FILE_STANDARD( STATUS )

*    Flux calibrate a standard divided spectrum
      ELSE IF ( NAME .EQ. 'FLUX_CALIBRATE' ) THEN
         CALL RED4_FLUX_CALIBRATE( STATUS )

*    Add two data structures
      ELSE IF ( NAME .EQ. 'IADD4' ) THEN
         CALL RED4_IADSUB ('IADD', STATUS)

*    Divide two data structures
      ELSE IF ( NAME .EQ. 'IDIV4' ) THEN
         CALL RED4_IADSUB ('IDIV', STATUS)

*    Multiply two data structures
      ELSE IF ( NAME .EQ. 'IMULT4' ) THEN
         CALL RED4_IADSUB ('IMULT', STATUS)

*    Subtract two data structures
      ELSE IF ( NAME .EQ. 'ISUB4' ) THEN
         CALL RED4_IADSUB ('ISUB', STATUS)

*    Produce the logical AND of two bad pixel masks
      ELSE IF ( NAME .EQ. 'IAND4' ) THEN
         CALL RED4_IORAND( 'IAND', STATUS )

*    Produce the logical exclusive OR of two bad pixel masks
      ELSE IF ( NAME .EQ. 'IEOR4' ) THEN
         CALL RED4_IORAND( 'IEOR', STATUS )

*    Produce the logical OR of two bad pixel masks
      ELSE IF ( NAME .EQ. 'IOR4' ) THEN
         CALL RED4_IORAND( 'IOR', STATUS )

*    Produce the logical inverse of a bad pixel masks
      ELSE IF ( NAME .EQ. 'INOT4' ) THEN
         CALL RED4_INOT( STATUS )

*    Init the system
      ELSE IF ( NAME .EQ. 'INIT' ) THEN
         CALL RED4_INIT_SYS( STATUS )

*    Produce a normalised, flat-field ripple spectrum
      ELSE IF ( NAME .EQ. 'IRFLAT' ) THEN
         CALL RED4_IRFLAT( STATUS )

*    List the INDEX file
      ELSE IF ( NAME .EQ. 'LIST_INDEX' ) THEN
         CALL RED4_LIST_INDEX (STATUS)

*    Log a comment
      ELSE IF ( NAME .EQ. 'LOG_COMMENT' ) THEN
         CALL RED4_LOG_COMMENT( STATUS )

*    Log emlt info
      ELSE IF ( NAME .EQ. 'LOG_EMLT' ) THEN
         CALL RED4_LOG_EMLT( STATUS )

*    Log slit info
      ELSE IF ( NAME .EQ. 'LOG_SLIT' ) THEN
         CALL RED4_LOG_SLIT( STATUS )

*    Log type 1 info
      ELSE IF ( NAME .EQ. 'LOG_TYPE1' ) THEN
         CALL RED4_LOG_TYPE1( STATUS )

*    Convert a group into a flux calibration standard
      ELSE IF ( NAME .EQ. 'MAKE_STANDARD' ) THEN
         CALL RED4_MAKE_STANDARD( STATUS )

*    Create integration structure
      ELSE IF ( NAME .EQ. 'MAKE_INT' ) THEN
         CALL RED4_MAKE_INTEGRATION( STATUS )

*    Create observation structure
      ELSE IF ( NAME .EQ. 'MAKE_OBS' ) THEN
         CALL RED4_MAKE_OBSERVATION( STATUS )

*    Mend some data by interpolating across bad pixels
      ELSE IF ( NAME .EQ. 'MEND' ) THEN
         CALL RED4_MEND( STATUS )

*    Extract a 1-D spectrum from up to 3 rows of a 2-D image
      ELSE IF ( NAME .EQ. 'NODEXTRACT4' ) THEN
         CALL RED4_NODEXTRACT( STATUS )

*    Normalise a flat field
      ELSE IF ( NAME .EQ. 'NORMALISE_FF' ) THEN
         CALL RED4_NORMALISE_FF( STATUS )

*    Open log file
      ELSE IF ( NAME .EQ. 'OPEN_LOG' ) THEN
         CALL RED4_OPEN_LOG( STATUS )

*    Poke a FITS item in a data structure
      ELSE IF ( NAME .EQ. 'POKE_FITS' ) THEN
         CALL RED4_POKE_FITS( STATUS )

*    Do POLYFIT enhanced sky subtraction
      ELSE IF ( NAME .EQ. 'POLYFIT' ) THEN
         CALL RED4_POLYFIT( STATUS )

*    Read the $CGS4_ENG/emlt.lis file
      ELSE IF ( NAME .EQ. 'READ_EMLT' ) THEN
         CALL RED4_READ_EMLT( STATUS )

*    Read the observation file (autofit)
      ELSE IF ( NAME .EQ. 'READ_OBS' ) THEN
         CALL RED4_READ_OBS( STATUS )

*    Create reduced integration structure
      ELSE IF ( NAME .EQ. 'RED_INT' ) THEN
         CALL RED4_REDUCE_INTEGRATION( STATUS )

*    Create reduced observation structure
      ELSE IF ( NAME .EQ. 'RED_OBS' ) THEN
         CALL RED4_REDUCE_OBSERVATION( STATUS )

*    Subtract an integration from a coadded observation
      ELSE IF ( NAME .EQ. 'REMOVE_INT' ) THEN
         CALL RED4_REMOVE_INTEGRATION( STATUS )

*    Subtract an observation from a coadded group
      ELSE IF ( NAME .EQ. 'REMOVE_OBS' ) THEN
         CALL RED4_REMOVE_OBSERVATION( STATUS )

*    Subtract a pair of observations from a coadded group
      ELSE IF ( NAME .EQ. 'REMOVE_PAIR' ) THEN
         CALL RED4_REMOVE_PAIR( STATUS )

*    Report the output from EMLT
      ELSE IF ( NAME .EQ. 'REPORT_EMLT' )  THEN
         CALL RED4_REPORT_EMLT( STATUS )

*    Report the output of the slit calculation
      ELSE IF ( NAME .EQ. 'REPORT_SLIT' ) THEN
         CALL RED4_REPORT_SLIT( STATUS )

*    Reset the common block pointers and virtual memory storing BIAS, DARK, FLAT and STANDARD
      ELSE IF ( NAME .EQ. 'RESET' ) THEN
         CALL RED4_RESET( STATUS )
         CALL ERR_ANNUL( STATUS )

*    Sets the format of the reduction files
      ELSE IF ( NAME .EQ. 'SET_FORMAT' ) THEN
         CALL RED4_SET_FORMAT( STATUS )

*    Switch verbose messages on or off, according to a parameter
      ELSE IF ( NAME .EQ. 'SET_VERBOSE' ) THEN
         VERBOSE = .FALSE.
         CALL PAR_GET0L( 'VERBOSE', VERBOSE, STATUS )
         IF ( VERBOSE ) THEN
            CALL MSG_OUT( ' ', 'Verbose messages switched ON in the RED4 task', STATUS )
         ELSE
            CALL MSG_OUT( ' ', 'Verbose messages switched OFF in the RED4 task', STATUS )
         END IF

*    Get stats on an image
      ELSE IF ( NAME .EQ. 'STATS' ) THEN
        CALL RED4_STATS( STATUS )

*    Quick status check for the RED4 task
      ELSE IF ( NAME .EQ. 'STATUS' ) THEN
        CALL MSG_OUT( ' ', 'RED4 Task  : Portable-CGS4DR RED4 VPKG_VERS', STATUS )
        CALL MSG_OUT( ' ', 'RED4 Task  : The uncached RED4 task is OK', STATUS )

*    Invalid action name
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'NAME', NAME )
         CALL ERR_REP( ' ', 'RED4: Invalid action, ^NAME', STATUS )
      END IF

      END
