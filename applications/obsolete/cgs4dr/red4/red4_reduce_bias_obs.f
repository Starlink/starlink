*+  RED4_REDUCE_BIAS_OBS - Reduce a BIAS observation
      SUBROUTINE RED4_REDUCE_BIAS_OBS( OBS_NAME, STATUS )
*    Description :
*     This routine take a BIAS observation of name Oyymmdd_n in ODIR, searches
*     IDIR (the raw integration directory) for the integrations that belong to
*     it (Iyymmdd_n_m) and reduces them all at once into a reduced observation
*     file ROyymmdd_n in RODIR. Individual files containing reduced integrations
*     are not produced. The reduction in this case consists of dividing the
*     numbers coadded in the ADP for each integration by the number of
*     exposures that went into the coadd, then adding the result into the
*     result array of the observation. Errors are calculated from the spread
*     of the numbers about the mean. Chopped integrations are not allowed,
*     nor are ones with weighted frames.
*    Invocation :
*     CALL RED4_REDUCE_BIAS_OBS (OBS_NAME, STATUS)
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*    Method :
*    Deficiencies :
*     Note that the logical names IDIR:, ODIR:, RIDIR:, RODIR: CANNOT
*     be changed, because the character handling in the code has
*     assumptions about these names hard-wired in various places.
*
*     DSA status values do not conform to the ADAM scheme. It has
*     therefore been necessary to trap these statuses before they
*     get translated into confusing messages like "exceeded quota".
*     The traps can be removed once DSA conforms.
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     John Lightfoot (REVAD::JFL)
*     Steven Beard   (REVAD::SMB)
*     Phil Daly      (JACH::PND)
*    History :
*     1989:  Original (JFL)
*     22-Jan-1990: Modified to overcome compilation problems
*                  in RED4_DO_COADD.                             (SMB)
*     20-Feb-1990: Null bad pixel mask changed from ' ' to '#',
*                  as it was difficult to process ' ' with ICL
*                  variables.                                    (SMB)
*      9-Mar-1990: MAXDIM and RMAXDIM parameters added.          (SMB)
*      9-Mar-1990: Modified to report type of integrations being
*                  reduced.                                      (SMB)
*      9-Mar-1990: Division by number of exposures commented out,
*                  since this operation is now carried out
*                  automatically by the ADP. (If this works, the
*                  code may be deleted).                         (SMB)
*     12-Mar-1990: Bug fix. Superfluous call to DSA_CLOSE removed.(SMB)
*     24-Apr-1990: Because of memory corruption problems, the
*                  code needs to be compiled with array bounds
*                  checking switched on. The Figaro dynamic
*                  memory functions (DYN_ELEMENT, DYNAMIC_MEM,
*                  DYN_INCREMENT) would not allow this. Code
*                  modified to use %val() instead.            (SMB)
*      2-May-1990: The routines DSA_GET_DATA_INFO and
*                  DSA_SET_DATA_INFO require a double
*                  precision array for the NUM_ARRAY argument.
*                  This routine was giving an integer (0),
*                  which could cause problems. Fixed.         (SMB)
*      4-May-1990: All masks now written to CGS4_MASKS
*                  directory.                                 (SMB)
*      5-Jul-1990: Commented out code removed.                (SMB)
*      6-Jul-1990: Made to write object name and exposure
*                  time in a standard Figaro structure (as
*                  well as the special CGS4 structure). (Why
*                  was this not done in the first place??).   (SMB)
*     10-Jul-1990: Hard-wired "4"s replaced by "FLOATSIZE".   (SMB)
*     20-Jul-1990: "Integration does not exist" message was
*                  confusing. Replaced.                       (SMB)
*     20-Jul-1990: Modified to check that the number of
*                  detector increments is 1. Oversampled
*                  BIAS frames are not allowed.               (SMB)
*     23-Jul-1990: Checked to ensure that 4 digit
*                  observation and integration numbers can
*                  be processed.                              (SMB)
*     31-Jul-1990: The 15 character limit on the length of
*                  the name of the DSA COADDS structures was
*                  causing problems when the observation and
*                  integration numbers became large. COADDS
*                  structure naming convention changed from
*                  Iyymmdd_oooo_iiii to I_oooo_iiii.          (SMB)
*     21-Aug-1990: Phase 1 of major changes: Header
*                  information in the observation and reduced
*                  observation files is now written in a .FITS
*                  structure.                                 (SMB)
*      3-Sep-1990: Phase 2 of major changes: Header
*                  information in the integration and reduced
*                  integration files is now written in a .FITS
*                  structure.                                 (SMB)
*      4-Sep-1990: The phase 2 changes made the reduced
*                  observation data structure created when
*                  reducing a whole observation in one go
*                  different from that created when reducing
*                  one integration at a time. This effect fixed
*                  by copying the .FITS structure from the raw
*                  integration to the CGS4_COADDS structure,
*                  rather than from the reduced integration
*                  template.                                  (SMB)
*      5-Sep-1990: The phase 2 changes caused the DTA cache
*                  bug to reappear. Aaaargh!! Investigations
*                  narrowed down the cause of the problems to
*                  in the mapping of the data array in the
*                  CGS4_INDEX structure. Further investigations
*                  revealed a bug in DTA_CACHEL. The problem
*                  went away when this was fixed. All
*                  experimental code now removed.             (SMB)
*      7-Sep-1990: Typing mistakes fixed.                     (SMB)
*      7-Sep-1990: Output made less verbose.                  (SMB)
*     14-Sep-1990: MASKUSED parameter written to FITS
*                  structure of reduced observation file.     (SMB)
*     14-Sep-1990: Code simplified by encapsulating low-level
*                  DTA calls in RED4_COPY_STRUCTURE.          (SMB)
*     26-Sep-1990: Dimension check between integrations and
*                  bad pixel mask included.                   (SMB)
*      7-Nov-1990: Modified to check for the -50.0 values
*                  set by the ADP when it detects a bad value.(SMB)
*     21-Nov-1990: Modified to use VERBOSE flag.              (SMB)
*     23-Nov-1990: Bug fix: Only call RED4_FLAG_BAD if the
*                  status is ok (to prevent adjustable array
*                  bounds violation).                         (SMB)
*     29-Nov-1990: MAXDIM and RMAXDIM constants moved to
*                  RED4_COMMON.INC. Unnecessarily large
*                  character variables reduced in size.       (SMB)
*     13-Feb-1991: Modified to reject observations with no
*                  integrations.                              (SMB)
*     24-Feb-1991: DSA error statuses trapped, as these no
*                  not conform to the ADAM error scheme.      (SMB)
*     22-Feb-1993: Conform to error strategy                  (PND)
*      7-Nov-1994: Make vaguely portable                       (AB)
*     19-Mar-1996: Add FIRST for UPDATE/WRITE problem in DSA  (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
*    Status :
      INTEGER STATUS
*    Input :
      CHARACTER*(*) OBS_NAME             ! name of the observation file to be reduced, e.g. O890816_1
*    External references :
      INTEGER CHR_LEN                    ! ADAM stringlength function
      INTEGER DSA_TYPESIZE               ! DSA type size function
*    Global variables :
      INCLUDE 'RED4_COMMON.INC'          ! RED4 common block.
*    Local constants :
      BYTE BAD
      PARAMETER ( BAD = 1 )
      BYTE GOOD
      PARAMETER ( GOOD = 0 )
*    Local variables :
      LOGICAL VARIANCE_MAP               ! T if variance array is mapped.
      LOGICAL LOOPING                    ! T while looping through component integrations
      LOGICAL EXIST                      ! T if the integration file exists
      INTEGER FLOATSIZE                  ! Number of bytes per element in a 'FLOAT' array
      INTEGER BYTESIZE                   ! Number of bytes per element in a 'BYTE' array
      INTEGER INTEGRATION                ! Number of current integration
      INTEGER TOTAL_INTS                 ! The total number of integrations
      INTEGER NDIM                       ! The dimensions of the integration array
      INTEGER N_EXPOSURES                ! The number of exposures that were coadded in the integration
      INTEGER DET_NINCR                  ! The number of detector positions which are in the observation
      INTEGER DET_INDEX                  ! The index of the detector position at which integration was taken
      INTEGER INT_SLOT                   ! For integration data
      INTEGER INT_DATA                   !        "
      INTEGER DATA_SLOT                  ! For reduce observation data
      INTEGER RED_DATA                   !        "
      INTEGER QUAL_SLOT                  ! Quality
      INTEGER RED_QUAL                   !        "
      INTEGER VAR_SLOT                   ! Variances
      INTEGER RED_VAR                    !        "
      INTEGER COADDS_SLOT                ! Coadds data in reduced observation
      INTEGER COADDS_PTR                 !        "
      INTEGER INDEX_SLOT                 ! Index array in reduced observation
      INTEGER INDEX_PTR                  !        "
      INTEGER MASK_SLOT                  ! Pixel quality mask
      INTEGER MASK_DATA                  !        "
      INTEGER WORK_SLOT                  ! Temporary work area
      INTEGER WORK_DATA                  !        "
      INTEGER WORK_Q_SLOT                ! Temporary quality
      INTEGER WORK_QUAL                  !        "
      INTEGER NELM                       ! Number of elements in integration array
      INTEGER NPLANE                     ! Number of elements in one plane of the integration array
      INTEGER OBSDIMS( RMAXDIM )         ! Dimensions of reduced observation data
      INTEGER INTDIMS( RMAXDIM )         ! Dimensions of reduced integration data
      INTEGER INDEX_DIMS( RMAXDIM )      ! Dimensions of index array in reduced observation data
      INTEGER LDAY, LDATE, LHOUR         ! Lengths of date strings
      INTEGER IGNORE                     ! Unimportant parameter
      INTEGER NELM_OBS                   ! Number of elements in reduced observation daa array
      INTEGER CLEN                       ! Non-blank length of character string
      INTEGER CPOS                       ! Non-blank position of character in string
      REAL EXPOSURE_TIME                 ! On-chip exposure time
      REAL INTEGRATION_TIME              ! EXPOSURE_TIME * N_EXPOSURES
      REAL OBSERVATION_TIME              ! Total integration for det_index 1 columns in reduced observation
      CHARACTER*80 COADDED_INTS          ! DTA name of .COADDED_INTS structure in reduced observation file
      CHARACTER*80 OBSREDFILE            ! Full name of file holding reduced obs
      CHARACTER*80 INTNAME               ! The name of a particular integratioN belonging to the observation
      CHARACTER*80 COADD_NAME            ! The name of the COADD structure corresponding to an integration
      CHARACTER*80 ROOT                  ! The root from which the INTNAMEs are derived
      CHARACTER*20 INT_TYPE              ! Type of integration
      CHARACTER*80 MASK                  ! File containing dud-pixel info
      CHARACTER*80 RECORD                ! DTA name of extensions in reduced integration and observation structures
      CHARACTER*40 OBJECT_NAME           ! The name of the object
      CHARACTER*32 CHAR_ARRAY(2)         ! Array to hold data units and title
      CHARACTER*40 TIME                  ! A string holding the date of the reduction
      CHARACTER*20 DAY, DATE, HOUR       ! Components of date
      CHARACTER*4 COMMENT                ! Dummy comment
      CHARACTER*20 CINTEGRATION          ! Integration number in character format
      CHARACTER*20 LPREFIX               ! Prefix to add to file
*
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
      SAVE FIRST
*-

*    Return if entry status bad
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get FITS items from OBS_IN (already open)
      CALL DSA_GET_FITS_C( 'OBS_IN', 'INTTYPE', 0, INT_TYPE, COMMENT, STATUS )
      CALL DSA_GET_FITS_I( 'OBS_IN', 'DETNINCR', 0, DET_NINCR, COMMENT, STATUS )
      CALL DSA_GET_FITS_F( 'OBS_IN', 'DEXPTIME', 0, EXPOSURE_TIME, COMMENT, STATUS )
      CALL DSA_GET_FITS_C( 'OBS_IN', 'OBJECT', 0, OBJECT_NAME, COMMENT, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_BIAS_OBS: '/
     :     /'Error getting FITS items from observation file', STATUS )
         GOTO 500
      END IF

*    Chop observations are meaningless so report an error
      IF ( INDEX(INT_TYPE,'CHOP') .NE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_BIAS_OBS: '/
     :     /'This BIAS observation was taken in chop '/
     :     /'mode. There must have been a mistake!', STATUS )
         GOTO 500

*    Weighted frames are meaningless, so report an error
      ELSE IF ( INDEX(INT_TYPE,'WEIGHTED' ) .NE. 0) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_BIAS_OBS: '/
     :     /'This BIAS observation was taken in '/
     :     /'weighted frames mode which is '/
     :     /'meaningless. There must have been a '/
     :     /'mistake!', STATUS )
         GOTO 500

*    Check exposure time is positive
      ELSE IF ( EXPOSURE_TIME .LE. 0.0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_BIAS_OBS: '/
     :     /'Exposure time is negative', STATUS )
         GOTO 500

*    Check that the number of detector increments is 1 (no oversampling)
      ELSE IF ( DET_NINCR .NE. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_BIAS_OBS: '/
     :     /'This BIAS observation was taken with '/
     :     /'oversampling, which is not sensible. '/
     :     /'There must have been a mistake!', STATUS )
         GOTO 500
      ENDIF

*    Get the name of the quality mask file, if there is one, and map it
      CALL PAR_GET0C( 'MASK', MASK, STATUS )
      CALL PAR_CANCL( 'MASK', STATUS )
      IF ( INDEX( MASK, SEPARATOR ) .EQ. 0 ) THEN
         CALL RED4_GET_PREFIX( 'MASK', LPREFIX, STATUS )
         MASK = LPREFIX(:CHR_LEN(LPREFIX)) // MASK(1:CHR_LEN(MASK))
      END IF
      IF ( INDEX( MASK, '#') .EQ. 0 ) THEN
         CALL MSG_SETC( 'MASK', MASK )
         CALL MSG_OUT( ' ', 'Using the bad pixel mask ^MASK', STATUS )
         CALL DSA_NAMED_INPUT( 'MASK', MASK, STATUS )
         CALL DSA_MAP_DATA( 'MASK', 'READ', 'BYTE', MASK_DATA, MASK_SLOT, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_REDUCE_BIAS_OBS: '/
     :      /'Error opening MASK data', STATUS )
            GOTO 500
         ENDIF
      ENDIF

*    Map in the data array of the reduced observation file, and the variance and quality arrays.
      BYTESIZE  = DSA_TYPESIZE( 'BYTE', STATUS )
      FLOATSIZE = DSA_TYPESIZE( 'FLOAT', STATUS )
      CALL DSA_USE_QUALITY( 'OBS_RED', STATUS )
      CALL DSA_MAP_DATA( 'OBS_RED', 'WRITE', 'FLOAT', RED_DATA, DATA_SLOT, STATUS )
      CALL DSA_MAP_QUALITY( 'OBS_RED', 'WRITE', 'BYTE', RED_QUAL, QUAL_SLOT, STATUS )
      CALL DSA_MAP_VARIANCE( 'OBS_RED', 'WRITE', 'FLOAT', RED_VAR, VAR_SLOT, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         VARIANCE_MAP = .TRUE.
      ELSE
         VARIANCE_MAP = .FALSE.
      END IF
      CALL DSA_DATA_SIZE( 'OBS_RED', RMAXDIM, NDIM, OBSDIMS, NELM_OBS, STATUS )

*    Obtain the number of bytes per element in a data array of type 'FLOAT' and 'BYTE'
*    Open the COADDS structure of the reduced observation file. This will
*    hold info on how many coadds have gone into the result array. Map in
*    its data array, and get the DTA name of the associated .COADDED_INTS
*    structure that will hold the (compressed) names of those
*    integrations that are added
      CALL DSA_GET_ACTUAL_NAME( 'OBS_RED', OBSREDFILE, STATUS )
      CALL DSA_NAMED_INPUT( 'COADDS', OBSREDFILE(1:CHR_LEN(OBSREDFILE))//'.MORE.CGS4_COADDS', STATUS )
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL DSA_MAP_DATA( 'COADDS', 'WRITE', 'SHORT', COADDS_PTR, COADDS_SLOT, STATUS )
        CALL DSA_SPECIFIC_STRUCTURE( 'COADDS', 'COADDED_INTS', 'WRITE', COADDED_INTS, STATUS )
      ELSE
        CALL DSA_MAP_DATA( 'COADDS', 'UPDATE', 'SHORT', COADDS_PTR, COADDS_SLOT, STATUS )
        CALL DSA_SPECIFIC_STRUCTURE( 'COADDS', 'COADDED_INTS', 'UPDATE', COADDED_INTS, STATUS )
      ENDIF

*    Map in the index array of the reduced observation file
      CALL DSA_NAMED_INPUT( 'INDEX', OBSREDFILE(1:CHR_LEN(OBSREDFILE))//'.MORE.CGS4_INDEX', STATUS )
      CALL DSA_MAP_DATA( 'INDEX', 'READ', 'SHORT', INDEX_PTR, INDEX_SLOT, STATUS )
      CALL DSA_DATA_SIZE( 'INDEX', RMAXDIM, NDIM, INDEX_DIMS, NELM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_BIAS_OBS: '/
     :     /'Error sizing coadds or index data', STATUS )
         GOTO 500
      ENDIF

*    Loop through the possible integration file names, and reduce them into the output file
      INTEGRATION = 0
      CPOS = INDEX( OBS_NAME, SEPARATOR )
      CLEN = CHR_LEN( OBS_NAME )
      CALL RED4_GET_PREFIX( 'I', LPREFIX, STATUS )
      ROOT = LPREFIX(:CHR_LEN(LPREFIX)) // 'i' // OBS_NAME(2+CPOS:CLEN) // '_'
      LOOPING = .TRUE.
      WORK_DATA = 0
      WORK_QUAL = 0
      OBSERVATION_TIME = 0.0

      DO WHILE ( LOOPING )

*       Construct integration file name
         INTEGRATION = INTEGRATION + 1
         CALL CHR_ITOC( INTEGRATION, CINTEGRATION, CPOS  )
         INTNAME = ROOT(1:CHR_LEN(ROOT)) // CINTEGRATION(1:CPOS)

*       We've finished if it doesn't exist
         CALL DSA_SEEK_NAMED_STRUCTURE( INTNAME, EXIST, STATUS )
         IF ( .NOT. EXIST ) THEN
            TOTAL_INTS = INTEGRATION - 1
            IF ( TOTAL_INTS .EQ. 0 ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_REDUCE_BIAS_OBS: '/
     :           /'Data has no integrations!', STATUS )
               GOTO 500
            ELSE IF ( TOTAL_INTS .EQ. 1 ) THEN
               CALL MSG_OUT( ' ', '1 integration only', STATUS )
            ELSE
               CALL MSG_SETI( 'TOTAL_INTS', TOTAL_INTS )
               CALL MSG_OUT( ' ', '^TOTAL_INTS integrations in total', STATUS )
            END IF
            LOOPING = .FALSE.
            GOTO 100
         ELSE
            CALL MSG_SETC( 'INTNAME', INTNAME )
            CALL MSG_OUT(' ', 'Reducing BIAS integration ^INTNAME', STATUS )
         ENDIF

*       Open the file
         CALL DSA_NAMED_INPUT( 'INT_IN', INTNAME, STATUS )

*       Get some FITS items for INT_IN
         CALL DSA_GET_FITS_I( 'INT_IN', 'NEXP', 0, N_EXPOSURES, COMMENT, STATUS )
         CALL DSA_GET_FITS_I( 'INT_IN', 'DINDEX', 0, DET_INDEX, COMMENT, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_REDUCE_BIAS_OBS: '/
     :        /'Error getting FITS item from integration file', STATUS )
            GOTO 500
         ENDIF

         IF ( N_EXPOSURES .LT. 1 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_REDUCE_BIAS_OBS: '/
     :        /'No exposures were made during the '/
     :        /'integration', STATUS )
            GOTO 500
         ELSE IF ( ( INDEX_DIMS(2) .LT. DET_INDEX ) .OR. ( DET_INDEX .LT.1 ) ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_REDUCE_BIAS_OBS: '/
     :        /'Detector position is outside '/
     :        /'range of the index array for this observation', STATUS )
            GOTO 500
         ENDIF

*       If a bad pixel mask has been specified, check the dimensions match
         IF ( INDEX( MASK, '#' ) .EQ. 0 ) THEN
            CALL DSA_MATCH_DIMENSION( 'INT_IN', 1, 'MASK', 1, STATUS )
            CALL DSA_MATCH_DIMENSION( 'INT_IN', 2, 'MASK', 2, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_REDUCE_BIAS_OBS: '/
     :           /'Mask and Data dimensions do not match!', STATUS )
               GOTO 500
            ENDIF
         ENDIF

*       Work out integration time
         INTEGRATION_TIME = N_EXPOSURES * EXPOSURE_TIME

*       If the integration is for detector index position 1, then add this to the observation time.
         IF ( DET_INDEX .EQ. 1 ) OBSERVATION_TIME = OBSERVATION_TIME + INTEGRATION_TIME

*       Find the size of the input data array
         CALL DSA_DATA_SIZE( 'INT_IN', MAXDIM, NDIM, INTDIMS, NELM, STATUS )
         NPLANE = INTDIMS(1) * INTDIMS(2)

*       Map the data and get (first time around) get work array
         CALL DSA_MAP_DATA( 'INT_IN', 'READ', 'FLOAT', INT_DATA, INT_SLOT, STATUS )
         IF ( WORK_DATA .EQ. 0 ) CALL DSA_GET_WORK_ARRAY( NPLANE, 'FLOAT', WORK_DATA, WORK_SLOT, STATUS )
         IF ( WORK_QUAL .EQ. 0 ) THEN
            CALL DSA_GET_WORK_ARRAY( NPLANE, 'BYTE', WORK_QUAL, WORK_Q_SLOT, STATUS )
            IF ( INDEX( MASK,'#').EQ.0 ) THEN
               IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Copying bad pixel mask', STATUS )
               CALL GEN_FILL( NPLANE, BAD, %val(WORK_QUAL) )
               CALL GEN_MOVEB( NPLANE, %val(MASK_DATA), %val(WORK_QUAL) )
            ELSE
               CALL GEN_FILL( NPLANE, GOOD, %val(WORK_QUAL) )
            ENDIF
         ENDIF
         IF ( STATUS .NE. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_REDUCE_BIAS_OBS: Unable to map integration or workspace', STATUS )
            GOTO 500
         ENDIF

*       Initialise it then copy Phase A
         CALL GEN_CFILL( 1, NPLANE, 0.0, %val(WORK_DATA) )
         CALL GEN_MOVEF( NPLANE, %val(INT_DATA), %val(WORK_DATA) )

*       Flag pixels which ADP has set to -50.0
         CALL RED4_FLAG_BAD( NPLANE, -50.0, %val(WORK_DATA), %val(WORK_QUAL), STATUS )

*       If the status is ok, coadd the work array into the observation result.
         IF ( VERBOSE .AND. STATUS.EQ.SAI__OK ) THEN
            CALL MSG_SETC( 'INTNAME', INTNAME )
            CALL MSG_OUT(' ', 'Coadding integration ^INTNAME', STATUS )
         ENDIF
         CALL RED4_DO_COADD( %val(WORK_DATA), %val(WORK_QUAL), INTDIMS(1), INTDIMS(2),
     :     %val(INDEX_PTR), INDEX_DIMS(1), INDEX_DIMS(2), DET_INDEX, %val(RED_DATA),
     :     %val(RED_VAR), %val(RED_QUAL), %val(COADDS_PTR), OBSDIMS(1), OBSDIMS(2), STATUS )

*       Copy structures
         CALL RED4_INTTOCOADD( INTNAME, COADD_NAME, STATUS )
         RECORD = COADDED_INTS(1:CHR_LEN(COADDED_INTS)) // '.' // COADD_NAME
         CALL RED4_COPY_STRUCTURE( 'INT_IN.'//FITS_STRUCTURE, RECORD, STATUS )

*       Release the slots
         CALL DSA_UNMAP( INT_SLOT, STATUS )
         CALL DSA_CLOSE_STRUCTURE( 'INT_IN', STATUS )

*      Break out of the loop if an error occurred.
         IF ( STATUS .NE. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_REDUCE_BIAS_OBS: '/
     :        /'Error in DSA reduction', STATUS )
            GOTO 500
         ENDIF
 100  CONTINUE
      ENDDO

      CALL DSA_FREE_WORKSPACE( WORK_SLOT, STATUS )
      CALL DSA_FREE_WORKSPACE( WORK_Q_SLOT, STATUS )
      IF ( VERBOSE .AND. STATUS.EQ.SAI__OK ) CALL MSG_OUT( ' ', 'BIAS reduction completed OK', STATUS )

*    Check that the variances are non-negative, they'll crash DSA close if they are
      IF ( VARIANCE_MAP .AND. ( NELM_OBS .GT. 0 ) ) CALL GEN_CLIPF( %val(RED_VAR),
     :   NELM_OBS, 0.0, VAL__MAXR, IGNORE, IGNORE, %val(RED_VAR) )

*    Set the data label and units
      CHAR_ARRAY (1) = 'A/D numbers'
      CHAR_ARRAY (2) = 'BIAS'
      CALL DSA_SET_DATA_INFO( 'OBS_RED', 2, CHAR_ARRAY, 0, 0.0D0, STATUS )
      CALL DSA_SET_OBJECT( 'OBS_RED', OBJECT_NAME, STATUS )

*    Record how the reduction went in the FITS structure of the reduced observation.
      CALL GEN_TIME( 6, DAY, LDAY, DATE, LDATE, HOUR, LHOUR )
      TIME = DATE(1:LDATE)//' at '//HOUR(1:LHOUR)
      CLEN = MAX( 1, CHR_LEN( TIME ) )
      CALL DSA_PUT_FITS_C( 'OBS_RED', 'STREDUCE', TIME(1:CLEN), ' ', STATUS )
      CALL DSA_PUT_FITS_F( 'OBS_RED', 'EXPOSED', OBSERVATION_TIME, ' ', STATUS )
      CALL DSA_SET_EXPOSURE( 'OBS_RED', OBSERVATION_TIME, STATUS )
      CALL DSA_PUT_FITS_C( 'OBS_RED', 'MASKUSED', MASK(1:CHR_LEN(MASK)), ' ', STATUS )

*    Destination for error GOTOs
 500  CONTINUE
      END
