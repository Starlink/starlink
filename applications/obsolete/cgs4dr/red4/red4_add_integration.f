*+  RED4_ADD_INTEGRATION - coadds desired integration into observation
      SUBROUTINE RED4_ADD_INTEGRATION (STATUS)
*    Description :
*     This routine adds the result of an integration to the reduced
*     observation coadd. If the reduced integration cannot be found an
*     error message is output and no more is done. If an integration
*     is successfully added then a copy of its FITS structure is placed
*     in the COADDED_INTS structure of the reduced observation, both as
*     record that the integration has been added and of how it had been
*     reduced. All integrations are added with equal weight, any error
*     array associated with the integration (from ADP statistics) is ignored.
*     The error array of the reduce observation is calculated from the
*     dispersion of its component integrations about the mean.
*     If there is a record of the integration already having been coadded
*     then a warning message is output and no more done.
*
*     Most of the FITS header information will already have been copied
*     from the observation file, but the following items are updated
*     from the reduced integration file as follows :-
*
*     EXPOSED     - Value is accumulated (new = old + current)
*     RUTSTART    - Minimum value used   (new = MIN(old,current))
*     RUTEND      - Maximum value used   (new = MAX(old,current))
*     AMSTART     - Carried with RUTSTART to form AMSTART in observation
*     AMSTART     - Carried with RUTEND to form AMEND in observation
*    Invocation :
*     CALL RED4_ADD_INTEGRATION (STATUS)
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*    Method :
*     <description of how the subroutine works>
*    Deficiencies :
*     This code needs restructuring without GOTOs !
*     More source comments are needed.
*
*     The logic of this routine contains the following deficiencies :-
*
*     1. It is assumed that equal numbers of integrations will be added
*        at each of the detector positions. (The COADDS array could be
*        checked to ensure this is true).
*     2. It is assumed that integrations at other detector positions have
*        the same integration time as the ones at detector position 1.
*        (The total observation time is determined only from the latter).
*     3. The MASKUSED, BIASUSED, DARKUSED, FLATUSED, LINEARIS and
*        NORMALIS parameters written to the FITS structure of the reduced
*        observation file refer only to the most recent integration
*        added. They will refer to the observation as a whole only if
*        every integration has used the same BIAS/DARK/FLAT etc...
*        (Usually this is true).
*
*     The routine is inefficient in its determination of file names.
*     It converts the name of the integration into the name of the
*     reduced observation. It then converts this to the name of the
*     observation, passes this to RED4_MAKE_OBSREDFILE, which then
*     promptly converts this back into the name of the reduced
*     observation!
*
*     The routine will not reject an integration which has not been
*     reduced properly. Some check on STREDUCE need to be added.
*
*     DSA status values do not conform to the ADAM scheme. It has
*     therefore been necessary to trap these statuses before they
*     get translated into confusing messages like "exceeded quota".
*     The traps can be removed once DSA conforms.
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     John Lightfoot (REVAD::JFL)
*     Steven Beard.  (REVAD::SMB)
*     Phil Daly.     (JACH::PND)
*    History :
*     1989:  Original (JFL)
*     22-Jan-1990: Modified to overcome compilation problems
*                  in RED4_DO_COADD.                          (SMB)
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
*      6-Jul-1990: Made to write object name and exposure
*                  time in a standard Figaro structure (as
*                  well as the special CGS4 structure). (Why
*                  was this not done in the first place??).
*                  Code spaced out more.                      (SMB)
*     23-Jul-1990: Character handling improved, and modified
*                  to accept 4 digit observation numbers.     (SMB)
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
*     29-Aug-1990: Typing mistakes fixed.                     (SMB)
*     21-Aug-1990: Phase 2 of major changes: Header
*                  information in the integration and reduced
*                  integration files is now written in a .FITS
*                  structure. MAXINDDIM, MAXOBSDIM and
*                  MAXINTDIM parameters added. Bug in
*                  calculation of observation time fixed.    (SMB)
*      7-Sep-1990: Modified to obtain the observation type
*                  and to set the NORMALIS flag in the FITS
*                  structure if the integration is a FLAT,
*                  for consistency.                          (SMB)
*      7-Sep-1990: Output made less verbose.                 (SMB)
*     13-Sep-1990: Modified so that data reduction information
*                  is copied to the main FITS structure of
*                  the reduced observation file.             (SMB)
*     14-Sep-1990: Description of deficiencies added.        (SMB)
*     14-Sep-1990: Code simplified by encapsulating low-level
*                  DTA calls in RED4_COPY_STRUCTURE.         (SMB)
*     23-Oct-1990: Commented out code removed.               (SMB)
*     25-Oct-1990: Bug fix: It has been discovered that the
*                  RUTSTART and RUTEND parameters in the
*                  observation file are not written until the
*                  last integration has finished, so they
*                  cannot be obtained from this file when
*                  reducing integration by integration. Code
*                  modified to obtain these parameters from
*                  the reduced integration file, using the
*                  same algorithm as RED4_ADD_OBSERVATION_2. (SMB)
*     29-Nov-1990: Unnecessarily large character variables
*                  reduced in size.                          (SMB)
*     29-Nov-1990: Made to copy linearisation coefficients
*                  to reduced observation FITS structure.    (SMB)
*      3-Jan-1991: Bug in character handling fixed.          (SMB)
*     20-Jan-1991: Integration files no longer have an AMEND
*                  parameter. It is now assumed that the air
*                  mass is constant throughout an integration.
*                  AMSTART is now propagated instead.        (SMB)
*     15-Feb-1991: Asterisks added to message if integration
*                  has already been added.                   (SMB)
*     24-Feb-1991: DSA error statuses trapped, as these do
*                  not conform to the ADAM error scheme.     (SMB)
*      2-Aug-1991: Copy normalisation method to reduced
*                  observation.                              (SMB)
*     18-Feb-1993: Conform to error strategy                 (PND)
*      9-Nov-1994: Try to make more portable                 (AB)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'                      ! Contains SAI__ERROR
      INCLUDE 'RED4_COMMON.INC'
*    Status :
      INTEGER STATUS
*    External references :
      INTEGER ICH_LEN                        ! Figaro string length function
*    Local Constants :
      INTEGER MAXOBSDIM                      ! Maximum observation dimensions
      PARAMETER ( MAXOBSDIM = 2 )
      INTEGER MAXINTDIM                      ! Maximum integration dimensions
      PARAMETER ( MAXINTDIM = 2 )
      INTEGER DTA__OK                    ! DTA success status
      PARAMETER ( DTA__OK = 0 )
*    Local variables :
      LOGICAL EXIST                          ! T if reduced observation file
*                                            !    exists
      LOGICAL VARIANCE_MAP                   ! T if variance array mapped.
      LOGICAL FITS_EXIST                     ! T if FITS item exists
      INTEGER DSA_STATUS                     ! DSA status value
      INTEGER ADDRESS                        ! DSA mapping stuff
      INTEGER INDEX_PTR                      ! for index array...
      INTEGER INDEX_SLOT                     !        "
      INTEGER COADDS_PTR                     ! for coadds array
      INTEGER COADDS_SLOT                    !        "
      INTEGER OBSDATA_PTR                    ! for observation data array
      INTEGER OBSDATA_SLOT                   !        "
      INTEGER OBSVAR_PTR                     ! for observation variance array
      INTEGER OBSVAR_SLOT                    !        "
      INTEGER OBSQUAL_PTR                    ! for observation quality array
      INTEGER OBSQUAL_SLOT                   !        "
      INTEGER INTDATA_PTR                    ! for integration data array
      INTEGER INTDATA_SLOT                   !        "
      INTEGER INTQUAL_PTR                    ! for integration quality array
      INTEGER INTQUAL_SLOT                   !        "
      INTEGER INDEX_DIMS( MAXINDDIM )        ! dimensions of index array
      INTEGER OBSDIMS( MAXOBSDIM )           ! dims of observation data array
      INTEGER INTDIMS( MAXINTDIM )           ! dims of reduced integration
*                                            !    data array
      INTEGER DET_INDEX                      ! the index of the detector position
*                                            !    the integration was taken at
      INTEGER LDAY, LDATE, LHOUR             ! lengths of date strings
      INTEGER DTA_STATUS                     !
      INTEGER NELM_IND                       !
      INTEGER NELM_INT                       !
      INTEGER NELM_OBS                       !
      INTEGER NDIM                           !
      INTEGER N                              !
      INTEGER I                              ! Loop counter
      INTEGER CLEN                           ! Non-blank length of character string
      INTEGER CPOS                           ! Character position
      INTEGER IGNORE                         ! unimportant parameter
      INTEGER ELEMENTS                       ! Number of elements in FITS item
      INTEGER STRLEN                         ! Size of character FITS item
      INTEGER ORDER                          ! Normalisation polynomial order
      INTEGER BOXSIZE                        ! Normalisation box size
      DOUBLE PRECISION DIGNORE               ! Ignored variable
      DOUBLE PRECISION COEFF                 ! Linearisation coefficient
      REAL INTEGRATION_TIM                   ! integration time in secs of the
*                                            !    reduced integration being added
      REAL OBSERVATION_TIM                   ! total integration time of the
*                                            !    points at detector index 1 in
*                                            !    the reduced observation
      REAL
     :  RUTSTART,                  ! UT at start of observation.
     :  RUTEND,                    ! UT at end of observation.
     :  INTRUTSTART,               ! UT at start of integration.
     :  INTRUTEND,                 ! UT at end of integration.
     :  INTAMSTART                 ! Air mass during integration.
      CHARACTER*80 OBSFILE                   ! name of observation file
      CHARACTER*80 OBSREDFILE                ! name of reduced observation file
      CHARACTER*80 INTFILE                   ! basic name of integration file
*                                            !    (unreduced)
      CHARACTER*80 INTREDFILE                ! full name of intfile i.e. RDIR:
*                                            !    intfile
      CHARACTER*80 COADDED_INTS              ! DTA name of structure in reduced
*                                            !    observation file holding info
*                                            !    on integrations already added
      CHARACTER*80 COADD_NAME                ! name of structure in COADDED_INTS
*                                            !    detailing integration added
      CHARACTER*80 COADD_TEST                ! The name of the COADD structure
*                                            !    which would correspond to
*                                            !    a given integration.
      CHARACTER*20 OBS_TYPE                  ! The observation type (BIAS,
*                                            !    DARK, FLAT etc...)
      CHARACTER*40 OBJECT_NAME               ! The name of the object.
      CHARACTER*32 INT_INFO(2)               ! units and title of integration data
      CHARACTER*40 TIME                      ! String holding date of last
*                                            !    addition
      CHARACTER*80 BUFFER                    ! Buffer used for copying the name
*                                            !    of the mask, BIAS, DARK or
*                                            !    FLAT used.
      CHARACTER*20 DAY, DATE, HOUR           ! Components of date
      CHARACTER*3 LINEARIS                   ! LINEARIS item from FITS structure
      CHARACTER*3 NORMALIS                   ! NORMALIS item from FITS structure
      CHARACTER*20 NORM_METHOD               ! Normalisation method
      CHARACTER*8 ITEM                       ! Name of FITS item
      CHARACTER*4 ACCESS                     ! Access code for FITS item
      CHARACTER*4 COMMENT                    ! Dummy comment
*    Internal References :
*    Local data :
*-

*    Return if entry status bad
      IF (STATUS .NE. ADAM__OK) RETURN

*    Initialisation of DSA_ routines
      DSA_STATUS = ADAM__OK
      CALL DSA_OPEN( DSA_STATUS )

      IF ( DSA_STATUS .NE. ADAM__OK ) THEN

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_ADD_INTEGRATION: '/
     :      /'Error opening DSA', STATUS )
      END IF

*    Get the name of the integration file whose reduced counterpart is to be
*    coadded.
      CALL PAR_GET0C ('INTFILE', INTFILE, STATUS)

*    Construct the name of the reduced observation and reduced integration
*    files. e.g. original integration file is I890816_1_1
*                reduced integration file is RI890816_1_1
*                observation file is          O890816_1
*                reduced observation file is RO890816_1
      CALL RED4_INTTORINT( INTFILE, INTREDFILE, STATUS )
      CALL RED4_INTTOROBS( INTFILE, OBSREDFILE, STATUS )

*    Look for the reduced observation file, create it if it does not
*    exist (it's simpler to do this with another incarnation of DSA
*    inside the MAKE routine, hence the closing and reopening of
*    on either side of it.
      DSA_STATUS = STATUS
      CALL DSA_SEEK_NAMED_STRUCTURE( OBSREDFILE, EXIST, DSA_STATUS )

      IF ( ( .NOT. EXIST ) .AND. ( DSA_STATUS .EQ. ADAM__OK ) ) THEN

         CALL RED4_INTTOOBS (INTFILE, OBSFILE, STATUS)

         DSA_STATUS = STATUS
         CALL DSA_CLOSE( DSA_STATUS )

         IF ( DSA_STATUS .NE. ADAM__OK )THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_ADD_INTEGRATION: '/
     :         /'First error closing DSA', STATUS )
         END IF
         CALL RED4_MAKE_OBSREDFILE( OBSFILE, STATUS )
         DSA_STATUS = STATUS
         CALL DSA_OPEN ( DSA_STATUS )
      ENDIF

*    Open reduced observation and integration files
      CALL RED4_CHECK_INPUT( OBSREDFILE, STATUS )
      CALL DSA_NAMED_INPUT ('OBSRED', OBSREDFILE, DSA_STATUS)
      CALL RED4_CHECK_INPUT( INTREDFILE, STATUS )
      CALL DSA_NAMED_INPUT ('INTRED', INTREDFILE, DSA_STATUS)

*    Determine the observation type and issue a message.
      CALL DSA_GET_FITS_C( 'OBSRED', 'OBSTYPE', 0, OBS_TYPE, COMMENT,
     :  DSA_STATUS )

      IF ( DSA_STATUS .NE. ADAM__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_ADD_INTEGRATION: '/
     :      /'Error getting FITS item OBSTYPE', STATUS )
      END IF

      CALL MSG_SETC( 'OBS_TYPE', OBS_TYPE )
      CALL MSG_SETC( 'INTREDFILE', INTREDFILE )
      CALL MSG_SETC( 'OBSREDFILE', OBSREDFILE )
      CALL MSG_OUT( ' ', 'Adding ^OBS_TYPE integration ^INTREDFILE '/
     :  /'to ^OBSREDFILE.', STATUS )

*    Open the COADDS structure holding info on how many coadds have gone into
*    the result array, map in its data array, and get the DTA name of the
*    associated .MORE.CGS4_COADDS structure that will tell us which
*    integrations have already been 'added'
      DSA_STATUS = STATUS
      CALL DSA_NAMED_INPUT ('COADDS', OBSREDFILE(:ICH_LEN(OBSREDFILE))//
     :   '.MORE.CGS4_COADDS', DSA_STATUS)

      CALL DSA_MAP_DATA ('COADDS', 'UPDATE', 'SHORT', ADDRESS,
     :   COADDS_SLOT, DSA_STATUS)
      COADDS_PTR = ADDRESS

      CALL DSA_SPECIFIC_STRUCTURE ('COADDS', 'COADDED_INTS',
     :   'UPDATE', COADDED_INTS, DSA_STATUS)

*    Search this structure for an occurence of the integration being added
*    now. The integration is only added if it is not already present.
      IF ( DSA_STATUS .NE. ADAM__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_ADD_INTEGRATION: '/
     :      /'Error searching for CGS4 specific structure', STATUS )
      END IF

      IF ( STATUS .EQ. ADAM__OK ) THEN

         N = 1
         DTA_STATUS = DTA__OK

*      Convert the integration name into its corresponding coadd
*      structure name
         CALL RED4_INTTOCOADD( INTFILE, COADD_TEST, STATUS )

         DO WHILE ( DTA_STATUS .EQ. DTA__OK )

*         Obtain the name of the next structure in the COADDED_INTS structure
            CALL DTA_NMVAR (COADDED_INTS, N, COADD_NAME, DTA_STATUS)

            IF ( DTA_STATUS .EQ. DTA__OK ) THEN

*            If the structure already exists, the integration has already
*            been added, and should be ignored.
               IF ( COADD_NAME .EQ. COADD_TEST ) THEN

                  CALL DSA_WRUSER ('****** This integration has ')
                  CALL DSA_WRUSER ('already been added - ignored '/
     :              /'******.\N')
                  GOTO 500
               ENDIF
            ENDIF

            N = N + 1
         END DO
      ENDIF

      DTA_STATUS = DTA__OK

*    Get the detector position index of this particular integration
      DSA_STATUS = STATUS
      CALL DSA_GET_FITS_I( 'INTRED', 'DINDEX', 0, DET_INDEX, COMMENT,
     :  DSA_STATUS )

*    If the detector index is 1, read in the INTEGRATION_TIMe of the
*    reduced integration and the same from the reduced observation file
*    The INTEGRATION_TIM object in the reduced observation file will
*    give the total integration time of the coadded points at detector
*    position 1.
      IF ( (DSA_STATUS .EQ. ADAM__OK) .AND. (DET_INDEX .EQ. 1) ) THEN

*       integration time for reduced integration
         CALL DSA_GET_FITS_F( 'INTRED', 'EXPOSED', 0, INTEGRATION_TIM,
     :     COMMENT, DSA_STATUS )

*       integration time for reduced observation
         CALL DSA_GET_FITS_F( 'OBSRED', 'EXPOSED', 0, OBSERVATION_TIM,
     :     COMMENT, DSA_STATUS )
      ENDIF

*   Abort if an error has occurred at this point. (This is done for
*   consistency with the old code. GOTOs should be removed eventually).
      IF ( DSA_STATUS .NE. ADAM__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_ADD_INTEGRATION: '/
     :      /'Error getting FITS items', STATUS )
      END IF

      IF ( STATUS .NE. ADAM__OK ) GOTO 500

*    Map in the index array
      CALL DSA_NAMED_INPUT ('INDEX', OBSREDFILE(:ICH_LEN(OBSREDFILE))//
     :   '.MORE.CGS4_INDEX', DSA_STATUS)

      CALL DSA_MAP_DATA ('INDEX', 'READ', 'SHORT', ADDRESS, INDEX_SLOT,
     :   DSA_STATUS)
      INDEX_PTR = ADDRESS

*    check that the index array can handle this index position (always
*    should do but the program will crash*?! if it doesn't)
      CALL DSA_DATA_SIZE ('INDEX', MAXINDDIM, NDIM, INDEX_DIMS,
     :  NELM_IND, DSA_STATUS)

      IF (DSA_STATUS .EQ. ADAM__OK) THEN

         IF ((INDEX_DIMS(2) .LT. DET_INDEX) .OR.
     :       (DET_INDEX .LT.1 ))THEN

            CALL DSA_WRUSER ('Detector position is outside range ')
            CALL DSA_WRUSER ('of the index array for this ')
            CALL DSA_WRUSER ('observation.\N')
            GOTO 500
         ENDIF
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_ADD_INTEGRATION: '/
     :      /'Error sizing data array', STATUS )
      ENDIF

*    Preparations are complete, ready to process the data.........
*    Use quality arrays, and map in the relevant data, quality, variances etc.,
*    for the reduced observation
      CALL DSA_USE_QUALITY ('OBSRED', DSA_STATUS)

      CALL DSA_MAP_DATA ('OBSRED', 'UPDATE', 'FLOAT', ADDRESS,
     :   OBSDATA_SLOT, DSA_STATUS)
      OBSDATA_PTR = ADDRESS

      CALL DSA_MAP_VARIANCE ('OBSRED', 'UPDATE', 'FLOAT', ADDRESS,
     :   OBSVAR_SLOT, DSA_STATUS)
      OBSVAR_PTR = ADDRESS

      IF ( DSA_STATUS .EQ. ADAM__OK ) THEN

         VARIANCE_MAP = .TRUE.
      ELSE

         VARIANCE_MAP = .FALSE.
      END IF

      CALL DSA_MAP_QUALITY ('OBSRED', 'UPDATE', 'BYTE', ADDRESS,
     :   OBSQUAL_SLOT, DSA_STATUS)
      OBSQUAL_PTR = ADDRESS

      CALL DSA_DATA_SIZE ('OBSRED', MAXOBSDIM, NDIM, OBSDIMS, NELM_OBS,
     :  DSA_STATUS)

*    and for the reduced integration (errors derived from CIRACS coadding
*    statistics are ignored)
      CALL DSA_USE_QUALITY ('INTRED', DSA_STATUS)

      CALL DSA_MAP_DATA ('INTRED', 'READ', 'FLOAT', ADDRESS,
     :   INTDATA_SLOT, DSA_STATUS)
      INTDATA_PTR = ADDRESS

      CALL DSA_MAP_QUALITY ('INTRED', 'READ', 'BYTE', ADDRESS,
     :   INTQUAL_SLOT, DSA_STATUS)
      INTQUAL_PTR = ADDRESS

      CALL DSA_DATA_SIZE ('INTRED', MAXINTDIM, NDIM, INTDIMS, NELM_INT,
     :  DSA_STATUS)

*    Call the routine that does the processing, the check for STATUS stops
*    and adjustable array error if the routine is entered with some arrays
*    not successfully mapped
      IF ( DSA_STATUS .NE. ADAM__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_ADD_INTEGRATION: '/
     :     /'Error mapping data, quality or variance', STATUS )
      END IF

      IF (STATUS .EQ. ADAM__OK) THEN

         CALL RED4_DO_COADD (%val(INTDATA_PTR),
     :                       %val(INTQUAL_PTR),
     :                       INTDIMS(1),
     :                       INTDIMS(2),
     :                       %val(INDEX_PTR),
     :                       INDEX_DIMS(1),
     :                       INDEX_DIMS(2),
     :                       DET_INDEX,
     :                       %val(OBSDATA_PTR),
     :                       %val(OBSVAR_PTR),
     :                       %val(OBSQUAL_PTR),
     :                       %val(COADDS_PTR),
     :                       OBSDIMS(1),
     :                       OBSDIMS(2),
     :                       STATUS)
      ENDIF

*    Check that the variance are non-negative, they'll crash DSA close
*    if they are. (The following code is executed even if the status
*    is bad at this point).
      IF ( VARIANCE_MAP .AND. (NELM_OBS .GT. 0) ) THEN

         CALL GEN_CLIPF (%val(OBSVAR_PTR),
     :      NELM_OBS, 0.0, 1.7E38, IGNORE, IGNORE,
     :      %val(OBSVAR_PTR))
      ENDIF

*    Set the data title and information in the output structure to be the
*    same as those in the input
      DSA_STATUS = STATUS
      CALL DSA_GET_DATA_INFO ('INTRED', 2, INT_INFO, 0, DIGNORE,
     :  DSA_STATUS)
      CALL DSA_SET_DATA_INFO ('OBSRED', 2, INT_INFO, 0, 0.0D0,
     : DSA_STATUS)

*    Set the object name in the output structure to be the same as
*    the input.
      CALL DSA_OBJECT_NAME( 'INTRED', OBJECT_NAME, DSA_STATUS )
      CALL DSA_SET_OBJECT( 'OBSRED', OBJECT_NAME, DSA_STATUS )

*    If all OK, copy the contents of the .FITS structure in the
*    integration file into a COADD structure corresponding to the integration
      CALL RED4_COPY_STRUCTURE( 'INTRED.'//FITS_STRUCTURE,
     :  COADDED_INTS(:ICH_LEN(COADDED_INTS))//'.'//COADD_TEST,
     :  STATUS )

*    If all OK and the detector index of the added integration is 1, then add
*    the integration's integration time to that of the observation, and
*    update the value in the observation reduction file (both FITS and
*    OBS structures).
      DSA_STATUS = STATUS
      IF ((STATUS .EQ. ADAM__OK) .AND. (DET_INDEX .EQ. 1)) THEN

         OBSERVATION_TIM = OBSERVATION_TIM + INTEGRATION_TIM

         CALL DSA_PUT_FITS_F( 'OBSRED', 'EXPOSED', OBSERVATION_TIM, ' ',
     :     DSA_STATUS )

         CALL DSA_SET_EXPOSURE( 'OBSRED', OBSERVATION_TIM, DSA_STATUS )
      ENDIF

*   If all is ok, update RUTSTART and RUTEND to be the minimum and maximum
*   respectively of those found to far (carry the AMSTART parameter with
*   these). RUTSTART and RUTEND will be zero if not defined.
      IF ( DSA_STATUS .EQ. ADAM__OK ) THEN

         CALL DSA_GET_FITS_F( 'OBSRED', 'RUTSTART', 0,
     :     RUTSTART, COMMENT, DSA_STATUS )
         CALL DSA_GET_FITS_F( 'OBSRED', 'RUTEND', 0,
     :     RUTEND, COMMENT, DSA_STATUS )

         CALL DSA_GET_FITS_F( 'INTRED', 'RUTSTART', 0,
     :     INTRUTSTART, COMMENT, DSA_STATUS )
         CALL DSA_GET_FITS_F( 'INTRED', 'RUTEND', 0,
     :     INTRUTEND, COMMENT, DSA_STATUS )

         CALL DSA_GET_FITS_F( 'INTRED', 'AMSTART', 0,
     :     INTAMSTART, COMMENT, DSA_STATUS )

         IF ( ( RUTSTART .LE. 0.0 ) .OR.
     :        ( INTRUTSTART .LT. RUTSTART ) ) THEN

            CALL DSA_PUT_FITS_F( 'OBSRED', 'RUTSTART',
     :        INTRUTSTART, ' ', DSA_STATUS )

            CALL DSA_PUT_FITS_F( 'OBSRED', 'AMSTART',
     :        INTAMSTART, ' ', DSA_STATUS )
         END IF

         IF ( ( RUTEND .LE. 0.0 ) .OR.
     :        ( INTRUTEND .GT. RUTEND ) ) THEN

            CALL DSA_PUT_FITS_F( 'OBSRED', 'RUTEND',
     :        INTRUTEND, ' ', DSA_STATUS )

            CALL DSA_PUT_FITS_F( 'OBSRED', 'AMEND',
     :        INTAMSTART, ' ', DSA_STATUS )
         END IF
      END IF

*    Copy the names of the bad pixel mask, and the BIAS, DARK and FLAT
*    frames used, to the .FITS structure of the reduced observation
*    file, provided these items exist in the reduced integration file.
*    (Note that these will overwrite any existing items in the reduced
*    observation file. If several integrations are added, the parameters
*    of the last integration added will remain. This will not matter if
*    all the integrations have been reduced in the same way).
      CALL DSA_SEEK_FITS( 'INTRED', 'MASKUSED', FITS_EXIST, ACCESS,
     :  ELEMENTS, STRLEN, DSA_STATUS )

      IF ( FITS_EXIST ) THEN

         CALL DSA_GET_FITS_C( 'INTRED', 'MASKUSED', 0, BUFFER, COMMENT,
     :     DSA_STATUS )
         CLEN = MAX( 1, ICH_LEN( BUFFER ) )
         CALL DSA_PUT_FITS_C( 'OBSRED', 'MASKUSED', BUFFER(1:CLEN), ' ',
     :     DSA_STATUS )
      END IF

      CALL DSA_SEEK_FITS( 'INTRED', 'BIASUSED', FITS_EXIST, ACCESS,
     :  ELEMENTS, STRLEN, DSA_STATUS )

      IF ( FITS_EXIST ) THEN

         CALL DSA_GET_FITS_C( 'INTRED', 'BIASUSED', 0, BUFFER, COMMENT,
     :     DSA_STATUS )
         CLEN = MAX( 1, ICH_LEN( BUFFER ) )
         CALL DSA_PUT_FITS_C( 'OBSRED', 'BIASUSED', BUFFER(1:CLEN), ' ',
     :     DSA_STATUS )
      END IF

      CALL DSA_SEEK_FITS( 'INTRED', 'DARKUSED', FITS_EXIST, ACCESS,
     :  ELEMENTS, STRLEN, DSA_STATUS )

      IF ( FITS_EXIST ) THEN

         CALL DSA_GET_FITS_C( 'INTRED', 'DARKUSED', 0, BUFFER, COMMENT,
     :     DSA_STATUS )
         CLEN = MAX( 1, ICH_LEN( BUFFER ) )
         CALL DSA_PUT_FITS_C( 'OBSRED', 'DARKUSED', BUFFER(1:CLEN), ' ',
     :     DSA_STATUS )
      END IF

      CALL DSA_SEEK_FITS( 'INTRED', 'FLATUSED', FITS_EXIST, ACCESS,
     :  ELEMENTS, STRLEN, DSA_STATUS )

      IF ( FITS_EXIST ) THEN

         CALL DSA_GET_FITS_C( 'INTRED', 'FLATUSED', 0, BUFFER, COMMENT,
     :     DSA_STATUS )
         CLEN = MAX( 1, ICH_LEN( BUFFER ) )
         CALL DSA_PUT_FITS_C( 'OBSRED', 'FLATUSED', BUFFER(1:CLEN), ' ',
     :     DSA_STATUS )
      END IF

*    Copy the LINEARIS flag from the reduced integration file to
*    the .FITS structure of the reduced observation file, if this item
*    exists in the reduced integration file.
      CALL DSA_SEEK_FITS( 'INTRED', 'LINEARIS', FITS_EXIST, ACCESS,
     :  ELEMENTS, STRLEN, DSA_STATUS )

      IF ( FITS_EXIST ) THEN

         CALL DSA_GET_FITS_C( 'INTRED', 'LINEARIS', 0, LINEARIS,
     :     COMMENT, DSA_STATUS )
         CALL DSA_PUT_FITS_C( 'OBSRED', 'LINEARIS', LINEARIS, ' ',
     :     DSA_STATUS )

*      Copy the linearisation coefficients, if these exist.
         I = 1
         CPOS = 0
         CALL CHR_PUTC( 'LINCF', ITEM, CPOS )
         CALL CHR_PUTI( I, ITEM, CPOS )

         CALL DSA_SEEK_FITS( 'INTRED', ITEM(1:CPOS), FITS_EXIST, ACCESS,
     :     ELEMENTS, STRLEN, DSA_STATUS )

         DO WHILE ( (DSA_STATUS .EQ. ADAM__OK) .AND. (FITS_EXIST) )

            CALL DSA_GET_FITS_D( 'INTRED', ITEM(1:CPOS), 0, COEFF,
     :        COMMENT, DSA_STATUS )
            CALL DSA_PUT_FITS_D( 'OBSRED', ITEM(1:CPOS), COEFF, ' ',
     :        DSA_STATUS )

            I = I + 1
            CPOS = 0
            CALL CHR_PUTC( 'LINCF', ITEM, CPOS )
            CALL CHR_PUTI( I, ITEM, CPOS )

            CALL DSA_SEEK_FITS( 'INTRED', ITEM(1:CPOS), FITS_EXIST,
     :        ACCESS, ELEMENTS, STRLEN, DSA_STATUS )
         END DO
      END IF

*    If no errors have occurred, and the observation being reduced is
*    a FLAT, update the NORMALIS flag in the .FITS structure of the
*    reduced observation file, together with the normalisation method
*    used.
      IF ( (DSA_STATUS .EQ. ADAM__OK) .AND.
     :     (OBS_TYPE .EQ. 'FLAT') ) THEN

         CALL DSA_GET_FITS_C( 'INTRED', 'NORMALIS', 0, NORMALIS,
     :     COMMENT, DSA_STATUS )
         CALL DSA_PUT_FITS_C( 'OBSRED', 'NORMALIS', NORMALIS,
     :     ' ', DSA_STATUS )

         CALL DSA_GET_FITS_C( 'INTRED', 'NMETHOD', 0, NORM_METHOD,
     :     COMMENT, DSA_STATUS )
         CLEN = MAX( 1, ICH_LEN( NORM_METHOD ) )
         CALL DSA_PUT_FITS_C( 'OBSRED', 'NMETHOD', NORM_METHOD(1:CLEN),
     :     ' ', DSA_STATUS )

         IF ( NORM_METHOD .EQ. 'POLYFIT' ) THEN

            CALL DSA_GET_FITS_I( 'INTRED', 'NORDER', 0, ORDER,
     :        COMMENT, DSA_STATUS )
            CALL DSA_PUT_FITS_I( 'OBSRED', 'NORDER', ORDER,
     :        ' ', DSA_STATUS )

         ELSE IF ( NORM_METHOD .EQ. 'SMOOTH' ) THEN

            CALL DSA_GET_FITS_I( 'INTRED', 'NBOXSIZE', 0, BOXSIZE,
     :        COMMENT, DSA_STATUS )
            CALL DSA_PUT_FITS_I( 'OBSRED', 'NBOXSIZE', BOXSIZE,
     :        ' ', DSA_STATUS )
         END IF
      END IF

*    Finally, if all OK update the date of last access to the reduced
*    observation
      IF (DSA_STATUS .EQ. ADAM__OK) THEN

         CALL GEN_TIME (6, DAY, LDAY, DATE, LDATE, HOUR, LHOUR)
         TIME = DATE(:LDATE)//' at '//HOUR(:LHOUR)
         CLEN = MAX( 1, ICH_LEN( TIME ) )

         CALL DSA_PUT_FITS_C( 'OBSRED', 'STREDUCE', TIME(1:CLEN),
     :     ' ', DSA_STATUS )
      ENDIF

 500  CONTINUE

*   Close down DSA and tidy up.
      CALL DSA_CLOSE (DSA_STATUS)

      IF ( DSA_STATUS .NE. ADAM__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_ADD_INTEGRATION: '/
     :      /'Second error closing DSA', STATUS )
      END IF

      END
