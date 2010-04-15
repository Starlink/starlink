*+ RED4_MAKE_OBSREDFILE - Create the reduction file for a particular observation
      SUBROUTINE RED4_MAKE_OBSREDFILE (OBSFILE, STATUS)
*    Description :
*     This subroutine creates the reduction container file for a specified
*      observation
*    Invocation :
*     CALL RED4_MAKE_OBSREDFILE (OBSFILE, STATUS)
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*    Method :
*     <description of how the subroutine works>
*    Deficiencies :
*     DSA status values do not conform to the ADAM scheme. It has
*     therefore been necessary to trap these statuses before they
*     get translated into confusing messages like "exceeded quota".
*     The traps can be removed once DSA conforms.
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     J F Lightfoot (REVAD::JFL)
*     S M Beard     (REVAD::SMB)
*     P N Daly      (JACH::PND)
*    History :
*          1989 ?: Original version.                            (JFL)
*      2-Mar-1990: History added. Bug fix: The RANK array was
*                  mapped as SHORT but declared as INTEGER
*                  inside RED4_SETUP_AXIS1. Now mapped an INT.  (SMB)
*     24-Apr-1990: Because of memory corruption problems, the
*                  code needs to be compiled with array bounds
*                  checking switched on. The Figaro dynamic
*                  memory functions  (DYN_ELEMENT, DYNAMIC_MEM,
*                  DYN_INCREMENT) would not allow this. Code
*                  modified to use %val () instead.             (SMB)
*     23-Jul-1990: Unused variables removed. Character handling
*                  improved to allow 4 digit observation
*                  numbers. Code spaced out more.               (SMB)
*     21-Aug-1990: Phase 1 of major changes: Header
*                  information in the observation and reduced
*                  observation files is now written in a .FITS
*                  structure.                                   (SMB)
*     14-Sep-1990: Code simplified by encapsulating low-level
*                  DTA calls in RED4_COPY_STRUCTURE.            (SMB)
*     19-Sep-1990: BAD and MAXDIMS parameters added. Hard-wired
*                  4 and 2 replaced by FLOATSIZE and SHORTSIZE. (SMB)
*     23-Oct-1990: Commented out code removed.                  (SMB)
*      9-Nov-1990: Bug fix. An integer array was being passed
*                  to RED4_SETUP_AXIS1 when only a scalar
*                  integer was expected.  (How this ever worked
*                  I don't know!).                              (SMB)
*     21-Nov-1990: Wavelength calibration added. The X axis
*                  array can now be written with column numbers
*                  or with wavelength. Unused variables removed.(SMB)
*     23-Nov-1990: DSA_SPECIFIC_STRUCTURE was producing a
*                  warning message about it being bad practise
*                  to treat .X as an application-specific
*                  structure. Replaced with simple string
*                  concatenation.                               (SMB)
*     23-Nov-1990: INDEX array initialised to zero, as it was
*                  causing spurious numeric errors to appear
*                  whenever an error occurred.                  (SMB)
*     13-Feb-1991: STREDUCE item initialised.                   (SMB)
*     22-Feb-1991: Modified to allow "supersampling".           (SMB)
*     23-Feb-1991: Mistakes corrected. The data array should be
*                  slightly larger when supersampling. Traps
*                  included for invalid oversampling parameters.(SMB)
*     23-Feb-1991: Reset DET_INCR, to get around occasional
*                  problems with data acquisition system.       (SMB)
*     23-Feb-1991: More mistakes fixed.                         (SMB)
*     24-Feb-1991: DSA error statuses trapped, as these do
*                  not conform to the ADAM error scheme.        (SMB)
*      8-Oct-1991: Call RED4_GET_OBSERVATION instead of
*                  RED4_SEEK_OBSERVATION, so that calibration
*                  observations can be specified explicitly when
*                  required (DRIMP/5.1 and 5.2).                (SMB)
*     22-Feb-1993: Conform to error strategy                    (PND)
*     17-Dec-1993: Update for NDFs and use RESHAPE_DATA         (PND)
*     11-Jan-1994: Replace COERCE_DATA_ARRAY with RESHAPE_DATA  (PND)
*      2-Mar-1994: Update for IRCAM                             (PND)
*     23-Mar-1994: Update for IRCAM3                            (PND)
*     17-Apr-1995: Allow any array size for wavelength calib    (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Input:
      CHARACTER*(*) OBSFILE                ! The name of the observation file
*    Status :
      INTEGER STATUS
*    External references :
      INTEGER ICH_LEN                      ! Figaro string functions
      INTEGER DSA_TYPESIZE                 ! DSA type size function
*    Global variables :
      INCLUDE 'RED4_COMMON.INC'            ! RED4 common block
*    Local Constants :
      INTEGER MAXDIMS                      ! Maximum number of dimensions
      PARAMETER ( MAXDIMS = 2 )
      INTEGER MAXAXDIMS                    ! Maximum number of dimensions
      PARAMETER ( MAXAXDIMS = 5 )
      INTEGER MAXINDDIMS                   ! Maximum number of dimensions
      PARAMETER ( MAXINDDIMS = 2 )
      INTEGER NINFO                        ! Number of AXIS info items.
      PARAMETER ( NINFO = 2 )
      INTEGER BAD                          ! Bad quality value
      PARAMETER ( BAD = 1 )
*    Local variables :
      INTEGER CPOS                         ! Position in character string
      INTEGER BYTESIZE                     ! Number of bytes per BYTE value
      INTEGER FLOATSIZE                    ! Number of bytes per FLOAT value
      INTEGER INTSIZE                      ! Number of bytes per INT value
      INTEGER SHORTSIZE                    ! Number of bytes per SHORT value.
      INTEGER DATA_SLOT                    ! ... for data array
      INTEGER DATA_PTR                     !                "
      INTEGER VAR_SLOT                     ! ... for variance array
      INTEGER VAR_PTR                      !                "
      INTEGER QUAL_SLOT                    ! ... for quality array
      INTEGER QUAL_PTR                     !                "
      INTEGER COADDS_SLOT                  ! ... for COADDS array
      INTEGER COADDS_PTR                   !                "
      INTEGER AXIS1_SLOT                   ! ... for AXIS1 array
      INTEGER AXIS1_PTR                    !                "
      INTEGER INDEX_SLOT                   ! ... for index array
      INTEGER INDEX_PTR                    !                "
      INTEGER DET_NINCR                    ! the number of detector positions
*                                               in this observation
      INTEGER DET_SIZE( MAXDIMS )          ! the size of the detector array
      INTEGER SUPERSAMPLING                ! Supersampling factor.
      INTEGER OVERSAMPLING                 ! Oversampling factor.
      INTEGER DIMS( MAXDIMS )              ! dimensions of main result array
      INTEGER AXDIMS( MAXAXDIMS )          ! dimensions of main result array
      INTEGER NELM                         ! the total number of elements in the
      INTEGER AXNELM                       ! the total number of elements in the
      INTEGER AXNDIM                       ! the total number of dimensions in the
      INTEGER IND_DIMS( MAXINDDIMS )       ! dimensions of the index array
      INTEGER IND_NELM                     ! Number of elements in index array
      DOUBLE PRECISION DIGNORE             ! Ignored parameter
      REAL DET_INCR                        ! the size of the step between
*                                               the detector translation positions
*                                               making up this observation
      REAL DPPIXEL                         ! Number of detector positions per
*                                               pixel (should be whole no.)
      REAL GLAMBDA                         ! Grating wavelength. This is the
*                                          !    wavelength at the centre of
*                                          !    pixel number 31.
      REAL GDISP                           ! Grating dispersion in microns
*                                          !    per pixel (when not oversampled)
      LOGICAL PROCEED_LAMBDA               ! T if the data are to be
*                                          !    wavelength calibrated
      CHARACTER*80 OBSRED                  ! name of reduced observation file
      CHARACTER*80 INDEX_FILE              ! The name of the relevant index file.
      CHARACTER*80 CALIB_NAME              ! The name of a suitable calibration
*                                          !    observation
      CHARACTER*32 CHAR_ARRAY( NINFO )     ! Array to hold axes info
      CHARACTER*20 OBS_TYPE                ! The type of the observation
      CHARACTER*20 TO_WAVELENGTH           ! Should the data be wavelength
*                                          !   calibrated (YES, NO or ASK) ?
      CHARACTER*20 LAMBDA_METHOD           ! Wavelength calibration method
*                                               (ESTIMATED or CALIBRATED)
      CHARACTER*20 INSTRUMENT              ! IRCAM (ALICE) or CGS4
      CHARACTER*4 COMMENT                  ! Dummy comment
      CHARACTER*4 ACCESS
      INTEGER     ELEMENTS
      INTEGER     STRLEN
      LOGICAL     EXIST
*    Internal References :
*    Local data :
*-

*    Return if entry status bad
      IF ( STATUS.NE.SAI__OK ) RETURN

*    Open the DSA system
      CALL DSA_OPEN (STATUS)

*    Determine the number of bytes per element in datatypes
      FLOATSIZE = DSA_TYPESIZE( 'FLOAT', STATUS )
      BYTESIZE  = DSA_TYPESIZE( 'BYTE', STATUS )
      INTSIZE   = DSA_TYPESIZE( 'INT', STATUS )
      SHORTSIZE = DSA_TYPESIZE( 'SHORT', STATUS )

*    Input the observation file
      CALL RED4_CHECK_INPUT( OBSFILE, STATUS )
      CALL DSA_NAMED_INPUT ('OBSERVATION', OBSFILE, STATUS)
      IF ( STATUS.NE.SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_MAKE_OBSREDFILE: '/
     :     /'Error opening input observation', STATUS )
      END IF

*    Construct a reduced observation filename
      CALL RED4_OBSTOROBS( OBSFILE, OBSRED, STATUS )
      IF ( STATUS.NE.SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'OBSFILE', OBSFILE )
         CALL MSG_SETC( 'OBSRED', OBSRED )
         CALL ERR_REP( ' ', 'RED4_MAKE_OBSREDFILE: '/
     :     /'Error converting from ^OBSFILE to ^OBSRED', STATUS )
      ELSE
         CALL MSG_SETC( 'OBSRED', OBSRED )
         CALL MSG_OUT( ' ', 'Creating reduced observation file '/
     :     /'^OBSRED', STATUS )
      END IF

*    Open an output file based upon the template
      CALL DSA_NAMED_INPUT ('OBSRED_TEMPLATE', OBSRED_TEMPLATE, STATUS)
      CALL DSA_NAMED_OUTPUT ('OBSRED', OBSRED, 'OBSRED_TEMPLATE',
     :   0, 1, STATUS)

*    Tell DSA to use quality rather than magic numbers
      CALL DSA_USE_QUALITY ('OBSRED', STATUS)

*   Copy over the entire contents of the FITS structure
      CALL RED4_COPY_STRUCTURE( 'OBSERVATION.'//FITS_STRUCTURE,
     :   'OBSRED.'//FITS_STRUCTURE, STATUS )
      IF ( STATUS.NE.SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_MAKE_OBSREDFILE: '/
     :     /'Error copying FITS structure', STATUS )
      END IF

*    Initialise the STREDUCE item to "(not properly reduced)".
      CALL DSA_PUT_FITS_C( 'OBSRED', 'STREDUCE',
     :  '(not properly reduced)', ' ', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_MAKE_OBSREDFILE: '/
     :     /'Error setting FITS item STREDUCE', STATUS )
      END IF

*    Now determine the size of the data set, and create it
      IF (STATUS .EQ. SAI__OK) THEN

*       get number of detector positions
         EXIST = .FALSE.
         CALL DSA_SEEK_FITS( 'OBSERVATION', 'DETNINCR', EXIST,
     :      ACCESS, ELEMENTS, STRLEN, STATUS )
         IF ( EXIST ) THEN
            CALL DSA_GET_FITS_I( 'OBSERVATION', 'DETNINCR', 0,
     :        DET_NINCR, COMMENT, STATUS )
         ELSE
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_MAKE_OBSREDFILE: '/
     :        /'FITS item DETNINCR does not exist!', STATUS )
            GOTO 500
         END IF

         IF (DET_NINCR .LT. 1) THEN

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_MAKE_OBSREDFILE: '/
     :        /'The number of detector steps is '/
     :        /'less than 1', STATUS )
            GOTO 500
         ENDIF

*       the size of the steps
         EXIST = .FALSE.
         CALL DSA_SEEK_FITS( 'OBSERVATION', 'DETINCR', EXIST,
     :      ACCESS, ELEMENTS, STRLEN, STATUS )
         IF ( EXIST ) THEN
            CALL DSA_GET_FITS_F( 'OBSERVATION', 'DETINCR', 0, DET_INCR,
     :        COMMENT, STATUS )
         ELSE
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_MAKE_OBSREDFILE: '/
     :        /'FITS item DETINCR does not exist!', STATUS )
            GOTO 500
         ENDIF

         IF (DET_INCR .EQ. 0.0) THEN

            DET_INCR = 1.0
            CALL MSG_OUT( ' ', 'WARNING - '/
     :        /'The detector increment is '/
     :        /'zero (reset to 1.0)', STATUS )
         ENDIF

*      If there is only one detector step, DET_INCR should be 1.
*      Give a warning and reset it if it is not.
         IF ( DET_NINCR .LE. 1 ) THEN
            IF ( DET_INCR .GT. 1.0 ) THEN

               CALL MSG_OUT( ' ', 'WARNING - There is only one '/
     :           /'detector step, but the detector', STATUS )
               CALL MSG_SETR( 'DET_INCR', DET_INCR )
               CALL MSG_OUT( ' ', '          increment is ^DET_INCR. '/
     :           /'It has been reset to 1.0.', STATUS )

               DET_INCR = 1.0
               CALL DSA_PUT_FITS_F( 'OBSERVATION', 'DETINCR',
     :           DET_INCR, ' ', STATUS )
            END IF
         END IF
      ENDIF

*    the size of the detector
      CALL DSA_GET_FITS_I( 'OBSERVATION', 'DCOLUMNS', 0, DET_SIZE(1),
     :  COMMENT, STATUS )
      CALL DSA_GET_FITS_I( 'OBSERVATION', 'DROWS', 0, DET_SIZE(2),
     :  COMMENT, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_MAKE_OBSREDFILE: '/
     :     /'Error getting detector size from FITS items', STATUS )
         GOTO 500
      END IF

*   Determine the supersampling factor and oversampling factor.
*   (If DET_NINCR is 1 there is no SUPERSAMPLING, regardless of DET_INCR).
      IF ( DET_NINCR .GT. 1 ) THEN

         SUPERSAMPLING = NINT( DET_NINCR * DET_INCR )
      ELSE

         SUPERSAMPLING = 1
      END IF
      OVERSAMPLING = DET_NINCR / SUPERSAMPLING

*   If supersampling, check that there are a whole number of detector
*   positions per pixel.
      IF ( SUPERSAMPLING .GT. 1 ) THEN

         DPPIXEL= REAL( DET_NINCR ) / REAL( SUPERSAMPLING )

         IF ( (DPPIXEL - INT( DPPIXEL )) .GT. 1.0E-4 ) THEN

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_MAKE_OBSREDFILE: '/
     ;         /'There are not a whole number '/
     :         /'of detector positions per pixel', STATUS )
            GOTO 500
         END IF
      END IF

*   Calculate the size of the data array and reshape it
      DIMS(1) = OVERSAMPLING * ( DET_SIZE(1) + SUPERSAMPLING - 1 )
      DIMS(2) = DET_SIZE(2)
      NELM = DIMS(1) * DIMS(2)
      CALL DSA_RESHAPE_DATA ('OBSRED', 'OBSRED', MAXDIMS, DIMS,
     :  STATUS)
      IF (STATUS .NE. SAI__OK) THEN

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_MAKE_OBSREDFILE: '/
     :      /'Failed to reshape data', STATUS )
         GOTO 500
      ENDIF

*   Map the data and initialie it
      CALL DSA_MAP_DATA ('OBSRED', 'WRITE', 'FLOAT', DATA_PTR,
     :   DATA_SLOT, STATUS)
      IF (STATUS .EQ. SAI__OK) THEN

         IF ( VERBOSE ) CALL MSG_OUT( ' ',
     :     'Initialising reduced observation data array', STATUS )
         CALL GEN_CFILL (1, NELM, 0.0, %val(DATA_PTR))
      ENDIF

*   Map the variance and initialie it
      CALL DSA_MAP_VARIANCE ('OBSRED', 'WRITE', 'FLOAT', VAR_PTR,
     :   VAR_SLOT, STATUS)
      IF (STATUS .EQ. SAI__OK) THEN

         IF ( VERBOSE ) CALL MSG_OUT( ' ',
     :     'Initialising reduced observation variance array', STATUS )
         CALL GEN_CFILL (1, NELM, 0.0, %val(VAR_PTR))
      ENDIF

*   Map the quality and initialie it
      CALL DSA_MAP_QUALITY ('OBSRED', 'WRITE', 'BYTE', QUAL_PTR,
     :   QUAL_SLOT, STATUS)
      IF (STATUS .EQ. SAI__OK) THEN

         IF ( VERBOSE ) CALL MSG_OUT( ' ',
     :     'Initialising reduced observation quality array', STATUS )
         CALL GEN_FILL (NELM, BAD, %val(QUAL_PTR))
      ENDIF

*    Open the coadds structure, create the same size array in that
*    and fill it with zeros
      CALL DSA_NAMED_INPUT ('COADDS', OBSRED(:ICH_LEN(OBSRED))//
     :  '.MORE.CGS4_COADDS', STATUS)
      CALL DSA_COERCE_DATA_ARRAY ('COADDS', 'SHORT', MAXDIMS, DIMS,
     :  STATUS)
      CALL DSA_MAP_DATA ('COADDS', 'WRITE', 'SHORT', COADDS_PTR,
     :   COADDS_SLOT, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN

         IF ( VERBOSE ) CALL MSG_OUT( ' ',
     :     'Initialising reduced observation coadds array', STATUS )
         CALL GEN_FILL (SHORTSIZE*NELM, 0, %val(COADDS_PTR))
      ENDIF

*    Obtain the observation type.
*    It is assumed that BIAS, DARK and FLAT observations should not
*    be wavelength calibrated.
      CALL DSA_GET_FITS_C( 'OBSERVATION', 'OBSTYPE', 0, OBS_TYPE,
     :  COMMENT, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_MAKE_OBSREDFILE: '/
     :     /'Error getting FITS item OBSTYPE', STATUS )
         GOTO 500
      END IF

*    Check if we are running IRCAM
      CALL DSA_GET_FITS_C( 'OBSERVATION', 'INSTRUME', 0,
     :  INSTRUMENT, COMMENT, STATUS )
      CALL CHR_RMBLK( INSTRUMENT )
      CALL CHR_UCASE( INSTRUMENT )

      IF ( INDEX( INSTRUMENT, 'IRCAM' ) .GT. 0  .OR.
     :     INDEX( INSTRUMENT, 'ALICE' ) .GT. 0  ) THEN

         PROCEED_LAMBDA = .FALSE.
      ELSE

         IF ( ( OBS_TYPE .NE. 'BIAS' ) .AND.
     :        ( OBS_TYPE .NE. 'DARK' ) .AND.
     :        ( OBS_TYPE .NE. 'FLAT' ) ) THEN

*         This observation may be wavelength calibrated.
*         Obtain the wavelength calibration parameters.
            CALL PAR_GET0C( 'TO_WAVELENGTH', TO_WAVELENGTH, STATUS )
            IF ( TO_WAVELENGTH .EQ. 'YES' ) THEN

               PROCEED_LAMBDA = .TRUE.
            ELSE IF ( TO_WAVELENGTH .EQ. 'NO' ) THEN

               PROCEED_LAMBDA = .FALSE.
            ELSE

               CALL PAR_CANCL('PROCEED_LAMBDA', PROCEED_LAMBDA, STATUS)
               CALL PAR_GET0L('PROCEED_LAMBDA', PROCEED_LAMBDA, STATUS)
            END IF

*          Obtain the wavelength calibration method.
            IF ( PROCEED_LAMBDA ) THEN

               CALL PAR_GET0C('LAMBDA_METHOD', LAMBDA_METHOD, STATUS)
            END IF
         ELSE

*         BIAS, DARK and FLAT observations are not wavelength calibrated.
            PROCEED_LAMBDA = .FALSE.
         END IF
      END IF

      IF ( VERBOSE .AND. PROCEED_LAMBDA )
     :  CALL MSG_OUT( ' ', 'Wavelength calibration enabled', STATUS )

*    Now we must construct the axis1 array and the index arrays that
*    point the data taken at each position to the appropriate slots in
*    the main array. This is done even for the cases where the
*    detector isn't moved, seems simpler that way.
*    Open the index structure, create the appropriate size array in that
*    and map it in (stored as SHORT to save space, but note that it is
*    mapped as INT).
      CALL DSA_NAMED_INPUT ('INDEX', OBSRED(:ICH_LEN(OBSRED))//
     :   '.MORE.CGS4_INDEX', STATUS)
      IND_DIMS(1) = DET_SIZE(1)
      IND_DIMS(2) = DET_NINCR
      IND_NELM = IND_DIMS(1) * IND_DIMS(2)
      CALL DSA_COERCE_DATA_ARRAY ('INDEX', 'SHORT', MAXINDDIMS,
     :   IND_DIMS, STATUS)
      CALL DSA_MAP_DATA ('INDEX', 'WRITE', 'INT', INDEX_PTR,
     :   INDEX_SLOT, STATUS)

*    Initialise the index array to zero, to ensure there are no
*    numeric errors when it is converted from INT to WORD.
      CALL GEN_FILL( INTSIZE*IND_NELM, 0, %val(INDEX_PTR) )

      IF ( STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_MAKE_OBSREDFILE: '/
     :     /'Error initialising INDEX array', STATUS )
      END IF

*   The AXIS1 (X-axis) values are now calculated according the whether
*   wavelength calibration is required.
*   If ESTIMATED wavelength calibration is required, the values are
*   calculated from the grating central wavelength and dispersion.
*   If CALIBRATED wavelength calibration is required, the X-axis
*   structure is copied from a CALIBRATION observation.
*   If no wavelength calibration is required, the axis1 array is filled
*   with pixel co-ordinates.
      IF ( PROCEED_LAMBDA ) THEN

*      Wavelength calibration is required.
*      Determine the calibration method required.
         IF ( LAMBDA_METHOD .EQ. 'ESTIMATED' ) THEN

           IF( VERBOSE ) CALL MSG_OUT( ' ',
     :       'Estimating wavelength scale from grating dispersion', STATUS )

*         A wavelength scale estimated from the grating parameters is required.
*         Obtain the grating central wavelength and dispersion.
            CALL DSA_GET_FITS_F( 'OBSERVATION', 'GLAMBDA', 0, GLAMBDA,
     :        COMMENT, STATUS )
            CALL DSA_GET_FITS_F( 'OBSERVATION', 'GDISP', 0, GDISP,
     :        COMMENT, STATUS )

*         Ensure that GLAMBDA and GDISP have sensible values.
*         If not, reset them to produce a pixel scale, and
*         give a warning.
            IF ( STATUS .NE. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_MAKE_OBSREDFILE: '/
     :           /'Fourth error getting FITS items', STATUS )
            END IF

            IF ( ( GLAMBDA .LE. 0.0 ) .OR.
     :           ( ABS(GDISP) .LT. 1.0E-10 ) ) THEN

               CALL MSG_OUT( ' ', 'WARNING - Grating parameters are '/
     :           /'invalid. A pixel scale will be used.', STATUS )
               GLAMBDA = REAL( DET_SIZE(1) / 2 )
               GDISP   = 1.0
            END IF

*         Issue a message if required.
            IF ( VERBOSE ) THEN

               CALL MSG_OUT( ' ', 'Estimating wavelength scale',
     :           STATUS )
               CALL MSG_SETR( 'GLAMBDA', GLAMBDA )
               CALL MSG_SETI( 'CP', DET_SIZE(1)/2 )
               CALL MSG_OUT( ' ', 'Central (pixel ^CP) wavelength = '/
     :           /'^GLAMBDA microns.', STATUS )
               CALL MSG_SETR( 'GDISP', GDISP )
               CALL MSG_OUT( ' ', 'Grating dispersion = ^GDISP '/
     :           /'microns/pixel.', STATUS )
            END IF

*         Map the AXIS1 array.
            CALL DSA_MAP_AXIS_DATA ('OBSRED', 1, 'WRITE', 'FLOAT',
     :         AXIS1_PTR, AXIS1_SLOT, STATUS)

*         Calculate the axis1 values, sort them into ascending order and fill
*         the store in the index array, IF (STATUS.... stops array bound error
*         if DSA has failed to open one of the files and the pointers mean
*         nothing.
            IF ( STATUS .NE. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_MAKE_OBSREDFILE: '/
     :           /'First error mapping axis data', STATUS )
            END IF

            IF (STATUS .EQ. SAI__OK) THEN

               CALL RED4_SETUP_AXIS1 (%val(AXIS1_PTR),
     :            %val(INDEX_PTR), DIMS(1),
     :            IND_DIMS(1), DET_INCR, DET_NINCR, GLAMBDA, GDISP,
     :            STATUS)
            ENDIF

*         Set up the label and units for the X axis.
            CHAR_ARRAY(1) = 'microns'
            CHAR_ARRAY(2) = 'Estimated wavelength'
            CALL DSA_SET_AXIS_INFO( 'OBSRED', 1, NINFO, CHAR_ARRAY, 0,
     :        DIGNORE, STATUS )
         ELSE

           IF ( VERBOSE ) CALL MSG_OUT( ' ',
     :       'Calibrating wavelength scale using '/
     :      /'previous reduced observation', STATUS )

*         A calibrated wavelength scale is required.
*         Construct the name of the relevant index file, which should
*         have a name of the form CGS4_yymmdd.INDEX
*         Index files are found in the directory whose logical name is
*         CGS4_INDEX.
            CALL RED4_OBSTOINDEX( OBSFILE, INDEX_FILE, STATUS )

*         Obtain a suitable CALIBRATION observation (i.e. one which has
*         a matching instrument configuration and oversampling factor),
*         either by searching for one in the index file, or by using one
*         specified explicitly.
            CALL RED4_GET_OBSERVATION( INDEX_FILE, 'OBSERVATION',
     :        'CALIBRATION', CALIB_NAME, STATUS )

*         If a suitable observation has been found, announce it.
*         Otherwise generate a warning message and forget about
*         the wavelength calibration.
            IF ( CALIB_NAME .NE. ' ' ) THEN

               CALL MSG_SETC( 'CALIB_NAME', CALIB_NAME )
               CALL MSG_OUT( ' ', 'Using the reduced CALIBRATION '/
     :           /'observation in ^CALIB_NAME.', STATUS )

*            Open the CALIBRATION observation.
               CALL DSA_NAMED_INPUT( 'CALIB', CALIB_NAME, STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'RED4_MAKE_OBSREDFILE: '/
     :              /'Error opening calib data', STATUS )
               END IF

*            Copy over the entire contents of the X structure from the
*            calibration observation file into the X structure of the
*            reduced observation file. (The X structure must not already
*            exist, otherwise DTA_CYVAR will not work).
*            The calls to DSA_SPECIFIC_STRUCTURE have been replaced
*            by a simple string concatenation because they were
*            generating warning messages about it being bad practise
*            to refer to .X as an application-specific structure.
*            But copying the .X structure is exactly what we want to do!
*            Unfortunately, this fudge has also removed the check that
*            the .X structure in the CALIB structure exists.
*              CALL RED4_COPY_STRUCTURE( 'CALIB.'//CALIB_STRUCTURE,
*    :            'OBSRED.'//CALIB_STRUCTURE, STATUS )
*              IF ( STATUS .NE. SAI__OK ) THEN
*                 STATUS = SAI__ERROR
*                 CALL ERR_REP( ' ', 'RED4_MAKE_OBSREDFILE: '/
*    :              /'Failed to copy calib structure', STATUS )
*              END IF

               EXIST = .FALSE.
               CALL DSA_SEEK_AXIS( 'CALIB', 1, EXIST, STATUS )
               IF ( ( .NOT. EXIST ) .OR. ( STATUS .NE. SAI__OK ) ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'RED4_MAKE_OBSREDFILE: '/
     :              /'Failed to find X-axis in calibration', STATUS )
               ENDIF
               CALL DSA_AXIS_SIZE( 'CALIB', 1, MAXAXDIMS, AXNDIM,
     :           AXDIMS, AXNELM, STATUS )
               CALL DSA_RESHAPE_AXIS( 'OBSRED', 1, 'CALIB', 1, AXNDIM,
     :           AXDIMS, STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'RED4_MAKE_OBSREDFILE: '/
     :              /'Failed to reshape X-axis', STATUS )
               ENDIF

*            Now the index array needs to be set up. The same routine
*            is used as without wavelength calibration, but a dummy X
*            array is filled.
*            Map some workspace for the dummy axis1 array.
               CALL DSA_GET_WORK_ARRAY( DIMS(1), 'FLOAT', AXIS1_PTR,
     :           AXIS1_SLOT, STATUS)

*            Load dummy values into GLAMBDA and GDISP.
               GLAMBDA = REAL( DET_SIZE(1) / 2 )
               GDISP   = 1.0

*             Fill the store in the index array, IF (STATUS.... stops array
*             bound error if DSA has failed to open one of the files and the
*             pointers mean nothing.
               IF ( STATUS .NE. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'RED4_MAKE_OBSREDFILE: '/
     :              /'Error getting work data', STATUS )
               END IF

               IF (STATUS .EQ. SAI__OK) THEN

                  CALL RED4_SETUP_AXIS1 (%val(AXIS1_PTR),
     :               %val(INDEX_PTR), DIMS(1),
     :               IND_DIMS(1), DET_INCR, DET_NINCR, GLAMBDA, GDISP,
     :               STATUS)
               ENDIF
            ELSE

               IF ( STATUS .NE. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'RED4_MAKE_OBSREDFILE: '/
     :              /'Error mapping data', STATUS )
               END IF

*            Give a warning and set up the axis1 array as if no wavelength
*            calibration had been asked for.
               CALL MSG_OUT( ' ', 'WARNING - No suitable reduced '/
     :           /'CALIBRATION observation found', STATUS )
               CALL MSG_OUT( ' ', 'This observation will NOT be '/
     :           /'wavelength calibrated', STATUS )

*            Define the wavelength parameters so that pixel units will be used
*            (remembering that GLAMBDA is the wavelength at the centre of pixel
*            31, and GDISP is the wavelength increment per pixel).
               GLAMBDA = REAL( DET_SIZE(1) / 2 )
               GDISP   = 1.0

*            Map the AXIS1 array.
               CALL DSA_MAP_AXIS_DATA ('OBSRED', 1, 'WRITE', 'FLOAT',
     :            AXIS1_PTR, AXIS1_SLOT, STATUS)

*             Calculate the axis1 values, sort them into ascending order and
*             fill the store in the index array, IF (STATUS.... stops array
*             bound error if DSA has failed to open one of the files and the
*             pointers mean nothing.
               IF ( STATUS .NE. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'RED4_MAKE_OBSREDFILE: '/
     :              /'Second error mapping axis data', STATUS )
               END IF

               IF (STATUS .EQ. SAI__OK) THEN

                  CALL RED4_SETUP_AXIS1 (%val(AXIS1_PTR),
     :               %val(INDEX_PTR), DIMS(1),
     :               IND_DIMS(1), DET_INCR, DET_NINCR, GLAMBDA, GDISP,
     :               STATUS)
               ENDIF

*            Set up the label and units for the X axis.
              CHAR_ARRAY(1) = ' '

               IF ( DET_NINCR .EQ. 1 ) THEN

                  CHAR_ARRAY(2) = 'Detector columns'
               ELSE

                  CPOS = 0
                  CHAR_ARRAY(2) = ' '
                  CALL CHR_PUTC( 'Detector columns - ',
     :              CHAR_ARRAY(2), CPOS )
                  CALL CHR_PUTI( DET_NINCR, CHAR_ARRAY(2), CPOS )
                  CALL CHR_PUTC( 'X', CHAR_ARRAY(2), CPOS )
               END IF

               CALL DSA_SET_AXIS_INFO( 'OBSRED', 1, NINFO, CHAR_ARRAY,
     :           0, DIGNORE, STATUS )
            END IF
         END IF
      ELSE
*      Wavelength calibration is not required.
*      Define the wavelength parameters so that pixel units will be used
*      (remembering that GLAMBDA is the wavelength at the centre of pixel
*      31, and GDISP is the wavelength increment per pixel).
         GLAMBDA = REAL( DET_SIZE(1) / 2 )
         GDISP   = 1.0

*      Map the AXIS1 array.
         CALL DSA_MAP_AXIS_DATA ('OBSRED', 1, 'WRITE', 'FLOAT',
     :      AXIS1_PTR, AXIS1_SLOT, STATUS)

*       Calculate the axis1 values, sort them into ascending order and fill
*       the store in the index array, IF (STATUS.... stops array bound error
*       if DSA has failed to open one of the files and the pointers mean
*       nothing.
         IF ( STATUS .NE. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_MAKE_OBSREDFILE: '/
     :        /'Third error mapping axis data', STATUS )
         END IF

         IF (STATUS .EQ. SAI__OK) THEN

            CALL RED4_SETUP_AXIS1 (%val(AXIS1_PTR),
     :         %val(INDEX_PTR), DIMS(1),
     :         IND_DIMS(1), DET_INCR, DET_NINCR, GLAMBDA, GDISP,
     :         STATUS)
         ENDIF

*      Set up the label and units for the X axis.
        CHAR_ARRAY(1) = ' '

         IF ( DET_NINCR .EQ. 1 ) THEN

            CHAR_ARRAY(2) = 'Detector columns'
         ELSE

            CPOS = 0
            CHAR_ARRAY(2) = ' '
            CALL CHR_PUTC( 'Detector columns - ',
     :        CHAR_ARRAY(2), CPOS )
            CALL CHR_PUTI( DET_NINCR, CHAR_ARRAY(2), CPOS )
            CALL CHR_PUTC( 'X', CHAR_ARRAY(2), CPOS )
         END IF

         CALL DSA_SET_AXIS_INFO( 'OBSRED', 1, NINFO, CHAR_ARRAY, 0,
     :     DIGNORE, STATUS )
      END IF

*   Set up the label and units for the Y axis. (This is not affected by
*   any wavelength calibration or oversampling).
      CHAR_ARRAY(1) = ' '
      CHAR_ARRAY(2) = 'Detector rows'
      CALL DSA_SET_AXIS_INFO( 'OBSRED', 2, NINFO, CHAR_ARRAY, 0,
     :  DIGNORE, STATUS )

*   Destination for error GOTOs
 500  CONTINUE
      CALL DSA_CLOSE (STATUS)
      IF ( STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_MAKE_OBSREDFILE: '/
     :     /'Error closing DSA', STATUS )
      END IF

      END
