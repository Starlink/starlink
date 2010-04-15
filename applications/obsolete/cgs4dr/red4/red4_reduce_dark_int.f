*+  RED4_REDUCE_DARK_INT - Reduce a DARK integration.
      SUBROUTINE RED4_REDUCE_DARK_INT (INT_NAME, STATUS)
*    Description :
*     Reduce a DARK integration.
*
*     The integration structure has already been opened as 'INT_IN'
*     and the parent observation file as 'OBSERVATION' before this
*     routine is called
*    Invocation :
*     CALL RED4_REDUCE_DARK_INT (INT_NAME, STATUS)
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*    Method :
*    Deficiencies :
*     This routine needs restructuring without GOTOs, and the error checking
*     and reporting leaves a lot to be desired.
*
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
*     Steven Beard (REVAD::SMB)
*     Phil Daly (JACH::PND)
*    History :
*     1989:  Original version                                    (JFL)
*     22-Jan-1990: History added. Method added from notes made
*                  after studying source code.                   (SMB)
*     4-Feb-1990: New version of RED4_SEEK_OBSERVATION that copies BIAS
*                 observation into 'virtual common', if not already there. (JFL)
*     20-Feb-1990: Null bad pixel mask changed from ' ' to '#',
*                  as it was difficult to process ' ' with ICL
*                  variables.                                    (SMB)
*      9-Mar-1990: MAXDIM parameter added.                       (SMB)
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
*     24-Apr-1990: DYN_INCREMENT replaced by direct
*                  manipulation of address (with the aid of
*                  DSA_TYPESIZE).                             (SMB)
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
*     20-Jul-1990: Modified to check that the number of
*                  detector increments is 1. Oversampled
*                  DARK frames are no longer allowed.         (SMB)
*     23-Jul-1990: Character handling improved, and modified
*                  to accept 4 digit observation numbers.     (SMB)
*     22-Aug-1990: Phase 1 of major changes: Header
*                  information in the observation and reduced
*                  observation files is now written in a .FITS
*                  structure. Linearisation coefficients are
*                  now obtained from a file rather than being
*                  buried in the data structure. KTC mode
*                  replaced by NDR.                           (SMB)
*      3-Sep-1990: Phase 2 of major changes: Header
*                  information in the integration and reduced
*                  integration files is now written in a .FITS
*                  structure.                                 (SMB)
*      4-Sep-1990: Typing mistakes fixed.                     (SMB)
*      7-Sep-1990: Output made less verbose.                  (SMB)
*     14-Sep-1990: Code simplified by encapsulating low-level
*                  DTA calls in RED4_COPY_STRUCTURE.          (SMB)
*     25-Sep-1990: Size match on bad pixel mask included.     (SMB)
*     26-Sep-1990: The size check was a mistake, since the
*                  data array in an integration can be 3-D.
*                  Modified into a check on the first 2
*                  dimensions.                                (SMB)
*     26-Sep-1990: Modified to check the MASK parameter, to
*                  be more consistent with the other routines.(SMB)
*      4-Oct-1990: Modified to cope with the standard
*                  deviation array returned by the ADP when
*                  sector monitoring is enabled in NDR mode.  (SMB)
*      7-Nov-1990: Modified to check for the -50.0 values
*                  set by the ADP when it detects a bad value.(SMB)
*     21-Nov-1990: The routine RED4_SEEK_OBSERVATION has to be
*                  changed to allow wavelength calibration.
*                  This routine modified to keep up.          (SMB)
*     23-Nov-1990: Bug fix: Only call RED4_FLAG_BAD if the
*                  status is ok (to prevent adjustable array
*                  bounds violation).                         (SMB)
*     29-Nov-1990: Modified to read linearisation coefficients
*                  from a text file. MAXDIM and RMAXDIM constants
*                  moved to RED4_COMMON.INC. Unnecessarily
*                  large character variables reduced in size. (SMB)
*     24-Feb-1991: DSA error statuses trapped, as these no
*                  not conform to the ADAM error scheme.      (SMB)
*      1-Oct-1991: Change GEN_routines.                       (PND)
*      8-Oct-1991: Call RED4_GET_OBSERVATION instead of
*                  RED4_SEEK_OBSERVATION, so that calibration
*                  observations can be specified explicitly when
*                  required (DRIMP/5.1 and 5.2).              (SMB)
*     20-Jul-1992: Add SUBTRACT_BIAS option                   (PND)
*     22-Feb-1993: Conform to error strategy                  (PND)
*      7-Jan-1993: Allow NDFs, include PRM_PAR                (PND)
*     17-Jan-1994: Add parameterised template files           (PND)
*     10-Nov-1994: Attempt to make vaguely portable           (AB)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
*    Status :
      INTEGER STATUS
*    Input :
      CHARACTER*(*) INT_NAME             ! the basic name of the integration
*                                        !    file to be reduced, e.g. I890816_1_1
*    External references :
      INTEGER DSA_TYPESIZE               ! DSA type size enquiry function
      INTEGER CHR_LEN                    ! ADAM stringlength function
      CHARACTER*2 GEN_NTH                ! Figaro "Nth" determination function
*                                            (i.e. 1st, 2nd, 3rd, 4th ...)
*    Global variables :
      INCLUDE 'RED4_COMMON.INC'          ! RED4 common block holding pointers
*                                             to BIAS observation in virtual
*                                             memory
*    Local Constants :
*    Local variables :
      INTEGER FLOATSIZE                  ! Bytes per element of 'FLOAT' array
      INTEGER NDIM                       ! the dimensions of the integration
*                                             array
      INTEGER DIMS( MAXDIM )             !                "
      INTEGER N_EXPOSURES                ! the number of exposures that were
*                                             coadded in the integration
      INTEGER IN_SLOT                    ! integration
      INTEGER IN_DATA                    !        "
      INTEGER DATA_SLOT                  ! reduced integration
      INTEGER RED_DATA                   !        "
      INTEGER QUAL_SLOT                  ! reduced quality
      INTEGER RED_QUAL                   !        "
      INTEGER VAR_SLOT                   ! reduced variances
      INTEGER RED_VAR                    !        "
      INTEGER MASK_SLOT                  ! bad-pixel mask
      INTEGER MASK_DATA                  !        "
      INTEGER NELM                       ! number of elements in integration
*                                             array
      INTEGER NPLANE                     ! number of elements in one plane of
*                                             the integration array
      INTEGER LDAY, LDATE, LHOUR         ! lengths of date strings
      INTEGER N_COEFFS                   ! number of coefficients in
*                                             array linearisation polynomial
      INTEGER I                          ! DO loop
      INTEGER IGNORE                     ! ignored subroutine return
      INTEGER DET_NINCR                  ! The number of detector positions
*                                        !   which are being combined into
*                                        !   the final observation.
      INTEGER CLEN                       ! Non-blank length of character string
      INTEGER CPOS                       ! Position in character string.
      INTEGER DSA_STATUS                 ! DSA status value
      LOGICAL VARIANCE_MAP               ! T if variance array mapped.
      REAL EXPOSURE_TIME                 ! on-chip exposure time
      REAL INTEGRATION_TIME              ! EXPOSURE_TIME * N_EXPOSURES
      DOUBLE PRECISION
     :  LCOEFFS( MAXCOEFFS )             ! Linearisation coefficients array
      CHARACTER*80 INT_RED               ! The name of the reduced integration
*                                             file.
      CHARACTER*80 OBSFILE               ! The name of the parent observation
*                                             file.
      CHARACTER*80 INDEX_FILE            ! The name of the relevant index file.
      CHARACTER*80 BIAS_NAME             ! The name of the file containing the
*                                             reduced BIAS observation
      CHARACTER*20 INT_TYPE              ! type of integration
      CHARACTER*80 MASK                  ! name of file containing dud-pixel
*                                             info
      CHARACTER*80 LINCOEFFS             ! name of file containing
*                                        !    linearisation coefficients
      CHARACTER*40 OBJECT_NAME           ! The name of the object.
      CHARACTER*32 CHAR_ARRAY(2)         ! array to hold data units and title
      CHARACTER*40 TIME                  ! A string holding the date of the
*                                             reduction
      CHARACTER*20 DAY, DATE, HOUR       ! components of date
      CHARACTER*20 LPREFIX               ! Prefix to apply
      CHARACTER*4  SUBTRACT_BIAS         ! Should a BIAS be subtracted?
      CHARACTER*8  ITEM                  ! Name of FITS item
      CHARACTER*4  COMMENT               ! Dummy comment
      LOGICAL      PROCEED_BIAS          ! T if BIAS frames are to be subtracted
*    Internal References :
*    Local data :
*-

*    Return if entry status bad
      IF ( STATUS .NE. SAI__OK ) RETURN

*    The integration structure has already been opened as 'INT_IN'
*    and the parent observation file as 'OBSERVATION' before this
*    routine is called
*    first of all try to check that we have all the information for
*    the integration reduction proceed.
*    Get the INT_TYPE (i.e. the observation mode; CHOP, STARE etc...)
      DSA_STATUS = SAI__OK
      CALL DSA_GET_FITS_C( 'OBSERVATION', 'INTTYPE', 0, INT_TYPE,
     :  COMMENT, DSA_STATUS )

*   Abort if an error has occurred at this point. (This is done for
*   consistency with the old code. GOTOs should be removed eventually).
      IF ( DSA_STATUS .NE. SAI__OK ) THEN

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_DARK_INT: '/
     :     /'Error getting FITS item INTTYPE', STATUS )
         GOTO 500
      ELSE

*    DARK observations should be very straightforward. Chopped observations
*    or weighted frames are meaningless, so treat them as an error.
         IF (INDEX(INT_TYPE,'CHOP') .NE. 0) THEN

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_REDUCE_DARK_INT: '/
     :        /'This DARK integration was taken in '/
     :        /'chop mode. There must have been a '/
     :        /'mistake!', STATUS )
            GOTO 500
         ENDIF

         IF (INDEX(INT_TYPE,'WEIGHTED') .NE. 0) THEN

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_REDUCE_DARK_INT: '/
     :        /'This DARK integration was taken in '/
     :        /'weighted frames mode which is '/
     :        /'meaningless. There must have been a '/
     :        /'mistake!', STATUS )
            GOTO 500
         END IF
      ENDIF

*    Check that the number of detector increments is 1, and hence that
*    the integration has not been made with oversampling (which is only
*    relevant for SPECTRA-type observations).
      CALL DSA_GET_FITS_I( 'OBSERVATION', 'DETNINCR', 0, DET_NINCR,
     :  COMMENT, DSA_STATUS )

      IF ( DSA_STATUS .NE. SAI__OK ) THEN

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_DARK_INT: '/
     :     /'Error getting FITS item DETNINCR', STATUS )
         GOTO 500
      ELSE

         IF ( DET_NINCR .NE. 1 ) THEN

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_REDUCE_DARK_INT: '/
     :        /'This DARK integration was taken with '/
     :        /'oversampling, which is not sensible. '/
     :        /'There must have been a mistake!', STATUS )
            GOTO 500
         END IF
      END IF

*   Get the number of exposures in this integration N_EXPOSURES
      CALL DSA_GET_FITS_I( 'INT_IN', 'NEXP', 0, N_EXPOSURES,
     :  COMMENT, DSA_STATUS )

      IF (DSA_STATUS .NE. SAI__OK) THEN

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_DARK_INT: '/
     :     /'Error getting FITS item INTTYPE', STATUS )
         GOTO 500
      ELSE

         IF (N_EXPOSURES .LT. 1) THEN

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_REDUCE_DARK_INT: '/
     :        /'No exposures were made during the '/
     :        /'integration', STATUS )
            GOTO 500
         ENDIF
      ENDIF

*    get the exposure time in this integration EXPOSURE_TIME
      CALL DSA_GET_FITS_F( 'OBSERVATION', 'DEXPTIME', 0, EXPOSURE_TIME,
     :  COMMENT, DSA_STATUS )

      IF (DSA_STATUS .NE. SAI__OK) THEN

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_DARK_INT: '/
     :     /'Error getting FITS item DEXPTIME', STATUS )
         GOTO 500
      ELSE

         IF (EXPOSURE_TIME .LE. 0.0) THEN

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_REDUCE_DARK_INT: '/
     :        /'Exposure time is negative', STATUS )
            GOTO 500
         ENDIF
      ENDIF

*   Abort if an error has occurred at this point. (This is done for
*   consistency with the old code. GOTOs should be removed eventually).
      IF ( STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_DARK_INT: '/
     :     /'Error getting FITS items', STATUS )
         GOTO 500
      END IF

*    Work out integration time
      INTEGRATION_TIME = N_EXPOSURES * EXPOSURE_TIME

*    Find out if the data are to be linearised with some previously
*    defined linearisation polynomial. The polymonial coefficients
*    are held in a text file. If the name of the file is given
*    as '#', then no linearisation will be carried out.
      CALL PAR_GET0C( 'LINCOEFFS', LINCOEFFS, STATUS )
      CALL CHR_UCASE( LINCOEFFS )
      CALL PAR_CANCL( 'LINCOEFFS', STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN

         IF ( INDEX( LINCOEFFS, '#' ) .EQ. 0 ) THEN

*         Read the linearisation coefficients from the named file.
            CALL RED4_READ_COEFFS( LINCOEFFS, MAXCOEFFS, N_COEFFS,
     :        LCOEFFS, STATUS )
         END IF
      END IF

*    Find the size of the input data array
      DSA_STATUS = STATUS
      CALL DSA_DATA_SIZE ('INT_IN', MAXDIM, NDIM, DIMS, NELM,
     :  DSA_STATUS)

*    and the number of elements in a single plane of that array
      NPLANE = DIMS(1) * DIMS(2)

*    Map in the array
      CALL DSA_MAP_DATA ('INT_IN', 'READ', 'FLOAT', IN_DATA, IN_SLOT,
     :   DSA_STATUS)

*    Obtain the number of bytes per element in a data array of
*    type 'FLOAT' from DSA
      FLOATSIZE = DSA_TYPESIZE( 'FLOAT', DSA_STATUS )

*    See if we should subtract a bias
      IF ( DSA_STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_DARK_INT: '/
     :     /'Error getting FLOAT size', STATUS )
      END IF

      CALL PAR_GET0C( 'SUBTRACT_BIAS', SUBTRACT_BIAS, STATUS )
      IF ( SUBTRACT_BIAS .EQ. 'YES' ) THEN

         PROCEED_BIAS = .TRUE.
      ELSE IF ( SUBTRACT_BIAS .EQ. 'NO' ) THEN

         PROCEED_BIAS = .FALSE.
      ELSE

         CALL PAR_CANCL( 'PROCEED_BIAS', STATUS )
         CALL PAR_GET0L( 'PROCEED_BIAS', PROCEED_BIAS, STATUS )
      END IF

      IF ( INDEX( INT_TYPE, 'NDR' ) .NE. 0 ) PROCEED_BIAS = .FALSE.

      IF ( VERBOSE ) THEN

         IF ( .NOT. PROCEED_BIAS ) CALL MSG_OUT( ' ',
     :      'No BIAS subtraction will be performed', STATUS )
      END IF

*    If non-destructive reads are not being used, then we need a ready
*    reduced BIAS observation for this reduction. RED4_GET_OBSERVATION
*    will copy the most suitable observation into virtual memory
*    (pointers held in RED4_COMMON)
      IF ( PROCEED_BIAS .AND. (INDEX(INT_TYPE,'NDR').EQ.0) ) THEN

*      Find the name of the observation file responsible for this
*      integration, from it construct the name of the the observation
*      index file which should have a name of the form CGS4_yymmdd.INDEX
*      Index files are found in the directory whose logical name is
*      CGS4_INDEX.
         CALL DSA_GET_FITS_C( 'INT_IN', 'OBSFILE', 0, OBSFILE, COMMENT,
     :     DSA_STATUS )

         IF ( DSA_STATUS .NE. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_REDUCE_DARK_INT: '/
     :        /'Error getting OBSFILE', STATUS )
         END IF

         CALL RED4_OBSTOINDEX( OBSFILE, INDEX_FILE, STATUS )

*      Either search this index file for as suitable BIAS observation,
*      or use one specified explicitly.
         CALL RED4_GET_OBSERVATION ( INDEX_FILE, 'OBSERVATION',
     :      'BIAS', BIAS_NAME, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN

            IF (BIAS_NAME .EQ. ' ') THEN

               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_REDUCE_DARK_INT: '/
     :           /'No suitable reduced BIAS observation '/
     :           /'found. Data reduction aborted!', STATUS )
               GOTO 500
            ELSE

               CALL MSG_SETC( 'BIAS_NAME', BIAS_NAME )
               CALL MSG_OUT( ' ', 'Using the reduced BIAS '/
     :            /'observation ^BIAS_NAME', STATUS)
            ENDIF
         ENDIF
      ENDIF

*    Find what dud-pixel mask is to be used, if any. Map in the quality
*    array if need be and check its size.
      CALL PAR_GET0C ('MASK', MASK, STATUS)
      CALL PAR_CANCL ('MASK', STATUS)
      CPOS = INDEX (MASK, ':')
      IF (CPOS .EQ. 0) CPOS = INDEX( MASK, '/')
      IF ( CPOS .EQ. 0 ) THEN
         CALL RED4_GET_PREFIX ('MASK', LPREFIX, STATUS)
         CLEN = CHR_LEN( MASK )
         MASK = LPREFIX(:CHR_LEN(LPREFIX)) // MASK(1:CLEN)
      END IF

      IF ( INDEX( MASK, '#' ) .EQ. 0 ) THEN

         IF ( VERBOSE ) THEN

            CALL MSG_SETC( 'MASK', MASK )
            CALL MSG_OUT( ' ', 'Using the bad pixel mask ^MASK',
     :        STATUS )
         END IF

         DSA_STATUS = STATUS
         CALL DSA_NAMED_INPUT ('MASK', MASK, DSA_STATUS)

         CALL DSA_MATCH_DIMENSION( 'MASK', 1, 'INT_IN', 1, DSA_STATUS )
         CALL DSA_MATCH_DIMENSION( 'MASK', 2, 'INT_IN', 2, DSA_STATUS )

         CALL DSA_MAP_DATA ('MASK', 'READ', 'BYTE', MASK_DATA,
     :      MASK_SLOT, DSA_STATUS)
      ENDIF

*    Now we're as sure as can be that everything's gonna be jus' fine,
*    so open the output file according to the reduced integration template
*    and with the correct filename e.g. RI890801_1_1 from I890801_1_1
      CALL DSA_NAMED_INPUT ('INTRED_TEMPLATE',
     :   INTRED_TEMPLATE, DSA_STATUS)

      IF ( DSA_STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_DARK_INT: '/
     :     /'Error opening MASK data', STATUS )
      END IF

      CALL RED4_INTTORINT( INT_NAME, INT_RED, STATUS )

      IF (STATUS .EQ. SAI__OK) THEN

         CALL MSG_SETC( 'INT_RED', INT_RED )
         CALL MSG_OUT( ' ', 'Reducing a DARK '/
     :     /'integration into ^INT_RED', STATUS )
      ENDIF

      DSA_STATUS = STATUS
      CALL DSA_NAMED_OUTPUT ('INT_RED', INT_RED, 'INTRED_TEMPLATE',
     :   0, 0, DSA_STATUS)

*    Copy the contents of the FITS structure from the raw integration file
*    over to the reduced integration file (deleting the old structure first,
*    if present)
      CALL RED4_COPY_STRUCTURE( 'INT_IN.'//FITS_STRUCTURE,
     :   'INT_RED.'//FITS_STRUCTURE, STATUS )

*   Abort if an error has occurred. This is bad practise, but has been
*   included to maintain the behaviour of the original code.
*   GOTOs should eventually be removed.
      IF ( STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_DARK_INT: '/
     :     /'An ADAM error has occurred', STATUS )
         GOTO 500
      END IF

*    Map in the data array of the reduced integration file, and the
*    variance and quality arrays
      DSA_STATUS = STATUS
      CALL DSA_USE_QUALITY ('INT_RED', DSA_STATUS)

      CALL DSA_MAP_DATA ('INT_RED', 'WRITE', 'FLOAT', RED_DATA,
     :   DATA_SLOT, DSA_STATUS)
      CALL DSA_MAP_QUALITY ('INT_RED', 'WRITE', 'BYTE', RED_QUAL,
     :   QUAL_SLOT, DSA_STATUS)
      CALL DSA_MAP_VARIANCE ('INT_RED', 'WRITE', 'FLOAT', RED_VAR,
     :   VAR_SLOT, DSA_STATUS)

      IF ( DSA_STATUS .EQ. SAI__OK ) THEN

         VARIANCE_MAP = .TRUE.
      ELSE

         VARIANCE_MAP = .FALSE.
      END IF

*    the variance array should be full of zeros at this point, but
*    can come out of this program full of gibberish if nothing is done
*    to it, so make sure it is filled with zeros initially
      IF (DSA_STATUS .EQ. SAI__OK) THEN

         CALL GEN_FILL (FLOATSIZE*NPLANE, 0, %val(RED_VAR))
      ENDIF

*    Do the reduction.
*    Copy the Phase A plane of the integration array into the result data array
*    If a bad pixel mask has been specified, copy its contents into the
*    data quality array. Otherwise initialise this to zero.
      IF (STATUS .EQ. SAI__OK) THEN

         CALL GEN_MOVE (FLOATSIZE*NPLANE, %val(IN_DATA), %val(RED_DATA))

         IF ( INDEX( MASK, '#' ) .EQ. 0 ) THEN

            CALL GEN_MOVE( NPLANE, %val(MASK_DATA), %val(RED_QUAL) )
         ELSE

            CALL GEN_FILL (NPLANE, 0, %val(RED_QUAL))
         END IF
      ENDIF

*    Flag as "bad" any values which the ADP has set to -50.0.
      IF ( DSA_STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_DARK_INT: '/
     :     /'A general DSA error has occurred', STATUS )
      END IF

      IF ( STATUS .EQ. SAI__OK ) THEN

         CALL RED4_FLAG_BAD( NPLANE, -50.0, %val(RED_DATA),
     :     %val(RED_QUAL), STATUS )
      END IF

*   Check for error.
      IF (STATUS .EQ. SAI__OK) THEN

*       work out the SECTOR statistics, if enabled
         IF (INDEX(INT_TYPE,'SECTORS') .NE. 0) THEN

*          the plane of the integration array holding the statistical
*          information
            IN_DATA = IN_DATA + FLOATSIZE * NPLANE

*         In NDR mode, the statistical information will be a "standard
*         deviation" array. In other modes it will be a "sum of X squared"
*         array. These need to be converted into a "standard error squared"
*         array (incorrectly called "variance" in several places).
            IF ( INDEX(INT_TYPE, 'NDR') .NE. 0 ) THEN

               CALL RED4_SIGMA_TO_SESQ( NPLANE, %val(IN_DATA),
     :           N_EXPOSURES, %val(RED_VAR), STATUS )
            ELSE

               CALL RED4_SXX_TO_SESQ( %val(RED_DATA), %val(IN_DATA),
     :           NPLANE, N_EXPOSURES, %val(RED_VAR), STATUS)
            END IF
         ENDIF

*       If non-destructive reads are not being used, subtract the bias
*       observation from the result, add the variances, propagate the quality
         IF ( PROCEED_BIAS .AND. (INDEX(INT_TYPE,'NDR').EQ.0) ) THEN

            CALL GEN_SUBAFV (NPLANE, %val(RED_DATA),
     :         %val(BIAS_DATA), %val(RED_DATA),
     :         %val(RED_QUAL), %val(BIAS_QUAL), %val(RED_QUAL),
     :         %val(RED_VAR), %val(BIAS_VAR), %val(RED_VAR),
     :         .TRUE., .FALSE., 0, .TRUE. )
         ENDIF
      ENDIF

*   Linearise the data if required.
      IF ( STATUS .EQ. SAI__OK ) THEN

         IF ( INDEX( LINCOEFFS, '#' ) .EQ. 0 ) THEN

            IF ( VERBOSE ) THEN

               CALL MSG_SETI( 'NCOEFFS', N_COEFFS )
               CALL MSG_SETC( 'NTH', GEN_NTH(N_COEFFS) )
               CALL MSG_SETC( 'LINCOEFFS', LINCOEFFS )
               CALL MSG_OUT( ' ', 'Linearising using '/
     :           /'^NCOEFFS^NTH order polynomial in ^LINCOEFFS',
     :           STATUS )
            END IF

            CALL RED4_LINEARISE( N_COEFFS, LCOEFFS, NPLANE,
     :         %val(RED_DATA), %val(RED_VAR), %val(RED_QUAL),
     :         STATUS )
         END IF
      END IF

*    Check that the variances are non-negative, they'll crash DSA_CLOSE
*    (original Figaro version anyway) if they are
      IF ( VARIANCE_MAP .AND. (NPLANE.GT.0) ) THEN

         CALL GEN_CLIPF (%val(RED_VAR), NPLANE, 0.0,
     :      VAL__MAXR, IGNORE, IGNORE, %val(RED_VAR))
      ENDIF

*    Set the data label and units
      CHAR_ARRAY (1) = 'A/D numbers per exposure'
      CHAR_ARRAY (2) = 'DARK'
      CALL CHR_CTOC( CHAR_ARRAY(2), CHAR_ARRAY(2), CLEN )

      IF ( PROCEED_BIAS ) THEN

        CHAR_ARRAY(2) = '(' // CHAR_ARRAY(2)(1:CLEN) // '-BIAS)'
        CALL CHR_CTOC( CHAR_ARRAY(2), CHAR_ARRAY(2), CLEN )
      ENDIF

      DSA_STATUS = STATUS
      CALL DSA_SET_DATA_INFO ('INT_RED', 2, CHAR_ARRAY, 0, 0.0D0,
     :  DSA_STATUS)

*    Obtain the name of the object and write it to the reduced
*    integration structure in the standard Figaro way.
      CALL DSA_GET_FITS_C( 'OBSERVATION', 'OBJECT', 0, OBJECT_NAME,
     :  COMMENT, DSA_STATUS )
      CALL DSA_SET_OBJECT( 'INT_RED', OBJECT_NAME, DSA_STATUS )

*    End of reduction
*    Record how the reduction went in the CGS4_INTRED extension
*    Date & time
      CALL GEN_TIME (6, DAY, LDAY, DATE, LDATE, HOUR, LHOUR)
      TIME = DATE(:LDATE)//' at '//HOUR(:LHOUR)
      CLEN = MAX( 1, CHR_LEN( TIME ) )
      CALL DSA_PUT_FITS_C( 'INT_RED', 'STREDUCE', TIME(1:CLEN), ' ',
     :  DSA_STATUS )

*    the name of the dud pixel mask, if used
      IF ( INDEX( MASK, '#' ) .EQ. 0 ) THEN

         CALL DSA_PUT_FITS_C( 'INT_RED', 'MASKUSED',
     :     MASK, ' ', DSA_STATUS)
      ELSE IF (INDEX(INT_TYPE,'NDR') .EQ. 0) THEN

         CALL DSA_PUT_FITS_C( 'INT_RED', 'MASKUSED', 'Propagated '/
     :     /'from BIAS observation', ' ', DSA_STATUS)
      ELSE

         CALL DSA_PUT_FITS_C( 'INT_RED', 'MASKUSED', '(none)',
     :     ' ', DSA_STATUS)
      END IF

*    the name of the BIAS observation, if used
      IF ( PROCEED_BIAS .AND. (INDEX(INT_TYPE,'NDR').EQ.0) ) THEN

         CLEN = MAX( 1, CHR_LEN( BIAS_NAME ) )
         CALL DSA_PUT_FITS_C( 'INT_RED', 'BIASUSED', BIAS_NAME(1:CLEN),
     :     ' ', DSA_STATUS )
      ELSE

         CALL DSA_PUT_FITS_C( 'INT_RED', 'BIASUSED', '(none)',
     :     ' ', DSA_STATUS )
      ENDIF

*    the integration time. (Write this to the FITS structure and to
*    the standard Figaro OBS structure).
      CALL DSA_PUT_FITS_F( 'INT_RED', 'EXPOSED', INTEGRATION_TIME,
     :  ' ', DSA_STATUS )
      CALL DSA_SET_EXPOSURE( 'INT_RED', INTEGRATION_TIME, DSA_STATUS )

*    whether linearised or not, and the linearisation coefficients
      IF ( INDEX( LINCOEFFS, '#' ) .EQ. 0 ) THEN

         CALL DSA_PUT_FITS_C( 'INT_RED', 'LINEARIS', 'yes',
     :     ' ', DSA_STATUS )

         DO I = 1, N_COEFFS

            CPOS = 0
            CALL CHR_PUTC( 'LINCF', ITEM, CPOS )
            CALL CHR_PUTI( I, ITEM, CPOS )

            CALL DSA_PUT_FITS_D( 'INT_RED', ITEM(1:CPOS), LCOEFFS(I),
     :        ' ', DSA_STATUS )
         END DO
      ELSE

         CALL DSA_PUT_FITS_C( 'INT_RED', 'LINEARIS', 'no',
     :     ' ', DSA_STATUS )
      ENDIF

*   Destination for error GOTOs
 500  CONTINUE
      IF ( DSA_STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_DARK_INT: '/
     :     /'A DSA error has been detected', STATUS )
      END IF

      END
