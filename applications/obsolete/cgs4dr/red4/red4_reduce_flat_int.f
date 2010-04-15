*+  RED4_REDUCE_FLAT_INT - reduce a FLAT integration
      SUBROUTINE RED4_REDUCE_FLAT_INT (INT_NAME, STATUS)
*    Description :
*     This routine reduces a FLAT integration in IDIR:Iyymmdd_o_i into
*     RIDIR:RIyymmdd_o_i.
*
*     The reduction consists of subtracting
*     suitable BIAS (any) and DARK (same chip exposure time) observations
*     from the raw integration.
*
*     Sector statistics are calculated if associated with the raw integration.
*     Quality information is propagated from the DARK observation used.
*     The FLAT result is optionally normalised.
*
*     The integration structure has already been opened as 'INT_IN'
*     and the parent observation file as 'OBSERVATION' before this
*     routine is called
*    Invocation :
*     CALL RED4_REDUCE_FLAT_INT (INT_NAME, STATUS)
*    Parameters :
*    Method :
*    Deficiencies :
*     The code is still badly structured, and the error checking and
*     reporting leaves a lot to be desired.
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
*    Authors :
*     John Lightfoot. (REVAD::JFL)
*     Steven Beard.   (REVAD::SMB)
*     Phil Daly.      (JACH::PND)
*    History :
*     Original version: 5-Feb-1990 REVAD::JFL
*     Changed to use BIAS and DARK observations from 'virtual' common block:
*        6-Feb-1990 REVAD::JFL
*     Goto 500s replaced by bad status: 6-Feb-1990 REVAD::JFL
*      9-Mar-1990: MAXDIM parameter added.                        (SMB)
*      9-Mar-1990: Division by number of exposures commented out,
*                  since this operation is now carried out
*                  automatically by the ADP. (If this works, the
*                  code may be deleted).                          (SMB)
*     12-Mar-1990: Bug fix. Superfluous call to DSA_CLOSE removed.(SMB)
*     23-Apr-1990: FLAT_FIELD changed to FLAT.                    (SMB)
*     23-Apr-1990: Inconsistency in DATA_INFO removed.            (SMB)
*     24-Apr-1990: Because of memory corruption problems, the
*                  code needs to be compiled with array bounds
*                  checking switched on. The Figaro dynamic
*                  memory functions (DYN_ELEMENT, DYNAMIC_MEM,
*                  DYN_INCREMENT) would not allow this. Code
*                  modified to use %val() instead.            (SMB)
*     24-Apr-1990: DYN_INCREMENT replaced by direct
*                  manipulation of address (with the aid of
*                  DSA_TYPESIZE).                             (SMB)
*      1-May-1990: Ability to smooth and normalise a
*                  flat-field partially added.                (SMB)
*      2-May-1990: The routines DSA_GET_DATA_INFO and
*                  DSA_SET_DATA_INFO require a double
*                  precision array for the NUM_ARRAY argument.
*                  This routine was giving an integer (0),
*                  which could cause problems. Fixed.         (SMB)
*      3-May-1990: Normalisation code completed.              (SMB)
*      5-Jul-1990: Modified to set NORMALISED item in data
*                  structure. Commented out code removed.     (SMB)
*      6-Jul-1990: Made to write object name and exposure
*                  time in a standard Figaro structure (as
*                  well as the special CGS4 structure). (Why
*                  was this not done in the first place??).   (SMB)
*     10-Jul-1990: Dark subtraction made optional. Parameter names
*                  made more consistent with RED4. Hard-wired
*                  "4"s replaced by "FLOATSIZE". Ability to specify
*                  a bad pixel mask as well as propagating data
*                  quality during reduction added.            (SMB)
*     20-Jul-1990: Status check put around call to
*                  RED4_NORMALISE_FIT, to prevent an "adjustable
*                  array dimension error" if the arrays have not
*                  been mapped. This is a side effect of the poor
*                  structure and error checking.              (SMB)
*     20-Jul-1990: Modified to check that the number of
*                  detector increments is 1. Oversampled
*                  FLAT frames are no longer allowed.         (SMB)
*     23-Jul-1990: Character handling improved, and modified
*                  to accept 4 digit observation numbers.     (SMB)
*     22-Aug-1990: Renamed to RED4_REDUCE_FLAT_INT for
*                  consistency.                               (SMB)
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
*     29-Nov-1990: Modified to read linearisation coefficients
*                  from a text file. MAXDIM and RMAXDIM constants
*                  moved to RED4_COMMON.INC. Unnecessarily
*                  large character variables reduced in size. (SMB)
*     24-Feb-1991: DSA error statuses trapped, as these do
*                  not conform to the ADAM error scheme.      (SMB)
*      1-Aug-1991: Alternate normalisation method, by
*                  smoothing, included. Normalisation method
*                  written into FITS structure.               (SMB)
*      1-Oct-1991: Change GEN_routines.                       (PND)
*      8-Oct-1991: Call RED4_GET_OBSERVATION instead of
*                  RED4_SEEK_OBSERVATION, so that calibration
*                  observations can be specified explicitly when
*                  required (DRIMP/5.1 and 5.2).              (SMB)
*     20-Jul-1992: Add SUBTRACT_BIAS option                   (PND)
*     23-Feb-1993: Conform to error strategy                  (PND)
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
*                                             file to be reduced, e.g. I890816_1_1
*    External references :
      INTEGER DSA_TYPESIZE               ! DSA type size enquiry function
      INTEGER CHR_LEN                    ! ADAM stringlength function
      CHARACTER*2 GEN_NTH                ! Figaro "Nth" determination function
*                                            (i.e. 1st, 2nd, 3rd, 4th ...)
*    Global variables :
      INCLUDE 'RED4_COMMON.INC'          ! 'Virtual' common block containing
*                                             results of observations needed to
*                                             perform this reduction.
*    Local Constants :
*    Local variables :
      INTEGER FLOATSIZE                  ! Bytes per element of 'FLOAT' array
      INTEGER NDIM                       ! the dimensions of the integration
*                                        !    array
      INTEGER DIMS( MAXDIM )             !                "
      INTEGER N_EXPOSURES                ! the number of exposures that were
*                                             coadded in the integration
      INTEGER IN_SLOT                    ! integration
      INTEGER IN_DATA                    !        "
      INTEGER DATA_SLOT                  ! reduced integration
      INTEGER RED_DATA                   !        "
      INTEGER QUAL_SLOT                  ! reduced quality
      INTEGER RED_QUAL                   !        "
      INTEGER VAR_SLOT                   ! reduce variance
      INTEGER RED_VAR                    !        "
      INTEGER MASK_SLOT                  ! Bad pixel mask data
      INTEGER MASK_DATA                  !        "
      INTEGER
     :  SPECTRUM_SLOT,                   ! Slot for spectrum workspace
     :  SPECTRUM_PTR,                    ! Address of spectrum workspace
     :  SUM_SLOT,                        ! Slot for sum workspace
     :  SUM_PTR,                         ! Address of sum workspace
     :  SQ_SLOT,                         ! Slot for spectrum quality
     :  SQ_PTR,                          ! Address for spectrum quality
     :  SM_SLOT,                         ! Slot for smoothed spectrum
     :  SM_PTR,                          ! Address for smoothed spectrum
     :  SMQ_SLOT,                        ! Slot for smoothed spectrum quality
     :  SMQ_PTR,                         ! Address for smoothed spectrum quality
     :  X_SLOT,                          ! Slot for X workspace
     :  X_PTR,                           ! Address of X workspace
     :  Y_SLOT,                          ! Slot for Y workspace
     :  Y_PTR                            ! Address of Y workspace
      INTEGER NELM                       ! number of elements in integration
*                                             array
      INTEGER NPLANE                     ! number of elements in one plane of
*                                        !    the integration array
      INTEGER LDAY, LDATE, LHOUR         ! lengths of date strings
      INTEGER N_COEFFS                   ! number of non-zero coefficients in
*                                             array linearisation polynomial
      INTEGER I                          ! DO loop counter
      INTEGER ORDER                      ! The order of the polynomial
*                                             used to smooth the flat field
      INTEGER BOXSIZE                    ! Size of smooth box used to
*                                             normalise the flat field
      INTEGER IGNORE                     ! ignored subroutine return
      INTEGER DET_NINCR                  ! The number of detector positions
*                                        !   which are being combined into
*                                        !   the final observation.
      INTEGER CLEN                       ! Non-blank length of character string
      INTEGER CPOS                       ! Position in character string.
      INTEGER DSA_STATUS                 ! DSA status value
      REAL EXPOSURE_TIME                 ! on-chip exposure time
      REAL INTEGRATION_TIME              ! EXPOSURE_TIME * N_EXPOSURES
      DOUBLE PRECISION
     :  LCOEFFS( MAXCOEFFS )             ! Linearisation coefficients array
      CHARACTER*80 MASK                  ! The name of the bad pixel mask.
      CHARACTER*80 LINCOEFFS             ! The name of the linearisation
*                                        !    coefficients file.
      CHARACTER*80 INT_RED               ! the name of the reduced integration
*                                             file
      CHARACTER*80 OBSFILE               ! The name of the parent observation
*                                             file.
      CHARACTER*80 INDEX_FILE            ! The name of the relevant index file.
      CHARACTER*80 BIAS_NAME             ! The name of the file containing the
*                                             reduced BIAS observation
      CHARACTER*80 DARK_NAME             ! The name of the file containing the
*                                             reduced DARK observation
      CHARACTER*20 INT_TYPE              ! type of integration
      CHARACTER*40 OBJECT_NAME           ! The name of the object
      CHARACTER*32 CHAR_ARRAY(2)         ! array to hold data units and title
      CHARACTER*40 TIME                  ! A string holding the date of the
*                                             reduction
      CHARACTER*20 DAY, DATE, HOUR       ! components of date
      CHARACTER*4 SUBTRACT_BIAS          ! Should a BIAS frame be subtracted ?
*                                        !   (YES, NO or ASK)
      CHARACTER*4 SUBTRACT_DARK          ! Should a DARK frame be subtracted ?
*                                        !   (YES, NO or ASK)
      CHARACTER*4 NORMALISE_FF           ! Should the flat field be
*                                        !   normalised ? (YES, NO or ASK).
      CHARACTER*20 NORM_METHOD           ! The flat field normalisation method
*                                        !   required ('POLYFIT' or 'SMOOTH')
      CHARACTER*8 ITEM                   ! Name of FITS item
      CHARACTER*4 COMMENT                ! Dummy comment
      CHARACTER*20 LPREFIX               ! Prefix to apply
      LOGICAL PROCEED_BIAS               ! T if BIAS frames are to be subtracted
      LOGICAL PROCEED_DARK               ! T if DARK frames are to be subtracted
      LOGICAL PROCEED_NORM               ! T if the flat-field is to be normalised
      LOGICAL VARIANCE_MAP               ! T if variances mapped
*    Internal References :
*    Local data :
*-

*    Return if entry status bad
      IF ( STATUS .NE. SAI__OK ) RETURN

*    The integration structure has already been opened as 'INT_IN'
*    and the parent observation file as 'OBSERVATION' before this
*    routine is called
*    first try to check that we have all the information for
*    the integration reduction proceed.
*    Get the INT_TYPE (i.e. the observation mode; CHOP, STARE etc...)
      DSA_STATUS = SAI__OK
      CALL DSA_GET_FITS_C( 'OBSERVATION', 'INTTYPE', 0, INT_TYPE,
     :  COMMENT, DSA_STATUS )

*    FLAT observations should be straightforward. Chopped observations
*    or weighted frames are meaningless, so treat them as an error.
      IF (DSA_STATUS .EQ. SAI__OK) THEN

         IF (INDEX(INT_TYPE,'CHOP') .NE. 0) THEN

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_REDUCE_FLAT_INT: '/
     :        /'This FLAT integration was taken '/
     :        /'in chop mode. There must have been a '/
     :        /'mistake!', STATUS )
         ENDIF

         IF (INDEX(INT_TYPE,'WEIGHTED') .NE. 0) THEN

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_REDUCE_FLAT_INT: '/
     :        /'This FLAT integration was taken '/
     :        /'in weighted frames mode which is '/
     :        /'meaningless. There must have been a '/
     :        /'mistake!', STATUS )
         END IF
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_FLAT_INT: '/
     :     /'Error getting FITS items INTTYPE', STATUS )
      ENDIF

*    Check that the number of detector increments is 1, and hence that
*    the integration has not been made with oversampling (which is only
*    relevant for SPECTRA-type observations).
      CALL DSA_GET_FITS_I( 'OBSERVATION', 'DETNINCR', 0,
     :  DET_NINCR, COMMENT, DSA_STATUS )

      IF ( DSA_STATUS .EQ. SAI__OK ) THEN

         IF ( DET_NINCR .NE. 1 ) THEN

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_REDUCE_FLAT_INT: '/
     :        /'This FLAT integration was taken with '/
     :        /'oversampling, which is not sensible. '/
     :        /'There must have been a mistake!', STATUS )
         END IF
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_FLAT_INT: '/
     :     /'Error getting FITS item DETNINCR', STATUS )
      END IF

*   Get the number of exposures in this integration N_EXPOSURES
      CALL DSA_GET_FITS_I( 'INT_IN', 'NEXP', 0, N_EXPOSURES,
     :  COMMENT, DSA_STATUS )

      IF (DSA_STATUS .EQ. SAI__OK) THEN

         IF (N_EXPOSURES .LT. 1) THEN

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_REDUCE_FLAT_INT: '/
     :        /'No exposures were made during the '/
     :        /'integration', STATUS )
         ENDIF
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_FLAT_INT: '/
     :     /'Error getting FITS item NEXP', STATUS )
      ENDIF

*    get the exposure time in this integration EXPOSURE_TIME
      CALL DSA_GET_FITS_F( 'OBSERVATION', 'DEXPTIME', 0, EXPOSURE_TIME,
     :  COMMENT, DSA_STATUS )

      IF (DSA_STATUS .EQ. SAI__OK) THEN

         IF (EXPOSURE_TIME .LE. 0.0) THEN

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_REDUCE_FLAT_INT: '/
     :        /'Exposure time is negative', STATUS )
         ENDIF
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_FLAT_INT: '/
     :     /'Error getting FITS item DEXPTIME', STATUS )
      ENDIF

*    Work out integration time
      INTEGRATION_TIME = N_EXPOSURES * EXPOSURE_TIME

*    Find out if the data are to be linearised with some previously
*    defined linearisation polynomial. The polynomial coefficients
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

*    Determine if the observation is to be bias-subtracted
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

*    Determine if the observation is to be dark-subtracted
      CALL PAR_GET0C( 'SUBTRACT_DARK', SUBTRACT_DARK, STATUS )

      IF ( SUBTRACT_DARK .EQ. 'YES' ) THEN

         PROCEED_DARK = .TRUE.
      ELSE IF ( SUBTRACT_DARK .EQ. 'NO' ) THEN

         PROCEED_DARK = .FALSE.
      ELSE

         CALL PAR_CANCL( 'PROCEED_DARK', STATUS )
         CALL PAR_GET0L( 'PROCEED_DARK', PROCEED_DARK, STATUS )
      END IF

      IF ( VERBOSE ) THEN

         IF ( .NOT. PROCEED_DARK ) CALL MSG_OUT( ' ',
     :      'No DARK subtraction will be performed', STATUS )
      END IF

*    Determine if the flat-field is to be normalised, and the
*    normalisation method to be used.
      CALL PAR_GET0C( 'NORMALISE_FF', NORMALISE_FF, STATUS )

      IF ( NORMALISE_FF .EQ. 'YES' ) THEN

         PROCEED_NORM = .TRUE.
      ELSE IF ( NORMALISE_FF .EQ. 'NO' ) THEN

         PROCEED_NORM = .FALSE.
      ELSE

         CALL PAR_CANCL( 'PROCEED_NORM', STATUS )
         CALL PAR_GET0L( 'PROCEED_NORM', PROCEED_NORM, STATUS )
      END IF

      IF ( VERBOSE ) THEN

         IF ( .NOT. PROCEED_NORM ) CALL MSG_OUT( ' ',
     :      'No FLAT field normalisation will be performed', STATUS )
      END IF

*   If the flat field is going to be normalised, obtain the
*   normalisation method required.
      IF ( PROCEED_NORM ) THEN

         CALL PAR_GET0C( 'NORM_METHOD', NORM_METHOD, STATUS )

         IF ( NORM_METHOD .EQ. 'POLYFIT' ) THEN

            CALL PAR_GET0I( 'ORDER', ORDER, STATUS )

         ELSE IF ( NORM_METHOD .EQ. 'SMOOTH' ) THEN

            CALL PAR_GET0I( 'BOXSIZE', BOXSIZE, STATUS )
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
     :  DSA_STATUS)

*    Obtain the number of bytes per element in a data array of
*    type 'FLOAT' from DSA
      FLOATSIZE = DSA_TYPESIZE( 'FLOAT', DSA_STATUS )

*    Find the name of the observation file responsible for this
*    integration, from it construct the name of the the observation
*    index file which should have a name of the form CGS4_yymmdd.INDEX
*    Index files are found in the directory whose logical name is
*    CGS4_INDEX.
      CALL DSA_GET_FITS_C( 'INT_IN', 'OBSFILE', 0, OBSFILE, COMMENT,
     :  DSA_STATUS )

      IF ( DSA_STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_FLAT_INT: '/
     :     /'Error getting FITS item OBSFILE', STATUS )
      END IF

      CALL RED4_OBSTOINDEX( OBSFILE, INDEX_FILE, STATUS )

*    If non-destructive reads are not being used, then we need a ready
*    reduced BIAS observation for this reduction.
*    RED4_GET_OBSERVATION will either look for a suitable one in the
*    index file or use one which has been explicitly specified.
*    The data, errors and quality for this observation will be held in
*    virtual memory, with the relevant pointers stored in /RED4_COMMON/.
      IF ( PROCEED_BIAS .AND. (INDEX(INT_TYPE,'NDR').EQ.0) ) THEN

         CALL RED4_GET_OBSERVATION ( INDEX_FILE, 'OBSERVATION',
     :      'BIAS', BIAS_NAME, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN

            IF (BIAS_NAME .EQ. ' ') THEN

               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_REDUCE_FLAT_INT: '/
     :           /'No suitable reduced BIAS observation '/
     :           /'found. Data reduction aborted!', STATUS )
            ELSE

               CALL MSG_SETC( 'BIAS_NAME', BIAS_NAME )
               CALL MSG_OUT( ' ', 'Using the reduced BIAS observation '/
     :           /'^BIAS_NAME', STATUS )
            ENDIF
         ENDIF
      ENDIF

*    We may also need a ready reduced DARK observation for this reduction.
*    RED4_GET_OBSERVATION will either look for a suitable one in the
*    index file or use one which has been explicitly specified.
*    The data, errors and quality for this observation will be held in
*    virtual memory, with the relevant pointers stored in /RED4_COMMON/.
      IF ( PROCEED_DARK ) THEN

         CALL RED4_GET_OBSERVATION ( INDEX_FILE, 'OBSERVATION',
     :     'DARK', DARK_NAME, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN

            IF (DARK_NAME .EQ. ' ') THEN

               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_REDUCE_FLAT_INT: '/
     :           /'No suitable reduced DARK observation '/
     :           /'found. Data reduction aborted!', STATUS )
            ELSE

               CALL MSG_SETC( 'DARK_NAME', DARK_NAME )
               CALL MSG_OUT( ' ', 'Using the reduced DARK observation '/
     :           /'^DARK_NAME', STATUS )
            ENDIF
         ENDIF
      ENDIF

*    Find out if a bad pixel mask has been specified
*    ('#' means no mask has been specified).
*    If one has, open it and check its size.
      CALL PAR_GET0C ('MASK', MASK, STATUS)
      CALL PAR_CANCL ('MASK', STATUS)
      CPOS = INDEX( MASK, ':')
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
     :     MASK_SLOT, DSA_STATUS)
      ENDIF

*    Now we're as sure as can be that everything's gonna be jus' fine,
*    so open the output file according to the reduced integration template
*    and with the correct filename e.g. RI890801_1_1 from I890801_1_1
      CALL DSA_NAMED_INPUT ('INTRED_TEMPLATE',
     :   INTRED_TEMPLATE, DSA_STATUS)

      IF ( DSA_STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_FLAT_INT: '/
     :     /'Error opening integration template', STATUS )
      END IF

      CALL RED4_INTTORINT( INT_NAME, INT_RED, STATUS )

      IF (STATUS .EQ. SAI__OK) THEN

         CALL MSG_SETC( 'INT_RED', INT_RED )
         CALL MSG_OUT( ' ',
     :     'Reducing FLAT integration into ^INT_RED', STATUS )
      ENDIF

      DSA_STATUS = STATUS
      CALL DSA_NAMED_OUTPUT ('INT_RED', INT_RED, 'INTRED_TEMPLATE',
     :   0, 0, DSA_STATUS)

*    Copy the contents of the FITS structure from the raw integration file
*    over to the reduced integration file (deleting the old structure first,
*    if present)
      CALL RED4_COPY_STRUCTURE( 'INT_IN.'//FITS_STRUCTURE,
     :   'INT_RED.'//FITS_STRUCTURE, STATUS )

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

*    make doubly sure the variances are initialised to 0, DSA should do
*    this but doesn't always do so
      IF (DSA_STATUS .EQ. SAI__OK) THEN

         CALL GEN_FILL (FLOATSIZE*NPLANE, 0, %val(RED_VAR))
      ENDIF

*    Do the reduction.
*    Copy the Phase A plane of the integration array into the result data array
*    If a bad pixel mask has been specified, copy its contents into the
*    data quality array. Otherwise initialise this to zero.
      IF (DSA_STATUS .EQ. SAI__OK) THEN

         CALL GEN_MOVE (FLOATSIZE*NPLANE, %val(IN_DATA),
     :      %val(RED_DATA))

         IF ( INDEX( MASK, '#' ) .EQ. 0 ) THEN

            CALL GEN_MOVE( NPLANE, %val(MASK_DATA), %val(RED_QUAL) )
         ELSE

            CALL GEN_FILL (NPLANE, 0, %val(RED_QUAL))
         END IF
      ENDIF

*    Flag as "bad" any values which the ADP has set to -50.0.
      IF ( DSA_STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_FLAT_INT: '/
     :      /'Error prior to flagging bad values', STATUS )
      END IF

      IF ( STATUS .EQ. SAI__OK ) THEN

         CALL RED4_FLAG_BAD( NPLANE, -50.0, %val(RED_DATA),
     :     %val(RED_QUAL), STATUS )
      END IF

*    Check for any error
      IF (STATUS .EQ. SAI__OK) THEN

*       work out the sector statistics if enabled
         IF (INDEX(INT_TYPE,'SECTORS') .NE. 0) THEN

*          the plane of the integration array holding the statistical
*             information
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

*       If non-destructive reads are not being used, subtract the BIAS
*       observation from the result, propagating variances and data quality.
         IF ( INDEX( INT_TYPE, 'NDR' ) .EQ. 0 ) THEN

            IF ( PROCEED_BIAS .AND. (STATUS .EQ. SAI__OK) ) THEN

               CALL GEN_SUBAFV (NPLANE, %val(RED_DATA),
     :            %val(BIAS_DATA), %val(RED_DATA),
     :            %val(RED_QUAL), %val(BIAS_QUAL), %val(RED_QUAL),
     :            %val(RED_VAR), %val(BIAS_VAR), %val(RED_VAR),
     :            .TRUE., .FALSE., 0, .TRUE. )
            ENDIF
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

*    Now subtract the DARK data, propagate variances and quality
      IF ( PROCEED_DARK .AND. (STATUS .EQ. SAI__OK) ) THEN

         CALL GEN_SUBAFV (NPLANE, %val(RED_DATA),
     :      %val(DARK_DATA), %val(RED_DATA),
     :      %val(RED_QUAL), %val(DARK_QUAL), %val(RED_QUAL),
     :      %val(RED_VAR), %val(DARK_VAR), %val(RED_VAR),
     :      .TRUE., .FALSE., 0, .TRUE. )
      ENDIF

*    Normalise the flat field if required.
      IF ( PROCEED_NORM ) THEN

*      Use the normalisation method specified.
         IF ( NORM_METHOD .EQ. 'POLYFIT' ) THEN

*         Obtain the workspace required by the normalisation routine.
            DSA_STATUS = STATUS
            CALL DSA_GET_WORK_ARRAY( DIMS(1), 'FLOAT', SPECTRUM_PTR,
     :        SPECTRUM_SLOT, DSA_STATUS )
            CALL DSA_GET_WORK_ARRAY( DIMS(1), 'INT', SUM_PTR,
     :        SUM_SLOT, DSA_STATUS )
            CALL DSA_GET_WORK_ARRAY( DIMS(1), 'FLOAT', X_PTR,
     :        X_SLOT, DSA_STATUS )
            CALL DSA_GET_WORK_ARRAY( DIMS(1), 'FLOAT', Y_PTR,
     :        Y_SLOT, DSA_STATUS )

            IF ( DSA_STATUS .NE. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_REDUCE_FLAT_INT: '/
     :            /'First error getting work array', STATUS )
            END IF

            CALL MSG_SETI( 'ORDER', ORDER )
            CALL MSG_SETC( 'NTH', GEN_NTH(ORDER) )
            CALL MSG_OUT( ' ', 'Normalising with ^ORDER^NTH '/
     :        /'order polynomial', STATUS )

*         Normalise the integration array, using a polynomial fit.
*         (The external status check prevents an "adjustable array
*         dimension error" if any of the arrays are not mapped).
            IF ( STATUS .EQ. SAI__OK ) THEN

               CALL RED4_NORMALISE_FIT( DIMS(1), DIMS(2), ORDER,
     :           %val(RED_DATA), %val(RED_VAR), %val(RED_QUAL),
     :           %val(SPECTRUM_PTR), %val(SUM_PTR),
     :           %val(X_PTR), %val(Y_PTR), STATUS )
            END IF

         ELSE IF ( NORM_METHOD .EQ. 'SMOOTH' ) THEN

*         Obtain the workspace required by the smoothing normalisation
*         routine.
            DSA_STATUS = STATUS
            CALL DSA_GET_WORK_ARRAY( DIMS(1), 'FLOAT', SPECTRUM_PTR,
     :        SPECTRUM_SLOT, DSA_STATUS )
            CALL DSA_GET_WORK_ARRAY( DIMS(1), 'INT', SUM_SLOT,
     :        SUM_SLOT, DSA_STATUS )
            CALL DSA_GET_WORK_ARRAY( DIMS(1), 'BYTE', SQ_PTR,
     :        SQ_SLOT, DSA_STATUS )
            CALL DSA_GET_WORK_ARRAY( DIMS(1), 'FLOAT', SM_PTR,
     :        SM_SLOT, DSA_STATUS )
            CALL DSA_GET_WORK_ARRAY( DIMS(1), 'BYTE', SMQ_PTR,
     :        SMQ_SLOT, DSA_STATUS )

            IF ( DSA_STATUS .NE. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_REDUCE_FLAT_INT: '/
     :           /'Second error getting work array', STATUS )
            END IF

            CALL MSG_SETI( 'BOXSIZE', BOXSIZE )
            CALL MSG_OUT( ' ', 'Normalising with ^BOXSIZE-pixel '/
     :        /'box smooth', STATUS )

*         Normalise the reduced observation array using a box smooth.
*         (The external status check is to prevent the routine crashing
*         with an "adjustable array dimension error" if any of the arrays
*         are not mapped).
            IF ( STATUS .EQ. SAI__OK ) THEN

               CALL RED4_NORMALISE_SMOOTH( DIMS(1), DIMS(2),
     :           BOXSIZE, %val(RED_DATA), %val(RED_VAR), %val(RED_QUAL),
     :           %val(SPECTRUM_PTR), %val(SUM_PTR), %VAL(SQ_PTR),
     :           %val(SM_PTR), %val(SMQ_PTR), STATUS )
            END IF
         ELSE

*         Unknown normalisation method.
            CALL MSG_SETC( 'METHOD', NORM_METHOD )
            CALL MSG_OUT( ' ', '**** Unknown normalisation method '/
     :        /'^METHOD. No normalisation carried out', STATUS )
         END IF
      END IF

*    Check that the variances are non-negative, they'll crash DSA_CLOSE
*    (original Figaro version anyway) if they are
      IF ( VARIANCE_MAP .AND. (NPLANE.GT.0) ) THEN

         CALL GEN_CLIPF (%val(RED_VAR), NPLANE, 0.0,
     :      VAL__MAXR, IGNORE, IGNORE, %val(RED_VAR))
      ENDIF

*    Set the data label and units
*    If the flat-field has been normalised, its units will just
*    be arbitrary numbers. Otherwise they will be A/D numbers per
*    exposure.
      IF ( PROCEED_NORM ) THEN

         CHAR_ARRAY (1) = 'Normalised number'
      ELSE

         CHAR_ARRAY (1) = 'A/D numbers per exposure'
      END IF

      CHAR_ARRAY(2) = 'FLAT'
      CALL CHR_CTOC( CHAR_ARRAY(2), CHAR_ARRAY(2), CLEN )

      IF ( PROCEED_BIAS ) THEN
         CHAR_ARRAY(2) = '(' // CHAR_ARRAY(2)(1:CLEN) // '-BIAS)'
         CALL CHR_CTOC( CHAR_ARRAY(2), CHAR_ARRAY(2), CLEN )
      ENDIF

      IF ( PROCEED_DARK ) THEN
         CHAR_ARRAY(2) = '(' // CHAR_ARRAY(2)(1:CLEN) // '-DARK)'
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
*    record how the reduction went in the CGS4_INTRED extension
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
      ELSE IF ( PROCEED_DARK ) THEN

         CALL DSA_PUT_FITS_C( 'INT_RED', 'MASKUSED', 'Propagated '/
     :     /'from DARK observation', ' ', DSA_STATUS)
      ELSE IF ( PROCEED_BIAS .AND. (INDEX(INT_TYPE,'NDR').NE.0) ) THEN

         CALL DSA_PUT_FITS_C( 'INT_RED', 'MASKUSED', 'Propagated '/
     :     /'from BIAS observation', ' ', DSA_STATUS)
      ELSE

         CALL DSA_PUT_FITS_C( 'INT_RED', 'MASKUSED', '(none)',
     :     ' ', DSA_STATUS)
      END IF

*    the name of the BIAS observation if used
      IF ( PROCEED_BIAS .AND. (INDEX(INT_TYPE,'NDR').EQ.0) ) THEN

         CLEN = MAX( 1, CHR_LEN( BIAS_NAME ) )
         CALL DSA_PUT_FITS_C( 'INT_RED', 'BIASUSED',
     :     BIAS_NAME(1:CLEN), ' ', DSA_STATUS )
      ELSE

         CALL DSA_PUT_FITS_C( 'INT_RED', 'BIASUSED', '(none)',
     :     ' ', DSA_STATUS )
      ENDIF

*    the name of the DARK observation, if used.
      IF ( PROCEED_DARK ) THEN

         CLEN = MAX( 1, CHR_LEN( DARK_NAME ) )
         CALL DSA_PUT_FITS_C( 'INT_RED', 'DARKUSED',
     :     DARK_NAME(1:CLEN), ' ', DSA_STATUS )
      ELSE

         CALL DSA_PUT_FITS_C( 'INT_RED', 'DARKUSED', '(none)',
     :     ' ', DSA_STATUS )
      ENDIF

*    the integration time. (Write this in both a CGS4 structure and
*    a Figaro structure).
      CALL DSA_PUT_FITS_F( 'INT_RED', 'EXPOSED', INTEGRATION_TIME,
     :  ' ', DSA_STATUS )
      CALL DSA_SET_EXPOSURE( 'INT_RED', INTEGRATION_TIME, DSA_STATUS )

*    whether linearised or not & the linearisation coefficients
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

*    whether normalised or not
      IF ( PROCEED_NORM ) THEN

         CALL DSA_PUT_FITS_C( 'INT_RED', 'NORMALIS', 'yes',
     :     ' ', DSA_STATUS )
         CLEN = MAX( 1, CHR_LEN( NORM_METHOD ) )
         CALL DSA_PUT_FITS_C( 'INT_RED', 'NMETHOD', NORM_METHOD(1:CLEN),
     :     ' ', DSA_STATUS )

         IF ( NORM_METHOD .EQ. 'POLYFIT' ) THEN

            CALL DSA_PUT_FITS_I( 'INT_RED', 'NORDER', ORDER,
     :        ' ', DSA_STATUS )

         ELSE IF ( NORM_METHOD .EQ. 'SMOOTH' ) THEN

            CALL DSA_PUT_FITS_I( 'INT_RED', 'NBOXSIZE', BOXSIZE,
     :        ' ', DSA_STATUS )
         END IF
      ELSE

         CALL DSA_PUT_FITS_C( 'INT_RED', 'NORMALIS', 'no',
     :     ' ', DSA_STATUS )
      ENDIF

      IF ( DSA_STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_FLAT_INT: '/
     :     /'Error putting FITS items', STATUS )
      END IF

      END
