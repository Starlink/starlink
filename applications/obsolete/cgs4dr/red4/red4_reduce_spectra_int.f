*+  RED4_REDUCE_SPECTRA_INT - Reduce a SPECTRA-type integration
      SUBROUTINE RED4_REDUCE_SPECTRA_INT (INT_NAME, TYPE, STATUS)
*    Description :
*     This routine reduces an OBJECT, CALIBRATION, SKY, ARC or STANDARD
*     integration in IDIR:Iyymmdd_o_i into RIDIR:RIyymmdd_o_i.
*
*     If the integration was a STARE the reduction consists of:-
*                divide by the number of ADP coadds
*                work out sector statistics if enabled
*                if not using non-destructive reads
*                   subtract suitable BIAS (any)
*                linearise
*                subtract suitable DARK (same chip exposure time)
*                divide by suitable FLAT (same chip exposure time
*                   and optical configuration), if required.
*
*     If the integration was a CHOP the reduction consists of:-
*                divide phase A and phase B by the number of ADP coadds
*                work out sector statistics if enabled (refer to A-B)
*                if not using non-destructive reads
*                   subtract suitable BIAS (any) from phase A and phase B
*                linearise phase A and phase B and variances
*                subtract phase A from phase B
*                divide by suitable FLAT (same chip exposure time
*                   and optical configuration), if required.
*
*     Variances and quality are propagated from all the reduced observations
*     used in the reduction.
*     Weighted coadds are not treated properly.
*
*     The integration structure has already been opened as 'INT_IN'
*     and the parent observation file as 'OBSERVATION' before this
*     routine is called
*    Invocation :
*     CALL RED4_REDUCE_SPECTRA_INT (INT_NAME, TYPE, STATUS)
*    Parameters :
*    Method :
*    Deficiencies :
*     The description says weighted coadds are not treated properly.
*     What does this mean ? Is it serious ?
*
*     The routine is still badly structured and there is insufficient
*     error checking. The error messages reported by this routine can
*     sometimes be misleading as well.
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
*     John Lightfoot (REVAD::JFL)
*     Steven Beard (REVAD::SMB)
*     Phil Daly (JACH::PND)
*    History :
*     Original version: 5-Feb-1990 REVAD::JFL
*     GOTO 500s replaced by inherited bad status: 6-Feb-1990 REVAD::JFL
*      8-Feb-1990: Extra parameter, PROMPT_FF, added so the
*                  prompting for a flat field may be controlled
*                  by CRED4.                                      (SMB)
*     20-Feb-1990: Null bad pixel mask changed from ' ' to '#',
*                  as it was difficult to process ' ' with ICL
*                  variables.                                     (SMB)
*      2-Mar-1990: Status check added before call to
*                  RED4_INDEX_ARITHMETIC, to prevent an adjustable
*                  array bounds violation.                        (SMB)
*      9-Mar-1990: MAXDIM and RMAXDIM parameters added.           (SMB)
*      9-Mar-1990: Modified to report actual type of integration. (SMB)
*      9-Mar-1990: Division by number of exposures commented out,
*                  since this operation is now carried out
*                  automatically by the ADP. (If this works, the
*                  code may be deleted).                          (SMB)
*     12-Mar-1990: Bug fix. Superfluous call to DSA_CLOSE removed.(SMB)
*     23-Apr-1990: FLAT_FIELD changed to FLAT. DATA_INFO
*                  inconsistencies tidied up.                     (SMB)
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
*     11-Jun-1990: This routine crashed when presented with an
*                  observation which was neither STARE or CHOP.
*                  Extra check added.                         (SMB)
*      5-Jul-1990: Commented out code removed. Modified so that
*                  flat field observations to not have to be
*                  oversampled - RED4_INDEX_ARITHMETIC replaced
*                  with GEN_DIVAFE.                           (SMB)
*      6-Jul-1990: Made to write object name and exposure
*                  time in a standard Figaro structure (as
*                  well as the special CGS4 structure). (Why
*                  was this not done in the first place??).   (SMB)
*      9-Jul-1990: Dark subtraction made optional. Parameter names
*                  made more consistent with CRED4. Hard-wired
*                  "4"s replaced by "FLOATSIZE". Ability to specify
*                  a bad pixel mask as well as propagating data
*                  quality during reduction added.                (SMB)
*     23-Jul-1990: Character handling improved, and modified
*                  to accept 4 digit observation numbers.         (SMB)
*     28-Aug-1990: Phase 1 of major changes: Header
*                  information in the observation and reduced
*                  observation files is now written in a .FITS
*                  structure. Linearisation coefficients are
*                  now obtained from a file rather than being
*                  buried in the data structure. KTC mode
*                  replaced by NDR. CHOP mode modified so that
*                  phase A is subtracted from phase B, rather
*                  than the other way round.                      (SMB)
*      3-Sep-1990: Phase 2 of major changes: Header
*                  information in the integration and reduced
*                  integration files is now written in a .FITS
*                  structure.                                     (SMB)
*      3-Sep-1990: CHOP mode bugs spotted and fixed.              (SMB)
*      4-Sep-1990: ERR_OUT replaced with ERR_REP.                 (SMB)
*      4-Sep-1990: Typing mistakes fixed.                         (SMB)
*      7-Sep-1990: Output made less verbose.                      (SMB)
*     14-Sep-1990: Code simplified by encapsulating low-level
*                  DTA calls in RED4_COPY_STRUCTURE.              (SMB)
*     25-Sep-1990: Size match on bad pixel mask included.         (SMB)
*     26-Sep-1990: The size check was a mistake, since the
*                  data array in an integration can be 3-D.
*                  Modified into a check on the first 2
*                  dimensions.                                    (SMB)
*      4-Oct-1990: Modified to cope with the standard
*                  deviation array returned by the ADP when
*                  sector monitoring is enabled in NDR mode.      (SMB)
*     24-Oct-1990: SKY, CALIBRATION and ARC observations
*                  taken in CHOP mode are not allowed.            (SMB)
*      1-Nov-1990: Normalisation to 1 second exposure time
*                  removed. Signal will now be given as
*                  A/D numbers per exposure.                      (SMB)
*      7-Nov-1990: Modified to check for the -50.0 values
*                  set by the ADP when it detects a bad value.    (SMB)
*     21-Nov-1990: The routine RED4_SEEK_OBSERVATION has to be
*                  changed to allow wavelength calibration.
*                  This routine modified to keep up.              (SMB)
*     23-Nov-1990: Bug fix: Only call RED4_FLAG_BAD if the
*                  status is ok (to prevent adjustable array
*                  bounds violation).                             (SMB)
*     29-Nov-1990: Modified to read linearisation coefficients
*                  from a text file. MAXDIM and RMAXDIM constants
*                  moved to RED4_COMMON.INC. Unnecessarily
*                  large character variables reduced in size.     (SMB)
*     22-Feb-1991: Trap for SKY integration taken in CHOP mode
*                  removed.                                       (SMB)
*     24-Feb-1991: Some error reporting mistakes fixed,
*                  which would have made this routine
*                  fail under ADAM V1.9.                          (SMB)
*     24-Feb-1991: DSA error statuses trapped, as these do
*                  not conform to the ADAM error scheme.          (SMB)
*      3-Sep-1991: GEN_MULCAFE renamed to GEN_MULCAFEV. The CGS4
*                  software was assuming this routine dealt with
*                  variances, but the actual Figaro routine dealt
*                  with standard deviation.                       (SMB)
*     11-Sep-1991: GEN_MULCAFEV renamed to GEN_MULCAFV, and argument
*                  list made compatible with Figaro version.      (SMB)
*      1-Oct-1991: Change other GEN_routines.                     (PND)
*      8-Oct-1991: Call RED4_GET_OBSERVATION instead of
*                  RED4_SEEK_OBSERVATION, so that calibration
*                  observations can be specified explicitly when
*                  required (DRIMP/5.1 and 5.2).                  (SMB)
*     23-Jul-1992: Add SUBTRACT_BIAS option                       (PND)
*     23-Feb-1993: Conform to error strategy                      (PND)
*      7-Jan-1994: Allow NDFs, include PRM_PAR                    (PND)
*     17-Jan-1994: Allow parameterised template files             (PND)
*     10-Nov-1994: Make vaguely portable                          (AB)
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
*                                             file to be reduced, e.g.
*                                             I890816_1_1.
      CHARACTER*(*) TYPE                 ! the particular type of 'SPECTRA'
*                                             observation this is;
*                                             'CALIBRATION', 'SKY', 'ARC'
*                                             'STANDARD' or 'OBJECT'
*    External references :
      INTEGER DSA_TYPESIZE               ! DSA type size enquiry function
      INTEGER CHR_LEN                    ! ADAM stringlength function
      CHARACTER*2 GEN_NTH                ! Figaro "Nth" determination function
*                                            (i.e. 1st, 2nd, 3rd, 4th ...)
*    Global variables :
      INCLUDE 'RED4_COMMON.INC'          ! RED4 common block
*    Local Constants :
*    Local variables :
      INTEGER FLOATSIZE                  ! Bytes per element of 'FLOAT' array
      LOGICAL PROCEED_BIAS               ! T if subtracting BIAS
*                                             observation
      LOGICAL PROCEED_DARK               ! T if subtracting DARK
*                                             observation
      LOGICAL PROCEED_FF                 ! T if dividing by flat-field
*                                             observation
      LOGICAL VARIANCE_MAP               ! T if variance array of reduced
*                                            observation has been mapped
      INTEGER NDIM                       ! the dimensions of the raw
*                                             integration array
      INTEGER DIMS( MAXDIM )             !                "
      INTEGER RED_DIMS( RMAXDIM )        ! the dimensions of the reduced
*                                             integration data array
      INTEGER N_EXPOSURES                ! the number of exposures that were
*                                             coadded in the integration
      INTEGER DET_INDEX                  ! the detector position index for this
*                                             integration
      INTEGER IN_SLOT                    ! input integration
      INTEGER IN_DATA                    !        "
      INTEGER DATA_SLOT                  ! reduced integration data
      INTEGER RED_DATA                   !        "
      INTEGER QUAL_SLOT                  ! reduced integration quality
      INTEGER RED_QUAL                   !        "
      INTEGER VAR_SLOT                   ! reduced integration variance
      INTEGER RED_VAR                    !        "
      INTEGER MASK_SLOT                  ! quality mask
      INTEGER MASK_DATA                  !        "
      INTEGER PHASEA_SLOT                ! temporary storage for phase A
      INTEGER PHASEA_DATA                !        "
      INTEGER PHASEB_SLOT                ! temporary storage for phase B
      INTEGER PHASEB_DATA                !        "
      INTEGER NELM                       ! number of elements in integration
*                                             array
      INTEGER NPLANE                     ! number of elements in one plane of
*                                             the integration array
      INTEGER LDAY, LDATE, LHOUR         ! lengths of date strings
      INTEGER N_COEFFS                   ! number of non-zero coefficients in
*                                             array linearisation polynomial
      INTEGER I                          ! DO loop counter
      INTEGER IGNORE                     ! ignored subroutine return
      INTEGER DSA_STATUS                 ! DSA status value
      INTEGER CLEN                       ! Non-blank length of character string
      INTEGER CPOS                       ! Position in character string.
      REAL EXPOSURE_TIME                 ! on-chip exposure time
      REAL INTEGRATION_TIME              ! EXPOSURE_TIME * N_EXPOSURES
      DOUBLE PRECISION
     :  LCOEFFS( MAXCOEFFS )             ! Linearisation coefficients array
      CHARACTER*4 SUBTRACT_BIAS          ! Controls whether observation
*                                        !    is to be bias subtracted
*                                        !    (YES, NO or ASK).
      CHARACTER*4 SUBTRACT_DARK          ! Controls whether observation
*                                        !    is to be dark subtracted
*                                        !    (YES, NO or ASK).
      CHARACTER*4 DIVIDE_BY_FF           ! Controls whether observation
*                                        !    is to be divided by flat-field
*                                        !    (YES, NO or ASK).
      CHARACTER*80 INT_RED               ! the name of the reduced integration
*                                             file
      CHARACTER*80 OBSFILE               ! The name of the parent observation
*                                             file.
      CHARACTER*80 INDEX_FILE            ! The name of the relevant index file.
      CHARACTER*80 BIAS_NAME             ! The name of the file containing the
*                                             reduced BIAS observation
      CHARACTER*80 DARK_NAME             ! The name of the file containing the
*                                             reduced DARK observation
      CHARACTER*80 FLAT_NAME             ! The name of the file containing the
*                                             reduced FLAT observation
      CHARACTER*80 MASK                  ! the name of the file containing the
*                                             bad pixel mask
      CHARACTER*80 LINCOEFFS             ! the name of the file containing the
*                                             linearisation coefficients
       CHARACTER*20 INT_TYPE              ! type of integration
      CHARACTER*40 OBJECT_NAME           ! The name of the object
      CHARACTER*32 CHAR_ARRAY(2)         ! array to hold data units and title
      CHARACTER*40 TIME                  ! A string holding the date of the
*                                             reduction
      CHARACTER*20 DAY, DATE, HOUR       ! components of date
      CHARACTER*8 ITEM                   ! Name of FITS item
      CHARACTER*4 COMMENT                ! Dummy comment
      CHARACTER*20 LPREFIX                ! Prefix to apply
*    Internal References :
*    Local data :
*-

*    Return if entry status bad
      IF ( STATUS .NE. SAI__OK ) RETURN

*    The integration structure has already been opened as 'INT_IN'
*    and the parent observation file as 'OBSERVATION' before this
*    routine is called
*    start the preparations-
*    first try to check that we have all the information for
*    the integration reduction to proceed.
*    Get the INT_TYPE (otherwise known as the observing mode, 'STARE',
*    'CHOP' etc...)
      DSA_STATUS = SAI__OK
      CALL DSA_GET_FITS_C( 'OBSERVATION', 'INTTYPE', 0, INT_TYPE,
     :  COMMENT, DSA_STATUS )

*    Check to make sure that a CALIBRATION or ARC observation has
*    not been made in CHOP mode. (Note that a SKY observation is now
*    allowed. In CHOP mode it refers to an observation made in the
*    offset beam).
      IF ( ( INDEX(INT_TYPE,'STARE') .EQ. 0 ) .AND.
     :     ( ( TYPE .EQ. 'CALIBRATION' ) .OR.
     :       ( TYPE .EQ. 'ARC' ) ) ) THEN

         STATUS = SAI__ERROR
         CALL MSG_SETC( 'TYPE', TYPE )
         CALL ERR_REP( ' ', 'RED4_REDUCE_SPECTRA_INT: '/
     :     /'^TYPE observation was taken in chop ' /
     :     /'mode. There must have been a mistake!', STATUS )
      END IF

*   Get the number of exposures in this integration N_EXPOSURES
      CALL DSA_GET_FITS_I( 'INT_IN', 'NEXP', 0, N_EXPOSURES,
     :  COMMENT, DSA_STATUS )

      IF (DSA_STATUS .EQ. SAI__OK) THEN

         IF (N_EXPOSURES .LT. 1) THEN

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_REDUCE_SPECTRA_INT: '/
     :        /'No exposures were made during the '/
     :        /'integration', STATUS )
         ENDIF
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_SPECTRA_INT: '/
     :     /'Error getting FITS item NEXP', STATUS )
      ENDIF

*    get the exposure time in this integration EXPOSURE_TIME
      CALL DSA_GET_FITS_F( 'OBSERVATION', 'DEXPTIME', 0, EXPOSURE_TIME,
     :  COMMENT, DSA_STATUS )

      IF (DSA_STATUS .EQ. SAI__OK) THEN

         IF (EXPOSURE_TIME .LE. 0.0) THEN

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_REDUCE_SPECTRA_INT: '/
     :        /'Exposure time is negative', STATUS )
         ENDIF
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_SPECTRA_INT: '/
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

*    get the detector position index of this particular integration
      DSA_STATUS = STATUS
      CALL DSA_GET_FITS_I( 'INT_IN', 'DINDEX', 0, DET_INDEX,
     :  COMMENT, DSA_STATUS )

*    Find the size of the input data array
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

      IF ( DSA_STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_SPECTRA_INT: '/
     :     /'Error getting FLOAT size', STATUS )
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

*    Determine if the observation is to be divided by a flat-field.
      CALL PAR_GET0C( 'DIVIDE_BY_FF', DIVIDE_BY_FF, STATUS )

      IF ( DIVIDE_BY_FF .EQ. 'YES' ) THEN

         PROCEED_FF = .TRUE.

      ELSE IF ( DIVIDE_BY_FF .EQ. 'NO' ) THEN

         PROCEED_FF = .FALSE.
      ELSE

         CALL PAR_CANCL( 'PROCEED_FF', STATUS )
         CALL PAR_GET0L( 'PROCEED_FF', PROCEED_FF, STATUS )
      END IF

      IF ( VERBOSE ) THEN

         IF ( .NOT. PROCEED_FF ) CALL MSG_OUT( ' ',
     :      'No FLAT fielding will be performed', STATUS )
      END IF

*    Find the name of the observation file responsible for this
*    integration, from it construct the name of the the observation
*    index file which should have a name of the form CGS4_yymmdd.INDEX
*    Index files are found in the directory whose logical name is
*    CGS4_INDEX.
      DSA_STATUS = STATUS
      CALL DSA_GET_FITS_C( 'INT_IN', 'OBSFILE', 0, OBSFILE, COMMENT,
     :  DSA_STATUS )

      IF ( DSA_STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_SPECTRA_INT: '/
     :     /'Error getting FITS item OBSFILE', STATUS )
      END IF

      CALL RED4_OBSTOINDEX( OBSFILE, INDEX_FILE, STATUS )

*    If non-destructive reads are not being used, then we need a ready
*    reduced BIAS observation for this reduction. Either search the index
*    file for a suitable BIAS, or use one which has been specified
*    explicitly. The data, errors and quality for this observation
*    will be held in virtual memory, with the relevant pointers stored
*    in /RED4_COMMON/.
      IF ( PROCEED_BIAS .AND. (INDEX(INT_TYPE,'NDR').EQ.0) ) THEN

         CALL RED4_GET_OBSERVATION ( INDEX_FILE, 'OBSERVATION',
     :      'BIAS', BIAS_NAME, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN

            IF (BIAS_NAME .EQ. ' ') THEN

               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_REDUCE_SPECTRA_INT: '/
     :           /'No suitable reduced BIAS observation '/
     :           /'found. Data reduction aborted!', STATUS )
            ELSE

               CALL MSG_SETC( 'BIAS_NAME', BIAS_NAME )
               CALL MSG_OUT( ' ', 'Using the reduced BIAS observation '/
     :           /'^BIAS_NAME', STATUS )
            ENDIF
         ENDIF
      ENDIF

*    If the observation was taken in STARE mode, a reduced DARK observation
*    may also be needed for this reduction. Either search the index
*    file for a suitable BIAS, or use one which has been specified
*    explicitly. The data, errors and quality for this observation
*    will be held in virtual memory, with the relevant pointers stored
*    in /RED4_COMMON/.
      IF ( PROCEED_DARK .AND. (INDEX(INT_TYPE,'STARE').NE.0) ) THEN

         CALL RED4_GET_OBSERVATION ( INDEX_FILE, 'OBSERVATION',
     :      'DARK', DARK_NAME, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN

            IF (DARK_NAME .EQ. ' ') THEN

               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_REDUCE_SPECTRA_INT: '/
     :           /'No suitable reduced DARK observation '/
     :           /'found. Data reduction aborted!', STATUS )
            ELSE

               CALL MSG_SETC( 'DARK_NAME', DARK_NAME )
               CALL MSG_OUT( ' ', 'Using the reduced DARK observation '/
     :           /'^DARK_NAME', STATUS )
            ENDIF
         ENDIF
      ENDIF

*    If we are dividing by a flat field then we need a ready reduced FLAT
*    observation for this reduction. Either search the index
*    file for a suitable FLAT, or use one which has been specified
*    explicitly. The data, errors and quality for this observation
*    will be held in virtual memory, with the relevant pointers stored
*    in /RED4_COMMON/.
      IF ( PROCEED_FF ) THEN

         CALL RED4_GET_OBSERVATION ( INDEX_FILE, 'OBSERVATION',
     :      'FLAT', FLAT_NAME, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN

            IF (FLAT_NAME .EQ. ' ') THEN

               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_REDUCE_SPECTRA_INT: '/
     :           /'No suitable reduced FLAT observation '/
     :           /'found. Data reduction aborted!', STATUS)
            ELSE

               CALL MSG_SETC( 'FLAT_NAME', FLAT_NAME )
               CALL MSG_OUT( ' ', 'Using the reduced FLAT observation '/
     :           /'^FLAT_NAME', STATUS )

*            Give a warning if the FLAT has not been normalised
*            (This should perhaps be treated as an error ?)
               IF ( .NOT. FLAT_NORMALISED ) THEN

                  CALL MSG_OUT( ' ', '**************************'/
     :              /'*******************************', STATUS )
                  CALL MSG_OUT( ' ', '****** WARNING:  This FLAT'/
     :              /' has NOT been normalised ******', STATUS )
                  CALL MSG_OUT( ' ', '**************************'/
     :              /'*******************************', STATUS )
                  CALL MSG_OUT( ' ', ' ', STATUS )
                  CALL MSG_OUT( ' ',
     :              'Reset the RED4 task using', STATUS )
                  CALL MSG_OUT( ' ', '"ICL> OBEYW RED4 RESET"', STATUS )
                  CALL MSG_OUT( ' ', ' ', STATUS )
               END IF
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
         IF ( DSA_STATUS .NE. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_REDUCE_SPECTRA_INT: '/
     :        /'Error mapping MASK data', STATUS )
         END IF
      ENDIF

*    Now we're as sure as can be that everything's gonna be jus' fine,
*    so open the output file according to the reduced integration template
*    and with the correct filename e.g. RI890801_1_1 from I890801_1_1
      DSA_STATUS = STATUS
      CALL DSA_NAMED_INPUT ('INTRED_TEMPLATE',
     :   INTRED_TEMPLATE, DSA_STATUS)

      IF ( DSA_STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_SPECTRA_INT: '/
     :     /'Error mapping integration template', STATUS )
      END IF

      CALL RED4_INTTORINT( INT_NAME, INT_RED, STATUS )

      IF (STATUS .EQ. SAI__OK) THEN

         CALL MSG_SETC( 'TYPE', TYPE )
         CALL MSG_SETC( 'INT_RED', INT_RED )
         CALL MSG_OUT( ' ', 'Reducing ^TYPE integration into '/
     :     /'^INT_RED', STATUS )
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

      RED_DIMS(1) = DIMS(1)
      RED_DIMS(2) = DIMS(2)

*    make doubly sure that the variances are initialised to zero, DSA
*    should do this but doesn't always do so
      IF (DSA_STATUS .EQ. SAI__OK) THEN

         CALL GEN_FILL (FLOATSIZE*NPLANE, 0, %val(RED_VAR))
      ENDIF

      IF ( DSA_STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_SPECTRA_INT: '/
     :     /'First error in DSA routines', STATUS )
      END IF

*    Do the reduction.
*    Simpler if split it up into CHOP case and STARE
      IF (INDEX(INT_TYPE,'STARE') .NE. 0) THEN

*       Copy the Phase A plane of the integration array into the result data
*       array. If a bad pixel mask is being used, copy its contents into
*       the data quality array. Otherwise fill the data quality array
*       with zeros.
         IF (DSA_STATUS .EQ. SAI__OK) THEN

            CALL GEN_MOVE (FLOATSIZE*NPLANE, %val(IN_DATA),
     :         %val(RED_DATA))

            IF ( INDEX( MASK, '#' ) .EQ. 0 ) THEN

               CALL GEN_MOVE( NPLANE, %val(MASK_DATA), %val(RED_QUAL) )
            ELSE

               CALL GEN_FILL( NPLANE, 0, %val(RED_QUAL) )
            END IF
         ENDIF

*       Flag as "bad" any values which the ADP has set to -50.0.

         IF ( DSA_STATUS .NE. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_REDUCE_SPECTRA_INT: '/
     :         /'Error prior to flagging bad value', STATUS )
         END IF

         IF ( STATUS .EQ. SAI__OK ) THEN

            CALL RED4_FLAG_BAD( NPLANE, -50.0, %val(RED_DATA),
     :        %val(RED_QUAL), STATUS )
         END IF

*      Check everything is ok so far.
         IF (STATUS .EQ. SAI__OK) THEN

*          Work out the SECTOR statistics, if enabled
            IF (INDEX(INT_TYPE,'SECTORS') .NE. 0) THEN

               IN_DATA = IN_DATA + FLOATSIZE * NPLANE

*            In NDR mode, the statistical information will be a "standard
*            deviation" array. In other modes it will be a "sum of X squared"
*            array. These need to be converted into a "standard error squared"
*            array (incorrectly called "variance" in several places).
              IF ( INDEX(INT_TYPE, 'NDR') .NE. 0 ) THEN

                  CALL RED4_SIGMA_TO_SESQ( NPLANE, %val(IN_DATA),
     :              N_EXPOSURES, %val(RED_VAR), STATUS )
               ELSE

                  CALL RED4_SXX_TO_SESQ( %val(RED_DATA), %val(IN_DATA),
     :              NPLANE, N_EXPOSURES, %val(RED_VAR), STATUS)
               END IF
            ENDIF

*          If non-destructive reads are not being used, subtract the
*          BIAS observation from the result, propagating variances and quality
            IF ( PROCEED_BIAS .AND. (INDEX(INT_TYPE,'NDR').EQ.0) ) THEN

               CALL GEN_SUBAFV (NPLANE, %val(RED_DATA),
     :            %VAL(BIAS_DATA), %val(RED_DATA),
     :            %val(RED_QUAL), %VAL(BIAS_QUAL),
     :            %val(RED_QUAL), %val(RED_VAR),
     :            %VAL(BIAS_VAR), %val(RED_VAR),
     :            .TRUE., .FALSE., 0, .TRUE.)
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

*       Now subtract the DARK data, propagating variances and quality
         IF ( PROCEED_DARK .AND. (STATUS .EQ. SAI__OK) ) THEN

            CALL GEN_SUBAFV (NPLANE, %val(RED_DATA),
     :         %val(DARK_DATA), %val(RED_DATA),
     :         %val(RED_QUAL), %val(DARK_QUAL),
     :         %val(RED_QUAL), %val(RED_VAR),
     :         %VAL(DARK_VAR), %val(RED_VAR),
     :         .TRUE., .FALSE., 0, .TRUE.)
         ENDIF
      ELSE IF (INDEX(INT_TYPE,'CHOP') .NE. 0) THEN

*       CHOP case.
*       Get work space for phase A and B data, copy in the data arrays.
*       Copy quality array from bad pixel mask or, if there isn't one
*       initialise quality array to zero.
         DSA_STATUS = STATUS
         CALL DSA_GET_WORKSPACE (FLOATSIZE*NPLANE, PHASEA_DATA,
     :      PHASEA_SLOT, DSA_STATUS)
         CALL DSA_GET_WORKSPACE (FLOATSIZE*NPLANE, PHASEB_DATA,
     :      PHASEB_SLOT, DSA_STATUS)

         IF (DSA_STATUS .EQ. SAI__OK) THEN

            CALL GEN_MOVE (FLOATSIZE*NPLANE, %val(IN_DATA),
     :         %val(PHASEA_DATA))

            IN_DATA = IN_DATA + FLOATSIZE * NPLANE

            CALL GEN_MOVE (FLOATSIZE*NPLANE, %val(IN_DATA),
     :         %val(PHASEB_DATA))

            IF ( INDEX( MASK, '#' ) .EQ. 0 ) THEN

               CALL GEN_MOVE( NPLANE, %val(MASK_DATA),
     :           %val(RED_QUAL) )
            ELSE

               CALL GEN_FILL( NPLANE, 0, %val(RED_QUAL) )
            END IF
         ENDIF

*       Flag as "bad" any values which the ADP has set to -50.0
*       in either phase.

         IF ( DSA_STATUS .NE. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_REDUCE_SPECTRA_INT: '/
     :        /'Second error in DSA routines', STATUS )
         END IF

         CALL RED4_FLAG_BAD( NPLANE, -50.0, %val(PHASEA_DATA),
     :     %val(RED_QUAL), STATUS )
         CALL RED4_FLAG_BAD( NPLANE, -50.0, %val(PHASEB_DATA),
     :     %val(RED_QUAL), STATUS )

         IF (STATUS .EQ. SAI__OK) THEN

*          work out the SECTOR statistics if enabled, after first
*          subtracting PHASE A from PHASE B, into the result array.
            IF (INDEX(INT_TYPE,'SECTORS') .NE. 0) THEN

               CALL GEN_SUBAFV (NPLANE, %val(PHASEB_DATA),
     :            %val(PHASEA_DATA), %val(RED_DATA),
     :            0, 0, 0, 0, 0, 0, .FALSE., .FALSE., 0, .FALSE.)

               IN_DATA = IN_DATA + FLOATSIZE * NPLANE

*            In NDR mode, the statistical information will be a "standard
*            deviation" array. In other modes it will be a "sum of X squared"
*            array. These need to be converted into a "standard error squared"
*            array (incorrectly called "variance" in several places).
              IF ( INDEX(INT_TYPE, 'NDR') .NE. 0 ) THEN

                  CALL RED4_SIGMA_TO_SESQ( NPLANE, %val(IN_DATA),
     :              N_EXPOSURES, %val(RED_VAR), STATUS )
               ELSE

                  CALL RED4_SXX_TO_SESQ( %val(RED_DATA), %val(IN_DATA),
     :              NPLANE, N_EXPOSURES, %val(RED_VAR), STATUS)
               END IF
            ENDIF
         ENDIF

*       If non-destructive reads were not used, then subtract the BIAS
*       observation from the phases to prepare for linearisation,
*       variances are not handled because the BIAS errors will (just
*       about) cancel out when the phases are subtracted. The BIAS
*       quality is propagated into the result array during the
*       subtraction of the BIAS from the PHASEA array.
         IF ( PROCEED_BIAS .AND. (INDEX(INT_TYPE,'NDR').EQ.0) ) THEN

            IF (STATUS .EQ. SAI__OK) THEN

               CALL GEN_SUBAFV (NPLANE, %val(PHASEA_DATA),
     :            %VAL(BIAS_DATA), %val(PHASEA_DATA),
     :            %val(RED_QUAL), %VAL(BIAS_QUAL),
     :            %val(RED_QUAL), 0, 0, 0, .TRUE., .FALSE.,
     :            0, .FALSE.)

               CALL GEN_SUBAFV (NPLANE, %val(PHASEB_DATA),
     :            %VAL(BIAS_DATA), %val(PHASEB_DATA),
     :            0, 0, 0, 0, 0, 0, .FALSE., .FALSE., 0, .FALSE.)
            ENDIF
         ENDIF

*      Linearise the data if required. The sector statistics are linearised
*      according to the PHASE A signal
         IF ( STATUS .EQ. SAI__OK ) THEN

            IF ( INDEX( LINCOEFFS, '#' ) .EQ. 0 ) THEN

               IF ( VERBOSE ) THEN

                  CALL MSG_SETI( 'NCOEFFS', N_COEFFS )
                  CALL MSG_SETC( 'NTH', GEN_NTH(N_COEFFS) )
                  CALL MSG_SETC( 'LINCOEFFS', LINCOEFFS )
                  CALL MSG_OUT( ' ', 'Linearising both phases '/
     :              /'using ^NCOEFFS^NTH order polynomial in '/
     :              /'^LINCOEFFS', STATUS )
               END IF

               CALL RED4_LINEARISE( N_COEFFS, LCOEFFS, NPLANE,
     :            %val(PHASEA_DATA), %val(RED_VAR),
     :            %val(RED_QUAL), STATUS )

*             The reduced data array is used as a dummy variance array here,
*             it should have been set to a sensible number to avoid probs
*             with undefined numbers (often v. large).
               CALL GEN_FILL (FLOATSIZE*NPLANE, 0, %val(RED_DATA))

               CALL RED4_LINEARISE( N_COEFFS, LCOEFFS, NPLANE,
     :            %val(PHASEB_DATA), %val(RED_DATA),
     :            %val(RED_QUAL), STATUS )
            END IF
         END IF

*       No need to subtract DARK observation, just subtract PHASEA from
*       PHASEB into the result array, variances and quality are already in
*       the right place.
         IF (STATUS .EQ. SAI__OK) THEN

            CALL GEN_SUBAFV (NPLANE, %val(PHASEB_DATA),
     :         %val(PHASEA_DATA), %val(RED_DATA),
     :         0, 0, 0, 0, 0, 0, .FALSE., .FALSE., 0, .FALSE.)
         ENDIF
      ELSE

*      Neither CHOP nor STARE mode has been specified. The data
*      file must be in error.
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_SPECTRA_INT: '/
     :     /'Error as the integration type is  '/
     :     /'neither CHOP nor STARE', STATUS )
      ENDIF

*    Now, having dealt with the STARE-CHOP situation, rejoin the common
*    processing path.
*    If desired divide by the FLAT observation.
      IF ( PROCEED_FF .AND. (STATUS .EQ. SAI__OK) ) THEN

         CALL GEN_DIVAFV( NPLANE,
     :     %val(RED_DATA), %val(FLAT_DATA), %val(RED_DATA),
     :     %val(RED_QUAL), %val(FLAT_QUAL), %val(RED_QUAL),
     :     %val(RED_VAR), %val(FLAT_VAR), %val(RED_VAR),
     :     .TRUE., .FALSE., 0, .TRUE. )
      ENDIF

*    Check that the variances are non-negative, they'll crash DSA_CLOSE
*    (original Figaro version anyway) if they are
      IF ( VARIANCE_MAP .AND. (NPLANE.GT.0) ) THEN

         CALL GEN_CLIPF (%val(RED_VAR), NPLANE, 0.0,
     :      VAL__MAXR, IGNORE, IGNORE, %val(RED_VAR))
      ENDIF

*    Set the data label and units
      IF ( PROCEED_FF ) THEN

*      If the flat-field has been normalised, then the units will
*      be the same as the original spectrum. Otherwise they will
*      just be an arbitrary ratio.
         IF ( FLAT_NORMALISED ) THEN

            CHAR_ARRAY (1) = 'A/D numbers per exposure'
         ELSE

            CHAR_ARRAY (1) = 'Flat-field ratio'
         END IF

         CHAR_ARRAY(2) = TYPE(:CHR_LEN(TYPE))
         CALL CHR_CTOC( CHAR_ARRAY(2), CHAR_ARRAY(2), CLEN )

         IF ( PROCEED_BIAS .AND. (INDEX(INT_TYPE,'STARE').NE.0) ) THEN

            CHAR_ARRAY(2) = '(' // CHAR_ARRAY(2)(1:CLEN) // '-BIAS)'
            CALL CHR_CTOC( CHAR_ARRAY(2), CHAR_ARRAY(2), CLEN )
         END IF

         IF ( PROCEED_DARK .AND. (INDEX(INT_TYPE,'STARE').NE.0) ) THEN

            CHAR_ARRAY(2) = '(' // CHAR_ARRAY(2)(1:CLEN) //'-DARK)/FLAT'
            CALL CHR_CTOC( CHAR_ARRAY(2), CHAR_ARRAY(2), CLEN )
         ELSE

            CHAR_ARRAY(2) = CHAR_ARRAY(2)(1:CLEN) // '/FLAT'
            CALL CHR_CTOC( CHAR_ARRAY(2), CHAR_ARRAY(2), CLEN )
         END IF
      ELSE

         CHAR_ARRAY (1) = 'A/D numbers per exposure'

         CHAR_ARRAY (2) = TYPE(:CHR_LEN(TYPE))
         CALL CHR_CTOC( CHAR_ARRAY(2), CHAR_ARRAY(2), CLEN )

         IF ( PROCEED_BIAS .AND. (INDEX(INT_TYPE,'STARE').NE.0) ) THEN

            CHAR_ARRAY(2) = '(' // CHAR_ARRAY(2)(1:CLEN) // '-BIAS)'
            CALL CHR_CTOC( CHAR_ARRAY(2), CHAR_ARRAY(2), CLEN )
         END IF

         IF ( PROCEED_DARK .AND. (INDEX(INT_TYPE,'STARE').NE.0) ) THEN

            CHAR_ARRAY(2) = '(' // CHAR_ARRAY(2)(1:CLEN) // '-DARK)'
            CALL CHR_CTOC( CHAR_ARRAY(2), CHAR_ARRAY(2), CLEN )
         ENDIF
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
*    record how the reduction went in the FITS structure.
*    Date & time
      CALL GEN_TIME (6, DAY, LDAY, DATE, LDATE, HOUR, LHOUR)

      TIME = DATE(:LDATE)//' at '//HOUR(:LHOUR)
      CLEN = MAX( 1, CHR_LEN( TIME ) )

      CALL DSA_PUT_FITS_C( 'INT_RED', 'STREDUCE', TIME(1:CLEN), ' ',
     :  DSA_STATUS )

*    The name of the bad pixel mask, if used.
      IF ( INDEX( MASK, '#' ) .EQ. 0 ) THEN

         CALL DSA_PUT_FITS_C( 'INT_RED', 'MASKUSED',
     :     MASK, ' ', DSA_STATUS)
      ELSE IF ( PROCEED_FF ) THEN

         CALL DSA_PUT_FITS_C( 'INT_RED', 'MASKUSED', 'Propagated '/
     :     /'from FLAT observation', ' ', DSA_STATUS)
      ELSE IF ( PROCEED_DARK .AND.
     :          (INDEX(INT_TYPE,'STARE').NE.0) ) THEN

         CALL DSA_PUT_FITS_C( 'INT_RED', 'MASKUSED', 'Propagated '/
     :     /'from DARK observation', ' ', DSA_STATUS)
      ELSE IF ( PROCEED_BIAS .AND.
     :          (INDEX(INT_TYPE,'NDR').NE.0) ) THEN

         CALL DSA_PUT_FITS_C( 'INT_RED', 'MASKUSED', 'Propagated '/
     :     /'from BIAS observation', ' ', DSA_STATUS)
      ELSE

         CALL DSA_PUT_FITS_C( 'INT_RED', 'MASKUSED', '(none)',
     :     ' ', DSA_STATUS)
      END IF

*    the name of the BIAS observation, if used
      IF ( PROCEED_BIAS .AND. (INDEX(INT_TYPE,'NDR').EQ.0) ) THEN

         CLEN = MAX( 1, CHR_LEN( BIAS_NAME ) )
         CALL DSA_PUT_FITS_C( 'INT_RED', 'BIASUSED',
     :     BIAS_NAME(1:CLEN), ' ', DSA_STATUS )
      ELSE

         CALL DSA_PUT_FITS_C( 'INT_RED', 'BIASUSED', '(none)',
     :     ' ', DSA_STATUS )
      ENDIF

*    the name of the DARK observation, if used
      IF ( PROCEED_DARK .AND. (INDEX(INT_TYPE,'STARE').NE.0) ) THEN

         CLEN = MAX( 1, CHR_LEN( DARK_NAME ) )
         CALL DSA_PUT_FITS_C( 'INT_RED', 'DARKUSED',
     :     DARK_NAME(1:CLEN), ' ', DSA_STATUS )
      ELSE

         CALL DSA_PUT_FITS_C( 'INT_RED', 'DARKUSED', '(none)',
     :     ' ', DSA_STATUS )
      ENDIF

*    the name of the FLAT observation, if used.
      IF ( PROCEED_FF ) THEN

         CLEN = MAX( 1, CHR_LEN( FLAT_NAME ) )
         CALL DSA_PUT_FITS_C( 'INT_RED', 'FLATUSED',
     :     FLAT_NAME(1:CLEN), ' ', DSA_STATUS )
      ELSE

         CALL DSA_PUT_FITS_C( 'INT_RED', 'FLATUSED', '(none)',
     :     ' ', DSA_STATUS )
      ENDIF

*    The integration time. (Write this in both the FITS structure
*    and the standard Figaro structure).
      CALL DSA_PUT_FITS_F( 'INT_RED', 'EXPOSED', INTEGRATION_TIME,
     :  ' ', DSA_STATUS )
      CALL DSA_SET_EXPOSURE( 'INT_RED', INTEGRATION_TIME, DSA_STATUS )

*    whether linearised or not & the linearisation coefficients used
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

      IF ( DSA_STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_SPECTRA_INT: '/
     :     /'Error setting FITS items', STATUS )
      END IF

      END
