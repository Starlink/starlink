*+  RED4_REDUCE_SPECTRA_OBS - Reduce a complete SPECTRA observation
      SUBROUTINE RED4_REDUCE_SPECTRA_OBS (OBS_NAME, TYPE, STATUS)
*    Description :
*     This routine takes a SPECTRA observation of name Oyymmdd_oooo in ODIR,
*     searches IDIR (the raw integration directory) for the integrations that
*     belong to it (Iyymmdd_oooo_iiii) and reduces them all at once into a reduced
*     observation file ROyymmdd_oooo in RODIR. Individual files containing
*     reduced integrations are not produced.
*
*     If the integration was a STARE the reduction of each integration
*     consists of:-
*                if non-destructive reads are not being used
*                   subtract suitable BIAS (any)
*                linearise
*                subtract suitable DARK (same chip exposure time)
*                divide by suitable FLAT (same chip exposure time
*                   and optical configuration), if required.
*
*     If the integration was a CHOP the reduction of each integration
*     consists of:-
*                if non-destructive reads are not being used
*                   subtract suitable BIAS (any) from phase A and phase B
*                linearise phase A and phase B
*                subtract phase A from phase B
*                divide by suitable FLAT (same chip exposure time
*                   and optical configuration), if required.
*
*     Quality is propagated from all the reduced observations used in the
*     reduction, but not variances.
*
*     The result of each integration is coadded into the result for the
*     observation. Variances are calculated from the spread of the numbers
*     about the mean.
*
*     Weighted coadds are not treated properly yet.
*
*     When this routine is entered the parent observation file has already been
*     opened by the DSA system, and a file to hold the reduced observation
*     has been created and opened.
*
*    Invocation :
*     CALL RED4_REDUCE_SPECTRA_OBS (OBS_NAME, STATUS)
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*    Method :
*    Deficiencies :
*     The description says weighted coadds are not treated properly yet.
*     What does this mean ? Is it serious ?
*
*     The code is badly structured, and the error checking and reporting
*     leaves a lot to be desired. Misleading error messages can
*     sometimes be produced.
*
*     Note that the logical names IDIR:, ODIR:, RIDIR:, RODIR: CANNOT
*     be changed, because the character handling in the code has
*     assumptions about these names hard-wired in various places.
*
*     The size of the bad pixel mask is only checked against the first
*     integration, which is not consistent with RED4_REDUCE_BIAS_OBS
*     or RED4_REDUCE_DARK_OBS.
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
*     26-Feb-1990: Original                                       (JFL)
*      2-Mar-1990: Status check added before call to
*                  RED4_INDEX_ARITHMETIC, to prevent an adjustable
*                  array bounds violation.                        (SMB)
*      4-Mar-1990: VARIANCE_MAP added to ensure that variance array is checked
*                  for negative numbers if it has been mapped, even if status
*                  is bad at the checkpoint.                      (JFL)
*      7-Mar-1990: Bug fix. In CHOP mode phase A was being
*                  linearised twice instead of A and B.           (SMB)
*      9-Mar-1990: MAXDIM and RMAXDIM parameters added.           (SMB)
*      9-Mar-1990: Modified to report type of integrations being
*                  reduced.                                       (SMB)
*      9-Mar-1990: Division by number of exposures commented out,
*                  since this operation is now carried out
*                  automatically by the ADP. (If this works, the
*                  code may be deleted).                          (SMB)
*     12-Mar-1990: Bug fix. Superfluous call to DSA_CLOSE removed
*                  Mistakes in comments corrected.                (SMB)
*     23-Apr-1990: FLAT_FIELD changed to FLAT.                    (SMB)
*     24-Apr-1990: Because of memory corruption problems, the
*                  code needs to be compiled with array bounds
*                  checking switched on. The Figaro dynamic
*                  memory functions (DYN_ELEMENT, DYNAMIC_MEM,
*                  DYN_INCREMENT) would not allow this. Code
*                  modified to use %val() instead.                (SMB)
*     24-Apr-1990: DYN_INCREMENT replaced by direct
*                  manipulation of address (with the aid of
*                  DSA_TYPESIZE).                                 (SMB)
*      1-May-1990: Experiments with the debugger have shown
*                  that it is not necessary to initialise the
*                  data and variance arrays in the reduced
*                  observation, since these are already
*                  initialised by RED4_MAKE_OBSRED. This code
*                  commented out, as it was inconsistent with
*                  RED4_REDUCE_BIAS_OBS, RED4_REDUCE_DARK_OBS
*                  and RED4_REDUCE_FLAT_OBS.                        (SMB)
*      2-May-1990: Add check for (NELM_OBS.GT.0) as well as check
*                  on VARIANCE_MAP.                               (SMB)
*      2-May-1990: The routines DSA_GET_DATA_INFO and
*                  DSA_SET_DATA_INFO require a double
*                  precision array for the NUM_ARRAY argument.
*                  This routine was giving an integer (0),
*                  which could cause problems. Fixed.             (SMB)
*      4-May-1990: Superfluous messages commented out.            (SMB)
*      4-May-1990: All masks now written to CGS4_MASKS
*                  directory.                                     (SMB)
*     11-Jun-1990: This routine crashed when presented with an
*                  observation which was neither STARE or CHOP.
*                  Extra check added.                             (SMB)
*      5-Jul-1990: Commented out code removed. Modified so that
*                  flat field observations to not have to be
*                  oversampled - RED4_INDEX_ARITHMETIC replaced
*                  with GEN_DIVAFE.                               (SMB)
*      6-Jul-1990: Made to write object name and exposure
*                  time in a standard Figaro structure (as
*                  well as the special CGS4 structure). (Why
*                  was this not done in the first place??).       (SMB)
*      9-Jul-1990: Dark subtraction made optional. Parameter names
*                  made more consistent with CRED4. Hard-wired
*                  "4"s replaced by "FLOATSIZE". Ability to specify
*                  a bad pixel mask as well as propagating data
*                  quality during reduction added.                (SMB)
*     20-Jul-1990: "Integration does not exist" message was
*                  confusing. Replaced.                           (SMB)
*     23-Jul-1990: Checked to ensure that 4 digit
*                  observation and integration numbers can
*                  be processed.                              (SMB)
*     31-Jul-1990: The 15 character limit on the length of
*                  the name of the DSA COADDS structures was
*                  causing problems when the observation and
*                  integration numbers became large. COADDS
*                  structure naming convention changed from
*                  Iyymmdd_oooo_iiii to I_oooo_iiii.          (SMB)
*     28-Aug-1990: Phase 1 of major changes: Header
*                  information in the observation and reduced
*                  observation files is now written in a .FITS
*                  structure. Linearisation coefficients are
*                  now obtained from a file rather than being
*                  buried in the data structure. KTC mode
*                  replaced by NDR. CHOP mode modified so that
*                  phase A is subtracted from phase B, rather
*                  than the other way round.                  (SMB)
*      3-Sep-1990: Phase 2 of major changes: Header
*                  information in the integration and reduced
*                  integration files is now written in a .FITS
*                  structure.                                 (SMB)
*      4-Sep-1990: ERR_OUT replaced with ERR_REP.             (SMB)
*      6-Sep-1990: The phase 2 changes made the reduced
*                  observation data structure created when
*                  reducing a whole observation in one go
*                  different from that created when reducing
*                  one integration at a time. This effect fixed
*                  by copying the .FITS structure from the raw
*                  integration to the CGS4_COADDS structure,
*                  rather than from the reduced integration
*                  template.                                  (SMB)
*      7-Sep-1990: Typing mistakes fixed.                     (SMB)
*      7-Sep-1990: Output made less verbose.                  (SMB)
*     14-Sep-1990: MASKUSED, BIASUSED, DARKUSED, FLATUSED and
*                  LINEARIS parameters written to FITS
*                  structure of reduced observation file.     (SMB)
*     14-Sep-1990: Code simplified by encapsulating low-level
*                  DTA calls in RED4_COPY_STRUCTURE.          (SMB)
*     26-Sep-1990: Dimension check between integrations and
*                  bad pixel mask included.                   (SMB)
*     27-Sep-1990: Dimension check didn't work because for
*                  some reason the first integration is
*                  opened before the mask and subsequent
*                  integrations are opened afterwards. Check
*                  now only with first integration when mask
*                  opened.                                    (SMB)
*     24-Oct-1990: SKY, CALIBRATION and ARC observations
*                  taken in CHOP mode are not allowed.        (SMB)
*     25-Oct-1990: Data should not be labelled OBJECT - DARK
*                  when in CHOP mode. Modified to OBJECT.     (SMB)
*      1-Nov-1990: Normalisation to 1 second exposure time
*                  removed. Signal will now be given as
*                  A/D numbers per exposure.                  (SMB)
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
*     13-Feb-1991: Modified to reject observations with no
*                  integrations or with an incomplete number
*                  of scans.                                  (SMB)
*     22-Feb-1991: Trap for SKY observation taken in CHOP mode
*                  removed.                                   (SMB)
*     24-Feb-1991: Some error reporting mistakes fixed,
*                  which would have made this routine
*                  fail under ADAM V1.9.                      (SMB)
*     24-Feb-1991: DSA error statuses trapped, as these do
*                  not conform to the ADAM error scheme.      (SMB)
*     29-Jul-1991: Bug reported by Andy Adamson on 28-JUL-1991
*                  fixed. In CHOP mode, the status was not
*                  being checked properly, which lead to an
*                  adjustable array bounds error. Corrected.  (SMB)
*      3-Sep-1991: GEN_MULCAFE renamed to GEN_MULCAFEV. The CGS4
*                  software was assuming this routine dealt with
*                  variances, but the actual Figaro routine dealt
*                  with standard deviation.                   (SMB)
*     11-Sep-1991: GEN_MULCAFEV renamed to GEN_MULCAFV, and argument
*                  list made compatible with Figaro version.  (SMB)
*      1-Oct-1991: Change other GEN_routines.                 (PND)
*      8-Oct-1991: Call RED4_GET_OBSERVATION instead of
*                  RED4_SEEK_OBSERVATION, so that calibration
*                  observations can be specified explicitly when
*                  required (DRIMP/5.1 and 5.2).              (SMB)
*     23-Jul-1992: Add SUBTRACT_BIAS option                   (PND)
*      7-Dec-1992: Remove COPY_STRUCTURE error report         (PND)
*     22-Feb-1993: Conform to error strategy                  (PND)
*     30-Jun-1993: Add message for non-normalised FF          (PND)
*      7-Jan-1993: Allow NDFs, include PRM_PAR                (PND)
*      3-May-1995: Add support for ALICE CHOP data            (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
*    Status :
      INTEGER STATUS
*    Input :
      CHARACTER*(*) OBS_NAME             ! the basic name of the observation
*                                             file to be reduced, e.g.O890816_1
      CHARACTER*(*) TYPE                 ! the particular type of 'SPECTRA'
*                                             this is; 'CALIBRATION', 'SKY',
*                                             'STANDARD', 'ARC' or 'OBJECT'
*    External references :
      INTEGER DSA_TYPESIZE               ! DSA type size enquiry function
      INTEGER CHR_LEN                    ! ADAM stringlength function
      CHARACTER*13 ICH_CI                ! Figaro integer->string function
      CHARACTER*2 GEN_NTH                ! Figaro "Nth" determination function
*                                            (i.e. 1st, 2nd, 3rd, 4th ...)
*    Global variables :
      INCLUDE 'RED4_COMMON.INC'          ! 'Virtual common' block, holding
*                                             reduced observations required
*                                             for this reduction.
*    Local variables :
      INTEGER FLOATSIZE                  ! Bytes per element of 'FLOAT' array
      INTEGER BYTESIZE                   ! Bytes per element of 'BYTE' array
      LOGICAL LOOPING                    ! T while looping through component
*                                            integrations
      LOGICAL EXIST                      ! T if the integration file currently
*                                            being searched for exists
      LOGICAL FIRST                      ! T if this is the first integration
*                                            in the observation to be reduced
      LOGICAL PROCEED_BIAS               ! T if subtracting BIAS observation
      LOGICAL PROCEED_DARK               ! T if subtracting DARK observation
      LOGICAL PROCEED_FF                 ! T if dividing by a FLAT
*                                            observation
      LOGICAL VARIANCE_MAP               ! T if variance array of reduced
*                                            observation has been mapped
      INTEGER INTEGRATION                ! number of integration currently being
*                                            added to the observation
      INTEGER TOTAL_INTS                 ! total number of integrations
      INTEGER NDIM                       ! the dimensions of the integration
*                                             array
      INTEGER DIMS( MAXDIM )             !                "
      INTEGER N_EXPOSURES                ! the number of exposures that were
*                                             coadded in the integration
      INTEGER DET_INDEX                  ! the index of the detector position
*                                             at which the current integration
*                                             was taken
      INTEGER DET_NINCR                  ! Detector oversampling factor.
      INTEGER INT_SLOT                   !  for integration data
      INTEGER INT_DATA                   !        "
      INTEGER INT2_SLOT                  !  for integration data
      INTEGER INT2_DATA                  !        "
      INTEGER DATA_SLOT                  !  for reduce observation data
      INTEGER RED_DATA                   !        "
      INTEGER QUAL_SLOT                  !  quality
      INTEGER RED_QUAL                   !        "
      INTEGER VAR_SLOT                   !  variances
      INTEGER RED_VAR                    !        "
      INTEGER MASK_SLOT                  !  quality mask
      INTEGER MASK_DATA                  !        "
      INTEGER COADDS_SLOT                !  coadds data in reduced observation
      INTEGER COADDS_PTR                 !        "
      INTEGER INDEX_SLOT                 !  index array in reduced observation.
*                                              the index array points each
*                                              column of an integration at a
*                                              particular detector position to
*                                              its place in the reduced data
      INTEGER INDEX_PTR                  !        "
      INTEGER WORK_SLOT                  !  temporary work area
      INTEGER WORK_DATA                  !        "
      INTEGER WORK_V_SLOT                !  with variances
      INTEGER WORK_VAR                   !        "
      INTEGER WORK_Q_SLOT                !  and quality
      INTEGER WORK_QUAL                  !        "
      INTEGER PHASEA_SLOT                !  temporary storage for phase A
      INTEGER PHASEA_DATA                !        "
      INTEGER PHASEB_SLOT                !  temporary storage for phase B
      INTEGER PHASEB_DATA                !        "
      INTEGER NELM                       ! number of elements in integration
*                                             array
      INTEGER NELM_OBS                   ! number of elements in reduced
*                                             observation data array
      INTEGER NPLANE                     ! number of elements in one plane of
*                                             the integration array
      INTEGER OBSDIMS( RMAXDIM )         ! dimensions of reduced observation
*                                             data
      INTEGER INTDIMS( RMAXDIM )         ! dimensions of reduced integration
*                                             data
      INTEGER INDEX_DIMS( RMAXDIM )      ! dimensions of index array in
*                                             reduced observation data.
      INTEGER N_COEFFS                   ! number of non-zero coefficients in
*                                             array linearisation polynomial
      INTEGER I                          ! DO loop counter
      INTEGER LDAY, LDATE, LHOUR         ! lengths of date strings
      INTEGER NLOW                       ! unimportant parameter
      INTEGER NHIGH                      ! unimportant parameter
      INTEGER CLEN                       ! Non-blank length of character string
      INTEGER CPOS                       ! Position in character string.
      INTEGER IPOS                       ! Position in character string.
      REAL EXPOSURE_TIME                 ! on-chip exposure time
      REAL INTEGRATION_TIME              ! EXPOSURE_TIME * N_EXPOSURES
      REAL OBSERVATION_TIME              ! total integration for det_index 1
*                                             columns in reduced observation
      DOUBLE PRECISION
     :  LCOEFFS( MAXCOEFFS )             ! Linearisation coefficients array
      CHARACTER*4 DIVIDE_BY_FF           ! Controls whether the
*                                        !    observation is to be divided by
*                                        !    a flat-field (YES, NO or ASK)
      CHARACTER*4 SUBTRACT_BIAS          ! Controls whether the
*                                        !    observation is to be BIAS
*                                        !    subtracted (YES, NO or ASK)
      CHARACTER*4 SUBTRACT_DARK          ! Controls whether the
*                                        !    observation is to be DARK
*                                        !    subtracted (YES, NO or ASK)
      CHARACTER*20 LPREFIX               ! Local prefix
      CHARACTER*80 INDEX_FILE            ! The name of the relevant index file.
      CHARACTER*80 BIAS_NAME           ! the name of the file containing the
*                                             reduced BIAS observation used
      CHARACTER*80 DARK_NAME           ! the name of the file containing the
*                                             reduced DARK observation used
      CHARACTER*80 FLAT_NAME             ! the name of the file containing the
*                                             reduced FLAT observation
*                                             used
      CHARACTER*80 MASK                  ! the name of the file containing
*                                             the bad pixel mask
      CHARACTER*80 LINCOEFFS             ! the name of the file containing
*                                             the linearisation coeffs
      CHARACTER*80 COADDED_INTS          ! DTA name of .COADDED_INTS structure
*                                             in reduced observation file
      CHARACTER*80 OBSREDFILE            ! full name of file holding reduced obs
      CHARACTER*80 INTNAME               ! the name of a particular integration
*                                             belonging to the observation
*                                             being reduced
      CHARACTER*80 INT2NAME              ! the name of a particular integration
      CHARACTER*80 COADD_NAME            ! The name of the COADD structure
*                                        !    corresponding to an integration
      CHARACTER*80 ROOT                  ! the root from which the INTNAMEs are
*                                             derived
      CHARACTER*20 INT_TYPE              ! type of integration
      CHARACTER*80 RECORD                ! DTA name of reduction record extensions
*                                             in reduced integration and
*                                             observation structures
      CHARACTER*40 OBJECT_NAME           ! The name of the object
      CHARACTER*32 CHAR_ARRAY(2)         ! array to hold data units and title
      CHARACTER*40 TIME                  ! A string holding the date of the
*                                             reduction
      CHARACTER*20 DAY, DATE, HOUR       ! components of date
      CHARACTER*8 ITEM                   ! Name of FITS item
      CHARACTER*4 COMMENT                ! Dummy comment
*-

*    Return if entry status bad
      IF ( STATUS.NE.SAI__OK ) RETURN

*    The observation file has already been opened as 'OBS_IN', and a file
*    created to contain the reduced observation and opened as 'OBS_RED',
*    before this routine is called. Get the INT_TYPE and uppercase it.
      CALL DSA_GET_FITS_C( 'OBS_IN', 'INTTYPE', 0, INT_TYPE, COMMENT, STATUS )
      CALL CHR_UCASE( INT_TYPE )

*    Check to make sure that a CALIBRATION or ARC observation has
*    not been made in CHOP mode. (Note that a SKY observation is now
*    allowed. In CHOP mode it refers to an observation made in the
*    offset beam).
      IF ( (INDEX(INT_TYPE,'STARE').EQ.0) .AND. ((TYPE.EQ.'CALIBRATION').OR.(TYPE.EQ.'ARC')) ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'TYPE', TYPE )
         CALL ERR_REP( ' ', 'RED4_REDUCE_SPECTRA_OBS: '/
     :     /'^TYPE observation was taken in chop mode. There must have been a mistake!', STATUS )
         GOTO 500
      ENDIF

*    Find out if the data are to be linearised with some previously
*    defined linearisation polynomial. The polynomial coefficients
*    are held in a text file. If the name of the file is given
*    as '#', then no linearisation will be carried out.
      CALL PAR_GET0C( 'LINCOEFFS', LINCOEFFS, STATUS )
      CALL PAR_CANCL( 'LINCOEFFS', STATUS )
      CALL CHR_UCASE( LINCOEFFS )
      IF ( INDEX( LINCOEFFS, '#' ) .EQ. 0 ) THEN
        CALL RED4_READ_COEFFS( LINCOEFFS, MAXCOEFFS, N_COEFFS, LCOEFFS, STATUS )
      ENDIF

*    Get the chip exposure time for the integrations in this observation
      CALL DSA_GET_FITS_F( 'OBS_IN', 'DEXPTIME', 0, EXPOSURE_TIME, COMMENT, STATUS )
      IF ( EXPOSURE_TIME .LE. 0.0 ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'RED4_REDUCE_SPECTRA_OBS: '/
     :    /'Exposure time is negative', STATUS )
        GOTO 500
      ENDIF

*    Get the detector oversampling factor.
      CALL DSA_GET_FITS_I( 'OBS_IN', 'DETNINCR', 0, DET_NINCR, COMMENT, STATUS )

*    Map in the data array of the reduced observation file, and the variance and quality arrays.
      CALL DSA_USE_QUALITY( 'OBS_RED', STATUS )
      CALL DSA_MAP_DATA( 'OBS_RED', 'WRITE', 'FLOAT', RED_DATA, DATA_SLOT, STATUS )
      CALL DSA_MAP_QUALITY( 'OBS_RED', 'WRITE', 'BYTE', RED_QUAL, QUAL_SLOT, STATUS )
      CALL DSA_MAP_VARIANCE( 'OBS_RED', 'WRITE', 'FLOAT', RED_VAR, VAR_SLOT, STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN
         VARIANCE_MAP = .TRUE.
      ELSE
         VARIANCE_MAP = .FALSE.
         CALL ERR_ANNUL( STATUS )
      ENDIF

*    Obtain the number of bytes per element in a data array of types 'FLOAT' and 'BYTE' from DSA
      FLOATSIZE = DSA_TYPESIZE( 'FLOAT', STATUS )
      BYTESIZE  = DSA_TYPESIZE( 'BYTE', STATUS )

*   Determine the size of the data arrays in the reduced observation file.
      CALL DSA_DATA_SIZE( 'OBS_RED', RMAXDIM, NDIM, OBSDIMS, NELM_OBS, STATUS )

*    Open the COADDS structure of the reduced observation file. This will
*    hold info on how many coadds have gone into the result array. Map in
*    its data array, and get the DTA name of the associated .COADDED_INTS
*    structure that will hold the (compressed) names of those
*    integrations that are added
      CALL DSA_GET_ACTUAL_NAME( 'OBS_RED', OBSREDFILE, STATUS )
      CALL DSA_NAMED_INPUT( 'COADDS',
     :   OBSREDFILE(1:CHR_LEN(OBSREDFILE))//'.MORE.CGS4_COADDS', STATUS )
      CALL DSA_MAP_DATA ('COADDS', 'UPDATE', 'SHORT', COADDS_PTR, COADDS_SLOT, STATUS )
      CALL DSA_SPECIFIC_STRUCTURE( 'COADDS', 'COADDED_INTS', 'UPDATE', COADDED_INTS, STATUS )

*    Map in the index array of the reduced observation file
      CALL DSA_NAMED_INPUT ( 'INDEX',
     :   OBSREDFILE(1:CHR_LEN(OBSREDFILE))//'.MORE.CGS4_INDEX', STATUS )
      CALL DSA_MAP_DATA( 'INDEX', 'READ', 'SHORT', INDEX_PTR, INDEX_SLOT, STATUS )
      CALL DSA_DATA_SIZE( 'INDEX', RMAXDIM, NDIM, INDEX_DIMS, NELM, STATUS )

*    Trap errors so far
      IF ( STATUS.NE.SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_SPECTRA_OBS: '/
     :     /'Error opening or mapping data', STATUS )
         GOTO 500
      ENDIF

*    Determine if the observation is to be bias-subtracted
      CALL PAR_GET0C( 'SUBTRACT_BIAS', SUBTRACT_BIAS, STATUS )
      CALL CHR_UCASE( SUBTRACT_BIAS )
      IF ( SUBTRACT_BIAS.EQ.'YES' ) THEN
         PROCEED_BIAS = .TRUE.
      ELSE IF ( SUBTRACT_BIAS.EQ.'NO' ) THEN
         PROCEED_BIAS = .FALSE.
      ELSE
         CALL PAR_CANCL( 'PROCEED_BIAS', STATUS )
         CALL PAR_GET0L( 'PROCEED_BIAS', PROCEED_BIAS, STATUS )
      ENDIF

      IF ( INDEX(INT_TYPE,'NDR').NE.0 ) PROCEED_BIAS = .FALSE.

      IF ( VERBOSE .AND. .NOT.PROCEED_BIAS ) THEN
        CALL MSG_OUT( ' ', 'No BIAS subtraction will be performed', STATUS )
      ENDIF

*    Determine if the observation is to be dark-subtracted
      CALL PAR_GET0C( 'SUBTRACT_DARK', SUBTRACT_DARK, STATUS )
      CALL CHR_UCASE( SUBTRACT_DARK )
      IF ( SUBTRACT_DARK.EQ.'YES' ) THEN
         PROCEED_DARK = .TRUE.
      ELSE IF ( SUBTRACT_DARK.EQ.'NO' ) THEN
         PROCEED_DARK = .FALSE.
      ELSE
         CALL PAR_CANCL( 'PROCEED_DARK', STATUS )
         CALL PAR_GET0L( 'PROCEED_DARK', PROCEED_DARK, STATUS )
      ENDIF

      IF ( VERBOSE .AND. .NOT.PROCEED_DARK ) THEN
         CALL MSG_OUT( ' ', 'No DARK subtraction will be performed', STATUS )
      ENDIF

*    Determine if the observation is to be divided by a flat-field.
      CALL PAR_GET0C( 'DIVIDE_BY_FF', DIVIDE_BY_FF, STATUS )
      CALL CHR_UCASE( DIVIDE_BY_FF )
      IF ( DIVIDE_BY_FF.EQ.'YES' ) THEN
         PROCEED_FF = .TRUE.
      ELSE IF ( DIVIDE_BY_FF.EQ.'NO' ) THEN
         PROCEED_FF = .FALSE.
      ELSE
         CALL PAR_CANCL( 'PROCEED_FF', STATUS )
         CALL PAR_GET0L( 'PROCEED_FF', PROCEED_FF, STATUS )
      ENDIF

      IF ( VERBOSE .AND. .NOT.PROCEED_FF ) THEN
        CALL MSG_OUT( ' ', 'No FLAT fielding will be performed', STATUS )
      ENDIF

*    Now loop through the possible integration files, stopping when we come across one that doesn't exist.
      INTEGRATION = 0
      CPOS = INDEX( OBS_NAME, ':' )
      IF (CPOS .EQ. 0) CPOS = INDEX( OBS_NAME, '/')
      CLEN = CHR_LEN( OBS_NAME )

      CALL RED4_GET_PREFIX( 'I', LPREFIX, STATUS)
      ROOT = LPREFIX(:CHR_LEN(LPREFIX))//'i'//OBS_NAME(2+CPOS:CLEN)//'_'

      LOOPING = .TRUE.
      FIRST = .TRUE.
      OBSERVATION_TIME = 0.0

      DO WHILE ( LOOPING )

*       Construct integration file name
         INTEGRATION = INTEGRATION + 1
         INTNAME  = ROOT(1:CHR_LEN(ROOT))//ICH_CI(INTEGRATION)

*       We've finished if it doesn't exist (but check chop phases A and B)
         EXIST = .FALSE.
         CALL DSA_SEEK_NAMED_STRUCTURE( INTNAME, EXIST, STATUS )
         IF ( .NOT.EXIST ) THEN
           INTNAME  = INTNAME(1:CHR_LEN(INTNAME))//'a'
           IF ( INDEX(INT_TYPE,'CHOP').NE.0 ) THEN
             CALL MSG_SETC( 'INTNAME', INTNAME )
             CALL MSG_OUT( ' ', 'Seeking CHOP integration ^INTNAME', STATUS )
           ENDIF
           CALL DSA_SEEK_NAMED_STRUCTURE( INTNAME, EXIST, STATUS )
         ENDIF

         IF ( .NOT.EXIST ) THEN

           TOTAL_INTS = INTEGRATION - 1
           IF ( TOTAL_INTS.EQ.0 ) THEN
             STATUS = SAI__ERROR
             CALL ERR_REP( ' ', 'RED4_REDUCE_SPECTRA_OBS: '/
     :         /'This observation has no integrations', STATUS )
             GOTO 500
           ELSE IF ( MOD(TOTAL_INTS,DET_NINCR).NE.0 ) THEN
             STATUS = SAI__ERROR
             CALL ERR_REP( ' ', 'RED4_REDUCE_SPECTRA_OBS: '/
     :         /'This observation does not contain '/
     :         /'a whole number of scans', STATUS )
             GOTO 500
           ELSE IF ( TOTAL_INTS.EQ.1 ) THEN
             CALL MSG_OUT( ' ', '1 integration only', STATUS )
           ELSE
             CALL MSG_SETI( 'TOTAL_INTS', TOTAL_INTS )
             CALL MSG_OUT( ' ', '^TOTAL_INTS integrations in total', STATUS )
           ENDIF

           LOOPING = .FALSE.
         ELSE

*          Issue a message
            IF ( VERBOSE ) THEN
              CALL MSG_SETC( 'TYPE', TYPE )
              CALL MSG_SETC( 'INTNAME', INTNAME )
              CALL MSG_OUT( ' ', 'Reducing ^TYPE integration ^INTNAME', STATUS )
            ENDIF

*          Open the file
            CALL DSA_NAMED_INPUT( 'INT_IN', INTNAME, STATUS )

*          Get the number of exposures in this integration N_EXPOSURES
            CALL DSA_GET_FITS_I( 'INT_IN', 'NEXP', 0, N_EXPOSURES, COMMENT, STATUS )
            IF ( N_EXPOSURES.LT.1 ) THEN
              STATUS = SAI__ERROR
              CALL ERR_REP( ' ', 'RED4_REDUCE_SPECTRA_OBS: '/
     :          /'No exposures were made during the integration', STATUS )
              GOTO 500
            ENDIF

*          Get the detector position index of this particular integration
            CALL DSA_GET_FITS_I( 'INT_IN', 'DINDEX', 0, DET_INDEX, COMMENT, STATUS )

*          Check that the index array can handle this index position (always
*          should do but the program will crash*?! if it doesn't)
            IF ( (INDEX_DIMS(2).LT.DET_INDEX) .OR. (DET_INDEX.LT.1) ) THEN
              STATUS = SAI__ERROR
              CALL ERR_REP( ' ', 'RED4_REDUCE_SPECTRA_OBS: '/
     :          /'Detector position is outside '/
     :          /'range of the index array for this observation', STATUS )
            ENDIF

*          Work out integration time
            INTEGRATION_TIME = N_EXPOSURES * EXPOSURE_TIME
            IF ( DET_INDEX.EQ.1 ) THEN
               OBSERVATION_TIME = OBSERVATION_TIME + INTEGRATION_TIME
            ENDIF

*          If this is the first integration to be reduced, search for
*          reduced BIAS, DARK and FLAT observations that will be required.
*          ALL INTEGRATIONS WILL BE REDUCED USING THE SAME SET OF OBSERVATIONS!
            IF ( FIRST ) THEN

*             Reset flag
               FIRST = .FALSE.

*            Construct the name of the the observation index file,
*            which should have a name of the form CGS4_yymmdd.INDEX
*            Index files are found in the directory whose logical name is CGS4_INDEX.
               CALL RED4_OBSTOINDEX( OBS_NAME, INDEX_FILE, STATUS )

*             Obtain the name of a suitable BIAS observation if
*             non-destructive reads are not being used. Either search for
*             a suitable BIAS observation in the index file, or use one which
*             has been explicitly specified. The data, errors and quality for
*             this observation will be held in virtual memory, with the
*             relevant pointers stored in /RED4_COMMON/.
*             (Note that subtraction of a BIAS observation is compulsory when
*             destructive reads are being used).
               IF ( PROCEED_BIAS .AND. (INDEX(INT_TYPE,'NDR').EQ.0) ) THEN
                 CALL RED4_GET_OBSERVATION( INDEX_FILE, 'OBS_IN', 'BIAS', BIAS_NAME, STATUS )
                 IF ( BIAS_NAME.EQ.' ' ) THEN
                   STATUS = SAI__ERROR
                   CALL ERR_REP( ' ', 'RED4_REDUCE_SPECTRA_OBS: '/
     :               /'No suitable reduced BIAS observation found. '/
     :               /'Data reduction aborted!', STATUS )
                   GOTO 500
                 ELSE
                   CALL MSG_SETC( 'BIAS_NAME', BIAS_NAME )
                   CALL MSG_OUT( ' ', 'Using the reduced BIAS '/
     :               /'observation in ^BIAS_NAME', STATUS)
                 ENDIF
               ENDIF

*             Obtain the name of a suitable DARK observation if the integration
*             was taken in STARE mode. Either search for a suitable DARK
*             observation in the index file, or use one which has been
*             explicitly specified. The data, errors and quality for
*             this observation will be held in virtual memory, with the
*             relevant pointers stored in /RED4_COMMON/.
               IF ( PROCEED_DARK .AND. (INDEX(INT_TYPE,'STARE').NE.0) ) THEN
                 CALL RED4_GET_OBSERVATION( INDEX_FILE, 'OBS_IN', 'DARK', DARK_NAME, STATUS )
                 IF ( DARK_NAME.EQ.' ' ) THEN
                   STATUS = SAI__ERROR
                   CALL ERR_REP( ' ', 'RED4_REDUCE_SPECTRA_OBS: '/
     :               /'No suitable reduced DARK observation found. '/
     :               /'Data reduction aborted!', STATUS )
                   GOTO 500
                 ELSE
                   CALL MSG_SETC( 'DARK_NAME', DARK_NAME )
                   CALL MSG_OUT( ' ', 'Using the reduced DARK '/
     :             /'observation in ^DARK_NAME', STATUS)
                 ENDIF
               ENDIF

*             Obtain the name of a suitable FLAT observation if necessary.
*             Either search for a suitable FLAT observation in the index
*             file, or use one which has been explicitly specified. The
*             data, errors and quality for this observation will be held
*             in virtual memory, with the relevant pointers stored in /RED4_COMMON/.
               IF ( PROCEED_FF ) THEN
                 CALL RED4_GET_OBSERVATION( INDEX_FILE, 'OBS_IN', 'FLAT', FLAT_NAME, STATUS )
                 IF ( FLAT_NAME.EQ.' ' ) THEN
                   STATUS = SAI__ERROR
                   CALL ERR_REP( ' ', 'RED4_REDUCE_SPECTRA_OBS: '/
     :               /'No suitable reduced FLAT observation found. '/
     :               /'Data reduction aborted!', STATUS )
                   GOTO 500
                 ELSE
                   IF ( FLAT_OVERSAMPLED )  THEN
                     CALL MSG_SETC( 'FLAT_NAME', FLAT_NAME )
                     CALL MSG_OUT( ' ', 'Using the reduced oversampled FLAT '/
     :               /'observation in ^FLAT_NAME', STATUS)
                   ELSE
                     CALL MSG_SETC( 'FLAT_NAME', FLAT_NAME )
                     CALL MSG_OUT( ' ', 'Using the reduced non-oversampled FLAT '/
     :               /'observation in ^FLAT_NAME', STATUS)
                   ENDIF
*                 Give a warning if the FLAT has not been normalised
*                 (This should perhaps be treated as an error ?)
                   IF ( .NOT.FLAT_NORMALISED ) THEN
                     CALL MSG_BLANK( STATUS )
                     CALL MSG_OUT( ' ', '*********************************************************', STATUS )
                     CALL MSG_OUT( ' ', '****** WARNING: This FLAT has NOT been normalised! ******', STATUS )
                     CALL MSG_OUT( ' ', '*********************************************************', STATUS )
                     CALL MSG_BLANK( STATUS )
                     CALL MSG_OUT( ' ', 'Re-reduce the FLAT field and reset the RED4 task using:', STATUS )
                     CALL MSG_OUT( ' ', '   "ICL> OBEYW RED4 RESET"', STATUS )
                     CALL MSG_SETC( 'TYPE', TYPE )
                     CALL MSG_SETC( 'OBS_NAME', OBS_NAME )
                     CALL MSG_OUT( ' ', 'Then re-reduce the ^TYPE observation ^OBS_NAME', STATUS )
                     CALL MSG_BLANK( STATUS )
                   ENDIF
                 ENDIF
               ENDIF

*            Find out if a bad pixel mask has been specified
*            ('#' means no mask has been specified).
*            Check that the first two dimensions of the mask
*            data array match those of the integration.
               CALL PAR_GET0C( 'MASK', MASK, STATUS )
               CALL PAR_CANCL( 'MASK', STATUS )
               CPOS = INDEX( MASK, ':')
               IF (CPOS .EQ. 0) CPOS = INDEX( MASK, '/')
               IF ( CPOS .EQ. 0 ) THEN
                  CALL RED4_GET_PREFIX ('MASK', LPREFIX, STATUS)
                  CLEN = CHR_LEN( MASK )
                  MASK = LPREFIX(:CHR_LEN(LPREFIX)) // MASK(1:CLEN)
               END IF

               IF ( INDEX(MASK,'#').EQ.0 ) THEN
                  IF ( VERBOSE ) THEN
                     CALL MSG_SETC( 'MASK', MASK )
                     CALL MSG_OUT( ' ', 'Using the bad pixel mask ^MASK', STATUS )
                  ENDIF
                  CALL DSA_NAMED_INPUT( 'MASK', MASK, STATUS )
                  CALL DSA_MATCH_DIMENSION( 'MASK', 1, 'INT_IN', 1, STATUS )
                  CALL DSA_MATCH_DIMENSION( 'MASK', 2, 'INT_IN', 2, STATUS )
                  CALL DSA_MAP_DATA( 'MASK', 'READ', 'BYTE', MASK_DATA, MASK_SLOT, STATUS )
                  IF ( STATUS.EQ.SAI__OK .AND. VERBOSE ) CALL MSG_OUT( ' ', 'Mapped MASK data OK', STATUS )
               ENDIF

*             In addition ..... find the size of the input integration array and create 3 2-d
*             work arrays to match, to hold the data, variances and quality
*             of the reduced integrations. The variance array is only used to
*             fill up subroutine calls so fill it with zeros (the errors on
*             individual integrations are ignored in the observation coadd).
*             All the integrations will be the same size so the work arrays need
*             only be created once.
               CALL DSA_DATA_SIZE( 'INT_IN', MAXDIM, NDIM, DIMS, NELM, STATUS )
               INTDIMS(1) = DIMS(1)
               INTDIMS(2) = DIMS(2)
               NPLANE = DIMS(1) * DIMS(2)

               CALL DSA_GET_WORKSPACE( FLOATSIZE*NPLANE, WORK_DATA, WORK_SLOT, STATUS )
               CALL DSA_GET_WORKSPACE( FLOATSIZE*NPLANE, WORK_VAR, WORK_V_SLOT, STATUS )
               CALL DSA_GET_WORKSPACE( BYTESIZE*NPLANE, WORK_QUAL, WORK_Q_SLOT, STATUS )

*             Lastly, if this was a CHOP observation, then get workspace
*             to hold the phase A and phase B numbers during the initial
*             part of each integration reduction.
               IF ( INDEX(INT_TYPE,'CHOP').NE.0 ) THEN
                  CALL DSA_GET_WORKSPACE( FLOATSIZE*NPLANE, PHASEA_DATA, PHASEA_SLOT, STATUS )
                  CALL DSA_GET_WORKSPACE( FLOATSIZE*NPLANE, PHASEB_DATA, PHASEB_SLOT, STATUS )
               ENDIF

               IF ( STATUS.NE.SAI__OK ) THEN
                 STATUS = SAI__ERROR
                 CALL ERR_REP( ' ', 'RED4_REDUCE_SPECTRA_OBS: Failed to get work arrays', STATUS )
                 GOTO 500
               ENDIF
            ENDIF                  ! end of special setup for first integration

*          Map in the integration data array
            CALL DSA_MAP_DATA( 'INT_IN', 'READ', 'FLOAT', INT_DATA, INT_SLOT, STATUS )

*          Do the reduction ...

*          STARE data
            IF ( INDEX(INT_TYPE,'STARE').NE.0 .AND. (STATUS.EQ.SAI__OK) ) THEN

*            STARE: Initialise work arrays
              IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Initialising WORK arrays in STARE mode', STATUS )
              CALL GEN_FILL( FLOATSIZE*NPLANE, 0.0, %val(WORK_DATA) )
              CALL GEN_FILL( FLOATSIZE*NPLANE, 0.0, %val(WORK_VAR) )
              CALL GEN_FILL( BYTESIZE*NPLANE, 0, %val(WORK_QUAL) )

*            STARE: Copy data from integration and b.p.m.
              CALL GEN_MOVE( FLOATSIZE*NPLANE, %val(INT_DATA), %val(WORK_DATA) )
              IF ( INDEX( MASK,'#').EQ.0 ) THEN
                IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Moving MASK data to WORK quality', STATUS )
                CALL GEN_MOVE( BYTESIZE*NPLANE, %val(MASK_DATA), %val(WORK_QUAL) )
              ENDIF

*            STARE: Flag as "bad" any values which the ADP has set to -50.0.
              CALL RED4_FLAG_BAD( NPLANE, -50.0, %val(WORK_DATA), %val(WORK_QUAL), STATUS )

*             STARE: If non-destructive reads are not being used, subtract the BIAS observation from the result.
               IF ( PROCEED_BIAS .AND. (INDEX(INT_TYPE,'NDR').EQ.0) .AND. (STATUS.EQ.SAI__OK) ) THEN
                 IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Subtracting BIAS', STATUS )
                 CALL GEN_SUBAFV( NPLANE, %val(WORK_DATA), %val(BIAS_DATA), %val(WORK_DATA),
     :             %val(WORK_QUAL), %val(BIAS_QUAL), %val(WORK_QUAL), 0, 0, 0, .TRUE., .FALSE., 0, .FALSE. )
               ENDIF

*            STARE: Linearise the data if required.
               IF ( INDEX(LINCOEFFS,'#').EQ.0 .AND. (STATUS.EQ.SAI__OK) ) THEN
                 IF ( VERBOSE ) THEN
                   CALL MSG_SETI( 'NCOEFFS', N_COEFFS )
                   CALL MSG_SETC( 'NTH', GEN_NTH(N_COEFFS) )
                   CALL MSG_SETC( 'LINCOEFFS', LINCOEFFS )
                   CALL MSG_OUT( ' ', 'Linearising using ^NCOEFFS^NTH order polynomial in ^LINCOEFFS', STATUS )
                 ENDIF
                 CALL RED4_LINEARISE( N_COEFFS, LCOEFFS, NPLANE, %val(WORK_DATA), %val(WORK_VAR), %val(WORK_QUAL), STATUS )
               ENDIF

*             STARE: Now subtract the DARK observation, propagating quality but not variances.
               IF ( PROCEED_DARK .AND. (STATUS.EQ.SAI__OK) ) THEN
                 IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Subtracting DARK', STATUS )
                 CALL GEN_SUBAFV( NPLANE, %val(WORK_DATA), %val(DARK_DATA), %val(WORK_DATA),
     :              %val(WORK_QUAL), %val(DARK_QUAL), %val(WORK_QUAL), 0, 0, 0, .TRUE., .FALSE., 0, .FALSE. )
               ENDIF

*          CHOP data
            ELSE IF ( INDEX(INT_TYPE,'CHOP').NE.0 .AND. (STATUS.EQ.SAI__OK) ) THEN

*             CHOP: Initialise work arrays
               IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Initialising WORK arrays in CHOP mode', STATUS )
               CALL GEN_FILL( FLOATSIZE*NPLANE, 0.0, %val(PHASEA_DATA) )
               CALL GEN_FILL( FLOATSIZE*NPLANE, 0.0, %val(PHASEB_DATA) )
               CALL GEN_FILL( FLOATSIZE*NPLANE, 0.0, %val(WORK_DATA) )
               CALL GEN_FILL( FLOATSIZE*NPLANE, 0.0, %val(WORK_VAR) )
               CALL GEN_FILL( BYTESIZE*NPLANE, 0, %val(WORK_QUAL) )

*            CHOP: Copy data from integration and b.p.m.
               CALL GEN_MOVE( FLOATSIZE*NPLANE, %val(INT_DATA), %val(PHASEA_DATA) )
               IF ( INDEX( MASK,'#').EQ.0 ) THEN
                 IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Moving MASK data to WORK quality', STATUS )
                 CALL GEN_MOVE( BYTESIZE*NPLANE, %val(MASK_DATA), %val(WORK_QUAL) )
               ENDIF

*             CHOP: Old style data is 3-D integration array (x,y,2) so use offset
               IF ( NDIM.EQ.3 ) THEN
                 INT_DATA = INT_DATA + ( FLOATSIZE * NPLANE )
                 CALL GEN_MOVE( FLOATSIZE*NPLANE, %val(INT_DATA), %val(PHASEB_DATA) )

               ELSE IF ( NDIM.EQ.2 ) THEN
                 IPOS = INDEX( INTNAME, 'a' )

*               CHOP: New style data in 2 separate integration iyymmdd_oooo_iiiia/b arrays
                 IF ( IPOS.NE.0 ) THEN
                   INT2NAME  = INTNAME(1:IPOS-1)//'b'
                   IF ( INDEX(INT_TYPE,'CHOP').NE.0 ) THEN
                     CALL MSG_SETC( 'INT2NAME', INT2NAME )
                     CALL MSG_OUT( ' ', 'Seeking CHOP integration ^INT2NAME', STATUS )
                   ENDIF
                   EXIST = .FALSE.
                   CALL DSA_SEEK_NAMED_STRUCTURE( INT2NAME, EXIST, STATUS )
                   IF ( .NOT.EXIST ) THEN
                     STATUS = SAI__ERROR
                     CALL ERR_REP( ' ', 'RED4_REDUCE_SPECTRA_OBS: '/
     :                 /'Phase B plane missing!', STATUS )
                     GOTO 500
                   ELSE
                     CALL MSG_SETC( 'TYPE', TYPE )
                     CALL MSG_SETC( 'INT2NAME', INT2NAME )
                     CALL MSG_OUT( ' ', 'Reducing ^TYPE'/
     :                 /' integration ^INT2NAME', STATUS )
                     CALL DSA_NAMED_INPUT( 'INT2_IN', INT2NAME, STATUS )
                     CALL DSA_MAP_DATA( 'INT2_IN', 'READ', 'FLOAT', INT2_DATA, INT2_SLOT, STATUS )
                     CALL GEN_MOVE( FLOATSIZE*NPLANE, %val(INT2_DATA), %val(PHASEB_DATA) )
                     CALL DSA_UNMAP( INT2_SLOT, STATUS )
                     CALL DSA_CLOSE_STRUCTURE( 'INT2_IN', STATUS )
                   ENDIF
                 ELSE

*                 New style CHOP data in 2-D integration array of differences returned by ALICE so we can do nothing
                   CONTINUE
                 ENDIF
               ENDIF

*            CHOP: Flag as "bad" any values which the ADP has set to -50.0, for both phases.
               CALL RED4_FLAG_BAD( NPLANE, -50.0, %val(PHASEA_DATA), %val(WORK_QUAL), STATUS )
               CALL RED4_FLAG_BAD( NPLANE, -50.0, %val(PHASEB_DATA), %val(WORK_QUAL), STATUS )

*             CHOP: If non-destructive reads are not being used then subtract
*             the BIAS observation from the two phases. The quality of
*             the BIAS is propagated into the the work area that will
*             hold the result of the integration during the subtraction
*             of the BIAS from the phase A.
               IF ( PROCEED_BIAS .AND. (INDEX(INT_TYPE,'NDR').EQ.0) .AND. (STATUS.EQ.SAI__OK) ) THEN
                 IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Subtracting BIAS from Phase A', STATUS )
                 CALL GEN_SUBAFV( NPLANE, %val(PHASEA_DATA), %val(BIAS_DATA), %val(PHASEA_DATA),
     :              %val(WORK_QUAL), %val(BIAS_QUAL), %val(WORK_QUAL), 0, 0, 0, .TRUE., .FALSE., 0, .FALSE. )
                 IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Subtracting BIAS from Phase B', STATUS )
                 CALL GEN_SUBAFV( NPLANE, %val(PHASEB_DATA), %val(BIAS_DATA), %val(PHASEB_DATA),
     :             0, 0, 0, 0, 0, 0, .FALSE., .FALSE., 0, .FALSE. )
               ENDIF

*            CHOP: Linearise both phases of the data if required.
               IF ( INDEX( LINCOEFFS,'#').EQ.0 .AND. (STATUS.EQ.SAI__OK) ) THEN
                 IF ( VERBOSE ) THEN
                   CALL MSG_SETI( 'NCOEFFS', N_COEFFS )
                   CALL MSG_SETC( 'NTH', GEN_NTH(N_COEFFS) )
                   CALL MSG_SETC( 'LINCOEFFS', LINCOEFFS )
                   CALL MSG_OUT( ' ', 'Linearising both phases using ^NCOEFFS^NTH order polynomial in ^LINCOEFFS', STATUS )
                 ENDIF
                 IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Linearising DATA Phase A', STATUS )
                 CALL RED4_LINEARISE( N_COEFFS, LCOEFFS, NPLANE, %val(PHASEA_DATA), %val(WORK_VAR), %val(WORK_QUAL), STATUS )
                 IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Linearising DATA Phase B', STATUS )
                 CALL RED4_LINEARISE( N_COEFFS, LCOEFFS, NPLANE, %val(PHASEB_DATA), %val(WORK_VAR), %val(WORK_QUAL), STATUS )
               ENDIF

*             CHOP: No need to subtract the DARK observation, just subtract phase A
*             from phase B into the work array, quality should already be correctly set
               IF ( STATUS.EQ.SAI__OK ) THEN
                  IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Subtracting Phase A from Phase B', STATUS )
                  CALL GEN_SUBAFV( NPLANE, %val(PHASEA_DATA), %val(PHASEB_DATA), %val(WORK_DATA),
     :               0, 0, 0, 0, 0, 0, .FALSE., .FALSE., 0, .FALSE. )
               ENDIF
            ELSE

*            Neither CHOP nor STARE mode has been specified. The data file must be in error.
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_REDUCE_SPECTRA_OBS: '/
     ;           /'Error as the integration type is  '/
     :           /'neither CHOP nor STARE or bad status detected', STATUS )
               GOTO 500
            ENDIF

*          Re-join common processing path
*          Divide by the flat-field, FLAT, if necessary
            IF ( PROCEED_FF .AND. (STATUS.EQ.SAI__OK) .AND. .NOT.FLAT_OVERSAMPLED ) THEN
               IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Dividing by NON-oversampled FLAT', STATUS )
               CALL GEN_DIVAFV( NPLANE, %val(WORK_DATA), %val(FLAT_DATA), %val(WORK_DATA),
     :           %val(WORK_QUAL), %val(FLAT_QUAL), %val(WORK_QUAL),
     :           %val(WORK_VAR), %val(FLAT_VAR), %val(WORK_VAR), .TRUE., .FALSE., 0, .TRUE. )
            ENDIF

*          Coadd the work array into the observation result. External
*          status check is to prevent adjustable array errors if any
*          of the arrays aren't mapped.
            CALL RED4_DO_COADD( %val(WORK_DATA), %val(WORK_QUAL), INTDIMS(1), INTDIMS(2), %val(INDEX_PTR),
     :        INDEX_DIMS(1), INDEX_DIMS(2), DET_INDEX, %val(RED_DATA), %val(RED_VAR), %val(RED_QUAL),
     :        %val(COADDS_PTR), OBSDIMS(1), OBSDIMS(2), STATUS )

*          End of integration reduction -
*          Copy the FITS structure from the raw integration
*          file into a structure corresponding to the integration just
*          added in the COADDED_INTS structure. This is to record the fact
*          that this integration has been coadded into the result.
            CALL RED4_INTTOCOADD( INTNAME, COADD_NAME, STATUS )
            RECORD = COADDED_INTS(1:CHR_LEN(COADDED_INTS)) // '.' // COADD_NAME
            CALL RED4_COPY_STRUCTURE( 'INT_IN.'//FITS_STRUCTURE, RECORD, STATUS )

*          Release the integration data array, and close the file
            CALL DSA_UNMAP( INT_SLOT, STATUS )
            CALL DSA_CLOSE_STRUCTURE( 'INT_IN', STATUS )
         ENDIF

*       Break loop when status is bad
         IF ( STATUS.NE.SAI__OK ) LOOPING = .FALSE.
      ENDDO

*    Divide by the flat-field, FLAT, if necessary
      IF ( PROCEED_FF .AND. (NELM_OBS.GT.0) .AND. FLAT_OVERSAMPLED ) THEN
         IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Dividing by oversampled FLAT', STATUS )
         CALL GEN_DIVAFV( NELM_OBS,
     :     %val(RED_DATA), %val(FLAT_DATA), %val(RED_DATA),
     :     %val(RED_QUAL), %val(FLAT_QUAL), %val(RED_QUAL),
     :     %val(RED_VAR), %val(FLAT_VAR), %val(RED_VAR),
     :     .TRUE., .FALSE., 0, .TRUE. )
      ENDIF

*    Check that the variances are non-negative, they'll crash DSA close
*    if they are. The check is made if the variances have been mapped,
*    even if status is now bad.
      IF ( (VARIANCE_MAP) .AND. (NELM_OBS.GT.0) ) THEN
         CALL GEN_CLIPF( %val(RED_VAR), NELM_OBS, 0.0, VAL__MAXR, NLOW, NHIGH, %val(RED_VAR) )
         IF ( VERBOSE ) THEN
            CALL MSG_SETR( 'MAXR', VAL__MAXR )
            CALL MSG_SETI( 'NLOW', NLOW )
            CALL MSG_SETI( 'NHIGH', NHIGH )
            CALL MSG_OUT( ' ', 'Variances clipped: ^NLOW points below zero, ^NHIGH points above ^MAXR', STATUS )
         ENDIF
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
         ENDIF

         CHAR_ARRAY(2) = TYPE(1:CHR_LEN(TYPE))
         CALL CHR_CTOC( CHAR_ARRAY(2), CHAR_ARRAY(2), CLEN )

         IF ( PROCEED_BIAS .AND. (INDEX(INT_TYPE,'STARE').NE.0) ) THEN
            CHAR_ARRAY (2) = '('//CHAR_ARRAY(2)(1:CLEN)//'-BIAS)'
            CALL CHR_CTOC( CHAR_ARRAY(2), CHAR_ARRAY(2), CLEN )
         ENDIF

         IF ( PROCEED_DARK .AND. (INDEX(INT_TYPE,'STARE').NE.0) ) THEN
            CHAR_ARRAY (2) = '('//CHAR_ARRAY(2)(1:CLEN)//'-DARK)/FLAT'
            CALL CHR_CTOC( CHAR_ARRAY(2), CHAR_ARRAY(2), CLEN )
         ELSE
            CHAR_ARRAY (2) = CHAR_ARRAY(2)(1:CLEN)//'/FLAT'
            CALL CHR_CTOC( CHAR_ARRAY(2), CHAR_ARRAY(2), CLEN )
         ENDIF
      ELSE

         CHAR_ARRAY (1) = 'A/D numbers per exposure'
         CHAR_ARRAY (2) = TYPE(1:CHR_LEN(TYPE))
         CALL CHR_CTOC( CHAR_ARRAY(2), CHAR_ARRAY(2), CLEN )

         IF ( PROCEED_BIAS .AND. (INDEX(INT_TYPE,'STARE').NE.0) ) THEN
            CHAR_ARRAY (2) = '('//CHAR_ARRAY(2)(1:CLEN)//'-BIAS)'
            CALL CHR_CTOC( CHAR_ARRAY(2), CHAR_ARRAY(2), CLEN )
         ENDIF

         IF ( PROCEED_DARK .AND. (INDEX(INT_TYPE,'STARE').NE.0) ) THEN
            CHAR_ARRAY (2) = '('//CHAR_ARRAY(2)(1:CLEN)//'-DARK)'
            CALL CHR_CTOC( CHAR_ARRAY(2), CHAR_ARRAY(2), CLEN )
         ENDIF
      ENDIF

      CALL DSA_SET_DATA_INFO( 'OBS_RED', 2, CHAR_ARRAY, 0, 0.0D0, STATUS )

*    Obtain the name of the object and write it to the reduced
*    observation structure in the standard Figaro way.
      CALL DSA_GET_FITS_C( 'OBS_IN', 'OBJECT', 0, OBJECT_NAME, COMMENT, STATUS )
      CALL DSA_SET_OBJECT( 'OBS_RED', OBJECT_NAME, STATUS )

*    Record how the reduction went in the FITS structure of the reduced
*    observation.
*    date&time
      CALL GEN_TIME (6, DAY, LDAY, DATE, LDATE, HOUR, LHOUR)

      TIME = DATE(1:LDATE)//' at '//HOUR(1:LHOUR)
      CLEN = MAX( 1, CHR_LEN( TIME ) )

      CALL DSA_PUT_FITS_C( 'OBS_RED', 'STREDUCE', TIME(1:CLEN), ' ', STATUS )

*    the integration time for the observation as a whole
*    (Write this in both the CGS4 structure and the standard Figaro structure).
      CALL DSA_PUT_FITS_F( 'OBS_RED', 'EXPOSED', OBSERVATION_TIME, ' ', STATUS )
      CALL DSA_SET_EXPOSURE( 'OBS_RED', OBSERVATION_TIME, STATUS )

*  The name of the bad pixel mask, if used
      IF ( INDEX(MASK,'#').EQ.0 ) THEN
         CALL DSA_PUT_FITS_C( 'OBS_RED', 'MASKUSED', MASK, ' ', STATUS )
      ELSE IF ( PROCEED_FF ) THEN
         CALL DSA_PUT_FITS_C( 'OBS_RED', 'MASKUSED', 'Propagated '/
     :     /'from FLAT observation', ' ', STATUS )
      ELSE IF ( PROCEED_DARK .AND. (INDEX(INT_TYPE,'STARE').NE.0) ) THEN
         CALL DSA_PUT_FITS_C( 'OBS_RED', 'MASKUSED', 'Propagated '/
     :     /'from DARK observation', ' ', STATUS )
      ELSE IF ( PROCEED_BIAS .AND. (INDEX(INT_TYPE,'NDR').NE.0) ) THEN
         CALL DSA_PUT_FITS_C( 'OBS_RED', 'MASKUSED', 'Propagated '/
     :     /'from BIAS observation', ' ', STATUS )
      ELSE
         CALL DSA_PUT_FITS_C( 'OBS_RED', 'MASKUSED', '(none)', ' ', STATUS )
      ENDIF

*   The name of the BIAS observation, if used.
      IF ( PROCEED_BIAS .AND. (INDEX(INT_TYPE,'NDR').EQ.0) ) THEN
         CALL DSA_PUT_FITS_C( 'OBS_RED', 'BIASUSED', BIAS_NAME, ' ', STATUS )
      ELSE
         CALL DSA_PUT_FITS_C( 'OBS_RED', 'BIASUSED', '(none)', ' ', STATUS )
      ENDIF

*   The name of the DARK observation, if used.
      IF ( PROCEED_DARK .AND. (INDEX(INT_TYPE,'STARE').NE.0) ) THEN
         CALL DSA_PUT_FITS_C( 'OBS_RED', 'DARKUSED', DARK_NAME, ' ', STATUS )
      ELSE
         CALL DSA_PUT_FITS_C( 'OBS_RED', 'DARKUSED', '(none)', ' ', STATUS )
      ENDIF

*   The name of the FLAT observation, if used.
      IF ( PROCEED_FF ) THEN
         CALL DSA_PUT_FITS_C( 'OBS_RED', 'FLATUSED', FLAT_NAME, ' ', STATUS )
      ELSE
         CALL DSA_PUT_FITS_C( 'OBS_RED', 'FLATUSED', '(none)', ' ', STATUS )
      ENDIF

*   Whether linearised or not & the linearisation coefficients used.
      IF ( INDEX(LINCOEFFS,'#').EQ.0 ) THEN
         CALL DSA_PUT_FITS_C( 'OBS_RED', 'LINEARIS', 'yes', ' ', STATUS )
         DO I = 1, N_COEFFS
            CPOS = 0
            CALL CHR_PUTC( 'LINCF', ITEM, CPOS )
            CALL CHR_PUTI( I, ITEM, CPOS )
            CALL DSA_PUT_FITS_D( 'OBS_RED', ITEM(1:CPOS), LCOEFFS(I), ' ', STATUS )
         ENDDO
      ELSE
         CALL DSA_PUT_FITS_C( 'OBS_RED', 'LINEARIS', 'no', ' ', STATUS )
      ENDIF
 500  CONTINUE
      END
