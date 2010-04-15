*+  RED4_REDUCE_BIAS_INT - Reduce a BIAS integration.
      SUBROUTINE RED4_REDUCE_BIAS_INT (INT_NAME, STATUS)
*    Description :
*     This routine takes a raw bias integration file, Iyymmdd_o_i say, and
*     reduces it into RIDIR:RIyymmdd_o_i. The reduction in this case just
*     involves dividing the numbers coadded in the ADP by the number of
*     exposures that went into the coadd. Chopped integrations are not allowed,
*     nor are ones with weighted frames. Some details of the
*     integration (those that do not change during an observation) are
*     obtained from the integration's parent observation file in ODIR:Oyymmdd_o.
*
*     The integration structure has already been opened as 'INT_IN'
*     and the parent observation file as 'OBSERVATION' before this
*     routine is called
*    Invocation :
*     CALL RED4_REDUCE_BIAS_INT (INT_NAME, STATUS)
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*    Method :
*    Deficiencies :
*     The code needs restructuring without GOTOs, and the error checking
*     and reporting still leaves a lot to be desired.
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
*     1989:  Original version                                     (JFL)
*     22-Jan-1990: History added. Method added from notes made
*                  after studying source code.                    (SMB)
*     20-Feb-1990: Null bad pixel mask changed from ' ' to '#',
*                  as it was difficult to process ' ' with ICL
*                  variables.                                     (SMB)
*      9-Mar-1990: MAXDIM parameter added.                        (SMB)
*      9-Mar-1990: Division by number of exposures commented out,
*                  since this is now done by the ADP. (The code
*                  may be deleted if this works).                 (SMB)
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
*                  FLAT frames are no longer allowed.         (SMB)
*     23-Jul-1990: Character handling improved, and modified
*                  to accept 4 digit observation numbers.     (SMB)
*     21-Aug-1990: Phase 1 of major changes: Header
*                  information in the observation and reduced
*                  observation files is now written in a .FITS
*                  structure.                                 (SMB)
*      3-Sep-1990: Phase 2 of major changes: Header
*                  information in the integration and reduced
*                  integration files is now written in a .FITS
*                  structure.                                 (SMB)
*      4-Sep-1990: Typing mistakes fixed.                     (SMB)
*      4-Sep-1990: The variable FLOATSIZE was being used
*                  before being defined. Bug fixed.           (SMB)
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
*     21-Nov-1990: Tidied up a little.                        (SMB)
*     23-Nov-1990: Bug fix: Only call RED4_FLAG_BAD is the
*                  status is ok (to prevent adjustable array
*                  bounds violation).                         (SMB)
*     29-Nov-1990: MAXDIM and RMAXDIM constants moved to
*                  RED4_COMMON.INC. Unnecessarily large character
*                  variables reduced in size.                 (SMB)
*     29-Nov-1990: Bug fix: This routine was only mapping a
*                  quality array if a bad pixel mask was
*                  specified. It will now map an array
*                  regardless, and if there is no bad pixel
*                  mask fill it with zeros.                   (SMB)
*     24-Feb-1991: DSA error statuses trapped, as these no
*                  not conform to the ADAM error scheme.      (SMB)
*     22-Feb-1993: Conform to error strategy                  (PND)
*      7-Jan-1993: Allow NDF reduction                        (PND)
*     17-Jan-1993: Add parameterised template files           (PND)
*     24-Mar-1994: Remove DSA_SPECIFIC_STRUCTURE              (PND)
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
      CHARACTER*(*) INT_NAME             ! The name of the integration file to
*                                             be reduced, e.g. I890816_1_1
*    External references :
      INTEGER DSA_TYPESIZE               ! DSA type size enquiry function
      INTEGER CHR_LEN                    ! ADAM stringlength function
*    Global variables :
      INCLUDE 'RED4_COMMON.INC'          ! RED4 common block
*    Local Constants :
*    Local variables :
      INTEGER FLOATSIZE                  ! Bytes per element of 'FLOAT' array
      INTEGER NDIM                       ! the dimensions of the integration
*                                             array
      INTEGER DIMS( MAXDIM )             !                "
      INTEGER N_EXPOSURES                ! The number of exposures that were
*                                             coadded in the integration
      INTEGER IN_SLOT                    !        "
      INTEGER IN_DATA                    !        "
      INTEGER DATA_SLOT                  !        "
      INTEGER RED_DATA                   !        "
      INTEGER QUAL_SLOT                  !        "
      INTEGER RED_QUAL                   !        "
      INTEGER VAR_SLOT                   !        "
      INTEGER RED_VAR                    !        "
      INTEGER MASK_SLOT                  !        "
      INTEGER MASK_DATA                  !        "
      INTEGER NELM                       ! number of elements in integration
*                                             array
      INTEGER NPLANE                     ! number of elements in one plane of
*                                             the integration array
      INTEGER NBYTES                     ! number of bytes to be transferred
      INTEGER LDAY, LDATE, LHOUR         ! lengths of date strings
      INTEGER IGNORE                     ! unimportant parameter
      INTEGER DET_NINCR                  ! The number of detector positions
*                                        !   which are being combined into
*                                        !   the final observation.
      INTEGER CLEN                       ! Non-blank length of character string
      INTEGER CPOS                       ! Position in a string
      INTEGER DSA_STATUS                 ! DSA status value
      LOGICAL VARIANCE_MAP               ! T if variance array mapped.
      REAL EXPOSURE_TIME                 ! on-chip exposure time
      REAL INTEGRATION_TIME              ! EXPOSURE_TIME * N_EXPOSURES
      CHARACTER*80 INT_RED               ! the name of the reduced integration
*                                             file
      CHARACTER*20 INT_TYPE              ! type of integration
      CHARACTER*80 MASK                  ! file containing dud-pixel info
      CHARACTER*40 OBJECT_NAME           ! The name of the object
      CHARACTER*32 CHAR_ARRAY(2)         ! array to hold data units and title
      CHARACTER*40 TIME                  ! A string holding the date of the
*                                             reduction
      CHARACTER*20 DAY, DATE, HOUR       ! components of date
      CHARACTER*20 LPREFIX               ! Prefix to apply
      CHARACTER*4 COMMENT                ! Dummy comment
*    Internal References :
*    Local data :
*-

*    Return if entry status bad
      IF ( STATUS .NE. SAI__OK ) RETURN

*    The integration structure has already been opened as 'INT_IN'
*    and the parent observation file as 'OBSERVATION' before this
*    routine is called
*    first of all try to check that we have enough information for the
*    reduction to proceed.
*    Get the INT_TYPE (i.e. the observation mode; STARE, CHOP etc...)
      DSA_STATUS = SAI__OK
      CALL DSA_GET_FITS_C( 'OBSERVATION', 'INTTYPE', 0, INT_TYPE,
     :  COMMENT, DSA_STATUS )

*    BIAS observations should be very straightforward. Chop observations,
*    and weighted frames are meaningless, so treat them as an error.
      IF (INDEX(INT_TYPE,'CHOP') .NE. 0) THEN

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_BIAS_INT: '/
     :     /'This BIAS integration was taken in chop '/
     :     /'mode. There must have been a mistake!', STATUS )
         GOTO 500
      ENDIF

      IF (INDEX(INT_TYPE,'WEIGHTED') .NE. 0) THEN

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_BIAS_INT: '/
     :     /'This BIAS integration was taken in '/
     :     /'weighted frames mode which is '/
     :     /'meaningless. There must have been a '/
     :     /'mistake!', STATUS )
         GOTO 500
      ENDIF

*    Check that the number of detector increments is 1, and hence that
*    the integration has not been made with oversampling (which is only
*    relevant for SPECTRA-type observations).
      CALL DSA_GET_FITS_I( 'OBSERVATION', 'DETNINCR', 0, DET_NINCR,
     :  COMMENT, DSA_STATUS )

      IF ( DSA_STATUS .EQ. SAI__OK ) THEN
         IF ( DET_NINCR .NE. 1 ) THEN

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_REDUCE_BIAS_INT: '/
     :        /'This BIAS integration was taken with '/
     :        /'oversampling, which is not sensible. '/
     :        /'There must have been a mistake!', STATUS )
            GOTO 500
         END IF
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_BIAS_INT: '/
     :     /'Error getting FITS item DETNINCR', STATUS )
      END IF

*   Get the number of exposures in this integration N_EXPOSURES
      CALL DSA_GET_FITS_I( 'INT_IN', 'NEXP', 0, N_EXPOSURES,
     :  COMMENT, DSA_STATUS )

      IF (DSA_STATUS .EQ. SAI__OK) THEN
         IF (N_EXPOSURES .LT. 1) THEN

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_REDUCE_BIAS_INT: '/
     :        /'No exposures were made during the '/
     :        /'integration', STATUS )
            GOTO 500
         ENDIF
      ENDIF

*    Get the exposure time in this integration EXPOSURE_TIME
      CALL DSA_GET_FITS_F( 'OBSERVATION', 'DEXPTIME', 0,
     :  EXPOSURE_TIME, COMMENT, DSA_STATUS )

      IF (DSA_STATUS .EQ. SAI__OK) THEN
         IF (EXPOSURE_TIME .LE. 0.0) THEN

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_REDUCE_BIAS_INT: '/
     :        /'Exposure time is negative', STATUS )
            GOTO 500
         ENDIF
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_BIAS_INT: '/
     :     /'Error getting FITS item DEXPTIME', STATUS )
      ENDIF

*    If an error has occurred, jump to the end.
      IF ( DSA_STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_BIAS_INT: '/
     :     /'Error getting FITS items', STATUS )
         GOTO 500
      END IF

*    Work out integration time
      INTEGRATION_TIME = N_EXPOSURES * EXPOSURE_TIME

*    Obtain the number of bytes per element in a data array of
*    type 'FLOAT' from DSA
      FLOATSIZE = DSA_TYPESIZE( 'FLOAT', DSA_STATUS )

*    Find the size of the input data array
      CALL DSA_DATA_SIZE ('INT_IN', MAXDIM, NDIM, DIMS, NELM,
     :  DSA_STATUS)

*    and the number of elements and bytes in a single plane of that array
      NPLANE = DIMS(1) * DIMS(2)
      NBYTES = FLOATSIZE * NPLANE

*    Map in the array
      CALL DSA_MAP_DATA ('INT_IN', 'READ', 'FLOAT', IN_DATA, IN_SLOT,
     :   DSA_STATUS )

*    Now we're as sure as can be that everything's gonna be jus' fine.
*    so open the reduced integration file. A new one will be created
*    specifically, according to the reduced integration template, and with
*    the correct filename  e.g. RI890801_1_1 from I890801_1_1
      CALL DSA_NAMED_INPUT ('INTRED_TEMPLATE',
     :   INTRED_TEMPLATE, DSA_STATUS)

      IF ( DSA_STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_BIAS_INT: '/
     :     /'Error opening INTRED template', STATUS )
      END IF

      CALL RED4_INTTORINT( INT_NAME, INT_RED, STATUS )

      DSA_STATUS = STATUS
      CALL DSA_NAMED_OUTPUT ('INT_RED', INT_RED, 'INTRED_TEMPLATE',
     :   0, 0, DSA_STATUS)

      CALL MSG_SETC( 'INT_RED', INT_RED )
      CALL MSG_OUT( ' ', 'Reducing a BIAS integration into '/
     :  /'^INT_RED', STATUS )

*    Copy the contents of the FITS structure from the raw integration file
*    over to the reduced integration file (deleting the old structure first,
*    if present)
      CALL RED4_COPY_STRUCTURE( 'INT_IN.'//FITS_STRUCTURE,
     :   'INT_RED.'//FITS_STRUCTURE, STATUS )

*   Abort if an error has occurred. This is bad practise, but has been
*   included to maintain the behaviour of the original code.
*   GOTOs should eventually be removed.
      IF ( DSA_STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_BIAS_INT: '/
     :     /'Error copying FITS structures', STATUS )
         GOTO 500
      END IF

*    Get the name of the quality mask file, if there is one, map in
*    the quality array and check it has the right size.
      CALL PAR_GET0C ('MASK', MASK, STATUS)
      CALL PAR_CANCL ('MASK', STATUS)
      CPOS = INDEX( MASK, ':')
      IF (CPOS .EQ. 0) CPOS = INDEX( MASK, '/')
      IF ( CPOS .EQ. 0 ) THEN
         CALL RED4_GET_PREFIX ('MASK', LPREFIX, STATUS)
         CLEN = CHR_LEN( MASK )
         MASK = LPREFIX(:CHR_LEN(LPREFIX)) // MASK(1:CLEN)
      END IF

      IF ( INDEX( MASK, '#' )  .EQ. 0 ) THEN
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
         IF ( DSA_STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_BIAS_INT: '/
     :     /'Error mapping MASK data', STATUS )
         END IF
      ENDIF

*    Map in the data array of the reduced integration file, and the
*    variance and quality arrays if they will be required.
      DSA_STATUS = STATUS
      CALL DSA_USE_QUALITY ('INT_RED', DSA_STATUS)
      CALL DSA_MAP_DATA ('INT_RED', 'WRITE', 'FLOAT', RED_DATA,
     :   DATA_SLOT, DSA_STATUS)
      CALL DSA_MAP_QUALITY ('INT_RED', 'WRITE', 'BYTE', RED_QUAL,
     :   QUAL_SLOT, DSA_STATUS)

      IF (INDEX(INT_TYPE,'SECTORS') .NE. 0) THEN

         CALL DSA_MAP_VARIANCE ('INT_RED', 'WRITE', 'FLOAT', RED_VAR,
     :      VAR_SLOT, DSA_STATUS)

         IF ( DSA_STATUS .EQ. SAI__OK ) THEN

            VARIANCE_MAP = .TRUE.
         ELSE

            VARIANCE_MAP = .FALSE.
         END IF
      ELSE

         VARIANCE_MAP = .FALSE.
      ENDIF

      IF ( DSA_STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_BIAS_INT: '/
     :      /'Error mapping data, variance or quality', STATUS )
      END IF

*    Initialise the variance array to zero.
      IF ( VARIANCE_MAP .AND. (STATUS .EQ. SAI__OK) ) THEN

         CALL GEN_FILL( NBYTES, 0, %val(RED_VAR) )
      END IF

*    Do the reduction.
*    Copy the Phase A plane of the integration array into the result data array
      IF (STATUS .EQ. SAI__OK) THEN

         CALL GEN_MOVE (NBYTES, %val(IN_DATA), %val(RED_DATA))

*       Now the SECTOR statistics, if enabled
         IF (INDEX(INT_TYPE,'SECTORS') .NE. 0) THEN

*          the plane of the integration array holding the statistical info
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
      ENDIF

*    If there is a bad pixel mask, copy its contents to the quality array.
*    Otherwise fill the quality array with zeros.
      IF ( STATUS .EQ. SAI__OK ) THEN

         IF ( INDEX( MASK, '#' )  .EQ. 0 ) THEN

            CALL GEN_MOVE (NPLANE, %val(MASK_DATA), %val(RED_QUAL))
         ELSE

            CALL GEN_FILL( NPLANE, 0, %val(RED_QUAL) )
         ENDIF
      ENDIF

*    Flag as "bad" any values which the ADP has set to -50.0.
      IF ( STATUS .EQ. SAI__OK ) THEN

         CALL RED4_FLAG_BAD( NPLANE, -50.0, %val(RED_DATA),
     :     %val(RED_QUAL), STATUS )
      END IF

*    Check that the variances are non-negative, they'll crash DSA_CLOSE
*    if they are
      IF ( VARIANCE_MAP .AND. (NPLANE.GT.0) ) THEN

         CALL GEN_CLIPF (%val(RED_VAR), NPLANE, 0.0,
     :      VAL__MAXR, IGNORE, IGNORE, %val(RED_VAR))
      ENDIF

*    Set the data label and units
      CHAR_ARRAY (1) = 'A/D numbers'
      CHAR_ARRAY (2) = 'BIAS'

      DSA_STATUS = STATUS
      CALL DSA_SET_DATA_INFO ('INT_RED', 2, CHAR_ARRAY, 0, 0.0D0,
     :  DSA_STATUS)

*    Obtain the name of the object and write it to the reduced
*    integration structure in the standard Figaro way.
      CALL DSA_GET_FITS_C( 'OBSERVATION', 'OBJECT', 0,
     :  OBJECT_NAME, COMMENT, DSA_STATUS )

      CLEN = MAX( 1, CHR_LEN( OBJECT_NAME ) )
      CALL DSA_SET_OBJECT( 'INT_RED', OBJECT_NAME(1:CLEN), DSA_STATUS )

*    End of reduction
*    Record how the reduction went in the FITS structure.
*    date&time
      CALL GEN_TIME (6, DAY, LDAY, DATE, LDATE, HOUR, LHOUR)
      TIME = DATE(:LDATE)//' at '//HOUR(:LHOUR)
      CLEN = MAX( 1, CHR_LEN( TIME ) )
      CALL DSA_PUT_FITS_C( 'INT_RED', 'STREDUCE', TIME(1:CLEN), ' ',
     :  DSA_STATUS )

*    the name of the dud pixel mask if used
      IF ( INDEX( MASK, '#' )  .EQ. 0 ) THEN

         CALL DSA_PUT_FITS_C( 'INT_RED', 'MASKUSED',
     :     MASK, ' ', DSA_STATUS )
      ELSE

         CALL DSA_PUT_FITS_C( 'INT_RED', 'MASKUSED', '(none)',
     :     ' ', DSA_STATUS )
      END IF

*    the integration time. (Write this to both the FITS structure and
*    the OBS structure).
      CALL DSA_PUT_FITS_F( 'INT_RED', 'EXPOSED', INTEGRATION_TIME,
     :  ' ', DSA_STATUS )
      CALL DSA_SET_EXPOSURE( 'INT_RED', INTEGRATION_TIME, DSA_STATUS )

*   Destination for error GOTOs.
 500  CONTINUE

      IF ( DSA_STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REDUCE_BIAS_INT: '/
     :     /'A DSA error has been detected', STATUS )
      END IF

      END
