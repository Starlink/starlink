*+  RED4_GET_OBSERVATION - Get a calibration observation as specified
      SUBROUTINE RED4_GET_OBSERVATION ( INDEX_FILE, OBSREF,
     :   TYPE_REQUIRED, OBS_NAME, STATUS)
*    Description :
*     This subroutine obtains a reduced observation that can be used
*     in the reduction sequence of the current integration/observation.
*     For example the observation of a star will need the results of
*     BIAS, DARK, and FLAT observations in its reduction sequence, each
*     of these will be obtained using this routine.
*
*     The way in which the observation is obtained may be specified
*     by a search mode parameter. There is one such parameter for each
*     type of observation. If the appropriate search mode parameter
*     is 'SPECIFIED', then the name of the observation is specified
*     explicitly in another parameter. This observation is then used
*     regardless of the suitability of its instrument settings (although
*     a warning message is given if they are not suitable). Other values
*     of the search mode parameter result in the index file being
*     searched for a suitable observation.
*
*     Having obtained an appropriate observation, the routine checks the
*     RED4 common to see if that observation is already in common, if not
*     it gets the appropriate amount of virtual memory and copies in the
*     data, error and quality arrays from the observation.
*
*     Note that this routine maintains four separate areas of virtual
*     memory - areas for BIAS, DARK, FLAT and STANDARD. The same code is
*     used to allocate and free these areas, but pointers to them are
*     stored in COMMON and copied when necessary.
*
*     On entry the parent observation file must have been opened as
*     OBSREF by DSA.
*    Invocation :
*     CALL RED4_GET_OBSERVATION ( INDEX_FILE, OBSREF, TYPE_REQUIRED,
*    :   OBS_NAME, STATUS)
*    Parameters :
*    Method :
*    Deficiencies :
*     DSA status values do not conform to the ADAM scheme. It has
*     therefore been necessary to trap these statuses before they
*     get translated into confusing messages like "exceeded quota".
*     The traps can be removed once DSA conforms.
*    Bugs :
*    Authors :
*     J. Lightfoot (REVAD::JFL)
*     S.M.Beard    (REVAD::SMB)
*     P.N.Daly     (JACH::PND)
*    History :
*      7-Oct-1991: Original version, taken from the original
*                  RED4_SEEK_OBSERVATION.                       (SMB,JFL)
*      9-Oct-1991: Mistake corrected: SPECIFIED_OBS needs
*                  prefixing with RODIR or RGDIR.               (SMB)
*     13-May-1992: Fix trivial bug. Set OBS_MATCH blank when
*                  TYPE_REQUIRED is BIAS or DARK, to ensure
*                  garbage is not passed to any other routine.  (SMB)
*     19-Feb-1993: Conform to error strategy                    (PND)
*     30-Jun-1993: Check specified observation                  (PND)
*      6-Dec-1993: Update for IRCAM                             (PND)
*     19-Jan-1994: Put IF-block around PAR to get specified_obs (PND)
*     22-May-1995: Add flag for oversampled FLATs               (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables:
      INCLUDE 'RED4_COMMON.INC'        ! RED4 common block containing pointers
*                                           to data in virtual memory.
*    Import :
      CHARACTER*(*) INDEX_FILE         ! The name of the index file to be
*                                           searched. should be of form
*                                           CGS4_890818.INDEX
      CHARACTER*(*) OBSREF             ! The DSA reference name for the
*                                           observation file.
      CHARACTER*(*) TYPE_REQUIRED      ! the type of the reduced observation that
*                                           is required
*    Export:
      CHARACTER*(*) OBS_NAME           ! the name of the file containing the
*                                           desired reduced observation
*    Status :
      INTEGER STATUS
*    External references:
      INTEGER DSA_TYPESIZE
*    Local Constants :
*    Local variables :
*
*     --- General variables ---
*
      INTEGER ELEMENTS, STRLEN         ! Ignore as not used but needed by DSA
*
      REAL AMTOLER                     ! The air mass % tolerance
      REAL FLAT_DETINCR                ! Detector increment
      INTEGER FLAT_DETNINCR            ! Detector increment
      INTEGER SUPERSAMPLING
      INTEGER OVERSAMPLING
*
      CHARACTER*3 NORMALIS             ! NORMALIS item from .FITS structure
      CHARACTER*4 COMMENT              ! Dummy comment
      CHARACTER*10 SEARCH_MODE         ! The search mode to be used for the
*                                      !   observation. Can be 'SPECIFIED',
*                                      !   'FORWARDS', 'BACKWARDS' or 'BOTH'.
      CHARACTER*80 REPLY               ! The reply given to the SPECIFIED_xxx
*                                      !   parameter prompts.
      CHARACTER*80 SPECIFIED_OBS       ! The name of the observation to be
*                                      !   used if SEARCH_MODE is 'SPECIFIED'.
      CHARACTER*132 OBS_MATCH          ! String containing the names of the
*                                      !   items which must match for an
*                                      !   observation to be considered
*                                      !   suitable.
*
      LOGICAL FITS_EXISTS              ! True if FITS item exists
      LOGICAL NEW_DATA                 ! T if a new observation must be mapped
*                                           into virtual memory
      LOGICAL FIRST_TIME               ! T if this is the first time the
*                                      !   routine has been called.
*
*     --- Temporary pointers to data in virtual memory ---
*
      INTEGER DATA_VM                  ! virtual memory pointer to data array
      INTEGER VAR_VM                   ! ... same for variances
      INTEGER QUAL_VM                  ! ... same for quality
      INTEGER NELM_VM                  ! number of elements mapped into virtual
*                                            memory
*
*     --- Address, dimension, slot numbers, etc. for observation to be
*         mapped into virtual memory ---
*
      INTEGER NDIM
      INTEGER DIMS( MAXDIM )           ! dimensions of observation data array
      INTEGER NELM                     ! total number of elements in the image
      INTEGER OBS_DATA                 !        "
      INTEGER OBS_VAR                  !        "
      INTEGER OBS_QUAL                 !        "
      INTEGER OBS_SLOT                 ! DSA slots used
      INTEGER OBS_V_SLOT               !        "
      INTEGER OBS_Q_SLOT               !        "
      INTEGER FLOATSIZE
      INTEGER BYTESIZE
*    Local data :
      DATA FIRST_TIME / .TRUE. /       ! Initialise FIRST_TIME to .TRUE.
      SAVE FIRST_TIME
*-

*    Check for error on entry.
      IF (STATUS .NE. SAI__OK) RETURN

      FLOATSIZE = DSA_TYPESIZE( 'FLOAT', STATUS )
      BYTESIZE  = DSA_TYPESIZE( 'BYTE', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'RED4_GET_OBSERVATION: '/
     :     /'Unable to determine FLOATSIZE or BYTESIZE', STATUS )
        GOTO 500
      ENDIF

*    If this is the first time this routine has been called, initialise
*    the common block variables (to prevent uninitialised variables being
*    tested below).
      IF ( FIRST_TIME ) THEN

*      Reset the FIRST_TIME flag, so this IF block will not be
*      executed in subsequent calls to this routine.
         FIRST_TIME = .FALSE.

*      Variables remembering the current BIAS, DARK and FLAT frames
*      which are stored in virtual memory.
         CURRENT_BIAS     = ' '
         CURRENT_DARK     = ' '
         CURRENT_FLAT     = ' '
         CURRENT_STANDARD = ' '

*      Pointers containing the addresses of the start of virtual
*      memory where the current BIAS frame arrays are stored, and the
*      number of elements in the BIAS frame.
         BIAS_DATA = 0
         BIAS_VAR  = 0
         BIAS_QUAL = 0
         BIAS_NELM = 0

*      Pointers containing the addresses of the start of virtual
*      memory where the current DARK frame arrays are stored, and the
*      number of elements in the DARK frame.
         DARK_DATA = 0
         DARK_VAR  = 0
         DARK_QUAL = 0
         DARK_NELM = 0

*      Pointers containing the addresses of the start of virtual
*      memory where the current FLAT frame arrays are stored, and the
*      number of elements in the FLAT frame. Also the address of
*      the start of virtual memory where an index array is stored.
         FLAT_DATA  = 0
         FLAT_VAR   = 0
         FLAT_QUAL  = 0
         FLAT_NELM  = 0

*      Flag indicating if the current flat field has been normalised
         FLAT_NORMALISED = .FALSE.
         FLAT_OVERSAMPLED = .FALSE.

*      Pointers containing the addresses of the start of virtual
*      memory where the current STANDARD frame arrays are stored, and the
*      number of elements in the STANDARD frame. Also the address of
*      the start of virtual memory where an index array is stored.
         STANDARD_DATA  = 0
         STANDARD_VAR   = 0
         STANDARD_QUAL  = 0
         STANDARD_NELM  = 0
      END IF

*    Check which type of observation is required and obtain the
*    appropriate values of the parameters indicating how the observation
*    should be specified. Also obtain the parameters indicating which items
*    should be matched for a FLAT, CALIBRATION or STANDARD observation.
*    Abort if any of these fail.
      IF ( TYPE_REQUIRED .EQ. 'BIAS' ) THEN

         CALL PAR_GET0C( 'BIAS_MODE', SEARCH_MODE, STATUS )
         IF ( SEARCH_MODE .EQ. 'SPECIFIED' ) THEN
            CALL PAR_GET0C( 'SPECIFIED_BIAS', REPLY, STATUS )
            CALL RED4_CHECK_INPUT( REPLY, STATUS )
            SPECIFIED_OBS = REPLY
         END IF
         OBS_MATCH = ' '

         IF ( STATUS .NE. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Error obtaining %BIAS_MODE and '/
     :        /'%SPECIFIED_BIAS parameters.', STATUS )
         END IF
      ELSE IF ( TYPE_REQUIRED .EQ. 'DARK' ) THEN

         CALL PAR_GET0C( 'DARK_MODE', SEARCH_MODE, STATUS )
         IF ( SEARCH_MODE .EQ. 'SPECIFIED' ) THEN
            CALL PAR_GET0C( 'SPECIFIED_DARK', REPLY, STATUS )
            CALL RED4_CHECK_INPUT( REPLY, STATUS )
            SPECIFIED_OBS = REPLY
         END IF
         OBS_MATCH = ' '

         IF ( STATUS .NE. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Error obtaining %DARK_MODE and '/
     :        /'%SPECIFIED_DARK parameters.', STATUS )
         END IF
      ELSE IF ( TYPE_REQUIRED .EQ. 'FLAT' ) THEN

         CALL PAR_GET0C( 'FLAT_MODE', SEARCH_MODE, STATUS )
         IF ( SEARCH_MODE .EQ. 'SPECIFIED' ) THEN
            CALL PAR_GET0C( 'SPECIFIED_FLAT', REPLY, STATUS )
            CALL RED4_CHECK_INPUT( REPLY, STATUS )
            SPECIFIED_OBS = REPLY
         END IF
         CALL PAR_GET0C( 'FLAT_MATCH', OBS_MATCH, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Error obtaining %FLAT_MODE, '/
     :        /'%SPECIFIED_FLAT and %FLAT_MATCH parameters.', STATUS )
         END IF
      ELSE IF ( TYPE_REQUIRED .EQ. 'CALIBRATION' ) THEN

         CALL PAR_GET0C( 'CALIB_MODE', SEARCH_MODE, STATUS )
         IF ( SEARCH_MODE .EQ. 'SPECIFIED' ) THEN
            CALL PAR_GET0C( 'SPECIFIED_CALIB', REPLY, STATUS )
            CALL RED4_CHECK_INPUT( REPLY, STATUS )
            SPECIFIED_OBS = REPLY
         END IF
         CALL PAR_GET0C( 'CALIB_MATCH', OBS_MATCH, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Error obtaining %CALIB_MODE, '/
     :        /'%SPECIFIED_CALIB and %CALIB_MATCH parameters.', STATUS )
         END IF
      ELSE IF ( TYPE_REQUIRED .EQ. 'STANDARD' ) THEN

         CALL PAR_GET0C( 'STANDARD_MODE', SEARCH_MODE, STATUS )
         IF ( SEARCH_MODE .EQ. 'SPECIFIED' ) THEN
            CALL PAR_GET0C( 'SPECIFIED_STD', REPLY, STATUS )
            CALL RED4_CHECK_INPUT( REPLY, STATUS )
            SPECIFIED_OBS = REPLY
         END IF
         CALL PAR_GET0C( 'STANDARD_MATCH', OBS_MATCH, STATUS )
         CALL PAR_GET0R( 'AMTOLER', AMTOLER, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Error obtaining %STANDARD_MODE, '/
     :        /'%SPECIFIED_STD, %STANDARD_MATCH and %AMTOLER '/
     :        /'parameters.', STATUS )
         END IF
      ELSE

         STATUS = SAI__ERROR
         CALL MSG_SETC( 'TYPE_REQUIRED', TYPE_REQUIRED )
         CALL ERR_REP( ' ', 'RED4_GET_OBSERVATION: unknown '/
     :     /'observation type, ^TYPE_REQUIRED.', STATUS )
      END IF

*   Check everything has worked so far.
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Convert the character parameters to upper case.
         CALL CHR_UCASE( SEARCH_MODE )
         CALL CHR_UCASE( OBS_MATCH )

*      Determine whether a suitable observation should be searched for
*      or whether a particular observation has been specified.
         IF ( SEARCH_MODE .EQ. 'SPECIFIED' ) THEN

*         A particular observation has been specified explicitly.
*         Check the observation is of the correct type. Also check the
*         observation's instrument settings and give a warning if the
*         observation is not suitable.
            OBS_NAME = SPECIFIED_OBS
            CALL RED4_CHECK_OBSERVATION( OBSREF, TYPE_REQUIRED,
     :        OBS_MATCH, AMTOLER, OBS_NAME, STATUS )
         ELSE

*         A suitable observation should be searched for. OBS_NAME
*         will be returned blank if no suitable observation could
*         be found.
            CALL RED4_SEEK_OBSERVATION( INDEX_FILE, OBSREF,
     :        TYPE_REQUIRED, SEARCH_MODE, OBS_MATCH, AMTOLER,
     :        OBS_NAME, STATUS )
         END IF
      END IF

*   Check everything has worked and that a suitable observation
*   has been found.
      IF ( (STATUS .EQ. SAI__OK) .AND. (OBS_NAME .NE. ' ') ) THEN

*      Now check that observation isn't already in common, otherwise set
*      the pointers to be ready for reading in the new data
         NEW_DATA = .FALSE.

         IF (TYPE_REQUIRED .EQ. 'BIAS') THEN

*         A BIAS has been obtained. Check if the same BIAS observation
*         has already been obtained in a previous incarnation of this
*         routine. If it has, then it will have already have been
*         mapped and written to virtual memory. If not, the obtained
*         BIAS needs to be mapped.
            IF (OBS_NAME .NE. CURRENT_BIAS) THEN

*            The obtained BIAS is different from any previously
*            mapped. Recall the pointers stored in the common
*            block into the temporary pointers used by this routine.
*            (Note that the pointers are initialised to zero when
*            this routine is first executed).
*            (The temporary pointers are used later on for freeing
*            and allocating virtual memory).
               DATA_VM = BIAS_DATA
               VAR_VM = BIAS_VAR
               QUAL_VM = BIAS_QUAL
               NELM_VM = BIAS_NELM
               NEW_DATA = .TRUE.
            ENDIF
         ELSE IF (TYPE_REQUIRED .EQ. 'DARK') THEN

*         A DARK has been obtained. Check if the same DARK observation
*         has already been obtained in a previous incarnation of this
*         routine. If it has, then it will have already have been
*         mapped and written to virtual memory. If not, the obtained
*         DARK needs to be mapped.
            IF (OBS_NAME .NE. CURRENT_DARK) THEN

*            The obtained DARK is different from any previously
*            mapped. Recall the pointers stored in the common
*            block into the temporary pointers used by this routine.
*            (Note that the pointers are initialised to zero when
*            this routine is first executed).
*            (The temporary pointers are used later on for freeing
*            and allocating virtual memory).
               DATA_VM = DARK_DATA
               VAR_VM = DARK_VAR
               QUAL_VM = DARK_QUAL
               NELM_VM = DARK_NELM
               NEW_DATA = .TRUE.
            ENDIF
         ELSE IF (TYPE_REQUIRED .EQ. 'FLAT') THEN

*         A FLAT has been obtained. Check if the same FLAT observation
*         has already been obtained in a previous incarnation of this
*         routine. If it has, then it will have already have been
*         mapped and written to virtual memory. If not, the obtained
*         FLAT needs to be mapped.
            IF (OBS_NAME .NE. CURRENT_FLAT) THEN

*            The obtained FLAT is different from any previously
*            mapped. Recall the pointers stored in the common
*            block into the temporary pointers used by this routine.
*            (Note that the pointers are initialised to zero when
*            this routine is first executed).
*            (The temporary pointers are used later on for freeing
*            and allocating virtual memory).
               DATA_VM = FLAT_DATA
               VAR_VM = FLAT_VAR
               QUAL_VM = FLAT_QUAL
               NELM_VM = FLAT_NELM
               NEW_DATA = .TRUE.
            ENDIF
         ELSE IF (TYPE_REQUIRED .EQ. 'CALIBRATION') THEN

*         A CALIBRATION has been obtained. Make sure that no attempt
*         is made to open this or map it.
            NEW_DATA = .FALSE.
         ELSE IF (TYPE_REQUIRED .EQ. 'STANDARD') THEN

*         A STANDARD has been obtained. Check if the same STANDARD observation
*         has already been obtained in a previous incarnation of this
*         routine. If it has, then it will have already have been
*         mapped and written to virtual memory. If not, the obtained
*         STANDARD needs to be mapped.
            IF (OBS_NAME .NE. CURRENT_STANDARD) THEN

*            The obtained STANDARD is different from any previously
*            mapped. Recall the pointers stored in the common
*            block into the temporary pointers used by this routine.
*            (Note that the pointers are initialised to zero when
*            this routine is first executed).
*            (The temporary pointers are used later on for freeing
*            and allocating virtual memory).
               DATA_VM = STANDARD_DATA
               VAR_VM = STANDARD_VAR
               QUAL_VM = STANDARD_QUAL
               NELM_VM = STANDARD_NELM
               NEW_DATA = .TRUE.
            ENDIF
         ENDIF

*       If the observation required is different from that previously
*       obtained it will need to be mapped into virstual memory and
*       pointers to this virtual memory stored in common.
         IF (NEW_DATA) THEN

*          Open the data structure containing the required observation
*          and obtain the size of the data array. Also, tell DSA that
*          the data quality method will be used for error handling.
            CALL RED4_CHECK_INPUT( OBS_NAME, STATUS )
            CALL DSA_NAMED_INPUT ('SEEK_OBS', OBS_NAME, STATUS)
            CALL DSA_DATA_SIZE ('SEEK_OBS', MAXDIM, NDIM, DIMS, NELM,
     :         STATUS)
            CALL DSA_USE_QUALITY ('SEEK_OBS', STATUS)

*          Map in the data array, variance array and quality array
*          associated with the required observation.
            CALL DSA_MAP_DATA ('SEEK_OBS', 'READ', 'FLOAT',
     :         OBS_DATA, OBS_SLOT, STATUS)
            CALL DSA_MAP_VARIANCE ('SEEK_OBS', 'READ', 'FLOAT',
     :         OBS_VAR, OBS_V_SLOT, STATUS)
            CALL DSA_MAP_QUALITY ('SEEK_OBS', 'READ', 'BYTE',
     :         OBS_QUAL, OBS_Q_SLOT, STATUS)

*          If the observation is a FLAT, then also find out if it
*          has been normalised by looking for a NORMALIS item in its
*          FITS structure.
            IF (TYPE_REQUIRED .EQ. 'FLAT') THEN

               FITS_EXISTS = .FALSE.
               CALL DSA_SEEK_FITS( 'SEEK_OBS', 'NORMALIS',
     :            FITS_EXISTS, COMMENT, ELEMENTS, STRLEN, STATUS )

               IF ( FITS_EXISTS )  THEN

                  CALL DSA_GET_FITS_C( 'SEEK_OBS', 'NORMALIS', 0,
     :              NORMALIS, COMMENT, STATUS )
                  CALL DSA_GET_FITS_F( 'SEEK_OBS', 'DETINCR', 0,
     :              FLAT_DETINCR, COMMENT, STATUS )
                  CALL DSA_GET_FITS_I( 'SEEK_OBS', 'DETNINCR', 0,
     :              FLAT_DETNINCR, COMMENT, STATUS )
               ELSE

                  NORMALIS = 'NO'
                  FLAT_DETINCR = 1.0
                  FLAT_DETNINCR = 1
               ENDIF

               CALL CHR_UCASE( NORMALIS )

               IF ( NORMALIS .EQ. 'YES' ) THEN

                  FLAT_NORMALISED = .TRUE.
               ELSE

                  FLAT_NORMALISED = .FALSE.
               END IF

               SUPERSAMPLING = 1
               OVERSAMPLING = 1
               IF ( FLAT_DETNINCR .GT. 1 ) SUPERSAMPLING = NINT( FLAT_DETNINCR * FLAT_DETINCR )
               OVERSAMPLING = FLAT_DETNINCR / SUPERSAMPLING
               IF ( OVERSAMPLING .GT. 1 ) THEN
                 FLAT_OVERSAMPLED = .TRUE.
               ELSE
                 FLAT_OVERSAMPLED = .FALSE.
               ENDIF
               IF ( FLAT_OVERSAMPLED .AND. VERBOSE )
     :            CALL MSG_OUT( ' ', 'FLAT is OVERSAMPLED', STATUS )
            ENDIF

*          Check that everything has worked so far.
            IF ( STATUS .NE. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'Error mapping data', STATUS )
            END IF

            IF (STATUS .EQ. SAI__OK) THEN

*          Check size of current virtual memory allocation by comparing
*          the number of elements of the previously mapped observation
*          with the number of elements required for the current
*          observation. If the numbers match, the same virtual memory
*          can be used. If the numbers don't match, the virtual memory
*          needs to be freed and some more re-allocated.
               IF (NELM_VM .NE. NELM) THEN

*               NELM_VM is initialised to zero, and is set to zero
*               when no virtual memory has been mapped. Do not attempt
*               to free any virtual memory if it has not been mapped.
                  IF (NELM_VM .GT. 0) THEN

*                  The old virtual memory needs to be freed.
*                  (Note that the same code below is used to free
*                  the memory belonging to BIAS, DARK or FLAT frames,
*                  depending upon the pointers recalled from the common
*                  block.
*                  Free the area used for the 4-byte DATA array
*                  and check this has worked.
                     CALL PSX_FREE( DATA_VM, STATUS )
                     IF ( STATUS .NE. SAI__OK ) THEN
                        STATUS = SAI__ERROR
                        CALL ERR_REP (' ',
     :                     'Error freeing data VM', STATUS)
                     ENDIF

*                  Free the area used for the 4-byte VARIANCE array
*                  and check this has worked.
                     CALL PSX_FREE( VAR_VM, STATUS )
                     IF ( STATUS .NE. SAI__OK ) THEN
                        STATUS = SAI__ERROR
                        CALL ERR_REP (' ',
     :                     'Error freeing variance VM', STATUS)
                     ENDIF

*                  Free the area used for the 1-byte QUALITY array
*                  and check this has worked.
                     CALL PSX_FREE( QUAL_VM, STATUS )
                     IF ( STATUS .NE. SAI__OK ) THEN
                        STATUS = SAI__ERROR
                        CALL ERR_REP (' ',
     :                     'Error freeing quality VM', STATUS)
                     ENDIF

                     NELM_VM = 0
                  ENDIF

*                Allocate the appropriate amount of virtual memory.
*                (Note that the same code below is used to allocate
*                memory for to BIAS, DARK or FLAT frames, depending
*                upon the pointers recalled from the common block.
*                Allocate memory for NELM elements of the 4-byte
*                DATA array, and check it has worked.
                  CALL PSX_MALLOC( FLOATSIZE*NELM, DATA_VM, STATUS )
                  CALL GEN_FILL( FLOATSIZE*NELM, 0.0, %val(DATA_VM) )
                  IF ( STATUS .NE. SAI__OK ) THEN
                     STATUS = SAI__ERROR
                     CALL ERR_REP (' ',
     :                  'Error getting data VM', STATUS)
                  ENDIF

*                Allocate memory for NELM elements of the 4-byte
*                VARIANCE array, and check it has worked.
                  CALL PSX_MALLOC( FLOATSIZE*NELM, VAR_VM, STATUS )
                  CALL GEN_FILL( FLOATSIZE*NELM, 0.0, %val(VAR_VM) )
                  IF ( STATUS .NE. SAI__OK ) THEN
                     STATUS = SAI__ERROR
                     CALL ERR_REP (' ',
     :                  'Error getting variance VM', STATUS)
                  ENDIF

*                Allocate memory for NELM elements of the 1-byte
*                QUALITY array, and check it has worked.
                  CALL PSX_MALLOC( BYTESIZE*NELM, QUAL_VM, STATUS )
                  CALL GEN_FILL( BYTESIZE*NELM, 0, %val(DATA_VM) )
                  IF ( STATUS .NE. SAI__OK ) THEN
                    STATUS = SAI__ERROR
                    CALL ERR_REP (' ',
     :                  'Error getting quality VM', STATUS)
                  ENDIF

*                Store the current amount of virtual memory allocated
*                in the common block, so it may be recalled when the
*                routine is next used.
                  NELM_VM = NELM
               ENDIF
            ENDIF

*          Check everything has worked so far.
            IF (STATUS .EQ. SAI__OK) THEN

*            Copy the data, variance and quality arrays which have
*            been mapped from the observation into the areas of
*            virtual memory allocated for them.
               CALL GEN_MOVE( FLOATSIZE*NELM,
     :           %val(OBS_DATA), %val(DATA_VM) )
               CALL GEN_MOVE( FLOATSIZE*NELM,
     :           %val(OBS_VAR),  %val(VAR_VM) )
               CALL GEN_MOVE( BYTESIZE*NELM,
     :           %val(OBS_QUAL), %val(QUAL_VM) )

*            Set the common block pointers appropriately, and
*            dimensions in the case of FLAT.
               IF (TYPE_REQUIRED .EQ. 'BIAS') THEN

*               For a BIAS observation, store the temporary pointers
*               into the BIAS pointers in the common block, and
*               remember the name of the currently stored BIAS.
*               remember the name of the currently stored BIAS.
                  IF (VERBOSE) THEN
                    CALL MSG_SETC( 'ON', OBS_NAME )
                    CALL MSG_SETC( 'CB', CURRENT_BIAS )
                    CALL MSG_OUT( ' ', 'Current BIAS = ^CB, New BIAS = ^ON, Remapped memory', STATUS )
                  ENDIF
                  CURRENT_BIAS = OBS_NAME
                  BIAS_DATA = DATA_VM
                  BIAS_VAR = VAR_VM
                  BIAS_QUAL = QUAL_VM
                  BIAS_NELM = NELM_VM
               ELSE IF (TYPE_REQUIRED .EQ. 'DARK') THEN

*               For a DARK observation, store the temporary pointers
*               into the DARK pointers in the common block, and
*               remember the name of the currently stored DARK.
*               remember the name of the currently stored DARK.
                  IF (VERBOSE) THEN
                    CALL MSG_SETC( 'ON', OBS_NAME )
                    CALL MSG_SETC( 'CD', CURRENT_DARK )
                    CALL MSG_OUT( ' ', 'Current DARK = ^CD, New DARK = ^ON, Remapped memory', STATUS )
                  ENDIF
                  CURRENT_DARK = OBS_NAME
                  DARK_DATA = DATA_VM
                  DARK_VAR = VAR_VM
                  DARK_QUAL = QUAL_VM
                  DARK_NELM = NELM_VM
               ELSE IF (TYPE_REQUIRED .EQ. 'FLAT') THEN

*               For a DARK observation, store the temporary pointers
*               into the DARK pointers in the common block, and
*               remember the name of the currently stored DARK.
                  IF (VERBOSE) THEN
                    CALL MSG_SETC( 'ON', OBS_NAME )
                    CALL MSG_SETC( 'CB', CURRENT_FLAT )
                    CALL MSG_OUT( ' ', 'Current FLAT = ^CB, New FLAT = ^ON, Remapped memory', STATUS )
                  ENDIF
                  CURRENT_FLAT = OBS_NAME
                  FLAT_DATA = DATA_VM
                  FLAT_VAR = VAR_VM
                  FLAT_QUAL = QUAL_VM
                  FLAT_NELM = NELM_VM
               ELSE IF (TYPE_REQUIRED .EQ. 'STANDARD') THEN

*               For a STANDARD observation, store the temporary pointers
*               into the STANDARD pointers in the common block, and
*               remember the name of the currently stored STANDARD.
                  IF (VERBOSE) THEN
                    CALL MSG_SETC( 'ON', OBS_NAME )
                    CALL MSG_SETC( 'CB', CURRENT_STANDARD )
                    CALL MSG_OUT( ' ', 'Current STANDARD = ^CB, New STANDARD = ^ON, Remapped memory', STATUS )
                  ENDIF
                  CURRENT_STANDARD = OBS_NAME
                  STANDARD_DATA = DATA_VM
                  STANDARD_VAR = VAR_VM
                  STANDARD_QUAL = QUAL_VM
                  STANDARD_NELM = NELM_VM
               END IF
            ENDIF

            IF ( VERBOSE ) THEN
              CALL MSG_SETI( 'BD', BIAS_DATA )
              CALL MSG_SETI( 'BV', BIAS_VAR )
              CALL MSG_SETI( 'BQ', BIAS_QUAL )
              CALL MSG_SETI( 'BN', BIAS_NELM )
              CALL MSG_SETC( 'BF', CURRENT_BIAS )
              CALL MSG_OUT( ' ', 'Current_BIAS=^BF, Data_ptr=^BD,
     :          Variance_ptr=^BV, Quality_ptr=^BQ, Nelms=^BN', STATUS )
              CALL MSG_SETI( 'DD', DARK_DATA )
              CALL MSG_SETI( 'DV', DARK_VAR )
              CALL MSG_SETI( 'DQ', DARK_QUAL )
              CALL MSG_SETI( 'DN', DARK_NELM )
              CALL MSG_SETC( 'DF', CURRENT_DARK )
              CALL MSG_OUT( ' ', 'Current_DARK=^DF, Data_ptr=^DD,
     :          Variance_ptr=^DV, Quality_ptr=^DQ, Nelms=^DN', STATUS )
              CALL MSG_SETI( 'FD', FLAT_DATA )
              CALL MSG_SETI( 'FV', FLAT_VAR )
              CALL MSG_SETI( 'FQ', FLAT_QUAL )
              CALL MSG_SETI( 'FN', FLAT_NELM )
              CALL MSG_SETC( 'FF', CURRENT_FLAT )
              CALL MSG_OUT( ' ', 'Current_FLAT=^FF, Data_ptr=^FD,
     :          Variance_ptr=^FV, Quality_ptr=^FQ, Nelms=^FN', STATUS )
              CALL MSG_SETI( 'SD', STANDARD_DATA )
              CALL MSG_SETI( 'SV', STANDARD_VAR )
              CALL MSG_SETI( 'SQ', STANDARD_QUAL )
              CALL MSG_SETI( 'SN', STANDARD_NELM )
              CALL MSG_SETC( 'SF', CURRENT_STANDARD )
              CALL MSG_OUT( ' ', 'Current_STANDARD=^SF, Data_ptr=^SD,
     :          Variance_ptr=^SV, Quality_ptr=^SQ, Nelms=^SN', STATUS )
            ENDIF

*         The following DSA calls are required because DSA is not
*         opened and closed by this routine, so it must tidy up
*         any operations it has carried out.
*         The quality array must be processed explicitly, even though
*         we haven't done anything to it. This is a feature of DSA.
            CALL DSA_POST_PROCESS_QUALITY ('SEEK_OBS', STATUS)

*         Unmap the data mapped above.
            CALL DSA_UNMAP (OBS_SLOT, STATUS)
            CALL DSA_UNMAP (OBS_V_SLOT, STATUS)
            CALL DSA_UNMAP (OBS_Q_SLOT, STATUS)

*        Finally close the reduced observation structure
            CALL DSA_CLOSE_STRUCTURE ('SEEK_OBS', STATUS)
         ENDIF

         IF ( STATUS .NE. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Error closing DSA', STATUS )
         END IF
      END IF
 500  CONTINUE

      END
