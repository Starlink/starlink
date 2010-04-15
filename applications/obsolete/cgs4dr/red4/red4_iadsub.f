C+
      SUBROUTINE RED4_IADSUB (COMMAND, STATUS)
C
C     R E D 4 _ I A D S U B
C
C     Adds, multiplies, divides or subtracts two images.
C
C     Command parameters -
C
C     IMAGE1 The name of the structure containing the first image.
C
C     IMAGE2 The name of the structure containing the second
C            image data.
C
C     OUTPUT The name of the result of the operation.  This can
C            be the same as for IMAGE1.  If not, a new structure
C            is created, with everything but the data (and any error
C            or data quality information) a direct copy of the first
C            image.
C
C     The command itself (IADD4,IMULT4,IDIV4 or ISUB4) is used to
C     differentiate between the four operations.
C
C                                      KS / CIT 26th Sept 1983
C     Deficiencies :
C
C     This routine uses the first input structure IMAGE1 as a template
C     for the OUTPUT structure. This will be ok for simple data
C     structures, but if IMAGE1 contains header information in its
C     FITS, OBS or MORE structures, the information written to output
C     may be inappropriate. (For example, the EXPOSED item in the FITS
C     structure will give the total exposure time for IMAGE1 and not
C     for OUTPUT). Perhaps a more intelligent header processing
C     algorithm is required.
C
C     Modified:
C
C     8th  May  1986  KS / AAO.  Now tidies up after an error in
C                     the usual Figaro way, after label 500.
C     3rd  June 1987  KS / AAO.  Re-written using the new DSA_ routines.
C     24th July 1987  DJA/ AAO.  Modified dynamic memory handling - now
C                     uses DYN_ routines
C     20th Mar  1989  JM / RAL.  Modified to handle quality and errors.
C     31st Oct  1989  JFL/ ROE.  Modified to fit into RED4 monolith and use
C                                variances. Still a mixture of ADAM and
C                                Figaro PAR calls, statuses etc.
C     24th Apr  1990  SMB/ ROE.  Because of memory corruption problems, the
C                                code needs to be compiled with array bounds
C                                checking switched on. The Figaro dynamic
C                                memory functions (DYN_ELEMENT, DYNAMIC_MEM,
C                                DYN_INCREMENT) would not allow this. Code
C                                modified to use %val() instead.
C     17th May  1990  SMB/ ROE.  SLOTs now distinguished with different
C                                variables. (I didn't like lumping them
C                                all together. It was bad practise).
C                                Incorrect code indentation fixed.
C                                Code spaced out more.
C     10th Sep  1990  SMB/ ROE.  Warning message about the template used
C                                made a bit more explanatory.
C      1st Oct  1991  PND/ JAC.  Change GEN_*AFE calls to GEN_*AFV to
C                                propagate variance instead of error
C     19th Feb  1993  PND/ JAC.  Conform to error strategy
C      1st July 1993  PND/ JAC.  Replace DSA_WRUSER with MSG_OUT etc
C+
C
C     ADAM stuff
C
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
C
C     Input parameters
C
      INTEGER STATUS                ! ADAM status
      CHARACTER COMMAND*(*)         ! Figaro command being serviced.
C
C     Local variables
C
      INTEGER   ADDRESS             ! Address of dynamic memory element
      INTEGER   DIMS(10)            ! Dimensions of first image.  Ignored.
      INTEGER   NDIM                ! Dimensionality of first image.  Ignored.
      INTEGER   NELM                ! Number of elements in input image.
      INTEGER   IGNORE              ! Disregarded status return value.
      INTEGER   D1PTR               ! Dynamic pointer to first image data.
      INTEGER   D2PTR               ! Dynamic pointer to second image data.
      INTEGER   D3PTR               ! Dynamic pointer to output image data.
      INTEGER   D1SLOT              ! DSA slot number for first image data
      INTEGER   D2SLOT              ! DSA slot number for second image data
      INTEGER   D3SLOT              ! DSA slot number for output image data
      INTEGER   Q1PTR               ! Dynamic pointer to first image quality.
      INTEGER   Q2PTR               ! Dynamic pointer to second image quality.
      INTEGER   Q3PTR               ! Dynamic pointer to output image quality.
      INTEGER   Q1SLOT              ! DSA slot number for first image quality
      INTEGER   Q2SLOT              ! DSA slot number for second image quality
      INTEGER   Q3SLOT              ! DSA slot number for output image quality
      INTEGER   V1PTR               ! Dynamic pointer to first image variances.
      INTEGER   V2PTR               ! Dynamic pointer to second image variances.
      INTEGER   V3PTR               ! Dynamic pointer to output image variances.
      INTEGER   V1SLOT              ! DSA slot number for first image variances
      INTEGER   V2SLOT              ! DSA slot number for second image variances
      INTEGER   V3SLOT              ! DSA slot number for output image variances

      LOGICAL   VARIANCES           ! True if both inputs have error arrays
      LOGICAL   VAR1                ! True if IMAGE1 has an error array
      LOGICAL   VAR2                ! True if IMAGE2 has an error array
      REAL      FBAD                ! Flag value for 'FLOAT' data
      LOGICAL   FLAGS               ! True if either image has flagged data
C                                        values
      LOGICAL   QUAL                ! True if either image has a quality array
      CHARACTER*80 IMAGE1           ! Name of file holding first image
      CHARACTER*80 IMAGE2           ! Name of file holding second image
      CHARACTER*80 OUTPUT           ! Name of file to hold output image
      CHARACTER*80 ERROR_TYPE       ! Type of errors, GAUSSIAN only presently

      IF (STATUS .NE. ADAM__OK) RETURN
C
C     Initial settings
C
      CALL DSA_OPEN (STATUS)
C
C     Open the two input image files, using named inputs and direct PAR calls
C     to avoid the weaknesses of JFL's Figaro/ADAM parameter wraparounds
      CALL PAR_GET0C ('IMAGE1', IMAGE1, STATUS)
      CALL RED4_CHECK_INPUT( IMAGE1, STATUS )

      CALL DSA_NAMED_INPUT ('IMAGE1', IMAGE1, STATUS)
      IF ( STATUS .NE. ADAM__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'IMAGE1', IMAGE1 )
         CALL ERR_REP( ' ', 'RED4_IADSUB: '/
     :     /'Unable to open input data ^IMAGE1', STATUS )
         GOTO 500
      END IF

      CALL PAR_GET0C ('IMAGE2', IMAGE2, STATUS)
      CALL RED4_CHECK_INPUT( IMAGE2, STATUS )

      CALL DSA_NAMED_INPUT ('IMAGE2', IMAGE2, STATUS)
      IF ( STATUS .NE. ADAM__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'IMAGE2', IMAGE2 )
         CALL ERR_REP( ' ', 'RED4_IADSUB: '/
     :     /'Unable to open input data ^IMAGE2', STATUS )
         GOTO 500
      END IF

      CALL PAR_GET0C ('OUTPUT', OUTPUT, STATUS)

C
C     Get dimensions of input data, and check that the images
C     match in size.  Check for axis mismatches, but allow them.
      CALL DSA_DATA_SIZE ('IMAGE1', 10, NDIM, DIMS, NELM, STATUS)
      CALL DSA_MATCH_SIZES ('IMAGE1', 'IMAGE2', STATUS)
      IGNORE = STATUS
      CALL DSA_MATCH_AXES ('IMAGE1', 'IMAGE2', IGNORE)
C
C     Open output image
C
      IF ( STATUS .EQ. ADAM__OK ) THEN

         CALL MSG_OUT( ' ',
     :     'WARNING: Header and miscellaneous info from ', STATUS )
         CALL MSG_SETC( 'IMAGE1', IMAGE1 )
         CALL MSG_OUT( ' ',
     :     '^IMAGE1 will be copied unchanged to ', STATUS )
         CALL MSG_SETC( 'OUTPUT', OUTPUT )
         CALL MSG_OUT( ' ', '^OUTPUT, so it may be incorrect', STATUS )
      ENDIF

      CALL DSA_NAMED_OUTPUT ('OUTPUT', OUTPUT, 'IMAGE1', 0, 0, STATUS)
C
C     Map data arrays
C
      CALL DSA_MAP_DATA ( 'IMAGE1', 'READ', 'FLOAT', ADDRESS, D1SLOT,
     :   STATUS)
      D1PTR = ADDRESS
      CALL DSA_MAP_DATA ('IMAGE2', 'READ', 'FLOAT', ADDRESS, D2SLOT,
     :   STATUS)
      D2PTR = ADDRESS
      CALL DSA_MAP_DATA ('OUTPUT', 'WRITE', 'FLOAT', ADDRESS, D3SLOT,
     :   STATUS)
      D3PTR = ADDRESS
C
C     Process the various data arrays - allow for error arrays,
C     quality arrays and flagged data values.
C
C     If either image has quality data, map that
C
      CALL DSA_SEEK_QUALITY ('IMAGE1', QUAL, STATUS)
      IF (QUAL .EQ. .FALSE.) THEN
         CALL DSA_SEEK_QUALITY ('IMAGE2', QUAL, STATUS)
      ENDIF

      IF (QUAL) THEN

         CALL DSA_USE_QUALITY ('IMAGE1', STATUS)
         CALL DSA_USE_QUALITY ('IMAGE2', STATUS)
         CALL DSA_USE_QUALITY ('OUTPUT', STATUS)

         CALL DSA_MAP_QUALITY ('IMAGE1', 'READ', 'BYTE', ADDRESS,
     :     Q1SLOT, STATUS)
         Q1PTR = ADDRESS
         CALL DSA_MAP_QUALITY ('IMAGE2', 'READ', 'BYTE', ADDRESS,
     :     Q2SLOT, STATUS)
         Q2PTR = ADDRESS
         CALL DSA_MAP_QUALITY ('OUTPUT', 'WRITE', 'BYTE', ADDRESS,
     :     Q3SLOT, STATUS)
         Q3PTR = ADDRESS
      END IF
C
C     If either image had flagged data values, use them and get
C     the flag value being used.
C
      CALL DSA_SEEK_FLAGGED_VALUES ('IMAGE1', FLAGS, STATUS)

      IF (FLAGS .EQ. .FALSE.) THEN
         CALL DSA_SEEK_FLAGGED_VALUES ('IMAGE2', FLAGS, STATUS)
      ENDIF

      IF (FLAGS) THEN

         CALL DSA_USE_FLAGGED_VALUES ('IMAGE1', STATUS)
         CALL DSA_USE_FLAGGED_VALUES ('IMAGE2', STATUS)
         CALL DSA_USE_FLAGGED_VALUES ('OUTPUT', STATUS)
         CALL DSA_GET_FLAG_VALUE ('FLOAT', FBAD, STATUS)
      END IF
C
C     If the ERRORS parameter is not NONE and if both images have error
C     arrays, map them and the output error array
C
      CALL PAR_GET0C ('ERRORS', ERROR_TYPE, STATUS)
      CALL DSA_SEEK_ERRORS ('IMAGE1', VAR1, STATUS)
      CALL DSA_SEEK_ERRORS ('IMAGE2', VAR2, STATUS)
      VARIANCES = .FALSE.

      IF (VAR1 .AND. VAR2 .AND. (ERROR_TYPE .NE. 'NONE')) THEN

         CALL DSA_MAP_VARIANCE ('IMAGE1', 'READ', 'FLOAT', ADDRESS,
     :                         V1SLOT, STATUS)
         V1PTR = ADDRESS
         CALL DSA_MAP_VARIANCE ('IMAGE2', 'READ', 'FLOAT', ADDRESS,
     :                         V2SLOT, STATUS)
         V2PTR = ADDRESS
         CALL DSA_MAP_VARIANCE ('OUTPUT', 'WRITE', 'FLOAT', ADDRESS,
     :                         V3SLOT, STATUS)
         V3PTR = ADDRESS
         VARIANCES = .TRUE.

      ELSE IF ((VAR1.AND..NOT.VAR2) .OR. (.NOT.VAR1.AND.VAR2)) THEN

         CALL MSG_OUT( ' ', 'Only one image has error information; '/
     :     /'Output array will have no error array', STATUS )
      END IF

      IF ( STATUS .NE. ADAM__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_IADSUB: '/
     :     /'A DSA has been detected', STATUS )
         GOTO 500
      END IF
C
C     Operate on the image data
C
      IF ((ERROR_TYPE.EQ.'GAUSSIAN') .OR. (ERROR_TYPE.EQ.'NONE')) THEN

         IF (COMMAND .EQ. 'IADD') THEN

            CALL GEN_ADDAFV (NELM,
     :        %val(D1PTR),%val(D2PTR),%val(D3PTR),
     :        %val(Q1PTR),%val(Q2PTR),%val(Q3PTR),
     :        %val(V1PTR),%val(V2PTR),%val(V3PTR),
     :                       QUAL, FLAGS, FBAD, VARIANCES)
         ELSE IF (COMMAND.EQ.'ISUB') THEN

            CALL GEN_SUBAFV (NELM,
     :       %val(D1PTR), %val(D2PTR), %val(D3PTR),
     :       %val(Q1PTR), %val(Q2PTR), %val(Q3PTR),
     :       %val(V1PTR), %val(V2PTR), %val(V3PTR),
     :                      QUAL, FLAGS, FBAD, VARIANCES)
         ELSE IF (COMMAND.EQ.'IMULT') THEN

            CALL GEN_MULTAFV (NELM,
     :       %val(D1PTR), %val(D2PTR), %val(D3PTR),
     :       %val(Q1PTR), %val(Q2PTR), %val(Q3PTR),
     :       %val(V1PTR), %val(V2PTR), %val(V3PTR),
     :                      QUAL, FLAGS, FBAD, VARIANCES)
         ELSE IF (COMMAND.EQ.'IDIV') THEN

            CALL GEN_DIVAFV (NELM,
     :       %val(D1PTR), %val(D2PTR), %val(D3PTR),
     :       %val(Q1PTR), %val(Q2PTR), %val(Q3PTR),
     :       %val(V1PTR), %val(V2PTR), %val(V3PTR),
     :                      QUAL, FLAGS, FBAD, VARIANCES)
         END IF
      ELSE

         CALL MSG_OUT( ' ',
     :     'Only GAUSSIAN error propagation or none at ', STATUS )
         CALL MSG_OUT( ' ',
     :     'all can be handled at the ', STATUS )
         CALL MSG_OUT( ' ',
     :     'moment. Check the parameter ERRORS', STATUS )

      ENDIF
C
C     Tidy up.
C
500   CONTINUE
      CALL DSA_CLOSE (STATUS)
      END
