C+
      SUBROUTINE RED3_SCALE (STATUS)
C
C     R E D 3 _ S C A L E
C
C     Scales an image/spectrum by the formula output = FACTOR*input + CONSTANT
C
C     Command parameters -
C
C     INPUT  The name of the structure containing the input image.
C
C     FACTOR Scale factor to multiply by.
C
C     CONSTANT Constant to be added.
C
C     OUTPUT The name of the result of the operation.  This can
C            be the same as for INPUT. If not, a new structure
C            is created, with everything but the data (and any error
C            or data quality information) a direct copy of the first
C            image.
C
C                                      AB / JAC 24th Sept 1990
C     Modified:
C
C     24th Sept 1990 AB / JAC Original, from RED4_IADSUB
C     30-Nov-95: remove adamdefns, adamerrs, add sae_par for unix porting (KK)
*     27-Feb-96: rename from red4_
C+
      IMPLICIT NONE
C
C     ADAM stuff
C
      INCLUDE 'SAE_PAR'
C
C     Input parameters
C
      INTEGER STATUS                ! ADAM status
C
C     Local variables
C
      INTEGER   ADDRESS             ! Address of dynamic memory element
      INTEGER   DIMS(10)            ! Dimensions of first image.  Ignored.
      INTEGER   NDIM                ! Dimensionality of first image.  Ignored.
      INTEGER   NELM                ! Number of elements in input image.
      INTEGER   DIPTR               ! Dynamic pointer to input image data.
      INTEGER   DOPTR               ! Dynamic pointer to output image data.
      INTEGER   DISLOT              ! DSA slot number for input image data
      INTEGER   DOSLOT              ! DSA slot number for output image data
      INTEGER   QIPTR               ! Dynamic pointer to input image quality.
      INTEGER   QOPTR               ! Dynamic pointer to output image quality.
      INTEGER   QISLOT              ! DSA slot number for input image quality
      INTEGER   QOSLOT              ! DSA slot number for output image quality
      INTEGER   VIPTR               ! Dynamic pointer to input image variances.
      INTEGER   VOPTR               ! Dynamic pointer to output image variances.
      INTEGER   VISLOT              ! DSA slot number for input image variances
      INTEGER   VOSLOT              ! DSA slot number for output image variances

      LOGICAL   VARIANCES           ! True if both inputs have error arrays
      LOGICAL   VARI                ! True if INPUT has an error array
      REAL      FBAD                ! Flag value for 'FLOAT' data
      LOGICAL   FLAGS               ! True if image has flagged data values
      LOGICAL   QUAL                ! True if image has a quality array
      CHARACTER*80 INPUT            ! Name of file holding image
      CHARACTER*80 OUTPUT           ! Name of file to hold output image
      CHARACTER*80 ERROR_TYPE       ! Type of errors, GAUSSIAN only presently

      REAL FACTOR, CONSTANT         ! FACTOR and CONSTANT applied to data

      IF (STATUS .NE. SAI__OK) RETURN
C
C     Initial settings
C
      CALL DSA_OPEN (STATUS)
C
C     Open the input image file, using named inputs and direct PAR calls
C     to avoid the weaknesses of JFL's Figaro/ADAM parameter wraparounds
C
      CALL PAR_GET0C ('INPUT', INPUT, STATUS)
      CALL DSA_NAMED_INPUT ('INPUT', INPUT, STATUS)

C     Get dimensions of input data

      CALL DSA_DATA_SIZE ('INPUT', 10, NDIM, DIMS, NELM, STATUS)

C     Get FACTOR and CONSTANT

      CALL PAR_GET0R ('FACTOR', FACTOR, STATUS)
      CALL PAR_GET0R ('CONSTANT', CONSTANT, STATUS)
C
C     Open output image
C
      IF (STATUS .EQ. SAI__OK) THEN

         CALL DSA_WRUSER ('WARNING. Currently INPUT is being used ')
         CALL DSA_WRUSER ('as the template for the OUTPUT file, ')
         CALL DSA_WRUSER ('including the MORE structure which  ')
         CALL DSA_WRUSER ('will be inappropriate.\N')
      ENDIF

      CALL PAR_GET0C ('OUTPUT', OUTPUT, STATUS)
      CALL DSA_NAMED_OUTPUT ('OUTPUT', OUTPUT, 'INPUT', 0, 0, STATUS)
C
C     Map data arrays
C
      CALL DSA_MAP_DATA ( 'INPUT', 'READ', 'FLOAT', ADDRESS, DISLOT,
     :   STATUS)
      DIPTR = ADDRESS

      CALL DSA_MAP_DATA ('OUTPUT', 'WRITE', 'FLOAT', ADDRESS, DOSLOT,
     :   STATUS)
      DOPTR = ADDRESS
C
C     Process the various data arrays - allow for error arrays,
C     quality arrays and flagged data values.
C
C     If image has quality data, map that
C
      CALL DSA_SEEK_QUALITY ('INPUT', QUAL, STATUS)

      IF (QUAL) THEN

         CALL DSA_USE_QUALITY ('INPUT', STATUS)
         CALL DSA_USE_QUALITY ('OUTPUT', STATUS)

         CALL DSA_MAP_QUALITY ('INPUT', 'READ', 'BYTE', ADDRESS,
     :     QISLOT, STATUS)
         QIPTR = ADDRESS

         CALL DSA_MAP_QUALITY ('OUTPUT', 'WRITE', 'BYTE', ADDRESS,
     :     QOSLOT, STATUS)
         QOPTR = ADDRESS
      END IF
C
C     If image has flagged data values, use them and get
C     the flag value being used.
C
      CALL DSA_SEEK_FLAGGED_VALUES ('INPUT', FLAGS, STATUS)

      IF (FLAGS) THEN

         CALL DSA_USE_FLAGGED_VALUES ('INPUT', STATUS)
         CALL DSA_USE_FLAGGED_VALUES ('OUTPUT', STATUS)
         CALL DSA_GET_FLAG_VALUE ('FLOAT', FBAD, STATUS)
      END IF
C
C     If the ERRORS parameter is not NONE and if image has error
C     array, map it and the output error array
C
      CALL PAR_GET0C ('ERRORS', ERROR_TYPE, STATUS)
      CALL DSA_SEEK_ERRORS ('INPUT', VARI, STATUS)
      VARIANCES = .FALSE.

      IF (VARI .AND. (ERROR_TYPE .NE. 'NONE')) THEN

         CALL DSA_MAP_VARIANCE ('INPUT', 'READ', 'FLOAT', ADDRESS,
     :                         VISLOT, STATUS)
         VIPTR = ADDRESS

         CALL DSA_MAP_VARIANCE ('OUTPUT', 'WRITE', 'FLOAT', ADDRESS,
     :                         VOSLOT, STATUS)
         VOPTR = ADDRESS
         VARIANCES = .TRUE.

      END IF

      IF (STATUS .NE. SAI__OK) GO TO 500
C
C     Operate on the image data
C
      IF ((ERROR_TYPE.EQ.'GAUSSIAN') .OR. (ERROR_TYPE.EQ.'NONE')) THEN

         CALL GEN_SCALE (NELM, %val(DIPTR),%val(DOPTR),
     :        %val(QIPTR),%val(QOPTR), %val(VIPTR),%val(VOPTR),
     :        QUAL, FLAGS, FBAD, VARIANCES,FACTOR,CONSTANT)

      ELSE

         CALL DSA_WRUSER ('Only GAUSSIAN error propagation or none at ')
         CALL DSA_WRUSER ('all can be handled at the ')
         CALL DSA_WRUSER ('moment. Check the parameter ERRORS.\N')

      ENDIF
C
C     Tidy up.
C
500   CONTINUE

      CALL DSA_CLOSE (STATUS)
C
      END
