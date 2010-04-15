C+
      SUBROUTINE RED3_EXTRACT (STATUS)
C
C     E X T R A C T
C
C     Averages a number of consecutive rows from an image to
C     produce a 1D data object, and calculates an error on the mean
C     for each point (A 'row' is all the pixels with a given y-value.)
C
C     Command parameters -
C
C     'IMAGE'    The name of the image from which the rows
C                are to be taken.
C
C     'YSTART'   The Y-value of the first row to be used.
C
C     'YEND'     The Y-value of the last row to be used.
C
C     'SPECTRUM' The name of the resulting data.
C
C     Output data -
C
C     SPECTRUM is created with the same structure as IMAGE,
C     except that the data will only have one dimension, and if
C     IMAGE had a Y axis structure, this will be omitted.  Any X
C     axis structure will be copied unchanged.
C
C                                     KS / CIT 29th June 1984
C     Modified:
C
C     8th July 1988  Rewritten to use DSA routines.  KS / AAO.
C     18th July 1990 Rewritten to calculate average, errors, quality.
C                                                   JFL / JACH.
C     30-Nov-95: remove adamdefns, adamerrs, add sae_par for unix porting (KK)
*     27-Feb-96: rename from red4_
C+
      IMPLICIT NONE
C
C     Functions
C
      INTEGER GEN_BSEARCH
C
C     ADAM include files
C
      INCLUDE 'SAE_PAR'
C
C     ADAM status
C
      INTEGER STATUS
C
C     Local variables
C
      LOGICAL   VARIANCE           ! TRUE if image has error array
      LOGICAL   QUALITY            ! TRUE if image has quality array
      LOGICAL   FLAGGED            ! TRUE if image has flagged values
      INTEGER   DIMS(2)            ! Image dimensions
      INTEGER   IPTR               ! VM address for image data
      INTEGER   APTR               !           "          Y-axis
      INTEGER   IVPTR              !           "          variance
      INTEGER   IQPTR              !           "          quality
      INTEGER   IYEN               ! Last image row to extract
      INTEGER   IYST               ! First image row to extract
      INTEGER   ITEMP              !
      INTEGER   NDIM               ! Number of image dimensions
      INTEGER   NELM               ! Number of elements in image - ignored
      INTEGER   NX                 ! First dimension of image
      INTEGER   NY                 ! Second dimension of image
      INTEGER   SLOT               ! Slot number for mapped data - ignored
      INTEGER   SPTR               ! VM address for spectrum data
      INTEGER   SVPTR              !           "            variance
      INTEGER   SQPTR              !           "            quality
      INTEGER   NUMPTR             !           "            work array
      INTEGER   SUMPTR             !           "                "
      INTEGER   SUMSQPTR           !           "                "
      REAL      YSTART             ! Y position in IMAGE where extraction starts
      REAL      YEND               !   and ends
      REAL      FBAD               ! Value of bad pixel flag
      CHARACTER*80 IMAGE           ! Name of image file
      CHARACTER*80 SPECT           ! Name of file to contain extracted spectrum
C
C     Parameters controlling the way DSA_OUTPUT opens the spectrum file
C
      INTEGER   NEW_FILE, NO_DATA
      PARAMETER (NEW_FILE=1, NO_DATA=1)

      IF (STATUS .NE. SAI__OK) RETURN

!      CALL DSA_WRUSER (
!     :'This is NOT the standard Figaro version of EXTRACT. It will ')
!      CALL DSA_WRUSER (
!     :'calculate the mean values of the input image columns and the ')
!      CALL DSA_WRUSER (
!     :'error on the mean.\N')
C
C     Open DSA routines
C
      CALL DSA_OPEN(STATUS)
C
C     Open IMAGE file
C
      CALL PAR_GET0C ('IMAGE', IMAGE, STATUS)
      CALL DSA_NAMED_INPUT ('IMAGE',IMAGE,STATUS)
C
C     Get size of data in IMAGE
C
      CALL DSA_DATA_SIZE ('IMAGE',2,NDIM,DIMS,NELM,STATUS)
      NX=DIMS(1)
      IF (NDIM.EQ.1) THEN
         NY=1
      ELSE
         NY=DIMS(2)
      END IF
C
C     Get range of Y values.
C
      CALL DSA_AXIS_RANGE ('IMAGE',2,' ',.FALSE.,YSTART,YEND,
     :                                             IYST,IYEN,STATUS)
C
C     Map axis data and determine array indices of area to be extracted
C
      CALL DSA_MAP_AXIS_DATA ('IMAGE',2,'READ','FLOAT',APTR,SLOT,
     :   STATUS)
      CALL PAR_GET0R ('YSTART',YSTART,STATUS)
      CALL PAR_GET0R ('YEND',YEND,STATUS)
      IF (STATUS .EQ. SAI__OK) THEN
         IYST = GEN_BSEARCH (%VAL(APTR),NY,YSTART)
         IYEN = GEN_BSEARCH (%VAL(APTR),NY,YEND)
      ENDIF
      IF (IYEN .LT. IYST) THEN
         ITEMP = IYST
         IYST = IYEN
         IYEN = ITEMP
      ENDIF
C
C     Search for error, quality arrays, magic values
C
      CALL DSA_SEEK_ERRORS ('IMAGE', VARIANCE, STATUS)
      CALL DSA_SEEK_QUALITY ('IMAGE', QUALITY, STATUS)
      CALL DSA_SEEK_FLAGGED_VALUES ('IMAGE', FLAGGED, STATUS)
C
C     Create new spectrum file.
C
      CALL PAR_GET0C ('SPECTRUM', SPECT, STATUS)
      CALL DSA_NAMED_OUTPUT ('SPECT',SPECT,'IMAGE',NO_DATA,
     :                                              NEW_FILE,STATUS)
C
C     Create the new data and axis arrays in the spectrum file.
C
      CALL DSA_RESHAPE_DATA ('SPECT','IMAGE',1,NX,STATUS)
      CALL DSA_RESHAPE_AXIS ('SPECT',1,'IMAGE',1,1,NX,STATUS)
C
C     Map the input and output data + variances and quality if present
C
      IF (QUALITY) THEN
         CALL DSA_USE_QUALITY ('SPECT', STATUS)
         CALL DSA_USE_QUALITY ('IMAGE', STATUS)
      ELSE IF (FLAGGED) THEN
         CALL DSA_USE_FLAGGED_VALUES ('SPECT', STATUS)
         CALL DSA_USE_FLAGGED_VALUES ('IMAGE', STATUS)
         CALL DSA_GET_FLAG_VALUE ('FLOAT',FBAD,STATUS)
      ENDIF
      CALL DSA_MAP_DATA ('SPECT','WRITE','FLOAT',SPTR,SLOT,STATUS)
      CALL DSA_MAP_DATA ('IMAGE','READ','FLOAT',IPTR,SLOT,STATUS)
      CALL DSA_MAP_VARIANCE ('SPECT','WRITE','FLOAT',SVPTR,SLOT,
     :   STATUS)
      IF (VARIANCE) THEN
         CALL DSA_MAP_VARIANCE ('IMAGE','READ','FLOAT',IVPTR,SLOT,
     :      STATUS)
      ENDIF
      IF (QUALITY) THEN
         CALL DSA_MAP_QUALITY ('SPECT','WRITE','BYTE',SQPTR,SLOT,
     :      STATUS)
         CALL DSA_MAP_QUALITY ('IMAGE','READ','BYTE',IQPTR,SLOT,
     :      STATUS)
      ENDIF

      IF (MAX(IYST,1) .EQ. MIN(IYEN,NY)) THEN
         IF (VARIANCE .AND. (STATUS .EQ. SAI__OK)) THEN
!            CALL DSA_WRUSER (
!     :'The spectrum variance will be copied from the input image.\N')
         ENDIF
      ELSE
         IF (STATUS .EQ. SAI__OK) THEN
!            CALL DSA_WRUSER (
!     :'The spectrum variance will be calculated from the spread ')
!            CALL DSA_WRUSER (
!     :'of the image values about the mean.\N')
         ENDIF
      ENDIF
C
C     Get some workspace
C
      CALL DSA_GET_WORK_ARRAY (NX,'INT',NUMPTR,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY (NX,'FLOAT',SUMPTR,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY (NX,'FLOAT',SUMSQPTR,SLOT,STATUS)
C
C     Perform the extraction
C
      IF (STATUS .EQ. SAI__OK) THEN
         CALL FIGE_XTRACT(%VAL(IPTR),%VAL(IVPTR),%VAL(IQPTR),NX,NY,
     :      IYST,IYEN,VARIANCE,QUALITY,FLAGGED,FBAD,%VAL(SPTR),
     :      %VAL(SVPTR),%VAL(SQPTR),%VAL(NUMPTR),%VAL(SUMPTR),
     :      %VAL(SUMSQPTR))
      ENDIF
C
C     Close down everything
C
      CALL DSA_CLOSE(STATUS)
C
      END
