      SUBROUTINE SUBSET
C+
C
C   -----------
C   S U B S E T
C   -----------
C
C   Description
C   -----------
C   Creates a subset of an image.
C
C
C   Scope of program
C   ----------------
C   - Images of up to six dimensions accepted.
C   - Data array types SHORT and FLOAT supported; others converted to FLOAT.
C   - Subsetting supported (obviously).
C   - Magic values not supported since not relevant.
C   - Quality and error arrays supported.
C   - Batch execution supported.
C
C
C   Environment
C   -----------
C   FIGARO
C
C
C   Parameters (read or written)
C   ----------------------------
C   IMAGE   Name of the structure containing the image to be subsetted.
C           (character)(prompted for).
C
C   START   Coordinate in each dimension of IMAGE at which the subset
C           operation is to start. (real, array)(prompted for).
C
C   END     Coordinate in each dimension of IMAGE at which the subset
C           operation is to end. (real, array)(prompted for).
C
C   OUTPUT  Name of the structure containing the image subset. May be the
C           same as IMAGE. (character)(prompted for).
C
C   Keywords
C   --------
C   None.
C
C
C   Propagation of data structure
C   -----------------------------
C   - All standard objects except the data array and axes are copied from
C     IMAGE.
C   - Data array and axes are reshaped.
C
C
C   Method
C   ------
C   - The structure IMAGE is copied to OUTPUT, with the exception of the
C     data array and axes. DSA_RESHAPE_ routines are called to modify the
C     dimensions of the OUTPUT axis and data arrays.
C   - The calibrations of the appropriate parts of the IMAGE axes are copied
C     to the OUTPUT axes.
C   - A subroutine appropriate to the data type is called to transfer the
C     required subset of the IMAGE data array to the OUTPUT data array.
C
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library DSA:
C     DSA_CLOSE
C     DSA_DATA_SIZE
C     DSA_INPUT
C     DSA_MAP_AXIS_DATA
C     DSA_MAP_DATA
C     DSA_MAP_ERRORS
C     DSA_MAP_QUALITY
C     DSA_OPEN
C     DSA_OUTPUT
C     DSA_RESHAPE_AXIS
C     DSA_RESHAPE_DATA
C     DSA_SEEK_ERRORS
C     DSA_SEEK_QUALITY
C     DSA_USE_FLAGGED_VALUES
C     DSA_WRUSER
C
C   Library DYN:
C     DYN_ELEMENT
C
C   Library GEN:
C     GEN_BSEARCH
C     GEN_ELEMF
C
C   Library NDP:
C     NDP_DISPLAY_PROGRESS
C     NDP_GET_IMAGE_INFO
C     NDP_PAR_RDARY
C
C
C   Internal subroutines called
C   ---------------------------
C   SUBSET_DATA_W
C   SUBSET_DATA_R
C   SUBSET_DATA_B
C   SUBSET_NEWAXIS
C
C
C   INCLUDE statements
C   ------------------
C   INCLUDE 'DYNAMIC_MEMORY'
C   INCLUDE 'MAGIC_VALUES'
C   INCLUDE 'NUMERIC_RANGES'
C
C
C   Extensions to FORTRAN77
C   -----------------------
C   END DO / IMPLICIT NONE / INCLUDE / Names > 6 characters
C
C
C   Possible future upgrades
C   ------------------------
C
C
C   Author/s
C   --------
C   Nick Fuller  RGO  (RGVAD::NMJF or NMJF@UK.AC.RGO.STAR)
C   Julian Gold  RGO  (CAVAD::GOLDJIL or GOLDJIL@UK.AC.CAM.AST-STAR)
C
C   History
C   -------
C   01-FEB-1989   - Original program
C   15-OCT-1991   - Quality and error array handling implemented. Code
C                   written in GENERIC format. (GOLDJIL)
C   10-DEC-1992   - Unix version. (GOLDJIL)
C   06-OCT-1994   - Removed lots of unused variables. (GJP)
C
C
C+-----------------------------------------------------------------------------
C
      IMPLICIT NONE
C
C     Functions used
C
      INTEGER  DYN_ELEMENT,GEN_BSEARCH
      REAL     GEN_ELEMF
C
C     Local variables
C
      INTEGER   ADDRESS             ! Address of dynamic memory element
      INTEGER   AXPTR(6)            ! Dynamic memory pointers to input axes
      INTEGER   AXSLOT(6)           ! Map slot numbers for input axes
      LOGICAL   BADPIX              ! Value of bad pixel flag
      INTEGER   DIMS(10)            ! Dimensions of image
      REAL      EMIN(6)             ! Minimum end coordinates
      REAL      EMAX(6)             ! Maximum end coordinates
      REAL      END(6)              ! End coordinate of subset
      LOGICAL   ERR                 ! Error data present flag
      INTEGER   ENDPIX(6)           ! End pixel of subset
      INTEGER   I                   ! Loop counter
      INTEGER   IEPTR               ! Dynamic pointer to i/p error array
      INTEGER   IESLOT              ! Map slot number for i/e error array
      INTEGER   IMPTR               ! Dynamic pointer to image data
      INTEGER   IQPTR               ! Dynamic pointer to i/p quality array
      INTEGER   IQSLOT              ! Map slot number for i/p quality array
      INTEGER   ISLOT               ! Map slot number for input data
      INTEGER   J                   ! Loop counter
      INTEGER   NDIM                ! Number of dimensions in image
      INTEGER   NELM                ! Number of elements in image
      INTEGER   OAXPTR(6)           ! Dynamic memory pointers to output axes
      INTEGER   OAXSLOT(6)          ! Map slot numbers for output axes
      INTEGER   ODIMS(10)           ! Dimensions of subset
      INTEGER   OEPTR               ! Dynamic pointer to o/p error array
      INTEGER   OESLOT              ! Map slot number for o/p error array
      INTEGER   ONDIM               ! Number of dimensions in subset
      INTEGER   ONELM               ! Number of elements in subset
      INTEGER   OQPTR               ! Dynamic pointer to o/p quality array
      INTEGER   OQSLOT              ! Map slot number for o/p quality array
      INTEGER   OSLOT               ! Map slot number for output data
      INTEGER   OUTPTR              ! Dynamic pointer to output image data
      LOGICAL   QUAL                ! Quality array present flag
      REAL      SMIN(6)             ! Minimum start coordinates
      REAL      SMAX(6)             ! Maximum start coordinates
      INTEGER   STAPIX(6)           ! Start pixel of subset
      REAL      START(6)            ! Start coordinate of subset
      INTEGER   STATUS              ! Status code
      CHARACTER TYPE*8              ! Data array type
C
      INTEGER   NEW_FILE,NO_DATA
      PARAMETER (NEW_FILE=1,NO_DATA=1)
C
      INCLUDE 'DYNAMIC_MEMORY'
      INCLUDE 'MAGIC_VALUES'
      INCLUDE 'NUMERIC_RANGES'
C
C     Initialize
C
      STATUS=0
      DO I=1,6
        STAPIX(I)=1
        ENDPIX(I)=1
      END DO
C
C     Open DSA system
C
      CALL DSA_OPEN(STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C     Open file for IMAGE
C
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C     Get information about IMAGE
C
      CALL NDP_GET_IMAGE_INFO('IMAGE',.TRUE.,.FALSE.,TYPE,BADPIX,STATUS)
      IF(STATUS.NE.0)GO TO 500
      CALL DSA_SEEK_ERRORS('IMAGE',ERR,STATUS)
      IF(STATUS.NE.0)GO TO 500
      CALL DSA_SEEK_QUALITY('IMAGE',QUAL,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C     Get dimensions of data array
C
      CALL DSA_DATA_SIZE('IMAGE',6,NDIM,DIMS,NELM,STATUS)
      IF(STATUS.NE.0)GO TO 500
      DO I=1,NDIM
        ENDPIX(I)=DIMS(I)
      END DO
C
C     Map IMAGE axes
C
      DO I=1,NDIM
        CALL DSA_MAP_AXIS_DATA
     &    ('IMAGE',I,'READ','FLOAT',ADDRESS,AXSLOT(I),STATUS)
        IF(STATUS.NE.0)GO TO 500
        AXPTR(I)=DYN_ELEMENT(ADDRESS)
      END DO
C
C     Set coordinate limits
C
      DO I=1,NDIM
        SMIN(I)=GEN_ELEMF(DYNAMIC_MEM(AXPTR(I)),1)
        EMIN(I)=SMIN(I)
        SMAX(I)=GEN_ELEMF(DYNAMIC_MEM(AXPTR(I)),DIMS(I))
        EMAX(I)=SMAX(I)
      END DO
C
C     Get coordinates of subset
C
      CALL NDP_PAR_RDARY('START',SMIN,SMAX,'N',' ',NDIM,6,START)
      CALL NDP_PAR_RDARY('END',EMIN,EMAX,'N',' ',NDIM,6,END)
C
C     Find nearest pixels
C
      DO I=1,NDIM
        STAPIX(I)=GEN_BSEARCH(DYNAMIC_MEM(AXPTR(I)),DIMS(I),START(I))
        ENDPIX(I)=GEN_BSEARCH(DYNAMIC_MEM(AXPTR(I)),DIMS(I),END(I))
      END DO
C
C     Compute dimensions and number of elements in subset
C
      J=0
      ONDIM=0
      ONELM=1
      DO I=1,NDIM
        IF(STAPIX(I).NE.ENDPIX(I))THEN
          J=J+1
          ONDIM=ONDIM+1
          ODIMS(J)=ENDPIX(I)-STAPIX(I)+1
          ONELM=ONELM*ODIMS(J)
        END IF
      END DO
C
C     Open file for OUTPUT
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',NO_DATA,NEW_FILE,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C     Reshape OUTPUT data array
C
      CALL DSA_WRUSER('%SUBSET-I-RESHAPE  ')
      CALL DSA_WRUSER('Reshaping output data array\\N')
      CALL DSA_RESHAPE_DATA('OUTPUT','IMAGE',ONDIM,ODIMS,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C     Subset the axes
C
      J=0
      DO I=1,NDIM
        IF(STAPIX(I).NE.ENDPIX(I))THEN
          J=J+1
C
C     - reshape OUTPUT axis
C
          CALL DSA_RESHAPE_AXIS('OUTPUT',J,'IMAGE',I,1,ODIMS(J),STATUS)
C
C     - map OUTPUT axis
C
          CALL DSA_MAP_AXIS_DATA
     &      ('OUTPUT',J,'WRITE','FLOAT',ADDRESS,OAXSLOT(J),STATUS)
          IF(STATUS.NE.0)GO TO 500
          OAXPTR(J)=DYN_ELEMENT(ADDRESS)
C
C     - copy values from corresponding IMAGE axis
C
          CALL SUBSET_NEWAXIS
     &      (DYNAMIC_MEM(AXPTR(I)),DYNAMIC_MEM(OAXPTR(J)),
     &       DIMS(I),ODIMS(J),STAPIX(I))
        END IF
      END DO
C
C     Magic values are not to be removed from the data arrays (except in
C     the presence of quality data)
C
      IF (.NOT.QUAL) THEN
        CALL DSA_USE_FLAGGED_VALUES('IMAGE',STATUS)
        IF(STATUS.NE.0)GO TO 500
        CALL DSA_USE_FLAGGED_VALUES('OUTPUT',STATUS)
        IF(STATUS.NE.0)GO TO 500
      END IF
C
C     Map IMAGE data array
C
      IF(TYPE.EQ.'SHORT')THEN
        CALL DSA_MAP_DATA('IMAGE','READ','SHORT',ADDRESS,ISLOT,STATUS)
      ELSE
        CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',ADDRESS,ISLOT,STATUS)
      END IF
      IF(STATUS.NE.0)GO TO 500
      IMPTR=DYN_ELEMENT(ADDRESS)
C
C     Map OUTPUT data array
C
      IF(TYPE.EQ.'SHORT')THEN
        CALL DSA_MAP_DATA('OUTPUT','WRITE','SHORT',ADDRESS,OSLOT,STATUS)
      ELSE
        CALL DSA_MAP_DATA('OUTPUT','WRITE','FLOAT',ADDRESS,OSLOT,STATUS)
      END IF
      IF(STATUS.NE.0)GO TO 500
      OUTPTR=DYN_ELEMENT(ADDRESS)
C
C     Map error arrays if present
C
      IF (ERR) THEN
        IF(TYPE.EQ.'SHORT')THEN
          CALL DSA_MAP_ERRORS('IMAGE','READ','SHORT',ADDRESS,
     &                        IESLOT,STATUS)
          IF (STATUS.NE.0) GO TO 500
          IEPTR=DYN_ELEMENT(ADDRESS)
          CALL DSA_MAP_ERRORS('OUTPUT','WRITE','SHORT',ADDRESS,
     &                        OESLOT,STATUS)
          IF (STATUS.NE.0) GO TO 500
          OEPTR=DYN_ELEMENT(ADDRESS)
        ELSE
          CALL DSA_MAP_ERRORS('IMAGE','READ','FLOAT',ADDRESS,
     &                        IESLOT,STATUS)
          IF (STATUS.NE.0) GO TO 500
          IEPTR=DYN_ELEMENT(ADDRESS)
          CALL DSA_MAP_ERRORS('OUTPUT','WRITE','FLOAT',ADDRESS,
     &                        OESLOT,STATUS)
          IF (STATUS.NE.0) GO TO 500
          OEPTR=DYN_ELEMENT(ADDRESS)
        END IF
      END IF
C
C     Map quality array if present
C
      IF (QUAL) THEN
        CALL DSA_MAP_QUALITY('IMAGE','READ','BYTE',ADDRESS,
     &                       IQSLOT,STATUS)
        IF (STATUS.NE.0) GO TO 500
        IQPTR=DYN_ELEMENT(ADDRESS)
        CALL DSA_MAP_QUALITY('OUTPUT','WRITE','BYTE',ADDRESS,
     &                       OQSLOT,STATUS)
        IF (STATUS.NE.0) GO TO 500
        OQPTR=DYN_ELEMENT(ADDRESS)
      END IF
C
C     Subset data array
C
      CALL DSA_WRUSER('%SUBSET-I-SUBDAT  ')
      CALL DSA_WRUSER('Subsetting data array\\N')
      IF(TYPE.EQ.'SHORT')THEN
        CALL SUBSET_DATA_W(DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &                     DIMS,NDIM,NELM,ODIMS,ONELM,STAPIX,ENDPIX)
        IF (ERR) THEN
         CALL DSA_WRUSER('%SUBSET-I-SUBERR  ')
          CALL DSA_WRUSER('Subsetting error array\\n')
          CALL SUBSET_DATA_W(DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR),
     &                     DIMS,NDIM,NELM,ODIMS,ONELM,STAPIX,ENDPIX)
        END IF ! (ERR)
      ELSE
        CALL SUBSET_DATA_R(DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &                     DIMS,NDIM,NELM,ODIMS,ONELM,STAPIX,ENDPIX)
        IF (ERR) THEN
          CALL DSA_WRUSER('%SUBSET-I-SUBERR  ')
          CALL DSA_WRUSER('Subsetting error array\\n')
          CALL SUBSET_DATA_R(DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR),
     &                     DIMS,NDIM,NELM,ODIMS,ONELM,STAPIX,ENDPIX)
        END IF ! (ERR)
      END IF ! (TYPE...)
      IF (QUAL) THEN
        CALL DSA_WRUSER('%SUBSET-I-SUBQUAL  ')
        CALL DSA_WRUSER('Subsetting quality array\\n')
        CALL SUBSET_DATA_B(DYNAMIC_MEM(IQPTR),DYNAMIC_MEM(OQPTR),
     &                     DIMS,NDIM,NELM,ODIMS,ONELM,STAPIX,ENDPIX)
      END IF
C
C     Tidy up and exit
C
500   CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END





      SUBROUTINE SUBSET_NEWAXIS(ARRAY,OARRAY,NELM,ONELM,STAPIX)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER   NELM,ONELM,STAPIX
      REAL      ARRAY(NELM),OARRAY(ONELM)
C
C     Local variables
C
      INTEGER   I            ! Loop counter
      INTEGER   J            ! Loop counter
C
      J=1
C
      DO I=STAPIX,STAPIX+ONELM-1
        OARRAY(J)=ARRAY(I)
        J=J+1
      END DO
C
      END





