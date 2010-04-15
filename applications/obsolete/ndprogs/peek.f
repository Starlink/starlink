      SUBROUTINE PEEK
C+
C
C   -------
C   P E E K
C   -------
C
C   Description
C   -----------
C   Displays individual pixel values in an n-D image. If n = 1, 2 or 3,
C   the values of adjacent pixels are also displayed.
C
C
C   Scope of program
C   ----------------
C   - Images of up to six dimensions accepted.
C   - Data array types SHORT and FLOAT supported; others converted to FLOAT.
C   - Subsetting not supported since not relevant.
C   - Magic values supported.
C   - Variance arrays not supported.
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
C   IMAGE  Name of the structure containing the image. (character)
C          (prompted for)
C
C   PIXEL  Coordinates if the pixel to be inspected. (integer,array)
C          (prompted for)
C
C
C   Keywords
C   --------
C   AGAIN  Instruction to inspect another pixel.
C
C
C   Propagation of data structure
C   -----------------------------
C   Not relevant.
C
C
C   Method
C   ------
C   - The IMAGE structure is tested for the bad pixel flag. If it is found
C     and non-zero, magic values are assumed to be present and are left in
C     the data.
C   - An image subset is computed, consisting of the required pixel plus
C     its two neighbours in each dimension.
C   - The minimum and maximum values in the selected image subset are
C     obtained by calling the appropriate NDP_STATS routine. These are used
C     to determine the maximum string length required to write out a data
C     value.
C   - A subroutine appropriate to the data type is called to display the
C     pixel value. For dimensionalities of 1, 2, and 3 the neighbouring
C     values are also displayed. In such cases, dimensions above 1 are
C     accessed in reverse so that the display appears with the correct
C     orientation.
C
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library DSA:
C      DSA_CLOSE
C      DSA_DATA_SIZE
C      DSA_INPUT
C      DSA_MAP_DATA
C      DSA_MAP_QUALITY
C      DSA_OPEN
C      DSA_OUTPUT
C      DSA_SEEK_QUALITY
C      DSA_USE_FLAGGED_VALUES
C      DSA_WRUSER
C
C   Library DYN:
C      DYN_ELEMENT
C
C   Library ICH:
C      ICH_ENCODE
C      ICH_LEN
C
C   Library NDP:
C      NDP_GET_IMAGE_INFO
C      NDP_PAR_RDARY
C      NDP_STATS_W
C      NDP_STATS_WQ
C      NDP_STATS_R
C      NDP_STATS_RQ
C
C   Library PAR:
C      PAR_CNPAR
C      PAR_RDARY
C      PAR_RDCHAR
C      PAR_RDKEY
C      PAR_WRUSER
C
C
C   Internal subroutines called
C   ---------------------------
C   PEEK_DATA_W
C   PEEK_DATA_WQ
C   PEEK_DATA_R
C   PEEK_DATA_RQ
C
C
C   INCLUDE statements
C   ------------------
C   INCLUDE 'DYNAMIC_MEMORY'
C   INCLUDE 'MAGIC_VALUES'
C   INCLUDE 'NUMERIC_RANGES'
C   INCLUDE 'DCV_FUN'
C
C
C   Extensions to FORTRAN 77
C   ------------------------
C   DO WHILE / END DO / IMPLICIT NONE / INCLUDE / Names > 6 characters
C
C
C   Possible future upgrades
C   ------------------------
C   - Display adjacent pixel values for dimensionalities > 3 ?
C
C
C   Author/s
C   --------
C   Nick Fuller  RGO  (RGVAD::NMJF or NMJF@UK.AC.RGO.STAR)
C   Chris Benn   RGO  (RGVAD::CRB or CRB@UK.AC.RGO.STAR)
C   Julian Gold  RGO  (CAVAD::GOLDJIL or GOLDJIL@UK.AC.CAM.AST-STAR)
C
C   History
C   -------
C   01-FEB-1989  - Original program
C   07-Oct-1991  - Quality arrays now processed (GOLDJIL)
C   03-DEC-1992  - Unix version (GOLDJIL)
C   06-OCT-1994  - Removed unused variables. (GJP)
C
C+-----------------------------------------------------------------------------
C
      IMPLICIT NONE
C
C   Functions used
C
      INTEGER  DYN_ELEMENT
C
C   Local variables
C
      INTEGER   ADDRESS             ! Address of dynamic memory element
      LOGICAL   AGAIN               ! Instruction to inspect another pixel
      LOGICAL   BADPIX              ! Value of bad pixel flag
      INTEGER   DIMS(10)            ! Dimensions of IMAGE
      REAL      DUMARR(6)           ! REAL array dummy variable
      INTEGER   ENDPIX(6)           ! End pixel of subset
      INTEGER   I                   ! Loop counter
      INTEGER   IMPTR               ! Dynamic pointer to IMAGE data array
      INTEGER   ISLOT               ! Map slot number for IMAGE data
      REAL      MAGICPIX            ! Returned by NDP_STATS_*
      REAL      MAXPIX(6)           ! Returned by NDP_STATS_*
      REAL      MAXVAL              ! Returned by NDP_STATS_*
      REAL      MEAN                ! Returned by NDP_STATS_*
      REAL      MINPIX(6)           ! Returned by NDP_STATS_*
      REAL      MINVAL              ! Returned by NDP_STATS_*
      INTEGER   NDIM                ! Number of dimensions in IMAGE
      INTEGER   NELM                ! Number of elements in IMAGE
      INTEGER   PIXEL(6)            ! Coordinates of pixel
      REAL      PMAX(6)             ! Maximum pixel values
      REAL      PMIN(6)             ! Minimum pixel values
      LOGICAL   QUAL                ! Quality data present?
      INTEGER   QPTR                ! Dynamic pointer to quality
      INTEGER   QSLOT               ! Quality array slot number
      REAL      SIGMA               ! Returned by NDP_STATS_*
      REAL      SIZE                ! Returned by NDP_STATS_*
      INTEGER   STAPIX(6)           ! Start pixel of subset
      INTEGER   STATUS              ! Status code
      REAL      TOTAL               ! Returned by NDP_STATS_*
      CHARACTER TYPE*8              ! IMAGE data array type
C
      INCLUDE 'DYNAMIC_MEMORY'
      INCLUDE 'MAGIC_VALUES'
      INCLUDE 'NUMERIC_RANGES'
C
C   Initialize
C
      STATUS=0
      AGAIN=.TRUE.
      DO I=1,6
        PMIN(I)=1.0
      END DO
C
C   Open DSA system
C
      CALL DSA_OPEN(STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   Open file for IMAGE
C
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   Get information about IMAGE
C
      CALL NDP_GET_IMAGE_INFO('IMAGE',.TRUE.,.FALSE.,TYPE,BADPIX,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   Get dimensions of IMAGE data array
C
      CALL DSA_DATA_SIZE('IMAGE',6,NDIM,DIMS,NELM,STATUS)
      NELM=1
      DO I=1,NDIM
        NELM=NELM*DIMS(I)
        PMAX(I)=REAL(DIMS(I))
      END DO
C
C   Map IMAGE data array
C
      IF(TYPE.EQ.'SHORT')THEN
        CALL DSA_MAP_DATA ('IMAGE','READ','SHORT',ADDRESS,ISLOT,STATUS)
      ELSE
        CALL DSA_MAP_DATA ('IMAGE','READ','FLOAT',ADDRESS,ISLOT,STATUS)
      END IF
      IF(STATUS.NE.0)GO TO 500
      IMPTR=DYN_ELEMENT(ADDRESS)
C
C   Get and map quality information if necessary
C
      CALL DSA_SEEK_QUALITY('IMAGE',QUAL,STATUS)
      IF (QUAL) THEN
        CALL DSA_MAP_QUALITY('IMAGE','READ','BYTE',ADDRESS,QSLOT,STATUS)
        QPTR=DYN_ELEMENT(ADDRESS)
      ELSE
c
c   - use magic values
c
        IF (BADPIX) CALL DSA_USE_FLAGGED_VALUES('IMAGE',STATUS)
      END IF
      IF (STATUS.NE.0) GO TO 500
C
C   Perform loop for a set of pixel coordinates
C
      DO WHILE (AGAIN)
C
C   - get coordinates
C
        CALL PAR_CNPAR('PIXEL')
        CALL NDP_PAR_RDARY('PIXEL',PMIN,PMAX,'N',' ',NDIM,6,DUMARR)
        DO I=1,6
          PIXEL(I)=INT(DUMARR(I))
        END DO
C
C   - compute start and end pixels for statistics computation
C
        DO I=1,NDIM
          STAPIX(I)=MAX(PIXEL(I)-1,1)
          ENDPIX(I)=MIN(PIXEL(I)+1,DIMS(I))
        END DO
C
C   - get statistics of subset
C
        IF(TYPE.EQ.'SHORT')THEN
          IF(.NOT.BADPIX)THEN
            CALL NDP_STATS_W
     &        (DYNAMIC_MEM(IMPTR),DIMS,NDIM,NELM,STAPIX,ENDPIX,
     &         TOTAL,MAXVAL,MINVAL,MEAN,MAXPIX,MINPIX,
     &         SIGMA,SIZE,MAGIC_SHORT,QUAL,DYNAMIC_MEM(QPTR),
     &         MAGICPIX)
          ELSE
            CALL NDP_STATS_WQ
     &        (DYNAMIC_MEM(IMPTR),DIMS,NDIM,NELM,STAPIX,ENDPIX,
     &         TOTAL,MAXVAL,MINVAL,MEAN,MAXPIX,MINPIX,
     &         SIGMA,SIZE,MAGIC_SHORT,MAGICPIX)
          END IF
        ELSE
          IF(.NOT.BADPIX)THEN
            CALL NDP_STATS_R
     &        (DYNAMIC_MEM(IMPTR),DIMS,NDIM,NELM,STAPIX,ENDPIX,
     &         TOTAL,MAXVAL,MINVAL,MEAN,MAXPIX,MINPIX,
     &         SIGMA,SIZE,MAGIC_FLOAT,QUAL,DYNAMIC_MEM(QPTR),
     &         MAGICPIX)
          ELSE
            CALL NDP_STATS_RQ
     &        (DYNAMIC_MEM(IMPTR),DIMS,NDIM,NELM,STAPIX,ENDPIX,
     &         TOTAL,MAXVAL,MINVAL,MEAN,MAXPIX,MINPIX,
     &         SIGMA,SIZE,MAGIC_FLOAT,MAGICPIX)
          END IF
        END IF
C
C   - compute start and end pixels to be displayed
C
        DO I=1,NDIM
          STAPIX(I)=PIXEL(I)-1
          ENDPIX(I)=PIXEL(I)+1
        END DO
C
C   - display the required pixel and its neighbours
C
        IF(TYPE.EQ.'SHORT')THEN
          IF(.NOT.BADPIX)THEN
            CALL PEEK_DATA_W(DYNAMIC_MEM(IMPTR),DIMS,NDIM,NELM,
     &                       PIXEL,STAPIX,ENDPIX,MAXVAL,MINVAL,
     &                       QUAL,DYNAMIC_MEM(QPTR))
          ELSE
            CALL PEEK_DATA_WQ(DYNAMIC_MEM(IMPTR),DIMS,NDIM,NELM,
     &                        PIXEL,STAPIX,ENDPIX,MAXVAL,MINVAL,
     &                        MAGIC_SHORT)
          END IF
        ELSE
          IF(.NOT.BADPIX)THEN
            CALL PEEK_DATA_R(DYNAMIC_MEM(IMPTR),DIMS,NDIM,NELM,
     &                       PIXEL,STAPIX,ENDPIX,MAXVAL,MINVAL,
     &                       QUAL,DYNAMIC_MEM(QPTR))
          ELSE
            CALL PEEK_DATA_RQ(DYNAMIC_MEM(IMPTR),DIMS,NDIM,NELM,
     &                        PIXEL,STAPIX,ENDPIX,MAXVAL,MINVAL,
     &                        MAGIC_FLOAT)
          END IF
        END IF
C
C   - ask whether another inspection is required
C
        CALL PAR_CNPAR('AGAIN')
        CALL PAR_RDKEY('AGAIN',.TRUE.,AGAIN)
      END DO
C
C   Tidy up and exit
C
500   CONTINUE
      CALL DSA_CLOSE(STATUS)
      END






