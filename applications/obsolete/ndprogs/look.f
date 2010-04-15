      SUBROUTINE LOOK
C+
C
C   -------
C   L O O K
C   -------
C
C   Description
C   -----------
C   Displays pixel values of a 2-D image or image subset.
C
C
C   Scope of program
C   ----------------
C   - Only 2-D images accepted.
C   - Data array types SHORT and FLOAT supported; others converted to FLOAT.
C   - Subsetting supported.
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
C   IMAGE   Name of the structure containing the image. (character)
C           (prompted for).
C
C   STAPIX  Start pixel in each dimension of the subset to be inspected.
C          (real, array)(prompted for).
C
C   ENDPIX  End pixel in each dimension of the subset to be inspected.
C           (real, array)(prompted for).
C
C
C   Keywords
C   --------
C   AGAIN   Instruction to display another subset.
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
C   - The minimum and maximum values in the selected image subset are
C     obtained by calling the appropriate NDP_STATS routine. These are used
C     to determine the maximum string length required to write out a data
C     value.
C   - A subroutine appropriate to the data type is called to display the
C     data values. Dimension 2 of the array is accessed in reverse so that
C     the display appears with the correct orientation.
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
C
C   Library PAR:
C      PAR_CNPAR
C      PAR_RDARY
C      PAR_RDCHAR
C      PAR_RDKEY
C      PAR_WRUSER
C
C   Library NDP:
C      NDP_GET_IMAGE_INFO
C      NDP_PAR_RDARY
C      NDP_STATS_W
C      NDP_STATS_WQ
C      NDP_STATS_R
C      NDP_STATS_RQ
C
C
C   Internal subroutines called
C   ---------------------------
C   LOOK_DATA_W
C   LOOK_DATA_WQ
C   LOOK_DATA_R
C   LOOK_DATA_RQ
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
C   - Input the number of decimal places to be displayed. At present this is
C     hard-coded as 1.
C
C
C   Author/s
C   --------
C   Nick Fuller  RGO  (RGVAD::NMJF or NMJF@UK.AC.RGO.STAR)
C   Peter Allan  Manchester (MAVAD::PMA)
C   Julian Gold  RGO (CAVAD::GOLDJIL or GOLDJIL@UK.AC.CAM.AST-STAR)
C
C   History
C   -------
C   01-FEB-1989  - Original program
C   22-JUL-1990   - Modified to pass the sizes of adjustable arrays to
C                   subroutines as individual variables rather than as
C                   elements of arrays. This change was made necessary by
C                   the VAX Fortran 5.2 compiler. (PMA)
C   07-OCT-1991   - Processing of quality arrays added (GOLDJIL)
C   08-OCT-1991   - String buffer increased in size in LOOK_DATA_<T>
C                   to avoid string overflow (GOLDJIL)
C   02-DEC-1992   - Unix version (GOLDJIL)
C   06-OCT-1994   - Removed unused variables. (GJP)
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
      LOGICAL   QUAL                ! Never mind the width, feel the quality
      INTEGER   QPTR                ! Dynamic pointer to quality array
      INTEGER   QSLOT               ! Slot number for quality array
      REAL      SIGMA               ! Returned by NDP_STATS_*
      REAL      SIZE                ! Returned by NDP_STATS_*
      INTEGER   STAPIX(6)           ! Start pixel of subset
      INTEGER   STATUS              ! Status code
      REAL      TOTAL               ! Returned by NDP_STATS_*
      CHARACTER TYPE*8              ! IMAGE data array type
      REAL      VMAX(2)             ! Maximum values for NDP_PAR_RDARY
      REAL      VMIN(2)             ! Minimum values for NDP_PAR_RDARY
C
      INCLUDE 'DYNAMIC_MEMORY'
      INCLUDE 'MAGIC_VALUES'
      INCLUDE 'NUMERIC_RANGES'
C
C   Initialize
C
      STATUS=0
      AGAIN=.TRUE.
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
      CALL DSA_DATA_SIZE('IMAGE',2,NDIM,DIMS,NELM,STATUS)
      IF(NDIM.LT.2)THEN
        CALL DSA_WRUSER('This program handles 2-D images only.\\N')
        GO TO 500
      END IF
      DO I=1,NDIM
        VMIN(I)=1.0
        VMAX(I)=REAL(DIMS(I))
      END DO
C
C   Magic values are not to be removed from the data array
C
      CALL DSA_USE_FLAGGED_VALUES('IMAGE',STATUS)
      IF(STATUS.NE.0)GO TO 500
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
C   Any quality data present?
C
      CALL DSA_SEEK_QUALITY('IMAGE',QUAL,STATUS)
      IF (QUAL) THEN
        CALL DSA_MAP_QUALITY('IMAGE','READ','BYTE',ADDRESS,QSLOT,STATUS)
        QPTR=DYN_ELEMENT(ADDRESS)
      END IF
      IF (STATUS.NE.0) GO TO 500
C
C   Perform loop for an each subset
C
      DO WHILE (AGAIN)
C
C   - get start and end pixels
C
        CALL PAR_CNPAR('STAPIX')
        CALL PAR_CNPAR('ENDPIX')
        CALL NDP_PAR_RDARY('STAPIX',VMIN,VMAX,'N',' ',NDIM,2,DUMARR)
        DO I=1,NDIM
          STAPIX(I)=INT(DUMARR(I))
        END DO
        CALL NDP_PAR_RDARY('ENDPIX',VMIN,VMAX,'N',' ',NDIM,2,DUMARR)
        DO I=1,NDIM
          ENDPIX(I)=INT(DUMARR(I))
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
C   - display the image
C
        IF(TYPE.EQ.'SHORT')THEN
          IF(.NOT.BADPIX)THEN
            CALL LOOK_DATA_W(DYNAMIC_MEM(IMPTR),DIMS(1),DIMS(2),STAPIX,
     &                       ENDPIX,MAXVAL,MINVAL,
     &                       QUAL,DYNAMIC_MEM(QPTR))
          ELSE
            CALL LOOK_DATA_WQ(DYNAMIC_MEM(IMPTR),DIMS(1),DIMS(2),STAPIX,
     &                        ENDPIX,MAXVAL,MINVAL,MAGIC_SHORT)
          END IF
        ELSE
          IF(.NOT.BADPIX)THEN
            CALL LOOK_DATA_R(DYNAMIC_MEM(IMPTR),DIMS(1),DIMS(2),STAPIX,
     &                       ENDPIX,MAXVAL,MINVAL,
     &                       QUAL,DYNAMIC_MEM(QPTR))
          ELSE
            CALL LOOK_DATA_RQ(DYNAMIC_MEM(IMPTR),DIMS(1),DIMS(2),STAPIX,
     &                        ENDPIX,MAXVAL,MINVAL,MAGIC_FLOAT)
          END IF
        END IF
C
C   - ask whether another display is required
C
        CALL PAR_CNPAR('AGAIN')
   10   CALL PAR_RDKEY('AGAIN',.TRUE.,AGAIN)
      END DO
C
C   Tidy up and exit
C
500   CONTINUE
      CALL DSA_CLOSE(STATUS)
      END






