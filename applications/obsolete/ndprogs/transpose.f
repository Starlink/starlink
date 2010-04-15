      SUBROUTINE TRANSPOSE
C+
C
C   -----------------
C   T R A N S P O S E
C   -----------------
C
C   Description
C   -----------
C   Transposes the dimensions of an image by resequencing the data array.
C
C   Scope of program
C   ----------------
C   - Currently handles 3-D images only.
C   - Data array types SHORT and FLOAT supported; others converted to FLOAT.
C   - Subsetting not supported.
C   - Magic value blanking not supported since not relevant.
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
C   IMAGE   Name of the structure containing the input image. (character)
C           (prompted for).
C
C   ORDER   Order of the axes in the output image, in terms of the input
C           axes. (integer, array)(prompted for).
C
C   OUTPUT  Name of the structure containing the output image. May be the
C           same as IMAGE. (character)(prompted for).
C
C   --------
C   Keywords
C   --------
C   None.
C
C
C   Propagation of data structure
C   -----------------------------
C   - All standard objects except the data array and axes are copied from
C     IMAGE.
C   - Data array and axes are modified and reshaped.
C
C
C   Method
C   ------
C   - The structure IMAGE is copied to OUTPUT, with the exception of the
C     data array and axes. DSA_RESHAPE_ routines are called to modify the
C     dimensions of the OUTPUT axis and data arrays.
C   - The calibrations, label, and units of the IMAGE axes are copied to the
C     appropriate OUTPUT axes.
C   - A subroutine appropriate to the dimensionality and data type is
C     called to transpose the IMAGE data array into the OUTPUT data array.
C
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library DSA:
C     DSA_CLOSE
C     DSA_DATA_SIZE
C     DSA_GET_AXIS_INFO
C     DSA_INPUT
C     DSA_MAP_AXIS_DATA
C     DSA_MAP_DATA
C     DSA_MAP_ERRORS
C     DSA_MAP_QUALITY
C     DSA_OPEN
C     DSA_OUTPUT
C     DSA_RESHAPE_AXIS
C     DSA_RESHAPE_DATA
C     DSA_SET_AXIS_INFO
C     DSA_TYPESIZE
C     DSA_SEEK_ERRORS
C     DSA_SEEK_QUALITY
C     DSA_USE_FLAGGED_VALUES
C     DSA_WRUSER
C
C   Library DYN:
C     DYN_ELEMENT
C
C   Library GEN:
C     GEN_MOVE
C
C   Library ICH:
C     ICH_ENCODE
C     ICH_LEN
C
C   Library NDP:
C     NDP_DISPLAY_PROGRESS
C     NDP_GET_IMAGE_INFO
C
C   Library PAR:
C     PAR_RDARY
C
C
C   Internal subroutines called
C   ---------------------------
C   TRANSPOSE_3D_W
C   TRANSPOSE_3D_R
C   TRANSPOSE_3D_B
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
C   DO WHILE / IMPLICIT NONE / INCLUDE / Names > 6 characters
C
C
C   Possible future upgrades
C   ------------------------
C   - Only 3-D images are handled at present. Subroutines to be written:
C       TRANSPOSE_2D_<T>
C       TRANSPOSE_4D_<T>
C       TRANSPOSE_5D_<T>
C       TRANSPOSE_6D_<T>
C   - More complex transposition strategies will be required for the higher
C     dimensionalities, possibly including multiple passes, dimensional
C     subsetting, and axis flipping. The AIPS program TRANS could be used as
C     a guide.
C
C
C   Author/s
C   --------
C   Nick Fuller  RGO  (RGVAD::NMJF or NMJF@UK.AC.RGO.STAR)
C   Jim Lewis    RGO  (CAVAD::JRL or JRL@UK.AC.CAM.AST-STAR)
C   Julian Gold  RGO  (CAVAD::GOLDJIL or GOLDJIL@UK.AC.CAM.AST-STAR)
C
C   History
C   -------
C   01-FEB-1989   - Original program
C   03-AUG-1990   - Fixed so that adjustable array sizes are not passed
C                   as elements of another array.  This practice has been
C                   banned by the v5.2 compiler.  (JRL)
C   12-NOV-1990   - Fixed bug in TRANSPOSE_3D_R which caused access violations
C                   when trying to transpose the whole array at once.  (JRL)
C   18-OCT-1991   - Quality and error array handling added.
C                   Subroutines written in GENERIC form.
C                   Fixed horrible bug that crashes program on certain
C                   permutations of indices when arrays aren't cubes.(GOLDJIL)
C   10-DEC-1992   - Unix version. (GOLDJIL)
C   06-OCT-1994   - Removed lots of unused variables. (GJP)
C
C
C+-----------------------------------------------------------------------------
C
      IMPLICIT NONE
C
C     Functions used.
C
      INTEGER  DSA_TYPESIZE,DYN_ELEMENT
C
C     Local variables.
C
      INTEGER   ADDRESS       ! Address of dynamic memory element
      CHARACTER AXINFO(2)*64  ! Axis label and units
      INTEGER   AXPTR(6)      ! Dynamic memory pointers to input axes
      INTEGER   AXSLOT(6)     ! Map slot numbers for input axes
      LOGICAL   BADPIX        ! Value of bad pixel flag
      INTEGER   DIMS(10)      ! Dimensions of input image
      REAL      DUMARR(6)     ! REAL array dummy variable
      INTEGER   DUMINT        ! INTEGER dummy variable
      INTEGER   ELEM          ! Element size in bytes
      LOGICAL   ERR           ! Flags presence of an error array
      INTEGER   I             ! Loop counter
      INTEGER   IEPTR         ! Dynamic pointer to image error array
      INTEGER   IESLOT        ! Map slot number for image error array
      INTEGER   IMPTR         ! Dynamic pointer to image data
      INTEGER   IQPTR         ! Dynamic pointer to image quality array
      INTEGER   IQSLOT        ! Map slot number for image quality array
      INTEGER   ISLOT         ! Map slot number for input data
      INTEGER   J             ! Loop counter
      INTEGER   NDIM          ! Dimensionality of image
      INTEGER   NELM          ! Number of elements in image
      INTEGER   OAXPTR(6)     ! Dynamic memory pointers to output axes
      INTEGER   OAXSLOT(6)    ! Map slot numbers for output axes
      INTEGER   ODIMS(10)     ! Dimensions of output image
      INTEGER   OEPTR         ! Dynamic pointer to output error array
      INTEGER   OESLOT        ! Map slot number for output error array
      INTEGER   OQPTR         ! Dynamic pointer to output quality array
      INTEGER   OQSLOT        ! Map slot number for output quality array
      INTEGER   ORDER(6)      ! Order of transposed axes in terms of input axes
      INTEGER   OSLOT         ! Map slot number for output data
      INTEGER   OUTPTR        ! Dynamic pointer to output image data
      LOGICAL   QUAL          ! Flags presence of a quality array
      INTEGER   STATUS        ! Status code
      CHARACTER TYPE*8        ! Data array type
      REAL      VMIN(6)       ! Minimum values of dimensions
      REAL      VMAX(6)       ! Maximum values of dimensions
C
      INTEGER   NEW_FILE,NO_DATA
      PARAMETER (NEW_FILE=1,NO_DATA=1)
C
      INCLUDE 'DYNAMIC_MEMORY'
      INCLUDE 'MAGIC_VALUES'
      INCLUDE 'NUMERIC_RANGES'
C
C     Initialize.
C
      STATUS=0
      DO I=1,6
        ORDER(I)=I
        VMIN(I)=1.0
      END DO
C
C     Open DSA system
C
      CALL DSA_OPEN(STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C     Open file for IMAGE.
C
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C     Get information about IMAGE.
C
      CALL NDP_GET_IMAGE_INFO('IMAGE',.TRUE.,.FALSE.,TYPE,BADPIX,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C     Get element size.
C
      ELEM=DSA_TYPESIZE(TYPE,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C     Get dimensions of IMAGE data array.
C
      CALL DSA_DATA_SIZE('IMAGE',3,NDIM,DIMS,NELM,STATUS)
      IF(STATUS.NE.0)GO TO 500
      IF(NDIM.LT.3)THEN
        CALL DSA_WRUSER
     &    ('This program handles 3-D images only.\\N')
        GO TO 500
      END IF
      DO I=1,NDIM
        VMAX(I)=REAL(NDIM)
      END DO
C
C     Get error/quality array information
C
      CALL DSA_SEEK_ERRORS('IMAGE',ERR,STATUS)
      IF(STATUS.NE.0)GO TO 500
      CALL DSA_SEEK_QUALITY('IMAGE',QUAL,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C     Get transposed axis order.
C
   10 CONTINUE
      CALL PAR_RDARY('ORDER',VMIN,VMAX,'N',' ',NDIM,6,DUMARR)
      DO I=1,NDIM
        ORDER(I)=INT(DUMARR(I))
        ODIMS(I)=DIMS(ORDER(I))
      END DO
C
C     - check that all axes are between 1 and NDIM.
C
      DO I=1,NDIM
        IF(ORDER(I).GT.NDIM)THEN
          CALL DSA_WRUSER('Non-existent axis specified.\\N')
          GO TO 10
        END IF
      END DO
C
C     - check that each axis is specified once only.
C
      DO I=1,NDIM
        DO J=1,NDIM
          IF(J.NE.I .AND. ORDER(J).EQ.ORDER(I))THEN
            CALL DSA_WRUSER('Specify each axis once only.\\N')
            GO TO 10
          END IF
        END DO
      END DO
C
C     - check that a transposition has been specified.
C
      DO I=1,6
        IF(ORDER(I).NE.I)GO TO 20
      END DO
      CALL DSA_WRUSER('No transposition specified.\\N')
      GO TO 500
C
C     Open file for OUTPUT.
C
20    CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',NO_DATA,NEW_FILE,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C     Reshape OUTPUT data array.
C
      CALL DSA_WRUSER('Reshaping data array...\\N')
      CALL DSA_RESHAPE_DATA('OUTPUT','IMAGE',NDIM,ODIMS,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C     Transpose the axes.
C     - map IMAGE axes.
C
      DO I=1,NDIM
        CALL DSA_MAP_AXIS_DATA
     &    ('IMAGE',I,'READ','FLOAT',ADDRESS,AXSLOT(I),STATUS)
        IF(STATUS.NE.0)GO TO 500
        AXPTR(I)=DYN_ELEMENT(ADDRESS)
      END DO
C
      DO I=1,NDIM
C
C    - reshape OUTPUT axis.
C
        CALL DSA_RESHAPE_AXIS('OUTPUT',I,'IMAGE',I,1,ODIMS(I),STATUS)
        IF(STATUS.NE.0)GO TO 500
C
C     - map OUTPUT axis.
C
        CALL DSA_MAP_AXIS_DATA
     &    ('OUTPUT',I,'WRITE','FLOAT',ADDRESS,OAXSLOT(I),STATUS)
        IF(STATUS.NE.0)GO TO 500
        OAXPTR(I)=DYN_ELEMENT(ADDRESS)
C
C     - copy values from appropriate IMAGE axis.
C
        ELEM=DSA_TYPESIZE('FLOAT',STATUS)
        CALL GEN_MOVE
     &    (DIMS(ORDER(I))*ELEM,DYNAMIC_MEM(AXPTR(ORDER(I))),
     &     DYNAMIC_MEM(OAXPTR(I)))
C
C     - transfer axis labels and units.
C
        CALL DSA_GET_AXIS_INFO
     &    ('IMAGE',ORDER(I),2,AXINFO,0,DUMINT,STATUS)
        CALL DSA_SET_AXIS_INFO('OUTPUT',I,2,AXINFO,0,DUMINT,STATUS)
      END DO
C
C     Magic values are not to be removed from the data arrays unless a
C     quality array is present
C
      IF (.NOT.QUAL) THEN
        CALL DSA_USE_FLAGGED_VALUES('IMAGE',STATUS)
        IF(STATUS.NE.0)GO TO 500
        CALL DSA_USE_FLAGGED_VALUES('OUTPUT',STATUS)
        IF(STATUS.NE.0)GO TO 500
      END IF
C
C     Map IMAGE and OUTPUT arrays
C
      IF(TYPE.EQ.'SHORT')THEN
        CALL DSA_MAP_DATA('IMAGE','READ','SHORT',ADDRESS,ISLOT,STATUS)
        IF(STATUS.NE.0)GO TO 500
        IMPTR=DYN_ELEMENT(ADDRESS)
c
        CALL DSA_MAP_DATA('OUTPUT','WRITE','SHORT',ADDRESS,OSLOT,STATUS)
        IF (STATUS.NE.0)GO TO 500
        OUTPTR=DYN_ELEMENT(ADDRESS)
c
        IF (ERR) THEN
          CALL DSA_MAP_ERRORS('IMAGE','READ','SHORT',ADDRESS,IESLOT,
     &                        STATUS)
          IF(STATUS.NE.0)GO TO 500
          IEPTR=DYN_ELEMENT(ADDRESS)
c
          CALL DSA_MAP_ERRORS('OUTPUT','WRITE','SHORT',ADDRESS,OESLOT,
     &                        STATUS)
          IF(STATUS.NE.0)GO TO 500
          OEPTR=DYN_ELEMENT(ADDRESS)
        END IF ! (ERR)
      ELSE
        CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',ADDRESS,ISLOT,STATUS)
        IF(STATUS.NE.0)GO TO 500
        IMPTR=DYN_ELEMENT(ADDRESS)
c
        CALL DSA_MAP_DATA('OUTPUT','WRITE','FLOAT',ADDRESS,OSLOT,STATUS)
        IF (STATUS.NE.0)GO TO 500
        OUTPTR=DYN_ELEMENT(ADDRESS)
c
        IF (ERR) THEN
          CALL DSA_MAP_ERRORS('IMAGE','READ','FLOAT',ADDRESS,IESLOT,
     &                        STATUS)
          IF(STATUS.NE.0)GO TO 500
          IEPTR=DYN_ELEMENT(ADDRESS)
c
          CALL DSA_MAP_ERRORS('OUTPUT','WRITE','FLOAT',ADDRESS,OESLOT,
     &                        STATUS)
          IF(STATUS.NE.0)GO TO 500
          OEPTR=DYN_ELEMENT(ADDRESS)
        END IF ! (ERR)
      END IF
      IF (QUAL) THEN
        CALL DSA_MAP_QUALITY('IMAGE','READ','BYTE',ADDRESS,IQSLOT,
     &                       STATUS)
        IF(STATUS.NE.0)GO TO 500
        IQPTR=DYN_ELEMENT(ADDRESS)
c
        CALL DSA_MAP_QUALITY('OUTPUT','WRITE','BYTE',ADDRESS,OQSLOT,
     &                       STATUS)
        IF(STATUS.NE.0)GO TO 500
        OQPTR=DYN_ELEMENT(ADDRESS)
      END IF
C
C     Transpose.
C
      CALL DSA_WRUSER('Transposing data array...\\N')
      IF(NDIM.EQ.3)THEN
        IF(TYPE.EQ.'SHORT')THEN
          CALL TRANSPOSE_3D_W
     &       (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &        DIMS(1),DIMS(2),DIMS(3),
     &        DIMS(ORDER(1)),DIMS(ORDER(2)),DIMS(ORDER(3)),
     &        ORDER)
          IF (ERR) THEN
            CALL DSA_WRUSER('Transposing error array...\\n')
            CALL TRANSPOSE_3D_W
     &       (DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR),
     &        DIMS(1),DIMS(2),DIMS(3),
     &        DIMS(ORDER(1)),DIMS(ORDER(2)),DIMS(ORDER(3)),
     &        ORDER)
          END IF ! (ERR)
        ELSE
          CALL TRANSPOSE_3D_R
     &       (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &        DIMS(1),DIMS(2),DIMS(3),
     &        DIMS(ORDER(1)),DIMS(ORDER(2)),DIMS(ORDER(3)),
     &        ORDER)
          IF (ERR) THEN
            CALL DSA_WRUSER('Transposing error array...\\n')
            CALL TRANSPOSE_3D_R
     &       (DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR),
     &        DIMS(1),DIMS(2),DIMS(3),
     &        DIMS(ORDER(1)),DIMS(ORDER(2)),DIMS(ORDER(3)),
     &        ORDER)
          END IF ! (ERR)
        END IF
        IF (QUAL) THEN
          CALL DSA_WRUSER('Transposing quality array...\\n')
          CALL TRANSPOSE_3D_B
     &       (DYNAMIC_MEM(IQPTR),DYNAMIC_MEM(OQPTR),
     &        DIMS(1),DIMS(2),DIMS(3),
     &        DIMS(ORDER(1)),DIMS(ORDER(2)),DIMS(ORDER(3)),
     &        ORDER)
        END IF ! (QUAL)
      END IF
C
C  Make sure bad pixel flag is correct
C
      CALL DSA_SET_FLAGGED_VALUES('OUTPUT',BADPIX,STATUS)
C
C     Tidy up and exit.
C
500   CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END
