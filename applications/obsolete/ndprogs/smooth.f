      SUBROUTINE SMOOTH
C+
C
C   -----------
C   S M O O T H
C   -----------
C
C   Description
C   -----------
C   Smooths an image in one, two, or three dimensions by convolution
C   with a top hat, Gaussian, or sinc function.
C
C
C   Scope of program
C   ----------------
C   - Images of up to six dimensions accepted.
C   - Data array types SHORT and FLOAT supported; others converted to FLOAT.
C   - Subsetting supported.
C   - Magic values supported.
C   - Quality and variance arrays supported.
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
C   IMAGE   Name of the structure containing the image to be smoothed.
C           (character)(prompted for).
C
C   START   Coordinate in each dimension of IMAGE at which the smooth
C           operation is to start. (real, array)(prompted for).
C
C   END     Coordinate in each dimension of IMAGE at which the smooth
C           operation is to end. (real, array)(prompted for).
C
C   SMOOTH  Smoothing function. (integer)(prompted for).
C           1 = top hat
C           2 = Gaussian
C           3 = sinc
C
C   FNDIM   Number of dimensions in the smoothing function, less than or
C           equal to the number in the image, up to a maximum of three.
C           (integer)(prompted for).
C
C   BOX     Size of the function box in pixels. (integer)(prompted for).
C
C   WIDTHS  Width of the function in each dimension. This is either the sigma
C           in a Gaussian function, or the constant k in the sinc function
C           sin(kx)/kx. (real, array)(prompted for).
C
C   OUTPUT  Name of the structure containing the output image. May be the
C           same as IMAGE. (character)(prompted for).
C
C   Keywords
C   --------
C   WHOLE   Instructs the program to smooth the whole image. Otherwise, a
C           subset of the image may be selected.
C
C
C   Propagation of data structure
C   -----------------------------
C   - Principal structure is IMAGE.
C   - All standard objects are copied from IMAGE.
C   - Data array is modified.
C
C
C   Method
C   ------
C   - The IMAGE structure is tested for the bad pixel flag. If it is found
C     and non-zero, magic values are assumed to be present and are left in
C     the data. Quality and error arrays are tested for.
C   - The dimensionality of the smoothing function may be less than or equal
C     to that of the image unless the image has more than three dimensions,
C     in which case the function has the maximum dimensionality of three.
C   - The size of the smoothing function BOX is restricted to odd numbers in
C     order to simplify the convolution.
C   - The structure IMAGE is copied to OUTPUT.
C   - The smoothing function of the required dimensionality is generated.
C   - A subroutine appropriate to the data type, dimensionality, and
C     presence or absence of magic values is called to convolve the data
C     array with the smoothing function.
C
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library DSA:
C     DSA_CLOSE
C     DSA_INPUT
C     DSA_DATA_SIZE
C     DSA_GET_WORKSPACE
C     DSA_MAP_DATA
C     DSA_OPEN
C     DSA_OUTPUT
C     DSA_TYPESIZE
C     DSA_USE_FLAGGED_VALUES
C     DSA_WRUSER
C
C   Library DYN:
C     DYN_ELEMENT
C
C   Library ICH:
C     ICH_ENCODE
C     ICH_LEN
C
C   Library NDP:
C     NDP_AXIS_RANGE
C     NDP_DISPLAY_PROGRESS
C     NDP_GET_IMAGE_INFO
C     NDP_PAR_RDARY
C
C   Library PAR:
C     PAR_CNPAR
C     PAR_RDCHAR
C     PAR_RDKEY
C     PAR_RDVAL
C
C
C   Internal subroutines called
C   ---------------------------
C   SMOOTH_1DFUNC
C   SMOOTH_1D1D_W
C   SMOOTH_1D1D_WQ
C   SMOOTH_1D1D_R
C   SMOOTH_1D1D_RQ
C   SMOOTH_2DFUNC
C   SMOOTH_2D1D_W
C   SMOOTH_2D1D_WQ
C   SMOOTH_2D1D_R
C   SMOOTH_2D1D_RQ
C   SMOOTH_2D2D_W
C   SMOOTH_2D2D_WQ
C   SMOOTH_2D2D_R
C   SMOOTH_2D2D_RQ
C   SMOOTH_3DFUNC
C   SMOOTH_3D1D_W
C   SMOOTH_3D1D_WQ
C   SMOOTH_3D1D_R
C   SMOOTH_3D1D_RQ
C   SMOOTH_3D2D_W
C   SMOOTH_3D2D_WQ
C   SMOOTH_3D2D_R
C   SMOOTH_3D2D_RQ
C   SMOOTH_3D3D_W
C   SMOOTH_3D3D_WQ
C   SMOOTH_3D3D_R
C   SMOOTH_3D3D_RQ
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
C   - Support higher dimensionalities. Subroutines to be written:
C       SMOOTH_sDFUNC
C       SMOOTH_nDsD_<T>   n = dimensionality of image
C       SMOOTH_nDsD_IQ    s = dimensionality of smoothing function
C                         and  s .le. n
C
C
C   Author/s
C   --------
C   Nick Fuller  RGO  (RGVAD::NMJF or NMJF@UK.AC.RGO.STAR)
C   Jim Lewis    RGO  (CAVAD::JRL or JRL@UK.AC.CAM.AST-STAR)
C   Julian Gold  RGO  (CAVAD::GOLDJIL or GOLDJIL@UK.AC.CAM.AST-STAR)
C
C
C   History
C   -------
C   01-FEB-1989   - Original program
C   03-AUG-1990   - Fixed so that adjustable array sizes are not passed
C                   as elements of another array.  This practice has been
C                   banned by the v5.2 compiler.  (JRL)
C   15-JAN-1991   - Fixed subroutines which generate smoothing functions to
C                   use the correct form for a gaussian. (JRL)
C   22-OCT-1991   - Quality and error arrays implemented, code written in
C                   GENERIC form. (GOLDJIL)
C   07-DEC-1992   - Unix version (GOLDJIL)
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
      INTEGER   ADDRESS            ! Address of dynamic memory element
      LOGICAL   BADPIX             ! Value of bad pixel flag
      INTEGER   BOX                ! Size of function box in pixels
      LOGICAL   DIMOK              ! Flag to verify suitable dimensions
      INTEGER   BOXMAX             ! Maximum possible size of function box
      INTEGER   DIMS(10)           ! Dimensions of IMAGE
      REAL      DUMREAL            ! REAL dummy variable
      INTEGER   ELEM               ! Element size in bytes
      REAL      END(6)             ! End coordinates of subset
      INTEGER   ENDPIX(6)          ! End pixel of subset
      LOGICAL   ERR                ! Flags presence of error arrays
      INTEGER   FNDIM              ! Dimensionality of function
      INTEGER   FTYPE              ! Type of function
      REAL      WIDTH(3)           ! Function width in each dimension
      INTEGER   I                  ! Loop counter
      INTEGER   IEPTR              ! Dynamic pointer to IMAGE error array
      INTEGER   IESLOT             ! Map slot number for IMAGE error array
      INTEGER   IMPTR              ! Dynamic pointer to IMAGE data array
      INTEGER   IQPTR              ! Dynamic pointer to IMAGE quality array
      INTEGER   IQSLOT             ! Map slot number for IMAGE quality array
      INTEGER   ISLOT              ! Map slot number for IMAGE data
      INTEGER   NDIM               ! Number of dimensions in IMAGE
      INTEGER   NELM               ! Number of elements in IMAGE
      INTEGER   OEPTR              ! Dynamic pointer to OUTPUT error array
      INTEGER   OESLOT             ! Map slot number for OUTPUT error array
      INTEGER   OQPTR              ! Dynamic pointer to OUTPUT quality array
      INTEGER   OQSLOT             ! Map slot number for OUTPUT quality array
      INTEGER   OSLOT              ! Map slot number for output data
      INTEGER   OUTPTR             ! Dynamic pointer to output data
      LOGICAL   QUAL               ! Flags presence of quality arrays
      INTEGER   STAPIX(6)          ! Start pixel of subset
      REAL      START(6)           ! Start coordinate of subset
      INTEGER   STATUS             ! Status code
      CHARACTER TYPE*8             ! IMAGE data array type
      REAL      VMAX(3)            ! Maximum parameter values
      REAL      VMIN(3)            ! Minimum parameter values
      INTEGER   WKBYTES            ! Number of bytes in workspace
      INTEGER   WKSLOT             ! Map slot number for workspace
      INTEGER   WKPTR              ! Dynamic pointer to workspace
C
      INTEGER   NEW_FILE,NO_DATA
      PARAMETER (NEW_FILE=1,NO_DATA=0)
C
      INCLUDE 'DYNAMIC_MEMORY'
      INCLUDE 'MAGIC_VALUES'
      INCLUDE 'NUMERIC_RANGES'
C
C     Initialize.
C
      STATUS=0
C
C     Open DSA system.
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
      CALL NDP_GET_IMAGE_INFO('IMAGE',.TRUE.,.TRUE.,TYPE,BADPIX,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C     Get dimensions of IMAGE data array.
C
      CALL DSA_DATA_SIZE('IMAGE',6,NDIM,DIMS,NELM,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C     Get IMAGE axis range.
C
      CALL NDP_AXIS_RANGE
     &  ('IMAGE',DIMS,NDIM,START,END,STAPIX,ENDPIX,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C     Find out about error and quality arrays
C
      CALL DSA_SEEK_ERRORS('IMAGE',ERR,STATUS)
      IF (STATUS.NE.0) GO TO 500
      CALL DSA_SEEK_QUALITY('IMAGE',ERR,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get dimensionality of function.
C
      CALL PAR_RDVAL
     &  ('FNDIM',1.0,REAL(MIN(NDIM,3)),REAL(MIN(NDIM,3)),' ',DUMREAL)
      FNDIM=INT(DUMREAL)
C
C     Get smoothing function type.
C
      DIMOK = .FALSE.
      DO WHILE (.NOT. DIMOK)
        CALL PAR_RDVAL('SMOOTH',1.0,4.0,1.0,' ',DUMREAL)
        FTYPE=INT(DUMREAL)
        IF ((FTYPE .EQ. 4).AND.(FNDIM .NE. 1)) THEN
          CALL DSA_WRUSER('%SMOOTH-E-FTYPE  ')
          CALL DSA_WRUSER('The Moffat smoothing function is ')
          CALL DSA_WRUSER('for 1-D only\\n')
          CALL PAR_CNPAR('SMOOTH')
        ELSE
          DIMOK = .TRUE.
        END IF
      END DO

C
C     Get size of function box - must be an odd number.
C
      BOXMAX=DIMS(1)
      IF(NDIM.GE.2)BOXMAX=MIN(BOXMAX,DIMS(2))
      IF(NDIM.GE.3)BOXMAX=MIN(BOXMAX,DIMS(3))
   10 CONTINUE
      CALL PAR_CNPAR('BOX')
      CALL PAR_RDVAL('BOX',3.0,REAL(BOXMAX),3.0,' ',DUMREAL)
      IF(DUMREAL/2.0 .EQ. REAL(INT(DUMREAL/2)))THEN
        CALL DSA_WRUSER('BOX must be an odd integer.\\N')
        GO TO 10
      END IF
      BOX=INT(DUMREAL)
C
C     Get width of function in each dimension. This is the dispersion
C     for a Gaussian function or the constant k in the sinc function sin(kx)/kx.
C
      IF(FTYPE.EQ.2 .OR. FTYPE.EQ.3)THEN
        DO I=1,3
          VMIN(I)=0.0
          VMAX(I)=REAL(BOX)
        END DO
        CALL NDP_PAR_RDARY('WIDTHS',VMIN,VMAX,'N',' ',FNDIM,3,WIDTH)
      END IF
C
C     Get workspace for function box.
C
      ELEM=DSA_TYPESIZE('FLOAT',STATUS)
      WKBYTES=(BOX**FNDIM)*ELEM
      CALL DSA_GET_WORKSPACE(WKBYTES,ADDRESS,WKSLOT,STATUS)
      IF(STATUS.NE.0)GO TO 500
      WKPTR=DYN_ELEMENT(ADDRESS)
C
C     Open file for OUTPUT.
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',NO_DATA,NEW_FILE,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C     If no quality array is present, magic values are not to be removed from
C     the data arrays
C
      IF (.NOT.QUAL) THEN
        CALL DSA_USE_FLAGGED_VALUES('IMAGE',STATUS)
        IF(STATUS.NE.0)GO TO 500
        CALL DSA_USE_FLAGGED_VALUES('OUTPUT',STATUS)
        IF(STATUS.NE.0)GO TO 500
      END IF
C
C     Map data arrays, and error/quality arrays if present
C
      IF(TYPE.EQ.'SHORT')THEN
        CALL DSA_MAP_DATA('IMAGE','READ','SHORT',ADDRESS,ISLOT,STATUS)
        IF(STATUS.NE.0)GO TO 500
        IMPTR=DYN_ELEMENT(ADDRESS)
        CALL DSA_MAP_DATA('OUTPUT','WRITE','SHORT',ADDRESS,OSLOT,STATUS)
        IF(STATUS.NE.0)GO TO 500
        OUTPTR=DYN_ELEMENT(ADDRESS)
        IF (ERR) THEN
          CALL DSA_MAP_ERRORS('IMAGE','READ','SHORT',
     &                        ADDRESS,IESLOT,STATUS)
          IF(STATUS.NE.0)GO TO 500
          IEPTR=DYN_ELEMENT(ADDRESS)
          CALL DSA_MAP_ERRORS('OUTPUT','WRITE','SHORT',
     &                        ADDRESS,OESLOT,STATUS)
          IF(STATUS.NE.0)GO TO 500
          OEPTR=DYN_ELEMENT(ADDRESS)
        END IF
      ELSE
        CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',ADDRESS,ISLOT,STATUS)
        IMPTR=DYN_ELEMENT(ADDRESS)
        CALL DSA_MAP_DATA('OUTPUT','WRITE','FLOAT',ADDRESS,OSLOT,STATUS)
        IF(STATUS.NE.0)GO TO 500
        OUTPTR=DYN_ELEMENT(ADDRESS)
        IF (ERR) THEN
          CALL DSA_MAP_ERRORS('IMAGE','READ','FLOAT',
     &                        ADDRESS,IESLOT,STATUS)
          IF(STATUS.NE.0)GO TO 500
          IEPTR=DYN_ELEMENT(ADDRESS)
          CALL DSA_MAP_ERRORS('OUTPUT','WRITE','FLOAT',
     &                        ADDRESS,OESLOT,STATUS)
          IF(STATUS.NE.0)GO TO 500
          OEPTR=DYN_ELEMENT(ADDRESS)
        END IF
      END IF
      IF (QUAL) THEN
        CALL DSA_MAP_QUALITY('IMAGE','READ','BYTE',
     &                       ADDRESS,IQSLOT,STATUS)
        IF (STATUS.NE.0) GO TO 500
        IQPTR=DYN_ELEMENT(ADDRESS)
        CALL DSA_MAP_QUALITY('OUTPUT','WRITE','BYTE',
     &                       ADDRESS,OQSLOT,STATUS)
        IF (STATUS.NE.0) GO TO 500
        OQPTR=DYN_ELEMENT(ADDRESS)
      END IF
C
C     Call routine appropriate to the dimensionalities of the image and
C     smoothing function.
C
      CALL DSA_WRUSER('Smoothing...\\N')
C
      IF(NDIM.EQ.1)THEN
        IF(TYPE.EQ.'SHORT')THEN
          IF(.NOT.BADPIX)THEN
            CALL SMOOTH_1D1D_W
     &        (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &         DYNAMIC_MEM(WKPTR),DIMS(1),STAPIX,ENDPIX,
     &         FTYPE,BOX,WIDTH,
     &         ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR),
     &         QUAL,DYNAMIC_MEM(IQPTR),DYNAMIC_MEM(OQPTR))
          ELSE
            CALL SMOOTH_1D1D_WQ
     &        (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &         DYNAMIC_MEM(WKPTR),DIMS(1),STAPIX,ENDPIX,
     &         FTYPE,BOX,WIDTH,MAGIC_SHORT,
     &         ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR))
          END IF
        ELSE
          IF(.NOT.BADPIX)THEN
            CALL SMOOTH_1D1D_R
     &        (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &         DYNAMIC_MEM(WKPTR),DIMS(1),STAPIX,ENDPIX,
     &         FTYPE,BOX,WIDTH,
     &         ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR),
     &         QUAL,DYNAMIC_MEM(IQPTR),DYNAMIC_MEM(OQPTR))
          ELSE
            CALL SMOOTH_1D1D_RQ
     &        (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &         DYNAMIC_MEM(WKPTR),DIMS(1),STAPIX,ENDPIX,
     &         FTYPE,BOX,WIDTH,MAGIC_FLOAT,
     &         ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR))
          END IF
        END IF
      ELSE IF(NDIM.EQ.2)THEN
        IF(FNDIM.EQ.1)THEN
          IF(TYPE.EQ.'SHORT')THEN
            IF(.NOT.BADPIX)THEN
              CALL SMOOTH_2D1D_W
     &          (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &           DYNAMIC_MEM(WKPTR),DIMS(1),DIMS(2),STAPIX,ENDPIX,
     &           FTYPE,BOX,WIDTH,
     &           ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR),
     &           QUAL,DYNAMIC_MEM(IQPTR),DYNAMIC_MEM(OQPTR))
            ELSE
              CALL SMOOTH_2D1D_WQ
     &          (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &           DYNAMIC_MEM(WKPTR),DIMS(1),DIMS(2),STAPIX,ENDPIX,
     &           FTYPE,BOX,WIDTH,MAGIC_SHORT,
     &           ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR))
            END IF
          ELSE
            IF(.NOT.BADPIX)THEN
              CALL SMOOTH_2D1D_R
     &          (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &           DYNAMIC_MEM(WKPTR),DIMS(1),DIMS(2),STAPIX,ENDPIX,
     &           FTYPE,BOX,WIDTH,
     &           ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR),
     &           QUAL,DYNAMIC_MEM(IQPTR),DYNAMIC_MEM(OQPTR))
            ELSE
              CALL SMOOTH_2D1D_RQ
     &          (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &           DYNAMIC_MEM(WKPTR),DIMS(1),DIMS(2),STAPIX,ENDPIX,
     &           FTYPE,BOX,WIDTH,MAGIC_FLOAT,
     &           ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR))
            END IF
          END IF
        ELSE IF(FNDIM.EQ.2)THEN
          IF(TYPE.EQ.'SHORT')THEN
            IF(.NOT.BADPIX)THEN
              CALL SMOOTH_2D2D_W
     &          (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &           DYNAMIC_MEM(WKPTR),DIMS(1),DIMS(2),STAPIX,ENDPIX,
     &           FTYPE,BOX,WIDTH,
     &           ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR),
     &           QUAL,DYNAMIC_MEM(IQPTR),DYNAMIC_MEM(OQPTR))
            ELSE
              CALL SMOOTH_2D2D_WQ
     &          (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &           DYNAMIC_MEM(WKPTR),DIMS(1),DIMS(2),STAPIX,ENDPIX,
     &           FTYPE,BOX,WIDTH,MAGIC_SHORT,
     &           ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR))
            END IF
          ELSE
            IF(.NOT.BADPIX)THEN
              CALL SMOOTH_2D2D_R
     &          (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &           DYNAMIC_MEM(WKPTR),DIMS(1),DIMS(2),STAPIX,ENDPIX,
     &           FTYPE,BOX,WIDTH,
     &           ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR),
     &           QUAL,DYNAMIC_MEM(IQPTR),DYNAMIC_MEM(OQPTR))
            ELSE
              CALL SMOOTH_2D2D_RQ
     &          (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &           DYNAMIC_MEM(WKPTR),DIMS(1),DIMS(2),STAPIX,ENDPIX,
     &           FTYPE,BOX,WIDTH,MAGIC_FLOAT,
     &           ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR))
            END IF
          END IF
        END IF
      ELSE IF(NDIM.EQ.3)THEN
        IF(FNDIM.EQ.1)THEN
          IF(TYPE.EQ.'SHORT')THEN
            IF(.NOT.BADPIX)THEN
              CALL SMOOTH_3D1D_W
     &          (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &           DYNAMIC_MEM(WKPTR),DIMS(1),DIMS(2),DIMS(3),
     &           STAPIX,ENDPIX,FTYPE,BOX,WIDTH,
     &           ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR),
     &           QUAL,DYNAMIC_MEM(IQPTR),DYNAMIC_MEM(OQPTR))
            ELSE
              CALL SMOOTH_3D1D_WQ
     &          (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &           DYNAMIC_MEM(WKPTR),DIMS(1),DIMS(2),DIMS(3),
     &           STAPIX,ENDPIX,FTYPE,BOX,WIDTH,MAGIC_SHORT,
     &           ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR))
            END IF
          ELSE
            IF(.NOT.BADPIX)THEN
              CALL SMOOTH_3D1D_R
     &          (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &           DYNAMIC_MEM(WKPTR),DIMS(1),DIMS(2),DIMS(3),
     &           STAPIX,ENDPIX,FTYPE,BOX,WIDTH,
     &           ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR),
     &           QUAL,DYNAMIC_MEM(IQPTR),DYNAMIC_MEM(OQPTR))
            ELSE
              CALL SMOOTH_3D1D_RQ
     &          (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &           DYNAMIC_MEM(WKPTR),DIMS(1),DIMS(2),DIMS(3),
     &           STAPIX,ENDPIX,FTYPE,BOX,WIDTH,MAGIC_FLOAT,
     &           ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR))
            END IF
          END IF
        ELSE IF(FNDIM.EQ.2)THEN
          IF(TYPE.EQ.'SHORT')THEN
            IF(.NOT.BADPIX)THEN
              CALL SMOOTH_3D2D_W
     &          (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &           DYNAMIC_MEM(WKPTR),DIMS(1),DIMS(2),DIMS(3),
     &           STAPIX,ENDPIX,FTYPE,BOX,WIDTH,
     &           ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR),
     &           QUAL,DYNAMIC_MEM(IQPTR),DYNAMIC_MEM(OQPTR))
            ELSE
              CALL SMOOTH_3D2D_WQ
     &          (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &           DYNAMIC_MEM(WKPTR),DIMS(1),DIMS(2),DIMS(3),
     &           STAPIX,ENDPIX,FTYPE,BOX,WIDTH,MAGIC_SHORT,
     &           ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR))
            END IF
          ELSE
            IF(.NOT.BADPIX)THEN
              CALL SMOOTH_3D2D_R
     &          (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &           DYNAMIC_MEM(WKPTR),DIMS(1),DIMS(2),DIMS(3),
     &           STAPIX,ENDPIX,FTYPE,BOX,WIDTH,
     &           ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR),
     &           QUAL,DYNAMIC_MEM(IQPTR),DYNAMIC_MEM(OQPTR))
            ELSE
              CALL SMOOTH_3D2D_RQ
     &          (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &           DYNAMIC_MEM(WKPTR),DIMS(1),DIMS(2),DIMS(3),
     &           STAPIX,ENDPIX,FTYPE,BOX,WIDTH,MAGIC_FLOAT,
     &           ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR))
            END IF
          END IF
        ELSE IF(FNDIM.EQ.3)THEN
          IF(TYPE.EQ.'SHORT')THEN
            IF(.NOT.BADPIX)THEN
              CALL SMOOTH_3D3D_W
     &          (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &           DYNAMIC_MEM(WKPTR),DIMS(1),DIMS(2),DIMS(3),
     &           STAPIX,ENDPIX,FTYPE,BOX,WIDTH,
     &           ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR),
     &           QUAL,DYNAMIC_MEM(IQPTR),DYNAMIC_MEM(OQPTR))
            ELSE
              CALL SMOOTH_3D3D_WQ
     &          (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &           DYNAMIC_MEM(WKPTR),DIMS(1),DIMS(2),DIMS(3),
     &           STAPIX,ENDPIX,FTYPE,BOX,WIDTH,MAGIC_SHORT,
     &           ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR))
            END IF
          ELSE
            IF(.NOT.BADPIX)THEN
              CALL SMOOTH_3D3D_R
     &          (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &           DYNAMIC_MEM(WKPTR),DIMS(1),DIMS(2),DIMS(3),
     &           STAPIX,ENDPIX,FTYPE,BOX,WIDTH,
     &           ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR),
     &           QUAL,DYNAMIC_MEM(IQPTR),DYNAMIC_MEM(OQPTR))
            ELSE
              CALL SMOOTH_3D3D_RQ
     &          (DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR),
     &           DYNAMIC_MEM(WKPTR),DIMS(1),DIMS(2),DIMS(3),
     &           STAPIX,ENDPIX,FTYPE,BOX,WIDTH,MAGIC_FLOAT,
     &           ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR))
            END IF
          END IF
        END IF
      END IF
C
C     Tidy up and exit.
C
  500 CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END





      SUBROUTINE SMOOTH_1DFUNC(FTYPE,BOX,WIDTH,FUNC)
C
      IMPLICIT NONE
C
C     Parameters.
C
      INTEGER FTYPE,BOX
      REAL    WIDTH(3),FUNC(BOX)
C
C     Local variables.
C
      REAL      A
      REAL      C
      INTEGER   I
      REAL      R
      REAL      X
C
C     Generate 1-D function box.
C     - top hat.
C
      IF(FTYPE.EQ.1)THEN
        DO I=1,BOX
          FUNC(I)=1.0
        END DO
C
C     - Gaussian.
C
      ELSE IF(FTYPE.EQ.2)THEN
        A=1.0/(2.0*WIDTH(1)*WIDTH(1))
        C=REAL(BOX+1)/2.0
        DO I=1,BOX
          X=REAL(I)
          R=(X-C)*(X-C)
          FUNC(I)=EXP(-A*R)
        END DO
C
C     - sinc.
C
      ELSE IF(FTYPE.EQ.3)THEN
        C=REAL(BOX+1)/2.0
        DO I=1,BOX
          X=WIDTH(1)*(REAL(I)-C)
          IF(ABS(X).GE.1.0)THEN
            FUNC(I)=SIN(X)/X
          ELSE
            FUNC(I)=1.0
          END IF
        END DO
C
C     - Moffat
C
      ELSE IF (FTYPE .EQ. 4) THEN
        C = REAL(BOX+1)/2.0
        DO I = 1,BOX
          A = WIDTH(1)/1.02
          X = (REAL(I)-C)/A
          FUNC(I) = 1.0 / ((1.0+X*X)**3)
        END DO
      END IF
c     if(box.le.11)call smooth_func(box,func,1)
C
      END





      SUBROUTINE SMOOTH_2DFUNC(FTYPE,BOX,WIDTH,FUNC)
C
      IMPLICIT NONE
C
C     Parameters.
C
      INTEGER FTYPE,BOX
      REAL    WIDTH(3),FUNC(BOX,BOX)
C
C     Local variables.
C
      REAL      A1
      REAL      A2
      REAL      C
      INTEGER   I
      INTEGER   J
      REAL      X
      REAL      XARG
      REAL      Y
      REAL      YARG
C
C     Generate 2-D function box.
C     - top hat.
C
      IF(FTYPE.EQ.1)THEN
        DO J=1,BOX
          DO I=1,BOX
            FUNC(I,J)=1.0
          END DO
        END DO
C
C     - Gaussian.
C
      ELSE IF(FTYPE.EQ.2)THEN
        A1=1.0/(2.0*WIDTH(1)*WIDTH(1))
        A2=1.0/(2.0*WIDTH(2)*WIDTH(2))
        C=REAL(BOX+1)/2.0
        DO J=1,BOX
          Y=REAL(J)
          YARG = (Y-C)*(Y-C)*A2
          DO I=1,BOX
            X=REAL(I)
            XARG = (X-C)*(X-C)*A1
            FUNC(I,J)=EXP(-(XARG+YARG))
          END DO
        END DO
C
C     - sinc.
C
      ELSE IF(FTYPE.EQ.3)THEN
        C=REAL(BOX+1)/2.0
        DO J=1,BOX
          Y=WIDTH(2)*(REAL(J)-C)
          IF(ABS(Y).GE.1.0)THEN
            A2=SIN(Y)/Y
          ELSE
            A2=1.0
          END IF
          DO I=1,BOX
            X=WIDTH(1)*(REAL(I)-C)
            IF(ABS(X).GE.1.0)THEN
              A1=SIN(X)/X
            ELSE
              A1=1.0
            END IF
            FUNC(I,J)=A1*A2
          END DO
        END DO
      END IF
c     if(box.le.11)call smooth_func(box,func,2)
C
      END





      SUBROUTINE SMOOTH_3DFUNC(FTYPE,BOX,WIDTH,FUNC)
C
      IMPLICIT NONE
C
C     Parameters.
C
      INTEGER FTYPE,BOX
      REAL    WIDTH(3),FUNC(BOX,BOX,BOX)
C
C     Local variables.
C
      REAL      A1
      REAL      A2
      REAL      A3
      REAL      C
      INTEGER   I
      INTEGER   J
      INTEGER   K
      REAL      X
      REAL      XARG
      REAL      Y
      REAL      YARG
      REAL      Z
      REAL      ZARG
C
C     Generate 3-D function box.
C     - top hat.
C
      IF(FTYPE.EQ.1)THEN
        DO K=1,BOX
          DO J=1,BOX
            DO I=1,BOX
              FUNC(I,J,K)=1.0
            END DO
          END DO
        END DO
C
C     - Gaussian.
C
      ELSE IF(FTYPE.EQ.2)THEN
        A1=1.0/(2.0*WIDTH(1)*WIDTH(1))
        A2=1.0/(2.0*WIDTH(2)*WIDTH(2))
        A3=1.0/(2.0*WIDTH(3)*WIDTH(3))
        C=REAL(BOX+1)/2.0
        DO K=1,BOX
          Z=REAL(K)
          ZARG = (Z-C)*(Z-C)*A3
          DO J=1,BOX
            Y=REAL(J)
            YARG = (Y-C)*(Y-C)*A2
            DO I=1,BOX
              X=REAL(I)
              XARG = (X-C)*(X-C)*A1
              FUNC(I,J,K)=EXP(-(XARG + YARG + ZARG))
            END DO
          END DO
        END DO
C
C     - sinc.
C
      ELSE IF(FTYPE.EQ.3)THEN
        C=REAL(BOX+1)/2.0
        DO K=1,BOX
          Z=WIDTH(3)*(REAL(K)-C)
          IF(ABS(Z).GE.1.0)THEN
            A3=SIN(Z)/Z
          ELSE
            A3=1.0
          END IF
          DO J=1,BOX
            Y=WIDTH(2)*(REAL(J)-C)
            IF(ABS(Y).GE.1.0)THEN
              A2=SIN(Y)/Y
            ELSE
              A2=1.0
            END IF
            DO I=1,BOX
              X=WIDTH(1)*(REAL(I)-C)
              IF(ABS(X).GE.1.0)THEN
                A1=SIN(X)/X
              ELSE
                A1=1.0
              END IF
              FUNC(I,J,K)=A1*A2*A3
            END DO
          END DO
        END DO
      END IF
c     if(box.le.11)call smooth_func(box,func,3)
C
      END



      subroutine smooth_func(box,func,ndim)
c
      integer   box,ndim
      real      func(box**ndim)
c
      integer   ich_len,ich_encode
c
      character form*16,string*80
      integer   dims(3),i,ii,inc(3),j,k,next,off,off2,off3
c
      do i=1,3
        if(ndim.ge.i)then
          dims(i)=box
        else
          dims(i)=1
        end if
      end do
c
      do i=1,ndim
        inc(i)=1
        do ii=1,i-1
          inc(i)=inc(i)*box
        end do
      end do
c
      form='(1x,'
      dumint=ich_encode(form,float(box),5,0,next)
      form(next:)='f7.4)'
c
      do k=1,dims(3)
        off3=(k-1)*inc(3)
          do j=1,dims(2)
            off2=(j-1)*inc(2)
            off=1+off2+off3
            write(string,form(:ich_len(form)))(func(i),i=off,off+box-1)
            call dsa_wruser(string(:ich_len(string))//'\\n')
          end do
          call dsa_wruser(' \\n')
      end do
c
      end



