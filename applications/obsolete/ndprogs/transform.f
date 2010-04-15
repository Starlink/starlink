      SUBROUTINE TRANSFORM
C+
C
C   -----------------
C   T R A N S F O R M
C   -----------------
C
C
C   Description
C   -----------
C   Geometric transformation program which can perform rebinning operations
C   on an image, in the form of linear shift, rotation, and resampling. The
C   operations may be performed separately or in combination. Several
C   methods for interpolating new pixel values are available.
C
C
C   Scope of program
C   ----------------
C   - Images of two or three dimensions accepted.
C   - Data array types SHORT and FLOAT supported; others converted to FLOAT.
C   - Subsetting not supported.
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
C   IMAGE     Name of the structure containing the input image. (character)
C             (prompted for).
C
C   SHIFT     Shift amount for each dimension in unresampled pixels. A
C             positive value shifts to a higher pixel number. (real, array)
C             (prompted for).
C
C   CENTRE    Rotation centre in each dimension in unresampled pixels.
C             (real, array)(prompted for).
C
C   ANGLE     Anticlockwise rotation angle about each axis in degrees.
C             (real, array)(prompted for).
C
C   RESAMPLE  Resampling factor for each dimension in output pixels per
C             input pixel. (real, array)(prompted for).
C
C   STAPIX    Required start pixel of OUTPUT, relative to origin of IMAGE
C             (1,1...) after shift and rotation. (integer, array)
C             (prompted for).
C
C   ENDPIX    Required end pixel of OUTPUT, relative to origin of IMAGE
C             (1,1...) after shift and rotation. (integer, array)
C             (prompted for).
C
C   INTERP    Method used to compute the value of each output pixel.
C             (integer)(prompted for). These method numbers are also used in
C             UNMAGIC. Methods 2 (average of neighbours) and 5 (replace with
C             a constant) are offered in UNMAGIC but are not appropriate to
C             TRANSFORM.
C             1 = nearest neighbour
C             3 = linear interpolation
C             4 = higher order (not yet available)
C
C   OUTPUT    Name of the structure containing the output image. May be the
C             same as IMAGE. (character)(prompted for).
C
C   AXKEY     Keys for axes to be calibrated.(integer, array)(prompted for).
C
C   AXSTART   Start calibration value for each axis. (real, array)(prompted
C             for).
C
C   AXEND     End calibration value for each axis. (real, array)(prompted
C             for).
C
C   AXLOG     Keys for axes to be calibrated logarithmically.
C             (integer, array)(prompted for).
C
C   ERR_VAL   The value to pad error arrays out with when the output
C             array becomes larger thsan the input array.
C
C   Keywords
C   --------
C   AXES      Instruction to calibrate the axes of OUTPUT. If specified,
C             the axis start and increment values may be selected.
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
C   - The IMAGE structure is tested for the bad data flag. If it is found
C     and non-zero, magic values are assumed to be present and are left in
C     the data.
C   - Quality and error arrays are tested for.
C   - 3-D rotation matrices are computed if required.
C   - All rotations are allowed, but rotation about axis 1 or 2 is much less
C     efficient in terms of page faulting than rotation about axis 3. Both
C     of the former involve accessing the third dimension of the data array
C     in a non-sequential fashion. A working set management method like that
C     used in AXFLIP and TRANSPOSE would be very difficult to devise. It
C     would be logical to run TRANSPOSE, do the most efficient rotation in
C     TRANSFORM, and then run TRANSPOSE again, but in practice this would
C     probably take even longer.
C   - The dimensions of OUTPUT are computed by applying the required shift,
C     rotation, and resampling to the corners of IMAGE.
C   - The structure IMAGE is copied to OUTPUT, with the exception of the
C     data array and axes. DSA_RESHAPE_ routines are called to modify the
C     dimensions of the OUTPUT axis and data/quality/error arrays.
C   - If it is required to calibrate the OUTPUT axes, the axis number(s),
C     start and end value(s), and linear/logarithmic flag(s) are prompted
C     for. The axes are then mapped and calibrated.
C   - A subroutine appropriate to the data type, the presence or absence of
C     rotation, the required interpolation method, and the presence or
C     absence of magic values is called to transform the IMAGE data array
C     into the OUTPUT data array. All the routines use the same approach to
C     transformation, which is to back-transform each pixel location of
C     OUTPUT so as to find the IMAGE pixel whose value is to be used. In
C     most cases this will not be an exact pixel, so a value must be
C     interpolated at the intermediate location. The nearest neighbour
C     method does not interpolate but simply uses NINT to round the pixel
C     number. Interpolation of a value for the back-transformed pixel is
C     impossible if it lies outside IMAGE, or if any of the adjacent IMAGE
C     pixels is magic. Therefore the OUTPUT pixel is given the magic value.
C     Error and quality arrays are handled by subsequent calls to the
C     appropriate routine for efficiency (rather than handle them all at
C     once.)
C   - The OUTPUT bad data flag is set if magic values have appeared for the
C     reasons given above.
C
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library DSA:
C      DSA_CLOSE
C      DSA_DATA_SIZE
C      DSA_INPUT
C      DSA_MAP_AXIS_DATA
C      DSA_MAP_DATA
C      DSA_MAP_ERRORS
C      DSA_MAP_QUALITY
C      DSA_RESHAPE_AXIS
C      DSA_RESHAPE_DATA
C      DSA_OPEN
C      DSA_OUTPUT
C      DSA_SEEK_ERRORS
C      DSA_SEEK_QUALITY
C      DSA_USE_FLAGGED_VALUES
C      DSA_WRUSER
C
C   Library DYN:
C      DYN_ELEMENT
C
C   Library GEN:
C      GEN_ELEMF
C
C   Library ICH:
C      ICH_ENCODE
C      ICH_LEN
C
C   Library NDP:
C      NDP_CORNERS
C      NDP_DISPLAY_PROGRESS
C      NDP_GET_IMAGE_INFO
C      NDP_LINEAR
C      NDP_PAR_RDARY
C      NDP_SET_AXES
C      NDP_SET_BAD_PIXEL
C
C   Library PAR:
C      PAR_RDVAL
C      PAR_SDVAL
C
C
C   Internal subroutines called
C   ---------------------------
C   TRANSFORM_NEWDIMS
C   TRANSFORM_2DNOROT1_W
C   TRANSFORM_2DNOROT1_R
C   TRANSFORM_2DNOROT3_W
C   TRANSFORM_2DNOROT3_WQ
C   TRANSFORM_2DNOROT3_R
C   TRANSFORM_2DNOROT3_RQ
C   TRANSFORM_2DROT1_W
C   TRANSFORM_2DROT1_R
C   TRANSFORM_2DROT3_W
C   TRANSFORM_2DROT3_WQ
C   TRANSFORM_2DROT3_R
C   TRANSFORM_2DROT3_RQ
C   TRANSFORM_2DTRANNR
C   TRANSFORM_2DTRANR
C   TRANSFORM_3DNOROT1_W
C   TRANSFORM_3DNOROT1_R
C   TRANSFORM_3DNOROT3_W
C   TRANSFORM_3DNOROT3_WQ
C   TRANSFORM_3DNOROT3_R
C   TRANSFORM_3DNOROT3_RQ
C   TRANSFORM_3DROT1_W
C   TRANSFORM_3DROT1_R
C   TRANSFORM_3DROT3_W
C   TRANSFORM_3DROT3_WQ
C   TRANSFORM_3DROT3_R
C   TRANSFORM_3DROT3_RQ
C   TRANSFORM_3DTRANNR
C   TRANSFORM_3DTRANR
C   TRANSFORM_3DMATRIX
C
C
C   INCLUDE statements
C   ------------------
C   INCLUDE 'DYNAMIC_MEMORY'
C   INCLUDE 'MAGIC_VALUES'
C   INCLUDE 'NUMERIC_RANGES'
C
C
C   Extensions to FORTRAN 77
C   ------------------------
C   END DO / IMPLICIT NONE / INCLUDE / Names > 6 characters
C
C
C   Possible future upgrades:
C   ------------------------
C   - Provide a higher order interpolation method.
C   - Cater for more than 3 dimensions.
C
C
C   Author/s
C   --------
C   Nick Fuller   RGO  (RGVAD::NMJF or NMJF@UK.AC.RGO.STAR)
C   Chris Benn    RGO  (RGVAD::CRB or CRB@UK.AC.RGO.STAR)
C   Peter Allan   Manchester (MAVAD::PMA)
C   Jim Lewis     RGO  (CAVAD::JRL or JRL@UK.AC.CAM.AST-STAR)
C   Julian Gold   RGO  (CAVAD::GOLDJIL or GOLDJIL@UK.AC.CAM.AST-STAR)
C
C
C   History
C   -------
C   01-FEB-1989   - Original program
C   22-JUL-1990   - Modified to pass the sizes of adjustable arrays to
C                   subroutines as individual variables rather than as
C                   elements of arrays. This change was made necessary by
C                   the VAX Fortran 5.2 compiler. (PMA)
C   28-JAN-1991   - Several bugs fixed.  Magic values were being passed
C                   wrongly to REAL routines.  The NOROT routines wouldn't
C                   shift properly as the shifts were being added back in
C                   a devious way.  The method 3 routines were also trying
C                   to interpolate past the edges of the images as well.
C                   (JRL)
C   01-DEC-1991   - Quality and error array handling added. (GOLDJIL)
C   11-MAR-1992   - Now checks that resampling factors are non-zero (GOLDJIL).
C   10-DEC-1992   - Unix version (GOLDJIL)
C   06-OCT-1994   - Removed lots of unused variables. (GJP)
C
C
C+-----------------------------------------------------------------------------
C
      IMPLICIT NONE
C
C   Functions.
C
      INTEGER  DYN_ELEMENT,ICH_ENCODE,ICH_LEN
      REAL     GEN_ELEMF
C
C   Local variables.
C
      INTEGER   ADDRESS             ! Address of dynamic memory element
      REAL      ANGLE(6)            ! Rotation angles
      LOGICAL   AXES                ! Instruction to calibrate OUTPUT axes
      INTEGER   AXPTR(6)            ! Dynamic pointers to IMAGE axes
      INTEGER   AXSLOT(6)           ! Map slot numbers for IMAGE axes
      LOGICAL   BADPIX              ! Value of bad pixel flag
      REAL      CENTRE(6)           ! Centre of rotation in input pixels
      INTEGER   SIZEDIFF            ! Difference in size due to transformation
      INTEGER   DIMS(10)            ! Dimensions of IMAGE
      REAL      DUMARR(6)           ! REAL array dummy variable
      INTEGER   DUMINT              ! INTEGER dummy variable
      REAL      DUMREAL             ! REAL dummy variable
      REAL      END(6)              ! End values of IMAGE axes
      LOGICAL   ERR                 ! Error array presence flag
      REAL      ERR_VAL             ! Value to pad error array with
      INTEGER   I                   ! Loop counter
      LOGICAL   IEPTR               ! Dynamic pointer to IMAGE error array
      INTEGER   IESLOT              ! Map slot number for IMAGE error array
      INTEGER   IMPTR               ! Dynamic pointer to IMAGE data array
      INTEGER   INTERP              ! Interpolation method number
      INTEGER   IQPTR               ! Dynamic pointer to IMAGE quality array
      INTEGER   IQSLOT              ! Map slot number for IMAGE quality array
      INTEGER   IROT                ! Number of rotation axes
      INTEGER   ISLOT               ! Map slot number for IMAGE data
      REAL      MAGICPIX            ! Number of pixels set to magic value
      INTEGER   MINCORN             ! Minimum no. of non-magic corner pixels
      INTEGER   NDIM                ! Number of dimensions in IMAGE
      INTEGER   NELM                ! Number of elements in IMAGE
      INTEGER   NEXT                ! Pointer returned by ICH_ENCODE
      INTEGER   ODIMS(10)           ! Dimensions of OUTPUT
      INTEGER   OEPTR               ! Dynamic pointer to OUTPUT error array
      INTEGER   OESLOT              ! Map slot number for OUTPUT error array
      INTEGER   ONDIM               ! Number of dimensions in OUTPUT
      INTEGER   ONELM               ! Number of elements in OUTPUT
      INTEGER   OQPTR               ! Dynamic pointer to OUTPUT quality array
      INTEGER   OQSLOT              ! Map slot number for OUTPUT quality array
      LOGICAL   ORTHO               ! Orthogonal rotation flag
      INTEGER   OSLOT               ! Map slot number for OUTPUT data
      INTEGER   OUTPTR              ! Dynamic pointer to OUTPUT data array
      LOGICAL   QUAL                ! Quality array presence flag
      LOGICAL   ROTATE              ! Rotation flag
      REAL      SAMPLE(6)           ! Resampling factors in output/input pixels
      REAL      SHIFT(6)            ! Shift amounts in input pixels
      REAL      START(6)            ! Start values of IMAGE axes
      INTEGER   STAPIX(6)           ! Start pixel of output data array
      INTEGER   STATUS              ! Status code
      CHARACTER STRING*80           ! Message string
      CHARACTER TYPE*8              ! Data array type
      REAL      VMAX(6)             ! Maximum values for NDP_PAR_RDARY
      REAL      VMIN(6)             ! Minimum values for NDP_PAR_RDARY
C
      INTEGER   NEW_FILE,NO_DATA
      PARAMETER (NEW_FILE=1,NO_DATA=1)
C
      INCLUDE 'DYNAMIC_MEMORY'
      INCLUDE 'MAGIC_VALUES'
      INCLUDE 'NUMERIC_RANGES'
C
C   Initialize.
C
      STATUS=0
      ROTATE=.FALSE.
      ORTHO=.FALSE.
      DO I=1,6
        ODIMS(I)=1
        SHIFT(I)=0.0
        ANGLE(I)=0.0
        CENTRE(I)=1.0
        SAMPLE(I)=1.0
      END DO
C
C   Open DSA system.
C
      CALL DSA_OPEN(STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   Open file for IMAGE.
C
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   Display information on IMAGE.
C
      CALL NDP_GET_IMAGE_INFO('IMAGE',.TRUE.,.TRUE.,TYPE,BADPIX,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   Get dimensions of IMAGE data array.
C
      CALL DSA_DATA_SIZE('IMAGE',3,NDIM,DIMS,NELM,STATUS)
      IF(NDIM.EQ.1)THEN
        CALL DSA_WRUSER
     &    ('This program handles 2-D and 3-D images only.\\N')
        GO TO 500
      END IF
      ONDIM=NDIM
C
C   Get error and quality array information
C
      CALL DSA_SEEK_ERRORS('IMAGE',ERR,STATUS)
      CALL DSA_SEEK_QUALITY('IMAGE',QUAL,STATUS)
      IF (STATUS.NE.0)GO TO 500
C
C   Map IMAGE axes and get start and end values.
C
      DO I=1,NDIM
        CALL DSA_MAP_AXIS_DATA
     &    ('IMAGE',I,'READ','FLOAT',ADDRESS,AXSLOT(I),STATUS)
        IF(STATUS.NE.0)GO TO 500
        AXPTR(I)=DYN_ELEMENT(ADDRESS)
        START(I)=GEN_ELEMF(DYNAMIC_MEM(AXPTR(I)),1)
        END(I)=GEN_ELEMF(DYNAMIC_MEM(AXPTR(I)),DIMS(I))
      END DO
C
C   Get shift amounts.
C
      DO I=1,NDIM
        VMIN(I)=MIN_FLOAT
        VMAX(I)=MAX_FLOAT
      END DO
      CALL DSA_WRUSER('Operations will be performed in the order ')
      CALL DSA_WRUSER('shift - rotate - resample.\\N')
      CALL NDP_PAR_RDARY('SHIFT',VMIN,VMAX,'N',' ',NDIM,3,SHIFT)
C
C   Get rotation centre - may be outside the image.
C
      DO I=1,NDIM
        VMIN(I)=REAL(MIN_INT)
        VMAX(I)=REAL(MAX_INT)
      END DO
      CALL NDP_PAR_RDARY('CENTRE',VMIN,VMAX,'N',' ',NDIM,3,CENTRE)
C
C   Get rotation angles. Note that in the 3-D case, the angles entered are
C   re-ordered because the variable ANGLE(1) pertains to the axis through the
C   XY plane, ANGLE(2) to the axis through the XZ plane, and ANGLE(3) to the
C   axis through the YZ plane.
C
      DO I=1,IROT
        VMIN(I)=-180.0
        VMAX(I)=180.0
      END DO
      IF(NDIM.EQ.2)IROT=1
      IF(NDIM.EQ.3)IROT=3
      CALL NDP_PAR_RDARY('ANGLE',VMIN,VMAX,'N',' ',IROT,3,DUMARR)
      IF(NDIM.EQ.2)THEN
        ANGLE(1)=DUMARR(1)
      ELSE IF(NDIM.EQ.3)THEN
        ANGLE(1)=DUMARR(3)
        ANGLE(2)=DUMARR(2)
        ANGLE(3)=DUMARR(1)
      END IF
C
C   Set rotation flag if any angle is non-zero.
C
      DO I=1,IROT
        IF(ANGLE(I).NE.0.0)ROTATE=.TRUE.
      END DO
C
C   Set orthogonal rotation flag if all relevant angles are either zero or
C   multiples of 90 degrees.
C
      IF(NDIM.EQ.2)THEN
        IF(REAL(INT(ANGLE(1))).EQ.ANGLE(1) .AND.
     &     MOD(INT(ANGLE(1)),90).EQ.0)ORTHO=.TRUE.
      ELSE IF(NDIM.EQ.3)THEN
        IF((REAL(INT(ANGLE(1))).EQ.ANGLE(1) .AND.
     &      MOD(INT(ANGLE(1)),90).EQ.0) .AND.
     &     (REAL(INT(ANGLE(2))).EQ.ANGLE(2) .AND.
     &      MOD(INT(ANGLE(2)),90).EQ.0) .AND.
     &     (REAL(INT(ANGLE(3))).EQ.ANGLE(3) .AND.
     &      MOD(INT(ANGLE(3)),90).EQ.0))ORTHO=.TRUE.
      END IF
C
C   Convert angles to radians.
C
      DO I=1,IROT
        ANGLE(I)=ANGLE(I)*3.141593/180.0
      END DO
C
C   Get resampling factors.
C
      DO I=1,NDIM
        VMIN(I)=0.0
        VMAX(I)=MAX_FLOAT
      END DO
      CALL NDP_PAR_RDARY('RESAMPLE',VMIN,VMAX,'N',' ',NDIM,3,SAMPLE)
C
C   Check to see if resamples are zero
C
      DO I=1,NDIM
        IF (SAMPLE(I) .EQ. 0.0) THEN
          CALL DSA_WRUSER('Zero is an invalid resampling factor. \\n')
          GO TO 500
        END IF
      END DO
C
C   Calculate the dimensions of the transformed image.
C
      CALL TRANSFORM_NEWDIMS
     &  (DIMS,NDIM,SHIFT,ANGLE,CENTRE,SAMPLE,ROTATE,ORTHO,
     &   ODIMS,ONELM,STAPIX)
C
C   If the output array is larger than in amy dimension than the input array
C   AND an error array is present, get the value to pad the error array with.
C
      IF (ERR) THEN
        DO I=1,NDIM
          SIZEDIFF = ODIMS(I) - DIMS(I)
          IF (SIZEDIFF .GT. 0) THEN
            CALL PAR_RDVAL('ERR_VAL',-32768.0,32767.0,
     &                     0.0,' ',ERR_VAL)
            GO TO 111
          END IF
        END DO
      END IF
111   CONTINUE
C
C   Get interpolation method.
C
      CALL PAR_RDVAL('INTERP',1.0,4.0,1.0,' ',DUMREAL)
      INTERP=INT(DUMREAL)
C
C   Get number of non-magic corner pixels.
C
c     CALL PAR_RDVAL('MINCORN',1.0,REAL(2**NDIM),1.0,' ',DUMREAL)
c     MINCORN=INT(DUMREAL)
      MINCORN=2**NDIM
C
C   Open file for OUTPUT.
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',NO_DATA,NEW_FILE,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C   Reshape OUTPUT data array and axes.
C
      CALL DSA_WRUSER('Reshaping data array...\\N')
      CALL DSA_RESHAPE_DATA('OUTPUT','IMAGE',NDIM,ODIMS,STATUS)
      DO I=1,NDIM
        CALL DSA_RESHAPE_AXIS('OUTPUT',I,'IMAGE',I,1,ODIMS(I),STATUS)
      END DO
C
C   Calibrate the OUTPUT axes if required.
C
      CALL PAR_RDKEY('AXES',.FALSE.,AXES)
      IF(AXES)THEN
        CALL NDP_SET_AXES('OUTPUT',ODIMS,NDIM,STATUS)
        IF(STATUS.NE.0)GO TO 500
      END IF
C
C   Magic values are not to be removed from the data arrays (providing there's
C   no quality arrays)
C
      IF (.NOT.QUAL) THEN
        CALL DSA_USE_FLAGGED_VALUES('IMAGE',STATUS)
        IF(STATUS.NE.0)GO TO 500
        CALL DSA_USE_FLAGGED_VALUES('OUTPUT',STATUS)
        IF(STATUS.NE.0)GO TO 500
      END IF
C
C   Map data arrays (and error arrays if applicable)
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
          IF (STATUS.NE.0)GO TO 500
          IEPTR=DYN_ELEMENT(ADDRESS)
          CALL DSA_MAP_ERRORS('OUTPUT','WRITE','SHORT',
     &                        ADDRESS,OESLOT,STATUS)
          IF (STATUS.NE.0)GO TO 500
          OEPTR=DYN_ELEMENT(ADDRESS)
        END IF
      ELSE
        CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',
     &                    ADDRESS,ISLOT,STATUS)
        IF(STATUS.NE.0)GO TO 500
        IMPTR=DYN_ELEMENT(ADDRESS)
        CALL DSA_MAP_DATA('OUTPUT','WRITE','FLOAT',
     &                    ADDRESS,OSLOT,STATUS)
        IF(STATUS.NE.0)GO TO 500
        OUTPTR=DYN_ELEMENT(ADDRESS)
        IF (ERR) THEN
          CALL DSA_MAP_ERRORS('IMAGE','READ','FLOAT',
     &                        ADDRESS,IESLOT,STATUS)
          IF (STATUS.NE.0)GO TO 500
          IEPTR=DYN_ELEMENT(ADDRESS)
          CALL DSA_MAP_ERRORS('OUTPUT','WRITE','FLOAT',
     &                        ADDRESS,OESLOT,STATUS)
          IF (STATUS.NE.0)GO TO 500
          OEPTR=DYN_ELEMENT(ADDRESS)
        END IF
      END IF
C
C   Map quality arrays if necessary
C
       IF (QUAL) THEN
         CALL DSA_MAP_QUALITY('IMAGE','READ','BYTE',
     &                        ADDRESS,IQSLOT,STATUS)
         IF (STATUS.NE.0)GO TO 500
         IQPTR=DYN_ELEMENT(ADDRESS)
         CALL DSA_MAP_QUALITY('OUTPUT','WRITE','BYTE',
     &                        ADDRESS,OQSLOT,STATUS)
         IF (STATUS.NE.0)GO TO 500
         OQPTR=DYN_ELEMENT(ADDRESS)
       END IF
C
C   Perform the transformation. The magic value is supplied to non-magic
C   routines for any OUTPUT pixels which lie outside ARRAY.
C
      CALL DSA_WRUSER('Transforming...\\N')
C
      IF(TYPE.EQ.'SHORT')THEN
        IF(NDIM.EQ.2)THEN
          IF(ROTATE)THEN
            IF(INTERP.EQ.1)THEN
              CALL TRANSFORM_2DROT1_W(DYNAMIC_MEM(IMPTR),
     &                                DYNAMIC_MEM(OUTPTR),
     &                                DIMS,NDIM,ODIMS,ONDIM,
     &                                SHIFT,ANGLE,CENTRE,SAMPLE,
     &                                STAPIX,MAGICPIX,MAGIC_SHORT,
     &                                DIMS(1),DIMS(2),DIMS(3),
     &                                ODIMS(1),ODIMS(2),ODIMS(3))
              IF (ERR) CALL TRANSFORM_2DROT1_W(
     &                                DYNAMIC_MEM(IEPTR),
     &                                DYNAMIC_MEM(OEPTR),
     &                                DIMS,NDIM,ODIMS,ONDIM,
     &                                SHIFT,ANGLE,CENTRE,SAMPLE,
     &                                STAPIX,MAGICPIX,INT(ERR_VAL),
     &                                DIMS(1),DIMS(2),DIMS(3),
     &                                ODIMS(1),ODIMS(2),ODIMS(3))
              IF (QUAL) CALL TRANSFORM_2DROT1_B(
     &                                DYNAMIC_MEM(IQPTR),
     &                                DYNAMIC_MEM(OQPTR),
     &                                DIMS,NDIM,ODIMS,ONDIM,
     &                                SHIFT,ANGLE,CENTRE,SAMPLE,
     &                                STAPIX,MAGICPIX,1,
     &                                DIMS(1),DIMS(2),DIMS(3),
     &                                ODIMS(1),ODIMS(2),ODIMS(3))
            ELSE IF(.NOT.BADPIX)THEN
              IF(INTERP.EQ.3)THEN
                CALL TRANSFORM_2DROT3_W(DYNAMIC_MEM(IMPTR),
     &                                  DYNAMIC_MEM(OUTPTR),
     &                                  DIMS,NDIM,ODIMS,ONDIM,MINCORN,
     &                                  SHIFT,ANGLE,CENTRE,SAMPLE,
     &                                  STAPIX,MAGICPIX,MAGIC_SHORT,
     &                                  DIMS(1),DIMS(2),
     &                                  ODIMS(1),ODIMS(2))
                IF (ERR) CALL TRANSFORM_2DROT3_W(
     &                                  DYNAMIC_MEM(IEPTR),
     &                                  DYNAMIC_MEM(OEPTR),
     &                                  DIMS,NDIM,ODIMS,ONDIM,MINCORN,
     &                                  SHIFT,ANGLE,CENTRE,SAMPLE,
     &                                  STAPIX,MAGICPIX,INT(ERR_VAL),
     &                                  DIMS(1),DIMS(2),
     &                                  ODIMS(1),ODIMS(2))
                IF (QUAL) CALL TRANSFORM_2DROT3_B(
     &                                  DYNAMIC_MEM(IQPTR),
     &                                  DYNAMIC_MEM(OQPTR),
     &                                  DIMS,NDIM,ODIMS,ONDIM,MINCORN,
     &                                  SHIFT,ANGLE,CENTRE,SAMPLE,
     &                                  STAPIX,MAGICPIX,1,
     &                                  DIMS(1),DIMS(2),
     &                                  ODIMS(1),ODIMS(2))
              END IF
            ELSE IF(BADPIX)THEN
              IF(INTERP.EQ.3)THEN
                CALL TRANSFORM_2DROT3_WQ(DYNAMIC_MEM(IMPTR),
     &                                   DYNAMIC_MEM(OUTPTR),
     &                                   DIMS,NDIM,ODIMS,ONDIM,MINCORN,
     &                                   SHIFT,ANGLE,CENTRE,SAMPLE,
     &                                   STAPIX,MAGICPIX,MAGIC_SHORT,
     &                                   DIMS(1),DIMS(2),
     &                                   ODIMS(1),ODIMS(2))
                IF (ERR) CALL TRANSFORM_2DROT3_W(
     &                                   DYNAMIC_MEM(IEPTR),
     &                                   DYNAMIC_MEM(OEPTR),
     &                                   DIMS,NDIM,ODIMS,ONDIM,MINCORN,
     &                                   SHIFT,ANGLE,CENTRE,SAMPLE,
     &                                   STAPIX,MAGICPIX,INT(ERR_VAL),
     &                                   DIMS(1),DIMS(2),
     &                                   ODIMS(1),ODIMS(2))
              END IF
            END IF
          ELSE
            IF(INTERP.EQ.1)THEN
              CALL TRANSFORM_2DNOROT1_W(DYNAMIC_MEM(IMPTR),
     &                                  DYNAMIC_MEM(OUTPTR),
     &                                  DIMS,NDIM,ODIMS,ONDIM,
     &                                  SHIFT,SAMPLE,
     &                                  MAGICPIX,MAGIC_SHORT,
     &                                  DIMS(1),DIMS(2),
     &                                  ODIMS(1),ODIMS(2))
              IF (ERR) CALL TRANSFORM_2DNOROT1_W(
     &                                  DYNAMIC_MEM(IEPTR),
     &                                  DYNAMIC_MEM(OEPTR),
     &                                  DIMS,NDIM,ODIMS,ONDIM,
     &                                  SHIFT,SAMPLE,
     &                                  MAGICPIX,INT(ERR_VAL),
     &                                  DIMS(1),DIMS(2),
     &                                  ODIMS(1),ODIMS(2))
              IF (QUAL) CALL TRANSFORM_2DNOROT1_B(
     &                                  DYNAMIC_MEM(IQPTR),
     &                                  DYNAMIC_MEM(OQPTR),
     &                                  DIMS,NDIM,ODIMS,ONDIM,
     &                                  SHIFT,SAMPLE,
     &                                  MAGICPIX,1,
     &                                  DIMS(1),DIMS(2),
     &                                  ODIMS(1),ODIMS(2))
            ELSE IF(.NOT.BADPIX)THEN
              IF(INTERP.EQ.3)THEN
                CALL TRANSFORM_2DNOROT3_W(DYNAMIC_MEM(IMPTR),
     &                                    DYNAMIC_MEM(OUTPTR),
     &                                    DIMS,NDIM,ODIMS,ONDIM,
     &                                    MINCORN,SHIFT,SAMPLE,
     &                                    MAGICPIX,MAGIC_SHORT,
     &                                    DIMS(1),DIMS(2),
     &                                    ODIMS(1),ODIMS(2))
                IF (ERR) CALL TRANSFORM_2DNOROT3_W(
     &                                    DYNAMIC_MEM(IEPTR),
     &                                    DYNAMIC_MEM(OEPTR),
     &                                    DIMS,NDIM,ODIMS,ONDIM,
     &                                    MINCORN,SHIFT,SAMPLE,
     &                                    MAGICPIX,INT(ERR_VAL),
     &                                    DIMS(1),DIMS(2),
     &                                    ODIMS(1),ODIMS(2))
                IF (QUAL) CALL TRANSFORM_2DNOROT3_B(
     &                                    DYNAMIC_MEM(IQPTR),
     &                                    DYNAMIC_MEM(OQPTR),
     &                                    DIMS,NDIM,ODIMS,ONDIM,
     &                                    MINCORN,SHIFT,SAMPLE,
     &                                    STAPIX,MAGICPIX,1,
     &                                    DIMS(1),DIMS(2),
     &                                    ODIMS(1),ODIMS(2))
              END IF
            ELSE IF(BADPIX)THEN
              IF(INTERP.EQ.3)THEN
                CALL TRANSFORM_2DNOROT3_WQ(DYNAMIC_MEM(IMPTR),
     &                                     DYNAMIC_MEM(OUTPTR),
     &                                     DIMS,NDIM,ODIMS,ONDIM,
     &                                     MINCORN,SHIFT,SAMPLE,
     &                                     MAGICPIX,MAGIC_SHORT,
     &                                     DIMS(1),DIMS(2),
     &                                     ODIMS(1),ODIMS(2))
                IF (ERR) CALL TRANSFORM_2DNOROT3_W(
     &                                    DYNAMIC_MEM(IEPTR),
     &                                    DYNAMIC_MEM(OEPTR),
     &                                    DIMS,NDIM,ODIMS,ONDIM,
     &                                    MINCORN,SHIFT,SAMPLE,
     &                                    MAGICPIX,INT(ERR_VAL),
     &                                    DIMS(1),DIMS(2),
     &                                    ODIMS(1),ODIMS(2))
              END IF
            END IF
          END IF
        ELSE IF(NDIM.EQ.3)THEN
          IF(ROTATE)THEN
            IF(INTERP.EQ.1)THEN
              CALL TRANSFORM_3DROT1_W(DYNAMIC_MEM(IMPTR),
     &                                DYNAMIC_MEM(OUTPTR),
     &                                DIMS,NDIM,ODIMS,ONDIM,
     &                                SHIFT,ANGLE,CENTRE,SAMPLE,
     &                                STAPIX,MAGICPIX,MAGIC_SHORT,
     &                                DIMS(1),DIMS(2),DIMS(3),
     &                                ODIMS(1),ODIMS(2),ODIMS(3))
              IF (ERR) CALL TRANSFORM_3DROT1_W(
     &                                DYNAMIC_MEM(IEPTR),
     &                                DYNAMIC_MEM(OEPTR),
     &                                DIMS,NDIM,ODIMS,ONDIM,
     &                                SHIFT,ANGLE,CENTRE,SAMPLE,
     &                                STAPIX,MAGICPIX,INT(ERR_VAL),
     &                                DIMS(1),DIMS(2),DIMS(3),
     &                                ODIMS(1),ODIMS(2),ODIMS(3))
              IF (QUAL) CALL TRANSFORM_3DROT1_B(
     &                                DYNAMIC_MEM(IQPTR),
     &                                DYNAMIC_MEM(OQPTR),
     &                                DIMS,NDIM,ODIMS,ONDIM,
     &                                SHIFT,ANGLE,CENTRE,SAMPLE,
     &                                STAPIX,MAGICPIX,1,
     &                                DIMS(1),DIMS(2),DIMS(3),
     &                                ODIMS(1),ODIMS(2),ODIMS(3))
            ELSE IF(.NOT.BADPIX)THEN
              IF(INTERP.EQ.3)THEN
                CALL TRANSFORM_3DROT3_W(DYNAMIC_MEM(IMPTR),
     &                                  DYNAMIC_MEM(OUTPTR),
     &                                  DIMS,NDIM,ODIMS,ONDIM,MINCORN,
     &                                  SHIFT,ANGLE,CENTRE,SAMPLE,
     &                                  STAPIX,MAGICPIX,MAGIC_SHORT,
     &                                  DIMS(1),DIMS(2),DIMS(3),
     &                                  ODIMS(1),ODIMS(2),ODIMS(3))
                IF (ERR) CALL TRANSFORM_3DROT3_W(
     &                                  DYNAMIC_MEM(IEPTR),
     &                                  DYNAMIC_MEM(OEPTR),
     &                                  DIMS,NDIM,ODIMS,ONDIM,MINCORN,
     &                                  SHIFT,ANGLE,CENTRE,SAMPLE,
     &                                  STAPIX,MAGICPIX,INT(ERR_VAL),
     &                                  DIMS(1),DIMS(2),DIMS(3),
     &                                  ODIMS(1),ODIMS(2),ODIMS(3))
                IF (QUAL) CALL TRANSFORM_3DROT3_B(
     &                                  DYNAMIC_MEM(IQPTR),
     &                                  DYNAMIC_MEM(OQPTR),
     &                                  DIMS,NDIM,ODIMS,ONDIM,MINCORN,
     &                                  SHIFT,ANGLE,CENTRE,SAMPLE,
     &                                  STAPIX,MAGICPIX,1,
     &                                  DIMS(1),DIMS(2),DIMS(3),
     &                                  ODIMS(1),ODIMS(2),ODIMS(3))
              END IF
            ELSE IF(BADPIX)THEN
              IF(INTERP.EQ.3)THEN
                CALL TRANSFORM_3DROT3_WQ(DYNAMIC_MEM(IMPTR),
     &                                   DYNAMIC_MEM(OUTPTR),
     &                                   DIMS,NDIM,ODIMS,ONDIM,MINCORN,
     &                                   SHIFT,ANGLE,CENTRE,SAMPLE,
     &                                   STAPIX,MAGICPIX,MAGIC_SHORT,
     &                                   DIMS(1),DIMS(2),DIMS(3),
     &                                   ODIMS(1),ODIMS(2),ODIMS(3))
                IF (ERR) CALL TRANSFORM_3DROT3_W(
     &                                  DYNAMIC_MEM(IEPTR),
     &                                  DYNAMIC_MEM(OEPTR),
     &                                  DIMS,NDIM,ODIMS,ONDIM,MINCORN,
     &                                  SHIFT,ANGLE,CENTRE,SAMPLE,
     &                                  STAPIX,MAGICPIX,INT(ERR_VAL),
     &                                  DIMS(1),DIMS(2),DIMS(3),
     &                                  ODIMS(1),ODIMS(2),ODIMS(3))
              END IF
            END IF
          ELSE
            IF(INTERP.EQ.1)THEN
              CALL TRANSFORM_3DNOROT1_W(DYNAMIC_MEM(IMPTR),
     &                                  DYNAMIC_MEM(OUTPTR),
     &                                  DIMS,NDIM,ODIMS,ONDIM,
     &                                  SHIFT,SAMPLE,
     &                                  MAGICPIX,MAGIC_SHORT,
     &                                  DIMS(1),DIMS(2),DIMS(3),
     &                                  ODIMS(1),ODIMS(2),ODIMS(3))
              IF (ERR) CALL TRANSFORM_3DNOROT1_W(
     &                                  DYNAMIC_MEM(IEPTR),
     &                                  DYNAMIC_MEM(OEPTR),
     &                                  DIMS,NDIM,ODIMS,ONDIM,
     &                                  SHIFT,SAMPLE,
     &                                  MAGICPIX,INT(ERR_VAL),
     &                                  DIMS(1),DIMS(2),DIMS(3),
     &                                  ODIMS(1),ODIMS(2),ODIMS(3))
              IF (QUAL) CALL TRANSFORM_3DROT1_B(
     &                                  DYNAMIC_MEM(IQPTR),
     &                                  DYNAMIC_MEM(OQPTR),
     &                                  DIMS,NDIM,ODIMS,ONDIM,
     &                                  SHIFT,SAMPLE,
     &                                  MAGICPIX,1,
     &                                  DIMS(1),DIMS(2),DIMS(3),
     &                                  ODIMS(1),ODIMS(2),ODIMS(3))
            ELSE IF(.NOT.BADPIX)THEN
              IF(INTERP.EQ.3)THEN
                CALL TRANSFORM_3DNOROT3_W(DYNAMIC_MEM(IMPTR),
     &                                    DYNAMIC_MEM(OUTPTR),
     &                                    DIMS,NDIM,ODIMS,ONDIM,
     &                                    MINCORN,SHIFT,SAMPLE,
     &                                    MAGICPIX,MAGIC_SHORT,
     &                                    DIMS(1),DIMS(2),DIMS(3),
     &                                    ODIMS(1),ODIMS(2),ODIMS(3))
                IF (ERR) CALL TRANSFORM_3DNOROT3_W(
     &                                    DYNAMIC_MEM(IEPTR),
     &                                    DYNAMIC_MEM(OEPTR),
     &                                    DIMS,NDIM,ODIMS,ONDIM,
     &                                    MINCORN,SHIFT,SAMPLE,
     &                                    MAGICPIX,INT(ERR_VAL),
     &                                    DIMS(1),DIMS(2),DIMS(3),
     &                                    ODIMS(1),ODIMS(2),ODIMS(3))
                IF (QUAL) CALL TRANSFORM_3DNOROT3_B(
     &                                    DYNAMIC_MEM(IQPTR),
     &                                    DYNAMIC_MEM(OQPTR),
     &                                    DIMS,NDIM,ODIMS,ONDIM,
     &                                    MINCORN,SHIFT,SAMPLE,
     &                                    MAGICPIX,1,
     &                                    DIMS(1),DIMS(2),DIMS(3),
     &                                    ODIMS(1),ODIMS(2),ODIMS(3))
              END IF
            ELSE IF(BADPIX)THEN
              IF(INTERP.EQ.3)THEN
                CALL TRANSFORM_3DNOROT3_WQ(DYNAMIC_MEM(IMPTR),
     &                                     DYNAMIC_MEM(OUTPTR),
     &                                     DIMS,NDIM,ODIMS,ONDIM,
     &                                     MINCORN,SHIFT,SAMPLE,
     &                                     MAGICPIX,MAGIC_SHORT,
     &                                     DIMS(1),DIMS(2),DIMS(3),
     &                                     ODIMS(1),ODIMS(2),ODIMS(3))
                IF (ERR) CALL TRANSFORM_3DNOROT3_W(
     &                                     DYNAMIC_MEM(IEPTR),
     &                                     DYNAMIC_MEM(OEPTR),
     &                                     DIMS,NDIM,ODIMS,ONDIM,
     &                                     MINCORN,SHIFT,SAMPLE,
     &                                     MAGICPIX,INT(ERR_VAL),
     &                                     DIMS(1),DIMS(2),DIMS(3),
     &                                     ODIMS(1),ODIMS(2),ODIMS(3))
              END IF
            END IF
          END IF
        END IF
      ELSE
        IF(NDIM.EQ.2)THEN
          IF(ROTATE)THEN
            IF(INTERP.EQ.1)THEN
              CALL TRANSFORM_2DROT1_R(DYNAMIC_MEM(IMPTR),
     &                                DYNAMIC_MEM(OUTPTR),
     &                                DIMS,NDIM,ODIMS,ONDIM,
     &                                SHIFT,ANGLE,CENTRE,SAMPLE,
     &                                STAPIX,MAGICPIX,MAGIC_FLOAT,
     &                                DIMS(1),DIMS(2),DIMS(3),
     &                                ODIMS(1),ODIMS(2),ODIMS(3))
              IF (ERR) CALL TRANSFORM_2DROT1_R(
     &                                DYNAMIC_MEM(IEPTR),
     &                                DYNAMIC_MEM(OEPTR),
     &                                DIMS,NDIM,ODIMS,ONDIM,
     &                                SHIFT,ANGLE,CENTRE,SAMPLE,
     &                                STAPIX,MAGICPIX,ERR_VAL,
     &                                DIMS(1),DIMS(2),DIMS(3),
     &                                ODIMS(1),ODIMS(2),ODIMS(3))
              IF (QUAL) CALL TRANSFORM_2DROT1_B(
     &                                DYNAMIC_MEM(IQPTR),
     &                                DYNAMIC_MEM(OQPTR),
     &                                DIMS,NDIM,ODIMS,ONDIM,
     &                                SHIFT,ANGLE,CENTRE,SAMPLE,
     &                                STAPIX,MAGICPIX,1,
     &                                DIMS(1),DIMS(2),DIMS(3),
     &                                ODIMS(1),ODIMS(2),ODIMS(3))
            ELSE IF(.NOT.BADPIX)THEN
              IF(INTERP.EQ.3)THEN
                CALL TRANSFORM_2DROT3_R(DYNAMIC_MEM(IMPTR),
     &                                  DYNAMIC_MEM(OUTPTR),
     &                                  DIMS,NDIM,ODIMS,ONDIM,MINCORN,
     &                                  SHIFT,ANGLE,CENTRE,SAMPLE,
     &                                  STAPIX,MAGICPIX,MAGIC_FLOAT,
     &                                  DIMS(1),DIMS(2),
     &                                  ODIMS(1),ODIMS(2))
                IF (ERR) CALL TRANSFORM_2DROT3_R(
     &                                  DYNAMIC_MEM(IEPTR),
     &                                  DYNAMIC_MEM(OEPTR),
     &                                  DIMS,NDIM,ODIMS,ONDIM,MINCORN,
     &                                  SHIFT,ANGLE,CENTRE,SAMPLE,
     &                                  STAPIX,MAGICPIX,ERR_VAL,
     &                                  DIMS(1),DIMS(2),
     &                                  ODIMS(1),ODIMS(2))
                IF (QUAL) CALL TRANSFORM_2DROT3_B(
     &                                  DYNAMIC_MEM(IQPTR),
     &                                  DYNAMIC_MEM(OQPTR),
     &                                  DIMS,NDIM,ODIMS,ONDIM,MINCORN,
     &                                  SHIFT,ANGLE,CENTRE,SAMPLE,
     &                                  STAPIX,MAGICPIX,1,
     &                                  DIMS(1),DIMS(2),
     &                                  ODIMS(1),ODIMS(2))
              END IF
            ELSE IF(BADPIX)THEN
              IF(INTERP.EQ.3)THEN
                CALL TRANSFORM_2DROT3_RQ(DYNAMIC_MEM(IMPTR),
     &                                   DYNAMIC_MEM(OUTPTR),
     &                                   DIMS,NDIM,ODIMS,ONDIM,MINCORN,
     &                                   SHIFT,ANGLE,CENTRE,SAMPLE,
     &                                   STAPIX,MAGICPIX,MAGIC_FLOAT,
     &                                   DIMS(1),DIMS(2),
     &                                   ODIMS(1),ODIMS(2))
                IF (ERR) CALL TRANSFORM_2DROT3_R(
     &                                  DYNAMIC_MEM(IEPTR),
     &                                  DYNAMIC_MEM(OEPTR),
     &                                  DIMS,NDIM,ODIMS,ONDIM,MINCORN,
     &                                  SHIFT,ANGLE,CENTRE,SAMPLE,
     &                                  STAPIX,MAGICPIX,ERR_VAL,
     &                                  DIMS(1),DIMS(2),
     &                                  ODIMS(1),ODIMS(2))
              END IF
            END IF
          ELSE
            IF(INTERP.EQ.1)THEN
              CALL TRANSFORM_2DNOROT1_R(DYNAMIC_MEM(IMPTR),
     &                                  DYNAMIC_MEM(OUTPTR),
     &                                  DIMS,NDIM,ODIMS,ONDIM,
     &                                  SHIFT,SAMPLE,
     &                                  MAGICPIX,MAGIC_FLOAT,
     &                                  DIMS(1),DIMS(2),
     &                                  ODIMS(1),ODIMS(2))
              IF (ERR) CALL TRANSFORM_2DNOROT1_R(
     &                                  DYNAMIC_MEM(IEPTR),
     &                                  DYNAMIC_MEM(OEPTR),
     &                                  DIMS,NDIM,ODIMS,ONDIM,
     &                                  SHIFT,SAMPLE,
     &                                  MAGICPIX,ERR_VAL,
     &                                  DIMS(1),DIMS(2),
     &                                  ODIMS(1),ODIMS(2))
              IF (QUAL) CALL TRANSFORM_2DNOROT1_B(
     &                                  DYNAMIC_MEM(IQPTR),
     &                                  DYNAMIC_MEM(OQPTR),
     &                                  DIMS,NDIM,ODIMS,ONDIM,
     &                                  SHIFT,SAMPLE,
     &                                  MAGICPIX,1,
     &                                  DIMS(1),DIMS(2),
     &                                  ODIMS(1),ODIMS(2))
            ELSE IF(.NOT.BADPIX)THEN
              IF(INTERP.EQ.3)THEN
                CALL TRANSFORM_2DNOROT3_R(DYNAMIC_MEM(IMPTR),
     &                                    DYNAMIC_MEM(OUTPTR),
     &                                    DIMS,NDIM,ODIMS,ONDIM,
     &                                    MINCORN,SHIFT,SAMPLE,
     &                                    MAGICPIX,MAGIC_FLOAT,
     &                                    DIMS(1),DIMS(2),
     &                                    ODIMS(1),ODIMS(2))
                IF (ERR) CALL TRANSFORM_2DNOROT3_R(
     &                                    DYNAMIC_MEM(IEPTR),
     &                                    DYNAMIC_MEM(OEPTR),
     &                                    DIMS,NDIM,ODIMS,ONDIM,
     &                                    MINCORN,SHIFT,SAMPLE,
     &                                    MAGICPIX,ERR_VAL,
     &                                    DIMS(1),DIMS(2),
     &                                    ODIMS(1),ODIMS(2))
                IF (QUAL) CALL TRANSFORM_2DNOROT3_B(
     &                                    DYNAMIC_MEM(IQPTR),
     &                                    DYNAMIC_MEM(OQPTR),
     &                                    DIMS,NDIM,ODIMS,ONDIM,
     &                                    MINCORN,SHIFT,SAMPLE,
     &                                    STAPIX,MAGICPIX,1,
     &                                    DIMS(1),DIMS(2),
     &                                    ODIMS(1),ODIMS(2))
              END IF
            ELSE IF(BADPIX)THEN
              IF(INTERP.EQ.3)THEN
                CALL TRANSFORM_2DNOROT3_RQ(DYNAMIC_MEM(IMPTR),
     &                                     DYNAMIC_MEM(OUTPTR),
     &                                     DIMS,NDIM,ODIMS,ONDIM,
     &                                     MINCORN,SHIFT,SAMPLE,
     &                                     MAGICPIX,MAGIC_FLOAT,
     &                                     DIMS(1),DIMS(2),
     &                                     ODIMS(1),ODIMS(2))
                IF (ERR) CALL TRANSFORM_2DNOROT3_R(
     &                                    DYNAMIC_MEM(IEPTR),
     &                                    DYNAMIC_MEM(OEPTR),
     &                                    DIMS,NDIM,ODIMS,ONDIM,
     &                                    MINCORN,SHIFT,SAMPLE,
     &                                    MAGICPIX,ERR_VAL,
     &                                    DIMS(1),DIMS(2),
     &                                    ODIMS(1),ODIMS(2))
              END IF
            END IF
          END IF
        ELSE IF(NDIM.EQ.3)THEN
          IF(ROTATE)THEN
            IF(INTERP.EQ.1)THEN
              CALL TRANSFORM_3DROT1_R(DYNAMIC_MEM(IMPTR),
     &                                DYNAMIC_MEM(OUTPTR),
     &                                DIMS,NDIM,ODIMS,ONDIM,
     &                                SHIFT,ANGLE,CENTRE,SAMPLE,
     &                                STAPIX,MAGICPIX,MAGIC_FLOAT,
     &                                DIMS(1),DIMS(2),DIMS(3),
     &                                ODIMS(1),ODIMS(2),ODIMS(3))
              IF (ERR) CALL TRANSFORM_3DROT1_R(
     &                                DYNAMIC_MEM(IEPTR),
     &                                DYNAMIC_MEM(OEPTR),
     &                                DIMS,NDIM,ODIMS,ONDIM,
     &                                SHIFT,ANGLE,CENTRE,SAMPLE,
     &                                STAPIX,MAGICPIX,ERR_VAL,
     &                                DIMS(1),DIMS(2),DIMS(3),
     &                                ODIMS(1),ODIMS(2),ODIMS(3))
              IF (QUAL) CALL TRANSFORM_3DROT1_B(
     &                                DYNAMIC_MEM(IQPTR),
     &                                DYNAMIC_MEM(OQPTR),
     &                                DIMS,NDIM,ODIMS,ONDIM,
     &                                SHIFT,ANGLE,CENTRE,SAMPLE,
     &                                STAPIX,MAGICPIX,1,
     &                                DIMS(1),DIMS(2),DIMS(3),
     &                                ODIMS(1),ODIMS(2),ODIMS(3))
            ELSE IF(.NOT.BADPIX)THEN
              IF(INTERP.EQ.3)THEN
                CALL TRANSFORM_3DROT3_R(DYNAMIC_MEM(IMPTR),
     &                                  DYNAMIC_MEM(OUTPTR),
     &                                  DIMS,NDIM,ODIMS,ONDIM,MINCORN,
     &                                  SHIFT,ANGLE,CENTRE,SAMPLE,
     &                                  STAPIX,MAGICPIX,MAGIC_FLOAT,
     &                                  DIMS(1),DIMS(2),DIMS(3),
     &                                  ODIMS(1),ODIMS(2),ODIMS(3))
                IF (ERR) CALL TRANSFORM_3DROT3_R(
     &                                  DYNAMIC_MEM(IEPTR),
     &                                  DYNAMIC_MEM(OEPTR),
     &                                  DIMS,NDIM,ODIMS,ONDIM,MINCORN,
     &                                  SHIFT,ANGLE,CENTRE,SAMPLE,
     &                                  STAPIX,MAGICPIX,ERR_VAL,
     &                                  DIMS(1),DIMS(2),DIMS(3),
     &                                  ODIMS(1),ODIMS(2),ODIMS(3))
                IF (QUAL) CALL TRANSFORM_3DROT3_B(
     &                                  DYNAMIC_MEM(IQPTR),
     &                                  DYNAMIC_MEM(OQPTR),
     &                                  DIMS,NDIM,ODIMS,ONDIM,MINCORN,
     &                                  SHIFT,ANGLE,CENTRE,SAMPLE,
     &                                  STAPIX,MAGICPIX,1,
     &                                  DIMS(1),DIMS(2),DIMS(3),
     &                                  ODIMS(1),ODIMS(2),ODIMS(3))
              END IF
            ELSE IF(BADPIX)THEN
              IF(INTERP.EQ.3)THEN
                CALL TRANSFORM_3DROT3_RQ(DYNAMIC_MEM(IMPTR),
     &                                   DYNAMIC_MEM(OUTPTR),
     &                                   DIMS,NDIM,ODIMS,ONDIM,MINCORN,
     &                                   SHIFT,ANGLE,CENTRE,SAMPLE,
     &                                   STAPIX,MAGICPIX,MAGIC_FLOAT,
     &                                   DIMS(1),DIMS(2),DIMS(3),
     &                                   ODIMS(1),ODIMS(2),ODIMS(3))
                IF (ERR) CALL TRANSFORM_3DROT3_R(
     &                                  DYNAMIC_MEM(IEPTR),
     &                                  DYNAMIC_MEM(OEPTR),
     &                                  DIMS,NDIM,ODIMS,ONDIM,MINCORN,
     &                                  SHIFT,ANGLE,CENTRE,SAMPLE,
     &                                  STAPIX,MAGICPIX,ERR_VAL,
     &                                  DIMS(1),DIMS(2),DIMS(3),
     &                                  ODIMS(1),ODIMS(2),ODIMS(3))
              END IF
            END IF
          ELSE
            IF(INTERP.EQ.1)THEN
              CALL TRANSFORM_3DNOROT1_R(DYNAMIC_MEM(IMPTR),
     &                                  DYNAMIC_MEM(OUTPTR),
     &                                  DIMS,NDIM,ODIMS,ONDIM,
     &                                  SHIFT,SAMPLE,
     &                                  MAGICPIX,MAGIC_FLOAT,
     &                                  DIMS(1),DIMS(2),DIMS(3),
     &                                  ODIMS(1),ODIMS(2),ODIMS(3))
              IF (ERR) CALL TRANSFORM_3DNOROT1_R(
     &                                  DYNAMIC_MEM(IEPTR),
     &                                  DYNAMIC_MEM(OEPTR),
     &                                  DIMS,NDIM,ODIMS,ONDIM,
     &                                  SHIFT,SAMPLE,
     &                                  MAGICPIX,ERR_VAL,
     &                                  DIMS(1),DIMS(2),DIMS(3),
     &                                  ODIMS(1),ODIMS(2),ODIMS(3))
              IF (QUAL) CALL TRANSFORM_3DROT1_B(
     &                                  DYNAMIC_MEM(IQPTR),
     &                                  DYNAMIC_MEM(OQPTR),
     &                                  DIMS,NDIM,ODIMS,ONDIM,
     &                                  SHIFT,SAMPLE,
     &                                  MAGICPIX,1,
     &                                  DIMS(1),DIMS(2),DIMS(3),
     &                                  ODIMS(1),ODIMS(2),ODIMS(3))
            ELSE IF(.NOT.BADPIX)THEN
              IF(INTERP.EQ.3)THEN
                CALL TRANSFORM_3DNOROT3_R(DYNAMIC_MEM(IMPTR),
     &                                    DYNAMIC_MEM(OUTPTR),
     &                                    DIMS,NDIM,ODIMS,ONDIM,
     &                                    MINCORN,SHIFT,SAMPLE,
     &                                    MAGICPIX,MAGIC_FLOAT,
     &                                    DIMS(1),DIMS(2),DIMS(3),
     &                                    ODIMS(1),ODIMS(2),ODIMS(3))
                IF (ERR) CALL TRANSFORM_3DNOROT3_R(
     &                                    DYNAMIC_MEM(IEPTR),
     &                                    DYNAMIC_MEM(OEPTR),
     &                                    DIMS,NDIM,ODIMS,ONDIM,
     &                                    MINCORN,SHIFT,SAMPLE,
     &                                    MAGICPIX,ERR_VAL,
     &                                    DIMS(1),DIMS(2),DIMS(3),
     &                                    ODIMS(1),ODIMS(2),ODIMS(3))
                IF (QUAL) CALL TRANSFORM_3DNOROT3_B(
     &                                    DYNAMIC_MEM(IQPTR),
     &                                    DYNAMIC_MEM(OQPTR),
     &                                    DIMS,NDIM,ODIMS,ONDIM,
     &                                    MINCORN,SHIFT,SAMPLE,
     &                                    MAGICPIX,1,
     &                                    DIMS(1),DIMS(2),DIMS(3),
     &                                    ODIMS(1),ODIMS(2),ODIMS(3))
              END IF
            ELSE IF(BADPIX)THEN
              IF(INTERP.EQ.3)THEN
                CALL TRANSFORM_3DNOROT3_RQ(DYNAMIC_MEM(IMPTR),
     &                                     DYNAMIC_MEM(OUTPTR),
     &                                     DIMS,NDIM,ODIMS,ONDIM,
     &                                     MINCORN,SHIFT,SAMPLE,
     &                                     MAGICPIX,MAGIC_FLOAT,
     &                                     DIMS(1),DIMS(2),DIMS(3),
     &                                     ODIMS(1),ODIMS(2),ODIMS(3))
                IF (ERR) CALL TRANSFORM_3DNOROT3_R(
     &                                     DYNAMIC_MEM(IEPTR),
     &                                     DYNAMIC_MEM(OEPTR),
     &                                     DIMS,NDIM,ODIMS,ONDIM,
     &                                     MINCORN,SHIFT,SAMPLE,
     &                                     MAGICPIX,ERR_VAL,
     &                                     DIMS(1),DIMS(2),DIMS(3),
     &                                     ODIMS(1),ODIMS(2),ODIMS(3))
              END IF
            END IF
          END IF
        END IF
      END IF
C
C   Set bad pixel flag if appropriate.
C
      IF((.NOT.QUAL).AND.(INT(MAGICPIX).GT.0)) THEN
        STRING='No. of pixels set to magic value = '
        DUMINT=ICH_ENCODE(STRING,MAGICPIX,36,0,NEXT)
        CALL DSA_WRUSER(STRING(:ICH_LEN(STRING))//'\\N')
        CALL NDP_SET_BAD_PIXEL('OUTPUT',.TRUE.,.TRUE.,STATUS)
      END IF
C
C   Tidy up and exit.
C
  500 CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END





      SUBROUTINE TRANSFORM_NEWDIMS
     &  (DIMS,NDIM,SHIFT,ANGLE,CENTRE,SAMPLE,ROTATE,ORTHO,
     &   ODIMS,ONELM,STAPIX)
C
      IMPLICIT NONE
C
C  Functions.
C
      INTEGER  ICH_ENCODE,ICH_LEN
C
C  Parameters.
C
      INTEGER   DIMS(10)
      INTEGER   NDIM
      REAL      SHIFT(6)
      REAL      ANGLE(6)
      REAL      CENTRE(6)
      REAL      SAMPLE(6)
      LOGICAL   ROTATE
      LOGICAL   ORTHO
      INTEGER   ODIMS(10)
      INTEGER   ONELM
      INTEGER   STAPIX(6)
C
C  Local variables.
C
      REAL      A(3,3)
      REAL      B(3,3)
      REAL      COORDS1(6)
      REAL      COORDS2(6)
      INTEGER   CORNERS(6,64)
      INTEGER   DUMINT
      REAL      DUMMY(6)
      INTEGER   ENDPIX(6)
      INTEGER   I
      INTEGER   LARGE
      INTEGER   SMALL
      INTEGER   J
      INTEGER   MODE
      INTEGER   NCORN
      REAL      NEWCORN(6,64)
      INTEGER   NEXT
      INTEGER   PIXELS(6,64)
      CHARACTER STRING*80
C
      INCLUDE 'NUMERIC_RANGES'
C
C   Initialize.
C
      DO I=1,NDIM
        DUMMY(I)=1.0
      END DO
C
C   Compute rotation matrices if required.
C
      IF(ROTATE.AND.NDIM.EQ.3)CALL TRANSFORM_3DMATRIX(ANGLE,CENTRE,A,B)
C
C   Set up array of corner pixels before transformation.
C
      CALL NDP_CORNERS(DIMS,NDIM,NCORN,CORNERS)
C
C   For each corner -
C
      MODE=1
      DO J=1,NCORN
        DO I=1,NDIM
          COORDS1(I)=REAL(CORNERS(I,J))
        END DO
C
C   - apply shift and/or rotation, but not resampling.
C
        IF(ROTATE)THEN
          IF(NDIM.EQ.2)THEN
            CALL TRANSFORM_2DTRANR
     &        (SHIFT,ANGLE,CENTRE,DUMMY,MODE,COORDS1,COORDS2)
          ELSE IF(NDIM.EQ.3)THEN
            CALL TRANSFORM_3DTRANR
     &        (SHIFT,ANGLE,CENTRE,DUMMY,MODE,COORDS1,COORDS2,A,B)
          END IF
        ELSE
          IF(NDIM.EQ.2)THEN
            CALL TRANSFORM_2DTRANNR
     &        (SHIFT,DUMMY,MODE,COORDS1,COORDS2)
          ELSE IF(NDIM.EQ.3)THEN
            CALL TRANSFORM_3DTRANNR
     &        (SHIFT,DUMMY,MODE,COORDS1,COORDS2)
          END IF
        END IF
C
C   - if the rotation is orthogonal, round new corners to the nearest pixel.
C     Otherwise, round up to the next pixel (or down if negative) so as not to
C     lose any data at the extremities.
C
        DO I=1,NDIM
          NEWCORN(I,J)=COORDS2(I)
C
          IF(ORTHO)THEN
            PIXELS(I,J)=NINT(NEWCORN(I,J))
          ELSE
            IF(NEWCORN(I,J).LT.0.0 .AND.
     &        NEWCORN(I,J).NE.INT(NEWCORN(I,J)))THEN
              PIXELS(I,J)=INT(NEWCORN(I,J)-1.0)
            ELSE IF(NEWCORN(I,J).GT.0.0 .AND.
     &        NEWCORN(I,J).NE.INT(NEWCORN(I,J)))THEN
              PIXELS(I,J)=INT(NEWCORN(I,J)+1.0)
            ELSE
              PIXELS(I,J)=INT(NEWCORN(I,J))
            END IF
          END IF
C
        END DO
      END DO
C
C   Sort out the minimum and maximum pixels of the new corners.
C
      DO I=1,NDIM
        SMALL=PIXELS(I,1)
        LARGE=PIXELS(I,1)
        DO J=1,NCORN
          SMALL=MIN(SMALL,PIXELS(I,J))
          LARGE=MAX(LARGE,PIXELS(I,J))
        END DO
        STAPIX(I)=SMALL
        ENDPIX(I)=LARGE
      END DO
C
C   Display results.
C
      CALL DSA_WRUSER(' \\N')
      CALL DSA_WRUSER('After shifting and/or rotation, the image is ')
      CALL DSA_WRUSER('bounded by the following pixels:\\N')
      DO I=1,NDIM
        STRING='  Axis '
        DUMINT=ICH_ENCODE(STRING,REAL(I),8,0,NEXT)
        STRING(NEXT:)=' start pixel = '
        DUMINT=ICH_ENCODE(STRING,REAL(STAPIX(I)),NEXT+15,0,NEXT)
        CALL DSA_WRUSER(STRING(:ICH_LEN(STRING))//'\\N')
        STRING='  Axis '
        DUMINT=ICH_ENCODE(STRING,REAL(I),8,0,NEXT)
        STRING(NEXT:)=' end pixel   = '
        DUMINT=ICH_ENCODE(STRING,REAL(ENDPIX(I)),NEXT+15,0,NEXT)
        CALL DSA_WRUSER(STRING(:ICH_LEN(STRING))//'\\N')
      END DO
C
C   Calculate new dimensions and number of elements after any resampling and
C   display results.
C
      ONELM=1
      DO I=1,NDIM
        ODIMS(I)=(ABS(ENDPIX(I)-STAPIX(I))+1)*SAMPLE(I)
        ONELM=ONELM*ODIMS(I)
      END DO
      CALL DSA_WRUSER(' \\N')
      STRING='After resampling, the image dimensions are ['
      NEXT=44
      DO I=1,NDIM
        DUMINT=ICH_ENCODE(STRING,REAL(ODIMS(I)),NEXT+1,0,NEXT)
        IF(I.LT.NDIM)STRING(NEXT:)=','
      END DO
      STRING(NEXT:)=']'
      CALL DSA_WRUSER(STRING(:ICH_LEN(STRING))//'\\N')
      CALL DSA_WRUSER(' \\N')
C
C   See whether the new dimensions are acceptable.
C
c     CALL PAR_RDKEY('ACCEPT',.TRUE.,ACCEPT)
C
C   - if not, get start and end pixels from user.
C
c     IF(.NOT.ACCEPT)THEN
c       DO I=1,NDIM
c         VMIN(I)=REAL(MIN_WNT)
c         VMAX(I)=REAL(MAX_INT)
c       END DO
c       CALL NDP_PAR_RDARY('STAPIX',VMIN,VMAX,'N',' ',NDIM,6,DUMARR)
c       DO I=1,NDIM
c         STAPIX(I)=INT(DUMARR(I))
c       END DO
c       CALL NDP_PAR_RDARY('ENDPIX',VMIN,VMAX,'N',' ',NDIM,6,DUMARR)
c       DO I=1,NDIM
c         ENDPIX(I)=INT(DUMARR(I))
c       END DO
C
C   - calculate dimensions and number of elements.
C
c       ONELM=1
c       DO I=1,NDIM
c         ODIMS(I)=(ABS(ENDPIX(I)-STAPIX(I))+1)*SAMPLE(I)
c         ONELM=ONELM*ODIMS(I)
c       END DO
c     END IF
C
      END





      SUBROUTINE TRANSFORM_2DTRANNR(SHIFT,SAMPLE,MODE,COORDS1,COORDS2)
C
C   Mode =  1 to obtain COORDS2 after transformation applied to COORDS1.
C   Mode = -1 to obtain COORDS2 which, after transformation, gave COORDS1.
C
      IMPLICIT NONE
C
      INTEGER MODE
      REAL    SHIFT(6),SAMPLE(6),COORDS1(6),COORDS2(6)
C
      IF(MODE.EQ.1)THEN
        COORDS2(1)=(COORDS1(1)+SHIFT(1))*SAMPLE(1)
        COORDS2(2)=(COORDS1(2)+SHIFT(2))*SAMPLE(2)
      ELSE
        COORDS2(1)=(COORDS1(1)/SAMPLE(1))-SHIFT(1)
        COORDS2(2)=(COORDS1(2)/SAMPLE(2))-SHIFT(2)
      END IF
C
      END





      SUBROUTINE TRANSFORM_2DTRANR
     &  (SHIFT,ANGLE,CENTRE,SAMPLE,MODE,COORDS1,COORDS2)
C
C   Mode =  1 to obtain COORDS2 after transformation applied to COORDS1.
C   Mode = -1 to obtain COORDS2 which, after transformation, gave COORDS1.
C
      IMPLICIT NONE
C
      INTEGER MODE
      REAL    SHIFT(6),ANGLE(6),CENTRE(6),SAMPLE(6),
     &        COORDS1(6),COORDS2(6)
C
      REAL    C,S,TEMP1(6),TEMP2(6)
C
      C=COS(ANGLE(1))
      S=SIN(ANGLE(1))
C
      IF(MODE.EQ.1)THEN
        TEMP1(1)=COORDS1(1)-CENTRE(1)+SHIFT(1)
        TEMP1(2)=COORDS1(2)-CENTRE(2)+SHIFT(2)
        TEMP2(1) = C*TEMP1(1) - S*TEMP1(2)
        TEMP2(2) = S*TEMP1(1) + C*TEMP1(2)
        COORDS2(1)=(TEMP2(1)+CENTRE(1))*SAMPLE(1)
        COORDS2(2)=(TEMP2(2)+CENTRE(2))*SAMPLE(2)
C
      ELSE
        TEMP1(1)=(COORDS1(1)/SAMPLE(1))-CENTRE(1)
        TEMP1(2)=(COORDS1(2)/SAMPLE(2))-CENTRE(2)
        TEMP2(1) = C*TEMP1(1) + S*TEMP1(2)
        TEMP2(2) = -S*TEMP1(1) + C*TEMP1(2)
        COORDS2(1)=TEMP2(1)+CENTRE(1)-SHIFT(1)
        COORDS2(2)=TEMP2(2)+CENTRE(2)-SHIFT(2)
      END IF
C
      END





      SUBROUTINE TRANSFORM_3DTRANNR
     &  (SHIFT,SAMPLE,MODE,COORDS1,COORDS2)
C
C   Mode =  1 to obtain COORDS2 after transformation applied to COORDS1.
C   Mode = -1 to obtain COORDS2 which, after transformation, gave COORDS1.
C
      IMPLICIT NONE
C
      INTEGER MODE
      REAL    SHIFT(6),SAMPLE(6),COORDS1(6),COORDS2(6)
C
      IF(MODE.EQ.1)THEN
        COORDS2(1)=(COORDS1(1)+SHIFT(1))*SAMPLE(1)
        COORDS2(2)=(COORDS1(2)+SHIFT(2))*SAMPLE(2)
        COORDS2(3)=(COORDS1(3)+SHIFT(3))*SAMPLE(3)
      ELSE
        COORDS2(1)=(COORDS1(1)/SAMPLE(1))-SHIFT(1)
        COORDS2(2)=(COORDS1(2)/SAMPLE(2))-SHIFT(2)
        COORDS2(3)=(COORDS1(3)/SAMPLE(3))-SHIFT(3)
      END IF
C
      END





      SUBROUTINE TRANSFORM_3DTRANR
     &  (SHIFT,ANGLE,CENTRE,SAMPLE,MODE,COORDS1,COORDS2,A,B)
C
C   Mode =  1 to obtain COORDS2 after transformation applied to COORDS1.
C   Mode = -1 to obtain COORDS2 which, after transformation, gave COORDS1.
C
      IMPLICIT NONE
C
      INTEGER MODE
      REAL    SHIFT(6),ANGLE(6),CENTRE(6),SAMPLE(6),
     &        COORDS1(6),COORDS2(6)
C
      INTEGER I
      REAL    A(3,3),B(3,3),TEMP1(6),TEMP2(6)
C
      DO I=1,3
        TEMP2(I)=0.0
      END DO
C
      IF(MODE.EQ.1)THEN
        TEMP1(1)=COORDS1(1)-CENTRE(1)+SHIFT(1)
        TEMP1(2)=COORDS1(2)-CENTRE(2)+SHIFT(2)
        TEMP1(3)=COORDS1(3)-CENTRE(3)+SHIFT(3)
        DO I=1,3
          TEMP2(1) = TEMP2(1) + A(1,I)*TEMP1(I)
          TEMP2(2) = TEMP2(2) + A(2,I)*TEMP1(I)
          TEMP2(3) = TEMP2(3) + A(3,I)*TEMP1(I)
        END DO
        COORDS2(1)=(TEMP2(1)+CENTRE(1))*SAMPLE(1)
        COORDS2(2)=(TEMP2(2)+CENTRE(2))*SAMPLE(2)
        COORDS2(3)=(TEMP2(3)+CENTRE(3))*SAMPLE(3)
C
      ELSE
        TEMP1(1)=(COORDS1(1)/SAMPLE(1))-CENTRE(1)
        TEMP1(2)=(COORDS1(2)/SAMPLE(2))-CENTRE(2)
        TEMP1(3)=(COORDS1(3)/SAMPLE(3))-CENTRE(3)
        DO I=1,3
          TEMP2(1) = TEMP2(1) + B(1,I)*TEMP1(I)
          TEMP2(2) = TEMP2(2) + B(2,I)*TEMP1(I)
          TEMP2(3) = TEMP2(3) + B(3,I)*TEMP1(I)
        END DO
        COORDS2(1)=TEMP2(1)+CENTRE(1)-SHIFT(1)
        COORDS2(2)=TEMP2(2)+CENTRE(2)-SHIFT(2)
        COORDS2(3)=TEMP2(3)+CENTRE(3)-SHIFT(3)
      END IF
C
      END





      SUBROUTINE TRANSFORM_3DMATRIX(ANGLE,CENTRE,A,B)
c
c   Calculates rotation matrices A (forward) and B (reverse) for 3-D rotation.
c
      implicit none
c
      real    angle(6),centre(6),a(3,3),b(3,3)
c
      integer i
      real    c(3),s(3)
c
      do i=1,3
        s(i)=sin(angle(i))
        c(i)=cos(angle(i))
      end do
c
      a(1,1) = c(1)*c(2)
      a(1,2) = -s(1)*c(3) + c(1)*s(2)*s(3)
      a(1,3) = s(1)*s(3) + c(1)*s(2)*c(3)

      a(2,1) = s(1)*c(2)
      a(2,2) = c(1)*c(3) + s(1)*s(2)*s(3)
      a(2,3) = -c(1)*s(3) + s(1)*s(2)*c(3)

      a(3,1) = -s(2)
      a(3,2) = c(2)*s(3)
      a(3,3) = c(2)*c(3)

      b(1,1) = c(1)*c(2)
      b(1,2) = s(1)*c(2)
      b(1,3) = -s(2)

      b(2,1) = -s(1)*c(3) + c(1)*s(2)*s(3)
      b(2,2) = c(1)*c(3) + s(1)*s(2)*s(3)
      b(2,3) = c(2)*s(3)

      b(3,1) = s(1)*s(3) + c(1)*s(2)*c(3)
      b(3,2) = -c(1)*s(3) + s(1)*s(2)*c(3)
      b(3,3) = c(2)*c(3)
c
      end

