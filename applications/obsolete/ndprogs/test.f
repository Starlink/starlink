      SUBROUTINE TEST
C+
C
C   -------
C   T E S T
C   -------
C
C   Description
C   -----------
C   Creates a new structure containing a data array and axes. The data
C   array may be of type BYTE, SHORT, or FLOAT, and may be filled with a
C   specified value or with magic values. Error arrays and/or quality arrays
C   can also be included. It will the fill the data array with a test
c   pattern
C
C
C   Scope of program
C   ----------------
C   - Images of up to six dimensions may be created.
C   - Data array types BYTE, SHORT, and FLOAT may be created.
C   - Subsetting not supported since not relevant.
C   - Magic values supported (a data array of magic values may be created).
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
C   OUTPUT  Name of the structure containing the output image. (character)
C           (prompted for).
C
C   NDIM    Number of dimensions in the image. (integer)(prompted for).
C
C   SIZE    Size of each dimension of the image. (integer, array)
C           (prompted for).
C
C   AXKEY   Keys for axes to be calibrated. (integer, array)(prompted for).
C
C   AXSTART Start calibration value for each axis. (real, array)
C           (prompted for).
C
C   AXEND   End calibration value for each axis. (real, array)
C           (prompted for).
C
C   AXLOG   Keys for axes to be calibrated logarithmically.
C           (integer, array)(prompted for).
C
C   VALUE   Value to be assigned to every data array element. (real)
C           (prompted for).
C
C   EXTRA   Request for Quality arrays,error arrays or Magic values
C           to be part of the structure.(character, prompted)
C
C   DTYPE   Data type (float, byte or short). (character, prompted)
C
C   ERRVAL  Value to be assigned to the error array (float, prompted)
C
C   QVAL    Value to be assigned to the quality array (byte, prompted)
C
C   Keywords
C   --------
C   AXES    Instruction to calibrate the axes of the image.
C
C
C   Propagation of data structure
C   -----------------------------
C   Not relevant.
C
C
C   Method
C   ------
C   - The dimensions, data type and the presence of magic values, quality
C     arrays and error arrays are obtained from the user.
C   - If it is required to calibrate the axes, the structure variable for
C     axis calibration is set, and the axis number(s), start and end
C     value(s), and linear/logarithmic flag(s) are prompted for. The axes
C     are then calibrated.
C   - Unless it is required to assign the magic value to every pixel, a
C     specific value is prompted for. The data array is filled with the
C     dummy value.
C   - The bad data flag is set if the magic value was used.
C   - A quality or error array is created if requested.
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library DSA:
C      DSA_CFILLB
C      DSA_CFILLF
C      DSA_CFILLS
C      DSA_CLOSE
C      DSA_MAP_AXIS_DATA
C      DSA_MAP_DATA
C      DSA_MAP_ERRORS
C      DSA_MAP_QUALITY
C      DSA_OPEN
C      DSA_OUTPUT
C      DSA_SIMPLE_OUTPUT
C      DSA_WRUSER
C
C   Library DYN:
C      DYN_ELEMENT
C
C   Library ICH:
C      ICH_CI
C      ICH_DELIM
C      ICH_FOLD
C      ICH_LEN
C
C   Library NDP:
C      NDP_PAR_RDARY
C      NDP_SET_AXES
C      NDP_SET_BAD_PIXEL
C
C   Library PAR:
C      PAR_GIVEN
C      PAR_RDKEY
C
C
C   Internal subroutines called
C   ---------------------------
C   TEST_FILL_<T>
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
C   - Prompt for the label and units of each axis.
C
C
C   Author/s
C   --------
C   Julian Gold  RGO  (CAVAD::GOLDJIL or GOLDJIL@UK.AC.CAM.AST-STAR)
C
C   History
C   -------
C   09-MAR-1992  Original program (based on DUMMY)
C   10-DEC-1992  Unix version.
C   06-OCT-1994  Removed lots of unused variables. (GJP)
C
C
C+-----------------------------------------------------------------------------
C
      IMPLICIT NONE
C
C     Functions used
C
      CHARACTER ICH_CI*8
      INTEGER   DYN_ELEMENT,ICH_LEN
      INTEGER   ICH_DELIM
C
C     Local variables
C
      INTEGER   ADDRESS            ! Address of dynamic memory element
      LOGICAL   AXES               ! Instruction to calibrate axis arrays
      LOGICAL   BYTE               ! Instruction to create BYTE data array
      INTEGER   DIMS(10)           ! Dimensions of image
      INTEGER   DUMINT             ! INTEGER dummy variable
      REAL      DUMREAL            ! REAL dummy variable
      INTEGER   EPTR               ! Dynamic pointer to error data
      LOGICAL   ERR   		   ! Flag for error array creation
      INTEGER   ESLOT              ! Slot number for error array
      CHARACTER EXTRA*8            ! Quality/error/magic value string
      LOGICAL   FLOAT              ! Instruction to create FLOAT data array
      INTEGER   I                  ! Loop counter
      CHARACTER INFO*32            ! DSA_SIMPLE_OUTPUT request string
      LOGICAL   MAGIC              ! Instruction to use the magic value
      INTEGER   NDIM               ! Dimensions of image
      INTEGER   NELM               ! Number of elements in image
      INTEGER   OSLOT              ! Map slot number for data array
      INTEGER   OUTPTR             ! Dynamic pointer to data array
      CHARACTER OUTPUT*64          ! Name of output image structure
      INTEGER   QPTR               ! Dynamic pointer to quality/error array
      INTEGER   QSLOT              ! Map slot number for quality/error array
      LOGICAL   QUAL		   ! Flag for quality array creation
      BYTE      QVAL               ! Value for quality array
      LOGICAL   SHORT              ! Instruction to create SHORT data array
      REAL      SIZE(6)            ! Dimensions of image
      INTEGER   STATUS             ! Status code
      CHARACTER TYPE*8             ! Data array type
      REAL      VMIN(6)            ! Minimum values for NDP_PAR_RDARY
      REAL      VMAX(6)            ! Maximum values for NDP_PAR_RDARY
C
      INCLUDE 'DYNAMIC_MEMORY'
      INCLUDE 'MAGIC_VALUES'
      INCLUDE 'NUMERIC_RANGES'
C
C     Initialize
C
      STATUS=0
C
C     Open DSA system
C
      CALL DSA_OPEN(STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C     Get output structure name and open it
C
      CALL PAR_RDCHAR('OUTPUT',' ',OUTPUT)
      CALL DSA_OUTPUT('OUTPUT','OUTPUT',' ',1,1,STATUS)
      IF (STATUS .NE. 0) GO TO 500
C
C     Get number of dimensions
C
      CALL PAR_RDVAL('NDIM',1.0,6.0,3.0,' ',DUMREAL)
      NDIM=INT(DUMREAL)
C
C     Get image dimensions and compute number of elements
C
      DUMINT=MAX_INT
      DO I=1,6
        VMIN(I)=1.0
        VMAX(I)=REAL(DUMINT)
      END DO
      CALL NDP_PAR_RDARY('SIZE',VMIN,VMAX,'N',' ',NDIM,6,SIZE)
      NELM=1
      DO I=1,NDIM
        DIMS(I)=INT(SIZE(I))
        NELM=NELM*DIMS(I)
      END DO
C
C     Get data type
C
      SHORT=.FALSE.
      FLOAT=.FALSE.
      BYTE=.FALSE.
      CALL PAR_RDCHAR('DTYPE','SHORT',TYPE)
      CALL ICH_FOLD(TYPE)
      IF (TYPE(1:1) .EQ. 'B') THEN
        BYTE = .TRUE.
        TYPE = 'BYTE'
      ELSE IF (TYPE(1:1) .EQ. 'F') THEN
        FLOAT = .TRUE.
        TYPE = 'FLOAT'
      ELSE
        SHORT = .TRUE.
        TYPE = 'SHORT'
      END IF
      CALL DSA_WRUSER('Data will be of type ')
      CALL DSA_WRUSER(TYPE(:ICH_LEN(TYPE))//'.\\N')
C
C     Determine if quality/error arrays or magic values are to be used
C
      MAGIC = .FALSE.
      QUAL  = .FALSE.
      ERR   = .FALSE.
      CALL PAR_RDCHAR('EXTRA',' ',EXTRA)
      CALL ICH_FOLD(EXTRA)
      IF (ICH_DELIM(EXTRA,1,'M') .NE. 0) MAGIC = .TRUE.
      IF (ICH_DELIM(EXTRA,1,'Q') .NE. 0) QUAL = .TRUE.
      IF (ICH_DELIM(EXTRA,1,'E') .NE. 0) ERR = .TRUE.
C
C     Now create the structure
C
      INFO = 'D'
      IF (QUAL) INFO = INFO//',Q'
      IF (ERR)  INFO = INFO//',E'
      DO I=1,NDIM
        INFO = INFO//',A'//ICH_CI(I)
      END DO
      CALL DSA_SIMPLE_OUTPUT('OUTPUT',INFO,TYPE,NDIM,DIMS,STATUS)
C
C     Map OUTPUT data array
C
      CALL DSA_MAP_DATA('OUTPUT','WRITE',TYPE,ADDRESS,OSLOT,STATUS)
      IF(STATUS.NE.0)GO TO 500
      OUTPTR=DYN_ELEMENT(ADDRESS)
C
C     Find out whether axes are to be calibrated
C
      AXES=.FALSE.
      CALL PAR_RDKEY('AXES',.FALSE.,AXES)
C
C     - if so, set structure variable for axis calibration
C
      IF(AXES)THEN
C
C     - calibrate axes
C
        CALL NDP_SET_AXES('OUTPUT',DIMS,NDIM,STATUS)
        IF(STATUS.NE.0)GO TO 500
      END IF
C
C     Map and set up quality or error array if needed
C
      IF(QUAL)THEN
        QVAL = INT(DUMREAL)
        CALL DSA_WRUSER('Creating quality array...\\n')
        CALL DSA_MAP_QUALITY('OUTPUT','WRITE','BYTE',
     :                       ADDRESS,QSLOT,STATUS)
        IF (STATUS .NE. 0) GO TO 500
        QPTR = DYN_ELEMENT(ADDRESS)
        CALL DSA_CFILLB(NELM,0,DYNAMIC_MEM(QPTR))
      END IF
      IF(ERR)THEN
        CALL DSA_WRUSER('Creating error array...\\n')
        CALL DSA_MAP_ERRORS('OUTPUT','WRITE',TYPE,
     :                      ADDRESS,ESLOT,STATUS)
        IF (STATUS .NE. 0) GO TO 500
        EPTR = DYN_ELEMENT(ADDRESS)
        IF (SHORT) THEN
          CALL DSA_CFILLS(NELM,1,DYNAMIC_MEM(EPTR))
        ELSE IF (BYTE) THEN
          CALL DSA_CFILLB(NELM,1,DYNAMIC_MEM(EPTR))
        ELSE
          CALL DSA_CFILLF(NELM,1.0,DYNAMIC_MEM(EPTR))
        END IF
      END IF
C
C     Set data array values
C
      CALL DSA_WRUSER('Creating data array...\\N')
      IF(SHORT)THEN
        CALL TEST_FILL_W(NELM,DYNAMIC_MEM(OUTPTR))
      ELSE IF(FLOAT)THEN
        CALL TEST_FILL_R(NELM,DYNAMIC_MEM(OUTPTR))
      ELSE IF(BYTE)THEN
        CALL TEST_FILL_B(NELM,DYNAMIC_MEM(OUTPTR))
      END IF
C
C     Set bad pixel flag if appropriate
C
      CALL NDP_SET_BAD_PIXEL('OUTPUT',.FALSE.,MAGIC,STATUS)
      CALL DSA_WRUSER('Done.\\n')
C
C     Tidy up and exit
C
500   CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END

