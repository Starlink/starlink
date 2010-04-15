      SUBROUTINE DUMMY
C+
C
C   ---------
C   D U M M Y
C   ---------
C
C   Description
C   -----------
C   Creates a new structure containing a data array and axes. The data
C   array may be of type BYTE, SHORT, or FLOAT, and may be filled with a
C   specified value or with magic values. Error arrays and/or quality arrays
C   can also be included.
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
C   Library GEN:
C      GEN_CFILL
C      GEN_FILL
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
C      PAR_RDKEY
C      PAR_RDVAL
C
C
C   Internal subroutines called
C   ---------------------------
C   DUMMY_FILL_W
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
C   Nick Fuller  RGO  (RGVAD::NMJF or NMJF@UK.AC.RGO.STAR)
C   Julian Gold  RGO  (CAVAD::GOLDJIL or GOLDJIL@UK.AC.CAM.AST-STAR)
C
C   History
C   -------
C   01-FEB-1989   - Original program. (NMJF)
C   19-APR-1991   - Quality and error array handling added. (GOLDJIL)
C   04-JLY-1991   - Data type parameter acquisition re-written to specify
C                   by entering a single character.
C                   Removal of dependence on .DEF file constructs.
C                   Ability to specify any desired quality/error value.
C                   General tidy-up. (GOLDJIL)
C   18-MAR-1992   - Changed to link with shareable library. (GOLDJIL)
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
      REAL      PMAX               ! Maximum parameter value for PAR_RDVAL
      REAL      PMIN               ! Minimum parameter value for PAR_RDVAL
      INTEGER   QPTR               ! Dynamic pointer to quality/error array
      INTEGER   QSLOT              ! Map slot number for quality/error array
      LOGICAL   QUAL		   ! Flag for quality array creation
      BYTE      QVAL               ! Value for quality array
      LOGICAL   SHORT              ! Instruction to create SHORT data array
      REAL      SIZE(6)            ! Dimensions of image
      INTEGER   STATUS             ! Status code
      CHARACTER TYPE*8             ! Data array type
      BYTE      VAL_BYTE           ! BYTE pixel value
      INTEGER*2 VAL_SHORT          ! INTEGER*2 pixel value
      REAL      VAL_FLOAT          ! REAL pixel value
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
C     Get pixel value for data array
C
      IF(.NOT.MAGIC)THEN
        IF(SHORT)THEN
          PMIN=REAL(MIN_SHORT)
          PMAX=REAL(MAX_SHORT)
        ELSE IF(FLOAT)THEN
          PMIN=MIN_FLOAT
          PMAX=MAX_FLOAT
        ELSE IF(BYTE)THEN
          PMIN=REAL(MIN_BYTE)
          PMAX=REAL(MAX_BYTE)
        END IF
        CALL PAR_RDVAL('VALUE',PMIN,PMAX,0.0,' ',DUMREAL)
        VAL_SHORT=INT(DUMREAL)
        VAL_FLOAT=DUMREAL
        VAL_BYTE=INT(DUMREAL)
      ELSE
        VAL_SHORT=MAGIC_SHORT
        VAL_FLOAT=MAGIC_FLOAT
        VAL_BYTE=MAGIC_BYTE
      END IF
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
        CALL PAR_RDVAL('QVAL',0.0,PMAX,0.0,' ',DUMREAL)
        QVAL = INT(DUMREAL)
        CALL DSA_WRUSER('Creating quality array...\\n')
        CALL DSA_MAP_QUALITY('OUTPUT','WRITE','BYTE',
     :                       ADDRESS,QSLOT,STATUS)
        IF (STATUS .NE. 0) GO TO 500
        QPTR = DYN_ELEMENT(ADDRESS)
        CALL GEN_FILL(NELM,QVAL,DYNAMIC_MEM(QPTR))
      END IF
      IF(ERR)THEN
        CALL PAR_RDVAL('ERRVAL',PMIN,PMAX,0.0,' ',DUMREAL)
        CALL DSA_WRUSER('Creating error array...\\n')
        CALL DSA_MAP_ERRORS('OUTPUT','WRITE',TYPE,
     :                      ADDRESS,ESLOT,STATUS)
        IF (STATUS .NE. 0) GO TO 500
        EPTR = DYN_ELEMENT(ADDRESS)
        IF (SHORT) THEN
          CALL DUMMY_FILL_W(NELM,INT(DUMREAL),DYNAMIC_MEM(EPTR))
        ELSE IF (BYTE) THEN
          CALL GEN_FILL(NELM,INT(DUMREAL),DYNAMIC_MEM(EPTR))
        ELSE
          CALL GEN_CFILL(1,NELM,DUMREAL,DYNAMIC_MEM(EPTR))
        END IF
      END IF
C
C     Set data array values
C
      CALL DSA_WRUSER('Creating data array...\\N')
      IF(SHORT)THEN
        CALL DUMMY_FILL_W(NELM,VAL_SHORT,DYNAMIC_MEM(OUTPTR))
      ELSE IF(FLOAT)THEN
        CALL GEN_CFILL(1,NELM,VAL_FLOAT,DYNAMIC_MEM(OUTPTR))
      ELSE IF(BYTE)THEN
        CALL GEN_FILL(NELM,VAL_BYTE,DYNAMIC_MEM(OUTPTR))
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


      subroutine dummy_fill_w(nelm,value,array)
c
c  Routine to fill an integer*2 array with a particular value.
c
      integer    nelm
      integer*2  value,array(nelm)
c
      integer    i
c
      do i = 1,nelm
        array(i) = value
      end do
      return
      end
