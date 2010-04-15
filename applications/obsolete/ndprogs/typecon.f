      SUBROUTINE TYPECON
C+
C
C   -------------
C   T Y P E C O N
C   -------------
C
C   Description
C   -----------
C   Converts the data array of an image from SHORT to FLOAT, or from FLOAT
C   to SHORT.
C
C
C   Scope of program
C   ----------------
C   - Images of up to six dimensions accepted.
C   - Data array types BYTE, SHORT, INT, FLOAT, and DOUBLE supported.
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
C   IMAGE   Name of the structure containing the input image. (character)
C           (prompted for).
C
C   OUTPUT  Name of the structure containing the output image. (character)
C           (prompted for).
C
C
C   Keywords
C   --------
C   SHORT   Instruction to create a data array of type SHORT.
C
C   FLOAT   Instruction to create a data array of type FLOAT.
C
C
C   Propagation of data structure
C   -----------------------------
C   - All standard objects are copied from IMAGE.
C   - Data array is modified.
C
C
C   Method
C   ------
C   - The IMAGE structure is tested for the bad data flag, quality and error
C     arrays. If the bad data flag is found
C     and non-zero, magic values are assumed to be present and are left in
C     the data, unless a quality array is present.
C   - The instruction to convert to either SHORT or FLOAT is either deduced
C     from the data type of IMAGE, or prompted for.
C   - An empty output structure is opened. A call to DSA_SIMPLE_OUTPUT then
C     builds the output file of the required type.
C   - The axis structure (if present) is copied to OUTPUT.
C   - The data, quality and error arrays (where applicable) are copied to
C     OUTPUT.
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library DSA:
C      DSA_AXIS_SIZE
C      DSA_CLOSE
C      DSA_DATA_SIZE
C      DSA_INPUT
C      DSA_MAP_DATA
C      DSA_MAP_ERRORS
C      DSA_MAP_QUALITY
C      DSA_OPEN
C      DSA_OUTPUT
C      DSA_RESHAPE_AXIS
C      DSA_SEEK_AXIS
C      DSA_SEEK_ERRORS
C      DSA_SEEK_QUALITY
C      DSA_SET_FLAGGED_VALUES
C      DSA_SIMPLE_OUTPUT
C      DSA_TYPESIZE
C      DSA_USE_FLAGGED_VALUES
C      DSA_WRUSER
C
C   Library DYN:
C      DYN_ELEMENT
C
C   Library GEN:
C      GEN_MOVE
C
C   Library ICH:
C      ICH_CI
C      ICH_LEN
C
C   Library NDP:
C      NDP_GET_IMAGE_INFO
C
C   Library PAR:
C      PAR_GIVEN
C      PAR_RDKEY
C
C
C   Internal subroutines called
C   ---------------------------
C   None.
C
C
C   INCLUDE statements
C   ------------------
C   INCLUDE 'DYNAMIC_MEMORY'
C   INCLUDE 'MAGIC_VALUES'
C   INCLUDE 'NUMERIC_RANGES'
C
C   Extensions to FORTRAN77
C   -----------------------
C   END DO / IMPLICIT NONE / INCLUDE / Names > 6 characters
C
C
C   Possible future upgrades
C   ------------------------
C
C   Author/s
C   --------
C   Nick Fuller  RGO  (RGVAD::NMJF or NMJF@UK.AC.RGO.STAR)
C   Julian Gold  RGO  (CAVAD::GOLDJIL or GOLDJIL@UK.AC.CAM.AST-STAR)
C
C   History
C   -------
C   01-FEB-1989   - Original program
C   14-JUN-1990   - Fixed to set magic value flag if it's needed  (JRL)
C   18-JUN-1990   - Fixed bug which caused the program to die if axis arrays
C                   didn't exist.  (JRL)
C   11-NOV-1991   - Got rid of nasty array coercion, added quality and error
C                   handling, tidied up (and all before lunchtime). (GOLDJIL)
C   10-DEC-1992   - Unix version. (GOLDJIL)
C   06-OCT-1994   - Removed lots of unused variables. (GJP)
C
C
C+-----------------------------------------------------------------------------
C
      IMPLICIT NONE
C
C  Functions.
C
      INTEGER   DSA_TYPESIZE,DYN_ELEMENT
      CHARACTER ICH_CI*8
      LOGICAL   PAR_GIVEN
C
C  Local variables.
C
      INTEGER   ADDRESS            ! Address of dynamic memory element
      LOGICAL   AXIS(10)           ! Flags presence of an axis array
      LOGICAL   BADPIX             ! Value of bad pixel flag
      INTEGER   DIMS(10)           ! Dimensions of image
      INTEGER   DIMSA(10)          ! Dimensions of axis arrays
      INTEGER   ELEM               ! Element size in bytes
      LOGICAL   ERR                ! Error array presence flag
      LOGICAL   FLOAT              ! Instruction to create FLOAT data array
      INTEGER   I                  ! Loop counter
      INTEGER   IEPTR              ! Dyn. pointer to IMAGE error array
      INTEGER   IESLOT             ! Slot number for IMAGE error array
      INTEGER   IMPTR              ! Dynamic pointer to IMAGE array
      INTEGER   IQPTR              ! Dyn. pointer to IMAGE quality array
      INTEGER   IQSLOT             ! Slot number for IMAGE quality array
      INTEGER   ISLOT              ! Map slot number for IMAGE array
      INTEGER   NDIM               ! Dimensions of image
      INTEGER   NDIMA              ! Dimensions of axis arrays
      INTEGER   NELM               ! Number of elements in image
      INTEGER   NELMA              ! Number of elements in an axis array
      INTEGER   OEPTR              ! Dyn. pointer to OUTPUT error array
      INTEGER   OESLOT             ! Slot number for OUTPUT error array
      CHARACTER OPTIONS*32         ! DSA_SIMPLE_OUTPUT option string
      INTEGER   OQPTR              ! Dyn. pointer to OUTPUT quality array
      INTEGER   OQSLOT             ! Slot number for OUTPUT quality array
      INTEGER   OSLOT              ! Map slot number for OUTPUT array
      INTEGER   OUTPTR             ! Dynamic pointer to OUTPUT array
      LOGICAL   QUAL               ! Quality array presence flag
      LOGICAL   SHORT              ! Instruction to create SHORT data array
      INTEGER   STATUS             ! Status code
      CHARACTER TYPE*8             ! Data array type
C
      INTEGER   NEW_FILE,NO_DATA
      PARAMETER (NEW_FILE=1,NO_DATA=1)
C
      INCLUDE 'DYNAMIC_MEMORY'
      INCLUDE 'MAGIC_VALUES'
      INCLUDE 'NUMERIC_RANGES'
C
C  Initialize.
C
      STATUS=0
C
C  Open DSA system.
C
      CALL DSA_OPEN(STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C  Open file for IMAGE.
C
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C  Get information about IMAGE.
C
      CALL NDP_GET_IMAGE_INFO('IMAGE',.TRUE.,.FALSE.,TYPE,BADPIX,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C  Get dimensions of IMAGE data array.
C
      CALL DSA_DATA_SIZE('IMAGE',3,NDIM,DIMS,NELM,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C  Get axis, quality and error array information
C
      CALL DSA_SEEK_QUALITY('IMAGE',QUAL,STATUS)
      CALL DSA_SEEK_ERRORS('IMAGE',ERR,STATUS)
      DO I=1,NDIM
        CALL DSA_SEEK_AXIS('IMAGE',I,AXIS(I),STATUS)
      END DO
      IF (STATUS.NE.0) GO TO 500
C
C  Get SHORT or FLOAT keyword.
C
      IF(PAR_GIVEN('SHORT'))THEN
        CALL PAR_RDKEY('SHORT',.TRUE.,SHORT)
        IF(PAR_GIVEN('FLOAT'))THEN
          CALL PAR_RDKEY('FLOAT',.FALSE.,FLOAT)
          IF(FLOAT.AND.SHORT)THEN
            CALL DSA_WRUSER
     &              ('SHORT and FLOAT are mutually exclusive.\\N')
            GO TO 500
          END IF
        END IF
        FLOAT=.NOT.SHORT
      ELSE IF(PAR_GIVEN('FLOAT'))THEN
        CALL PAR_RDKEY('FLOAT',.FALSE.,FLOAT)
C
C  If possible, deduce conversion from the data type of IMAGE.
C
      ELSE
        IF(TYPE.EQ.'SHORT')THEN
          FLOAT=.TRUE.
          SHORT=.FALSE.
        ELSE IF(TYPE.EQ.'FLOAT')THEN
          SHORT=.TRUE.
          FLOAT=.FALSE.
        ELSE
          CALL DSA_WRUSER('Unable to determine required conversion.\\N')
        END IF
      END IF
C
C  Check that we are not wasting our time!
C
      IF(TYPE.EQ.'SHORT')THEN
        IF(SHORT)THEN
          CALL DSA_WRUSER('IMAGE data array is already SHORT.\\N')
          GO TO 500
        END IF
      ELSE IF(TYPE.EQ.'FLOAT')THEN
        IF(FLOAT)THEN
          CALL DSA_WRUSER('IMAGE data array is already FLOAT.\\N')
          GO TO 500
        END IF
      END IF
C
C  Open file for OUTPUT.
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT',' ',NO_DATA,NEW_FILE,STATUS)
      IF(STATUS.NE.0)GO TO 500
      OPTIONS='D'
      IF (QUAL) OPTIONS=OPTIONS // ',Q(BYTE)'
      IF (ERR)  OPTIONS=OPTIONS // ',E'
      DO I=1,NDIM
        IF (AXIS(I)) THEN
          OPTIONS=OPTIONS//',A'//ICH_CI(I)
        END IF
      END DO
      IF (SHORT) THEN
        CALL DSA_SIMPLE_OUTPUT('OUTPUT',OPTIONS,'SHORT',
     &                         NDIM,DIMS,STATUS)
      ELSE
        CALL DSA_SIMPLE_OUTPUT('OUTPUT',OPTIONS,'FLOAT',
     &                         NDIM,DIMS,STATUS)
      END IF
C
C  Copy axis structures.
C
      DO I=1,NDIM
        IF (AXIS(I)) THEN
          CALL DSA_AXIS_SIZE('IMAGE',I,10,NDIMA,DIMSA,NELMA,STATUS)
          CALL DSA_RESHAPE_AXIS('OUTPUT',I,'IMAGE',I,NDIMA,DIMSA,STATUS)
          IF (STATUS .NE. 0) GO TO 500
        END IF
      END DO
C
C  Magic values are not to be removed from the data arrays (unless a quality
C  array is present)
C
      IF (.NOT.QUAL) THEN
        CALL DSA_USE_FLAGGED_VALUES('IMAGE',STATUS)
        IF(STATUS.NE.0)GO TO 500
      END IF
C
C  Map data,quality and error arrays (if present)
C
      IF(SHORT)THEN
        CALL DSA_WRUSER('Converting to SHORT...\\N')
        CALL DSA_MAP_DATA('IMAGE','READ','SHORT',ADDRESS,ISLOT,STATUS)
        IF(STATUS.NE.0)GO TO 500
        IMPTR=DYN_ELEMENT(ADDRESS)
        CALL DSA_MAP_DATA('OUTPUT','WRITE','SHORT',ADDRESS,OSLOT,STATUS)
        IF(STATUS.NE.0)GO TO 500
        OUTPTR=DYN_ELEMENT(ADDRESS)
        IF (ERR) THEN
          CALL DSA_MAP_ERRORS('IMAGE','READ','SHORT',
     &                        ADDRESS,IESLOT,STATUS)
          IF (STATUS.NE.0) GO TO 500
          IEPTR=DYN_ELEMENT(ADDRESS)
          CALL DSA_MAP_ERRORS('OUTPUT','WRITE','SHORT',
     &                        ADDRESS,OESLOT,STATUS)
          IF (STATUS.NE.0) GO TO 500
          OEPTR=DYN_ELEMENT(ADDRESS)
        END IF
      ELSE
        CALL DSA_WRUSER('Converting to FLOAT...\\N')
        CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',ADDRESS,ISLOT,STATUS)
        IF(STATUS.NE.0)GO TO 500
        IMPTR=DYN_ELEMENT(ADDRESS)
        CALL DSA_MAP_DATA('OUTPUT','WRITE','FLOAT',ADDRESS,OSLOT,STATUS)
        IF(STATUS.NE.0)GO TO 500
        OUTPTR=DYN_ELEMENT(ADDRESS)
        IF (ERR) THEN
          CALL DSA_MAP_ERRORS('IMAGE','READ','FLOAT',
     &                        ADDRESS,IESLOT,STATUS)
          IF (STATUS.NE.0) GO TO 500
          IEPTR=DYN_ELEMENT(ADDRESS)
          CALL DSA_MAP_ERRORS('OUTPUT','WRITE','FLOAT',
     &                        ADDRESS,OESLOT,STATUS)
          IF (STATUS.NE.0) GO TO 500
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
C  - copy data array, quality array, error array where appropriate
C
      IF(SHORT)THEN
        ELEM=DSA_TYPESIZE('SHORT',STATUS)
      ELSE
        ELEM=DSA_TYPESIZE('FLOAT',STATUS)
      END IF
      CALL GEN_MOVE(NELM*ELEM,DYNAMIC_MEM(IMPTR),DYNAMIC_MEM(OUTPTR))
      IF (ERR)
     & CALL GEN_MOVE(NELM*ELEM,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR))
      IF (QUAL)
     & CALL GEN_MOVE(NELM,DYNAMIC_MEM(IQPTR),DYNAMIC_MEM(OQPTR))
C
C  Set magic values flag if they are present in the input data
C
       CALL DSA_SET_FLAGGED_VALUES('OUTPUT',BADPIX,STATUS)
C
C  Tidy up and exit.
C
500   CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END

