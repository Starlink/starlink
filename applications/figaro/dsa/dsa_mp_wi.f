C+
C                        D S A _ M A P _ W I D T H
C
C  Routine name:
C     DSA_MAP_WIDTH
C
C  Function:
C     Maps one of the axis width arrays in a structure.
C
C  Description:
C     This routine maps a specified axis width array in a structure,
C     returning a memory address that may be used to access it.   The
C     whole array is mapped.  If there is in fact no axis width array,
C     then a dummy array is generated and its address is returned,
C     unless the mapping is for write or update, in which case
C     such an array is created in the data structure and mapped. The
C     contents of the array depend on the axis data.  If there is an
C     existing axis data array, then the width values are the differences
C     between successive axis data elements.  If there is a single width
C     value for the axis, that value is duplicated in all the elements
C     of the mapped array.  If neither of these obtains, then each
C     element of the mapped array is set to 1.0
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_MAP_WIDTH (REF_NAME,AXIS,MODE,TYPE,ADDRESS,
C                                                       SLOT,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME     (Fixed string,descr) The reference name associated
C                      with the structure.
C     (>) AXIS         (Integer,ref) The number of the axis in question.
C                      Should be between 1 and 6.
C     (>) MODE         (Fixed string,descr) One of 'READ','WRITE', or
C                      'UPDATE', indicating the way the data is going to
C                      be accessed.  Only the first character is significant.
C     (>) TYPE         (Fixed string,descr) The type of data array to be
C                      mapped onto the structure array.  This can be 'BYTE',
C                      'CHAR','FLOAT','DOUBLE','SHORT','USHORT' or 'INT'.
C                      If type conversion is needed, it will be performed
C                      automatically.
C     (<) ADDRESS      (Integer,ref) The address of the mapped data array.
C     (<) SLOT         (Integer,ref) A handle value associated with this
C                      mapping, which may be used to reference it later,
C                      for example to explicitly unmap the data.
C     (!) STATUS       (Integer,ref) Status return code.  If a bad status
C                      value is passed to it, this routine returns
C                      immediately.
C
C  External variables used:
C     Only common variables used internally by the DSA_ routines.
C
C  External subroutines / functions used:
C     ICH_FOLD, ICH_CI, GEN_NTH, DSA_REF_SLOT, DSA_SEEK_AXIS,
C     DSA_AXIS_SIZE, DSA_DATA_SIZE, DSA_MAP_ARRAY, DSA_MAP_DUMMY,
C     DSA_FILL_WIDTH, DSA__AXIS_WIDTH_NAME, DSA_SEEK_WIDTH, DSA_ARRAY_SIZE,
C     DSA__CREATE_AXIS, DSA_CREATE_ARRAY, DSA_WRUSER, DSA_GET_ACTUAL_NAME,
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and the
C     data structure must have been opened, eg by DSA_INPUT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 26th October 1994
C-
C  Common variable details:
C     (>) MAX_AXES      (Integer parameter) Maximum number of axes in data.
C     (<) WIDTH_UPDATE  (Logical array) Indicates that an axis width array
C                       has been updated (or at least, mapped for update).
C
C  Subroutine / function details:
C     ICH_FOLD      Convert string to upper case.
C     ICH_CI        Format an integer into a character string.
C     GEN_NTH       Return 'st', 'nd', 'rd', 'th' etc given an integer.
C     DSA_MAP_ARRAY Map named data array.
C     DSA_CREATE_ARRAY Create an array of given size and type.
C     DSA_DATA_SIZE Get the size of a structure's main data array.
C     DSA_REF_SLOT  Look up reference name in common tables.
C     DSA_MAP_DUMMY Map a dummy data array.
C     DSA_GET_ACTUAL_NAME  Get name of structure from ref name.
C     DSA_FILL_WIDTH Fill a newly created width array.
C     DSA_SEEK_AXIS See if an axis data array exists.
C     DSA_SEEK_WIDTH  See if an axis width array exists.
C     DSA_AXIS_SIZE Get the dimensions of an axis data array.
C     DSA_WRUSER    Output a message to the user.
C     DSA__CREATE_AXIS  Ensure that a specified axis structure exists.
C     DSA__AXIS_WIDTH_NAME Get DTA name for the axis width array.
C
C  History:
C     26th Aug 1988.   Original version.  KS / AAO.
C     8th Sept 1989.   Call to DSA_MAP_ARRAY now sets propagate flag
C                      false.  KS/AAO.
C     11th Dec 1989.   Now sets update flag for write or update mapping KS/AAO.
C     2nd March 1990.  Now uses DSA__ routines to get structure details, rather
C                      than assuming the original Figaro data structure. KS/AAO
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     23rd Aug 1992    Remove unused variable declarations. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C     26th Oct 1994     Now uses new calling sequence for DSA_MAP_ARRAY. KS/AAO.
C+
      SUBROUTINE DSA_MAP_WIDTH (REF_NAME,AXIS,MODE,TYPE,ADDRESS,
     :                                                 SLOT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) REF_NAME, MODE, TYPE
      INTEGER AXIS, ADDRESS, SLOT, STATUS
C
C     Functions used
C
      INTEGER ICH_FOLD
      CHARACTER ICH_CI*1, GEN_NTH*2
C
C     DSA_ common definition
C
      INCLUDE 'DSA_COMMON'
C
C     DSA_ error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      CHARACTER CHR*1                       ! First character of MODE
      LOGICAL   CREATED                     ! Indicates new array was created
      INTEGER   DIMS(MAX_AXES)              ! Dimensions of data array
      INTEGER   DTA_STATUS                  ! Status code from DTA_ routines
      CHARACTER ERROR*64                    ! DTA system error description
      LOGICAL   EXIST                       ! True if axis data array exists
      INTEGER   I                           ! Loop index
      INTEGER   INVOKE                      ! Dummy function return value
      INTEGER   LENGTH                      ! Object name length
      CHARACTER NAME*128                    ! Name of dummy array
      INTEGER   NDIM                        ! Number of data array dimensions
      INTEGER   NELM                        ! Number of data array elements
      CHARACTER OBJ_NAME*128                ! DTA_ name of data object
      INTEGER   REF_SLOT                    ! Reference table slot # - ignored
      LOGICAL   SINGLE                      ! True if width is a single value
      CHARACTER STRUCTURE*128               ! Name of structure
      CHARACTER TYPE_UC*8                   ! Upper case version of TYPE
      DOUBLE PRECISION WIDTH                ! Single width value
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     We need an upper case version of TYPE
C
      TYPE_UC=TYPE
      INVOKE=ICH_FOLD(TYPE_UC)
C
C     See if there in in fact any axis width data.  Note that this will
C     also check that AXIS is a valid number.
C
      CALL DSA_SEEK_WIDTH (REF_NAME,AXIS,EXIST,SINGLE,WIDTH,STATUS)
      IF (STATUS.NE.0) GO TO 500      ! Error exit
C
C     See if the mode requires that the array exist
C
      CHR=MODE(1:1)
      INVOKE=ICH_FOLD(CHR)
C
      IF ((EXIST.AND.(.NOT.SINGLE)).OR.
     :                    (CHR.EQ.'W').OR.(CHR.EQ.'U')) THEN
C
C        The axis array either is required to exist or does exist.
C        Either way, we want to map an actual data object, not a dummy
C        array.
C
C        Look up the reference name in the tables and get the name
C        of the axis width array.
C
         CALL DSA_REF_SLOT (REF_NAME,REF_SLOT,STATUS)
         CALL DSA__AXIS_WIDTH_NAME (REF_SLOT,AXIS,OBJ_NAME,LENGTH)
C
         IF (EXIST.AND.(.NOT.SINGLE)) THEN
C
C           If it did exist, get its dimensions.
C
            CREATED=.FALSE.
            CALL DSA_ARRAY_SIZE(OBJ_NAME(:LENGTH),
     :                                  MAX_AXES,NDIM,DIMS,ERROR,STATUS)
            NELM=1
            DO I=1,NDIM
               NELM=NELM*DIMS(I)
            END DO
         ELSE
C
C           If it didn't exist, we have to create a suitable array.
C           The dimensions will be the same as the data array for that
C           axis.
C
            CALL DSA_AXIS_SIZE (REF_NAME,AXIS,MAX_AXES,NDIM,DIMS,
     :                                                     NELM,STATUS)
            CALL DSA__CREATE_AXIS (REF_SLOT,AXIS,DTA_STATUS)
            CALL DSA_CREATE_ARRAY (' ',OBJ_NAME(:LENGTH),'FLOAT',
     :                                                NDIM,DIMS,STATUS)
            IF (STATUS.NE.0) GO TO 500  ! Error exit
            CREATED=.TRUE.
         END IF
C
C        Now map the data array
C
         CALL DSA_MAP_ARRAY (OBJ_NAME(:LENGTH),MODE,TYPE_UC,1,NELM,
     :                 NELM,.FALSE.,.FALSE.,.FALSE.,ADDRESS,SLOT,STATUS)
C
C        If it had to be created, fill it up.
C
         IF (CREATED) THEN
            CALL DSA_FILL_WIDTH (REF_NAME,AXIS,SINGLE,WIDTH,ADDRESS,
     :                                     NDIM,DIMS,TYPE_UC,STATUS)
         END IF
      ELSE
C
C        There is no axis width array, so we need to generate a dummy one.
C        Again, we make one the size of the axis data array.
C
         CALL DSA_AXIS_SIZE (REF_NAME,AXIS,MAX_AXES,NDIM,DIMS,
     :                                                   NELM,STATUS)
C
C        Now map a dummy array of the appropriate size and fill it
C        with the numbers from 1 to N.
C
         CALL DSA_GET_ACTUAL_NAME(REF_NAME,STRUCTURE,STATUS)
         NAME=ICH_CI(AXIS)//GEN_NTH(AXIS)//'-axis width array in '//
     :                                                        STRUCTURE
         CALL DSA_MAP_DUMMY (NAME,MODE,TYPE_UC,NELM,ADDRESS,SLOT,STATUS)
C
         CALL DSA_FILL_WIDTH (REF_NAME,AXIS,SINGLE,WIDTH,ADDRESS,
     :                                    NDIM,DIMS,TYPE_UC,STATUS)
C
      END IF
C
C     If mapping was not readonly, set update flag.
C
      IF (CHR.NE.'R') WIDTH_UPDATE(AXIS,REF_SLOT)=.TRUE.
C
C     Exit
C
  500 CONTINUE
C
      END
