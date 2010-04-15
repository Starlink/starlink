C+
C                   D S A _ M A P _ A X I S _ D A T A
C
C  Routine name:
C     DSA_MAP_AXIS_DATA
C
C  Function:
C     Maps one of the axis data arrays in a structure.
C
C  Description:
C     This routine maps a specified axis array in a structure, returning
C     a memory address that may be used to access it.   The whole array
C     is mapped.  If there is in fact no axis data array, then a dummy
C     array containing the values 1..N is generated and its address is
C     returned, unless the mapping is for write or update, in which case
C     such an array is created in the data structure and mapped.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_MAP_AXIS_DATA (REF_NAME,AXIS,MODE,TYPE,ADDRESS,
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
C                      'CHAR','FLOAT','DOUBLE','SHORT' or 'INT'.  If type
C                      conversion is needed, it will be performed auto-
C                      matically.
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
C     ICH_FOLD, DSA_FIND_REF, DSA_SEEK_AXIS, DSA_AXIS_SIZE
C     DSA_DATA_SIZE, DSA_MAP_ARRAY, DSA_MAP_DUMMY, DSA_NFILL_ARRAY
C     DSA_WRUSER, DSA_GET_ACTUAL_NAME, DSA_CREATE_ARRAY, DSA__CREATE_AXIS,
C     DSA__AXIS_DATA_NAME, ICH_CI, GEN_NTH
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
C     (>) MAX_AXES    (Integer parameter) Maximum number of axes in data.
C     (<) AXIS_UPDATE (Logical array) Indicates that an axis data array
C                     has been updated (or at least, mapped for update).
C
C  Subroutine / function details:
C     ICH_FOLD      Convert string to upper case.
C     ICH_CI        Convert integer to string
C     DSA_CREATE_ARRAY Create an array of given size and type.
C     DSA_MAP_ARRAY Map named data array.
C     DSA_DATA_SIZE Get the size of a structure's main data array.
C     DSA_FIND_REF  Look up reference name in common tables.
C     DSA_MAP_DUMMY Map a dummy data array.
C     DSA_GET_ACTUAL_NAME  Get name of structure from ref name.
C     DSA_NFILL_ARRAY Fill a dummy array with the numbers 1..N
C     DSA_SEEK_AXIS See if an axis data array exists.
C     DSA_AXIS_SIZE Get the dimensions of an axis data array.
C     DSA_WRUSER    Output a message to the user.
C     DSA__CREATE_AXIS    Make sure a given axis structure exists.
C     DSA__AXIS_DATA_NAME Get DTA_ system name of axis data array.
C     GEN_NTH       Given an integer, returns 'st','nd','rd' etc.
C
C  History:
C     6th July 1987.   Original version.  KS / AAO.
C     25th Feb 1988.   Bug fix.  If data array is n-D and axis data had
C                      to be created, it was being mapped with too many
C                      elements.  Also correct one comment.
C     20th July 1988.  Modify call to DSA_MAP_ARRAY - quality processing
C                      flags set false.
C     8th  Sept 1989.  Modify call to DSA_MAP_ARRAY - flagged value
C                      propagation flag set false.
C     11th Dec  1989.  Add setting of update flag for write or update
C                      mapping.  KS / AAO.
C     19th Jan  1990.  Use DSA__ routines to get details of data structure.
C                      KS/AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     23rd Aug 1992    Remove unused variable declarations. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C     26th Oct 1994     Now uses new calling sequence for DSA_MAP_ARRAY. KS/AAO.
C+
      SUBROUTINE DSA_MAP_AXIS_DATA (REF_NAME,AXIS,MODE,TYPE,ADDRESS,
     :                                                 SLOT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) REF_NAME, MODE, TYPE
      INTEGER AXIS, ADDRESS, SLOT, STATUS
C
C     Functions used  (declaring ICH_CI as one char long simplifies things,
C     but assumes that no more than 9 axes are supported!)
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
      INTEGER   DIMS(MAX_AXES)              ! Dimensions of data array
      LOGICAL   EXIST                       ! True if axis data array exists
      INTEGER   IGNORE                      ! Dummy status argument
      INTEGER   INVOKE                      ! Dummy function return value
      INTEGER   LENGTH                      ! Object name length
      CHARACTER NAME*128                    ! Name of dummy array
      INTEGER   NDIM                        ! Number of data array dimensions
      INTEGER   NELM                        ! Number of data array elements
      CHARACTER OBJ_NAME*128                ! DTA_ name of data object
      CHARACTER REF_NAME_UC*32              ! Upper case version of REF_NAME
      INTEGER   REF_SLOT                    ! Reference table slot # - ignored
      CHARACTER STRUCTURE*128               ! Name of structure
      CHARACTER TYPE_UC*8                   ! Upper case version of TYPE
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     We need an upper case version of REF_NAME and TYPE
C
      TYPE_UC=TYPE
      INVOKE=ICH_FOLD(TYPE_UC)
      REF_NAME_UC=REF_NAME
      INVOKE=ICH_FOLD(REF_NAME_UC)
C
C     See if there in in fact any axis data.  Note that this will
C     also check that AXIS is a valid number.
C
      CALL DSA_SEEK_AXIS (REF_NAME,AXIS,EXIST,STATUS)
      IF (STATUS.NE.0) GO TO 500      ! Error exit
C
C     See if the mode requires that the array exist
C
      CHR=MODE(1:1)
      INVOKE=ICH_FOLD(CHR)
C
      IF (EXIST.OR.(CHR.EQ.'W').OR.(CHR.EQ.'U')) THEN
C
C        The axis array either is required to exist or does exist.
C        Either way, we want to map an actual data object, not a dummy
C        array.
C
C        Look up the reference name in the tables and get the name
C        of the axis data array in question.
C
         CALL DSA_REF_SLOT (REF_NAME,REF_SLOT,STATUS)
         CALL DSA__AXIS_DATA_NAME (REF_SLOT,AXIS,OBJ_NAME,LENGTH)
C
         IF (EXIST) THEN
C
C           If it did exist, get its dimensions.
C
            CALL DSA_AXIS_SIZE(REF_NAME,AXIS,MAX_AXES,NDIM,DIMS,
     :                                                    NELM,STATUS)
         ELSE
C
C           If it didn't exist, we have to create a suitable array.
C           The dimensions will be those of the appropriate axis of
C           the main data array.
C
            CALL DSA_DATA_SIZE (REF_NAME,MAX_AXES,NDIM,DIMS,NELM,STATUS)
            CALL DSA__CREATE_AXIS (REF_SLOT,AXIS,IGNORE)
            NELM=DIMS(AXIS)
            CALL DSA_CREATE_ARRAY (' ',OBJ_NAME(:LENGTH),'FLOAT',1,NELM,
     :                                                           STATUS)
            IF (STATUS.NE.0) GO TO 500             ! Error exit
         END IF
C
C        Now map the data array
C
         CALL DSA_MAP_ARRAY (OBJ_NAME(:LENGTH),MODE,TYPE_UC,1,NELM,
     :                 NELM,.FALSE.,.FALSE.,.FALSE.,ADDRESS,SLOT,STATUS)
C
C        If it had to be created, fill it with the numbers 1..N
C
         IF (.NOT.EXIST) THEN
            CALL DSA_NFILL_ARRAY (NELM,ADDRESS,TYPE_UC,STATUS)
         END IF
      ELSE
C
C        There is no axis data array, so we need to generate a dummy one.
C        First, we need to know the dimensions of the main array.  We
C        will generate an array the length of the appropriate axis.
C
         CALL DSA_DATA_SIZE (REF_NAME,MAX_AXES,NDIM,DIMS,NELM,STATUS)
         NELM=DIMS(AXIS)
C
C        Now map a dummy array of the appropriate size and fill it
C        with the numbers from 1 to N.
C
         CALL DSA_GET_ACTUAL_NAME(REF_NAME,STRUCTURE,STATUS)
         NAME=ICH_CI(AXIS)//GEN_NTH(AXIS)//' axis data array in '//
     :                                                        STRUCTURE
         CALL DSA_MAP_DUMMY (NAME,MODE,TYPE_UC,NELM,ADDRESS,SLOT,STATUS)
C
         CALL DSA_NFILL_ARRAY (NELM,ADDRESS,TYPE_UC,STATUS)
C
      END IF
C
C     If mapping is not readonly, set update flag.
C
      IF (CHR.NE.'R') AXIS_UPDATE(AXIS,REF_SLOT)=.TRUE.
C
C     Exit
C
  500 CONTINUE
C
      END
