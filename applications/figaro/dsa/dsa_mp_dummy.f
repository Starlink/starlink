C+
C                      D S A _ M A P _ D U M M Y
C
C  Routine name:
C     DSA_MAP_DUMMY
C
C  Function:
C     Creates and maps a workspace data array.
C
C  Description:
C     This routine creates a workspace array of specified dimension
C     and type and maps it, returning the address of the dynamic
C     memory array that may be used to access the data.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_MAP_DUMMY (NAME,MODE,TYPE,NELM,ADDRESS,SLOT,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) NAME       (Fixed string,descr) A name describing the data
C                    being mapped.  This is only used in error messages.
C     (>) MODE       (Fixed string,descr) A string that should be one of
C                    'READ', 'WRITE', or 'UPDATE', depending on how the
C                    data is to be accessed.  Only the first character is
C                    significant.
C     (>) TYPE       (Fixed string,descr) A string specifying the type of
C                    the array that the data is to be mapped onto.  This
C                    can be one of 'BYTE', 'CHAR', 'FLOAT', 'SHORT',
C                    'USHORT', 'INT' or 'DOUBLE' - and must be in upper
C                    case.
C     (>) NELM       (Integer,ref) The number of elements in the data
C                    array.
C     (<) ADDRESS    (Integer,ref) The memory address of the mapped data.
C     (<) SLOT       (Integer,ref) Slot number used as a handle for the
C                    mapping.
C     (!) STATUS     (Integer,ref) Status code.  If a non-zero value is
C                    passed, this routine will return immediately.
C
C  External variables used:
C     Only common variables used internally by the DSA_ system.
C
C  External subroutines / functions used:
C     DSA_WRUSER, DSA_GET_WORKSPACE, ICH_FOLD, ICH_LEN
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Common variable details:
C     (>) MAX_MAP_CALLS (Integer parameter) Maximum number of map calls.
C     (!) MAP_CALL_USED (Logical array) Indicates table entry in use.
C     (<) MAP_CALL_MODE (Character array) Mode of mapping.
C     (<) MAP_CALL_SLOT (Integer array) Map table entry corresponding to call.
C     (<) MAP_CALL_WORK (Integer array) Work entry corresponding to call.
C
C  Subroutine / function details:
C     ICH_LEN       Position of last non-blank char in string
C     ICH_FOLD      Convert a string to upper case
C     DSA_GET_WORKSPACE  Get a specified amount of workspace.
C     DSA_WRUSER    Output a string to the user.
C
C  History:
C     7th July 1987    Original version.  KS / AAO.
C     21st July 1988   Now sets type in common tables.  KS / AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_MAP_DUMMY (NAME,MODE,TYPE,NELM,ADDRESS,
     :                                                SLOT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM, ADDRESS, SLOT, STATUS
      CHARACTER*(*) NAME, MODE, TYPE
C
C     Functions used
C
      INTEGER ICH_FOLD, ICH_LEN
C
C     DSA_ system common definition
C
      INCLUDE 'DSA_COMMON'
C
C     DSA_ system error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      INTEGER   BYTES                    ! Number of workspace bytes needed
      CHARACTER CHR*1                    ! Temporary, used for start of MODE
      INTEGER   I                        ! Loop index
      INTEGER   INVOKE                   ! Dummy function argument
      INTEGER   TYPE_CODE                ! Integer code for requested type
      LOGICAL   TYPE_OK                  ! Indicates requested type is valid
      INTEGER   WORK_SLOT                ! Slot number of work area
C
C     DSA_ type definitions.  Defines MAX_TYPES, xxxx_TYPE,
C                             TYPE_NAMES, TYPE_SIZE, FMTCON_CODE
C
      INCLUDE 'DSA_TYPES'
C
C     If bad status passed, return now
C
      IF (STATUS.NE.0) RETURN
C
C     Make sure MODE is valid.
C
      CHR=MODE(1:1)
      INVOKE=ICH_FOLD(CHR)
      IF (INDEX('RWU',CHR).EQ.0) THEN
         CALL DSA_WRUSER('Invalid access mode (')
         CALL DSA_WRUSER(MODE(:ICH_LEN(MODE)))
         CALL DSA_WRUSER(') specified for ')
         CALL DSA_WRUSER(NAME(:ICH_LEN(NAME)))
         CALL DSA_WRUSER('.')
         CALL DSA_WRFLUSH
         STATUS=DSA__ACCINV
         GO TO 500
      END IF
C
C     Get a spare slot to record the mapping.
C
      SLOT=0
      DO I=1,MAX_MAP_CALLS
         IF (.NOT.MAP_CALL_USED(I)) THEN
            SLOT=I
            GO TO 320       ! Break out of I loop
         END IF
      END DO
  320 CONTINUE
      IF (SLOT.EQ.0) THEN
         CALL DSA_WRUSER('Unable to find a free map call slot for ')
         CALL DSA_WRUSER(NAME(:ICH_LEN(NAME)))
         CALL DSA_WRUSER('. Too many items mapped at once.')
         CALL DSA_WRFLUSH
         STATUS=DSA__NOMPSL
         GO TO 500
      END IF
      MAP_CALL_WORK(SLOT)=0
      MAP_CALL_SLOT(SLOT)=0
      MAP_CALL_MODE(SLOT)=CHR
C
C     Now see what TYPE of array has been requested.
C
      TYPE_OK=.FALSE.
      DO I=1,MAX_TYPES
         IF (TYPE.EQ.TYPE_NAMES(I)) THEN
            TYPE_CODE=I
            TYPE_OK=.TRUE.
            GO TO 360            ! Break out of I loop
         END IF
      END DO
  360 CONTINUE
      IF (.NOT.TYPE_OK) THEN
         CALL DSA_WRUSER('Unable to map ')
         CALL DSA_WRUSER(NAME(:ICH_LEN(NAME)))
         CALL DSA_WRUSER(' as type ')
         CALL DSA_WRUSER(TYPE(:ICH_LEN(TYPE)))
         CALL DSA_WRUSER('.  Type is not valid.  Programming error.')
         CALL DSA_WRFLUSH
         STATUS=DSA__INVTYP
         GO TO 500
      END IF
C
C     Work out how much workspace is needed, and allocate it.
C
      BYTES=NELM*TYPE_SIZE(TYPE_CODE)
      CALL DSA_GET_WORKSPACE (BYTES,ADDRESS,WORK_SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
      WORK_TYPE(WORK_SLOT)=TYPE
      MAP_CALL_WORK(SLOT)=WORK_SLOT
C
C     Now that everything is OK, we flag the map call table slot as in use.
C
      MAP_CALL_USED(SLOT)=.TRUE.
C
C     Exit
C
  500 CONTINUE
C
      END
