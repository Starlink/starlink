C+
C                       D S A _ G E T _ W O R K S P A C E
C
C  Routine name:
C     DSA_GET_WORKSPACE
C
C  Function:
C     Gets a specified amount of dynamic memory.
C
C  Description:
C     This routine obtains an amount of dynamic memory, and returns
C     its address. It also returns a slot number which should be used
C     to refer to the memory in order to release it later.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_GET_WORKSPACE (BYTES,ADDRESS,SLOT,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) BYTES        (Integer,ref) The number of bytes of workspace
C                      to be obtained.
C     (<) ADDRESS      (Integer,ref) The actual address of the start of
C                      the workspace.
C     (<) SLOT         (Integer,ref) The slot number that should be used
C                      to refer to this workspace when it is released.
C     (!) STATUS       (Integer,ref) Status value.  If bad status is
C                      passed it, this routine returns immediately.
C
C  External subroutines / functions used:
C     EMS_ANNUL, EMS_BEGIN, EMS_END, ICH_CI, ICH_LEN, PSX_MALLOC
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the DSA_ system.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 2nd September 1992
C-
C  Subroutine / function details:
C     EMS_ANNUL    Clear any pending EMS error status
C     EMS_BEGIN    Begin a new EMS error reporting environment
C     EMS_END      End the current EMS environment
C     ICH_LEN      Position of last non-blank char in string.
C     ICH_CI       Format an integer into a character string.
C     PSX_MALLOC   Allocate virtual memory
C
C  History:
C     30th June 1987   Original version.  KS / AAO.
C     28th Sept 1989   Modified to use $EXPREG above a certain array size -
C                      this helps prevent fragmentation.  KS / AAO.
C     21st Aug  1992   Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug  1992   Introduced GEN_SUCCESS to handle tests on status values
C                      that follow the VMS convention. KS/AAO.
C     29th Aug  1992   "INCLUDE" filenames now upper case. KS/AAO
C      2nd Sep  1992   Now uses PSX_MALLOC instead of the VMS routines
C                      LIB$GET_VM and SYS$EXPREG. KS/AAO.
C+
      SUBROUTINE DSA_GET_WORKSPACE (BYTES,ADDRESS,SLOT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER  BYTES, ADDRESS, SLOT, STATUS
C
C     Functions used
C
      CHARACTER ICH_CI*16
      INTEGER ICH_LEN
C
C     DSA_ system common
C
      INCLUDE 'DSA_COMMON'
C
C     DSA_ error messages
C
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      INTEGER   EMSTAT                   ! Status used for EMS calls.
      INTEGER   I                        ! Loop index
      CHARACTER NUMBER*16                ! Used to format number of bytes.
      INTEGER   PSX_STATUS               ! Status returned by PSX routine.
C
C     Return immediately if bad status passed
C
      IF (STATUS.NE.0) RETURN
C
C     Get a free slot in the common tables
C
      SLOT=0
      DO I=1,MAX_WORK
         IF (.NOT.WORK_USED(I)) THEN
            SLOT=I
            GO TO 340            ! Break out of I loop
         END IF
      END DO
  340 CONTINUE
C
      IF (SLOT.EQ.0) THEN
         CALL DSA_WRUSER('Unable to allocate workspace common slot.')
         CALL DSA_WRUSER(
     :       ' Too many workspace requests have been made without ')
         CALL DSA_WRUSER('enough workspace release calls.  Possible ')
         CALL DSA_WRUSER(' programming error.')
         CALL DSA_WRFLUSH
         STATUS=DSA__NOWKSL
         GO TO 500              ! Error exit
      END IF
C
C     Get workspace from system. The original VMS version used SYS$EXPREG
C     calls for large amounts of memory and LIB$GET_VM calls for smaller
C     amounts (hence the existence of the WORK_VM_LIMIT parameter). Now it
C     simply assumed that the PSX_MALLOC implementation will use the best
C     routine. The error reporting is not as good as it should be here,
C     since we have to disable EMS (DSA not being a proper EMS environment)
C     and the saving of the bad PSX status code in VMS_CODE is clearly an
C     historical oddity that should be sorted out sometime.
C
      EMSTAT=0
      CALL EMS_BEGIN(EMSTAT)
      PSX_STATUS=0
      CALL PSX_MALLOC(BYTES,ADDRESS,PSX_STATUS)
      EMSTAT=0
      CALL EMS_ANNUL(EMSTAT)
      CALL EMS_END(EMSTAT)
      IF (PSX_STATUS.NE.0) THEN
         CALL DSA_WRUSER('Unable to allocate ')
         NUMBER=ICH_CI(BYTES)
         CALL DSA_WRUSER(NUMBER(:ICH_LEN(NUMBER)))
         CALL DSA_WRUSER(' bytes of workspace. ')
         VMS_CODE=PSX_STATUS
         CALL DSA_WRFLUSH
         STATUS=DSA__VMSERR
         GO TO 500             ! Error exit
      END IF
C
C     Fill in workspace common values
C
      WORK_USED(SLOT)=.TRUE.
      WORK_LINK(SLOT)=0
      WORK_POINTER(SLOT)=ADDRESS
      WORK_TYPE(SLOT)=' '
      WORK_BYTES(SLOT)=BYTES
C
C     Exit
C
  500 CONTINUE
C
      END
