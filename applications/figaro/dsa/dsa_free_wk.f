C+
C                     D S A _ F R E E _ W O R K S P A C E
C
C  Routine name:
C     DSA_FREE_WORKSPACE
C
C  Function:
C     Releases previously obtained workspace.
C
C  Description:
C     When workspace is obtained through a call to DSA_GET_WORKSPACE,
C     or through a call to DSA_GET_WORK_ARRAY, a handle value (SLOT)
C     is returned to identify the workspace obtained.  The workspace
C     can be released through a call to DSA_FREE_WORKSPACE, specifying
C     that slot number.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_FREE_WORKSPACE (SLOT,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) SLOT        (Integer,ref) The slot number returned by a
C                     call to DSA_GET_WORKSPACE.
C     (!) STATUS      (Integer,ref) Status value.  If bad status is
C                     passed, this routine returns immediately.
C
C  External variables used:
C     Only common variables internal to the DSA_ routines.
C
C  External subroutines / functions used:
C     DSA_WRUSER, EMS_ANNUL, EMS_BEGIN, EMS_END, PSX_FREE
C
C  Prior requirements:
C     DSA_OPEN should have been called to initialise the system.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 2nd September 1992
C-
C  Common variable details:
C     (>) MAX_MAPS      (Integer parameter) Maximum number of mapped arrays.
C     (>) MAP_USED      (Logical array) Indicates map slot in use.
C     (!) MAP_WORK      (Integer array) Workspace slot associated with array.
C     (>) MAX_WORK      (Integer parameter) Number of workspace slots available.
C     (!) WORK_USED     (Logical array) Indicates workspace slot in use.
C     (>) WORK_LINK     (Integer array) Link to other work slots.
C     (>) WORK_POINTER  (Integer array) Memory address of workspace array.
C
C  Subroutine / function details:
C     DSA_WRUSER   Write message to user
C     EMS_ANNUL    Clear any pending EMS error status
C     EMS_BEGIN    Begin a new EMS error reporting environment
C     EMS_END      End the current EMS environment
C     PSX_FREE     Release virtual memory obtained through PSX_MALLOC
C
C  History:
C     9th  July 1987  Original version.  KS / AAO.
C     28th Sept 1989  Now uses $DELTVA for large arrays.  KS / AAO.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug  1992  Introduced GEN_SUCCESS to handle tests on status values
C                     that follow the VMS convention. KS/AAO.
C     29th Aug  1992  "INCLUDE" filenames now upper case. KS/AAO
C      2nd Sep  1992  Now uses PSX_FREE instead of the VMS routines
C                      LIB$FREE_VM and SYS$DELTVA. KS/AAO.
C+
      SUBROUTINE DSA_FREE_WORKSPACE (SLOT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER SLOT, STATUS
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
      INTEGER   EMSTAT                ! Status used for EMS calls
      INTEGER   I                     ! Loop index
      INTEGER   PSX_STATUS            ! Status used for PSX calls
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Make sure the slot is actually in use
C
      IF (.NOT.WORK_USED(SLOT)) THEN
         CALL DSA_WRUSER(
     :      'An attempt has been made to release an unused workspace ')
         CALL DSA_WRUSER('slot.  Probable programming error.')
         CALL DSA_WRFLUSH
         STATUS=DSA__NOWKSL
         GO TO 500              ! Error exit
      END IF
C
C     Flag the slot as unused
C
      WORK_USED(SLOT)=.FALSE.
C
C     Release the virtual memory allocated for the slot. The error reporting
C     is not as good as it should be here, since we have to disable EMS (DSA
C     not being a proper EMS environment) and the saving of the bad PSX
C     status code in VMS_CODE is clearly an historical oddity that should be
C     sorted out sometime.
C
      EMSTAT=0
      CALL EMS_BEGIN(EMSTAT)
      PSX_STATUS=0
      CALL PSX_FREE(WORK_POINTER(SLOT),PSX_STATUS)
      EMSTAT=0
      CALL EMS_ANNUL(EMSTAT)
      CALL EMS_END(EMSTAT)
      IF (PSX_STATUS.NE.0) THEN
         CALL DSA_WRUSER('Error attempting to release workspace. ')
         VMS_CODE=PSX_STATUS
         CALL DSA_WRFLUSH
         STATUS=DSA__VMSERR
         GO TO 500             ! Error exit
      END IF
C
C     If necessary, fix up the links to this work slot and the
C     various map and other work slots.
C
      DO I=1,MAX_MAPS
         IF (MAP_USED(I)) THEN
            IF (MAP_WORK(I).EQ.SLOT) MAP_WORK(I)=WORK_LINK(SLOT)
         END IF
      END DO
      DO I=1,MAX_WORK
         IF (WORK_USED(I)) THEN
            IF (WORK_LINK(I).EQ.SLOT) WORK_LINK(I)=WORK_LINK(SLOT)
         END IF
      END DO
C
C     Exit
C
  500 CONTINUE
C
      END
