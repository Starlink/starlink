C+
C                   D S A _ G E T _ W O R K _ A R R A Y
C
C  Routine name:
C     DSA_GET_WORK_ARRAY
C
C  Function:
C     Gets a dynamic memory for an array of specified type and size.
C
C  Description:
C     Given a type (eg 'FLOAT') and a number of elements, this routine
C     obtains workspace for an array of the specified size and type,
C     and returns its address. It also returns a slot number which should
C     be used to refer to the memory in order to release it later.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_GET_WORK_ARRAY (ELEMENTS,TYPE,ADDRESS,SLOT,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) ELEMENTS     (Integer,ref) The number of elements of the
C                      specified type in the workspace to be obtained.
C     (>) TYPE         (Fixed string,descr) The type of the workspace
C                      array to be obtained.  This must be one of the
C                      types recognised by the DSA_ routines, and while
C                      the case is not significant, cannot be abbreviated.
C     (<) ADDRESS      (Integer,ref) The actual address of the start of
C                      the workspace.
C     (<) SLOT         (Integer,ref) The slot number that should be used
C                      to refer to this workspace when it is released.
C     (!) STATUS       (Integer,ref) Status value.  If bad status is
C                      passed it, this routine returns immediately.
C
C  External subroutines / functions used:
C     DSA_TYPESIZE, DSA_GET_WORKSPACE
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the DSA_ system.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     DSA_TYPESIZE    Get size in bytes for a specified type.
C     DSA_GET_WORKSPACE  Get a specified number of bytes of workspace.
C
C  History:
C     30th June 1987   Original version.  KS / AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_GET_WORK_ARRAY (ELEMENTS,TYPE,ADDRESS,SLOT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER  ELEMENTS, ADDRESS, SLOT, STATUS
      CHARACTER*(*) TYPE
C
C     Functions used
C
      INTEGER DSA_TYPESIZE
C
C     Local variables
C
      INTEGER   BYTES                    ! Number of bytes required.
C
C     Return immediately if bad status passed
C
      IF (STATUS.NE.0) RETURN
C
C     Work out number of bytes required, and obtain them.
C
      BYTES=DSA_TYPESIZE(TYPE,STATUS)*ELEMENTS
      IF (STATUS.EQ.0) CALL DSA_GET_WORKSPACE(BYTES,ADDRESS,SLOT,STATUS)
C
      END
