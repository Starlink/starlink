C+
C                         D S A _ A R R A Y _ D E F I N E D
C
C  Routine name:
C     DSA_ARRAY_DEFINED
C
C  Function:
C     Returns the state of a named data array.
C
C  Description:
C     Given the DTA_ system name of a data array, this routine
C     returns its state (defined or undefined).  At present, it can only
C     handle a limited number of structured array types.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_ARRAY_DEFINED (NAME,DFINED,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) NAME       (Fixed string,descr) The DTA_ system name of the
C                    array in question.
C     (<) DFINED     (Logical,ref) True if array contents are defined,
C                    false otherwise.
C     (!) STATUS     (Integer,ref) Returned status value.  If a non-zero
C                    status value is passed, this routine returns
C                    immediately.
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system.
C
C  Note:
C     This is an DSA_ system internal routine, and should not be
C     called directly from outside the DSA_ package.
C
C  History:
C     20 Jul 1995  Original version.  hme / UoE, Starlink.
C+
      SUBROUTINE DSA_ARRAY_DEFINED (NAME,DFINED,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL DFINED
      INTEGER STATUS
      CHARACTER*(*) NAME
C
C     DSA_ system common definition
C
      INCLUDE 'DSA_COMMON'
C
C     DSA_ error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Functions used
C
      INTEGER ICH_LEN
C
C     Local variables
C
      CHARACTER ARRAY_NAME*80    ! Name of actual array in structure
      INTEGER   DTA_STATUS       ! Status returned by DTA_ routines
      LOGICAL   KNOWN            ! True if a known structure type
      INTEGER   LENAME           ! Number of chars in ARRAY_NAME
      LOGICAL   STRUCT           ! True if named object a structure
      CHARACTER TYPE*16          ! Type of named object
      CHARACTER VARIANT*16       ! Variant for structured array
C
C     If bad status passed, return now.
C
      IF (STATUS.NE.0) RETURN
C
C     See if the object is primitive or not
C
      CALL DTA_STRUC (NAME,STRUCT,DTA_STATUS)
      IF (DTA_STATUS.NE.0) GO TO 500
C
C     If it is, get its state
C
      IF (.NOT.STRUCT) THEN
         CALL DTA_DFNED (NAME,DFINED,DTA_STATUS)
         IF (DTA_STATUS.NE.0) GO TO 500
      ELSE
C
C        If it isn't, see if we know the type
C
         CALL DTA_TYVAR (NAME,TYPE,DTA_STATUS)
         IF (DTA_STATUS.NE.0) GO TO 500
C
C        Look to see if this might be a structured array of a type we
C        know about.  If it is, get the name of its actual array object.
C
         CALL DSA__STRUCT_ARRAY (NAME,ICH_LEN(NAME),TYPE,VARIANT,
     :                                ARRAY_NAME,LENAME,KNOWN,STATUS)
         IF (STATUS.NE.0) GO TO 500
         IF (KNOWN) THEN
            CALL DTA_DFNED(ARRAY_NAME(:LENAME),DFINED,DTA_STATUS)
            IF (DTA_STATUS.NE.0) GO TO 500
         ELSE
            STATUS=DSA__NOARRY
         END IF
      END IF
C
C     On way out, see if an error code needs an error string generating.
C
  500 CONTINUE
      IF (DTA_STATUS.NE.0) THEN
         STATUS=DSA__DTAERR
         DTA_CODE=DTA_STATUS
      END IF
C
      END
