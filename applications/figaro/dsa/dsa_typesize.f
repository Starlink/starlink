C+
C                       D S A _ T Y P E S I Z E
C
C  Routine name:
C     DSA_TYPESIZE
C
C  Function:
C     Returns the size in bytes of an element of a given type.
C
C  Description:
C     Given one of the types recognised by the DSA_ routines ('FLOAT',
C     'INT', etc), this routine returns the number of bytes required
C     by a single element of that type.
C
C  Language:
C     FORTRAN
C
C  Call:
C     BYTES = DSA_TYPESIZE (TYPE,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) TYPE        (Fixed string,descr) The type in question.  Case
C                     is not significant, but the type name must be
C                     given in full and not abbreviated.
C     (!) STATUS      (Integer,ref) Status code.  If bad status is passed,
C                     this routine will return immediately.  This routine
C                     regards an unrecognised type as an error, but does
C                     not output any error messages; if it is passed a
C                     structure type name it returns a size of zero and
C                     error status, but keeps quiet.
C
C  Returns:
C     (<) BYTES       (Integer,function value) The number of bytes
C                     corresponding to TYPE.  If TYPE is invalid, zero
C                     is returned.
C
C  External variables used: None.
C
C  External subroutines / functions used:
C     ICH_FOLD
C
C  Prior requirements: None.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     ICH_FOLD      Convert character string to upper case.
C
C  History:
C     29th July 1987.  Original version.  KS / AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     23rd Aug 1992    Ensure function value is defined before allowing a
C                      RETURN (the DecStation compiler spotted this one) KS/AAO.
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C+
      INTEGER FUNCTION DSA_TYPESIZE (TYPE,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
      CHARACTER*(*) TYPE
C
C     Functions used
C
      INTEGER ICH_FOLD
C
C     DSA_ error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      INTEGER   I                        ! Loop index
      INTEGER   INVOKE                   ! Dummy function argument
      CHARACTER TYPE_UC*8                ! Upper case version of TYPE
C
C     DSA_ type definitions  (defines MAX_TYPES, TYPE_NAMES, TYPE_SIZE)
C
      INCLUDE 'DSA_TYPES'
C
C     Return immediately on bad status
C
      DSA_TYPESIZE=0
      IF (STATUS.NE.0) RETURN
C
C     Convert TYPE to upper case
C
      TYPE_UC=TYPE
      INVOKE=ICH_FOLD(TYPE_UC)
C
C     Loop through type names until a match is found.
C
      DO I=1,MAX_TYPES
         IF (TYPE_UC.EQ.TYPE_NAMES(I)) THEN
            DSA_TYPESIZE=TYPE_SIZE(I)
            GO TO 320                   ! Break out of I loop
         END IF
      END DO
      STATUS=DSA__INVTYP
  320 CONTINUE
C
      END
