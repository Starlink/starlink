C+
C
C                      D S A _ V A R I A N C E _ T Y P E
C
C  Routine name:
C     DSA_VARIANCE_TYPE
C
C  Function:
C     Returns the type of the variance data array.
C
C  Description:
C     This routine returns the DTA_ system type of the variance data array
C     of a structure.  If the array does not exist, an error message is
C     output and bad status is returned.  If the data array is a
C     structured type, this is also indicated.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_VARIANCE_TYPE (REF_NAME,TYPE,STRUCT,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME     (Fixed string,descr) The reference name associated
C                      with the structure.
C     (<) TYPE         (Fixed string,descr) The DTA_ system type of the
C                      variance data array.
C     (<) STRUCT       (Logical,ref) Set to indicate whether or not the
C                      array is a structured type.
C     (!) STATUS       (Integer,ref) Status return code.  If a bad status
C                      value is passed to it, this routine returns
C                      immediately.
C
C  External variables used: None.
C
C  External subroutines / functions used:
C     DSA_ERROR_TYPE
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and the
C     data structure must have been opened, eg by DSA_INPUT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     DSA_ERROR_TYPE   Gets the type of the error data array.
C
C  History:
C     7th  Dec  1989.   Original version.  KS / AAO.
C     12th Mar  1990.   Modified to allow for different data formats,
C                       rather than assuming the original Figaro format. KS/AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_VARIANCE_TYPE (REF_NAME,TYPE,STRUCT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL STRUCT
      CHARACTER*(*) REF_NAME,TYPE
      INTEGER STATUS
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Whether the variance information is stored as an uncertainty array
C     or as an actual variance array, DSA_ERROR_TYPE gets its type, so
C     all we have to do is call that.
C
      CALL DSA_ERROR_TYPE (REF_NAME,TYPE,STRUCT,STATUS)
C
      END
