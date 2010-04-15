C+
C                           D S A _ S T A T U S
C  Routine name:
C     DSA_STATUS
C
C  Function:
C     Determine if a DSA system error has occurred.
C
C  Description:
C     The DSA system maintains an overall status value that is cleared
C     by a call to DSA_OPEN and will normally be set (if any error has
C     occurred) during the final call to DSA_CLOSE. This routine returns
C     the value of that status variable, and so indicates if any error
C     occurred between the last call to DSA_OPEN and the corresponding
C     call to DSA_CLOSE. It is intended for use by master programs that
C     invoke applications code that uses the DSA library but which do not
C     themselves return any indication of success or failure, and allows
C     the master program to discover for itself if any error occurred.
C     (Early versions of Figaro had the DSA system call FIG_SETERR, but
C     the layering of the subroutine libraries was confused by this, since
C     normally DSA routines do not call FIG routines.)
C
C  Language: Fortran
C
C  Call:
C     STATUS=DSA_STATUS()
C
C  Parameters: None
C
C  Returns:
C     (<) STATUS    (Integer) The overall DSA system status code. Zero
C                   indicates OK, non-zero indicates some error (a DSA__
C                   error value).
C
C  Prior requirements:
C     A DSA_OPEN, DSA_CLOSE sequence should have been performed.
C
C  Version date: 17th Dec 1992.
C
C  Support: K. Shortridge, AAO
C-
C  Common variables used:
C     (>) SYS_STATUS  (Integer) Overall DSA system status.
C
C  History:
C     17th Dec 1992.  Original version. KS/AAO.
C+
      INTEGER FUNCTION DSA_STATUS()
C
      IMPLICIT NONE
C
C     DSA common variables
C
      INCLUDE 'DSA_COMMON'
C
C     Return the overall DSA system status
C
      DSA_STATUS=SYS_STATUS
C
      END
