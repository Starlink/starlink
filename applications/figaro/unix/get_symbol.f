C+
      LOGICAL FUNCTION GET_SYMBOL(SYMBOL,DSTSTR)
C
C     L I B $ S Y S _ T R N L O G
C Description:
C     A simple version of the VMS system call.  Symbols are replaced by
C     Environment variables.
C
C     Parameters -   (">" input, "!" modified, "<" output)
C
C     (>) SYMBOL     (String) The symbol to lookup
C     (<) DSTSTR     (String) The symbol value
C
C     Return value -
C
C        TRUE if ok
C
C     History:
C
C     14th Sep 1989.  SNS / CIT. Original.
C     19th Mar 1993 TNW/Durham, use psx_getenv, return logical status
C+
      IMPLICIT NONE
C
      CHARACTER SYMBOL*(*)
      CHARACTER DSTSTR*(*)
C
C
C     Local Variables
C
      INTEGER STATUS

      STATUS = 0
      CALL PSX_GETENV(SYMBOL,DSTSTR,STATUS)
      GET_SYMBOL = STATUS.EQ.0
      END
