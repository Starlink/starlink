C+
      INTEGER FUNCTION GEN_TRNLOG(INSTRING,OUTSTRING,LENGTH)
C
C     G E N _ T R N L O G
C Description:
C     Translates a logical name until it is fully translated,
C     using successive calls to LIB$SYS_TRNLOG.
C
C     Parameters   (">" input, "<" output)
C
C     (>) INSTRING    (Character) Contains the logical name
C                     to be translated.
C     (<) OUTSTRING   (Character) Contains the result of the
C                     translation, blank filled on the right
C                     or truncated.
C     (<) LENGTH      (Integer) The number of characters in the
C                     translated string - ie the result is the
C                     string OUTSTRING(:LENGTH)
C
C     Returns -  (if called as a function)
C
C     (<) GEN_TRNLOG  (Integer) The status of the translation.
C                     0 => No error, name translated OK.
C                     Anything else an error
C
C     Subroutines / functions called -
C
C     PSX_GETENV  (PSX) Translate environment variable
C
C                                       KS / CIT  31st Jan 1983
C     History:
C
C     2nd  Mar 1988.  CKL / CIT. Modified for Convex Unix.
C     12th Mar 1989.  SNS / CIT. Unix & VMS versions merged.
C     19th Mar 1993. TNW/Durham. Call PSX_GETENV
C+
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) INSTRING,OUTSTRING
      INTEGER LENGTH
C
C     Local variables
C
      INTEGER STATUS,CHR_LEN
C
      STATUS = 0
      CALL PSX_GETENV(INSTRING,OUTSTRING,STATUS)
      LENGTH = CHR_LEN(OUTSTRING)
      GEN_TRNLOG=STATUS
      END
