#if HAVE_CONFIG_H
#  include "config.h"
#endif
C+
      INTEGER FUNCTION ICH_CLEAN(STRING)
C
C     I C H _ C L E A N
C
C     Cleans unprintable characters from a character string.
C
C     Parameters -   ("!" modified,  ">" output)
C
C     (!) STRING   (Character variable) Passed as the string
C                  to be cleaned.  Returns with the cleaned
C                  string.
C
C     Returns -  (if called as a function)
C
C     (<) ICH_CLEAN (Integer) The number of the last non-blank
C                   character in the cleaned string.  In some
C                   circumstances this is the logical length of
C                   the string.
C
C     Note -
C
C     The cleaning process sets to blanks the first non-printing
C     character in the string AND all subsequent characters.
C
C     This function explicitly assumes that the string contains
C     ASCII characters.
C
C                                     KS / UCL  23rd June 1982
C+
      CHARACTER*(*) STRING
C
      INTEGER I,ICH,BLANK,DEL
C
#if HAVE_BOZ
      DATA BLANK,DEL/Z'20',Z'7F'/
#elif HAVE_OLD_TYPELESS_BOZ
      DATA BLANK,DEL/'20'X,'7F'X/
#else
      Do not have X'' or ''X typeless BOZ
#endif
C
      ICH_CLEAN=0
      DO I=1,LEN(STRING)
         ICH=ICHAR(STRING(I:I))
         IF ((ICH.LT.BLANK).OR.(ICH.GE.DEL)) THEN
            STRING(I:)=' '
            GO TO 320
         END IF
         IF (ICH.NE.BLANK) THEN
            ICH_CLEAN=I
         END IF
      END DO
C
  320 CONTINUE
C
      END
