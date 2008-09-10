#if HAVE_CONFIG_H
#  include "config.h"
#endif
C+
      INTEGER FUNCTION ICH_TIDY(STRING)
C
C     I C H _ T I D Y
C
C     Removes unprintable characters from a character string.
C
C     Parameters -   ("!" modified,  ">" output)
C
C     (!) STRING   (Character variable) Passed as the string
C                  to be tidied. Returns with the tidied
C                  string.
C
C     Returns -  (if called as a function)
C
C     (<) ICH_TIDY  (Integer) The number of the last non-blank
C                   character in the tidied string.  In some
C                   circumstances this is the logical length of
C                   the string.
C
C     Note -
C
C     The tidying process simply changes all non-printing 
C     characters to blanks.  It is not as dramatic a process as
C     that performed by ICH_CLEAN.
C
C     This function explicitly assumes that the string contains
C     ASCII characters.
C
C                                     KS / UCL  12th Dec 1982
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
      ICH_TIDY=0
      DO I=1,LEN(STRING)
         ICH=ICHAR(STRING(I:I))
         IF ((ICH.LT.BLANK).OR.(ICH.GE.DEL)) THEN
            STRING(I:I)=' '
         END IF
         IF (ICH.NE.BLANK) THEN
            ICH_TIDY=I
         END IF
      END DO
C
      END
