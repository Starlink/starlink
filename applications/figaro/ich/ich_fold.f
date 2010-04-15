C+
      INTEGER FUNCTION ICH_FOLD(STRING)
C
C     I C H _ F O L D
C
C     Converts all lower case ASCII characters in a character
C     string to their upper case equivalents.
C
C     Parameters -   ("!" modified, "<" output)
C
C     (!) STRING    (Character) The string to be folded.
C
C     Returns -  (if called as a function)
C
C     (<) ICH_FOLD  (Integer) The number - starting from 1 -
C                   of the last non-blank character in the
C                   string.  In some cases this can be the
C                   logical length of the string.
C
C                                 KS / UCL  17th June 1982
C+
      CHARACTER*(*) STRING
C
      CHARACTER CHR,LITLA,LITLZ,ICH_LOWER
C
C     Setting little a and little z like this ensures that
C     the program can survive conversion into upper case.
C
      LITLA=ICH_LOWER('A')
      LITLZ=ICH_LOWER('Z')
C
      ICH_FOLD=0
      DO I=1,LEN(STRING)
         CHR=STRING(I:I)
         IF ((CHR.GE.LITLA).AND.(CHR.LE.LITLZ)) THEN
            STRING(I:I)=CHAR(ICHAR(CHR)-32)
            ICH_FOLD=I
         ELSE
            IF (CHR.NE.' ') THEN
               ICH_FOLD=I
            END IF
         END IF
      END DO
C
      END
