C+
      INTEGER FUNCTION ICH_DFOLD(STRING)
C
C     I C H _ D F O L D
C
C     Converts all upper case ASCII characters in a character
C     string to their lower case equivalents.
C
C     Parameters -   ("!" modified, "<" output)
C
C     (!) STRING    (Character) The string to be de-folded.
C
C     Returns -  (if called as a function)
C
C     (<) ICH_DFOLD  (Integer) The number - starting from 1 -
C                   of the last non-blank character in the
C                   string.  In some cases this can be the
C                   logical length of the string.
C
C                                 KS / UCL  17th June 1982
C     Modified:
C     25 Oct 1992  HME / UoE, Starlink.  Adapted from ICH_FOLD.
C     30 Oct 1992  HME / EIA, Starlink.  Return variable was still
C                  ICH_FOLD.
C+
      CHARACTER*(*) STRING
C
      CHARACTER CHR,LITLA,LITLZ
C
      LITLA='A'
      LITLZ='Z'
C
      ICH_DFOLD=0
      DO I=1,LEN(STRING)
         CHR=STRING(I:I)
         IF ((CHR.GE.LITLA).AND.(CHR.LE.LITLZ)) THEN
            STRING(I:I)=CHAR(ICHAR(CHR)+32)
            ICH_DFOLD=I
         ELSE
            IF (CHR.NE.' ') THEN
               ICH_DFOLD=I
            END IF
         END IF
      END DO
C
      END
