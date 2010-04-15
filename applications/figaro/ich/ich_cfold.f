C+
      INTEGER FUNCTION ICH_CFOLD(STRING,QUOTES)
C
C     I C H _ C F O L D
C
C     Conditional FOLD of a character string into upper case
C
C     ICH_CFOLD is intended for use where not all of a string
C     is to be folded.  Strings within sets of delimiting
C     characters remain unfolded.  Delimiting characters
C     come in pairs - one starts a delimited string, the other
C     ends it.  The two members of a pair may be the same,
C     such as ' and ', or they may differ, such as [ and ].
C
C     Parameters -   (">" input, "!" modified, "<" output)
C
C     (!) STRING     (Character) The string to be folded.
C
C     (>) QUOTES     (Character) A string consisting of pairs
C                    of characters.  The first of each pair
C                    is the starting delimiter for an unfolded
C                    string, the second is the terminator.
C                    If the length of QUOTES is odd, it is
C                    assumed that the terminator for the last
C                    delimiter is the same character.
C
C     Returns -  (if called as a function)
C
C     (<) ICH_CFOLD  (Integer) The number - starting from 1 -
C                    of the last non-blank character in STRING.
C                    In some cases this is the logical length
C                    of STRING.
C
C     Example:
C
C     CHARACTER STRING*38
C
C     STRING = 'String with "quotes" and [ braces ] '
C
C     N = ICH_CFOLD (STRING,'[]""')
C
C     will return  N = 35, and
C                  STRING = 'STRING WITH "quotes" AND [ braces ]   '
C
C                                           KS / UCL  17th June 1982
C+
      CHARACTER*(*) STRING,QUOTES
C
      LOGICAL INQUOTE
      CHARACTER CHR,LITLA,LITLZ,DTERM,ICH_LOWER
C
C     Setting little a and little z like this ensures that the
C     program will survive conversion to upper case.
C
      LITLA=ICH_LOWER('A')
      LITLZ=ICH_LOWER('Z')
C
      ICH_CFOLD=0
      INQUOTE=.FALSE.
C
      DO I=1,LEN(STRING)
         CHR=STRING(I:I)
         IF (INQUOTE) THEN
            IF (CHR.EQ.DTERM) THEN
               INQUOTE=.FALSE.
            END IF
         ELSE
            IF ((CHR.GE.LITLA).AND.(CHR.LE.LITLZ)) THEN
               STRING(I:I)=CHAR(ICHAR(CHR)-32)
            ELSE
               DO J=1,LEN(QUOTES),2
                  IF (CHR.EQ.QUOTES(J:J)) THEN
                     INQUOTE=.TRUE.
                     IF (J.EQ.LEN(QUOTES)) THEN
                        DTERM=CHR
                     ELSE
                        DTERM=QUOTES(J+1:J+1)
                     END IF
                  END IF
                END DO
             END IF
          END IF
          IF (CHR.NE.' ') THEN
             ICH_CFOLD=I
          END IF
      END DO
C
      END
