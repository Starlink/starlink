C+
      SUBROUTINE FIG_MAKE_AXIS_LABEL(WORD1,WORD2,LABEL)
C
C     F I G _ M A K E _ A X I S _ L A B E L
C
C     Joins together the two strings WORD1 and WORD2 after first
C     removing superfluous white space
C
C     Parameters -  (">" input , "<" output)
C
C     (>) WORD1   (Character) The 1st (ie leftmost) word
C     (>) WORD2   (Character) The 2nd (right) word
C     (<) LABEL   (Character) The combined string
C
C                                     DJA / AAO. 22nd July 1987
C+
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) WORD1,WORD2,LABEL
C
C     Functions used
C
      INTEGER ICH_LEN
C
C     Local variables
C
      INTEGER LENWORD
C
      LENWORD=ICH_LEN(WORD1)
      IF (LENWORD.GT.0) THEN
         LABEL=WORD1(:LENWORD)//'  '//WORD2
      ELSE
         LABEL=WORD2
      END IF
C
      END
