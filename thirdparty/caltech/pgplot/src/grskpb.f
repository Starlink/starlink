C*GRSKPB -- skip blanks in character string
C+
      SUBROUTINE GRSKPB (S, I)
      CHARACTER*(*) S
      INTEGER I
C
C GRSKPB: increment I so that it points to the next non-blank
C character in string S.  'Blank' characters are space and tab (ASCII 
C character value 9).
C
C Arguments:
C  S      (input)  : character string to be parsed.
C  I      (in/out) : on input, I is the index of the first character
C                    in S to be examined; on output, either it points
C                    to the next non-blank character, or it is equal
C                    to LEN(S)+1 (if all the rest of the string is 
C                    blank).
C--
C  1985 Oct 8 - New routine, based on SKIPBL (T. J. Pearson).
C-----------------------------------------------------------------------
C
   10 IF (I.GT.LEN(S)) RETURN
      IF (S(I:I).NE.' ' .AND. S(I:I).NE.CHAR(9)) RETURN
      I = I+1
      GOTO 10
      END
