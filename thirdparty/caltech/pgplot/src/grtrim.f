C*GRTRIM -- length of string excluding trailing blanks
C+
      INTEGER FUNCTION GRTRIM(S)
      CHARACTER*(*) S
C
C Find the length of a character string excluding trailing blanks.
C A blank string returns a value of 0.
C
C Argument:
C  S      (input)  : character string.
C
C Returns:
C  GRTRIM          : number of characters in S, excluding trailing
C                    blanks, in range 0...LEN(S). A blank string
C                    returns a value of 0.
C
C Subroutines required:
C  None
C
C Fortran 77 extensions:
C  None
C
C History:
C  1987 Nov 12 - TJP.
C-----------------------------------------------------------------------
      INTEGER  I
C
      IF (S.EQ.' ') THEN
          GRTRIM = 0
      ELSE
          DO 10 I=LEN(S),1,-1
              GRTRIM = I
              IF (S(I:I).NE.' ') GOTO 20
   10     CONTINUE
          GRTRIM = 0
   20     CONTINUE
      END IF
      END
