C*GRCTOI -- convert character string to integer
C+
      INTEGER FUNCTION GRCTOI (S, I)
      CHARACTER*(*) S
      INTEGER I
C
C GRCTOI: attempt to read an integer from a character string, and return
C the result. No attempt is made to avoid integer overflow. A valid 
C integer is any sequence of decimal digits.
C
C Returns:
C  GRCTOI           : the value of the integer; if the first character
C                    read is not a decimal digit, the value returned
C                    is zero.
C Arguments:
C  S      (input)  : character string to be parsed.
C  I      (in/out) : on input, I is the index of the first character
C                    in S to be examined; on output, either it points
C                    to the next character after a valid integer, or
C                    it is equal to LEN(S)+1.
C
C--
C  1985 Oct  8 - New routine, based on CTOI (T. J. Pearson).
C  1997 Jun  3 - allow leading + or - sign (TJP).
C-----------------------------------------------------------------------
      INTEGER K, SIGN, X
      CHARACTER*1 DIGITS(0:9)
      DATA  DIGITS/'0','1','2','3','4','5','6','7','8','9'/
C
      X = 0
      SIGN = +1
      IF (I.GT.LEN(S)) GOTO 30
      IF (S(I:I).EQ.'+') THEN
         I = I+1
      ELSE IF (S(I:I).EQ.'-') THEN
         I = I+1
         SIGN = -1
      END IF
 10   IF (I.GT.LEN(S)) GOTO 30
      DO 20 K=0,9
         IF (S(I:I).EQ.DIGITS(K)) THEN
            X = X*10 + K
            I = I+1
            GOTO 10
         END IF
 20   CONTINUE
 30   GRCTOI = X*SIGN
      RETURN
      END
