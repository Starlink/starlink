C*GRITOC - convert integer to character string
C+
      INTEGER FUNCTION GRITOC(INT, STR)
      INTEGER INT
      CHARACTER*(*) STR
C
C Convert integer INT into (decimal) character string in STR.
C-----------------------------------------------------------------------
      CHARACTER*10 DIGITS
      INTEGER D, I, INTVAL, J, L
      CHARACTER K
      DATA DIGITS /'0123456789'/
C
      INTVAL = ABS(INT)
      I = 0
C
C Generate digits in reverse order.
C
  10  CONTINUE
          I = I+1
          D = 1 + MOD(INTVAL, 10)
          STR(I:I) = DIGITS(D:D)
          INTVAL = INTVAL/10
          IF (I.LT.LEN(STR) .AND. INTVAL.NE.0) GOTO 10
C
C Add minus sign if necessary.
C
      IF (INT.LT.0 .AND. I.LT.LEN(STR)) THEN
          I = I+1
          STR(I:I) = '-'
      END IF
      GRITOC = I
C
C Reverse string in place.
C
      L = I/2
      DO 20 J=1,L
          K = STR(I:I)
          STR(I:I) = STR(J:J)
          STR(J:J) = K
          I = I-1
   20 CONTINUE
C-----------------------------------------------------------------------
      END
