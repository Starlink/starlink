      INTEGER FUNCTION GRCTOI (S, I)
*+
*    - - - - - - - -
*      G R C T O I     (GKS emulation of GRPCKG)
*    - - - - - - - -
*
*    Convert character string to integer
*
*    Attempt to read an integer from a character string, and return
*    the result. No attempt is made to avoid integer overflow. A valid
*    integer is any sequence of decimal digits.
*
*    Inputs
*       S         c  character string to be parsed.
*       I         i  on input, I is the index of the first character
*                    in S to be examined; on output, either it points
*                    to the next character after a valid integer, or
*                    it is equal to LEN(S)+1.
*
*    Outputs
*       GRCTOI    i  the value of the integer; if the first character
*                    read is not a decimal digit, the value returned
*                    is zero.
*
*   D.L.Terrett  Starlink  Sep 1993
*+
      IMPLICIT NONE

      CHARACTER*(*) S
      INTEGER I

      INTEGER K
      CHARACTER*1 DIGITS(0:9)
      DATA  DIGITS/'0','1','2','3','4','5','6','7','8','9'/

      GRCTOI = 0
   10 IF (I.GT.LEN(S)) RETURN
      DO 20 K=0,9
          IF (S(I:I).EQ.DIGITS(K)) GOTO 30
   20 CONTINUE
      RETURN
   30 GRCTOI = GRCTOI*10 + K
      I = I+1
      GOTO 10
      END
