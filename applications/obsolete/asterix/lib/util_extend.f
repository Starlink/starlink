*+  UTIL_EXTEND - Extends 'array length' for use with NAG FFT routines.
      SUBROUTINE UTIL_EXTEND (IN, OUT)
*    Description :
*      Given a value IN, returns a value OUT >= IN which is
*      suitable for use with the NAG FFT routines.
*
*    Authors :
*       Trevor ponman
*    History :
*     11-JAN-1989 :  Original  (PLA_AST88@uk.ac.bham.sr.star)
*    Type Definitions :
      IMPLICIT NONE
*    Structure definitions :
*    Import :
      INTEGER IN      ! original axis length

*    Export :
      INTEGER OUT     ! required axis length

*    Local variables :
      INTEGER M

      REAL    R, D
*-
*    If IN < 21 then return IN
      IF (IN .LT. 21) THEN
        OUT = IN
        RETURN

      END IF

*    Calculate D = log2(IN) - INT[log2(IN)]
      R = ALOG10(REAL(IN)) / ALOG10(2.0)
      M = INT(R)
      D = R - REAL(M)

      IF (IN .EQ. 2**M) THEN
        OUT = IN
        RETURN

      END IF

*    Extend in one of four ways depending on the size of D
      IF (D .LT. 0.1699) THEN
        OUT = 9 * 2**(M-3)

      ELSE IF (D .LT. 0.3219) THEN
        OUT = 5 * 2**(M-2)

      ELSE IF (D .LT. 0.5849) THEN
        OUT = 3 * 2**(M-1)

      ELSE
        OUT = 2**(M+1)

      END IF
      END
