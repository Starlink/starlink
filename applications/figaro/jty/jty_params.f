      SUBROUTINE JTY_PARAMS(A,CENTER,HEIGHT,WIDTH)
C Routine to convert parabola polynomial coefficients to height, center, width
C
C History : Original version J. Tonry.
C Modified: TDCA 18-FEB-1999. Minor style changes.

      IMPLICIT NONE

      DOUBLE PRECISION A(3)
      REAL CENTER, HEIGHT, WIDTH

C Note potential for division by zero.
      CENTER = -A(2) / (2*A(3))
      HEIGHT = A(1) - A(2)*A(2) / (4*A(3))
      WIDTH = -2*HEIGHT / A(3)
      IF(WIDTH.GE.0) THEN
          WIDTH = SQRT(WIDTH)
      ELSE
          WIDTH = -SQRT(-WIDTH)
      END IF
      RETURN
      END
