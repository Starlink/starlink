      SUBROUTINE RTD1_BINOM (X, M, VAL)
C----------------------------------------------------------------------
C   BINOM generates binomial coefficients for use in the Everett
C   interpolation routines. It is called only by SETCOF.  From the
C   Everett interpolation package originally coded by Larry Goad, KPNO.
C----------------------------------------------------------------------
      INTEGER*2  M
      DOUBLE PRECISION X, VAL(1)
      INTEGER*2  I
      DOUBLE PRECISION R, XL
C-----------------------------------------------------------------------
      VAL(1) = 1.
      R = 0.0D0
      XL = X + 1.
C
      DO 20 I = 1,M
         XL = XL - 1.
         R = R + 1.
         VAL(I+1) = VAL(I) * XL / R
 20   CONTINUE
C
      RETURN
      END
