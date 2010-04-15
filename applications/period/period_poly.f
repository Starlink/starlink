
      FUNCTION PERIOD_POLY(COEFF, NPOLY, X)

C============================================================================
C             NPOLY
C Evaluates   SUM COEFF(I) * X**(I-1).
C             I=1
C
C Written by Vikram Singh Dhillon @LPO 21-August-1993.
C
C  10-MAR-1997 (GJP):
C     Modified to avoid REAL*8.
C
C  07-MAY-1997 (BLY):
C     Modified power raising to use INTEGER powers not DOUBLE PRECISION.
C     which caused core dumps on Digital unix (and is clearly insane anyway).
C
C============================================================================

      IMPLICIT NONE

      INTEGER NPOLY
      DOUBLE PRECISION PERIOD_POLY, COEFF(NPOLY), X
      DOUBLE PRECISION X0, X1, X2, X3, X4, X5, X6, X7, X8, X9

      X0 = COEFF(1)
      X1 = COEFF(2)*(X)
      X2 = COEFF(3)*(X**2)
      X3 = COEFF(4)*(X**3)
      X4 = COEFF(5)*(X**4)
      X5 = COEFF(6)*(X**5)
      X6 = COEFF(7)*(X**6)
      X7 = COEFF(8)*(X**7)
      X8 = COEFF(9)*(X**8)
      X9 = COEFF(10)*(X**9)

      IF ( NPOLY.EQ.1 ) THEN
         PERIOD_POLY = X0
      ELSE IF ( NPOLY.EQ.2 ) THEN
         PERIOD_POLY = X0 + X1
      ELSE IF ( NPOLY.EQ.3 ) THEN
         PERIOD_POLY = X0 + X1 + X2
      ELSE IF ( NPOLY.EQ.4 ) THEN
         PERIOD_POLY = X0 + X1 + X2 + X3
      ELSE IF ( NPOLY.EQ.5 ) THEN
         PERIOD_POLY = X0 + X1 + X2 + X3 + X4
      ELSE IF ( NPOLY.EQ.6 ) THEN
         PERIOD_POLY = X0 + X1 + X2 + X3 + X4 + X5
      ELSE IF ( NPOLY.EQ.7 ) THEN
         PERIOD_POLY = X0 + X1 + X2 + X3 + X4 + X5 + X6
      ELSE IF ( NPOLY.EQ.8 ) THEN
         PERIOD_POLY = X0 + X1 + X2 + X3 + X4 + X5 + X6 + X7
      ELSE IF ( NPOLY.EQ.9 ) THEN
         PERIOD_POLY = X0 + X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8
      ELSE IF ( NPOLY.EQ.10 ) THEN
         PERIOD_POLY = X0 + X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9
      END IF

      RETURN
      END
