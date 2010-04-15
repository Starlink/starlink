      DOUBLE PRECISION FUNCTION D_PROBST(T, IDF, IFAULT)

*+
* Name:
*     D_PROBST

* Invocation:
*     (DOUBLE PRECISION) = D_PROBST(T, IDF, IFAULT)

* Description:
*     Calculates Student T probability (lower Tail)
*     Algorithm AS 3  APPL. STATIST. (1968) VOL.17, P.189
*
* Purpose:
*     Calculates Student T probability (lower Tail)

*

* Arguments:
*  T=DOUBLE (Given)
*     Tail percentile
*  IDF=DOUBLE (Given)
*     Degrees of Freedom
*  IFAULT=INTEGER (Returned)
*     0 on Success

* Author:
*     JWP: Manchester, March 1997
*




      INTEGER IDF, IFAULT, IM2, IOE, KS

      DOUBLE PRECISION A, B, C, F, G1, S, FK, T, ZERO, ONE, TWO, HALF
      DOUBLE PRECISION ZSQRT, ZATAN
*
*        G1 IS RECIPROCAL OF PI
*
      DATA ZERO, ONE, TWO, HALF, G1
     $     /0.0, 1.0, 2.0,  0.5, 0.3183098862/
*
*      ZSQRT(A) = DSQRT(A)
      ZSQRT(A) = SQRT(A)
      ZATAN(A) = DATAN(A)
*
      IFAULT = 1
      D_PROBST = ZERO
      IF (IDF .LT. 1) RETURN
      IFAULT = 0
      F = IDF
      A = T / ZSQRT(F)
      B = F / (F + T ** 2)
      IM2 = IDF - 2
      IOE = MOD(IDF, 2)
      S = ONE
      C = ONE
      F = ONE
      KS = 2 + IOE
      FK = KS
      IF (IM2 .LT. 2) GOTO 20
      DO 10 K = KS, IM2, 2
      C = C * B * (FK - ONE) / FK
      S = S + C
      IF (S .EQ. F) GOTO 20
      F = S
      FK = FK + TWO
   10 CONTINUE
   20 IF (IOE .EQ. 1) GOTO 30
      D_PROBST = HALF + HALF * A * ZSQRT(B) * S
      RETURN
   30 IF (IDF .EQ. 1) S = ZERO
      D_PROBST = HALF + (A * B * S + ZATAN(A)) * G1
      RETURN
      END








