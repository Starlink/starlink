      SUBROUTINE JTY_FITLPOLY(NPT,X,Y,SCALE,NCOEFF,COEFF)
* X and Y are data arrays with NPT points to fit.
* SCALE receives XMIN and XMAX, and returns A and B.
* where Z = A * (X - B)
* A polynomial of degree NCOEFF-1 is fit and the coefficients are
* returned in COEFF.
*
* The parameter statement had no brackets. The Sun compiler doesn't like
* that. Changed WRITE to PAR_WRUSER.
*                                HME/UoE, Starlink. 12 Oct 1992.
*
* Array bound of SCALE changed from 1 to 2.
*                                TDCA/RAL, Starlink. 26 May 1999.
*
* Return bad coeff values for singular matrices.
*                                DSB, JAC/UCLan, 26 Jan 2009.

      PARAMETER (MAXFIT=8)
      REAL*8 COV(MAXFIT*MAXFIT), VEC(MAXFIT)
      REAL*8 COEFF(3)
      REAL*8 A, B, Z, JTY_LPI, DET
      REAL*4 X(NPT), Y(NPT), SCALE(2)
      INTEGER STATUS

      A = 2.0D0 / (SCALE(2) - SCALE(1))
      B = 0.5D0 * (SCALE(2) + SCALE(1))

      DO 5 J = 1,NCOEFF
5     VEC(J) = 0
      DO 6 I = 1,NCOEFF*NCOEFF
6     COV(I) = 0

      DO 10 N = 1,NPT
      Z = A * (DBLE(X(N)) - B)

      DO 20 J = 1,NCOEFF
      COEFF(J) = JTY_LPI(J,Z)
      VEC(J) = VEC(J) + COEFF(J) * Y(N)
      DO 20 I = 1,J
20    COV((J-1)*NCOEFF+I) = COV((J-1)*NCOEFF+I) + COEFF(I) * COEFF(J)

10    CONTINUE

      DO 30 J = 2,NCOEFF
      DO 30 I = 1,J-1
30    COV((I-1)*NCOEFF+J) = COV((J-1)*NCOEFF+I)

      CALL JTY_INVERT(NCOEFF,COV,COEFF,DET)
      IF(DET.EQ.0) THEN

          DO 35 J = 1,NCOEFF
35        COEFF( J ) = 0.0

          CALL PAR_WRUSER('JTY_FITLPOLY: singular covariance matrix',
     :       STATUS)
          RETURN
      ENDIF

      DO 40 J = 1,NCOEFF
      COEFF(J) = 0
      DO 40 I = 1,NCOEFF
40    COEFF(J) = COEFF(J) + COV((J-1)*NCOEFF+I) * VEC(I)

      SCALE(1) = A
      SCALE(2) = B
      RETURN
      END


