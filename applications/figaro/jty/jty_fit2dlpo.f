      SUBROUTINE JTY_FIT2DLPOLY(NPT,X,Y,Z,SCALE,NCX,NCY,COEFF)
* X, Y and Z are data arrays with NPT points to fit.
* SCALE receives XMIN, XMAX, YMIN and YMAX and returns AX, BX, AY, BY
* where X' = AX * (X - BX)  and Y' = AY * (Y - BY)
* A two-dimensional polynomial of degree NCX-1 in X and NCY-1 in Y
* is fit and the coefficients are returned in COEFF.
*
* The parameter statement had no brackets. The Sun compiler doesn't like
* that. Changed WRITE to PAR_WRUSER.
*                           HME/UoE, Starlink. 12 Oct 1992.
      PARAMETER (MAXFIT=8)
      REAL*8 COV(MAXFIT*MAXFIT*MAXFIT*MAXFIT), VEC(MAXFIT*MAXFIT)
      REAL*8 COEFF(1)
      REAL*8 AX, BX, AY, BY, XSC, YSC, JTY_LPI, DET
      REAL*4 X(1), Y(1), Z(1), SCALE(4)
      INTEGER STATUS

      AX = 2.0D0 / (SCALE(2) - SCALE(1))
      BX = 0.5D0 * (SCALE(2) + SCALE(1))
      AY = 2.0D0 / (SCALE(4) - SCALE(3))
      BY = 0.5D0 * (SCALE(4) + SCALE(3))

      NFIT = NCX*NCY

      DO 5 J = 1,NFIT
5     VEC(J) = 0
      DO 6 I = 1,NFIT*NFIT
6     COV(I) = 0

      DO 10 N = 1,NPT
      XSC = AX * (X(N) - BX)
      YSC = AY * (Y(N) - BY)

      DO 20 J = 1,NFIT
      JX = (J-1)/NCY + 1
      JY = MOD(J-1,NCY) + 1
      COEFF(J) = JTY_LPI(JX,XSC) * JTY_LPI(JY,YSC)
      VEC(J) = VEC(J) + COEFF(J) * Z(N)
      DO 20 I = 1,J
20    COV(I+(J-1)*NFIT) = COV(I+(J-1)*NFIT) + COEFF(I) * COEFF(J)

10    CONTINUE

      DO 30 J = 2,NFIT
      DO 30 I = 1,J-1
30    COV(J+(I-1)*NFIT) = COV(I+(J-1)*NFIT)

      CALL JTY_INVERT(NFIT,COV,COEFF,DET)
      IF(DET.EQ.0) THEN

          DO 35 J = 1,NFIT
35        COEFF(J) = 0

          CALL PAR_WRUSER('JTY_FIT2DLPOLY: singular covariance matrix',
     :       STATUS)
          RETURN
      ENDIF

      DO 40 J = 1,NFIT
      COEFF(J) = 0
      DO 40 I = 1,NFIT
40    COEFF(J) = COEFF(J) + COV(I+(J-1)*NFIT) * VEC(I)

      SCALE(1) = AX
      SCALE(2) = BX
      SCALE(3) = AY
      SCALE(4) = BY

      RETURN
      END
