      REAL FUNCTION JTY_LPOLY2D(X,Y,SCALE,NCX,NCY,COEFF)
* The desired fit has NCX coefficients in X and NCY in Y.
      REAL*8 COEFF(1), XSC, YSC, JTY_LPI, TEMP
      REAL*4 SCALE(1)

      NFIT = NCX*NCY

      XSC = SCALE(1) * (X - SCALE(2))
      YSC = SCALE(3) * (Y - SCALE(4))

      TEMP = 0

      DO 20 J = 1,NFIT
      JX = (J-1)/NCY + 1
      JY = MOD(J-1,NCY) + 1
20    TEMP = TEMP + COEFF(J) * JTY_LPI(JX,XSC) * JTY_LPI(JY,YSC)

      JTY_LPOLY2D = TEMP

      RETURN
      END
