      REAL FUNCTION JTY_POLY2D(X,Y,NCX,NCY,COEFF)
* The desired fit has NCX coefficients in X and NCY in Y.
      REAL*8 COEFF(1), T1, T2, DX, DY

      DX = DBLE(X)
      DY = DBLE(Y)

      T2 = 0

      DO J = NCX*NCY,NCY,-NCY
          T1 = 0
          DO I = J,J-NCY+1,-1
              T1 = T1 * DY + COEFF(I)
          ENDDO
          T2 = T2 * DX + T1
      ENDDO

      JTY_POLY2D = T2

      RETURN
      END
