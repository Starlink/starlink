       SUBROUTINE SMOOTH(X,YIN,YOUT,NPOINT,SIGMA)

       REAL X(1), YIN(1), YOUT(1)
       REAL SIGMA
       INTEGER NPOINT
       REAL GAUSS(0:      100)
       REAL THRSIG, SIGMA2, SUMWT, SUM, DX, X0, BINSZE, FRAC
       INTEGER I, J, INT

*  Local Data:
       DATA GAUSS/1.00000, 0.99910, 0.99641, 0.99193, 0.98570, 0.97775,
     :            0.96812, 0.95686, 0.94403, 0.92969, 0.91393, 0.89682,
     :            0.87845, 0.85890, 0.83828, 0.81669, 0.79422, 0.77097,
     :            0.74707, 0.72260, 0.69768, 0.67240, 0.64688, 0.62120,
     :            0.59547, 0.56978, 0.54422, 0.51887, 0.49381, 0.46912,
     :            0.44486, 0.42109, 0.39788, 0.37527, 0.35331, 0.33204,
     :            0.31149, 0.29168, 0.27264, 0.25439, 0.23693, 0.22027,
     :            0.20442, 0.18936, 0.17510, 0.16162, 0.14891, 0.13696,
     :            0.12573, 0.11522, 0.10540, 0.09624, 0.08772, 0.07981,
     :            0.07248, 0.06571, 0.05946, 0.05371, 0.04843, 0.04359,
     :            0.03916, 0.03512, 0.03144, 0.02810, 0.02506, 0.02231,
     :            0.01983, 0.01760, 0.01558, 0.01378, 0.01216, 0.01071,
     :            0.00941, 0.00826, 0.00724, 0.00633, 0.00553, 0.00481,
     :            0.00419, 0.00364, 0.00315, 0.00273, 0.00235, 0.00203,
     :            0.00175, 0.00150, 0.00129, 0.00110, 0.00094, 0.00080,
     :            0.00068, 0.00058, 0.00049, 0.00042, 0.00035, 0.00030,
     :            0.00025, 0.00021, 0.00018, 0.00015, 0.00012/

*   Correction factors must be applied because this subroutine was originally
*   written as a normal distribution, not a gaussian.
       SIGMA2 = 2.0*SIGMA*SIGMA
       THRSIG = 4.242640687*SIGMA
       BINSZE = 0.01*THRSIG
       DO 200 I = 1, NPOINT
          X0 = X(I)
          SUM = YIN(I)
          SUMWT = 1.0
          J = I - 1
   50     CONTINUE
          IF (J.NE.0) THEN
             DX = ABS(X(J)-X0)
             IF (DX.GE.THRSIG) THEN
                J = 0
             ELSE
                FRAC = DX/BINSZE
                INT = FRAC
                FRAC = FRAC - INT
                WT = (1.0-FRAC)*GAUSS(INT) + FRAC*GAUSS(INT+1)
                SUM = SUM + WT*YIN(J)
                SUMWT = SUMWT + WT
                J = J - 1
             ENDIF
             GOTO 50
          ENDIF
          J = I + 1
  100     CONTINUE
          IF (J.LE.NPOINT) THEN
             DX = ABS(X(J)-X0)
             IF (DX.GE.THRSIG) THEN
                J = NPOINT + 1
             ELSE
                FRAC = DX/BINSZE
                INT = FRAC
                FRAC = FRAC - INT
                WT = (1.0-FRAC)*GAUSS(INT) + FRAC*GAUSS(INT+1)
                SUM = SUM + WT*YIN(J)
                SUMWT = SUMWT + WT
                J = J + 1
             ENDIF
             GOTO 100
          ENDIF
          YOUT(I) = SUM/SUMWT
  200  CONTINUE

       END
