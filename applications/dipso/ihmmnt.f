*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
*
*
*   SUBROUTINE IHMMNT
*
*   Calculates first and second moments of a P Cygni profile
*
*
*   IMPORTS:
*       MAXSZE    (INTEGER) Maximum size of X,Y arrays
*       NPOINT    (INTEGER) No. of points in X,Y arrays
*
*       WAV0      (REAL)    (Weighted) central wavelength
*       VINF      (REAL)    Terminal velocity (km/s)
*       WAVE      (REAL)    Array of x points
*       FLUX      (REAL)    Array of y points
*
*       SUBCHK    (LOGICAL) TRUE on successul completion
*
*
*   EXPORTS:
*
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
       SUBROUTINE IHMMNT(WAVE,FLUX,WAV0,VINF,MAXSZE,NPOINT,SUBCHK)
*
*
       IMPLICIT NONE
*
       INTEGER I, J, K, MAXSZE, NPOINT
       INTEGER I1, I2
*
       REAL WAVE(MAXSZE), FLUX(MAXSZE)
       REAL WAV0, VINF
       REAL W0, W1
       REAL X0, X1, X2, Y0, Y1, Y2
       REAL YY1, YY2
       REAL SLOPE
       REAL FC1, FC2, INCR
       REAL TEMP
*
       LOGICAL SUBCHK
*
*
*
       W0 = 0.0
       W1 = 0.0
       SUBCHK = .TRUE.
*
*
*
       CALL CPAIR(X1,Y1,X2,Y2)
       IF (X1.EQ.X2) THEN
   50     CONTINUE
          WRITE (*,'('' MOMENT:  no X range'')')
          SUBCHK = .FALSE.
          GOTO 600
       ELSEIF (X1.GT.X2) THEN
          TEMP = X1
          X1 = X2
          X2 = TEMP
          TEMP = Y1
          Y1 = Y2
          Y2 = TEMP
       ENDIF
*
*
       DO 100 I = 1, NPOINT
          I1 = I
          IF (WAVE(I).GT.X1) GOTO 200
  100  CONTINUE
  200  CONTINUE
       I2 = I1
       DO 300 I = I1, NPOINT
          IF (WAVE(I).GT.X2) GOTO 400
          I2 = I
  300  CONTINUE
  400  CONTINUE
       IF (WAVE(I2).LE.WAVE(I1)) GOTO 50
*
*
       X0 = X1
       Y0 = Y1
       SLOPE = (Y2-Y1)/(X2-X1)
       X1 = WAVE(I1)
       FC1 = Y0 + SLOPE*(X1-X0)
       Y1 = (FLUX(I1)-FC1)/FC1
       YY1 = X1 - WAV0
       DO 500 I = I1 + 1, I2
          X2 = WAVE(I)
          FC2 = Y0 + SLOPE*(X2-X0)
          Y2 = (FLUX(I)-FC2)/FC2
          YY2 = X2 - WAV0
*
          W0 = W0 + (X2-X1)*(Y2+Y1)/2.0
          W1 = W1 + (X2-X1)*(Y2*YY2+Y1*YY1)/2.0
*
          X1 = X2
          Y1 = Y2
          YY1 = YY2
          FC1 = FC2
  500  CONTINUE
*
*
       INCR = (2.997925E+5/VINF)/WAV0
       W0 = W0*INCR
       W1 = W1*INCR**2
*
*
       WRITE (*,'('' MOMENT:  W0 ='',1PE10.3)') W0
       WRITE (*,'(''          W1 ='',1PE10.3)') W1

  600  CONTINUE

       END
