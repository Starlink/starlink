      SUBROUTINE JTY_PARABOLA(JPIX,N,SLOPE,CONST,
     :   CENTER,HEIGHT,WIDTH,DATA)
C Subroutine to fit a parabola to some data
C JPIX      = element of the line peak
C N         = n where 2n+1 points are fit
C SLOPE     = slope of continuum to be subtracted
C CONST     = constant of continuum to be subtracted
C CENTER    = center of fit parabola
C HEIGHT    = height of fit parabola
C WIDTH     = width of fit parabola
C **************************CONVENTION******************************
C The first element of the array is taken to be pixel 0.0 to 1.0
C ******************************************************************
C
C Original version by J.Tonry.  Modified by KS to use real arrays for
C data
C                                              KS / CIT 12th July 1983
C
C After TAB-removal, one line exceeded column 72.
C                                    HME/UoE, Starlink. 01-SEP-1992.
      REAL*4 DATA(1)
      SUM0 = 0
      SUM1 = 0
      SUM2 = 0
      DO 10 I = -N,N
      X = JPIX - 1 + I
      Y = DATA(JPIX+I) - SLOPE*X - CONST
      SUM0 = SUM0 + Y
      SUM1 = SUM1 + I*Y
10    SUM2 = SUM2 + I*I*Y

      A = 45.*(SUM2 - N*(N+1)/3.*SUM0) /
     :    (N*(N+1)*(2*N-1)*(2*N+1)*(2*N+3))
      B = -3.*SUM1 / (N*(N+1)*(2*N+1))
      C = SUM0 / (2*N+1)
      IF(A.GE.0) THEN
          CENTER = 0
          WIDTH = 0
          HEIGHT = 0
          RETURN
      ENDIF
      CENTER = JPIX - 0.5 + B/(2*A)
      HEIGHT = C - A * (N*(N+1)/3. + B*B/(4*A*A))
      WIDTH = -2 * HEIGHT / A
      IF(WIDTH.GE.0) THEN
          WIDTH = SQRT(WIDTH)
      ELSE
          WIDTH = -SQRT(-WIDTH)
      ENDIF
      RETURN
      END
