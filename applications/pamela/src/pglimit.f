      SUBROUTINE PGLIMIT( NPLOT, XPLOT, XSMALL, XLARGE, SIZE)
*
*  DETERMINES REASONABLE PLOT LIMITS
*
*  Input:
*       NPLOT   = NUMBER OF VALUES TO BE PLOTTED
*       XPLOT   = ARRAY OF VALUES TO BE PLOTTED
*       SIZE    = DESIRED UNITS BETWEEN PLOT LIMITS
*                 PUT = 0. TO GET AUTO CHOICE
*  Output:
*       XSMALL  = LOWER PLOT LIMIT
*       XLARGE  = UPPER PLOT LIMIT
*
*  ADAPTED FROM 'PLOTLIM' AUG 1984 BY KEITH HORNE AT IOA
*
      REAL XPLOT(NPLOT)
      DOUBLE PRECISION CALC1, CALC2
      REAL THRESH
      INTEGER MAXTEST
      PARAMETER (THRESH=8.)     ! SIGMA LIMIT FOR REJECT
      PARAMETER (MAXTEST = 500) ! MAXIMUM NUMBER OF DATA POINTS TO CHECK
*
*  TEST INPUTS
*
      IF(NPLOT.LE.0) THEN
        PRINT *,'** NULL ARRAY IN ''PGLIMIT''.', NPLOT
        RETURN
      END IF
      UNIT = ABS( SIZE )
*
*  Compute mean and sigma of plot data (use only MAXTEST points)
*
      CALC1=0.D0
      CALC2=0.D0
      NTEST = MIN(NPLOT,MAXTEST)
      SCALE = REAL(NPLOT-1)/REAL(NTEST-1)
      DO ITEST=0,NTEST-1
        I = NINT( SCALE*REAL(ITEST) ) + 1
        CALC1 = CALC1 + XPLOT(I)
        CALC2 = CALC2 + XPLOT(I)*XPLOT(I)
      END DO
      XMU = REAL(CALC1/REAL(NTEST))
      XSG = REAL(CALC2 - CALC1*XMU)
      IF(NTEST.GT.1) XSG = XSG / (NTEST-1)
      XSG = SQRT( MAX( XSG,UNIT/2. ) )
*
*  Find extrema, but reject THRESH*SIGMA deviants
*
      XLARGE=XMU
      XSMALL=XMU
      DO ITEST=0, NTEST-1
        I = NINT( REAL(ITEST)*SCALE ) + 1
        TEST = 0.
        IF( XSG.GT.0. ) TEST = (XPLOT(I) - XMU) / XSG
        IF(ABS(TEST).LT.THRESH) THEN
          XLARGE = MAX(XLARGE, XPLOT(I))
          XSMALL = MIN(XSMALL, XPLOT(I))
        END IF
      END DO
*
* If UNIT = 0. then set size tick automatically
*
      IF(UNIT.EQ.0.) THEN
        UNIT = 10.**(REAL(NINT(LOG10(XLARGE-XSMALL)-1.7)))
      END IF
*
*  Round limits to nearest multiples of UNIT below and above
*  XLARGE and XSMALL, with a test to avoid integer overflow
*
      IF( UNIT.GT.0. .AND. ABS(XLARGE) .LT. 1.E8*UNIT
     &  .AND. ABS(XSMALL).LT.1.E8*UNIT) THEN
        XLARGE = UNIT*REAL(NINT(XLARGE/UNIT+0.5))
        XSMALL = UNIT*REAL(NINT(XSMALL/UNIT-0.5))
      ELSE IF(UNIT.GT.0. .AND. ABS(XLARGE-XSMALL) .LT. 
     &  1.E8*UNIT) THEN
        XMID = (XSMALL+XLARGE)/2.
        XDEV = UNIT*REAL(NINT((XLARGE-XMID)/UNIT+0.5))
        XLARGE = XMID + XDEV
        XSMALL = XMID - XDEV
      END IF
*
      RETURN
      END
