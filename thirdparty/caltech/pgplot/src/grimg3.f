C*GRIMG3 -- gray-scale map of a 2D data array, using dither
C+
      SUBROUTINE GRIMG3 (A, IDIM, JDIM, I1, I2, J1, J2,
     1                   BLACK, WHITE, PA, MODE)
      INTEGER IDIM, JDIM, I1, I2, J1, J2, MODE
      REAL    A(IDIM,JDIM)
      REAL    BLACK, WHITE
      REAL    PA(6)
C--
C 2-Sep-1994 - moved from GRGRAY [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER  I,IX,IX1,IX2,IY,IY1,IY2,J
      REAL     DEN,VALUE,BW
      REAL     XXAA,XXBB,YYAA,YYBB,XYAA,XYBB,YXAA,YXBB,XYAAIY,YXAAIY
      INTEGER  M, IAA, ICC, JRAN, ILAST, JLAST, IXSTEP, IYSTEP
      REAL     RAND, RM, FAC, FACL
      PARAMETER (M=714025, IAA=1366, ICC=150889, RM=1.0/M)
      PARAMETER (FAC=65000.0)
      INTRINSIC MOD, NINT, REAL, LOG
C-----------------------------------------------------------------------
C
      IF (MODE.LT.0 .OR. MODE.GT.2) RETURN
C
C Initialize random-number generator (based on RAN2 of Press et al.,
C Numerical Recipes)
C
      JRAN = 76773
C
      IX1 = NINT(GRXMIN(GRCIDE))+1
      IX2 = NINT(GRXMAX(GRCIDE))-1
      IY1 = NINT(GRYMIN(GRCIDE))+1
      IY2 = NINT(GRYMAX(GRCIDE))-1
      DEN = PA(2)*PA(6)-PA(3)*PA(5)
C
C Calculate constants.
C
      BW   = ABS(BLACK-WHITE)
      FACL = LOG(1.0+FAC)
      XXAA = (-PA(6))*PA(1)/DEN
      XXBB = PA(6)/DEN
      XYAA = (-PA(3))*PA(4)/DEN
      XYBB = PA(3)/DEN
      YYAA = (-PA(2))*PA(4)/DEN
      YYBB = PA(2)/DEN
      YXAA = (-PA(5))*PA(1)/DEN
      YXBB = PA(5)/DEN
C
C Choose step size: at least 1/200 inch, assuming the line-width
C unit is 1/200 inch.
C
      IXSTEP = MAX(1,NINT(GRWIDT(GRCIDE)*GRPXPI(GRCIDE)/200.0))
      IYSTEP = MAX(1,NINT(GRWIDT(GRCIDE)*GRPYPI(GRCIDE)/200.0))
C
C Draw dots.
C
      ILAST = 0
      JLAST = 0
      DO 120 IY=IY1,IY2,IYSTEP
          XYAAIY = XXAA-XYAA-XYBB*IY
          YXAAIY = YYAA+YYBB*IY-YXAA
          DO 110 IX=IX1,IX2,IXSTEP
              I = NINT(XYAAIY+XXBB*IX)
              IF (I.LT.I1.OR.I.GT.I2) GOTO 110
              J = NINT(YXAAIY-YXBB*IX)
              IF (J.LT.J1.OR.J.GT.J2) GOTO 110
              IF (I.NE.ILAST .OR. J.NE.JLAST) THEN
                  ILAST = I
                  JLAST = J
                  VALUE = ABS(A(I,J)-WHITE)/BW
                  IF (MODE.EQ.0) THEN
C                     -- "linear"
                      CONTINUE
                  ELSE IF (MODE.EQ.1) THEN
C                     -- "logarithmic"
                      VALUE = LOG(1.0+FAC*VALUE)/FACL
                  ELSE IF (MODE.EQ.2) THEN
C                     -- "square root"
                      VALUE = SQRT(VALUE)
                  END IF
              END IF
              JRAN = MOD(JRAN*IAA+ICC, M)
              RAND = JRAN*RM
              IF (VALUE.GT.RAND) CALL GRDOT0(REAL(IX),REAL(IY))
  110     CONTINUE
  120  CONTINUE
C-----------------------------------------------------------------------
       END
