C Program to fit the correlation peak
      SUBROUTINE JTY_PEAKFIT(NPT,VELOCITY,VMIN,VMAX,CFN,CENTER,
     :               WIDTH,A,ERROR)
C
C History:  Original version J. Tonry.
C Modified: KS 22-SEP-1983. VMIN and VMAX made parameters, test
C           that fit is to at least 3 pixels added.
C         : HME (UoE) 01-SEP-1992. The parameter statement had
C           no brackets. The Sun compiler doesn't like that.
C         : TDCA (RAL) 18-FEB-1999. Minor style changes.
C         : TDCA (RAL) 03-MAR-1999. Explictly declared V0, PEAK
C           and WIDTH.
C         : TIMJ (JACH) DO loop must take integer args

      REAL VELOCITY(NPT), CFN(NPT)
      REAL SCALE(2)
      REAL V0, PEAK, WIDTH
      DOUBLE PRECISION A(3)
      LOGICAL*1 ERROR
      PARAMETER ( PEAKFRAC = .75 )
      INTEGER NSTART, NEND

      ERROR = .FALSE.
      CMAX = 0

      NSTART = 0.1 * NPT
      NEND   = 0.9 * NPT

      DO I = NSTART, NEND
          IF(VELOCITY(I).GT.VMIN.AND.VELOCITY(I).LT.VMAX) THEN
              IF(CFN(I).GT.CMAX) THEN
                  CMAX = CFN(I)
                  IMAX = I
              END IF
          END IF
      END DO
      SCALE(1) = VELOCITY(IMAX-2)
      SCALE(2) = VELOCITY(IMAX+2)
      CALL JTY_FITLPOLY(5,VELOCITY(IMAX-2),CFN(IMAX-2),SCALE,3,A)
      CALL JTY_POLYC(3,SCALE,A,A)
      CALL JTY_PARAMS(A,V0,PEAK,WIDTH)
      IL = 0
      IR = 0
      NSEARCH = 50
      I = 1
      DO WHILE((IL.EQ.0.OR.IR.EQ.0).AND.I.LT.NSEARCH)
          IF(CFN(IMAX+I).LE.PEAK*PEAKFRAC.AND.IR.EQ.0) THEN
              IR = IMAX+I
          END IF
          IF(CFN(IMAX-I).LE.PEAK*PEAKFRAC.AND.IL.EQ.0) THEN
              IL = IMAX-I
          END IF
          I = I + 1
      END DO

C                            (test for at least 3 pixels added KS)
      IF(IL.EQ.0 .OR. IR.EQ.0 .OR. (IR-IL).LT.4) THEN
          IL = IMAX - 2
          IR = IMAX + 2
      END IF
      SCALE(1) = VELOCITY(IL)
      SCALE(2) = VELOCITY(IR)
      CALL JTY_FITLPOLY(IR-IL+1,VELOCITY(IL),CFN(IL),SCALE,3,A)
      CALL JTY_POLYC(3,SCALE,A,A)
      CALL JTY_PARAMS(A,VL,HL,WL)

      SCALE(1) = VELOCITY(IL+1)
      SCALE(2) = VELOCITY(IR-1)
      CALL JTY_FITLPOLY(IR-IL-1,VELOCITY(IL+1),CFN(IL+1),SCALE,3,A)
      CALL JTY_POLYC(3,SCALE,A,A)
      CALL JTY_PARAMS(A,VU,HU,WU)
      AVHL = .5*(CFN(IL) + CFN(IR))
      AVHU = .5*(CFN(IL+1) + CFN(IR-1))
      FRAC = (PEAK*PEAKFRAC - AVHL) / (AVHU - AVHL)

      CENTER = FRAC * VU + (1-FRAC) * VL
      WIDTH = FRAC * WU + (1-FRAC) * WL

      RETURN
      END









