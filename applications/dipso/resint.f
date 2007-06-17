**==RESINT.FOR
       SUBROUTINE RESINT(YFIT,ALAM,M,NPTS,COLDEN,KTEST)
       DIMENSION ALAM(M), YFIT(M)
       DATA IZ/0/, IONE/1/, OUT/1.0E-2/
       DATA DUM/1.0E-5/, DEDUM/1.0E1/, ZERO/0.0E0/, ONE/1.0E0/
       KTEST = IZ
       DO 100 I = 1, NPTS
          YFITI = COLDEN*ALAM(I)
          IF (YFITI.LE.DUM) THEN
             YFIT(I) = ZERO
          ELSEIF (YFITI.LT.DEDUM) THEN
             YFIT(I) = ONE - EXP(-YFITI)
          ELSE
             YFIT(I) = ONE
          ENDIF
  100  CONTINUE
       IF (YFIT(1).GT.OUT) KTEST = IONE
       IF (YFIT(NPTS).GT.OUT) KTEST = IONE
       RETURN
       END
