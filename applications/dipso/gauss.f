**==GAUSS.FOR
       SUBROUTINE GAUSS(X,YFIT,Y1,NTERMS,M,KTEST,JTEST,WID)
       COMMON /MAD   / BINST, BINST1, BINST2
       DIMENSION X(M), YFIT(M), Y1(M)
       DATA ONE/1.0E0/, ZERO/0.0E0/, DIABLE/1.0E0/
       DATA HUN/2.0E2/, OUT/1.0E-6/, OUTING/ - 5.0E1/
       DATA IZ/0/, IONE/1/, NSAFE/4/, CRASH/9.9E-1/
       DATA FUDGE/1.665109E0/
       JTEST = IZ
       KTEST = IZ
       FWH = BINST*FUDGE
!     WRITE(6,5)BINST,FWH
       DO 100 I = 1, NTERMS
          Y1(I) = ZERO
          YFIT(I) = ONE - YFIT(I)
  100  CONTINUE
       WID = RINTEG(X,YFIT,M,NTERMS)
       SUM1 = ZERO
       SUM = ZERO
       DO 200 I = 1, NTERMS
          SUM1 = SUM1 + YFIT(I)
          DO 150 J = 1, NTERMS
             BUGGER = -(((X(J)-X(I))/BINST)**2)
             IF (BUGGER.GE.OUTING) THEN
                IF (YFIT(J).GE.OUT) THEN
                   Y1(I) = Y1(I) + (YFIT(J)*EXP(BUGGER)/BINST)
                ENDIF
             ENDIF
  150     CONTINUE
          SUM = SUM + Y1(I)
  200  CONTINUE
       SUM = SUM1/SUM
       DO 300 I = 1, NTERMS
          Y1(I) = Y1(I)*SUM
  300  CONTINUE
       WID1 = RINTEG(X,Y1,M,NTERMS)
       WID = (WID1-WID)*HUN/(WID1+WID)
       IF (ABS(WID).LE.DIABLE) THEN
          DO 350 I = 1, NTERMS
             YFIT(I) = ONE - Y1(I)
  350     CONTINUE
          KTEST = IZ
          NS = NTERMS - NSAFE
          NS1 = NS + IONE
          DO 400 I = 1, NSAFE
             II = NS1 - I
             IF (YFIT(I).LE.CRASH) KTEST = IONE
             IF (YFIT(II).LE.CRASH) KTEST = IONE
  400     CONTINUE
          RETURN
       ELSE
          JTEST = IONE
          RETURN
       ENDIF
99001  FORMAT ('   Convolution with Gaussian, b = ',E10.2,' km/s',
     :         ' FWHM = ',E10.2,' km/s')
       END
