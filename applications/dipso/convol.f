**==CONVOL.FOR
       SUBROUTINE CONVOL(X,Y,YY,N,M,MODE1,MK,KTEST,IK,JTEST,WID,RESP,
     :                   LTEST,MAX,RF,POSN,NRF,NUT,STEP)
       DIMENSION X(M), Y(M), YY(M)
       DIMENSION RESP(MK), RF(1), POSN(1)
       DATA ONE/1.0E0/, IONE/1/, ZERO/0.0E0/, IZ/0/, TEST/1.0E-3/
       DATA DIABLE/5.0E-1/, HUN/2.0E2/, NSAFE/3/
       DO 100 I = 1, N
          YY(I) = ZERO
          Y(I) = ONE - Y(I)
  100  CONTINUE
       WID = RINTEG(X,Y,M,N)
       KTEST = IZ
       JTEST = IZ
       IK = IZ
       CALL ARB(STEP,MK,RESP,NBLUE,MAX,IK,LTEST,RF,POSN,NRF,NUT)
       IF (LTEST.GT.IZ) RETURN
       IF (IK.GT.IZ) RETURN
       LOON = NBLUE + NSAFE
       NRED = MAX - NBLUE
       LOONY = N - NRED - NSAFE
       DO 200 I = 1, LOON
          IF (Y(I).GT.TEST) KTEST = IONE
  200  CONTINUE
       DO 300 I = LOONY, N
          IF (Y(I).GT.TEST) KTEST = IONE
  300  CONTINUE
       IF (KTEST.GT.IZ) RETURN
       SUM = ZERO
       SUM1 = ZERO
       DO 400 I = LOON, LOONY
          YI = Y(I)
          SUM = SUM + YI
          IF (YI.GE.TEST) THEN
             KIT = I - NBLUE + IONE
             KAT = I + NRED
             NN = IZ
             DO 320 K = KIT, KAT
                NN = NN + IONE
                YY(K) = YY(K) + YI*RESP(NN)
  320        CONTINUE
          ENDIF
  400  CONTINUE
       DO 500 I = 1, N
          SUM1 = SUM1 + YY(I)
  500  CONTINUE
       SUM = SUM/SUM1
       DO 600 I = 1, N
          Y(I) = YY(I)*SUM
  600  CONTINUE
       WID1 = RINTEG(X,Y,M,N)
       WID = (WID1-WID)*HUN/(WID1+WID)
       IF (ABS(WID).LE.DIABLE) THEN
          DO 650 I = 1, N
             Y(I) = ONE - Y(I)
  650     CONTINUE
          RETURN
       ELSE
          JTEST = IONE
          RETURN
       ENDIF
       END
