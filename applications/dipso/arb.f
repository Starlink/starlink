**==ARB.FOR
       SUBROUTINE ARB(STEP,MK,RESP,MID,MAX,IK,LTEST,RF,POSN,NRF,NUT)
       DIMENSION RESP(MK), RF(1), POSN(1)
       COMMON /MAD5  / MARB
       DATA ZERO/0.0E0/, IZ/0/, ONE/1.0E0/, IONE/1/, IT/2/,
     :      HALF/ - 5.0E-1/
       DATA TWO/2.0E0/
       LTEST = IZ
       KTEST = IONE
       L = IZ
       DO 100 I = 2, NRF
          IF (RF(I).GT.RF(KTEST)) KTEST = I
  100  CONTINUE
       KTEST1 = NRF
       NRFF = NRF + IONE
       DO 200 I = 2, NRF
          II = NRFF - I
          IF (RF(II).GT.RF(KTEST1)) KTEST1 = II
  200  CONTINUE
       IF (KTEST.NE.KTEST1) THEN
          KTEST = (KTEST+KTEST1)/IT
          POSNM = (FLOAT(KTEST1)+FLOAT(KTEST))/TWO
          RFM = RF(KTEST)
          L = IONE
       ENDIF
       IF (KTEST.NE.IONE .AND. KTEST.NE.NRF) THEN
          IF (L.LE.IZ) THEN
             KIT = KTEST - IONE
             KAT = KTEST + IONE
             X1 = POSN(KIT)
             X2 = POSN(KTEST)
             X3 = POSN(KAT)
             XX1 = X1*X1
             XX2 = X2*X2
             XX3 = X3*X3
             Y1 = RF(KIT)
             Y2 = RF(KTEST)
             Y3 = RF(KAT)
             CALL DETEV(XX1,XX2,XX3,X1,X2,X3,ONE,ONE,ONE,DEL)
             CALL DETEV(Y1,Y2,Y3,X1,X2,X3,ONE,ONE,ONE,A)
             CALL DETEV(XX1,XX2,XX3,Y1,Y2,Y3,ONE,ONE,ONE,B)
             CALL DETEV(XX1,XX2,XX3,X1,X2,X3,Y1,Y2,Y3,C)
             DEL = ONE/DEL
             A = A*DEL
             B = B*DEL
             C = C*DEL
             POSNM = HALF*B/A
             RFM = (A*POSNM+B)*POSNM + C
          ENDIF
!          WRITE(*,*) POSNM,POSN(IONE),STEP
          MID = (POSNM-POSN(IONE))/STEP
          MAX = (POSN(NRF)-POSN(IONE))/STEP
          IF (MAX.LE.MK) THEN
             DO 220 I = 1, MAX
                POS = POSNM - (MID-I)*STEP
                RESP(I) = RLINT(POS,RF,POSN,MM,NRF)
  220        CONTINUE
             RETURN
          ELSE
             IK = IONE
             RETURN
          ENDIF
       ELSE
          WRITE (6,99002)
          LTEST = IONE
          RETURN
       ENDIF
99001  FORMAT (' CONVOLUTION WITH ARBITRARY PSRF')
99002  FORMAT (' ERROR CV-3 CONVOLUTION ABANDONED')
       END
