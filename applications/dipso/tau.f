*
*     EVALUATION OF ABSORPTION COEFFICIENT
*
       SUBROUTINE TAU
     : (X,M,YFIT,A,NPTS,NTERMS,K,TABLE,ALAM,YFIT1,MOUT)

       INTEGER TABLE, BIKE

       COMMON /DMISCOG/ NFLAM, EWBYLM, N1
       COMMON /MAD1  / DELTA, C, WAVE0, CONST, WAVE1, AK1, F1K
       COMMON /MAD3  / WIDTH, COLDEN, NCYCLE
       COMMON /BLEEP / BLEEP

       REAL X(M), YFIT(M), ALAM(M), YFIT1(M)
       REAL A(MOUT)
       CHARACTER*1 BLEEP
       REAL COLDEN(3), WIDTH(3), NFLAM(99), EWBYLM(99)

       IZ = 0
       TOTAL = 0.0
       DO 100 I = 1, NTERMS, 3
          TOTAL = TOTAL + A(I+2)
  100  CONTINUE

       DO 200 I = 1, NPTS
          YFITI = 0.0
          WAVE = X(I)
          WAVE4 = WAVE*WAVE
          DELAM = DELTA*WAVE4
          WAVE4 = WAVE4*WAVE4
          DO 150 J = 1, NTERMS, 3
             VR = A(J+1)
             W = A(J+2)/TOTAL
             B = WAVE1/A(J)
             AA = DELAM*B
             VNU = (WAVE-WAVE0*(1.0+VR/C))*B
             YFITI = YFITI + W*CONST*WAVE4*VOIGT(VNU,AA)*B
  150     CONTINUE
          ALAM(I) = YFITI
  200  CONTINUE

*   Produce CoG

       IF (TABLE.EQ.1) THEN
          N1 = 0
          DO 250 N = 10, 18
             DO 220 NN = 1, 10
                COLD = NN*(10.0**N)
                CALL RESINT(YFIT,ALAM,M,NPTS,COLD,KTEST)
                IF (KTEST.NE.IZ) THEN
                   WRITE (*,
     :             '(''   ISCOG:  grid is too small to calculate'',
     :             '' damping section in full;''/
     :             ''   use ISOPT to increase V2-V1 range'',A)')
     :             BLEEP
                   GOTO 300
                ENDIF
                N1 = N1 + 1
                EWBYLM(N1) = RINTEG(X,YFIT,M,NPTS)/WAVE0
                NFLAM(N1) = COLD*F1K*WAVE0
  220        CONTINUE
  250     CONTINUE
       ELSE
          CALL RESINT(YFIT,ALAM,M,NPTS,COLDEN(1),KTEST)
          WIDTH(1) = RINTEG(X,YFIT,M,NPTS)
          NCYCLE = 1
          IF (KTEST.NE.IZ) THEN
             WRITE (*,
     :       '(''   ISCALC:  line is broader than X grid;''/
     :       ''   use ISOPT to increase V2-V1 range'',A)') BLEEP
          ENDIF
       ENDIF

  300  CONTINUE

       END
