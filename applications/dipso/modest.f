       SUBROUTINE MODEST(OK,X,NPTS,XWK,XP,DXP,JFORCE)

*   Mode estimation for sampled data from a continuous distribution,
*   by estimating the rate of an inhomogeneous Poisson process by
*   Jth waiting times, selecting maximum likelihood value of J
*   Reference:  Numerical Recipes, section 13.3

       IMPLICIT NONE

       INTEGER NPTS
       REAL X(NPTS)
       LOGICAL OK, FOUND

       REAL XWK(NPTS), XP(NPTS/5), DXP(NPTS/5)


*

       INTEGER I, J, N, JFORCE
       INTEGER J1, J2, JMAX

       REAL FCTR1, FCTR2, PTEMP, PMAX
       REAL PVAL, PLIM, HMAX
       REAL XLOW, XHI
       REAL TX1, TX2, TY1, TY2
       REAL RJ, HJ, PA, PX, PAX
       REAL GAMMODE

       CHARACTER*1 BLEEP
       COMMON /BLEEP / BLEEP

*  Initialise

       OK = .TRUE.

*  Estimate window size, J

       J1 = MAX(5,MIN(9,NPTS/30))
       J1 = MIN(J1,MAX(JFORCE,3))
       J2 = NPTS/5

       IF (J1.GE.J2) THEN
          WRITE (*,'(''   MODE:  insufficient samples'',A)') BLEEP
          OK = .FALSE.
          GOTO 800
       ENDIF

*   Sort input data

       DO 100 I = 1, NPTS
          XWK(I) = X(I)
  100  CONTINUE

       CALL HEAPSRT(NPTS,XWK)

*   Evaluate p(x) for all J, store X and DX at peak p(x) for each J

       DO 200 J = J1, J2
          PVAL = 0.0
          FCTR1 = REAL(J)/REAL(NPTS)
          DO 150 I = 1, NPTS - J
             FCTR2 = XWK(I+J) - XWK(I)
             IF (FCTR2.EQ.0.0) THEN
                IF ((I+J+1).LE.NPTS) THEN
                   FCTR2 = (XWK(I+J+1)-XWK(I))*0.1
                ELSEIF (I.GT.1) THEN
                   FCTR2 = (XWK(I+J)-XWK(I-1))*0.1
                ENDIF
             ENDIF
             IF (FCTR2.EQ.0.0) FCTR2 = 1.0E-30

             PTEMP = FCTR1/FCTR2
             IF (PTEMP.GT.PVAL) THEN
                PVAL = PTEMP
                XP(J) = 0.5*(XWK(I)+XWK(I+J))
                DXP(J) = FCTR2
             ENDIF
  150     CONTINUE
  200  CONTINUE

*   Find maximum likelihood J value for XP

       IF (JFORCE.GE.3) THEN
          J1 = JFORCE
          J2 = JFORCE
       ENDIF

       HMAX = 0.0
       DO 300 J = J1, J2
          HJ = 1.0
          RJ = REAL(J)
          DO 250 N = J1, J2
             IF (J.NE.N) THEN
                PA = REAL(N)
                PX = RJ*DXP(N)/DXP(J)
                PAX = GAMMODE(PA,PX,OK)
                IF (.NOT.OK) GOTO 800
                HJ = HJ*PAX
             ENDIF
  250     CONTINUE

          IF (HJ.GT.HMAX) THEN
             HMAX = HJ
             JMAX = J
          ENDIF
  300  CONTINUE



*   Estimate 1 sigma range for mode

       N = 0
       FOUND = .FALSE.
       FCTR1 = REAL(JMAX)/REAL(NPTS)
       PMAX = FCTR1/DXP(JMAX)
       PLIM = PMAX*(1.0-1.0/SQRT(REAL(JMAX)))
       XLOW = XP(JMAX) - XWK(1)
       XHI = XWK(NPTS) - XP(JMAX)

       WRITE (*,'(''   MODE:  value'',1PE12.4)') XP(JMAX)
       DO 400 I = 1, NPTS - JMAX
          PVAL = FCTR1/(XWK(I+JMAX)-XWK(I))
  350     CONTINUE
          IF (N.NE.0) THEN
             IF (PVAL*1.001.GT.PMAX) FOUND = .TRUE.
             IF (PVAL.LT.PLIM) THEN
                IF (.NOT.FOUND) THEN
                   N = 0
                   GOTO 350
                ENDIF
                IF (I.GT.1) THEN
                   TX1 = 0.5*(XWK(I+JMAX-1)+XWK(I-1))
                   TX2 = 0.5*(XWK(I+JMAX)+XWK(I))
                   TY1 = FCTR1/(XWK(I+JMAX-1)-XWK(I-1))
                   TY2 = PVAL
                   XHI = (TY2-PLIM)/(TY2-TY1)
                   XHI = XHI*TX1 + (1.0-XHI)*TX2
                ELSE
                   XHI = 0.5*(XWK(I+JMAX)+XWK(I))
                ENDIF
                XHI = XHI - XP(JMAX)
                GOTO 500
             ENDIF
          ELSEIF (PVAL.GE.PLIM) THEN
             IF (PVAL.EQ.PMAX) FOUND = .TRUE.
             IF (I.GT.1) THEN
                TX1 = 0.5*(XWK(I+JMAX-1)+XWK(I-1))
                TX2 = 0.5*(XWK(I+JMAX)+XWK(I))
                TY1 = FCTR1/(XWK(I+JMAX-1)-XWK(I-1))
                TY2 = PVAL
                XLOW = (TY2-PLIM)/(TY2-TY1)
                XLOW = XLOW*TX1 + (1.0-XLOW)*TX2
             ELSE
                XLOW = 0.5*(XWK(I+JMAX)+XWK(I))
             ENDIF
             XLOW = XP(JMAX) - XLOW
             N = I
          ENDIF
  400  CONTINUE

       IF (N.EQ.1) THEN
          WRITE (*,
     :    '(''          Limits not found (no significant peak)'',A)')
     :    BLEEP
       ELSE
          WRITE (*,
     :    '(''          No significant upper limit located'',A)') BLEEP
          WRITE (*,'(''          Minus'',1PE12.4)') XLOW
       ENDIF
       GOTO 700

  500  CONTINUE
       WRITE (*,'(''          Plus '',1PE12.4)') XHI
       IF (N.EQ.1) THEN
          WRITE (*,
     :    '(''          No significant lower limit located'',A)') BLEEP
       ELSE
          WRITE (*,'(''          Minus'',1PE12.4)') XLOW
       ENDIF

*   Estimate 2 sigma range for mode

       IF (JMAX.GT.4) THEN

          N = 0
          FOUND = .FALSE.
          FCTR1 = REAL(JMAX)/REAL(NPTS)
          PMAX = FCTR1/DXP(JMAX)
          PLIM = PMAX*(1.0-2.0/SQRT(REAL(JMAX)))
          XLOW = XP(JMAX) - XWK(1)
          XHI = XWK(NPTS) - XP(JMAX)
          XLOW = XP(JMAX) - XWK(1)
          XHI = XWK(NPTS) - XP(JMAX)

          DO 550 I = 1, NPTS - JMAX
             PVAL = FCTR1/(XWK(I+JMAX)-XWK(I))
  520        CONTINUE
             IF (N.NE.0) THEN
                IF (PVAL*1.001.GT.PMAX) FOUND = .TRUE.
                IF (PVAL.LT.PLIM) THEN
                   IF (.NOT.FOUND) THEN
                      N = 0
                      GOTO 520
                   ENDIF
                   IF (I.GT.1) THEN
                      TX1 = 0.5*(XWK(I+JMAX-1)+XWK(I-1))
                      TX2 = 0.5*(XWK(I+JMAX)+XWK(I))
                      TY1 = FCTR1/(XWK(I+JMAX-1)-XWK(I-1))
                      TY2 = PVAL
                      XHI = (TY2-PLIM)/(TY2-TY1)
                      XHI = XHI*TX1 + (1.0-XHI)*TX2
                   ELSE
                      XHI = 0.5*(XWK(I+JMAX)+XWK(I))
                   ENDIF
                   XHI = XHI - XP(JMAX)
                   GOTO 600
                ENDIF
             ELSEIF (PVAL.GE.PLIM) THEN
                IF (PVAL.EQ.PMAX) FOUND = .TRUE.
                IF (I.GT.1) THEN
                   TX1 = 0.5*(XWK(I+JMAX-1)+XWK(I-1))
                   TX2 = 0.5*(XWK(I+JMAX)+XWK(I))
                   TY1 = FCTR1/(XWK(I+JMAX-1)-XWK(I-1))
                   TY2 = PVAL
                   XLOW = (TY2-PLIM)/(TY2-TY1)
                   XLOW = XLOW*TX1 + (1.0-XLOW)*TX2
                ELSE
                   XLOW = 0.5*(XWK(I+JMAX)+XWK(I))
                ENDIF
                XLOW = XP(JMAX) - XLOW
                N = I
             ENDIF
  550     CONTINUE

          IF (N.EQ.1) THEN
             WRITE (*,
     :       '(''          2sigma limits not found '',
     :       ''(no significant peak)'',A)') BLEEP
          ELSE
             WRITE (*,'(''          2sigma minus'',1PE12.4)') XLOW
             WRITE (*,'(''          No 2sigma upper limit located'',A)')
     :               BLEEP
          ENDIF
       ENDIF
       GOTO 700

  600  CONTINUE
       IF (N.EQ.1) THEN
          WRITE (*,'(''          No 2sigma lower limit located'',A)')
     :            BLEEP
       ELSE
          WRITE (*,'(''          2sigma minus'',1PE12.4)') XLOW
       ENDIF
       WRITE (*,'(''          2sigma plus '',1PE12.4)') XHI

  700  CONTINUE
       IF (JFORCE.LT.3) THEN
          WRITE (*,
     :    '(''   MODE:  optimum J (range):'',I4,
     :    '' ('',I1,'' -'',I4,'')'')') JMAX, J1, J2
       ELSE
          WRITE (*,
     :    '(''   MODE:  forced J, optimum J:'',    I4,'','',I4)')
     :    JFORCE, JMAX
       ENDIF

  800  CONTINUE

       END
