*
       LOGICAL FUNCTION ISBACH(CMD,PARAMS,OK,WORKSZ,MAXSTK,NONSTK,XCALC,
     : YCALC,YFIT,ALAM,YFIT1,BXCALC,BYFIT,BYFIT1,STATUS)
       INCLUDE 'SAE_PAR'
       INTEGER STATUS

*
*   'Master routine for controlling InterStellar options.
*    Based on "BACH" (Clive Davenhall, Dane Maslen, Clive
*    Thomas, Paul Phillips), ported to DIPSO and modified
*    by Des Middlemas and Ian Howarth
*
       EXTERNAL IS_BLK

       CHARACTER*(*) CMD
       CHARACTER*(*) PARAMS
       CHARACTER*40 ATHEDK
       COMMON /ATHEDC/ ATHEDK
       INTEGER MAXPT
       INTEGER SLEN
       INTEGER WORKSZ


       PARAMETER (NUT=2800,MOUT=120)

       COMMON /DMISCOG/ NFLAM, EWBYLM, N1
       COMMON /DMISOPT/ OPTCAL, OPTOK, BLEND, COLUMN, QUART, SCALE,
     :                  MODE, CONV, THE, SAVE, VEL, V1, V2
       COMMON /IDHIRF/ NRF, MAXNRF, POSN, RF
       COMMON /IDHIRF1/ CTITLE
       CHARACTER*40 CTITLE
       REAL POSN(500), RF(500)
       COMMON /MAD   / BINST, BINST1, BINST2
       COMMON /MAD1  / DELTA, C, WAVE0, CONST, WAVE1, AK1, F1K
       COMMON /MAD3  / WIDTH(3), COLDEN(3), NCYCLE
       COMMON /MAD5  / MARB

       REAL TOTWEI
       SAVE DMISOPT, MAD, MAD1, MAD3, MAD5, ATHEDC, TOTWEI

       LOGICAL OPTCAL, OPTOK, BEEP, MODEL, ATOMIC, PSRF, OK

       COMMON /BEEP  / BEEP
       CHARACTER*1 BLEEP
       COMMON /BLEEP / BLEEP

       INTEGER BLEND, CONV, MODE, N, NTERMS, Q1K
       INTEGER FITTED, SAVE, NRF, VEL
       INTEGER BNPTS, BNTERM, IBRK(MOUT)
       INTEGER THREE, Q1, OPTION, TABLE, N1

       SAVE Q1, Q1K

       CHARACTER VALSTR*20, IRFILE*30, COGTIT*28, STKTIT*1
       CHARACTER DATFIL*80, OPTFIL*40, TITLE*80, OPT*1

       REAL BPARAM(MOUT), NFLAM(99), EWBYLM(99)
       REAL RESCAL, VSHIFT, WORV, VALUE
       REAL A(MOUT)
       SAVE A

       REAL XCALC(WORKSZ/8), YCALC(WORKSZ/8), YFIT(WORKSZ/8),
     :      BXCALC(WORKSZ/8), BYFIT(WORKSZ/8), BYFIT1(WORKSZ/8),
     :      ALAM(WORKSZ/8), YFIT1(WORKSZ/8)

       DATA CO1/2.654418269E-20/, CO2/7.478975686E-37/, TEN13/1.0E13/
       DATA ONE/1.0E0/, ZERO/0.0E0/, IZ/0/, THREE/3/
       DATA IONE/1/, ITWO/2/, THRE/3.0E0/, IFI/5/, IF6/6/
       DATA MCONV/30/, MARB1/30/
       DATA CCONV/1.499E-16/
       DATA MODEL, ATOMIC, PSRF/3*.FALSE./
       DATA N, NTERMS/0, 0/, MAXBRK/1/
       DATA IRFILE/'IRF'/, NCHRS/4/
       DATA COGTIT/'Curve of growth for '/

*    Return if an error has already occurred.
       ISBACH = .FALSE.
       IF( STATUS .NE. SAI__OK ) RETURN

*    Assume OK until error is detected

       MAXNRF = 500
       OK = .TRUE.

*    Assume command found until it is not

       ISBACH = .TRUE.

*    Set array limit

       M = WORKSZ/8

*    Compare command string to those handled here;  act if found.

       CALL SSTRIP(PARAMS)
       IF (CMD.EQ.'ISOPT') THEN
          CALL OPTIONS(PARAMS,MAXSTK,NONSTK,OK,STATUS)
          IF (.NOT.OK) GOTO 300
       ELSEIF (CMD.EQ.'ISINP') THEN
          CALL MODELS(A,N,TOTWEI,MODEL,OK,STATUS)
          NTERMS = N*3
          IF (.NOT.OK) GOTO 300
       ELSEIF (CMD.EQ.'ISATM') THEN
          WAVE = 0.0
          CALL DECODE(CMD,PARAMS,0,1,WAVE,' ',OK)
          IF (.NOT.OK) GOTO 300
          CALL ISATM(WAVE0,WAVE,F1K,Q1,Q1K,ATOMIC,OK,STATUS)
          IF (.NOT.OK) GOTO 300
       ELSEIF (CMD.EQ.'ISCALC' .OR. CMD.EQ.'ISCOG') THEN
          I = MAXSTK - NONSTK
          IF ((CONV.GT.0) .AND. (CMD.EQ.'ISCALC')) I = I - 1
          IF (I.LE.0) THEN
             WRITE (*,
     :       '(''   '',A,'':  insufficient stack space to '',
     :       ''save reslts'')') CMD(1:SLEN(CMD))
             OK = .FALSE.
             GOTO 300
          ENDIF
          THE = 15.
          QUART = 0.25
          OPTION = -1
          TABLE = 0
          BASCOR = 0
          BINST1 = 0
          BINST2 = 0
          IF (.NOT.MODEL) THEN
             WRITE (*,
     :       '(''   '',A,'':  no clouds in model (use ISINP)'')')
     :       CMD(1:SLEN(CMD))
             OK = .FALSE.
             GOTO 300
          ENDIF
          IF (.NOT.ATOMIC) THEN
             WRITE (*,'(''   '',A,'':  no atomic data (use ISATM)'')')
     :       CMD(1:SLEN(CMD))
             OK = .FALSE.
             GOTO 300
          ENDIF
          IF (.NOT.OPTCAL .AND. CMD.NE.'ISCOG') THEN
             WRITE (*,
     :       '(''   ISCALC:  using default ISOPT options'',A)') BLEEP
             VEL = 2
             CONV = 0
             V1 = -100.0
             V2 = +100.0
             BINST = 0
             BLEND = 0
          ENDIF
          IF ((TOTWEI).LT.1E2) THEN
             WRITE (*,
     :       '(''   '',A,'':  columns too small (<100 cm-2)'')')
     :       CMD(1:SLEN(CMD))
             OK = .FALSE.
             GOTO 300
          ENDIF
          MARB = MARB1
          TABLE = 0
          BINST1 = 0.
          BINST2 = 0.
          IOSDAT = 0
          STRIDE = A(IONE)
          DO 50 I = 1, NTERMS, 3
             IF (A(I).LT.STRIDE) STRIDE = A(I)
   50     CONTINUE
          AK1 = F1K*FLOAT(Q1)/(FLOAT(Q1K)*WAVE0*WAVE0*CCONV)
          CONST = CO2*AK1*FLOAT(Q1K)/FLOAT(Q1)
          DELTA = CO1*AK1
          WAVE1 = C/WAVE0
          STEP = A(IONE)
          ISTART = IONE
          ISTOP = NTERMS - ITWO
          START = A(ITWO) - (THE*STEP)
          STOP = A(NTERMS-IONE) + (THE*A(NTERMS-ITWO))
          DO 100 J = 1, NTERMS, 3
             DUMMY = A(J+IONE) - (THE*A(J))
             IF (DUMMY.LT.START) THEN
                ISTART = J
                START = DUMMY
             ENDIF
             DUMMY = A(J+IONE) + (THE*A(J))
             IF (DUMMY.GT.STOP) THEN
                ISTOP = J
                STOP = DUMMY
             ENDIF
  100     CONTINUE
          STEP = STRIDE*QUART
          SAFE = THRE*STEP
          BIN = BINST*BINST
          START = A(ISTART+IONE) - (THE*SQRT(A(ISTART)*A(ISTART)+BIN))
     :             - SAFE
          STOP = A(ISTOP+IONE) + (THE*SQRT(A(ISTOP)*A(ISTOP)+BIN))
     :            + SAFE
          START = MIN(START,V1)
          STOP = MAX(STOP,V2)
          START = WAVE0*(ONE+START/C)
          STOP = WAVE0*(ONE+STOP/C)
          STEP = WAVE0*STEP/C
          NPTS = ((STOP-START)/STEP) + IONE
          IF (NPTS.GT.M) THEN
             WRITE (*,
     :       '(''   '',A,'':   too many points in model''/
     :       ''   Decrease velocity range or increase smallest "b"'')')
     :       CMD(1:SLEN(CMD))
             OK = .FALSE.
             GOTO 300
          ENDIF
          XCALC(1) = START
          DO 150 J = 2, NPTS
             XCALC(J) = XCALC(J-1) + STEP
  150     CONTINUE
          IF (CMD.EQ.'ISCOG') THEN
             TABLE = 1
             CALL TAU
     :       (XCALC,M,YFIT,A,NPTS,NTERMS,OPTION,TABLE,ALAM,YFIT1,MOUT)
             NBRKOB = 1
             IF (NTERMS.EQ.3) THEN
                COGTIT(1:) = ' Curve of growth, b ='
                WRITE (COGTIT(22:26),'(F5.1)') A(1)
             ELSE
                COGTIT = ' Curve of growth,    cloud model'
                WRITE (COGTIT(18:20),'(I3)') NTERMS/3
             ENDIF
             CALL UPUSH
     :       (99,NFLAM,EWBYLM,N1,MAXBRK,N1,NBRKOB,COGTIT,WAVE0,OK)
             GOTO 300
          ELSE
             COLDEN(1) = TOTWEI
             TABLE = 0
             CALL TAU
     :       (XCALC,M,YFIT,A,NPTS,NTERMS,OPTION,TABLE,ALAM,YFIT1,MOUT)
             DO 160 I = 1, NPTS
                YFIT(I) = ONE - YFIT(I)
  160        CONTINUE
             IF (BLEND.GT.0) THEN
                BNPTS = M
                NBRK = MOUT
                CALL GETSTK
     :          (BLEND,BNPTS,BXCALC,BYFIT,NBRK,IBRK,STKTIT,WORV,OK)
                IF (.NOT.(OK)) THEN
                   WRITE (*,
     :             '(''   '',A,'':  failed to access stack entry'',I3)')
     :             CMD(1:SLEN(CMD)), BLEND
                   WRITE (*,
     :             '(''            Unable to BLEND!!''/
     :             ''            Setting BLEND=0'',A)') BLEEP
                   BLEND = 0
                   OK = .TRUE.
                ELSEIF (WORV.EQ.1.0) THEN
                   WRITE (*,
     :             '(''   '',A,'':  stack entry'',I3,
     :             '' is not in velocity units'',A)')
     :             CMD(1:SLEN(CMD)), BLEND, BLEEP
                ELSE
                   WORV = WORV*C
                   WRITE (*,
     :             '(''   ISCALC:  blending with stack entry'',I3/
     :             ''   BLEND reset to zero'')') BLEND
                   BLEND = 0
                   RESCAL = WORV/WAVE0
                   VSHIFT = (RESCAL-1.0)*C
                   DO 165 I = 1, BNPTS
                      IF (BXCALC(BNPTS/2).GT.8E2) THEN
                         BXCALC(I) = (BXCALC(I)/WORV-ONE)*C*RESCAL +
     :                   VSHIFT
                      ELSE
                         BXCALC(I) = BXCALC(I)*RESCAL + VSHIFT
                      ENDIF
  165              CONTINUE
*    Intensities are calculated in lambda space;  convert to
*    velocities to blend.
                   DO 170 I = 1, NPTS
                      XCALC(I) = C*((XCALC(I)/WAVE0)-ONE)
  170              CONTINUE
                   CALL REMAP(BXCALC,BYFIT,BNPTS,XCALC,BYFIT1,NPTS,1)
*    Convert back to wavelength space
                   DO 175 I = 1, NPTS
                      YFIT(I) = YFIT(I)*BYFIT1(I)
                      XCALC(I) = WAVE0*(XCALC(I)/C+ONE)
  175              CONTINUE
                ENDIF
             ENDIF
             WORV = 1.0
*    Unconvolved profile in velocity space?
             IF (VEL.GE.1) THEN
                DO 180 I = 1, NPTS
                   XCALC(I) = C*((XCALC(I)/WAVE0)-ONE)
  180           CONTINUE
                WORV = WAVE0/C
             ENDIF
             NBRKOB = 1
             TITLE = ATHEDK(1:SLEN(ATHEDK))//' profile; B,N,V='
             IF (NTERMS.EQ.3) THEN
                L1 = SLEN(TITLE)
                WRITE (TITLE(L1+1:L1+4),'(F4.1)') A(1)
                L1 = SLEN(TITLE)
                TITLE(L1+1:L1+1) = ','
                L1 = L1 + 1
                WRITE (TITLE(L1+1:L1+5),'(F5.2)') LOG10(A(3))
                L1 = SLEN(TITLE)
                L1 = L1 + 1
                TITLE(L1:L1) = ','
                WRITE (TITLE(L1+1:L1+6),'(F6.1)') A(2)
             ELSE
                TITLE(1:) = ATHEDK(1:SLEN(ATHEDK))
                L1 = SLEN(TITLE) + 1
                WRITE (TITLE(L1:L1+2),'(I3)') NTERMS/3
                L1 = SLEN(TITLE)
                TITLE(L1+2:) = 'cloud model'
             ENDIF
             CALL UPUSH
     :       (M,XCALC,YFIT,NPTS,MAXBRK,NPTS,NBRKOB,TITLE,WORV,OK)
*    Back to lambda space
             IF (VEL.GE.1) THEN
                DO 190 I = 1, NPTS
                   XCALC(I) = (XCALC(I)/C+ONE)*WAVE0
  190           CONTINUE
             ENDIF
             IF (.NOT.OK) THEN
                WRITE(*,'(''   ISCALC:  Error pushing data to stack'')')
                GOTO 300
             ENDIF
             IF (CONV.GT.IZ) THEN
                WRITE (*,'(''   ISCALC:  convolving...'')')
*    Convolve in velocity space
                DO 200 I = 1, NPTS
                   XCALC(I) = C*((XCALC(I)/WAVE0)-ONE)
  200           CONTINUE
                IF (CONV.EQ.IONE) THEN
                   CALL GAUSS(XCALC,YFIT,YFIT1,NPTS,M,KTEST,JTEST,WID)
                ELSE
                   CALL CONVOL(XCALC,YFIT,YFIT1,NPTS,M,CONV,MCONV,KTEST,
     :             IK,JTEST,WID,RESP,LTEST,MAXI,RF,POSN,NRF,NUT,STEP)
                   IF (LTEST.GT.IZ) THEN
                      GOTO 300
                   ENDIF
                   IF (IK.GT.IZ) THEN
                      WRITE (*,
     :                '(''   ISCALC:  error CV-4;  please report '',
     :                ''this error to ZUVAD::IDH with details of '',
     :                ''data and command sequence used'',A)') BLEEP
                      OK = .FALSE.
                      GOTO 300
                   ENDIF
                ENDIF
                IF (KTEST.GT.IZ) THEN
                   WRITE (*,
     :             '(''   ISCALC:  insufficient data for convolution''/
     :             ''   Decrease V1 or increase V2'')')
                   OK = .FALSE.
                   GOTO 300
                ELSEIF (JTEST.GT.IZ) THEN
                   WRITE (*,
     :             '(''   ISCALC:  equivalent width of profile has'',
     :             '' changed by more than 0.5% during convolution'')')
                   OK = .FALSE.
                   GOTO 300
                ENDIF
                WORV = WAVE0/C
*    Store convolved profile in wavelength space?
                IF (VEL.NE.1) THEN
                   DO 205 I = 1, NPTS
                      XCALC(I) = (XCALC(I)/C+ONE)*WAVE0
  205              CONTINUE
                   WORV = 1.
                ENDIF
                NBRKOB = 1
                TITLE = ' Convolved '//ATHEDK(1:SLEN(ATHEDK))
                CALL UPUSH
     :          (M,XCALC,YFIT,NPTS,MAXBRK,NPTS,NBRKOB,TITLE,WORV,OK)
                IF (.NOT.OK) THEN
                   WRITE (*,
     :             '(''   ISCALC:  Error pushing profile to stack'')')
                   GOTO 300
                ENDIF
             ENDIF
          ENDIF
       ELSE
          ISBACH = .FALSE.
       ENDIF

  300  CONTINUE

       END
