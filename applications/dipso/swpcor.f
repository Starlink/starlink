C
       SUBROUTINE SWPCOR (NPTS,YR,IIAPER,WAVE,FLUX,OK)
C
C     Corrects IUE SWP spectra for `aging', after
C     Bohlin & Grillmair (1988, ApJSup 66, 209)
C     Uses data file in DIPSODIR:SWPCOR.DAT
C
C
C     YR is the time of observation e.g. 78.147 means 1978.147
C     IIAPER = 1 for trailed spectra
C              2 for small aperture
C              3 for large aperture
C     TCORR subscripts are (1)  Max. no. of 5A wave bins
C                          (2)  Aperture
C                          (3)  Camera (3=SWP)
C                          (4)  IYR value (1 through NYR)
C
       REAL WAVE(*)
       REAL FLUX(*)

       CHARACTER*1 BLEEP
       COMMON /BLEEP/ BLEEP

       INTEGER ICAM
       PARAMETER (ICAM=3)
       REAL YRCORR(3,3,9)
       REAL TCORR(301,3,3,9)
       DIMENSION WAVEC(301), CORR(301)


       CHARACTER*80   prefix
       CHARACTER*80   prefix2
       CHARACTER*80   prefix3
       CHARACTER*80   prefix4
       INTEGER        plen,p2len,p3len,p4len

       COMMON /prefix1/ prefix,prefix2,prefix3,prefix4
       COMMON /prefix2/ plen,p2len,p3len,p4len

       LOGICAL OK
       INTEGER NYR        ! Number of years of corrections (since 1978)
       PARAMETER (NYR=9)
       LOGICAL ISTSW    ! .T. if this call the first call to correct SWP

       CHARACTER*5 AP(3)

       SAVE ISTSW, YRCORR, TCORR

*  Local Data:
       DATA AP/'TRAIL', 'SMALL', 'LARGE'/
       DATA ISTSW/.TRUE./

C*

       IF (NPTS.GT.0) THEN
          IF (WAVE(1).LT.1150.) THEN
             WRITE (*,
     :       '(''   IUECOR:  camera 3 (SWP), wavelengths below 1150A'',
     :       A)') BLEEP
             OK = .FALSE.
          ENDIF
          IF (WAVE(NPTS).GT.1976.) THEN
             WRITE (*,
     :       '(''   IUECOR:  camera 3 (SWP), wavelengths above 1975A'',
     :       A)') BLEEP
             OK = .FALSE.
          ENDIF
       ELSE
          WRITE (*,
     :    '(''   IUECOR:  no data in current arrays'',A)') BLEEP
          OK = .FALSE.
       ENDIF
       IF (.NOT.OK) GOTO 800

C    Initialise arrays and read in data on first-time call to SWPCOR

       IF (ISTSW) THEN
          ISTSW = .FALSE.
          WRITE (*,
     :    '(''   IUECOR:  loading data from DIPSODIR:SWPCOR.DAT'')')
          IUNIT29 = 29
          CLOSE (IUNIT29)
          OPEN (UNIT=IUNIT29,
     :          FILE=prefix(1:plen)//'SWPCOR.DAT',STATUS='OLD',
     :    FORM='FORMATTED')
          DO 50 I = 1, 301
             DO 20 J = 1, 3
                DO 10 K = 1, 3
                   DO 5 L = 1, NYR
                      TCORR(I,J,K,L) = 0.
    5              CONTINUE
   10           CONTINUE
   20        CONTINUE
   50     CONTINUE

  100     CONTINUE
          READ (IUNIT29,'(4I4,F12.3)',END=150)
     :    JPTS, JIAPER, JCAM, JYR,  YRCORR(JIAPER, JCAM, JYR)
          YRCORR(JIAPER,JCAM,JYR) = YRCORR(JIAPER,JCAM,JYR) - 1900.
          READ (IUNIT29,'(12F6.3)')
     :    (TCORR(I,JIAPER,JCAM,JYR),I=1,JPTS)
          GOTO 100
  150     CONTINUE
          CLOSE (IUNIT29)
       ENDIF

       JPTS = 165
       WAVE1 = 1145

       DO 200 I = 1, JPTS
          WAVEC(I) = WAVE1 + 5.*REAL(I)
  200  CONTINUE

       DO 300 I = 1, NPTS
          IF (WAVE(I).LT.WAVEC(1)-2.5) ISTPT = I
          IF (WAVE(I).LE.WAVEC(JPTS)+5.5) LSTPT = I
  300  CONTINUE
       ISTPT = ISTPT + 1
       WRITE (*,
     : '(''   IUECOR:''/
     : ''   Correcting over range'',F8.1,'' -'',F8.1/
     : ''   Time is'',F8.3/
     : ''   Aperture is'',A5)')
     : WAVE(ISTPT), WAVE(LSTPT), YR, AP(IIAPER)

C    Interpolate to YR

       DO 400 I = 1, NYR - 1
          II = I
          IF (YR.LT.YRCORR(IIAPER,ICAM,I+1)) GOTO 500
  400  CONTINUE
       WRITE (*,'(''   IUECOR:  time correction extrapolated to'',
     : F8.3,A)') YR, BLEEP
  500  CONTINUE
       IYR = II
       FRAC = (YR-YRCORR(IIAPER,ICAM,IYR))
     :        /(YRCORR(IIAPER,ICAM,IYR+1)-YRCORR(IIAPER,ICAM,IYR))
       DO 600 I = 1, JPTS
          IF (TCORR(I,IIAPER,ICAM,IYR).EQ.0. .OR.
     :    TCORR(I,IIAPER,ICAM,IYR+1).EQ.0.) THEN
             WRITE (*,'(''   IUECOR:  edit zero time correction for''/
     :       ''   Year:'',F6.2/
     :       ''   Wave:'',F7.1/
     :       ''   Aper: '',A/
     :       ''   Camera:'',I3)')
     :       YR, WAVEC(I), AP(IIAPER), ICAM
             OK = .FALSE.
             GOTO 800
          ENDIF
          CORR(I) = TCORR(I,IIAPER,ICAM,IYR)
     :               + (TCORR(I,IIAPER,ICAM,IYR+1)
     :               - TCORR(I,IIAPER,ICAM,IYR))*FRAC
  600  CONTINUE

C    Apply correction according to nearest neighbour

       DO 700 I = ISTPT, LSTPT
          IF (WAVE(I).GT.WAVEC(1)-2.5 .AND. WAVE(I).LT.WAVEC(1)) THEN
             N = 1
             GOTO 650
          ENDIF
          IF (WAVE(I).GT.WAVEC(JPTS) .AND.
     :    WAVE(I).LT.WAVEC(JPTS)+5.5) THEN
             N = JPTS
             GOTO 650
          ENDIF
          N = SWCOR2(WAVE(I),JPTS,WAVEC) + 0.5
  650     CONTINUE
          IF (CORR(N).LE.0.) THEN
             WRITE (*,'(''   IUECOR:   time correction is out of '',
     :       ''range at'',I5,2F8.2,A)') N, WAVEC(N), CORR(N), BLEEP
             OK = .FALSE.
             GOTO 800
          ENDIF
          FLUX(I) = FLUX(I)/CORR(N)
  700  CONTINUE

  800  CONTINUE
       END
