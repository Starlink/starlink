       FUNCTION SWCOR2(WAVEN,NPTS,WAVE)
C
C  FIND THE POINT, SWCOR2, IN THE WAVELENGTH ARRAY, WAVE, THAT CORRESPONDS
C    TO THE WAVELENGTH WAVEN, USING A BINARY SEARCH.
C
C  INPUT : WAVEN
C        : NPTS THE NUMBER OF POINTS IN THE WAVE ARRAY
C        : WAVE
C  OUTPUT: SWCOR2
C
       REAL*4 WAVE(1), AT, DEL, SGN, WAT, WAVEN
C
       INTEGER IAT, ISGN, NPTS, NXT
       LOGICAL OK
C
       IF (WAVEN.GE.WAVE(1)) THEN
          IF (WAVEN.LE.WAVE(NPTS)) THEN
C
             DEL = NPTS
             SGN = 1.0
             AT = 0.0
   20        CONTINUE
             DEL = DEL*0.5
             AT = AT + SGN*DEL
             IAT = AT + 0.5
             SGN = +1.0
             IF (WAVE(IAT).GT.WAVEN) SGN = -1.0
             IF (DEL.GT.1.0) GOTO 20
             ISGN = SGN
             IF (WAVE(IAT).EQ.WAVE(IAT+ISGN)) IAT = IAT + ISGN
             AT = IAT
             NXT = IAT + ISGN
             WAT = WAVE(IAT)
             SWCOR2 = ((WAVEN-WAT)/(WAVE(NXT)-WAT))*SGN + AT
             GOTO 100
          ENDIF
       ENDIF
       WRITE (*,
     : '(''   IUECOR:  SWP data, wavelength out of range at'',F10.3,
     : ''A'')') WAVEN
       OK = .FALSE.

  100  CONTINUE

       END
