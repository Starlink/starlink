       REAL FUNCTION GAMMODE (A,X,OK)
       LOGICAL OK
       CHARACTER*1 BLEEP
       COMMON /BLEEP / BLEEP

       IF ((X.LT.0.) .OR. (A.LE.0.)) THEN
          WRITE (*,
     :    '(''   MODE:  negative argument encountered in GAMMODE'',A)')
     :    BLEEP
          OK = .FALSE.
          GOTO 100
       ENDIF

       IF (X.LT.A+1.) THEN
          CALL GSERMODE(GAMSER,A,X,GLN,OK)
          IF (.NOT.OK) GOTO 100
          GAMMODE = GAMSER
       ELSE
          CALL GCFMODE(GAMMCF,A,X,GLN,OK)
          IF (.NOT.OK) GOTO 100
          GAMMODE = 1. - GAMMCF
       ENDIF

  100  CONTINUE

       END
