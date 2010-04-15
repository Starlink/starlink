
       SUBROUTINE GSERMODE (GAMSER, A, X, GLN, OK)
       LOGICAL OK
       CHARACTER*1 BLEEP
       COMMON /BLEEP / BLEEP

       PARAMETER (ITMAX=100,EPS=3.E-7)

       GLN = GAMMLN(A)
       IF (X.EQ.0.) THEN
          GAMSER = 0.
          GOTO 300
       ENDIF
       AP = A
       SUM = 1./A
       DEL = SUM
       DO 100 N = 1, ITMAX
          AP = AP + 1.
          DEL = DEL*X/AP
          SUM = SUM + DEL
          IF (ABS(DEL).LT.ABS(SUM)*EPS) GOTO 200
  100  CONTINUE
       WRITE (*,
     : '(''   MODE:  A too large, ITMAX too small in GSERMODE'',A)')
     : BLEEP
  200  CONTINUE
       GAMSER = SUM*EXP(-X+A*LOG(X)-GLN)

  300  CONTINUE

       END
