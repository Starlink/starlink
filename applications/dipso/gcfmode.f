       SUBROUTINE GCFMODE (GAMMCF, A, X, GLN, OK)
       LOGICAL OK
       CHARACTER*1 BLEEP
       COMMON /BLEEP / BLEEP

       PARAMETER (ITMAX=100,EPS=3.E-7)

       GLN = GAMMLN(A)
       GOLD = 0.
       A0 = 1.
       A1 = X
       B0 = 0.
       B1 = 1.
       FAC = 1.
       DO 100 N = 1, ITMAX
          AN = FLOAT(N)
          ANA = AN - A
          A0 = (A1+A0*ANA)*FAC
          B0 = (B1+B0*ANA)*FAC
          ANF = AN*FAC
          A1 = X*A0 + ANF*A1
          B1 = X*B0 + ANF*B1
          IF (A1.NE.0.) THEN
             FAC = 1./A1
             G = B1*FAC
             IF (ABS((G-GOLD)/G).LT.EPS) GOTO 200
             GOLD = G
          ENDIF
  100  CONTINUE
       WRITE (*,
     : '(''   MODE:  A too large, ITMAX too small in GCFMODE'',A)')
     : BLEEP
       OK = .FALSE.
       GOTO 300
  200  CONTINUE
       GAMMCF = EXP(-X+A*ALOG(X)-GLN)*G

  300  CONTINUE

       END
