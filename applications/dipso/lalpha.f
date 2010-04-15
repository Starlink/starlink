       SUBROUTINE LALPHA(HCOL,FACMAX,X,Y,NX,BREAK,NBREAK)
       INTEGER NINPRF
       PARAMETER (NINPRF=584)
       REAL HCOL, FACMAX, X(1), Y(1)
       INTEGER NX, NBREAK, BREAK(1)
       INTEGER I, J, K
       REAL SPACE, CONST
       SPACE = 5.0E-12*SQRT(HCOL)
       CONST = 4.253E-20*HCOL
       IF (FACMAX.LT.2.0) THEN
          FACMAX = 2.0
       ENDIF
*    Changed to use existing X grid
       J = 0
       DO 100 I = 1, NX
          DL = X(I) - 1215.67
          Y(I) = EXP(-MIN(50.0,CONST/DL**2))
          J = J + 1
          IF (DL.GT.10.0) THEN
             IF (Y(I).GT.0.9999) GOTO 200
          ENDIF
  100  CONTINUE
       GOTO 400

  200  CONTINUE
       DO 300 I = J, NX
          Y(I) = 1.0
  300  CONTINUE

  400  CONTINUE

       END
