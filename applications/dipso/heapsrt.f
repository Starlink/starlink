       SUBROUTINE HEAPSRT(N,RA)
       DIMENSION RA(N)

       IF (N.LE.1) GOTO 300

       L = N/2 + 1
       IR = N
  100  CONTINUE
       IF (L.GT.1) THEN
          L = L - 1
          RRA = RA(L)
       ELSE
          RRA = RA(IR)
          RA(IR) = RA(1)
          IR = IR - 1
          IF (IR.EQ.1) THEN
             RA(1) = RRA
             GOTO 300
          ENDIF
       ENDIF
       I = L
       J = L + L
  200  CONTINUE
       IF (J.LE.IR) THEN
          IF (J.LT.IR) THEN
             IF (RA(J).LT.RA(J+1)) J = J + 1
          ENDIF
          IF (RRA.LT.RA(J)) THEN
             RA(I) = RA(J)
             I = J
             J = J + J
          ELSE
             J = IR + 1
          ENDIF
          GOTO 200
       ENDIF
       RA(I) = RRA
       GOTO 100

  300  CONTINUE

       END
