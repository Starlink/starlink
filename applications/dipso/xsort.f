       SUBROUTINE XSORT(N,RA,RB)
       DIMENSION RA(N), RB(N)
       L = N/2 + 1
       IR = N
  100  CONTINUE
       IF (L.GT.1) THEN
          L = L - 1
          RRA = RA(L)
          RRB = RB(L)
       ELSE
          RRA = RA(IR)
          RRB = RB(IR)
          RA(IR) = RA(1)
          RB(IR) = RB(1)
          IR = IR - 1
          IF (IR.EQ.1) THEN
             RA(1) = RRA
             RB(1) = RRB
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
             RB(I) = RB(J)
             I = J
             J = J + J
          ELSE
             J = IR + 1
          ENDIF
          GOTO 200
       ENDIF
       RA(I) = RRA
       RB(I) = RRB
       GOTO 100

  300  CONTINUE

       END
