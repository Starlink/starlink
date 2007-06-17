      FUNCTION ELFG(WAVE,WAVE0,DWAVE,PROF)
      IP=PROF+0.1
      ELFG=0.
      IF(IP.EQ.1) THEN
      D=0.600561*DWAVE
      X=(WAVE-WAVE0)/D
      X=X*X
      IF(X.GT.80.) RETURN
      ELFG=EXP(-X)
      RETURN
      ENDIF
      IF(IP.EQ.2) THEN
      X=ABS(WAVE-WAVE0)
      IF(X.GT.DWAVE) RETURN
      ELFG=(DWAVE-X)/DWAVE
      RETURN
      ENDIF
      IF(IP.GT.5) THEN
      IP=IP-5
      X=WAVE-WAVE0
      ELFG=ELFPNT(IP,X,IFAIL)
      RETURN
      ENDIF
      RETURN
      END
