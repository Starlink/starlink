      SUBROUTINE PDA_CFFTF (N,C,WSAVE)
      DIMENSION       C(1)       ,WSAVE(1)
      IF (N .EQ. 1) RETURN
      IW1 = N+N+1
      IW2 = IW1+N+N
      CALL PDA_CFFTF1 (N,C,WSAVE,WSAVE(IW1),WSAVE(IW2))
      RETURN
      END
