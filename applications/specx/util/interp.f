* History:
*    20-SEP-2000 (AJC):
*       Declare BUF assumed size array
C---------------------------------------------------------------------

      REAL FUNCTION INTERP(BUF,Z,N)

C   This program uses the second-order Everett interpolation formula to
C   obtain the function values from the tabulated function and the
C   calculated second differences.
C
C   If x is on the first interval...uses Newton fwd difference formula.
C
C   If x is on the last interval...uses Gregory-Newton bwd difference formula

      REAL*4 BUF(*)

      IF(Z.LT.1.)   Z=1.0
      IF(Z.GT.FLOAT(N))   Z=FLOAT(N)
      M=Z
      K=M+1
      R=Z-M
      IF(M.EQ.1)   GO TO 10
      IF(M.EQ.N-1)   GO TO 20
      IF(M.NE.N)   GO TO 5
      INTERP=BUF(N)
      RETURN
    5 DIFFM=BUF(M+1)+BUF(M-1)-2.*BUF(M)
      DIFFK=BUF(K+1)+BUF(K-1)-2.*BUF(K)
      INTERP=(1.-R)*BUF(M)+R*BUF(K)+(R*(R-1.)/6.)*((2.-R)*DIFFM+
     1 (1.+R)*DIFFK)
      RETURN
   10 DIFF2=BUF(1)+BUF(3)-2.*BUF(2)
      INTERP=BUF(1)+R*(BUF(2)-BUF(1))+R*(R-1.)/2.*DIFF2
      RETURN
   20 DIFFN1=BUF(N)+BUF(N-2)-2.*BUF(N-1)
      R=R-1.
      INTERP=BUF(N)+R*(BUF(N)-BUF(N-1))+R*(R+1.)/2.*DIFFN1
      RETURN
      END

