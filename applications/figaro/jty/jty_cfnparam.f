      SUBROUTINE JTY_CFNPARAM(N,A,K1,K2,K3,K4,SCALE,VELOCITY,
     1    FTDATA,FTTEMP,FTCFN,CFN,V,W,R,H)
C
C     This routine contained a statement function, which the Sun
C     compiler did not like for some reason. Possibly because it used
C     variables PC rather than parameters. Since the function is
C     referenced in only one statement I put the expression in there.
*
* The parameter statement had no brackets. The Sun compiler doesn't like
* that. The WRITE was turned into a PAR_WRUSER.
*                                HME/UoE, Starlink. 12 Oct 1992.
C
      COMPLEX FTDATA(1), FTTEMP(1), FTCFN(1)
      REAL*4 VELOCITY(1), CFN(1)
      REAL*8 PC(5)
      LOGICAL*1 ERROR
      INTEGER STATUS
      PARAMETER (C=299793.)

      LOG2N = NINT(ALOG(FLOAT(N))/ALOG(2.))
      CALL JTY_CORRELATE(N,FTDATA,FTTEMP,FTCFN)
      CALL JTY_CCVECTOR(N,FTCFN,CFN)
      CALL JTY_FILTER(N,K1,K2,K3,K4,CFN)
      CALL JTY_FFT2C(CFN,LOG2N,-1)
      CALL JTY_CRVECTOR(N,CFN,CFN)
      CALL JTY_FLIP(N,CFN)
      CALL JTY_PEAKFIT(N,VELOCITY,CFN,V,W,PC,ERROR)
*     IF(ERROR) WRITE(6,*) 'Error in PEAKFIT'
      IF(ERROR) CALL PAR_WRUSER('Error in PEAKFIT',STATUS)
      H = PC(1) + V*(PC(2) + V*(PC(3) + V*(PC(4) + V*PC(5))))
      H = H / SCALE / N
      SHIFT = A * ALOG(V/C+1)
      CALL JTY_ASPART(N,K1,K2,K3,K4,SHIFT,FTCFN,ARMS,SRMS)
      ARMS = ARMS / SCALE / N
      SRMS = SRMS / SCALE / N
      IF(ARMS.GT.0) THEN
          R = H / (2*ARMS)
      ELSE
          R = 1000
      END IF
c     WRITE(6,2000) V, H, W, R
c2000 FORMAT(1X,'V =',F7.0,'  H =',F5.2,'  W =',F6.1,'  R =',F6.2)
c     WRITE(6,2001) RMS, ARMS, SRMS
c2001 FORMAT(1X,'rms =',F7.1,'  Arms =',F7.4,'  Srms =',F7.4)
      RETURN
      END
