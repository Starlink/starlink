C------------------------------------------------------------------------------

      SUBROUTINE PRE(EQ1,EQ2,PREMAT)

C     CALCULATES PRECESSION MATRIX FOR GIVEN EQ1 TO EQ2 ( IN BESSELIAN YRS)

      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 PREMAT(3,3)
      REAL*4 EQ1,EQ2

      DATA CSAR/4.8481368111D-06/

      T0=DBLE(EQ1-1900.0)/100.D0
      T=DBLE((EQ2-EQ1)/100.D0)
      ZETA=((2304.25D0+1.396D0*T0)*T+0.302D0*T*T+0.018D0*T*T*T)*CSAR
      ZETT=ZETA +0.791D0*T*T*CSAR
      THET=((2004.682D0-0.853D0*T0)*T-0.426D0*T*T-0.042D0*T*T*T)*CSAR
      SZETA=DSIN(ZETA)
      CZETA=DCOS(ZETA)
      SZETT=DSIN(ZETT)
      CZETT=DCOS(ZETT)
      STHET=DSIN(THET)
      CTHET=DCOS(THET)
      A=SZETA*SZETT
      B=CZETA*SZETT
      C=SZETA*CZETT
      D=CZETA*CZETT
      PREMAT(1,1)=D*CTHET-A
      PREMAT(1,2)=-C*CTHET-B
      PREMAT(1,3)=-STHET*CZETT
      PREMAT(2,1)=B*CTHET+C
      PREMAT(2,2)=-A*CTHET+D
      PREMAT(2,3)=-STHET*SZETT
      PREMAT(3,1)=CZETA*STHET
      PREMAT(3,2)=-SZETA*STHET
      PREMAT(3,3)=CTHET

      RETURN
      END

C------------------------------------------------------------------------------

