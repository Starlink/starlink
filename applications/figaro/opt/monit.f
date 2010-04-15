      subroutine monit(m,n,xc,rc,ajc,ljc,sing,igrade,ncall,nf,miw,liw,
     :   wn,niw)
*+
* Name:
*    MONIT

* Invocation:
*    CALL MONIT(M,N,XC,RC,AJC,LJC,SING,IGRADE,NCALL,NF,MIW,LIW,
*        WN,NIW)

* Purpose:
*   print parameter values at current iteration

* Description
*   print parameter values at current iteration
*
* Arguments:
*    M = INTEGER (Given)
*
*    N = INTEGER (Given)
*
*    XC(N) = DOUBLE PRECISION ARRAY (Given)
*
*    RC(M) = DOUBLE PRECISION ARRAY (Given)
*
*    AJC(LJC,N) = DOUBLE PRECISION ARRAY (Given)
*
*    LJC = INTEGER (Given)
*
*    SING(N) = DOUBLE PRECISION ARRAY (Given)
*
*    IGRADE = INTEGER (Given)
*
*    NCALL = INTEGER (Given)
*
*    NF = INTEGER (Given)
*
*    MIW(LIW) = INTEGER ARRAY (Given)
*
*    LIW = INTEGER (Given)
*
*    WN(NIW) = DOUBLE PRECISION ARRAY (Given)
*
*    NIW = INTEGER (Given)
*
*
      implicit none
* global:
      include 'opt_cmn'
*-
      integer n
      integer m
      integer niw
      integer ljc
      integer liw
      double precision xc(n)
      double precision rc(m)
      double precision wn(niw)
      double precision ajc(ljc,n)
      double precision sing(n),sum
      integer ncall
      integer nf
      integer igrade
      integer i,k
      integer miw(liw)
* -------------------------------------------------------------------
*
* calculate sum of squares
*
      fc=0.0d0
      do i=1,m
        fc=fc+rc(i)*rc(i)
      end do
*
* output iteration data
*
      write(opt_lu,1) ncall,nf,fc,igrade
   1  format(//,2x,'ITERATION ',i3,4x,'FUNC EVALS ',i4,'SUM OF SQUARES',
     :  e16.8,'SUBSPACE ',i4,//2x,'COEFFICIENTS')
      write(opt_lu,2) (i,xc(i),i=1,n)
   2  format(/,4(2x,'X(',i5,') = ',e16.8))
      write(opt_lu,3)
   3  format(/,2x,'FUNCTION VALUES')
      call wrtdble(m,rc,'(/,4(2x,''RC('',i5,'') = '',e16.8))',opt_lu)
      write(opt_lu,5) (sing(i),i=1,n)
   5  format(//,2x,'SINGULAR VALUES',4(1x,e16.8))
      do i = 1,n
        sum=0.0d0
        do k = 1,m
          sum = sum+ajc(k,i)*rc(k)
        end do
        gc(i) = sum
      enddo
      write(opt_lu,6) (gc(i),i=1,n)
   6  format(//,2x,'GRADIENT-VECTOR AT CURRENT SOLUTION',4(1x,e16.8))
      end
