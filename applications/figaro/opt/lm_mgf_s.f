      subroutine lm_mgf_s(iflag,m,n,xc,rc,fjac,ldfjac,dens,data,weight)
*+
* Name:
*    LM_MGF_S

* Invocation:
*    CALL LM_MGF_S(IFLAG,M,N,XC,RC,FJAC,LDFJAC,DENS,DATA,WEIGHT)
*
* Description:
*    To calculate residuals or Jacobian for a MG fit model.
*
* Purpose:
*    To calculate residuals or Jacobian for a MG fit model.
*
* Arguments:
*      IFLAG = INTEGER (Given)
*        Flag to determine action of routine, 1 => fvec, 2 => fjac
*      M = INTEGER (Given)
*        Number of data points
*      N = INTEGER (Given)
*        Number of variables
*      XC(N) = DOUBLE PRECISION ARRAY (Given)
*        Function parameters
*      LDFJAC = INTEGER (Given)
*        Dimension of fjac
*      DENS(M) = DOUBLE PRECISION ARRAY (Given)
*        Y data
*      DATA(M) = DOUBLE PRECISION ARRAY (Given)
*        X data
*      WEIGHT(M) = DOUBLE PRECISION ARRAY (Given)
*        Weights on data
*      RC(M) = DOUBLE PRECISION ARRAY (Returned)
*        Residuals
*      FJAC(LDFJAC,N) = DOUBLE PRECISION ARRAY (Returned)
*        Jacobian
*
* Authors:
*    T.N.Wilkins, Cambridge,  7-OCT-1991
* History:
*-
      implicit none
      integer m
      integer n
      double precision xc(n)
      double precision rc(m)
      integer ldfjac,iflag
      double precision fjac(ldfjac,n)
      double precision data(m),dens(m),weight(m)

*

      integer i,npar,nless2
      double precision dif,rcc
      integer PBASE
      parameter (PBASE = 1)
      double precision tmp,tmp2

* Calculate residuals

      nless2 = n - 2

      if(iflag.eq.1) then
        do i = 1, m
          rcc = xc(1) - dens(i)
          do npar = 2, nless2, 3
            dif = (data(i) - xc(npar+2))/xc(npar)
            rcc = rcc + xc(npar+1)*exp(-0.5d0*dif*dif)
          end do
          rc(i) = rcc*weight(i)
        end do
      else
        do i=1,m

*   base

          fjac( i, PBASE) = weight(i)
          do npar = 2, nless2, 3
            tmp = xc(npar + 1)/xc(npar)
            dif    = ( data(i) - xc(npar + 2) ) / xc(npar)
*
* compute derivatives
*

*   height

            fjac(i,npar+1) = exp ( -0.5d0 * dif * dif) * weight(i)

*   mean

            tmp2 = tmp * fjac(i,npar+1) * dif
            fjac(i, npar+2) = tmp2

*   sigma

            fjac(i, npar) = tmp2 * dif
          end do
        end do
      endif
      end
