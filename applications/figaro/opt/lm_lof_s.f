      subroutine lm_lof_s(iflag,m,n,xc,rc,fjac,ldfjac,dens,data,weight)
*+
* Name:
*    LM_LOF_S

* Invocation:
*    CALL LM_LOF_S(IFLAG,M,N,XC,RC,FJAC,LDFJAC,DENS,DATA,WEIGHT)
*
* Description:
*    To calculate residuals or Jacobian for a Lorentz fit model.
*
* Purpose:
*    To calculate residuals or Jacobian for a Lorentz fit model.
*
* Arguments:
*      IFLAG = INTEGER (Given)
*        Flag to determine action of routine, 1 => fvec,
*                        2 => fjac
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
*      SUMA(M) = DOUBLE PRECISION ARRAY (Workspace)
*  Subroutines/functions referenced:
*
* Author:
*    T.N.Wilkins, Cambridge,  21-Aug-1992 based on lm_mgf_s
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

      integer i,width,nless2
      double precision dif,rcc
      integer PBASE
      parameter (PBASE = 1)
      double precision fexp
      double precision tmp,tmp2

* Calculate residuals

      nless2 = n - 2
      if(iflag.eq.1) then
        do i = 1, m
          rcc = xc(1) - dens(i)
          do width = 2, nless2, 3
            dif = (data(i) - xc(width+2))/xc(width)
            fexp = 1.0d0 /  (1.0d0 + dif*dif )
            rcc = rcc + xc(width+1)*fexp
          end do
          rc(i) = rcc*weight(i)
        end do
      else
        do i=1,m

*   base

          fjac( i, PBASE) = weight(i)
          do width = 2, nless2, 3
            dif  = ( data(i) - xc(width + 2) ) / xc(width)
            fexp = 1.0d0 /  (1.0d0 + dif*dif )
*
* compute derivatives
*

*   height

            fjac(i,width+1) = fexp * weight(i)

*   mean

            tmp = xc(width + 1) * fexp
            tmp2 = tmp * 2.0d0 * fjac(i,width+1) * dif/xc(width)
            fjac(i, width+2) = tmp2

*   sigma

            fjac(i, width) = tmp2 * dif
          end do
        end do
      endif
      end
