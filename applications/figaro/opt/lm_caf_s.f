      subroutine lm_caf_s(iflag,m,n,xc,rc,fjac,ldfjac,dens,data,weight)
*+
* Name:
*    LM_CAF_S

* Invocation:
*    CALL LM_CAF_S(IFLAG,M,N,XC,RC,FJAC,LDFJAC,DENS,DATA,WEIGHT)
*
* Description:
*    To calculate residuals or Jacobian for a CAUCHY fit model.
*
* Purpose:
*    To calculate residuals or Jacobian for a CAUCHY fit model.
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
*    T.N.Wilkins, Cambridge,  14-OCT-1991 based on lm_mgf_s and ca_fun
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
      double precision xto2,power,term

* local

      double precision bra,zscore,brap1

* dlog(2.0d0)

      double precision A

* dummy variables:

      double precision p1
      double precision t1
      double precision t2
      double precision ckett
      double precision z5
      double precision z1
      double precision z2
      double precision z3

* do loop index

      integer i

      integer BASE, WIDTH, HEIGHT, CENTRE, CAUCHY
      parameter (BASE = 1, WIDTH = 2, HEIGHT = 3, CENTRE = 4,
     :     CAUCHY = 5)

* dlog(2.0d0)

      parameter (A = 0.6931471806d0)

      double precision ca
* -------------------------------------------------------------------


* CAUCHY squared

      xto2 = xc(CAUCHY)*xc(CAUCHY)
*
* arithmetic trap for small cauchy
*
      if  (xto2.gt.1.0d-4) then
        power = -1.0d0/xto2
        term  = 2.0d0**xto2-1.0d0
      else
        power = 1.0d-4
        term  = 0.0d0
        xto2  = 1.0d04
      end if
      p1 = power-1.0d0

* calculate residuals

      if(iflag.eq.1) then
        do i=1,m
          zscore  = (data(i)-xc(CENTRE))/xc(WIDTH)
          ckett   = 2.0d0*zscore
          bra     = term*ckett*ckett + 1.0d0
          brap1   = bra**p1

*   abs(bra)**power

          ca      = brap1*bra
          rc(i)   = (xc(HEIGHT)*ca-dens(i)+xc(BASE))*weight(i)
        enddo

      else
        t1 = 2.0d0*xc(HEIGHT)/(xto2)
        t2 = t1/xc(WIDTH)

        do i=1,m
*
* compute derivatives
*
          zscore  = (data(i)-xc(CENTRE))/xc(WIDTH)
          ckett   = (2.0d0*zscore)
          ckett   = ckett*ckett
          bra     = term *ckett + 1.0d0
          brap1   = bra**p1

*   bra**power

          ca      = brap1*bra
          z5      = brap1 * weight(i)
          z1      = z5*term

*   fwhm

          fjac(i,WIDTH) = t2*z1*ckett

*   bra**power  !height

          fjac(i,HEIGHT) = ca * weight(i)

*   mean

          fjac(i,CENTRE) = 4.0d0*t2*z1*zscore

*   base

          fjac(i,BASE) = weight(i)
          z2      = bra*log(bra)/xc(CAUCHY)
          z3      = A*xc(CAUCHY)*(term+1.0d0)*ckett

*   cauchy

          fjac(i,CAUCHY) = t1*z5*(z2-z3)
        end do
      endif
      end
