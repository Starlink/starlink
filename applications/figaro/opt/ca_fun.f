      subroutine ca_fun(m,n,xc,rc,ajc,data,dens,weight)
*+
* Name:
*    CA_FUN

* Invocation:
*    CALL CA_FUN(M,N,XC,RC,AJC,DATA,DENS,WEIGHT)
*
* Purpose:
*   Calculate derivatives for 1d cauchy fit

* Description:
*   Calculates residual between model and data for adaptive cauchy
*
* Arguments:
*    M = INTEGER (Given)
*        number of data points
*    N = INTEGER (Given)
*        number of variables
*    XC(N) = DOUBLE PRECISION ARRAY (Given)
*        model parameters
*         xc(2)    FWHM or sigma??
*         xc(3)    height of gaussian
*         xc(4)    mean
*         xc(1)    Background
*         xc(5)    cauchy exponent
*    DATA(M) = DOUBLE PRECISION ARRAY (Given)
*        X data
*    DENS(M) = DOUBLE PRECISION ARRAY (Given)
*        counts
*    WEIGHT(M) = DOUBLE PRECISION ARRAY (Given)
*       weights
*    RC(M) = DOUBLE PRECISION ARRAY (Returned)
*        residuals
*    AJC(M,N) = DOUBLE PRECISION ARRAY (Returned)
*        jacobian matrix at the point xc
* Authors:
*   DJA: D.J.Axon, Manchester
*   TNW: T.N.Wilkins, Cambridge

* History:
*   DJA: Original code
*   Combination of RCAUCH and CAUCHY_DERIV, TNW 5/10/90
*   TNW 25/10/90 Weighting added
*   TNW 21/3/91 Some optimisation
*   TNW 20-JUN-1991 Bug fix re weights
*-
      implicit none
      integer m
      double precision term,power
      double precision xto2
      integer n
      double precision xc(n)
      double precision rc(m)
      double precision ajc(m,n)
      double precision data(m),dens(m)
      double precision weight(m)

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
      t1 = 2.0d0*xc(HEIGHT)/(xto2)
      t2 = t1/xc(WIDTH)

* calculate residuals

      do i=1,m
        zscore  = (data(i)-xc(CENTRE))/xc(WIDTH)
        ckett   = (2.0d0*zscore)
        ckett   = ckett*ckett
        bra     = term*ckett + 1.0d0
        brap1   = bra**p1

*   abs(bra)**power

        ca      = brap1*bra
        rc(i)   = (xc(HEIGHT)*ca-dens(i)+xc(BASE))*weight(i)
*
* compute derivatives
*
        z5       = brap1 * weight(i)
        z1       = z5*term

*   fwhm

        ajc(i,WIDTH) = t2*z1*ckett

*   abs(bra)**power  !height

        ajc(i,HEIGHT) = ca * weight(i)

*   mean

        ajc(i,CENTRE) = 4.0d0*t2*z1*zscore

*   base

        ajc(i,BASE) = weight(i)
        z2       = bra*log(bra)/xc(CAUCHY)
        z3       = A*xc(CAUCHY)*(term+1.0d0)*ckett

*   cauchy

        ajc(i,CAUCHY) = t1*z5*(z2-z3)
      end do
      end
