      subroutine sk_fun(m,n,xc,rc,ajc,data,dens,weight)
*+
* Name:
*    SK_FUN

* Invocation:
*    CALL SK_FUN(M,N,XC,RC,AJC,DATA,DENS,WEIGHT)

* Purpose:
*  Calculate derivatives and residuals for 1d skewed gaussian fit
*
* Description:
*  Calculate derivatives and residuals for 1d skewed gaussian fit
*
* Arguments:
*    M = INTEGER (Given)
*        number of data points
*    N = INTEGER (Given)
*        number of free parameters
*    XC(N) = DOUBLE PRECISION ARRAY (Given)
*        optimization parameters
*       xc(WIDTH)  sigma
*       xc(HEIGHT)  height of gaussian
*       xc(CENTRE)  mean
*       xc(BASE)  background
*       xc(SKEW)  skew parameter
*    DATA(M) = DOUBLE PRECISION ARRAY (Given)
*        X value
*    DENS(M) = DOUBLE PRECISION ARRAY (Given)
*        Y value
*    WEIGHT(M) = DOUBLE PRECISION ARRAY (Given)
*
*    RC(M) = DOUBLE PRECISION ARRAY (Returned)
*        residuals
*    AJC(M,N) = DOUBLE PRECISION ARRAY (Returned)
*        jacobian matrix at the point xc
* History:
*  TNW 5/10/90 Combine rskewg and skew_deriv
*  TNW Cambridge, 20-JUN-1991 Bug fix re weights
*-
      implicit none
      integer m
      integer n
      double precision xc(n)
      double precision rc(m)
      double precision ajc(m,n)
      double precision data(m),dens(m)
      double precision weight(m)

* local
      double precision fexp,zscore,bra,skeww
      double precision A
      double precision d
      double precision f
      double precision c
      double precision e
      integer i
*

* 2.0d0*dlog(2.0d0)

      parameter (A = 1.386294361d0)
*
*     calculates residual between model and data for skewed gaussian

* local

      double precision A0
      double precision rmeanr
      integer BASE, WIDTH, HEIGHT, CENTRE, SKEW
      parameter (BASE = 1, WIDTH = 2, HEIGHT = 3, CENTRE = 4,
     :     SKEW = 5)

* dlog(2.0d0)

      parameter ( A0 = 0.6931471806d0 )
*
* compute residuals
*
      do  i   =   1,m
        zscore = ( data(i)- xc(CENTRE) )/ xc(WIDTH)
        rmeanr = 2.0d0 * xc(SKEW) * zscore
        skeww  = rmeanr
*
*   Test to see if RMEANR<=-1
*
        if (rmeanr.gt.-1.0d0) then
          bra  = (log((1.0d0+rmeanr)))/(xc(SKEW))
          fexp = exp(-A0*(bra*bra))
        else
          bra  = 0.0d0
          fexp = 0.0d0
        end if
        rc(i)= ( xc(HEIGHT) * fexp - dens(i) + xc(BASE) ) * weight(i)

*   base

        ajc(i,1) = weight(i)

*   height

        ajc(i,3) = fexp * weight(i)
        d        = 1.0d0+skeww
*
*  ** Carry out test to see if d=0
*
        if  (d.gt.0.0d0) then
          f        = A*xc(HEIGHT)*fexp*bra * weight(i)
          c        = 2.0d0*f/(d*xc(WIDTH))
          e        = bra-2.0d0*zscore/d

*     mean

          ajc(i,4) = c

*     fwhm

          ajc(i,2) = zscore*c

*     skew

          ajc(i,5) = f*e/xc(SKEW)
        else
          ajc(i,2) = 0.0d0
          ajc(i,3) = 0.0d0
          ajc(i,4) = 0.0d0
          ajc(i,5) = 0.0d0
        end if
      end do
      end
