      subroutine bl_fun(m,n,xc,rc,ajc,data,dens,weight)
*+
* Name:
*    BL_FUN

* Invocation:
*    CALL BL_FUN(M,N,XC,RC,AJC,DATA,DENS,WEIGHT)
*
* Purpose:
*     Calculate residual between model and data and the gradient of
*     the function, for single Lorentzian mode (with or without base).

* Description:
*     Calculate residual between model and data and the gradient of
*     the function, for single Lorentzian mode (with or without base).

* Arguments:
*   N = INTEGER (Given)
*        Number of parameters
*   M = INTEGER (Given)
*        Number of data points
*   DATA(M) = DOUBLE PRECISION ARRAY (Given)
*        channel numbers etc. scaled to range 0-1
*   DENS(M) = DOUBLE PRECISION ARRAY (Given)
*        counts scaled to range 0-1
*   WEIGHT(M) = DOUBLE PRECISION ARRAY (Given)
*        Weights on data points
*   RC(M) = DOUBLE PRECISION ARRAY (Returned)
*        Residuals
*   AJC(M,N) = DOUBLE PRECISION ARRAY (Returned)
*        Jacobian matrix at the point xc
* History:
*   Combination of RESID1 and GAUSS_DERIV. TNW 5/10/90. This means less do
*   loops and 2 work arrays can be removed.
*   TNW 25/10/90 Weighting added
*   TNW Cambridge, 20-JUN-1991 Bug fix re weights
*   DJA Manchester, 8-JULY-1991 Bug fix re weights
*   DJA Manchester, 25-JULY-1991 created from BG_FUN
*   TNW Cambridge, 23/6/92 Bug fixes
*    "      "      1/7/92, handle multiples
*-
      implicit none
      integer n
      integer m
      double precision xc(n)
      double precision rc(m)
      double precision weight(m)
      double precision ajc(m,n)
      double precision data(m),dens(m)

* local

      double precision fval,tmp2
      integer i
      integer PBASE,centre,width,height,nless2
      parameter (PBASE = 1)
      double precision base
      double precision fexp,xb,rcc
* ------------------------------------------------------------------

      base = xc(PBASE)

      nless2 = n - 2
      do i=1,m

*   base

        ajc(i, PBASE) = weight(i)
        rcc = base - dens(i)
        do width = 2, nless2, 3
          height = width + 1
          centre = width + 2
          xb = ( data(i) - xc(centre) ) / xc(width)
          fexp = 1.0d0 /  (1.0d0 + xb*xb )
          fval = xc(height)*fexp
          rcc = rcc + fval
*
* compute derivatives
*
*   height

          ajc(i,height) = fexp * weight(i)

*   mean

          tmp2 = fval * 2.0d0 * ajc(i,height) * xb/xc(width)
          ajc(i, centre) = tmp2

*   sigma

          ajc(i, width) = tmp2 * xb
        end do
        rc(i) = rcc * weight(i)
      end do
      end
