      subroutine mga_fun(m,n,xc,rc,ajc,data,dens,weight)
*+
* Name:
*    MGA_FUN

* Invocation:
*    CALL MGA_FUN(M,N,XC,RC,AJC,DATA,DENS,WEIGHT)
* Purpose:
*   Calculate derivatives for unconstrained gaussian fit.

* Description:
*   Calculate derivatives for unconstrained gaussian fit.
*
* Arguments:
*    M = INTEGER (Given)
*        Number of points in data
*    XC(N) = DOUBLE PRECISION ARRAY (Given)
*        Fit parameters
*    RC(M) = DOUBLE PRECISION ARRAY (Given)
*        Residuals on fit?
*    N = INTEGER (Given)
*
*    WEIGHT(M) = DOUBLE PRECISION ARRAY (Given)
*        Weights on data points
*    DATA(M) = DOUBLE PRECISION ARRAY (Given)
*
*    DENS(M) = DOUBLE PRECISION ARRAY (Given)
*
*    AJC(M,N) = DOUBLE PRECISION ARRAY (Returned)
*        Derivatives
* History:
*   LJC removed, TNW 24/1/89
*   TNW 25/10/90 Weighting added
*   TNW 21/3/91 Some optimisation
*   TNW Cambridge, 20-JUN-1991 Bug fix re weights
*   DJA Manchester, 8-JULY-1991 Bug fix re weights
*-
      implicit none
      integer m
      double precision data(m)
      double precision dens(m)
      double precision weight(m)
      integer n
      double precision xc(n),rc(m),ajc(m,n)
      double precision invw
      double precision b
      double precision invsigsq
      double precision inv2sigsq
      double precision x
      double precision h,v,dx,expv,rcc
      integer i,nless2,pos

      b  = xc(1)
      nless2 = n - 2
      do i = 1, m
        x   = data(i)
        rcc = b - dens(i)
        do pos = 2, nless2, 3
          invw = 1.0d0/xc(pos)
          h = xc(pos+1)
          v = xc(pos+2)
          invsigsq  = invw * invw
          inv2sigsq = 0.5d0 * invsigsq
          dx = x-v
          expv = exp(-dx * dx * inv2sigsq)
          rcc = rcc + h * expv

*   diff wrt h

          ajc(i,pos+1) = expv * weight(i)

*   diff wrt v

          ajc(i,pos+2) = ajc(i,pos+1) * h * dx * invsigsq


*   wrt w

          ajc(i,pos) = ajc(i,pos+2) * dx * invw

        end do

*   diff wrt b

        ajc(i,1) = weight(i)
        rc(i) = rcc * weight(i)
      enddo
      end
