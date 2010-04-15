      subroutine fw2_fun(m,n,xc,rc,ajc,ratio,data,dens,weight)
*+
* Name:
*    FW2_FUN

* Invocation:
*    CALL FW2_FUN(M,N,XC,RC,AJC,RATIO,DATA,DENS,WEIGHT)

* Purpose:
*   Calculate derivatives for two gaussian fit with fixed width ratio

* Description:
*   Calculate derivatives for two gaussian fit with fixed width ratio

* Arguments:
*    M = INTEGER (Given)
*        Number of points in data
*    XC(6) = DOUBLE PRECISION ARRAY (Given)
*        Fit parameters
*    N = INTEGER (Given)
*
*    DATA(M) = DOUBLE PRECISION ARRAY (Given)
*        X data
*    DENS(M) = DOUBLE PRECISION ARRAY (Given)
*        Y data
*    WEIGHT(M) = DOUBLE PRECISION ARRAY (Given)
*        weights
*    RATIO = DOUBLE PRECISION (Given)
*        Ratio of widths parameters
*    RC(M) = DOUBLE PRECISION ARRAY (Returned)
*        Residuals on fit
*    AJC(M,N) = DOUBLE PRECISION ARRAY (Returned)
*        Derivatives
* History:
*   LJC removed, TNW 24/1/89
*  TNW 18/3/91, LSQ_FW2 and RESID_FW2 combined
*  TNW Cambridge, 20-JUN-1991 Bug fix re weights
*  DJA Manchester, 8-JULY-1991 Bug fix re weights
*-
      implicit none
      double precision ratio
      integer m
      double precision data(m)
      double precision dens(m)
      double precision weight(m)
      integer n
      double precision xc(n),rc(m),ajc(m,n)
      double precision h1
      double precision h2
      double precision v1
      double precision v2
      double precision w1
      double precision w2
      double precision sigsq1
      double precision sigsq2
      double precision twosigsq1
      double precision twosigsq2
      double precision x
      double precision zs1,zs2,expval1,expval2,b
      integer i

      h1 = xc(3)
      v1 = xc(4)
      w1 = xc(2)
      h2 = xc(5)
      v2 = xc(6)
      w2 = w1 * ratio
      b  = xc(1)
      sigsq1    = w1 * w1
      sigsq2    = w2 * w2
      twosigsq1 = 2 * sigsq1
      twosigsq2 = 2 * sigsq2
      do i = 1, m
        x      = data(i)
        zs1      = x-v1
        zs2      = x-v2
        expval1  = exp( -zs1*zs1 / twosigsq1 )
        expval2  = exp( -zs2*zs2 / twosigsq2 )

*   diff wrt h1

        ajc(i,3) = expval1 * weight(i)

*   diff wrt v1

        ajc(i,4) = ajc(i,3) * h1 * zs1 / sigsq1

*   diff wrt h2

        ajc(i,5) = expval2 * weight(i)

*   diff wrt v2

        ajc(i,6) =  ajc(i,5) * h2 * zs2 / sigsq2

*   diff wrt b

        ajc(i,1) = weight(i)

*   O.K.

        ajc(i,2) = ajc(i,4) * zs1 / w1 + ajc(i,6) * zs2 / w1
        rc(i) = (h1 * expval1 + h2 * expval2 + b - dens(i) )*weight(i)
      enddo
      end
