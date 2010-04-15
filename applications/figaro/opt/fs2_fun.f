      subroutine fs2_fun(m,n,xc,rc,ajc,ratio,data,dens,weight)
*+
* Name:
*    FS2_FUN

* Invocation:
*    CALL FS2_FUN(M,N,XC,RC,AJC,RATIO,DATA,DENS,WEIGHT)

* Purpose:
*   Calculate derivatives for two gaussian fit with fixed separation

* Description:
*   Calculate derivatives for two gaussian fit with fixed separation

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
*        Separation of components
*    RC(M) = DOUBLE PRECISION ARRAY (Returned)
*        Residuals on fit
*    AJC(M,N) = DOUBLE PRECISION ARRAY (Returned)
*        Derivatives
* History:
*  TNW 24/1/89, LJC removed
*  TNW 18/3/91, LSQ_FS2 and RESID_FS2 combined
*  TNW Cambridge, 20-JUN-1991 Bug fix re weights
*  DJA Manchester, 8-JUly-1991 Bug fix re weights
*
      implicit none
      double precision ratio
      integer m
      double precision weight(m)
      double precision data(m)
      double precision dens(m)
*-
      integer n
      double precision xc(n),rc(m),ajc(m,n)
      double precision h1
      double precision h2
      double precision v1
      double precision v2
      double precision invw1
      double precision invw2
      double precision inv2sigsq1
      double precision inv2sigsq2
      double precision invsigsq1
      double precision invsigsq2
      double precision separation
      double precision x
      double precision b
      integer i
      double precision zs1,zs2,dv1,dv2
      double precision expval1,expval2
      separation = ratio
      h1 = xc(3)
      v1 = xc(4)
      invw1 = 1.0d0/xc(2)
      h2 = xc(6)
      v2 = v1+separation
      invw2 = 1.0d0/xc(5)
      invsigsq1    = invw1 * invw1
      invsigsq2    = invw2 * invw2
      inv2sigsq1 = 0.5 * invsigsq1
      inv2sigsq2 = 0.5 * invsigsq2
      b  = xc(1)

      do i = 1, m
        x        = data(i)
        zs1      = x-v1
        zs2      = x-v2
        expval1  = exp( -zs1*zs1 * inv2sigsq1 )
        expval2  = exp( -zs2*zs2 * inv2sigsq2 )
*   diff wrt h1

        ajc(i,3) = expval1 * weight(i)

*   diff wrt h2

        ajc(i,6) = expval2 * weight(i)

*   diff wrt v1

        dv1 = ajc(i,3) * h1 * zs1 * invsigsq1

*   diff wrt v2

        dv2 = ajc(i,6) * h2 * zs2 * invsigsq2
        ajc(i,4) = dv1 + dv2

*   diff wrt b

        ajc(i,1) = weight(i)

*   diff wrt s1

        ajc(i,2) = dv1 * zs1 * invw1

*   diff wrt s2

        ajc(i,5) = dv2 * zs2 * invw2

* Residuals

        rc(i) = (h1 * expval1 + h2 * expval2 + b - dens(i)) *weight(i)
      enddo
      end
