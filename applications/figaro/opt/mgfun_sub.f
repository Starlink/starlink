      subroutine mgfun_sub(mpts,n,data,fc,dens,xc,gc1,weight,g)
*+
* Name:
*    MGFUN_SUB

* Invocation:
*    CALL MGFUN_SUB(MPTS,N,DATA,FC,DENS,XC,GC1,WEIGHT,G)

* Purpose:
*   Derivative/function evaluation for E04KDF Gaussian fitting

* Description:
*   To perform the functions of MGFUN which require direct access to
*   work arrays. This is to allow these to be passed as pointers in
*   common.
*
* Arguments:
*    MPTS = INTEGER (Given)
*        Number of points
*    N = INTEGER (Given)
*        Number of parameters
*    DATA(MPTS) = DOUBLE PRECISION ARRAY (Given)
*        X array data
*    FC = DOUBLE PRECISION (Given)
*
*    DENS(MPTS) = DOUBLE PRECISION ARRAY (Given)
*        Y array (intensity) data
*    XC(N) = DOUBLE PRECISION ARRAY (Given)
*        Fit parametes
*    GC1(N) = DOUBLE PRECISION ARRAY (Given)
*
*    WEIGHT(MPTS) = DOUBLE PRECISION ARRAY (Given)
*        Weights on points
*    G(N) = DOUBLE PRECISION ARRAY (Given)
*

* History:
*      T.N.Wilkins 23/11/88
*      TNW 5/12/88 Altered to use GEN_MOVE, later changed to COPD2D
*      TNW Cambridge, 1/7/92 Simplified a bit
*-
      implicit none
      integer n,mpts
      double precision data(mpts),dens(mpts),xc(n),gc1(n)
      double precision rc,fc,weight(mpts)
      integer i,k,ip1,ip2,ii,nless2
      double precision gmult,gs,g(n),xb,tmpval

      nless2 = n - 2

* zero the residual sum of squares for current pass

      fc=0.0d0

* main loop over gaussians and data points calculating
* fc and gc for each free parameter.
* rc will contain sum of squares at current ith data point.

      do i = 1 , mpts
        rc = xc(1)

* loop over the gaussians


*   base

        g(1) = 1.0d0
        do ii = 2, nless2, 3
          ip1   = ii+1
          ip2   = ii+2
          xb    = (data(i)-xc(ip2))/xc(ii)
          g(ip1) = exp(-0.5d0*xb*xb)
          gmult = xb/xc(ii)
          gs    = xc(ip1)*g(ip1)
          g(ip2) = gs*gmult
          g(ii) = g(ip2)*xb
          rc  = rc + gs
        end do

* the residuals

        rc = (rc - dens(i))* weight(i)

* sum of squares

        fc = fc + rc * rc

* form the gradient vector appropriate for least squares

        tmpval = 2.0d0 * rc * weight(i)
        do k = 1, n
          gc1(k) = gc1(k) + tmpval * g(k)
        end do
      end do
      end
