      subroutine mlfun_sub(mpts,n,data,fc,dens,xc,gc1,weight,g)
*+
* Name:
*    MLFUN_SUB

* Invocation:
*    CALL MLFUN_SUB(MPTS,N,DATA,FC,DENS,XC,GC1,WEIGHT,G)

* Purpose:
*   Derivatives/residuals for Lorentzian fitting

* Description:
*   To perform the functions of MLFUN which require direct access to
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
*
*      T.N.Wilkins 23/11/88
*      TNW 5/12/88 Altered to use GEN_MOVE, later changed to COPD2D
*      DJA created from MGFUN 25/7/91
*      TNW Cambridge, 23/6/92 Bug fixes
*       "      "    , 1/7/92 GC not used, G now correct I think
*-
      implicit none
      integer n,mpts
      double precision data(mpts),dens(mpts),xc(n),gc1(n)
      double precision rc,fc,weight(mpts)
      integer nless2,i,k,pheight,pcentre,pwidth
      double precision fval,g(n),xb,tmp

      nless2 = n - 2

* zero the residual sum of squares for current pass

      fc=0.0d0

* main loop over Lorentzians and data points calculating
* fc and gc1 for each free parameter.
* rc will contain sum of squares at current ith data point.

      do i = 1 , mpts
        rc = xc(1) - dens(i)

* loop over the Lorentzians


*   base

        g(1)=1.0d0
        do pwidth = 2, nless2, 3
          pheight = pwidth + 1
          pcentre = pwidth + 2
          xb    = (data(i)-xc(pcentre))/xc(pwidth)

          g(pheight) = 1.0d0/(1.0d0 + xb*xb)

          fval    = xc(pheight)*g(pheight)

          g(pcentre)= 2.0d0*fval*xb*g(pheight)/xc(pwidth)
          g(pwidth)= g(pcentre) * xb

          rc  = rc + fval
        end do

* the residuals

        rc = rc * weight(i)

* sum of squares

        fc = fc + rc * rc

* form the gradient vector appropriate for least squares

        tmp = 2.0d0 * rc * weight(i)
        do k=1,n
          gc1(k) = gc1(k) + g(k) * tmp
        end do
      end do
      end
