      real function skew(xx,xc)
*+
* Name:
*    SKEW

* Invocation:
*   (REAL) = SKEW(XX,XC)
* Purpose:
*   Evalutes a 5 PARAMETER SKEW FUNCTIOM

* Description:
* It is assumed that xc(5) has been set to an appropriate value
* before entering this routine and that xc(1) is the true FWHM
* rather than the optimisation parameter XHALF.

* History:
* TNW/CAVAD 24/8/90 Changed to use real*4 for XC
*-
      implicit none
      real xc(5)
      real rmeanr
      real fexp
      real rmean
      real A
      real skewp
      real xx
      real xhalf
*

* log(2.0d0)

      parameter (A = 0.6931471806)

      xhalf = xc(2)*xc(5)/sinh(xc(5))
      rmean = (xx-xc(4))/xhalf
      rmeanr = 2.0*xc(5)*rmean
*
*  ** Test to see if RMEANR<=-1
*
      if (rmeanr.gt.-1.0e0) then
        skewp = (log((1.0+rmeanr)))/(xc(5))
        fexp  = exp(-A*skewp*skewp)
      else
        fexp  = 0.0
        skewp = 0.0
      end if
      skew  = xc(3)*fexp+xc(1)
      end
