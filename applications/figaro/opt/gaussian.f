      real function gaussian(xx,xc)
*+
* Name:
*    GAUSSIAN

* Invocation:
*   (REAL) = GAUSSIAN(XX,XC)

* Purpose:
*   Evalutes a 4 parameter gaussian

* Description:
*   Evalutes a 4 parameter gaussian

* Arguments:
*   XX = REAL (Given)
*        Value of X to evaluate Gaussian at
*   XC(4) = REAL ARRAY (Given)
*        Fit parameters:- Base, Width,height,centre
*   GAUSSIAN = REAL (Returned)
*        Value of function
*
* History:
*  Optimised, TNW/CAVAD, 21/3/91
*  Changed to single precision, TNW 20/9/91
*-

* must declare everything

      implicit none
      real xc(4)
      real xx
      real tmp
*
*   Check for zero width - if so set value to zero also...
*
      if(xc(2).eq.0.0) then
        gaussian = xc(1)
      else
        tmp = ( xx - xc(4) ) / xc(2)
        tmp = -0.5*tmp*tmp
        gaussian = xc(3)*exp(tmp) + xc(1)
      endif
      end
