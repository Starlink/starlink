      real function cauchy(xx,xc)
*+
* Name:
*    CAUCHY

* Invocation:
*   (REAL) = CAUCHY(XX,XC)

* Purpose:
*   Evalutes a 5 PARAMETER CAUCHY FUNCTION

* Description:
*   Evalutes a 5 PARAMETER CAUCHY FUNCTION

* Notes:
*  it is assumed that xc(5) has been set to an appropriate value
*  before entering this routine. XC(5) not used.

*  Arguments:
*    XX = REAL (Given)
*        The value of X for which the function is to be evaluated
*    XC(6) = REAL ARRAY (Given)
*        The fit parameters, in the order width,height,centre,
*                base, not used, Cauchy
*   CAUCHY = REAL (Returned)
*        The value of the Cauchy function
* History:
*   TNW/CAVAD 24/8/90 Changed to work in real*4
*   Optimised, TNW/Cambridge, 21/3/91
*- ----------------------------------------------------------
      implicit none
      real xc(6)
      real power
      real term
      real xto2
      real bra
      real ckett
      real rmean
      real ca
      real xx

      xto2 = xc(5)*xc(5)
*
* arithmetic trap for small cauchy
*
      if(xto2.gt.1.0e-4) then
        power = -1.0/xto2
        term  = 2.0**xto2-1.0
      else
        power = 1.0e-4
        term  = 0.0
        xto2  = 1.0e04
      end if
      rmean  = (xx-xc(4))/xc(2)
      ckett  = 2.0*rmean
      ckett  = ckett*ckett
      bra    = term*ckett+1.0
      ca     = bra**power
      cauchy = xc(3)*ca+xc(1)
      end
