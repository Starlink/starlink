      real function lorentz(x,pars)
*+
* Name:
*    LORENTZ

* Invocation:
*   (REAL) = LORENTZ(X,PARS)

* Purpose:
*   Evaluate Lorentzian function

* Description:
*   Evaluate Lorentzian function

* Arguments:
*   X = REAL (Given)
*     Position to evaluate function at
*   PARS = REAL ARRAY (Given)
*     Function paramters

* Returned value:
*   LORENTZ = REAL
*     Value of function

* Authors:
*   TNW: T.N.Wilkins, IoA Cambridge

* History:
*   TNW: 4/2/92, Original version
*   TNW: corrected 23/6/92
*
*-
      implicit none
      real x,pars(4)
      real xb

      xb = (x-pars(4))/pars(2)
      lorentz = pars(1) + pars(3)/(1.0 + xb*xb)
      end
