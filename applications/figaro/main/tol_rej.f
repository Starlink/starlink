      logical function tol_rej(fitpar,tol_min,tol_max)
*+
* Name:
*    TOL_REJ

* Invocation:
*   (LOGICAL) = TOL_REJ(FITPAR,TOL_MIN,TOL_MAX)

* Purpose:
*   Test if tolerances on a fit paramter are met.

* Description:
*   Test if tolerances on a fit paramter are met.
*   This is based on W_REJ, and is designed to replace it and also H_REJ
*   and V_REJ.

* Arguments:
*    FITPAR = REAL (Given)
*        Value
*    TOL_MIN = REAL (Given)
*        Minimum allowed value
*    TOL_MAX = REAL (Given)
*        Maximum allowed value
* Returned value:
*    TOL_REJ = LOGICAL (Given)
*        If failed tolerances
*-
      implicit none
      real fitpar,tol_max,tol_min

      tol_rej = ( fitpar.gt.tol_max ) .or. (fitpar .lt. tol_min )
      end
