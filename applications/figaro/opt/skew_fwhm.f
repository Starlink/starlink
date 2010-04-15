      real function skew_fwhm(b,xhalf)
*+
* Name:
*    SKEW_FWHM

* Invocation:
*   (REAL) = SKEW_FWHM(B,XHALF)

* Purpose:
*  Evaluate FWHM of SKEW function
*
* Description:
*   The full width at half maximum of the skew function is returned as
*   the function value.
* Arguments:
*    B = REAL (Given)
*
*    XHALF = REAL (Given)
*
* Returned (function value):
*    SKEW_FWHM     (r)
*-
      implicit none
      real b
      real xhalf

      skew_fwhm = sinh(b) * xhalf / b
      end
