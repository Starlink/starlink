      real function lorentz_flux(fitpar,fiterr,error)
*+
* Name:
*    LORENTZ_FLUX

* Invocation:
*   (REAL) = LORENTZ_FLUX(FITPAR,FITERR,ERROR)

* Purpose:
*   Evaluate flux for Lorentzian function

* Description:
*   Evaluate flux for Lorentzian function
*
* Arguments:-
*    FITPAR(2) = REAL ARRAY (Given)
*        Fit parameters (width, height)
*    FITERR(2) = REAL ARRAY (Given)
*        Errors on fit parameters
*    LORENTZ_FLUX = REAL (Returned)
*        Flux
*    ERROR = REAL (Returned)
*        Error on flux

* Authors:
*   TNW: T.N.Wilkins IoA Cambridge

* History:
*   TNW: 3/7/92 Original version
*-
      implicit none
      real fitpar(*),fiterr(*),error,HALFPI,ss1,ss2
      parameter (HALFPI = 1.5707963)
      lorentz_flux = fitpar(1) * fitpar(2) * HALFPI
      ss1 = fitpar(1) * fiterr(2)
      ss2 = fitpar(2) * fiterr(1)
      error = HALFPI * sqrt(ss1 * ss1 + ss2 * ss2)
      end
