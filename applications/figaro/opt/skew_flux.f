      real function skew_flux(sg_pars,sg_error,error)
*+
* Name:
*    SKEW_FLUX

* Invocation:
*   (REAL) = SKEW_FLUX(SG_PARS,SG_ERROR,ERROR)

* Purpose:
*   Evaluate the flux under a skew Gaussian fit.

* Description:
*   The flux and the error of the flux given by SG_PARS and SG_ERR
*   are returned.

* Arguments:
*   SG_PARS(5) = REAL ARRAY (Given)
*        Fit parameters
*   SG_ERROR(5) = REAL ARRAY (Given)
*        Errors on fit parameters
*   SKEW_FLUX = REAL (Returned)
*        Flux
*   ERROR = REAL (Returned)
*        Error on flux
* History:
*   CONST2 set to correct value T.N.Wilkins 3/5/88
*-
* import
      real sg_error(5)
      real sg_pars(5)
* local
      real fwhm
      real height
      real skew
      real ss1
      real ss2
      real ss3
      real ss
      real flux
      real CONST2
      real FACTOR
      parameter (CONST2 = 1.064467)
      parameter (FACTOR = 2.7725887)

* export
      real error

* ---------------------------------------------------------------
      fwhm   =    sg_pars(1)
      height =    sg_pars(2)
      skew   =    sg_pars(5)
      ss1    =    sg_error(1)/fwhm
      ss2    =    sg_error(2)/height

      ss3    =    skew/FACTOR
      ss3    =    4.0e0*ss3*ss3

      ss     =    ss1*ss1+ss2*ss2
*
      flux   =    CONST2 * height* fwhm
      skew_flux = flux * exp( ( skew * skew ) / FACTOR)
      error  =    skew_flux * sqrt(ss+ss3*sg_error(5)*sg_error(5))
*
      end
