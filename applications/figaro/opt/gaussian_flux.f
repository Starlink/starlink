      real function gaussian_flux(sg_pars,sg_error,error)
*+
* Name:
*    GAUSSIAN_FLUX

* Invocation:
*   (REAL) = GAUSSIAN_FLUX(SG_PARS,SG_ERROR,ERROR)

* Purpose:
*    Calculate the flux in a gaussian line profile

* Description:
*    Calculate the flux and its error in a gaussian line profile
*
* Arguments:-
*    SG_PARS(2) = REAL ARRAY (Given)
*        Fit parameters (width, height)
*    SG_ERROR(2) = REAL ARRAY (Given)
*        Errors on fit parameters
*    (note that for SG_PARS/SG_ERROR the width is full width at 1/2 max)
*    GAUSSIAN_FLUX = REAL (Returned)
*        Flux
*    ERROR = REAL (Returned)
*        Error on flux
*
*  Bug fix 17-APR-1991 TNW/CAVAD
*-
      implicit none
* import

* fit parameters

      real sg_pars(2)

* error on each parmaeter

      real sg_error(2)
* local

* sqrt of two pi

      real RTWOPI
      real height
      real sigma
      real ss1
      real ss2
      real EFOLD
      parameter (EFOLD = 2.35482004 )
      parameter (RTWOPI = 2.506628275 )
*
* export
*

* error of flux

      real error
* ------------------------------------------------------------------
*
* identify each fit parameter for clarity
*
      sigma  = sg_pars(1) / EFOLD
      height = sg_pars(2)

* evaluate flux

      gaussian_flux = RTWOPI * height * sigma
*
*  error terms
*
      ss1    = sg_error(1) / sg_pars(1)
      ss2    = sg_error(2) / height

* add in quadrature and set return values for error
*
      error  = gaussian_flux * sqrt( ss1 * ss1 + ss2 * ss2 )
      end
