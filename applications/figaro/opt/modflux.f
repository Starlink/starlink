      subroutine modflux(area,fitpar,area_error,fiterr,model)
*+
* Name:
*    MODFLUX

* Invocation:
*    CALL MODFLUX(AREA,FITPAR,AREA_ERROR,FITERR,MODEL)
*
* Purpose:
*      Evaluate area under model fit to line profile

* Description:
*      Evaluate area under model fit to line profile
*
* Arguments:
*    FITPAR(MODEL+3) = REAL ARRAY (Given)
*        Fit parameters (elements required are 1 = height and 2 = fwhm,
*        also if applicable skew = 5 or Cauchy = 6).
*    FITERR(MODEL+3) = REAL ARRAY (Given)
*        Errors on FITPAR
*    MODEL = INTEGER (Given)
*        Model: - 3 Gaussian without base
*               - 4 Gaussian with base
*               - 5 Skew Gaussian with base
*               - 6 Cauchy function with base
*    AREA = REAL (Returned)
*
*    AREA_ERROR = REAL (Returned)
*
* History:
*   IMAP made argument, TNW 24/1/89
*   N removed, TNW 27/1/89
*-
      implicit none
      integer model
      real fiterr(model+3)
      real fitpar(model+3)
      real area
      real area_error

* local

      real error
      include 'fit_coding_inc'
      real gaussian_flux
      real skew_flux
      real cauchy_flux
      real lorentz_flux
*
* area under gaussian
*
      if(model.eq.GAUSSIAN_MODEL) then
        area = gaussian_flux(fitpar,fiterr,error)
*
* area under skew
*
      else if(model.eq.SKEW_MODEL) then
        area = skew_flux(fitpar,fiterr,error)
*
* area under cauchy

      else  if(model.eq.CAUCHY_MODEL) then
        area = cauchy_flux(fitpar,fiterr,error)

* Area under Lorentz

      else  if(model.eq.LORENTZ_MODEL) then
        area = lorentz_flux(fitpar,fiterr,error)
      end if
*
* error on area
*
      area_error = error
      end
