      subroutine rescale_errs(fitpar,var,n,fiterr,model)
*+
* Name:
*    RESCALE_ERRS

* Invocation:
*    CALL RESCALE_ERRS(FITPAR,VAR,N,FITERR,MODEL)

* Purpose:
*   Rescale errors

* Description:
*   Rescale the line profile parameters which are affected
*   by the scaling applied for optimization. Convert all width
*   measures to FWHM. At the same time coerce from REAL*8 to REAL*4.
*
* Arguments:
*    FITPAR(N) = REAL ARRAY (Given)
*        rescaled answers (only used for skew model)
*    VAR(N) = DOUBLE PRECISION ARRAY (Given)
*        scaled errors
*    N = INTEGER (Given)
*        number of free parameters
*    MODEL = INTEGER (Given)
*        Model of fit
*    FITERR(N) = REAL ARRAY (Returned)
*        rescaled errors

* Global variables:
*     DENSC = DOUBLE PRECISION (Returned)
*        Scaling factor for data values (opt_cmn)
*     DATSC = DOUBLE PRECISION (Returned)
*        Scaling factor for axis values (opt_cmn)

* History:
*   TNW, 21-Oct-1991 Made to cope with multiples
*
*
      implicit none
*
* global variables

      include 'opt_cmn'
      include 'fit_coding_inc'
*-
      integer n,model
      double precision var(n)
      real fitpar(n)
      real fiterr(n)

* local

* error on

      real error
      integer ip,cmp,ncmp
* ---------------------------------------------------------------------
*
*  ** Write sigma of current parameters into FITERR
*

* base

      ip = 1
      fiterr(ip) = real(sqrt(abs(var(ip)))*densc)

      ncmp = n/3

      do cmp = 1, ncmp

* sigma/fwhm

        ip = ip + 1
        fiterr(ip) = real(sqrt(abs(var(ip)))*datsc)

* height

        ip = ip + 1
        fiterr(ip) = real(sqrt(abs(var(ip)))*densc)

* mean

        ip = ip + 1
        fiterr(ip) = real(sqrt(abs(var(ip)))*datsc)

*
* calculate error on FWHM for skew gaussian
*
        if(model.eq.SKEW_MODEL) then
          ip = ip + 1
          fiterr(ip) = real(sqrt(abs(var(ip))))

          call sk_fwhm_err(fitpar,fiterr,7,error)
*
* and store in width
*
          fiterr(ip-3)=error

        else if(model.eq.CAUCHY_MODEL) then
          ip = ip + 1
          fiterr(ip) = real(sqrt(abs(var(ip))))
        end if
      enddo
      end
