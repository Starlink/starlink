      subroutine rescale_pars(fitpar,optpar,n,model)
*+
* Name:
*    RESCALE_PARS

* Invocation:
*    CALL RESCALE_PARS(FITPAR,OPTPAR,N,MODEL)

* Purpose:
*   Rescale parameters to match "real" data

* Description:
*   Rescale the line profile parameters which are affected
*   by the scaling applied for optimization. If a gaussian
*   convert sigma to FWHM. At the same time coerce from REAL*8 to REAL*4.
*
* Arguments:
*    OPTPAR(N) = DOUBLE PRECISION ARRAY (Given)
*        scaled answers
*    N = INTEGER (Given)
*        Number of parameters
*    MODEL = INTEGER (Given)
*        Model of fit
*    FITPAR(N) = REAL ARRAY (Returned)
*        rescaled answers

* Global variables:
*     DENSC = DOUBLE PRECISION (Returned)
*        Scaling factor for data values (opt_cmn)
*     DATSC = DOUBLE PRECISION (Returned)
*        Scaling factor for axis values (opt_cmn)
*     DATAZERO = DOUBLE PRECISION (Returned)
*        Offset for axis values (opt_cmn)
*     DENSZERO = DOUBLE PRECISION (Returned)
*        Offset for data values (opt_cmn)

* History:
*  TNW Cambridge, 21-Oct-1991
*-

      implicit none

* global data

      include 'opt_cmn'
      include 'fit_coding_inc'
      integer n,model
      double precision optpar(n)
      real fitpar(n)

* external references

      real skew_fwhm
      integer ncmp, cmp, ip
*
* local
*
* fill in correct locations of fitpar
*
      ncmp = n/3

* base

      ip = 1
      fitpar(ip) = real(optpar(ip)*densc+denszero)
      do cmp = 1, ncmp

* fwhm

        ip = ip + 1
        fitpar(ip) = real(optpar(ip)*datsc)

* height

        ip = ip + 1
        fitpar(ip) = real(optpar(ip)*densc)

* mean

        ip = ip + 1
        fitpar(ip) = real(optpar(ip)*datsc+datazero)


* evaluate FWHM for skew fit
*
        if(model.eq.SKEW_MODEL) then
          ip = ip + 1
          fitpar(ip) = real(optpar(ip))
          fitpar(ip - 3) = skew_fwhm(fitpar(ip),fitpar(ip-3))
        else if( model.eq.CAUCHY_MODEL) then
          ip = ip + 1
          fitpar(ip) = real(optpar(ip))
        end if
      enddo

      end
