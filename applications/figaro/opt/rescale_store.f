      subroutine rescale_store(fitpar,guess,ncmp,reftim,ppcmp)
*+
* Name:
*    RESCALE_STORE

* Invocation:
*    CALL RESCALE_STORE(FITPAR,GUESS,NCMP,REFTIM,PPCMP)

* Purpose:
*   Re-scale the line profile parameters for optimization.

* Description:
*   This routine handles a Multiple profile. It
*   is assumed that the input parameters have already been
*   placed in the correct locations of opt_answers

* Arguments:
*    GUESS(PPCMP,MAX_CMP,MAX_TIMES) = REAL ARRAY (Given)
*        scaled answers
*    NCMP = INTEGER (Given)
*        Number of components
*    REFTIM = INTEGER (Given)
*        Times or reference (not always the same as common variable TIMES).
*    PPCMP = INTEGER (Given)
*        Number of parameters per component
*    FITPAR(MAX_PARMS) = REAL ARRAY (Returned)
*        rescaled answers
* Global vvariables:
*    MAX_CMP = INTEGER (Given)
*       Maximum number of components arrays have room for (include file opt_cmn)
*    MAX_TIMES = INTEGER (Given)
*       Maximum number of fits we can store (include file opt_cmn)
*
      implicit none
      include 'opt_cmn'
      integer ncmp
      integer ppcmp
*-
      integer reftim
      real guess(ppcmp,max_cmp,max_times)
      real fitpar(MAX_PARMS)

* local
      integer ip,cmp,k
* ---------------------------------------------------------------

* Base

      fitpar(1)=guess(1,1,reftim)*real(densc) + real(denszero)
      ip=1
      do cmp = 1, ncmp

*   sigma/fwhm n

        ip = ip + 1
        fitpar(ip) = guess(2,cmp,reftim)*real(datsc)

*   height n

        ip = ip + 1
        fitpar(ip) = guess(3,cmp,reftim)*real(densc)
        ip = ip + 1
        fitpar(ip) = guess(4,cmp,reftim)*real(datsc)
     :                  +real(datazero) !mean n

*     Any further parameters

        do k = 5, ppcmp
          ip = ip + 1
          fitpar(ip) = guess(k,cmp,reftim)
        end do
      end do
      end
