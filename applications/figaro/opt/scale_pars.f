      subroutine scale_pars(fitpar,optparm,n)
*+
* Name:
*    SCALE_PARS

* Invocation:
*    CALL SCALE_PARS(FITPAR,OPTPARM,N)

* Purpose:
*  Scale parameters to match data.

* Description:
*   Scale the line profile parameters for optimization. This is for
* a single component only (i.e. N up to 6).

* Arguments:
*   N = INTEGER (Given)
*        Number of parameters
*   FITPAR(N) = REAL ARRAY (Given)
*        Parameters (unscaled)
*   OPTPARM(N) = DOUBLE PRECISION ARRAY (Returned)
*        Parameters (scaled)
*- -----------------------------------------------------------------

* must declare everything

      implicit none
* import
      integer n

* scaled answers

      double precision optparm(n)
* export

* rescaled answers

      real fitpar(n)
* global
      include 'opt_cmn'
      integer cmp, ncmp, rem, ip
* --------------------------------------------------------------------
* scale guesses
*
      ncmp = n/3

* Remainder. Note that this will be 1 for skew or cauchy, but -1 for
* tied doubles (hence better this way than mod(n-1,3))

      rem = n - ncmp*3 - 1

* base

      ip = 1
      optparm(ip)  =  (fitpar(ip)-denszero)/densc

      do cmp = 1, ncmp

* sigma/fwhm

        ip = ip + 1
        optparm(ip)  =  fitpar(ip)/datsc

* height

        ip = ip + 1
        optparm(ip)  =  fitpar(ip)/densc


* mean

        ip = ip + 1
        optparm(ip)  =  (fitpar(ip)-datazero)/datsc

* skew or cauchy

        if(rem.eq.1) then
          ip = ip + 1
          optparm(ip)  =  fitpar(ip)
        endif
      enddo
      end
