      subroutine c_norm_wav_pl( xc, n_par, old, new )
*+
* Name:
*    C_NORM_WAV_PL

* Invocation:
*    CALL C_NORM_WAV_PL( XC, N_PAR, OLD, NEW )
* Purpose:
*   Change the Normalization wavelength of the POWERLAW fit
*
* Description:
*   Change the Normalization wavelength of the POWERLAW fit
*
* Arguments:
*   XC(N_PAR) = DOUBLE PRECISION ARRAY (Given)
*        Fit parameters for this POWERLAW
*   N_par = INTEGER (Given)
*        Number of parameters = 3
*   Old = DOUBLE PRECISION (Given)
*        Old value of NORMALIZATION_WAVELENGTH
*   NEW = DOUBLE PRECISION (Given)
*        New Vlaue of Normalization_WAVELENGTH
*
      implicit none
      include 'opt_cmn'
*-
      integer n_par
      double precision xc(n_par)
      double precision old, new

* symbolic constants for the model parameters

      integer INDEX
      integer AMPLITUDE
      parameter (AMPLITUDE = 2)
      parameter (INDEX = 1)

* Update the AMPLITUDE to reflect the new NORmALIZATION

      xc(AMPLITUDE) = xc(AMPLITUDE) * ( old / new ) ** xc(INDEX)

      end
