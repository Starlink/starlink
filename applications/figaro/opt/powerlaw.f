      real function powerlaw( x, xc, n_par, normalization_wave )
*+
* Name:
*    POWERLAW

* Invocation:
*   (REAL) = POWERLAW( X, XC, N_PAR, NORMALIZATION_WAVE )

* Purpose:
*       Continuum Model 5: POWER LAW

* Description:
*   The Normalization_wavelength is not a fitted paramter.
*   The power law model is defined as :-
*     Normalization*(Lambda/Normalization_wave)**(-Power law index).
*   Were Normalization, and Power Law Index are parameters of the fit
*   We also add a CENTRE parameter to allow the fit to center up on the data
*   Normalization parameters for continuum models should always be in the
*   wavelength range of the data, which guarantees that it is not strongly
*   correlated with, e.g. the power law index.
*   The default value for the continuum Normalization_wavei is set to the
*   END of the wavelength range of the data.
*   In practice because we normalise everything this is actaully = 1.0
*   A Function is provided for re-defining the Normalization Wavelength to
*   be different after fitting


* Arguments:
*    X = REAL (Given)
*        Position at which POWERLAW evaluated
*    XC(N_PAR) = DOUBLE PRECISION ARRAY (Given)
*        Parameters
*    N_PAR = INTEGER (Given)
*        Number of parameters; should be 3
*    NORMALIZATION_WAVE = DOUBLE PRECISION (Given)
*

*       XC(1) = Spectral Index
*       XC(2) = Amplitude
*       XC(3) = centre
*-
      implicit none
      real x
      integer n_par
      double precision xc(n_par)
      double precision normalization_wave

      integer INDEX
      integer AMPLITUDE
      integer CENTRE
      parameter (CENTRE = 3)
      parameter (AMPLITUDE = 2)
      parameter (INDEX = 1)

      real temp

      temp = ( x-xc(CENTRE) ) / normalization_wave
      powerlaw = xc(AMPLITUDE) * temp ** ( -xc(INDEX) )

*       Note the -Index

      end
