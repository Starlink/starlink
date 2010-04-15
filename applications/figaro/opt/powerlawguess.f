      subroutine powerlawguess( x1, y1, x2, y2, xc, n_par,
     :     normalization_wave )
*+
* Name:
*    POWERLAWGUESS

* Invocation:
*    CALL POWERLAWGUESS( X1, Y1, X2, Y2, XC, N_PAR,
*          NORMALIZATION_WAVE )

* Description:
* make sensible inital guesses for parameters of powerlaw model
* good alterantive guesses for parameters are
*  spectral index = -1
*  centre         = 0.5
*  amplitude      = max dens value in data
* We try and define them empirically from the data
*
* Arguments:
*    X1 = REAL (Given)
*
*    Y1 = REAL (Given)
*
*    X2 = REAL (Given)
*
*    Y2 = REAL (Given)
*
*    XC(N_PAR) = DOUBLE PRECISION ARRAY (Given)
*
*    N_PAR = INTEGER (Given)
*
*    NORMALIZATION_WAVE = DOUBLE PRECISION (Given)
*
*-
      implicit none
      real x1, y1, x2, y2
      integer n_par
      double precision xc(n_par)
      double precision normalization_wave
      integer INDEX
      integer AMPLITUDE
      integer CENTRE
      parameter (CENTRE = 3)
      parameter (AMPLITUDE = 2)
      parameter (INDEX = 1)

      xc(CENTRE) = (x2-x1)/2
      xc(INDEX) = -( log(y2) - log(y1) ) / ( log(x2) - log(x1) )
      xc (AMPLITUDE ) =
     :            y1 * ( ( x1 / normalization_wave ) ** xc(INDEX) )

      end
