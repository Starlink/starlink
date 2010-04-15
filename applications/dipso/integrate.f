*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
*
*   INTEGRATE.FOR
*
*   Integrates fluxes using trapezoidal integration.
*
*   IMPORTS:
*     NPOINTS         (Integer) Number of points in X, Y arrays
*     X, Y            (Real)    Arrays of X, Y data
*
*   EXPORTS:
*     VALUE           (Real)    Integrated flux
*
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
       SUBROUTINE INTEGRATE(NPOINTS,X,Y,VALUE)

*

       IMPLICIT NONE

       INTEGER NPOINTS, I

       REAL VALUE, X(1), Y(1)

*

       VALUE = 0.0

       IF (NPOINTS.LT.2) RETURN

       DO 100 I = 2, NPOINTS
          VALUE = VALUE + (Y(I-1)+Y(I))*0.5*(X(I)-X(I-1))
  100  CONTINUE

*

       END
