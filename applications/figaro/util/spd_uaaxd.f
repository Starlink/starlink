      DOUBLE PRECISION FUNCTION SPD_UAAXD( XMIN, XMAX, COEFF, XVAL )
*+
*  Name:
*     SPD_UAAX{DR}

*  Purpose:
*     Get one value for a 7th order Chebyshev series.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = SPD_UAAXD( XMIN, XMAX, COEFF, XVAL )

*  Description:
*     This function returns for one given abscissa value the ordinate
*     value of a finite series of Chebyshev polynomials (first kind).
*     The function value is:
*
*     y(x) = 0.5 c_1 T_0(x') + c_2 T_1(x') + c_3 T_2(x') + c_4 T_3(x')
*              + c_5 T_4(x') + c_6 T_5(x') + c_7 T_6(x') + c_8 T_7(x')
*
*          = (0.5 c_1 -   1 c_3 +  1 c_5 - 1 c_7)
*          + (  1 c_2 -   3 c_4 +  5 c_6 - 7 c_8) x'
*          + (  2 c_3 -   8 c_5 + 18 c_7) x'^2
*          + (  4 c_4 -  20 c_6 + 56 c_8) x'^3
*          + (  8 c_5 -  48 c_7) x'^4
*          + ( 16 c_6 - 112 c_8) x'^5
*          +   32 c_7 x'^6
*          +   64 c_8 x'^7
*
*     where
*
*     x' = [(x - x_{min}) - (x_{max} - x)] / [x_{max} - x_{min}]

*  Arguments:
*     XMIN = DOUBLE PRECISION (Given)
*        The minimum x value where the Chebyshev polynomials are valid.
*     XMAX = DOUBLE PRECISION (Given)
*        The maximum x value where the Chebyshev polynomials are valid.
*     COEFF( 8 ) = DOUBLE PRECISION (Given)
*        Chebyshev polynomial coefficients.
*     XVAL = DOUBLE PRECISION (Given)
*        Given x value array.

*  Returned Value:
*     SPD_UAAXD = DOUBLE PRECISION
*        The ordinate value of the profile at the given abscissa value.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     27 Jul 1992 (hme):
*        Adapted from SPFFDT.
*     27 Jan 1995 (hme):
*        Renamed from SPACMx.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INTEGER MAXPOL             ! Number of coefficients
      PARAMETER ( MAXPOL = 8 )

*  Arguments Given:
      DOUBLE PRECISION XMIN
      DOUBLE PRECISION XMAX
      DOUBLE PRECISION XVAL
      DOUBLE PRECISION COEFF( MAXPOL )

*  Local Variables:
      DOUBLE PRECISION XX                  ! Argument for E02AEF
      DOUBLE PRECISION PCOEFF( MAXPOL )    ! Coefficients of ordinary polynomial

*.

*  Turn COEFF(1...8) into PCOEFF(1...8).
      PCOEFF(1) =    COEFF(1)/2D0 -COEFF(3) +   COEFF(5) -  COEFF(7)
      PCOEFF(2) =    COEFF(2) -  3*COEFF(4) + 5*COEFF(6) -7*COEFF(8)
      PCOEFF(3) =  2*COEFF(3) -  8*COEFF(5) +18*COEFF(7)
      PCOEFF(4) =  4*COEFF(4) - 20*COEFF(6) +56*COEFF(8)
      PCOEFF(5) =  8*COEFF(5) - 48*COEFF(7)
      PCOEFF(6) = 16*COEFF(6) -112*COEFF(8)
      PCOEFF(7) = 32*COEFF(7)
      PCOEFF(8) = 64*COEFF(8)

*  Transform the argument.
      XX =  ( ( XVAL - XMIN ) - ( XMAX - XVAL ) ) / ( XMAX - XMIN )

*  Calculate the (ordinary) polynomial.
      SPD_UAAXD =        PCOEFF(1) + XX * ( PCOEFF(2)
     :            + XX * ( PCOEFF(3) + XX * ( PCOEFF(4)
     :            + XX * ( PCOEFF(5) + XX * ( PCOEFF(6)
     :            + XX * ( PCOEFF(7) + XX *   PCOEFF(8) ) ) ) ) ) )

*  Return.
      END
