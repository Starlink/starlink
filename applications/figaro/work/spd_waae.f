      SUBROUTINE SPD_WAAE( MAXPAR, INELM, COEFF, XMIN, XMAX,
     :   INX, FITDAT, STATUS )
*+
*  Name:
*     SPD_WAAE

*  Purpose:
*     Evaluate Chebyshev series.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WAAE( MAXPAR, INELM, COEFF, XMIN, XMAX,
*        INX, FITDAT, STATUS )

*  Description:
*     This routine evaluates a finite series of Chebyshev polynomials
*     (first kind). The series goes up to seventh order. Chebyshev
*     polynomials T_n(x') are defined only on an interval [-1,+1]. This
*     interval is mapped into the space of the given array of x values
*     via the two parameters x_{min} and x_{max}:
*
*     x' = [(x - x_{min}) - (x_{max} - x)] / [x_{max} - x_{min}]
*
*     The result then is:
*
*     y(x) = 0.5 c_1 T_0(x') + c_2 T_1(x') + c_3 T_2(x') + c_4 T_3(x')
*              + c_5 T_4(x') + c_6 T_5(x') + c_7 T_6(x') + c_8 T_7(x')
*
*     which would be defined for x within [x_{min},x_{max}]. However,
*     this routine actually evaluates an equivalent ordinary polynomial
*     in x', which is defined for all real numbers x' (and x):
*
*     y(x) = (0.5 c_1 -   1 c_3 +  1 c_5 - 1 c_7)
*          + (  1 c_2 -   3 c_4 +  5 c_6 - 7 c_8) x'
*          + (  2 c_3 -   8 c_5 + 18 c_7) x'^2
*          + (  4 c_4 -  20 c_6 + 56 c_8) x'^3
*          + (  8 c_5 -  48 c_7) x'^4
*          + ( 16 c_6 - 112 c_8) x'^5
*          +   32 c_7 x'^6
*          +   64 c_8 x'^7

*  Arguments:
*     MAXPAR = INTEGER (Given)
*        Maximum order of polynomial plus 1.
*     INELM = INTEGER (Given)
*        Size of arrays INX and FITDAT.
*     COEFF( MAXPAR ) = REAL (Given)
*        Chebyshev polynomial coefficients.
*     XMIN = REAL (Given)
*        The minimum x value where the Chebyshev series is valid.
*     XMAX = REAL (Given)
*        The maximum x value where the Chebyshev series is valid.
*     INX( INELM ) = REAL (Given)
*        Given x value array.
*     FITDAT( INELM ) = REAL (Returned)
*        Data array of evaluation of the polynomial.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This is set to SAI__ERROR if the number of
*        coefficients given is not 8, or if XMIN is greater than or
*        equal to XMAX.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     02 May 1991 (hme):
*        Original version (NUMFIT).
*     31 May 1991 (hme):
*        Avoid divide by SIG if SIG=0.
*        Extrapolate the polynomial fit by turning the series of
*        Chebyshev polynomials into an ordinary polynomial (with
*        argument x', but not restricted to [-1,+1]).
*     26 Jun 1991 (hme):
*        SPFFDT.
*     02 Apr 1992 (hme):
*        Adapt SPABT from SPFFDT. Reduce to evaluating Chebyshev series
*        only. Use single precision coefficients.
*     25 Jan 1995 (hme):
*        Renamed from SPABT.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Bad values

*  Arguments Given:
      INTEGER MAXPAR
      INTEGER INELM
      REAL COEFF( MAXPAR )
      REAL XMIN
      REAL XMAX
      REAL INX( INELM )

*  Arguments Returned:
      REAL FITDAT( INELM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop index
      REAL XX                    ! Transformed argument
      REAL PCOEFF( 8 )           ! Coefficients of ordinary polynomial

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check polynomial order.
      IF ( MAXPAR .NE. 8 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_WAAE_INVORD',
     :      'SPD_WAAE: Error: Invalid polynomial order.', STATUS )
         GO TO 500
      END IF

*  Check abscissa scaling interval.
      IF ( XMIN .GE. XMAX ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_WAAE_INVRNG',
     :      'SPD_WAAE: Error: Invalid abscissa range.', STATUS )
         GO TO 500
      END IF

*  Turn COEFF(1...8) into PCOEFF(1...8).
      PCOEFF(1) =     COEFF(1)/2.   -COEFF(3) +   COEFF(5) -  COEFF(7)
      PCOEFF(2) =     COEFF(2) -  3.*COEFF(4) + 5*COEFF(6) -7*COEFF(8)
      PCOEFF(3) =  2.*COEFF(3) -  8.*COEFF(5) +18*COEFF(7)
      PCOEFF(4) =  4.*COEFF(4) - 20.*COEFF(6) +56*COEFF(8)
      PCOEFF(5) =  8.*COEFF(5) - 48.*COEFF(7)
      PCOEFF(6) = 16.*COEFF(6) -112.*COEFF(8)
      PCOEFF(7) = 32.*COEFF(7)
      PCOEFF(8) = 64.*COEFF(8)

*  Loop through pixels.
      DO 1 I = 1, INELM

*     Transform the argument.
         XX = ( ( INX(I) - XMIN ) - ( XMAX - INX(I) ) )
     :      / ( XMAX - XMIN )

*     Calculate the (ordinary) polynomial.
         FITDAT(I) = REAL( PCOEFF(1) + XX * ( PCOEFF(2)
     :            + XX * ( PCOEFF(3) + XX * ( PCOEFF(4)
     :            + XX * ( PCOEFF(5) + XX * ( PCOEFF(6)
     :            + XX * ( PCOEFF(7) + XX *   PCOEFF(8) ) ) ) ) ) ) )
 1    CONTINUE

*  Return.
 500  CONTINUE
      END
