      SUBROUTINE SPD_WZED( MAXGAU, MAXPOL, INELM, TYPE, CONT,
     :   CENTRE, PEAK, SIGMA, COEFF, XMIN, XMAX, INX, FITDAT, STATUS )
*+
*  Name:
*     SPD_WZED

*  Purpose:
*     Calculate fit data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZED( MAXGAU, MAXPOL, INELM, TYPE, CONT,
*        CENTRE, PEAK, SIGMA, COEFF, XMIN, XMAX, INX, FITDAT, STATUS )

*  Description:
*     This routine calculates the fit data array for FITGAUSS. This is
*     either a sum of several Gauss components or a finite series of
*     Chebyshev polynomials (first kind). In the latter case
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
*     MAXGAU = INTEGER (Given)
*        Size of arrays for Gauss components.
*     MAXPOL = INTEGER (Given)
*        Maximum order of polynomial plus 1.
*     INELM = INTEGER (Given)
*        Size of arrays INX and FITDAT.
*     TYPE = INTEGER (Given)
*        If type is 0 or negative, a polynomial fit is selected. The
*        absolute value of TYPE is the order of the polynomial. TYPE must
*        be greater than or equal to -(MAXPOL-1).
*        If type is positive, a multi-Gauss fit is selected. The value
*        of TYPE is the number of Gauss components. TYPE must be less
*        than or equal to MAXGAU.
*     CONT = REAL (Given)
*        For Gauss fits indicates the level of the continuum. Reasonable
*        values are 0 and 1. Before a Gauss fit can be successful, the
*        spectrum must either have the baseline subtracted (CONT=0.) or
*        have been divided by the continuum (CONT=1.). But formally any
*        constant value for the continuum is possible.
*     CENTRE( MAXGAU ) = REAL (Given)
*        Centre position for each Gauss component.
*     PEAK( MAXGAU ) = REAL (Given)
*        Peak height for each Gauss component.
*     SIGMA( MAXGAU ) = REAL (Given)
*        Dispersion for each Gauss component.
*     COEFF( MAXPOL ) = DOUBLE PRECISION (Given)
*        Chebyshev polynomial coefficients.
*     XMIN = REAL (Given)
*        The minimum x value where the polynomial is valid.
*     XMAX = REAL (Given)
*        The maximum x value where the polynomial is valid.
*     INX( INELM ) = REAL (Given)
*        Given x value array.
*     FITDAT( INELM ) = REAL (Returned)
*        Data array with fit values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
*     27 Jan 1995 (hme):
*        Renamed from SPFFDT.
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
      INTEGER MAXGAU
      INTEGER MAXPOL
      INTEGER INELM
      INTEGER TYPE
      REAL CONT
      REAL CENTRE( MAXGAU )
      REAL PEAK( MAXGAU )
      REAL SIGMA( MAXGAU )
      REAL XMIN
      REAL XMAX
      REAL INX( INELM )
      DOUBLE PRECISION COEFF( MAXPOL )

*  Arguments Returned:
      REAL FITDAT( INELM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I, C               ! Loop indices
      REAL CEN, PEA, SIG         ! Gauss parameters
      REAL ARG                   ! Argument of EXP function
      DOUBLE PRECISION XX        ! Argument for E02AEF
      DOUBLE PRECISION PCOEFF( 8 ) ! Coefficients of ordinary polynomial

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Gauss fit.
      IF ( TYPE .GT. 0 ) THEN

*     Set fit data to (constant) continuum.
         DO 1 I = 1, INELM
            FITDAT(I) = CONT
 1       CONTINUE

*     Add one Gauss component after the other.
         DO 3 C = 1, TYPE
            CEN = CENTRE(C)
            PEA = PEAK(C)
            SIG = SIGMA(C)

*     A delta distribution is ignored.
            IF ( SIG .NE. 0. ) THEN
               DO 2 I = 1, INELM
                  ARG = INX(I) - CEN
                  ARG = ARG * ARG / 2. / SIG / SIG
                  FITDAT(I) = FITDAT(I) + PEA * EXP(-ARG)
 2             CONTINUE
            END IF
 3       CONTINUE

*  Polynomial fit.
      ELSE IF ( XMAX - XMIN .GT. 0. ) THEN

*     Turn COEFF(1...8) into PCOEFF(1...8).
         PCOEFF(1) =    COEFF(1)/2D0 -COEFF(3) +   COEFF(5) -  COEFF(7)
         PCOEFF(2) =    COEFF(2) -  3*COEFF(4) + 5*COEFF(6) -7*COEFF(8)
         PCOEFF(3) =  2*COEFF(3) -  8*COEFF(5) +18*COEFF(7)
         PCOEFF(4) =  4*COEFF(4) - 20*COEFF(6) +56*COEFF(8)
         PCOEFF(5) =  8*COEFF(5) - 48*COEFF(7)
         PCOEFF(6) = 16*COEFF(6) -112*COEFF(8)
         PCOEFF(7) = 32*COEFF(7)
         PCOEFF(8) = 64*COEFF(8)

*     Loop through pixels.
         DO 4 I = 1, INELM

*        Transform the argument.
            XX = DBLE( ( ( INX(I) - XMIN ) - ( XMAX - INX(I) ) )
     :         / ( XMAX - XMIN ) )

*        Calculate the (ordinary) polynomial.
            FITDAT(I) = REAL( PCOEFF(1) + XX * ( PCOEFF(2)
     :               + XX * ( PCOEFF(3) + XX * ( PCOEFF(4)
     :               + XX * ( PCOEFF(5) + XX * ( PCOEFF(6)
     :               + XX * ( PCOEFF(7) + XX *   PCOEFF(8) ) ) ) ) ) ) )
 4       CONTINUE

*  No valid fit.
      ELSE
         DO 5 I = 1, INELM
            FITDAT(I) = VAL__BADR
 5       CONTINUE
      END IF

      END
