      REAL FUNCTION PON_EVALPOLY( X, NPOLY, COEFF )
*+
*  Name:
*     PON_EVALPOLY

*  Purpose:
*     Evaluate the given polynomial expression at X.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = PON_EVALPOLY( X, NPOLY, COEFF )

*  Description:
*     Evaluate the given polynomial, defined by NPOLY and COEFF, at X.
*
*        PON_EVALPOLY = COEFF( 1 )
*                       + COEFF( 2 ) * X
*                       + COEFF( 3 ) * X**2
*                       + ...

*  Arguments:
*     X = DOUBLE PRECISION (Given)
*        X-axis value at which to evaluate the polynomial.
*     NPOLY = INTEGER (Given)
*        Number of polynomial coefficients.
*     COEFF( NPOLY ) = DOUBLE PRECISION (Given)
*        Polynomial coefficients.

*  Returned Value:
*     PON_EVALPOLY = REAL
*        The value of the given polynomial at X.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-JUN-1992 (JBVAD::PAH):
*        Original version.
*     24-JUN-1992 (PCTR):
*        Code tidy and prologue changes.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      DOUBLE PRECISION X

      INTEGER NPOLY

      DOUBLE PRECISION COEFF( NPOLY )

*  Local Variables:
      DOUBLE PRECISION Y         ! Polynomial value accumulator
      DOUBLE PRECISION XPOWER    ! Power of X
      INTEGER I                  ! Loop index

*.

*  Initialise Y and XPOWER.
      Y = COEFF( 1 )
      XPOWER = X

*  Loop to calculate the returned value of the polynomial at X.
      DO 10 I = 2, NPOLY
         Y = Y + XPOWER*COEFF( I )
         XPOWER = XPOWER*X
 10   CONTINUE

      PON_EVALPOLY = REAL ( Y )
      END
* $Id$
