      SUBROUTINE KPG1_CHEPD( X, NTERM, T, STATUS )
*+
*  Name:
*     KPG1_CHEPx
 
*  Purpose:
*     Evaluates a Chebyshev polynomial.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL KPG1_CHEPx( X, NTERM, T, STATUS )
 
*  Description:
*     This evaluates a Chebyshev polynomial for orders zero to NTERM-1
*     at a given normalised [-1,+1] co-ordinate.   It uses a recurrence
*     relationship to evaluate beyond the second term.
 
*  Arguments:
*     X = ? (Given)
*        The normalised co-ordinate for which the Chebyshev polynomial
*        is to be evaluated.  It is assumed to lie in the range
*        [-1,+1] having been normalised using the limits that created
*        the coefficients.
*     NTERM = INTEGER (Given)
*        The number of terms in the Chebyshev polynomial.  It equals the
*        order plus one.
*     T( NTERM ) = ? (Returned)
*        The evaluated Chebyshev polynomial for each term.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  References:
*     -  T. Hopkins & C.Phillips, 1988, "Numerical Methods in Practice",
*     Addison-Wesley, p.190-191.
*     [routine_references]...
 
*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     1996 October 7 (MJC):
*        Original version.
*     {enter_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
 
*  Arguments Given:
      DOUBLE PRECISION X
      INTEGER NTERM
 
*  Arguments Returned:
      DOUBLE PRECISION T( NTERM )
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Variables:
      INTEGER I                  ! Order loop counter
 
*.
 
*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Handle the most-likely case first.
      IF ( NTERM .GT. 2 ) THEN
 
*  The zeroth- and first-order terms are one and the co-ordinate by
*  definition.
         T( 1 ) = 1.0D0
         T( 2 ) = X
 
*  Apply the Clenshaw recurrence relationship.
         DO I = 3, NTERM
            T( I ) = 2.0D0 * X * T( I - 1 ) - T( I - 2 )
         END DO
 
      ELSE IF ( NTERM .EQ. 2 ) THEN
         T( 1 ) = 1.0D0
         T( 2 ) = X
 
      ELSE IF ( NTERM .EQ. 1 ) THEN
         T( 1 ) = 1.0D0
      END IF
 
      END
