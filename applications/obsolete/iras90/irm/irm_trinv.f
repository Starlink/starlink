      SUBROUTINE IRM_TRINV( C, D, STATUS )
*+
*  Name:
*     IRM_TRINV

*  Purpose:
*     Invert a linear transformation.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_TRINV( C, D, STATUS )

*  Description:
*     A linear transformation mapping (X,Y) to (U,V) is described by 6
*     coefficients such that:
*
*     U = C1 + C2*X + C3*Y
*
*     V = C4 + C5*X + C6*Y
*
*     This routine returns the coefficients, D, of the inverse mapping
*     from (U,V) to (X,Y), where
*
*     X = D1 + D2*U + D3*V
*
*     Y = D4 + D5*U + D6*V
*
*     An error is reported if the inverse transformation is singular.

*  Arguments:
*     C( 6 ) = REAL (Given)
*        The coefficients of the forward transformation.
*     D( 6 ) = RETURNED (Given)
*        The coefficients of the inverse transformation.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-NOV-1991 (DSB):
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
      REAL C( 6 )

*  Arguments Returned:
      REAL D( 6 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL    DET                ! Transformation determinant.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Calculate the determinant.
      DET = C( 2 )*C( 6 ) - C( 3 )*C( 5 )

*  If the transformation is not singular, calculatwe the coefficients of
*  the inverse transformation.
      IF( DET .NE. 0.0 ) THEN
         D( 1 ) = ( C( 4 )*C( 3 ) - C( 1 )*C( 6 ) )/DET
         D( 2 ) = C( 6 )/DET
         D( 3 ) = -C( 3 )/DET

         D( 4 ) = ( C( 1 )*C( 5 ) - C( 4 )*C( 2 ) )/DET
         D( 5 ) = -C( 5 )/DET
         D( 6 ) = C( 2 )/DET

*  If the transformation is singular, report an error.
      ELSE
         STATUS = SAI__ERROR
         CALL ERR_REP( 'IRM_TRINV_ERR1',
     :  'IRM_TRINV: Unable to invert a linear transformation.',
     :                 STATUS )
      END IF

      END
