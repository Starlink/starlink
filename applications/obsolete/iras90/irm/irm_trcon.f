      SUBROUTINE IRM_TRCON( C1, C2, C3, STATUS )
*+
*  Name:
*     IRM_TRCON

*  Purpose:
*     Concatenate two linear transformations.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_TRCON( C1, C2, C3, STATUS )

*  Description:
*     A linear transformation mapping (X,Y) to (U,V) is described by 6
*     coefficients such that:
*
*     U = C1 + C2*X + C3*Y
*
*     V = C4 + C5*X + C6*Y
*
*     This routine concatenates two such transformation to produce a
*     third transformation. If C1 contains the coefficients of the
*     transformation from (X,Y) to (U,V), and C2 contains the
*     coefficients of the transformation from (U,V) to (A,B), then C3
*     is returned holding the coefficients of the transformation from
*     (X,Y) to (A,B).

*  Arguments:
*     C1( 6 ) = REAL (Given)
*        The coefficients of the first transformation.
*     C2( 6 ) = REAL (Given)
*        The coefficients of the second transformation.
*     C3( 6 ) = RETURNED (Given)
*        The coefficients of the concatenated transformation.
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
      REAL C1( 6 )
      REAL C2( 6 )

*  Arguments Returned:
      REAL C3( 6 )

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up the output coefficients.
      C3( 1 ) = C2( 1 ) + C2( 2 )*C1( 1 ) + C2( 3 )*C1( 4 )
      C3( 2 ) =           C2( 2 )*C1( 2 ) + C2( 3 )*C1( 5 )
      C3( 3 ) =           C2( 2 )*C1( 3 ) + C2( 3 )*C1( 6 )

      C3( 4 ) = C2( 4 ) + C2( 5 )*C1( 1 ) + C2( 6 )*C1( 4 )
      C3( 5 ) =           C2( 5 )*C1( 2 ) + C2( 6 )*C1( 5 )
      C3( 6 ) =           C2( 5 )*C1( 3 ) + C2( 6 )*C1( 6 )


      END
