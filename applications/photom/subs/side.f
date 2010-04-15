************************************************************************

      REAL FUNCTION SIDE ( TEST, P1, P2 )

*+
*  Name :
*     SIDE
*
*  Purpose :
*     Defines which side of the line given by p1 and p2 the test point is.
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     SIDE ( TEST, P1, P2 )
*
*  Description :
*     Defines which side of the line given by p1 and p2 the test point is.
*     Going from point 1 to point 2 the value of side is positive if the test
*     point is on the left hand side of the line; negative if on the right
*     hand side; and has a value of zero if the test point lies on the line.
*
*  Arguments :
*     TEST( 2 ) = REAL (Given)
*        X and y coordinates of test point
*     P1( 2 ) = REAL (Given)
*        X and y coordinates of first point of line
*     P2( 2 ) = REAL (Given)
*        X and y coordinates of second point of line
*     SIDE = REAL (Returned)
*        Value signifying which side of line test point is
*
*  Algorithm :
*     {algorithm_description}...
*
*  Deficiencies :
*     {routine_deficiencies}...
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*     {enter_new_authors_here}
*
*  History :
*     10-JUL-1987 (NE):
*        Original version.
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Arguments Given :
      REAL TEST( 2 )
      REAL P1( 2 )
      REAL P2( 2 )

*  Local Variables :
      REAL DX, DX1, DY, DY1
*.

*   Calculate the differences to be used in the test
      DX = P2( 1 ) - P1( 1 )
      DY = P2( 2 ) - P1( 2 )
      DX1 = TEST( 1 ) - P1( 1 )
      DY1 = TEST( 2 ) - P1( 2 )

*   Return the required result
      SIDE = DX * DY1 - DY * DX1

      END

* $Id$
