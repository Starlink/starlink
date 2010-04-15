************************************************************************

      LOGICAL FUNCTION INSIDE ( TEST, P1, P2 )

*+
*  Name :
*     INSIDE
*
*  Purpose :
*     Determine whether the test point is inside the box defined by the
*     two corners p1 and p2.
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     INSIDE( TEST, P1, P2 )
*
*  Description :
*     Determine whether the test point is inside the box defined by the
*     two corners p1 and p2. If the point lies on the edge of the box
*     then it is counted as being inside.
*
*  Arguments :
*     TEST( 2 ) = REAL (Given)
*        X and y coordinate of test point
*     P1( 2 ) = REAL (Given)
*        X and y coordinate of corner of box
*     P2( 2 ) = REAL (Given)
*        X and y coordinate of corner of box
*     INSIDE = LOGICAL (Returned)
*        Is test point inside box
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
      REAL HIX, HIY, LOX, LOY
*.

*   Find the lower left and upper right corners of box
      IF ( P1( 1 ) .LE. P2( 1 ) ) THEN
         LOX = P1( 1 )
         HIX = P2( 1 )
      ELSE
         LOX = P2( 1 )
         HIX = P1( 1 )
      ENDIF

      IF ( P1( 2 ) .LE. P2( 2 ) ) THEN
         LOY = P1( 2 )
         HIY = P2( 2 )
      ELSE
         LOY = P2( 2 )
         HIY = P1( 2 )
      ENDIF

*   Check if test point is inside box
      INSIDE = ( ( TEST( 1 ) .GE. LOX ) .AND. ( TEST( 1 ) .LE. HIX )
     :     .AND. ( TEST( 2 ) .GE. LOY ) .AND. ( TEST( 2 ) .LE. HIY ) )

      END

* $Id$
