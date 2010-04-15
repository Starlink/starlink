************************************************************************

      REAL FUNCTION TRAREA ( P1, P2, P3 )

*+
*  Name :
*     TRAREA
*
*  Purpose :
*     This works out the area of a triangle given three vertices p1, p2, p3.
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     TRAREA( P1, P2, P3 )
*
*  Description :
*     This works out the area of a triangle given three vertices p1, p2, p3.
*     Each point is an array of two scalars; the x and y coordinates.
*
*  Arguments :
*     P1( 2 ) = REAL (Given)
*        First vertex of triangle
*     P2( 2 ) = REAL (Given)
*        Second vertex of triangle
*     P3( 2 ) = REAL (Given)
*        Third  vertex of triangle
*     TRAREA = REAL (Returned)
*        Area of triangle
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
*     10-JAN-1988 (NE):
*        Original version.
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Arguments Given :
      REAL P1( 2 )
      REAL P2( 2 )
      REAL P3( 2 )
*.

*   Compute area of triangle
      TRAREA = ABS( 0.5 *
     :              ( ( P2( 1 ) - P1( 1 ) ) * ( P3( 2 ) - P1( 2 ) ) -
     :                ( P3( 1 ) - P1( 1 ) ) * ( P2( 2 ) - P1( 2 ) ) ) )

      END

* $Id$
