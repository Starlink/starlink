************************************************************************

      LOGICAL FUNCTION POLYIN ( TEST, NL, L, NR, R )

*+
*  Name :
*     POLYIN
*
*  Purpose :
*     Determines if the test point is inside the convex polygon defined by
*     the right and left monotone polygonal sectors ( MPS ).
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     POLYIN( TEST, NL, L, NR, R )
*
*  Description :
*     Determines if the test point is inside the convex polygon defined by
*     the right and left monotone polygonal sectors ( MPS ). The MPS are
*     formed by splitting the polygon into two chains of vertices at the
*     highest and lowest points. Because of convexity the y-coordinates of
*     each segment decrease monotonically. The algorithm first uses a divide
*     and conquer strategy to decide which strips between two vertices,
*     from each of the MPS, contain the test point. The test point is then
*     tested to see if it lies on the correct side of the line to be inside
*     the polygon. If the test point lies on one of the polygon edges it is
*     said to be inside.
*     The right hand MPS is anticlockwise from the highest vertex.
*     The left hand MPS is clockwise from the highest vertex.
*
*     NOTE. The validity of the MPS arrays are assumed, and not checked.
*
*  Arguments :
*     TEST( 2 ) = REAL (Given)
*        X and y coordinates of test point
*     NL = INTEGER (Given)
*        Number of points in list l
*     L( 2, NL ) = REAL (Given)
*        List of x and y coordinates giving left hand MPS
*     NR = INTEGER (Given)
*        Number of points in list r
*     R( 2, NR ) = REAL (Given)
*        List of x and y coordinates giving right hand MPS
*     POLYIN = LOGICAL (Returned)
*        Is point inside polygon
*
*  Algorithm :
*     {algorithm_description}...
*
*  Deficiencies :
*     {routine_deficiencies}...
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*     PWD: Peter W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}
*
*  History :
*     10-JUL-1987 (NE):
*        Original version.
*     29-APR-1996 (PWD):
*        Fixed so that ILPOS and IRPOS never exceed the array bounds.
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Arguments Given :
      REAL TEST( 2 )
      INTEGER NL
      REAL L( 2, NL )
      INTEGER NR
      REAL R( 2, NR )

*  Local Variables :
      INTEGER I, ILPOS, IRPOS
      REAL SIDE
*.

*   If point lies outside y-range of polygon then polyin = .false.
      IF ( ( TEST( 2 ) .GT. R( 2, 1 ) ) .OR.
     :     ( TEST( 2 ) .LT. R( 2, NR ) ) ) THEN
         POLYIN = .FALSE.
      ELSE

*   Find the horizontal band containing the test point for each MPS,
*   using the divide and conquer strategy.
         IRPOS = ( NR + 1 ) / 2
         I = NR
         DO WHILE ( I .GT. 1 )
            I = ( I + 1 ) / 2
            IF ( TEST( 2 ) .GT. R( 2, IRPOS ) ) THEN
               IRPOS = IRPOS - I / 2
            ELSE
               IRPOS = IRPOS + ( I + 1 ) / 2
            ENDIF
            IRPOS = MIN( IRPOS, NR )
         ENDDO

*   Do the same for the left hand MPS
         ILPOS = ( NL + 1 ) / 2
         I = NL
         DO WHILE ( I .GT. 1 )
            I = ( I + 1 ) / 2
            IF ( TEST( 2 ) .GT. L( 2, ILPOS ) ) THEN
               ILPOS = ILPOS - I / 2
            ELSE
               ILPOS = ILPOS + ( I + 1 ) / 2
            ENDIF
            ILPOS = MIN( ILPOS, NL )
         ENDDO

*   Check that the test point is on the inside of both MPS. Note the inside
*   of the polygon is to the right of MPS r and to the left of MPS l.
*   Note changing dimensionality of arrays passed to function side.
         POLYIN = ( ( SIDE( TEST, R( 1, IRPOS - 1 ), R( 1, IRPOS ) )
     :            .GE. 0.0 ) .AND.
     :              ( SIDE( TEST, L( 1, ILPOS - 1 ), L( 1, ILPOS ) )
     :            .LE. 0.0 ) )

      ENDIF

      END

* $Id$
