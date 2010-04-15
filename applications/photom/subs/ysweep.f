************************************************************************

      SUBROUTINE YSWEEP ( NM, MPS, NS, SQUARE, NB, NY, YLIST )

*+
*  Name :
*     YSWEEP
*
*  Purpose :
*     This computes the y-values of the horizontal lines needed by POLINS
*     for the plane sweep technique.
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL YSWEEP( NM, MPS, NS, SQUARE, NB, NY, YLIST )
*
*  Description :
*     This computes the y-values of the horizontal lines needed by POLINS
*     for the plane sweep technique. The input lists are a monotone
*     polygonal sector and the vertices of a square. The output list is
*     made up of the following :- if the top or bottom edges of the square
*     intersect the MPS then they are included at the beginning and end of
*     the ylist; all MPS vertices with y-values between the top and bottom
*     of the square; y-values where the MPS crosses the side of the square.
*     The MPS is given by decreasing y-value.
*     The vertices of the square must be given according to the following
*     plan  2 - 1
*           |   |
*           3 - 4  i.e. anti-clockwise from top right.
*
*     NOTE. The validitiy of the MPS arrays are assumed, and not checked.
*
*  Arguments :
*     NM = INTEGER (Given)
*        Number of elements in MPS
*     MPS( 2, NM ) = REAL (Given)
*        List of vertices in monotone polygonal sector
*     NS = INTEGER (Given)
*        Number of elements in square ( should be 4 )
*     SQUARE( 2, NS ) = REAL (Given)
*        List of vertices in square
*     NB = INTEGER (Given)
*        First vertex of mps below top of the square
*        or 1 if unsure of this value
*     NY = INTEGER (Given and Returned)
*        Number of values in ylist
*     YLIST( NY ) = REAL (Returned)
*        List of y-values for plane sweep
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
*     10-JAN-1988
*        Corrected for polygon edge going through both sides of the
*        square, and only tested the range vertices of the mps that
*        bound the square. And added calls to VINSEC rather than INSECT.
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Arguments Given :
      INTEGER NM
      REAL MPS( 2, NM )
      INTEGER NS
      REAL SQUARE( 2, NS )
      INTEGER NB

*  Arguments Given and Returned :
      INTEGER NY

*  Arguments Returned :
      REAL YLIST( NY )

*  Local Variables :
      INTEGER J

      REAL CROSSL( 2 ), CROSSR( 2 ), SIDE, SIDE1, SIDE11, SIDE2, SIDE22

      LOGICAL TL, TR
*.

*   Initialise the output variable
      NY = 0

*   Check if top of square or top of MPS is to be included
      IF ( ( SQUARE( 2, 1 ) .LT. MPS( 2, 1 ) ) .AND.
     :     ( SQUARE( 2, 1 ) .GT. MPS( 2, NM ) ) ) THEN
         NY = NY + 1
         YLIST( NY ) = SQUARE( 2, 1 )
      ENDIF
      IF ( ( MPS( 2, 1 ) .LE. SQUARE( 2, 1 ) ) .AND.
     :     ( MPS( 2, 1 ) .GE. SQUARE( 2, 4 ) ) ) THEN
         NY = NY + 1
         YLIST( NY ) = MPS( 2, 1 )
      ENDIF

*   Start the search from the vertex just above the top of the square
      IF ( NB .GT. 1 ) THEN
         J = NB - 1
      ELSE
         J = 1
      ENDIF

*   Establish which side of the squares' vertical sides the first useful
*   vertex of the MPS is.
      SIDE1 = SIDE( MPS( 1, J ), SQUARE( 1, 1 ), SQUARE( 1, 4 ) )
      SIDE2 = SIDE( MPS( 1, J ), SQUARE( 1, 2 ), SQUARE( 1, 3 ) )

*   This has established the side of the first point. Start from the next
      J = J + 1

*   Get the y-values to be used in the plane sweep. The vertical sides
*   need only be checked if the vertices are on different sides as this
*   is more efficient than checking the intersection every time.
*   Can stop the search once the previous vertex has cleared the bottom
*   of the square
      DO WHILE ( ( J .LE. NM ) .AND.
     :           ( MPS( 2, J - 1 ) .GE. SQUARE( 2, 3 ) ) )

         SIDE11 = SIDE( MPS( 1, J ), SQUARE( 1, 1 ), SQUARE( 1, 4 ) )
         SIDE22 = SIDE( MPS( 1, J ), SQUARE( 1, 2 ), SQUARE( 1, 3 ) )

*   Note have to set tl and tr to be set false every time the vertices are
*   on the same side, otherwise their values will be carried over and may
*   incorrectly pass the test
         IF ( SIDE1 * SIDE11 .LT. 0.0 ) THEN
            CALL VINSEC( SQUARE( 1, 1 ) , SQUARE( 1, 4 ),
     :                   MPS( 1, J - 1 ), MPS( 1, J ), TL, CROSSL )
         ELSE
            TL = .FALSE.
         ENDIF

         IF ( SIDE2 * SIDE22 .LT. 0.0 ) THEN
            CALL VINSEC( SQUARE( 1, 2 ) , SQUARE( 1, 3 ),
     :                   MPS( 1, J - 1 ), MPS( 1, J ), TR, CROSSR )
         ELSE
            TR = .FALSE.
         ENDIF

*   If the edge of the polygon passes through both sides of the square
*   then have to ensure that they are taken in the correct order.
         IF ( TL .AND. TR ) THEN
            NY = NY + 2
            IF ( CROSSL( 2 ) .GT. CROSSR( 2 ) ) THEN
               YLIST( NY - 1 ) = CROSSL( 2 )
               YLIST( NY ) = CROSSR( 2 )
            ELSE
               YLIST( NY - 1 ) = CROSSR( 2 )
               YLIST( NY ) = CROSSL( 2 )
            ENDIF
         ELSEIF ( TL ) THEN
            NY = NY + 1
            YLIST( NY ) = CROSSL( 2 )
         ELSEIF ( TR ) THEN
            NY = NY + 1
            YLIST( NY ) = CROSSR( 2 )
         ENDIF

*   Replace the old side indicators with the new ones
         SIDE1 = SIDE11
         SIDE2 = SIDE22

*   If the vertex is within limits then include it
         IF ( ( MPS( 2, J ) .LE. SQUARE( 2, 1 ) ) .AND.
     :        ( MPS( 2, J ) .GE. SQUARE( 2, 3 ) ) ) THEN
            NY = NY + 1
            YLIST( NY ) = MPS( 2, J )
         ENDIF

*   Increment the counter j
         J = J + 1

      ENDDO

*   Check if bottom of square is to be included
      IF ( ( SQUARE( 2, 4 ) .LT. MPS( 2, 1 ) ) .AND.
     :     ( SQUARE( 2, 4 ) .GT. MPS( 2, NM ) ) ) THEN
         NY = NY + 1
         YLIST( NY ) = SQUARE( 2, 4 )
      ENDIF

      END

* $Id$
