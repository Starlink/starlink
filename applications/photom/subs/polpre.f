************************************************************************

      SUBROUTINE POLPRE ( NP, POLY, NL, L, NR, R, XMIN, XMAX,
     :                    YMIN, YMAX )

*+
*  Name :
*     POLPRE
*
*  Purpose :
*     This takes the polygon vertices stored in poly and preprocesses them
*     into two montone polygonal sectors ( MPS ).
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL POLPRE( NP, POLY, NL, L, NR, R, XMIN, XMAX, YMIN, YMAX )
*
*  Description :
*     This takes the polygon vertices stored in poly and preprocesses them
*     into two montone polygonal sectors ( MPS ) giving the left and right
*     handed chains. The two MPS chains are separated by the highest and
*     lowest y values. The polygon array is also sorted to give the vertices
*     in descending order from the highest point.
*     The right hand MPS is anticlockwise from the highest vertex.
*     The left hand MPS is clockwise from the highest vertex.
*
*  Arguments :
*     NP = INTEGER (Given)
*        Number of vertices in the polygon
*     POLY( 2, NP ) = REAL (Given and Returned)
*        Array of polygon vertices
*     NL = INTEGER (Returned)
*        Number of vertices in left hand MPS
*     L( 2, NP ) = REAL (Returned)
*        Array of vertices in left hand MPS
*     NR = INTEGER (Returned)
*        Number of vertices in right hand MPS
*     R( 2, NP ) = REAL (Returned)
*        Array of vertices in right hand MPS
*     XMIN = REAL (Returned)
*        Lowest x value among vertices
*     XMAX = REAL (Returned)
*        Highest x value among vertices
*     YMIN = REAL (Returned)
*        Lowest y value among vertices
*     YMAX = REAL (Returned)
*        Highest y value among vertices
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
*     12-MAR-1992 (NE):
*        Add ORDER argument to SHSORT
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Arguments Given :
      INTEGER NP

*  Arguments Given and Returned :
      REAL POLY( 2, NP )

*  Arguments Returned :
      INTEGER NL
      REAL L( 2, NP )
      INTEGER NR
      REAL R( 2, NP )
      REAL XMIN
      REAL XMAX
      REAL YMIN
      REAL YMAX

*  Local Variables :
      INTEGER I, J

      REAL INFNIT, SIDE, WORK( 2 )
*.

*   Do some initialisation
      INFNIT = 1.2E35

*   Sort the polygon into decreasing y value
      CALL SHSORT( 'D', 2, NP, 2, WORK, POLY )

*   Split the polygon into the two MPS. Put the first element into both.
      DO I = 1, 2
         L( I, 1 ) = POLY( I, 1 )
         R( I, 1 ) = POLY( I, 1 )
      ENDDO
      NL = 1
      NR = 1

*   Put the vertices into the left or right chain depending on which side
*   of the line, joining the lowest and highest points, they lie. Note
*   that there is a change of dimensionality in passing to function SIDE
      DO J = 2, NP - 1
         IF ( SIDE( POLY( 1, J ), POLY( 1, 1 ), POLY( 1, NP ) )
     :        .GT. 0.0 ) THEN
            NL = NL + 1
            DO I = 1, 2
               L( I, NL ) = POLY( I, J )
            ENDDO

         ELSE
            NR = NR + 1
            DO I = 1, 2
               R( I, NR ) = POLY( I, J )
            ENDDO
         ENDIF
      ENDDO

*   Put the last element into both chains
      NL = NL + 1
      NR = NR + 1
      DO I = 1, 2
         L( I, NL ) = POLY( I, NP )
         R( I, NR ) = POLY( I, NP )
      ENDDO

*   Find the minimum and maximum positions among the vertices for
*   each axis. The minimum x value must be in the right hand MPS and
*   the maximum x value must be in the left hand MPS. The minimum and
*   maximum y values are found in the sorted poly array.
      XMIN = INFNIT
      DO J = 1, NR
         XMIN = MIN( XMIN, R( 1, J ) )
      ENDDO

      XMAX = -INFNIT
      DO J = 1, NL
         XMAX = MAX( XMAX, L( 1, J ) )
      ENDDO

      YMIN = POLY( 2, NP )
      YMAX = POLY( 2, 1 )

      END

* $Id$
