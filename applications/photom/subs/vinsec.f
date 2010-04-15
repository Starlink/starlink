************************************************************************

      SUBROUTINE VINSEC ( V1, V2, P1, P2, INSECT, CROSS )

*+
*  Name :
*     VINSEC
*
*  Purpose :
*     This calculates whether two lines defined by their endpoints cross.
*     The first line has to be vertical,
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL VINSEC( V1, V2, P1, P2, INSECT, CROSS )
*
*  Description :
*     This calculates whether two lines defined by their endpoints cross.
*     The first line has to be vertical, the second line can be at any
*     angle. The point of intersection of the two lines extended to infinity
*     is returned even if two line segments do not actually intersect.
*
*  Arguments :
*     V1( 2 ) = REAL (Given)
*        X and y coordinate of point 1 of verticalline
*     V2( 2 ) = REAL (Given)
*        X and y coordinate of point 2 of vertical line
*     P1( 2 ) = REAL (Given)
*        X and y coordinate of point 1 of line
*     P2( 2 ) = REAL (Given)
*        X and y coordinate of point 2 of line
*     INSECT = LOGICAL (Returned)
*        Do the lines intersect
*     CROSS( 2 ) = REAL (Returned)
*        X and y coordinates of point of intersection
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
      REAL V1( 2 )
      REAL V2( 2 )
      REAL P1( 2 )
      REAL P2( 2 )

*  Arguments Returned :
      LOGICAL INSECT
      REAL CROSS( 2 )

*  Local Variables :
      LOGICAL INSIDE
      REAL DELX, INFNIT, SFA
*.

*   Do some initialisation
      INFNIT = 1.2E35
      SFA = 2.0E-6

*   Check that the first line is vertical. If not return a funny number.
      IF ( ABS( V1( 1 ) - V2( 1 ) ) .GT. SFA ) THEN
         INSECT = .FALSE.
         CROSS( 1 ) = INFNIT
         CROSS( 2 ) = INFNIT

      ELSE

*   If the second line is vertical then there is no intersection.
         DELX = P2( 1 ) - P1( 1 )
         IF ( ABS( DELX ) .LT. SFA ) THEN
            CROSS( 2 ) = INFNIT

         ELSE
*   Calculate y value of intersection point
            CROSS( 2 ) = P1( 2 ) + ( P2( 2 ) - P1( 2 ) ) *
     :                             ( V1( 1 ) - P1( 1 ) ) / DELX
         ENDIF

*   The x value of the intersection is just the x value of the vertical
         CROSS( 1 ) = V1( 1 )

*   Check if the intersection occurs between the lines endpoints
         INSECT = ( INSIDE( CROSS, V1, V2 ) .AND.
     :              INSIDE( CROSS, P1, P2 ) )

      ENDIF

      END

* $Id$
