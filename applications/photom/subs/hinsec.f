************************************************************************

      SUBROUTINE HINSEC ( H1, H2, P1, P2, INSECT, CROSS )

*+
*  Name :
*     HINSEC
*
*  Purpose :
*     This calculates whether two lines defined by their endpoints cross.
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL HINSEC( H1, H2, P1, P2, INSECT, CROSS )
*
*  Description :
*     This calculates whether two lines defined by their endpoints cross.
*     The first line has to be horizontal, the second line can be at any
*     angle. The point of intersection of the two lines extended to infinity
*     is returned even if two line segments do not actually intersect.
*
*  Arguments :
*     H1( 2 ) = REAL (Given)
*        X and y coordinate of point 1 of horizontal line
*     H2( 2 ) = REAL (Given)
*        X and y coordinate of point 2 of horizontal line
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
*     10-JUN-1988 (NE):
*        Changed to a subroutine call
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Arguments Given :
      REAL H1( 2 )
      REAL H2( 2 )
      REAL P1( 2 )
      REAL P2( 2 )

*  Arguments Returned :
      LOGICAL INSECT
      REAL CROSS( 2 )

*  Local Variables :
      LOGICAL INSIDE
      REAL DELY, INFNIT, SFA
*.

*   Do some initialisation
      INFNIT = 1.2E35
      SFA = 2.0E-6

*   Check that the first line is horizontal. If not return a funny number.
      IF ( ABS( H1( 2 ) - H2( 2 ) ) .GT. SFA ) THEN
         INSECT = .FALSE.
         CROSS( 1 ) = INFNIT
         CROSS( 2 ) = INFNIT

      ELSE

*   If the second line is horizontal then there is no intersection.
         DELY = P2( 2 ) - P1( 2 )
         IF ( ABS( DELY ) .LT. SFA ) THEN
            CROSS( 1 ) = INFNIT

         ELSE
*   Calculate x value of intersection point
            CROSS( 1 ) = P1( 1 ) + ( P2( 1 ) - P1( 1 ) ) *
     :                             ( H1( 2 ) - P1( 2 ) ) / DELY
         ENDIF

*   The y value of the intersection is just the y value of the horizontal
         CROSS( 2 ) = H1( 2 )

*   Check if the intersection occurs between the lines endpoints
         INSECT = ( INSIDE( CROSS, H1, H2 ) .AND.
     :              INSIDE( CROSS, P1, P2 ) )

      ENDIF

      END

* $Id$
