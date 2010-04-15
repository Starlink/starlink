************************************************************************

      REAL FUNCTION POLARE ( NL, L, NR, R, NP, POLY )

*+
*  Name :
*     POLARE
*
*  Purpose :
*     Calculates the area of the polygon given by the two MPS.
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     POLARE( NL, L, NR, R, NP, POLY )
*
*  Description :
*     Calculates the area of the polygon given by the two MPS.
*     NOTE. The validity of the MPS arrays are assumed, and not checked.
*
*  Arguments :
*     NL = INTEGER (Given)
*        Number of points in left MPS
*     L( 2, NL ) = REAL (Given)
*        List of points in left MPS
*     NR = INTEGER (Given)
*        Number of points in right MPS
*     R( 2, NR ) = REAL (Given)
*        List of points in right MPS
*     NP = INTEGER (Given)
*        Number of points in work space array
*     POLY( 2, NP ) = REAL (Given)
*        Work space array to construct polygon
*     POLARE = REAL (Returned)
*        Area of polygon
*
*  Algorithm :
*     {algorithm_description}...
*
*  Deficiencies :
*     {routine_deficiencies}...
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*     PWD: Peter W. Draper (Starlink, Durham University)
*     {enter_new_authors_here}
*
*  History :
*     10-JAN-1988 (NE):
*        Original version.
*     6-NOV-1996 (PWD):
*        Stopped modification of NP variable (supposed to be given).
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Arguments Given :
      INTEGER NL
      REAL L( 2, NL )
      INTEGER NR
      REAL R( 2, NR )
      INTEGER NP
      REAL POLY( 2, NP )

*  Local Variables :
      INTEGER I, J, JJ, IC

      REAL TRAREA
*.

*   Do some initialisation
      POLARE = 0.0

*   Verify input
      IF ( NP .GE. NL + NR ) THEN

*   Make an anti-clockwise polygon from the two MPS

         IC = 0
         DO J = 1, NR
            IC = IC + 1
            DO I = 1, 2
               POLY( I, IC ) = R( I, J )
            ENDDO
         ENDDO

*   Have to reverse order of left MPS
         DO J = 1, NL
            JJ = NL + 1 - J
            IC = IC + 1
            DO I = 1, 2
               POLY( I, IC ) = L( I, JJ )
            ENDDO
         ENDDO

*   Calculate area of polygon from triangles using point 1 as an anchor.
         DO J = 3, IC
            POLARE = POLARE + TRAREA( POLY( 1, 1 ), POLY( 1, J - 1 ),
     :                                POLY( 1, J ) )
         ENDDO

      ENDIF

      END

* $Id$
