************************************************************************

      SUBROUTINE POLINS ( NL, L, NR, R, NS, SQUARE, YLIST, LYLIST,
     :                    RYLIST, NP, INSL, INSR )

*+
*  Name :
*     POLINS
*
*  Purpose :
*     This computes the intersection of a polygon and a square using the
*     plane sweep technique.
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL POLINS ( NL, L, NR, R, NS, SQUARE, YLIST, LYLIST,
*    :              RYLIST, NP, INSL, INSR )
*
*  Description :
*     This computes the intersection of a polygon and a square using the
*     plane sweep technique. The polygon is defined by its two monotone
*     polygonal sectors ( MPS ) l and r which list the vertices between the
*     highest and lowest points of the poygon.
*     The intersection is given as a resultant list of vertices in two MPS
*     insl and insr of the same length.
*     The vertices of the square must be given according to the following
*     plan  2 - 1
*           |   |
*           3 - 4  i.e. anti-clockwise from top right.
*
*     NOTE. The validity of the MPS arrays are assumed, and not checked.
*
*  Arguments :
*     NL = INTEGER (Given)
*        Number of vertices in left hand MPS
*     L( 2, NL ) = REAL (Given)
*        Array of vertices in left hand MPS
*     NR = INTEGER (Given)
*        Number of vertices in right hand MPS
*     R( 2, NL ) = REAL (Given)
*        Array of vertices in right hand MPS
*     NS = INTEGER (Given)
*        Number of vertices in square ( should be 4 )
*     SQUARE( 2, NS ) = REAL (Given)
*        Array of vertices in square
*     YLIST( NP + 6 ) = REAL (Given)
*        Work space array
*     LYLIST( NP + 4 ) = REAL (Given)
*        Work space array
*     RYLIST( NP + 4 ) = REAL (Given)
*        Work space array
*     NP = INTEGER (Given and Returned)
*        Size of array of insl, insr
*        Number of vertices in intersection polygon
*     INSL( 2, NP + 4 ) = REAL (Returned)
*        Array of vertices in left MPS of intersection
*     INSR( 2, NP + 4 ) = REAL (Returned)
*        Array of vertices in right MPS of intersection
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
*     31-JUL-1987 (NE):
*        Original version.
*     10-JAN-1988 (NE):
*        Used calls to HINSEC rather than INSECT.
*     29-APR-1996 (PWD):
*        Added PLL & PRR variables to trap PL-1 & PR-1 array indices
*        being 0 on occasion.
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Arguments Given and Returned :
      INTEGER NP

*  Arguments Given :
      INTEGER NL
      REAL L( 2, NL )
      INTEGER NR
      REAL R( 2, NR )
      INTEGER NS
      REAL SQUARE( 2, NS )
      REAL YLIST( NP + 6 )
      REAL LYLIST( NP + 4 )
      REAL RYLIST( NP + 4 )

*  Arguments Returned :
      REAL INSL( 2, NP + 4 )
      REAL INSR( 2, NP + 4 )

*  Local Variables :
      INTEGER I, INP, J, LNY, NY, PL, PR, RNY, PLL, PRR

      REAL CROSSL( 2 ), CROSSR( 2 ), INFNIT, OLDY, SFA,
     :     TEST1( 2 ), TEST2( 2 ), YTEST

      LOGICAL TL, TR
*.

*   Do some initialisations
      INFNIT = 1.2E35
      INP = 0

*   Find the first vertex of each MPS below the level of the top of square
      PL = 1
      PR = 1
      DO WHILE ( ( L( 2, PL ) .GT. SQUARE( 2, 1 ) ) .AND.
     :           ( PL .LE. NL ) )
         PL = PL + 1
      ENDDO
      DO WHILE ( ( R( 2, PR ) .GT. SQUARE( 2, 1 ) ) .AND.
     :           ( PR .LE. NR ) )
         PR = PR + 1
      ENDDO

*   Obtain the list of vertices within the y range of the square.
*   The top and bottom of the square are included as the first and last
*   elements of the list only if the y range of the polygon crosses
*   them, i.e. if the polygon begins ( or ends ) in the range of the square
*   the list starts ( terminates ) with the first ( last ) vertex of the
*   polygon.

      LNY = NL
      RNY = NR
      CALL YSWEEP( NL, L, NS, SQUARE, PL, LNY, LYLIST )
      CALL YSWEEP( NR, R, NS, SQUARE, PR, RNY, RYLIST )
      NY = LNY + RNY
      CALL MERGE( 1, 1, LNY, LYLIST, 1, RNY, RYLIST, NY, YLIST )

      TEST1( 1 ) = INFNIT
      TEST2( 1 ) = -INFNIT

*   Use one of the vertices of square to define the accuracy to which the
*   test for overlap will be conducted. This is done to keep the accuracy
*   approximately within the seventh significant digit without a priori
*   knowing the possible values used in the test.
       SFA = MAX( ABS( SQUARE( 1, 1 ) ) * 2.0E-6,
     :            ABS( SQUARE( 1, 2 ) ) * 2.0E-6 )

*   Test for the intersection of the polygon and square using a
*   horizontal cut. The y-value of the cut is taken from the list of
*   vertices ( from both the left and right MPS ) in order of decreasing y
*   The top and bottom of the square is also used. This is the plane sweep
*   Some of the values in ylist may have the same value. These only have
*   to be tested once so skip over repeated values.
      OLDY = INFNIT

      DO J = 1, NY
         YTEST = YLIST( J )

         IF ( YTEST .NE. OLDY ) THEN

*   Make sure that the correct vertices are being used
            DO WHILE ( L( 2, PL ) .GT. YTEST )
               PL = PL + 1
               IF ( PL .GT. NL ) GOTO 20
            ENDDO
            DO WHILE ( R( 2, PR ) .GT. YTEST )
               PR = PR + 1
               IF ( PR .GT. NR ) GOTO 20
            ENDDO

*   Establish intersection of the horizontal line y=ytest with polygon
            TEST1( 2 ) = YTEST
            TEST2( 2 ) = YTEST

            PLL = MAX( 1, PL - 1 )
            CALL HINSEC( TEST1, TEST2, L( 1, PLL ), L( 1, PL ),
     :                   TL, CROSSL )
            PRR = MAX( 1, PR - 1 )
            CALL HINSEC( TEST1, TEST2, R( 1, PRR ), R( 1, PR ),
     :                   TR, CROSSR )

*   There are three cases to consider : 1 - square is outside polygon,
*   2 - square is inside polygon, 3 - square overlaps polygon.
*   Case 1 implies no intersection so do nothing.
*   The accuracy of the test matters here because nothing is done if
*   the edges of the square are outside the polygon. It is unimportant
*   for later tests because all cases are considered.
            IF ( ( SQUARE( 1, 1 ) .LT. CROSSR( 1 ) - SFA ) .OR.
     :           ( SQUARE( 1, 2 ) .GT. CROSSL( 1 ) + SFA ) ) THEN

            ELSE

*   Check if square is totally inside polygon.
               IF ( ( SQUARE( 1, 2 ) .GT. CROSSR( 1 ) ) .AND.
     :              ( SQUARE( 1, 1 ) .LT. CROSSL( 1 ) ) ) THEN
                  INP = INP + 1
                  INSL( 1, INP ) = SQUARE( 1, 1 )
                  INSL( 2, INP ) = YTEST
                  INSR( 1, INP ) = SQUARE( 1, 2 )
                  INSR( 2, INP ) = YTEST

               ELSE

*   They must overlap somehow.
                  INP = INP + 1
                  IF ( SQUARE( 1, 1 ) .GT. CROSSL( 1 ) ) THEN
                     DO I = 1, 2
                        INSL( I, INP ) = CROSSL( I )
                     ENDDO
                  ELSE
                     INSL( 1, INP ) = SQUARE( 1, 1 )
                     INSL( 2, INP ) = YTEST
                  ENDIF

                  IF ( SQUARE( 1, 2 ) .LT. CROSSR( 1 ) ) THEN
                     DO I = 1, 2
                        INSR( I, INP ) = CROSSR( I )
                     ENDDO
                  ELSE
                     INSR( 1, INP ) = SQUARE( 1, 2 )
                     INSR( 2, INP ) = YTEST
                  ENDIF
               ENDIF
            ENDIF

            OLDY = YTEST
         ENDIF

      ENDDO

  20  CONTINUE

      NP = INP

      END

* $Id$
