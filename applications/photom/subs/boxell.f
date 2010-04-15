************************************************************************

      SUBROUTINE BOXELL ( NE, ELLIPS, NXL, NXH, NYL, NYH, NX, NY, SENSE,
     :                    GRID, GS, AREA, CUTOFF, L, R, YLIST, LYLIST,
     :                    RYLIST, INSL, INSR, POLY )

*+
*  Name :
*     BOXELL
*
*  Purpose :
*     This calculates the area of each cell on a rectangular grid
*     intersected by the ellipse.
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL BOXELL( NE, ELLIPS, NXL, NXH, NYL, NYH, NX, NY, SENSE, GRID,
*    :             GS, AREA, CUTOFF, L, R, YLIST, LYLIST, RYLIST, INSL,
*    :             INSR, POLY )
*
*  Description :
*     This calculates the area of each cell on a rectangular grid
*     intersected by the ellipse.
*
*  Arguments :
*     NE = INTEGER (Given)
*        Number of vertices of ellipse
*     ELLIPS( 2, NE ) = REAL (Given)
*        Array of vertices of ellipse
*     NXL = INTEGER (Given)
*        Lower x bound of useful area of grid array
*     NXH = INTEGER (Given)
*        Upper x bound of useful area of grid array
*     NYL = INTEGER (Given)
*        Lower y bound of useful area of grid array
*     NYH = INTEGER (Given)
*        Upper y bound of useful area of grid array
*     NX = INTEGER (Given)
*        X dimension of image array
*     NY = INTEGER (Given)
*        Y dimension of image array
*     SENSE = REAL (Given)
*        Sign of summation, positive or negative
*     GS = INTEGER (Given)
*        Size of grid array
*     L( 2, NE ) = REAL (Given)
*        Work array of vertices in left-hand MPS
*     R( 2, NE ) = REAL (Given)
*        Work array of vertices in right-hand MPS
*     YLIST( NE + 6 ) = REAL (Given)
*        Work space array for Y-sorted list of intersection vertices
*     LYLIST( NE + 4 ) = REAL (Given)
*        Work space array for left-hand Y-list of intersection vertices
*     RYLIST( NE + 4 ) = REAL (Given)
*        Work space array for right-hand Y-list of intersection vertices
*     INSL( 2, NE + 4 ) = REAL (Given)
*        Work space array for left-hand MPS of intersection vertices
*     INSR( 2, NE + 4 ) = REAL (Given)
*        Work space array for rigth-hand MPS of intersection vertices
*     POLY( 2, 2 * NE + 8 ) = REAL (Given)
*        Work space array for intersection polygon
*     GRID( GS, GS ) = REAL (Returned)
*        Array of cells containing areas cut by ellipse for each element
*     AREA = REAL (Returned)
*        Total area cut by ellipse
*     CUTOFF = LOGICAL (Returned)
*        Is the ellipse cut by the edge of the array
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
*     10-AUG-1987 (NE):
*        Original version.
*     10-OCT-1987 (NE):
*        Check for the active area lying outside the grid
*     10-JAN-1988 (NE):
*        Added sense to determine direction of summation
*     10-JAN-1988 (NE):
*        The grid elements are no longer set to zero
*        It is assumed the grid array is zeroed outside
*     10-MAR-1988 (NE):
*        Added check for cutoff of ellipse by edge of array
*     10-JAN-1992 (NE):
*        Limit size of grid array to a square of size GS
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Arguments Given :
      INTEGER NE
      REAL ELLIPS( 2, NE )
      INTEGER NXL
      INTEGER NXH
      INTEGER NYL
      INTEGER NYH
      INTEGER NX
      INTEGER NY
      REAL SENSE
      INTEGER GS
      REAL L( 2, NE )
      REAL R( 2, NE )
      REAL YLIST( NE + 6 )
      REAL LYLIST( NE + 4 )
      REAL RYLIST( NE + 4 )
      REAL INSL( 2, NE + 4 )
      REAL INSR( 2, NE + 4 )
      REAL POLY( 2, 2 * NE + 8 )

*  Arguments Returned :
      REAL GRID( GS, GS )
      REAL AREA
      LOGICAL CUTOFF

*  External References:
      EXTERNAL POLYIN
      LOGICAL POLYIN
      EXTERNAL POLARE
      REAL POLARE

*  Local Variables :
      LOGICAL L1, L2, L3, L4

      INTEGER I, II, J, JJ, NL, NP, NR

      REAL GSIGN, SQUARE( 2, 4 ), TAREA, XMAX, XMIN, X1, X2,
     :     YMAX, YMIN, Y1, Y2
*.

*   Transfer the sign of the summation to a multiplying factor of 1
      GSIGN = SIGN( 1.0, SENSE )

*   Pre-process the ellipse into its two monotone polygonal sectors ( MPS )
      NL = NE + 2
      NR = NE + 2
      CALL POLPRE( NE, ELLIPS, NL, L, NR, R, XMIN, XMAX, YMIN, YMAX )

*   Check if ellipse is cut by the edge of the array
      CUTOFF = .FALSE.
      IF ( ( XMIN .LT. 0.0 ) .OR. ( XMAX .GT. REAL( NX ) ) .OR.
     :     ( YMIN .LT. 0.0 ) .OR. ( YMAX .GT. REAL( NY ) ) ) THEN
         CUTOFF = .TRUE.
      ENDIF

*   Initialise the area to zero
      AREA = 0.0

*   Step through the active area, making sure the element lies within the
*   bounds of the grid
*   Remember that the centre of a pixel is at i.5, j.5
      DO J = NYL, NYH

*   Calculate the index into the grid array
         JJ = J - NYL + 1
         IF ( ( J .GE. 1 ) .AND. ( J .LE. NY ) ) THEN
            Y1 = REAL( J ) - 1.0
            Y2 = REAL( J )

*   Check that this square intersects the y limits of the ellipse
            IF ( ( Y2 .GT. YMIN ) .AND. ( Y1 .LT. YMAX ) ) THEN

*   Step through the x loop, making sure the element is in bounds
*   Remember that the centre of a pixel is at i.5, j.5
               DO I = NXL, NXH

*   Calculate the index into the grid array
                  II = I - NXL + 1
                  IF ( ( I .GE. 1 ) .AND. ( I .LE. NX ) ) THEN
                     X1 = REAL( I ) - 1.0
                     X2 = REAL( I )

*   Check that this square intersects the x limits of the ellipse
                     IF ( ( X2 .GT. XMIN ) .AND. ( X1 .LT. XMAX ) ) THEN

*   Construct the polygon representing the square element of the grid
                        SQUARE( 1, 1 ) = X2
                        SQUARE( 2, 1 ) = Y2
                        SQUARE( 1, 2 ) = X1
                        SQUARE( 2, 2 ) = Y2
                        SQUARE( 1, 3 ) = X1
                        SQUARE( 2, 3 ) = Y1
                        SQUARE( 1, 4 ) = X2
                        SQUARE( 2, 4 ) = Y1

*   Find which of the vertices of the square are inside the ellipse
                        L1 = POLYIN( SQUARE( 1, 1 ), NL, L, NR, R )
                        L2 = POLYIN( SQUARE( 1, 2 ), NL, L, NR, R )
                        L3 = POLYIN( SQUARE( 1, 3 ), NL, L, NR, R )
                        L4 = POLYIN( SQUARE( 1, 4 ), NL, L, NR, R )

*   If all the vertices are inside then the intersected area is one
                        IF ( L1 .AND. L2 .AND. L3 .AND. L4 ) THEN
                           GRID( II, JJ ) = GRID( II, JJ ) + 1.0 * GSIGN
                           AREA = AREA + 1.0

*   If one or more ( but not all ) vertices of the square are inside
*   the ellipse then have to do a more careful calculation. This is done
*   by calculating the intersection of the square and the ellipse.
*   There can also be the case where none of the squares vertices are
*   within the ellipse but the ellipse still intersects the square.
                        ELSE
                           NP = NE
                           CALL POLINS( NL, L, NR, R, 4, SQUARE, YLIST,
     :                                  LYLIST, RYLIST, NP, INSL, INSR )

*   There has to be at least one point in each intersection MPS
                           IF ( NP .GE. 2 ) THEN
                              TAREA = POLARE( NP, INSL, NP, INSR,
     :                                        2 * NP, POLY )
                              GRID( II, JJ ) = GRID( II, JJ ) + TAREA *
     :                                         GSIGN
                              AREA = AREA + TAREA
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF

*   End of I loop
               ENDDO
            ENDIF
         ENDIF

*   End of J loop
      ENDDO

      END

* $Id$
