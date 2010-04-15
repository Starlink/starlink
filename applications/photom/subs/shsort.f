************************************************************************

      SUBROUTINE SHSORT ( ORDER, NX, NY, KEY, WORK, DATA )

*+
*  Name :
*     SHSORT
*
*  Purpose :
*     Performs a shell sort on a 2-d array using the key to signify which
*     column to be used for the sort.
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL SHSORT( ORDER, NX, NY, KEY, WORK, DATA )
*
*  Description :
*     Performs a shell sort on a 2-d array using the key to signify which
*     column to be used for the sort. All the elements in each row are
*     moved during the sort. Shell sort is like insertion sort except the
*     elements are moved by an amount h which is set to be part of the
*     descending sequence  ...1093, 364, 121, 40, 13, 4, 1
*     The array is sorted into either ascending or descending order
*     according to the value of ORDER.
*
*  Arguments :
*     ORDER = CHARACTER (Given)
*        Order of sort A = ascending or D = descending. Default is D.
*     NX = INTEGER (Given)
*        First dimension of data array
*     NY = INTEGER (Given)
*        Second dimension of data array
*     KEY = INTEGER (Given)
*        Column to be used as key for sort
*     WORK( NX ) = REAL (Given)
*        Work space of dimension nx
*     DATA( NX, NY ) = REAL (Given and Returned)
*        Data array to sort by rows
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
*        Allow for ascending or descending sort.
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Arguments Given :
      CHARACTER ORDER
      INTEGER NX
      INTEGER NY
      INTEGER KEY
      REAL WORK( NX )

*  Arguments Given and Returned :
      REAL DATA( NX, NY )

*  Local Variables :
      INTEGER H, I, J, JJ
*.

*   Calculate the starting value of the step sequence
      H = 1
      DO WHILE ( H .LE. NY )
         H = 3 * H + 1
      ENDDO

*   Repeat the main loop for ascending or descending order
      IF ( ( ORDER .EQ. 'a' ) .OR. ( ORDER .EQ. 'A' ) ) THEN

*   Main loop for ascending order
         DO WHILE ( H .GT. 1 )
            H = H / 3

            DO JJ = H + 1, NY
               J = JJ

*   Store j'th value of data array in temporary location
               DO I = 1, NX
                  WORK( I ) = DATA( I, J )
               ENDDO

*   Compare ( and move if necessary ) the current element with previous
*   elements at positions differing from the current one by h.
               DO WHILE ( DATA( KEY, J - H ) .GT. WORK( KEY ) )
                  DO I = 1, NX
                     DATA( I, J ) = DATA( I, J - H )
                  ENDDO
                  J = J - H
                  IF ( J .LE. H ) GOTO 10
               ENDDO

  10           CONTINUE

*  Replace stored values into new position j
               DO I = 1, NX
                  DATA( I, J ) = WORK( I )
               ENDDO

            ENDDO
         ENDDO

*   Main loop for descending order
      ELSE
         DO WHILE ( H .GT. 1 )
            H = H / 3

            DO JJ = H + 1, NY
               J = JJ

*   Store j'th value of data array in temporary location
               DO I = 1, NX
                  WORK( I ) = DATA( I, J )
               ENDDO

*   Compare ( and move if necessary ) the current element with previous
*   elements at positions differing from the current one by h.
               DO WHILE ( DATA( KEY, J - H ) .LT. WORK( KEY ) )
                  DO I = 1, NX
                     DATA( I, J ) = DATA( I, J - H )
                  ENDDO
                  J = J - H
                  IF ( J .LE. H ) GOTO 20
               ENDDO

  20           CONTINUE

*  Replace stored values into new position j
               DO I = 1, NX
                  DATA( I, J ) = WORK( I )
               ENDDO

            ENDDO
         ENDDO
      ENDIF

      END

* $Id$
