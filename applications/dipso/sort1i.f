      SUBROUTINE SORT1I( SIZE, ARRAY )
*+
*  Name:
*     SORT1I

*  Purpose:
*     Sort a vector of integers into ascending order.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SORT1I( SIZE, ARRAY )

*  Description:
*     A simple bubble sort method is used.

*  Arguments:
*     SIZE = INTEGER (Given)
*        The size of the array.
*     ARRAY( SIZE ) = INTEGER (Given and Returned)
*        The array of values.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-AUG-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER SIZE

*  Arguments Given and Returned:
      INTEGER ARRAY( SIZE )

*  Local Variables:
      INTEGER
     :        HI,		 ! Index of the higher element of a pair
     :        LAST,              ! Index of the last unsorted element.
     :        LO,       	 ! Index of the lower element of a pair
     :        TEMP               ! Temporary value.

      LOGICAL
     :        DONE               ! Have all elements been sorted?
*.


*  Initialisation.
      DONE = .FALSE.
      LAST = SIZE

*  Loop round until the array has been sorted.
      DO WHILE( .NOT. DONE )

*  Set a flag to indicate that no pairs of adjacent values have yet been
*  found which are in the wrong order.
         DONE = .TRUE.

*  Loop round each pair of adjacent values in the array. The elements
*  with indices higher than LAST will have been sorted by previous passes
*  through this loop and so don't need to be re-sorted.
         DO HI = 2, LAST
            LO = HI - 1

*  If the lower element has a greater value than the higher element, swap
*  them, and set a flag to indicate that at least one pair of adjacent
*  values have been found out of order.
            TEMP = ARRAY( LO )
            IF( TEMP .GT. ARRAY( HI ) ) THEN
               ARRAY( LO ) = ARRAY( HI )
               ARRAY( HI ) = TEMP
               DONE = .FALSE.
            END IF

         END DO

*  The largest value will have "bubbled" its way to the top of the array.
*  Therefore we can leave the top value out of the next pass through the
*  loop.
         LAST = LAST - 1

      END DO

      END
