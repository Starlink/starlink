      SUBROUTINE PDA_RINPI( PERM, N, X, IFAIL )
*+
*  Name:
*     PDA_RINPI

*  Purpose:
*     Reorder an array inplace using a permutation index.

*  Language:
*     Fortran 77.

*  Invocation:
*     CALL PDA_SIPLI( PERM, N, X, IFAIL )

*  Description:
*     This routine reorders an array (in place) using an permutation 
*     vector. This is most likely the output from one of the sorting 
*     routines PDA_QSI[A|D]<T>.

*  Arguments:
*     PERM( N ) = INTEGER (Given)
*        The index vector. Note this is modified but should be returned
*        in the same state as when input. Indices may not be negative.
*     N = INTEGER (Given)
*        Number of elements.
*     X( N ) = INTEGER (Given and Returned)
*        The array to reorder.
*     IFAIL = INTEGER (Returned)
*        Status flag. Set 0 for success, otherwise the permutation isn't
*        correct.

*  Notes: 
*     - Re-ordering is trivial if two arrays are available.
*          DO I = 1, N
*             XX( I ) = X( PERM( I ) )
*          END DO
*
*       The XX array contains the sorted values on completion.

*  Timing:
*      Proportional to N.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     31-AUG-1996 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Arguments Given:
      INTEGER N
      INTEGER PERM( N )
      
*  Arguments Given and Returned:
      INTEGER X( N )
      INTEGER IFAIL
      
*  Local Variables:
      INTEGER I                 ! Loop variable
      INTEGER IAT               ! Current index
      INTEGER IAT0              ! Previous index
      INTEGER XTEMP             ! Value from start of cycle
*.

*  Check that PERM is a valid permutation
      IFAIL = 0
      DO 1 I = 1, N
         IAT = ABS( PERM( I ) )

*  Make sure index is within the range of the array to be reordered.
         IF ( ( IAT .GE. 1 ) .AND. ( IAT .LE. N ) ) THEN

*  And that we can follow the permutation, without encountering a
*  negative value. Which indicates that we've already been there and
*  hence the permutation has a sub-cycle.
            IF( PERM( IAT ) .GT. 0 ) THEN
               PERM( IAT ) = -PERM( IAT )
            ELSE 
               IFAIL = 1
            END IF
         ELSE 
            IFAIL = 1
         END IF
         IF ( IFAIL .NE. 0 ) GO TO 99
 1    CONTINUE


*  Now rearrange the values. All the permutation values are negative at
*  this point and serve as an indicator of which values have been moved
*  (these become the positive ones).
      DO 2 I = 1, N

*  Do not process values that have been moved into already.
         IF ( PERM( I ) .LT. 0 ) THEN 

*  Remember this position keep a copy of its value.
            IAT = I
            IAT0 = IAT
            XTEMP = X( I )

*  Loop while we have a permutation index that is negative, overwriting
*  values until we hit an index we've already done. 
 3          CONTINUE
            IF ( PERM( IAT ) .LT. 0 ) THEN 
               X( IAT ) = X( -PERM( IAT ) )
               IAT0 = IAT
               PERM( IAT ) = -PERM( IAT )
               IAT = PERM( IAT )
               GO TO 3
            END IF

*  Back to start of cycle. Overwrite with initial value.
            X( IAT0 ) = XTEMP
         END IF
 2    CONTINUE
C
 99   CONTINUE
      END
* $Id$
