      SUBROUTINE KPG1_QSRTC( EL, LOW, HIGH, ARRAY, STATUS )
*+
*  Name:
*     KPG1_QSRTX

*  Purpose:
*     Sorts a vector via the Quicksort algorithm.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_QSRTx( EL, LOW, HIGH, ARRAY, STATUS )

*  Description:
*     This routine sorts a vector in situ between an upper and lower
*     bounds using the Quicksort algorithm.

*  Arguments:
*     EL = INTEGER (Given)
*        The number of elements in the array that is to be sorted.
*     LOW = INTEGER (Given)
*        The lower bound within the array, below which the array
*        elements will not be sorted.  It should be less than the
*        upper bound and must be within the array.  In the latter case
*        an error will result and the routine will not sort the array.
*     HIGH = INTEGER (Given)
*        The upper bound within the array, above which the array
*        elements will not be sorted.  It should be greater than the
*        lower bound and must be within the array.  In the latter case
*        an error will result and the routine will not sort the array.
*     ARRAY( EL ) = ? (Given and Returned)
*        The array to be sorted.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     Quicksort works by picking a random "pivot" element in the array
*     then moving every element that is bigger to one side of the
*     pivot, and every element that is smaller to the other side.  The
*     procedure is repeated with the two subdivisions created by the
*     pivot.  When the number of elements in a subdivision reaches two,
*     the array is sorted.
*
*     Since recursion is not possible in Fortran, pushdown stacks are
*     used to mimic the recursive operation of the Quicksort algorithm.
*     These are also more efficient than recursion because they avoid
*     the expensive subroutine calls, especially for the many small
*     subdivisions.  The stacks contains the subdivisions to be sorted.
*     The stack is popped to obtain a subfile to sort.  The partitioning
*     pushs the larger subdivisions on to the stack, and the smaller
*     subdivision is processed immediately.  Hence the stack size
*     is only lg(EL).

*  Implementation Status:
*     -  There is a routine for each of the data types integer, real,
*     double precision, and character: replace "x" in the routine nam
*     by I, R, D, or C respectively as appropriate.
*     -  If the maximum bound is less than the minimum, the bounds are
*     swapped.

*  References:
*     -  Sedgwick, R., 1988, "Algorithms" (Addison-Wesley).

*  Timing:
*     For N elements to be sorted the timing goes as NlnN.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 January 11 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER
     :  EL,
     :  LOW,
     :  HIGH

*  Arguments Given and Returned:
      CHARACTER*(*)
     :  ARRAY( EL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXSTAK             ! The size of the stack which is
                                 ! log base 2 of the maximum number of
                                 ! elements in the array
      PARAMETER ( MXSTAK = 32 )

*  Local Variables:
      INTEGER
     :  I,                       ! Ascending pointer to an array element
     :  J,                       ! Descending pointer to an array
                                 ! element
     :  LOWER( MXSTAK ),         ! Stack for the elements of the array
                                 ! below the pivot element
     :  LBND,                    ! Polarity-checked version of the lower
                                 ! bound
     :  PSTACK,                  ! Pointer to the stack
     :  UBND,                    ! Polarity-checked version of the upper
                                 ! bound
     :  UPPER( MXSTAK )          ! Stack for the elements of the array
                                 ! above the pivot element

      CHARACTER*200
     :  PIVOT,                   ! Pivot element
     :  TEMP                     ! Used for swapping array elements

*.

*    Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Validate the array limits.
*    ==========================

*    Check that they lie within the array's bounds.  If not report an
*    the error and exit.

      IF ( LOW .LT. 1 .OR. LOW .GT. EL .OR. HIGH .LT. 1 .OR.
     :     HIGH .GT. EL ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPG1_QSRTx',
     :     'Sorting limits are outside the bounds of the array.',
     :     STATUS )
         GOTO 999
      END IF

*    Check the polarity.

      IF ( LOW .GT. HIGH ) THEN

*       A swap is necessary.

         LBND = LOW
         UBND = HIGH
      ELSE

*       Just copy the input values.

         LBND = LOW
         UBND = HIGH
      END IF

*    ^^^^^^^^^^^^^^^^^^^^^^^^^^

*    Intialise the stacks.

      LOWER( 1 ) = LBND
      UPPER( 1 ) = UBND
      PSTACK = 1

*    Loop until the stack is empty.

      DO WHILE ( PSTACK .GT. 0 )

*       Pop the stack.

         IF ( LOWER( PSTACK ) .GE. UPPER( PSTACK ) ) THEN
            PSTACK = PSTACK - 1
         ELSE

*          Partition the array.
*          ====================

            I = LOWER( PSTACK )
            J = UPPER( PSTACK )
            PIVOT = ARRAY( J )

*          Move in from both sides towards the pivot element.

            DO WHILE ( I .LT. J )
               DO WHILE ( ( I .LT. J )  .AND. ARRAY( I ) .LE. PIVOT )
                  I = I + 1
               END DO

               DO WHILE ( ( J .GT. I )  .AND. ARRAY( J ) .GE. PIVOT )
                  J = J - 1
               END DO

*             If the pivot element is not yet reached, it means that two
*             elements on either side are out of order, so swap them.

               IF ( I .LT. J ) THEN
                  TEMP = ARRAY( I )
                  ARRAY( I ) = ARRAY( J )
                  ARRAY( J ) = TEMP
               END IF
            END DO

*          Move the pivot element back to its proper place in the array.

            J = UPPER( PSTACK )
            TEMP = ARRAY( I )
            ARRAY( I ) = ARRAY( J )
            ARRAY( J ) = TEMP

*          Push values on to the stacks to further subdivide the
*          array.

            IF ( ( I - LOWER( PSTACK ) ) .LT.
     :           ( UPPER( PSTACK ) - I ) ) THEN
               LOWER( PSTACK + 1 ) = LOWER( PSTACK )
               UPPER( PSTACK + 1 ) = I - 1
               LOWER( PSTACK )     = I + 1
            ELSE
               LOWER( PSTACK + 1 ) = I + 1
               UPPER( PSTACK + 1 ) = UPPER( PSTACK )
               UPPER( PSTACK )     = I - 1
            END IF

*          Increment the stack counter.

            PSTACK = PSTACK + 1
         END IF
      END DO

  999 CONTINUE

      END
