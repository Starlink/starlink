      SUBROUTINE ARD1_STORR( VALUE, SIZE, NEXT, PNTR, STATUS )
*+
*  Name:
*     ARD1_STORR

*  Purpose:
*     Store a real value in a dynamic array

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_STORR( VALUE, SIZE, NEXT, PNTR, STATUS )

*  Description:
*     The supplied value is stored in the array pointed to by PNTR, at
*     the index given by NEXT. The array is extended if there is
*     insufficient room to do this. NEXT is increment to point to the
*     next element in the array, and SIZE is returned holding the new
*     array size.

*  Arguments:
*     VALUE = REAL (Given)
*        The value to be stored.
*     SIZE = INTEGER (Given and Returned)
*        The current size of the array.
*     NEXT = INTEGER (Given and Returned)
*        The array index at which to store the value. Incremented by one
*        on return.
*     PNTR = INTEGER (Given and Returned)
*        A pointer to the real array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-FEB-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'ARD_CONST'        ! ARD private constants

*  Arguments Given:
      REAL VALUE

*  Arguments Given and Returned:
      INTEGER SIZE
      INTEGER NEXT
      INTEGER PNTR

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the array is not big enough increase its size.
      IF( NEXT .GT. SIZE ) THEN
         SIZE = NEXT + ARD__SZINC
         CALL PSX_REALLOC( VAL__NBR*SIZE, PNTR, STATUS )
      END IF         

*  Store the value.
      CALL ARD1_PUTR( VALUE, SIZE, NEXT, %VAL( PNTR ), STATUS )

*  Increment the index at which the next value will be stored.
      NEXT = NEXT + 1

      END
