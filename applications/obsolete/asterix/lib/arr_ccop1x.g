      SUBROUTINE ARR_CCOP1<T>( N, IN, COPY, OUT, STATUS )
*+
*  Name:
*     ARR_CCOP1<T>

*  Purpose:
*     Copy <COMM> elements from IN to OUT whose COPY value is true

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ARR_CCOP1<T>( N, IN, COPY, OUT, STATUS )

*  Description:
*     All the input data from IN whose COPY() value is set true are copied
*     to the output array. This routine assumes that space for OUT has
*     been allocated, either by making sure it is as large as IN or by
*     counting the true elements of COPY.

*  Arguments:
*     N = INTEGER (given)
*        Number of elements in inputs
*     IN[] = <TYPE> (given)
*        The input data
*     COPY[] = LOGICAL (given)
*        Copy specified elements?
*     OUT[] = <TYPE> (returned)
*        The copied elements
*     STATUS = INTEGER (given)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     Allocation of OUT is adequate for copy of all true elements of IN

*  References:
*     ARR Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/arr.html

*  Keywords:
*     package:arr, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     11 Dec 1995 (DJA):
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
      INTEGER			N
       <TYPE>			IN(*)
      LOGICAL			COPY(*)

*  Arguments Returned:
      <TYPE>			OUT(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			I			! Loop over input
      INTEGER			J			! Output counter
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop over inputs
      J = 0
      DO I = 1, N

*    Copy this element?
        IF ( COPY(I) ) THEN
          J = J + 1
          OUT(J) = IN(I)
        END IF

      END DO

      END
