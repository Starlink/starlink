      SUBROUTINE ARR_FLIP1<T>( N, IN, OUT, STATUS )
*+
*  Name:
*     ARR_FLIP1<T>

*  Purpose:
*     Flip one dimensional array of <COMM> values

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ARR_FLIP1<T>( N, IN, OUT, STATUS )

*  Description:
*     Reverses the order of the values in the array IN, writing them to
*     the array OUT.

*  Arguments:
*     N = INTEGER (given)
*        The number of values in the input array
*     IN = <TYPE>[] (given)
*        The values to flip
*     OUT = <TYPE>[] (returned)
*        The flipped array
*     STATUS = INTEGER (given)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     Due to the simple way this routine is implemented IN and OUT must
*     be different arrays.

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

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
*     30 Jun 1995 (DJA):
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
      <TYPE> 			IN(N)

*  Arguments Returned:
      <TYPE> 			OUT(N)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			I,J 			! Loops over arrays
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Reverse order of elements
      J = N
      DO I = 1, N
         OUT(J) = IN(I)
         J = J - 1
      END DO

      END
