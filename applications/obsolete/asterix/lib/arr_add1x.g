      SUBROUTINE ARR_ADD1<T>( N, IN, CONST, OUT, STATUS )
*+
*  Name:
*     ARR_ADD1<T>

*  Purpose:
*     Add a constant to a <TYPE> array

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ARR_ADD1<T>( N, IN, CONST, OUT, STATUS )

*  Description:
*     Adds a constant to the input array, creating an output
*     array. The IN and OUT arrays may be the same array.

*  Arguments:
*     N = INTEGER (given)
*        Number of values to multiply
*     IN[] = <TYPE> (given)
*        The input data
*     CONST = <TYPE> (given)
*        The additive constant
*     OUT[] = <TYPE> (returned)
*        The output data
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
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  References:
*     ARR Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/arr.html

*  Keywords:
*     package:arr, usage:public, arrays, arithmetic

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     7 Feb 1995 (DJA):
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
      <TYPE>			IN(*), CONST

*  Arguments Returned:
      <TYPE>			OUT(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			I			! Loop over data
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Scale values
      DO I = 1, N
        OUT(I) = IN(I) + CONST
      END DO

      END
