      SUBROUTINE ARR_MULT<T>( CONST, NEL, ARRAY, STATUS )
*+
*  Name:
*     ARR_MULT<T>

*  Purpose:
*     Multiply an array by a constant

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ARR_MULT<T>( CONST, NEL, ARRAY, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     CONST = <TYPE> (given)
*        Constant to multiply with
*     N = INTEGER (given)
*        Number of elements in array
*     ARRAY[N] = <TYPE> (given and returned)
*        Array to be multiplied
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

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  {machine}-specific features used:
*     {routine_machine_specifics}...

*  {DIY_prologue_heading}:
*     {DIY_prologue_text}

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
*     23 May 1996 (DJA):
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
      <TYPE>			CONST
      INTEGER			N

*  Arguments Given and Returned:
      <TYPE>			ARRAY(*)

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Variables:
      INTEGER			I			! Loop over ARRAY
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Perform multiplication
      DO I = 1, N
        ARRAY(I) = ARRAY(I) * CONST
      END DO

      END
