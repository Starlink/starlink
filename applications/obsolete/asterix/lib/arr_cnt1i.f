      SUBROUTINE ARR_CNT1I( N, ARRAY, VALUE, COUNT, STATUS )
*+
*  Name:
*     ARR_CNT1I

*  Purpose:
*     Count occurances of VALUE in the <COMM> array ARRAY

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ARR_CNT1<T>( N, ARRAY, VALUE, COUNT, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     N = INTEGER (given)
*        Number of values to search
*     ARRAY[N] = <TYPE> (given)
*        The values to search
*     VALUE = <TYPE> (given)
*        The value to search for
*     COUNT = INTEGER (returned)
*        The number of occurances of VALUE in ARRAY
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
*     5 Mar 1996 (DJA):
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
      INTEGER			ARRAY(*), VALUE

*  Arguments Returned:
      INTEGER			COUNT

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Variables:
      INTEGER			I			! Loop over input
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      COUNT = 0

*  Loop over counting values
      DO I = 1, N
        IF ( ARRAY(I) .EQ. VALUE ) COUNT = COUNT + 1
      END DO

      END
