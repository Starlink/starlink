      SUBROUTINE SORT_MVIDX<T>( N, IN, INDEX, OUT, STATUS )
*+
*  Name:
*     SORT_MVIDX<T>

*  Purpose:
*     Moves <COMM> data using a supplied index

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL SORT_MVIDX<T>( N, IN, INDEX, OUT, STATUS )

*  Description:
*     Moves the values in the array IN to the slots in the array OUT
*     given by the ordering array INDEX. This index ca be constructed
*     using (for example) one of the SORT_IDX* routines.

*  Arguments:
*     N = INTEGER (given)
*        Number of values in arrays
*     IN = <TYPE>[] (given)
*        Input values to be moved
*     INDEX = INTEGER[] (given)
*        The ordering list
*     OUT = <TYPE>[] (returned)
*        The output ordered array
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
*     SORT Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/sort.html

*  Keywords:
*     package:sort, usage:public

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
      INTEGER                	N                  	! Number of names
      <TYPE>       		IN(*)              	! Input list data
      INTEGER                	INDEX(*)           	! Output index

*  Arguments Returned:
      <TYPE>       		OUT(*)              	! Output ordered list

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			I			! Loop over index
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Move each input data item to its appropriate output slot
      DO I = 1 ,N
        OUT(I) = IN(INDEX(I))
      END DO

      END
