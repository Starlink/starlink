      SUBROUTINE ARR_RANG1<T>( N, ARRAY, <T>MIN, <T>MAX, STATUS )
*+
*  Name:
*     ARR_RANG1<T>

*  Purpose:
*     Find range of values in a <COMM> array

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ARR_RANG1<T>( N, ARRAY, <T>MIN, <T>MAX, STATUS )

*  Description:
*     Obtains the minimum and maximum values in a <COMM> array.
*     All values are consider good.

*  Arguments:
*     N = INTEGER (given)
*        Number of values in array
*     ARRAY[] = <TYPE> (given)
*        Array of values whose range is to be found
*     <T>MIN = <TYPE> (returned)
*        The minimum value in the array
*     <T>MAX = <TYPE> (returned)
*        The maximum value in the array
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
*     ARR Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/arr.html

*  Keywords:
*     package:arr, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     7 Mar 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'

*  Arguments Given:
      INTEGER			N			! Number of values
      <TYPE>			ARRAY(*)		! Values

*  Arguments Returned:
      <TYPE>			<T>MIN			! Minimum value
      <TYPE>			<T>MAX			! Maximum value

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			I			! Loop over array
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      <T>MIN = VAL__MAX<T>
      <T>MAX = VAL__MIN<T>

*  Find range
      DO I = 1, N
        IF ( ARRAY(I) .LT. <T>MIN ) <T>MIN = ARRAY(I)
        IF ( ARRAY(I) .GT. <T>MAX ) <T>MAX = ARRAY(I)
      END DO

      END
