      SUBROUTINE ARR_BSRCH<T>( N, ARRAY, VALUE, MID, STATUS )
*+
*  Name:
*     ARR_BSRCH<T>

*  Purpose:
*     Binary search of sorted <COMM> array

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ARR_BSRCH<X>( N, ARRAY, VALUE, MID, STATUS )

*  Description:
*     Binary search of sorted <COMM> array. The array is assumed to be
*     sorted into ascending order.

*  Arguments:
*     N = INTEGER (given)
*        Number of values in array
*     ARRAY[] = <TYPE> (given)
*        Sorted array of values
*     VALUE = <TYPE> (given)
*        Value for search
*     MID = INTEGER (returned)
*        Element of ARRAY nearest VALUE, rounding downwards
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
*     CGP: Clive Page (University of Leicester)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     5 Mar 1988 (CGP):
*        Original version.
*     4 Apr 1995 (DJA):
*        Reformatted prologue and added status
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER			N			! See above
      <TYPE>			ARRAY(*)		!
      <TYPE>			VALUE			!

*  Arguments Returned:
      INTEGER			MID			! See above

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			K,L
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( VALUE .LT. ARRAY(1) ) THEN
	MID = 1

      ELSE IF ( VALUE .GT. ARRAY(N) ) THEN
	MID = N

      ELSE

	K = 1
	L = N
	MID = N/2
*
 10	CONTINUE
	IF ( VALUE .GE. ARRAY(MID) ) THEN
	  K = MID
	ELSE
	  L = MID
	END IF
	MID = (K+L)/2
	IF ( (K.LT.MID) .AND. (MID .LT. L) ) GOTO 10

      END IF

      END
