      SUBROUTINE ARR_PRANG1<T>( N, ARRAY, PMIN, <T>MIN, PMAX,
     :                          <T>MAX, STATUS )
*+
*  Name:
*     ARR_PRANG1<T>

*  Purpose:
*     Find range of values in a <COMM> array, and their pixel numbers

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ARR_PRANG1<T>( N, ARRAY, PMIN, <T>MIN, PMAX, <T>MAX, STATUS )

*  Description:
*     Obtains the minimum and maximum values in a <COMM> array, and the
*     pixel numbers at which the first occurrence of the minimum and
*     maximum occur. All values are consider good.

*  Arguments:
*     N = INTEGER (given)
*        Number of values in array
*     ARRAY[] = <TYPE> (given)
*        Array of values whose range is to be found
*     PMIN = INTEGER (returned)
*        Pixel number of first occurrence of minimum value
*     <T>MIN = <TYPE> (returned)
*        The minimum value in the array
*     PMAX = INTEGER (returned)
*        Pixel number of first occurrence of maximum value
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
      INTEGER			PMIN			! Pixel no. of minimum
      <TYPE>			<T>MIN			! Minimum value
      INTEGER			PMAX			! Pixel no. of maximum
      <TYPE>			<T>MAX			! Maximum value

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			I			! Loop over array
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      PMIN = 0
      <T>MIN = VAL__MAX<T>
      PMAX = 0
      <T>MAX = VAL__MIN<T>

*  Find range
      DO I = 1, N
        IF ( ARRAY(I) .LT. <T>MIN ) THEN
          <T>MIN = ARRAY(I)
          PMIN = I
        END IF
        IF ( ARRAY(I) .GT. <T>MAX ) THEN
          <T>MAX = ARRAY(I)
          PMAX = I
        END IF
      END DO

      END
