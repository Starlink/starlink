      SUBROUTINE ARR_PRANG1R( N, ARRAY, PMIN, RMIN, PMAX,
     :                          RMAX, STATUS )
*+
*  Name:
*     ARR_PRANG1R

*  Purpose:
*     Find range of values in a REAL array, and their pixel numbers

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ARR_PRANG1R( N, ARRAY, PMIN, RMIN, PMAX, RMAX, STATUS )

*  Description:
*     Obtains the minimum and maximum values in a REAL array, and the
*     pixel numbers at which the first occurrence of the minimum and
*     maximum occur. All values are consider good.

*  Arguments:
*     N = INTEGER (given)
*        Number of values in array
*     ARRAY[] = REAL (given)
*        Array of values whose range is to be found
*     PMIN = INTEGER (returned)
*        Pixel number of first occurrence of minimum value
*     RMIN = REAL (returned)
*        The minimum value in the array
*     PMAX = INTEGER (returned)
*        Pixel number of first occurrence of maximum value
*     RMAX = REAL (returned)
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
*     ARR Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/arr.html

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
      REAL			ARRAY(*)		! Values

*  Arguments Returned:
      INTEGER			PMIN			! Pixel no. of minimum
      REAL			RMIN			! Minimum value
      INTEGER			PMAX			! Pixel no. of maximum
      REAL			RMAX			! Maximum value

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			I			! Loop over array
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      PMIN = 0
      RMIN = VAL__MAXR
      PMAX = 0
      RMAX = VAL__MINR

*  Find range
      DO I = 1, N
        IF ( ARRAY(I) .LT. RMIN ) THEN
          RMIN = ARRAY(I)
          PMIN = I
        END IF
        IF ( ARRAY(I) .GT. RMAX ) THEN
          RMAX = ARRAY(I)
          PMAX = I
        END IF
      END DO

      END
