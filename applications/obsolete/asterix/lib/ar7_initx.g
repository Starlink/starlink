      SUBROUTINE AR7_INIT<T>( VALUE, DIMS, ARRAY, STATUS )
*+
*  Name:
*     AR7_INIT<T>

*  Purpose:
*     Initialise 7-D array with <TYPE> value

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL AR7_INIT<T>( VALUE, DIMS, ARRAY, STATUS )

*  Description:
*     Sets all the elements of a 7-D array to VALUE. Use this to initialise
*     7-D arrays, or arrays whose DIMS array has been padded up to 7-D.

*  Arguments:
*     VALUE = <TYPE> (given)
*        Value to insert into array
*     DIMS[7] = INTEGER (given)
*        Dimensions of 7-D array
*     ARRAY[] = <TYPE> (returned)
*        The initialised array
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

*  References:
*     AR7 Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/ar7.html

*  Keywords:
*     package:ar7, usage:public, initialisation, 7-D

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (JET-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     30 Jan 95 (DJA):
*        Original version.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      <TYPE>			VALUE			! Initialisation value
      INTEGER			DIMS(7)			! Array dimensions

*  Arguments Returned:
      <TYPE>			ARRAY(*)		! Array to initialise

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			I			! Loop over elements
      INTEGER			NELEM			! Total # elements
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find total number of elements
      NELEM = DIMS(1) * DIMS(2) * DIMS(3) * DIMS(4) * DIMS(5) *
     :        DIMS(6) * DIMS(7)

*  Initialise elements
      DO I = 1, NELEM
        ARRAY(I) = VALUE
      END DO

      END
