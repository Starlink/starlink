      SUBROUTINE ARR_INIT1<T>( VALUE, N, ARRAY, STATUS )
*+
*  Name:
*     ARR_INIT1<T>

*  Purpose:
*     Initialise elements of a <TYPE> array

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARR_INIT1<T>( VALUE, N, ARRAY, STATUS )

*  Description:
*     Initialise the elements of a 1-dimensional array

*  Arguments:
*     VALUE = <TYPE> (given)
*        Value to use to initialise array
*     N = INTEGER (given)
*        Number of values to initialise. ARRAY must be declared to be at
*        least this size.
*     ARRAY[] = <TYPE> (returned)
*        The initialised array
*     STATUS = INTEGER (given)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Notes:
*     {routine_notes}...

*  Keywords:
*     package:arr, usage:public, array, initialisation

*  Copyright:
*     {routine_copyright}

*  Authors:
*     JCMP: Jim Peden (University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     27 Nov 85 (JCMP):
*        Original version.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      <TYPE>			VALUE			! Initialisation value
      INTEGER			N			! Number of values

*  Arguments Returned:
      <TYPE>			ARRAY(*)		! Array to initialise

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER			I			! Loop variable
*.

*  Check inherited global status.
      IF ( STATUS .EQ. SAI__OK ) THEN
	DO I = 1, N
	   ARRAY(I) = VALUE
	ENDDO
      ENDIF

      END
