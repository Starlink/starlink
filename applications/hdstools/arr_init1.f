      SUBROUTINE ARR_INIT1D( VALUE, N, ARRAY, STATUS )
*+
*  Name:
*     ARR_INIT1D

*  Purpose:
*     Initialise elements of a DOUBLE PRECISION array

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ARR_INIT1D( VALUE, N, ARRAY, STATUS )

*  Description:
*     Initialise the elements of a 1-dimensional array

*  Arguments:
*     VALUE = DOUBLE PRECISION (given)
*        Value to use to initialise array
*     N = INTEGER (given)
*        Number of values to initialise. ARRAY must be declared to be at
*        least this size.
*     ARRAY[] = DOUBLE PRECISION (returned)
*        The initialised array
*     STATUS = INTEGER (given)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Notes:
*     {routine_notes}...

*  References:
*     ARR Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/arr.html

*  Keywords:
*     package:arr, usage:public, array, initialisation

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

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
      DOUBLE PRECISION			VALUE			! Initialisation value
      INTEGER			N			! Number of values

*  Arguments Returned:
      DOUBLE PRECISION			ARRAY(*)		! Array to initialise

*  Status:
      INTEGER 			STATUS             	! Global status

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
      SUBROUTINE ARR_INIT1I( VALUE, N, ARRAY, STATUS )
*+
*  Name:
*     ARR_INIT1I

*  Purpose:
*     Initialise elements of a INTEGER array

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ARR_INIT1I( VALUE, N, ARRAY, STATUS )

*  Description:
*     Initialise the elements of a 1-dimensional array

*  Arguments:
*     VALUE = INTEGER (given)
*        Value to use to initialise array
*     N = INTEGER (given)
*        Number of values to initialise. ARRAY must be declared to be at
*        least this size.
*     ARRAY[] = INTEGER (returned)
*        The initialised array
*     STATUS = INTEGER (given)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Notes:
*     {routine_notes}...

*  References:
*     ARR Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/arr.html

*  Keywords:
*     package:arr, usage:public, array, initialisation

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

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
      INTEGER			VALUE			! Initialisation value
      INTEGER			N			! Number of values

*  Arguments Returned:
      INTEGER			ARRAY(*)		! Array to initialise

*  Status:
      INTEGER 			STATUS             	! Global status

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
      SUBROUTINE ARR_INIT1L( VALUE, N, ARRAY, STATUS )
*+
*  Name:
*     ARR_INIT1L

*  Purpose:
*     Initialise elements of a LOGICAL array

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ARR_INIT1L( VALUE, N, ARRAY, STATUS )

*  Description:
*     Initialise the elements of a 1-dimensional array

*  Arguments:
*     VALUE = LOGICAL (given)
*        Value to use to initialise array
*     N = INTEGER (given)
*        Number of values to initialise. ARRAY must be declared to be at
*        least this size.
*     ARRAY[] = LOGICAL (returned)
*        The initialised array
*     STATUS = INTEGER (given)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Notes:
*     {routine_notes}...

*  References:
*     ARR Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/arr.html

*  Keywords:
*     package:arr, usage:public, array, initialisation

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

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
      LOGICAL			VALUE			! Initialisation value
      INTEGER			N			! Number of values

*  Arguments Returned:
      LOGICAL			ARRAY(*)		! Array to initialise

*  Status:
      INTEGER 			STATUS             	! Global status

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
      SUBROUTINE ARR_INIT1R( VALUE, N, ARRAY, STATUS )
*+
*  Name:
*     ARR_INIT1R

*  Purpose:
*     Initialise elements of a REAL array

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ARR_INIT1R( VALUE, N, ARRAY, STATUS )

*  Description:
*     Initialise the elements of a 1-dimensional array

*  Arguments:
*     VALUE = REAL (given)
*        Value to use to initialise array
*     N = INTEGER (given)
*        Number of values to initialise. ARRAY must be declared to be at
*        least this size.
*     ARRAY[] = REAL (returned)
*        The initialised array
*     STATUS = INTEGER (given)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Notes:
*     {routine_notes}...

*  References:
*     ARR Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/arr.html

*  Keywords:
*     package:arr, usage:public, array, initialisation

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

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
      REAL			VALUE			! Initialisation value
      INTEGER			N			! Number of values

*  Arguments Returned:
      REAL			ARRAY(*)		! Array to initialise

*  Status:
      INTEGER 			STATUS             	! Global status

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
