      SUBROUTINE ARD1_PUTD( VALUE, SIZE, INDEX, ARRAY, STATUS )
*+
*  Name:
*     ARD1_PUTI

*  Purpose:
*     Put a _double value into an array

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_PUTD( VALUE, SIZE, INDEX, ARRAY, STATUS )

*  Description:
*     The supplied value is stored in the array at the given index.

*  Arguments:
*     VALUE = DOUBLE PRECISION (Given)
*        The value to be stored.
*     SIZE = INTEGER (Given)
*        The size of the array.
*     INDEX = INTEGER (Given)
*        The index at which to store the value.
*     ARRAY( SIZE ) = DOUBLE PRECISION (Given and Returned)
*        The array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-JUL-2001 (DSB):
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
      DOUBLE PRECISION VALUE
      INTEGER SIZE
      INTEGER INDEX

*  Arguments Given and Returned:
      DOUBLE PRECISION ARRAY( SIZE )

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store the value at the given index.
      ARRAY( INDEX ) = VALUE

      END
