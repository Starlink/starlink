      SUBROUTINE ARD1_PUTI( VALUE, SIZE, INDEX, ARRAY, STATUS )
*+
*  Name:
*     ARD1_PUTI

*  Purpose:
*     Put an integer value into an array

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_PUTI( VALUE, SIZE, INDEX, ARRAY, STATUS )

*  Description:
*     The supplied value is stored in the array at the given index.

*  Arguments:
*     VALUE = INTEGER (Given)
*        The value to be stored.
*     SIZE = INTEGER (Given)
*        The size of the array.
*     INDEX = INTEGER (Given)
*        The index at which to store the value.
*     ARRAY( SIZE ) = INTEGER (Given and Returned)
*        The array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-FEB-1994 (DSB):
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
      INTEGER VALUE
      INTEGER SIZE
      INTEGER INDEX

*  Arguments Given and Returned:
      INTEGER ARRAY( SIZE )

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store the value at the given index.
      ARRAY( INDEX ) = VALUE

      END
