      SUBROUTINE KPS1_STOR( SIZE, INDEX, VALUE, DATA, STATUS )
*+
*  Name:
*     KPS1_STOR

*  Purpose:
*     Store a real value in an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_STOR( SIZE, INDEX, VALUE, DATA, STATUS )

*  Description:
*     The supplied value sis stored in the array at the given index.

*  Arguments:
*     SIZE = INTEGER (Given)
*        Size of the array.
*     INDEX = INTEGER (Given)
*        The index at which to store the supplied value.
*     VALUE = REAL (Given)
*        The value to be stored in the array.
*     DATA( SIZE ) = REAL (Given and Returned)
*        The array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-NOV-1993 (DSB):
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
      INTEGER SIZE
      INTEGER INDEX
      REAL VALUE

*  Arguments Given and Returned:
      REAL DATA( SIZE )

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Put the supplied value in the data array at the supplied index.
      DATA( INDEX ) = VALUE

      END
