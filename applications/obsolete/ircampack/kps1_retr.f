      SUBROUTINE KPS1_RETR( SIZE, INDEX, DATA, VALUE, STATUS )
*+
*  Name:
*     KPS1_RETR

*  Purpose:
*     Retrieve a real value from an array

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_RETR( SIZE, INDEX, DATA, VALUE, STATUS )

*  Description:
*     The value stored at a given index within the supplied array is
*     returned.

*  Arguments:
*     SIZE = INTEGER (Given)
*        The size of the array.
*     INDEX = INTEGER (Given)
*        The index within the array of the required value.
*     DATA( SIZE ) = REAL (Given)
*        The input array.
*     VALUE = REAL (Returned)
*        The returned value.
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
      REAL DATA( SIZE )

*  Arguments Returned:
      REAL VALUE

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the required value from the supplied data array at the supplied
*  index.
      VALUE = DATA( INDEX )

      END
