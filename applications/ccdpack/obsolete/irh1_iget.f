      SUBROUTINE IRH1_IGET( SIZE, ARRAY, INDEX, TEXT, STATUS )
*+
*  Name:
*     IRH1_IGET

*  Purpose:
*     Get an element from a character array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH1_IGET( SIZE, ARRAY, INDEX, TEXT, STATUS )

*  Description:
*     The string held in the given array at the given index is returned.

*  Arguments:
*     SIZE = INTEGER (Given)
*        The size of the array specified by argument ARRAY.
*     ARRAY( SIZE ) = CHARACTER (Given)
*        The array. 
*     INDEX = INTEGER (Given)
*        The index of the element to get from ARRAY. 
*     TEXT = CHARACTER (Returned)
*        The text from the specified element of ARRAY.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-MAY-1991 (DSB):
*        Original version.
*     26-FEB-1992 (PDRAPER):
*        Added DAT_PAR.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.

*  Arguments Given:
      INTEGER SIZE
      CHARACTER ARRAY( SIZE )*(*)
      INTEGER INDEX

*  Arguments Returned:
      CHARACTER TEXT*(*)

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the supplied index is outside the array bounds, return a blank
*  string.
      IF( INDEX .LE. 0 .OR. INDEX .GT. SIZE ) THEN
         TEXT = ' '

*  Otherwise, get the array element.
      ELSE
         TEXT = ARRAY( INDEX )
 
      END IF

      END
* $Id$
