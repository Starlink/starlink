      SUBROUTINE IRH1_ISHOW( SIZE, ARRAY, INDXLO, INDXHI, STATUS )
*+
*  Name:
*     IRH1_ISHOW

*  Purpose:
*     List names in a group subsection to the terminal.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH1_ISHOW( SIZE, ARRAY, INDXLO, INDXHI, STATUS )

*  Description:
*     Each non-blank name in the array subsection is displayed using
*     MSG_OUT. One name per line, preceeded with 5 spaces.

*  Arguments:
*     SIZE = INTEGER (Given)
*        The size of the array.
*     ARRAY( SIZE ) = CHARACTER (Given)
*        The array of names to be listed.
*     INDXLO = INTEGER (Given)
*        Low index limit of the array subsection to be displayed. Values
*        less than one cause one to be used instead.
*     INDXHI = INTEGER (Given)
*        High index limit of the array subsection to be displayed.
*        Values greater than SIZE cause SIZE to be used instead.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-MAY-1991 (DSB):
*        Original version.
*     26-FEB-1992 (PDRAPER):
*        Changed to use message token instead of argument concatenation
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER SIZE
      CHARACTER ARRAY( SIZE )*(*)
      INTEGER INDXLO
      INTEGER INDXHI

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop count.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write each name to the screen.
      DO I = MAX( 1, INDXLO), MIN( SIZE, INDXHI )
         CALL MSG_SETC( 'ARRAYI', ARRAY( I ) )
         CALL MSG_OUT( 'REPORT', '     ^ARRAYI', STATUS )
      END DO

      END
* $Id$
