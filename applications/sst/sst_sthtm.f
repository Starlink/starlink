      SUBROUTINE SST_STHTM( FD, PACK, STATUS )
*+
*  Name:
*     SST_STHTM

*  Purpose:
*     Start an html document.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_STHTM( FD, PACK, STATUS )

*  Description:
*     The routine writes the commands necessary to start an html
*     document to the file connected to UNIT.

*  Arguments:
*     FD = INTEGER (Given)
*        FIO file descriptor. The html startup commands are written to
*        this file.
*     PACK = CHARACTER * ( * ) (Given)
*        Name of the package (or similar) that is being converted to
*        html. This ise used in the document titles.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     6-DEC-1994 (PDRAPER):
*        Original version
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER FD
      CHARACTER * ( * ) PACK

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a title for the document.
      CALL FIO_WRITE( FD, '<TITLE>', STATUS )
      CALL FIO_WRITE( FD, PACK, STATUS )
      CALL FIO_WRITE( FD, ' -- routine descriptions.', STATUS )
      CALL FIO_WRITE( FD, '</TITLE>', STATUS )

*  And a header.
      CALL FIO_WRITE( FD, '<H1>', STATUS )
      CALL FIO_WRITE( FD, PACK, STATUS )
      CALL FIO_WRITE( FD, ' -- index of routine descriptions', STATUS )
      CALL FIO_WRITE( FD, '</H1>', STATUS )

99    CONTINUE
* @(#)sst_sthtm.f   1.4   95/03/06 10:56:50   96/07/05 10:27:33
      END
