      SUBROUTINE HISTA3( SIZE, TEXT, LOGPOS, FD, STATUS )
*+
*  Name:
*     HISTA3

*  Purpose:
*     Display the text of a history record.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HISTA3( SIZE, TEXT, LOGPOS, FD, STATUS )

*  Description:
*     The supplied text is displayed on the terminal screen (subject to
*     the conditional message filter level), and optionally logged to a
*     text file.

*  Arguments:
*     SIZE = INTEGER ( Given )
*        The number of lines of text in TEXT.
*     TEXT = CHARACTER( SIZE ) * ( * ) ( Given )
*        The array of text.
*     LOGPOS = LOGICAL ( Given )
*        True if display is to be written to a log file.
*     FD = INTEGER ( Given )
*        The file descriptor by which to access the log file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-SEP-1992 (DSB):
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
      CHARACTER TEXT( SIZE )*(*)
      LOGICAL LOGPOS
      INTEGER FD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER   INDEX            ! Index into the TEXT array from which
                                 ! the next line of text will be read.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop round displaying each line of text.
      DO INDEX = 1, SIZE
         CALL HISTC0( TEXT( INDEX ), LOGPOS, FD, STATUS )
      END DO

      END
