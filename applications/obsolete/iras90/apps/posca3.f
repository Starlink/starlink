      SUBROUTINE POSCA3( STRING, LOGPOS, FD, STATUS )
*+
*  Name:
*     POSCA3

*  Purpose:
*     Write a line of text to the screen and to the log file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POSCA3( STRING, LOGPOS, FD, STATUS )

*  Description:
*     Any tokens in the supplied string are expanded by calling
*     MSG_LOAD. The resulting string is then written to the screen
*     so long as the conditional message filter level is not MSG__QUIET.
*     The screen output is paged. If LOGPOS is true, then the text is
*     also written to the log file.

*  Arguments:
*     STRING = CHARACTER * ( * ) ( Given )
*        The string to write out. May contain MSG tokens.
*     LOGPOS = LOGICAL ( Given )
*        True if the strting is to be written to a log file.
*     FD = INTEGER ( Given )
*        The file descriptor by which to access the log file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-SEP-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG constants.

*  Arguments Given:
      CHARACTER STRING*(*)
      LOGICAL LOGPOS
      INTEGER FD

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER IRM_PTOUT

*  Local Variables:
      INTEGER   BLEN             ! Used length of BUF.
      CHARACTER BUF*255          ! Buffer for a line of text
      INTEGER   FILTER           ! Conditional message filter level.
      INTEGER   ISTAT            ! Local status value.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the conditional message filter level.
      CALL MSG_IFLEV( FILTER )

*  Expand any tokens.
      CALL MSG_LOAD( ' ', STRING, BUF, BLEN, STATUS )

*  Write the string to the screen so long as the conditional message
*  filter level is not MSG__QUIET.
      IF( FILTER .NE. MSG__QUIET ) ISTAT = IRM_PTOUT( BUF( : BLEN ) )

*  Write the first 80 characters of the string to any log file.
      IF( LOGPOS ) CALL FIO_WRITE( FD, BUF( : MIN( 80, BLEN  ) ),
     :                             STATUS )

      END
