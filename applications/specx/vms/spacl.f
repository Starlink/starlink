      INTEGER FUNCTION SPACL( STRING )
*+
*  Name:
*     SPACL

*  Purpose:
*     Output routine for help system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = SPACL( STRING )

*  Description:
*     This routine interfaces the help system with the terminal to
*     send a line of output. It is called by the help system and uses
*     C-coded routines at a lower level.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The string got from the help system and to be sent to the
*        terminal.

*  Returned Value:
*     SPACL = INTEGER
*        The returned value is 1 for OK, -1 if an error occured.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     21 Jul 1992 (hme):
*        Original version.
*     04 Nov 1992 (hme):
*        Adapted from Specdre's SPACL. Put string into token so that
*        MSG_OUT will not interpret metacharacters.
*        Enable paging, i.e. increment line counter and compare to page
*        length.
*     01 Jul 1993 (hme):
*        Back from Figaro to Specdre. Disuse ADAM, use C code instead.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) STRING

*  Status:
      INTEGER STATUS             ! Local status

*  Local Constants:
      INTEGER NLNEXT             ! So many lines for next-screen prompt
      PARAMETER ( NLNEXT = 3 )

*  Local Variables:
      INTEGER PAGE               ! Page length
      INTEGER WIDTH              ! Page width
      INTEGER LINE               ! Lines printed so far

*.

*  Set status OK.
      STATUS = SAI__OK

*  Get page length and line number.
      CALL SPAEG( 0, PAGE, WIDTH, LINE, STATUS )

*  If necessary, ask permission to start a new page (screen).
      IF ( PAGE .GT. 0 .AND. PAGE .LE. LINE + NLNEXT ) THEN
         CALL SPAEJ( STATUS )
         LINE = 1
      ELSE
         LINE = LINE + 1
      END IF

*  Send the string. Just for consistency, this is also done with some
*  C code.
      CALL SPAEK( STRING, LEN(STRING), STATUS )

*  Set return values according to status.
      IF ( STATUS .EQ. SAI__OK ) THEN
         SPACL = 1
      ELSE
         SPACL = -1
      END IF

*  Set the new count for lines on page.
      CALL SPAEG( 2, PAGE, WIDTH, LINE, STATUS )

*  Return.
      END
