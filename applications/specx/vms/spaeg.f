      SUBROUTINE SPAEG( MODE, PAGE, WIDTH, LINE, STATUS )
*+
*  Name:
*     SPAEG

*  Purpose:
*     Count lines on a page.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPAEG( MODE, PAGE, WIDTH, LINE, STATUS )

*  Description:
*     This routine counts lines printed on a page or screen. The PAGE
*     and LINE can each be given or returned arguments. Accordingly the
*     MODE can be between 0 and 3. When PAGE is given, so is WIDTH.

*  Arguments:
*     MODE = INTEGER (Given)
*        The mode of operation:
*        0: page length and line number are returned arguments.
*        1: page length is given, line number returned.
*        2: page length is returned, line number given.
*        3: page length and line number are given.
*        Thus PAGE is given/returned when bit 0 is 0/1,
*        and LINE is given/returned when bit 1 is 0/1.
*        Any other number for mode is equivalent to MODE = 0.
*     PAGE = INTEGER (Given and Returned)
*        The length of the page, i.e. how many lines fit on the page.
*     WIDTH = INTEGER (Given and Returned)
*        The width of the page, i.e. how many columns fit on the screen.
*     LINE = INTEGER (Given and Returned)
*        The number of lines printed so far on the page.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (EIA, Starlink)
*     {enter_new_authors_here}

*  History:
*     04 Nov 1992 (hme):
*        Original version.
*     30 Jun 1993 (hme):
*        Port to Specdre, add WIDTH and STATUS arguments.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER MODE

*  Arguments Given and Returned:
      INTEGER PAGE
      INTEGER WIDTH
      INTEGER LINE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER PAGECP
      INTEGER WIDCP
      INTEGER LINECP
      SAVE PAGECP, WIDCP, LINECP

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Do the job.
      IF ( MODE .EQ. 1 ) THEN
         PAGECP = PAGE
         WIDCP = WIDTH
         LINE = LINECP
      ELSE IF ( MODE .EQ. 2 ) THEN
         PAGE = PAGECP
         WIDTH = WIDCP
         LINECP = LINE
      ELSE IF ( MODE .EQ. 3 ) THEN
         PAGECP = PAGE
         WIDCP = WIDTH
         LINECP = LINE
      ELSE
         PAGE = PAGECP
         WIDTH = WIDCP
         LINE = LINECP
      END IF

*  Return.
      END
