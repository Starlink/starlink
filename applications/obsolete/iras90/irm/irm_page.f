      SUBROUTINE IRM_PAGE
*+
*  Name:
*     IRM_PAGE

*  Purpose:
*     Start a new page of output by routine IRM_PTOUT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     SUBROUTINE IRM_PAGE

*  Description:
*     Routine IRM_PTOUT displays text in pages. This routine forces the
*     start of a new page.

*  Authors:
*     (DSB) David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-SEP-1992 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! no default typing allowed

*  Global Variables:
      INCLUDE 'IRM_COM'        ! IRM common blocks.
*        MCM_CMD = CHARACTER * ( 80 ) (Read and Write)
*           The text entered in response to the "Press RETURN
*           to continue..." prompt.
*        MCM_LTEXT = INTEGER (Read and Write)
*           Lines of text output this screenful.
*        MCM_TXTON = LOGICAL (Read and Write)
*           If true, text output is enabled.
*        MCM_LTOP = INTEGER (Read and Write)
*           Top line number for the scrolling region.
*        MCM_LBOT = INTEGER (Read and Write)
*           Bottom line number for the scrolling region.
*        MCM_ANSI = LOGICAL (Read and Write)
*           If true, an ANSI terminal is in use.
*        MCM_LUCMD = INTEGER (Read and Write)
*           Logical-unit number of the command input.
*        MCM_LUTER = INTEGER (Read and Write)
*           Logical-unit number of the terminal output.

*  External References:
      EXTERNAL IRM1_BLDAT       ! Initialise IRM common blocks.

*  Local Variables:
      INTEGER STATUS		! Local status.
      INTEGER WIDTH             ! Screen width.

*.

*  If not already done, initialise COMMON-block variables.
      IF( MCM_LUTER .EQ. -1 ) THEN

*  Nothing has been output, and there is no command.
         MCM_LTEXT = 0
         MCM_TXTON = .TRUE.
         MCM_CMD = ' '

*  Fixed for test purposes.  Note these are hardware specific.
         MCM_ANSI = .FALSE.
         MCM_LUCMD = 5
         MCM_LUTER = 6

      END IF

*  Find the height and width of the screen.  Use the full screen area.
*  A zero or negative LBOT (which occurs when there is an error) will
*  suppress paging.
      CALL ONE_SCRSZ( WIDTH, MCM_LBOT, STATUS )
      MCM_LTOP = 1

*  Reset the line count.
      MCM_LTEXT = 0

*  Ensure output is switched on.
      MCM_TXTON = .TRUE.

      END
