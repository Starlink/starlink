      SUBROUTINE SHL_PAGRST( STATUS )
*+
*  Name:
*     SHL_PAGRST

*  Purpose:
*     Prepare the SHL library for paged text output

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SHL_PAGRST( STATUS)

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2004 Oct 4 (TIMJ):
*        Original version. Public wrapper to PTHLPO

*  Notes:
*     Currently all values are hard-wired. In the future the interface
*     may change to support specific controls of the initial state.

*  Bugs:
*     {note_new_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions

*  Global Variables:
      INCLUDE 'SHL_HLPCMD'       ! Common block for page status
*        CMD = CHARACTER * ( 80 ) (Write)
*           The command line.
*        LHELP = INTEGER (Write)
*           Lines of output this screenful.
*        HELPN = LOGICAL (Write)
*           If true, output is enabled.
*        LTOP = INTEGER (Write)
*           Top line number for the scrolling region.
*        LBOT = INTEGER (Write)
*           Bottom line number for the scrolling region.
*        ANSI = LOGICAL (Write)
*           If true, an ANSI terminal is in use.
*        LUCMD = INTEGER (Write)
*           Logical-unit number of the command input.
*        LUTERM = INTEGER (Write)
*           Logical-unit number of the terminal output.

      
*.

*  Status:
      INTEGER STATUS             ! Inherited global status

*  Local Variables:
      INTEGER WIDTH              ! Screen width

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Nothing has been output, and there is no command.
      LHELP = 0
      HELPN = .TRUE.
      CMD = ' '

*  Fixed for test purposes.  Note these are hardware specific.
      ANSI = .FALSE.
      LUCMD = 5
      LUTERM = 6

*  Find the height and width of the screen.  Use the full screen area.
*  A zero or negative LBOT (which occurs when there is an error) will
*  suppress paging.
      CALL ONE_SCRSZ( WIDTH, LBOT, STATUS )
      LTOP = 1

      END
