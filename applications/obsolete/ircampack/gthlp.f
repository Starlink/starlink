      SUBROUTINE GTHLP( HELPLB, KEYWRD, INTER, STATUS )
*+
*  Name:
*     GTHLP

*  Purpose:
*     Prints help text from a library of help information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GTHLP( HELPLB, KEYWRD, INTER, STATUS )

*  Description:
*     Prints help text from an help library. A specific keyword/topic
*     can be specified.

*  Arguments:
*     HELPLB = CHARACTER * ( * ) (Given)
*        The name of the help library file.
*     KEYWRD = CHARACTER * ( * ) (Given)
*        The keyword about which help is required.
*     INTER = LOGICAL (Given)
*        If true then an interactive help session is entered. Otherwise,
*        control is returned to the calling routine when the help item
*        specified by KEYWRD has been displayed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-OCT-1993 (DSB):
*        Original version modified from routine IRAS90 routine IRM_GTHLP.FOR
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions

*  Global Variables:
      INCLUDE 'IRC_COM'        ! IRCAMPACK common blocks.
*        IRC_CMD = CHARACTER * ( 80 ) (Write)
*           The text entered in response to the "Press RETURN
*           to continue..." prompt.
*        IRC_LTEXT = INTEGER (Read and Write)
*           Lines of text output this screenful.
*        IRC_TXTON = LOGICAL (Read)
*           If true, text output is enabled.
*        IRC_LTOP = INTEGER (Read)
*           Top line number for the scrolling region.
*        IRC_LBOT = INTEGER (Read)
*           Bottom line number for the scrolling region.
*        IRC_ANSI = LOGICAL (Read)
*           If true, an ANSI terminal is in use.
*        IRC_LUCMD = INTEGER (Read)
*           Logical-unit number of the command input.
*        IRC_LUTER = INTEGER (Read)
*           Logical-unit number of the terminal output.

*  Arguments Given:
      CHARACTER * ( * )
     :  HELPLB,
     :  KEYWRD

      LOGICAL
     :  INTER

*  Status:
      INTEGER STATUS             ! Inherited global status

*  External References:
      INTEGER
     :  CHR_LEN,                 ! Character string length ignoring
                                 ! trailing blanks
     :  GHLPI,                   ! Routine for reading help command
     :  HLP_HELP,                ! Interactive help
     :  PTOUT                    ! Routine for outputting help

      EXTERNAL
     :  GHLPI,                   ! Gets the help information
     :  NAMTR,                   ! Interactive help library-name
                                 ! translation
     :  PTOUT,                   ! Outputs the help information
     :  BLDAT                    ! Initialise IRM common blocks.


*  Local Variables:
      CHARACTER * ( 80 )
     :  ERRMSG                   ! Error message

      INTEGER
     :  HFLAGS,                  ! Help flags
     :  HLPLEN,                  ! Length of the help library name
     :  ISTAT,                   ! Local status
     :  LUHLP,                   ! Logical unit for reading the help
                                 ! library
     :  KWRDLN,                  ! Length of the keyword
     :  WIDTH                    ! Width of the screen in characters

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the lengths of the input strings.
      HLPLEN = MAX( 1, CHR_LEN( HELPLB ) )
      KWRDLN = MAX( 1, CHR_LEN( KEYWRD ) )

*  Initialise COMMON-block variables.
*  ==================================

*  Nothing has been output, and there is no command.
      IRC_LTEXT = 0
      IRC_TXTON = .TRUE.
      IRC_CMD = ' '

*  Fixed for test purposes.  Note these are hardware specific.
      IRC_ANSI = .FALSE.
      IRC_LUCMD = 5
      IRC_LUTER = 6

*  Find the height and width of the screen.  Use the full screen area.
*  A zero or negative LBOT (which occurs when there is an error) will
*  suppress paging.
      CALL SCRSZ( WIDTH, IRC_LBOT, STATUS )
      IRC_LTOP = 1

*  Get help.
*  =========

*  Reserve a logical unit number for accessing the help library.
      CALL LUNIT( LUHLP, STATUS )

*  Set the flags to enable prompting if required.
      IF( INTER ) THEN
         HFLAGS = 1
      ELSE
	 HFLAGS = 0
      END IF

*  Initiate interactive help session.
      ISTAT = HLP_HELP( PTOUT, WIDTH, KEYWRD( :KWRDLN ), LUHLP,
     :                  HELPLB( :HLPLEN ), HFLAGS, GHLPI,
     :                  NAMTR )

*  Watch for an error status.
      IF ( ISTAT .NE. 1 ) THEN
         CALL HLP_ERRMES( ISTAT, ERRMSG )
         CALL MSG_SETC( 'ERR', ERRMSG )
         CALL MSG_SETC( 'TOPIC', KEYWRD )
         CALL MSG_SETC( 'LIB', HELPLB )
         STATUS = SAI__ERROR
         CALL ERR_REP( 'GTHLP_ERR1',
     :   'GTHLP: Error accessing help on topic ^TOPIC in library'//
     :   ' ^LIB. Reason was "^ERR". ', STATUS )
      END IF

      END
