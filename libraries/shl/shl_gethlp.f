      SUBROUTINE SHL_GETHLP( HELPLB, KEYWRD, STATUS )
*+
*  Name:
*     SHL_GETHLP

*  Purpose:
*     Prints help text from a library of help information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SHL_GETHLP( HELPLB, KEYWRD, STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Prints help text from an help library. A specific keyword/topic
*     can be specified.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1984 Nov 3 (MJC):
*        Original.
*     1986 Nov 14 (MJC):
*        Converted to ADAM style prologue and status argument added.
*     1988 Sept 7 (MJC):
*        Used KAPPA input and output routines for LIB$GET_INPUT and
*        LIB$PUT_OUTPUT.
*     1992 June 17 (MJC):
*        Uses portable help system.  Converted to SST prologue and
*        documented global parameters.
*     1992 August 4 (MJC):
*        Incorporate revisions to the portable help system.
*     15 July 2004 (TIMJ):
*        Import into shared SHL library
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions

*  Global Variables:
      INCLUDE 'SHL_HLPCMD'           ! KAPPA help I/O
*        CMD = CHARACTER * ( 80 ) (Write)
*           The command line.
*        LHELP = INTEGER (Write)
*           Lines of help output this screenful.
*        HELPN = LOGICAL (Write)
*           If true, help output is enabled.
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

*  Arguments Given:
      CHARACTER * ( * )
     :  HELPLB,
     :  KEYWRD

*  Status:
      INTEGER STATUS             ! Inherited global status

*  External References:
      INTEGER
     :  CHR_LEN,                 ! Character string length ignoring
                                 ! trailing blanks
     :  SHL_GTHLPI,             ! Routine for reading help command
     :  HLP_HELP,                ! Interactive help
     :  SHL_PTHLPO              ! Routine for outputting help 

      EXTERNAL
     :  SHL_GTHLPI,             ! Gets the help information
     :  HLP_NAMETR,              ! Interactive help library-name
                                 ! translation
     :  SHL_PTHLPO              ! Outputs the help information

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
      HLPLEN = CHR_LEN( HELPLB )
      KWRDLN = CHR_LEN( KEYWRD )

*  Initialise COMMON-block variables.
*  ==================================

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

*  Get help.
*  =========

*  Set logical unit number for help.  This is hardware dependent.
      LUHLP = 1

*  Set the flags to enable prompting.
      HFLAGS = 1

*  Initiate interactive help session.
      ISTAT = HLP_HELP( SHL_PTHLPO, WIDTH, KEYWRD( 1:KWRDLN ), LUHLP,
     :                  HELPLB( 1:HLPLEN ), HFLAGS, SHL_GTHLPI,
     :                  HLP_NAMETR )

*  Watch for an error status.
      IF ( ISTAT .NE. 1 ) THEN
         CALL HLP_ERRMES( ISTAT, ERRMSG )
         CALL MSG_SETC( 'ERR', ERRMSG )
         CALL MSG_SETC( 'TOPIC', KEYWRD )
         CALL MSG_SETC( 'LIB', HELPLB )
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SHL_GETHLP_ERR',
     :     'Error accessing help on topic ^TOPIC in library ^LIB. '/
     :     /'Reason was "^ERR". ', STATUS )
      END IF

      END
