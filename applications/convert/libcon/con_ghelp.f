      SUBROUTINE CON_GHELP( HELPLB, KEYWRD, STATUS )
*+
*  Name:
*     CON_GHELP

*  Purpose:
*     Prints help text from a library of help information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_GHELP( HELPLB, KEYWRD, STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Prints help text from an help library. A specific keyword/topic
*     can be specified.

*  Arguments:
*     HELPLB = CHARACTER * ( * ) (Given)
*        The name of the help library.
*     KEYWRD = CHARACTER * ( * ) (Given)
*        The keyword to select the information required from the help
*        library.
*     STATUS = INTEGER (Given and Returned)
*        Global status value.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1993 July 30 (MJC):
*        Original based on GETHLP of KAPPA.
*     {enter_any_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions

*  Global Variables:
      INCLUDE 'HLPCMD'           ! CONVERT help I/O
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
      CHARACTER * ( * ) HELPLB
      CHARACTER * ( * ) KEYWRD

*  Status:
      INTEGER STATUS             ! Inherited global status

*  External References:
      INTEGER CHR_LEN            ! Character string length ignoring 
                                 ! trailing blanks
      INTEGER CON_GHLPI          ! Routine for reading help command
      INTEGER HLP_HELP           ! Interactive help
      INTEGER CON_PHLPO          ! Routine for outputting help 

      EXTERNAL CON_GHLPI         ! Gets the help information
      EXTERNAL HLP_NAMETR        ! Interactive help library-name
                                 ! translation
      EXTERNAL CON_PHLPO         ! Outputs the help information

*  Local Variables:
      CHARACTER * ( 80 ) ERRMSG  ! Error message
      INTEGER HFLAGS             ! Help flags
      INTEGER HLPLEN             ! Length of the help library name
      INTEGER ISTAT              ! Local status
      INTEGER LUHLP              ! Logical unit for reading the help
                                 ! library
      INTEGER KWRDLN             ! Length of the keyword
      INTEGER WIDTH              ! Width of the screen in characters

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
      CALL CON_SCRSZ( WIDTH, LBOT, STATUS )
      LTOP = 1

*  Get help.
*  =========

*  Set logical unit number for help.  This is hardware dependent.
      LUHLP = 1

*  Set the flags to enable prompting.
      HFLAGS = 1

*  Initiate interactive help session.
      ISTAT = HLP_HELP( CON_PHLPO, WIDTH, KEYWRD( 1:KWRDLN ), LUHLP,
     :                  HELPLB( 1:HLPLEN ), HFLAGS, CON_GHLPI,
     :                  HLP_NAMETR )

*  Watch for an error status.
      IF ( ISTAT .NE. 1 ) THEN
         CALL HLP_ERRMES( ISTAT, ERRMSG )
         CALL MSG_SETC( 'ERR', ERRMSG )
         CALL MSG_SETC( 'TOPIC', KEYWRD )
         CALL MSG_SETC( 'LIB', HELPLB )
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CON_GHELP_ERR',
     :     'Error accessing help on topic ^TOPIC in library ^LIB. '/
     :     /'Reason was "^ERR". ', STATUS )
      END IF

      END
