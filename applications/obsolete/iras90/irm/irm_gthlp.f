      SUBROUTINE IRM_GTHLP( HELPLB, KEYWRD, STATUS )
*+
*  Name:
*     IRM_GTHLP

*  Purpose:
*     Prints help text from a library of help information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_GTHLP( HELPLB, KEYWRD, STATUS )

*  Description:
*     Prints help text from an help library. A specific keyword/topic
*     can be specified.

*  Arguments:
*     HELPLB = CHARACTER * ( * ) (Given)
*        The name of the help library file.
*     KEYWRD = CHARACTER * ( * ) (Given)
*        The keyword about which help is required.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - This is the VMS version.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-SEP-1992 (DSB):
*        Original version modified from KAPPA routine GETHLP.FOR written
*        by MJC.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions

*  Global Variables:
      INCLUDE 'IRM_COM'        ! IRM common blocks.
*        MCM_CMD = CHARACTER * ( 80 ) (Write)
*           The text entered in response to the "Press RETURN
*           to continue..." prompt.
*        MCM_LTEXT = INTEGER (Read and Write)
*           Lines of text output this screenful.
*        MCM_TXTON = LOGICAL (Read)
*           If true, text output is enabled.
*        MCM_LTOP = INTEGER (Read)
*           Top line number for the scrolling region.
*        MCM_LBOT = INTEGER (Read)
*           Bottom line number for the scrolling region.
*        MCM_ANSI = LOGICAL (Read)
*           If true, an ANSI terminal is in use.
*        MCM_LUCMD = INTEGER (Read)
*           Logical-unit number of the command input.
*        MCM_LUTER = INTEGER (Read)
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
     :  IRM1_GHLPI,              ! Routine for reading help command
     :  HLP_HELP,                ! Interactive help
     :  IRM_PTOUT                ! Routine for outputting help 

      EXTERNAL
     :  IRM1_GHLPI,              ! Gets the help information
     :  IRM1_NAMTR,              ! Interactive help library-name
                                 ! translation
     :  IRM_PTOUT,               ! Outputs the help information
     :  IRM1_BLDAT               ! Initialise IRM common blocks.


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
      MCM_LTEXT = 0
      MCM_TXTON = .TRUE.
      MCM_CMD = ' '

*  Fixed for test purposes.  Note these are hardware specific.
      MCM_ANSI = .FALSE.
      MCM_LUCMD = 5
      MCM_LUTER = 6

*  Find the height and width of the screen.  Use the full screen area.
*  A zero or negative LBOT (which occurs when there is an error) will
*  suppress paging.
      CALL IRM1_SCRSZ( WIDTH, MCM_LBOT, STATUS )
      MCM_LTOP = 1

*  Get help.
*  =========

*  Reserve a logical unit number for accessing the help library.
      CALL IRM1_LUNIT( LUHLP, STATUS )

*  Set the flags to enable prompting.
      HFLAGS = 1

*  Initiate interactive help session.
      ISTAT = HLP_HELP( IRM_PTOUT, WIDTH, KEYWRD( :KWRDLN ), LUHLP,
     :                  HELPLB( :HLPLEN ), HFLAGS, IRM1_GHLPI, 
     :                  IRM1_NAMTR )

*  Watch for an error status.
      IF ( ISTAT .NE. 1 ) THEN
         CALL HLP_ERRMES( ISTAT, ERRMSG )
         CALL MSG_SETC( 'ERR', ERRMSG )
         CALL MSG_SETC( 'TOPIC', KEYWRD )
         CALL MSG_SETC( 'LIB', HELPLB )
         STATUS = SAI__ERROR
         CALL ERR_REP( 'IRM_GTHLP_ERR1',
     :   'IRM_GTHLP: Error accessing help on topic ^TOPIC in library'//
     :   ' ^LIB. Reason was "^ERR". ', STATUS )
      END IF

      END
