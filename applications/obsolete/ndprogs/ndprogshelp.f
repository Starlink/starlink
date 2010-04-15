
      SUBROUTINE NDPROGSHELP( STATUS )
*+
*  Name:
*     NDPROGSHELP

*  Purpose:
*     Gives help about the NDPROGS package.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL NDPROGSHELP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Displays help about NDPROGS using the NDPROGSHELP.SHL library.
*
*     Here are some of the main options:
*        NDPROGSHELP
*           No parameter is given so the introduction and the top-level
*           help index is displayed.
*        NDPROGSHELP application
*           Gives help on the named application.
*
*     Once in the help library, it can be navigated in the normal
*     way.  CTRL/Z to exit from any level, and <CR> to move up a
*     level in the hierarchy.

*  Usage:
*     NDPROGSHELP [TOPIC] [SUBTOPIC] [SUBSUBTOPIC] [SUBSUBSUBTOPIC]

*  ADAM Parameters:
*     TOPIC = LITERAL (Read)
*        Topic for which help is to be given. [" "]
*     SUBTOPIC = LITERAL (Read)
*        Subtopic for which help is to be given. [" "]
*     SUBSUBTOPIC = LITERAL (Read)
*        Subsubtopic for which help is to be given. [" "]
*     SUBSUBSUBTOPIC = LITERAL (Read)
*        Subsubsubtopic for which help is to be given. [" "]

*  Algorithm:
*     -  Check for error on entry; return if not o.k.
*     -  Obtain topic and subtopics required.  Concatenate them
*        separated by spaces.
*     -  If an error has occurred set all topics to be null.
*     -  Get help on required topic.

*  Implementation Status:
*     -  Uses the portable help system.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1994 November 22 (GJP):
*          Taken from the CCDHELP application - originally
*          written by Malcolm Currie and Peter Draper.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! SSE global definitions
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_PAR'
      INCLUDE 'PAR_ERR'        ! Parameter-system errors

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER
     :  CHR_LEN                ! Length of character strings ignoring
                               ! trailing blanks

*  Local Constants:
      CHARACTER*12 LIBNAM      ! Name of the NDPROGS help library
      PARAMETER ( LIBNAM = 'NDPROGS_HELP' )
      INTEGER MAXLEV           ! Maximum number of help levels
      PARAMETER ( MAXLEV = 4 )

*  Local Variables:
      CHARACTER*19
     :  HLPTXT*80,             ! Composite command
     :  LIBRAY*132,            ! Library name and path
     :  PATH*122,              ! Library path
     :  TOPIC( MAXLEV )        ! Name of the topic required

      INTEGER
     :  I,                     ! Loop counter
     :  NC                     ! Number of characters in the help string
                               ! less trailing blanks
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Translate the environment variable/logical name.
      CALL PSX_GETENV( LIBNAM, PATH, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Form the full library name.
         NC = CHR_LEN( PATH )
         LIBRAY = PATH( :NC )//'.shl'

*  Get topic and subtopics.
         CALL PAR_GET0C( 'TOPIC', TOPIC(1), STATUS )
         CALL PAR_CANCL( 'TOPIC', STATUS )

         CALL PAR_GET0C( 'SUBTOPIC', TOPIC(2), STATUS )
         CALL PAR_CANCL( 'SUBTOPIC', STATUS )

         CALL PAR_GET0C( 'SUBSUBTOPIC', TOPIC(3), STATUS )
         CALL PAR_CANCL( 'SUBSUBTOPIC', STATUS )

         CALL PAR_GET0C( 'SUBSUBSUBTOPIC', TOPIC(4), STATUS )
         CALL PAR_CANCL( 'SUBSUBSUBTOPIC', STATUS )

*  Concatenate the help topics into a single string
         HLPTXT = TOPIC( 1 )
         NC = CHR_LEN( TOPIC( 1 ) ) + 1
         DO I = 2, MAXLEV
            CALL CHR_APPND( ' '//TOPIC( I ), HLPTXT, NC )
         END DO

*  Use a null string when something has gone wrong obtaining the
*  topics and sub-topics.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            HLPTXT = '         '
         END IF

*  Get help text.
         CALL GETHLP( LIBRAY, HLPTXT, STATUS )
      END IF

      END


      SUBROUTINE GETHLP( HELPLB, KEYWRD, STATUS )
*+
*  Name:
*     GETHLP

*  Purpose:
*     Prints help text from a library of help information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GETHLP( HELPLB, KEYWRD, STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Prints help text from an help library. A specific keyword/topic
*     can be specified.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
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
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_PAR'

*  Global Variables:
      INCLUDE 'hlpcmd.inc'           ! KAPPA help I/O
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
     :  GTHLPI,                  ! Routine for reading help command
     :  HLP_HELP,                ! Interactive help
     :  PTHLPO                   ! Routine for outputting help

      EXTERNAL
     :  GTHLPI,                  ! Gets the help information
     :  HLP_NAMETR,              ! Interactive help library-name
                                 ! translation
     :  PTHLPO                   ! Outputs the help information

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
      CALL KPG1_SCRSZ( WIDTH, LBOT, STATUS )
      LTOP = 1

*  Get help.
*  =========

*  Set logical unit number for help.  This is hardware dependent.
      LUHLP = 1

*  Set the flags to enable prompting.
      HFLAGS = 1

*  Initiate interactive help session.
      ISTAT = HLP_HELP( PTHLPO, WIDTH, KEYWRD( 1:KWRDLN ), LUHLP,
     :                  HELPLB( 1:HLPLEN ), HFLAGS, GTHLPI, HLP_NAMETR )

*  Watch for an error status.
      IF ( ISTAT .NE. 1 ) THEN
         CALL HLP_ERRMES( ISTAT, ERRMSG )
         CALL MSG_SETC( 'ERR', ERRMSG )
         CALL MSG_SETC( 'TOPIC', KEYWRD )
         CALL MSG_SETC( 'LIB', HELPLB )
         STATUS = SAI__ERROR
         CALL ERR_REP( 'GETHLP_ERR',
     :     'Error accessing help on topic ^TOPIC in library ^LIB. '/
     :     /'Reason was "^ERR". ', STATUS )
      END IF

      END


      INTEGER FUNCTION GTHLPI( STRING, PROMPT, LINCH )
*+
*  Name:
*     GTHLPI

*  Purpose:
*     Gets one line input during an help session.

*  Language:
*     Vax Fortran 77

*  Invocation:
*     Called as sixth argument of LBR$OUTPUT_HELP or seventh of
*     HLP_OUTHLP.

*  Description:
*     Analogue of VAX/VMS RTL routine LIB$GET_INPUT.  During a
*     HELP session, gets one line of input. VAX DEPENDENT!
*     However, the Vax extensions used are available on SUN and
*     DECstation compilers.  Note, inherited status is not used.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The line of text to be output to the screen.
*     PROMPT = CHARACTER * ( * ) (Given)
*         Prompt string.
*     LINCH = INTEGER (Read)
*         Length of the input string in characters.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     GTHLPI = INTEGER
*        The status.  If the line was inpput correctly a value of 1 is
*        returned.

*  Authors:
*     (PTW) P.T.Wallace (STARLINK)
*     (MJC) Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1988 April 27 (PTW):
*        Original called GET_INPUT.
*     1988 September 7 (MJC):
*        Modified to work under ADAM in KAPPA.
*     1992 June 22 (MJC):
*        Converted to SST prologue and documented global parameters.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! SSE global definitions
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_PAR'

*  Global Variables:
      INCLUDE 'hlpcmd.inc'           ! KAPPA help I/O
*        CMD = CHARACTER * ( 80 ) (Write)
*           The command line.
*        HELPN = LOGICAL (Read)
*           If true, help output is enabled.
*        LUCMD = INTEGER (Read)
*           Logical-unit number of the command input.
*        LUTERM = INTEGER (Read)
*           Logical-unit number of the terminal output.

*  Arguments Given:
      CHARACTER PROMPT * ( * )

*  Arguments Returned:
      CHARACTER STRING * ( * )
      INTEGER LINCH

*  External References:
      INTEGER
     :  CHR_LEN                  ! Character string length ignoring
                                 ! trailing blanks

*  Local Variables:
      INTEGER
     :  J                        ! Local status

      CHARACTER
     :  BUFA * ( 132 ),          ! Work space
     :  BUFB * ( 132 )           ! Work space

*-

*  Was Something entered during the paged output?
      IF ( CMD .NE. ' ' ) THEN

*  Yes: use it, forget it, reenable output
         STRING = CMD
         CMD = ' '
         HELPN = .TRUE.

      ELSE

*  No: output suppressed?
         IF ( HELPN ) THEN

*  Output enabled: write prompt, if any.

            IF ( PROMPT .NE. '  ' )
     :        WRITE ( LUTERM, '(1X,A,$)' ) PROMPT

*  Get a line of uppercase input.
            CALL SREAD ( LUCMD, BUFA, BUFB, STRING, J )

*  Treat comment or EOF as blank input.
            IF ( J .GT. 0 ) STRING = ' '

*  Skip if end of file (CTRL/Z).
            IF ( J .LT. 0 ) GO TO 900

         ELSE

*  HELP suppressed: return blank.
            STRING = ' '

         END IF

      END IF

*  Determine length of input, ignoring trailing blanks.
      LINCH =  CHR_LEN( STRING )

*  Reset the line count.
      LHELP =  0

*  Wrap up.
      GO TO 9900

*  CTRL/Z handling---suppress further output and return blank line.
 900  CONTINUE
      HELPN = .FALSE.
      STRING = ' '
      LINCH = 0

*  Set the status and exit.
 9900 CONTINUE
      GTHLPI = 1

      END


      INTEGER FUNCTION PTHLPO( STRING )
*+
*  Name:
*     PTHLPO

*  Purpose:
*     Outputs one line of HELP, waiting at end of each screenful.

*  Language:
*     Vax Fortran 77

*  Invocation:
*     Called as first argument of LBR$OUTPUT_HELP or HLP_OUTHLP.

*  Description:
*     Analogue of VAX/VMS RTL routine LIB$PUT_OUTPUT.  Outputs one
*     line of HELP, waiting at the end of each screenful. VAX DEPENDENT!
*     However, the Vax extensions used are available on SUN and
*     DECstation compilers.  Note, inherited status is not used.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The line of text to be output to the screen.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     PTHLPO = INTEGER
*        The status.  If the line was output correctly a value of 1 is
*        returned.

*  Authors:
*     (PTW) P.T.Wallace (STARLINK)
*     (MJC) Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1988 April 27 (PTW):
*        Original called PUT_OUTPUT.
*     1988 September 7 (MJC):
*        Modified to work under ADAM in KAPPA.
*     1992 June 22 (MJC):
*        Added a constraint to output with paging when the screen height
*        and width could not be determined.  Converted to SST prologue
*        and documented global parameters.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! SSE global definitions
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_PAR'

*  Global Variables:
      INCLUDE 'hlpcmd.inc'           ! KAPPA help I/O
*        CMD = CHARACTER * ( 80 ) (Write)
*           The command line.
*        LHELP = INTEGER (Read and Write)
*           Lines of help output this screenful.
*        HELPN = LOGICAL (Read)
*           If true, help output is enabled.
*        LTOP = INTEGER (Read)
*           Top line number for the scrolling region.
*        LBOT = INTEGER (Read)
*           Bottom line number for the scrolling region.  If
*        ANSI = LOGICAL (Read)
*           If true, an ANSI terminal is in use.
*        LUCMD = INTEGER (Read)
*           Logical-unit number of the command input.
*        LUTERM = INTEGER (Read)
*           Logical-unit number of the terminal output.

*  Arguments Given:
      CHARACTER
     :  STRING * ( * )

*  External References:
      INTEGER
     :  CHR_LEN                  ! Character string length ignoring
                                 ! trailing blanks

*  Local Variables:
      INTEGER
     :  J,                       ! Local status
     :  JT,                      ! First non-space column of line number
                                 ! CT
     :  LINCH                    ! Number of characters in the line to
                                 ! output

      CHARACTER
     :  BUFA * ( 132 ),          ! Work space
     :  BUFB * ( 132 ),          ! Work space
     :  CT * ( 2 )               ! Top line number

*.

*  Proceed unless HELP suppressed.
      IF ( HELPN ) THEN

*  Check if the scrolling region is full or there is scrolling.
         IF ( LHELP .GE. LBOT-LTOP-2 .AND. LBOT .GT. LTOP ) THEN

*  It is:  therefore issue a prompt.  **Note the Vax specific $ format
*  specifier.**
            WRITE ( LUTERM,
     :        '(/1X,''Press RETURN to continue ...'',$)' )

*  Get a line of uppercase input.
            CALL SREAD ( LUCMD, BUFA, BUFB, CMD, J )

*  Treat a comment or EOF as blank input.
            IF ( J .NE. 0 ) CMD = ' '

*  Skip if an EOF is encountered.
            IF ( J .LT. 0 ) GO TO 900

*  If non-blank input, suppress further output.
            IF ( CMD .NE. ' ' ) GO TO 900

*  Reset the line count.
            LHELP = 0

         END IF

*  First line about to be output?
         IF ( LHELP .LT. 1 ) THEN

*  Is the output going to an ANSI terminal?
            IF ( ANSI ) THEN

*  Yes, so clear and home cursor.
               WRITE ( CT,'(I2)') LTOP
               IF ( CT(:1) .EQ. ' ' ) THEN
                  JT = 2
               ELSE
                  JT = 1
               END IF

               WRITE ( LUTERM, '($,''+'',A)' )
     :                     CHAR(27)//'[2J'//
     :                     CHAR(27)//'['//CT(JT:)//';1H'  ! VAX specific

            ELSE

*  Non-ANSI terminal: output some blank lines.
               WRITE ( LUTERM, '(//)' )

            END IF

         END IF

*       Find the length of the output.
         LINCH = CHR_LEN( STRING )

*       Output the line of help.
         WRITE ( LUTERM, '(1X,A)' ) STRING( :LINCH )

*       Increment the line count.
         LHELP = LHELP + 1

      END IF

*    Wrap up.

      GO TO 9900

*    For CTRL/Z or non-blank input: suppress further output.

 900  CONTINUE
      HELPN = .FALSE.

*    Set the status and exit.

 9900 CONTINUE
      PTHLPO = 1

      END


*+ SREAD - Read, condition, paraphrase and classify a string.

      SUBROUTINE SREAD (LU, V1, V2, V3, JSTAT)

*
*  - - - - - -
*   S R E A D
*  - - - - - -
*
*  Read, condition, paraphrase and classify a string.
*
*  A string is input from the nominated I/O unit LU (in A format)
*  and, unless end-of-file was detected, three versions of it are
*  created.
*
*  Version 1 (V1) is as input except that spaces replace any
*  non-printing characters (e.g. TABs).
*
*  Version 2 (V2) is the same as version 1 except that any leading
*  spaces or trailing comments are removed.
*
*  Version 3 (V3) is the same as version 2 except that outside
*  "string arguments", lowercase is converted to uppercase.
*
*  Given:
*     LU         i         I/O unit for input
*
*  Returned:
*     V1         c*(*)     string with TABS etc eliminated
*     V2         c*(*)     string left justified and without comments
*     JSTAT      i         status:  -1 = end-of-file
*                                    0 = not a comment
*                                   +1 = comment
*
*  Notes:
*
*  1)  "String arguments" are groups of characters enclosed by
*      pairs of either single or double quotes.  The contents
*      are protected against conversion to uppercase.  The delimiters
*      are ultimately removed by means of the GETSTR routine.
*
*  2)  A '!' character is only interpreted as start of comment
*      if it is not within a string argument.
*
*  3)  If end-of-file is detected, spaces are returned for V1,
*      V2 and V3.
*
*  4)  The algorithm uses several characters which are outside
*      the ANSI FORTRAN 77 set (exclamation point, double quote,
*      tilde, lowercase).  Different machine types may need a
*      rewrite.
*
*  5)  This subprogram was suggested by the RDNEXT routine of
*      Russell Owen (UW/ARC).
*
*  Called:   CHR_UCASE
*
*  P.T.Wallace   Starlink   27 April 1988
*  Malcolm J. Currie   1988 Sep 9  - changed UC to CHR_UCASE for
*                                    KAPPA use
*
*+

      IMPLICIT NONE

      INTEGER LU
      CHARACTER*(*) V1,V2,V3
      INTEGER JSTAT

      INTEGER L1,L2,I1,I2
      CHARACTER CDELIM,C
      LOGICAL COMENT



*  Obtain the string lengths
      L1=LEN(V1)
      L2=LEN(V2)

*  Initialise second pointer
      I2=0

*  Preset the first two strings to spaces
      V1=' '
      V2=' '

*  Reset the "string argument has started" flag
      CDELIM=' '

*  Reset the "comment has started" flag
      COMENT=.FALSE.

*  Preset the status to EOF
      JSTAT=-1

*  Read the string
      READ (LU,'(A)',END=9999) V1

*  Not EOF:  preset the status to "comment"
      JSTAT=1

*  Step along the raw input string
      DO I1=1,L1

*     Next character
         C=V1(I1:I1)

*     Replace non-printing characters with spaces
         IF (C.LT.' '.OR.C.GT.'~') C=' '

*     "String argument" delimiter character?
         IF (.NOT.COMENT.AND.
     :       C.EQ.'"'.OR.C.EQ.'''') THEN

*        Yes:  leading or trailing?
            IF (CDELIM.EQ.' ') THEN

*           Leading:  remember
               CDELIM=C

            ELSE IF (C.EQ.CDELIM) THEN

*           Trailing:  remember
               CDELIM=' '

            END IF

         END IF

*     Store character in first string
         V1(I1:I1)=C

*     Start of comment?
         IF (CDELIM.EQ.' '.AND.C.EQ.'!') COMENT=.TRUE.

*     Are we within a comment?
         IF (.NOT.COMENT) THEN

*        No:  leading space?
            IF (I2.NE.0.OR.C.NE.' ') THEN

*           No:  reset "comment" flag
               JSTAT=0

*           Store character in string 2 unless already filled
               IF (I2.LT.L2) THEN
                  I2=I2+1
                  V2(I2:I2)=C
               END IF
            END IF
         END IF
      END DO

*  Generate uppercase version and exit
 9999 CONTINUE
      V3=V2
      CALL CHR_UCASE(V3)

      END


      SUBROUTINE KPG1_SCRSZ( WIDTH, HEIGHT, STATUS )
*+
*  Name:
*     KPG1_SCRSZ

*  Purpose:
*     Interrogates the system to find the width and height of the screen
*     on which it is running.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_SCRSZ( WIDTH, HEIGHT, STATUS )

*  Description:
*     This routine interrogates the system to find the width and height of the screen
*     on which it is running.  Should an error occur or the width is
*     not positive, set to the default of 80 characters by 24 lines.


*  Arguments:
*     WIDTH = INTEGER (Returned)
*        The width of the screen in characters.
*     HEIGHT = INTEGER (Returned)
*        The height of the screen in lines.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This is the UNIX version.

*  [optional_subroutine_items]...
*  Authors:
*     AJC: Alan Chipperfield (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1992 June 17 (AJC):
*        Original version.
*     1992 June 22 (MJC):
*        Renamed and modified for KAPPA use.  Converted to SST prologue.
*     1993 March 14 (PDRAPER):
*        Removed kpg1_trmsz call and replaced with subpar_trmsz.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_PAR'

*  Arguments Returned:
      INTEGER WIDTH
      INTEGER HEIGHT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL SUBPAR_TRMSZ
      INTEGER SUBPAR_TRMSZ         ! C function for actually obtaining
                                 ! the height and width

*  Local Variables:
      INTEGER ISTAT              ! Local status

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Inquire the terminal size.
      ISTAT = SUBPAR_TRMSZ( WIDTH, HEIGHT )

*  If this failed to get a good value, set default which causes no
*  paging.
      IF ( ( ISTAT .NE. 1 ) .OR. ( WIDTH .LE. 0 ) ) THEN
         WIDTH = 80
         HEIGHT = 0
      END IF

      END
* @(#)kpg1_scrsz.f	1.1     11/8/93     1
