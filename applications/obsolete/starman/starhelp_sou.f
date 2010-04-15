CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C STARMANHELP --  Help system for Starman
C
C         a.j.penny                ral                   1994 Dec

      subroutine starmanhelp ( ierradam )

      implicit none

      integer    ierradam        !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_starmanhelp

      call starman_end ( ierradam )

      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_STARMANHELP.F
C
C  Contains:-
C  T_STARMANHELP    Uses the help system to read STARMAN_HELP_M.SHL
C  STHE_DOIT        Does the Starmanhelp
C  STHE_GETHELP     Prints help text from a library of help information.
C  ONE_SCRSZ  Get the width and height of the screen
C  STHE_PTHLPO      (Fn) Output a line of HELP
C  STHE_GTHLPI      (Fn) Gets one line input during an help session.
C  STHE_SREAD       Read, condition, paraphrase and classify a string.
C
C  alan penny                   ral            1994 Dec

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_STARMANHELP -- Uses the help system to read STARMAN_HELP_M.SHL
C
C  Copied straight from Malcolm Currie's KAPPA Help system
C  alan penny                   ral     1994 Dec

      subroutine t_starmanhelp

      implicit none
      include 'STARMAN_INC'
      include 'SAE_PAR'
C--
      integer status
Cbegin


      if ( ST_FAILED ) return

      status = SAI__OK
      call sthe_doit ( status )
      if ( status.ne.SAI__OK ) ST_FAILED = .true.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C STHE_DOIT -- Does the Starmanhelp
C
C     Displays help about STARMAN. The help information has classified
C     and alphabetical lists of commands, general information about
C     STARMAN and related material; it describes individual commands in
C     detail.
C
C     Here are some of the main options:
C        STARMANHELP
C           No parameter is given so the introduction and the top-level
C           help index is displayed.
C        STARMANHELP application/topic
C           This gives help about the specified application or topic.
C        STARMANHELP application/topic subtopic
C           This lists help about a subtopic of the specified
C           application or topic. The hierarchy of topics has a maximum
C           of four levels.
C        STARMANHELP Hints
C           This gives hints for new and intermediate users.
C        STARMANHELP SUMMARY
C           This shows a one-line summary of each application.
C        STARMANHELP CLASSIFIED classification
C           This lists a one-line summary of each application in the
C           given functionality classification.
C
C     Once in the help library, it can be navigated in the normal
C     way.  CTRL/Z to exit from any level, and <CR> to move up a
C     level in the hierarchy.
C
C  Copied straight from Malcolm Currie's KAPPA Help system
C  alan penny                   ral     1994 Dec

      subroutine sthe_doit ( status )

      implicit none
      include 'SAE_PAR'
      include 'DAT_PAR'
      include 'PAR_PAR'
      include 'PAR_ERR'

      integer status		!i/o: Starlink STATUS flag
C--
      INTEGER
     :  CHR_LEN                ! Length of character strings ignoring
                               ! trailing blanks
*  Local Constants:
      CHARACTER*14 LIBNAM      ! Name of the STARMAN help library
      PARAMETER ( LIBNAM = 'STARMAN_HELP_M' )
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
Cbegin

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Translate the environment variable/logical name.
      CALL PSX_GETENV ( LIBNAM, PATH, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Form the full library name.
         NC = CHR_LEN( PATH )
         LIBRAY = PATH( :NC )//'.shl'

*  Get topic and subtopics.
         CALL PAR_GET0C ( 'TOPIC', TOPIC(1), STATUS )
         CALL PAR_CANCL ( 'TOPIC', STATUS )

         CALL PAR_GET0C ( 'SUBTOPIC', TOPIC(2), STATUS )
         CALL PAR_CANCL ( 'SUBTOPIC', STATUS )

         CALL PAR_GET0C ( 'SUBSUBTOPIC', TOPIC(3), STATUS )
         CALL PAR_CANCL ( 'SUBSUBTOPIC', STATUS )

         CALL PAR_GET0C ( 'SUBSUBSUBTOPIC', TOPIC(4), STATUS )
         CALL PAR_CANCL ( 'SUBSUBSUBTOPIC', STATUS )

*  Concatenate the help topics into a single string
         HLPTXT = TOPIC ( 1 )
         NC = CHR_LEN ( TOPIC( 1 ) ) + 1
         DO I = 2, MAXLEV
            CALL CHR_APPND ( ' '//TOPIC( I ), HLPTXT, NC )
         END DO

*  Use a null string when something has gone wrong obtaining the
*  topics and sub-topics.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL ( STATUS )
            HLPTXT = '         '
         END IF

*  Get help text.
         CALL STHE_GETHLP ( LIBRAY, HLPTXT, STATUS )
      END IF

      END


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C STHE_GETHELP -- Prints help text from a library of help information.
C
C   Malcolm J. Currie (STARLINK)
C   Alan Penny                          RAL     1994 Dec

      SUBROUTINE STHE_GETHLP( HELPLB, KEYWRD, STATUS )

      implicit none
      include 'SAE_PAR'
      include 'DAT_PAR'
      include 'PAR_PAR'

      CHARACTER * ( * )
     :  HELPLB,
     :  KEYWRD

      INTEGER STATUS             ! Inherited global status

*    Use separate comon blocks for the character and other data types.
      COMMON /KPE_HLPCMD/ CMD, IOKF
      COMMON /KPE_HLPIO/ LREP, LHELP, HELPN, LTOP, LBOT, ANSI, LUCMD,
     :                   LUTERM
C--
*
*    Command line.
      CHARACTER CMD*80

*    Status returned from command (space = OK).
      CHARACTER IOKF*1

*    Report length option (0 = normal, 1 = short).
      INTEGER LREP

*    Lines of HELP output this screenful.
      INTEGER LHELP

*    HELP output enable/disable.
      LOGICAL HELPN

*    Top and bottom line numbers for scrolling region.
      INTEGER LTOP,LBOT

*    Flag: .TRUE. = ANSI terminal in use.
      LOGICAL ANSI

*    Command input logical-unit number.
      INTEGER LUCMD

*    Terminal output logical-unit number.
      INTEGER LUTERM

*  External References:
      INTEGER
     :  CHR_LEN,                 ! Character string length ignoring
                                 ! trailing blanks
     :  STHE_GTHLPI,             ! Routine for reading help command
     :  HLP_HELP,                ! Interactive help
     :  STHE_PTHLPO              ! Routine for outputting help

      EXTERNAL
     :  STHE_GTHLPI,             ! Gets the help information
     :  HLP_NAMETR,              ! Interactive help library-name
                                 ! translation
     :  STHE_PTHLPO              ! Outputs the help information

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

Cbegin

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
      ISTAT = HLP_HELP( STHE_PTHLPO, WIDTH, KEYWRD( 1:KWRDLN ), LUHLP,
     :                  HELPLB( 1:HLPLEN ), HFLAGS, STHE_GTHLPI,
     :                  HLP_NAMETR )

*  Watch for an error status.
      IF ( ISTAT .NE. 1 ) THEN
         CALL HLP_ERRMES( ISTAT, ERRMSG )
         CALL MSG_SETC( 'ERR', ERRMSG )
         CALL MSG_SETC( 'TOPIC', KEYWRD )
         CALL MSG_SETC( 'LIB', HELPLB )
         STATUS = SAI__ERROR
         CALL ERR_REP( 'STHE_GETHLP_ERR',
     :     'Error accessing help on topic ^TOPIC in library ^LIB. '/
     :     /'Reason was "^ERR". ', STATUS )
      END IF

      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C STHE_PTHLPO -- (Fn) Output a line of HELP
C     Analogue of VAX/VMS RTL routine LIB$PUT_OUTPUT.  Outputs one
C     line of HELP, waiting at the end of each screenful. VAX DEPENDENT!
C     However, the Vax extensions used are available on SUN and
C     DECstation compilers.  Note, inherited status is not used.
C
C  Invocation:
C     Called as first argument of LBR$OUTPUT_HELP or HLP_OUTHLP.
C
C  Copied straight from Malcolm Currie's KAPPA Help system
C  alan penny                   ral     1994 Dec

      INTEGER FUNCTION STHE_PTHLPO( STRING )

      IMPLICIT NONE

      include 'SAE_PAR'
      include 'DAT_PAR'
      include 'PAR_PAR'

C     INTEGER STHE_PTHLPO 	!o:If line output correctly, set to 1

      CHARACTER STRING * ( * )	!i: Line of text to be output to the screen.

*    Use separate comon blocks for the character and other data types.
      COMMON /KPE_HLPCMD/ CMD, IOKF
      COMMON /KPE_HLPIO/ LREP, LHELP, HELPN, LTOP, LBOT, ANSI, LUCMD,
     :                   LUTERM

C--
      CHARACTER CMD*80

*    Status returned from command (space = OK).
      CHARACTER IOKF*1

*    Report length option (0 = normal, 1 = short).
      INTEGER LREP

*    Lines of HELP output this screenful.
      INTEGER LHELP

*    HELP output enable/disable.
      LOGICAL HELPN

*    Top and bottom line numbers for scrolling region.
      INTEGER LTOP,LBOT

*    Flag: .TRUE. = ANSI terminal in use.
      LOGICAL ANSI

*    Command input logical-unit number.
      INTEGER LUCMD

*    Terminal output logical-unit number.
      INTEGER LUTERM

*

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

Cbegin

*  Proceed unless HELP suppressed.
      IF ( HELPN ) THEN

*  Check if the scrolling region is full or there is scrolling.
         IF ( LHELP .GE. LBOT-LTOP-2 .AND. LBOT .GT. LTOP ) THEN

*  It is:  therefore issue a prompt.  **Note the Vax specific $ format
*  specifier.**
            WRITE ( LUTERM,
     :        '(/1X,''Press RETURN to continue ...'',$)' )

*  Get a line of uppercase input.
            CALL STHE_SREAD ( LUCMD, BUFA, BUFB, CMD, J )

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
      STHE_PTHLPO = 1


      END


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C STHE_GTHLPI -- (Fn) Gets one line input during an help session.
C     Analogue of VAX/VMS RTL routine LIB$GET_INPUT.  During a
C     HELP session, gets one line of input. VAX DEPENDENT!
C     However, the Vax extensions used are available on SUN and
C     DECstation compilers.  Note, inherited status is not used.
C
C     Called as sixth argument of LBR$OUTPUT_HELP or seventh of
C     HLP_OUTHLP.
C
C  Copied straight from Pat Wallace and Malcolm Currie's KAPPA Help system
C  alan penny                   ral     1994 Dec

      INTEGER FUNCTION STHE_GTHLPI( STRING, PROMPT, LINCH )

      IMPLICIT NONE

      include 'SAE_PAR'
      include 'DAT_PAR'
      include 'PAR_PAR'

C     integer   sthe_gthlpi		!o: Set to 1 if output ok

      CHARACTER PROMPT * ( * )		!i: Prompt string
      CHARACTER STRING * ( * )		!i: Line of text to be output to the screen.
      INTEGER LINCH			!i: Length of the input string in characters.

      COMMON /KPE_HLPCMD/ CMD, IOKF
      COMMON /KPE_HLPIO/ LREP, LHELP, HELPN, LTOP, LBOT, ANSI, LUCMD,
     :                   LUTERM
C--
*    Command line.
      CHARACTER CMD*80

*    Status returned from command (space = OK).
      CHARACTER IOKF*1

*    Report length option (0 = normal, 1 = short).
      INTEGER LREP

*    Lines of HELP output this screenful.
      INTEGER LHELP

*    HELP output enable/disable.
      LOGICAL HELPN

*    Top and bottom line numbers for scrolling region.
      INTEGER LTOP,LBOT

*    Flag: .TRUE. = ANSI terminal in use.
      LOGICAL ANSI

*    Command input logical-unit number.
      INTEGER LUCMD

*    Terminal output logical-unit number.
      INTEGER LUTERM


*        CMD = CHARACTER * ( 80 ) (Write)
*           The command line.
*        HELPN = LOGICAL (Read)
*           If true, help output is enabled.
*        LUCMD = INTEGER (Read)
*           Logical-unit number of the command input.
*        LUTERM = INTEGER (Read)
*           Logical-unit number of the terminal output.


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

Cbegin

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
            CALL STHE_SREAD ( LUCMD, BUFA, BUFB, STRING, J )

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
      STHE_GTHLPI = 1

      END


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C STHE_SREAD - Read, condition, paraphrase and classify a string.
C  A string is input from the nominated I/O unit LU (in A format)
C  and, unless end-of-file was detected, three versions of it are
C  created.
C
C  Version 1 (V1) is as input except that spaces replace any
C  non-printing characters (e.g. TABs).
C
C Version 2 (V2) is the same as version 1 except that any leading
C  spaces or trailing comments are removed.
C
C  Version 3 (V3) is the same as version 2 except that outside
C  "string arguments", lowercase is converted to uppercase.
C
C  Given:
C     LU         i         I/O unit for input
C
C  Returned:
C     V1         c*(*)     string with TABS etc eliminated
C     V2         c*(*)     string left justified and without comments
C     JSTAT      i         status:  -1 = end-of-file
C                                    0 = not a comment
C                                   +1 = comment
C
C  Notes:
C
C  1)  "String arguments" are groups of characters enclosed by
C      pairs of either single or double quotes.  The contents
C      are protected against conversion to uppercase.  The delimiters
C      are ultimately removed by means of the GETSTR routine.
C
C  2)  A '!' character is only interpreted as start of comment
C      if it is not within a string argument.
C
C  3)  If end-of-file is detected, spaces are returned for V1,
C      V2 and V3.
C
C  4)  The algorithm uses several characters which are outside
C      the ANSI FORTRAN 77 set (exclamation point, double quote,
C      tilde, lowercase).  Different machine types may need a
C      rewrite.
C
C  5)  This subprogram was suggested by the RDNEXT routine of
C      Russell Owen (UW/ARC).
C
C  Called:   CHR_UCASE
C
C  P.T.Wallace   Starlink   27 April 1988
C  Malcolm J. Currie   1988 Sep 9  - changed UC to CHR_UCASE for KAPPA use
C  alan penny                   ral     1994 Dec

      SUBROUTINE STHE_SREAD (LU, V1, V2, V3, JSTAT)

      IMPLICIT NONE

      INTEGER LU
      CHARACTER*(*) V1,V2,V3
      INTEGER JSTAT
C--
      INTEGER L1,L2,I1,I2
      CHARACTER CDELIM,C
      LOGICAL COMENT
Cbegin


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
