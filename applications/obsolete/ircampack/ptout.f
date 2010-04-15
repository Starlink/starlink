      INTEGER FUNCTION PTOUT( STRING )
*+
*  Name:
*     PTOUT

*  Purpose:
*     Outputs one line of text, waiting at end of each screenful.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     ISTAT = PTOUT( STRING )

*  Description:
*     Analogue of VAX/VMS RTL routine LIB$PUT_OUTPUT.  Outputs one
*     line of text, waiting at the end of each screenful. VAX DEPENDENT!
*     However, the Vax extensions used are available on SUN and
*     DECstation compilers.  Note, inherited status is not used.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The line of text to be output to the screen.

*  Returned Value:
*     PTOUT = INTEGER
*        The status.  If the line was output correctly a value of 1 is
*        returned, otherwise 0 is returned.

*  Authors:
*     (DSB) David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-OCT-1993 (DSB):
*        Original version, modified from IRAS90 routine IRM_PTOUT.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Standard SAE constants.

*  Global Variables:
      INCLUDE 'IRC_COM'        ! IRCAMPACK common blocks.
*        IRC_CMD = CHARACTER * ( 80 ) (Read and Write)
*           The text entered in response to the "Press RETURN
*           to continue..." prompt.
*        IRC_LTEXT = INTEGER (Read and Write)
*           Lines of text output this screenful.
*        IRC_TXTON = LOGICAL (Read and Write)
*           If true, text output is enabled.
*        IRC_LTOP = INTEGER (Read and Write)
*           Top line number for the scrolling region.
*        IRC_LBOT = INTEGER (Read and Write)
*           Bottom line number for the scrolling region.
*        IRC_ANSI = LOGICAL (Read and Write)
*           If true, an ANSI terminal is in use.
*        IRC_LUCMD = INTEGER (Read and Write)
*           Logical-unit number of the command input.
*        IRC_LUTER = INTEGER (Read and Write)
*           Logical-unit number of the terminal output.

*  Arguments Given:
      CHARACTER STRING * ( * )

*  External References:
      EXTERNAL BLDAT             ! Initialise IRCAMPACK common blocks.
      INTEGER  CHR_LEN           ! Character string length ignoring
                                 ! trailing blanks

*  Local Variables:
      INTEGER
     :  J,                       ! Local status
     :  JT,                      ! First non-space column of line number
                                 ! CT
     :  LINCH,			 ! Number of characters in the line to
                                 ! output
     :  STATUS,                  ! Local status value.
     :  WIDTH                    ! Screen width in characters.

      CHARACTER
     :  BUFA * ( 132 ),          ! Work space
     :  BUFB * ( 132 ),          ! Work space
     :  CT * ( 2 )               ! Top line number

*.

*  Set returned value to a bad value.
      PTOUT = 0

*  If not already done, initialise COMMON-block variables.
      IF( IRC_LUTER .EQ. -1 ) THEN

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

      END IF

*  Proceed unless output of text is suppressed
      IF ( IRC_TXTON ) THEN

*  Check if the scrolling region is full or there is scrolling.
         IF ( IRC_LTEXT .GE. IRC_LBOT - IRC_LTOP - 2 .AND.
     :        IRC_LBOT .GT. IRC_LTOP ) THEN

*  It is:  therefore issue a prompt.  **Note the Vax specific $ format
*  specifier.**
            WRITE ( IRC_LUTER,
     :        '(/1X,''Press RETURN to continue ...'',$)' )

*  Get a line of uppercase input.
            CALL SREAD ( IRC_LUCMD, BUFA, BUFB, IRC_CMD, J )

*  Reset the height and width of the screen in case they have been
*  changed.
            CALL SCRSZ( WIDTH, IRC_LBOT, STATUS )

*  Treat a comment or EOF as blank input.
            IF ( J .NE. 0 ) IRC_CMD = ' '

*  Skip if an EOF is encountered.
            IF ( J .LT. 0 ) GO TO 900

*  If non-blank input, suppress further output.
            IF ( IRC_CMD .NE. ' ' ) GO TO 900

*  Reset the line count.
            IRC_LTEXT = 0

         END IF

*  First line about to be output?
         IF ( IRC_LTEXT .LT. 1 ) THEN

*  Is the output going to an ANSI terminal?
            IF ( IRC_ANSI ) THEN

*  Yes, so clear and home cursor.
               WRITE ( CT,'(I2)') IRC_LTOP
               IF ( CT(:1) .EQ. ' ' ) THEN
                  JT = 2
               ELSE
                  JT = 1
               END IF

               WRITE ( IRC_LUTER, '($,''+'',A)' )
     :                     CHAR(27)//'[2J'//
     :                     CHAR(27)//'['//CT(JT:)//';1H'  ! VAX specific

            ELSE

*  Non-ANSI terminal: output some blank lines.
               WRITE ( IRC_LUTER, '(//)' )

            END IF

         END IF

*  Find the length of the output.
         LINCH = CHR_LEN( STRING )

*  Output the line of text.
         IF( LINCH .GT. 0 ) THEN
            WRITE ( IRC_LUTER, '(1X,A)' ) STRING( :LINCH )
         ELSE
            WRITE ( IRC_LUTER, '(1X,A)' ) ' '
         END IF

*  Increment the line count.
         IRC_LTEXT = IRC_LTEXT + 1

      END IF

*  Wrap up.

      GO TO 9900

*  For CTRL/Z or non-blank input: suppress further output.

 900  CONTINUE
      IRC_TXTON = .FALSE.

*  Set the status OK, and exit.

 9900 CONTINUE
      PTOUT = 1


      END
