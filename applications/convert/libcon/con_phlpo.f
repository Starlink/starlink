      INTEGER FUNCTION CON_PHLPO( STRING )
*+
*  Name:
*     CON_PHLPO

*  Purpose:
*     Outputs one line of HELP, waiting at end of each screenful.

*  Language:
*     Vax Fortran 77

*  Invocation:
*     Called as first argument of LBR$OUTPUT_HELP or HLP_OUTHLP.

*  Description:
*     Analogue of VAX/VMS RTL routine LIB$PUT_OUTPUT.  Outputs one
*     line of HELP (VMS or portable HELP), waiting at the end of each
*     screenful.  Note, inherited status is not used.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The line of text to be output to the screen.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     CON_PHLPO = INTEGER
*        The status.  If the line was output correctly a value of 1 is
*        returned.

*  Authors:
*     (MJC) Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1993 July 30 (MJC):
*        Original based on KAPPA's PTHLPO.
*     {enter_any_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions

*  Global Variables:
      INCLUDE 'HLPCMD'           ! KAPPA help I/O
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
      CHARACTER STRING * ( * )

*  External References:
      INTEGER CHR_LEN            ! Character string length ignoring
                                 ! trailing blanks

*  Local Variables:
      CHARACTER BUFA * ( 132 )   ! Work space
      CHARACTER BUFB * ( 132 )   ! Work space
      CHARACTER CT * ( 2 )       ! Top line number
      INTEGER J                  ! Local status
      INTEGER JT                 ! First non-space column of line number
                                 ! CT
      INTEGER LINCH              ! Number of characters in the line to
                                 ! output

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
            CALL CON_SREAD( LUCMD, BUFA, BUFB, CMD, J )

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
      CON_PHLPO = 1

      END
