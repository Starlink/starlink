      INTEGER FUNCTION IRM_PTOUT( STRING )
*+
*  Name:
*     IRM_PTOUT

*  Purpose:
*     Outputs one line of text, waiting at end of each screenful.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     ISTAT = IRM_PTOUT( STRING )

*  Description:
*     Analogue of VAX/VMS RTL routine LIB$PUT_OUTPUT.  Outputs one
*     line of text, waiting at the end of each screenful. VAX DEPENDENT!
*     However, the Vax extensions used are available on SUN and
*     DECstation compilers.  Note, inherited status is not used.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The line of text to be output to the screen.

*  Returned Value:
*     IRM_PTOUT = INTEGER
*        The status.  If the line was output correctly a value of 1 is
*        returned, otherwise 0 is returned.

*  Authors:
*     (PTW) P.T.Wallace (STARLINK)
*     (MJC) Malcolm J. Currie (STARLINK)
*     (DSB) David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-SEP-1992 (DSB):
*        Original version, modified from KAPPA routine PTHLPO written
*        by PTW and MJC.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Standard SAE constants.

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

*  Arguments Given:
      CHARACTER STRING * ( * )

*  External References:
      EXTERNAL IRM1_BLDAT        ! Initialise IRM common blocks.
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
      IRM_PTOUT = 0

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

*  Find the height and width of the screen.  Use the full screen area.
*  A zero or negative LBOT (which occurs when there is an error) will
*  suppress paging.
         CALL ONE_SCRSZ( WIDTH, MCM_LBOT, STATUS )
         MCM_LTOP = 1

      END IF

*  Proceed unless output of text is suppressed
      IF ( MCM_TXTON ) THEN

*  Check if the scrolling region is full or there is scrolling.
         IF ( MCM_LTEXT .GE. MCM_LBOT - MCM_LTOP - 2 .AND.
     :        MCM_LBOT .GT. MCM_LTOP ) THEN

*  It is:  therefore issue a prompt.  **Note the Vax specific $ format
*  specifier.**
            WRITE ( MCM_LUTER,
     :        '(/1X,''Press RETURN to continue ...'',$)' )

*  Get a line of uppercase input.
            CALL IRM1_SREAD ( MCM_LUCMD, BUFA, BUFB, MCM_CMD, J )

*  Reset the height and width of the screen in case they have been
*  changed.
            CALL ONE_SCRSZ( WIDTH, MCM_LBOT, STATUS )

*  Treat a comment or EOF as blank input.
            IF ( J .NE. 0 ) MCM_CMD = ' '

*  Skip if an EOF is encountered.
            IF ( J .LT. 0 ) GO TO 900

*  If non-blank input, suppress further output.
            IF ( MCM_CMD .NE. ' ' ) GO TO 900

*  Reset the line count.
            MCM_LTEXT = 0

         END IF

*  First line about to be output?
         IF ( MCM_LTEXT .LT. 1 ) THEN

*  Is the output going to an ANSI terminal?
            IF ( MCM_ANSI ) THEN

*  Yes, so clear and home cursor.
               WRITE ( CT,'(I2)') MCM_LTOP
               IF ( CT(:1) .EQ. ' ' ) THEN
                  JT = 2
               ELSE
                  JT = 1
               END IF

               WRITE ( MCM_LUTER, '($,''+'',A)' )
     :                     CHAR(27)//'[2J'//
     :                     CHAR(27)//'['//CT(JT:)//';1H'  ! VAX specific

            ELSE

*  Non-ANSI terminal: output some blank lines.
               WRITE ( MCM_LUTER, '(//)' )

            END IF

         END IF

*  Find the length of the output.
         LINCH = CHR_LEN( STRING )

*  Output the line of text.
         IF( LINCH .GT. 0 ) THEN
            WRITE ( MCM_LUTER, '(1X,A)' ) STRING( :LINCH )
         ELSE
            WRITE ( MCM_LUTER, '(1X,A)' ) ' '
         END IF

*  Increment the line count.
         MCM_LTEXT = MCM_LTEXT + 1

      END IF

*  Wrap up.

      GO TO 9900

*  For CTRL/Z or non-blank input: suppress further output.

 900  CONTINUE
      MCM_TXTON = .FALSE.

*  Set the status OK, and exit.

 9900 CONTINUE
      IRM_PTOUT = 1


      END
