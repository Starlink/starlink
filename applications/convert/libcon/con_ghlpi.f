      INTEGER FUNCTION CON_GHLPI( STRING, PROMPT, LINCH )
*+
*  Name:
*     CON_GHLPI

*  Purpose:
*     Gets one line input during an help session.

*  Language:
*     Vax Fortran 77

*  Invocation:
*     Called as sixth argument of LBR$OUTPUT_HELP or seventh of
*     HLP_OUTHLP.

*  Description:
*     Analogue of VAX/VMS RTL routine LIB$GET_INPUT.  It gets one line
*     of input during a  VMS or portable HELP session.  Note that the
*     inherited status is not used.

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
*     CON_GHLPI = INTEGER
*        The status.  If the line was inpput correctly a value of 1 is
*        returned.

*  Authors:
*     (MJC) Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1993 July 30 (MJC):
*        Original based on KAPPA's CON_GHLPI.
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
      INTEGER CHR_LEN            ! Character string length ignoring
                                 ! trailing blanks

*  Local Variables:
      CHARACTER BUFA * ( 132 )   ! Work space
      CHARACTER BUFB * ( 132 )   ! Work space
      INTEGER J                  ! Local status

*.

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
            CALL SREAD( LUCMD, BUFA, BUFB, STRING, J )

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
      CON_GHLPI = 1

      END
