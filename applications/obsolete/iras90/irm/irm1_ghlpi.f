      INTEGER FUNCTION IRM1_GHLPI( STRING, PROMPT, LINCH )
*+
*  Name:
*     IRM1_GHLPI

*  Purpose:
*     Gets one line input during an help session.

*  Language:
*     Vax Fortran 77

*  Invocation:
*     ISTAT = IRM1_GHLPI( STRING, PROMPT, LINCH )

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

*  Returned Value:
*     IRM1_GHLPI = INTEGER
*        The status.  If the line was input correctly a value of 1 is
*        returned, otherwise zero is returned.

*  Authors:
*     PTW: P.T.Wallace (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-SEP-1992 (DSB):
*        Original version, modified from KAPPA routine GTHLPI written by
*        PTW and MJC.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! SSE global definitions

*  Global Variables:
      INCLUDE 'IRM_COM'        ! IRM common blocks.
*        MCM_CMD = CHARACTER * ( 80 ) (Write)
*           The text entered in response to the "Press RETURN
*           to continue..." prompt.
*        MCM_LTEXT = INTEGER (Read and Write)
*           Lines of text output this screenful.
*        MCM_TXTON = LOGICAL (Read)
*           If true, text output is enabled.
*        MCM_LUCMD = INTEGER (Read)
*           Logical-unit number of the command input.
*        MCM_LUTER = INTEGER (Read)
*           Logical-unit number of the terminal output.

*  Arguments Given:
      CHARACTER PROMPT * ( * )

*  Arguments Returned:
      CHARACTER STRING * ( * )
      INTEGER LINCH

*  External References:
      EXTERNAL IRM1_BLDAT        ! Initialise IRM common blocks.
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

*  Set the returned value bad.
      IRM1_GHLPI = 0

*  Was Something entered at the "Press RETURN to continue..." prompt?
      IF( MCM_CMD .NE. ' ' ) THEN

*  Yes: return it, forget it, re-enable output
         STRING = MCM_CMD
         MCM_CMD = ' '
         MCM_TXTON = .TRUE.

      ELSE

*  No: output suppressed?
         IF ( MCM_TXTON ) THEN

*  Output enabled: write prompt, if any.

            IF ( PROMPT .NE. '  ' ) 
     :        WRITE ( MCM_LUTER, '(1X,A,$)' ) PROMPT

*  Get a line of uppercase input.
            CALL IRM1_SREAD ( MCM_LUCMD, BUFA, BUFB, STRING, J )

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
      MCM_LTEXT =  0

*  Wrap up.
      GO TO 9900

*  CTRL/Z handling---suppress further output and return blank line.
 900  CONTINUE
      MCM_TXTON = .FALSE.
      STRING = ' '
      LINCH = 0

*  Set the status and exit.
 9900 CONTINUE
      IRM1_GHLPI = 1

      END
