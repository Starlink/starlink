      INTEGER FUNCTION SPACK( STRING, PROMPT, NCHAR )
*+
*  Name:
*     SPACK

*  Purpose:
*     Input routine for help system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = SPACK( STRING, PROMPT, NCHAR )

*  Description:
*     This routine interfaces the help system with the terminal to get a
*     line of input. It is called by the help system and uses a routine
*     coded in C to prompt for input with printf and fgets.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Returned)
*        The string got through parameter TOPIC and returned to the
*        calling routine.
*     PROMPT = CHARACTER * ( * ) (Given)
*        The prompt string to be used.
*     NCHAR = INTEGER (Returned)
*        The used length of STRING (i.e. excluding trailing blanks).

*  Returned Value:
*     SPACK = INTEGER
*        The returned value is 1 for OK, -1 if an error occured. The
*        only error that can occur is that the user's reply was an
*        end-of-file character only.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     21 Jul 1992 (hme):
*        Original version.
*     04 Nov 1992 (hme):
*        Adapted from Specdre's SPACK.
*        Enable paging, i.e. reset the number of lines.
*     01 Jul 1993 (hme):
*        Back from Figaro to Specdre. Disuse ADAM, use C code instead.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) PROMPT

*  Arguments Returned:
      CHARACTER * ( * ) STRING
      INTEGER NCHAR

*  Status:
      INTEGER STATUS             ! Local status

*  Local Variables:
      INTEGER IGNORE, IGNOR2     ! Returned by line counter

*  Internal References:
      INTEGER CHR_LEN            ! Used length of string

*.

*  Set status OK.
      STATUS = SAI__OK

*  Display prompt and wait for a reply.
      CALL SPAEH( PROMPT, LEN(PROMPT), STRING, LEN(STRING), STATUS )

*  Set return values according to status. A bad status signals a sole
*  EOF was given as reply. This is translated here into a return
*  value +1 for good and -1 for bad. So the portable help system will
*  exit when an EOF has been given to SPAEH.
      IF ( STATUS .EQ. SAI__OK ) THEN
         NCHAR = CHR_LEN( STRING )
         SPACK = 1
      ELSE
         NCHAR = 0
         SPACK = -1
      END IF

*  Reset the count for lines on page.
      CALL SPAEG( 2, IGNORE, IGNOR2, 0, STATUS )

*  Return.
      END
