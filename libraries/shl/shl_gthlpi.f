      INTEGER FUNCTION SHL_GTHLPI( STRING, PROMPT, LINCH )
*+
*  Name:
*     SHL_GTHLPI

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
*     SHL_GTHLPI = INTEGER
*        The status.  If the line was inpput correctly a value of 1 is
*        returned.

*  Copyright:
*     Copyright (C) 1988, 1992 Science & Engineering Research Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     (PTW) P.T.Wallace (STARLINK)
*     (MJC) Malcolm J. Currie (STARLINK)
*     (TIMJ) Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1988 April 27 (PTW):
*        Original called GET_INPUT.
*     1988 September 7 (MJC):
*        Modified to work under ADAM in KAPPA.
*     1992 June 22 (MJC):
*        Converted to SST prologue and documented global parameters.
*     2004 July 15 (TIMJ):
*        Move to SHL library
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! SSE global definitions

*  Global Variables:
      INCLUDE 'SHL_HLPCMD'    ! SHL help I/O
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
            CALL SHL_SREAD ( LUCMD, BUFA, BUFB, STRING, J )

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
      SHL_GTHLPI = 1

      END
