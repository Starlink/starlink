      INTEGER FUNCTION SHL_PTHLPO( STRING )
*+
*  Name:
*     SHL_PTHLPO

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
*     SHL_PTHLPO = INTEGER
*        The status.  If the line was output correctly a value of 1 is
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
*        Original called PUT_OUTPUT.
*     1988 September 7 (MJC):
*        Modified to work under ADAM in KAPPA.
*     1992 June 22 (MJC):
*        Added a constraint to output with paging when the screen height
*        and width could not be determined.  Converted to SST prologue
*        and documented global parameters.
*     2004 July 15 (TIMJ):
*        Relocate to SHL library
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
            CALL SHL_SREAD ( LUCMD, BUFA, BUFB, CMD, J )

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
      SHL_PTHLPO = 1

      END
