      SUBROUTINE NDF1_TWRAP( IN, INDENT, FP, OUT )
*+
*  Name:
*     NDF1_TWRAP

*  Purpose:
*     Perform line-breaking on a stream of text.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_TWRAP( IN, INDENT, FP, OUT )

*  Description:
*     The routine splits a stream of input text into separate output
*     lines, performing line-breaking at suitable blanks if possible.
*
*     It should be called repeatedly. On each invocation, it starts
*     inspecting the input text stream (in argument IN) at the character
*     position identified by the FP argument. It hen forms the longest
*     possible output line from the characters which follow, subject to
*     not overflowing the output buffer (argument OUT). Lines are broken
*     at a blank if possible (otherwise at the last character which will
*     fit into the output line).
*
*     On return, FP is advanced to point beyond the last input character
*     transferred to the output line, ready for the next invocation. If
*     no further non-blank input characters remain to be processed, then
*     FP is returned set to zero.

*  Arguments:
*     IN = CHARACTER * ( * ) (Given)
*        The input text stream.
*     INDENT = INTEGER (Given)
*        The number of leading spaces to include in the output buffer.
*        It can be set negative to indicate that any spaces in the input
*        should be preserved.
*     FP = INTEGER (Given and Returned)
*        The formatting pointer. On entry it should point to the next
*        input character to be considered (if less than 1, then 1 is
*        used instead). On exit, it is advanced to point at the first
*        character to be considered on the next invocation. A value of
*        zero is returned if the input text is exhausted (so that no
*        further invocations are needed).
*     OUT = CHARACTER * ( * ) (Returned)
*        Variable to receive the output lines. Its length determines the
*        maximum length of these lines.

*  Copyright:
*     Copyright (C) 1994 Particle Physics & Astronomy Research Council

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     18-JUN-1993 (RFWS):
*        Original version.
*     10-AUG-1993 (RFWS):
*        Eliminate blanks which coincide with line breaks.
*     17-NOV-1994 (RFWS):
*        Do not strip leading blanks before transferring text to the
*        output.
*     19-OCT-2009 (DSB):
*        Add argument INDENT.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) IN
      INTEGER INDENT

*  Arguments Given and Returned:
      INTEGER FP

*  Arguments Returned:
      CHARACTER * ( * ) OUT

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Variables:
      INTEGER BREAK              ! Position of line break character
      INTEGER I                  ! Loop counter for characters
      INTEGER L1                 ! Significant length of input string
      INTEGER L2                 ! Length of output character variable
      INTEGER NC                 ! No. of significant input characters
      INTEGER OFIRST             ! Index of first output character
*.

*  Obtain the number of significant characters in the input string and
*  the maximum length of the output string.
      L1 = CHR_LEN( IN )
      L2 = LEN( OUT )

*  Advance FP to the start of the string if necessary.
      FP = MAX( FP, 1 )

*  Ensure the output buffer starts with the required number of leading
*  spaces, and reduce the maximum length of the output string by the number
*  of leading spaces.
      IF( INDENT .GT. 0 ) THEN
         OUT( : INDENT ) = ' '
         L2 = L2 - INDENT
      END IF

*  Unless input spaces are being preserved (indicated by INDENT being
*  negative), advance FP over any leading spaces in the input.
      IF( INDENT .GE. 0 .AND. L1 .GT. 0 ) THEN
         DO WHILE( FP .LT. L1 .AND. IN( FP : FP ) .EQ. ' ' )
            FP = FP + 1
         END DO

*  Note the index of the first output character to be written.
         OFIRST = INDENT + 1
      ELSE
         OFIRST = 1
      END IF

*  If all the input characters are blank, then set the output string
*  blank.
      IF ( FP .GT. L1 ) THEN
         OUT = ' '

*  Otherwise, calculate how many significant input characters there
*  are.
      ELSE
         NC = L1 - FP + 1

*  If these characters will fit into the output string, then copy them
*  into the output buffer (following any leading spaces) and advance FP
*  beyond the last character transferred.
         IF ( NC .LE. L2 ) THEN
            OUT( OFIRST : ) = IN( FP : L1 )
            FP = L1 + 1

*  Otherwise, the input text must be broken, at a blank if possible.
*  Search backwards through the input string, starting at the first
*  character to overflow the output, looking for a suitable blank.
         ELSE
            DO 3 BREAK = FP + L2, FP + 1, -1
               IF ( IN( BREAK : BREAK ) .EQ. ' ' ) GO TO 4
 3          CONTINUE

*  If no suitable line break character was found, then simply break the
*  input line at the first character to overflow the output.
            BREAK = FP + L2
 4          CONTINUE

*  Copy input characters to the output (after any the leading spaces), as
*  far as the line break. Advance FP to point at the next input character.
            OUT( OFIRST : )  = IN( FP : BREAK - 1 )
            FP = BREAK

*  If the break character was a blank, then skip over it (it is replaced
*  by the line break).
            IF ( IN( FP : FP ) .EQ. ' ' ) FP = FP + 1
         END IF
      END IF

*  If FP now points beyond the last non-blank input character, then the
*  input string is exhausted. Set FP to zero to indicate this.
      IF ( FP .GT. L1 ) FP = 0

      END
