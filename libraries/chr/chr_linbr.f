      SUBROUTINE CHR_LINBR( STR1, IPOSN, STR2 )
*+
*  Name:
*     CHR_LINBR

*  Purpose:
*     Break a line of text into a sequence of shorter lines.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_LINBR( STR1, IPOSN, STR2 )

*  Description:
*     Break a long line of text into a sequence of shorter lines,
*     making the breaks between words at spaces if possible.
*     The maximum length of an output line is determined by
*     the size of the character variable supplied to contain it.
*     This routine should be called repeatedly to generate successive
*     output lines from a single long input line.  Initially, the
*     context argument IPOSN should be set to zero; it will be updated
*     after each call, ready to generate the next output line. A value
*     of zero is returned for IPOSN when there are no more output
*     lines. Any unprintable characters (e.g. tabs) are treated as if
*     they were blanks for the purpose of identifying line-breaks.

*  Arguments:
*     STR1 = CHARACTER * ( * ) (Given)
*        The line of text to be broken into shorter lines. Leading
*        blanks are ignored.
*     IPOSN = INTEGER (Given and Returned)
*        On entry, this argument specifies the character position in
*        STR1 from which to start generating the next returned line.
*        If a value less than 1 is given, then 1 will be used.
*
*        On exit, this argument is set to one more than the position
*        in STR1 of the last non-blank character which appears in the
*        returned line STR2 (i.e. the position at which generation of the
*        next returned line should begin). If STR2 is blank because there
*        are no more characters to process, then IPOSN is returned set
*        to zero.
*     STR2 = CHARACTER * ( * ) (Returned)
*        The returned line, left justified. The length of this argument
*        determines the maximum length of the returned line.

*  Copyright:
*     Copyright (C) 1991, 1994 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     PCTR: P.C.T. Rees (STARLINK)
*     ACC:  A.C. Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-FEB-1991 (RFWS):
*        Original version.
*     25-FEB-1991 (RFWS):
*        Improved prologue.
*     10-MAR-1994 (ACC for PCTR):
*        Modifications to prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) STR1

*  Arguments Given and Returned:
      INTEGER IPOSN

*  Arguments Returned:
      CHARACTER * ( * ) STR2

*  Local Variables:
      CHARACTER CVALUE           ! Input character
      INTEGER I                  ! Loop counter for character positions
      INTEGER IEND               ! Last character before line break
      INTEGER LAST               ! Last possible output character
      LOGICAL BREAK              ! Suitable line-break found?
      LOGICAL SPACE              ! Character blank (or non-printing)?

*.

*  If the starting position does not lie beyond the end of the input
*  string, then there is potentially some output.
      IF ( IPOSN .LE. LEN( STR1 ) ) THEN

*     If the starting position is before the beginning of the string,
*     then advance it to the first character position.
         IF ( IPOSN .LT. 1 ) IPOSN = 1

*     Search forward from the starting position to find a suitable first
*     output character.
         DO 10 I = IPOSN, LEN( STR1 )
            CVALUE = STR1( I : I )

*        Look for a non-blank printing character.
            IF ( CVALUE .NE. ' ' ) THEN
               CALL CHR_CLEAN( CVALUE )
               IF ( CVALUE .NE. ' ' ) GO TO 20
            END IF
 10      CONTINUE
 20      CONTINUE

*     Store its position as the new starting position.
         IPOSN = I
      END IF

*  If the starting position is now beyond the end of the input string,
*  then there is no output. Set IPOSN to zero to indicate this and
*  return a blank output string.
      IF ( IPOSN .GT. LEN( STR1 ) ) THEN
         IPOSN = 0
         STR2 = ' '

*  Otherwise, calculate the position of the last possible input
*  character which could fit into the output string given the current
*  starting position.
      ELSE
         LAST = MIN( IPOSN+LEN( STR2 )-1, LEN( STR1 ) )

*     Note whether this last character is followed by the end of the
*     input string.
         IF ( LAST .EQ. LEN( STR1 ) ) THEN
            BREAK = .TRUE.

*     If not, see if it is followed by a blank or a non-printing
*     character (all of these count as the same thing).
         ELSE
            CVALUE = STR1( LAST+1 : LAST+1 )

            IF ( CVALUE .EQ. ' ' ) THEN
               BREAK = .TRUE.
            ELSE
               CALL CHR_CLEAN( CVALUE )
               BREAK = ( CVALUE .EQ. ' ' )
            END IF
         END IF

*     Search backwards from the last possible character position to find
*     a non-blank printing character which is followed by a suitable
*     line-break point.
         DO 30 IEND = LAST, IPOSN, -1

*        First check each character to see if it is a space or
*        non-printing character.
            CVALUE = STR1( IEND : IEND )

            IF ( CVALUE .EQ. ' ' ) THEN
               SPACE = .TRUE.
            ELSE
               CALL CHR_CLEAN( CVALUE )
               SPACE = ( CVALUE .EQ. ' ' )
            END IF

*        If so, then note we have a suitable break point once the next
*        non-blank printing character is found.
            IF ( SPACE ) THEN
               BREAK = .TRUE.

*        When that character is found, stop searching.
            ELSE IF ( BREAK ) THEN
               GO TO 40
            END IF
 30      CONTINUE
 40      CONTINUE

*     If no suitable character was found, then the line does not contain
*     a good break point. Simply break at the last possible point.
         IF ( IEND .LT. IPOSN ) IEND = LAST

*     Extract the output line and update the starting position for next
*     time the routine is called.
         STR2 = STR1( IPOSN : IEND )
         IPOSN = IEND + 1

*     If the remainder of the input string is blank, set IPOSN to zero.
         IF ( LAST .EQ. LEN( STR1 ) ) IPOSN = 0
      END IF

      END
