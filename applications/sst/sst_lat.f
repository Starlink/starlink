      SUBROUTINE SST_LAT( INDENT, LINE, STATUS )
*+
*  Name:
*     SST_LAT

*  Purpose:
*     Send a line to the output file in Latex format.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_LAT( INDENT, LINE, STATUS )

*  Description:
*     The routine sends a line to the output file in Latex format, with
*     a specified number of blanks preceding it to provide indentation.
*     Leading and trailing blanks within the line itself are removed
*     before applying the indentation. Any characters which Latex
*     regards as "special" are suitably escaped.

*  Arguments:
*     INDENT = INTEGER (Given)
*        Indentation level (number of blanks in front of first non-blank
*        output character).
*     LINE = CHARACTER * ( * ) (Given)
*        Line to be output.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Find the first and last non-blank characters to be output.
*     -  Loop to inspect the line for special characters.
*     -  First test if there is enough room in the output buffer to
*     accommodate any characters which may require escaping. If not,
*     then report an error.
*     -  If OK, detect those characters requiring a '\' escape
*     character.
*     -  Detect those requiring maths mode.
*     -  Detect those requiring {\tt} text.
*     -  Detect those needing a special maths symbol.
*     -  Use others literally.
*     -  If the input line was blank, then output a blank line.
*     Otherwise, output the processed buffer contents.

*  Copyright:
*     Copyright (C) 1990, 1991, 1994 Science & Engineering Research Council.
*     Copyright (C) 2005 Particls Physics & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify it under
*     the terms of the GNU General Public License as published by the Free Software
*     Foundation; either version 2 of the License, or (at your option) any later
*     version.
*
*     This program is distributed in the hope that it will be useful,but WITHOUT ANY
*     WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
*     PARTICULAR PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License along with
*     this program; if not, write to the Free Software Foundation, Inc., 59 Temple
*     Place,Suite 330, Boston, MA  02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     8-JAN-1990 (RFWS):
*        Original version.
*     8-AUG-1990 (RFWS):
*        Changed to check for internal buffer overflow and to output
*        the line by calling SST_PUT.
*     13-AUG-1990 (RFWS):
*        Changed to use maths symbols instead of \verb.
*     10-DEC-1991 (RFWS):
*        Escape "&" character correctly (previously omitted).
*     5-DEC-1994 (PDRAPER):
*        Added double \\ to get working on UNIX.
*     14-APR-2005 (PDRAPER):
*        Converted to use pre-defined backslash character.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'SST_PAR'          ! SST_ constants

*  Arguments Given:
      INTEGER INDENT
      CHARACTER * ( * ) LINE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 1 ) CH       ! Single input character
      CHARACTER * ( 3 * SST__SZLIN ) BUF ! Output buffer
      INTEGER F                  ! First non-blank character
      INTEGER I                  ! Loop counter for input characters
      INTEGER II                 ! Loop counter for output characters
      INTEGER L                  ! Last non-blank character

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the first and last non-blank characters to be output.
      BUF = ' '
      CALL CHR_FANDL( LINE, F, L )

*  Loop to inspect the line for special characters.
      II = 0
      DO 1 I = F, L
         CH = LINE( I : I )

*  First test if there is enough room in the output buffer to
*  accommodate any characters which may require escaping. If not, then
*  report an error.
         IF ( II + 12 .GT. LEN( BUF ) ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'BUFSIZ', LEN( BUF ) )
            CALL ERR_REP( 'SST_LAT_BUFEX',
     :      'Exceeded size of internal buffer (^BUFSIZ characters) '//
     :      'while adding escape characters to Latex output line.',
     :      STATUS )

*  If OK, detect those characters requiring a '\' escape character.
         ELSE IF ( ( CH .EQ. '$' ) .OR.
     :             ( CH .EQ. '%' ) .OR.
     :             ( CH .EQ. '#' ) .OR.
     :             ( CH .EQ. '&' ) .OR.
     :             ( CH .EQ. '_' ) .OR.
     :             ( CH .EQ. '{' ) .OR.
     :             ( CH .EQ. '}' ) ) THEN
            BUF( II + 1 : II + 2 ) = SST__BKSLH // CH
            II = II + 2

*  Detect those requiring maths mode.
         ELSE IF ( ( CH .EQ. '+' ) .OR.
     :             ( CH .EQ. '*' ) .OR.
     :             ( CH .EQ. '|' ) .OR.
     :             ( CH .EQ. '<' ) .OR.
     :             ( CH .EQ. '>' ) ) THEN
            BUF( II + 1 : II + 3 ) = '$' // CH // '$'
            II = II + 3

*  Detect those requiring {\tt} text.
         ELSE IF ( ( CH .EQ. '''' ) .OR.
     :             ( CH .EQ. '"' ) ) THEN
            BUF( II + 1 : II + 7 ) =
     :         '{' // SST__BKSLH // 'tt ' // CH // '}'
            II = II + 7

*  Detect those needing special maths symbols.
         ELSE IF ( CH .EQ. '^' ) THEN
            BUF( II + 1 : II + 8 ) = '$' // SST__BKSLH //'wedge$'
            II = II + 8

         ELSE IF ( CH .EQ. '~' ) THEN
            BUF( II + 1 : II + 6 ) = '$' // SST__BKSLH //'sim$'
            II = II + 6

         ELSE IF ( CH .EQ. SST__BKSLH ) THEN
            BUF( II + 1 : II + 12 ) = '$'// SST__BKSLH //'backslash$'
            II = II + 12

*  Use others literally.
         ELSE
            BUF( II + 1 : II + 1 ) = LINE( I : I )
            II = II + 1
         END IF
1     CONTINUE

*  If the input line was blank, then output a blank line.
      IF ( II .EQ. 0 ) THEN
         CALL SST_PUT( INDENT, ' ', STATUS )

*  Otherwise, output the processed buffer contents.
      ELSE
         CALL SST_PUT( INDENT, BUF( : II ), STATUS )
      END IF

      END
* @(#)sst_lat.f   1.3   94/12/05 11:58:43   96/07/05 10:27:32
