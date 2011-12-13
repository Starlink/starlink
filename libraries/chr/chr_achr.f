      CHARACTER * 1 FUNCTION CHR_ACHR( ASCII )
*+
*  Name:
*     CHR_ACHR

*  Purpose:
*     Return the character for a given ASCII value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = CHR_ACHR( ASCII )

*  Description:
*     The given ASCII value is converted to a single returned character
*     in the machine's character set. If no such character
*     exists within the machine's character set, the character code 0
*     (the ASCII NUL character) is returned.

*  Arguments:
*     ASCII = INTEGER (Given)
*        The position of the character within the ASCII character set.

*  Returned Value:
*     CHR_ACHR = CHARACTER * 1
*        A character value within the machine's character set.

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
*     PCTR: P.C.T. Rees (STARLINK)
*     ACC:  A.C. Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-FEB-1991 (PCTR):
*        Original version.
*     10-MAR-1994 (ACC for PCTR):
*        Modifications to prologue.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  System-specific:
*     This subprogram has been implemented for machines which use
*     the ASCII character set.
*     Semi-portable code (commented out) has been appended which
*     may be used on any machine architecture. This code is only
*     semi-portable because it uses an extended character set to that
*     of Fortran 77.
*     The appended code may be made fully portable by replacing all
*     non-Fortran 77 characters in the DATA statement with tokens which
*     may be subsequently decoded by CHR_ATOK.

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER ASCII

*  Version for machines which use the ASCII character set.
*  Local Constants:
      INTEGER DEL                ! ASCII delete character code
      PARAMETER ( DEL = 127 )

      INTEGER NUL                ! ASCII null character code
      PARAMETER ( NUL = 0 )

*.

*  Check that the given ASCII value is within the correct range.
      IF ( ( ASCII .GT. NUL ) .AND. ( ASCII .LE. DEL ) ) THEN
         CHR_ACHR = CHAR( ASCII )
      ELSE
         CHR_ACHR = CHAR( NUL )
      END IF

*  Semi-portable version.
*  External References:
*     CHARACTER * 1 CHR_ATOK     ! Return an ASCII character value

*  Local Constants:
*     INTEGER DEL                ! ASCII delete character code
*     PARAMETER ( DEL = 127 )

*     INTEGER MXTVAL             ! Number of unprintable ASCII characters
*     PARAMETER ( MXTVAL = 128 )

*     INTEGER NUL                ! ASCII null character code
*     PARAMETER ( NUL = 0 )

*  Local Variables:
*     CHARACTER * 3 TOKVAL( MXTVAL ) ! Array of ASCII character labels

*  Local Data:
*     DATA TOKVAL / 'NUL', 'SOH', 'STX', 'ETX', 'EOT', 'ENQ', 'ACK',
*    : 'BEL', 'BS',  'HT',  'LF',  'VT',  'FF',  'CR',  'SO',  'SI',
*    : 'DLE', 'DC1', 'DC2', 'DC3', 'DC4', 'NAK', 'SYN', 'ETB', 'CAN',
*    : 'EM',  'SUB', 'ESC', 'FS',  'GS',  'RS',  'US',  ' ',   '!',
*    : '"',   '#',   '$',   '%',   '&',   '''',  '(',   ')',   '*',
*    : '+',   ',',   '-',   '.',   '/',   '0',   '1',   '2',   '3',
*    : '4',   '5',   '6',   '7',   '8',   '9',   ':',   ';',   '<',
*    : '=',   '>',   '?',   '@',   'A',   'B',   'C',   'D',   'E',
*    : 'F',   'G',   'H',   'I',   'J',   'K',   'L',   'M',   'N',
*    : 'O',   'P',   'Q',   'R',   'S',   'T',   'U',   'V',   'W',
*    : 'X',   'Y',   'Z',   '[',   '\',   ']',   '^',   '_',   '`',
*    : 'a',   'b',   'c',   'd',   'e',   'f',   'g',   'h',   'i',
*    : 'j',   'k',   'l',   'm',   'n',   'o',   'p',   'q',   'r',
*    : 's',   't',   'u',   'v',   'w',   'x',   'y',   'z',   '{',
*    : '|',   '}',   '~',   'DEL' /

*.

*  Check that the given ASCII value is within the correct range.
*     IF ( ( ASCII .GT. NUL ) .AND. ( ASCII .LE. DEL ) ) THEN

*  Use CHR_ATOK to decode the corresponding character token into a
*  character value.
*        CHR_ACHR = CHR_ATOK( TOKVAL( ASCII-1 ) )
*     ELSE

*     The given ASCII value is not within the correct range, so set
*     the returned value to NUL.
*        CHR_ACHR = CHR_ATOK( 'NUL' )
*     END IF

      END
