      INTEGER FUNCTION CHR_IACHR( CVALUE )
*+
*  Name:
*     CHR_IACHR

*  Purpose:
*     Return the ASCII value for the given character.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = CHR_IACHR( CVALUE )

*  Description:
*     The given character, encoded using the machine's character set,
*     is converted to an integer indicating its position in the ASCII
*     character set. If no such character exists, zero is returned.

*  Arguments:
*     CVALUE = CHARACTER * 1 (Given)
*        The character to be converted to its position within the
*        ASCII character set.

*  Returned Value:
*     CHR_IACHR = INTEGER
*        An integer position within the ASCII character set.

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

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * 1 CVALUE

*  External References:
      CHARACTER * 1 CHR_ATOK     ! Return an ASCII character value

*  Version for machines which use the ASCII character set.
*  Local Constants:
      INTEGER DEL                ! ASCII delete character code
      PARAMETER ( DEL = 127 )

      INTEGER NUL                ! ASCII null character code
      PARAMETER ( NUL = 0 )

*.

*  Check that the given character value is within the correct range.
      IF ( LGT( CVALUE, CHR_ATOK( 'NUL' ) )
     :    .AND. LLE( CVALUE, CHR_ATOK( 'DEL' ) ) ) THEN
         CHR_IACHR = ICHAR( CVALUE )
      ELSE
         CHR_IACHR = NUL
      END IF

*  Semi-portable version.
*  Local Constants:
*     INTEGER DEL                ! ASCII delete character code
*     PARAMETER ( DEL = 127 )

*     INTEGER MXTVAL             ! Number of unprintable ASCII characters
*     PARAMETER ( MXTVAL = 128 )

*     INTEGER NUL                ! ASCII null character code
*     PARAMETER ( NUL = 0 )

*  Local Variables:
*     INTEGER ICHR               ! Character loop index

*     CHARACTER * 3 TOKVAL( MXTVAL ) ! Array of ASCII character labels

*  Local Data:
*     DATA TOKVAL /  'NUL', 'SOH', 'STX', 'ETX', 'EOT', 'ENQ', 'ACK',
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

*  Check that the given character value is within the correct range.
*     IF ( LGT( CVALUE, CHR_ATOK( 'NUL' ) )
*    :    .AND. LLE( CVALUE, CHR_ATOK( 'DEL' ) ) ) THEN

*     Loop to search the ASCII character set for the given character.
*        DO 10 ICHR = 2, MXTVAL
*           IF ( CHR_ATOK( TOKVAL( ICHR ) ) .EQ. CVALUE ) GO TO 20
*10      CONTINUE

*     No match, so set the returned value to NUL.
*        ICHR = 1
*20      CONTINUE

*     Set the returned value.
*        CHR_IACHR = ICHR - 1
*     ELSE
*        CHR_IACHR = NUL
*     END IF

      END
