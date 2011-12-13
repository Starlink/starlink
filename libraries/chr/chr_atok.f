      CHARACTER * 1 FUNCTION CHR_ATOK( TOKEN )
*+
*  Name:
*     CHR_ATOK

*  Purpose:
*     Return the character for a given ASCII character token.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = CHR_ATOK( TOKEN )

*  Description:
*     The given ASCII character token is converted to a single returned
*     character in the machine's character set.  All
*     non-printable ASCII characters are represented by their equivalent
*     token strings. If no such ASCII character exists, the character
*     code 0 (the ASCII NUL character) is returned. The routine is intended
*     for the portable initialisation of unprintable characters.

*  Arguments:
*     TOKEN = CHARACTER * ( * ) (Given)
*        A printable character string representing the character to
*        be returned, e.g., 'BEL', 'BS', etc.

*  Returned Value:
*     CHR_ATOK = CHARACTER * 1
*        The character code within the ASCII character set.

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
*     25-FEB-1991 (PCTR):
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
      CHARACTER TOKEN * ( * )

*  External References:
      LOGICAL CHR_SIMLR          ! Whether strings are same (ignoring case)

*  Local Constants:
      INTEGER DEL                ! ASCII delete character code
      PARAMETER ( DEL = 127 )

      INTEGER MXTVAL             ! Number of unprintable ASCII characters
      PARAMETER ( MXTVAL = 34 )

      INTEGER NUL                ! ASCII null character code
      PARAMETER ( NUL = 0 )

*  Local Variables:
      CHARACTER * 3 TOKVAL( MXTVAL ) ! Array of ASCII character tokens

      INTEGER IVAL               ! Loop index
      INTEGER LENGTH             ! Declared length of the given string
      INTEGER ASCVAL( MXTVAL )   ! Array of ASCII codes

*  Local Data:
      DATA ASCVAL /  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12,
     :              13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
     :              26, 27, 28, 29, 30, 31, 32, 127 /

      DATA TOKVAL / 'NUL', 'SOH', 'STX', 'ETX', 'EOT', 'ENQ', 'ACK',
     :              'BEL', 'BS',  'HT',  'LF',  'VT',  'FF',  'CR',
     :              'SO',  'SI',  'DLE', 'DC1', 'DC2', 'DC3', 'DC4',
     :              'NAK', 'SYN', 'ETB', 'CAN', 'EM',  'SUB', 'ESC',
     :              'FS',  'GS',  'RS',  'US',  'SP',  'DEL' /

*.

*  Initialise the returned value to a NUL character.
      CHR_ATOK = CHAR( NUL )

*  Get the declared length of the given string.
      LENGTH = LEN( TOKEN )

*  Is the given string a single character?
      IF ( LENGTH .EQ. 1 ) THEN

*     The given string is a single character, so no token. Check that the
*     given character is part of the ASCII character set.
         IF ( LGT( TOKEN, CHAR( NUL ) )
     :       .AND. LLE( TOKEN, CHAR( DEL ) ) ) THEN
            CHR_ATOK = TOKEN
         END IF
      ELSE IF ( LENGTH .GT. 1 ) THEN

*     The given string is not a single character, so search through the
*     allowed tokens for the correct unprintable character.
         DO 10 IVAL = 1, MXTVAL

            IF ( CHR_SIMLR( TOKEN, TOKVAL( IVAL ) ) ) THEN
               CHR_ATOK = CHAR( ASCVAL( IVAL ) )
               GO TO 20
            END IF
 10      CONTINUE
 20      CONTINUE
      END IF

      END
