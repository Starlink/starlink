      SUBROUTINE CHR_ITOH( IVALUE, STRING, STATUS )
*+
*  Name:
*     CHR_ITOH

*  Purpose:
*     Write a hexadecimal string from an INTEGER value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_ITOH( IVALUE, STRING, STATUS )

*  Description:
*     Encode an INTEGER value into a hexadecimal string using the
*     machine's character set. The result is right-justified in the
*     returned string. In the event of an error, '*'s are written to
*     the string.

*  Arguments:
*     IVALUE = INTEGER (Given)
*        Value to be encoded.
*     STRING = CHARACTER * ( * ) (Returned)
*        Hexadecimal string encoded from the given value.
*     STATUS = INTEGER (Given and Returned)
*        The status value. If this value is not SAI__OK on input, the
*        routine returns without action. If the routine fails to
*        complete successfully, STATUS is returned set to SAI__ERROR.

*  Note:
*     This subroutine assumes a 32-bit, twos-complement representation
*     of an INTEGER.

*  Algorithm:
*     Portable Version:
*        Encode the given string explicitly.

*  Copyright:
*     Copyright (C) 1991, 1994 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     26-JUN-1991 (PCTR):
*        Original version.
*     10-MAR-1994 (ACC for PCTR):
*        Modifications to prologue.
*     24-MAY-1995 (AJC)
*        Remove superfluous comma from CHR_FILL call
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER IVALUE

*  Arguments Returned:
      CHARACTER STRING * ( * )

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER HEXDEC             ! Decimal 16
      PARAMETER ( HEXDEC = 16 )

      INTEGER LITTLE             ! Smallest integer (4 bytes)
      PARAMETER ( LITTLE = -2147483647 - 1 )

      INTEGER MAXHEX             ! Maximum length of hexadecimal (4 bytes)
      PARAMETER ( MAXHEX = 7 )

      INTEGER BUFLEN             ! Length of BUFFER (MAXHEX + 1)
      PARAMETER ( BUFLEN = 8 )

*  Local Variables:
      LOGICAL ISNEG              ! Whether IVALUE is negative

      CHARACTER * 16 HEXCHR      ! Hexadecimal characters
      CHARACTER * ( BUFLEN ) BUFFER  ! Intermediate buffer

      INTEGER ICH                ! Character count
      INTEGER ICODE              ! Encode string element
      INTEGER ITEST              ! Remainder
      INTEGER IVAL               ! Integer value
      INTEGER JVAL               ! Integer value
      INTEGER NUM_CHARS          ! Number of characters written to BUFFER
      INTEGER STRLEN             ! Declared length of STRING

*  Local Data:
      DATA HEXCHR / '0123456789ABCDEF' /

*.


*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the declared length of the returned string.
      STRLEN = LEN( STRING )

*  Initialise the returned string.
      STRING = ' '
      BUFFER = ' '

*  Determine if the given value is negative.
      IF ( IVALUE .GT. 0 ) THEN
         ISNEG = .FALSE.
      ELSE IF ( IVALUE .LT. 0 ) THEN

*  Value is negative, check that STRING will hold at least
*    8 hex digits (32 bits)
         IF ( STRLEN .LT. BUFLEN ) THEN
            CALL CHR_FILL( '*', STRING )
            STATUS = SAI__ERROR
            GO TO 999
         END IF
         ISNEG = .TRUE.
      ELSE

*     The string is zero.
         STRING( 1 : 1 ) = '0'

*     Return.
         GO TO 999
      END IF

*  Test if the given value is negative and initialise IVAL.
      IF ( ISNEG ) THEN
         JVAL = LITTLE
         IVAL = IVALUE - JVAL
      ELSE
         IVAL = IVALUE
      END IF

*  Encode the integer into a right-justified binary string: first
*  initialise ITEST.
      ITEST = HEXDEC

*  Loop to complete loading the returned string.
      DO 10 ICH = BUFLEN, 1, -1
         ICODE = MOD( IVAL, ITEST ) + 1
         BUFFER( ICH : ICH ) = HEXCHR( ICODE : ICODE )
         IVAL = IVAL / HEXDEC
         NUM_CHARS = ICH
         IF ( ( .NOT. ISNEG ) .AND. ( IVAL .EQ. 0 ) ) GO TO 20
 10   CONTINUE
 20   CONTINUE

*  Check that overflow has not occurred.
      IF ( ISNEG ) THEN
          ICODE = INDEX( HEXCHR,
     :                   BUFFER( BUFLEN-MAXHEX : BUFLEN-MAXHEX ) ) + 8
          BUFFER( BUFLEN-MAXHEX : BUFLEN-MAXHEX )
     :         = HEXCHR( ICODE : ICODE )
      ELSE IF ( IVAL .GT. 0 ) THEN
         CALL CHR_FILL( '*', STRING )
         STATUS = SAI__ERROR
         GO TO 999
      END IF

*  Write BUFFER left-justified into STRING
      IF ( ISNEG ) THEN
         STRING = BUFFER
      ELSE
         STRING = BUFFER( NUM_CHARS : BUFLEN )
      END IF

 999  CONTINUE

      END
