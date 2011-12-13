      SUBROUTINE CHR_ITOB( IVALUE, STRING, STATUS )
*+
*  Name:
*     CHR_ITOB

*  Purpose:
*     Write an INTEGER value into a binary string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_ITOB( IVALUE, STRING, STATUS )

*  Description:
*     Encode an INTEGER value into a binary string. The result is
*     right-justified in the returned string. In the event of an error,
*     '*'s are written to the string.

*  Arguments:
*     IVALUE = INTEGER (Given)
*        Value to be encoded.
*     STRING = CHARACTER * ( * ) (Returned)
*        Binary string encoded from the given value.

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
*     AJC:  A.J. Chipperfield (STARLINK)
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
      INTEGER BINDEC             ! Decimal 2
      PARAMETER ( BINDEC = 2 )

      INTEGER LITTLE             ! Smallest integer (4 bytes)
      PARAMETER ( LITTLE = -2147483647 - 1 )

      INTEGER MAXBIN             ! Maximum length of binary string (4 bytes)
      PARAMETER ( MAXBIN = 31 )

      INTEGER BUFLEN             ! Length of BUFFER (MAXBIN + 1)
      PARAMETER ( BUFLEN = 32 )

*  Local Variables:
      LOGICAL ISNEG              ! Whether IVALUE is negative

      CHARACTER * 2 BINCHR       ! Binary characters
      CHARACTER * (BUFLEN) BUFFER  ! Internal buffer

      INTEGER ICH                ! Character count
      INTEGER ICODE              ! Encode string element
      INTEGER ITEST              ! Remainder
      INTEGER IVAL               ! Integer value
      INTEGER JVAL               ! Integer value
      INTEGER NUM_CHARS          ! Number of characters written to BUFFER
      INTEGER STRLEN             ! Declared length of STRING

*  Local Data:
      DATA BINCHR / '01' /

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

*  Value is negative, check that STRING will hold at least 32 bits
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
      ITEST = BINDEC

*  Loop to complete loading the returned string.
      DO 10 ICH = BUFLEN, 1, -1
         ICODE = MOD( IVAL, ITEST ) + 1
         BUFFER( ICH : ICH ) = BINCHR( ICODE : ICODE )
         IVAL = IVAL / BINDEC
         NUM_CHARS = ICH
         IF ( ( .NOT. ISNEG ) .AND. ( IVAL .EQ. 0 ) ) GO TO 20
 10   CONTINUE
 20   CONTINUE

*  Check that overflow has not occurred.
      IF ( ISNEG ) THEN
         BUFFER( BUFLEN-MAXBIN : BUFLEN-MAXBIN ) = '1'
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
