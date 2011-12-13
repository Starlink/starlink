      SUBROUTINE CHR_BTOI( STRING, IVALUE, STATUS )
*+
*  Name:
*     CHR_BTOI

*  Purpose:
*     Read an INTEGER value from a binary string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_BTOI( STRING, IVALUE, STATUS )

*  Description:
*     The given binary string is decoded into an INTEGER value.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        String to be decoded, e.g. `10101100'.
*     IVALUE = INTEGER (Returned)
*        Value decoded from the given string.
*     STATUS = INTEGER (Given and Returned)
*        The status value. If this value is not SAI__OK on input, the
*        routine returns without action. If the routine fails to
*        complete successfully, STATUS is returned set to SAI__ERROR.

*  Note:
*     This subroutine assumes a 32-bit, twos-complement representation
*     of an INTEGER.

*  Algorithm:
*     Portable Version:
*        Decode the given string explicitly.

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
*     16-APR-1991 (PCTR):
*        Original version.
*     10-MAR-1994 (ACC for PCTR):
*        Modifications to prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER STRING * ( * )

*  Arguments Returned:
      INTEGER IVALUE

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN            ! String length (ignoring trailing blanks)

*  Local Constants:
      CHARACTER * 3 BINCHR       ! Binary characters
      PARAMETER ( BINCHR = '01 ' )

      INTEGER BINDEC             ! Decimal 2
      PARAMETER ( BINDEC = 2 )

      INTEGER LITTLE             ! Smallest integer (4 bytes)
      PARAMETER ( LITTLE = -2147483647 - 1 )

      INTEGER MAXBIN             ! Maximum length of BINSTR (4 bytes)
      PARAMETER ( MAXBIN = 31 )

*  Local Variables:
      INTEGER BINPOW( 0 : MAXBIN ) ! Internal array for binary decode
      INTEGER I                  ! Loop index
      INTEGER IVAL               ! Integer value of binary digit
      INTEGER NCHAR              ! Character count
      INTEGER NPOWER             ! Highest power of 2 in STRING
      INTEGER POWER              ! Power of 2 used in decoding STRING

      CHARACTER CVALUE           ! Single STRING element

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise IVALUE.
      IVALUE = 0
      NCHAR = CHR_LEN( STRING )

      IF ( NCHAR .GT. 0 ) THEN

*     Build BINPOW.
         BINPOW( 0 ) = 0
         NPOWER = 0
         POWER = -1

         DO 10 I = NCHAR, 1, -1
            CVALUE = STRING( I : I )
            IVAL = INDEX( BINCHR, CVALUE )

            IF ( IVAL .GT. 0 ) THEN

               IF ( IVAL .LE. BINDEC ) THEN
                  POWER = POWER + 1

                  IF ( POWER .LE. MAXBIN ) THEN
                     IF ( IVAL .GT. 1 ) NPOWER = POWER
                     BINPOW( POWER ) = IVAL - 1
                  ELSE

                     IF ( IVAL .GT. 1 ) THEN

*                    Binary string has an integer value greater than
*                    4 bytes.
                        STATUS = SAI__ERROR
                        GO TO 999
                     END IF
                  END IF
               END IF
            ELSE

*           Illegal binary character.
               STATUS = SAI__ERROR
               GO TO 999
            END IF
 10      CONTINUE

*     Go ahead and decode the binary string.
         IF ( NPOWER .GT. 0 ) THEN

            DO 20 POWER = MIN( NPOWER, MAXBIN-1 ), 1, -1
               IVALUE = ( IVALUE + BINPOW( POWER ) ) * BINDEC
 20         CONTINUE

*        Add least significant element.
            IVALUE = IVALUE + BINPOW( 0 )

*        Finish off with the most significant element.
            IF ( NPOWER .EQ. MAXBIN ) THEN
               IVAL = BINPOW( MAXBIN )

               IF ( IVAL .EQ. 1 ) THEN

*              Negative.
                  IVALUE = LITTLE + IVALUE
               END IF
            END IF
         ELSE
            IVALUE = BINPOW( 0 )
         END IF
      END IF

 999  CONTINUE

      END
