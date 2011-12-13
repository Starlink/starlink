      SUBROUTINE CHR_HTOI( STRING, IVALUE, STATUS )
*+
*  Name:
*     CHR_HTOI

*  Purpose:
*     Read an INTEGER value from a hexadecimal string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_HTOI( STRING, IVALUE, STATUS )

*  Description:
*     The given hexadecimal string is decoded into an INTEGER value.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        String to be decoded.
*     IVALUE = INTEGER (Returned)
*        Value decoded from the given string.
*     STATUS = INTEGER (Given and Returned)
*        The status value. If this value is not SAI__OK on input,
*        the routine returns without action. If the routine fails
*        to complete successfully, STATUS is returned set to
*        SAI__ERROR.

*  Algorithm:
*     Portable Version:
*        Decode the given string explicitly.
*     VAX-specific Version:
*        Construct a Z-format and decode using a Fortran 77 internal
*        READ into the integer argument.

*  Note:
*     This subroutine assumes a 32-bit, twos-compliment representation
*     of an INTEGER.

*  Copyright:
*     Copyright (C) 1982, 1984, 1988, 1989, 1990, 1991, 1994 Science & Engineering Research Council.
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
*     JRG: Jack Giddings (UCL)
*     ACD: A.C. Davenhall (ROE)
*     AJC: A.J. Chipperfield (STARLINK)
*     DLT: D.L. Terrett (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     ACC:  A.C. Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-MAR-1982 (JRG):
*        Original version.
*     19-NOV-1984 (ACD):
*        Documentation improved.
*     3-OCT-1988 (AJC):
*        Documentation improved.
*     16-AUG-1989 (AJC):
*        Use SAE_PAR.
*     22-JAN-1990 (DLT):
*        Eliminate overlapping substring assignment.
*     15-FEB-1991 (PCTR):
*        Portable version.
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

*  Portable version.
*  External References:
      INTEGER CHR_LEN            ! String length (ignoring trailing blanks)

      CHARACTER CHR_UPPER        ! Uppercase character conversion

*  Local Constants:
      CHARACTER HEXCHR * 17      ! Hexadecimal characters
      PARAMETER ( HEXCHR = '0123456789ABCDEF ' )

      INTEGER HEXDEC             ! Decimal 16
      PARAMETER ( HEXDEC = 16 )

      INTEGER LITTLE             ! Smallest integer (4 bytes)
      PARAMETER ( LITTLE = -2147483647 - 1 )

      INTEGER MAXHEX             ! Maximum length of HEXSTR (4 bytes)
      PARAMETER ( MAXHEX = 7 )

*  Local Variables:
      INTEGER HEXPOW( 0 : MAXHEX ) ! Internal array for hexadecimal decode
      INTEGER I                  ! Loop index
      INTEGER IVAL               ! Integer value of hexadecimal digit
      INTEGER NCHAR              ! Character count
      INTEGER NPOWER             ! Highest power of 16 in STRING
      INTEGER POWER              ! Power of 16 used in decoding STRING

      CHARACTER CVALUE           ! Single STRING element

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise IVALUE.
      IVALUE = 0
      NCHAR = CHR_LEN( STRING )

      IF ( NCHAR .GT. 0 ) THEN

*     Build HEXPOW.
         HEXPOW( 0 ) = 0
         NPOWER = 0
         POWER = -1

         DO 10 I = NCHAR, 1, -1
            CVALUE = CHR_UPPER( STRING( I : I ) )
            IVAL = INDEX( HEXCHR, CVALUE )

            IF ( IVAL .GT. 0 ) THEN

               IF ( IVAL .LE. HEXDEC ) THEN
                  POWER = POWER + 1

                  IF ( POWER .LE. MAXHEX ) THEN
                     IF ( IVAL .GT. 1 ) NPOWER = POWER
                     HEXPOW( POWER ) = IVAL - 1
                  ELSE

                     IF ( IVAL .GT. 1 ) THEN

*                    Hexadecimal string has an integer value greater than
*                    4 bytes.
                        STATUS = SAI__ERROR
                        GO TO 999
                     END IF
                  END IF
               END IF
            ELSE

*           Illegal hexadecimal character.
               STATUS = SAI__ERROR
               GO TO 999
            END IF
 10      CONTINUE

*     Go ahead and decode the hexadecimal string.
         IF ( NPOWER .GT. 0 ) THEN

            DO 20 POWER = MIN( NPOWER, MAXHEX-1 ), 1, -1
               IVALUE = ( IVALUE + HEXPOW( POWER ) ) * HEXDEC
 20         CONTINUE

*        Add least significant element.
            IVALUE = IVALUE + HEXPOW( 0 )

*        Finish off with the most significant element.
            IF ( NPOWER .EQ. MAXHEX ) THEN
               IVAL = HEXPOW( MAXHEX )

               IF ( IVAL .LT. 8 ) THEN

*              Positive.
                  IVALUE = IVALUE + IVAL * HEXDEC**MAXHEX
               ELSE IF ( IVAL .GT. 8 ) THEN

*              Negative, so twos compliment required.
                  IVALUE = IVALUE - ( 16-IVAL ) * HEXDEC**MAXHEX
               ELSE

*              Negative, but close to four-byte overflow.
                  IVALUE = IVALUE + LITTLE
               END IF
            END IF
         ELSE
            IVALUE = HEXPOW( 0 )
         END IF
      END IF

 999  CONTINUE

*  VAX-specific version.
*  External References:
*     INTEGER CHR_LEN            ! String length

*  Local Variables:
*     INTEGER IOSTAT             ! Fortran I/O status
*     INTEGER NCHAR              ! Character count

*     CHARACTER COUNT * 3        ! Character count
*     CHARACTER FORMAT * 10      ! Fortran 77 format string

*.

*     IF ( STATUS .NE. SAI__OK ) RETURN

*  Check for commas in the given string.
*     IF ( INDEX( STRING, ',' ) .NE. 0 ) THEN
*        STATUS = SAI__ERROR
*     ELSE
*        NCHAR = CHR_LEN( STRING )
*        WRITE ( COUNT, '(I3)', IOSTAT=IOSTAT ) NCHAR

*        IF ( IOSTAT .NE. 0 ) THEN
*           STATUS = SAI__ERROR
*        ELSE
*           FORMAT = '(BN, Z'//COUNT//')'
*           READ( STRING( 1 : NCHAR ), FORMAT, IOSTAT=IOSTAT ) IVALUE

*           IF ( IOSTAT .NE. 0 ) THEN
*              STATUS = SAI__ERROR
*           END IF
*        END IF
*     END IF

*  Check the returned status value and set the returned INTEGER value
*  on error.
*     IF ( STATUS .EQ. SAI__ERROR ) IVALUE = 0

      END
