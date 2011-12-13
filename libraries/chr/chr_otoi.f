      SUBROUTINE CHR_OTOI( STRING, IVALUE, STATUS )
*+
*  Name:
*     CHR_OTOI

*  Purpose:
*     Read an INTEGER value from an octal string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_OTOI( STRING, IVALUE, STATUS )

*  Description:
*     The given octal string is decoded into an INTEGER value.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        String to be decoded.
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
*     VAX-specific Version:
*        Construct an O-format and decode using a Fortran 77 internal
*        READ into the integer argument.

*  Copyright:
*     Copyright (C) 1982, 1984, 1988, 1989, 1991, 1994 Science & Engineering Research Council.
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
*     PCTR: P.C.T. Rees (STARLINK)
*     ACC:  A.C. Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-MAR-1982 (JRG):
*        Original version.
*     19-NOV-1984 (ACD):
*        Documentation improved.
*     26-OCT-1988 (AJC):
*        Documentation improved.
*     16-AUG-1989 (AJC):
*        Use SAE_PAR.
*     18-FEB-1991 (PCTR):
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

*  Portable Version.
*  External References:
      INTEGER CHR_LEN            ! String length (ignoring trailing blanks)

*  Local Constants:
      INTEGER LITTLE             ! Smallest integer (4 bytes)
      PARAMETER ( LITTLE = -2147483647 - 1 )

      CHARACTER OCTCHR * 9       ! Octal characters
      PARAMETER ( OCTCHR = '01234567 ' )

      INTEGER OCTDEC             ! Decimal 8
      PARAMETER ( OCTDEC = 8 )

      INTEGER MAXOCT             ! Maximum length of OCTSTR (4 bytes)
      PARAMETER ( MAXOCT = 10 )

*  Local Variables:
      INTEGER I                  ! Loop index
      INTEGER IVAL               ! Integer value of octal digit
      INTEGER NCHAR              ! Character count
      INTEGER NPOWER             ! Highest power of 8 in STRING
      INTEGER OCTPOW( 0 : MAXOCT ) ! Internal array for octal decode
      INTEGER POWER              ! Power of 8 used in decoding STRING

      CHARACTER CVALUE           ! Single STRING element

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise IVALUE.
      IVALUE = 0
      NCHAR = CHR_LEN( STRING )

      IF ( NCHAR .GT. 0 ) THEN

*     Build OCTPOW.
         OCTPOW( 0 ) = 0
         NPOWER = 0
         POWER = -1

         DO 10 I = NCHAR, 1, -1
            CVALUE = STRING( I : I )
            IVAL = INDEX( OCTCHR, CVALUE )

            IF ( IVAL .GT. 0 ) THEN

               IF ( IVAL .LE. OCTDEC ) THEN
                  POWER = POWER + 1

                  IF ( POWER .LE. MAXOCT ) THEN
                     IF ( IVAL .GT. 1 ) NPOWER = POWER
                     OCTPOW( POWER ) = IVAL - 1
                  ELSE

                     IF ( IVAL .GT. 1 ) THEN

*                    Octal string has an integer value greater than
*                    4 bytes.
                        STATUS = SAI__ERROR
                        GO TO 999
                     END IF
                  END IF
               END IF
            ELSE

*           Illegal octal character.
               STATUS = SAI__ERROR
               GO TO 999
            END IF
 10      CONTINUE

*     Go ahead and decode the octal string.
         IF ( NPOWER .GT. 0 ) THEN

            DO 20 POWER = MIN( NPOWER, MAXOCT-1 ), 1, -1
               IVALUE = ( IVALUE + OCTPOW( POWER ) ) * OCTDEC
 20         CONTINUE

*        Add the least significant element.
            IVALUE = IVALUE + OCTPOW( 0 )

*        Finish off the most significant element.
            IF ( NPOWER .EQ. MAXOCT ) THEN
               IVAL = OCTPOW( MAXOCT )

               IF ( IVAL .EQ. 1 ) THEN

*              Positive.
                  IVALUE = IVALUE + OCTDEC**MAXOCT
               ELSE IF ( IVAL .EQ. 2 ) THEN

*              Negative, but close to four-byte overflow.
                  IVALUE = IVALUE + LITTLE
               ELSE IF ( IVAL .EQ. 3 ) THEN

*              Negative, so twos compliment required.
                  IVALUE = IVALUE - OCTDEC**MAXOCT
               ELSE

*              Overflow.
                  STATUS = SAI__ERROR
                  IVALUE = 0
               END IF
            END IF
         ELSE
            IVALUE = OCTPOW( 0 )
         END IF
      END IF

 999  CONTINUE

*  VAX-specific version.
*  External References:
*     INTEGER CHR_LEN            ! String length (ignoring trailing blanks)

*  Local Variables:
*     INTEGER IOSTAT             ! Fortran  I/O status
*     INTEGER NCHAR              ! Character count

*     CHARACTER COUNT * 3        ! Fortran 77 READ format string
*     CHARACTER FORMAT * 10      ! Fortran 77 WRITE format string

*.

*     IF ( STATUS .NE. SAI__OK ) RETURN

*  Check for commas in the given string.
*     IF ( INDEX( STRING, ',' ) .NE. 0 ) THEN
*        STATUS = SAI__ERROR
*     ELSE
*        NCHAR = CHR_LEN( STRING )
*        WRITE( COUNT, '(I3)', IOSTAT=IOSTAT ) NCHAR

*     IF ( IOSTAT .NE. 0 ) THEN
*        STATUS = SAI__ERROR
*     ELSE
*        FORMAT = '(BN, O'//COUNT//')'
*        READ ( STRING( 1 : NCHAR ), FORMAT, IOSTAT=IOSTAT ) IVALUE

*        IF ( IOSTAT .NE. 0 ) THEN
*           STATUS = SAI__ERROR
*        END IF
*     END IF

*  Check the returned status value and set the returned INTEGER value
*  on error.
*     IF ( STATUS .EQ. SAI__ERROR ) IVALUE = 0

      END
