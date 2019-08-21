      SUBROUTINE NDF1_PSFMT( FMT, F1, F2, E1, E2, STATUS )
*+
*  Name:
*     NDF1_PSFMT

*  Purpose:
*     Parse a foreign data format specification.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_PSFMT( FMT, F1, F2, E1, E2, STATUS )

*  Description:
*     The routine parses a foreign data format specification of the
*     form NAME(.ext) and locates the "NAME" and ".ext" fields which
*     contain the data format name and the associated file type
*     extension string. Extraneous blanks are ignored and the
*     specificaton is checked for validity.

*  Arguments:
*     FMT = CHARACTER * ( * ) (Given)
*        The foreign data format specification to be parsed.
*     F1 = INTEGER (Returned)
*        Character position of the start of the "NAME" field.
*     F2 = INTEGER (Returned)
*        Character position of the end of the "NAME" field.
*     E1 = INTEGER (Returned)
*        Character position of the start of the ".ext" field.
*     E2 = INTEGER (Returned)
*        Character position of the end of the ".ext" field.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

*  License:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or
*     (at your option) any later version.
*
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program (see SLA_CONDITIONS); if not, write to the
*     Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
*     Boston, MA  02110-1301  USA

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     12-OCT-1993 (RFWS):
*        Original version.
*     15-APR-1994 (RFWS):
*        Added the ANON argument.
*     4-MAY-1994 (RFWS):
*        Added checks on the validity of characters appearing in format
*        names and file extensions.
*     23-DEC-2005 (TIMJ):
*        Use CHR_FPARX rather than NDF1_FPARX
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      CHARACTER * ( * ) FMT

*  Arguments Returned:
      INTEGER F1
      INTEGER F2
      INTEGER E1
      INTEGER E2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 64 ) OKCHRS  ! Valid characters for name/extension
      INTEGER I                  ! Loop counter for characters
      INTEGER I1                 ! First non-blank character position
      INTEGER I2                 ! Last non-blank character position
      INTEGER J1                 ! Start of parentheses
      INTEGER J2                 ! End of parentheses

*  Local Data:
      DATA OKCHRS /
     :'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_-'
     :            /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the first and last non-blank characters in the input string and
*  report an error if the string is entirely blank.
      CALL CHR_FANDL( FMT, I1, I2 )
      IF ( I1 .GT. I2 ) THEN
         STATUS = NDF__FMTIN
         CALL ERR_REP( 'NDF1_PSFMT_BL',
     :                 'Blank data format specified.', STATUS )

*  If OK, search for a parenthesised expression, which should contain
*  the file type extension string.
      ELSE
         CALL CHR_FPARX( FMT( I1 : I2 ), '(', ')', J1, J2 )
         J1 = J1 + I1 - 1
         J2 = J2 + I1 - 1

*  Check that this expression is present.
         IF ( J1 .GT. J2 ) THEN
            STATUS = NDF__FMTIN
            CALL MSG_SETC( 'FMT', FMT( I1 : I2 ) )
            CALL ERR_REP( 'NDF1_PSFMT_PAR',
     :           'Missing parenthesis in the data format ' //
     :           'specification ''^FMT''.', STATUS )

*  Check that the parentheses are not adjacent.
         ELSE IF ( J2 - J1 .LE. 1 ) THEN
            STATUS = NDF__FMTIN
            CALL MSG_SETC( 'FMT', FMT( I1 : I2 ) )
            CALL ERR_REP( 'NDF1_PSFMT_EXT1',
     :           'Missing file type extension in the data format ' //
     :           'specification ''^FMT''.', STATUS )

*  Check that there is a data format name in front of it.
         ELSE IF ( J1 .EQ. I1 ) THEN
            STATUS = NDF__FMTIN
            CALL MSG_SETC( 'FMT', FMT( I1 : I2 ) )
            CALL ERR_REP( 'NDF1_PSFMT_NAME',
     :           'Missing format name in the data format ' //
     :           'specification ''^FMT''.', STATUS )

*  Check that there is nothing following it.
         ELSE IF ( J2 .NE. I2 ) THEN
            STATUS = NDF__FMTIN
            CALL MSG_SETC( 'FMT', FMT( I1 : I2 ) )
            CALL ERR_REP( 'NDF1_PSFMT_MORE',
     :           'Extra characters following the data format in the ' //
     :           'specification ''^FMT''.', STATUS )

*  If OK, remove surrounding blanks from the name field and the file
*  type string within the parentheses.
         ELSE
            CALL CHR_FANDL( FMT( : J1 - 1 ), F1, F2 )
            CALL CHR_FANDL( FMT( J1 + 1 : J2 - 1 ), E1, E2 )
            E1 = E1 + J1
            E2 = E2 + J1

*  Check that the file type string is not blank.
            IF ( E2 - E1 .LT. 1 ) THEN
               STATUS = NDF__FMTIN
               CALL MSG_SETC( 'FMT', FMT( I1 : I2 ) )
               CALL ERR_REP( 'NDF1_PSFMT_EXT2',
     :              'Missing file type extension in the data format ' //
     :              'specification ''^FMT''.', STATUS )

*  Check that the file type string starts with '.'.
            ELSE IF ( FMT( E1 : E1 ) .NE. '.' ) THEN
               STATUS = NDF__FMTIN
               CALL MSG_SETC( 'FMT', FMT( I1 : I2 ) )
               CALL ERR_REP( 'NDF1_PSFMT_DOT',
     :              'The leading ''.'' is missing from the file ' //
     :              'type extension in the data format ' //
     :              'specification ''^FMT''.', STATUS )

*  Examine each character in the format name for validity. Report an
*  error if an invalid character is found.
            ELSE
               DO 1 I = F1, F2
                  IF ( INDEX( OKCHRS, FMT( I : I ) ) .EQ. 0 ) THEN
                     STATUS = NDF__FMTIN
                     CALL MSG_SETC( 'BADCHR', FMT( I : I ) )
                     CALL MSG_SETC( 'FMT', FMT( F1 : F2 ) )
                     CALL ERR_REP( 'NDF1_PSFMT_BAD1',
     :                    'Invalid character ''^BADCHR'' ' //
     :                    'encountered in the data format ' //
     :                    'name ''^FMT''.',
     :                    STATUS )
                     GO TO 2
                  END IF
 1             CONTINUE
 2             CONTINUE

*  Similarly, check each character in the file type extension for
*  validity, reporting an error if necessary.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  DO 3 I = E1, E2
                     IF ( INDEX( '.' // OKCHRS, FMT( I : I ) )
     :                    .EQ. 0 ) THEN
                        STATUS = NDF__FMTIN
                        CALL MSG_SETC( 'BADCHR', FMT( I : I ) )
                        CALL MSG_SETC( 'EXT', FMT( E1 : E2 ) )
                        CALL ERR_REP( 'NDF1_PSFMT_BAD2',
     :                       'Invalid character ''^BADCHR'' ' //
     :                       'encountered in the file type ' //
     :                       'extension ''^EXT''.',
     :                       STATUS )
                        GO TO 4
                     END IF
 3                CONTINUE
 4                CONTINUE
               END IF
            END IF
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_PSFMT', STATUS )

      END
