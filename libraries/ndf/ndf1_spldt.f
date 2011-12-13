      SUBROUTINE NDF1_SPLDT( STR, SBEG, SEND, DELIM, MXFLD, FBEG, FEND,
     :                       NFIELD, STATUS )
*+
*  Name:
*     NDF1_SPLDT

*  Purpose:
*     Split the fields in a date/time string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_SPLDT( STR, SBEG, SEND, DELIM, MXFLD, FBEG, FEND,
*                      NFIELD, STATUS )

*  Description:
*     The routine identifies a series of fields within a string (or
*     sub-string) which are separated by one of a set of delimiters and
*     returns the character positions identifying the start and end of
*     each field.  Surrounding blank characters are stripped, both from
*     the original string (or sub-string) and from each field. An error
*     results if the expected number of fields is exceeded or
*     (optionally) if too few fields are found. An error also results
*     if any field is empty or entirely blank.
*
*     This routine is intended for identifying the separate date and
*     time fields within a date/time string.

*  Arguments:
*     STR = CHARACTER * ( * ) (Given)
*        The string to be analysed.
*     SBEG = INTEGER (Given)
*        Character position at which to start analysing STR.
*     SEND = INTEGER (Given)
*        Character position at which to finish analysing STR.
*     DELIM = CHARACTER * ( * ) (Given)
*        A sequence of delimiter characters, any one of which will be
*        taken as a field separator if it is found within STR (a blank
*        may also be used, although multiple adjacent blanks will only
*        count as a single occurrence of a delimiter).
*     MXFLD = INTEGER (Given)
*        The maximum number of fields expected. The absolute value of
*        this argument is used. If it is negative, then a smaller
*        number of fields will be accepted. If it is positive, then the
*        number of fields found must match the value given exactly or
*        an error will result.
*     FBEG( * ) = INTEGER (Returned)
*        The character positions of the start of each field (this array
*        should have at least ABS(MXFLD) elements).
*     FEND( * ) = INTEGER (Returned)
*        The character positions of the end of each field (this array
*        should have at least ABS(MXFLD) elements).
*     NFIELD = INTEGER (Returned)
*        The actual number of fields found.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     11-AUG-1993 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      CHARACTER * ( * ) STR
      INTEGER SBEG
      INTEGER SEND
      CHARACTER * ( * ) DELIM
      INTEGER MXFLD

*  Arguments Returned:
      INTEGER FBEG( * )
      INTEGER FEND( * )
      INTEGER NFIELD

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Variables:
      INTEGER F                  ! First non-blank character
      INTEGER I1                 ! Character position of field start
      INTEGER I2                 ! Character position of field end
      INTEGER L                  ! Last non-blank character (junk)
      INTEGER LFIELD             ! Significant length of field
      INTEGER LSTR               ! Last character in STR to consider

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the field count.
      NFIELD = 0

*  Initialise the character pointer to the first non-blank character
*  position to consider and also find the last non-blank character to
*  consider.
      CALL CHR_FANDL( STR( SBEG : SEND ), I1, LSTR )
      I1 = SBEG - 1 + I1
      LSTR = SBEG - 1 + LSTR

*  Loop to identify each field until an error occurs or we reach the end
*  of the region of STR being analysed.
 1    CONTINUE                   ! Start of "DO WHILE" loop
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( I1 .LE. LSTR ) ) THEN

*  Count each field and report an error if we have found too many.
         NFIELD = NFIELD + 1
         IF ( NFIELD .GT. ABS ( MXFLD ) ) THEN
            STATUS = NDF__DTMIN
            CALL ERR_REP( 'NDF1_SPLDT_XS',
     :                    'Invalid date/time specification; too ' //
     :                    'many fields found.', STATUS )
         ELSE

*  Advance the character pointer to the first non-blank character in
*  the field.
            CALL CHR_FANDL( STR( I1 : LSTR ), F, L )
            I1 = I1 - 1 + F

*  Find the final character in the field (the last character before the
*  next delimiter or end of string).
            DO 2 I2 = I1, LSTR
               IF ( INDEX( DELIM, STR( I2 : I2 ) ) .NE. 0 ) GO TO 3
 2          CONTINUE
 3          CONTINUE
            I2 = I2 - 1

*  Check if two adjacent delimiters were found and report an error if
*  so.
            IF ( I2 .LT. I1 ) THEN
               STATUS = NDF__DTMIN
               CALL ERR_REP( 'NDF1_SPLDT_MIS',
     :                       'Invalid date/time specification; ' //
     :                       'field value is missing.', STATUS )

*  Find the length of the field with trailing blanks removed and store
*  the first and last character positions.
            ELSE
               LFIELD = CHR_LEN( STR( I1 : I2 ) )
               FBEG( NFIELD ) = I1
               FEND( NFIELD ) = I1 + LFIELD - 1
            END IF
         END IF

*  Increment the character pointer to the start of the next field and
*  return to process it.
         I1 = I2 + 2
         GO TO 1
      END IF

*  If OK, but insufficient fields have been found, then report an
*  error.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( ( MXFLD .GT. 0 ) .AND. ( NFIELD .LT. MXFLD ) ) THEN
            STATUS = NDF__DTMIN
            CALL ERR_REP( 'NDF1_SPLDT_2FEW',
     :                    'Invalid date/time specification; too few ' //
     :                    'fields found.', STATUS )
         END IF
      END IF

      END
