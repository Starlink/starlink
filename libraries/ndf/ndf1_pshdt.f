      SUBROUTINE NDF1_PSHDT( STR, YMDHM, SEC, STATUS )
*+
*  Name:
*     NDF1_PSHDT

*  Purpose:
*     Parse a history date/time string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_PSHDT( STR, YMDHM, SEC, STATUS )

*  Description:
*     The routine parses a string containing a history date/time
*     specification read from the history component of an NDF data
*     structure. This is normally expected to be in one of the standard
*     history formats (either of the forms '1993-JUN-01 14:51:4.143' or
*     '1993/JUN/01 14:51:4.143'), but since actual data files may
*     contain other formats, this routine is designed to be flexible;
*     it will accept most reasonable variations on the above forms. The
*     resulting date/time is checked for validity.

*  Arguments:
*     STR = CHARACTER * ( * ) (Given)
*        Date/time string to be parsed.
*     YMDHM( 5 ) = INTEGER (Returned)
*       The year, month, day, hour and minute fields of the date/time
*       (in that order), stored as integers.
*     SEC = REAL (Returned)
*        The seconds (and fractions of a second) field.
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
*     1-JUN-1993 (RFWS):
*        Original version.
*     11-AUG-1993 (RFWS):
*        Extended to accept extra blanks around the date/time string or
*        any of its delimiters, to allow year numbers without the
*        century present, month abbreviatons longer than 3 characters,
*        month fields written as a decimal string, and to cope with
*        swapped year and day fields.
*     {enter_further_changes_here}

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

*  Arguments Returned:
      INTEGER YMDHM( 5 )
      REAL SEC

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL NDF1_SIMLR         ! String compare with abbreviation

*  Local Variables:
      CHARACTER * ( 9 ) MONTH( 12 ) ! Month names
      INTEGER A( 2 )             ! Starting positions of main fields
      INTEGER B( 2 )             ! Ending positions of main fields
      INTEGER F( 10 )            ! Character position of sub-field start
      INTEGER I                  ! Loop counter for fields
      INTEGER II                 ! Counter for field delimiters
      INTEGER IM                 ! Loop counter for months
      INTEGER ISTAT              ! Local status value
      INTEGER L( 10 )            ! Character position of sub-field end
      INTEGER NFIELD             ! Number of fields found
      INTEGER TMP                ! Temporary store for year field value
      LOGICAL OK                 ! Date/time valid?

*  Local Data:
      DATA MONTH / 'JANUARY', 'FEBRUARY', 'MARCH', 'APRIL', 'MAY',
     :             'JUNE', 'JULY', 'AUGUST', 'SEPTEMBER', 'OCTOBER',
     :             'NOVEMBER', 'DECEMBER' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  We start by splitting the whole string into separate fields
*  separated by one or more blanks (if we allow blanks around sub-field
*  delimiters then we may find up to 10 such fields in a valid
*  date/time string - an error results if more than this number are
*  found).
      CALL NDF1_SPLDT( STR, 1, LEN( STR ), ' ', -10, F, L, NFIELD,
     :                 STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  We actually want to split the string initially into only two main
*  fields containing the (Y/M/D) and (H:M:S) values. To detect which of
*  the field separators found is significant, we inspect each in turn
*  and reject those which lie adjacent to one of the sub-field
*  delimiters: '/-:'.
         II = 1
         A( 1 ) = F( 1 )
         DO 1 I = 1, NFIELD - 1
            IF ( ( INDEX( '/-:', STR( L( I ) : L( I ) ) ) .EQ. 0 ) .AND.
     :           ( INDEX( '/-:', STR( F( I + 1 ) : F( I + 1 ) ) )
     :             .EQ. 0 ) ) THEN

*  Check that only one suitable main field delimiter is found.
*  Otherwise there may be an extra field present or a sub-field
*  delimiter missing, so report an error.
               IF ( II .GT. 1 ) THEN
                  STATUS = NDF__DTMIN
                  CALL ERR_REP( 'NDF1_PSHDT_DEL1',
     :                          'Invalid data/time specification; ' //
     :                          'possible extra field or missing ' //
     :                          'delimiter.', STATUS )
                  GO TO 2
               END IF

*  Store the extent of the two main fields we want in the A and B
*  arrays.
               B( II ) = L( I )
               A( II + 1 ) = F( I + 1 )
               II = II + 1
            END IF
 1       CONTINUE
         B( II ) = L( I )

*  If exactly two main fields cannot be found, then there is probably a
*  missing field or delimiter, so report an error.
         IF ( II .LT. 2 ) THEN
            STATUS = NDF__DTMIN
            CALL ERR_REP( 'NDF1_PSHDT_DEL2',
     :                    'Invalid data/time specification; ' //
     :                    'possible missing field or delimiter.',
     :                    STATUS )
         END IF
 2       CONTINUE

*  Decompose the two main fields found above into exactly three
*  sub-fields each, using the appropriate delimiters. Store the
*  resulting field extents consecutively in the F and L arrays.
         CALL NDF1_SPLDT( STR, A( 1 ), B( 1 ), '/-', 3, F( 1 ), L( 1 ),
     :                    NFIELD, STATUS )
         CALL NDF1_SPLDT( STR, A( 2 ), B( 2 ), ':', 3, F( 4 ), L( 4 ),
     :                    NFIELD, STATUS )
      END IF

*  If an error occurred above, then report contextual information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'STR', STR )
         CALL ERR_REP( 'NDF1_PSHDT_CTX1',
     :                 'Error occurred while reading the history ' //
     :                 'date/time string ''^STR''.', STATUS )
      END IF

*  Initialise the local status value.
      ISTAT = SAI__OK

*  Attempt to read the year field, reporting an error if necessary.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL CHR_CTOI( STR( F( 1 ) : L( 1 ) ), YMDHM( 1 ), ISTAT )
         IF ( ISTAT .NE. SAI__OK ) THEN
            STATUS = NDF__DTMIN
            CALL MSG_SETC( 'STR', STR )
            CALL MSG_SETI( 'F', F( 1 ) )
            CALL MSG_SETI( 'L', L( 1 ) )
            CALL ERR_REP( 'NDF1_PSHDT_YR',
     :                    'Invalid year field encountered ' //
     :                    '(characters ^F:^L) in the history ' //
     :                    'date/time string ''^STR''.', STATUS )
         END IF
      END IF

*  Attempt to read the month field, first by comparing it with the name
*  of each month in turn, allowing abbreviation to no less than 3
*  characters.
      IF ( STATUS .EQ. SAI__OK ) THEN
         DO 3 IM = 1, 12
            IF ( NDF1_SIMLR( STR( F( 2 ) : L( 2 ) ), MONTH( IM ), 3 ) )
     :         GO TO 4
 3       CONTINUE
 4       CONTINUE

*  Accept the month number if OK.
         IF ( IM .LE. 12 ) THEN
            YMDHM( 2 ) = IM

*  Otherwise, attempt to read the field as a decimal string. If this
*  also fails, then report an error.
         ELSE
            CALL CHR_CTOI( STR( F( 2 ) : L( 2 ) ), YMDHM( 2 ), ISTAT )
            IF ( ISTAT .NE. SAI__OK ) THEN
               STATUS = NDF__DTMIN
               CALL MSG_SETC( 'STR', STR )
               CALL MSG_SETI( 'F', F( 2 ) )
               CALL MSG_SETI( 'L', L( 2 ) )
               CALL ERR_REP( 'NDF1_PSHDT_MON',
     :                       'Invalid month field encountered ' //
     :                       '(characters ^F:^L) in the history ' //
     :                       'date/time string ''^STR''.', STATUS )
            END IF
         END IF
      END IF

*  Attempt to read the day field, reporting an error if necessary.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL CHR_CTOI( STR( F( 3 ) : L( 3 ) ), YMDHM( 3 ), ISTAT )
         IF ( ISTAT .NE. SAI__OK ) THEN
            STATUS = NDF__DTMIN
            CALL MSG_SETC( 'STR', STR )
            CALL MSG_SETI( 'F', F( 3 ) )
            CALL MSG_SETI( 'L', L( 3 ) )
            CALL ERR_REP( 'NDF1_PSHDT_DAY',
     :                    'Invalid day field encountered ' //
     :                    '(characters ^F:^L) in the history ' //
     :                    'date/time string ''^STR''.', STATUS )
         END IF
      END IF

*  Attempt to read the hour field, reporting an error if necessary.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL CHR_CTOI( STR( F( 4 ) : L( 4 ) ), YMDHM( 4 ), ISTAT )
         IF ( ISTAT .NE. SAI__OK ) THEN
            STATUS = NDF__DTMIN
            CALL MSG_SETC( 'STR', STR )
            CALL MSG_SETI( 'F', F( 4 ) )
            CALL MSG_SETI( 'L', L( 4 ) )
            CALL ERR_REP( 'NDF1_PSHDT_HR',
     :                    'Invalid hour field encountered ' //
     :                    '(characters ^F:^L) in the history ' //
     :                    'date/time string ''^STR''.', STATUS )
         END IF
      END IF

*  Attempt to read the minute field, reporting an error if necessary.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL CHR_CTOI( STR( F( 5 ) : L( 5 ) ), YMDHM( 5 ), ISTAT )
         IF ( ISTAT .NE. SAI__OK ) THEN
            STATUS = NDF__DTMIN
            CALL MSG_SETC( 'STR', STR )
            CALL MSG_SETI( 'F', F( 5 ) )
            CALL MSG_SETI( 'L', L( 5 ) )
            CALL ERR_REP( 'NDF1_PSHDT_MIN',
     :                    'Invalid minute field encountered ' //
     :                    '(characters ^F:^L) in the history ' //
     :                    'date/time string ''^STR''.', STATUS )
         END IF
      END IF

*  Attempt to read the seconds field, reporting an error if necessary.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL CHR_CTOR( STR( F( 6 ) : L( 6 ) ), SEC, ISTAT )
         IF ( ISTAT .NE. SAI__OK ) THEN
            STATUS = NDF__DTMIN
            CALL MSG_SETC( 'STR', STR )
            CALL MSG_SETI( 'F', F( 6 ) )
            CALL MSG_SETI( 'L', L( 6 ) )
            CALL ERR_REP( 'NDF1_PSHDT_SEC',
     :                    'Invalid seconds field encountered ' //
     :                    '(characters ^F:^L) in the history ' //
     :                    'date/time string ''^STR''.', STATUS )
         END IF
      END IF

*  If OK, then save the original year field value and adjust this field
*  to give the full year number by adding the century if necessary.
*  Note that 80 (i.e. 1980) is chosen to pre-date the writing of any
*  NDF history records.
      IF ( STATUS .EQ. SAI__OK ) THEN
         TMP = YMDHM( 1 )
         IF ( YMDHM( 1 ) .LT. 80 ) THEN
            YMDHM( 1 ) = YMDHM( 1 ) + 2000
         ELSE IF ( YMDHM( 1 ) .LT. 100 ) THEN
            YMDHM( 1 ) = YMDHM( 1 ) + 1900
         END IF

*  Mark the error stack and check the date/time fields for validity.
         CALL ERR_MARK
         CALL NDF1_VDAT( YMDHM, SEC, STATUS )

*  If this check detects an error, then it may be that the year and day
*  fields have been swapped (a common variation). Swap these values,
*  restoring the original uncorrected year field value, and correct the
*  new year value by adding the century if necessary.
         IF ( STATUS .NE. SAI__OK ) THEN
            YMDHM( 1 ) = YMDHM( 3 )
            YMDHM( 3 ) = TMP
            IF ( YMDHM( 1 ) .LT. 80 ) THEN
               YMDHM( 1 ) = YMDHM( 1 ) + 2000
            ELSE IF ( YMDHM( 1 ) .LT. 100 ) THEN
               YMDHM( 1 ) = YMDHM( 1 ) + 1900
            END IF

*  Begin a new error reporting environment and check the new field
*  values for validity. Note if they are now OK and end the error
*  reporting environment.
            CALL ERR_BEGIN( STATUS )
            CALL NDF1_VDAT( YMDHM, SEC, STATUS )
            OK = ( STATUS .EQ. SAI__OK )
            CALL ERR_END( STATUS )

*  If the second check was successful, then annul the error from the
*  first check. Release the error stack.
            IF ( OK ) CALL ERR_ANNUL( STATUS )
         END IF
         CALL ERR_RLSE

*  If the validity check failed, then report contextual information.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETC( 'STR', STR )
            CALL ERR_REP( 'NDF1_PSHDT_CTX2',
     :                    'Error occurred while reading the history ' //
     :                    'date/time string ''^STR''.', STATUS )
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_PSHDT', STATUS )

      END
