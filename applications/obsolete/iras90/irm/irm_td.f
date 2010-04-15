      SUBROUTINE IRM_TD( STRING, IY, IM, ID, IHOUR, IMIN, SEC, MJD,
     :                   STATUS )
*+
*  Name:
*     IRM_TD

*  Purpose:
*     Extract separate fields from a time/date string and convert to
*     a Modified Julian Date.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_TD( STRING, IY, IM, ID, IHOUR, IMIN, SEC, MJD, STATUS )

*  Description:
*     The string should be in the same format as the follow example:
*
*     1992-JAN-1 11:23:23.22
*
*     If the string is valid, separate fields are returned identifying
*     the year, month, day, hour, minutes and seconds. The corresponding
*     Modified Julian Date is also returned. If the string is invalid an
*     error is reported. A blank string causes the current time to be
*     returned.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The string holding the time and date.
*     IY = INTEGER (Returned)
*        The year.
*     IM = INTEGER (Returned)
*        The month, in the range 1 to 12.
*     ID = INTEGER (Returned)
*        The day of the month, starting at 1.
*     IHOUR = INTEGER (Returned)
*        The hour, in the range 0 to 23.
*     IMIN = INTEGER (Returned)
*        The minute in the range 0 to 59.
*     SEC = REAL (Returned)
*        The seconds, in the range 0.0 to 59.999...
*     MJD = DOUBLE PRECISION (Returned)
*        The Modified Julian Date corresponding to the date and time
*        specified by STRING.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If any leading fields are omitted in the string, they default
*     to the values for the current time.
*     -  If any trailing fields are omitted in the string, they default
*     to zero (or 1 for the day and month).
*     -  Years in the range 0 to 49 are interpreted as 2000 to 2049.
*     -  Years in the range 50 to 99 are interpreted as 1950 to 1999.
*     -  No spaces are allowed in the string except to separate the time
*     and date sections.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-JUL-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER STRING*(*)

*  Arguments Returned:
      INTEGER IY
      INTEGER IM
      INTEGER ID
      INTEGER IHOUR
      INTEGER IMIN
      REAL SEC
      DOUBLE PRECISION MJD

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Used length of a string.

*  Local Variables:
      REAL    CSEC               ! Current seconds value.
      REAL    DAY                ! Fraction of a day.
      CHARACTER DELIM*1          ! Current field delimiter.
      CHARACTER DFLD( 3 )*10     ! Individual date fields.
      INTEGER I                  ! Loop count.
      INTEGER ISDST              ! Day-light saving flag.
      INTEGER JD                 ! Current day value.
      INTEGER JERROR             ! SLALIB error code.
      INTEGER JHOUR              ! Current hour value.
      INTEGER JM                 ! Current month value.
      INTEGER JMIN               ! Current minutes value.
      INTEGER JSECS              ! Current integer seconds value.
      INTEGER JY                 ! Current year value.
      CHARACTER LDELIM*1         ! Last field delimiter found.
      INTEGER LTEXT              ! Length of remaining text.
      CHARACTER MONTHS*48        ! List of month abbreviations.
      INTEGER NDF                ! No. of date fields found.
      LOGICAL NONBLK             ! True if any non-blank fields have
                                 ! yet been found.
      INTEGER NTF                ! No. of time fields found.
      INTEGER NTICKS             ! Integer representation of current
                                 ! time.
      INTEGER POS                ! Index of next field delimiter.
      CHARACTER TEXT*50          ! Local copy of supplied string.
      CHARACTER TEXT2*50         ! Temporary copy of TEXT.
      CHARACTER TFLD( 3 )*10     ! Individual time fields.
      INTEGER TSTRCT             ! Pointer to C time structure.
      INTEGER WDAY               ! Current day in week.
      INTEGER YDAY               ! Current day in year.

*  Local Data:
      DATA MONTHS / 'JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC,' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Defer any error reporting.
      CALL ERR_MARK

*  Get the current date and time fields.
      CALL PSX_TIME( NTICKS, STATUS )
      CALL PSX_LOCALTIME( NTICKS, JSECS, JMIN, JHOUR, JD, JM, JY, WDAY,
     :                    YDAY, ISDST, TSTRCT, STATUS )
      CSEC = REAL( JSECS )
      JM = JM + 1

*  Take a local copy of the string,  convert to upper case and remove
*  leading spaces.
      TEXT = STRING
      CALL CHR_UCASE( TEXT )
      CALL CHR_LDBLK( TEXT )

*  Set all time and date fields blank.
      DO I = 1, 3
         TFLD( I ) = ' '
         DFLD( I ) = ' '
      END DO

*  Initialise the number of date and time fields found to zero.
      NTF = 0
      NDF = 0

*  Indicate that the next delimiter to look for is a minus sign (i.e. a
*  date field delimiter). Set the last delimiter found to an illegal
*  value.
      DELIM = '-'
      LDELIM = '*'

*  Loop round extracting date and time fields from the supplied string
*  (from left to right) until the string is empty, or an error occurs.
      DO WHILE( TEXT .NE. ' ' .AND. STATUS .EQ. SAI__OK )
         LTEXT = CHR_LEN( TEXT )

*  Search for the next field delimiter.
         POS = INDEX( TEXT( : LTEXT ), DELIM )

*  If the delimiter of the requested type was not found, change the
*  delimiter to the type appropriate for the next field.
         IF( POS .EQ. 0 ) THEN

            IF( DELIM .EQ. '-' ) THEN
               DELIM = ' '

            ELSE IF( DELIM .EQ. ' ' ) THEN
               DELIM = ':'

*  If no more delimiters exist, use the remaining string as the final
*  field. If the last delimiter found was a minus sign, then the field
*  must be a date field, otherwise it must be a time field.
            ELSE
               IF( LDELIM .EQ. '-' .OR. LDELIM .EQ. '*' ) THEN

                  NDF = NDF + 1
                  IF( NDF .LE. 3 ) THEN
                     DFLD( NDF ) = TEXT( : LTEXT )
                  ELSE
                     STATUS = SAI__ERROR
                  END IF

               ELSE

                  NTF = NTF + 1
                  IF( NTF .LE. 3 ) THEN
                     TFLD( NTF ) = TEXT( : LTEXT )
                  ELSE
                     STATUS = SAI__ERROR
                  END IF

               END IF

               TEXT = ' '

            END IF

*  If a delimiter was found, save it.
         ELSE
            LDELIM = DELIM

*  If the field is a date field, increment the relevant field count
*  and store it if possible.
            IF( DELIM .EQ. '-' .OR. DELIM .EQ. ' ' ) THEN
               NDF = NDF + 1

               IF( NDF .LE. 3 ) THEN
                  IF( POS .GT. 1 ) DFLD( NDF ) = TEXT( : POS - 1 )
               ELSE
                  STATUS = SAI__ERROR
               END IF

*  If there is room for another time field, increment the relevant
*  field count and store it if possible.
            ELSE
               NTF = NTF + 1

               IF( NTF .LE. 3 ) THEN
                  IF( POS .GT. 1 ) TFLD( NTF ) = TEXT( : POS - 1 )
               ELSE
                  STATUS = SAI__ERROR
               END IF

            END IF

*  Set the field blank in the input string.
            TEXT2 = TEXT( POS + 1 : )
            TEXT = TEXT2

         END IF

      END DO

*----------------------------------------------------------------
*  If no date fields supplied...
      IF( NDF .EQ. 0 ) THEN

*  If only two time fields were supplied...
         IF( NTF .EQ. 2 ) THEN

*  ...see if the second one contains a decimal point.
            IF( INDEX( TFLD( 2 ), '.' ) .NE. 0 ) THEN

*  If it does, the first field is the minutes and the second field is
*  the seconds. Store the fields in their correct position, setting the
*  hours field blank.
               TFLD( 3 ) = TFLD( 2 )
               TFLD( 2 ) = TFLD( 1 )
               TFLD( 1 ) = ' '

*  If the second field does not contain a decimal point, assume the two
*  time fields are hours and minutes. Set the seconds field blank.
            ELSE
               TFLD( 3 ) = ' '

            END IF

*  If only one time field was supplied...
         ELSE IF( NTF .EQ. 1 ) THEN

*  ...see it contains a decimal point.
            IF( INDEX( TFLD( 1 ), '.' ) .NE. 0 ) THEN

*  If it does, it is the seconds field. Set the hours and minutes blank.
               TFLD( 3 ) = TFLD( 1 )
               TFLD( 2 ) = ' '
               TFLD( 1 ) = ' '

*  If it does not, assume the field is the hours field. Set the minutes
*  and seconds blank.
            ELSE
               TFLD( 3 ) = ' '
               TFLD( 2 ) = ' '

            END IF

         END IF

*----------------------------------------------------------------
*  Now deal with the cases where there is one date field. Any missing
*  time fields are assumed to be trailing fields and are  left blank.
      ELSE IF( NDF .EQ. 1 ) THEN

*  If there are any time fields then the date field should be the day.
*  Set the year and month fields blank.
         IF( NTF .GT. 0 ) THEN
            DFLD( 3 ) = DFLD( 1 )
            DFLD( 2 ) = ' '
            DFLD( 1 ) = ' '

*  If there are no time fields, see if the date field is a month.
         ELSE
            IM = ( INDEX( MONTHS, DFLD( 1 )( : 3 )//',' ) + 3 )/4
            IF( IM .NE. 0 ) THEN

*  If it is, set the year and day blank.
               DFLD( 2 ) = DFLD( 1 )
               DFLD( 1 ) = ' '
               DFLD( 3 ) = ' '

*  If it is not, assume it is the day. Set the year and month blank.
            ELSE
               DFLD( 3 ) = DFLD( 1 )
               DFLD( 2 ) = ' '
               DFLD( 1 ) = ' '
            END IF

         END IF


*----------------------------------------------------------------
*  Now deal with the cases where there are two date fields. Any missing
*  time fields are assumed to be trailing fields and are  left blank.
      ELSE IF( NDF .EQ. 2 ) THEN

*  If there are any time fields then the date fields should be the
*  month and day. Set the year field blank.
         IF( NTF .GT. 0 ) THEN
            DFLD( 3 ) = DFLD( 2 )
            DFLD( 2 ) = DFLD( 1 )
            DFLD( 1 ) = ' '

*  If there are no time fields, see if the first date field is a month.
         ELSE
            IM = ( INDEX( MONTHS, DFLD( 1 )( : 3 )//',' ) + 3 )/4
            IF( IM .NE. 0 ) THEN

*  If it is, set the year blank and assume the second date field is the
*  day.
               DFLD( 3 ) = DFLD( 2 )
               DFLD( 2 ) = DFLD( 1 )
               DFLD( 1 ) = ' '

*  If it is not, assume the first field is the year and the second
*  field is the month. Set the day blank.
            ELSE
               DFLD( 3 ) = ' '
            END IF

         END IF

      END IF

*  Set a flag indicating that no non-blank fields have yet been found.
      NONBLK = .FALSE.

*  If the year field was supplied, convet it to numerical form.
*  Otherwise use the current value.
      IF( DFLD( 1 ) .NE. ' ' ) THEN
         CALL CHR_CTOI( DFLD( 1 ), IY, STATUS )
         NONBLK = .TRUE.
      ELSE
         IY = JY
      END IF

*  If the month field was supplied, convert it to numerical form.
*  Otherwise set it to 1 unless no non-blank fields have yet been
*  found, in which case set it to the current value.
      IF( DFLD( 2 ) .NE. ' ' ) THEN
         IM = ( INDEX( MONTHS, DFLD( 2 )( : 3 )//',' ) + 3 )/4
         IF( IM .EQ. 0 ) STATUS = SAI__ERROR
         NONBLK = .TRUE.
      ELSE
         IF( NONBLK ) THEN
            IM = 1
         ELSE
            IM = JM
         END IF
      END IF

*  If the day field was supplied, convert it to numerical form.
*  Otherwise set it to 1 unless no non-blank fields have yet been
*  found, in which case set it to the current value.
      IF( DFLD( 3 ) .NE. ' ' ) THEN
         CALL CHR_CTOI( DFLD( 3 ), ID, STATUS )
         NONBLK = .TRUE.
      ELSE
         IF( NONBLK ) THEN
            ID = 1
         ELSE
            ID = JD
         END IF
      END IF

*  If the hour field was supplied, convert it to numerical form.
*  Otherwise set it to 0 unless no non-blank fields have yet been
*  found, in which case set it to the current value.
      IF( TFLD( 1 ) .NE. ' ' ) THEN
         CALL CHR_CTOI( TFLD( 1 ), IHOUR, STATUS )
         NONBLK = .TRUE.
      ELSE
         IF( NONBLK ) THEN
            IHOUR = 0
         ELSE
            IHOUR = JHOUR
         END IF
      END IF

*  If the minutes field was supplied, convert it to numerical form.
*  Otherwise set it to 0 unless no non-blank fields have yet been
*  found, in which case set it to the current value.
      IF( TFLD( 2 ) .NE. ' ' ) THEN
         CALL CHR_CTOI( TFLD( 2 ), IMIN, STATUS )
         NONBLK = .TRUE.
      ELSE
         IF( NONBLK ) THEN
            IMIN = 0
         ELSE
            IMIN = JMIN
         END IF
      END IF

*  If the seconds field was supplied, convert it to numerical form.
*  Otherwise set it to 0 unless no non-blank fields have yet been
*  found, in which case set it to the current value.
      IF( TFLD( 3 ) .NE. ' ' ) THEN
         CALL CHR_CTOR( TFLD( 3 ), SEC, STATUS )
         NONBLK = .TRUE.
      ELSE
         IF( NONBLK ) THEN
            SEC = 0
         ELSE
            SEC = CSEC
         END IF
      END IF

*  If all has gone OK, try to convert the hours, minutes and seconds
*  to a fraction of a day.
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL SLA_CTF2D( IHOUR, IMIN, SEC, DAY, JERROR )

*  If the day was calculated OK, try to convert the year, month and day
*  to a modified Julian date.
         IF( JERROR .EQ. 0 ) CALL SLA_CALDJ( IY, IM, ID, MJD, JERROR )

*  If an error was detected, set the global status.
         IF( JERROR .NE. 0 ) THEN
            STATUS = SAI__ERROR

*  Otherwise add the day fraction on to the modified julian date, and
*  ensure that the year holds the full value.
         ELSE
            MJD = MJD + DBLE( DAY )

            IF( IY .GE. 0 .AND. IY .LE. 49 ) THEN
               IY = IY + 2000

            ELSE IF( IY .GE. 50 .AND. IY .LE. 99 ) THEN
               IY = IY + 1900

            END IF

         END IF

      END IF

*  If an error status is set ensure that an error has been reported and
*  then annul the error condition. Report a general error.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRM_TD_ERR1', 'IRM_TD:', STATUS )
         CALL ERR_ANNUL( STATUS )

         STATUS = SAI__ERROR
         CALL MSG_SETC( 'S', STRING )
         CALL ERR_REP( 'IRM_TD_ERR2',
     :                 'IRM_TD: Illegal date/time given - "^S"',
     :                 STATUS )

      END IF

*  Release the error stack.
      CALL ERR_RLSE

      END
