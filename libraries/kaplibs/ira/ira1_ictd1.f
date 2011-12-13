      SUBROUTINE IRA1_ICTD1( ITEXT, NAME, NC, VALUE, STATUS )
*+
*  Name:
*     IRA1_ICTD1

*  Purpose:
*     Converts a formatted sky coordinate value into a DOUBLE PRECISION
*     value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_ICTD1( ITEXT, NAME, NC, VALUE, STATUS )

*  Description:
*     This routine provides the functionality of IRA_CTOD1 but without
*     argument verification.

*  Arguments:
*     ITEXT = CHARACTER * ( * ) (Given)
*        The string containing the formatted version of the sky
*        coordinate value. If this string is blank, VALUE is returned
*        with the "BAD" value (VAL__BADD), but no error status is
*        created.
*     NAME = CHARACTER * ( * ) (Given)
*        The full sky coordinate system name (with no equinox
*        specifier).
*     NC = INTEGER (Given)
*        Determines which sky coordinate is to be used. If a value of 1
*        is supplied, the string is interpreted as a sky longitude
*        value. If a value of 2 is supplied, the string is interpreted
*        as a sky latitude value.
*     VALUE = DOUBLE PRECISION (Returned)
*        The numerical value of the sky coordinate value held in ITEXT.
*        The value is in radians.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990, 1991 Science & Engineering Research Council.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-SEP-1990 (DSB):
*        Original version.
*     26-APR-1991 (DSB):
*        Changed for second version of IRA.
*     18-SEP-1991 (DSB):
*        Checks on individual field values included.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRA_ERR'          ! IRA errors

*  Arguments Given:
      CHARACTER ITEXT*(*)
      CHARACTER NAME*(*)
      INTEGER   NC

*  Arguments Returned:
      DOUBLE PRECISION VALUE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXFLD             ! Max. no. of fields which the input
                                 ! text string may contain.
      PARAMETER ( MAXFLD = 3 )

*  Local Variables:
      REAL      BADFLD           ! An out of range field value.
      DOUBLE PRECISION DENOM     ! Denominator value for converting a
                                 ! seconds or minutes value to degrees.
      DOUBLE PRECISION FHIA      ! Upper limit on field value when
                                 ! decoding a longitude value.
      DOUBLE PRECISION FHIB      ! Upper limit on field value when
                                 ! decoding a latitude value.
      DOUBLE PRECISION FVAL( MAXFLD )! Numerical value of each field.
      INTEGER   LEVEL            ! 2 means that net field must be
                                 ! seconds or minutes. 1 means that next
                                 ! field must be seconds.
      LOGICAL   LTIME            ! True if a time value is being
                                 ! converted.
      INTEGER   NFLD             ! No. of fields in input string.
      INTEGER   OSIGN            ! The sign of the output value.
      LOGICAL	OUTOK            ! True if values outside the normal
                                 ! field ranges are acceptable.
      CHARACTER TERM(MAXFLD)*1   ! Terminator character for each field
      CHARACTER TEXT*(IRA__SZFSC)! Local copy of the input text string.
      DOUBLE PRECISION VHI       ! Upper limit on absolute value of
                                 ! returned radians value.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If dealing with an equatorial coordinate system, the longitude
*  coordinate is expected to be a time measure (in hours) rather than an
*  angle measure. All other coordinate values are expected to be
*  angular. The user can explicitly override this default by specifying
*  degrees or hours (or radians) in the text string.
      IF( NAME .EQ. 'EQUATORIAL' .AND. NC .EQ. 1) THEN
         LTIME = .TRUE.
      ELSE
         LTIME = .FALSE.
      END IF

*  If the first character in the text is "*", allow field values outside
*  the usual ranges (e.g. a value of 370 degrees is accepted and treated
*  as 10 degrees). Otherwise an error is reported if any field value is
*  outside its normal range.
      IF( ITEXT(1:1) .EQ. '*' ) THEN
         TEXT = ITEXT(2:)
         OUTOK = .TRUE.
      ELSE
         TEXT = ITEXT
         OUTOK = .FALSE.
      END IF

*  If the remaining text is blank, set the output to the "bad" value.
      IF( TEXT .EQ. ' ') THEN
         VALUE = VAL__BADD

*  Otherwise, identify each field and attempt to obtain a DOUBLE
*  PRECISION value and terminator for each field.
      ELSE
         CALL IRA1_FPARS( TEXT, MAXFLD, FVAL, TERM, NFLD, OSIGN,
     :                    STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Set the upper limit on the magnitude of the final value. This limit
*  is not imposed if the text string starts with a "*" character.
*  Longitude values can vary between 0 and 2.PI.
            IF( NC .EQ. 1 ) THEN
               VHI = 2.0*IRA__PI + 1.0E-10

*  Negative sky longitude values are only allowed if the supplied text
*  started with a "*" character.
               IF( OSIGN .LT. 0 ) THEN
                  IF( .NOT. OUTOK ) THEN
                     STATUS = IRA__RANGE
                     BADFLD = -FVAL(1)
                     GO TO 999
                  END IF
               END IF

*  Latitude values can vary between + and - PI/2.
            ELSE
               VHI = IRA__PIBY2 + 1.0E-10

            END IF

*  Look at the first field (IRA1_FPARS returns the absolute value
*  of the first field, with the sign in OSIGN).
            IF( NFLD .GE. 1 ) THEN

*  If it is an "hours" field set the time flag to true, initialize the
*  output radians value, and indicate that the next field must be
*  either a minutes or seconds field.
               IF( TERM(1) .EQ. 'H' ) THEN
                  LTIME = .TRUE.
                  VALUE = FVAL(1)*15.0D0*IRA__DTOR
                  LEVEL = 2

                  FHIA = 24.0
                  FHIB = 6.0

*  Time values are not usually allowed when specifying sky latitudes.
                  IF( NC .EQ. 2 ) THEN
                     IF( .NOT. OUTOK ) THEN
                        STATUS = IRA__TIME
                        GO TO 999
                     END IF
                  END IF

*  If the first field is a "degrees" field set the time flag to false,
*  initialize the output radians value, and indicate that the next
*  field must be either a minutes or seconds field. Check that the field
*  value is acceptable.
               ELSE IF( TERM(1) .EQ. 'D' ) THEN
                  LTIME = .FALSE.
                  VALUE = FVAL(1)*IRA__DTOR
                  LEVEL = 2

                  FHIA = 360.0
                  FHIB = 90.0

*  If the first field is a "radians" field set the output radians
*  value. No further fields are allowed. If any were provided, set an
*  error state. Check that the field value is acceptable.
               ELSE IF( TERM(1) .EQ. 'R' ) THEN
                  VALUE = FVAL(1)
                  IF( NFLD .GT. 1 ) STATUS = IRA__REDFL

                  FHIA = 2.0*IRA__PI
                  FHIB = IRA__PIBY2

*  If the first field is "Encoded", assume that the value represents an
*  "encoded" hours or degrees value. Call a subroutine to decode the
*  value into hours or degrees, then convert to radians. In this case
*  no further fields are allowed.  If any were provided, set an error
*  state. Check that the resulting value is acceptable.
               ELSE IF( TERM(1) .EQ. 'E' ) THEN
                  CALL IRA1_DECOD( FVAL(1), VALUE, BADFLD, STATUS )

                  IF( STATUS .EQ. SAI__OK ) THEN
                     IF( LTIME ) THEN
                        VALUE = VALUE*15.0D0*IRA__DTOR
                     ELSE
                        VALUE = VALUE*IRA__DTOR
                     END IF

                     IF( NFLD .GT. 1 ) STATUS = IRA__REDFL

                     FHIA = VAL__MAXD
                     FHIB = VAL__MAXD

                  END IF

*  If the first field has no terminator, assume that it gives an hours
*  or degrees value, depending on the value of time flag. Indicate that
*  the next field must be either a minutes or seconds field. Check that
*  the field value is acceptable.
               ELSE IF( TERM(1) .EQ. ' ') THEN
                  IF( LTIME ) THEN
                     VALUE = FVAL(1)*15.0D0*IRA__DTOR
                     FHIA = 24.0
                     FHIB = 6

                  ELSE
                     VALUE = FVAL(1)*IRA__DTOR
                     FHIA = 360.0
                     FHIB = 90.0

                  END IF

                  LEVEL = 2

*  If the first field is a "minutes" field, use the time flag to
*  convert the field value to radians, and initialize the output value.
*  Indicate that the next field must be a seconds field and check that
*  the field value is acceptable.
               ELSE IF( TERM(1) .EQ. 'M' ) THEN

                  IF( LTIME ) THEN
                     VALUE = FVAL(1)*IRA__DTOR/4.0D0
                  ELSE
                     VALUE = FVAL(1)*IRA__DTOR/60.0D0
                  END IF

                  LEVEL = 1

                  FHIA = 60.0
                  FHIB = 60.0


*  If the first field is a "seconds" field, use the time flag to
*  convert the field value to radians, and initialize the output value.
*  In this case no further fields are allowed. If any were provided,
*  set an error state.
               ELSE IF( TERM(1) .EQ. 'S' ) THEN

                  IF( LTIME ) THEN
                     VALUE = FVAL(1)*IRA__DTOR/240.0D0
                  ELSE
                     VALUE = FVAL(1)*IRA__DTOR/3600.0D0
                  END IF

                  IF( NFLD .GT. 1 ) STATUS = IRA__REDFL

                  FHIA = 60.0
                  FHIB = 60.0

               END IF

*  If an error occurred, jump to the end.
               IF( STATUS .NE. SAI__OK ) GO TO 999

*  Unless the text string started with an "*" character, set an error
*  status for field values outside their normal range. Also check the
*  radians value is OK.
               IF( .NOT. OUTOK ) THEN
                  IF( ( NC .EQ. 1 .AND. FVAL(1) .GE. FHIA ) .OR.
     :                ( NC .EQ. 2 .AND. FVAL(1) .GT. FHIB ) .OR.
     :                ( VALUE .GT. VHI ) ) THEN
                     STATUS = IRA__RANGE
                     BADFLD = OSIGN*FVAL(1)
                  END IF
               END IF

            END IF

*  Now look at the second field. If it is an "hours", "degrees" or
*  "radians" field, set an error state.
            IF( NFLD .GE. 2 ) THEN
               IF( TERM(2) .EQ. 'H' .OR. TERM(2) .EQ. 'D' .OR.
     :             TERM(2) .EQ. 'R' ) THEN
                  STATUS = IRA__MIXFL
                  GO TO 999
               END IF

*  If it is negative or greater than 60.0, set an error state (unless
*  the string began with a "*").
               IF( FVAL(2) .LT. 0.0 .OR. FVAL(2) .GE. 60.0) THEN
                  IF( .NOT. OUTOK ) THEN
                     STATUS =IRA__RANGE
                     BADFLD = FVAL(2)
                     GO TO 999
                  END IF
               END IF

*  If the second field has no terminator, assume that it gives a
*  minutes or seconds value, depending on the value of LEVEL. Increment
*  the output radians value using the value of the time flag to
*  determine the conversion from field value to radians. If the field
*  is in fact a "seconds" field, no more fields are allowed. In this
*  case give an error if any are present.
               IF( TERM(2) .EQ. ' ') THEN

                  IF( LEVEL .EQ. 2 ) THEN
                     DENOM = 60.0D0
                     LEVEL = 1
                  ELSE
                     DENOM = 3600.0D0
                     IF( NFLD .GT. 2 ) STATUS = IRA__REDFL
                  END IF

                  IF( LTIME ) THEN
                     VALUE = VALUE + FVAL(2)*15.0D0*IRA__DTOR/DENOM
                  ELSE
                     VALUE = VALUE + FVAL(2)*IRA__DTOR/DENOM
                  END IF

*  If the second field is a "minutes" field, use the time flag to
*  convert the field value to radians, and increment the output value.
*  Indicate that the next field must be a seconds field. If the first
*  field was a "minutes" field, give an error.
               ELSE IF( TERM(2) .EQ. 'M' ) THEN
                  IF( LEVEL .EQ. 2 ) THEN

                     IF( LTIME ) THEN
                        VALUE = VALUE + FVAL(2)*IRA__DTOR/4.0D0
                     ELSE
                        VALUE = VALUE + FVAL(2)*IRA__DTOR/60.0D0
                     END IF

                     LEVEL = 1

                  ELSE
                     STATUS = IRA__MIXFL
                  END IF

*  If the second field is a "seconds" field, use the time flag to
*  convert the field value to radians, and increment the output value.
*  In this case no further fields are allowed. If any were provided,
*  set an error state.
               ELSE IF( TERM(2) .EQ. 'S' ) THEN

                  IF( LTIME ) THEN
                     VALUE = VALUE + FVAL(2)*IRA__DTOR/240.0D0
                  ELSE
                     VALUE = VALUE + FVAL(2)*IRA__DTOR/3600.0D0
                  END IF

                  IF( NFLD .GT. 2 ) STATUS = IRA__REDFL

               END IF

*  If an error occurred, jump to the end.
               IF( STATUS .NE. SAI__OK ) GO TO 999

*  Check that the radians value is within range (if necessary).
               IF( ( .NOT. OUTOK ) .AND. ( VALUE .GT. VHI ) ) THEN
                  STATUS = IRA__RANGE
                  BADFLD = FVAL(2)
               END IF

            END IF

*  Now look at the third field. It MUST be a "seconds" field. If it is
*  an "hours", "degrees", "radians" or "minutes" field, set an error
*  state.
            IF( NFLD .GE. 3 ) THEN
               IF( TERM(3) .EQ. 'H' .OR. TERM(3) .EQ. 'D' .OR.
     :             TERM(3) .EQ. 'R' .OR. TERM(3) .EQ. 'M' ) THEN
                  STATUS = IRA__MIXFL
                  GO TO 999
               END IF

*  If it is negative or greater than 60.0, set an error state (unless
*  the string began with a "*").
               IF( FVAL(3) .LT. 0.0 .OR. FVAL(3) .GE. 60.0) THEN
                  IF( .NOT. OUTOK ) THEN
                     STATUS =IRA__RANGE
                     BADFLD = FVAL(3)
                     GO TO 999
                  END IF
               END IF

*  Increment the output radians value using the value of the time flag
*  to determine the conversion from field value to radians.
               IF( LTIME ) THEN
                  VALUE = VALUE + FVAL(3)*IRA__DTOR/240.0D0
               ELSE
                  VALUE = VALUE + FVAL(3)*IRA__DTOR/3600.0D0
               END IF

*  Check that the radians value is within range (if necessary).
               IF( ( .NOT. OUTOK ) .AND. ( VALUE .GT. VHI ) ) THEN
                  STATUS = IRA__RANGE
                  BADFLD = FVAL(3)
               END IF

            END IF

*  Ensure that the output value has the correct sign.
            VALUE = VALUE * OSIGN

*  If a bad value was found give the appropriate error message.
 999        CONTINUE

            IF ( STATUS .EQ. IRA__REDFL ) THEN
               CALL MSG_SETC( 'TX', TEXT )
               CALL ERR_REP( 'IRA1_ICTD1_ERR1',
     :                  'IRA1_ICTD1: Redundant field found in "^TX"',
     :                       STATUS )

            ELSE IF ( STATUS .EQ. IRA__MIXFL ) THEN
               CALL MSG_SETC( 'TX', TEXT )
               CALL ERR_REP( 'IRA1_ICTD1_ERR2',
     :                  'IRA1_ICTD1: Field found out of order in "^TX"',
     :                       STATUS )

            ELSE IF ( STATUS .EQ. IRA__RANGE ) THEN
               CALL MSG_SETC( 'TX', TEXT )
               CALL MSG_SETR( 'FLD', BADFLD )
               CALL ERR_REP( 'IRA1_ICTD1_ERR3',
     :                    'IRA1_ICTD1: Value out of range - "^FLD" ',
     :                       STATUS )

            ELSE IF ( STATUS .EQ. IRA__TIME ) THEN
               CALL MSG_SETC( 'TX', TEXT )
               CALL ERR_REP( 'IRA1_ICTD1_ERR4',
     :     'IRA1_ICTD1: Hours value not allowed in this context: "^TX"',
     :                       STATUS )

            END IF

         END IF

      END IF

      END
