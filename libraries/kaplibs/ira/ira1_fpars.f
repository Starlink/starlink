      SUBROUTINE IRA1_FPARS( TEXT, MAXFLD, FVAL, TERM, NFLD, OSIGN,
     :                       STATUS )
*+
*  Name:
*     IRA1_FPARS

*  Purpose:
*     Read values and terminators from a formatted sky coordinate.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_FPARS( TEXT, MAXFLD, FVAL, TERM, NFLD, OSIGN, STATUS )

*  Description:
*     The input string should consist of upto MAXFLD fields. Each field
*     starts with a numeric value. The numeric value is terminated by
*     the first non-numeric character (numeric characters are
*     0-9 + - E . ) . The terminating string must be a single character
*     from the set R,D,H,M,S followed by a space, or alternatively, just
*     a space. The numeric values are decoded and returned as floating
*     point values in argument FVAL, and the terminating character for
*     each field is returned in TERM. The number of fields found is
*     returned in NFLD.
*
*     The absolute value of the first field is returned in FVAL(1), and
*     the sign in OSIGN. All other fields are returned in normal signed
*     format. The sign of the first field is determined by the presence
*     of a minus sign as the first character in the string.
*
*     If a field with no terminator has 5 or more digits to the left of
*     the decimal point, then the field is assigned a terminator of
*     "E", for Encoded.

*  Arguments:
*     TEXT = CHARACTER * ( * ) (Given)
*        The input text string.
*     MAXFLD = INTEGER (Given)
*        The maximum number of fields allowed in the input string. If
*        the string contains more than the maximum number of fields, an
*        error is generated.
*     FVAL( MAXFLD ) = DOUBLE PRECISION (Returned)
*        The decoded numeric field values. If any of the numeric values
*        cannot be decoded, an error is generated. The absolute value of
*        the first field is returned, while signed values are returned
*        for the others.
*     TERM( MAXFLD ) = CHARACTER * ( * ) (Returned)
*        The terminating character for each field. These are always
*        R,H,D,M,S,E or <space>.
*     NFLD = INTEGER (Returned)
*        The number of fields found in the input string.
*     OSIGN = INTEGER (Returned)
*        The sign of the first field, determined by the first character
*        in the string. Thus, if the first field is "-0", OSIGN is
*        returned as -1 and FVAL(1) is returned as zero.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*     11-JAN-1991 (DSB):
*        Original version.
*     26-APR-1991 (DSB):
*        Modified for IRA version 2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRA_ERR'          ! IRA errors

*  Arguments Given:
      CHARACTER TEXT*(*)
      INTEGER MAXFLD

*  Arguments Returned:
      DOUBLE PRECISION FVAL( MAXFLD )
      CHARACTER TERM( MAXFLD )*1
      INTEGER NFLD
      INTEGER OSIGN

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER CH*1             ! The current character (upper case).
      LOGICAL  CHR_ISDIG         ! Function which is true if a character
                                 ! is a digit.
      INTEGER  CHR_LEN           ! Function giving used length of a
                                 ! string.
      CHARACTER CHR_UPPER*1      ! Function giving upper case character.
      INTEGER  DOT               ! Position of decimal point relative to
                                 ! start of numeric field.
      INTEGER  END               ! Position of the end of the used part
                                 ! of the string relative to current character.
      CHARACTER FIELD*20         ! Numeric field string.
      DOUBLE PRECISION     FLDVAL! The floating point field value.
      INTEGER  FSTART            ! Position of the start of the current
                                 ! field.
      INTEGER  LENGTH            ! Used length of LTEXT
      CHARACTER LTEXT*(IRA__SZFSC)! Local copy of TEXT.
      LOGICAL  MORE              ! True if the end of the string has not
                                 ! yet been reached.
      LOGICAL  NEWFLD            ! True if a jump to the start of the
                                 ! next field is to be made.
      LOGICAL  NUMIC             ! True if the current character could
                                 ! be part of a numeric field.
      INTEGER  POSN              ! Position of the current character within
                                 ! the string.
      INTEGER  SPACE             ! Position of next space relative to
                                 ! current character.
      INTEGER  START             ! Position of the start of the next field
                                 ! relative to the current character.
      CHARACTER TERMCH*1         ! Current field's terminator character.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy the given text to a local variable to avoid modifying the input
*  string.
      LTEXT = TEXT

*  If the first non-blank character in the string is a minus sign,
*  set OSIGN to -1 and remove the minus sign from the string.
      CALL CHR_FANDL( LTEXT, START, END )

      IF( LTEXT( START : START ) .EQ. '-' ) THEN
         OSIGN = -1
         LTEXT( START : START ) = ' '

      ELSE
         OSIGN = 1

      END IF

*  Save the used length of the string.
      LENGTH = CHR_LEN( LTEXT )

*  Loop through each character in the string.
      NFLD = 0
      POSN = 1
      MORE =.TRUE.
      NEWFLD = .TRUE.

      DO WHILE( MORE .AND. POSN .LE. LENGTH )

*  If a new field is being started, jump to the next non-blank character.
*  This will be the start of the next field. Otherwise, just look at the
*  next character.
         IF( NEWFLD ) THEN
            CALL CHR_FANDL( LTEXT( POSN: ), START, END )
            POSN = POSN + START - 1
            FSTART = POSN
            NEWFLD = .FALSE.

         ELSE
            POSN = POSN + 1

         END IF

         CH = CHR_UPPER( LTEXT( POSN:POSN ) )

*  See if the end of the string has been reached.
         MORE = .NOT.( POSN .EQ. LENGTH .OR. END .EQ. 0 )

*  See if the current character could be part of a numeric value. This
*  is true if the character is a digit, or is one of "E" (which could be
*  the start of an exponent), ".", "+" or "-".
         NUMIC = ( CHR_ISDIG( CH ) .OR. INDEX( '+-.E', CH ) .NE. 0 )

*  If the current character could not be part of a numeric field, or if
*  it is the last character in the string, it is taken to be the end of
*  the current field.
         IF( (.NOT.NUMIC) .OR. (.NOT.MORE) ) THEN

*  Increment the field number.
            NFLD = NFLD + 1

*  If the current character is a numeric character (i.e. the end of the
*  string has been reached with no explicit terminator), extract the
*  numeric field from the start to the current character. The field is
*  considered to be terminated with a space. Give a message if the
*  decoding failed.
            IF( NUMIC ) THEN
               FIELD = LTEXT( FSTART:POSN )
               TERMCH = ' '

*  If the current character is not numeric, check that it is one of the
*  valid terminators, or a space.
            ELSE
               IF( INDEX( 'DHMSR ', CH ) .EQ. 0 ) THEN
                  STATUS = IRA__SCTER
                  CALL MSG_SETC( 'T', CH )
                  CALL ERR_REP( 'IRA1_FPARS_ERR1',
     :                 'IRA1_FPARS: Invalid field terminator found: ^T',
     :                          STATUS )
                  GO TO 999
               END IF

*  If so, extract the numeric field from the start to the last numeric
*  character, and store the terminator,
               IF( POSN .GT. FSTART ) THEN
                  FIELD = LTEXT( FSTART:POSN-1 )
               ELSE
                  FIELD = ' '
               ENDIF

               TERMCH = CH

*  Find the next space. If it is not the current or next character, then
*  give an error mesage. If it is, indicate that a new field is to be
*  found.
               IF( MORE ) THEN
                  SPACE = INDEX( LTEXT( POSN: ), ' ' )

                  IF( SPACE .NE. 0 ) THEN

                     IF( SPACE .EQ. 1 .OR. SPACE .EQ. 2 ) THEN
                        POSN = POSN + 1
                        NEWFLD = .TRUE.
                     ELSE
                        STATUS = IRA__SCTER
                        CALL MSG_SETC( 'T', TEXT( POSN:POSN+SPACE-1 ) )
                        CALL ERR_REP( 'IRA1_FPARS_ERR2',
     :                 'IRA1_FPARS: Invalid field terminator found: ^T',
     :                          STATUS )
                        GO TO 999
                     END IF

                  ELSE
                     MORE = .FALSE.
                  END IF

               END IF

            END IF

*  A blank field is taken as equivalent to zero unless the previous
*  field has a blank terminator. In this case use the terminator for the
*  current field as the terminator for the previous field, and reduce
*  the number of fields by 1.
            IF( FIELD .EQ. ' ' ) THEN
               IF( NFLD .GT. 1 ) THEN
                  IF( TERM( NFLD-1 ) .EQ. ' ' ) THEN
                     NFLD = NFLD - 1
                     FLDVAL = FVAL( NFLD )
                  ELSE
                     FLDVAL = 0
                  END IF
               ELSE
                  FLDVAL = 0
               END IF

*  Attempt to decode non-blank numeric field values.
            ELSE
               CALL CHR_CTOD( FIELD, FLDVAL, STATUS )

               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL MSG_SETC( 'FLD', FIELD( :CHR_LEN( FIELD ) ) )
                  CALL ERR_REP( 'IRA1_FPARS_ERR3',
     :                       'IRA1_FPARS: Error decoding value: ^FLD',
     :                          STATUS )
                  GO TO 999
               END IF

            END IF

*  Fields with blank terminators and no exponent, which have 5 or more
*  digits to the left of any decimal point are considered to have an E
*  terminator ( for Encoded).
            IF( TERMCH .EQ. ' ' .AND. INDEX( FIELD, 'E' ) .EQ. 0 ) THEN
               DOT = INDEX( FIELD, '.')
               IF( ( DOT .EQ. 0 .AND. CHR_LEN(FIELD) .GE. 5 ) .OR.
     :             ( DOT .GT. 5 ) ) TERMCH = 'E'
            END IF

*  Store the terminator and field value unless the maximum number of
*  fields has not been exceeded.
            IF( NFLD .LE. MAXFLD ) THEN
               TERM( NFLD ) = TERMCH
               FVAL( NFLD ) = FLDVAL

            ELSE
               STATUS = IRA__EXFLD
               CALL ERR_REP( 'IRA1_FPARS_ERR4',
     :                   'IRA1_FPARS: String contains too many fields',
     :                       STATUS )
               GO TO 999

            END IF

         END IF

      END DO

 999  CONTINUE

      END
