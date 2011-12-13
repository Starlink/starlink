      SUBROUTINE TRN1_PSCON( EXPRS, ISTART, CONST, IEND, STATUS )
*+
*  Name:
*     TRN1_PSCON

*  Purpose:
*     parse a constant.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_PSCON( EXPRS, ISTART, CONST, IEND, STATUS )

*  Description:
*     The routine parses an expression, looking for a constant starting
*     at the character position ISTART in the expression EXPRS.  If the
*     routine identifies the constant successfully, CONST will return
*     its value and IEND will be set to the position of the final
*     constant character in EXPRS
*
*     If the characters encountered are clearly not part of a constant
*     (it does not begin with a numeral or decimal point) the routine
*     returns with CONST and IEND set to zero, but without setting
*     STATUS.  However, if the first character appears to be a constant
*     but its syntax proves to be invalid, then STATUS is also set and
*     an error is reported.
*
*     This routine is not sensitive to case.  The constant must not
*     have a sign (+ or -) in front of it.

*  Copyright:
*     Copyright (C) 1988, 1992 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     28-MAR-1988 (RFWS):
*        Original version.
*     1-JUN-1988 (RFWS):
*        Changed argument list to match TRN1_PSVAR.
*     8-JUN-1988 (RFWS):
*        Considerably revised to improve detection of bad
*        constant syntax which CHR_CTOD does not detect.
*     13-FEB-1992 (RFWS):
*        Improved error reporting.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'TRN_ERR'          ! TRN_ error codes


*  Arguments Given:
      CHARACTER * ( * ) EXPRS   ! Expression to be parsed
      INTEGER ISTART            ! Initial character position of possible
                                ! constant in EXPRS


*  Arguments Given and Returned:
*     <declarations and descriptions for imported/exported arguments>


*  Arguments Returned:
      DOUBLE PRECISION CONST    ! Constant value
      INTEGER IEND              ! Position of the final constant
                                ! character in EXPRS


*  Status:
      INTEGER STATUS            ! Error status


*  External References:
      LOGICAL CHR_SIMLR         ! String comparison (case insensitive)


*  Global Variables:
*     <any INCLUDE files for global variables held in named COMMON>


*  Local Constants:
*     <local constants defined by PARAMETER>


*  Local Variables:
      LOGICAL ISCON		! Whether character is part of the
				! constant
      LOGICAL VALID		! Constant syntax valid?
      LOGICAL NUMER             ! Numeral encountered in current field?
      LOGICAL DPOINT            ! Decimal point encountered?
      LOGICAL EXPON             ! Exponent character encountered?
      LOGICAL SIGN              ! Sign encountered?
      INTEGER ISTAT             ! Local status variable
      CHARACTER * 1 CH          ! Single character from the expression


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Initialise.
      CONST = 0.0
      IEND = 0


*   Check if the expression starts with a numeral or a decimal point.
      CH = EXPRS( ISTART : ISTART )
      NUMER = ( ( CH .GE. '0' ) .AND. ( CH .LE. '9' ) )
      DPOINT = ( CH .EQ. '.' )


*   If it begins with any of these, the expression is clearly intended
*   to be a constant, so any failure will result in STATUS being set.
*   Assume failure until the constant is verified.
      IF( NUMER .OR. DPOINT ) THEN
        STATUS = TRN__CONIN


*   Initialise the remaining variables specifying the parser context.
        VALID = .TRUE.
        EXPON = .FALSE.
        SIGN = .FALSE.


*   Loop to increment the last constant character position until the
*   following character in the expression does not look like part of
*   the constant.
        IEND = ISTART
        ISCON = .TRUE.
        DO WHILE( ( IEND .LT. LEN( EXPRS ) ) .AND. ( ISCON ) )


*   Extract the next character and assume it is not part of the
*   constant until proved otherwise.  Then proceed to test it...
          CH = EXPRS( IEND + 1 : IEND + 1 )
          ISCON = .FALSE.


*   It may be part of a numerical constant if it is a numeral, wherever
*   it occurs.
          IF( ( CH .GE. '0' ) .AND. ( CH .LE. '9' ) ) THEN
            NUMER = .TRUE.
            ISCON = .TRUE.


*   Or a decimal point, so long as it is the first one and is not in
*   the exponent field.  Otherwise it is invalid.
          ELSE IF( CH .EQ. '.' ) THEN
            IF( .NOT. ( DPOINT .OR. EXPON ) ) THEN
              DPOINT = .TRUE.
              ISCON = .TRUE.
            ELSE
              VALID = .FALSE.
            ENDIF


*   Or if it is an 'E' or 'D' exponent character, so long as it is the
*   first one and at least one numeral has been encountered first.
*   Otherwise it is invalid.
          ELSE IF( CHR_SIMLR( CH, 'E' ) .OR. CHR_SIMLR( CH, 'D' ) ) THEN
            IF( ( .NOT. EXPON ) .AND. NUMER ) THEN
              EXPON = .TRUE.
              NUMER = .FALSE.
              ISCON = .TRUE.
            ELSE
              VALID = .FALSE.
            ENDIF


*   Or if it is a sign, so long as it is in the exponent field and is
*   the first sign with no previous numerals in the same field.
*   Otherwise it is invalid (unless numerals have been encountered, in
*   which case it marks the end of the constant).
          ELSE IF( ( CH .EQ. '+' ) .OR. ( CH .EQ. '-' ) ) THEN
            IF( EXPON .AND. ( .NOT. SIGN ) .AND. ( .NOT. NUMER ) ) THEN
              SIGN = .TRUE.
              ISCON = .TRUE.
            ELSE IF( .NOT. NUMER ) THEN
              VALID = .FALSE.
            ENDIF
          ENDIF


*   Increment the character count if the next character may be part of
*   the constant, or if it was invalid (it will then form part of the
*   error message).
          IF( ISCON .OR. ( .NOT. VALID ) ) IEND = IEND + 1


*   End of "Loop to increment the last character position..." loop.
        ENDDO


*   Finally, check that the last field contained a numeral.
        VALID = ( VALID .AND. NUMER )


*   If valid, try to parse the characters as a constant.
        IF( VALID ) THEN
          ISTAT = SAI__OK
          CALL CHR_CTOD( EXPRS( ISTART : IEND ), CONST, ISTAT )


*   If OK, clear STATUS, indicating success.
          IF( ISTAT .EQ. SAI__OK ) STATUS = SAI__OK


*   End of "suitable characters were found" condition.
        ENDIF


*   If STATUS is set, report an error and reset the output arguments.
        IF( STATUS .NE. SAI__OK ) THEN
          CALL MSG_SETC( 'EXPRS', EXPRS( : IEND ) )
          CALL ERR_REP( 'TRN1_PSCON_ERR',
     :                  'Invalid constant syntax in the expression ' //
     :                  '''^EXPRS''... (possible programming error).',
     :                  STATUS )
          IEND = 0
          CONST = 0.0
        ENDIF


*   End of "expression starts with a numeral or a decimal point or..."
*   condition.
      ENDIF


*   Exit routine.
      END
