      SUBROUTINE TRN1_ISNAM( STR, OK, LNAM )
*+
*  Name:
*     TRN1_ISNAM

*  Purpose:
*     Check if a character string is a name.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_ISNAM( STR, OK, LNAM )

*  Description:
*     The routine determines whether a character string contains a
*     name.  To be a name, the string must start with an alphabetic
*     character and continue with alphanumeric characters only
*     (including '_').  Trailing blanks are ignored in a name string,
*     but there must be no embedded blanks.
*
*     If the string is a name, OK is returned .TRUE. and the LNAM
*     argument returns the name's length.  Otherwise, OK is returned
*     .FALSE. and LNAM returns the position of the first character
*     which is not part of a name.  This routine is not sensitive to
*     case.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     12-MAY-1988 (RFWS):
*        Original version.
*     21-FEB-1992 (RFWS):
*        Use CHR_ISALF and CHR_ISALM for character class tests.
*     21-JUL-1992 (RFWS):
*        Removed STATUS variable - erroneously checked but not passed
*        as an argument.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'TRN_ERR'          ! TRN_ error codes

*  Arguments Given:
      CHARACTER * ( * ) STR     ! The string to be checked

*  Arguments Returned:
      LOGICAL OK                ! Whether the string is a name
      INTEGER LNAM              ! The length of the name, or the
                                ! position of the first character which
                                ! is not part of a name

*  External References:
      LOGICAL CHR_ISALF          ! Character is alphabetic?
      LOGICAL CHR_ISALM          ! Character alphenumeric/underscore?

*  Local Variables:
      LOGICAL BLANK             ! Whether a character is a blank

*.

*  Check the first character is valid.
      IF( .NOT. CHR_ISALF( STR( 1 : 1 ) ) ) THEN
        OK = .FALSE.
        LNAM = 1

*  Loop to check the remaining characters until a blank or an invalid
*  character is encountered.
      ELSE
        LNAM = 1
        BLANK = .FALSE.
        OK = .TRUE.
        DO WHILE ( ( LNAM .LT. LEN( STR ) ) .AND. ( .NOT. BLANK ) .AND.
     :             ( OK ) )
          LNAM = LNAM + 1
          BLANK = ( STR( LNAM : LNAM ) .EQ. ' ' )

*  Test each non-blank character for validity.
          IF( .NOT. BLANK ) OK = CHR_ISALM( STR( LNAM : LNAM ) )
        ENDDO

*  If there are any remaining characters, check that they are all
*  blank.
        IF( OK .AND. ( LNAM .LT. LEN( STR ) ) ) THEN
          OK = ( STR( LNAM + 1 : ) .EQ. ' ' )
        ENDIF

*  Correct the string length if it was terminated by a blank.
        IF( OK .AND. BLANK ) LNAM = LNAM - 1
      ENDIF

*  Exit routine.
      END
