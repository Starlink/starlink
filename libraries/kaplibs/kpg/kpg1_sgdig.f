      SUBROUTINE KPG1_SGDIG( STRING, NSDIG, STATUS )
*+
*  Name:
*     KPG1_SGDIG

*  Purpose:
*     Determines the number of significant digits in a number.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_SGDIG( STRING, NSDIG, STATUS )

*  Description:
*     This routine takes a string containing a numerical value, and
*     counts the number of significant digits in the value.  Thus
*     leading and trailing zeroes are excluded.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The string containing the numerical value.  An error is
*        returned if the value cannot be converted to a double-precision
*        value without error.
*     NSDIG = INTEGER (Returned)
*        The number of significant digits in the string if it is treated
*        as a numerical value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Copyright:
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 April 25 (MJC):
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
      CHARACTER * ( * ) STRING

*  Arguments Returned:
      INTEGER NSDIG

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_ISDIG          ! Is a character a digit?
      INTEGER CHR_LEN            ! String length excluding trailing
                                 ! blanks

*  Local Variables:
      INTEGER I                  ! Loop counter for each character
      LOGICAL INPART             ! Counting sig. digits in integer part?
      LOGICAL LEADIN             ! Leading zeroes are not significant?
      LOGICAL LOOP               ! Continue to count in fractional part?
      INTEGER NC                 ! Number of characters
      DOUBLE PRECISION VALUE     ! Numeric value of the string

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the number of digits.
      NSDIG = 0

*  Check that the string is indeed a numerical value.
      CALL CHR_CTOD( STRING, VALUE, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'S', STRING )
         CALL ERR_REP( 'CON_DSDIG_NOTANUM',
     :     'The string ^S is not a number.', STATUS )
         GOTO 999
      END IF

*  Start with the integer part.
      INPART = .TRUE.

*  Assume that there is a leading zero in the integer part.
      LEADIN = .TRUE.

*  Find the effective length of the string.
      NC = CHR_LEN( STRING )

*  Loop through the string, character by character, for the integer part
*  of the number.
      I = 0
      DO WHILE ( INPART .AND. I .LT. NC )

*  Increment the character pointer.
         I = I + 1

*  Deal with integer part.
         IF ( INPART ) THEN

*  Check whether the character is a digit.
            IF ( CHR_ISDIG( STRING( I:I ) ) ) THEN

*  Count non-zero digits.  Subsequent zeroes in the integer part of the
*  value are now significant.
               IF ( STRING( I:I ) .NE. '0' ) THEN
                  NSDIG = NSDIG + 1
                  LEADIN = .FALSE.

*  Only count a zero if it's not a leading zero.
               ELSE IF ( .NOT. LEADIN ) THEN
                  NSDIG = NSDIG + 1

               END IF

*  Look for the decimal point.  This indicates that the fractional part
*  should be examined.
            ELSE IF ( STRING( I:I ) .EQ. '.' ) THEN
               INPART = .FALSE.

            END IF

         END IF

      END DO

*  Leading zeroes in the fractional part are significant only when the
*  integer part is zero, i.e. contains no significant figures.
      LEADIN = NSDIG .EQ. 0

*  Loop through the string, character by character, for the fractional
*  part of the number.
      LOOP = .TRUE.
      DO WHILE ( LOOP .AND. I .LT. NC )

*  Increment the character pointer.
         I = I + 1

*  Check whether the character is a digit.
         IF ( CHR_ISDIG( STRING( I:I ) ) ) THEN

*  Count non-zero digits.  Subsequent zeroes in the fractional part of
*  the value may be significant.  Count them for now even though they
*  may be trailing zeros.  There is a test for these later.
            IF ( STRING( I:I ) .NE. '0' ) THEN
               NSDIG = NSDIG + 1
               LEADIN = .FALSE.

*  Only count a zero if it's not a leading zero.
            ELSE IF ( .NOT. LEADIN ) THEN
               NSDIG = NSDIG + 1

            END IF

*  Any other character terminates the loop.  These are presumably D or
*  E exponents.  Move the character pointer back one to the last digit.
         ELSE
            LOOP = .FALSE.
            NC = I - 1

         END IF

      END DO

*  Remove trailing zeroes from the count.  If the fractional part is all
*  zeroes, then none need removal, because they have not been counted.
      IF ( .NOT. LEADIN ) THEN

         I = NC
         DO WHILE( STRING( I:I ) .EQ. '0' )

*  The current character is a trailing zero in the fractional part.  So
*  decrement the count of significcant digits.
            NSDIG = NSDIG - 1

*  Decrement the character pointer to test the previous character.
            I = I - 1
         END DO
      END IF

  999 CONTINUE
      END
