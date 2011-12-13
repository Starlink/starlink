      SUBROUTINE FTS1_ISKEY( KEYWRD, VALID, STATUS )
*+
*  Name:
*     FTS1_ISKEY

*  Purpose:
*     Inquires whether or not a string is a valid FITS keyword.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_ISKEY( KEYWRD, VALID, STATUS )

*  Description:
*     This routine tests whether a given string would be a valid FITS
*     header keyword or not.  For a keyword to be valid it must be
*     no more than eight characters, and must comprise only uppercase
*     Latin letters, numbers, underscore, and hyphen.

*  Arguments:
*     KEYWRD = CHARACTER * ( * ) (Given)
*        The string to be tested.
*     VALID = LOGICAL (Returned)
*        If true, the string is a valid FITS header keyword.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine does not convert the string to uppercase or remove
*     leading blanks before validation.

*  [optional_subroutine_items]...
*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
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
*     1994 August 21 (MJC):
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
      CHARACTER * ( * ) KEYWRD

*  Arguments Returned:
      LOGICAL VALID

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_IACHR          ! Converts a character to its ASCII
                                 ! sequence number
      INTEGER CHR_LEN            ! Length of string less trailing blanks

*  Local Constants:
      INTEGER KEYLN              ! Maximum length of a FITS keyword
      PARAMETER ( KEYLN = 8 )

*  Local Variables:
      INTEGER ASCOL              ! ASCII sequence number of a character
      INTEGER I                  ! Character counter
      INTEGER NCKEY              ! Number of characters in the keyword

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the returned flag.
      VALID = .TRUE.

*  Get the effective length of the string.
      NCKEY = CHR_LEN( KEYWRD )

*  Invalidate the string if contains too many or too few characters.
      IF ( NCKEY .GT. KEYLN .OR. NCKEY .LT. 1 ) THEN
         VALID = .FALSE.

      ELSE

*  Test each character in turn.  Convert it to its ASCII collating-
*  sequence number, and then test against the list of acceptable
*  values.
         I = 1
  10     CONTINUE
            ASCOL = CHR_IACHR( KEYWRD( I:I ) )
            VALID = ASCOL .EQ. 45 .OR. ASCOL .EQ.  95 .OR.
*                   hyphen              underscore
     :            ( ASCOL .GE. 48 .AND. ASCOL .LE. 57 ) .OR.
*                   numbers
     :            ( ASCOL .GE. 65 .AND. ASCOL .LE. 90 )
*                   Latin uppercase letters

*  Loop to the next character.  Finish testing once an invalid character
*  has been detected, or there are no more characters to test.
            I = I + 1
            IF ( VALID .AND. I .LE. NCKEY ) GOTO 10

      END IF

      END
