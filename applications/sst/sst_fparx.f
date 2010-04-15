      SUBROUTINE SST_FPARX( STR, F, L )
*+
*  Name:
*     SST_FPARX

*  Purpose:
*     Find a parenthesised expression in a character string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_FPARX( STR, F, L )

*  Description:
*     The routine searches the string STR to identify a sub-string
*     containing a parenthesised expression and returns the character
*     positions of the opening and closing parentheses in the F and L
*     arguments. Allowance is made for nested parentheses. If a
*     parenthesesed expression was not found, then the returned value
*     of F will be greater than the returned value of L.

*  Arguments:
*     STR = CHARACTER * ( * ) (Given)
*        String to be searched.
*     F = INTEGER (Returned)
*        Character position of the opening parenthesis.
*     L = INTEGER (Returned)
*        Character position of the closing parenthesis.

*  Algorithm:
*     -  Initialise.
*     -  Inspect each character in the string to identify the opening
*     parenthesis. If found, then note its position.
*     -  If an opening parentheses was found, then search subsequent
*     characters for the closing parenthesis.
*     -  Initialise the count of nested parentheses.
*     -  Count opening parentheses.
*     -  Count closing parentheses.
*     -  If the count of nested parentheses falls to zero, then the
*     closing parenthesis has ben found, so note its position.

*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify it under
*     the terms of the GNU General Public License as published by the Free Software
*     Foundation; either version 2 of the License, or (at your option) any later
*     version.
*
*     This program is distributed in the hope that it will be useful,but WITHOUT ANY
*     WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
*     PARTICULAR PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License along with
*     this program; if not, write to the Free Software Foundation, Inc., 59 Temple
*     Place,Suite 330, Boston, MA  02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-OCT-1989 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) STR

*  Arguments Returned:
      INTEGER F
      INTEGER L

*  Local Variables:
      INTEGER I                  ! Loop counter for characters
      INTEGER IPAR               ! Count of nested parentheses
      LOGICAL FOUND              ! Whether first character found

*.

*  Initialise.
      F = 1
      L = 0
      FOUND = .FALSE.

*  Inspect each character in the string, looking for an opening
*  parenthesis.
      DO 1 I = 1, LEN( STR )
         IF ( STR( I : I ) .EQ. '(' ) THEN

*  If found, then note its position.
            FOUND = .TRUE.
            F = I
            GO TO 2
         END IF
1     CONTINUE
2     CONTINUE

*  If the start of an expression has been found, then search for the
*  end.
      IF ( FOUND ) THEN

*  Initialise the count of nested parentheses.
         IPAR = 1

*  Loop to inspect subsequent characters in the string.
         DO 3 I = F + 1, LEN( STR )

*  Count opening parentheses.
            IF ( STR( I : I ) .EQ. '(' ) THEN
               IPAR = IPAR + 1

*  Count closing parentheses.
            ELSE IF ( STR( I : I ) .EQ. ')' ) THEN
               IPAR = IPAR - 1

*  If the number of nested parentheses falls to zero, then the final
*  character of the expression has been found. Note its position.
               IF ( IPAR .EQ. 0 ) THEN
                  L = I
                  GO TO 4
               END IF
            END IF
3        CONTINUE
4        CONTINUE
      END IF

      END
* @(#)sst_fparx.f   1.1   94/12/05 11:31:24   96/07/05 10:27:30
