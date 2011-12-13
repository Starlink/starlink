      SUBROUTINE ARD1_MATCH( STR1, START, L, STR2, NMATCH, N1 )
*+
*  Name:
*     ARD1_MATCH

*  Purpose:
*     Find the number of matching characters at the start of two
*     strings.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_MATCH( STR1, START, L, STR2, NMATCH, N1 )

*  Description:
*     The number of characters at the start of the sub-string
*     STR1( START : L ) which match the corresponding character in
*     STR2( 1 : ) is found. The comparisons are case-insensitive and
*     blanks are ignored.

*  Arguments:
*     STR1 = CHARACTER * ( * ) (Given)
*        The first string.
*     START = INTEGER (Given)
*        The index of the first character to be checked in STR1.
*     L = INTEGER (Given)
*        The index of the last character to be checked in STR1.
*     STR2 = CHARACTER * ( * ) (Given)
*        The second string.
*     NMATCH = INTEGER (Returned)
*        The number of matching characters at the start of the two
*        strings (excluding spaces).
*     N1 = INTEGER (Returned)
*        The total number of characters from STR1 which match STR2,
*        including spaces.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     10-FEB-1994 (DSB):
*        Original version.
*     14-SEP-2001 (DSB):
*        Made comparisons case insensitive.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER STR1*(*)
      INTEGER START
      INTEGER L
      CHARACTER STR2*(*)

*  Arguments Returned:
      INTEGER NMATCH
      INTEGER N1

*  External References:
      CHARACTER CHR_UPPER*1

*  Local Variables:
      CHARACTER
     :  C1,                      ! Current character from STR1
     :  C2                       ! Current character from STR2

      INTEGER
     :  I1,                      ! Index of next character in STR1
     :  I2,                      ! Index of next character in STR2
     :  LEN2                     ! Declared length of STR2

*.

*  Save the declared length of the second string.
      LEN2 = LEN( STR2 )

*  Initialise the pointers to the next character in each string.
      I1 = START
      I2 = 1

*  Initialise the count of matching characters.
      NMATCH = 0

*  Loop round each character in the first string until a non-blank
*  character is found or the end of the string is reached.
 10   CONTINUE

*  Save the current character from the first string.
      C1 = STR1( I1 : I1 )

*  If it is a space, then we need to look at the next character.
      IF( C1 .EQ. ' ' ) THEN
         I1 = I1 + 1

*  If the end of the first string has been reached, then exit.
         IF( I1 .GT. L ) THEN
            GO TO 30

*  Otherwise, go back to check the new character.
         ELSE
            GO TO 10

         END IF

      END IF

*  We now have a non-blank character from the first string. Loop round
*  each character in the second string until a non-blank character is
*  found or the end of the string is reached.
 20   CONTINUE

*  Save the current character from the second string.
      C2 = STR2( I2 : I2 )

*  If it is a space, then we need to look at the next character.
      IF( C2 .EQ. ' ' ) THEN
         I2 = I2 + 1

*  If the end of the second string has been reached, then exit.
         IF( I2 .GT. LEN2 ) THEN
            GO TO 30

*  Otherwise, go back to check the new character.
         ELSE
            GO TO 20

         END IF

      END IF

*  We now have non-blank characters from both strings. Compare them. If
*  they are the same, then increment the number of matching characters,
*  increment the indices of the next characters to be checked in the
*  two strings, and go back to check them so long as neither string has
*  been exhausted.
      IF( CHR_UPPER( C1 ). EQ. CHR_UPPER( C2 ) ) THEN
         NMATCH = NMATCH + 1
         I1 = I1 + 1
         I2 = I2 + 1
         IF( I1 .LE. L .AND. I2 .LE. LEN2 ) GO TO 10
      END IF

*  Jump to here if the end of either of the strings is encountered while
*  looking for the next non-blank character.
 30   CONTINUE

*  Return the number of characters used from the first string.
      N1 = I1 - START

      END
