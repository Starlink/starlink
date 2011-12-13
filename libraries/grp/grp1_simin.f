      INTEGER FUNCTION GRP1_SIMIN( A, B, STATUS )
*+
*  Name:
*     GRP1_SIMIN

*  Purpose:
*     Find the index of a given string within another string ignoring
*     the case of both strings.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = GRP1_SIMIN( A, B, STATUS )

*  Description:
*     This function performs the same function as the Fortran intrinsic
*     function INDEX, except that the match between the two strings is
*     case insensitive.

*  Arguments:
*     A = CHARACTER * ( * ) (Given)
*        The string in which to search for the substring.
*     B = CHARACTER * ( * ) (Given)
*        The substring to search for.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     GRP1_SIMIN = INTEGER
*        The offset of the start of the first occurence of substring B
*        within string A (ignoring differences in case). If no match is
*        found a value of zero is returned.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
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
*     18-AUG-1992 (DSB):
*        Original version
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) A
      CHARACTER * ( * ) B

*  External References:
      EXTERNAL CHR_UPPER
      CHARACTER CHR_UPPER*1      ! Return upper case equiavelent of a
                                 ! single letter.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER AOFF               ! Offset within string A of character
                                 ! currently being checked.
      INTEGER BOFF               ! Offset within string B of character
                                 ! currently being searched for.
      LOGICAL FOUND              ! True if a complete match has been
                                 ! found.
      INTEGER LA                 ! Length of string A.
      INTEGER LB                 ! Length of string B.
      LOGICAL MORE               ! True if more characters from string
                                 ! A need to be checked.
      CHARACTER TARGET*1         ! Character currently being searched
                                 ! for.
*.

*  Initialise the returned offset to zero.
      GRP1_SIMIN = 0

*  Check the inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Store the length of the two strings.
      LA = LEN( A )
      LB = LEN( B )

*  If either string has zero length, return with an offset of zero.
      IF( LA .GT. 0 .AND. LB .GT. 0 ) THEN

*  Initialise the offset of the character in string A currently being
*  checked.
         AOFF = 1

*  Initialise the offset of the character in string B currently being
*  searched for.
         BOFF = 1

*  Store the upper case equivalent of the character from string B
*  currently being searched for.
         TARGET = CHR_UPPER( B( 1 : 1 ) )

*  Loop round until string B is found within string A, or string A
*  is exhausted.
         FOUND = .FALSE.
         MORE = .TRUE.
         DO WHILE( MORE )

*  See if the current character from string A matches the character from
*  string B currently being searched for.
            IF( CHR_UPPER( A( AOFF : AOFF ) ) .NE. TARGET ) THEN

*  If not, step to the next character in string A so long as this is not
*  the first character not to match.
               IF( GRP1_SIMIN .EQ. 0 ) THEN
                  AOFF = AOFF + 1

*  If a match between the two strings had been developing but has been
*  broken by this character, reset the target character to be the first
*  character in string B. Do not step to the next character in string A
*  since a check should be performed to see if the current character
*  matches the first character in string B.
               ELSE
                  GRP1_SIMIN = 0
                  BOFF = 1
                  TARGET = CHR_UPPER( B( 1 : 1 ) )

               END IF

*  If the current character from string A matches the character from
*  string B currently being searched for...
            ELSE

*  If this is the first character to match, save the offset.
               IF( GRP1_SIMIN .EQ. 0 ) GRP1_SIMIN = AOFF

*  If more characters from string B remain to be checked, increment the
*  target character to the next one, and increment the character from
*  string A which is to be checked.
               IF( BOFF .LT. LB ) THEN
                  AOFF = AOFF + 1
                  BOFF = BOFF + 1
                  TARGET = CHR_UPPER( B( BOFF : BOFF ) )

*  If all the characters in string B have been matched, return the
*  offset at which the first match was found.
               ELSE
                  MORE = .FALSE.
                  FOUND = .TRUE.

               END IF

            END IF

*  If no more characters remain to be checked in string A, return.
            IF( AOFF .GT. LA ) MORE = .FALSE.

         END DO

*  If a partial match had been found when the end of string A was
*  reached, set the returned offset to zero.
         IF( .NOT. FOUND ) GRP1_SIMIN = 0

      END IF

      END
