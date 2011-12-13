      LOGICAL FUNCTION CHR1_WILD4( STRING, WILDS, SLEN, WLEN, MATCH )
*+
*  Name:
*     CHR1_WILD4

*  Purpose:
*     Return whether a string matches a wild-card pattern which contains
*     single-character wild characters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = CHR1_WILD4( STRING, WILDS, SLEN, WLEN, MATCH )

*  Description:
*     The given candidate string (STRING) is matched with another
*     character string (WILDS) containing a pattern of characters and
*     single-character wild-card characters.
*     The wild-cards used are:
*
*        % a single character wild-card;
*
*     There is also a literal escape character '\' for use when the
*     characters '*' and '%' are to be interpreted literally within
*     the wild-card pattern.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The candidate string to be matched.
*     WILDS = CHARACTER * ( * ) (Given)
*        The wild-card pattern to be used in the match.
*     SLEN = INTEGER (Given)
*        The declared length of STRING.
*     WLEN = INTEGER (Given)
*        The declared length of WILDS.
*     MATCH = CHARACTER * ( * ) (Returned)
*        The wild-card match: this string must be the same length as
*        STRING. All characters matched individually are returned as
*        blanks in MATCH, and all characters matched by wild-cards are
*        returned assigned to the particular wild-cards they matched.
*        If the length of MATCH is less than that of STRING, then
*        CHR1_WILD4 returns the value .FALSE.

*  Returned Value:
*     CHR1_WILD4 = LOGICAL
*        Whether the two strings match after expanding the wild-card
*        pattern.

*  Algorithm:
*     Determine if there are any wild-card characters in the wild
*     card pattern.
*     Since there are only single-character wild-card in the wild-card
*     pattern, compare the two strings character by character.

*  Copyright:
*     Copyright (C) 1991, 1993 Science & Engineering Research Council.
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
*     PCTR: P.C.T. Rees (STARLINK)
*     ACC: A.C. Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-FEB-1991 (PCTR):
*        Original version.
*     8-OCT-1991 (PCTR):
*        Final (working) version with changes prompted by P.T. Wallace.
*     8-MAR-1993 (PCTR):
*        Cure bug which leads to a WILDN chracter being present
*        at the beginning of the WILDS string.
*     28-SEP-1993 (ACC):
*        Subprogram created during modularisation.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) STRING
      CHARACTER * ( * ) WILDS

      INTEGER SLEN               ! Declared length of STRING
      INTEGER WLEN               ! Declared length of WILDS

*  Arguments Returned:
      CHARACTER * ( * ) MATCH

*  Local Constants:
      INCLUDE 'CHR_SYS'

*  Local Variables:
      INTEGER ICHS               ! Character loop index for STRING
      INTEGER ICHW               ! Character loop index for WILDS

*.

      CHR1_WILD4 = .FALSE.

*  There are single-character wild characters in the wild-card pattern, so
*  perform a character by character comparison.
*  Initialise the character pointers.
      ICHS = 1
      ICHW = 1

*  Loop to perform the comparison.
*  DO WHILE loop.
 40   CONTINUE
      IF ( ( ICHS .LE. SLEN ) .AND. ( ICHW .LE. WLEN ) ) THEN

*     Check for single-character wild characters.
         IF ( WILDS( ICHW : ICHW ) .EQ. WILDA ) THEN

*        Update the match pattern, MATCH.
            MATCH( ICHS : ICHS ) = WILDA
         ELSE

*        Check for literal escape characters.
            IF ( WILDS( ICHW : ICHW ) .EQ. ESCAPE ) THEN
               IF ( ICHW .LT. WLEN ) ICHW = ICHW + 1
            END IF

*        Compare the characters.
            IF ( WILDS( ICHW : ICHW )
     :           .NE. STRING( ICHS : ICHS ) ) GO TO 50
         END IF

*     Increment the character pointers.
         ICHW = ICHW + 1
         ICHS = ICHS + 1
      GO TO 40
      END IF

*  Check for a successful match (there may be some remainder in either of the
*  two strings).
      IF ( ( ICHS-1 .EQ. SLEN )
     :    .AND. ( ICHW-1 .EQ. WLEN ) ) CHR1_WILD4 = .TRUE.
 50   CONTINUE

*  Annul the match pattern on failure.
      IF ( .NOT. CHR1_WILD4 ) MATCH = ' '

      END
