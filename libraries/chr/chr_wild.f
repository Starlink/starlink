      LOGICAL FUNCTION CHR_WILD( STRING, WILDS, MATCH )
*+
*  Name:
*     CHR_WILD

*  Purpose:
*     Return whether a string matches a wild-card pattern.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = CHR_WILD( STRING, WILDS, MATCH )

*  Description:
*     A candidate string is matched with a another character
*     string containing a pattern of characters and wild-card
*     characters.  The wild-cards used are:
*
*        % a single character wild-card;
*        * an arbitrary length string wild-card, including zero length.
*
*     There is also a literal escape character '\' for use when the
*     characters '*' and '%' are to be interpreted literally within
*     the wild-card pattern.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The candidate string to be matched.
*     WILDS = CHARACTER * ( * ) (Given)
*        The wild-card pattern to be used in the match.
*     MATCH = CHARACTER * ( * ) (Returned)
*        The wild-card match: this string must be the same length as
*        STRING. All characters matched individually are returned as
*        blanks in MATCH, and all characters matched by wild-cards are
*        returned assigned to the particular wild-cards they matched.
*        If the length of MATCH is less than that of STRING, then
*        CHR_WILD returns the value .FALSE.

*  Returned Value:
*     CHR_WILD = LOGICAL
*        Whether the two strings match after expanding the wild-card
*        pattern.

*  Algorithm:
*     -  Determine the declared lengths of each of the character
*     string arguments.
*     -  Deal with the trivial cases of match and no match first.
*     -  Determine if there are any wild-card characters in the wild
*     card pattern.
*        o  If there are no wild-card characters in the wild-card
*        pattern then simply compare the two strings.
*        o If there are only single-character wild-card in the wild-card
*        pattern then compare the two strings character by character.
*        o If there are multi-character wild cards present in the
*        wild-card pattern then compare the two strings substring by
*        substring, where a substring is a string bounded by the string
*        limits or a multi-character wild-card. Begin with the left-hand
*        substring, then the right-hand substring, and then match all
*        remaining substrings from the left-hand side.
*
*               |--CHR1_WILD1
*               |  validate inputs
*               |
*               |--CHR1_WILD2
*               |  find num wild chars
*               |
*     CHR_WILD--|--CHR1_WILD3
*               |  process no wild chars
*               |
*               |--CHR1_WILD4
*               |  process single char wild chars
*               |
*               |
*               |
*               |              |--CHR1_WILD6
*               |              |  search reverse
*               |--CHR1_WILD5--|
*                  process     |--CHR1_WILD7
*                  multi char     search for substring
*                  wild chars

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
*     27-SEP-1993 (ACC):
*        Modularised.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) STRING
      CHARACTER * ( * ) WILDS

*  Arguments Returned:
      CHARACTER * ( * ) MATCH

*  Local Constants include file:
      INCLUDE 'CHR_SYS'

*  Local Variables:
      LOGICAL CHR1_WILD1         ! Inputs valid?
      LOGICAL CHR1_WILD3         ! Match with no wild-cards?
      LOGICAL CHR1_WILD4         ! Match with single char wild-cards?
      LOGICAL CHR1_WILD5         ! Match with multi char wild-cards?

      INTEGER FIRSTN             ! Index of the left-hand WILDN character
      INTEGER LASTN              ! Index of the right-hand WILDN character
      INTEGER MLEN               ! Declared length of MATCH
      INTEGER NWILDA             ! Number of WILDA wild characters in WILDS
      INTEGER NWILDN             ! Number of WILDN wild characters in WILDS
      INTEGER SLEN               ! Declared length of STRING
      INTEGER WLEN               ! Declared length of WILDS

*.

*  Find the declared lengths of all character string arguments.
      SLEN = LEN( STRING )
      WLEN = LEN( WILDS )
      MLEN = LEN( MATCH )

*  Initialise the match string.
      MATCH = ' '

*  Initialise the returned value, CHR_WILD.
      CHR_WILD = .FALSE.

*  Check that inputs are valid
      IF ( .NOT. CHR1_WILD1( SLEN, MLEN, WLEN ) ) GO TO 999

*  Deal with the trivial case first.
      IF ( ( SLEN .EQ. 0 ) .AND. ( WLEN .EQ. 0 ) ) THEN

*     Trivial match, two zero length strings.
         CHR_WILD = .TRUE.
         GO TO 999
      END IF

*   Find the number of wild-card characters (NWILDA and NWILDN) in
*   WILDS.
      CALL CHR1_WILD2 ( WILDS, WLEN, NWILDA, NWILDN, FIRSTN, LASTN )

*   Are there multi-character wild characters in WILDS?
      IF ( NWILDN .EQ. 0 ) THEN

*     There are no multi-character wild characters in WILDS, so the
*     string comparison can be performed character by character.
*     Are there single-character wild characters in WILDS?
         IF ( NWILDA .EQ. 0 ) THEN

*        There are no wild characters in the wild-card pattern, so
*        perform a character by character comparison.
            CHR_WILD = CHR1_WILD3 ( STRING, WILDS, SLEN, WLEN )

         ELSE

*        There are single-character wild characters in the wild-card
*        pattern, so perform a character by character comparison.
            CHR_WILD = CHR1_WILD4 ( STRING, WILDS, SLEN, WLEN, MATCH )

         END IF
      ELSE

*     There are multi-character wild characters in WILDS, so the
*     string comparison must be made on the identifiable non-wild
*     characters in WILDS. Loop to search for and test each of the
*     non-wild substrings present in WILDS within the candidate string.
         CHR_WILD = CHR1_WILD5( STRING, WILDS, SLEN, WLEN, MLEN,
     :                        FIRSTN, LASTN, NWILDA, NWILDN, MATCH )

      END IF

 999  CONTINUE

      END
