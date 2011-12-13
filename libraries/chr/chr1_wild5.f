      LOGICAL FUNCTION CHR1_WILD5( STRING, WILDS, SLEN, WLEN, MLEN,
     :                           FIRSTN, LASTN, NWILDA, NWILDN, MATCH )
*+
*  Name:
*     CHR1_WILD5

*  Purpose:
*     Return whether a string matches a wild-card pattern.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = CHR1_WILD5( STRING, WILDS, SLEN, WLEN, MLEN,
*    :                     FIRSTN, LASTN, NWILDA, NWILDN, MATCH )

*  Description:
*     The given candidate string is matched with another character
*     string containing a pattern of characters and wild-card
*     characters.
*     The wild-cards used are:
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
*     SLEN = INTEGER (Given)
*        The declared length of STRING.
*     WLEN = INTEGER (Given)
*        The declared length of WILDS.
*     MLEN = INTEGER (Given)
*        The declared length of MATCH.
*     FIRSTN = INTEGER (Given)
*        The index of the left-hand WILDN character.
*     LASTN = INTEGER (Given)
*        The index of the right-hand WILDN character.
*     NWILDA = INTEGER (Given)
*        The number of WILDA wild characters in WILDS.
*     NWILDN = INTEGER (Given)
*        The number of WILDN wild characters in WILDS.
*     MATCH = CHARACTER * ( * ) (Returned)
*        The wild-card match: this string must be the same length as
*        STRING. All characters matched individually are returned as
*        blanks in MATCH, and all characters matched by wild-cards are
*        returned assigned to the particular wild-cards they matched.
*        If the length of MATCH is less than that of STRING, then
*        CHR1_WILD5 returns the value .FALSE.

*  Returned Value:
*     CHR1_WILD5 = LOGICAL
*        Whether the two strings match after expanding the wild-card
*        pattern.

*  Algorithm:
*     Since there are multi-character wild cards present in the
*     wild-card pattern then compare the two strings substring by
*     substring, where a substring is a string bounded by the string
*     limits or a multi-character wild-card. Begin with the left-hand
*     substring, then the right-hand substring, and then match all
*     remaining substrings from the left-hand side.

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
      INTEGER MLEN               ! Declared length of MATCH
      INTEGER FIRSTN             ! Index of the left-hand WILDN character
      INTEGER LASTN              ! Index of the right-hand WILDN character
      INTEGER NWILDA             ! Number of WILDA wild characters in WILDS
      INTEGER NWILDN             ! Number of WILDN wild characters in WILDS

*  Arguments Returned:
      CHARACTER * ( * ) MATCH

*  Local Constants:
      INCLUDE 'CHR_SYS'

*  Local Variables:
      LOGICAL ISMACH             ! Whether a string match has been found
      LOGICAL CHR1_WILD6         ! Whether errors matching last substring
      LOGICAL CHR1_WILD7         ! If errors searching for 1st substring

      INTEGER ICHM               ! Character loop index for MATCH
      INTEGER ICHW               ! Character loop index for WILDS
      INTEGER INW                ! WILDN substring loop index
      INTEGER ISTR1              ! Candidate string search start index
      INTEGER ISTR2              ! Candidate string search end index
      INTEGER ISUBOF             ! Substring start offset
      INTEGER ISUB1              ! Substring start index
      INTEGER ISUB2              ! Substring end index
      INTEGER IWLD1              ! Wild-card pattern search start index
      INTEGER IWLD2              ! Wild-card pattern search end index
      INTEGER FIRSTS             ! Index of the left-hand STRING character
      INTEGER FIRSTW             ! Index of the left-hand WILDS character
      INTEGER LASTS              ! Index of the right-hand STRING character
      INTEGER LASTW              ! Index of the right-hand WILDS character

*.

*  There are multi-character wild characters in WILDS, so the
*  string comparison must be made on the identifiable non-wild
*  characters in WILDS. Loop to search for and test each of the
*  non-wild substrings present in WILDS within the candidate string.

*  Initialise CHR1_WILD5.
         CHR1_WILD5 = .FALSE.

*  Search in reverse through the candidate string for a
*  match to the last substring in the wild-card pattern.
*  If there was an error, exit.
      IF ( .NOT. CHR1_WILD6( STRING, WILDS, SLEN, WLEN, FIRSTN, LASTN,
     :               MATCH, FIRSTS, FIRSTW, LASTS, LASTW ) ) GO TO 999

*  Check whether the wild-card pattern has been exhausted (for
*  NWILDN=1).
      IF ( FIRSTW .GT. LASTW ) THEN

*     The wild-card pattern has been exhausted with a resulting
*     successful match. Assign the returned value of CHR1_WILD5 and
*     update the match pattern.
         CHR1_WILD5 = .TRUE.

         DO 190 ICHM = FIRSTS, LASTS
            MATCH( ICHM : ICHM ) = WILDN
 190     CONTINUE
      ELSE

*  Loop to search for and compare all wild-card pattern substrings
*  within the candidate string.
*  Initialise the character loop indices.
         ISTR1 = FIRSTS
         ISTR2 = LASTS
         IWLD1 = FIRSTW
         IWLD2 = LASTW

*     Initialise the substring match flag.
         ISMACH = .FALSE.

*     Loop for all substrings in the wild-card pattern.
         DO 170 INW = 1, NWILDN+1

*        Loop to find the next multi-character wild character.
*        Initialise the loop index and the substring start index.
            ICHW = IWLD1
            ISUB1 = IWLD1

*        DO WHILE loop.
 70         CONTINUE
            IF ( ICHW .LE. IWLD2 ) THEN

*           Check for literal escape characters.
               IF ( WILDS( ICHW : ICHW ) .EQ. ESCAPE ) THEN
                  ICHW = ICHW + 1
                  IF ( ICHW .GT. IWLD2 ) GO TO 80
               ELSE IF ( WILDS( ICHW : ICHW ) .EQ. WILDN ) THEN
                  GO TO 80
               END IF

*           Increment the character pointer.
               ICHW = ICHW + 1
            GO TO 70
            END IF

 80         CONTINUE

*        Update the wild-card pattern start index.
            IWLD1 = ICHW + 1

*        Find substring indices.
            ISUB2 = ICHW - 1

*        Check that the substring is not of zero length.
            IF ( ISUB1 .LE. ISUB2 ) THEN

*           There is a substring, so search the substring for the
*           first non-WILDA character.

               DO 90 ICHW = ISUB1, ISUB2
                  IF ( WILDS( ICHW : ICHW ) .NE. WILDA ) GO TO 100
 90            CONTINUE

*           Determine the offset for the start of the search.
 100           CONTINUE
               ISUBOF = ICHW - ISUB1

*           Update the match pattern if necessary.
               IF ( ISUBOF .GT. 0 ) THEN
                  DO 110 ICHM = ISTR1, ISTR1+ISUBOF-1
                     MATCH( ICHM : ICHM ) = WILDA
 110              CONTINUE
               END IF

*           Check that the substring is not all single-character
*           wild characters.
               IF ( ICHW .LE. ISUB2 ) THEN

*              There is some substring to search for, so search
*              the candidate string for the first occurrence of
*              the substring.
*              If error encountered, exit.
                  IF ( .NOT. CHR1_WILD7( STRING, WILDS, LASTS, LASTW,
     :                                   ISUB1, ISUB2, ISUBOF, ISTR1,
     :                                   ISMACH, MATCH ) ) GO TO 999
               ELSE

*              The substring is all single-character wild characters,
*              so just update the candidate string start index.
                  ISTR1 = ISTR1 + ISUBOF
               END IF
            ELSE

*           For substrings of zero length, there is always a match.
               ISMACH = .TRUE.

*           Update the match pattern and candidate string indices
*           if necessary (i.e. a trailing multi-character wild
*           character).
               IF ( ISUB2 .EQ. LASTW ) THEN
                  DO 160 ICHM = ISTR1, LASTS
                     MATCH( ICHM : ICHM ) = WILDN
 160              CONTINUE

                  ISTR1 = ISTR2 + 1
               END IF
            END IF
 170     CONTINUE

*     Assign the returned value of CHR1_WILD5.
         CHR1_WILD5 = ISMACH

*     Check if the candidate string has been exhausted.
         IF ( ISTR1 .LE. ISTR2 ) THEN

*     The candidate string has not been exausted, so fill the remainder
*     of the match pattern with WILDN characters.
            DO 200 ICHM = ISTR1, ISTR2
               MATCH( ICHM : ICHM ) = WILDN
 200        CONTINUE
         END IF
      END IF

*  Annul the match pattern on failure.
 999  CONTINUE
      IF ( .NOT. CHR1_WILD5 ) MATCH = ' '

      END
