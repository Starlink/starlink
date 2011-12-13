      LOGICAL FUNCTION CHR1_WILD7( STRING, WILDS, LASTS, LASTW, ISUB1,
     :                             ISUB2, ISUBOF, ISTR1, ISMACH, MATCH )
*+
*  Name:
*     CHR1_WILD7

*  Purpose:
*     Return whether a string matches a wild-card pattern.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = CHR1_WILD7( STRING, WILDS, LASTS, LASTW, ISUB1,
*    :                     ISUB2, ISUBOF, ISTR1, ISMACH, MATCH )

*  Description:
*     The given candidate string is matched with another character
*     string containing a pattern of characters and wild-card characters.
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
*     LASTS = INTEGER (Given)
*        The index of the right-hand STRING character.
*     LASTW = INTEGER (Given)
*        The index of the right-hand WILDS character.
*     ISUB1 = INTEGER (Given)
*        The substring start index.
*     ISUB2 = INTEGER (Given)
*        The substring end index.
*     ISUBOF = INTEGER (Given)
*        The substring start offset.
*     ISTR1 = INTEGER (Given and Returned)
*        The candidate string search start index.
*     ISMACH = LOGICAL (Given and Returned)
*        Whether a string match has been found.
*     MATCH = CHARACTER * ( * ) (Returned)
*        The wild-card match: this string must be the same length as
*        STRING. All characters matched individually are returned as
*        blanks in MATCH, and all characters matched by wild-cards are
*        returned assigned to the particular wild-cards they matched.
*        If the length of MATCH is less than that of STRING, then
*        CHR1_WILD7 returns the value .FALSE.

*  Returned Value:
*     CHR1_WILD7 = LOGICAL
*        Whether errors were found.
*        If errors found, .FALSE.
*        If errors not found, .TRUE.

*  Algorithm:
*     Since there are multi-character wild cards present in the
*     wild-card pattern then compare the two strings substring by
*     substring, where a substring is a string bounded by the string
*     limits or a multi-character wild-card. Begin with the left-hand
*     substring, then the right-hand substring, and then match all
*     remaining substrings from the left-hand side.

*  Copyright:
*     Copyright (C) 1991, 1993 Science & Engineering Research Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
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
*     12-AUG-2004 (TIMJ):
*        Initialise variables that were generating warnings
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) STRING
      CHARACTER * ( * ) WILDS

      INTEGER LASTS              ! Index of the right-hand STRING character
      INTEGER LASTW              ! Index of the right-hand WILDS character
      INTEGER ISUB1              ! Substring start index
      INTEGER ISUB2              ! Substring end index
      INTEGER ISUBOF             ! Substring start offset

*  Arguments Returned:
      INTEGER ISTR1              ! Candidate string search start index

      LOGICAL ISMACH             ! Whether a string match has been found

      CHARACTER * ( * ) MATCH

*  Local Constants:
      INCLUDE 'CHR_SYS'

*  Local Variables:
      INTEGER ICHM               ! Character loop index for MATCH
      INTEGER ICHS               ! Character loop index for STRING
      INTEGER ICHW               ! Character loop index for WILDS
      INTEGER IMCH1              ! Match string start index
      INTEGER IMCH2              ! Match string end index
      INTEGER ISRCH              ! Character search index
      INTEGER ISRCS              ! Candidate string search index
      INTEGER ISRCW              ! Wild pattern search index

      CHARACTER * 1 CHRW         ! WILDS character value

*.

*  Variable initialising
      ISRCS = 0

*  Set errors found.
      CHR1_WILD7 = .FALSE.

*  There is some substring to search for, so search the candidate string
*  for the first occurrence of the substring.
*  Apply the offset to the substring and candidate string start indices
*  and the loop indices.
      ISTR1 = ISTR1 + ISUBOF
      ICHS = ISTR1
      ISUB1 = ISUB1 + ISUBOF
      ICHW = ISUB1

*  Loop to search for the next incidence of the first non-wild substring
*  character in the candidate string.
*  Assign the search character.
      IF ( WILDS( ICHW : ICHW ) .EQ. ESCAPE ) THEN

         IF ( ICHW .EQ. LASTW ) THEN
            GO TO 999
         ELSE
            ICHW = ICHW + 1
            CHRW = WILDS( ICHW : ICHW )
         END IF
      ELSE
         CHRW = WILDS( ICHW : ICHW )
      END IF

*  Re-initialise the string match flag.
      ISMACH = .FALSE.

*  Loop until a string match is found.
*  DO WHILE loop.
 120  CONTINUE
      IF ( .NOT. ISMACH ) THEN

*     Perform the search.
         ISRCH = INDEX( STRING( ISTR1 : ), CHRW )

*     Abort if a character match is not found.
         IF ( ISRCH .EQ. 0 ) GO TO 999

*     Loop to perform the string comparison.
*     Initialise the character pointers.
         ISRCS = ISTR1 + ISRCH - 1
         ISRCW = ICHW

*     Perform the comparison.
*     DO WHILE loop.
 130     CONTINUE
         IF ( ( ISRCS .LE. LASTS )
     :        .AND. ( ISRCW .LE. ISUB2 ) ) THEN

*        Check for single-character wild characters.
            IF ( WILDS( ISRCW : ISRCW ) .EQ. WILDA ) THEN

*           Update the match pattern, MATCH.
               MATCH( ISRCS : ISRCS ) = WILDA
            ELSE

*           Check for literal escape characters.
               IF ( WILDS( ISRCW : ISRCW )
     :              .EQ. ESCAPE ) THEN
                  ISRCW = ISRCW + 1
                  IF ( ISRCW .GT. ISUB2 ) GO TO 140
               END IF

*           Compare the characters.
               IF ( WILDS( ISRCW : ISRCW )
     :              .NE. STRING( ISRCS : ISRCS ) )
     :                 GO TO 140
            END IF

*        Increment the character pointers.
            ISRCW = ISRCW + 1
            ISRCS = ISRCS + 1
         GO TO 130
         END IF

 140     CONTINUE

*     Check for a successful match (there may be some remainder in
*     either of the two strings).
            IF ( ISRCW-1 .EQ. ISUB2 ) THEN
            IMCH1 = ISTR1
            IMCH2 = ISTR1 + ISRCH - 2
            ISMACH = .TRUE.
         ELSE
            IMCH1 = ISTR1
            IMCH2 = ISTR1 + ISRCH - 1
            ICHW = ISUB1
            ISTR1 = ISTR1 + ISRCH
         END IF

*     Update the match pattern if necessary.
         IF ( ISRCH .GT. 1 ) THEN
            DO 150 ICHM = IMCH1, IMCH2
               MATCH( ICHM : ICHM ) = WILDN
 150        CONTINUE
         END IF
      GO TO 120
      END IF

*  Update the candidate string start index.
      ISTR1 = ISRCS

*  Set no errors found.
      CHR1_WILD7 = .TRUE.

999   CONTINUE

      END
