      LOGICAL FUNCTION CHR1_WILD6( STRING, WILDS, SLEN, WLEN, FIRSTN,
     :                      LASTN, MATCH, FIRSTS, FIRSTW, LASTS, LASTW )
*+
*  Name:
*     CHR1_WILD6

*  Purpose:
*     Match last substring in wild-card pattern.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = CHR1_WILD6( STRING, WILDS, SLEN, WLEN, FIRSTN, LASTN,
*    :                     MATCH, FIRSTS, FIRSTW, LASTS, LASTW )

*  Description:
*     Search in reverse through the candidate string (STRING) for a
*     match to the last substring in the wild-card pattern.
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
*     FIRSTN = INTEGER (Given)
*        The index of the left-hand WILDN character.
*     LASTN = INTEGER (Given)
*        The index of the right-hand WILDN character.
*     MATCH = CHARACTER * ( * ) (Returned)
*        The wild-card match: this string must be the same length as
*        STRING. All characters matched individually are returned as
*        blanks in MATCH, and all characters matched by wild-cards are
*        returned assigned to the particular wild-cards they matched.
*        If the length of MATCH is less than that of STRING, then
*        CHR1_WILD6 returns the value .FALSE.
*     FIRSTS = INTEGER (Returned)
*        The index of the left-hand STRING character.
*     FIRSTW = INTEGER (Returned)
*        The index of the left-hand WILDS character.
*     LASTS = INTEGER (Returned)
*        The index of the right-hand STRING character.
*     LASTW = INTEGER (Returned)
*        The index of the right-hand WILDS character.

*  Returned Value:
*     CHR1_WILD6 = LOGICAL
*        If errors were encountered = .FALSE.
*        If no errors were encountered = .TRUE.

*  Algorithm:
*     Search in reverse through the candidate string (STRING) for a
*     match to the last substring in the wild-card pattern.

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
      INTEGER FIRSTN             ! Index of the left-hand WILDN character
      INTEGER LASTN              ! Index of the right-hand WILDN character

*  Arguments Returned:
      CHARACTER * ( * ) MATCH

      INTEGER FIRSTS             ! Index of the left-hand STRING character
      INTEGER FIRSTW             ! Index of the left-hand WILDS character
      INTEGER LASTS              ! Index of the right-hand STRING character
      INTEGER LASTW              ! Index of the right-hand WILDS character

*  Local Constants:
      INCLUDE 'CHR_SYS'

*  Local Variables:
      INTEGER ICHS               ! Character loop index for STRING
      INTEGER ICHW               ! Character loop index for WILDS

      CHARACTER * 1 CHRW         ! WILDS character value

*.

*  Initialise the first character indices for the candidate string and
*  the wild-card pattern.
      FIRSTS = 1
      FIRSTW = 1

*  Set errors-encountered flag.
      CHR1_WILD6 = .FALSE.

*  Search the beginning of the candidate string for a match to
*  the first substring in the wild-card pattern (assuming the
*  wild-card pattern does not begin with a WILDN character).
      IF ( FIRSTN .GT. FIRSTW ) THEN

*     There is a substring before the first multi-character wild
*     character, so perform the string comparison.  Initialise
*     the character loop indices.
         ICHW = FIRSTW
         ICHS = FIRSTS

*     DO WHILE loop.
 60      CONTINUE
         IF ( ICHW .LT. FIRSTN ) THEN

*        Check that the candidate string has not been over-run.
            IF ( ICHS .GT. SLEN ) GO TO 999

*        Check for an escaped character.
            IF ( WILDS( ICHW : ICHW ) .EQ. ESCAPE ) THEN

*           Assign the test character and test the string
*           character.
               ICHW = ICHW + 1
               CHRW = WILDS( ICHW : ICHW )
               IF ( STRING( ICHS : ICHS ) .NE. CHRW ) GO TO 999
            ELSE

*           Assign the test character and test the string
*           character.
               CHRW = WILDS( ICHW : ICHW )

*           Check for single-character wild characters.
               IF ( CHRW .EQ. WILDA ) THEN

*              Update the match pattern.
                  MATCH( ICHS : ICHS ) = WILDA
               ELSE
                  IF ( STRING( ICHS : ICHS ) .NE. CHRW ) GO TO 999
               END IF
            END IF

*        Update the character pointers.
            ICHW = ICHW + 1
            ICHS = ICHS + 1
         GO TO 60
         END IF

*     Update the last character indices.
         FIRSTS = ICHS
         FIRSTW = ICHW + 1
      ELSE

*     The first character in the wild-card pattern is a WILDN
*     character, so move the wild-card pattern pointer to the
*     next character.
         FIRSTW = FIRSTN + 1
      END IF

*  Search in reverse through the candidate string for a match to the
*  last substring in the wild-card pattern.
      LASTS = SLEN
      LASTW = WLEN

      IF ( LASTN .LT. WLEN ) THEN

*     There is a substring after the last multi-character wild
*     character, so perform the string comparison.
*     Initialise the character loop indices.
         ICHW = LASTW
         ICHS = LASTS

*     DO WHILE loop.
 160     CONTINUE
         IF ( ICHW .GT. LASTN ) THEN

*        Check that the candidate string has not been over-run.
            IF ( ICHS .LE. FIRSTS-1 ) GO TO 999

*        Check for an escaped character.
            IF ( WILDS( ICHW-1 : ICHW-1 ) .EQ. ESCAPE ) THEN

*           Assign the test character and test the string character.
               CHRW = WILDS( ICHW : ICHW )
               ICHW = ICHW - 1
               IF ( STRING( ICHS : ICHS ) .NE. CHRW ) GO TO 999
            ELSE

*           Assign the test character and test the string character.
               CHRW = WILDS( ICHW : ICHW )

*           Check for single-character  wild characters.
               IF ( CHRW .EQ. WILDA ) THEN

*              Update the match pattern.
                  MATCH( ICHS : ICHS ) = WILDA
               ELSE
                  IF ( STRING( ICHS : ICHS ) .NE. CHRW ) GO TO 999
               END IF
            END IF

*        Update the character pointers.
            ICHW = ICHW - 1
            ICHS = ICHS - 1
         GO TO 160
         END IF

*     Update the last character indices.
         LASTS = ICHS
         LASTW = ICHW - 1
      ELSE

*     The last character in the wild-card pattern is a WILDN
*     character, so move the wild-card pattern pointer to the
*     previous character.
         LASTW = LASTN - 1
      END IF

*  No errors encountered
      CHR1_WILD6 = .TRUE.

 999  CONTINUE

      END
