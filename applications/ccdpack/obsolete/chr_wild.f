      LOGICAL FUNCTION CHR_WILD( STRING, WILDS, ABBREV, MATCH )
*+
*  Name:
*     CHR_WILD

*  Purpose:
*     Return whether a string matches a wild-card pattern.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = CHR_WILD( STRING, WILDS, ABBREV, MATCH )

*  Description:
*     The given candidate string is matched with a another character
*     string containing a pattern of characters and wild-card
*     characters.  The wild-cards used are:
*
*        % a single character wild-card;
*        * an arbitrary length string wild-card, including zero length.
*
*     There is also a literal escape character '\' for use when the 
*     characters '*' and '%' are to be interpretted literally within 
*     the wild-card pattern.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The candidate string to be matched.
*     WILDS = CHARACTER * ( * ) (Given)
*        The wild-card pattern to be used in the match.
*     ABBREV = LOGICAL (Given)
*        Whether abbreviations are allowed. If the abbreviation flag
*        ABBREV, is given as .TRUE., the candidate string may be an
*        abbreviation substring of the wild-card pattern, WILDS, as
*        long as no wild-card characters appear in WILDS. If the
*        abbreviation flag is given as .FALSE. then STRING must match
*        the pattern given in WILDS.
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
*        pattern then simply compare the two strings, allowing 
*        abbreviations where neccesary.
*        o If there are only single-character wild-card in the wild-card
*        pattern then compare the two strings character by character.
*        o If there are multi-character wild cards present in the
*        wild-card pattern then compare the two strings substring by
*        substring, where a substring is a string bounded by the string
*        limits or a multi-character wild-card. Begin with the left-hand
*        substring, then the right-hand substring, and then match all
*        remaining substrings from the left-hand side.

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-FEB-1991 (PCTR):
*        Original version.
*     8-OCT-1991 (PCTR):
*        Final (working) version with changes prompted by P.T. Wallace.
*     8-MAR-1993 (PCTR):
*        Cure bug which leads to a WILDN character being assumed present
*        at the beginning of the WILDS string.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) STRING
      CHARACTER * ( * ) WILDS

      LOGICAL ABBREV

*  Arguments Returned:
      CHARACTER * ( * ) MATCH

*  Local Constants:
      CHARACTER ESCAPE           ! Escape character
      PARAMETER ( ESCAPE = '\\' )

      CHARACTER WILDA            ! Single character wild-card
      PARAMETER ( WILDA = '%' )

      CHARACTER WILDN            ! Arbitrary length string wild-card
      PARAMETER ( WILDN = '*' )

*  Local Variables:
      LOGICAL ISMACH             ! Whether a string match has been found

      INTEGER FIRSTN             ! Index of the left-hand WILDN character
      INTEGER FIRSTS             ! Index of the left-hand STRING character
      INTEGER FIRSTW             ! Index of the left-hand WILDS character
      INTEGER ICHM               ! Character loop index for MATCH
      INTEGER ICHS               ! Character loop index for STRING
      INTEGER ICHW               ! Character loop index for WILDS
      INTEGER IMCH1              ! Match string start index
      INTEGER IMCH2              ! Match string end index
      INTEGER INW                ! WILDN substring loop index
      INTEGER ISRCH              ! Character search index
      INTEGER ISRCS              ! Candidate string search index
      INTEGER ISRCW              ! Wild pattern search index
      INTEGER ISTR1              ! Candidate string search start index
      INTEGER ISTR2              ! Candidate string search end index
      INTEGER ISUBOF             ! Substring start offset
      INTEGER ISUB1              ! Substring start index
      INTEGER ISUB2              ! Substring end index
      INTEGER IWLD1              ! Wild-card pattern search start index
      INTEGER IWLD2              ! Wild-card pattern search end index
      INTEGER LASTN              ! Index of the right-hand WILDN character
      INTEGER LASTS              ! Index of the right-hand STRING character
      INTEGER LASTW              ! Index of the right-hand WILDS character
      INTEGER MLEN               ! Declared length of MATCH
      INTEGER NWILDA             ! Number of WILDA wild characters in WILDS
      INTEGER NWILDN             ! Number of WILDN wild characters in WILDS
      INTEGER SLEN               ! Declared length of STRING
      INTEGER WLEN               ! Declared length of WILDS

      CHARACTER * 1 CHRW         ! WILDS character value

*.

*  Find the declared lengths of all character string arguments.
      SLEN = LEN( STRING )
      WLEN = LEN( WILDS )
      MLEN = LEN( MATCH )

*  Initialise the returned value, CHR_WILD.
      CHR_WILD = .FALSE.

*  Initialise the match string.
      MATCH = ' '

*  Deal with the trivial cases first.
      IF ( SLEN .GT. MLEN ) THEN

*     The match pattern string is too short to contain the complete
*     match pattern (i.e. the length of the candidate string), so
*     return.
         CONTINUE
      ELSE IF ( ( SLEN .EQ. 0 ) .AND. ( WLEN .EQ. 0 ) ) THEN

*     Trivial match, two zero length strings.
         CHR_WILD = .TRUE.
      ELSE IF ( ( SLEN .GT. 0 ) .AND. ( WLEN .EQ. 0 ) ) THEN

*     The wild-card pattern has zero length and the candidate string
*     does not: no match is possible.
         CONTINUE
      ELSE

*     Find the number of wild-card characters in WILDS.
         NWILDA = 0
         NWILDN = 0
         FIRSTN = 0
         LASTN = 0
         ICHW = 1

*     DO WHILE loop.
 10      CONTINUE
         IF ( ICHW .LE. WLEN ) THEN

*        Check for escaped characters and increment the wild character 
*        when appropriate.
            IF ( WILDS( ICHW : ICHW ) .EQ. ESCAPE ) THEN
               ICHW = ICHW + 1
            ELSE IF ( WILDS( ICHW : ICHW ) .EQ. WILDA ) THEN
               NWILDA = NWILDA + 1
            ELSE IF ( WILDS( ICHW : ICHW ) .EQ. WILDN ) THEN
               NWILDN = NWILDN + 1
               IF ( NWILDN .EQ. 1 ) FIRSTN = ICHW
               LASTN = ICHW
            END IF

*        Increment the character pointer.
            ICHW = ICHW + 1
         GO TO 10
         END IF

*     If there are no multi-character wild characters in WILDS, then 
*     perform the comparison for single-character wild characters only.
         IF ( NWILDN .EQ. 0 ) THEN

*        There are no multi-character wild characters in WILDS, so the
*        string comparison can be performed character by character.
*        First check if any single-character wild characters are
*        present in the string: if there are none and the ABBREV flag
*        is set, then only check if STRING is an allowed abbreviation
*        of WILDS.
            IF ( ( NWILDA .EQ. 0 ) .AND. ABBREV ) THEN

*           There are no single-character wild characters in the
*           wild-card pattern and abbreviations are allowed, so perform
*           a character by character comparison. The candidate string
*           is allowed to be an abbreviation of the wild-card string.
*           First initialise the character pointers.
               ICHS = 1
               ICHW = 1

*           Loop to perform the comparison.
*           DO WHILE loop.
 20            CONTINUE
               IF ( ( ICHS .LE. SLEN ) .AND. ( ICHW .LE. WLEN ) ) THEN

*              Check for literal escape characters.
                  IF ( WILDS( ICHW : ICHW ) .EQ. ESCAPE ) THEN
                     IF ( ICHW .LT. WLEN ) ICHW = ICHW + 1
                  END IF

*              Compare the characters.
                  IF ( WILDS( ICHW : ICHW )
     :                 .NE. STRING( ICHS : ICHS ) ) GO TO 30

*              Increment the character pointers.
                  ICHW = ICHW + 1
                  ICHS = ICHS + 1
               GO TO 20
               END IF

*           Check for a successful match (there may be some remainder in
*           either of the two strings).
               IF ( ICHS-1 .EQ. SLEN ) CHR_WILD = .TRUE.
 30            CONTINUE
            ELSE

*           There are single-character wild characters in the wild-card 
*           pattern, so perform a character by character comparison. 
*           First initialise the character pointers.
               ICHS = 1
               ICHW = 1

*           Loop to perform the comparison.
*           DO WHILE loop.
 40            CONTINUE
               IF ( ( ICHS .LE. SLEN ) .AND. ( ICHW .LE. WLEN ) ) THEN

*              Check for single-character wild characters.
                  IF ( WILDS( ICHW : ICHW ) .EQ. WILDA ) THEN

*                 Update the match pattern, MATCH.
                     MATCH( ICHS : ICHS ) = WILDA
                  ELSE

*                 Check for literal escape characters.
                     IF ( WILDS( ICHW : ICHW ) .EQ. ESCAPE ) THEN
                        IF ( ICHW .LT. WLEN ) ICHW = ICHW + 1
                     END IF

*                 Compare the characters.
                     IF ( WILDS( ICHW : ICHW )
     :                    .NE. STRING( ICHS : ICHS ) ) GO TO 50
                  END IF

*              Increment the character pointers.
                  ICHW = ICHW + 1
                  ICHS = ICHS + 1
               GO TO 40
               END IF

*           Check for a successful match (there may be some remainder in
*           either of the two strings).
               IF ( ( ICHS-1 .EQ. SLEN )
     :              .AND. ( ICHW-1 .EQ. WLEN ) ) CHR_WILD = .TRUE.
 50            CONTINUE
            END IF
         ELSE

*        There are multi-character wild characters in WILDS, so the
*        string comparison must be made on the identifiable non-wild
*        characters in WILDS. Loop to search for and test each of the
*        non-wild substrings present in WILDS within the candidate
*        string.

*        First initialise the first character indices for the candidate
*        string and the wild-card patern.
            FIRSTS = 1
            FIRSTW = 1

*        Search the beginning of the candidate string for a match to
*        the first substring in the wild-card pattern (assuming the
*        wild-card pattern does not begin with a WILDN character).
            IF ( FIRSTN .GT. FIRSTW ) THEN

*           There is a substring before the first multi-character wild 
*           character, so perform the string comparison.  Initialise
*           the character loop indices.
               ICHW = FIRSTW
               ICHS = FIRSTS

*           DO WHILE loop.
 60            CONTINUE
               IF ( ICHW .LT. FIRSTN ) THEN

*              Check that the candidate string has not been over-run.
                  IF ( ICHS .GT. SLEN ) GO TO 999

*              Check for an escaped character.
                  IF ( WILDS( ICHW : ICHW ) .EQ. ESCAPE ) THEN

*                 Assign the test character and test the string
*                 character.
                     ICHW = ICHW + 1
                     CHRW = WILDS( ICHW : ICHW )
                     IF ( STRING( ICHS : ICHS ) .NE. CHRW ) GO TO 999
                  ELSE

*                 Assign the test character and test the string
*                 character.
                     CHRW = WILDS( ICHW : ICHW )

*                 Check for single-character wild characters.
                     IF ( CHRW .EQ. WILDA ) THEN

*                    Update the match pattern.
                        MATCH( ICHS : ICHS ) = WILDA
                     ELSE
                        IF ( STRING( ICHS : ICHS ) .NE. CHRW ) GO TO 999
                     END IF
                  END IF

*              Update the character pointers.
                  ICHW = ICHW + 1
                  ICHS = ICHS + 1
               GO TO 60
               END IF

*           Update the last character indices.
               FIRSTS = ICHS
               FIRSTW = ICHW + 1
            ELSE

*           The first character in the wild-card pattern is a WILDN
*           character, so move the wild-card pattern pointer to the
*           next character.
               FIRSTW = FIRSTN + 1
            END IF

*        Search in reverse through the candidate string for a match to
*        the last substring in the wild-card pattern.
            LASTS = SLEN
            LASTW = WLEN

            IF ( LASTN .LT. WLEN ) THEN

*           There is a substring after the last multi-character wild 
*           character, so perform the string comparison. 
*           Initialise the character loop indices.
               ICHW = LASTW
               ICHS = LASTS

*           DO WHILE loop.
 180           CONTINUE
               IF ( ICHW .GT. LASTN ) THEN

*              Check that the candidate string has not been over-run.
                  IF ( ICHS .LE. FIRSTS-1 ) GO TO 999

*              Check for an escaped character.
                  IF ( WILDS( ICHW-1 : ICHW-1 ) .EQ. ESCAPE ) THEN

*                 Assign the test character and test the string
*                 character.
                     CHRW = WILDS( ICHW : ICHW )
                     ICHW = ICHW - 1
                     IF ( STRING( ICHS : ICHS ) .NE. CHRW ) GO TO 999
                  ELSE

*                 Assign the test character and test the string
*                 character.
                     CHRW = WILDS( ICHW : ICHW )

*                 Check for single-character  wild characters.
                     IF ( CHRW .EQ. WILDA ) THEN

*                    Update the match pattern.
                        MATCH( ICHS : ICHS ) = WILDA
                     ELSE
                        IF ( STRING( ICHS : ICHS ) .NE. CHRW ) GO TO 999
                     END IF
                  END IF

*              Update the character pointers.
                  ICHW = ICHW - 1
                  ICHS = ICHS - 1
               GO TO 180
               END IF

*           Update the last character indices.
               LASTS = ICHS
               LASTW = ICHW - 1
            ELSE

*           The last character in the wild-card pattern is a WILDN
*           character, so move the wild-card pattern pointer to the
*           previous character.
               LASTW = LASTN - 1
            END IF

*        Check whether the wild-card pattern hhas been exhausted (for
*        NWILDN=1).
            IF ( FIRSTW .GT. LASTW ) THEN

*           The wild-card pattern has been exhausted with a resulting
*           successful match. Assign the returned value of CHR_WILD and
*           update the match pattern.
               CHR_WILD = .TRUE.

               DO 190 iCHM = FIRSTS, LASTS
                  MATCH( ICHM : ICHM ) = WILDN
 190           CONTINUE
            ELSE

*           Loop to search for and compare all wild-card pattern 
*           substrings within the candidate string.
*           First initialise the character loop indices.
               ISTR1 = FIRSTS
               ISTR2 = LASTS
               IWLD1 = FIRSTW
               IWLD2 = LASTW

*           Initialise the substring match flag.
               ISMACH = .FALSE.

*           Loop for all substrings in the wild-card pattern.
               DO 170 INW = 1, NWILDN-1

*              Loop to find the next multi-character wild character.
*              First initialise the loop index and the substring start
*              index.
                  ICHW = IWLD1
                  ISUB1 = IWLD1

*              DO WHILE loop.
 70               CONTINUE
                  IF ( ICHW .LE. IWLD2 ) THEN

*                 Check for literal escape characters.
                     IF ( WILDS( ICHW : ICHW ) .EQ. ESCAPE ) THEN
                        ICHW = ICHW + 1
                        IF ( ICHW .GT. IWLD2 ) GO TO 80
                     ELSE IF ( WILDS( ICHW : ICHW ) .EQ. WILDN ) THEN
                        GO TO 80
                     END IF

*                 Increment the character pointer.
                     ICHW = ICHW + 1
                  GO TO 70 
                  END IF

 80               CONTINUE

*              Update the wild-card pattern start index.
                  IWLD1 = ICHW + 1

*              Find substring indices.
                  ISUB2 = ICHW - 1

*              Check that the substring is not of zero length.
                  IF ( ISUB1 .LE. ISUB2 ) THEN

*                 There is a substring, so search the substring for the
*                 first non-WILDA character. 
                     ICHW = ISUB1

*                 DO WHILE loop.
 90                  CONTINUE
                     IF ( ICHW .LE. ISUB2 ) THEN

*                    Check for a single-character wild character.
                        IF ( WILDS( ICHW : ICHW ) .NE. WILDA ) GO TO 100

*                    Increment the loop index.
                        ICHW = ICHW + 1
                     GO TO 90
                     END IF

 100                 CONTINUE
 
*                 Determine the offset for the start of the search.
                     ISUBOF = ICHW - ISUB1

*                 Update the match pattern if necessary.
                     IF ( ISUBOF .GT. 0 ) THEN
                        DO 110 ICHM = ISTR1, ISTR1+ISUBOF-1
                           MATCH( ICHM : ICHM ) = WILDA
 110                    CONTINUE
                     END IF

*                 Check that the substring is not all single-character 
*                 wild characters.
                     IF ( ICHW .LE. ISUB2 ) THEN

*                    There is some substring to search for, so search
*                    the candidate string for the first occurrence of
*                    the substring. First apply the offset to the
*                    substring and candidate string start indices and
*                    the loop indices.
                        ISTR1 = ISTR1 + ISUBOF
                        ICHS = ISTR1
                        ISUB1 = ISUB1 + ISUBOF
                        ICHW = ISUB1

*                    Loop to search for the next incidence of the first
*                    non-wild substring character in the candidate
*                    string.  First assign the search character.
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

*                    Re-initialise the string match flag.
                        ISMACH = .FALSE.

*                   Loop until a string match is found.
*                    DO WHILE loop.
 120                    CONTINUE
                        IF ( .NOT. ISMACH ) THEN

*                       Perform the search.
                           ISRCH = INDEX( STRING( ISTR1 : ), CHRW )

*                       Abort if a character match is not found.
                           IF ( ISRCH .EQ. 0 ) GO TO 999

*                       Loop to perform the string comparison. First 
*                       initialise the character pointers.
                           ISRCS = ISTR1 + ISRCH - 1
                           ISRCW = ICHW

*                       Perform the comparison.
*                       DO WHILE loop.
 130                       CONTINUE
                           IF ( ( ISRCS .LE. LASTS )
     :                          .AND. ( ISRCW .LE. ISUB2 ) ) THEN

*                          Check for single-character wild characters.
                              IF ( WILDS( ISRCW : ISRCW )
     :                             .EQ. WILDA ) THEN

*                             Update the match pattern, MATCH.
                                 MATCH( ISRCS : ISRCS ) = WILDA
                              ELSE

*                             Check for literal escape characters.
                                 IF ( WILDS( ISRCW : ISRCW )
     :                                .EQ. ESCAPE ) THEN
                                    ISRCW = ISRCW + 1
                                    IF ( ISRCW .GT. ISUB2 ) GO TO 140
                                 END IF

*                             Compare the characters.
                                 IF ( WILDS( ISRCW : ISRCW )
     :                                .NE. STRING( ISRCS : ISRCS ) ) 
     :                                   GO TO 140
                              END IF

*                          Increment the character pointers.
                              ISRCW = ISRCW + 1
                              ISRCS = ISRCS + 1
                           GO TO 130
                           END IF

 140                       CONTINUE

*                       Check for a successful match (there may be some 
*                       remainder in either of the two strings).
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

*                       Update the match pattern if necessary.
                           IF ( ISRCH .GT. 1 ) THEN
                              DO 150 ICHM = IMCH1, IMCH2
                                 MATCH( ICHM : ICHM ) = WILDN
 150                          CONTINUE
                           END IF
                        GO TO 120
                        END IF

*                    Update the candidate string start index.
                        ISTR1 = ISRCS
                     ELSE

*                    The substring is all single-character wild
*                    characters, so just update the candidate string
*                    start index.
                        ISTR1 = ISTR1 + ISUBOF
                     END IF
                  ELSE

*                 For substrings of zero length, there is always a
*                 match.
                     ISMACH = .TRUE.

*                 Update the match pattern and candidate string indices
*                 if necessary (i.e. a trailing multi-character wild 
*                 character).
                     IF ( ISUB2 .EQ. LASTW ) THEN
                        DO 160 ICHM = ISTR1, LASTS
                           MATCH( ICHM : ICHM ) = WILDN
 160                    CONTINUE

                        ISTR1 = ISTR2 + 1
                     END IF
                  END IF
 170           CONTINUE

*           Assign the returned value of CHR_WILD.
               CHR_WILD = ISMACH

*           Check if the candidate string has been exhausted.
               IF ( ISTR1 .LE. ISTR2 ) THEN

*              The candidate string has not been exausted, so fill the
*              remainder of the match pattern with WILDN characters.
                  DO 200 ICHM = ISTR1, ISTR2
                     MATCH( ICHM : ICHM ) = WILDN
 200              CONTINUE
               END IF
            END IF
         END IF
      END IF

*  Annul the match pattern on failure.
 999  CONTINUE
      IF ( .NOT. CHR_WILD ) MATCH = ' '

      END
* $Id$
