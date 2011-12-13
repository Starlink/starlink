      SUBROUTINE TRN_STOK( TOKEN, VALUE, TEXT, NSUBS, STATUS )
*+
*  Name:
*     TRN_STOK

*  Purpose:
*     Substitute token.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN_STOK( TOKEN, VALUE, TEXT, NSUBS, STATUS )

*  Description:
*     The routine searches the TEXT string provided and replaces every
*     occurrence of the TOKEN sub-string with the contents of the VALUE
*     string.  Leading and trailing blanks in the VALUE string are
*     disregarded.  The substitution is not recursive.  Token
*     sub-strings in TEXT must be delimited by non-alphanumeric
*     characters.  If they are delimited by angle brackets thus:
*     <TOKEN>, then the brackets are regarded as part of the token and
*     are also replaced.
*
*     Token names must begin with an alphabetic character and continue
*     with alphanumeric characters (including '_') only.  They may be
*     of any length.  The routine returns the substituted text and a
*     count of the number of token values substituted (NSUBS).  This
*     routine is not sensitive to case.

*  Arguments:
*     TOKEN = CHARACTER * ( * ) (Given)
*        The token string.
*     VALUE = CHARACTER * ( * ) (Given)
*        The value string to be substituted.
*     TEXT = CHARACTER * ( * ) (Given & Returned)
*        The text to be processed.
*     NSUBS = INTEGER (Returned)
*        The number of token substitutions made.
*     STATUS = INTEGER (Given & Returned)
*        Inherited error status.

*  Algorithm:
*     - Initialise, then validate the TOKEN string.
*     - Find the lengths of the TOKEN and TEXT strings and the limits of
*       the VALUE string.
*     - Obtain upper- and lower-case versions of the initial TOKEN
*       character.
*     - Search through the TEXT string, finding possible occurrences of
*       the TOKEN string by looking for the first token character
*       (regardless of case).
*     - Check if an occurrence has been found by matching the
*       appropriate TEXT characters with the TOKEN string and checking
*       that the previous and following characters (if they exist) are
*       non-alphanumeric.
*     - Note if the previous and following characters are angle
*       brackets.
*     - If the token occurrence is delimited by angle brackets, increase
*       the region being considered to include them.
*     - Decide if the text which will replace the token is larger or
*       smaller in extent than the token occurrence and move the
*       following text to the right or left as appropriate.
*     - Substitute the VALUE string for the token occurrence.
*     - Update the TEXT string length and report an error if it is too
*       long (indicating that text has been truncated at the right).
*     - Update the start position for finding the next token occurrence
*       and continue the search for tokens until the TEXT string is
*       exhausted.

*  Copyright:
*     Copyright (C) 1988, 1992 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     18-AUG-1988 (RFWS):
*        Original version.
*     13-FEB-1992 (RFWS):
*        Improved error reporting and eliminated non-standard character
*        string concatenation.
*     21-FEB-1992 (RFWS):
*        Use CHR_ISALM to make character tests.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'TRN_ERR'          ! TRN_ error codes


*  Arguments Given:
      CHARACTER * ( * ) TOKEN   ! Token string

      CHARACTER * ( * ) VALUE   ! Value string to be substituted


*  Arguments Given and Returned:
      CHARACTER * ( * ) TEXT    ! Text to be processed


*  Arguments Returned:
      INTEGER NSUBS             ! Number of token substitutions made


*  Status:
      INTEGER STATUS            ! Error status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string
      LOGICAL CHR_ISALM          ! Character alphanumeric/underscore?
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*  Local Variables:
      LOGICAL FOUND              ! Whether the token character sequence
                                 ! has been found
      LOGICAL LPAR               ! Token delimited by '<' ?
      LOGICAL RPAR               ! Token delimited by '>' ?
      INTEGER LTOK               ! Non-blank length of TOKEN
      INTEGER LTXT               ! Non-blank length of TEXT
      INTEGER LVAL               ! Number of characters to be
                                 ! substituted from VALUE
      INTEGER VAL1               ! Position of the first non-blank VALUE
                                 ! character
      INTEGER VAL2               ! Position of the last non-blank VALUE
                                 ! character
      INTEGER ISTART             ! First TEXT character position to
                                 ! search
      INTEGER IFIRST             ! Position of first token character in
                                 ! TEXT
      INTEGER ILAST              ! Position of last token character in
                                 ! TEXT
      INTEGER ISHIFT             ! Number of character positions to
                                 ! shift text which follows a token
                                 ! substitution
      INTEGER I                  ! Loop counter for shifting characters
      CHARACTER * 1 CHU          ! Upper case initial token character
      CHARACTER * 1 CHL          ! Lower case initial token character
      INTEGER IFRSTU             ! Position of uppercase character
      INTEGER IFRSTL             ! Position of lowercase character

*.

*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN

*   Initialise the count of token replacements.
      NSUBS = 0

*   Validate the token string.
      CALL TRN1_VTOK( TOKEN, STATUS )


*   If there is no error, find the token and text lengths.
      IF( STATUS .EQ. SAI__OK ) THEN
        LTOK = CHR_LEN( TOKEN )
        LTXT = CHR_LEN( TEXT )


*   Find positions of the the first and last characters of the
*   replacement value and the number of replacement characters.
        IF( VALUE .EQ. ' ' ) THEN
          LVAL = 0
        ELSE
          VAL1 = 1
          DO WHILE ( VALUE ( VAL1 : VAL1 ) .EQ. ' ' )
            VAL1 = VAL1 + 1
          ENDDO
          VAL2 = CHR_LEN( VALUE )
          LVAL = VAL2 - VAL1 + 1
        ENDIF


*   Obtain upper- and lower-case versions of the initial token
*   character.
        CHU = TOKEN( 1 : 1 )
        CALL CHR_UCASE( CHU )
        CHL = TOKEN( 1 : 1 )
        CALL CHR_LCASE( CHL )


*   Loop to make substitutions until no more text remains to be
*   processed, or an error is encountered.
        ISTART = 1
        DO WHILE ( ( ( ISTART + LTOK - 1 ) .LE. LTXT ) .AND.
     :             ( STATUS .EQ. SAI__OK ) )


*   Initialise flags used in token identification.
          FOUND = .FALSE.
          LPAR = .FALSE.
          RPAR = .FALSE.


*   Find the next occurrence of the initial token character in the text,
*   regardless of case.
          IFRSTU = INDEX( TEXT( ISTART : LTXT ), CHU )
          IF ( IFRSTU .EQ. 0 ) IFRSTU = LTXT - ISTART + 2
          IFRSTL = INDEX( TEXT( ISTART : LTXT ), CHL )
          IF ( IFRSTL .EQ. 0 ) IFRSTL = LTXT - ISTART + 2
          IFIRST = ISTART - 1 + MIN( IFRSTU, IFRSTL )


*   Assume this is the start of a token sub-string and find its final
*   character position.  Check this lies within the text.
          ILAST = IFIRST + LTOK - 1
          IF( ILAST .LE. LTXT ) THEN


*   Compare the token with the identified section of text, without
*   regard for case.
            FOUND = CHR_SIMLR( TOKEN( : LTOK ), TEXT( IFIRST : ILAST ) )


*   If the token character sequence has been found, check the previous
*   character (if it exists) to ensure it is not alphanumeric.  Also
*   check if it is a delimiting '<' character.
            IF( FOUND ) THEN
              IF( IFIRST .GT. 1 ) THEN
                FOUND =
     :            ( .NOT. CHR_ISALM( TEXT( IFIRST - 1 : IFIRST - 1 ) ) )
                LPAR = ( TEXT( IFIRST - 1 : IFIRST - 1 ) .EQ. '<' )
              ENDIF


*   Make the same checks on the following character.
              IF( ILAST .LT. LTXT ) THEN
                FOUND = FOUND .AND.
     :            ( .NOT. CHR_ISALM( TEXT( ILAST + 1 : ILAST + 1 ) ) )
                RPAR = ( TEXT( ILAST + 1 : ILAST + 1 ) .EQ. '>' )
              ENDIF


*   End of "the token character sequence has been found" condition.
            ENDIF


*   End of "the final token character lies within the text" condition.
          ENDIF


*   If the initial token character was not part of a valid token
*   sub-string, ignore it and move the start position for the next
*   search to the following character.
          IF( .NOT. FOUND ) THEN
            ISTART = IFIRST + 1


*   If a valid token was found, adjust the first and last character
*   positions to account for angle brackets and calculate the amount by
*   which the text following the token will need to be shifted to
*   accommodate the replacement value.
          ELSE
            IF( LPAR .AND. RPAR ) THEN
              IFIRST = IFIRST - 1
              ILAST = ILAST + 1
            ENDIF
            ISHIFT = LVAL - ( ILAST - IFIRST + 1 )


*   If the following text shifts to the right, move the characters,
*   subject to the total length of the TEXT variable.
            IF( ISHIFT .GT. 0 ) THEN
              DO I = MIN( LTXT + ISHIFT, LEN( TEXT ) ),
     :               ILAST + 1 + ISHIFT, -1
                TEXT( I : I ) = TEXT( I - ISHIFT : I - ISHIFT )
              ENDDO


*   If the following text shifts to the left, move the characters, then
*   blank out any remaining text.
            ELSE IF( ISHIFT .LT. 0 ) THEN
              DO I = ILAST + 1 + ISHIFT, LTXT + ISHIFT
                TEXT( I : I ) = TEXT( I - ISHIFT : I - ISHIFT )
              ENDDO
              TEXT( LTXT + ISHIFT + 1 : LTXT ) = ' '
            ENDIF


*   Insert the replacement value in the text.
            IF( LVAL .GT. 0 )
     :        TEXT( IFIRST : MIN( IFIRST + LVAL - 1, LEN( TEXT ) ) ) =
     :          VALUE( VAL1 : )


*   Update the text length and check it does not exceed the length of
*   the TEXT variable.  If it does, put a '!' character in the last
*   TEXT position and report an error.
            LTXT = LTXT + ISHIFT
            IF( LTXT .GT. LEN( TEXT ) ) THEN
              TEXT( LEN( TEXT ) : ) = '!'
              STATUS = TRN__TRUNC
              CALL MSG_SETC( 'TEXT', TEXT )
              CALL ERR_REP( 'TRN_STOK_TRUNC',
     :                      'Text truncated: ''^TEXT'' (possible ' //
     :                      'programming error).', STATUS )


*   If the text length is still valid, move the start position for the
*   next search to the character following the replaced token value and
*   increment the count of token repacements.
            ELSE
              ISTART = ILAST + 1 + ISHIFT
              NSUBS = NSUBS + 1
            ENDIF


*   End of "a valid token was found" condition.
          ENDIF


*   End of "loop to make substitutions..." loop.
        ENDDO


*   End of "no error validating token" condition.
      ENDIF


*   Exit routine.
      END
