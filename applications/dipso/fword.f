      SUBROUTINE FWORD( STRING, POS, WORD, WLEN )
*+
* Name:
*     FWORD

*  Purpose:
*     Find a given word within a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FWORD( STRING, POS, WORD, WLEN )

*  Description:
*     The word within STRING with the position specified by POS is found
*     and returned in WORD (or as much of it as there is room for). Its
*     length is returned in WLEN.
*
*     A word is a group of contiguous non-blank (or tab) characters.
*     Blanks contained within single quotes are not treated as word
*     delimiters. Quotes, spaces and "\" characters can be escaped
*     using the "\" character if they need to be included literally within
*     a word (the escape characters are not included in the returned word).

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The string.
*     POS = INTEGER (Given)
*        The index of the required word. WLEN is returned equal to zero
*        if no word exists with the given index.
*     WORD = CHARACTER * ( * ) (Returned)
*        The required word extracted from STRING.
*     WLEN = INTEGER (Returned)
*        The index of the last character in the word.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-AUG-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) STRING
      INTEGER POS

*  Arguments Returned:
      CHARACTER * ( * ) WORD
      INTEGER WLEN

*  External References:
      INTEGER CHR_LEN            ! Index of last non-blank character

*  Local Variables:
      CHARACTER
     :     C*1,                  ! Current character
     :     ESC*1                 ! The escape character

      INTEGER
     :     I,                    ! Current character index
     :     NW,                   ! No. of words found so far
     :     WMAX                  ! Declared length of WORD

      LOGICAL
     :     BLANK,                ! An inter-word blank?
     :     LBLANK,               ! Was previous an inter-word blank?
     :     LESC,                 ! Was previous an escape character?
     :     QUOTED                ! Inside a quoted string?

*.

*  Initialise WLEN and WORD to indicate that the word has not been found.
      WLEN = 0
      WORD = ' '

*  If the string is blank, return with WLEN set to zero. Otherwise, find
*  the required word.
      IF( STRING .NE. ' ' ) THEN

*  Set up initial values needed in the following DO loop.
         LBLANK = .TRUE.
         LESC = .FALSE.
         NW = 0
         QUOTED = .FALSE.
         WMAX = LEN( WORD )
         ESC = '\\'    ! Some compilers treat \ as an escape character
                       ! and so need two ("\\") in the source code in
                       ! order to get one ("\") in the object code. ESC
                       ! has a declared length of 1 and so will only
                       ! store a single \ if the compiler doesn't treat
                       ! \ as an escape character.

*  Loop round each character in the string.
         DO I = 1, CHR_LEN( STRING )

*  Save the character.
            C = STRING( I : I )

*  If this is an unescaped escape character, flag that the next
*  character has been escaped and pass on.
            IF( ( C .EQ. ESC ) .AND. ( .NOT. LESC ) ) THEN
               LESC = .TRUE.

            ELSE

*  If this character is an unescaped single quote, toggle a flag which
*  says if we are in a quoted string or not, and pass on.
               IF( C .EQ. '''' .AND. ( .NOT. LESC ) ) THEN
                  QUOTED = .NOT. QUOTED

               ELSE

*  Set a flag to indicate if this character is an unescaped space or
*  non-printing (eg tab) character. Spaces, etc, are not treated as word
*  delimiters if they occur within quoted strings.
                  BLANK = ( ( C .LE. ' ' ) .AND. ( .NOT. LESC ) .AND.
     :                      ( .NOT. QUOTED ) )

*  If this is the first non-blank character after a sequence of blanks,
*  increment the word count.
                  IF( .NOT. BLANK .AND. LBLANK ) NW = NW + 1

*  If the word count has reached the required word, exit if the end of
*  the word has been reached, or if the returned string is full.
                  IF( NW .EQ. POS ) THEN
                     IF( BLANK .OR. WLEN .EQ. WMAX ) GO TO 999

*  Copy the current character to the returned string.
                     WLEN = WLEN + 1
                     WORD( WLEN : WLEN ) = C

                  END IF

                  LBLANK = BLANK

               END IF

               LESC = .FALSE.

            END IF

         END DO

      END IF

 999  CONTINUE

      END
