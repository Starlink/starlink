      SUBROUTINE IRM1_SPLIT( STRING, APNLIN, MAXLIN, LINENO, 
     :                       SUBSTR, STATUS )
*+
*  Name:
*     IRM1_SPLIT

*  Purpose:
*     Split a character string into several lines

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM1_SPLIT( STRING, APNLIN, MAXLIN, LINENO, SUBSTR, STATUS )

*  Description:
*     This routine splits a string into approximately specified number
*     of lines. Since the splitting must be at the end of a word, the
*     actual number of lines may more or less than the specified number.
*     A line is formed by appending words one after another. A word will
*     be apended to the line if the line length after appending the word
*     is less than the current max. line length, or if the line length
*     after appending is greater than current max. line length but its
*     exceeding over the assumed length by the amount less than the half
*     of the word length, in this case the current max. line length is
*     updated. If none of above conditions is satisfied, the line is
*     ended and a new line will begin untill all the words in the string
*     have been used. 


*  Arguments:
*     STRING = CHARACTE*(*) (Given)
*        The input string to be split.
*     APNLIN = INTEGER (Given)
*        The assumed number of lines into which the string will be
*        split. 
*     MAXLIN = INTEGER (Given)
*        The max. number of lines the string can be split into.
*     LINENO = INTEGER (Returned)
*        The actual number of lines into which the string is split.
*     SUBSTR( MAXLIN ) = CHARACTER*(*) (Returned)
*        Each line of input string after split.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     4-JAN-1991 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER*( * ) STRING
      INTEGER APNLIN
      INTEGER MAXLIN

*  Arguments Returned:
      INTEGER LINENO
      CHARACTER*( * ) SUBSTR( MAXLIN )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! The used length of a string

*  Local Constants:
      INTEGER MAXWRD             ! Max limit of the number of words in string
      PARAMETER ( MAXWRD = 256 )
      INTEGER MWRDLN             ! Max. limit of the length of a word 
      PARAMETER ( MWRDLN = 20 )
      
*  Local Variables:
      INTEGER APLN               ! Approximate length of a line.
      INTEGER BGNPSN( MAXWRD )   ! The begin positon of each word
      INTEGER ENDPSN( MAXWRD )   ! The end positon of each word
      INTEGER I                  ! Do loop index
      INTEGER LSTAT              ! Local status of routine CHR_DCWRD
      INTEGER MLINLN             ! Current max. line length
      INTEGER MXWDLN             ! Max. word length in the string
      INTEGER NEWLN              ! New length of a line after appending
      INTEGER NWORD              ! Number of words in the input string
      INTEGER STRGLN             ! The used length of input string
      INTEGER SUBLN              ! Current line length
      INTEGER USDWRD             ! Number of used words
      INTEGER WDLN( MAXWRD )     ! Length of each word in the string

      LOGICAL MORWRD             ! More words flag, if true, more words
                                 ! will be appended to a line.
                               
      CHARACTER*( MWRDLN ) WORD( MAXWRD )
                                 ! Each word in the input string

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Remove the leading blank of the input string, and get its used length
      CALL CHR_LDBLK( STRING )
      STRGLN = CHR_LEN( STRING )

*  If the specified number of line less than 2, assign the string to 
*  the first line and exit.
      IF ( APNLIN .LT. 2 ) THEN
         SUBSTR( 1 ) = STRING
         LINENO = 1
         GOTO 999
      END IF
      
*  If the input string is blank, set status as error, report and exit.
      IF ( STRGLN .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPLIT_ERR1',
     :                 'Attempt to split a blank string', STATUS )
         GOTO 999
      END IF

*  Get all the words in the input string.
      CALL CHR_DCWRD( STRING, MAXWRD, NWORD, BGNPSN, ENDPSN, WORD,
     :                LSTAT )

*  If number of words in the string exceeds max number, set status as
*  error, report error and exit.
      IF ( LSTAT .EQ. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPLIT_ERR2',
     :                 'Too many words in the sting.', STATUS )
         GOTO 999
      END IF

*  Find the max. word length in the string.
      MXWDLN = 0
      DO I = 1, NWORD
         WDLN( I ) = ENDPSN( I ) - BGNPSN( I ) + 1
         IF ( WDLN( I ) .GT. MXWDLN ) MXWDLN = WDLN( I )
      END DO
            
*  The assumed length of each line should at least equal the half of
*  max word length.
      APLN = MAX( INT( STRGLN / APNLIN ), MXWDLN / 2 + 1 )
      
*  Initialis current max. line length.
      MLINLN = APLN
      
*  Initialise the line number and the used word counter.
      LINENO = 0
      USDWRD = 0

*  Form lines untill all words have been used.
      DO WHILE ( USDWRD .LT. NWORD )
      
*  Use one more word.
         USDWRD = USDWRD + 1

*  Begin a new line.
         LINENO = LINENO + 1

*  If the number of line exceeds the max number, set status to error,
*  report the error and exit.
         IF( LINENO .GT. MAXLIN ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPLIT_ERR3',
     :                    'The number of lines exceed the max number.',
     :                     STATUS )
            GOTO 999
         END IF

*  Initialise the new line and get the length of the line.      
         SUBSTR( LINENO ) = WORD( USDWRD )
         SUBLN = WDLN( USDWRD )

*  If the line length exceeds the max. length or if all words have been
*  used, no more words will be appended to this line.
         IF ( SUBLN .GE. MLINLN .OR. USDWRD .EQ. NWORD ) THEN
            MORWRD = .FALSE.

*  Otherwise, more words will be appended to this line.
         ELSE
            MORWRD = .TRUE.
         END IF

*  Append words to the line untill no more words for this line.
         DO WHILE( MORWRD )

*  Calculate the new line length after appending a new word to the line.
            NEWLN = SUBLN + WDLN( USDWRD + 1 ) + 1

*  If the new length less than the current max. length, append the word.
            IF ( NEWLN .LE. MLINLN ) THEN
               USDWRD = USDWRD + 1
               SUBSTR( LINENO ) = SUBSTR( LINENO )( : SUBLN ) // ' '
     :                            // WORD( USDWRD )( : WDLN( USDWRD ) )
               SUBLN = NEWLN

*  Otherwise, see how long it exceeds the assumed length.
            ELSE

*  If it exceeds the assumed length by the amount less than the half of
*  the word length, append the word to the line,
               IF ( NEWLN - APLN .LE. WDLN( USDWRD + 1 ) / 2 ) THEN
                  USDWRD = USDWRD + 1
                  SUBSTR( LINENO ) = SUBSTR( LINENO )( : SUBLN ) // ' '
     :                            // WORD( USDWRD )( : WDLN( USDWRD ) )

*  and update the current max. line length and end the line.
                  MLINLN = NEWLN
                  MORWRD = .FALSE.
      
*  Otherwise, do not append the word, end the line.
               ELSE
                  MORWRD = .FALSE.
               END IF
            END IF

*  If all words have been used, end the line.
            IF ( USDWRD .EQ. NWORD ) MORWRD = .FALSE.

*  Go back to append more words to the line if it haven't been ended.
         END DO

*  Otherwise, go back to begin a new line if still some words remain.
      END DO

*  Otherwise, the splitting has been finished, end the routine.
 999  CONTINUE
      END
