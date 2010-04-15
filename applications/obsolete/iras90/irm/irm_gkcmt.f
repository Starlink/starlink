      SUBROUTINE IRM_GKCMT( NCARD, BUFFER, STCARD, NAME, CMTBGN,
     :                      CMTCRD, THERE, COMNT, CARDNO, STATUS )
*+
*  Name:
*     IRM_GKCMT

*  Purpose:
*     Get the comment string of a specified FITS keyword.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_GKCMT( NCARD, BUFFER, STCARD, NAME, CMTBGN, CMTCRD,
*                     THERE, COMNT, CARDNO, STATUS )

*  Description:
*     This routine searches a buffer containing the header card images
*     from a FITS file for the next card with the specified keyword, and
*     returns the comment string of the found keyword. The search
*     begins at a defined card image; and ends when the next end of a
*     header block, marked by the END keyword, is encountered or the
*     buffer is exhausted.
*
*     The comment string of a keyword begins with a comment indicator
*     character, specified by the argument CMTBGN, in the same card as
*     the keyword and may extend several cards with the continuation
*     cards having the keyword COMMENT or being blank at its keyword
*     position and preceded by a comment indicator. The argument
*     CMTCRD specifies maximum number of continuation comment cards
*     whose contents will be included in the returned string.
*
*     If the keyword is present, %THERE is true, otherwise it is false.
*     If there is no comment string for this keyword the returned
*     string will be blank.

*  Arguments:
*     NCARD = INTEGER (Given)
*        The number of card images in the buffer.
*     BUFFER( NCARD ) = CHARACTER * ( * ) (Given)
*        The buffer containing the header card images.
*     STCARD = INTEGER (Given)
*        The number of the card from which to start the search.
*     NAME = CHARACTER * ( * ) (Given)
*        The name of the keyword whose comment is required. This may be
*        a compound name to handle hierarchical keywords, and it has the
*        form keyword1.keyword2.keyword3 etc. The maximum number of
*        keywords per FITS card is 20. Comparisons are performed in
*        uppercase and blanks are removed. Each keyword must be no
*        longer than 8 characters.
*     CMTBGN = CHARACTER * ( 1 ) (Given)
*        The character which indicates the beginning of the comment
*        string of a keyword. Normally it is '/'.
*     CMTCRD = INTEGER (Given)
*        The maximum number of continuation comment cards whose
*        contents will be included in the returned string. If it is 0,
*        only the comment string in the same card as keyword will be
*        returned.  If the actual number of continuation comment cards
*        is less than that specified by CMTCRD, the contents of actual
*        number of cards will be included in the returned string.
*     THERE = LOGICAL (Returned)
*        If true the specified keyword is present.
*     COMNT = CHARACTER * ( * ) (Returned)
*        The comment string of the specified keyword. It should have
*        length not less than 72 + 72*CMTCRD characters.
*     CARDNO = INTEGER ( Returned)
*        The number of the last continuation comment card whose contents
*        are included in the returned string.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-AUG-1991 (WG):
*        Original version.
*     4-DEC-1992 (DSB):
*        Name changed from FTS1_GKCMT to IRM_GKCMT.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NCARD
      CHARACTER BUFFER( NCARD )*80
      INTEGER STCARD
      CHARACTER NAME*(*)
      CHARACTER CMTBGN*1
      INTEGER CMTCRD

*  Arguments Returned:
      LOGICAL THERE
      CHARACTER COMNT*(*)
      INTEGER CARDNO

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! The used length of a string

*  Local Constants:
      INTEGER MXWORD             ! Maximum number of hierarchical levels
                                 ! in a keyword
      PARAMETER ( MXWORD = 20 )

*  Local Variables:
      INTEGER CARD               ! Number of the card to be examined
      INTEGER CDKYLN             ! Length of the keyword of a card
      CHARACTER CMPKEY*80        ! Compound name
      INTEGER CMPLN              ! Length of compound name
      INTEGER CMTLN              ! Length of comment string
      LOGICAL CNTCMT             ! Comment continuation flag
      INTEGER CNTPSN             ! Position of continuation comment
      LOGICAL COMPND             ! Compound-keyword flag
      CHARACTER CONTIN*72        ! Continuation comment
      CHARACTER CRDKEY*8         ! Keyword of a card image
      INTEGER ENDW( MXWORD )     ! End column of each keyword
      INTEGER EQUALS             ! The position of equal sign
      INTEGER EXTCMT             ! The number of extracted continuation
                                 ! card images
      INTEGER FQTPSN             ! Position of the first quote mark
      INTEGER I                  ! Do loop indicator
      INTEGER INDPSN             ! Position of the comment indicator
      CHARACTER KEYWRD*80        ! Specified keyword
      INTEGER KEYLN              ! Length of specified keyword
      INTEGER LQTPSN             ! Position of the last quote mark
      INTEGER LSTAT              ! Local status
      INTEGER NWORD              ! Number of keywords in a card image
      INTEGER STARTW( MXWORD )   ! Start column of each keyword
      CHARACTER TMPCMT*72        ! Temporary string holding comment
      CHARACTER WORDS( MXWORD )*8! Keywords in a card image
      INTEGER WORDLN             ! Length of each word

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Remove blanks from the keyword to be searched for, change it to
*  uppercase and get its used length.
      KEYWRD = NAME
      CALL CHR_RMBLK( KEYWRD )
      CALL CHR_UCASE( KEYWRD )
      KEYLN = CHR_LEN( KEYWRD )

*  Initialise the card number to be examined, and the found flag.
      CARD = MAX( 1, STCARD )
      THERE = .FALSE.

*  To see whether it is a compound name.
      COMPND = INDEX( KEYWRD, '.' ) .NE. 0

*  If the keyword to be searched for is not compound, ...
      IF ( .NOT. COMPND ) THEN

*  Compare the keyword on each card with the given keyword, untill the
*  required card is found, or 'END' card is met, or no card left.
         DO WHILE ( ( .NOT. THERE ) .AND. ( CARD .LE. NCARD ) .AND.
     :              ( BUFFER( MIN( NCARD, CARD ) )( :3 ) .NE. 'END' ) )

*  Get the keyword of this card image, and its used length.
            CRDKEY = BUFFER( CARD )( :8 )
            CDKYLN = CHR_LEN( CRDKEY )

*  If the current card is the required keyword, ...
            IF ( CRDKEY( :CDKYLN ) .EQ. KEYWRD( :KEYLN ) ) THEN

*  Set the found flag and note down the card number.
               THERE = .TRUE.
               CARDNO = CARD

*  Otherwise go to the next card in the buffer.
            ELSE
               CARD = CARD + 1
            END IF
         END DO

*  If the keyword to be searched for is compound, ...
      ELSE

*  Get the keywords on the card and compare with given one, untill the
*  required card is found, or 'END' card is met, or no card left.
         DO WHILE ( ( .NOT. THERE ) .AND. ( CARD .LE. NCARD ) .AND.
     :              ( BUFFER( MIN( NCARD, CARD ) )( :3 ) .NE. 'END' ) )

*  Keywords are located between the first character and equal sign. So
*  find the position of the equal sign first.
            EQUALS = INDEX( BUFFER( CARD ), '=' )

*  And then, if equal sign, exists, extract the words from the FITS card
*  image.
            IF ( EQUALS .NE. 0 ) THEN
               CALL CHR_DCWRD( BUFFER( CARD )( :EQUALS - 1 ), MXWORD,
     :                         NWORD, STARTW, ENDW, WORDS, LSTAT )

*  Form the compound keyword by concatenateing the words via the
*  delimiter.
               CMPLN = 0
               CMPKEY = ' '
               DO I = 1, NWORD
                  WORDLN = ENDW( I ) - STARTW( I ) + 1
                  CALL CHR_PUTC( WORDS( I )( :WORDLN ), CMPKEY, CMPLN )
                  IF( I .NE. NWORD ) CALL CHR_PUTC( '.', CMPKEY, CMPLN )
               END DO

*  Compare the (compound) keyword of the current card image with that of
*  the compound keyword to be searched. If it is present, set the found
*  flag and note down the card number.
               IF ( CMPKEY( :CMPLN ) .EQ. KEYWRD( :KEYLN ) ) THEN
                  THERE = .TRUE.
                  CARDNO = CARD

*  Otherwise, go to the next card in the buffer.
               ELSE
                  CARD = CARD + 1
               END IF

*  If this card does not contain the equal sign, go to the next card.
            ELSE
               CARD = CARD + 1
            END IF
         END DO
      END IF

*  If the required keyword is found, get its comment string.
      IF ( THERE ) THEN

*  The comment string is follow the first comment indicator not included
*  in the quote mark. So find the first quote mark and the indicator
*  after the keyword.
         FQTPSN = INDEX( BUFFER( CARDNO )( KEYLN + 1: ), '''' )
         INDPSN = INDEX( BUFFER( CARDNO )( KEYLN + 1: ), CMTBGN )

*  Get the absolute position of first quote mark and the indicator.
         IF ( FQTPSN .NE. 0 ) FQTPSN = FQTPSN + KEYLN
         IF ( INDPSN .NE. 0 ) INDPSN = INDPSN + KEYLN

*  If the first quote mark exists and is before the indicator, find
*  the trailing quote mark.
         IF ( (FQTPSN .NE. 0 ) .AND. ( FQTPSN .LT. INDPSN ) ) THEN
            LQTPSN = INDEX( BUFFER( CARDNO )( FQTPSN + 1: ), '''' )
            IF ( LQTPSN .NE. 0 ) LQTPSN = LQTPSN + FQTPSN

*  If trailing quote mark is after the indicator, this indicator is not
*  comment indicator. Find a indicator after the trailing quote mark.
            IF ( INDPSN .LT. LQTPSN ) THEN
               INDPSN = INDEX( BUFFER( CARDNO )( LQTPSN + 1: ), CMTBGN )
               IF ( INDPSN .NE. 0 ) INDPSN = INDPSN + LQTPSN
            END IF
         END IF

*  If the indicator position is not 0, there is a comment string for
*  this keyword. Extract the string follow it.
         IF ( INDPSN .NE. 0 ) THEN
            COMNT = BUFFER( CARDNO )( INDPSN + 1: )

*  Extract the continuation of the comment if there is any and the
*  number of the extracted continuation cards less than the specified
*  number of continuation cards.
            EXTCMT = 0

*  There will be continuation if the next card has keyword COMMENT.
            IF ( ( EXTCMT .LT. CMTCRD ) .AND.
     :           ( BUFFER( CARDNO + 1 )( :7 ) .EQ. 'COMMENT' ) ) THEN
               CNTCMT = .TRUE.
               CNTPSN = 9
               CARDNO = CARDNO + 1

*  Or if the next card is blank at it keyword position.
            ELSE IF ( ( EXTCMT .LT. CMTCRD ) .AND.
     :                ( BUFFER( CARDNO + 1 )( :8 ) .EQ. ' ' ) ) THEN

*  And there is a comment indicator in this line.
               CNTPSN = INDEX( BUFFER( CARDNO + 1 )( 9: ), CMTBGN )
               IF ( CNTPSN .NE. 0 ) THEN
                  CNTCMT = .TRUE.
                  CNTPSN = CNTPSN + 9
                  CARDNO = CARDNO + 1

*  Otherwise, this line is not a continuation.
               ELSE
                  CNTCMT = .FALSE.
               END IF

*  Otherwise, there will no continuation.
            ELSE
               CNTCMT = .FALSE.
            END IF

*  Extract the contents of the continuation cards until all continuation
*  cards has been extracted or the number of the extracted cards exceeds
*  the specified max, number of continuation cards to be extracted.
            DO WHILE ( CNTCMT )
               CONTIN = BUFFER( CARDNO )( CNTPSN: )

*  Remove the leading blank of the continuation comment and append it to
*  the comment line.
               CALL CHR_LDBLK( CONTIN )
               CMTLN = CHR_LEN( COMNT )
               TMPCMT = COMNT
               COMNT = TMPCMT( :CMTLN)//' '//CONTIN

*  Check if the next card is the continuation.
               EXTCMT = EXTCMT + 1

*  There will be continuation if the next card has keyword COMMENT.
               IF ( ( EXTCMT .LT. CMTCRD ) .AND.
     :              ( BUFFER( CARDNO + 1 )( :7 ) .EQ. 'COMMENT' ) ) THEN
                  CNTCMT = .TRUE.
                  CNTPSN = 9
                  CARDNO = CARDNO + 1

*  Or if the next card is blank at it keyword position.
               ELSE IF ( ( EXTCMT .LT. CMTCRD ) .AND.
     :                   ( BUFFER( CARDNO + 1 )( :8 ) .EQ. ' ' ) ) THEN

*  And there is a comment indicator in this line.
                  CNTPSN = INDEX( BUFFER( CARDNO + 1 )( 9: ),
     :                            CMTBGN )
                  IF ( CNTPSN .NE. 0 ) THEN
                     CNTCMT = .TRUE.
                     CNTPSN = CNTPSN + 9
                     CARDNO = CARDNO + 1

*  Otherwise this line is not a continuation.
                  ELSE
                     CNTCMT = .FALSE.
                  END IF

*  Otherwise, there will no continuation.
               ELSE
                  CNTCMT = .FALSE.
               END IF
            END DO

*  Otherwise no comments for this keyword, set returned comment string
*  blank.
         ELSE
            COMNT = ' '
         END IF

*  Otherwise, the specified keyword is not found, set returned comment
*  string blank,
      ELSE
         COMNT = ' '
      END IF

      END
