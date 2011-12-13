      SUBROUTINE FTS1_UKEYC( NCARD, BUFFER, STCARD, NAME, VALUE, CMTBGN,
     :                       COMNT, COMCAR, THERE, CARD, STATUS )
*+
*  Name:
*     FTS1_UKEYC

*  Purpose:
*     Writes the value of type CHARACTER to a keyword from a buffer
*     of FITS-header card images.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_UKEYC( NCARD, BUFFER, STCARD, NAME, VALUE, CMTBGN,
*                      COMNT, COMCAR, THERE, CARD, STATUS )

*  Description:
*     This routine searches a buffer containing the header card images
*     from a FITS file for card with keyword %NAME; and, if found,
*     override its original value or with the given character value
*     %VALUE.  The keyword can have a comment string leading by the
*     character specified by %CMTBGN.  In addition comment-type
*     keywords---those with keywords COMMENT, HISTORY, and blank,
*     or argument COMCAR is .TRUE.---may also have their comments
*     revised by %COMNT (whereupon %VALUE is ignored).  Selection of
*     the desired card (especially important for commentary cards) can
*     be controlled by the starting the search at card numbered
*     %STCARD.  The search ends when the next end of a header block,
*     marked by the END keyword, is encountered or the buffer is
*     exhausted.  If the keyword is present %THERE is true, otherwise
*     it is false.  If the parameter is present more than once in the
*     header, only the first occurence will be used.
*
*     The name may be compound to permit writing of hierarchical
*     keywords.
*
*     The buffer of FITS header card image should be mapped in the
*     'UPDATE' mode.
*
*  Arguments:
*     NCARD = INTEGER (Given)
*        The number of card images in the buffer.
*     BUFFER( NCARD ) = CHARACTER * ( * ) (Given and Returned)
*        The buffer containing the header card images.
*     STCARD = INTEGER (Given)
*        The number of the card from which to start search.
*     NAME = CHARACTER * ( * ) (Given)
*        The name of the keyword to be written a new value.  This may be
*        a compound name to handle hierarchical keywords, and it has the
*        form keyword1.keyword2.keyword3 etc.  The maximum number of
*        keywords per FITS card is 20.  Comparisons are performed in
*        uppercase and blanks are removed.  Each keyword must be no
*        longer than 8 characters.  The total length must not exceed
*        48 characters.  This is to allow for the value, and indentation
*        into a blank-keyword card (as hierarchical keywords are not
*        standard and so cannot be part of the standard keyword name).
*     VALUE = CHARACTER * ( * ) (Given)
*        The value to be used to override the original value of the
*        keyword.  It is ignored when the keyword is COMMENT, HISTORY,
*        or blank.
*     CMTBGN = CHARACTER * ( 1 ) (Given)
*        The character which indicates the beginning of the comment
*        string of to be appended to the keyword.  Normally it is '/'.
*        when it is blank, no comment will be appended to the keyword.
*        It is ignored when the keyword is COMMENT, HISTORY, or blank.
*     COMNT = CHARACTER * ( * ) (Given)
*        The comment string of the keyword.  It may be truncated at the
*        end to put into the space left after writing keyword value for
*        non-commentary keywords.
*     COMCAR = LOGICAL (Given)
*        If .TRUE., the supplied card is a comment and thus the value
*        and comment delimiter are ignored, and just the keyword and
*        comment string are used to generate the header card.
*     THERE = LOGICAL (Returned)
*        If true, the specified keyword is present.
*     CARD = INTEGER (Returned)
*        The number of the last continuation comment card whose contents
*        are included in the returned string.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The comments are written from column 32 or higher if the value
*     demands more than the customary 20 characters for the value.  A
*     comment may be omitted if the value is so long to leave no room.
*     -  The BUFFER appears out of standard order because when this
*     routine is called, BUFFER is expected to be a mapped array.
*     This order prevents having to append several %VAL(length)
*     arguments instead of just the one.

*  Copyright:
*     Copyright (C) 1991, 1994 Science & Engineering Research Council.
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 September 9 (WG):
*        Original version.
*     1994 July 16 (MJC):
*       Renamed and tidied.  Fixed violations of the Fortran standard
*       for appending to character-strings.  Fixed bugs that made
*       comments and values uppercase, filtered the comments, and
*       positioned the comment string in the wrong location.  Padded
*       short strings to 8 characters.  Used parameters to define
*       lengths of the parts of a FITS header card.  Allowed commentary
*       keywords to be written.  Padded short strings to 8 characters.
*       Does not attempt to write a blank comment.
*     1996 November 13 (MJC):
*        Renamed from FTS1_WKEYC to FTS1_UKEYC, and calls new
*        FTS1_WKEYC.  Added the COMCAR argument.
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
      INTEGER STCARD
      CHARACTER * ( * ) NAME
      CHARACTER * ( * ) VALUE
      CHARACTER * ( 1 ) CMTBGN
      CHARACTER * ( * ) COMNT
      LOGICAL COMCAR

*  Arguments Given and Returned:
      CHARACTER * ( 80 ) BUFFER( NCARD )

*  Arguments Returned:
      LOGICAL THERE
      INTEGER CARD

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! The used length of a string

*  Local Constants:
      INTEGER COMCOL             ! Minimum column position for the start
                                 ! of a FITS comment
      PARAMETER ( COMCOL = 32 )

      INTEGER CVALLN             ! Minimum number of characters in a
                                 ! FITS header card value
      PARAMETER ( CVALLN = 8 )

      INTEGER HKEYLN             ! Maximum number of characters in a
                                 ! FITS header card hierarchical keyword
      PARAMETER ( HKEYLN = 48 )

      INTEGER KEYLN              ! Maximum number of characters in a
                                 ! FITS header card keyword or
                                 ! hierarchical component thereof
      PARAMETER ( KEYLN = 8 )

      INTEGER MXWORD             ! Maximum number of hierarchical levels
                                 ! in a keyword
      PARAMETER ( MXWORD = 20 )

      INTEGER VALLN              ! Maximum number of characters in a
                                 ! FITS header card value
      PARAMETER ( VALLN = 70 )

*  Local Variables:
      INTEGER CDKYLN             ! Length of the keyword of a card
      CHARACTER * ( HKEYLN ) CMPKEY ! Compound name
      INTEGER CMPLN              ! Length of compound name
      LOGICAL COMCRD             ! Commentary-keyword flag
      LOGICAL COMPND             ! Compound-keyword flag
      CHARACTER * ( KEYLN ) CRDKEY ! Keyword of a card image
      INTEGER ENDW( MXWORD )     ! End column of each keyword
      INTEGER EQUALS             ! The position of equal sign
      INTEGER I                  ! Do loop indicator
      INTEGER KEYLEN             ! Length of specified keyword
      CHARACTER * ( HKEYLN ) KEYWRD ! Specified keyword
      INTEGER LSTAT              ! Local status
      INTEGER NWORD              ! Number of keywords in a card image
      INTEGER STARTW( MXWORD )   ! Start column of each keyword
      INTEGER WORDLN             ! Length of each word
      CHARACTER * ( KEYLN ) WORDS( MXWORD ) ! Keywords in a card image

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Remove blanks from the keyword to be searched.  Change the keyword to
*  uppercase.
      KEYWRD = NAME
      CALL CHR_RMBLK( KEYWRD )
      CALL CHR_UCASE( KEYWRD )

*  See whether it is a commentary keyword.
      COMCRD = KEYWRD .EQ. 'COMMENT' .OR. KEYWRD .EQ. 'HISTORY' .OR.
     :         KEYWRD .EQ. ' ' .OR. COMCAR

*  Get the used length of the keyword.  By definition a blank card has
*  eight characters.
      IF ( KEYWRD .EQ. ' ' ) THEN
         KEYLEN = KEYLN
      ELSE
         KEYLEN = CHR_LEN( KEYWRD )
      END IF

*  Initialise the card number to be examined, and the found flag.
      CARD = MAX( 1, STCARD )
      THERE = .FALSE.

*  To see whether it is a compound name.
      COMPND = INDEX( KEYWRD, '.' ) .NE. 0

*  If the keyword to be searched for is not compound or is commentary...
      IF ( .NOT. COMPND ) THEN

*  Compare the keyword on each card with the given keyword, until the
*  required card is found, or the 'END' card is met, or no card is left.
         DO WHILE ( ( .NOT. THERE ) .AND. ( CARD .LE. NCARD ) .AND.
     :              ( BUFFER( MIN( NCARD, CARD ) )( :3 ) .NE. 'END' ) )

*  Get the keyword of this card image, and its used length.  By
*  definition a blank card has eight characters.
            CRDKEY = BUFFER( CARD )( :KEYLN )
            IF ( CRDKEY .EQ. ' ' ) THEN
               CDKYLN = KEYLN
            ELSE
               CDKYLN = CHR_LEN( CRDKEY )
            END IF

*  If the current card is the required keyword, ...
            IF ( CRDKEY( :CDKYLN ) .EQ. KEYWRD( :KEYLEN ) ) THEN

*  Set the found flag and get the position of the equals sign.
               THERE = .TRUE.
               EQUALS = INDEX( BUFFER( CARD ), '=' )

*  Otherwise go to the next card in the buffer.
            ELSE
               CARD = CARD + 1
            END IF
         END DO

*  If the keyword to be searched for is compound, ...
      ELSE

*  Get the keywords on the card and compare with given one, until the
*  required card is found, or the 'END' card is met, or no card is left.
         DO WHILE ( ( .NOT. THERE ) .AND. ( CARD .LE. NCARD ) .AND.
     :              ( BUFFER( MIN( NCARD, CARD ) )( :3 ) .NE. 'END' ) )

*  Keywords are located between the first character and the equals
*  sign.  So find the position of the equals sign first.
            EQUALS = INDEX( BUFFER( CARD ), '=' )

*  Then if the equals sign exists, extract the words from the FITS
*  card image.
            IF ( EQUALS .NE. 0 ) THEN
               CALL CHR_DCWRD( BUFFER( CARD )( :EQUALS - 1 ), MXWORD,
     :                         NWORD, STARTW, ENDW, WORDS, LSTAT )

*  Form the compound keyword by concatenating the words via the
*  delimiter.
               CMPLN = 0
               CMPKEY = ' '
               DO I = 1, NWORD
                  WORDLN = ENDW( I ) - STARTW( I ) + 1
                  CALL CHR_PUTC( WORDS( I )( :WORDLN ), CMPKEY, CMPLN )
                  IF( I .NE. NWORD ) CALL CHR_PUTC( '.', CMPKEY, CMPLN )
               END DO

*  Compare the (compound) keyword of the current card with that of the
*  compound keyword to be searched. If it is found, set the found flag.
               IF ( CMPKEY( :CMPLN ) .EQ. KEYWRD( :KEYLEN ) ) THEN
                  THERE = .TRUE.

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

*  If the keyword is found, form the new image card.
      IF ( THERE ) THEN

*  Call routine to write the header card.
         CALL FTS1_WKEYC( KEYWRD, VALUE, CMTBGN, COMNT, COMCRD,
     :                    BUFFER( CARD ), STATUS )

      END IF

      END
