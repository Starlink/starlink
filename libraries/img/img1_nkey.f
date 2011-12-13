      SUBROUTINE IMG1_NKEY( NCARD, BUFFER, NAME, FOUND, NOCCUR, STATUS )
*+
*  Name:
*     IMG1_NKEY

*  Purpose:
*    Counts the number of occurrences of a FITS keyword.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_NKEY( NCARD, BUFFER, NAME, FOUND, NOCCUR, STATUS )

*  Description:
*     This routine searches a buffer containing the header card images
*     from a FITS file for a keyword NAME. It counts all the occurrences
*     of the keyword and returns this value.
*
*     The keyword may be hierachical.

*  Arguments:
*     NCARD = INTEGER (Given)
*        The number of card images in the buffer.
*     BUFFER( NCARD ) = CHARACTER * 80 (Given)
*        The buffer containing the header card images.
*     NAME = CHARACTER * ( * ) (Given)
*        The name of the keyword. This may be a compound name to handle
*        hierarchical keywords, and it has the form
*        keyword1.keyword2.keyword3 etc. The maximum number of keywords
*        per FITS card is 20. Comparisons are performed in uppercase
*        and blanks are removed. Each keyword must be no longer than 8
*        characters.
*     FOUND = LOGICAL (Returned)
*        If true the keyword NAME is present (regardless of exit
*        status).
*     NOCCUR = INTEGER (Returned)
*        The number of occurrences of the keyword.
*     STATUS = INTEGER (Given)
*        Global status value.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
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
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     12-SEP-1994 (PDRAPER):
*        Original version based on IMG1_GKEY.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT  NONE             ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions

*  Arguments Given:
      INTEGER NCARD
      CHARACTER * ( * ) BUFFER( NCARD )
      CHARACTER * ( * ) NAME

*  Arguments Returned:
      LOGICAL FOUND
      INTEGER NOCCUR

*  Status:
      INTEGER STATUS

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Number of characters in a string
                                 ! ignoring trailing blanks

*  Local Constants:
      INTEGER MXWORD             ! Maximum number of hierarchical levels
                                 ! in a keyword
      PARAMETER ( MXWORD = 20 )

*  Local Variables:
      CHARACTER * ( 72 ) CMPKEY  ! Compound name
      CHARACTER * ( 8 ) CRDKEY   ! Card keyword
      CHARACTER * ( 8 ) WORDS( MXWORD ) ! The keywords in the current
                                        ! card image
      CHARACTER * ( 80 ) KEYWRD  ! The compound keyword
      INTEGER CARD               ! Current card index
      INTEGER ENDW( MXWORD )     ! End columns of each keyword in a card
                                 ! image
      INTEGER EQUALS             ! Column number containing the first
                                 ! equals sign in the current card image
      INTEGER I                  ! Loop counter
      INTEGER ISTAT              ! Local status
      INTEGER NC                 ! Number of characters
      INTEGER NCK                ! Number of characters in the compound
                                 ! keyword
      INTEGER NWORD              ! Number of keywords in the current
                                 ! card image
      INTEGER STARTW( MXWORD )   ! Start columns of each keyword in a
                                 ! card image
      LOGICAL COMPND             ! Supplied name is compound
*.


*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise some variables.
      CARD = 1
      FOUND = .FALSE.
      NOCCUR = 0

*  Remove blanks from the keyword to be searched for, and make it
*  uppercase for comparisons.
      KEYWRD = NAME
      CALL CHR_UCASE( KEYWRD )
      CALL CHR_RMBLK( KEYWRD )
      NC = MAX( CHR_LEN( KEYWRD ), 1 )

*  Is it a compound name?
      COMPND = INDEX( KEYWRD, '.' ) .NE. 0

*  The simple case.
*  ================
      IF ( .NOT. COMPND ) THEN

*  Now loop through the cards.  Compare the keyword on each word with
*  the given keyword, until the 'END' card is met, or there are no cards
*  remaining.
 1       CONTINUE                ! Start of 'DO WHILE' loop
         IF ( ( CARD .LE. NCARD ) .AND.
     :        ( BUFFER( MIN( NCARD, CARD ) )( :3 ) .NE. 'END' ) ) THEN

*  Extract the keyword from this card image.
            CRDKEY = BUFFER( CARD )( :8 )

*  Is the current card the required keyword?
            IF ( CRDKEY .EQ. KEYWRD( :NC ) ) THEN

*  Count this as an occurrence.
               NOCCUR = NOCCUR + 1

*  The keyword is present.
               FOUND = .TRUE.
            END IF

*  Onto the next card in the buffer.
            CARD = CARD + 1

*  Next 'WHILE'.
            GO TO 1
         END IF

*  Hierarchical-keyword case.
*  ==========================
      ELSE

*  Now loop through the cards ('END' terminates header).
 2       CONTINUE                ! Start of 'DO WHILE' loop
         IF ( ( CARD .LE. NCARD ) .AND.
     :        ( BUFFER( MIN( NCARD, CARD ) )( :3 ) .NE. 'END' ) ) THEN

*  Does the current card have a value, i.e. an equals sign.  This is
*  neccessary to be able to descriminate the value from a hierarchical
*  keyword.
            EQUALS = INDEX( BUFFER( CARD ), '=' ) + 1
            IF ( EQUALS .NE. 1 ) THEN

*  Extract the words from the FITS card image up to the equals sign,
*  assuming these to be keywords.
               CALL CHR_DCWRD( BUFFER( CARD )( :EQUALS - 2 ), MXWORD,
     :                         NWORD, STARTW, ENDW, WORDS, ISTAT )

*  Form compound name if there is more than one supposed keyword by
*  concatenating the words via the delimeter.
               IF ( NWORD .GT. 1 ) THEN
                  NCK = 0
                  CMPKEY = ' '
                  DO I = 1, NWORD
                     NC = ENDW( I ) - STARTW( I ) + 1
                     CALL CHR_PUTC( WORDS( I )( :NC ), CMPKEY, NCK )
                     IF ( I .NE. NWORD )
     :                    CALL CHR_PUTC( '.', CMPKEY, NCK )
                  END DO

*  Merely copy the first keyword.
               ELSE
                  CMPKEY = WORDS( 1 )
                  NCK = ENDW( 1 ) - STARTW( 1 ) + 1
               END IF

*  Compare the (compound) keyword of the current card image with that
*  of the compound keyword be searched for in the buffer.
               IF ( CMPKEY( :NCK ) .EQ. KEYWRD( :NCK ) ) THEN

*  Increment the occurence count.
                  NOCCUR = NOCCUR + 1

*  The keyword is present.
                  FOUND = .TRUE.
               END IF
            END IF

*  Onto the next card in the buffer.
            CARD = CARD + 1

*  Next 'WHILE'.
            GO TO 2
         END IF
      END IF
      END
* $Id$
