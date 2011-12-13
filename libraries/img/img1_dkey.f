      SUBROUTINE IMG1_DKEY( NCARD, BLOCK, KEYWRD, NOCCUR, DELETE,
     :                      STATUS )
*+
* Name:
*    IMG1_DKEY

*  Purpose:
*     Delete a FITS keyword, value and comment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_DKEY( NCARD, BLOCK, KEYWRD, NOCCUR, DELETE, STATUS )

*  Description:
*     This routine locates a record which contains the NOCCUR'th
*     occurrence of the FITS keyword (in a "FITS block") and sets the
*     record to ' '. The keyword may be hierarchical.

*  Arguments:
*     NCARD = INTEGER (Given)
*        The number of elements (cards) in BLOCK.
*     BLOCK( NCARD ) = CHARACTER * ( * ) (Given and Returned)
*        The FITS block (note this is passed at this point so that it is
*        before the other *(*) characters which allows this array to be
*        mapped -- see SUN/92).
*     KEYWRD = CHARACTER * ( * ) (Given)
*        The FITS keyword. This may be hierarchical. The components of
*        the name should be separated by periods (e.g. ING.DETHEAD
*        results in a keyword 'ING DETHEAD').
*     NOCCUR = INTEGER (Given)
*        The occurence of the keyword to delete. Less than equal to one
*        means the first one.
*     DELETE = LOGICAL (Returned)
*        Whether or not the keyword was located and its record
*        "deleted".
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
*     27-JUL-1994 (PDRAPER):
*        Original version.
*     12-SEP-1994 (PDRAPER):
*        Added ability to delete an occurence of a keyword.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) KEYWRD
      INTEGER NCARD
      INTEGER NOCCUR

*  Arguments Given and Returned:
      CHARACTER * ( * ) BLOCK( NCARD )

*  Arguments Returned:
      LOGICAL DELETE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Used length of string

*  Local Constants:
      INTEGER MXWORD             ! Maximum number of hierarchical levels
                                 ! in a keyword
      PARAMETER ( MXWORD = 20 )

*  Local Variables:
      CHARACTER * ( 72 ) CMPKEY  ! Compound name
      CHARACTER * ( 8 )  WORDS( MXWORD ) ! The keywords in the current
                                         ! card image
      CHARACTER * ( 8 ) CRDKEY   ! Card keyword
      CHARACTER * ( 80 ) BUFFER  ! The compound keyword
      INTEGER CARD               ! Current record number
      INTEGER ENDW( MXWORD )     ! End columns of each keyword in a card
                                 ! image
      INTEGER EQUALS             ! Column number containing the first
                                 ! equals sign in the current card image
      INTEGER I                  ! Loop counter
      INTEGER ISTAT              ! Local status
      INTEGER NC                 ! Number of characters
      INTEGER NCK                ! Number of characters in the compound
                                 ! keyword
      INTEGER NF                 ! Number of occurrences located
      INTEGER NWORD              ! Number of keywords in the current
                                 ! card image
      INTEGER STARTW( MXWORD )   ! Start columns of each keyword in a
                                 ! card image
      LOGICAL COMPND             ! Supplied name is compound
*.

*  Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise some variables.
      CARD = 1
      DELETE = .FALSE.
      NF = 0

*  Remove blanks from the keyword to be searched for, and make it
*  uppercase for comparisons.  Find its effective length.
      BUFFER = KEYWRD
      CALL CHR_UCASE( BUFFER )
      CALL CHR_RMBLK( BUFFER )
      NC = CHR_LEN( BUFFER )

*  Is it a compound name?
      COMPND = INDEX( BUFFER, '.' ) .NE. 0

*  The simple case.
*  ================
      IF ( .NOT. COMPND ) THEN

*  Now loop through the cards.  Compare the keyword on each word with
*  the given keyword, until the required card is found, or the 'END'
*  card is met, or there are no cards remaining.
         DO WHILE ( ( .NOT. DELETE ) .AND. ( CARD .LE. NCARD ) .AND.
     :              ( BLOCK( MIN( NCARD, CARD ) )( :3 ) .NE. 'END' ) )

*  Extract the keyword from this card image.
            CRDKEY = BLOCK( CARD )( :8 )

*  Is the current card the required keyword?
            IF ( CRDKEY( :CHR_LEN( CRDKEY ) ) .EQ. BUFFER( :NC ) ) THEN
               NF = NF + 1

*  Is this the correct occurence.
               IF ( NF .GE. NOCCUR ) THEN

*  The keyword is present set it to blank.
                  BLOCK( CARD ) = ' '

*  Keyword has been located and deleted.
                  DELETE = .TRUE.

               ELSE
*  Onto the next card in the buffer.
                  CARD = CARD + 1
               END IF
            ELSE

*  Onto the next card in the buffer.
                  CARD = CARD + 1
            END IF
         END DO

*  Hierarchical-keyword case.
*  ==========================
      ELSE

*  Now loop through the cards ('END' terminates header).
         DO WHILE ( .NOT. DELETE .AND. CARD .LE. NCARD .AND.
     :               BLOCK( MIN( NCARD, CARD ) )( :3 ) .NE. 'END' )

*  Does the current card have a value, i.e. an equals sign.  (This is
*  not foolproof because of the ING format et al.  uses blank fields,
*  comments and history to store data.
            EQUALS = INDEX( BLOCK( CARD ), '=' )
            IF ( EQUALS .NE. 0 ) THEN

*  Extract the words from the FITS card image up to the
*  equals sign, assuming these to be keywords.
               CALL CHR_DCWRD( BLOCK( CARD )( :EQUALS-1 ), MXWORD,
     :                         NWORD, STARTW, ENDW, WORDS, ISTAT )

*  Form compound name if there is more than one supposed
*  keyword by concatenating the words via the delimeter.
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
               IF ( CMPKEY( :NCK ) .EQ. BUFFER( :NCK ) ) THEN

*  Check that this is the required occurence.
                  IF ( NF .GE. NOCCUR ) THEN

*  The keyword is present.
                     DELETE = .TRUE.
                     BLOCK( CARD ) = ' '
                  ELSE

*  Onto the next card in the buffer.
                     CARD = CARD + 1
                  END IF
               ELSE

*  Onto the next card in the buffer.
                  CARD = CARD + 1
               END IF
            ELSE

*  Onto the next card in the buffer.
               CARD = CARD + 1
            END IF
         END DO
      END IF
      END
* $Id$
