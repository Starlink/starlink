      SUBROUTINE RTD1_GKEYC( NCARD, BUFFER, SCARD, NAME, NOCCUR, THERE,
     :                       VALUE, CARD, STATUS )
*+
*  Name:
*     RTD1_GKEYC

*  Purpose:
*     Get the value of a named header of type CHARACTER from a buffer of
*     FITS header card images.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RTD1_GKEYC( NCARD, BUFFER, SCARD, NAME, NOCCUR, THERE, VALUE,
*                      CARD, STATUS )

*  Description:
*     This routine searches a buffer containing the header card images
*     from a FITS file for a keyword NAME and returns its value (as a
*     character string), and the number of the card image within the
*     buffer array that contains the named keyword.  The search ends
*     when the next end of a header block, marked by the END keyword,
*     is encountered or the buffer is exhausted.  If the keyword is
*     present THERE is true, otherwise it is false.  If the keyword is
*     expected to be present more than once then the argument NOCCUR
*     controls which occurrence will be retrieved. If a keyword is not
*     found then no error results and the argument VALUE remains
*     unmodified.
*
*     The name may be compound to permit reading of hierarchical
*     keywords.  This routine will also work for HISTORY, COMMENT and
*     the ' ' (blank) keyword.

*  Arguments:
*     NCARD = INTEGER (Given)
*        The number of card images in the buffer.
*     BUFFER( NCARD ) = CHARACTER * 80 (Given)
*        The buffer containing the header card images.
*     SCARD = INTEGER (Given)
*        The number of the card from where the search will begin.  This
*        is needed because the headers may contain a dummy header
*        prior to an extension.
*     NAME = CHARACTER * ( * ) (Given)
*        The name of the keyword whose value is required.  This may be
*        a compound name to handle hierarchical keywords, and it has
*        the form keyword1.keyword2.keyword3 etc.  The maximum number of
*        keywords per FITS card is 20.  Comparisons are performed in
*        uppercase and blanks are removed.  Each keyword must be no
*        longer than 8 characters.
*     NOCCUR = INTEGER (Given)
*        The value of this argument specifies which occurence of a
*        keyword should be used, if, multiple ones are expected. (Any
*        value less then equal to 1 indicates the first occurrence)
*     THERE = LOGICAL (Returned)
*        If true the keyword NAME is present (regardless of exit
*        status).
*     VALUE = CHARACTER * ( * ) (Returned)
*        The value of the keyword.  The string is truncated to the
*        length of VALUE if the FITS value contains more characters than
*        tat.
*     CARD = INTEGER (Returned)
*        The number of the card containing the named keyword.  If the
*        card could not be found this is set to the position of the
*        last card or the 'END' card..
*     STATUS = INTEGER (Given)
*        Global status value.

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council
*     Copyright (C) 2007 Science and Technology Facilities Council
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of the
*     License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
*     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
*     GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA


*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     PWD: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     1988 Sep 22 (MJC):
*        Original version.
*     1989 Jul 17 (MJC):
*        Removed the trailing quote if present.
*     1989 Jul 28 (MJC):
*        Added extra argument CARD.
*     1990 November 19 (MJC):
*        Renamed from FITSGC, and converted the prologue to the SST
*        style.
*     1991 February 28 (MJC):
*        Converted BUFFER from an assumed-size to an adjustable array
*        via the NCARD argument in case the END keyword is missing.
*     1991 July 14 (MJC):
*        Modified to handle hierarchical keywords.
*     1991 August 31 (MJC):
*        Fixed bug in comparing card keyword with the selected keyword.
*     1992 June 24 (MJC):
*        Made to handle a value without a trailing blank.  Takes the
*        remainder of the card image as the value.
*     21-JUL-1994 (PWD):
*        Incorporated into IMG_ (was called FTS1_GKEYC in KAPPA).
*     21-JUL-1994 (PWD):
*        Rewrite to remove need for quoted strings so HISTORY and
*        COMMENT strings are allowed (these do not necessarily have the
*        form "keyword = ' value  '/ comment", "keyword 'value'" is more
*        likely).
*     21-JUL-1994 (PWD):
*        Re-created flexible formatting after SCCS crisis.
*     12-SEP-1994 (PWD):
*        Added ability to read occurrence for multiple FITS keywords.
*     08-DEC-2000 (PWD):
*        Changed to test for 'END ' rather than 'END'. A standard
*        END keyword should be blank up to the eighth column.
*     31-JUL-2007 (PWD):
*        Don't start search when NCARD is 0. This reads BUFFER(0).
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT  NONE             ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions

*  Arguments Given:
      INTEGER NCARD
      INTEGER SCARD
      CHARACTER * ( * ) BUFFER( NCARD )
      CHARACTER * ( * ) NAME
      INTEGER NOCCUR

*  Arguments Returned:
      LOGICAL THERE              ! Card containing the keyword is present
      CHARACTER * ( * ) VALUE    ! Value of the keyword
      INTEGER CARD               ! Number of the card image containing
                                 ! keyword NAME

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
      CHARACTER * ( 72 )  CMPKEY ! Compound name
      CHARACTER * ( 8 ) CRDKEY   ! Card keyword
      CHARACTER * ( 8 ) WORDS( MXWORD ) ! The keywords in the current
                                        ! card image
      CHARACTER * ( 80 ) KEYWRD  ! The compound keyword
      INTEGER ENDW( MXWORD )     ! End columns of each keyword in a card
                                 ! image
      INTEGER EQUALS             ! Column number containing the first
                                 ! equals sign in the current card image
      INTEGER NF                 ! Number of occurrences found
      INTEGER I                  ! Loop counter
      INTEGER ISTAT              ! Local status
      INTEGER LQCOL              ! Column containing the start of string
      INTEGER NC                 ! Number of characters
      INTEGER NCHAR              ! Dummy
      INTEGER NCK                ! Number of characters in the compound
                                 ! keyword
      INTEGER NWORD              ! Number of keywords in the current
                                 ! card image
      INTEGER STARTW( MXWORD )   ! Start columns of each keyword in a
                                 ! card image
      INTEGER TQCOL              ! Column of end of string
      LOGICAL COMPND             ! Supplied name is compound
      LOGICAL SPEC               ! Keyword is one of the specials
*.


*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise some variables.
      CARD = MAX( 1, SCARD )
      THERE = .FALSE.
      VALUE = ' '
      NF = 0

*  If header contains no cards, then nothing to do.
      IF ( NCARD .LE. 0 ) THEN
         RETURN
      END IF

*  Remove blanks from the keyword to be searched for, and make it
*  uppercase for comparisons.
      KEYWRD = NAME
      CALL CHR_UCASE( KEYWRD )
      CALL CHR_RMBLK( KEYWRD )
      NC = MAX( CHR_LEN( KEYWRD ), 1 )

*  Is this one of the special keywords 'COMMENT', 'HISTORY' or ' '?
      SPEC = KEYWRD( : NC ) .EQ. 'COMMENT' .OR.
     :       KEYWRD( : NC ) .EQ. 'HISTORY' .OR.
     :       KEYWRD( : NC ) .EQ. ' '

*  Is it a compound name?
      COMPND = INDEX( KEYWRD, '.' ) .NE. 0

*  The simple case.
*  ================
      IF ( .NOT. COMPND ) THEN

*  Length of name is limited to 8.
         NC = MIN( NC, 8 )

*  Now loop through the cards.  Compare the keyword on each word with
*  the given keyword, until the required card is found, or the 'END'
*  card is met, or there are no cards remaining.
 1       CONTINUE                ! Start of 'DO WHILE' loop
         IF ( ( .NOT. THERE ) .AND.
     :        ( CARD .LE. NCARD ) .AND.
     :        ( BUFFER( MIN( NCARD, CARD ) )( :4 ) .NE. 'END ' ) ) THEN

*  Extract the keyword from this card image.
            CRDKEY = BUFFER( CARD )( :8 )

*  Is the current card the required keyword?
            IF ( CRDKEY .EQ. KEYWRD( :NC ) ) THEN

*  Check that this is the required occurrence.
               NF = NF + 1
               IF ( NF .GE. NOCCUR ) THEN

*  The keyword is present.
                  THERE = .TRUE.

*  Does the current card have a value, i.e. an equals sign. Only check
*  this if the keyword isn't special, comments etc. may have arbitrary
*  equals signs embedded in them. If this isn't the case skip
*  over it, otherwise just set character position to be one after the
*  end of the keyword, except when special, this means that the '=' may
*  only be in column 9, so check for this.
                  IF ( .NOT. SPEC ) THEN
                     EQUALS = INDEX( BUFFER( CARD ), '=' ) + 1
                     IF ( EQUALS .EQ. 1 ) EQUALS = CHR_LEN( CRDKEY ) + 1
                  ELSE

*  Look for equals in 9.
                     IF ( BUFFER( CARD )( 9: 9 ) .EQ. '=' ) THEN
                        EQUALS = 10
                     ELSE
                        EQUALS = CHR_LEN( CRDKEY ) + 1
                     END IF
                  END IF

*  Extract the string from the buffer.
*  ===================================

*  Look for a first quote. If this exists then look for a second. If the
*  first quote doesn't exist then use the current position as the start
*  of the string, the final position is the presumed location of the
*  start of the comment (or the end of the string). Except for the
*  special keywords which are returned unchanged.
                  IF ( .NOT. SPEC ) THEN
                     CALL CHR_FANDL( BUFFER( CARD ) ( EQUALS: ), LQCOL,
     :                               TQCOL )
                     LQCOL = LQCOL + EQUALS - 1
                     TQCOL = 0
                     IF ( BUFFER( CARD ) ( LQCOL: LQCOL ) .EQ. '''' )
     :               THEN

*  Look for second quote.
                        LQCOL = LQCOL + 1
                        TQCOL = INDEX( BUFFER( CARD )( LQCOL: ), '''' )
                        IF ( TQCOL .EQ. 0 ) THEN

*  No second quote. Use second method for extracting string.
                           LQCOL = LQCOL - 1

*  Else set the position of TQCOL to avoid last quote.
                        ELSE
                           TQCOL = LQCOL + TQCOL - 2
                        END IF
                     END IF

*  If TQCOL is zero then no upper bound exists for the string. Attempt
*  to locate the start of the comment by looking for a '/'.
                     IF ( TQCOL .EQ. 0 ) THEN
                        TQCOL = INDEX( BUFFER( CARD )( LQCOL: ), '/' )
                        IF ( TQCOL .EQ. 0 ) THEN

*  No comment. Set the end of string to its length.
                           TQCOL = LEN( BUFFER( CARD ) )
                        ELSE

*  Comment found adjust TQCOL.
                           TQCOL = LQCOL + TQCOL - 2
                        END IF
                     END IF
                  ELSE

*  Special keyword.
                     LQCOL = EQUALS
                     TQCOL = CHR_LEN( BUFFER( CARD ) )

*  Make sure bounds are correct for blank case (LQCOL = TQCOL+1).
                     TQCOL = MAX( LQCOL, TQCOL )
                     LQCOL = MIN( LQCOL, TQCOL )
                  END IF

*  Extract the value, allowing for truncation.
                  CALL CHR_CTOC( BUFFER( CARD ) ( LQCOL: TQCOL ),
     :                           VALUE, NCHAR )
               ELSE

*  Onto the next card in the buffer.
                  CARD = CARD + 1
               END IF
            ELSE

*  Onto the next card in the buffer.
               CARD = CARD + 1
            END IF

*  Next 'WHILE'.
            GO TO 1
         END IF

*  Hierarchical-keyword case.
*  ==========================
      ELSE

*  Now loop through the cards ('END' terminates header).
 2       CONTINUE                ! Start of 'DO WHILE' loop
         IF ( ( .NOT. THERE ) .AND.
     :        ( CARD .LE. NCARD ) .AND.
     :        ( BUFFER( MIN( NCARD, CARD ) )( :4 ) .NE. 'END ' ) ) THEN

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

*  Check that this is the required occurrence.
                  NF = NF + 1
                  IF ( NF .GE. NOCCUR ) THEN

*  The keyword is present.
                     THERE = .TRUE.

*  Extract the string from the buffer.
*  ===================================

*  Look for a first quote. If this exists then look for a second. If the
*  first quote doesn't exist then use the current position as the start
*  of the string, the final position is the presumed location of the
*  start of the comment (or the end of the string).
                     CALL CHR_FANDL( BUFFER( CARD ) ( EQUALS: ), LQCOL,
     :                               TQCOL )
                     LQCOL = LQCOL + EQUALS - 1
                     TQCOL = 0
                     IF ( BUFFER( CARD ) ( LQCOL: LQCOL ) .EQ. '''' )
     :               THEN

*  Look for second quote.
                        LQCOL = LQCOL + 1
                        TQCOL = INDEX( BUFFER( CARD )( LQCOL: ), '''' )
                        IF ( TQCOL .EQ. 0 ) THEN

*  No second quote. Use second method for extracting string.
                           LQCOL = LQCOL - 1

*  Else set the position of TQCOL to avoid last quote.
                        ELSE
                           TQCOL = LQCOL + TQCOL - 2
                        END IF
                     END IF

*  If TQCOL is zero then no upper bound exists for the string. Attempt
*  to locate the start of the comment by looking for a '/'.
                     IF ( TQCOL .EQ. 0 ) THEN
                        TQCOL = INDEX( BUFFER( CARD )( LQCOL: ), '/' )
                        IF ( TQCOL .EQ. 0 ) THEN

*  No comment. Set the end of string to its length.
                           TQCOL = LEN( BUFFER( CARD ) )
                        ELSE

*  Comment found adjust TQCOL.
                           TQCOL = LQCOL + TQCOL - 2
                        END IF
                     END IF

*  Extract the value, allowing for truncation.
                     CALL CHR_CTOC( BUFFER( CARD ) ( LQCOL: TQCOL ),
     :                              VALUE, NCHAR )
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

*  Next 'WHILE'.
            GO TO 2
         END IF
      END IF
      END
* @(#)img1_gkeyc.f   1.18   96/06/03 16:06:13   96/11/22 14:16:21
