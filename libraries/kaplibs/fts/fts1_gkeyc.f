      SUBROUTINE FTS1_GKEYC( NCARD, BUFFER, SCARD, NAME, NOCCUR, THERE,
     :                       VALUE, COMENT, CARD, STATUS )
*+
*  Name:
*     FTS1_GKEYC

*  Purpose:
*     Gets the value and comment of a named header of type CHARACTER
*     from a buffer of FITS-header card images.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_GKEYC( NCARD, BUFFER, SCARD, NAME, NOCCUR, THERE, VALUE,
*                      COMENT, CARD, STATUS )

*  Description:
*     This routine searches a buffer containing the header card images
*     from a FITS file for a keyword NAME; and returns its value (as a
*     character string) and comment, and the number of the card image
*     within the buffer array that contains the named keyword.  The
*     search ends when the next end of a header block, marked by the
*     END keyword, is encountered or the buffer is exhausted.  If the
*     keyword is present THERE is true, otherwise it is false.  If the
*     keyword is expected to be present more than once then the
*     argument NOCCUR controls which occurrence will be retrieved.  If
*     a keyword is not found then no error results and the argument
*     VALUE remains unmodified.
*
*     The name may be compound to permit reading of hierarchical
*     keywords (with a blank regulation keyword).  This routine will
*     also work for HISTORY, COMMENT and the ' ' (blank) keyword
*     comment cards.  Cards without an equals sign present are also
*     regarded as comment cards.  Comment cards have no returned
*     value, only a comment.

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
*        The value of this argument specifies which occurrence of a
*        keyword should be used, if multiple ones are expected.  Any
*        value less than or equal to 1 indicates the first occurrence.
*     THERE = LOGICAL (Returned)
*        If .TRUE., the keyword given by argument NAME is present,
*        regardless of the exit status.
*     VALUE = CHARACTER * ( * ) (Returned)
*        The value of the keyword.  The string is truncated to the
*        length of VALUE if the FITS value contains more characters than
*        that.
*     COMENT = CHARACTER * ( * ) (Returned)
*        The comment associated with the keyword.
*     CARD = INTEGER (Returned)
*        The number of the card containing the named keyword.  If the
*        card could not be found this is set to zero.
*     STATUS = INTEGER (Given)
*        Global status value.

*  Copyright:
*     Copyright (C) 1988, 1989, 1990, 1991, 1992, 1994 Science &
*                   Engineering Research Council.
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
*     MJC: Malcolm J. Currie (STARLINK)
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     1988 September 22 (MJC):
*        Original version.
*     1989 July 17 (MJC):
*        Removed the trailing quote if present.
*     1989 July 28 (MJC):
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
*     1994 July 21 (PDRAPER):
*        Rewrite to remove need for quoted strings so HISTORY and
*        COMMENT strings are allowed (these do not necessarily have the
*        form "keyword = ' value  '/ comment", "keyword 'value'" is more
*        likely).
*     1994 September 12 (PDRAPER):
*        Added ability to locate a specified occurrence for multiple
*        FITS keywords.
*     1996 November 1 (MJC):
*        Reintegrated into the FTS1 library.  Allowed for doubled
*        quotes.  Some tidying.
*     1996 November 14 (MJC):
*        Added COMENT argument.  Returns the comments from a comment
*        card via the COMENT argument.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

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
      LOGICAL THERE              ! Card containing the keyword is
                                 ! present
      CHARACTER * ( * ) VALUE    ! Value of the keyword
      CHARACTER * ( * ) COMENT   ! Comment associated with the keyword
      INTEGER CARD               ! Number of the card image containing
                                 ! keyword NAME

*  Status:
      INTEGER STATUS

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Number of characters in a string
                                 ! ignoring trailing blanks

*  Local Constants:
      INTEGER KEYLN              ! Maximum number of characters in a
                                 ! FITS header card keyword or
                                 ! hierarchical component thereof
      PARAMETER ( KEYLN = 8 )

      INTEGER MXWORD             ! Maximum number of hierarchical levels
                                 ! in a keyword
      PARAMETER ( MXWORD = 20 )

*  Local Variables:
      CHARACTER * ( 72 ) CMPKEY  ! Compound name
      INTEGER COMCOL             ! Column number containing the comment
                                 ! character in the current card image
      LOGICAL COMCAR             ! Comment card?
      LOGICAL COMPND             ! Supplied name is compound?
      LOGICAL COMPRE             ! Comment present?
      CHARACTER * ( 8 ) CRDKEY   ! Card keyword
      INTEGER DQCOL              ! Column where to start search for
                                 ! doubled quotes
      LOGICAL DQPRES             ! Doubled quote present?
      INTEGER ENDCOL             ! Column of end of string section
      INTEGER ENDW( MXWORD )     ! End columns of each keyword in a card
                                 ! image
      INTEGER EQUALS             ! Column number containing the first
                                 ! equals sign in the current card image
      INTEGER I                  ! Loop counter
      INTEGER ISTAT              ! Local status
      CHARACTER * ( 80 ) KEYWRD  ! The compound keyword
      INTEGER LQCOL              ! Column containing the start of string
      INTEGER NC                 ! Number of characters
      INTEGER NCDQ               ! Column where doubled quotes located
      INTEGER NCHAR              ! Dummy
      INTEGER NCK                ! Number of characters in the compound
                                 ! keyword
      INTEGER NF                 ! Number of occurrences found
      INTEGER NWORD              ! Number of keywords in the current
                                 ! card image
      INTEGER STARTW( MXWORD )   ! Start columns of each keyword in a
                                 ! card image
      INTEGER STACOL             ! Column of start of string section
      INTEGER TQCOL              ! Column of end of string
      CHARACTER * ( KEYLN ) WORDS( MXWORD ) ! The keywords in the
                                 ! current card image

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise some variables.
      CARD = MAX( 1, SCARD )
      THERE = .FALSE.
      NF = 0
      DQPRES = .FALSE.
      COMENT = ' '

*  Remove blanks from the keyword to be searched for, and make it
*  uppercase for comparisons.
      KEYWRD = NAME
      CALL CHR_UCASE( KEYWRD )
      CALL CHR_RMBLK( KEYWRD )
      NC = MAX( CHR_LEN( KEYWRD ), 1 )

*  Is this one of the special keywords 'COMMENT', 'HISTORY' or blank?
      COMCAR = KEYWRD( : NC ) .EQ. 'COMMENT' .OR.
     :         KEYWRD( : NC ) .EQ. 'HISTORY' .OR.
     :         KEYWRD( : NC ) .EQ. ' '

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
   10    CONTINUE                ! Start of 'DO WHILE' loop
         IF ( ( .NOT. THERE ) .AND.
     :        ( CARD .LE. NCARD ) .AND.
     :        ( BUFFER( MIN( NCARD, CARD ) )( :3 ) .NE. 'END' ) ) THEN

*  Extract the keyword from this card image.
            CRDKEY = BUFFER( CARD )( :8 )

*  Is the current card the required keyword?
            IF ( CRDKEY .EQ. KEYWRD( :NC ) ) THEN

*  Check that this is the required occurrence.
               NF = NF + 1
               IF ( NF .GE. NOCCUR ) THEN

*  The keyword is present.
                  THERE = .TRUE.

*  Does the current card have a value, i.e. is an equals sign present?
*  Only check this if the keyword isn't special, comments etc. may have
*  arbitrary equals signs embedded in them.  If this isn't the case,
*  skip over it; otherwise just set character position to be one after
*  the end of the keyword, except when the keyword denotes a comment.
*  This means that the '=' may only be in column 9, so check for this.
                  IF ( .NOT. COMCAR ) THEN
                     EQUALS = INDEX( BUFFER( CARD ), '=' ) + 1
                     IF ( EQUALS .EQ. 1 ) EQUALS = CHR_LEN( CRDKEY ) + 1
                  ELSE

*  Look for an equals sign in column 9.
                     IF ( BUFFER( CARD )( 9: 9 ) .EQ. '=' ) THEN
                        EQUALS = 10
                     ELSE
                        EQUALS = CHR_LEN( CRDKEY ) + 1
                     END IF
                  END IF

*  Extract the string from the buffer.
*  ===================================

*  Look for a first quote.  If this exists then look for a second.  If
*  the first quote doesn't exist then use the current position as the
*  start of the string; the final position is the presumed location of
*  the start of the comment (or the end of the string).  The comment
*  keywords are an exception and are returned unchanged.
                  IF ( .NOT. COMCAR ) THEN
                     CALL CHR_FANDL( BUFFER( CARD ) ( EQUALS: ), LQCOL,
     :                               TQCOL )
                     LQCOL = LQCOL + EQUALS - 1
                     TQCOL = 0
                     IF ( BUFFER( CARD )( LQCOL:LQCOL ) .EQ. '''' ) THEN

*  Look out for doubled quotes, which are used in the FITS value to
*  indicate a single quote, as they would otherwise indicate the end of
*  the string.  Search from the start of the value, and then for each
*  doubled quote.  Set the flag to record any doubled quotes that are
*  present.
                        NCDQ = 1
                        DQCOL = LQCOL + 1
   20                   CONTINUE        ! Start of 'DO WHILE' loop
                        IF ( NCDQ .GT. 0 ) THEN
                           NCDQ = INDEX( BUFFER( CARD )( DQCOL: ),
     :                            '''''' )
                           DQPRES = DQPRES .OR. NCDQ .GT. 0
                           IF ( NCDQ .GT. 0 ) DQCOL = DQCOL + NCDQ + 1
                           GO TO 20
                        END IF

*  Look for the second quote (starting after any doubled quote).
                        LQCOL = LQCOL + 1
                        TQCOL = INDEX( BUFFER( CARD )( DQCOL: ), '''' )

                        IF ( TQCOL .EQ. 0 ) THEN

*  There is no second quote.  Use a second method for extracting the
*  string.
                           LQCOL = LQCOL - 1

*  Set the position of end of the string to avoid the last quote (hence
*  minus 2 rather than the normal minus 1).  Note the offset is
*  immediately following the last doubled quote.
                        ELSE
                           TQCOL = DQCOL + TQCOL - 2
                        END IF
                     END IF

*  If the location of the end of the string is zero then no upper bound
*  exists for the string.  Attempt to locate the start of the comment
*  by looking for the recommended  '/' delimiter.
                     COMCOL = INDEX( BUFFER( CARD )( LQCOL: ), '/' )
                     COMPRE = .TRUE.
                     IF ( TQCOL .EQ. 0 ) THEN
                        IF ( COMCOL .EQ. 0 ) THEN

*  No comment is present.  Set the end of string to its length.
                           TQCOL = LEN( BUFFER( CARD ) )
                           COMPRE = .FALSE.
                        ELSE

*  A comment was found so adjust location of the end of the string.
                           TQCOL = LQCOL + COMCOL - 2

                        END IF
                     END IF

*  Extract the comment.  When there is no recommended slash comment
*  delimiter, extract the comment after the space following the value.
*  When there is a slash either start the comment immediately following
*  the slash (when the comment abuts the slash), or allow for the
*  recommended space following the slash.  Other spaces are regarded as
*  significant leading spaces in the comment.
                     IF ( COMPRE ) THEN
                        IF ( COMCOL .EQ. 0 ) THEN
                           COMENT = BUFFER( CARD )( TQCOL + 2: )

                        ELSE
                           COMCOL = LQCOL + COMCOL - 1
                           IF ( BUFFER( CARD )( COMCOL + 1:
     :                          COMCOL + 1 ) .EQ. ' ' ) THEN
                              COMENT = BUFFER( CARD )( COMCOL + 2: )
                           ELSE
                              COMENT = BUFFER( CARD )( COMCOL + 1: )
                           END IF
                        END IF
                     END IF

*  Comment keyword.
*  ================
                  ELSE
                     LQCOL = EQUALS
                     TQCOL = CHR_LEN( BUFFER( CARD ) )

*  Make sure the bounds are correct for the blank case
*  (LQCOL = TQCOL+1).
                     TQCOL = MAX( LQCOL, TQCOL )
                     LQCOL = MIN( LQCOL, TQCOL )
                  END IF

*  Replace any doubled quotes with single quotes.  This is achieved by
*  repeatedly searching forwards for each doubled quote in the FITS
*  card, extracting from the start to the first of the quotes,
*  appending this to the value string.  When the last of the doubled
*  quotes has been located, just append any remainder of the string
*  from the FITS card.  The start position is immediately following the
*  double quote, except initially when it's the first string character.
                  IF ( DQPRES ) THEN
                     STACOL = LQCOL
                     ENDCOL = LQCOL
                     NCHAR = 0

   30                CONTINUE      ! Start of 'DO WHILE' loop
                     IF ( ENDCOL .LE. TQCOL ) THEN
                        CALL CHR_FIND( BUFFER( CARD ), '''', .TRUE.,
     :                                 ENDCOL )

                        IF ( ENDCOL .LE. TQCOL ) THEN
                           CALL CHR_APPND( BUFFER( CARD )( STACOL:
     :                                     ENDCOL ), VALUE, NCHAR )

                           ENDCOL = ENDCOL + 2
                           STACOL = ENDCOL
                        END IF
                        GO TO 30
                     END IF

                     IF ( STACOL .LE. TQCOL ) THEN
                        CALL CHR_APPND( BUFFER( CARD )( STACOL:TQCOL ),
     :                                  VALUE, NCHAR )
                     END IF
                  ELSE

*  Extract the value, allowing for truncation.
                     CALL CHR_CTOC( BUFFER( CARD )( LQCOL:TQCOL ),
     :                              VALUE, NCHAR )
                  END IF

               ELSE

*  Move to the next card in the buffer.
                  CARD = CARD + 1
               END IF
            ELSE

*  Move to the next card in the buffer.
               CARD = CARD + 1
            END IF

*  Return to the start of the 'DO WHILE' loop
            GO TO 10
         END IF

*  Hierarchical-keyword case.
*  ==========================
      ELSE

*  Now loop through the cards ('END' terminates header).
   40    CONTINUE                ! Start of 'DO WHILE' loop
         IF ( ( .NOT. THERE ) .AND.
     :        ( CARD .LE. NCARD ) .AND.
     :        ( BUFFER( MIN( NCARD, CARD ) )( :3 ) .NE. 'END' ) ) THEN

*  Does the current card have a value, i.e. is there an equals sign
*  present?  This is necessary to be able to descriminate the value
*  from a hierarchical keyword.
            EQUALS = INDEX( BUFFER( CARD ), '=' ) + 1
            IF ( EQUALS .NE. 1 ) THEN

*  Extract the words from the FITS card image up to the equals sign,
*  assuming these to be keywords.
               CALL CHR_DCWRD( BUFFER( CARD )( :EQUALS - 2 ), MXWORD,
     :                         NWORD, STARTW, ENDW, WORDS, ISTAT )

*  Form a compound name if there is more than one supposed keyword by
*  concatenating the words via the delimiter.
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

*  Look for a first quote.  If this exists then look for a second.  If
*  the first quote doesn't exist then use the current position as the
*  start of the string; the final position is the presumed location of
*  the start of the comment (or the end of the string).
                     CALL CHR_FANDL( BUFFER( CARD ) ( EQUALS: ), LQCOL,
     :                               TQCOL )
                     LQCOL = LQCOL + EQUALS - 1
                     TQCOL = 0
                     IF ( BUFFER( CARD )( LQCOL:LQCOL ) .EQ. '''' ) THEN

*  Look out for doubled quotes, which are used in the FITS value to
*  indicate a single quote, as they would otherwise indicate the end of
*  the string.  Search from the start of the value, and then for each
*  doubled quote.  Set the flag to record any double quotes that are
*  present.
                        NCDQ = 1
                        DQCOL = LQCOL + 1
   50                   CONTINUE        ! Start of 'DO WHILE' loop
                        IF ( NCDQ .GT. 0 ) THEN
                           NCDQ = INDEX( BUFFER( CARD )( DQCOL: ),
     :                            '''''' )
                           DQPRES = DQPRES .OR. NCDQ .GT. 0
                           IF ( NCDQ .GT. 0 ) DQCOL = DQCOL + NCDQ + 1
                           GO TO 50
                        END IF

*  Look for a second quote.
                        LQCOL = LQCOL + 1
                        TQCOL = INDEX( BUFFER( CARD )( LQCOL: ), '''' )
                        IF ( TQCOL .EQ. 0 ) THEN

*  There is no second quote.  Thus use second method for extracting the
*  string.
                           LQCOL = LQCOL - 1

*  Set the position of end of the string to avoid the last quote (hence
*  minus 2 rather than the normal minus 1).  Note the offset is
*  immediately following the last doubled quote.
                        ELSE
                           TQCOL = DQCOL + TQCOL - 2
                        END IF
                     END IF

*  If the end of the string location is zero, then no upper bound
*  exists for the string.  Attempt to locate the start of the comment
*  by looking for the recommended '/' delimiter.
                     COMCOL = INDEX( BUFFER( CARD )( LQCOL: ), '/' )
                     COMPRE = .TRUE.
                     IF ( TQCOL .EQ. 0 ) THEN
                        IF ( COMCOL .EQ. 0 ) THEN

*  There is no comment.  Set the end of the string to its length.
                           TQCOL = LEN( BUFFER( CARD ) )
                           COMPRE = .FALSE.
                        ELSE

*  A comment is present and adjust the column of the string
*  accordingly.
                           TQCOL = LQCOL + COMCOL - 2
                        END IF
                     END IF

*  Extract the comment.  When there is no recommended slash comment
*  delimiter, extract the comment after the space following the value.
*  When there is a slash either start the comment immediately following
*  the slash (when the comment abuts the slash), or allow for the
*  recommended space following the slash.  Other spaces are regarded as
*  significant leading spaces in the comment.
                     IF ( COMPRE ) THEN
                        IF ( COMCOL .EQ. 0 ) THEN
                           COMENT = BUFFER( CARD )( TQCOL + 2: )

                        ELSE
                           COMCOL = LQCOL + COMCOL - 1
                           IF ( BUFFER( CARD )( COMCOL + 1:
     :                          COMCOL + 1 ) .EQ. ' ' ) THEN
                              COMENT = BUFFER( CARD )( COMCOL + 2: )
                           ELSE
                              COMENT = BUFFER( CARD )( COMCOL + 1: )
                           END IF
                        END IF
                     END IF

*  Replace any doubled quotes with single quotes.  This is achieved by
*  repeatedly searching forwards for each doubled quote in the FITS
*  card, extracting from the start to the first of the quotes,
*  appending this to the value string.  When the last of the doubled
*  quotes has been located, just append any remainder of the string
*  from the FITS card.  The start position is immediately following the
*  double quote, except initially when it's the first string character.
                     IF ( DQPRES ) THEN
                        STACOL = LQCOL
                        ENDCOL = LQCOL
                        NCHAR = 0

   60                   CONTINUE      ! Start of 'DO WHILE' loop
                        IF ( ENDCOL .LE. TQCOL ) THEN
                           CALL CHR_FIND( BUFFER( CARD ), '''', .TRUE.,
     :                                    ENDCOL )

                           IF ( ENDCOL .LE. TQCOL ) THEN
                              CALL CHR_APPND( BUFFER( CARD )( STACOL:
     :                                        ENDCOL ), VALUE, NCHAR )

                             ENDCOL = ENDCOL + 2
                             STACOL = ENDCOL
                           END IF
                           GO TO 60
                        END IF

                        IF ( STACOL .LE. TQCOL ) THEN
                           CALL CHR_APPND( BUFFER( CARD )( STACOL:
     :                                     TQCOL ), VALUE, NCHAR )
                        END IF
                     ELSE

*  Extract the value, allowing for truncation.
                        CALL CHR_CTOC( BUFFER( CARD )( LQCOL:TQCOL ),
     :                                 VALUE, NCHAR )
                     END IF
                  ELSE

*  Move to the next card in the buffer.
                     CARD = CARD + 1
                  END IF
               ELSE

*  Move to the next card in the buffer.
                  CARD = CARD + 1
               END IF
            ELSE

*  Onto the next card in the buffer.
               CARD = CARD + 1
            END IF

*  Continue the 'DO 'WHILE' loop.
            GO TO 40
         END IF
      END IF

*  Return a comment card as the comment argument, not the value.
      IF ( COMCAR ) THEN
         COMENT = VALUE
         VALUE = ' '
      END IF

      IF ( .NOT. THERE ) CARD = 0

      END
