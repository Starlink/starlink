      SUBROUTINE FTS1_GKEYC( NCARD, BUFFER, SCARD, NAME, THERE, VALUE,
     :                       CARD, STATUS )
*+
*  Name:
*     FTS1_GKEYC

*  Purpose:
*     Get the value of a named header of type CHARACTER from a buffer of
*     FITS header card images.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_GKEYC( NCARD, BUFFER, SCARD, NAME, THERE, VALUE, CARD,
*     :                 STATUS )

*  Description:
*     This routine searches a buffer containing the header card images
*     from a FITS file for card with keyword %NAME; and returns its
*     CHARACTER value, %VALUE, and the number of the card image within
*     the buffer array that contains the named keyword.  The search
*     ends when the next end of a header block, marked by the END
*     keyword, is encountered or the buffer is exhausted.  If the
*     keyword is present %THERE is true, otherwise it is false.  If the
*     parameter is present more than once in the header, only the first
*     occurence will be used.
*
*     The name may be compound to permit reading of hierarchical
*     keywords.  This routine will also work for HISTORY and COMMENTS
*     provided there is but one value given on the line, i.e. only
*     one "keyword = 'value'" before any comment marker. An error will
*     result otherwise.

*  Arguments:
*     NCARD = INTEGER (Given)
*        The number of card images in the buffer.
*     BUFFER( NCARD ) = CHARACTER * 80 (Given)
*        The buffer containing the header card images.
*     SCARD = INTEGER (Given)
*        The number of the card from where the search will begin.  This
*        is needed because the headers make contain a dummy header
*        prior to an extension.
*     NAME = CHARACTER * ( * ) (Given)
*        The name of the keyword whose value is required.  This may be
*        a compound name to handle hierarchical keywords, and it has
*        the form keyword1.keyword2.keyword3 etc.  The maximum number of
*        keywords per FITS card is 20.  Comparisons are performed in
*        uppercase and blanks are removed.  Each keyword must be no
*        longer than 8 characters.
*     THERE = LOGICAL (Returned)
*        If true the keyword %NAME is present.
*     VALUE = CHARACTER * ( * ) (Returned)
*        The value of the keyword.  The string is truncated to the
*        length of VALUE if the FITS value contains more characters than
*        tat.
*     CARD = INTEGER (Returned)
*        The number of the card containing the named keyword.
*        If the card could not be found this is set to zero.
*     STATUS = INTEGER (Given)
*        Global status value.

*  Algorithm:
*     -  Initialise counter and flag.  Test whether the input name is
*     compound by looking for the delimiter.
*     -  If it is not compound loop for all cards until the last card,
*     or the requested card is located.  Compare each card with the
*     desired keyword.  Once the required card is found set flag to
*     say keyword has been found, define search limits for the trailing
*     quote and extract the character string from between the quotes
*     in the buffer, otherwise go to the next card.
*     -  For a compound name loop for all cards until the last card,
*     or the requested card is located.  Go to the next card if the
*     current card does not contain an equals sign.  Generate the
*     compound keyword from the keywords before the equals sign. Compare
*     the compound keyword with the desired name.  Once the required
*     card is found set flag to say keyword has been found, find the
*     extent of the value in the card by finding any comment delimeter,
*     define search limits for the trailing
*     quote and extract the character string from between the quotes
*     in the buffer, otherwise go to the next card.
*     -  Reset card number to zero if the keyword is not present

*  Copyright:
*     Copyright (C) 1988-1992 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
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
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT  NONE           ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'       ! SSE global definitions
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_PAR'

*  Arguments Given:
      INTEGER
     :  NCARD,                 ! Number of FITS card images to search
     :  SCARD                  ! Search-start card number

      CHARACTER * ( * )
     :  BUFFER( NCARD ) * 80,  ! FITS tape buffer
     :  NAME                   ! Name of the keyword

*  Arguments Returned:
      LOGICAL                  ! True if:
     :  THERE                  ! Card containing the keyword is
                               ! present

      CHARACTER * ( * )
     :  VALUE                  ! Value of the keyword

      INTEGER
     :  CARD                   ! Number of the card image containing
                               ! keyword NAME

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER
     :  CHR_LEN                ! Number of characters in a string
                               ! ignoring trailing blanks

*  Local Constants:
      INTEGER MXWORD           ! Maximum number of hierarchical levels
                               ! in a keyword
      PARAMETER ( MXWORD = 20 )

*  Local Variables:
      CHARACTER
     :  CMPKEY * 72,           ! Compound name
     :  CRDKEY * 8,            ! Card keyword
     :  KEYWRD * 80,           ! The compound keyword
     :  WORDS( MXWORD ) * 8    ! The keywords in the current card image

      INTEGER
     :  COLIM,                 ! Column limit up to which the trailing
                               ! quote is to be searched
     :  ENDW( MXWORD ),        ! End columns of each keyword in a card
                               ! image
     :  EQUALS,                ! Column number containing the first
                               ! equals sign in the current card image
     :  I,                     ! Loop counter
     :  ISTAT,                 ! Local status
     :  LQCOL,                 ! Column containing the leading quote of
                               ! the value
     :  NC,                    ! Number of characters
     :  NCK,                   ! Number of characters in the compound
                               ! keyword
     :  NWORD,                 ! Number of keywords in the current card
                               ! image
     :  STARTW( MXWORD ),      ! Start columns of each keyword in a
                               ! card image
     :  TQCOL                  ! Column of trailing quote

      LOGICAL                  ! True if:
     :  COMPND,                ! Supplied name is compound
     :  LEADQ                  ! Leading quote is present

*.


*    Check the inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise some variables.

      CARD = MAX( 1, SCARD )
      THERE = .FALSE.
      VALUE = ' '

*    Remove blanks from the keyword to be searched for, and make it
*    uppercase for comparisons.

      KEYWRD = NAME
      CALL CHR_UCASE( KEYWRD )
      CALL CHR_RMBLK( KEYWRD )
      NC = CHR_LEN( KEYWRD )

*    Is it a compound name?

      COMPND = INDEX( KEYWRD, '.' ) .NE. 0

*    The simple case.
*    ================

      IF ( .NOT. COMPND ) THEN

*        Now loop through the cards.  Compare the keyword on each word
*        with the given keyword, until the required card is found, or
*        the 'END' card is met, or there are no cardd remaining.

         DO WHILE ( ( .NOT. THERE ) .AND. ( CARD .LE. NCARD ) .AND.
     :              ( BUFFER( MIN( NCARD, CARD ) )( :3 ) .NE. 'END' ) )

*          Extract the keyword from this card image.

            CRDKEY = BUFFER( CARD )( :8 )

*          Is the current card the required keyword?

            IF ( CRDKEY( :CHR_LEN( CRDKEY ) ) .EQ. NAME( :NC ) ) THEN

*             The keyword is present.

               THERE = .TRUE.

*             Does the current card have a value, i.e. an equals sign.
*             (This is not foolproof because of the ING format et al.
*             uses blank fields, comments and history to store data.

               EQUALS = INDEX( BUFFER( CARD ), '=' )

*             Extract the string from the buffer.
*             ===================================

*             Find the positions of the first quote and slash.  It
*             does not matter at this point that the slash may be
*             part of the value.  Here we want to ensure that the quote
*             is not part of the comment.

               LQCOL = INDEX( BUFFER( CARD )( EQUALS + 1: ), '''' )
               COLIM = INDEX( BUFFER( CARD )( EQUALS + 1: ), '/' )

               IF ( LQCOL .EQ. 0 .OR.
     :              ( COLIM .LT. LQCOL .AND. COLIM .NE. 0 ) ) THEN
                  CALL MSG_SETC( 'NAME', KEYWRD )
                  CALL MSG_OUT( 'FTS1_GKEYC_NOQUOTE',
     :              'A FITS character value must be enclosed in '/
     :              /'quotes. ^NAME does not. Inform the source '/
     :              /'institution.', STATUS )
                  CALL MSG_OUT( 'FTS1_GKEYC_NOLQUOTE',
     :              'Using the remainder of the card image from '/
     :              /'column 11 as the value.', STATUS )

*                Set up an imaginary quote immediately following
*                the equals.

                  LQCOL = EQUALS + 1
                  LEADQ = .FALSE.

*             Allow for the search offset of EQUALS characters to the
*             leading quote.

               ELSE
                  LQCOL = LQCOL + EQUALS
                  LEADQ = .TRUE.
               END IF

*             Find the trailing quote.

               TQCOL = INDEX( BUFFER( CARD )( LQCOL + 1: ), '''' )
               IF ( TQCOL .EQ. 0 .AND. LEADQ ) THEN
                  CALL MSG_SETC( 'NAME', KEYWRD )
                  CALL MSG_OUT( 'FTS1_GKEYC_NOQUOTE',
     :              'A FITS character value must be enclosed in '/
     :              /'quotes. ^NAME does not. Inform the source '/
     :              /'institution.', STATUS )
                  CALL MSG_OUT( 'FTS1_GKEYC_NOQUOTE2',
     :              'Using the remainder of the card image as the '/
     :              /'value.', STATUS )

*                Set up an imaginary quote beyond the end of the card
*                image.

                  TQCOL = 81 - LQCOL
               END IF

*             If the character length of VALUE is less than that
*             required to hold the string, it may be truncated.  Define
*             new notional closing quote.

               TQCOL = MIN( TQCOL - 1, LEN( VALUE ) ) + LQCOL + 1

*             Extract the value.

               VALUE = BUFFER( CARD )( LQCOL + 1:TQCOL - 1 )

            ELSE

*             Onto the next card in the buffer.

               CARD = CARD + 1
            END IF
         END DO

*    Hierarchical-keyword case.
*    ==========================

      ELSE

*        Now loop through the cards ('END' terminates header).

         DO WHILE ( .NOT. THERE .AND. CARD .LE. NCARD .AND.
     :               BUFFER( MIN( NCARD, CARD ) )( :3 ) .NE. 'END' )

*          Does the current card have a value, i.e. an equals sign.
*          (This is not foolproof because of the ING format et al.
*          uses blank fields, comments and history to store data.

            EQUALS = INDEX( BUFFER( CARD ), '=' )
            IF ( EQUALS .NE. 0 ) THEN

*             Extract the words from the FITS card image up to the
*             equals sign, assuming these to be keywords.

               CALL CHR_DCWRD( BUFFER( CARD )( :EQUALS-1 ), MXWORD,
     :                         NWORD, STARTW, ENDW, WORDS, ISTAT )

*             Form compound name if there is more than one supposed
*             keyword by concatenating the words via the delimeter.

               IF ( NWORD .GT. 1 ) THEN
                  NCK = 0
                  CMPKEY = ' '
                  DO I = 1, NWORD
                     NC = ENDW( I ) - STARTW( I ) + 1
                     CALL CHR_PUTC( WORDS( I )( :NC ), CMPKEY, NCK )
                     IF ( I .NE. NWORD )
     :                 CALL CHR_PUTC( '.', CMPKEY, NCK )
                  END DO

*             Merely copy the first keyword.

               ELSE
                  CMPKEY = WORDS( 1 )
                  NCK = ENDW( 1 ) - STARTW( 1 ) + 1
               END IF

*             Compare the (compound) keyword of the current card image
*             with that of the compound keyword be searched for in the
*             buffer.

               IF ( CMPKEY( :NCK ) .EQ. KEYWRD( :NCK ) ) THEN

*                The keyword is present.

                  THERE = .TRUE.

*                Find the upper range of columns that contains the value
*                associated with the hierarchical keyword.  Therefore
*                find the leading and trailing quotes.  Though first
*                find the positions of the first slash.  It does not
*                matter at this point that the slash may be part of the
*                value.  Here we want to ensure that the quote is not
*                part of the comment.

                  LQCOL = INDEX( BUFFER( CARD )( EQUALS + 1: ), '''' )
                  COLIM = INDEX( BUFFER( CARD )( EQUALS + 1: ), '/' )

                  IF ( LQCOL .EQ. 0 .OR.
     :                 ( COLIM .LT. LQCOL .AND. COLIM .NE. 0 ) ) THEN
                     CALL MSG_SETC( 'NAME', KEYWRD )
                     CALL MSG_OUT( 'FTS1_GKEYC_NOQUOTE',
     :                 'A FITS character value must be enclosed in '/
     :                 /'quotes. ^NAME does not. Inform the source '/
     :                 /'institution.', STATUS )
                     CALL MSG_OUT( 'FTS1_GKEYC_NOLQUOTE',
     :                 'Using the remainder of the card image from '/
     :                 /'column 11 as the value.', STATUS )

*                   Set up an imaginary quote immediately following
*                   the equals.

                     LQCOL = EQUALS + 1
                     LEADQ = .FALSE.

*                Allow for the search offset of EQUALS characters to the
*                leading quote.

                  ELSE
                     LQCOL = LQCOL + EQUALS
                     LEADQ = .TRUE.
                  END IF

*                Find the trailing quote, aborting if there is not one.
*                Do not report another message if the string is devoid
*                of quotes.

                  TQCOL = INDEX( BUFFER( CARD )( LQCOL + 1: ), '''' )
                  IF ( TQCOL .EQ. 0 .AND. LEADQ ) THEN
                     CALL MSG_SETC( 'NAME', KEYWRD )
                     CALL MSG_OUT( 'FTS1_GKEYC_NOQUOTE',
     :                 'A FITS character value must be enclosed in '/
     :                 /'quotes. ^NAME does not. Inform the source '/
     :                 /'institution.', STATUS )
                     CALL MSG_OUT( 'FTS1_GKEYC_NOQUOTE2',
     :                 'Using the remainder of the card image as the '/
     :                 /'value.', STATUS )

*                Set up an imaginary quote beyond the end of the card
*                image.

                     TQCOL = 81 - LQCOL
                  END IF

*                If the character length of VALUE is less than that
*                required to hold the string, it may be truncated.
*                Define new notional closing quote.

                  TQCOL = MIN( TQCOL - 1, LEN( VALUE ) ) + LQCOL + 1

*                Extract the value.

                  VALUE = BUFFER( CARD )( LQCOL + 1:TQCOL - 1 )

               ELSE

*                Onto the next card in the buffer.

                  CARD = CARD + 1

               END IF

            ELSE

*             Onto the next card in the buffer.

               CARD = CARD + 1
            END IF
         END DO
      END IF

  999 CONTINUE
      IF ( .NOT. THERE ) CARD = 0

      END
* $Id$
