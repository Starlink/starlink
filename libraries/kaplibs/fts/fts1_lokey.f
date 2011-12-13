      SUBROUTINE FTS1_LOKEY( NCARD, HEADER, KEYWRD, OCCUR, CARD,
     :                       STATUS )
*+
*  Name:
*     FTS1_LOKEY

*  Purpose:
*     Locates an occurrence of a keyword in a FITS header.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_LOKEY( NCARD, HEADER, KEYWRD, OCCUR, CARD, STATUS )

*  Description:
*     This routines find the location of a certain occurrence of a
*     named keyword in an array of FITS headers.  Hierarchical
*     keywords are allowed.  A bad status is returned if the desired
*     keyword is not present in the header array.

*  Arguments:
*     NCARD = INTEGER (Given)
*        The number of cards in the FITS header array.
*     HEADER( NCARD ) = CHARACTER * ( 80 ) (Given)
*        The array of FITS headers.
*     KEYWRD = CHARACTER * ( * ) (Given)
*        The keyword to search for in the array.  This may be a
*        compound name to handle hierarchical keywords, and it has the
*        form keyword1.keyword2.keyword3 etc.  The maximum number of
*        keywords per FITS card is 20.  Comparisons are performed in
*        uppercase and blanks are removed.  Each keyword must be no
*        longer than 8 characters.
*     OCCUR = INTEGER (Given)
*        The occurrence of the keyword to locate.  Values less than 1
*        obtain the first occurrence.
*     CARD = INTEGER (Returned)
*        The number of card containing the desired keyword.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Copyright:
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
*     {enter_new_authors_here}

*  History:
*     1996 November 4 (MJC):
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
      INTEGER NCARD
      CHARACTER * ( * ) HEADER( NCARD )
      CHARACTER * ( * ) KEYWRD
      INTEGER OCCUR

*  Arguments Returned:
      INTEGER CARD

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:
      INTEGER HKEYLN             ! Maximum number of characters in a
                                 ! FITS header card hierarchical keyword
      PARAMETER ( HKEYLN = 48 )

      INTEGER KEYLEN             ! Maximum number of characters in a
                                 ! FITS header card keyword or
                                 ! hierarchical component thereof
      PARAMETER ( KEYLEN = 8 )

      INTEGER MXWORD             ! Maximum number of hierarchical levels
                                 ! in a keyword
      PARAMETER ( MXWORD = 20 )

*  Local Variables:
      CHARACTER * ( 72 ) CMPKEY  ! Compound name
      INTEGER COCCUR             ! Local positive copy of the count
      LOGICAL COMPND             ! Supplied name is compound?
      INTEGER ENDW( MXWORD )     ! End columns of each keyword in a card
                                 ! image
      INTEGER EQUALS             ! Column number containing the first
                                 ! equals sign in the current card image
      LOGICAL HERE               ! Name is in FITS array?
      INTEGER I                  ! Do loop index
      INTEGER ISTAT              ! Local status
      INTEGER KEYLN              ! Used length of KEYNAM
      CHARACTER * ( HKEYLN ) KEYNAM ! A keyword
      INTEGER NAMELN             ! Length of a keyword on a card
      INTEGER NC                 ! Number of characters
      INTEGER NCK                ! Number of characters in the compound
                                 ! keyword
      INTEGER NOCCUR             ! Occurrence counter
      INTEGER NWORD              ! Number of keywords in the current
                                 ! card image
      INTEGER STARTW( MXWORD )   ! Start columns of each keyword in a
                                 ! card image
      CHARACTER * ( KEYLEN ) WORDS( MXWORD ) ! The keywords in the
                                 ! current card image

*.

*  Initialise the returned value.
      CARD = 0

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert it and its position name to its formal format.
      KEYNAM = KEYWRD

*  Remove all blanks and convert it to upper case.  Allow for the blank
*  keyword.
      CALL CHR_RMBLK( KEYNAM )
      CALL CHR_UCASE( KEYNAM )
      KEYLN = CHR_LEN( KEYNAM )
      IF ( KEYLN .EQ. 0 ) KEYLN = KEYLEN

*  Determine whether the keyword is compound.
      COMPND = INDEX( KEYNAM( :KEYLN ), '.' ) .NE. 0

*  Initialise counters and flag for the search.
      HERE = .FALSE.
      NOCCUR = 1
      COCCUR = MAX( 1, OCCUR )

*  If the keyword is simple, ...
      IF ( .NOT. COMPND ) THEN

*  Find the desired occurrence of the keyword, by searching by name,
*  then by occurrence.  Record the fact that the positional keyword has
*  been located.
   10    CONTINUE         !  Start of 'DO WHILE' loop
         IF ( .NOT. HERE .AND. CARD .LT. NCARD .AND.
     :        NOCCUR .LE. COCCUR ) THEN

            CARD = CARD + 1
            NAMELN = CHR_LEN( HEADER( CARD )( :KEYLEN ) )

            IF ( KEYNAM( :KEYLN ) .EQ. HEADER( CARD )( :NAMELN ) ) THEN

               IF ( NOCCUR .EQ. COCCUR ) THEN
                  HERE = .TRUE.
               ELSE
                  NOCCUR = NOCCUR + 1
               END IF
            END IF

*  Return to start of 'DO WHILE' loop.
            GO TO 10
         END IF

*  The position keyword is compound.
*  =================================
      ELSE

*  Loop until a match is found.
   20    CONTINUE         !  Start of 'DO WHILE' loop
         IF ( .NOT. HERE .AND. CARD .LT. NCARD .AND.
     :        NOCCUR .LE. COCCUR ) THEN

            CARD = CARD + 1

*  Does the current card have a value, i.e. an equals sign?  This is
*  not foolproof because of the ING format et al. uses blank fields,
*  comments and history to store data.  Fortunately, these are not
*  likely to be compound names.
            EQUALS = INDEX( HEADER( CARD ), '=' )
            IF ( EQUALS .NE. 0 ) THEN

*  Extract the words from the FITS card image up to the equals sign,
*  assuming these to be keywords.
               CALL CHR_DCWRD( HEADER( CARD )( :EQUALS-1 ), MXWORD,
     :                         NWORD, STARTW, ENDW, WORDS, ISTAT )

*  Form the compound name if there is more than one supposed keyword by
*  concatenating the words via the delimiter.
               IF ( NWORD .GT. 1 ) THEN
                  NCK = 0
                  CMPKEY = ' '
                  DO I = 1, NWORD
                     NC = ENDW( I ) - STARTW( I ) + 1
                     CALL CHR_PUTC( WORDS( I )( :NC ), CMPKEY, NCK )
                     IF ( I .NE. NWORD )
     :                 CALL CHR_PUTC( '.', CMPKEY, NCK )
                  END DO

*  Merely copy the first keyword.
               ELSE
                  CMPKEY = WORDS( 1 )
                  NCK = ENDW( 1 ) - STARTW( 1 ) + 1
               END IF

*  Compare the (compound) keyword of the current card image with that
*  of the compound keyword being searched for in the buffer.
               IF ( CMPKEY( :NCK ) .EQ. KEYNAM( :NCK ) ) THEN

*  Find the desired occurrence of the keyword.  Record the fact that
*  the keyword has been located.
                  IF ( NOCCUR .EQ. COCCUR ) THEN
                     HERE = .TRUE.
                  ELSE
                     NOCCUR = NOCCUR + 1
                  END IF
               END IF
            END IF

*  Return to start of 'DO WHILE' loop.
            GO TO 20
         END IF
      END IF

*  Report if something went wrong.
      IF ( .NOT. HERE ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'KEY', KEYWRD )
         IF ( COCCUR .EQ. 1 ) THEN
            CALL ERR_REP( 'FTS1_LOKEY_NOTPRESENT',
     :        'FTS1_LOKEY: Supplied keyword ^KEY is not present in '/
     :        /'the FITS header.', STATUS )
         ELSE
            CALL MSG_SETI( 'N', COCCUR )
            CALL ERR_REP( 'FTS1_LOKEY_FEWEROCC',
     :        'FTS1_LOKEY: There are fewer than ^N occurrences of '/
     :        /'the supplied keyword ^KEY in the FITS header.', STATUS )
         END IF
      END IF

      END
