      SUBROUTINE FTS1_INKEY( NOLDCA, NKEY, NAMES, PSTNS, FTSCAR,
     :                       ACTNUM, IARY1, IARY2, CARY, STATUS )
*+
*  Name:
*     FTS1_INKEY

*  Purpose:
*     Inserts keywords to a FITS card array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_INKEY( NOLDCA, NKEY, NAMES, PSTNS, FTSCAR,
*     :                ACTNUM, IARY1, IARY2, CARY, STATUS )

*  Description:
*     This subroutine inserts a number of keyword cards into a
*     FITS-header array just before some given keywords, and returns
*     the position of these newly inserted cards in the FITS array
*     after insertion.  If a given position keyword is not in the FITS
*     header or is blank, its corresponding keyword card will be
*     inserted just before the end card or appended to the present FITS
*     array when end card does not exist.  If a keyword card to be
*     inserted already exists in the FITS array, that card will
*     be deleted and moved to the specified position.  For those new
*     keyword cards, an equals sign '=' will be put at the 9th column
*     for simple keyword or immediately after keywords for compound
*     ones; and a character value '{undefined}' will be given to them.
*
*     The following reserved keywords are not modified: SIMPLE, BITPIX,
*     NAXIS, NAXISn, EXTEND, PCOUNT, GCOUNT, XTENSION, BLOCKED, and END.

*  Arguments:
*     NOLDCA = INTEGER (Given)
*        Number of cards in the original FITS array.
*     NKEY = INTEGER (Given)
*        The number of keyword cards to be inserted into the FITS-header
*        array.
*     NAMES( NKEY ) = CHARACTER * ( * ) (Given)
*        The keywords to be inserted into FITS card array.
*     PSTNS( NKEY ) = CHARACTER * ( * ) (Given)
*        The position keyword names, before them the new keywords is
*        inserted.  If any name in it does not exit in FITS array, its
*        corresponding keyword will be inserted just before the end card
*        or appended to FITS array when end card does not exist.  If two
*        or more new cards have the same position name, they will all be
*        put just before the position name in the same order as they are
*        in NAMES.
*     FTSCAR( NOLDCA + NKEY ) = CHARACTER * ( * ) (Given and Returned)
*        On entry its first NOLDCA elements hold the original FITS
*        cards. On exit, its first ACTNUM elements hold the FITS cards
*        after the insersion.
*     ACTNUM = INTEGER (Returned)
*        The actual number of cards in the FITS array after inserting.
*     IARY1( NOLDCA + NKEY ) = INTEGER (Returned)
*        The first temporary working space.
*     IARY2( NOLDCA + NKEY ) = INTEGER (Returned)
*        The second temporary working space.
*     CARY( NOLDCA + NKEY ) = CHARACTER * ( * ) (Returned)
*        A temporary working space.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991, 1994 Science & Engineering Research Council.
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
*     9-OCT-1991 (WG):
*        Original version.
*     1994 July 16 (MJC):
*        Renamed and some tidying.  Removed violations of Fortran
*        standard appending character strings.  Fixed bugs.  Used
*        lengths in FTS_PAR file rather than recomputing them for
*        every keyword.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NOLDCA
      INTEGER NKEY
      CHARACTER * ( * ) NAMES( NKEY )
      CHARACTER * ( * ) PSTNS( NKEY )

*  Arguments Given and Returned:
      CHARACTER * ( * ) FTSCAR( NOLDCA + NKEY )

*  Arguments Returned:
      INTEGER ACTNUM
      INTEGER IARY1( NOLDCA + NKEY )
      INTEGER IARY2( NOLDCA + NKEY )
      CHARACTER * ( * ) CARY( NOLDCA + NKEY )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:
      INTEGER HKEYLN             ! Maximum number of characters in a
                                 ! FITS header card hierarchical keyword
      PARAMETER ( HKEYLN = 48 )

*  Local Variables:
      INTEGER CARD               ! A card No.
      INTEGER CHAIN1             ! Begin position of Chain 1
      INTEGER CHAIN2             ! Begin position of Chain 2
      LOGICAL CMPKEY             ! Compound keyword flag
      LOGICAL CMPPST             ! Compound position keyword flag
      INTEGER CPOS               ! Column position for appending text
      INTEGER ENDCAR             ! Card No. of End card
      INTEGER EQLPSN             ! Position of the equal sign
      LOGICAL HERE               ! Shows position name is in FITS array
      INTEGER I, J               ! Do loop index
      INTEGER KEYLN              ! Used length of KEYNAM
      CHARACTER * ( HKEYLN ) KEYNAM ! A keyword
      INTEGER LSTCAR             ! Last card No. of FITS array
      INTEGER NAMELN             ! Length of a keyword on a card
      INTEGER OLDLST             ! Last card No. of old FITS array
      INTEGER PSTLN              ! Used length of PSTNAM
      CHARACTER * ( HKEYLN ) PSTNAM ! A position keyword
      LOGICAL RELOC              ! Shows a card has been relocated
      LOGICAL RESVED             ! Shows a keyword is reserved
      LOGICAL THERE              ! Shows a new keyword is in FITS array
      INTEGER TMP                ! A temporary card no

*  Local Data:
      INCLUDE 'FTS_PAR'          ! FTS package constants and some
                                 ! declarations

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Search for the position of the end card in the original FITS array.
      ENDCAR = 0
      DO I = 1, NOLDCA
         IF ( FTSCAR( I )( : 8 ) .EQ. 'END' ) ENDCAR = I
      END DO

*  If there is a end-card in the original FITS array, the card before
*  it is the last FITS card.  Allow for the first card to be the END
*  card, say when this is the start of a new FITS extension.
      IF ( ENDCAR .NE. 0 ) THEN
         OLDLST = MAX( 1, ENDCAR - 1 )

*  Otherwise, the last element of FITS array is the last card.
      ELSE
         OLDLST = NOLDCA
      END IF

*  Set up two chains one of which links old FITS card from the first
*  card to the last card and the other from the last to the first. End
*  chain elements have value of 0.
      CHAIN1 = 1
      IARY1( 1 ) = 2
      IARY1( OLDLST ) = 0
      CHAIN2 = OLDLST
      IARY2( OLDLST ) = OLDLST - 1
      IARY2( 1 ) = 0
      DO I = 2, MAX( 2, OLDLST - 1 )
         IARY1( I ) = I + 1
         IARY2( I ) = I - 1
      END DO

*  Initially, last card is the original last card.
*  There is a special case when the first card is an END card.  In this
*  case we want to write the new cards from the first card, and append
*  the END after them.
      IF ( ENDCAR .EQ. 1 ) THEN
         LSTCAR = 0
      ELSE
         LSTCAR = OLDLST
      END IF

*  Process new keywords one by one.
      DO I = 1, NKEY

*  Convert it and its position name to its formal format.
         KEYNAM = NAMES( I )

*  Remove all blanks and convert it to upper case.
         CALL CHR_RMBLK( KEYNAM )
         CALL CHR_UCASE( KEYNAM )
         KEYLN = CHR_LEN( KEYNAM )

*  If the keyword is compound one, replace all '.' in the name with
*  blanks.
         CMPKEY = INDEX( KEYNAM( : NAMELN ), '.' ) .NE. 0
         IF ( CMPKEY ) THEN
            DO J = 1, KEYLN
               IF ( KEYNAM( J : J ) .EQ. '.' ) KEYNAM( J : J ) = ' '
            END DO
         END IF

*  Check whether this keyword is a reserved one.  The names and lengths
*  of the reserved keywords are stored in the FTS_PAR include file.
         RESVED = .FALSE.
         DO J = 1, FTS__NREKY
            IF ( KEYNAM( : KEYLN ) .EQ.
     :           FTS__REKEY( J )( : FTS__RKLEN( J ) ) ) RESVED = .TRUE.
         END DO

*  If the keyword is not blank, or reserved, process it.
         IF ( ( KEYLN .NE. 0 ) .AND. ( .NOT. RESVED ) ) THEN

*  Do the same for the position name.
            PSTNAM = PSTNS( I )
            CALL CHR_RMBLK( PSTNAM )
            CALL CHR_UCASE( PSTNAM )
            PSTLN = CHR_LEN( PSTNAM )
            CMPPST = INDEX( PSTNAM( : PSTLN ), '.' ) .NE. 0
            IF ( CMPPST ) THEN
               DO J = 1, PSTLN
                  IF ( PSTNAM( J : J ) .EQ. '.' ) PSTNAM( J : J ) = ' '
               END DO
            END IF

*  Append the new keyword at the end of the array.
            LSTCAR = LSTCAR + 1

*  If the position keyword is not blank, or 'END', see where the new
*  keyword should insert.
            CARD = 0
            HERE = .FALSE.
            IF ( PSTNAM( : PSTLN ) .NE. 'END' .AND. PSTLN .NE. 0 ) THEN

*  If the position keyword is simple, ...
               IF ( .NOT. CMPPST ) THEN
                  DO WHILE ( .NOT. HERE .AND. CARD .LT. OLDLST )
                     CARD = CARD + 1
                     NAMELN = CHR_LEN( FTSCAR( CARD )( : 8 ) )
                     IF ( PSTNAM( : PSTLN ) .EQ.
     :                    FTSCAR( CARD )( : NAMELN ) ) HERE = .TRUE.
                  END DO

*  If the position keyword is compound, ...
               ELSE
                  DO WHILE ( ( .NOT. HERE ) .AND. ( CARD .LT. OLDLST ) )
                     CARD = CARD + 1

*  Find the position of the equals sign '='.
                     EQLPSN = INDEX( FTSCAR( CARD ), '=' )

*  Only consider those cards containing an equals sign.
                     IF ( EQLPSN .NE. 0 ) THEN
                        NAMELN = CHR_LEN( FTSCAR( CARD )( :EQLPSN -1 ) )
                        IF ( PSTNAM( : PSTLN ) .EQ.
     :                     FTSCAR( CARD )( : NAMELN ) ) HERE = .TRUE.
                     END IF
                  END DO
               END IF
            END IF

*  If the card is to be inserted in the middle of the old FITS card
*  array, change the chains at the position to include it.
            IF ( HERE .AND. CARD .NE. CHAIN1 ) THEN
               IARY1( IARY2( CARD ) ) = LSTCAR
               IARY1( LSTCAR ) = CARD
               TMP = IARY2( CARD )
               IARY2( CARD ) = LSTCAR
               IARY2( LSTCAR ) = TMP

*  Or if the card is to be inserted at the beginning of FITS array, ...
            ELSE IF ( HERE .AND. CARD .EQ. CHAIN1 ) THEN
               CHAIN1 = LSTCAR
               IARY1( LSTCAR ) = CARD
               IARY2( CARD ) = LSTCAR
               IARY2( LSTCAR ) = 0

*  Or if the card is to be appended at the end of FITS array, ...
            ELSE IF ( .NOT. HERE ) THEN
               IARY1( CHAIN2 ) = LSTCAR
               IARY1( LSTCAR ) = 0
               IARY2( LSTCAR ) = CHAIN2
               CHAIN2 = LSTCAR
            END IF

*  Check whether the new keyword already exists in the original FITS
*  array. If so set the flag.
            CARD = 0
            THERE = .FALSE.

*  If the new key is not compound, ...
            IF ( .NOT. CMPKEY ) THEN
               DO WHILE( .NOT. THERE .AND. CARD .LT. OLDLST )
                  CARD = CARD + 1
                  NAMELN = CHR_LEN( FTSCAR( CARD )( : 8 ) )
                  IF ( KEYNAM( : KEYLN ) .EQ.
     :                 FTSCAR( CARD )( : NAMELN ) ) THERE = .TRUE.
               END DO

*  If the new key is compound, ...
            ELSE
               DO WHILE( .NOT. THERE .AND. CARD .LT. OLDLST )
                  CARD = CARD + 1

*  Find the position of the equals sign '='.
                  EQLPSN = INDEX( FTSCAR( CARD ), '=' )

*  Only consider those cards containing an equals sign.
                  IF ( EQLPSN .NE. 0 ) THEN
                     NAMELN = CHR_LEN( FTSCAR( CARD )( : EQLPSN - 1 ) )
                     IF ( KEYNAM( : KEYLN ) .EQ.
     :                    FTSCAR( CARD )( : NAMELN ) ) THERE = .TRUE.
                  END IF
               END DO
            END IF

*  If it is in the original FITS array, assign the value of the old
*  card to the new card.
            IF ( THERE ) THEN
               FTSCAR( LSTCAR ) = FTSCAR( CARD )

*  Determine the start position (less one column) of the text to append.
               IF ( .NOT. CMPKEY ) THEN
                  CPOS = 8
               ELSE
                  CPOS = EQLPSN
               END IF

*  Append the relocated token.
               CALL CHR_APPND( '= ''{relocated}''', FTSCAR( CARD ),
     :                         CPOS )

*  This a new keyword.
            ELSE

*  Determine the start position (less one column) of the text to append.
*  Give this new keyword a character value of '{undefined}'.
               IF ( .NOT. CMPKEY ) THEN
                  FTSCAR( LSTCAR ) = KEYNAM( : 8 )//'= ''{undefined}'''
               ELSE
                  FTSCAR( LSTCAR ) = KEYNAM( : KEYLN )/
     :                              /'= ''{undefined}'''
               END IF
            END IF
         END IF

*  Go back to process the next keyword.
      END DO

*  Remove the old cards having value '{relocated}' from the chains.
      DO CARD = 1, OLDLST
         EQLPSN = INDEX( FTSCAR( CARD ), '=' )
         IF ( EQLPSN .NE. 0 ) THEN
            RELOC = INDEX( FTSCAR( CARD ), '{relocated}' ) .NE. 0
            IF ( RELOC ) THEN

*  If the old card is in the middle of the chains, ...
               IF ( CARD .NE. CHAIN1 .AND. CARD .NE. CHAIN2 ) THEN
                  IARY1( IARY2( CARD ) ) = IARY1( CARD )
                  IARY2( IARY1( CARD ) ) = IARY2( CARD )

*  If the old card is at the beginning of the chain 1 (end of chain 2),
*  take the next one in the chain as the begin card for chain 1 and as
*  the end card for chain 2.
               ELSE IF ( CARD .EQ. CHAIN1 ) THEN
                  CHAIN1 = IARY1( CARD )
                  IARY2( IARY1( CARD ) ) = 0

*  If the old card is at the end of chain 1 (beginning of chain 2),
*  take the card before as the end card for chain 1 and as the begin
*  card for chain 2.
               ELSE IF ( CARD .EQ. CHAIN2 ) THEN
                  IARY1( IARY2( CARD ) ) = 0
                  CHAIN2 = IARY1( CARD )
               END IF
            END IF
         END IF
      END DO

*  Copy the cards to the temporary character work space in the order
*  given by chain 1.
      ACTNUM = 0
      CARD = CHAIN1
      DO WHILE( CARD .NE. 0 )
         ACTNUM = ACTNUM + 1
         CARY( ACTNUM ) = FTSCAR( CARD )
         CARD = IARY1( CARD )
      END DO

*  Copy the cards from the temporary character work space back to FITS
*  array.
      DO CARD = 1, ACTNUM
         FTSCAR( CARD ) = CARY( CARD )
      END DO

*  If there is an END-card originally, append an END-card.
      IF ( ENDCAR .NE. 0 ) THEN
         ACTNUM = ACTNUM + 1
         FTSCAR( ACTNUM ) = 'END'
      END IF

      END
