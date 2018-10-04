      SUBROUTINE FTS1_EDKEY( NOCARD, NKEY, MXCARD, EDITS, NAMES, PSTNS,
     :                       KOCCUR, POCCUR, VALUES, COMNTS, TYPES,
     :                       FTSCAR, ACTNUM, IARY1, IARY2, CARY,
     :                       EXISTS, STATUS )
*+
*  Name:
*     FTS1_EDKEY

*  Purpose:
*     Edits keywords in a FITS card array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_EDKEY( NOCARD, NKEY, MXCARD, EDITS, NAMES, PSTNS,
*                      KOCCUR, POCCUR, VALUES, COMNTS, TYPES, FTSCAR,
*                      ACTNUM, IARY1, IARY2, CARY, EXISTS, STATUS )

*  Description:
*     This subroutine inserts, updates, moves, deletes, reports, and
*     tests the existence of a number of keyword cards in a FITS-header
*     array.  The occurrence of keywords may be defined, when there are
*     more than one cards of the same name.  The location of each
*     insertion or move is immediately before some occurrence of a
*     corresponding keyword.  The routine returns the modified
*     FITS-header array.

*  Arguments:
*     NOCARD = INTEGER (Given)
*        Number of cards in the original FITS array.
*     NKEY = INTEGER (Given)
*        The number of keyword cards to be inserted into the FITS-header
*        array.
*     MXCARD = INTEGER (Given)
*        Maximum number of cards in the resultant FITS array.  This must
*        be not less than NOCARD + NKEY + 1.
*     EDITS( MAXMOD ) = CHARACTER * ( * ) (Returned)
*        The editing commands.  Allowed values are 'Amend', 'Delete',
*        'Exist', 'Move', 'Rename', 'Print', 'Update', and 'Write'
*        (case sensitive). Each element can be abbreviated to the
*        initial letter.
*
*        'Amend' acts as 'Update' if the keyword exists, but as 'Write'
*        if the keyword is absent.
*
*        'Delete' removes a named keyword.
*
*        'Exist' reports TRUE to standard output if the named keyword
*        exists in the header, and FALSE if the keyword is not present.

*        'Move' relocates a named keyword to be immediately before a
*        second keyword.  When this positional keyword is not supplied,
*        it defaults to the END card, and if the END card is absent,
*        the new location is at the end of the headers.
*
*        'Null' nullifies the value of the named keyword.  Spaces
*        substitute the keyword's value.
*
*        'Print' causes the value of a named keyword to be displayed to
*        standard output.  This will be a blank for a comment card.
*
*        'Rename' renames a keyword, using the value as the new keyword.
*
*        'Update' revises the value and/or the comment.  If a secondary
*        keyword is defined explicitly, the card may be relocated at
*        the same time.  If the secondary keyword does not exist, the
*        card being edited is not moved.  Update requires that the
*        keyword being edited exists.

*        'Write' creates a new card given a value and an optional
*        comment.  Its location uses the same rules as for the Move
*        command.
*     NAMES( NKEY ) = CHARACTER * ( * ) (Given)
*        The names of the keywords to be edited in the FITS card
*        array.  A name may be compound to handle hierarchical
*        keywords, and it has the form keyword1.keyword2.keyword3 etc.
*        The maximum number of keywords per FITS card is twenty.  Each
*        keyword must be no longer than eight characters, and be a valid
*        FITS keyword comprising alphanumeric characters, hyphen, and
*        underscore.  Any lowercase letters are converted to uppercase
*        and blanks are removed before inserted or comparison with the
*        existing keywords.
*
*        The keywords ' ', 'COMMENT', and 'HISTORY' are comment cards
*        and do not have a value.
*
*        The keyword must exist except for the Amend, Write, and Exist
*        commands.
*     PSTNS( NKEY ) = CHARACTER * ( * ) (Given)
*        The position keyword names.  A position name may be compound
*        to handle hierarchical keywords, and it has the form
*        keyword1.keyword2.keyword3 etc.  The maximum number of
*        keywords per FITS card is twenty.  Each keyword must be no
*        longer than eight characters.  When locating the position card,
*        comparisons are made in uppercase and with the blanks removed.

*        The new keywords are inserted immediately before each
*        corresponding position keyword.  If any name in it does not
*        exist in FITS array, the consequences will be as follows.  In
*        the Write, Amend (new keyword), and Move edits its
*        corresponding keyword will be inserted just before the END
*        card or appended to FITS array when the END card does not
*        exist; however, the card is not relocated for an Update or
*        Amend (with an existing keyword) edit.  If two or more new
*        cards have the same position name, they will all be put just
*        before the position name in the same order as they are in
*        NAMES.
*
*        A positional keyword is used by the Move, Write, Amend, and
*        Update editing commands.
*     KOCCUR( NKEY ) = INTEGER (Given)
*        The occurrences of the NAMES keywords to use.  Values less than
*        or equal to 1 will manipulate the first occurrence of the
*        keyword to insert.
*     POCCUR( NKEY ) = INTEGER (Given)
*        The occurrences of the PSTNS keywords to use.  Values less than
*        or equal to 1 will situate the inserted keyword immediately
*        before the first occurrence of the positional keyword.
*     VALUES( NKEY ) = CHARACTER * ( * ) (Given)
*        The new values of the NAMES keywords for the Update and Write
*        editing commands.  The special value '$V' means use the
*        current value of the NAMES keyword.  This makes it possible to
*        modify a comment, leaving the value unaltered.  In addition
*        $V(keyword) requests that the value of the keyword given
*        between the parentheses be assigned to the keyword being
*        edited.  This positional keyword must exist and have a value
*        for a Write edit; whereas the FITS-header value is unchanged
*        for Update if there are problems with this positional keyword.
*        edited.
*
*        For a Rename edit, VALUES has a different meaning; in this
*        case it stores the replacement keyword name.
*     COMNTS( NKEY ) = CHARACTER * ( * ) (Given)
*        The comments to be written to the NAMES keywords for the Amend,
*        Update, and Write editing commands.  The special value '$C'
*        means use the current comment.  In addition $C(keyword)
*        requests that the comment of the keyword given between the
*        parentheses be assigned to the keyword being edited.  If this
*        positional keyword does not exist the comment is unchanged for
*        Update, and is blank for a Write edit.
*     TYPES( NKEY ) = CHARACTER * ( * ) (Given)
*        The data types of the values to Write or Update.  This does
*        allow some numeric or logical values to be written as strings.
*        These will be one of the following: '_CHAR',  '_DOUBLE',
*        '_INTEGER', '_LOGICAL', '_REAL'.  In addition there are two
*        special values: 'COMMENT' to indicate that the card is a
*        comment (so strictly it has no type), and ' ' to indicate that
*        the data type is unknown, as occurs for a value defined by a
*        reference keyword.  The length should be at least eight
*        characters.
*     FTSCAR( MXCARD ) = CHARACTER * ( * ) (Given and Returned)
*        On entry its first NOCARD elements hold the original FITS
*        cards. On exit, its first ACTNUM elements hold the FITS cards
*        after the insersion.
*     ACTNUM = INTEGER (Returned)
*        The actual number of cards in the FITS array after inserting.
*     IARY1( MXCARD ) = INTEGER (Returned)
*        The first temporary working space.
*     IARY2( MXCARD ) = INTEGER (Returned)
*        The second temporary working space.
*     CARY( MXCARD ) = CHARACTER * ( * ) (Returned)
*        A temporary working space.
*     EXISTS = LOGICAL (Returned)
*        The result of the last "Exist" operation.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  When an error occurs during editing, warning messages are sent
*     at the normal reporting level, and processing continues to the
*     next editing command.
*     -  The FITS fixed format is used for writing or updating
*     headers, except for double-precision values requiring more space.
*     The comment is delineated from the value by the string ' / '.
*     -  The comments in comment cards begin one space following the
*     keyword or from column 10 whichever is greater.
*     -  At present the following reserved keywords are neither
*     modifiable nor movable: SIMPLE, BITPIX, NAXIS, NAXISn, EXTEND,
*     PCOUNT, GCOUNT, XTENSION, BLOCKED, and END.  This is because
*     order in the extension should be fixed and should not be
*     changed by any routine.  There is one exception: an END keyword
*     may be appended if one does not exist.

*  Copyright:
*     Copyright (C) 1996, 2000 Central Laboratory of the Research
*                   Councils.
*     Copyright (C) 2008, 2009, 2011, 2015 Science and Technology
*                   Facilties Council.
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
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1996 November 1 (MJC):
*        Original version based upon FTS1_INKEY.
*     2000 July 26 (MJC):
*        Do not adjust linked lists for 'Move' and 'Update' when the
*        card is already at the correct position.  Remove unused
*        variables.
*     2008 June 14 (MJC):
*        Add Amend hybrid option.
*     2009 January 11 (MJC):
*        Add Null option.
*     2009 January 19 (MJC):
*        Description modified for Null option retaining the Value
*        Indicator.
*     22-JUN-2011 (DSB):
*        Allow read-only operations to be performed without needing
*        write-access to anything.
*     2015 April 22 (MJC):
*        Permit an END keyword to be written at the end if one does not
*        already exist.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'MSG_PAR'          ! MSG__ constants

*  Arguments Given:
      INTEGER NOCARD
      INTEGER NKEY
      INTEGER MXCARD
      CHARACTER * ( * ) EDITS( NKEY )
      CHARACTER * ( * ) NAMES( NKEY )
      CHARACTER * ( * ) PSTNS( NKEY )
      INTEGER KOCCUR( NKEY )
      INTEGER POCCUR( NKEY )
      CHARACTER * ( * ) VALUES( NKEY )
      CHARACTER * ( * ) COMNTS( NKEY )
      CHARACTER * ( * ) TYPES( NKEY )

*  Arguments Given and Returned:
      CHARACTER * ( * ) FTSCAR( MXCARD )

*  Arguments Returned:
      INTEGER ACTNUM
      INTEGER IARY1( MXCARD )
      INTEGER IARY2( MXCARD )
      CHARACTER * ( * ) CARY( MXCARD )
      LOGICAL EXISTS

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string
      CHARACTER * ( 2 ) CHR_NTH  ! Ordinal abbreviation

*  Local Constants:
      INTEGER KEYLEN             ! Maximum number of characters in a
                                 ! FITS header card keyword or
                                 ! hierarchical component thereof
      PARAMETER ( KEYLEN = 8 )

      INTEGER HKEYLN             ! Maximum number of characters in a
                                 ! FITS header card hierarchical keyword
      PARAMETER ( HKEYLN = 48 )

      INTEGER VALLN              ! Maximum number of characters in a
                                 ! FITS header card value
      PARAMETER ( VALLN = 68 )

*  Local Variables:
      LOGICAL ADDEND             ! Write an END card requested?
      INTEGER CARD               ! A card number
      INTEGER CCARD              ! Number of reference-comment card
      INTEGER CDELIM             ! Character pointer to a delimiter
      INTEGER CHAIN1             ! Begin position of Chain 1
      INTEGER CHAIN2             ! Begin position of Chain 2
      INTEGER CLPPOS             ! Column of comment reference keyword
                                 ! left parenthesis
      LOGICAL CMPKEY             ! Compound keyword?
      LOGICAL COMCRD             ! Comment keyword?
      CHARACTER * ( 70 ) COMENT  ! FITS comment to write
      CHARACTER * ( VALLN ) CVALUE ! Value (as string)
      INTEGER DCARD              ! Dummy card number
      DOUBLE PRECISION DVALUE    ! FITS value
      CHARACTER * ( 1 ) EDIT     ! Edit command abbreviation
      INTEGER ENDCAR             ! Card number of END card
      INTEGER I                  ! Do-loop index
      LOGICAL INTTYP             ! One of the integer data types?
      INTEGER IVALUE             ! FITS value
      INTEGER J                  ! Do-loop index
      INTEGER KEYLN              ! Used length of KEYNAM
      CHARACTER * ( HKEYLN ) KEYNAM ! A keyword
      CHARACTER * ( HKEYLN ) KEYREC ! FITS comment reference keyword
                                 ! (can be hierarchical)
      CHARACTER * ( HKEYLN ) KEYREV ! FITS value reference keyword (can
                                 ! be hierarchical)
      INTEGER LREFCK             ! Length of the comment reference
                                 ! keyword
      INTEGER LREFVK             ! Length of the value reference keyword
      INTEGER LSTCAR             ! Last card number of FITS array
      LOGICAL LVALUE             ! FITS value
      INTEGER MXCLIM             ! Lowest allowed value for MXCARD
      CHARACTER * ( HKEYLN ) NEWKEY ! Replacement keyword
      INTEGER OLDLST             ! Last card number of old FITS array
      INTEGER PCARD              ! Number of positional card
      LOGICAL PEXIST             ! Shows position name is in FITS array
      INTEGER PSTLN              ! Used length of PSTNAM
      LOGICAL PRESNT             ! Named card is present? (dummy)
      CHARACTER * ( HKEYLN ) PSTNAM ! A position keyword
      CHARACTER * ( 70 ) RCOMNT  ! FITS comment of reference-value card
      LOGICAL RDONLY             ! Are all edits read-only?
      INTEGER RECOC              ! Reference-comment-keyword occurrence
                                 ! number
      LOGICAL REFCOM             ! Value contains comment reference
                                 ! keyword?
      LOGICAL REFVAL             ! Value contains value reference
                                 ! keyword?
      LOGICAL RESVED             ! Shows a keyword is reserved
      INTEGER REVOC              ! Reference-value-keyword occurrence
                                 ! number
      REAL RVALUE                ! FITS value
      LOGICAL TERMIN8            ! Write a persistent END card?
      LOGICAL THERE              ! Does the keyword exist?
      INTEGER TMP                ! Temporary card number in chain1
      INTEGER TMP2               ! Temporary card number in chain2
      CHARACTER * ( DAT__SZTYP ) TYPE ! Data type of the value to be
                                 ! written
      CHARACTER * ( VALLN ) VALUE ! String value
      INTEGER VCARD              ! Number of reference-value card
      INTEGER VLPPOS             ! Column of value reference keyword
                                 ! left parenthesis
      LOGICAL WREND              ! END card present?
      LOGICAL WRKEY              ! Write a new keyword (W or A modes)
      LOGICAL UPKEY              ! Update a keyword (U or A modes)

*  Local Data:
      INCLUDE 'FTS_PAR'          ! FTS package constants and some
                                 ! declarations

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set a flag indicating if none of the edits change anything.
      RDONLY = .TRUE.
      DO I = 1, NKEY
         IF( EDITS( I )( 1 : 1 ) .NE. 'E' .AND.
     :       EDITS( I )( 1 : 1 ) .NE. 'P' ) RDONLY = .FALSE.
      END DO

*  Validate the input dimensions.
      IF( RDONLY ) THEN
         MXCLIM = NOCARD
      ELSE
         MXCLIM = NOCARD + NKEY + 1
      END IF

      IF ( MXCARD .LT. MXCLIM . OR. NOCARD .LT. 1 .OR.
     :     NKEY .LT. 1 ) THEN
         CALL MSG_SETI( 'M', NOCARD )
         CALL MSG_SETI( 'N', NKEY )
         CALL MSG_SETI( 'D', MXCARD )
         CALL ERR_REP( 'FTS1_EDKEY',
     :     'The number of cards (^M), or edits (^N), or the workspace '/
     :     /'dimension (^D) is invalid. (Probable programmming error.)',
     :     STATUS )

         RETURN
      END IF

*  Locate the last card.
*  =====================

*  Search for the position of the end card in the original FITS array.
      ENDCAR = 0
      DO I = 1, NOCARD
         IF ( FTSCAR( I )( :KEYLEN ) .EQ. 'END' ) ENDCAR = I
      END DO
      TERMIN8 = .FALSE.

*  If there is a end-card in the original FITS array, the card before
*  it is the last FITS card.  Allow for the first card to be the END
*  card, say when this is the start of a new FITS extension.  OLDLST
*  is used to define the limits of the linked list.  We want the END
*  card, actual or virtual to be the insertion point for new cards.
      WREND = ENDCAR .NE. 0
      IF ( WREND ) THEN
         OLDLST = ENDCAR

*  Otherwise, the last element of FITS array is the last card.
      ELSE IF( RDONLY ) THEN
         OLDLST = NOCARD
         ENDCAR = OLDLST

      ELSE
         OLDLST = NOCARD + 1
         ENDCAR = OLDLST
         FTSCAR( ENDCAR ) = 'END '

      END IF

*  Initially, last card is the original last card.  The last card to
*  write is immediately before the virtual END card.  An actual END
*  card may be written upon completion of the editing.
      LSTCAR = OLDLST

*  Initialise linked lists.
*  ========================

*  Set up two chains one of which links old FITS card from the first
*  card to the last card and the other from the last to the first.  End
*  chain elements have value of 0.  Two lists are needed because a
*  single list only allows insertion after a position, and we want to
*  insert before a card.
      CHAIN1 = 1
      IARY1( 1 ) = 2
      IARY1( OLDLST ) = 0
      CHAIN2 = OLDLST
      IARY2( OLDLST ) = OLDLST - 1
      IARY2( 1 ) = 0
      IF ( OLDLST .GT. 2 ) THEN
         DO I = 2, MAX( 2, OLDLST - 1 )
            IARY1( I ) = I + 1
            IARY2( I ) = I - 1
         END DO
      END IF

*  Main loop through the array of edits.
*  =====================================

*  Process the editing commands one by one.
      DO I = 1, NKEY
         EDIT = EDITS( I )

*  Prepare the keyword name.
*  =========================

*  Convert it and its position name to its formal format.
         KEYNAM = NAMES( I )

*  Remove all blanks and convert it to upper case.
         CALL CHR_RMBLK( KEYNAM )
         CALL CHR_UCASE( KEYNAM )
         KEYLN = CHR_LEN( KEYNAM )
         IF ( KEYLN .EQ. 0 ) KEYLN = KEYLEN

*  Determine whether or not the keyword is compound.
         CMPKEY = INDEX( KEYNAM( :KEYLN ), '.' ) .NE. 0

*  Test for reserved keywords.
*  ===========================

*  Check whether this keyword is a reserved one.  The names and lengths
*  of the reserved keywords are stored in the FTS_PAR include file.
         RESVED = .FALSE.
         DO J = 1, FTS__NREKY
            IF ( KEYNAM( :KEYLN ) .EQ.
     :           FTS__REKEY( J )( :FTS__RKLEN( J ) ) ) RESVED = .TRUE.
         END DO

*  Permit one exception to the reserved keywords, namely allow an END
*  header to terminate the headers if one does not already exist.
         ADDEND = ( KEYNAM .EQ. 'END' .AND. EDIT .EQ. 'W' ) .AND.
     :              .NOT. WREND
         TERMIN8 = ADDEND .OR. TERMIN8

*  Do not need to write the END card.  It has been written if it
*  didn't xist and will be retained at the end of the edits.
         IF ( ADDEND ) GOTO 100

*  Cannot modify, move, delete, or write a reserved keyword.  Report a
*  warning message.  Decrement the card count for a new card that is no
*  longer going to be created.
         IF ( RESVED .AND.
     :        .NOT. ( EDIT .EQ. 'E' .OR. EDIT .EQ. 'P' ) ) THEN

            CALL MSG_SETC( 'E', EDIT )
            CALL MSG_SETC( 'KEY', KEYNAM )
            CALL MSG_OUTIF( MSG__QUIET, 'RESERVED',
     :        'Cannot ^E the reserved keyword ^KEY.', STATUS )

            GO TO 100
         END IF

*  Locate the editing keyword.
*  ===========================

*  A new card does not need locating.  By definition it will be after
*  the current last card in the array (though its effective position
*  may be different depending on the positional keyword and the
*  ordering of the linked lists).
         IF ( EDIT .EQ. 'W' ) THEN
            CARD = LSTCAR + 1
         ELSE

*  Check whether the new keyword already exists in the original FITS
*  array.  If so set the flag.
            THERE = .FALSE.

*  Find the desired occurrence of the keyword, by searching by name,
*  then by occurrence.  Record the fact that the positional keyword has
*  been located.  A bad status indicates that the keyword was not found.
*  Annul the error message in this case.
            CALL ERR_MARK
            CALL FTS1_LOKEY( LSTCAR, FTSCAR, KEYNAM,
     :                       MAX( KOCCUR( I ), 1 ), CARD, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_ANNUL( STATUS )
               CARD = LSTCAR
            ELSE
               THERE = .TRUE.
            END IF
            CALL ERR_RLSE
         END IF

*  The card must exist except for the Write, Amend, and Exist commands.
*  Just output a message so that the task can continue, and indeed the
*  warning can be suppressed.
         IF ( EDIT .NE. 'W' .AND. EDIT .NE. 'E' .AND.
     :        EDIT .NE. 'A' .AND. .NOT. THERE ) THEN
            CALL MSG_SETC( 'E', EDITS( I ) )
            CALL MSG_SETC( 'KEY', KEYNAM )
            IF ( KOCCUR( I ) .GT. 1 ) THEN
               CALL MSG_SETI( 'K', KOCCUR( I ) )
               CALL MSG_SETC( 'TH', CHR_NTH( KOCCUR( I ) ) )
               CALL MSG_OUTIF( MSG__NORM, 'MISSING',
     :           'The ^K^TH occurrence of keyword ^KEY does not '/
     :           /'exist.  Therefore, unable to execute the ^E '/
     :           /'command.', STATUS )
            ELSE
               CALL MSG_OUTIF( MSG__NORM, 'MISSING',
     :           'The keyword ^KEY does not exist.  Therefore, unable '/
     :           /'to execute the ^E command.', STATUS )
            END IF
            GO TO 100
         END IF

*  Decide whether or not we need to write a new keyword.  If the keyword
*  is absent for anm Amend edit, set the card index as if it were a
*  Write edit.
         WRKEY = EDIT .EQ. 'W' .OR. ( EDIT .EQ. 'A' .AND. .NOT. THERE )
         IF ( WRKEY ) CARD = LSTCAR + 1

*  Decide whether or not we need to update a keyword.
         UPKEY = EDIT .EQ. 'U' .OR. ( EDIT .EQ. 'A' .AND. THERE )

*  Prepare the positional keyword name.
*  ====================================

*  Only need to do this for certain editing commands.
         IF ( EDIT .EQ. 'M' .OR. EDIT .EQ. 'W' .OR.
     :        EDIT .EQ. 'A' .OR. EDIT .EQ. 'U' ) THEN

*  Strip leading blanks from the positional keyword, convert to
*  uppercase, and find its effective length.
            PSTNAM = PSTNS( I )
            CALL CHR_RMBLK( PSTNAM )
            CALL CHR_UCASE( PSTNAM )
            PSTLN = CHR_LEN( PSTNAM )

*  Locate the positional keyword.
*  ==============================

*  A blank positional keyword for the update edit means leave the
*  card unmoved.  FITSMOD and FTS1_RFMOD do not know whether the
*  keyword exists, so a null position keyword is set to END even for
*  Amend.  Here the existence of the keyword is known, and so the END
*  card can be replaced with a blank positional keyword since the
*  Amend wants to Update the header in situ.
            IF ( ( PSTNAM( :PSTLN ) .EQ. 'END' .AND. EDIT .EQ. 'A' .AND.
     :             THERE ) .OR.
     :             PSTNAM( :PSTLN ) .EQ. ' ' .AND. UPKEY ) THEN
               PCARD = CARD

*  If the position keyword is 'END' or blank, insert the card just
*  before the (virtual) END card.
            ELSE IF ( PSTNAM( :PSTLN ) .EQ. 'END' ) THEN
               PCARD = ENDCAR

            ELSE
               PEXIST = .FALSE.

*  Find the desired occurrence of the positional keyword, by searching
*  by name, then by occurrence.  Record the fact that the positional
*  keyword has been located.  A bad status indicates that the keyword
*  was not found.  Annul the error message in this case.
               CALL ERR_MARK
               CALL FTS1_LOKEY( LSTCAR, FTSCAR, PSTNAM,
     :                          MAX( POCCUR( I ), 1 ), PCARD, STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_ANNUL( STATUS )
               ELSE
                  PEXIST = .TRUE.
               END IF
               CALL ERR_RLSE

*  Deal with the case when the positional card was not found.  Issue a
*  warning, but continue with the editing for the Updata command, or
*  the next edit command for Move and Write instructions.   Decrement
*  the last card if it is no longer going to be used.
               IF ( .NOT. PEXIST ) THEN
                  CALL MSG_SETC( 'KEY', PSTNAM )
                  IF ( POCCUR( I ) .GT. 1 ) THEN
                     CALL MSG_SETI( 'P', POCCUR( I ) )
                     CALL MSG_SETC( 'TH', CHR_NTH( POCCUR( I ) ) )
                     CALL MSG_OUTIF( MSG__NORM, 'PMISSING',
     :                 'The ^P^TH occurrence of positional keyword '/
     :                 /'^KEY does not exist.', STATUS )
                  ELSE
                     CALL MSG_OUTIF( MSG__NORM, 'PMISSING',
     :                 'The positional keyword ^KEY does not exist.',
     :                 STATUS )
                  END IF

                  IF ( .NOT. UPKEY ) THEN
                     CALL MSG_SETC( 'E', EDITS( I ) )
                     CALL MSG_OUTIF( MSG__NORM, 'PMISSING',
     :                 'Therefore, unable to execute the ^E command.',
     :                  STATUS )
                     GO TO 100
                  END IF
                  PCARD = CARD

               END IF

            END IF
         END IF

*  Perform the Exist command.
*  ==========================
         IF ( EDIT .EQ. 'E' ) THEN
            EXISTS = THERE
            CALL MSG_SETL( 'KEYX', THERE )
            CALL MSG_OUTIF( MSG__QUIET, 'EXIST', '^KEYX', STATUS )

*  Perform the Read command.
*  ==========================
         ELSE IF ( EDIT .EQ. 'P' ) THEN

*  Determine the type of the value to read.
            CALL FTS1_QTYPE( FTSCAR( CARD ), TYPE, STATUS )

*  Obtain the value of the reference card, using the appropriate data
*  type.  Convert it to a token for output.
            IF ( TYPE .EQ. '_CHAR' ) THEN
               CALL FTS1_GKEYC( 1, FTSCAR( CARD ), 1, KEYNAM, 1,
     :                          PRESNT, VALUE, COMENT, DCARD, STATUS )
               CALL MSG_SETC( 'VALUE', VALUE )

            ELSE IF ( TYPE .EQ. '_REAL' ) THEN
               CALL FTS1_GKEYR( 1, FTSCAR( CARD ), 1, KEYNAM, 1,
     :                          PRESNT, RVALUE, COMENT, DCARD, STATUS )
               CALL MSG_SETR( 'VALUE', RVALUE )

            ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
               CALL FTS1_GKEYI( 1, FTSCAR( CARD ), 1, KEYNAM, 1,
     :                          PRESNT, IVALUE, COMENT, DCARD, STATUS )
               CALL MSG_SETI( 'VALUE', IVALUE )

            ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
               CALL FTS1_GKEYD( 1, FTSCAR( CARD ), 1, KEYNAM, 1,
     :                          PRESNT, DVALUE, COMENT, DCARD, STATUS )
               CALL MSG_SETD( 'VALUE', DVALUE )

            ELSE IF ( TYPE .EQ. '_LOGICAL' ) THEN
               CALL FTS1_GKEYL( 1, FTSCAR( CARD ), 1, KEYNAM, 1,
     :                          PRESNT, LVALUE, COMENT, DCARD, STATUS )
               CALL MSG_SETL( 'VALUE', LVALUE )

            ELSE IF ( TYPE .EQ. 'COMMENT' ) THEN
               CALL MSG_SETC( 'VALUE', ' ' )

            END IF

*  Report the value.
            CALL MSG_OUTIF( MSG__QUIET, 'PRINT', '^VALUE', STATUS )


*  Perform the Delete command.
*  ===========================
         ELSE IF ( EDIT .EQ. 'D' ) THEN

*  Assign a dummy card to this card (with a distinctive invalid keyword
*  so that it will not be located by a subsequent command.
            FTSCAR( CARD ) = '******** '

*  Delete requires this line to be removed from the linked lists.
*  If the card is to be deleted from the middle of the old FITS card
*  array, change the chains at the position to exclude it.
            IF ( CARD .NE. CHAIN1 ) THEN
               TMP = IARY2( CARD )
               IARY1( TMP ) = IARY1( CARD )
               IARY2( IARY1( CARD ) ) = TMP

*  If the old card is at the beginning of the chain 1 (end of chain 2),
*  take the next one in the chain as the begin card for chain 1 and as
*  the end card for chain 2.
            ELSE IF ( CARD .EQ. CHAIN1 ) THEN
               CHAIN1 = IARY1( CARD )
               IARY2( CHAIN1 ) = 0

*  If the old card is at the end of chain 1 (beginning of chain 2),
*  take the card before as the end card for chain 1 and as the begin
*  card for chain 2.
            ELSE IF ( CARD .EQ. CHAIN2 ) THEN
               CHAIN2 = IARY2( CARD )
               IARY1( CHAIN2 ) = 0

*  Decrement the unwanted card.
               LSTCAR = LSTCAR - 1
            END IF

*  Perform the Move command.
*  =========================
         ELSE IF ( EDIT .EQ. 'M' ) THEN

*  No need to move if the card is already in the correct location.
*  Indeed it can duplicate links and isolate elements like an ox-bow
*  lake.
            IF ( PCARD .NE. IARY1( CARD ) ) THEN

*  Move just requires different linkages in the linked lists.  If the
*  card is to be moved from the middle of the old FITS card array,
*  change the chains at the position.  Use temporary values to prevent
*  the original values being lost until the relinkage is complete.
               IF ( CARD .NE. CHAIN1 .AND. CARD .NE. CHAIN2 ) THEN
                  TMP = IARY1( CARD )
                  TMP2 = IARY2( CARD )

                  IARY1( IARY2( PCARD ) ) = CARD
                  IARY1( TMP2 ) = TMP
                  IARY1( CARD ) = PCARD

                  IARY2( CARD ) = IARY2( PCARD )
                  IARY2( PCARD ) = CARD
                  IARY2( TMP ) = TMP2

*  If the old card is at the beginning of chain 1 (end of chain 2),
*  take the next one in the chain as the new start card for chain 1 and
*  as the end card for chain 2.
               ELSE IF ( CARD .EQ. CHAIN1 ) THEN
                  CHAIN1 = IARY1( CARD )
                  IARY2( CHAIN1 ) = 0

                  IARY1( IARY2( PCARD ) ) = CARD
                  IARY1( CARD ) = PCARD

                  IARY2( CARD ) = IARY2( PCARD )
                  IARY2( PCARD ) = CARD

*  If the old card is at the end of chain 1 (beginning of chain 2),
*  take the card before as the end card for chain 1 and as the start
*  card for chain 2.
               ELSE IF ( CARD .EQ. CHAIN2 ) THEN
                  CHAIN2 = IARY2( CARD )
                  IARY1( CHAIN2 ) = 0

                  IARY1( IARY2( PCARD ) ) = CARD
                  IARY1( CARD ) = PCARD

                  IARY2( CARD ) = IARY2( PCARD )
                  IARY2( PCARD ) = CARD

               END IF
            END IF

*  Perform the Null command.
*  =========================
         ELSE IF ( EDIT .EQ. 'N' ) THEN
            CALL FTS1_BLVAL( FTSCAR( CARD ), STATUS )

*  Perform the Rename, Update, Amend, or Write command.
*  ====================================================
         ELSE IF ( EDIT .EQ. 'W' .OR. EDIT .EQ. 'U' .OR.
     :             EDIT .EQ. 'A' .OR. EDIT .EQ. 'R' ) THEN

*  Make work copies of the supplied value and comment.
            VALUE = VALUES( I )
            COMENT = COMNTS( I )

*  Obtain the renamed keyword from the value.
*  ===========================================
            IF ( EDIT .EQ. 'R' ) THEN

*  Validate the value as a keyword.  Although the card index to the
*  original keyword is known, use a new variable for the name because we
*  still need to search for the current value by name with the $V
*  special value.  Later it will be renamed to create the updated card
*  at that location.  The occurrence is not used here.
               CALL ERR_MARK
               CALL FTS1_EVKEY( VALUE, NEWKEY, LREFVK, REVOC, STATUS )

*  Check for a bad keyword name.  Issue a warning, but continue with
*  the editing.
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_ANNUL( STATUS )
                  CALL MSG_SETC( 'E', EDITS( I ) )
                  CALL MSG_SETC( 'KEY', NEWKEY )
                  CALL MSG_SETC( 'OLDKEY', KEYNAM )
                  CALL MSG_OUTIF( MSG__NORM, 'NEWKEY',
     :              'New keyword name ^KEY is invalid, undefined or '/
     :              /'not found.  Hence unable to rename the ^NEWKEY '/
     :              /'card.', STATUS )
                  GO TO 100
               END IF

*  In order to write out the card we need to obtain its current value
*  and comment, so they neatly mesh together.  The problem is most acute
*  when the old keyword was compound and the new one is not, and vice
*  versa.  So reassign the value.  The comment value will be obtained
*  at the same time as the value.
               VALUE( 1:3 ) = '$V '
               COMENT( 1:3 ) = '$C '

*  Determine whether or not the keyword is compound (reusing the
*  variable for the reasons given above).
               CMPKEY = INDEX( KEYNAM( :KEYLN ), '.' ) .NE. 0
            END IF

*  Is the card not a comment of some sort, i.e. COMMENT, HISTORY, or
*  blank?  Put another way, does the card have a value?
            COMCRD = KEYNAM .EQ. ' ' .OR. KEYNAM .EQ. 'COMMENT' .OR.
     :               KEYNAM .EQ. 'HISTORY' .OR.
     :               TYPES( I ) .EQ. 'COMMENT'

*  Interpret special characters in the value.
*  ==========================================

*  No need to determine the value for a comment card.
            IF ( .NOT. COMCRD ) THEN

*  Cannot write a non-comment card without a value.  Strictly we could
*  have a comment with keyword other than COMMENT, HISTORY, or blank;
*  but leave it like this for the first attempt.  Some tweaking
*  for comments will probably be needed.
               IF ( VALUE .EQ. ' ' .AND. WRKEY ) THEN

*  Issue a warning, but continue with the editing.  Decrement the last
*  card as it is no longer going to be used.
                  CALL MSG_SETC( 'E', EDITS( I ) )
                  CALL MSG_SETC( 'KEY', KEYNAM )
                  CALL MSG_OUTIF( MSG__NORM, 'NOVALUE',
     :              'No value supplied for Writing keyword ^KEY.  '/
     :              /'Therefore, unable to execute the ^E command.',
     :              STATUS )
                  GO TO 100
               END IF

*  At present the only special values are $V meaning use the current
*  value of the current keyword, and $V(keyword{[occurrence]}) meaning
*  use the value of the occurrence of a named keyword.
               REFVAL = VALUE( 1:2 ) .EQ. '$V'
               IF ( REFVAL ) THEN

*  Initialise as the current editing keyword and card.
                  KEYREV = KEYNAM
                  VCARD = CARD

*  Search for left parenthesis.
                  VLPPOS = INDEX( VALUE, '(' )
                  IF ( VLPPOS .NE. 0 ) THEN

*  Look for the right-hand parenthesis searching forwards from the
*  left-hand parenthesis.
                     CDELIM = VLPPOS + 1
                     CALL CHR_TOCHR( ')', VALUE, .TRUE., CDELIM )

*  Derive the provisional length of the value reference keyword.  It
*  does not matter if the right-hand parenthesis is omitted as the
*  character pointer is one character beyond the second keyword.
                     LREFVK = CDELIM - VLPPOS - 1

*  Exclude the special case when the length is 0 (already defaults to
*  current editing keyword).
                     IF ( LREFVK .GT. 0 ) THEN

*  Extract the uppercase positional keyword and any occurrence, and
*  find the length of the keyword.  Also validate the keyword.
                        CALL ERR_MARK
                        CALL FTS1_EVKEY( VALUE( VLPPOS + 1:CDELIM - 1 ),
     :                                   KEYREV, LREFVK, REVOC, STATUS )

*  Assign the card if there was an error.  This is the current value
*  for Update, and fatal for Write (use the special card -1).  Also
*  reset the keyword name for Update as it is needed to obtain the
*  value (and possibly the comment).
                        IF ( STATUS .NE. SAI__OK ) THEN
                           CALL ERR_ANNUL( STATUS )
                           IF ( WRKEY ) THEN
                              VCARD = -1
                           ELSE
                              VCARD = CARD
                              KEYREV = KEYNAM
                           END IF
                        END IF
                        CALL ERR_RLSE

*  If the positional keyword is the new keyword for a write operation
*  it has no value, hence set a flag to to issue a warning.
                     ELSE IF ( WRKEY ) THEN
                        VCARD = -1

                     END IF

*  Find the desired occurrence of the value keyword, by searching by
*  name, then by occurrence.  A bad status indicates that the keyword
*  was not found.  Annul the error message in this case, and use the
*  existing value.
                     IF ( VCARD .GT. 0 ) THEN
                        CALL ERR_MARK
                        CALL FTS1_LOKEY( LSTCAR, FTSCAR, KEYREV, REVOC,
     :                                   VCARD, STATUS )
                        IF ( STATUS .NE. SAI__OK ) THEN
                           CALL ERR_ANNUL( STATUS )
                           IF ( WRKEY ) THEN
                              VCARD = -1
                           ELSE
                              VCARD = CARD
                              KEYREV = KEYNAM
                           END IF
                        END IF
                        CALL ERR_RLSE

*  The value comes from the current card.
                     ELSE IF ( WRKEY ) THEN
                        VCARD = CARD

*  Print an error if no value keyword is provided.
                     END IF

*  If the positional keyword is the new keyword for a write operation
*  it has no value, hence set a flag to to issue a warning.
                  ELSE IF ( WRKEY ) THEN
                     VCARD = -1

                  END IF

*  Cannot write a non-comment card without a value.  Strictly we could
*  have a comment with keyword other than COMMENT, HISTORY, or blank.
*  Issue a warning, but continue with the editing.  Decrement the last
*  card as it is no longer going to be used.
                  IF ( VCARD .EQ. -1 ) THEN
                     CALL MSG_SETC( 'E', EDITS( I ) )
                     CALL MSG_SETC( 'KEY', KEYNAM )
                     CALL MSG_OUTIF( MSG__NORM, 'NOREFVALUE',
     :                 'Reference keyword invalid, undefined or not '/
     :                 /'found, hence there is no value for Writing '/
     :                 /'keyword ^KEY.  Therefore, unable to execute '/
     :                /'the ^E command.', STATUS )

                     GO TO 100
                  END IF

*  Obtain the reference value.
*  ===========================

*  Determine the type of the value to insert.
                  CALL FTS1_QTYPE( FTSCAR( VCARD ), TYPE, STATUS )

*  Obtain the value of the reference card, using the appropriate data
*  type.
                  IF ( TYPE .EQ. '_CHAR' ) THEN
                     CALL FTS1_GKEYC( 1, FTSCAR( VCARD ), 1, KEYREV, 1,
     :                                PRESNT, VALUE, RCOMNT, DCARD,
     :                                STATUS )

                  ELSE IF ( TYPE .EQ. '_REAL' ) THEN
                     CALL FTS1_GKEYR( 1, FTSCAR( VCARD ), 1, KEYREV, 1,
     :                                PRESNT, RVALUE, RCOMNT, DCARD,
     :                                STATUS )

                  ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
                     CALL FTS1_GKEYI( 1, FTSCAR( VCARD ), 1, KEYREV, 1,
     :                                PRESNT, IVALUE, RCOMNT, DCARD,
     :                                STATUS )

                  ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
                     CALL FTS1_GKEYD( 1, FTSCAR( VCARD ), 1, KEYREV, 1,
     :                                PRESNT, DVALUE, RCOMNT, DCARD,
     :                                STATUS )

                  ELSE IF ( TYPE .EQ. '_LOGICAL' ) THEN
                     CALL FTS1_GKEYL( 1, FTSCAR( VCARD ), 1, KEYREV, 1,
     :                                PRESNT, LVALUE, RCOMNT, DCARD,
     :                                STATUS )

                  END IF

*  Convert the supplied string value to the data type required.
*  There shouldn't be any conversion errors, because the types should
*  have been determined from the values.  However, just in case, we
*  start a new error context, and should something go wrong, we annul
*  the error and set the output data type to be character.
               ELSE
                  TYPE = TYPES( I )
                  CALL ERR_MARK
                  IF ( TYPES( I ) .EQ. '_REAL' ) THEN
                     CALL CHR_CTOR( VALUE, RVALUE, STATUS )

                  ELSE IF ( TYPES( I ) .EQ. '_INTEGER' ) THEN
                     CALL CHR_CTOI( VALUE, IVALUE, STATUS )

                  ELSE IF ( TYPES( I ) .EQ. '_DOUBLE' ) THEN
                     CALL CHR_CTOD( VALUE, DVALUE, STATUS )

                  ELSE IF ( TYPES( I ) .EQ. '_LOGICAL' ) THEN
                     CALL CHR_CTOL( VALUE, LVALUE, STATUS )

                  END IF
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL ERR_ANNUL( STATUS )
                     TYPE = '_CHAR'
                  END IF
               END IF

*  By definition a comment card does not have a value.
            ELSE
               VALUE = ' '
            END IF

*  Interpret special characters in the comment.
*  ============================================

*  At present the only special values are $C meaning use the current
*  comment of the current keyword, and $C(keyword{[occurrence]}) meaning
*  use the comment of the occurrence of a named keyword.
            REFCOM = COMENT( 1:2 ) .EQ. '$C'

            IF ( REFCOM ) THEN

*  Initialise as the current editing keyword.
               KEYREC = KEYNAM
               CCARD = CARD

*  Search for left parenthesis.
               CLPPOS = INDEX( COMENT, '(' )
               IF ( CLPPOS .NE. 0 ) THEN

*  Look for the right-hand parenthesis searching forwards from the
*  left-hand parenthesis.
                  CDELIM = CLPPOS + 1
                  CALL CHR_TOCHR( ')', COMENT, .TRUE., CDELIM )

*  Derive the provisional length of the comment reference keyword.  It
*  does not matter if the right-hand parenthesis is omitted as the
*  character pointer is one character beyond the second keyword.
                  LREFCK = CDELIM - CLPPOS - 1

*  Exclude the special case when the length is 0 (already defaults to
*  current editing keyword).
                  IF ( LREFCK .GT. 0 ) THEN

*  Extract the uppercase positional keyword and any occurrence, and
*  find the length of the keyword.  Also validate the keyword.
                     CALL ERR_MARK
                     CALL FTS1_EVKEY( COMENT( CLPPOS + 1:CDELIM - 1 ),
     :                                KEYREC, LREFCK, RECOC, STATUS )

                     IF ( STATUS .NE. SAI__OK ) THEN
                        CALL ERR_ANNUL( STATUS )

*  We can use the current comment for an Update, but in Write this
*  doesn't exist.  Also reset the keyword name for Update as it is
*  needed to obtain the comment.
                        IF ( WRKEY ) THEN
                           CCARD = -1
                        ELSE
                           CCARD = CARD
                           KEYREC = KEYNAM
                        END IF
                     END IF
                     CALL ERR_RLSE
                  END IF
               END IF

*  There is no need to search for a comment if it is already known.
               IF ( REFVAL .AND. KEYREC .EQ. KEYREV ) THEN
                  COMENT = RCOMNT

*  Find the desired occurrence of the comment keyword, by searching by
*  name, then by occurrence.  A bad status indicates that the keyword
*  was not found.  Annul the error message in this case, and use the
*  existing comment.
               ELSE
                  IF ( CCARD .GT. 0 ) THEN
                     CALL ERR_MARK
                     CALL FTS1_LOKEY( LSTCAR, FTSCAR, KEYREC, RECOC,
     :                                CCARD, STATUS )

                     IF ( STATUS .NE. SAI__OK ) THEN
                        CALL ERR_ANNUL( STATUS )

*  We can use the current comment for an Update, but in Write this
*  doesn't exist.
                        IF ( WRKEY ) THEN
                           CCARD = -1
                        ELSE
                           CCARD = CARD
                           KEYREC = KEYNAM
                        END IF
                     END IF
                     CALL ERR_RLSE

*  We can use the current comment for an Update, but in Write this
*  doesn't exist.
                  ELSE IF ( WRKEY ) THEN
                     CCARD = -1

                  ELSE
                     CCARD = CARD

                  END IF
               END IF

*  Obtain the reference comment.
*  =============================

*  Obtain the comment of the reference card.  If there was no reference
*  card or it pointed to itself in Write edit, the comment must be
*  blank.
               IF ( CCARD .GT. 0 ) THEN
                  CALL FTS1_GKEYC( 1, FTSCAR( CCARD ), 1, KEYREC, 1,
     :                             PRESNT, CVALUE, COMENT, DCARD,
     :                             STATUS )
               ELSE
                  COMENT = ' '

               END IF

*  Just use the supplied comment.  It has already been obtained for a
*  rename edit.
            ELSE IF ( EDIT .NE. 'R' ) THEN
               COMENT = COMNTS( I )
            END IF

*  Write or modify the current card.
*  ==================================

*  Switch to the new keyword name for Rename edit.  Use its type found
*  when obtaining its current value and comment.
            IF ( EDIT .EQ. 'R' ) THEN
               KEYNAM = NEWKEY
            END IF

*  Determine whether the type is one of the integers.
            INTTYP = TYPE .EQ. '_INTEGER' .OR. TYPE .EQ. '_BYTE' .OR.
     :               TYPE .EQ. '_UBYTE' .OR. TYPE .EQ. '_WORD' .OR.
     :               TYPE .EQ. '_UWORD'

*  Write the card.  Call the appropriate type for the routine.  The
*  character instantiation also writes comment cards.
            IF ( TYPE .EQ. '_CHAR' .OR. TYPE .EQ. ' ' .OR.
     :           COMCRD ) THEN
               CALL FTS1_WKEYC( KEYNAM, VALUE, '/', COMENT, COMCRD,
     :                          FTSCAR( CARD ), STATUS )

            ELSE IF ( TYPE .EQ. '_REAL' ) THEN
               CALL FTS1_WKEYR( KEYNAM, RVALUE, '/', COMENT,
     :                          FTSCAR( CARD ), STATUS )

            ELSE IF ( INTTYP ) THEN
               CALL FTS1_WKEYI( KEYNAM, IVALUE, '/', COMENT,
     :                          FTSCAR( CARD ), STATUS )

            ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
               CALL FTS1_WKEYD( KEYNAM, DVALUE, '/', COMENT,
     :                          FTSCAR( CARD ), STATUS )

            ELSE IF ( TYPE .EQ. '_LOGICAL' ) THEN
               CALL FTS1_WKEYL( KEYNAM, LVALUE, '/', COMENT,
     :                          FTSCAR( CARD ), STATUS )

            END IF

*  Update the linked lists to allow for the insertion or extension.
*  ================================================================
            IF ( WRKEY ) THEN

*  Now that all the tests are passed, increment the counter of the last
*  card.
               LSTCAR = LSTCAR + 1

*  If the card is to be inserted in the middle of the old FITS card
*  array, change the chains at the position to include it.  Use a
*  temporary value to prevent the original value being lost until the
*  relinkage is complete.  Note that there is no check for the end of
*  chain 2, as this is the immovable END card (ENDCAR is fixed).
               IF ( PCARD .NE. CHAIN1 ) THEN
                  IARY1( IARY2( PCARD ) ) = LSTCAR
                  IARY1( LSTCAR ) = PCARD
                  TMP = IARY2( PCARD )
                  IARY2( PCARD ) = LSTCAR
                  IARY2( LSTCAR ) = TMP

*  If the new card is placed at the beginning of chain 1 (end of chain
*  2), make it the start of chain 1 and alter the original start of
*  chain 1 by creating links to the new start.
               ELSE IF ( PCARD .EQ. CHAIN1 ) THEN
                  CHAIN1 = LSTCAR
                  IARY1( LSTCAR ) = PCARD
                  IARY2( PCARD ) = LSTCAR
                  IARY2( LSTCAR ) = 0

               END IF

*  Update the linked lists to allow for re-ordering (Update).
*  ==========================================================

*  In update look to see if the positional card exists is not the card
*  being edited.  If so we have to move the card to its new location.
            ELSE IF ( UPKEY .AND. PCARD .NE. CARD ) THEN

*  No need to move if the card is already in the correct location.
*  Indeed it can duplicate links and isolate elements like an ox-bow
*  lake.
               IF ( PCARD .NE. IARY1( CARD ) ) THEN

*  Move just requires different linkages in the linked lists.  If the
*  card is to be moved from the middle of the old FITS card array,
*  change the chains at the position.  Use temporary values to prevent
*  the original values being lost until the relinkage is complete.
                  IF ( CARD .NE. CHAIN1 .AND. CARD .NE. CHAIN2 ) THEN
                     TMP = IARY1( CARD )
                     TMP2 = IARY2( CARD )

                     IARY1( IARY2( PCARD ) ) = CARD
                     IARY1( TMP2 ) = TMP
                     IARY1( CARD ) = PCARD

                     IARY2( CARD ) = IARY2( PCARD )
                     IARY2( PCARD ) = CARD
                     IARY2( TMP ) = TMP2

*  If the old card is at the beginning of chain 1 (end of chain 2),
*  take the next one in the chain as the new start card for chain 1 and
*  as the end card for chain 2.
                  ELSE IF ( CARD .EQ. CHAIN1 ) THEN
                     CHAIN1 = IARY1( CARD )
                     IARY2( CHAIN1 ) = 0

                     IARY1( IARY2( PCARD ) ) = CARD
                     IARY1( CARD ) = PCARD

                     IARY2( CARD ) = IARY2( PCARD )
                     IARY2( PCARD ) = CARD

*  If the old card is at the end of chain 1 (beginning of chain 2),
*  take the card before as the end card for chain 1 and as the start
*  card for chain 2.
                  ELSE
                     CHAIN2 = IARY2( CARD )
                     IARY1( CHAIN2 ) = 0

                     IARY1( IARY2( PCARD ) ) = CARD
                     IARY1( CARD ) = PCARD

                     IARY2( CARD ) = IARY2( PCARD )
                     IARY2( PCARD ) = CARD

                  END IF
               END IF

            END IF
         END IF

*  Go back to process the next keyword.  Also come here after issuing a
*  warning message about some failed edit.
  100    CONTINUE
      END DO

*  Copy the reordered cards back into the supplied FITS header array.
*  ==================================================================

*  Copy the cards to the temporary character work space in the order
*  given by chain 1.
      ACTNUM = 0
      CARD = CHAIN1
      DO WHILE( CARD .NE. 0 )
         ACTNUM = ACTNUM + 1
         CARY( ACTNUM ) = FTSCAR( CARD )
         CARD = IARY1( CARD )
      END DO

*  If anything may have changed, copy the cards from the temporary
*  character work space back to FITS array.
      IF( .NOT. RDONLY ) THEN
         DO CARD = 1, ACTNUM
            FTSCAR( CARD ) = CARY( CARD )
         END DO
      END IF

*  If there was no END-card originally, remove the END-card that was
*  added unless its addition had been requested.
      IF ( .NOT. TERMIN8 ) ACTNUM = ACTNUM - 1

      END
