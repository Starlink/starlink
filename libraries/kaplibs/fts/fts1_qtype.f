      SUBROUTINE FTS1_QTYPE( CARD, TYPE, STATUS )
*+
*  Name:
*     FTS1_QTYPE

*  Purpose:
*     Determines the data type of a FITS header value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_QTYPE( CARD, TYPE, STATUS )

*  Description:
*     This routine takes a FITS header card and determines the HDS data
*     type of the value.  If there is no value because there is no
*     equals sign present or the keyword is HISTORY or COMMENT, the
*     type is returned as 'COMMENT'.

*  Arguments:
*     CARD = CHARACTER * ( * ) (Given)
*        The FITS header card.  It should be 80 character long, but
*        the routine might work with less depending on the length of
*        the value and keyword.
*     TYPE = CHARACTER * ( * ) (Returned)
*        The HDS data type of the header's value.   It is one of the
*        following: '_INTEGER', '_REAL', '_DOUBLE', '_LOGICAL', '_CHAR',
*        or 'COMMENT'.  The length should be at least
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Implementation Deficiencies:
*     There is no support for complex values.

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
*     1996 November 8 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      CHARACTER * ( * ) CARD

*  Arguments Returned:
      CHARACTER * ( * ) TYPE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER KEYLN              ! Maximum number of characters in a
                                 ! FITS header card keyword or
                                 ! hierarchical component thereof
      PARAMETER ( KEYLN = 8 )

      INTEGER MXWORD             ! Maximum number of words to separate
                                 ! in a header card
      PARAMETER ( MXWORD = 3 )

*  Local Variables:
      LOGICAL ABUT               ! Value abuts the equals sign?
      INTEGER CSTAT              ! Local status for CHR
      DOUBLE PRECISION DVAL      ! FITS-card value
      INTEGER ENDW( MXWORD )     ! End columns of each word
      INTEGER EQPOS              ! Position of the equals sign
      INTEGER EQWORD             ! Word number containing the value
                                 ! equals sign
      INTEGER I                  ! Loop counter
      INTEGER IVAL               ! FITS-card value
      LOGICAL LVAL               ! Integer FITS-card value
      INTEGER NDIGIT             ! Number of significant digits
      INTEGER NWORD              ! Number of keywords in the current
                                 ! card image
      INTEGER OFFSET             ! Offset from equals where to extract
                                 ! the words
      LOGICAL SPEC               ! Keyword is one of the specials?
      INTEGER STARTW( MXWORD )   ! Start columns of each word
      CHARACTER * ( 60 ) WORDS( MXWORD ) ! The words of the card
      CHARACTER * ( 60 ) VALUE  ! The words of the card

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Search for an equals sign.
      EQPOS = INDEX( CARD, '=' )

*  Is this one of the special keywords 'COMMENT', 'HISTORY'?  Blank
*  might be a hierarchical keyword, so cannot exclude that yet.  Also
*  a blank line is a comment.
      SPEC = CARD( 1:KEYLN ) .EQ. 'COMMENT ' .OR.
     :       CARD( 1:KEYLN ) .EQ. 'HISTORY ' .OR. CARD .EQ. ' '

*  Check for a comment keyword, a missing equals sign, or an equals sign
*  before column 9.
      IF ( SPEC .OR. EQPOS .LE. KEYLN ) THEN
         TYPE = 'COMMENT'

      ELSE

*  Break the line into words.  For this routine we need only separate
*  the first three.  Ignore the local status as there will usually be a
*  comment following, and we only want the (non-complex) value.  Start
*  KEYLN or two characters before the equals to pick up a keyword as
*  the first word, depending on whether this is a normal or compound
*  keyword.  Would like to be able to cope with the value abutting the
*  equals (although it should not).
         IF ( EQPOS .LT. KEYLN + 2 ) THEN
            OFFSET = KEYLN
         ELSE
            OFFSET = 2
         END IF

         CALL CHR_DCWRD( CARD( EQPOS - OFFSET: ), MXWORD, NWORD,
     :                   STARTW, ENDW, WORDS, CSTAT )

*  Allow for the offset to the equals sign.
         DO I = 1, MXWORD
            STARTW( I ) = STARTW( I ) + EQPOS - OFFSET - 1
            ENDW( I ) = ENDW( I ) + EQPOS - OFFSET - 1
         END DO

*  If there are fewer than 2 words or there is no equals sign at the end
*  of the keyword or starting the second word, this is a comment.
         IF ( NWORD .LT. 2 .OR. (
     :        CARD( ENDW( 1 ):ENDW( 1 ) ) .NE. '=' .AND.
     :        CARD( STARTW( 2 ):STARTW( 2 ) ) .NE. '=' ) ) THEN
            TYPE = 'COMMENT'

         ELSE

*  Does the current card have a value, i.e. is an equals sign present?
*  The place to look is the last character of the first word, or the
*  second word depending on the length of the keyword.  This method
*  allows for hierarchical keywords.
            ABUT = .FALSE.
            IF ( CARD( ENDW( 1 ):ENDW( 1 ) ) .EQ. '=' ) THEN
               EQWORD = 1
               VALUE = WORDS( 2 )

            ELSE IF ( CARD( STARTW( 2 ):STARTW( 2 ) ) .EQ. '=' ) THEN
               EQWORD = 2

*  Check in case the value is abutted to the equals sign, though this
*  violates the standard.
               ABUT = STARTW( 2 ) .NE. ENDW( 2 )
               IF ( ABUT ) THEN
                  VALUE = WORDS( 2 )( 2: )
               ELSE
                  VALUE = WORDS( 3 )
               END IF
            END IF

*  There must be a value following the word containing the equals,
*  as far as the standard goes.  ALready tested for two words.  Here
*  we try to locate the value in the third word or abutted to the
*  second word's equals.
            IF ( EQWORD .EQ. 2 .AND.
     :           .NOT. ( ABUT .OR. NWORD .GT. 2 ) ) THEN
               TYPE = 'COMMENT'

*  Is the start of the value a quote?  If it is, the type is character.
            ELSE IF ( VALUE( 1:1 ) .EQ. '''' ) THEN
               TYPE = '_CHAR'

            ELSE

*  Proceed to test for each data type in turn.  Attempt a conversion
*  and look for an error.

*  Check for an integer.
               CSTAT = 0
               CALL CHR_CTOI( VALUE, IVAL, CSTAT )
               IF ( CSTAT .EQ. 0 ) THEN
                  TYPE = '_INTEGER'

*  Check for a floating point.
               ELSE

                  CSTAT = 0
                  CALL CHR_CTOD( VALUE, DVAL, CSTAT )
                  IF ( CSTAT .EQ. 0 ) THEN

*  Determine how many significant digits it has.
                     CALL KPG1_SGDIG( VALUE, NDIGIT, CSTAT )
                     IF ( NDIGIT .GT. -INT( LOG10( VAL__EPSR ) ) ) THEN
                        TYPE = '_DOUBLE'
                     ELSE
                        TYPE = '_REAL'
                     END IF

*  Check for a logical.  Note a literal string Y, N, YES, NO, T, F etc.
*  should be in quotes.
                  ELSE
                     CSTAT = 0
                     CALL CHR_CTOL( VALUE, LVAL, CSTAT )
                     IF ( CSTAT .EQ. 0 ) THEN
                        TYPE = '_LOGICAL'

                     ELSE
                        TYPE = '_CHAR'
                     END IF
                  END IF
               END IF
            END IF
         END IF
      END IF

      END
