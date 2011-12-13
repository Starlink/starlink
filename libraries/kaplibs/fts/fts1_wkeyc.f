      SUBROUTINE FTS1_WKEYC( NAME, VALUE, CMTBGN, COMNT, COMCAR,
     :                       HEADER, STATUS )
*+
*  Name:
*     FTS1_WKEYC

*  Purpose:
*     Writes a FITS-header card for a CHARACTER value or a comment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_WKEYC( NAME, VALUE, CMTBGN, COMNT, COMCAR, HEADER,
*                      STATUS )

*  Description:
*     This routine writes a FITS-header card using the supplied keyword,
*     value, comment, and comment delimiter.  The card is either a
*     character string value, or a comment card.  The latter occurs when
*     the keyword is blank, HISTORY or COMMENT, or the COMCAR argument
*     is .TRUE.; in this case both the supplied value and comment
*     delimiter are ignored.  The name may be compound to permit
*     writing of hierarchical keywords.
*
*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        The name of the keyword to be written.  This may be a compound
*        name to handle hierarchical keywords, and it has the form
*        keyword1.keyword2.keyword3 etc.  The maximum number of
*        keywords per FITS card is 20.  The value is converted to
*        uppercase and blanks are removed before being used.  Each
*        keyword must be no longer than 8 characters.  The total length
*        must not exceed 48 characters.  This is to allow for the
*        value, and indentation into a blank-keyword card (as
*        hierarchical keywords are not standard and so cannot be part
*        of the standard keyword name).
*     VALUE = CHARACTER * ( * ) (Given)
*        The value of the keyword.  It is ignored when the keyword is
*        COMMENT, HISTORY, or blank, or COMCAR = .TRUE..
*     CMTBGN = CHARACTER * ( 1 ) (Given)
*        The character which indicates the beginning of the comment
*        string of to be appended to the keyword.  Normally it is '/'.
*        when it is blank, no comment will be appended to the keyword.
*        It is ignored when the keyword is COMMENT, HISTORY, or blank,
*        or COMCAR = .TRUE..
*     COMNT = CHARACTER * ( * ) (Returned)
*        The comment string of the keyword.  It may be truncated at the
*        end to put into the space left after writing keyword value for
*        non-commentary keywords.
*     COMCAR = LOGICAL (Given)
*        If .TRUE., the supplied card is a comment and thus the value
*        and comment delimiter are ignored, and just the keyword and
*        comment string are used to generate the header card.
*     HEADER = CHARACTER * ( 80 ) (Returned)
*        The created FITS-header card.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1996, 1998 Central Laboratory of the Research Councils.
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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 November 13 (MJC):
*        Original version.
*     5-JUN-1998 (DSB):
*        Move statement which initialises NCHAR outside the IF block
*        so that it is executed even if COMCRD is .TRUE.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) NAME
      CHARACTER * ( * ) VALUE
      CHARACTER * ( 1 ) CMTBGN
      CHARACTER * ( * ) COMNT
      LOGICAL COMCAR

*  Arguments Returned:
      CHARACTER * ( 80 ) HEADER

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! The used length of a string

*  Local Constants:
      INTEGER COMCOL             ! Minimum column position for the start
                                 ! of a FITS comment
      PARAMETER ( COMCOL = 32 )

      INTEGER CMPEQC             ! Minimum column position for the
                                 ! equals sign in a compound keyword
                                 ! of a FITS comment
      PARAMETER ( CMPEQC = 22 )

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

      INTEGER VALLN              ! Maximum number of characters in a
                                 ! FITS header card value
      PARAMETER ( VALLN = 70 )

*  Local Variables:
      INTEGER CARDLN             ! Used length of a card image
      LOGICAL COMCRD             ! Commentary-keyword flag
      LOGICAL COMPND             ! Compound-keyword flag
      INTEGER ENDPOS             ! End column of part of value
                                 ! extracted
      INTEGER EQUALS             ! The position of equal sign
      INTEGER KEYLEN             ! Length of specified keyword
      CHARACTER * ( HKEYLN ) KEYWRD ! Specified keyword
      INTEGER NCHAR              ! Number of characters in value
      INTEGER STAPOS             ! Start column of part of value
                                 ! extracted
      CHARACTER * ( VALLN ) VAL  ! Copy of value (for doubled quotes)
      INTEGER VALN               ! Length of the keyword value

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the header.
      HEADER = ' '

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

*  To see whether it is a compound name.
      COMPND = INDEX( KEYWRD, '.' ) .NE. 0

*  Specify the column for the equals sign.  In compound names, allow
*  for the blank keyword, then a space followed by a keyword then a
*  space.  Also replace the fullstops with spaces.  Bad status is
*  returned when the lengths of the replacement strings is not equal to
*  the substituted strings.  Here they are fixed, so STATUS need not be
*  checked.  Also specify the column one before where the keyword will
*  be written.
      IF ( COMPND ) THEN
         CARDLN = KEYLN + 1
         EQUALS = MAX( CMPEQC, KEYLEN + 1 + CARDLN )
         CALL CHR_TRCHR( '.', ' ', KEYWRD, STATUS )
      ELSE
         CARDLN = 0
         EQUALS = KEYLN + 1
      END IF

*  Start writing the card with the keyword.
      CALL CHR_APPND( KEYWRD, HEADER, CARDLN )

*  Make a copy of the supplied value so that any quotes present can be
*  doubled. Find the length of the value.
      VAL = VALUE
      NCHAR = CHR_LEN( VAL )
      IF ( .NOT. COMCRD .AND. VAL .NE. ' ' ) THEN

*  Initialise some column-position counters.
         STAPOS = 1
         ENDPOS = 1
         CARDLN = 0

*  Loop searching for quotes.  Copy the string up to and including the
*  quote.  Append a second quote.  When the quotes are exhausted, append
*  the remainder of the text.
   10    CONTINUE        ! Start of 'DO WHILE' loop
         IF ( ENDPOS .LE. NCHAR ) THEN
            CALL CHR_FIND( VALUE, '''', .TRUE., ENDPOS )

            IF ( ENDPOS .LE. NCHAR ) THEN
               CALL CHR_APPND( VALUE( STAPOS:ENDPOS ), VAL, CARDLN )
               CALL CHR_APPND( '''', VAL, CARDLN )
               ENDPOS = ENDPOS + 1
               STAPOS = ENDPOS

            ELSE
               CALL CHR_APPND( VALUE( STAPOS:NCHAR ), VAL, CARDLN )
               NCHAR = CARDLN
            END IF
            GO TO 10
         END IF
      END IF

*  Deal with non-commentary keywords first.
      IF ( .NOT. COMCRD ) THEN

*  Write the equals sign for a value.
         CARDLN = EQUALS - 1
         CALL CHR_APPND( '=', HEADER, CARDLN )

*  Format the value string.  Find the length of the value that will
*  appear in the card.  FITS imposes a minimum length of eight
*  characters for a string value.  We cannot use CHR_APPND to truncate
*  because the string must be enclosed in single quotes.
         VALN = MAX( CVALLN, MIN( VALLN, NCHAR ) )

*  Append the value, and obtain the used length.  Note that CHR_APPND
*  will not increment CARDLN if a space is appended or the value has
*  trailing blanks.
         CARDLN = EQUALS + 1
         HEADER( CARDLN:CARDLN ) = ' '
         CALL CHR_APPND( '''', HEADER, CARDLN )
         ENDPOS = CARDLN
         CALL CHR_APPND( VAL( : VALN ), HEADER, CARDLN )
         CARDLN = ENDPOS + VALN
         CALL CHR_APPND( '''', HEADER, CARDLN )

*  If there is a comment string following keyword value, ...
         IF ( CMTBGN .NE. ' ' .AND. COMNT .NE. ' ' ) THEN

*  If the length is less the minimum, pad the card with blanks to the
*  minimum length.  The comment cannot begin before the minimum length
*  of a non-character value, and so allowing for the space the comment
*  must not appear until two columns after this.
            CARDLN = MAX( CARDLN, COMCOL - 2 ) + 1

*  Append the comment string to the card, or as much as can fit on to
*  the card.  Notice that leading blanks are not removed from the
*  comment in case the user is attempting some clever alignment.  Since
*  a FITS comment may contain any ASCII character, it has not been
*  cleaned.
            CALL CHR_APPND( CMTBGN, HEADER, CARDLN )
            CARDLN = CARDLN + 1
            HEADER( CARDLN:CARDLN ) = ' '
            CALL CHR_APPND( COMNT, HEADER, CARDLN )
         END IF

*  Deal with comment-only cards.
      ELSE

*  Append the comment string to the card, or as much as can fit on to
*  the card.  Notice leading blanks are not removed from the comment in
*  case the user is attempting some clever alignment.  Since a FITS
*  comment may contain any ASCII character, it has not been cleaned.
         CARDLN = 9
         HEADER( CARDLN:CARDLN ) = ' '
         CALL CHR_APPND( COMNT, HEADER, CARDLN )
      END IF

      END
