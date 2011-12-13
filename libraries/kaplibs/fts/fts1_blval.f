      SUBROUTINE FTS1_BLVAL( HEADER, STATUS )
*+
*  Name:
*     FTS1_BLVAL

*  Purpose:
*     Blanks out the value from a FITS-header card.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_BLVAL( HEADER, STATUS )

*  Description:
*     This routine modifies a FITS-header card by replacing the value
*     with blank characters, leaving the keyword, value indicator and
*     any comment in situ.  If the header has no value, the header is
*     returned unchanged.
*
*  Arguments:
*     HEADER = CHARACTER * ( 80 ) (Given & Returned)
*        The FITS-header card.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2009 Science and Technology Facilities Council.
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
*     2009 January 11 (MJC):
*        Original version.
*     2009 January 20 (MJC):
*        Retain the Value Indicator.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given and Returned:
      CHARACTER * ( 80 ) HEADER

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER COMCOL             ! Minimum column position for the start
                                 ! of a FITS comment
      PARAMETER ( COMCOL = 32 )

      INTEGER KEYLN              ! Maximum number of characters in a
                                 ! FITS header card keyword or
                                 ! hierarchical component thereof
      PARAMETER ( KEYLN = 8 )

      INTEGER MXWORD             ! Maximum number of words before
                                 ! equals sign
      PARAMETER ( MXWORD = 6 )

      INTEGER VALLN              ! Maximum number of characters in a
                                 ! FITS header card value
      PARAMETER ( VALLN = 70 )

*  Local Variables:
      CHARACTER*( VALLN ) CWORK  ! Work string
      INTEGER END                ! End column to blank
      INTEGER ENDW( MXWORD )     ! End columns of each keyword in a card
                                 ! image
      INTEGER EQUALS             ! The position of first equals sign
      INTEGER ISTAT              ! CHR status
      INTEGER NWORD              ! Number of keywords in the current
                                 ! card image
      INTEGER START              ! Start column to blank
      INTEGER STARTW( MXWORD )   ! Start columns of each keyword in a
                                 ! card image
      CHARACTER * ( KEYLN ) WORDS( MXWORD ) ! Keywords in the header

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ignore comment headers.
      IF ( HEADER( 1:8 ) .NE. 'COMMENT' .AND.
     :     HEADER( 1:8 ) .NE. 'HISTORY' ) THEN

*  A further type of comment has a blank keyword, but this can also be
*  an hierarchical keyword.

*  Does the current card have a value, i.e. is there an equals sign
*  present?  This is necessary to be able to descriminate the value
*  from a hierarchical keyword.
         EQUALS = INDEX( HEADER, '=' )
         IF ( EQUALS .NE. 0 ) THEN

*  Extract the words from the FITS card image up to the equals sign,
*  assuming these to be the keyword.
            CALL CHR_DCWRD( HEADER( :EQUALS - 1 ), MXWORD,
     :                      NWORD, STARTW, ENDW, WORDS, ISTAT )

*  For a keyword, there must only be one word before the equals sign.
*  This test excludes a comment unless the equals sign is the first
*  character after a blank keyword, so we add a further check that the
*  first word contains a fullstop needed in a compound keyword.
*  Further check for a hierarchical-keyword convention.
            IF ( ( NWORD .EQ. 1 .AND. ( EQUALS .EQ. 9 .OR.
     :             INDEX( WORDS( 1 ), '.' ) .GT. 0 ) ) .OR.
     :           WORDS( 1 ) .EQ. 'HIERARCH' ) THEN

*  At this point the equals sign marks the start of the characters to
*  blank.  Now we find the end of the value.  This will depend on the
*  type of the value.

*  Copy the buffer from the equals sign and remove the leading blanks.
               CWORK = HEADER( EQUALS + 1: )
               CALL CHR_LDBLK( CWORK )

*  Columns of 11:30 should contain FITS item value if numeric or
*  logical.  Character values may extend to column 80.  Column 10
*  should be a space immediately following the equals sign.

*  Search for the leading quote of a string.
               IF ( CWORK( 1:1 ) .EQ. '''' ) THEN

*  Extract the string and its location.
                  CALL FTS1_GVALC( HEADER, START, END, CWORK, STATUS )

               ELSE

*  For normal headers the end position is 30, but for hierarchical
*  keywords it is shifted rightwards.  Since the normal value
*  numeric (ignoring complex) or boolean comprises a single word, find
*  the column where the first word in the remainder of the header lies
*  to derive the rightmost column of the value.
                  CALL CHR_DCWRD( HEADER( EQUALS + 1: ), MXWORD,
     :                            NWORD, STARTW, ENDW, WORDS, ISTAT )
                  END = ENDW( 1 ) + EQUALS + 1
               END IF
               START = EQUALS + 2

*  Blank the value.
               HEADER( START:END ) = ' '
            END IF
         END IF
      END IF

      END
