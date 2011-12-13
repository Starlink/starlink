      SUBROUTINE COI_SPECN( IMDESC, WORD, END, NUMBER, HEADER, OPOS,
     :                      STATUS )
*+
*  Name:
*     COI_SPECN

*  Purpose:
*     Creates the WAT1_nnn cards for a series of words.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COI_SPECN( IMDESC, WORD, END, NUMBER, HEADER, OPOS, STATUS )

*  Description:
*     This routine forms and writes WAT1_nnn IRAF MWCS header cards
*     to an OIF header file.  The supplied word is appended to the
*     current header under construction with a trailing space, provided
*     there is room.  If there is no room, the current header is
*     terminated with a closing quote, and written to the OIF file;
*     and the value is appended to the next header where the header
*     number is incremented by 1.

*  Arguments:
*     IMDESC = INTEGER (Given)
*        The IRAF IMFORT image descriptor.
*     WORD = CHARACTER * ( * ) (Given)
*        The word to append to the header.
*     END = LOGICAL (Given)
*        If .TRUE., the supplied word is the last, and so the final
*        closing double quote should be inserted before the single
*        quote and the header written.
*     NUMBER = INTEGER (Given and Returned)
*        The number of the header under construction.  For example, if
*        NUMBER=3, the header keyword would be WAT1_003.  If a new
*        header is started the supplied value will be incremented by 1
*        on exit.
*     HEADER = CHARACTER * ( 80 ) (Given and Returned)
*        The header under construction.
*     OPOS = INTEGER (Given and Returned)
*        The column in the header from where the next value is to be
*        written.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1997 April 20 (MJC):
*        Original version.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER IMDESC
      CHARACTER * ( * ) WORD
      LOGICAL END

*  Arguments Given and Returned:
      INTEGER NUMBER
      CHARACTER * ( 80 ) HEADER
      INTEGER OPOS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 4 ) CN       ! 1000 plus line number
      INTEGER NC                 ! Number of characters
      INTEGER WRDLEN             ! Effective word length

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Specify the word length including the trailing " if this is the last
*  word.
      NC = LEN( WORD )
      IF ( END ) THEN
         WRDLEN = NC + 2
      ELSE
         WRDLEN = NC + 1
      END IF

*  Append the word if it will fit with any trailing quote(s).
      IF ( WRDLEN + OPOS .LT. 79 ) THEN
         CALL CHR_APPND( WORD, HEADER, OPOS )

*  End the attribute value.  This assumes that the value is not going to
*  fill a header.
         IF ( END ) THEN
            CALL CHR_APPND( '" ''', HEADER, OPOS )
            CALL ADLINE( IMDESC, HEADER )
            OPOS = 0

*  Leave a space after the value ready for the next word.
         ELSE
            OPOS = OPOS + 1
         END IF

*  Does the word just fit into the header?
      ELSE IF ( WRDLEN + OPOS .EQ. 79 ) THEN
         CALL CHR_APPND( WORD, HEADER, OPOS )

*  Append the space following the value.
         OPOS = OPOS + 1

*  If the last value just fits on the line close the assignment and
*  write the record.
         IF ( END ) THEN
            CALL CHR_APPND( '"''', HEADER, OPOS )
            CALL ADLINE( IMDESC, HEADER )
            OPOS = 0

         ELSE

*  Append the closing quote to the header, and append the header to the
*  file.
            CALL CHR_APPND( '''', HEADER, OPOS )
            CALL ADLINE( IMDESC, HEADER )

*  Start the new WAT2_nnn card up to the leading quote.  Add 1000 to
*  the header line number to get the leading zeroes.
            NUMBER = NUMBER + 1
            CALL CHR_ITOC( 1000 + NUMBER, CN, NC )
            HEADER = 'WAT2_'
            OPOS = 5
            CALL CHR_APPND( CN( 2:4 ), HEADER, OPOS )
            CALL CHR_APPND( '= ''', HEADER, OPOS )

         END IF

*  There is no room for the value.
      ELSE

*  Close the value at the current position and append the header.
         CALL CHR_APPND( '''', HEADER, OPOS )
         CALL ADLINE( IMDESC, HEADER )

*  Start the new WAT2_nnn card up to the leading quote.  Add 1000 to
*  the header line number to get the leading zeroes.
         NUMBER = NUMBER + 1
         CALL CHR_ITOC( 1000 + NUMBER, CN, NC )
         HEADER = 'WAT2_'
         OPOS = 5
         CALL CHR_APPND( CN( 2:4 ), HEADER, OPOS )
         CALL CHR_APPND( '= ''', HEADER, OPOS )

*  Write the word to the new card.
         CALL CHR_APPND( WORD, HEADER, OPOS )

*  End the attribute value.  This assumes that the value is not going to
*  fill a header.
         IF ( END ) THEN
            CALL CHR_APPND( '"''', HEADER, OPOS )
            CALL ADLINE( IMDESC, HEADER )
            OPOS = 0

*  Leave a space after the value.
         ELSE
            OPOS = OPOS + 1
         END IF

      END IF

      END
