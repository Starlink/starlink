      SUBROUTINE COI_WCCAT( IMDESC, DIM, VALUE, LENGTH, STATUS )
*+
*  Name:
*     COI_WCCAT

*  Purpose:
*     Concatenates the values in IRAF MWCS WATd_nnn headers into a
*     string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COI_WCCAT( IMDESC, DIM, VALUE, LENGTH, STATUS )

*  Description:
*     This routine forms a string from concatening the values of the
*     WATd_nnn IRAF MWCS header cards in an OIF header file for the
*     given dimension d.

*  Arguments:
*     IMDESC = INTEGER (Given)
*        The IRAF IMFORT image descriptor.
*     DIM = INTEGER (Given)
*        The dimension d whose WATd_nnn card values are to be
*        concatenated.  It must be between one and three.
*     VALUE = CHARACTER * ( * ) (Returned)
*        The concatenated values.  This should be at least the expected
*        length of the values, i.e. 68 times the number of WATd_nnn
*        cards.  An error results if the extracted value exceeds the
*        length of this value.
*     LENGTH = INTEGER (Returned)
*        The effective length of the VALUE string.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     The IRAF file must be open.

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
*     1997 July 18 (MJC):
*        Original version.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER IMDESC
      INTEGER DIM

*  Arguments Returned:
      CHARACTER * ( * ) VALUE
      INTEGER LENGTH

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! String length ignoring trailing
                                 ! blanks

*  Local Constants:
      INTEGER IRAFOK             ! IMFORT good status.
      PARAMETER ( IRAFOK = 0 )

*  Local Variables:
      CHARACTER * ( 80 ) CARD    ! IRAF header card
      CHARACTER * ( 1 ) CD       ! d in WATd_nnn
      CHARACTER * ( 4 ) CN       ! 1000 plus line number nnn
      INTEGER KEYNO              ! Keyword counter
      CHARACTER * ( 8 ) KEYWRD   ! Header keyword
      INTEGER LINE               ! Line counter
      INTEGER NC                 ! Number of characters
      INTEGER NCVAL              ! Number of characters in value
      LOGICAL OVFLOW             ! Buffer overflowed?
      INTEGER POS                ! Column poisition from where to append
                                 ! value from the next header card
      LOGICAL THERE              ! Keyword is present?
      INTEGER TRAILQ             ! COlumn containing trailing quote

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the length of the VALUE buffer.  Initialise its used length
*  and its value.
      NCVAL = LEN( VALUE )
      POS = 0
      VALUE = ' '
      OVFLOW = .FALSE.
      THERE = .TRUE.

*  Keyword counter has leading zeroes, so add one thousand to the
*  initial line counter.
      LINE = 1001

*  Convert the dimension to a character.
      CALL CHR_ITOC( DIM, CD, NC )

*  Set the error flag to be OK.
  100 CONTINUE    ! Start of DO WHILE loop
      IF ( STATUS .EQ. SAI__OK .AND. .NOT. OVFLOW .AND. THERE ) THEN

*  Form the keyword.  First generate the nnn part of the keyword.
         CALL CHR_ITOC( LINE, CN, NC )
         KEYWRD = 'WAT'//CD//'_'//CN( 2:4 )

*  Obtain the header for the keyword.
         KEYNO = 1
         CALL COI_GHEAD( IMDESC, KEYWRD, KEYNO, THERE, STATUS )

*  If the next card doesn't exist, that's the end of the WATd_nnn
*  headers.
         IF ( THERE ) THEN

*  Obtain the header card.
            CALL GETLIN( IMDESC, KEYNO, CARD )

*  Search for the trailing quote around the value.  The leading quote
*  is in column 11 of the FITS-like header, so begin the search
*  following the leading quote.
            TRAILQ = INDEX( CARD( 12: ), '''' ) + 11

*  Check to see whether the value will fit into the buffer.
            OVFLOW = CHR_LEN( CARD( 12:TRAILQ - 1 ) ) + POS .GT. NCVAL

*  Append the value between the quotes, updating the current position in
*  the VALUE buffer.
            CALL CHR_APPND( CARD( 12:TRAILQ - 1 ), VALUE, POS )

*  The value is made up of words, which may be split across lines.
*  Only preserve a space when there was a trailing space in the value.
            IF ( CARD( TRAILQ-1:TRAILQ-1 ) .EQ. ' ' ) POS = POS + 1

*  Increment the line number
            LINE = LINE + 1
         END IF

*  Return to the head of the DO WHILE loop.
         GOTO 100
      END IF

*  Set the returned length.  Allow for an extra trailing space.
      LENGTH = POS - 1
      IF ( VALUE( POS:POS ) .EQ. ' ' ) LENGTH = LENGTH + 1

      END
