      SUBROUTINE KPS1_PUTS( VALUE, COMMA, LINE, LENG, FULL, STATUS )
*+
*  Name:
*     KPS1_PUTS

*  Purpose:
*     Put the supplied value string in the supplied text line.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL KPS1_PUTS( VALUE, COMMA, LINE, LENG, FULL, STATUS )

*  Description:
*     The supplied value string is planted in the text line followed by
*     an optional comma.  The line length and array indices are updated
*     as appropriate.  The routine determines whether or not there is
*     enough room to plant the value string, and returns a logical flag
*     stating what has happened.

*  Arguments:
*     VALUE = CHARACTER * ( * ) (Given)
*        Value string.
*     COMMA = LOGICAL (Given)
*        If true, a comma is written after the value.
*     LINE = CHARACTER*(*) (Given and Returned)
*        Line of text to be appended
*     LENG = INTEGER (Given and Returned)
*        Current length of the characters in LINE excluding any trailing
*        blanks.
*     FULL = LOGICAL (Returned)
*        If true the line given was full.
*     STATUS = INTEGER (Given)
*        Global status

*  Implementation Deficiencies:
*     At present, this ignores the true dimensionality, and produces
*     a list of values seperated by commas.

*  Copyright:
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     Copyright (C) 2011 Science & Technology Facilities Council.
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
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1995 May 9 (MJC)
*        Original version based upon HDSTRACE routine TRA_PUTS.
*     2011 May 11 (MJC):
*        Removed no-longer-used arguments NDIM, DIMS, IVALUE, and INDS.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! Switch off the default typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SAI Constants

*  Arguments Given:
      CHARACTER * ( * ) VALUE    ! Value string
      LOGICAL COMMA              ! True if a comma is to be appended to
                                 ! value

*  Arguments Given and Returned:
      CHARACTER * ( * ) LINE     ! Line to receive numbers
      INTEGER LENG               ! Current line length

*  Returned:
      LOGICAL FULL               ! Line was found full?

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! String length
      INTEGER CHR_SIZE           ! String size

*  Local Variables:
      INTEGER NCHAR              ! Value string length
      INTEGER NEED               ! Space needed

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the number of characters needed allowing for the ellipsis and a
*  prefixing comma if it is not the first element of an array.
      NCHAR = CHR_LEN( VALUE )
      NEED = NCHAR + 3
      IF ( COMMA ) THEN
         NEED = NEED + 1
      END IF

*  See if there is space in the line.
      FULL = ( CHR_SIZE( LINE ) - LENG ) .LE. NEED

      IF ( .NOT. FULL ) THEN

*  There is room to accommodate the value, so append it to the line.
*  Append the comma if required.
         CALL CHR_PUTC( VALUE( 1:NCHAR ), LINE, LENG )
         IF ( COMMA ) THEN
            CALL CHR_PUTC( ',', LINE, LENG )
         END IF
      END IF

      END
