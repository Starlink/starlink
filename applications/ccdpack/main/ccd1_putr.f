      SUBROUTINE CCD1_PUTR( VALUE, DECI, STRING, IAT, STATUS )
*+
*  Name:
*     CCD1_PUTR

*  Purpose:
*     Write a consise real value in a string with a given number of
*     decimal places.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_PUTR( VALUE, DECI, STRING, IAT, STATUS )

*  Description:
*     The routine writes the real value VALUE into a string starting at
*     a given poisition. It writes the string using F format with the
*     given number of decimal places removing unnecessary trailing and
*     leading blanks before insertion. The parameter IAT is undated to
*     be positioned at the end of the string on exit.

*  Arguments:
*     VALUE = REAL (Given)
*        The value to be written to the string.
*     DECI = INTEGER (Given)
*        The number of decimal places required.
*     STRING = CHARACTER * ( * ) (Given and Returned)
*        String which contains the written value.
*     IAT = INTEGER (Given and Returned)
*        On entry the position of the first characters of the encoded
*        real value. On exit the position of the last character of the
*        inserted string.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JAN-1992 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Primdat constants - size of string
                                 ! holding real value.

*  Arguments Given:
      REAL VALUE
      INTEGER DECI

*  Arguments Given and Returned:
      CHARACTER * ( * ) STRING
      INTEGER IAT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( VAL__SZR ) BUFFER ! String to hold encoded real
                                      ! value
      CHARACTER * ( 20 ) FMT1    ! Format statement
      CHARACTER * ( 20 ) FMT2    ! Format statement
      INTEGER FIRST              ! Position of first character
      INTEGER LAST               ! Position of last character
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Clear buffers and write value to string.
      BUFFER = ' '
      FMT1 = ' '
      FMT2 = ' '

*  Create format statement.
      WRITE( FMT1, '( I14 )' ) DECI
      CALL CHR_FANDL( FMT1, FIRST, LAST )
      FMT2 = '('//'F14.'//FMT1( FIRST :LAST )//')'

*  Write value to buffer
      WRITE( BUFFER, FMT2 ) VALUE

*  Strip leading and trailing blanks.
      CALL CHR_FANDL( BUFFER, FIRST, LAST )

*  Insert into string
      CALL CHR_PUTC( BUFFER( FIRST : LAST ), STRING, IAT )

      END
* $Id$
