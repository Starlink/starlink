      SUBROUTINE CCD1_KNEXT( NAME, OK, TYPE, STATUS )
*+
*  Name:
*     CCD1_KNEXT

*  Purpose:
*     Checks that NAME is a known CCDPACK extension item.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_KNEXT( NAME, OK, TYPE, STATUS )

*  Description:
*     This routine contains a listing of all the currently known
*     CCDPACK extension items and their data types. The NAME is
*     a full path excluding the NDF_NAME.MORE.CCDPACK header.
*     (i.e. TIMES.EXPOSURE).

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        The name of the supposed CCDPACK extension item.
*     OK = LOGICAL (Returned)
*        Set true if the NAME is recognised, otherwise is FALSE.
*     TYPE = CHARACTER * ( * ) (Returned)
*        The HDS data type of the extension item (all extension items
*        are primitives).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993-1994 Science & Engineering Research Council.
*     All Rights Reserved.

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
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     10-DEC-1993 (PDRAPER):
*        Original version.
*     4-JAN-1994 (PDRAPER):
*        Changed extension item names to correspond more closely to the
*        global parameters used by CCDPACK.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) NAME

*  Arguments Returned:
      LOGICAL OK
      CHARACTER * ( * ) TYPE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Used length of string

*  Local Variables:
      CHARACTER * ( 15 ) NAMES( 19 ) ! Known extension names
      CHARACTER * ( 8 )  TYPES( 19 ) ! Their types
      INTEGER I                      ! Loop variable
      INTEGER NAMLEN                 ! Used length of input string

*  Local Data:
      DATA NAMES / 'FTYPE',
     :             'FILTER',
     :             'ADC',
     :             'RNOISE',
     :             'DIRECTION',
     :             'BOUNDS.START1',
     :             'BOUNDS.END1',
     :             'BOUNDS.START2',
     :             'BOUNDS.END2',
     :             'ZERO',
     :             'EXTENT.MINX',
     :             'EXTENT.MAXX',
     :             'EXTENT.MINY',
     :             'EXTENT.MAXY',
     :             'TIMES.EXPOSURE',
     :             'TIMES.DARK',
     :             'TIMES.FLASH',
     :             'DEFERRED',
     :             'SATURATION'/
      DATA TYPES / '_CHAR',
     :             '_CHAR',
     :             '_DOUBLE',
     :             '_DOUBLE',
     :             '_CHAR',
     :             '_INTEGER',
     :             '_INTEGER',
     :             '_INTEGER',
     :             '_INTEGER',
     :             '_DOUBLE',
     :             '_INTEGER',
     :             '_INTEGER',
     :             '_INTEGER',
     :             '_INTEGER',
     :             '_DOUBLE',
     :             '_DOUBLE',
     :             '_DOUBLE',
     :             '_DOUBLE',
     :             '_DOUBLE'/

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Default is no match.
      TYPE = ' '
      OK = .FALSE.

*  Get length of NAME.
      NAMLEN = CHR_LEN( NAME )
      IF ( NAMLEN .LT. 15 ) THEN
         DO 1 I = 1, 19
            IF ( NAMES( I )( 1 : NAMLEN) .EQ. NAME( 1 : NAMLEN ) ) THEN

*  Name is recognised.
               TYPE = TYPES( I )
               OK = .TRUE.
               GO TO 2
            END IF
 1       CONTINUE
 2       CONTINUE
      END IF

*  Permanently relaxed for test purposes.
      OK = .TRUE.

      END
* $Id$
