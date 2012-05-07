      SUBROUTINE ARY1_INTYP( TYPE, NUMER, STATUS )
*+
*  Name:
*     ARY1_INTYP

*  Purpose:
*     Is an HDS type numeric?

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_INTYP( TYPE, NUMER, STATUS )

*  Description:
*     The routine returns a logical value indicating if the HDS data
*     type string supplied represents a primitive numeric type.

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        The data type string to be tested (case insensitive).
*     NUMER = LOGICAL (Returned)
*        Whether the data type is numeric. A .FALSE. value is returned
*        unless TYPE represents a primitive numeric HDS data type.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Test TYPE against each of the primitive numeric data type
*     strings, assigning the NUMER value accordingly.

*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-JUN-1989  (RFWS):
*        Original version.
*     2012-05-07 (TIMJ):
*        Add _INT64 support.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants

*  Arguments Given:
      CHARACTER * ( * ) TYPE

*  Arguments Returned:
      LOGICAL NUMER

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Test against each of the primitive numerical data types, setting
*  NUMER to .TRUE. if a match is found.
      IF ( CHR_SIMLR( TYPE, '_BYTE' ) .OR.
     :     CHR_SIMLR( TYPE, '_UBYTE' ) .OR.
     :     CHR_SIMLR( TYPE, '_DOUBLE' ) .OR.
     :     CHR_SIMLR( TYPE, '_INTEGER' ) .OR.
     :     CHR_SIMLR( TYPE, '_REAL' ) .OR.
     :     CHR_SIMLR( TYPE, '_WORD' ) .OR.
     :     CHR_SIMLR( TYPE, '_UWORD' ) .OR.
     :     CHR_SIMLR( TYPE, '_INT64' ) ) THEN
         NUMER = .TRUE.

*  If the type is not numeric, set NUMER to .FALSE..
      ELSE
         NUMER = .FALSE.
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_INTYP', STATUS )

      END
