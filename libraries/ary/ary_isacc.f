      SUBROUTINE ARY_ISACC( IARY, ACCESS, ISACC, STATUS )
*+
*  Name:
*     ARY_ISACC

*  Purpose:
*     Determine whether a specified type of array access is available.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_ISACC( IARY, ACCESS, ISACC, STATUS )

*  Description:
*     The routine determines whether a specified type of access to an
*     array is available, or whether it has been disabled. If access is
*     not available, then any attempt to access the array in this way
*     will fail.

*  Arguments:
*     IARY = INTEGER (Given)
*        Array identifier.
*     ACCESS = CHARACTER * ( * ) (Given)
*        The type of array access required: 'BOUNDS', 'DELETE',
*        'SHIFT', 'TYPE' or 'WRITE' (see the Notes section for
*        details).
*     ISACC = LOGICAL (Returned)
*        Whether the specified type of access is available.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The valid access types control the following operations on the
*     array:
*     -  'BOUNDS' permits the pixel-index bounds of a base array to be
*     altered.
*     -  'DELETE' permits deletion of the array.
*     -  'SHIFT' permits pixel-index shifts to be applied to a base
*     array.
*     -  'TYPE' permits the data type of the array to be altered.
*     -  'WRITE' permits new values to be written to the array, or the
*     array's state to be reset.

*  Algorithm:
*     -  Import the array identifier.
*     -  Determine if the specified type of access is permitted.
*     -  If an error occurred, then report context information.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-SEP-1989 (RFWS):
*        Original version.
*     9-OCT-1989 (RFWS):
*        Corrected minor prologue spelling error.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants

*  Arguments Given:
      INTEGER IARY
      CHARACTER * ( * ) ACCESS

*  Arguments Returned:
      LOGICAL ISACC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to the array entry in the ACB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the array identifier.
      CALL ARY1_IMPID( IARY, IACB, STATUS )

*  Determine whether access is available.
      CALL ARY1_ACCOK( IACB, ACCESS, ISACC, STATUS )

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARY_ISACC_ERR',
     :   'ARY_ISACC: Error enquiring whether access to an array is ' //
     :   'available.', STATUS )
         CALL ARY1_TRACE( 'ARY_ISACC', STATUS )
      END IF

      END
