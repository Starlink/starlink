      SUBROUTINE ARY_ISMAP( IARY, MAPPED, STATUS )
*+
*  Name:
*     ARY_ISMAP

*  Purpose:
*     Determine if an array is currently mapped.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_ISMAP( IARY, MAPPED, STATUS )

*  Description:
*     The routine returns a logical value indicating whether an array is
*     currently mapped for access through the identifier supplied.

*  Arguments:
*     IARY = INTEGER (Given)
*        Array identifier.
*     MAPPED = LOGICAL (Returned)
*        Whether the array is mapped for access through the IARY
*        identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Import the array identifier.
*     -  Determine whether the array is mapped from its MCB index
*     stored in the ACB.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-AUG-1989 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants

*  Global Variables:
      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_IMCB( ARY__MXACB ) = INTEGER (Read)
*           Index to the array's mapping entry in the MCB.

*  Arguments Given:
      INTEGER IARY

*  Arguments Returned:
      LOGICAL MAPPED

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to the array entry in the ACB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the array identifier.
      CALL ARY1_IMPID( IARY, IACB, STATUS )

*  The array is mapped if its MCB index in the ACB is not zero.
      IF ( STATUS .EQ. SAI__OK ) THEN
         MAPPED = ACB_IMCB( IACB ) .NE. 0

*  If an error occurred, then report context information and call the
*  error tracing routine.
      ELSE
         CALL ERR_REP( 'ARY_ISMAP_ERR',
     :   'ARY_ISMAP: Error determining whether an array is mapped ' //
     :   'for access.', STATUS )
         CALL ARY1_TRACE( 'ARY_ISMAP', STATUS )
      END IF

      END
