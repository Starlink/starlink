      SUBROUTINE ARY1_PBND( IACB, PRIM, STATUS )
*+
*  Name:
*     ARY1_PBND

*  Purpose:
*     Determine if array bounds are consistent with a primitive array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_PBND( IACB, PRIM, STATUS )

*  Description:
*     The routine returns a logical value indicating whether the bounds
*     of an array are consistent with that array being a primitive
*     array. The value .TRUE. is returned if the lower pixel index
*     bound in each array dimension is 1. Otherwise, a .FALSE. value is
*     returned. The array is identified to this routine by its ACB
*     index.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the array entry in the ACB.
*     PRIM = LOGICAL (Returned)
*        Whether the array bounds are consistent with a primitive
*        array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Initialise.
*     -  Loop to inspect the lower bound of each array dimension.
*     -  The array cannot be primitive if the lower bound is not 1.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     12-FEB-1990 (RFWS):
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
*        ACB_LBND( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read)
*           Array lower pixel index bounds.
*        ACB_NDIM( ARY_MXACB ) = INTEGER (Read)
*           Number of array dimensions.

*  Arguments Given:
      INTEGER IACB

*  Arguments Returned:
      LOGICAL PRIM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for dimensions

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      PRIM = .TRUE.

*  Loop to inspect the lower bound of each array dimension.
      DO 1 I = 1, ACB_NDIM( IACB )

*  The array cannot be primitive if the lower bound is not 1.
         IF ( ACB_LBND( I, IACB ) .NE. 1 ) THEN
            PRIM = .FALSE.
            GO TO 2
         END IF
1     CONTINUE
2     CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_PBND', STATUS )

      END
