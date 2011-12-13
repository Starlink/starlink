      SUBROUTINE KPS1_STQA1( NEL, IN, OUT, STATUS )
*+
*  Name:
*     KPS1_STQA1

*  Purpose:
*     Convert an integer ARD mask to a real IRQ mask.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_STQA1( NEL, IN, OUT, STATUS )

*  Description:
*     The input vector is copied to the output vector, with pixel
*     values being transformed as follows: positive values in the input
*     are transformed to bad values in the output, zero values in the
*     input remain zero in the output.

*  Arguments:
*     NEL = INTEGER (Given)
*        The size of the input and output vectors.
*     IN( NEL ) = INTEGER (Given)
*        The input vector.
*     OUT( NEL ) = REAL (Returned)
*        The output vector.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council. All
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-DEC-1994 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'PRM_PAR'          ! VAL__ constants.

*  Arguments Given:
      INTEGER NEL
      INTEGER IN( NEL )

*  Arguments Returned:
      REAL OUT( NEL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop count.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      DO I = 1, NEL

         IF( IN( I ) .GT. 0 ) THEN
            OUT( I ) = VAL__BADR

         ELSE
            OUT( I ) = 0.0

         END IF

      END DO

      END
