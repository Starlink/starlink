      SUBROUTINE ARD1_TRCOP( NDIM, CIN, COUT, STATUS )
*+
*  Name:
*     ARD1_TRCOP

*  Purpose:
*     Copy the co-efficients of a linear transformation

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_TRCOP( NDIM, CIN, COUT, STATUS )

*  Description:
*     The co-efficients supplied in CIN are copied to COUT.

*  Arguments:
*     NDIM = INTEGER (Given)
*        The dimensionality of the co-ordinate system.
*     CIN( * ) = REAL (Given)
*        The input co-efficients. The array should hold
*        NDIM*( NDIM + 1 ) values.
*     COUT( * ) = REAL (Returned)
*        The output co-efficients. The array should hold
*        NDIM*( NDIM + 1 ) values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-FEB-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NDIM
      REAL CIN( * )

*  Arguments Returned:
      REAL COUT( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop count

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy the co-efficients.
      DO I = 1, NDIM*( NDIM + 1 )
         COUT( I ) = CIN( I )
      END DO

      END
