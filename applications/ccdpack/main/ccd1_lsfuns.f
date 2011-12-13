      SUBROUTINE CCD1_LSFUNS( X, Y, PXY, N, DIFF, STATUS )
*+
*  Name:
*     CCD1_LSFUNS

*  Purpose:
*     Part of CCD1_LSFUN1, forms difference in positions.

*  Language:
*     Starlink Fortran 77

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
*     16-JUL-1992 (PDRAPER):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER N
      DOUBLE PRECISION X( N )
      DOUBLE PRECISION Y( N )
      DOUBLE PRECISION PXY( N, 2 )

*  Arguments Returned:
      DOUBLE PRECISION DIFF( N * 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable
      INTEGER J                  ! Counter for sums
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop over A and B forming sum of the squared differences.
      DO 1 I = 1, N
         DIFF( I ) = X( I ) - PXY( I, 1 )
 1    CONTINUE
      J = N
      DO 2 I = 1, N
         J = J + 1
         DIFF( J ) = Y( I ) - PXY( I, 2 )
 2    CONTINUE
      END
* $Id$
