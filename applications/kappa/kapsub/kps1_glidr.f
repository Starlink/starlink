      SUBROUTINE KPS1_GLIDR( LBND1, LBND2, UBND1, UBND2, DIN, NPOS,
     :                       PIXPOS, STATUS )
*+
*  Name:
*     KPS1_GLIDR

*  Purpose:
*     Store bad pixels positions in a supplied array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_GLIDR( LBND1, LBND2, UBND1, UBND2, DIN, NPOS, PIXPOS, STATUS )

*  Description:
*     This routine stores the pixel co-ordinates at the centre of all the
*     bad pixels in the supplied array.

*  Arguments:
*     LBND1 = INTEGER (Given)
*        Lower pixel index on axis 1.
*     LBND2 = INTEGER (Given)
*        Lower pixel index on axis 2.
*     UBND1 = INTEGER (Given)
*        Upper pixel index on axis 1.
*     UBND2 = INTEGER (Given)
*        Upper pixel index on axis 2.
*     DIN( LBND1:UBND1, LBND2:UBND2 ) = REAL (Given)
*        The input data array.
*     NPOS = INTEGER (Returned)
*        The number of returned positions. Returned equal to zero if
*        there are no bad pixels in the array,
*     PIXPOS( NPOS, 2 ) = DOUBLE PRECISION (Returned)
*        The array in which to store the first NPOS bad pixel positions
*        found in the supplied data array.
*     STATUS = INTEGER (Given and Returned)
*        The inherited status.

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-MAR-2000 (DSB):
*        Initial version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Global SSE parameters
      INCLUDE 'PRM_PAR'        ! VAL__ constants

*  Arguments Given:
      INTEGER LBND1
      INTEGER LBND2
      INTEGER UBND1
      INTEGER UBND2
      REAL DIN( LBND1:UBND1, LBND2:UBND2 )
      INTEGER NPOS

*  Arguments Returned:
      DOUBLE PRECISION PIXPOS( NPOS, 2 )

*  Global Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER COUNT,I,J
*.

*  Check inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

* Initialise the number of bad pixels found so far.
      COUNT = 0

*  Loop round the data array.
      DO J = LBND2, UBND2
         DO I = LBND1, UBND1

*  See if this pixel is bad.
            IF( DIN( I, J ) .EQ. VAL__BADR ) THEN

*  If so, increase the count of bad pixels.
               COUNT = COUNT + 1

*  If the array is not full, store the pixel co-ordinates at the centre
*  of this pixel.
               IF( COUNT .LE. NPOS ) THEN
                  PIXPOS( COUNT, 1 ) = DBLE( I ) - 0.5D0
                  PIXPOS( COUNT, 2 ) = DBLE( J ) - 0.5D0
               END IF

            END IF

         END DO

      END DO

      END
