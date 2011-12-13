      SUBROUTINE ARD1_WHO( RINDEX, NDIM, MSKSIZ, B, LBEXTB, UBEXTB,
     :                     LBINTB, UBINTB, STATUS )
*+
*  Name:
*     ARD1_WHO

*  Purpose:
*     Initialise an array to hold a WHOLE region.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_WHO( RINDEX, NDIM, MSKSIZ, B, LBEXTB, UBEXTB, LBINTB,
*                    UBINTB, STATUS )

*  Description:
*     The array B is filled with interior values. The interior bounding
*     box is returned "infinite" and the exterior bounding box is
*     returned "null".

*  Arguments:
*     RINDEX = INTEGER (Given)
*        The value to use to represent interior points.
*     NDIM = INTEGER (Given)
*        The number of dimensions in the mask.
*     MSKSIZ = INTEGER (Given)
*        The total number of elements in the B array.
*     B( MSKSIZ ) = INTEGER (Given and Returned)
*        The array.
*     LBEXTB( NDIM ) = INTEGER (Given and Returned)
*        The lower pixel bounds of the smallest box which contains all
*        exterior points in B. A value of VAL__MAXI for element 1 is
*        returned to indicate an "infinite" box. Other elements should
*        be ignored.
*     UBEXTB( NDIM ) = INTEGER (Given and Returned)
*        The upper pixel bounds of the smallest box which contains all
*        exterior points in B. The returned values should be ignored
*        since the box is "infinite".
*     LBINTB( NDIM ) = INTEGER (Given and Returned)
*        The lower pixel bounds of the smallest box which contains all
*        interior points in B. A value of VAL__MAXI for element 1 is
*        used to indicate an infinite box, and a value of VAL__MINI for
*        element 1 is used to indicate a zero sized box.
*     UBINTB( NDIM ) = INTEGER (Given and Returned)
*        The upper pixel bounds of the smallest box which contains all
*        interior points in B.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     15-APR-1994 (DSB):
*        Original version.
*     26-JUN-2001 (DSB):
*        Modified for ARD version 2.0.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants

*  Arguments Given:
      INTEGER RINDEX
      INTEGER NDIM
      INTEGER MSKSIZ

*  Arguments Given and Returned:
      INTEGER B( MSKSIZ )
      INTEGER LBEXTB( NDIM )
      INTEGER UBEXTB( NDIM )
      INTEGER LBINTB( NDIM )
      INTEGER UBINTB( NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :        I                  ! Loop count

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Fill the array with interior values.
      DO I = 1, MSKSIZ
         B( I ) = RINDEX
      END DO

*  Return VAL__MAXI for LBINTB( 1 ) to indicate that the interior
*  bounding box is infinite.
      LBINTB( 1 ) = VAL__MAXI

*  Return VAL__MINI for LBEXTB( 1 ) to indicate that the exterior
*  bounding box is null.
      LBEXTB( 1 ) = VAL__MINI

      END
