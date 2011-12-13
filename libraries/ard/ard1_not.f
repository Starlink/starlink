      SUBROUTINE ARD1_NOT( NDIM, LBND, UBND, MSKSIZ, B, LBEXTB,
     :                     UBEXTB, LBINTB, UBINTB, STATUS )
*+
*  Name:
*     ARD1_NOT

*  Purpose:
*     Perform an NOT operation on two arrays

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_NOT( NDIM, LBND, UBND, MSKSIZ, B, LBEXTB,
*                    UBEXTB, LBINTB, UBINTB, STATUS )

*  Description:
*     All values in the supplied array are inverted (interior goes to
*     exterior, exterior goes to interior background), and the interior
*     and exterior bounding boxes are swapped.

*  Arguments:
*     NDIM = INTEGER (Given)
*        The number of dimensions in each array.
*     LBND( NDIM ) = INTEGER (Given)
*        The lower pixel index bounds of each array.
*     UBND( NDIM ) = INTEGER (Given)
*        The upper pixel index bounds of each array.
*     MSKSIZ = INTEGER (Given)
*        The total number of elements in each array.
*     B( MSKSIZ ) = INTEGER (Given and Returned)
*        The operand array (in vector form). This should hold zero for
*        all exterior points, and a positive value for all interior
*        points. The results of the operation are written back into
*        this array.
*     LBEXTB( NDIM ) = INTEGER (Given and Returned)
*        The lower pixel bounds of the smallest box which contains all
*        exterior points in B. A value of VAL__MAXI for element 1 is
*        used to indicate an infinite box, and a value of VAL__MINI for
*        element 1 is used to indicate a zero sized box.
*     UBEXTB( NDIM ) = INTEGER (Given and Returned)
*        The upper pixel bounds of the smallest box which contains all
*        exterior points in B.
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
*     28-FEB-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ARD_CONST'        ! ARD private constants

*  Arguments Given:
      INTEGER NDIM
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )
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
     :        I,                 ! Loop count
     :        TEMP               ! Temporary storage

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop round every element in the supplied array.
      DO I = 1, MSKSIZ

*  Invert the current pixel.
         IF( B( I ) .EQ. 0 ) THEN
            B( I ) = 1
         ELSE
            B( I ) = 0
         END IF

      END DO

*  Now swap the interior and exterior bounding boxes.
      DO I = 1, NDIM
         TEMP = LBINTB( I )
         LBINTB( I ) = LBEXTB( I )
         LBEXTB( I ) = TEMP

         TEMP = UBINTB( I )
         UBINTB( I ) = UBEXTB( I )
         UBEXTB( I ) = TEMP

      END DO

      END
