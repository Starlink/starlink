      SUBROUTINE ARD1_CTOV( NDIM, LBND, UBND, INDX, VA, STATUS )
*+
*  Name:
*     ARD1_CTOV

*  Purpose:
*     Convert from N-d Cartesian indices to vector address.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_CTOV( NDIM, LBND, UBND, INDX, VA, STATUS )

*  Description:
*     The supplied Cartesian indices are converted into the vector
*     address for the same point (given the supplied bounds of the
*     array). If any of the Cartesian indices are outside the range
*     specified by UBND and LBND, then VA is returned as zero.

*  Arguments:
*     NDIM = INTEGER (Given)
*        The number of dimensions in the array.
*     LBND( NDIM ) = INTEGER (Given)
*        The lower pixel index bounds of the array.
*     UBND( NDIM ) = INTEGER (Given)
*        The upper pixel index bounds of the array.
*     INDX( NDIM ) = DOUBLE PRECISION (Given)
*        The Cartesian pixel indices.
*     VA = INTEGER (Returned)
*        The vector address.
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
*     1-MAR-1994 (DSB):
*        Original version.
*     18-JUL-2001 (DSB):
*        Modified for ARD version 2.0.
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
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )
      DOUBLE PRECISION INDX( NDIM )

*  Arguments Returned:
      INTEGER VA

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :        I,                 ! Loop count
     :        IND,               ! Current index value
     :        P                  ! Object size of current dimensionality

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the vector address.
      VA = 1

*  Initialise the size of the current object (pixel, row, plane, cube,
*  etc).
      P = 1

*  Loop round each dimension.
      DO I = 1, NDIM

*  Store the Cartesion index for this axis.
         IND = NINT( INDX( I ) )

*  If it is within the array bounds, increment the vector address.
         IF( IND .GE. LBND( I ) .AND. IND .LE. UBND( I ) ) THEN
            VA = VA + ( IND - LBND( I ) )*P
            P = P*( UBND( I ) - LBND( I ) + 1 )

*  Otherwise, set the vector address to zero and abort.
         ELSE
            VA = 0
            GO TO 999

         END IF

      END DO

*  Jump to here if an index is out of bounds.
 999  CONTINUE

      END
