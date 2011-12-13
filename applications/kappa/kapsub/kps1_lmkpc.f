      SUBROUTINE KPS1_LMKPC( NDIM, DIM, EL, PIXCEN, STATUS )
*+
*  Name:
*     KPS1_LMKPC

*  Purpose:
*     Store GRID Frame pixel centres in an array for LISTMAKE.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LMKPC( NDIM, DIM, EL, PIXCEN, STATUS )

*  Description:
*     This routine stores the GRID Frame co-ordinates of every pixel
*     in the supplied array.

*  Arguments:
*     NDIM = INTEGER (Given)
*        The number of axes.
*     DIM( NDIM ) = INTEGER (Given)
*        The dimension of each axis.
*     EL = INTEGER (Given)
*        The total number of pixels.
*     PIXCEN( EL, NDIM ) = DOUBLE PRECISION (Returned)
*        The returned pixel centre positions.
*     STATUS = INTEGER (Given)
*        Global status value.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
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
*     25-SEP-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      INTEGER NDIM
      INTEGER DIM( NDIM )
      INTEGER EL

*  Arguments Returned:
      DOUBLE PRECISION PIXCEN( EL, NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Position count
      INTEGER J( NDF__MXDIM )    ! pixel indices
      INTEGER K                  ! Axis count
*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the indices of the current pixel.
      DO K = 1, NDIM
         J( K ) = 1
      END DO

*  Do each pixel.
      DO I = 1, EL

*  Store the required GRID co-ordinates.
         DO K = 1, NDIM
            PIXCEN( I, K ) = DBLE( J( K ) )
         END DO

*  Increment the pixel indices.
         J( 1 ) = J( 1 ) + 1

         K = 1
         DO WHILE( K .LE. NDIM .AND. J( K ) .GT. DIM( K ) )
            J( K ) = 1
            K = K + 1
            J( K ) = J( K ) + 1
         END DO

      END DO

      END
