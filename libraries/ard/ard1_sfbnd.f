      SUBROUTINE ARD1_SFBND( TYPE, NPAR, PAR, MAP, FRM, LBINTB,
     :                       UBINTB, STATUS )
*+
*  Name:
*     ARD1_SFBND

*  Purpose:
*     Find the bounding box of a region in a SkyFrame user system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_SFBND( TYPE, NPAR, PAR, MAP, FRM, LBINTB,
*                      UBINTB, STATUS )

*  Description:
*     This function returns the bounds of a box in pixel coords which
*     encompasses the given region, assuming that the user coords Frame
*     is a spherical coordinate system.

*  Arguments:
*     TYPE = INTEGER (Given)
*        THe symbolic value representing the region type.
*     NPAR = INTEGER (Given)
*        The size of the PAR array.
*     PAR( NPAR ) = DOUBLE PRECISION (Given)
*        The parameters defining the region.
*     MAP = INTEGER (Given)
*        A pointer to the AST Mapping from pixel to user coords.
*     FRM = INTEGER (Given)
*        A pointer to the AST Frame describing user coords.
*     LBINTB( 2 ) = INTEGER (Returned)
*        The lower pixel bounds of the smallest box which contains all
*        interior points in the region.
*     UBINTB( 2 ) = INTEGER (Returned)
*        The upper pixel bounds of the smallest box which contains all
*        interior points in the region.
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
*     30-MAR-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'AST_PAR'          ! AST_ constants and functions
      INCLUDE 'ARD_CONST'        ! ARD_ private constants

*  Arguments Given:
      INTEGER TYPE
      INTEGER NPAR
      DOUBLE PRECISION PAR( NPAR )
      INTEGER MAP
      INTEGER FRM

*  Arguments Returned:
      INTEGER LBINTB( 2 )
      INTEGER UBINTB( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION
     :        RLB,                 ! Axis lower bound in pixel coords
     :        RUB,                 ! Axis upper bound in pixel coords
     :        UBXLB( ARD__MXDIM ), ! Lower bounds of bounding box in user coords
     :        UBXUB( ARD__MXDIM ), ! Upper bounds of bounding box in user coords
     :        XL( ARD__MXDIM ),    ! User coords at axis lower bound
     :        XU( ARD__MXDIM )     ! User coords at axis upper bound

      INTEGER
     :        I                    ! Loop count

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a bounding box for the region in user coords.
      CALL ARD1_UBBOX( 2, FRM, TYPE, NPAR, PAR, UBXLB, UBXUB, STATUS )

*  Find the corresponding bounding box in pixel indices.
      DO I = 1, 2

*  Find the bounds in pixel coords on the current axis.
         CALL AST_MAPBOX( MAP, UBXLB, UBXUB, .FALSE., I, RLB, RUB,
     :                    XL, XU, STATUS )

*  Convert from pixel coords to pixel indices and store as the new
*  interior bounding box. INclude a safety margin of 1 pixel.
         LBINTB( I ) = INT( RLB )
         UBINTB( I ) = INT( RUB ) + 2

      END DO

      END
