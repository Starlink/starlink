      SUBROUTINE KPS1_CHSHA( NTILE, DIMS, ASP, MARGIN, SHAPE, STATUS )
*+
*  Name:
*     KPS1_CHSHA

*  Purpose:
*     Calculates the shape of the grid of channel images.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CHSHA( NTILE, DIMS, SHAPE, STATUS )

*  Description:
*     This routine determines the arrangement of a supplied number of
*     equally shaped tiles in a two-dimensional grid, such that the grid
*     is as close to a specified aspect ratio as possible.  Both the
*     filling factor of the grid and the area covered are used to select
*     the most-efficient arrnagement.  This routine serves CHANMAP.

*  Arguments:
*     NTILE = INTEGER (Given)
*        The number of tiles.
*     DIMS( 2 ) = INTEGER*8 (Given)
*        The dimensions of a tile.
*     ASP = REAL (Given)
*        The target aspect ratio.
*     MARGIN = REAL (Given)
*        Fractional margin around the tiles.  A value of 0.0 indicates
*        abutting tiles.
*     SHAPE( 2 ) = INTEGER (Returned)
*        The number of tiles to place along each axis.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - There is no validation of the supplied arguments.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     MJC: Malcolm J. Currie  (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2006 April 26 (MJC):
*        Original version.
*     13-FEB-2020 (DSB):
*        Support huge NDFs.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NTILE
      INTEGER*8 DIMS( 2 )
      REAL ASP
      REAL MARGIN

*  Arguments Returned:
      INTEGER SHAPE( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER RANGE              ! How much to deviate from the square
      PARAMETER ( RANGE = 4 )

*  Local Variables:
      REAL ASPRAT                ! Ratio of desired to image aspect
                                 ! ratios
      REAL AREA                  ! Area covered by the configuration
      REAL BEST                  ! Best efficiency
      REAL EFFIC                 ! Efficiency of the configuration
      REAL CASP                  ! Aspect ratio of current configuration
      INTEGER GM                 ! Geometric mean of the no. of tiles
      INTEGER I                  ! Loop counter
      INTEGER LX                 ! Lowest number of tiles in x
      INTEGER LY                 ! Lowest number of tiles in y
      INTEGER HX                 ! Highest number of tiles in x
      INTEGER HY                 ! Highest number of tiles in y
      INTEGER J                  ! Loop counter
      INTEGER NC                 ! Number of tiles in current
                                 ! configuration
      REAL TASP                  ! Aspect ratio of a tile
      REAL USED                  ! Fraction of current configuration
                                 ! that would be used
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the tile's aspect ratio.
      TASP = REAL( DIMS( 1 ) ) / REAL( DIMS( 2 ) )

*  Find the ratio of the target to tile aspect ratios.  The idea is
*  to match the shape of the images best to the chosen map shape, and
*  tile accordingly.  So if this ratio is near 1.0, the number of tiles
*  in each axis should be the same, i.e. the next enclosing square.
*  Smaller than 1.0 it means that more images will fit across the width
*  of the base picture than will fit the height.
      ASPRAT = SQRT( ASP / TASP )

*  Now find the number of tiles along the side of a square grid to
*  accommodate all the channels.
      GM = NINT( SQRT( REAL( NTILE ) ) + 0.49999 )

*  To determine the best ratio, create various ratios around the square
*  +/- RANGE tiles.

*  The image has larger aspect ratio than the target.  There will be
*  fewer tiles along the x direction than the y.
      IF ( ASPRAT .LE. 1.0 ) THEN
         LX = MAX( 1, GM - RANGE )
         LY = GM
         HX = GM
         HY = GM + RANGE

*  The image has smaller aspect ratio than the target.  There will be
*  fewer tiles along the y direction than the x.
      ELSE
         LX = GM
         LY = MAX( 1, GM - RANGE )
         HX = GM + RANGE
         HY = GM
      END IF

      BEST = -1.0
      SHAPE( 1 ) = -1
      SHAPE( 2 ) = -1

*  Loop through the various combinations of pictures along each axis.
      DO I = LX, HX
         DO J = LY, HY

*  Can reject immediately if the number of tiles in the arrangement is
*  fewer than the number of images.
            NC = I * J
            IF ( NC .GE. NTILE ) THEN

*  We need to determine the fraction of the plotting area that would
*  contain the channel images.  This is the fraction of used frames
*  in the grid, times the fraction of each frame displaying the image.
               USED = REAL( NTILE ) / REAL( NC )

*  The formula for area depends on whether the image is limited in x or
*  y.  Find the aspect ratio of a single grid.  If this is greater than
*  the image aspect ratio, means the image is y-axis limited.
               CASP = ASP * REAL( J ) / REAL( I )
               IF ( CASP .GE. TASP ) THEN
                  AREA = ( ( 1.0 - 2.0 * MARGIN )**2 ) * TASP / CASP
               ELSE
                  AREA = ( ( 1.0 - 2.0 * MARGIN )**2 ) / TASP * CASP
               END IF

*  Determine the efficiency.
               EFFIC = AREA * USED

*  Test whether this gets closer to the desired aspect ratio, while
*  still offering sufficient slots for the number of images.
               IF ( EFFIC .GT. BEST ) THEN
                  BEST = EFFIC
                  SHAPE( 1 ) = I
                  SHAPE( 2 ) = J
               END IF
            END IF
         END DO
      END DO

      END
