      SUBROUTINE KPS1_DSBOR( XL, XU, YL, YU, BORWID, BORCI, STATUS )
*+
*  Name:
*     KPS1_DSBOR

*  Purpose:
*     Plots a border about a rectangular region.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_DSBOR( XL, XU, YL, YU, BORWID, BORCI, STATUS )

*  Description:
*     This routine plots a coloured border about a rectangular region
*     within the current viewport or zone.  Clipping may occur if the
*     border lies outside the current viewport or zone.  Four separate
*     cell arrays are drawn for each side of the border.
*
*     A bad status is returned if there is an error within GKS itself
*     during plotting of the cell arrays.

*  Arguments:
*     XL = REAL (Given)
*        The lower x world co-ordinate bound of the rectangular region
*        to be surrounded by a border.
*     XU = REAL (Given)
*        The upper x world co-ordinate bound of the rectangular region
*        to be surrounded by a border.
*     YL = REAL (Given)
*        The lower y world co-ordinate bound of the rectangular region
*        to be surrounded by a border.
*     YU = REAL (Given)
*        The upper y world co-ordinate bound of the rectangular region
*        to be surrounded by a border.
*     BORWID( 2 ) = REAL (Given)
*        The width of the border in the x and y directions.
*     BORCI = INTEGER (Given)
*        The colour index of the border.  It should lie within the
*        range of colour indices for the display device.  This routine
*        does not check this.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     -  GKS or SGS should be active and there should be a current
*     viewport or zone.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council. All
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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 July 23 (MJC):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      REAL XL
      REAL XU
      REAL YL
      REAL YU
      REAL BORWID( 2 )
      INTEGER BORCI

*  Status:
      INTEGER STATUS             ! Global status

*.

*    Check the inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Draw four cell arrays, each containing one "pixel", in the
*    requested colour index.
*    ==========================================================

*    Draw the full borders to prevent any gaps at the corners.

*    Left border.

      CALL KPG1_GCA( XL - BORWID( 1 ), YL - BORWID( 2 ), XL,
     :               YU + BORWID( 2 ), 1, 1, 1, 1, BORCI, STATUS )

*    Top border.

      CALL KPG1_GCA( XL - BORWID( 1 ), YU, XU + BORWID( 1 ),
     :               YU + BORWID( 2 ), 1, 1, 1, 1, BORCI, STATUS )

*    Right border.

      CALL KPG1_GCA( XU, YL - BORWID( 2 ), XU + BORWID( 1 ),
     :               YU + BORWID( 2 ), 1, 1, 1, 1, BORCI, STATUS )

*    Bottom border.

      CALL KPG1_GCA( XL - BORWID( 1 ), YL - BORWID( 2 ),
     :               XU + BORWID( 1 ), YL, 1, 1, 1, 1, BORCI, STATUS )

*    Check whether there has ben an error within GKS.

      CALL GKS_GSTAT( STATUS )

      END
