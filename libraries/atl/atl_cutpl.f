      SUBROUTINE ATL_CUTPL( IPLOT1, IFRM, DLBND, DUBND, IPLOT2,
     :                      STATUS )
*+
*  Name:
*     ATL_CUTPL

*  Purpose:
*     Create a Plot covering a sub-region of another Plot.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL_CUTPL( IPLOT1, IFRM, DLBND, DUBND, IPLOT2, STATUS )

*  Description:
*     This routine creates a new Plot with the same attributes as a
*     supplied Plot, but covering a sub-region within the world
*     coordinate system and graphics viewport.

*  Arguments:
*     IPLOT1 = INTEGER (Given)
*        The source Plot.
*     IFRM = INTEGER (Given)
*        Index of the Frame within IPLOT1 in which the bounds are supplied.
*     DLBND( * ) = DOUBLE PRECISION (Given)
*        The axis values at the lower left corner of the region to be
*        covered by the new Plot. The number of axis values supplied should
*        equal the number of axes in the Frame identified by IFRM.
*     DUBND( * ) = DOUBLE PRECISION (Given)
*        The axis values at the upper right corner of the region to be
*        covered by the new Plot. The number of axis values supplied should
*        equal the number of axes in the Frame identified by IFRM.
*     IPLOT2 = INTEGER (Returned)
*        The new Plot.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2006 Central Laboratory of the Research Councils.
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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-JUN-2006 (DSB):
*        Original version.
*     23-JUN-2006 (DSB):
*        Retain axis direction (lost by use of AST_MAPBOX).
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'ATL_PAR'          ! ATL constants

*  Arguments Given:
      INTEGER IPLOT1
      INTEGER IFRM
      DOUBLE PRECISION DLBND( * )
      DOUBLE PRECISION DUBND( * )

*  Arguments Returned:
      INTEGER IPLOT2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION BBOX( 4 )
      DOUBLE PRECISION D
      DOUBLE PRECISION T
      DOUBLE PRECISION XL( ATL__MXDIM )
      DOUBLE PRECISION XU( ATL__MXDIM )
      INTEGER MAP
      INTEGER NIN
      REAL GBOX( 4 )

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Transform the supplied bounds into the base (GRAPHICS) Frame of the
*  supplied Plot.
      MAP = AST_GETMAPPING( IPLOT1, IFRM, AST__BASE, STATUS )
      CALL AST_MAPBOX( MAP, DLBND, DUBND, .TRUE., 1, BBOX( 1 ),
     :                 BBOX( 3 ), XL, XU, STATUS )
      CALL AST_MAPBOX( MAP, DLBND, DUBND, .TRUE., 2, BBOX( 2 ),
     :                 BBOX( 4 ), XL, XU, STATUS )

*  Ensure the Mapped limits are the same way round as the supplied
*  limits. To do this, we transform the supplied points explicitly using
*  AST_TRANN.
      NIN = AST_GETI( AST_GETFRAME( IPLOT1, IFRM, STATUS ), 'Naxes',
     :                STATUS )
      CALL AST_TRANN( MAP, 1, NIN, 1, DLBND, .TRUE., 2, 1, XL, STATUS )
      CALL AST_TRANN( MAP, 1, NIN, 1, DUBND, .TRUE., 2, 1, XU, STATUS )
      CALL AST_ANNUL( MAP, STATUS )

*  Ensure that the bounds of the base Frame box found by AST_MAPBOX are
*  the same way round as the bounds found by transforming the supplied
*  positions.
      IF( XL( 1 ) .LT. XU( 1 ) .AND. BBOX( 1 ) .GT. BBOX( 3 ) .OR.
     :    XL( 1 ) .GT. XU( 1 ) .AND. BBOX( 1 ) .LT. BBOX( 3 ) ) THEN
         T = BBOX( 1 )
         BBOX( 1 ) = BBOX( 3 )
         BBOX( 3 ) = T
      END IF

      IF( XL( 2 ) .LT. XU( 2 ) .AND. BBOX( 2 ) .GT. BBOX( 4 ) .OR.
     :    XL( 2 ) .GT. XU( 2 ) .AND. BBOX( 2 ) .LT. BBOX( 4 ) ) THEN
         T = BBOX( 2 )
         BBOX( 2 ) = BBOX( 4 )
         BBOX( 4 ) = T
      END IF

*  Shrink the box slightly to cater for rounding errors.
      D = ( BBOX( 3 ) - BBOX( 1 ) )*1.0D-8
      BBOX( 1 ) = BBOX( 1 ) + D
      BBOX( 3 ) = BBOX( 3 ) - D

      D = ( BBOX( 4 ) - BBOX( 2 ) )*1.0D-8
      BBOX( 2 ) = BBOX( 2 ) + D
      BBOX( 4 ) = BBOX( 4 ) - D

*  Create a new Plot covering this area of GRAPHICS coords.
      GBOX( 1 ) = REAL( BBOX( 1 ) )
      GBOX( 2 ) = REAL( BBOX( 2 ) )
      GBOX( 3 ) = REAL( BBOX( 3 ) )
      GBOX( 4 ) = REAL( BBOX( 4 ) )
      IPLOT2 = AST_PLOT( IPLOT1, GBOX, BBOX, ' ', STATUS )

*  Remove the unnecessary GRAPHICS Frame inherited from IPLOT1. It's index
*  number within IPLOT2 will be one more than its index number in IPLOT1.
      CALL AST_REMOVEFRAME( IPLOT2,
     :                      AST_GETI( IPLOT1, 'Base', STATUS ) + 1,
     :                      STATUS )

*  Assign values to the public attributes of IPLOT2, copying them
*  (whether default or user-supplied) from IPLOT1.
      CALL ATL_CPPLA( IPLOT1, IPLOT2, .TRUE., STATUS )

      END
