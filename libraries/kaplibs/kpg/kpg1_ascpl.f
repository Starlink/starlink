      SUBROUTINE KPG1_ASCPL( IPLOT1, IFRM, DLBND, DUBND, IPLOT2, 
     :                       STATUS )
*+
*  Name:
*     KPG1_ASCPL

*  Purpose:
*     Create a Plot covering a sub-region of another Plot.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASCPL( IPLOT1, IFRM, DLBND, DUBND, IPLOT2, STATUS )

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
*        The lower bounds of the region to be covered by the new Plot.
*        The number of axis values supplied should equal the number of
*        axes in the Frame identified by IFRM.
*     DUBND( * ) = DOUBLE PRECISION (Given)
*        The upper bounds of the region to be covered by the new Plot.
*        The number of axis values supplied should equal the number of
*        axes in the Frame identified by IFRM.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-MAY-2006 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'NDF_PAR'          ! NDF constants 

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
      DOUBLE PRECISION XL( NDF__MXDIM )
      DOUBLE PRECISION XU( NDF__MXDIM )
      INTEGER MAP
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
      CALL AST_ANNUL( MAP, STATUS )

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

*  Remove the unnecessary GRAPHICS Frame inherited from IPLOT1. It'sindex
*  number within IPLOT2 will be one more than its index number in IPLOT1.
      CALL AST_REMOVEFRAME( IPLOT2, 
     :                      AST_GETI( IPLOT1, 'Base', STATUS ) + 1,
     :                      STATUS )

*  Copy the public attributes from IPLOT1 to IPLOT2.
      CALL KPG1_ASCPA( IPLOT1, IPLOT2, STATUS )

      END
