      SUBROUTINE KPG1_ASPLT( IWCS, BOX, OPTS, IPLOT, STATUS )
*+
*  Name:
*     KPG1_ASPLT

*  Purpose:
*     Creates an AST Plot covering the current PGPLOT viewport.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASPLT(  IWCS, BOX, OPTS, IPLOT, STATUS )

*  Description:
*     This routine create a Plot covering the current PGPLOT viewport.
*     The bounds of the PGPLOT window are changed if necessary to ensure
*     that the PGPLOT world co-ordinate system corresponds to
*     millimetres from the bottom-left corner of the view surface. This
*     is the co-ordinate system used in the Base (GRAPHICS) Frame of
*     the returned Plot.

*  Arguments:
*     IWCS = INTEGER (Given)
*        The FrameSet to include in the Plot. May be AST__NULL.
*     BOX( 4 ) = DOUBLE PRECISION (Given)
*        An array holding the bounds of the area within the Base Frame of
*        IWCS which is to be mapped linearly onto the current PGPLOT
*        viewport. The first pair of values should give the co-ordinates
*        at the bottom-left corner of the plotting area and the second
*        pair should give the co-ordinates at the top-right corner. The
*        co-ordinate on the horizontal axis should be given first in
*        each pair.
*     OPTS = CHARACTER * ( * ) (Given)
*        A set of Plot attribute settings to be used when creating the
*        Plot.
*     IPLOT = INTEGER (Returned)
*        An AST pointer to the Plot. Returned equal to AST__NULL if an
*        error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The PGPLOT interface to the AGI library should be opened before
*     calling this routine.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-SEP-1998 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      INTEGER IWCS
      DOUBLE PRECISION BOX( 4 )
      CHARACTER OPTS*(*)

*  Arguments Returned:
      INTEGER IPLOT

*  Status:
      INTEGER STATUS               ! Global status

*  Local Variables:
      INTEGER BASFRM               ! Pointer to GRAPHICS Frame
      REAL BX( 4 )                 ! Bounds of plotting area
*.

*  Initialise returned values.
      IPLOT = AST__NULL

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that the current PGPLOT world-co-ordinate system is millimetres
*  from the bottom left corner of the view surface.
      CALL PGQVP( 2, BX( 1 ), BX( 3 ), BX( 2 ), BX( 4 ) )
      CALL PGSWIN( BX( 1 ), BX( 3 ), BX( 2 ), BX( 4 ) )

*  Create the Plot.
      IPLOT = AST_PLOT( IWCS, BX, BOX, OPTS, STATUS )

*  Set the Label and Unit attributes of the Base (GRAPHICS) Frame axes of
*  the Plot.
      BASFRM = AST_GETFRAME( IPLOT, AST__BASE, STATUS )

      CALL AST_SETC( BASFRM, 'Label(1)', 'Distance from left-hand '//
     :               'edge of screen', STATUS )
      CALL AST_SETC( BASFRM, 'Label(2)', 'Distance from bottom '//
     :               'edge of screen', STATUS )

      CALL AST_SETC( BASFRM, 'Unit(1)', 'mm', STATUS )
      CALL AST_SETC( BASFRM, 'Unit(2)', 'mm', STATUS )

      CALL AST_ANNUL( BASFRM, STATUS )

      END
