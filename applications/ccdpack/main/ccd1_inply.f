      SUBROUTINE CCD1_INPLY( VERTX, VERTY, NVERT, INPX, INPY, NINP,
     :                       INSIDE, STATUS )
*+
*  Name:
*     CCD1_INPLY

*  Purpose:
*     Select points from list which fall within a polygon.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_INPLY( VERTX, VERTY, NVERT, INPX, INPY, NINP,
*                      INSIDE, STATUS )

*  Description:
*     This routine takes an input array of two-dimensional points, and
*     marks those which fall within a specified polygon.  For each
*     point which falls inside the polygon, it sets to TRUE the
*     corresponding element of an array of flags.  It does not touch
*     elements corresponding to points which do not fall inside the
*     polygon, so that successive calls of the routine result in a
*     boolean OR-like semantics, allowing inclusion in any one of a
*     group of polygons to be tested.
*
*     The vertices of the polygon must be supplied in clockwise or
*     anticlockwise order.  The first vertex may optionally be repeated
*     as the last one.

*  Arguments:
*     VERTX( NVERT ) = DOUBLE PRECISION (Given)
*        X coordinates of polygon vertices.
*     VERTY( NVERT ) = DOUBLE PRECISION (Given)
*        Y coordinates of polygon vertices.
*     NVERT = INTEGER (Given)
*        Number of vertices in polygon.
*     INPX( NINP ) = DOUBLE PRECISION (Given)
*        X coordinates of input list of points.
*     INPY( NINP ) = DOUBLE PRECISION (Given)
*        Y coordinates of input list of points.
*     NINP = INTEGER (Given)
*        Number of points in input list.
*     INSIDE( NINP ) = LOGICAL (Given and Returned)
*        An array of flags; if the point ( INPX( I ), INPY( I ) ) is
*        within the polygon specified by VERTX and VERTY then INSIDE( I )
*        will be set TRUE.
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

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
*     MBT: Mark Taylor (STARLINK - IoA)
*     {enter_new_authors_here}

*  History:
*     8-FEB-1999 (MBT):
*        Original version.
*     20-FEB-2001 (MBT):
*        Modified to set a logical array instead of producing an output
*        list of points.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NVERT
      DOUBLE PRECISION VERTX( NVERT )
      DOUBLE PRECISION VERTY( NVERT )
      INTEGER NINP
      DOUBLE PRECISION INPX( NINP )
      DOUBLE PRECISION INPY( NINP )

*  Arguments Given and Returned:
      LOGICAL INSIDE( NINP )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable
      INTEGER INOUT              ! Inclusion status of a point in polygon

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Go through list of input points.
      DO 1 I = 1, NINP

*  Determine whether this point is inside the polygon.
         CALL CCD1_PNPLY( INPX( I ), INPY( I ), VERTX, VERTY, NVERT,
     :                    INOUT, STATUS )

*  Update the flag array accordingly.
         IF ( INOUT .GE. 0 ) INSIDE( I ) = .TRUE.
 1    CONTINUE

      END
* $Id$
