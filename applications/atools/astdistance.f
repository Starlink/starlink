      SUBROUTINE ASTDISTANCE( STATUS )
*+
*  Name:
*     ASTDISTANCE

*  Purpose:
*     Calculate the distance between two points in a Frame

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTDISTANCE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application finds the distance between two points whose Frame
*     coordinates are given. The distance calculated is that along
*     the geodesic curve that joins the two points. The distance is
*     displayed on the screen ("<bad>" is displayed if the distance
*     cannot be calculated).
*
*     For example, in a basic Frame, the distance calculated will be
*     the Cartesian distance along the straight line joining the two
*     points. For a more specialised Frame describing a sky coordinate
*     system, however, it would be the distance along the great circle
*     passing through two sky positions.

*  Usage:
*     astdistance this point1 point2

*  ADAM Parameters:
*     FMT = LITERAL (Read)
*        The format in which to store output objects. Can be "AST", "XML",
*        "STCS", or any FitsChan encoding such as FITS-WCS. Only used
*        if the output object is written to a text file. An error is
*        reported if the output object cannot be written using the
*        requested format. ["AST"]
*     DISTANCE = _DOUBLE (Write)
*        The calculated distance.
*     THIS = LITERAL (Read)
*        An NDF, FITS file or text file holding the Frame. If an NDF is
*        supplied, the current Frame of the WCS FrameSet will be used. If a
*        FITS file is supplied, the Frame corresponding to the primary axis
*        descriptions will be used.
*     POINT1() = _DOUBLE (Read)
*        An array with one element for each Frame axis (Naxes attribute)
*        containing the coordinates of the first point.
*     POINT2() = _DOUBLE (Read)
*        An array with one element for each Frame axis (Naxes attribute)
*        containing the coordinates of the second point.

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-APR-2008 (DSB):
*        Original version.
*     14-NOV-2016 (GSB):
*        Added DISTANCE output parameter.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  External References:
      EXTERNAL AST_ISAFRAME

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER THIS, NC
      DOUBLE PRECISION DIST
      DOUBLE PRECISION POINT1( NDF__MXDIM )
      DOUBLE PRECISION POINT2( NDF__MXDIM )
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the Frame.
      CALL KPG1_GTOBJ( 'THIS', 'Frame', AST_ISAFRAME, THIS,
     :                 STATUS )

*  Get the number of axes in the Frame
      NC = AST_GETI( THIS, 'Naxes', STATUS )

*  Get the two points.
      CALL PAR_EXACD( 'POINT1', NC, POINT1, STATUS )
      CALL PAR_EXACD( 'POINT2', NC, POINT2, STATUS )

*  Get the distance.
      DIST = AST_DISTANCE( THIS, POINT1, POINT2, STATUS )

*  Display the result
      IF( DIST .NE. AST__BAD ) THEN
         CALL MSG_SETD( 'D', DIST )
      ELSE
         CALL MSG_SETC( 'D', '<bad>' )
      END IF

      CALL MSG_OUT( ' ', '^D', STATUS )

      CALL PAR_PUT0D( 'DISTANCE', DIST, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTDISTANCE_ERR', 'Error finding the distance'//
     :                 ' between two points.', STATUS )
      END IF

      END
