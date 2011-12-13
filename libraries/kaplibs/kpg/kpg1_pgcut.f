      SUBROUTINE KPG1_PGCUT( X1, X2, Y1, Y2, STATUS )
*+
*  Name:
*     KPG1_PGCUT

*  Purpose:
*     Cuts a section out of the current PGPLOT window.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL  KPG1_PGCUT( X1, X2, Y1, Y2, STATUS )

*  Description:
*     This routine sets the PGPLOT viewport so that it covers a specified
*     section of the current PGPLOT window. The world co-ordinate bounds
*     of the corresponding window are set to the supplied bounds.

*  Arguments:
*     X1 = REAL (Given)
*        The X world co-ordinate at the bottom left corner of the section
*        of the viewport to be cut.
*     X2 = REAL (Given)
*        The X world co-ordinate at the top right corner of the section
*        of the viewport to be cut.
*     Y1 = REAL (Given)
*        The Y world co-ordinate at the bottom left corner of the section
*        of the viewport to be cut.
*     Y2 = REAL (Given)
*        The Y world co-ordinate at the top right corner of the section
*        of the viewport to be cut.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998, 2002 Central Laboratory of the Research Councils.
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
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-AUG-1998 (DSB):
*        Original version.
*     22-AUG-2002 (DSB):
*        Report error if no overlap with current picture.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      REAL X1
      REAL X2
      REAL Y1
      REAL Y2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL M                     ! Ratio of viewport and window scales
      REAL NVX1, NVX2, NVY1, NVY2! New viewport bounds (NDC)
      REAL VX1, VX2, VY1, VY2    ! Original viewport bounds (NDC)
      REAL WX1, WX2, WY1, WY2    ! Original window bounds
      REAL LX1, LX2, LY1, LY2    ! Limited window bounds
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the requested window has non-zero area. Report an error otherwise.
      IF( X1 .EQ. X2 .OR. Y1 .EQ. Y2 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPG1_PGCUT_1', 'The requested graphics window'//
     :                 ' has zero size.', STATUS )
         GO TO 999
      END IF

*  Get the current extent of the PGPLOT window, in world co-ordinates.
      CALL PGQWIN( WX1, WX2, WY1, WY2 )

*  Limit the bounds of the area to be cut so that it does not extend
*  betond the current window.
      IF( WX2 .GT. WX1 ) THEN
         LX1 = MIN( WX2, MAX( WX1, X1 ) )
         LX2 = MIN( WX2, MAX( WX1, X2 ) )
      ELSE
         LX1 = MIN( WX1, MAX( WX2, X1 ) )
         LX2 = MIN( WX1, MAX( WX2, X2 ) )
      END IF

      IF( WY2 .GT. WY1 ) THEN
         LY1 = MIN( WY2, MAX( WY1, Y1 ) )
         LY2 = MIN( WY2, MAX( WY1, Y2 ) )
      ELSE
         LY1 = MIN( WY1, MAX( WY2, Y1 ) )
         LY2 = MIN( WY1, MAX( WY2, Y2 ) )
      END IF

*  Check the requested window overlaps the current window. Report an
*  error otherwise.
      IF( LX1 .EQ. LX2 .OR. LY1 .EQ. LY2 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPG1_PGCUT_2', 'The requested graphics fall '//
     :                 'entirely outside the current picture.', STATUS )
         GO TO 999
      END IF

*  Get the current extent of the PGPLOT viewport, in NDC.
      CALL PGQVP( 0, VX1, VX2, VY1, VY2 )

*  Only proceed if the current window has some area.
      IF( WX1 .NE. WX2 .AND. WY1 .NE. WY2 ) THEN

*  Get the increment in viewport co-ord for unit increment in window
*  co-ord on the X axis.
         M = ( VX1 - VX2 )/( WX1 - WX2 )

*  Find the X axis NDC bounds corresponding to the supplied window bounds.
         NVX1 = M*( LX1 - WX1 ) + VX1
         NVX2 = M*( LX2 - WX1 ) + VX1

*  Get the increment in viewport co-ord for unit increment in window
*  co-ord on the Y axis.
         M = ( VY1 - VY2 )/( WY1 - WY2 )

*  Find the Y axis NDC bounds corresponding to the supplied window bounds.
         NVY1 = M*( LY1 - WY1 ) + VY1
         NVY2 = M*( LY2 - WY1 ) + VY1

*  Establish the new viewport.
         CALL PGSVP( NVX1, NVX2, NVY1, NVY2 )

*  Establish the new window.
         CALL PGSWIN( LX1, LX2, LY1, LY2 )

      END IF

 999  CONTINUE

      END
