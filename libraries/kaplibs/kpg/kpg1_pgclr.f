      SUBROUTINE KPG1_PGCLR( STATUS )
*+
*  Name:
*     KPG1_PGCLR

*  Purpose:
*     Clears current PGPLOT viewport.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PGCLR( STATUS )

*  Description:
*     This routine clears the current PGPLOT viewport if possible. If it
*     is not possible (egfor a printer) it does nothing.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     - A PGPLOT image-display workstation must be open and active.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
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
*     30-SEP-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER CI                 ! Original colour index
      INTEGER CI2                ! Highest available colour index
      INTEGER CI1                ! Lowest available colour index
      INTEGER FS                 ! Original fill style
      REAL X1, X2, Y1, Y2        ! Bounds of original PGPLOT window
      REAL NX1, NX2, NY1, NY2    ! Bounds of original PGPLOT viewport
      REAL NX1E, NX2E, NY1E, NY2E! Bounds of extended PGPLOT viewport
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if we can write in the background colour on the current device.
      CALL PGQCOL( CI1, CI2 )

*  Do nothing if we can't.
      IF( CI1 .EQ. 0 ) THEN

*  Save the current fill area attributes.
         CALL PGQFS( FS )
         CALL PGQCI( CI )

*  Set "solid fill in background colour (pen 0)".
         CALL PGSFS( 1 )
         CALL PGSCI( 0 )

*  We have to jump some hurdles because the GKS version of PGPLOT
*  doesn't clear the edge pixels in the viewport. So we attempt to
*  clear a viewport which is slightly larger than the current viewport.
*  But to do this we need to make the viewport large because PGPLOT
*  clips at the edge of the viewport (GKS PGPLOT does not support
*  the disabling of clipping!!!).

*  Get the extent in world co-ords and NDC of the current viewport.
         CALL PGQWIN( X1, X2, Y1, Y2 )
         CALL PGQVP( 0, NX1, NX2, NY1, NY2 )

*  Set the viewport to the whole view surface. Set the window to the
*  same bounds.
         CALL PGSVP( 0.0, 1.0, 0.0, 1.0 )
         CALL PGSWIN( 0.0, 1.0, 0.0, 1.0 )

*  Extend the original viewport slightly to account for rounding error.
         NX1E = NX1 - 0.001
         NX2E = NX2 + 0.001
         NY1E = NY1 - 0.001
         NY2E = NY2 + 0.001

*  Draw a filled rectangle covering the extended viewport.
         CALL PGRECT( NX1E, NX2E, NY1E, NY2E )

*  Re-instate the original viewport and window.
         CALL PGSWIN( X1, X2, Y1, Y2 )
         CALL PGSVP( NX1, NX2, NY1, NY2 )

*  Re-instate the original attributes.
         CALL PGSFS( FS )
         CALL PGSCI( CI )

      END IF

      END
