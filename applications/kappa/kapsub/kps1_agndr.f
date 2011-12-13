      SUBROUTINE KPS1_AGNDR( IPLOT, IGRP, IREG, STATUS )
*+
*  Name:
*     KPS1_AGNDR

*  Purpose:
*     Draws an ARD region for ARDGEN.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_AGNDR( IPLOT, IGRP, IREG, STATUS )

*  Description:
*     Draws the outline of the region defined by the ARD region with
*     index IREG is the supplied group.

*  Arguments:
*     IPLOT = INTEGER (Given)
*        The Plot associated with the current AGI picture.
*     IGRP = INTEGER (Given)
*        The GRP group containing the regions.
*     IREG = INTEGER (Given)
*        The index of the region to be plotted.
*     STATUS = INTEGER (Given and Returned)
*        The inherited status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     18-SEP-2001 (DSB):
*        Total re-write for AST/PGPLOT/ARD V2.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions
      INCLUDE 'GRP_PAR'          ! GRP constants

*  Arguments Given:
      INTEGER IPLOT
      INTEGER IGRP
      INTEGER IREG

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER TEXT*(GRP__SZNAM)! GRP element text
      INTEGER I                  ! GRP element index
      INTEGER IGRP2              ! GRP identifier for group to be plotted
      INTEGER REG                ! Current region index
      INTEGER SIZE               ! Size of group.
      REAL GBOX( 4 )             ! The bounds of the plotting area

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a group holding just the requested region.
*  =================================================

*  Create the empty group.
      CALL GRP_NEW( ' ', IGRP2, STATUS )

*  Get the group size.
      CALL GRP_GRPSZ( IGRP, SIZE, STATUS )

*  Go through the group.
      REG = 0
      DO I = 1, SIZE

*  Get the text of the next element.
         CALL GRP_GET( IGRP, I, 1, TEXT, STATUS )

*  If the first character is not blank, this is the start of a new
*  region.  Increment the current region index.
         IF ( TEXT( 1 : 1 ) .NE. ' ' ) REG = REG + 1

*  If this element belongs to the required region, copy it into the new
*  group.
         IF( REG .EQ. IREG ) THEN
            CALL GRP_PUT( IGRP2, 1, TEXT, 0, STATUS )

*  If we have passed onto another region, break out of the loop.
         ELSE IF( REG .GT. IREG ) THEN
            GO TO 10
         END IF

      END DO

 10   CONTINUE

*  Plot the ARD decription in the new group.
*  =========================================

*  Get the bounds of the current PGPLOT window.
      CALL PGQWIN( GBOX( 1 ), GBOX( 3 ), GBOX( 2 ), GBOX( 4 ) )

*  Plot the boundary.
      CALL ARD_PLOT( IGRP2, IPLOT, GBOX, 0, STATUS )

*  Delete the group.
      CALL GRP_DELET( IGRP2, STATUS )

      END
