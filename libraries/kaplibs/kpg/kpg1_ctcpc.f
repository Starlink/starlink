      SUBROUTINE KPG1_CTCPC( CI, GI, NEL, IGRP, STATUS )
*+
*  Name:
*     KPG1_CTCPC

*  Purpose:
*     Copies string values from a catalogue column to a GRP group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_CTCPC( CI, GI, NEL, IGRP, STATUS )

*  Description:
*     This routine gets NEL values for a given CAT (see SUN/181)
*     column, derived from rows 1 to NEL of a given catalogue,
*     selection, or index, and appends them to the end of the supplied
*     GRP group (a new group is created if necessary).

*  Arguments:
*     CI = INTEGER (Given)
*        The CAT identifier for the catalogue, selection or index
*        containing the required data.
*     GI = INTEGER (Given)
*        The CAT identifier for the column, expression or parameter
*        to be evaluated for rows 1 to NEL of the component identified
*        by CI.
*     NEL = INTEGER (Given)
*        The number of rows to copy.
*     IGRP = INTEGER (Given and Returned)
*        The identiier for the GRP group in which the column values are
*        to be stored. If this is supplied as GRP__NOID, then a new group
*        is created. Otherwise the values are appended to the end of the
*        supplied group.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     21-NOV-2006 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants

*  Arguments Given:
      INTEGER CI
      INTEGER GI
      INTEGER NEL

*  Arguments Given and Returned:
      INTEGER IGRP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER TEXT*(GRP__SZNAM)! Text read from column
      INTEGER I                  ! Row index
      LOGICAL NULL               ! Was no value available?
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a new group if required.
      IF( IGRP .EQ. GRP__NOID ) THEN
         CALL GRP_NEW( ' ', IGRP, STATUS )
      END IF

*  Loop round each row.
      DO I = 1, NEL

*  Read the current row from the catalogue, selection or index into the
*  current row buffer.
         CALL CAT_RGET( CI, I, STATUS )

*  Get the column value.
         CALL CAT_EGT0C( GI, TEXT, NULL, STATUS )

*  Use a blank value if the value is null.
         IF( NULL ) TEXT = ' '

*  Append the value to the end of the group.
         CALL GRP_PUT1( IGRP, TEXT, 0, STATUS )

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

      END DO

 999  CONTINUE

      END
