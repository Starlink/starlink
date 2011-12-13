      SUBROUTINE NDG_SETSZ( IGRP, SIZE, STATUS )
*+
*  Name:
*     NDG_SETSZ

*  Purpose:
*     Reduces the size of an NDG group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG_SETSZ( IGRP, SIZE, STATUS )

*  Description:
*     This routine should be used instead of GRP_SETSZ to set the size of
*     a group created by NDG. It sets the size of the supplied group, and
*     also sets the size of each of the supplemental groups associated with
*     the supplied group.

*  Arguments:
*     IGRP = INTEGER (Given)
*        The NDG group as returned by NDG_ASSOC, etc. This should be the last
*        group in a GRP owner-slave chain.
*     SIZE = INTEGER (Given)
*        The new group size. Must be less than or equal to the size of the
*        smallest group in the chain.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-AUG-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants.

*  Arguments Given:
      INTEGER IGRP
      INTEGER SIZE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IGRP1, IGRP2       ! Group identifiers
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop round all groups i the chain.
      IGRP1 = IGRP
      DO WHILE( IGRP1 .NE. GRP__NOID )

*  Set the size of this group.
        CALL GRP_SETSZ( IGRP1, SIZE, STATUS )

*  Find the owner of this group. GRP__NOID is returned if the group
*  has no owner.
        CALL GRP_OWN( IGRP1, IGRP2, STATUS )
        IGRP1 = IGRP2

      END DO

      END
