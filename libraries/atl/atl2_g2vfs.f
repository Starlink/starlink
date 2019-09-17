      SUBROUTINE ATL2_G2VFS( IGRP, VFS, STATUS )
*+
*  Name:
*     ATL2_G2VFS

*  Purpose:
*     Create a VFS from a GRP group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL2_G2VFS( IGRP, VFS, STATUS )

*  Description:
*     Creates a VFS (see atl2.c) containing the text of a supplied GRP
*     group.

*  Arguments:
*     IGRP = INTEGER (Given)
*        An identifier for the group holding the text.
*     VFS = INTEGER (Returned)
*        Pointer to the VFS.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2019 East Asian Observatory.
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
*     DSB: David Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     16-SEP-2019 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants

*  Arguments Given:
      INTEGER IGRP

*  Arguments Returned:
      INTEGER VFS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I
      INTEGER SIZE
      CHARACTER NAME*(GRP__SZNAM)
*.

*  Initialise.
      VFS = 0

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a new empty VFS.
      CALL ATL2_INIT( VFS, STATUS )

*  Loop round every name in the GRP, appending it to the VFS.
      CALL GRP_GRPSZ( IGRP, SIZE, STATUS )
      DO I = 1, SIZE
         CALL GRP_GET( IGRP, I, 1, NAME, STATUS )
         CALL ATL2_APPND( NAME, VFS, STATUS )
      END DO

      END
