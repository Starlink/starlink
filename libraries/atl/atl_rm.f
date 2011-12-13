      SUBROUTINE ATL_RM( FILE, STATUS )
*+
*  Name:
*     ATL_RM

*  Purpose:
*     Remove a file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL_RM( FILE, STATUS )

*  Description:
*     This subroutine calls the "PSX_REMOVE" RTL function to remove a
*     specified file. No error occurs if the file cannot be removed for
*     any reason.

*  Arguments:
*     FILE = CHARACTER * ( * ) (Given)
*        The path to the file.
*     STATUS = INTEGER (Given and Returned)
*        The inherited global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
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
*     13-SEP-1999 (DSB):
*        Original version.
*     13-SEP-2005 (TIMJ):
*        Rewritten in Fortran to call PSX_REMOVE
*     30-MAY-2006 (DSB):
*        Moved into ATL library and changed prefix from "ATL1_" to "ATL_".
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER *(*) FILE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER PSXSTAT

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Local status
      PSXSTAT = SAI__OK

*     New error context
      CALL ERR_MARK

*     Remove the file
      CALL PSX_REMOVE( FILE, PSXSTAT )

*     Clear bad status
      IF (PSXSTAT .NE. SAI__OK) CALL ERR_ANNUL( STATUS )

*     Reset error context
      CALL ERR_RLSE

      END
