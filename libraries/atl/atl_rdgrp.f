      SUBROUTINE ATL_RDGRP( IGRP, IAST, STATUS )
*+
*  Name:
*     ATL_RDGRP

*  Purpose:
*     Read an AST Object from a GRP group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL_RDGRP( IGRP, IAST, STATUS )

*  Description:
*     Read an AST Object from a GRP group. The text in the group can be
*     either an AST Object dump, a set of FITS headers, or an STC-S
*     description.

*  Arguments:
*     IGRP = INTEGER (Given)
*        An identifier for the group holding the text.
*     IAST = INTEGER (Returned)
*        The AST Object, or AST__NULL.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - If the group contains the AST dump of a Channel (of any class),
*     then the Object returned via IAST will be the Channel itself. The
*     exception to this is that if the "Begin " line at the start of
*     the dump ends with the string "(Read)", then the returned IAST
*     Object will be the Object read from the Channel, rather than the
*     Channel itself. For instance, if the group contains the AST dump of
*     a FitsChan, and the first line of the dump is "Begin FitsChan(Read)",
*     then the returned IAST object will be the Object read from the
*     FitsChan, rather than the FitsChan itself. This facility is only
*     available for top level objects (e.g. FitsChans contained within
*     FitsChans cannot be read in this way).

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     12-JAN-2001 (DSB):
*        Original version.
*     30-JUN-2009 (DSB):
*        Support reading Objects from STC-S descriptions.
*     9-MAY-2019 (DSB):
*        Support reading Objects from text-encoded MOCs.
*     16-SEP-2019 (DSB):
*        Changed into a wrapper around ATL_RDVFS.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'GRP_PAR'          ! GRP constants

*  Arguments Given:
      INTEGER IGRP

*  Arguments Returned:
      INTEGER IAST

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string
      LOGICAL CHR_ISALF          ! Is character alphabetical?

*  Local Variables:
      INTEGER VFS
*.

*  Initialise.
      IAST = AST__NULL

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a VFS from the supplied GRP.
      CALL ATL2_G2VFS( IGRP, VFS, STATUS )

*  Read an object from the VFS.
      CALL ATL_RDVFS( VFS, IAST, STATUS )

*  Free the VFS.
      CALL ATL2_DELET( VFS, STATUS )

      END
