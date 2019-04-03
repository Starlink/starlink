      SUBROUTINE NDF1_HSCRT( LOC, STATUS )
*+
*  Name:
*     NDF1_HSCRT

*  Purpose:
*     Mark an HDS container file as a scratch file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_HSCRT( LOC, STATUS )

*  Description:
*     The routine marks an HDS container file for deletion (by HDS),
*     thus effectively turning it into a scratch file, which will be
*     deleted when the last primary locator associated with it is
*     annulled (or when HDS terminates if this occurs earlier).

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        Locator to an object in the container file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Particle Physics and Astronomy Research Council

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     27-APR-1994 (RFWS):
*        Original version.
*     28-APR-1994 (RFWS):
*        Changed to call NDF1_HTOP.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants

*  Arguments Given:
      CHARACTER * ( * ) LOC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) TOPLOC ! Top level locator for file

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain a top-level locator for the HDS container file.
      CALL NDF1_HTOP( LOC, 'UPDATE', TOPLOC, STATUS )

*  Mark the file for deletion (this also annuls the locator). The file
*  will not actually be deleted until its reference count falls to zero.
      CALL HDS_ERASE( TOPLOC, STATUS )

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_HSCRT', STATUS )

      END
