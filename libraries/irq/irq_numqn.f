      SUBROUTINE IRQ_NUMQN( LOCS, NAMES, STATUS )
*+
*  Name:
*     IRQ_NUMQN

*  Purpose:
*     Return number of defined quality names.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ_NUMQN( LOCS, NAMES, STATUS )

*  Description:
*     The number of quality names defined in the NDF specified by
*     LOCS is returned.

*  Arguments:
*     LOCS(5) = CHARACTER * ( * ) (Given)
*        An array of five HDS locators. These locators identify the NDF
*        and the associated quality name information.  They should have
*        been obtained using routine IRQ_FIND or routine IRQ_NEW.
*     NAMES = INTEGER (Returned)
*        The number of quality names defined in the structure located by
*        LOCS.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*     25-JUL-1991 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER LOCS(5)*(*)

*  Arguments Returned:
      INTEGER NAMES

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER INDF               ! Identifier for the NDF containing the
                                 ! quality names information.
      INTEGER NFREE              ! No. of free slots in the QUAL array.
      INTEGER TOTAL              ! Total no. of slots in the QUAL array.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the NDF identifier from LOCS, and check it is still valid.
      CALL IRQ1_INDF( LOCS, INDF, STATUS )

*  Get the number of free slots stored in the QUAL array.
      CALL DAT_GET0I( LOCS(4), NFREE, STATUS )

*  Get the total size of the QUAL array.
      CALL DAT_SIZE( LOCS(2), TOTAL, STATUS )

*  The number of defined names is the difference.
      NAMES = TOTAL - NFREE

*  If an error occur, give context information.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL NDF_MSG( 'NDF', INDF )
         CALL ERR_REP( 'IRQ_NUMQN_ERR1',
     :     'IRQ_NUMQN: Unable to find no. of quality names in NDF ^NDF',
     :        STATUS )
      END IF

      END
