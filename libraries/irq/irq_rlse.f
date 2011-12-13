      SUBROUTINE IRQ_RLSE( LOCS, STATUS )
*+
*  Name:
*     IRQ_RLSE

*  Purpose:
*     Release a temporary structure created by IRQ_NEW or IRQ_FIND.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ_RLSE( LOCS, STATUS )

*  Description:
*     This routine releases the resources reserved by a call to IRQ_NEW
*     or IRQ_FIND. The cloned NDF identifier held in LOCS(1) is
*     annulled, and then all the five HDS locators in LOCS are annulled.
*     If no defined quality names exist within the NDF, then the
*     structure used to hold such names is deleted and the QUALITY
*     component of the NDF is reset to an undefined state.
*
*     Note, this routine attempts to execute even if STATUS is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.

*  Arguments:
*     LOCS(5) = CHARACTER * ( * ) (Given)
*        An array of five HDS locators. These locators identify the NDF
*        and the associated quality name information.  They should have
*        been obtained using routine IRQ_FIND or routine IRQ_NEW.
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
*     26-JUL-1991 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'IRQ_PAR'          ! IRQ constants

*  Arguments Given:
      CHARACTER LOCS(5)*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER INDF               ! Cloned NDF identifier.
      INTEGER LUSED              ! Last used slot in QUAL structure.
      CHARACTER QNLOC*(DAT__SZLOC)! Locator to QUALITY_NAMES structure.
      INTEGER TSTAT              ! Temporary status.
      CHARACTER XLOC*(DAT__SZLOC)! Locator to NDF extension.

*.

*  Save the STATUS value and mark the error stack.
      TSTAT = STATUS
      CALL ERR_MARK
      STATUS = SAI__OK

*  Obtain the NDF identifier from LOCS, and check it is still valid.
      CALL IRQ1_INDF( LOCS, INDF, STATUS )

*  See if there are any defined names in the structure. If not, get a
*  locator to the NDF extension holding the QUALITY_NAMES structure.
      CALL DAT_GET0I( LOCS(3), LUSED, STATUS )
      IF( LUSED .EQ. 0 ) THEN
         CALL DAT_PAREN( LOCS(2), QNLOC, STATUS )
         CALL DAT_PAREN( QNLOC, XLOC, STATUS )
         CALL DAT_ANNUL( QNLOC, STATUS )
      END IF

*  Annul the temporary structure located by LOCS(1).
      CALL IRQ1_ANTMP( LOCS(1), STATUS )

*  Use DAT_ANNUL to annul the other locators, which are all associated
*  with objects stored in the NDF.
      CALL DAT_ANNUL( LOCS(2), STATUS )
      CALL DAT_ANNUL( LOCS(3), STATUS )
      CALL DAT_ANNUL( LOCS(4), STATUS )
      CALL DAT_ANNUL( LOCS(5), STATUS )

*  If there are no defined names in the structure, delete it and reset
*  the QUALITY component of the NDF.
      IF( LUSED .EQ. 0 ) THEN
         CALL DAT_ERASE( XLOC, IRQ__QINAM, STATUS )
         CALL DAT_ANNUL( XLOC, STATUS )
         CALL NDF_RESET( INDF, 'QUALITY', STATUS )
      END IF

*  Annul the NDF identifier.
      CALL NDF_MSG( 'NDF', INDF )
      CALL NDF_ANNUL( INDF, STATUS )

*  Annul any error if STATUS was previously bad, otherwise let the new
*  error report stand (but add context information). Release the error
*  stack.
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( TSTAT .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            STATUS = TSTAT
         ELSE
            CALL ERR_REP( 'IRQ_RLSE_ERR1',
     :'IRQ_RLSE: Unable to release quality name resources for NDF ^NDF',
     :                    STATUS )
         END IF
      ELSE
         STATUS = TSTAT
      END IF
      CALL ERR_RLSE

      END
