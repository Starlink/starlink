      SUBROUTINE IRQ1_INDF( LOCS, INDF, STATUS )
*+
*  Name:
*     IRQ1_INDF

*  Purpose:
*     Get the NDF identifier.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ1_INDF( LOCS, BIT, STATUS )

*  Description:
*     The NDF identifier is obtained from LOCS(1), and checked for
*     validity. If it is invalid an error is reported.

*  Arguments:
*     LOCS( 5 ) = CHARACTER * ( * ) (Given)
*        A set of 5 HDS locators. LOCS( 1 ) locates a temporary
*        structure holding a cloned NDF identifier. LOCS(2) locates the
*        QUAL array. LOCS(3) locates the LAST_USED value, holding the
*        index of the last used slot in the QUAL array. LOCS(4) locates
*        the NFREE value, holding the number of free slots in the QUAL
*        array. LOCS(5) locates the FREE array, which contains a stack
*        of the NFREE slot indices corresponding to free slots. This
*        stack is accessed in a "First-In-Last-Out" method.
*     INDF = INTEGER (Returned)
*        The cloned identifier for the NDF in which the quality name
*        information is stored.
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
      INCLUDE 'IRQ_ERR'          ! IRQ error values.

*  Arguments Given:
      CHARACTER LOCS(5)*(*)

*  Arguments Returned:
      INTEGER INDF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL VALID              ! True if NDF identifier is valid.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the identifier from LOCS(1).
      CALL DAT_GET0I( LOCS(1), INDF, STATUS )

*  See if it is still valid.
      CALL NDF_VALID( INDF, VALID, STATUS )

*  If it is not valid, report an error.
      IF( .NOT. VALID .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = IRQ__IVNDF
         CALL ERR_REP( 'IRQ1_INDF_ERR1',
     :               'IRQ1_INDF: NDF identifier has been annulled',
     :                STATUS )
      END IF

      END
