      SUBROUTINE IRQ1_ISLOT( LOCS, SLOT, STATUS )
*+
*  Name:
*     IRQ1_ISLOT

*  Purpose:
*     Initialise a slot in the QUAL structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ1_ISLOT( LOCS, SLOT, STATUS )

*  Description:
*     The specified slot of the QUAL array is initialised. The
*     components required for the slot are created, and set to values
*     which indicate that the slot is free and ready for use. The
*     number of free slots (located by LOCS(4) ) is incremented, and
*     the slot number added to the list of free slots (located by
*     LOCS(5) ).

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
*     SLOT = INTEGER (Given)
*        The index of the slot to be initialised.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
*     15-FEB-2008 (DSB):
*        Added a read-only flag to the QUAL structure.
*     4-MAR-2008 (DSB):
*        Added a "fixed bit number" flag to the QUAL structure.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'IRQ_PAR'          ! IRQ constants.
      INCLUDE 'IRQ_ERR'          ! IRQ error values.

*  Arguments Given:
      CHARACTER LOCS(5)*(*)
      INTEGER SLOT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) FCLOC ! Locator to a single cell
                                 ! in the FREE array.
      INTEGER NFREE              ! No. of free slots in QUAL.
      CHARACTER * ( DAT__SZLOC ) QCLOC ! Locator to a single cell
                                 ! in the QUAL array.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a locator to the requested slot in the QUAL array.
      CALL DAT_CELL( LOCS(2), 1, SLOT, QCLOC, STATUS )

*  Create all the required components...

*  1) The quality name. This is temporarily set to the value of symbolic
*  constant IRQ__SBAD to indicate that the slot is not yet complete.
*  (If the application is interupted while this routine is being
*  executed, an incomplete structure may result).
      CALL DAT_NEW0C( QCLOC, IRQ__NMNAM, IRQ__SZQNM, STATUS )
      CALL CMP_PUT0C( QCLOC, IRQ__NMNAM, IRQ__SBAD, STATUS )

*  2) A logical value indicating if the quality has a fixed value (set
*  or cleared) for every pixel in the NDF.
      CALL DAT_NEW0L( QCLOC, IRQ__FXNAM, STATUS )

*  3) A logical value the fixed quality value (set or cleared), if in
*  fact the value is fixed.
      CALL DAT_NEW0L( QCLOC, IRQ__VLNAM, STATUS )

*  4) If the value is not fixed, but varies from pixel to pixel, then a
*  bit must be reserved in the QUALITY component of the NDF to represent
*  the quality name. An integer value is created to hold the bit number
*  (least-significant bit is called Bit 1).
      CALL DAT_NEW0I( QCLOC, IRQ__BTNAM, STATUS )

*  5) A character string which holds a comment describing the quality
*  name.
      CALL DAT_NEW0C( QCLOC, IRQ__CMNAM, IRQ__SZCOM, STATUS )

*  6) A logical value indicating if the quality is read-only. If so, then
*  the slot describing the quality name cannot be removed using IRQ_REMQN
*  until such time as the quality is made read-write.
      CALL DAT_NEW0L( QCLOC, IRQ__RONAM, STATUS )

*  7) A logical value indicating if the bit number associated with the
*  quality should be left unchanged. If not, then the bit number may
*  change depending on whether or not all pixels do , or do not, have the
*  specified quality.
      CALL DAT_NEW0L( QCLOC, IRQ__FBNAM, STATUS )

*  Increment the number of free slots.
      CALL DAT_GET0I( LOCS(4), NFREE, STATUS )
      NFREE = NFREE + 1
      CALL DAT_PUT0I( LOCS(4), NFREE, STATUS )

*  Add the index of the slot to the end of the free slot list.
      CALL DAT_CELL( LOCS(5), 1, NFREE, FCLOC, STATUS )
      CALL DAT_PUT0I( FCLOC, SLOT, STATUS )
      CALL DAT_ANNUL( FCLOC, STATUS )

*  The slot is now complete. Overwrite the temporary value stored in the
*  NAME component, with a reserved string which indicates that the slot
*  is ready to be used.
      CALL CMP_PUT0C( QCLOC, IRQ__NMNAM, IRQ__SFREE, STATUS )

*  Annul the locator to the QUAL cell.
      CALL DAT_ANNUL( QCLOC, STATUS )

      END
