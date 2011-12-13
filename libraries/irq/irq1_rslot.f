      SUBROUTINE IRQ1_RSLOT( LOCS, SLOT, STATUS )
*+
*  Name:
*     IRQ1_RSLOT

*  Purpose:
*     Reserve a slot in the QUAL array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ1_RSLOT( LOCS, SLOT, STATUS )

*  Description:
*     The QUAL array is an array of structures. Each structure holds
*     information describing a single quality name. Each such structure
*     is referred to as a "slot" in the QUAL array. This routine finds a
*     free (i.e. currently unused) slot. If no free slots exist, the
*     QUAL array is extended. Note, calling this routine does not
*     reduce the number of free slots available. All that happens is
*     that a free slot is located. It is up to the calling routine to
*     decide whether to put anything into the slot. The number of free
*     slots is decremented when IRQ1_ADD is called.

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
*     SLOT = INTEGER (Returned)
*        An index for a currently free slot.
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
      INCLUDE 'IRQ_PAR'          ! IRQ constants.

*  Arguments Given:
      CHARACTER LOCS(5)*(*)

*  Arguments Returned:
      INTEGER SLOT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) FRCLOC ! Locator to a single cell of
                                 ! the FREE array.
      INTEGER I                  ! Loop count.
      INTEGER NFREE              ! No. of free slots remaining.
      INTEGER QUSIZE             ! Total size of QUAL array.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if there are currently any free slots.
      CALL DAT_GET0I( LOCS(4), NFREE, STATUS )

*  If any free slots remain, get the most recently freed slot from
*  a cell of the FREE list.
      IF( NFREE .GT. 0 ) THEN
         CALL DAT_CELL( LOCS(5), 1, NFREE, FRCLOC, STATUS )
         CALL DAT_GET0I( FRCLOC, SLOT, STATUS )
         CALL DAT_ANNUL( FRCLOC, STATUS )

*  If no free slots remain, the QUAL array will need to be extended.
      ELSE

*  Get the current size of the QUAL array.
         CALL DAT_SIZE( LOCS(2), QUSIZE, STATUS )

*  Increase the size of the QUAL array.
         CALL DAT_ALTER( LOCS(2), 1, QUSIZE + IRQ__INCQ, STATUS )

*  Increase the size of the FREE array. The FREE array should always
*  have the same size as the QUAL array.
         CALL DAT_ALTER( LOCS(5), 1, QUSIZE + IRQ__INCQ, STATUS )

*  Initialise each of the newly created slots.
         DO I = QUSIZE + IRQ__INCQ, QUSIZE + 1, -1
            CALL IRQ1_ISLOT( LOCS, I, STATUS )
         END DO

*  Return the first of the newly created slots.
         SLOT = QUSIZE + 1

      END IF

      END
