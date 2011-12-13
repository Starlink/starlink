      SUBROUTINE IRQ1_QBIT( LOCS, BIT, QNAME, STATUS )
*+
*  Name:
*     IRQ1_QBIT

*  Purpose:
*     Find the quality name associated with a specified bit number.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ1_QBIT( LOCS, BIT, QNAME, STATUS )

*  Description:
*     Each slot in the QUAL array is examined to see if it is currently
*     associated with the specified bit number. If it is, the
*     corresponding quality name is returned in QNAME. Otherwise, a blank
*     quality name is returned.

*  Arguments:
*     LOCS( 5 ) = CHARACTER * ( * ) (Given)
*        A set of five HDS locators. LOCS( 1 ) locates a temporary
*        structure holding a cloned NDF identifier. LOCS(2) locates the
*        QUAL array. LOCS(3) locates the LAST_USED value, holding the
*        index of the last used slot in the QUAL array. LOCS(4) locates
*        the NFREE value, holding the number of free slots in the QUAL
*        array. LOCS(5) locates the FREE array, which contains a stack
*        of the NFREE slot indices corresponding to free slots. This
*        stack is accessed in a "First-In-Last-Out" method.
*     BIT = INTEGER (Given)
*        The bit number to search for.
*     QNAME = CHARACTER * ( * ) (Returned)
*        The quality name associated with the given bit number, or blank
*        if the bit number is currently unused.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
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
*     4-MAR-2008 (DSB):
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
      INCLUDE 'IRQ_ERR'          ! IRQ error values.

*  Arguments Given:
      CHARACTER LOCS(5)*(*)
      INTEGER BIT

*  Arguments Returned:
      CHARACTER QNAME * ( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL FIXBIT             ! Does the quality have a fixed bit no.?
      LOGICAL FIXED              ! True if the quality does not need a
                                 ! QUALITY bit to represent it.
      INTEGER IBIT               ! Temporary bit number.
      INTEGER LUSED              ! Index of last used slot in QUAL array.
      CHARACTER NAME*(IRQ__SZQNM) ! The name stored in the current slot.
      CHARACTER QCLOC*(DAT__SZLOC) ! Locator to a single cell of the
                                 ! QUAL array.
      INTEGER SLOT               ! Current QUAL slot number.

*.

*  Initialise
      QNAME = ' '

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the index of the last used slot.
      CALL DAT_GET0I( LOCS(3), LUSED, STATUS )

*  Loop round each slot in turn.
      SLOT = 1
      DO WHILE( SLOT .LE. LUSED .AND. STATUS .EQ. SAI__OK )
         CALL DAT_CELL( LOCS(2), 1, SLOT, QCLOC, STATUS )

*  If the slot has been reset, pass on.
         CALL CMP_GET0C( QCLOC, IRQ__NMNAM, NAME, STATUS )
         IF( NAME .NE. IRQ__SFREE ) THEN

*  Otherwise, see if the quality has a corresponding bit in the QUALITY
*  array. If not, pass on.
            CALL CMP_GET0L( QCLOC, IRQ__FXNAM, FIXED, STATUS )
            CALL CMP_GET0L( QCLOC, IRQ__FBNAM, FIXBIT, STATUS )
            IF( .NOT. FIXED .OR. FIXBIT ) THEN

*  Get the corresponding bit number.
               CALL CMP_GET0I( QCLOC, IRQ__BTNAM, IBIT, STATUS )

*  If this is the requested bit, store the quality name.
               IF( IBIT .EQ. BIT ) THEN
                  QNAME = NAME
                  SLOT = LUSED + 1
               END IF

            END IF

         END IF

*  Annul the locator to the current slot.
         CALL DAT_ANNUL( QCLOC, STATUS )

*  Move on to the next slot.
         SLOT = SLOT + 1

      END DO

      END
