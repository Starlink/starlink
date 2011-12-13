      SUBROUTINE IRQ1_CHECK( LOCS, QNAME, THERE, SLOT, STATUS )
*+
*  Name:
*     IRQ1_CHECK

*  Purpose:
*     Check for a given quality name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ1_CHECK( LOCS, QNAME, THERE, SLOT, STATUS )

*  Description:
*     The supplied name is compared with each name stored in the QUAL
*     array until a match is found or the last used slot is reached.
*     If the name is not found, THERE is returned .false., otherwise
*     THERE is returned .TRUE.

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
*     QNAME = CHARACTER * ( * ) (Given)
*        The quality name to search for.
*     THERE = LOGICAL (Returned)
*        True if the quality is defined.
*     SLOT = INTEGER (Returned)
*        Slot in which the name was found.
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
      INCLUDE 'IRQ_ERR'          ! IRQ error values.

*  Arguments Given:
      CHARACTER LOCS(5)*(*)
      CHARACTER QNAME*(*)

*  Arguments Returned:
      LOGICAL THERE
      INTEGER SLOT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER LUSED              ! Index of last used slot in QUAL array.
      LOGICAL MORE               ! True if more names need to be checked.
      CHARACTER NAME*(IRQ__SZQNM) ! The name stored in the current slot.
      CHARACTER QCLOC*(DAT__SZLOC)! Index to a single cell of the QUAL
                                 ! array.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the index of the last used slot.
      CALL DAT_GET0I( LOCS(3), LUSED, STATUS )

*  If there are no slots defined, return with THERE set to false.
      THERE = .FALSE.
      IF( LUSED .EQ. 0 ) RETURN

*  Loop round each slot in turn until one is found which contains a name
*  matching the supplied name, or the last used slot is reached.
      MORE = .TRUE.
      SLOT = 0

      DO WHILE( MORE )

         SLOT = SLOT + 1

         CALL DAT_CELL( LOCS(2), 1, SLOT, QCLOC, STATUS )
         CALL CMP_GET0C( QCLOC, IRQ__NMNAM, NAME, STATUS )
         CALL DAT_ANNUL( QCLOC, STATUS )

         IF( NAME .EQ. IRQ__SBAD ) THEN
            STATUS = IRQ__BADST
            CALL ERR_REP( 'IRQ1_CHECK_ERR1',
     :                    'IRQ1_CHECK: Incomplete slot structure found',
     :                    STATUS )

         ELSE IF( NAME .EQ. QNAME .OR. SLOT .EQ. LUSED ) THEN
            MORE = .FALSE.

         END IF

      END DO

*  Set THERE appropriately.
      IF( NAME .EQ. QNAME ) THERE = .TRUE.

      END
