      SUBROUTINE IRQ1_RESET( LOCS, SLOT, STATUS )
*+
*  Name:
*     IRQ1_RESET

*  Purpose:
*     Reset a slot in the QUAL structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ1_RESET( LOCS, SLOT, STATUS )

*  Description:
*     The specified slot of the QUAL array is reset, i.e. any
*     association with a quality name is annulled. The slot can then be
*     re-used for a new quality name.  The number of free slots
*     (located by LOCS(4) ) is incremented, and the slot number added
*     to the list of free slots (located by LOCS(5) ).
*
*     An error occurs if the slot is flagged as read-only.

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
*        The index of the slot to be reset.
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
*        Report an error if the slot is read-only.
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
      INTEGER LUSED              ! Index of last used slot.
      INTEGER NFREE              ! No. of free slots in QUAL.
      CHARACTER * ( IRQ__SZQNM ) NAME ! The value of the NAME component
                                 ! of a QUAL cell.
      CHARACTER * ( DAT__SZLOC ) NMLOC ! Locator to the NAME component
                                 ! of a QUAL cell.
      CHARACTER * ( DAT__SZLOC ) QCLOC ! Locator to a single cell
                                 ! in the QUAL array.
      LOGICAL RDONLY             ! True if the the slot is read-only
      LOGICAL THERE              ! True if the components of QUAL
                                 ! already exist.
      CHARACTER * ( DAT__SZLOC ) TLOC ! Temporary locator to a single
                                 ! cell in the QUAL array.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a locator to the requested slot in the QUAL array.
      CALL DAT_CELL( LOCS(2), 1, SLOT, QCLOC, STATUS )

*  See if the NAME component exists.
      CALL DAT_THERE( QCLOC, IRQ__NMNAM, THERE, STATUS )

*  If it does, get a locator to it and get its current value.
      IF( THERE ) THEN
         CALL DAT_FIND( QCLOC, IRQ__NMNAM, NMLOC, STATUS )
         CALL DAT_GET0C( NMLOC, NAME, STATUS )
      ENDIF

*  Abort if an error occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  If the name did not exist, or was equal to the value of the symbolic
*  constant IRQ__SBAD, then the slot structure is not complete. Report
*  an error.
      IF( .NOT. THERE .OR. NAME .EQ. IRQ__SBAD ) THEN
         STATUS = IRQ__BADST
         CALL ERR_REP( 'IRQ1_RESET_ERR1',
     :                 'IRQ1_RESET: Incomplete slot structure found',
     :                 STATUS )
         GO TO 999
      END IF

*  If the "read-only" flag is set for this slot, report an error. Older
*  versiosn of IRQ (pre 15/2/2008) did not store a read-only flag in the
*  QUAL structure, so check for this, assuming .FALSE. if no flag is
*  available.
      CALL DAT_THERE( QCLOC, IRQ__RONAM, THERE, STATUS )
      IF( THERE ) THEN
         CALL CMP_GET0L( QCLOC, IRQ__RONAM, RDONLY, STATUS )
         IF( RDONLY .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = IRQ__RDONL
            CALL MSG_SETC( 'N', NAME )
            CALL ERR_REP( 'IRQ1_RESET_ERR2', 'IRQ1_RESET: The '//
     :                    'specified quality name (^N) is read-only.',
     :                    STATUS )
            GO TO 999
         END IF
      END IF

*  If the slot has already been reset, then leave the slot as it is.
      IF( NAME .NE. IRQ__SFREE ) THEN

*  Store a reserved string for the NAME which indicates that the slot
*  is free for use.
         CALL DAT_PUT0C( NMLOC, IRQ__SFREE, STATUS )

*  Increment the number of free slots.
         CALL DAT_GET0I( LOCS(4), NFREE, STATUS )
         NFREE = NFREE + 1
         CALL DAT_PUT0I( LOCS(4), NFREE, STATUS )

*  Add the index of the slot to the end of the free slot list.
         CALL DAT_CELL( LOCS(5), 1, NFREE, FCLOC, STATUS )
         CALL DAT_PUT0I( FCLOC, SLOT, STATUS )
         CALL DAT_ANNUL( FCLOC, STATUS )

*  Get the index of the last slot in use before this routine was
*  called.
         CALL DAT_GET0I( LOCS(3), LUSED, STATUS )

*  If the reset slot was the last slot in use, find the new current last
*  slot.
         IF( SLOT .EQ. LUSED ) THEN

 10         CONTINUE
            LUSED = LUSED - 1
            IF( LUSED .GT. 0 ) THEN

               CALL DAT_CELL( LOCS(2), 1, LUSED, TLOC, STATUS )
               CALL CMP_GET0C( TLOC, IRQ__NMNAM, NAME, STATUS )
               CALL DAT_ANNUL( TLOC, STATUS )

               IF( NAME .EQ. IRQ__SBAD ) THEN
                  STATUS = IRQ__BADST
                  CALL ERR_REP( 'IRQ1_RESET_ERR2',
     :                 'IRQ1_RESET: Incomplete slot structure found',
     :                 STATUS )

               ELSE IF( NAME .EQ. IRQ__SFREE ) THEN
                  GO TO 10

               END IF

            END IF

            CALL DAT_PUT0I( LOCS(3), LUSED, STATUS )

         END IF

      END IF

*  Annul the locator to the NAME component, and the QUAL cell.
 999  CONTINUE

      CALL DAT_ANNUL( NMLOC, STATUS )
      CALL DAT_ANNUL( QCLOC, STATUS )

      END
