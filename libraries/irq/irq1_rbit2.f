      SUBROUTINE IRQ1_RBIT2( LOCS, BIT1, BIT2, STATUS )
*+
*  Name:
*     IRQ1_RBIT2

*  Purpose:
*     Find two unused bits in the QUALITY component.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ1_RBIT2( LOCS, BIT1, BIT2, STATUS )

*  Description:
*     Each slot in the QUAL array is examined to see if it is currently
*     associated with a bit in the QUALITY component. If it is, the bit
*     is written off the list of available bits. The smallest two remaining
*     bit numbers are returned. If insufficient bits remain, an error is
*     reported.

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
*     BIT1 = INTEGER (Returned)
*        A free bit number within the QUALITY component. The least
*        significant bit is called bit 1 (not bit 0).
*     BIT2 = INTEGER (Returned)
*        A second free bit number within the QUALITY component.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2019 East Asian Observatory
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
*     14-NOV-2019 (DSB):
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

*  Arguments Returned:
      INTEGER BIT1
      INTEGER BIT2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL BUSED( IRQ__QBITS )! True if each bit is used.
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

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the index of the last used slot.
      CALL DAT_GET0I( LOCS(3), LUSED, STATUS )

*  Initialise flags for each bit.
      DO IBIT = 1, IRQ__QBITS
         BUSED( IBIT ) = .FALSE.
      END DO

*  Loop round each slot in turn.
      DO SLOT = 1, LUSED
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
               BUSED( IBIT ) = .TRUE.

            END IF

         END IF

*  Annul the locator to the current slot.
         CALL DAT_ANNUL( QCLOC, STATUS )

      END DO

*  Find the two lowest bit numbers not in use.
      BIT1 = 0
      BIT2 = 0
      DO IBIT = 1, IRQ__QBITS
         IF( .NOT. BUSED( IBIT ) ) THEN
            IF( BIT1 .EQ. 0 ) THEN
               BIT1 = IBIT
            ELSE IF( BIT2 .EQ. 0 ) THEN
               BIT2 = IBIT
            END IF
         END IF
      END DO

*  If two free bits could not be found, report an error.
      IF( BIT2 .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = IRQ__XBITS
         CALL ERR_REP( 'IRQ1_RBIT_ERR1',
     :                 'IRQ1_RBIT: Insufficient free bits left in '//
     :                 'QUALITY component', STATUS )
      END IF

      END
