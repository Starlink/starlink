      SUBROUTINE IRQ1_IANNU( IDQ, STATUS )
*+
*  Name:
*     IRQ1_IANNU

*  Purpose:
*     Annul a valid IRQ identifier.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ1_IANNU( IDQ, STATUS )

*  Description:
*     It is assumed that the supplied IRQ identifier is valid and
*     currently in use.  All component of the associated QEXP structure
*     are deleted. The identifier is flagged as no longer being in use,
*     and an invalid group identifier is returned.
*
*     Note, this routine executes even if STATUS is bad on entry.

*  Arguments:
*     IDQ = INTEGER (Given and Returned)
*        The IRQ identifier to be annulled. Assumed valid on entry.
*        Set to IRQ__NOID on exit.
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
*     16-JUL-1991 (DSB):
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

*  Global Variables:
      INCLUDE 'IRQ_COM'          ! IRQ common blocks.
*        QCM_LOCQ( IRQ__MAXQ ) = CHARACTER (Read and Write)
*           HDS locators to the individual QEXP structures.
*        QCM_LOCMS( IRQ__MAXQ ) = CHARACTER (Write)
*           HDS locator to the MASKS component of each QEXP structure.
*        QCM_LOCOP( IRQ__MAXQ ) = CHARACTER (Write)
*           HDS locator to the OPCODE component of each QEXP structure.
*        QCM_VALID( IRQ__MAXQ ) = LOGICAL (Write)
*           True if the corresponding compiled quality expression
*           identifier is valid (i.e. in use).

*  Arguments Given and Returned:
      INTEGER IDQ

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER TSTAT              ! Saved input STATUS value.

*.

*  Return immediately if the identifier is out of bounds.
      IF( IDQ .LE. 0 .OR. IDQ .GT. IRQ__MAXQ ) RETURN

*  Save the input status value and initialise the local status value.
      TSTAT = STATUS
      STATUS = SAI__OK

*  Annul the locator to the MASKS component and then erase it. This
*  automatically unmaps the corresponding array.
      CALL DAT_ANNUL( QCM_LOCMS( IDQ ), STATUS )
      CALL DAT_ERASE( QCM_LOCQ( IDQ ), 'MASKS', STATUS )

*  Annul the locator to the OPCODE component and then erase it. This
*  automatically unmaps the corresponding array.
      CALL DAT_ANNUL( QCM_LOCOP( IDQ ), STATUS )
      CALL DAT_ERASE( QCM_LOCQ( IDQ ), 'OPCODE', STATUS )

*  Annul the locator to the group structure.
      CALL DAT_ANNUL( QCM_LOCQ( IDQ ), STATUS )

*  Indicate that the group is no longer in use.
      QCM_VALID( IDQ ) = .FALSE.

*  Set the group identifier invalid.
      IDQ = IRQ__NOID

*  If status was bad on entry, restore its input value.
      IF( TSTAT .NE. SAI__OK ) STATUS = TSTAT

      END
