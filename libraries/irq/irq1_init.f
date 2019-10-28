      SUBROUTINE IRQ1_INIT( STATUS )
*+
*  Name:
*     IRQ1_INIT

*  Purpose:
*     Initialise the IRQ_ system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ1_INIT( STATUS )

*  Description:
*     A new temporary HDS object is created to hold an array of
*     compiled quality expression structures. All IRQ identifiers are
*     set invalid.

*  Arguments:
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
*     12-JUL-1991 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'IRQ_PAR'          ! IRQ constants.

*  Global Variables:
      INCLUDE 'IRQ_COM'          ! IRQ common blocks.
*        QCM_LOC = CHARACTER (Write)
*           An HDS locator to the array of compiled quality expression
*           structures.
*        QCM_STATE = CHARACTER (Write)
*           If QCM_STATE = IRQ__GOING then IRQ has been initialised.
*        QCM_VALID( IRQ__MAXQ ) = LOGICAL (Write)
*           True if the corresponding group identifier is valid (i.e. in
*           use).

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IDQ                ! An IRQ identifier.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a temporary HDS object to hold the structures. This is
*  a one dimensional array of QEXP structures with size given
*  by symbolic constant IRQ__MAXQ.
      CALL IRQ1_TEMP( 'QEXP', IRQ__MAXQ, QCM_LOC, STATUS )

*  Initialise all identifiers to "not in use".
      DO IDQ = 1, IRQ__MAXQ
         QCM_VALID( IDQ ) = .FALSE.
      END DO

*  If all has gone OK, set the common variable QCM_STATE to indicate
*  that the IRQ_ system is initialised.
      IF( STATUS .EQ. SAI__OK ) QCM_STATE = IRQ__GOING

      END
