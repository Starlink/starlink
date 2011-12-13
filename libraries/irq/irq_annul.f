      SUBROUTINE IRQ_ANNUL( IDQ, STATUS )
*+
*  Name:
*     IRQ_ANNUL

*  Purpose:
*     Release an IRQ identifier.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ_ANNUL( IDQ, STATUS )

*  Description:
*     All internal resources used by the specified compiled quality
*     expression identifier (created by IRQ_COMP) are released.
*
*     This routine attempts to execute even if STATUS is bad on entry,
*     although no further error report will be made if it subsequently
*     fails under these circumstances.

*  Arguments:
*     IDQ = INTEGER (Given)
*        An IRQ identifier for a compiled quality expression.
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
      INCLUDE 'IRQ_ERR'          ! IRQ error values.

*  Global Variables:
      INCLUDE 'IRQ_COM'          ! IRQ common blocks.
*        QCM_VALID( IRQ__MAXQ ) = LOGICAL (Read)
*           True if the corresponding IRQ identifier is valid (i.e. in
*           use).

*  Arguments Given:
      INTEGER IDQ

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER TSTAT              ! Saved input STATUS value.

*.

*  Save the STATUS value and mark the error stack.
      TSTAT = STATUS
      CALL ERR_MARK

*  Clear the local status value.
      STATUS = SAI__OK

*  If the IRQ identifier is not valid, report an error.
      IF( IDQ .LT. 1 .OR. IDQ .GT. IRQ__MAXQ ) THEN
         STATUS = IRQ__INVID

      ELSE IF( .NOT. QCM_VALID( IDQ ) ) THEN
         STATUS = IRQ__INVID

      END IF

      IF( STATUS .EQ. IRQ__INVID ) THEN
         CALL ERR_REP( 'IRQ_ANNUL_ERR1',
     :                 'IRQ_ANNUL: Invalid IRQ identifier supplied',
     :                 STATUS )
         GO TO 999
      END IF

*  Call IRQ1_IANNU to annul the compiled quality expression.
      CALL IRQ1_IANNU( IDQ, STATUS )

*  Annul any error if STATUS was previously bad, otherwise let the new
*  error report stand, but add a context message. Release the error
*  stack.
 999  CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN

         IF ( TSTAT .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            STATUS = TSTAT

         ELSE
            CALL ERR_REP( 'IRQ_ANNUL_ERR2',
     :            'IRQ_ANNUL: Unable to annul an IRQ identifier',
     :             STATUS )

         END IF

      ELSE
         STATUS = TSTAT

      END IF

      CALL ERR_RLSE

      END
