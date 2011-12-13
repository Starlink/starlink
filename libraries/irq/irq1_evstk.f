      SUBROUTINE IRQ1_EVSTK( NOPC, OPCODE, MXSTK, STATUS )
*+
*  Name:
*     IRQ1_EVSTK

*  Purpose:
*     Find the maximum size of the stack needed to evaluate a quality
*     expression.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ1_EVSTK( NOPC, OPCODE, MXSTK, STATUS )

*  Description:
*     A quality expression is evaluated by performing the instructions
*     stored in the OPCODE array. These instructions manipulate logical
*     values stored in a "First In-Last out" (FILO) stack. As the
*     instructions are performed, the size of this stack varies. It
*     grows by one for instance when a "LOAD QUALITY" instruction is
*     performed, and shrinks by one when an "AND" instruction is
*     performed. This routine finds the maximum size which this stacks
*     reaches as the expression is evaluated.

*  Arguments:
*     NOPC = INTEGER (Given)
*        The number of instructions in the OPCODE array.
*     OPCODE( NOPC ) = INTEGER (Given)
*        The instructions.
*     MXSTK = INTEGER (Returned)
*        Max. size of the evaluation stack.
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
*     4-OCT-1991 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRQ_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NOPC
      INTEGER OPCODE( NOPC )

*  Arguments Returned:
      INTEGER MXSTK

*  Status:
      INTEGER STATUS             ! Global status

*  Include definitions of instructions:
      INCLUDE 'IRQ_OPC'

*  Local Variables:
      INTEGER I                  ! Index of current instruction.
      INTEGER STKSIZ             ! Current stack size.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the maximum evaluation stack size to zero.
      MXSTK = 0

*  Initialise the current evaluation stack size to zero.
      STKSIZ = 0

*  Loop through all the instructions in OPCODE.
      DO I = 1, NOPC

*  Increment the current stack size by the number of values added to
*  the stack by this instruction.
         STKSIZ = STKSIZ + OPC_DSTK( OPCODE( I ) )

*  Update the maximum stack size.
         MXSTK = MAX( MXSTK, STKSIZ )

      END DO

      END
