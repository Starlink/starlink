      SUBROUTINE IRQ1_NULOP( I, NOPC, OPCODE, STATUS )
*+
*  Name:
*     IRQ1_NULOP

*  Purpose:
*     Nullify an instruction and the instructions which generate its
*     operands.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     IRQ1_NULOP( I, NOPC, OPCODE, STATUS )

*  Description:
*     The instruction with index I is replaced by NULL. If the
*     instruction has any operands, then all the instructions which are
*     used to generate the operands are also replaced by NULL.

*  Arguments:
*     I = INTEGER (Given)
*        Index of instruction to be nullified within OPCODE.
*     NOPC = INTEGER (Given)
*        The no. of instruction codes in OPCODE.
*     OPCODE( NOPC ) = INTEGER (Given and Returned)
*        List of instruction codes.
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
      INCLUDE 'IRQ_PAR'          ! IRQ constants.

*  Arguments Given:
      INTEGER I
      INTEGER NOPC

*  Arguments Given and Returned:
      INTEGER OPCODE( NOPC )

*  Status:
      INTEGER STATUS             ! Global status

*  Include instruction definitions:
      INCLUDE 'IRQ_OPC'

*  Local Variables:
      INTEGER II                 ! The index (within OPCODE) of the next
                                 ! instruction to be nullified.
      INTEGER INDEX( IRQ__NSYMS )! List of instruction indices,
                                 ! identifying the instructions to be
                                 ! nullified.
      INTEGER IOPN( OPC__MAXOP ) ! Indices of the instructions which
                                 ! generate the operands used by the
                                 ! current instruction.
      INTEGER J                  ! Loop count.
      INTEGER NEXT               ! The position of the next instruction
                                 ! index to be nullified within INDEX.
      INTEGER NONULL             ! The current number of instructions
                                 ! which need to be nullified.
      INTEGER OPANDS             ! The no. of operands which the current
                                 ! instruction has.
      INTEGER OPN( OPC__MAXOP )  ! Instruction codes which generate the
                                 ! operands used by the current
                                 ! instruction.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Put the given instruction index on the list of indices of
*  instructions to be nullified, and set the current number of such
*  indices to 1.
      NEXT = 1
      INDEX( NEXT ) = I
      NONULL = 1

*  Loop round until all relevant instructions have been nullified.
      DO WHILE( NEXT .LE. NONULL .AND. STATUS .EQ. SAI__OK )

*  Get the index of the next instruction to be nullified.
         II = INDEX( NEXT )

*  Get the number of operands which the instruction has.
         OPANDS = OPC_OPS( OPCODE( II ) )

*  If it has any operands, find them.
         IF( OPANDS .GT. 0 ) THEN
            CALL IRQ1_OPAND( NOPC, OPCODE, II, OPC__MAXOP, IOPN, OPN,
     :                       STATUS )

*  Add the instructions which produce the operands to the end of the
*  list of instructions to be nullified.
            DO J = 1, OPANDS
               INDEX( NONULL + J ) = IOPN( J )
            END DO

            NONULL = NONULL + OPANDS

         END IF

*  Replace the current instruction with a NULL instruction.
         OPCODE( II ) = OPC__NULL

*  Increment the position of the next instruction to be nullified.
         NEXT = NEXT + 1

      END DO

      END
