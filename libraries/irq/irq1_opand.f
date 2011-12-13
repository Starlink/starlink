      SUBROUTINE IRQ1_OPAND( NOPC, OPCODE, I, MAXOP, IOPN, OPN, STATUS )
*+
*  Name:
*     IRQ1_OPAND

*  Purpose:
*     Find the instructions which generate the operands of a given
*     instruction.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ1_OPAND( NOPC, OPCODE, I, MAXOP, IOPN, OPN, STATUS )

*  Description:
*     The operands for the specified instruction must be on the top of
*     the evaluation stack when the instruction is executed. This
*     routine works backwards through the instructions finding the
*     instructions nearest to the specified instruction which write
*     values to the stack positions expected for the required operands.

*  Arguments:
*     NOPC = INTEGER (Given)
*        The number of instructions in the OPCODE array.
*     OPCODE( NOPC ) = INTEGER (Given)
*        The instructions.
*     I = INTEGER (Given)
*        The index of the instructions for which the operands are to be
*        found.
*     MAXOP = INTEGER (Given)
*        Max. no. of operands per instruction.
*     IOPN( MAXOP ) = INTEGER (Returned)
*        The indices of the instructions within OPCODE which generate
*        the operands of the instruction with index I.
*     OPN( MAXOP ) = INTEGER (Returned)
*        The code representing the instructions within OPCODE which
*        generate the operands of the instruction with index I.
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
      INCLUDE 'IRQ_PAR'          ! IRQ error values.
      INCLUDE 'IRQ_ERR'          ! IRQ error values.

*  Arguments Given:
      INTEGER NOPC
      INTEGER OPCODE( NOPC )
      INTEGER I
      INTEGER MAXOP

*  Arguments Returned:
      INTEGER IOPN( MAXOP )
      INTEGER OPN( MAXOP )

*  Status:
      INTEGER STATUS             ! Global status

*  Include definitions of instructions:
      INCLUDE 'IRQ_OPC'

*  Local Variables:
      INTEGER II                 ! Index of current instruction.
      INTEGER NOP                ! No. of operands found so far.
      INTEGER NOPREQ             ! No. of required operands.
      INTEGER OPC                ! Current instruction code.
      INTEGER STKPOS             ! Stack position of next value to be
                                 ! written to the stack.
      INTEGER TARGET             ! Stack position of next operand.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store the number of operands required for the instruction with the
*  supplied index.
      NOPREQ = MIN( MAXOP, OPC_OPS( OPCODE( I ) ) )

*  If the instruction has no operands, report an error.
      IF( NOPREQ .EQ. 0 ) THEN
         STATUS = IRQ__NOOPS
         CALL MSG_SETI( 'OPC', OPCODE( I ) )
         CALL ERR_REP( 'IRQ1_OPAND_ERR1',
     :              'IRQ1_OPAND: Instruction "^OPC" has no operands '//
     :              '(programming error)', STATUS )
         GO TO 999
      END IF

*  Set the number of operands found so far to zero.
      NOP = 0

*  Variable TARGET holds the position on the evaluation stack, at which
*  the next operand to be found is stored.  The top of stack just
*  before the instruction with index I is executed, is position 1.
*  Entries lower in the stack have zero or negative positions.  When
*  the specified instruction is executed, the first operand is on the
*  top of the stack.
      TARGET = 1

*  The instructions which generate the operands for instruction I, must
*  have indices less than I. Search backwards from the instruction with
*  index I.
      II = I - 1

*  Variable STKPOS holds the position on the evaluation stack at which
*  the current instruction will write its result (if the operation does
*  in fact generate a result). STKPOS uses the same scheme as TARGET
*  (i.e. the top of stack just before instruction I is executed has
*  position 1).
      STKPOS = 1

*  Go backwards through the instructions until all the operands have
*  been found.
      DO WHILE( NOP .LT. NOPREQ )

*  If there are no more instructions, report an error.
         IF( II .LE. 0 ) THEN
            STATUS = IRQ__NOOPS
            CALL MSG_SETI( 'OPC', OPCODE( I ) )
            CALL MSG_SETI( 'I', I )
            CALL ERR_REP( 'IRQ1_OPAND_ERR2',
     :            'IRQ1_OPAND: Operands not found (programming error)',
     :             STATUS )
            GO TO 999
         END IF

*  Save the current instruction.
         OPC = OPCODE( II )

*  If the current instruction writes a result to the evaluation stack...
         IF( OPC_WRT( OPC ) .EQ. 1 ) THEN

*  ...and if the value is written to the position of the next operand...
            IF( STKPOS .EQ. TARGET ) THEN

*  Store the current instruction as one of the required operand
*  instructions.
               NOP = NOP + 1
               OPN( NOP ) = OPC
               IOPN( NOP ) = II

*  The next operand to be found will be put on the stack one position
*  lower than the operand just found.
               TARGET = TARGET - 1

            END IF

*  Update the position at which the next value will be written to the
*  evaluation stack.
            STKPOS = STKPOS - OPC_DSTK( OPC )

         END IF

*  Update the current instruction.
         II = II - 1

      END DO

 999  CONTINUE

      END
