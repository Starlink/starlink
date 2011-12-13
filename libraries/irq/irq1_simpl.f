      SUBROUTINE IRQ1_SIMPL( NOPC, OPCODE, STATUS )
*+
*  Name:
*     IRQ1_SIMPL

*  Purpose:
*     Simplify a compiled quality expression.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ1_SIMPL( NOPC, OPCODE, STATUS )

*  Description:
*     The following identities are used (if possible) to simplify the
*     quality expression (where A and B are logical variables):
*
*     A .EQV. FALSE = .NOT. A
*     A .EQV. TRUE  = A
*     A .XOR. FALSE = A
*     A .XOR. TRUE  = .NOT. A
*     A .OR. FALSE  = A
*     A .OR. TRUE   = TRUE
*     .NOT.A .OR. .NOT.B = .NOT.( A .AND. B )
*     A .AND. FALSE = FALSE
*     A .AND. TRUE  = A
*     .NOT.A .AND. .NOT.B = .NOT.( A .OR. B )
*     .NOT. TRUE    = FALSE
*     .NOT. FALSE   = TRUE
*     .NOT..NOT.A   = A
*     .NOT.( A .EQV. B ) = A .XOR. B
*     .NOT.( A .XOR. B ) = A .EQV. B

*  Arguments:
*     NOPC = INTEGER (Given)
*        No. of instructions (op. codes) in OPCODE.
*     OPCODE( NOPC ) = INTEGER (Given and Returned)
*        The instructions which must be executed in order to evaluate a
*        quality expression. Gaps introduced into OPCODE by the
*        simplification process are filled with NULL instructions, so
*        that the number of instructions in OPCODE does not change.
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
*     2-OCT-1991 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'IRQ_PAR'          ! IRQ constants.

*  Arguments Given:
      INTEGER NOPC

*  Arguments Given and Returned:
      INTEGER OPCODE( NOPC )

*  Status:
      INTEGER STATUS             ! Global status

*  Include definitions of instructions:
      INCLUDE 'IRQ_OPC'

*  Local Variables:
      LOGICAL CHANGE             ! True if a simplification has been
                                 ! introduced into the quality
                                 ! expression on the last pass through
                                 ! the expression.
      INTEGER I                  ! Index into the list of instructions.
      INTEGER INOT               ! Index of inserted NOT operator.
      INTEGER IOPN( OPC__MAXOP ) ! Indices of instructions which
                                 ! generate the operands of the current
                                 ! instruction.
      INTEGER OPC                ! The current instruction.
      INTEGER OPN( OPC__MAXOP )  ! Instructions which generate the
                                 ! operands of the current instruction.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Succesive passes are made through all the instructions in the
*  expression, introducing any possible simplifications. This continues
*  until a pass is made in which no simplifications were made. Indicate
*  that no simplifications have yet been made in this pass.
 10   CONTINUE
      CHANGE = .FALSE.

*  Loop round all the instructions in the expression.
      DO I = 1, NOPC
         OPC = OPCODE( I )

*  If the instruction does not have any operands, pass on.
         IF( OPC_OPS( OPC ) .GT. 0 ) THEN

*  Locate the instructions which generate the stack values which form
*  the operands for the current instruction.
            CALL IRQ1_OPAND( NOPC, OPCODE, I, OPC__MAXOP, IOPN, OPN,
     :                       STATUS )

*  See if the simplication "A .EQV. FALSE = .NOT. A" can be used.
*  If so, replace the LDF instruction with NULL and the EQV instruction
*  with NOT.
            IF( OPC .EQ. OPC__EQV ) THEN

               IF( OPN( 1 ) .EQ. OPC__LDF ) THEN
                  OPCODE( IOPN( 1 ) ) = OPC__NULL
                  OPCODE( I ) = OPC__NOT
                  CHANGE = .TRUE.

               ELSE IF( OPN( 2 ) .EQ. OPC__LDF ) THEN
                  OPCODE( IOPN( 2 ) ) = OPC__NULL
                  OPCODE( I ) = OPC__NOT
                  CHANGE = .TRUE.

*  See if the simplication "A .EQV. TRUE = A" can be used. If so,
*  replace the LDT instruction with NULL and the EQV instruction with
*  NULL.

               ELSE IF( OPN( 1 ) .EQ. OPC__LDT ) THEN
                  OPCODE( IOPN( 1 ) ) = OPC__NULL
                  OPCODE( I ) = OPC__NULL
                  CHANGE = .TRUE.

               ELSE IF( OPN( 2 ) .EQ. OPC__LDT ) THEN
                  OPCODE( IOPN( 2 ) ) = OPC__NULL
                  OPCODE( I ) = OPC__NULL
                  CHANGE = .TRUE.

*  See if the simplification ".NOT.( A .EQV. B ) = A .XOR. B" can be
*  used. This is so if the next instruction is NOT. If so, replace the
*  EQV instruction with XOR and the .NOT. instruction with NULL.
               ELSE IF( OPCODE( I + 1 ) .EQ. OPC__NOT ) THEN
                  OPCODE( I ) = OPC__XOR
                  OPCODE( I + 1 ) = OPC__NULL
                  CHANGE = .TRUE.

               END IF

*  See if the simplication "A .XOR. FALSE = A" can be used.  If so,
*  replace the LDF instruction with NULL and the XOR instruction with
*  NULL.
            ELSE IF( OPC .EQ. OPC__XOR ) THEN

               IF( OPN( 1 ) .EQ. OPC__LDF ) THEN
                  OPCODE( IOPN( 1 ) ) = OPC__NULL
                  OPCODE( I ) = OPC__NULL
                  CHANGE = .TRUE.

               ELSE IF( OPN( 2 ) .EQ. OPC__LDF ) THEN
                  OPCODE( IOPN( 2 ) ) = OPC__NULL
                  OPCODE( I ) = OPC__NULL
                  CHANGE = .TRUE.

*  See if the simplication "A .XOR. TRUE = .NOT. A" can be used. If so,
*  replace the LDT instruction with NULL and the EQV instruction with
*  NOT.

               ELSE IF( OPN( 1 ) .EQ. OPC__LDT ) THEN
                  OPCODE( IOPN( 1 ) ) = OPC__NULL
                  OPCODE( I ) = OPC__NOT
                  CHANGE = .TRUE.

               ELSE IF( OPN( 2 ) .EQ. OPC__LDT ) THEN
                  OPCODE( IOPN( 2 ) ) = OPC__NULL
                  OPCODE( I ) = OPC__NOT
                  CHANGE = .TRUE.

*  See if the simplification ".NOT.( A .XOR. B ) = A .EQV. B" can be
*  used. This is so if the next instruction is NOT. If so, replace the
*  XOR instruction with EQV and the .NOT. instruction with NULL.
               ELSE IF( OPCODE( I + 1 ) .EQ. OPC__NOT ) THEN
                  OPCODE( I ) = OPC__EQV
                  OPCODE( I + 1 ) = OPC__NULL
                  CHANGE = .TRUE.

               END IF

*  See if the simplication "A .OR. FALSE = A" can be used.  If so,
*  replace the LDF instruction with NULL and the OR instruction with
*  NULL.
            ELSE IF( OPC .EQ. OPC__OR ) THEN

               IF( OPN( 1 ) .EQ. OPC__LDF ) THEN
                  OPCODE( IOPN( 1 ) ) = OPC__NULL
                  OPCODE( I ) = OPC__NULL
                  CHANGE = .TRUE.

               ELSE IF( OPN( 2 ) .EQ. OPC__LDF ) THEN
                  OPCODE( IOPN( 2 ) ) = OPC__NULL
                  OPCODE( I ) = OPC__NULL
                  CHANGE = .TRUE.

*  See if the simplication "A .OR. TRUE = TRUE" can be used. If so,
*  replace the LDT instruction with NULL and the OR instruction with
*  LDT. Also, nullify all the instructions used to generate the
*  non-constant operand ("A").

               ELSE IF( OPN( 1 ) .EQ. OPC__LDT ) THEN
                  OPCODE( IOPN( 1 ) ) = OPC__NULL
                  OPCODE( I ) = OPC__LDT
                  CALL IRQ1_NULOP( IOPN( 2 ), NOPC, OPCODE, STATUS )
                  CHANGE = .TRUE.

               ELSE IF( OPN( 2 ) .EQ. OPC__LDT ) THEN
                  OPCODE( IOPN( 2 ) ) = OPC__NULL
                  OPCODE( I ) = OPC__LDT
                  CALL IRQ1_NULOP( IOPN( 1 ), NOPC, OPCODE, STATUS )
                  CHANGE = .TRUE.

*  See if the simplication ".NOT.A .OR. .NOT.B = .NOT. ( A .AND. B )"
*  can be used. If so, replace the NOT instructions with NULL and the
*  OR instruction with AND. Insert a NOT after the AND instruction.
               ELSE IF( OPN( 1 ) .EQ. OPC__NOT .AND.
     :                  OPN( 2 ) .EQ. OPC__NOT ) THEN
                  OPCODE( IOPN( 1 ) ) = OPC__NULL
                  OPCODE( IOPN( 2 ) ) = OPC__NULL
                  OPCODE( I ) = OPC__AND
                  INOT = I
                  CALL IRQ1_SPACE( 1, NOPC, OPCODE, INOT, STATUS )
                  OPCODE( INOT ) = OPC__NOT
                  CHANGE = .TRUE.

               END IF

*  See if the simplication "A .AND. FALSE = FALSE" can be used.  If so,
*  replace the LDF instruction with NULL and the AND instruction with
*  LDF. Also, nullify all the instructions used to generate the
*  non-constanty operand ("A").
            ELSE IF( OPC .EQ. OPC__AND ) THEN

               IF( OPN( 1 ) .EQ. OPC__LDF ) THEN
                  OPCODE( IOPN( 1 ) ) = OPC__NULL
                  OPCODE( I ) = OPC__LDF
                  CALL IRQ1_NULOP( IOPN( 2 ), NOPC, OPCODE, STATUS )
                  CHANGE = .TRUE.

               ELSE IF( OPN( 2 ) .EQ. OPC__LDF ) THEN
                  OPCODE( IOPN( 2 ) ) = OPC__NULL
                  OPCODE( I ) = OPC__LDF
                  CALL IRQ1_NULOP( IOPN( 1 ), NOPC, OPCODE, STATUS )
                  CHANGE = .TRUE.

*  See if the simplication "A .OR. TRUE = A" can be used. If so,
*  replace the LDT instruction with NULL and the OR instruction with
*  NULL.
               ELSE IF( OPN( 1 ) .EQ. OPC__LDT ) THEN
                  OPCODE( IOPN( 1 ) ) = OPC__NULL
                  OPCODE( I ) = OPC__NULL
                  CHANGE = .TRUE.

               ELSE IF( OPN( 2 ) .EQ. OPC__LDT ) THEN
                  OPCODE( IOPN( 2 ) ) = OPC__NULL
                  OPCODE( I ) = OPC__NULL
                  CHANGE = .TRUE.

*  See if the simplication ".NOT.A .AND. .NOT.B = .NOT. ( A .OR. B )"
*  can be used. If so, replace the NOT instructions with NULL and the
*  AND instruction with OR. Insert a NOT after the OR instruction.
               ELSE IF( OPN( 1 ) .EQ. OPC__NOT .AND.
     :                  OPN( 2 ) .EQ. OPC__NOT ) THEN
                  OPCODE( IOPN( 1 ) ) = OPC__NULL
                  OPCODE( IOPN( 2 ) ) = OPC__NULL
                  OPCODE( I ) = OPC__OR
                  INOT = I
                  CALL IRQ1_SPACE( 1, NOPC, OPCODE, INOT, STATUS )
                  OPCODE( INOT ) = OPC__NOT
                  CHANGE = .TRUE.

               END IF

*  See if the simplication .NOT. FALSE = TRUE" can be used.  If so,
*  replace the LDF instruction with NULL and the NOT instruction with
*  LDT.
            ELSE IF( OPC .EQ. OPC__NOT ) THEN

               IF( OPN( 1 ) .EQ. OPC__LDF ) THEN
                  OPCODE( IOPN( 1 ) ) = OPC__NULL
                  OPCODE( I ) = OPC__LDT
                  CHANGE = .TRUE.

*  See if the simplication .NOT. TRUE = FALSE" can be used.  If so,
*  replace the LDT instruction with NULL and the NOT instruction with
*  LDF.
               ELSE IF( OPN( 1 ) .EQ. OPC__LDT ) THEN
                  OPCODE( IOPN( 1 ) ) = OPC__NULL
                  OPCODE( I ) = OPC__LDF
                  CHANGE = .TRUE.

*  See if the simplication ".NOT..NOT.A = A" can be used.  If so,
*  replace the NOT instructions with NULLs.
               ELSE IF( OPN( 1 ) .EQ. OPC__NOT ) THEN
                  OPCODE( IOPN( 1 ) ) = OPC__NULL
                  OPCODE( I ) = OPC__NULL
                  CHANGE = .TRUE.

               END IF

            END IF

         END IF

*  If an error has been reported, abort.
         IF( STATUS .NE. SAI__OK ) GO TO 999

      END DO

*  If any simplifications were made, it may be possible to introduce
*  more simplifications. Go through the expression again.
      IF( CHANGE ) GO TO 10

*  If an error occurred, give a context message.
 999  CONTINUE
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRQ1_SIMPL_ERR1',
     :            'IRQ1_SIMPL: Error simplifing a quality expression',
     :                 STATUS )
      END IF

      END
