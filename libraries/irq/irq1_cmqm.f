      SUBROUTINE IRQ1_CMQM( NOPC, NQNAME, OPCODE, MASKS, NMASKS,
     :                      STATUS )
*+
*  Name:
*     IRQ1_CMQM

*  Purpose:
*     Combine multiple quality masks into single masks.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ1_CMQM( NOPC, NQNAME, OPCODE, MASKS, NMASKS, STATUS )

*  Description:
*     Initially, all quality bits are tested individually, by performing
*     a logical AND operation between a QUALITY byte, and a mask
*     containing a single set bit. It is more efficient to test more
*     than one bit if possible. E.G. a quality expression such as
*     A.AND.B, requires the quality bit corresponding to "A" to be
*     tested and loaded on the evaluation stack, then the same must be
*     done for "B", and finally and AND performed between the top two
*     stack entries. Instead, the two bits corresponding to qualities
*     "A" and "B" can be combined (using a bit-wise OR operation) into a
*     single mask. The two quality bits can then be loaded on to the
*     stack in a single operation which incorporates the AND in the
*     original quality expression. i.e. set the stack to .TRUE. if:
*
*         IAND( QUAL, MASK ) .EQ. MASK
*
*     and .FALSE. otherwise, where QUAL is the QUALITY byte, and MASK
*     is the combined mask. If the original expression had been
*     A.OR.B then instead of the above logical expression, the following
*     would be used:
*
*         IAND( QUAL, MASK ) .NE. 0
*
*     The masks used in the above two expressions can contain any number
*     of set bits. If the original expression had been A.EQV.B then the
*     stack is set to the value of:
*
*         ( IAND( QUAL, MASK ) .EQ. MASK ) .OR.
*         ( IAND( QUAL, MASK ) .EQ. 0 )
*
*     If the original expression had been A.XOR.B then the stack is set
*     to the value of:
*
*         ( IAND( QUAL, MASK ) .NE. MASK ) .OR.
*         ( IAND( QUAL, MASK ) .NE. 0 )
*
*     The masks used in the above two expressions must contain no more
*     than two set bits. If the original expression had been .NOT.A
*     then the stack is set to the value of:
*
*         IAND( QUAL, MASK ) .NE. MASK
*
*     The mask used in the above expression may only contain a single
*     set bit.
*
*     If any of the above cases are found in the compiled quality
*     expression given in OPCODE, then each LDQ instruction ( which
*     tests and loads a single quality bit) is replaced by one of
*     the instructions LDQE, LDQX, LDQO, LDQA or LDQN. These five
*     instructions are used to replace LDQ instructions which generate
*     the operands used by EQV, XOR, OR, AND or NOT instructions
*     respectively.

*  Arguments:
*     NOPC = INTEGER (Given)
*        No. of op. codes contained in OPCODE.
*     NQNAME = INTEGER (Given)
*        No. of individual quality names referenced in the quality
*        expression.
*     OPCODE( * ) = INTEGER (Given and Returned)
*        The integer op. codes representing the operations which must be
*        performed on a FILO stack to evaluate the quality expression.
*        On input this contains operations which test a single bit of
*        the QUALITY component. On output some of these operations may
*        have been combined together into bit-wise operations between
*        the QUALITY component and masks containing more than 1 set bit.
*        The gaps thus created are filled by NULL instructions.
*     MASKS( * ) = INTEGER (Given and Returned)
*        On entry, this contains a bit mask for each bit to be tested
*        in the QUALITY component (the bit to be tested is set in the
*        mask and all other bits are cleared). On exit, some of these
*        masks may have been combined together so that they contain
*        more than one set bit. Masks are used in the order they are
*        stored in this array, i.e. the first LDQ instruction in OPCODE
*        uses MASKS(1), the second uses MASKS(2), etc.
*     NMASKS = INTEGER (Returned)
*        No. of masks in the MASKS on exit. This will be less than
*        NQNAMES if any masks have been combined.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  VAX-specific features used:
*     -  Uses function IOR

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
*     30-SEP-1991 (DSB):
*        Original version.
*     6-MAR-2009 (DSB):
*        Fix bad logic that causes memory to be trashed if the quality
*        expresson contains more than 2 quality names.
*     12-MAR-2009 (DSB):
*        Ensure the returned values are correct even if no changes are
*        made to the quality masks.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRQ_PAR'          ! IRQ constants

*  Arguments Given:
      INTEGER NOPC
      INTEGER NQNAME
      INTEGER OPCODE( * )

*  Arguments Given and Returned:
      INTEGER MASKS( * )

*  Arguments Returned:
      INTEGER NMASKS

*  Status:
      INTEGER STATUS             ! Global status

*  Include IRQ instruction definitions:
      INCLUDE 'IRQ_OPC'

*  Local Variables:
      LOGICAL CHANGE             ! True if any changes made to OPCODE in
                                 ! the current pass.
      INTEGER I                  ! Loop count.
      INTEGER IOPN( OPC__MAXOP ) ! Indices of instructions which
                                 ! generate the operands of the current
                                 ! instruction.
      INTEGER MASK1              ! Mask used to create first operand.
      INTEGER MASK2              ! Mask used to create second operand.
      INTEGER NLDQ               ! No. of instructions which read a mask
                                 ! value from MASKS.
      INTEGER OPC                ! Current instruction code.
      INTEGER OPN( OPC__MAXOP )  ! Instruction codes which generate the
                                 ! operands of the current instruction.
      INTEGER QM( IRQ__NSYMS )   ! Index of mask used by each
                                 ! instruction in OPCODE.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Make succesive passes through the OPCODE array replacing instructions
*  which load a single quality bit with instructions which load several
*  quality bits. Do this until no further changes can be made.
 10   CONTINUE

*  Indicate that no changes have yet been made to the OPCODE array in
*  this pass.
      CHANGE = .FALSE.

*  Identify all the LDQ instructions which use quality masks.
      NLDQ = 0

      DO I = 1, NOPC
         IF( OPCODE( I ) .EQ. OPC__LDQ .OR.
     :       OPCODE( I ) .EQ. OPC__LDQE .OR.
     :       OPCODE( I ) .EQ. OPC__LDQX .OR.
     :       OPCODE( I ) .EQ. OPC__LDQO .OR.
     :       OPCODE( I ) .EQ. OPC__LDQA .OR.
     :       OPCODE( I ) .EQ. OPC__LDQN ) THEN
            NLDQ = NLDQ + 1
            QM( I ) = NLDQ
         ELSE
            QM( I ) = -1
         END IF
      END DO

*  Loop through the op. codes.
      DO I = 1, NOPC
         OPC = OPCODE( I )

*  Pass over instructions which have no operands.
         IF( OPC_OPS( OPC ) .GT. 0 ) THEN

*  Find the instructions which generate the operands.
            CALL IRQ1_OPAND( NOPC, OPCODE, I, OPC__MAXOP, IOPN, OPN,
     :                       STATUS )

*  Deal first with EQV operators.
            IF( OPC .EQ. OPC__EQV ) THEN

*  If both operands are created by loading a single quality bit (LDQ),
*  replace the two LDQ instructions with NULLs and the EQV instruction
*  with an LDQE instruction. Replace the first mask with the bit-wise OR
*  of the two masks, and replace the second mask with -1.
               IF( OPN( 1 ) .EQ. OPC__LDQ .AND.
     :             OPN( 2 ) .EQ. OPC__LDQ ) THEN
                  OPCODE( IOPN( 1 ) ) = OPC__NULL
                  OPCODE( IOPN( 2 ) ) = OPC__NULL
                  OPCODE( I ) = OPC__LDQE

                  MASK1 = MASKS( QM ( IOPN( 1 ) ) )
                  MASK2 = MASKS( QM ( IOPN( 2 ) ) )
                  MASKS( QM ( IOPN( 1 ) ) ) = IOR( MASK1, MASK2 )
                  MASKS( QM ( IOPN( 2 ) ) ) = -1

                  QM( I ) =  QM ( IOPN( 1 ) )
                  QM( IOPN( 1 ) ) = -1
                  QM( IOPN( 2 ) ) = -1

                  CHANGE = .TRUE.

               END IF

*  Now deal with XOR operators.
            ELSE IF( OPC .EQ. OPC__XOR ) THEN

*  If both operands are created by loading a single quality bit (LDQ),
*  replace the two LDQ instructions with NULLs and the XOR instruction
*  with an LDQX instruction. Replace the first mask with the bit-wise OR
*  of the two masks, and replace the second mask with -1.
               IF( OPN( 1 ) .EQ. OPC__LDQ .AND.
     :             OPN( 2 ) .EQ. OPC__LDQ ) THEN
                  OPCODE( IOPN( 1 ) ) = OPC__NULL
                  OPCODE( IOPN( 2 ) ) = OPC__NULL
                  OPCODE( I ) = OPC__LDQX

                  MASK1 = MASKS( QM ( IOPN( 1 ) ) )
                  MASK2 = MASKS( QM ( IOPN( 2 ) ) )
                  MASKS( QM ( IOPN( 1 ) ) ) = IOR( MASK1, MASK2 )
                  MASKS( QM ( IOPN( 2 ) ) ) = -1

                  QM( I ) =  QM ( IOPN( 1 ) )
                  QM( IOPN( 1 ) ) = -1
                  QM( IOPN( 2 ) ) = -1

                  CHANGE = .TRUE.

               END IF

*  Now deal with OR operators.
            ELSE IF( OPC .EQ. OPC__OR ) THEN

*  If both operands are created either by loading a single quality bit
*  (LDQ) or loading several bits (LDQO), replace the two LDQ(O)
*  instructions with NULLs and the OR instruction with an LDQO
*  instruction. Replace the first mask with the bit-wise OR of the two
*  masks, and replace the second mask with -1.
               IF( ( OPN( 1 ) .EQ. OPC__LDQ .OR.
     :               OPN( 1 ) .EQ. OPC__LDQO ) .AND.
     :             ( OPN( 2 ) .EQ. OPC__LDQ .OR.
     :               OPN( 2 ) .EQ. OPC__LDQO ) ) THEN
                  OPCODE( IOPN( 1 ) ) = OPC__NULL
                  OPCODE( IOPN( 2 ) ) = OPC__NULL
                  OPCODE( I ) = OPC__LDQO

                  MASK1 = MASKS( QM ( IOPN( 1 ) ) )
                  MASK2 = MASKS( QM ( IOPN( 2 ) ) )
                  MASKS( QM ( IOPN( 1 ) ) ) = IOR( MASK1, MASK2 )
                  MASKS( QM ( IOPN( 2 ) ) ) = -1

                  QM( I ) =  QM ( IOPN( 1 ) )
                  QM( IOPN( 1 ) ) = -1
                  QM( IOPN( 2 ) ) = -1

                  CHANGE = .TRUE.

               END IF

*  Now deal with AND operators.
            ELSE IF( OPC .EQ. OPC__AND ) THEN

*  If both operands are created either by loading a single quality bit
*  (LDQ) or loading several bits (LDQA), replace the two LDQ(A)
*  instructions with NULLs and the AND instruction with an LDQA
*  instruction. Replace the first mask with the bit-wise OR of the two
*  masks, and replace the second mask with -1.
               IF( ( OPN( 1 ) .EQ. OPC__LDQ .OR.
     :               OPN( 1 ) .EQ. OPC__LDQA ) .AND.
     :             ( OPN( 2 ) .EQ. OPC__LDQ .OR.
     :               OPN( 2 ) .EQ. OPC__LDQA ) ) THEN
                  OPCODE( IOPN( 1 ) ) = OPC__NULL
                  OPCODE( IOPN( 2 ) ) = OPC__NULL
                  OPCODE( I ) = OPC__LDQA

                  MASK1 = MASKS( QM ( IOPN( 1 ) ) )
                  MASK2 = MASKS( QM ( IOPN( 2 ) ) )
                  MASKS( QM ( IOPN( 1 ) ) ) = IOR( MASK1, MASK2 )
                  MASKS( QM ( IOPN( 2 ) ) ) = -1

                  QM( I ) =  QM ( IOPN( 1 ) )
                  QM( IOPN( 1 ) ) = -1
                  QM( IOPN( 2 ) ) = -1

                  CHANGE = .TRUE.

               END IF

*  Now deal with NOT operators.
            ELSE IF( OPC .EQ. OPC__NOT ) THEN

*  If the operand is created by loading a single quality bit (LDQ),
*  replace the LDQ instruction with NULL and the NOT instruction with
*  an LDQN instruction.
               IF( OPN( 1 ) .EQ. OPC__LDQ ) THEN
                  OPCODE( IOPN( 1 ) ) = OPC__NULL
                  OPCODE( I ) = OPC__LDQN

                  QM( I ) =  QM ( IOPN( 1 ) )
                  QM( IOPN( 1 ) ) = -1

                  CHANGE = .TRUE.

               END IF

            END IF

         END IF

      END DO

*  Shuffle all the valid mask values to the start of the masks array
      NMASKS = 0

      DO I = 1, NQNAME

         IF( MASKS( I ) .NE. -1 ) THEN
            NMASKS = NMASKS + 1
            MASKS( NMASKS ) = MASKS( I )
         END IF

      END DO

*  Pad out the end of the masks array with invalid mask values.
      DO I = NMASKS + 1, NQNAME
         MASKS( I ) = -1
      END DO

*  If any changes were introduced on this pass, do another pass.
      IF( CHANGE ) GO TO 10

      END
