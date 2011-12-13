      SUBROUTINE ARD1_ALTRP( SIZE, INSTR, STACK, ASTACK, OPCODE, MXSTK,
     :                       STATUS )
*+
*  Name:
*     ARD1_ALTRP

*  Purpose:
*     Convert algebraic expression to reverse polish form

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_ALTRP( SIZE, INSTR, STACK, ASTACK, OPCODE, MXSTK,
*                      STATUS )

*  Description:
*     The ARD expression supplied in "algebraic" form (i.e. as supplied
*     by the user) in INSTR is converted into reverse polish form, in
*     which instructions are stored in the order in which they must be
*     performed. The maximum size of the evaluation stack is returned.

*  Arguments:
*     SIZE = INTEGER (Given)
*        The size of the arrays.
*     INSTR( SIZE ) = INTEGER (Given)
*        The expression stack representing the ARD description in
*        algebraic form.
*     STACK( 0 : SIZE ) = INTEGER (Given and Returned)
*        Work space.
*     ASTACK( 0 : SIZE ) = INTEGER (Given and Returned)
*        Work space.
*     OPCODE( SIZE ) = INTEGER (Returned)
*        The ARD description in reverse polish form.
*     MXSTK = INTEGER (Returned)
*        The size of the evaluation stack needed to evaluate the
*        expression.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
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
*     23-FEB-1994 (DSB):
*        Original version (based on EDRS routine ALGTRP by R.F.
*        Warren-Smith).
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ARD_CONST'        ! ARD private constants
      INCLUDE 'ARD_ERR'          ! ARD error constants

*  Global Variables:
      INCLUDE 'ARD_COM'          ! ARD common blocks
*        CMN_NARG( ARD__NINST ) = INTEGER (Read)
*           The number of argument values following each instruction.
*        CMN_PRR( ARD__NINST ) = INTEGER (Read)
*           The precedence of the instruction (seen from the right).
*        CMN_PRL( ARD__NINST ) = INTEGER (Read)
*           The precedence of the instruction (seen from the left).
*        CMN_INSTR( ARD__NINST ) = CHARACTER*( ARD__SZINS ) (Read)
*           A description of each instruction.
*        CMN_DSTK( ARD__NINST ) = INTEGER (Read)
*           The change in size of the evaluation stack caused by each
*           instruction.

*  Arguments Given:
      INTEGER SIZE
      INTEGER INSTR( SIZE )

*  Arguments Given and Returned:
      INTEGER STACK( 0 : SIZE )
      INTEGER ASTACK( 0 : SIZE )

*  Arguments Returned:
      INTEGER OPCODE( SIZE )
      INTEGER MXSTK

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL ARD1_INIT         ! Initialise ARD common blocks

*  Local Variables:
      INTEGER
     :        IARG,              ! Index of current argument
     :        INST,              ! Current instruction code
     :        IOP,               ! Index to next input code
     :        NARG,              ! No. of instruction arguments
     :        NOPC,              ! Index into output stream
     :        STKSIZ,            ! Current size of evalution stack
     :        TOAS,              ! Index of top of argument stack
     :        TOS                ! Index of top of stack

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Pre-load an "End Expression" instruction on the bottom of the stack.
      STACK( 0 ) = ARD__END

*  Initialise pointers and counters.
      TOS = 0
      TOAS = 0
      IOP = 1
      NOPC = 0
      STKSIZ = 0
      MXSTK = 0
      INST = ARD__NUL

*  Loop round until the END instruction is output.
      DO WHILE( INST .NE. ARD__END )

*  If the top of stack and input stream have matching parentheses,
*  cancel them
         IF( ( STACK( TOS ) .EQ. ARD__OPN ) .AND.
     :       ( INSTR( IOP ) .EQ. ARD__CLS ) ) THEN
            TOS = TOS - 1
            IOP = IOP + 1

*  If the instruction on the top of the stack has a high enough
*  precedence, transfer it to the output stream.
         ELSE IF( CMN_PRR( STACK( TOS ) ) .GE.
     :            CMN_PRL( INSTR( IOP ) ) ) THEN
            INST = STACK( TOS )
            NOPC = NOPC + 1
            OPCODE( NOPC ) = INST
            TOS = TOS - 1

*  Transfer the instruction arguments from the argument stack to the
*  output stream.
            DO IARG = 1, CMN_NARG( INST )
               NOPC = NOPC + 1
               OPCODE( NOPC ) = ASTACK( TOAS )
               TOAS = TOAS - 1
            END DO

*  If a bracket appears in the output, it results from unpaired
*  parentheses in the input expression...report an error.
            IF( INST .EQ. ARD__OPN .OR. INST .EQ. ARD__CLS ) THEN
               STATUS = ARD__MSPAR

               IF( INST .EQ. ARD__OPN ) THEN
                  CALL MSG_SETC( 'CHAR', CMN_INSTR( ARD__CLS ) )
               ELSE
                  CALL MSG_SETC( 'CHAR', CMN_INSTR( ARD__OPN ) )
               END IF

               CALL ERR_REP( 'ARD1_ALTRP_ERR1', 'Missing character - '//
     :                       '"^CHAR" - in ARD description.', STATUS )
               GO TO 999
            END IF

*  Increment or decrement the size of the stack which would be used to
*  evaluate the expression.
            STKSIZ = STKSIZ + CMN_DSTK( INST )

*  Update the maximum stack size.
            MXSTK = MAX( MXSTK, STKSIZ )

*  Otherwise, transfer the next instruction to the stack.
         ELSE
            TOS = TOS + 1
            STACK( TOS ) = INSTR( IOP )

*  Instruction arguments are kept on a separate "argument stack". Copy
*  arguments for the next instruction to the argument stack in reverse
*  order so that when they are popped off the stack they will appear in
*  the correct forward order, ready for storing in the output stream.
            NARG = CMN_NARG( INSTR( IOP ) )

            DO IARG = NARG, 1, -1
               TOAS = TOAS + 1
               ASTACK( TOAS ) = INSTR( IOP + IARG )
            END DO

*  Store the pointer to the next instruction.
            IOP = IOP + 1 + NARG

         END IF

      END DO

*  Jump to here if an error occurs.
 999  CONTINUE

      END
