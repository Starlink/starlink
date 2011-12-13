      SUBROUTINE ARD1_CHECK( ADDINP, INSIZ, IN, OUTSIZ, OUT, STATUS )
*+
*  Name:
*     ARD1_CHECK

*  Purpose:
*     Check the order of operands and operators

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_CHECK( ADDINP, INSIZ, IN, OUTSIZ, OUT, STATUS )

*  Description:
*     If ADDINP is .TRUE., a "Load Supplied Mask" instruction is
*     inserted at the start of the returned expression stack.  A check
*     is then made that operators and operands occur in the correct
*     order in the supplied expression stack. .OR.  operators are
*     inserted anywhere where two operands occur without any
*     intervening operator. An error is reported if two operators occur
*     without any intervening operand.

*  Arguments:
*     ADDINP = LOGICAL (Given)
*        .TRUE. if an INPUT keyword is to be inserted at the start of
*        the ARD description.
*     INSIZ = INTEGER (Given)
*        The size of the input expression stack.
*     IN( INSIZ ) = INTEGER (Given)
*        The input expression stack. The last value in this array
*        should be a code representing the "End Expression" instruction.
*     OUTSIZ = INTEGER (Given)
*        The size of the output expression stack.
*     OUT( OUTSIZ ) = INTEGER (Returned)
*        The output expression stack. This is a copy of the input
*        expression stack, with the potential addition of some OR
*        operators and an LKR instruction.
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
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ARD_CONST'        ! ARD_ private constants
      INCLUDE 'ARD_ERR'          ! ARD_ error constants

*  Global Variables:
      INCLUDE 'ARD_COM'          ! ARD common blocks
*        CMN_INSTR( ARD__NINST ) = CHARACTER * ( ARD__SZINS ) (Read)
*           Strings used to describe each instruction.
*        CMN_OPL( ARD__NINST ) = INTEGER (Read)
*           If set to one, then an operand is needed to the left of the
*           corresponding operator.
*        CMN_OPR( ARD__NINST ) = INTEGER (Read)
*           If set to one, then an operand is needed to the right of the
*           corresponding operator.

*  Arguments Given:
      LOGICAL ADDINP
      INTEGER INSIZ
      INTEGER IN( INSIZ )
      INTEGER OUTSIZ

*  Arguments Returned:
      INTEGER OUT( OUTSIZ )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL ARD1_INIT         ! Initialise ARD common blocks.

*  Local Variables:
      INTEGER
     :        IIN,               ! Pointer to next input value
     :        INVAL,             ! Current input value
     :        IOUT,              ! Pointer to next free output element
     :        NARG               ! No. of arguments still to be read

      LOGICAL
     :        OPNEXT             ! Is an operator required next?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If an INPUT keyword is to be included at the start of the ARD
*  description, insert a Load Supplied Mask instruction into the output
*  stack. Also store the number of arguments following the LSM
*  instruction, and set a flag indicating if an operator or operand is
*  expected next (an operator is expected if the instruction looks like
*  an operand from the right). Initialise the pointer to the next free
*  element in the output stack.
      IF( ADDINP ) THEN
         INVAL = ARD__LSM
         OUT( 1 ) = INVAL
         NARG = CMN_NARG( INVAL )
         OPNEXT = ( CMN_OPR( INVAL ) .EQ. 0 )
         IOUT = 2

*  If an INPUT keyword is not to be inserted, initialise things
*  appropriately.
      ELSE
         INVAL = ARD__NUL
         NARG = 0
         OPNEXT = .FALSE.
         IOUT = 1

      END IF

*  Initialise the pointer to the next element to be read from the input
*  stack.
      IIN = 1

*  Loop round the input stack until an "End Expression" instruction is
*  found.
      DO WHILE( INVAL .NE. ARD__END )

*  Report an error if the end of the input stack has been passed.
         IF( IIN .GT. INSIZ ) THEN
            STATUS = ARD__INTER
            CALL MSG_SETC( 'OP1', CMN_INSTR( ARD__END ) )
            CALL ERR_REP( 'ARD1_CHECK_ERR1', 'No ^OP1 instruction '//
     :                    'found in ARD1_CHECK (programming error).',
     :                    STATUS )
            GO TO 999
         END IF

*  Check there is room for at least two more values on the output stack.
         IF( IOUT .GE. OUTSIZ - 1 ) THEN
            STATUS = ARD__INTER
            CALL MSG_SETI( 'S', OUTSIZ )
            CALL ERR_REP( 'ARD1_CHECK_ERR2', 'Output stack size (^S) '//
     :                    'not large enough in ARD1_CHECK '//
     :                    '(programming error).', STATUS )
            GO TO 999
         END IF

*  If the current input value is to be used as an instruction argument
*  rather than an instruction code, just copy it to the output stack.
         IF( NARG .GT. 0 ) THEN
            OUT( IOUT ) = IN( IIN )
            IOUT = IOUT + 1

*  Decrement the number of arguments still to be read.
            NARG = NARG - 1

*  Otherwise...
         ELSE

*  ...store the current instruction code from the input stack.
            INVAL = IN( IIN )

*  If an operator was expected...
            IF( OPNEXT ) THEN

*  ...but the instruction obtained from the input stack looks like an
*  operand from the left ( "(" and "NOT" for instance ), then insert an
*  OR operator into the output stack.
               IF( CMN_OPL( INVAL ) .EQ. 0 ) THEN
                  OUT( IOUT ) = ARD__OR
                  IOUT = IOUT + 1
               END IF

*  If an operand was expected...
            ELSE

*  ...but the instruction obtained from the input stack does not look
*  like an operand from the left, then report an error.
               IF( CMN_OPL( INVAL ) .NE. 0 ) THEN
                  STATUS = ARD__MISOP
                  CALL MSG_SETC( 'OP1', CMN_INSTR( OUT( IOUT - 1 ) ) )
                  CALL MSG_SETC( 'OP2', CMN_INSTR( INVAL ) )
                  CALL ERR_REP( 'ARD1_CHECK_ERR3', 'No region keyword'//
     :                          ' found between adjacent ^OP1 and '//
     :                          '^OP2 operators in an ARD description.',
     :                          STATUS )
                  GO TO 999
               END IF

            END IF

*  Put the current instruction into the output stack.
            OUT( IOUT ) = INVAL
            IOUT = IOUT + 1

*  Set up the number of arguments which should follow the instruction
*  code in the input stack.
            NARG = CMN_NARG( INVAL )

*  An operator is expected next if the current instruction looks like an
*  operand from the right ( ")" for instance).
            OPNEXT = ( CMN_OPR( INVAL ) .EQ. 0 )

         END IF

*  Increment the pointer to the next value in the input stack.
         IIN = IIN + 1

      END DO

*  Jump to here if an error occurs.
 999  CONTINUE

      END
