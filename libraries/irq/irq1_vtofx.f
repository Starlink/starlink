      SUBROUTINE IRQ1_VTOFX( N, QVALUE, NOPC, OPCODE, STATUS )
*+
*  Name:
*     IRQ1_VTOFX

*  Purpose:
*     Replace a "LOAD QUALITY" instruction with "LOAD constant".

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ1_VTOFX( N, QVALUE, NOPC, OPCODE, STATUS )

*  Description:
*     A compiled quality expression contains a list of instructions
*     which describe the operations which must be performed to evaluate
*     the quality expression. One of these "op. codes" instructs the
*     processing routine to put a quality value on the stack. This is
*     the "LOAD QUALITY" op. code.
*
*     The supplied list of op. codes is searched for the Nth "LOAD
*     QUALITY" op. code which is replaced by the "LOAD FALSE" or "LOAD
*     TRUE" op. codes, depending on the value of QVALUE. These two op.
*     codes cause the processing routine to load either .false.  or a
*     .true. value on to the stack.
*
*  Arguments:
*     N = INTEGER (Given)
*        The position of the quality to be replaced, within the list of
*        all qualities referenced in the quality expression.
*     QVALUE = LOGICAL (Given)
*        The constant value with which to replace the variable quality.
*     NOPC = INTEGER (Given)
*        The number of op codes in OPCODE.
*     OPCODE( NOPC ) = INTEGER (Given and Returned)
*        The op codes.
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
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRQ_PAR'          ! IRQ constants.

*  Arguments Given:
      INTEGER N
      LOGICAL QVALUE
      INTEGER NOPC

*  Arguments Given and Returned:
      INTEGER OPCODE( NOPC )

*  Status:
      INTEGER STATUS             ! Global status

*  Include definitions of all instructions an op. codes:
      INCLUDE 'IRQ_OPC'

*  Local Variables:
      INTEGER I                  ! Loop count.
      INTEGER NFOUND             ! Current no. of LOAD QUALITY op. codes
                                 ! found.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the number of "LOAD QUALITY" op. codes found to
*  zero.
      NFOUND = 0

*  Loop round all the op. codes in the compiled quality expression.
      DO I = 1, NOPC

*  If the current op code is "LOAD QUALITY" (LDQ), increment the number
*  found so far.
         IF( OPCODE( I ) .EQ. OPC__LDQ ) THEN
            NFOUND = NFOUND + 1

*  Check to see if the quality to be loaded is the one which is to
*  be replaced by a fixed value.
            IF( NFOUND .EQ. N ) THEN

*  If it is, then replace the op code with a "LOAD TRUE" or "LOAD FALSE"
*  op code, dependant on the supplied constant quality value.
               IF( QVALUE ) THEN
                  OPCODE( I ) = OPC__LDT

               ELSE
                  OPCODE( I ) = OPC__LDF

               END IF

            END IF

         END IF

      END DO

      END
