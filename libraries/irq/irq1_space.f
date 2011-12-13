      SUBROUTINE IRQ1_SPACE( NSPACE, NOPC, OPCODE, INDEX, STATUS )
*+
*  Name:
*     IRQ1_SPACE

*  Purpose:
*     Produce space in a list of op. codes.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ1_SPACE( NSPACE, NOPC, OPCODE, INDEX, STATUS )

*  Description:
*     A specified number of NULL instructions are introduced into the
*     OPCODE array, starting after an instruction with specified index.
*     The space is made by shuffling non-NULL instructions up or down
*     to remove previously existing NULL instructions. If the required
*     space cannot be made, an error is reported.

*  Arguments:
*     NSPACE = INTEGER (Given)
*        The number of spaces to be created.
*     NOPC = INTEGER (Given)
*        The number of op. codes in OPCODE.
*     OPCODE( NOPC ) = INTEGER (Given and Returned)
*        The list of instruction codes.
*     INDEX = INTEGER (Given and Returned)
*        On entry, I is index of the instruction after which the space
*        is to be created. On exit, I is the index of the first created
*        space.
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
      INCLUDE 'IRQ_PAR'          ! IRQ CONSTANTS.
      INCLUDE 'IRQ_ERR'          ! IRQ error values.

*  Arguments Given:
      INTEGER NSPACE
      INTEGER NOPC

*  Arguments Given and Returned:
      INTEGER OPCODE( NOPC )
      INTEGER INDEX

*  Status:
      INTEGER STATUS             ! Global status

*  Include instruction definitions:
      INCLUDE 'IRQ_OPC'

*  Local Variables:
      INTEGER I                  ! Instruction index in input.
      INTEGER II                 ! Index of last non-NULL instruction
                                 ! before position at which space is to
                                 ! be introduced in the output.
      INTEGER LAST               ! Index of last non-NULL instruction in
                                 ! output.
      INTEGER OPC                ! Current instruction code.
      INTEGER OUT                ! Instruction index in output.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise output pointer.
      OUT = 1

*  Remove all NULLs from OPCODE.
      DO I = 1, NOPC

         OPC = OPCODE( I )
         IF( OPC .NE. OPC__NULL ) THEN

            IF( I .LE. INDEX ) II = OUT

            OPCODE( OUT ) = OPC
            OUT = OUT + 1

         END IF

      END DO

*  Find the index of the last non-NULL instruction.
      LAST = OUT - 1

*  Report an error if there is insufficient room in the array to create
*  the requested space.
      IF( NSPACE .GT. NOPC - LAST ) THEN
         STATUS = IRQ__NOSPA
         CALL MSG_SETI( 'N', NSPACE )
         CALL ERR_REP( 'IRQ1_SPACE_ERR1',
     :              'IRQ1_SPACE: Unable to find ^N NULL instructions '//
     :              '(programming error)', STATUS )

*  Otherwise, create the space.
      ELSE

         DO I = LAST, II + 1, -1
            OPCODE( I + NSPACE ) = OPCODE( I )
         END DO

*  Fill in the created gaps with NULLs
         DO I = II + 1, II + NSPACE
            OPCODE( I ) = OPC__NULL
         END DO

         DO I = LAST + NSPACE + 1, NOPC
            OPCODE( I ) = OPC__NULL
         END DO

*  Return the index of the first created space.
         INDEX = II + 1

      END IF

      END
