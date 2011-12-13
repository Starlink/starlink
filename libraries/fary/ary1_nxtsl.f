      SUBROUTINE ARY1_NXTSL( BLOCK, SLOT, NEXT, STATUS )
*+
*  Name:
*     ARY1_NXTSL

*  Purpose:
*     Find the next slot which has been used in a specified common
*     block.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_NXTSL( BLOCK, SLOT, NEXT, STATUS )

*  Description:
*     The routine finds the next used slot in a common block following
*     the one supplied via the SLOT argument. It is intended to allow
*     the caller to obtain a list of all slots currently in use. The
*     first slot is obtained by supplying a value of zero for the SLOT
*     argument. A value of zero is returned for the NEXT argument if
*     there are no further slots in use.

*  Arguments:
*     BLOCK = INTEGER (Given)
*        The common block to search. The integer symbolic constants
*        ARY__DCB, ARY__ACB and ARY__MCB are available to identify
*        these.
*     SLOT = INTEGER (Given)
*        The slot number from which to start the search. A value of
*        zero should be supplied to search for the first slot in use.
*     NEXT = INTEGER (Returned)
*        The number of the slot found. A value of zero is returned if
*        there are no more slots in use.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This interface to the common blocks is provided so that a more
*     efficient implementation (e.g. using linked lists) might be added
*     later.

*  Algorithm:
*     -  Handle each common block separately.
*     -  Search the list of used slots, starting at the slot provided,
*     to find the next used slot. Return zero if none was found.
*     -  Report an error if the common block specified is invalid.

*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JUN-1989 (RFWS):
*        Original version.
*     22-AUG-1989 (RFWS):
*        Added EXTERNAL statement to ensure ARY1_INIT is linked.
*     7-SEP-1989 (RFWS):
*        Added message token for routine name, to prevent '$' from
*        affecting error messages.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_USED( ARY__MXDCB ) = LOGICAL (Read)
*           Whether a slot in the DCB has been used.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_USED( ARY__MXACB ) = LOGICAL (Read)
*           Whether a slot in the ACB has been used.

      INCLUDE 'ARY_MCB'          ! ARY_ Mapping Control Block
*        MCB_USED( ARY__MXMCB ) = LOGICAL (Read)
*           Whether a slot in the MCB has been used.

*  Arguments Given:
      INTEGER BLOCK
      INTEGER SLOT

*  Arguments Returned:
      INTEGER NEXT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL ARY1_INIT         ! Initialise common blocks

*  Local variables:
      INTEGER I                  ! Loop counter for slots

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      NEXT = 0

*  If requested, search for the next slot in use in the Data Control
*  Block.
      IF ( BLOCK .EQ. ARY__DCB ) THEN
         DO 1 I = SLOT + 1, ARY__MXDCB

*  Quit the search when a slot has been found.
            IF ( DCB_USED( I ) ) THEN
               NEXT = I
               GO TO 2
            END IF
1        CONTINUE
2        CONTINUE

*  If requested, search for the next slot in use in the Access Control
*  Block.
      ELSE IF ( BLOCK .EQ. ARY__ACB ) THEN
         DO 3 I = SLOT + 1, ARY__MXACB

*  Quit the search when a slot has been found.
            IF ( ACB_USED( I ) ) THEN
               NEXT = I
               GO TO 4
            END IF
3        CONTINUE
4        CONTINUE

*  If requested, search for the next slot in use in the Mapping Control
*  Block.
      ELSE IF ( BLOCK .EQ. ARY__MCB ) THEN
         DO 5 I = SLOT + 1, ARY__MXMCB

*  Quit the search when a slot has been found.
            IF ( MCB_USED( I ) ) THEN
               NEXT = I
               GO TO 6
            END IF
5        CONTINUE
6        CONTINUE

*  If the common block specified is not valid, then report an error.
      ELSE
         STATUS = ARY__FATIN
         CALL MSG_SETC( 'ROUTINE', 'ARY1_NXTSL' )
         CALL MSG_SETI( 'BADBLOCK', BLOCK )
         CALL ERR_REP( 'ARY1_NXTSL_BBLK',
     :   'Routine ^ROUTINE called with an invalid BLOCK argument ' //
     :   'of ^BADBLOCK (internal programming error).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_NXTSL', STATUS )

      END
