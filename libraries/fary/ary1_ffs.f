      SUBROUTINE ARY1_FFS( BLOCK, SLOT, STATUS )
*+
*  Name:
*     ARY1_FFS

*  Purpose:
*     Find a free slot in a common block.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_FFS( BLOCK, SLOT, STATUS )

*  Description:
*     The routine finds a free slot in one of the common blocks
*     maintained by the ARY_ system. The number of a free slot is
*     returned and the slot is marked as used. An error is reported if
*     no further free slots exist.

*  Arguments:
*     BLOCK = INTEGER (Given)
*        The block in which a free slot is required.  The integer
*        symbolic constants ARY__ACB, ARY__DCB, ARY__MCB and ARY__PCB
*        are available to identify these.
*     SLOT = INTEGER (Returned)
*        The number of a free slot in the requested block.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If STATUS is set on entry, then a SLOT value of zero will be
*     returned, although no further processing will occur.
*     -  A SLOT value of zero will also be returned if the routine
*     should fail for any reason.

*  Algorithm:
*     -  Set an initial value of SLOT before checking the inherited
*     status.
*     -  The relevant block is searched to find a slot which has not
*     been marked as used.
*     -  If a free slot is found, it is marked as used and its number is
*     returned.
*     -  If there is no free slot available, then report an error.
*     -  Report an error if the BLOCK argument supplied is not valid.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-MAY-1989 (RFWS):
*        Original version.
*     12-JUN-1989 (RFWS):
*        Adapted to new common block structure.
*     16-AUG-1989 (RFWS):
*        Changed so that SLOT is always returned as zero unless the
*        routine succeeds.
*     18-AUG-1989 (RFWS):
*        Changed to use a message token for the routine name, to prevent
*        '$' from affecting the message.
*     19-SEP-1989 (RFWS):
*        Added support for the Placeholder Control Block.
*     9-OCT-1989 (RFWS):
*        Updated prologue description of the BLOCK argument.
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
*        DCB_USED = LOGICAL( ARY__MXDCB ) (Read and Write)
*           Whether a slot in the DCB has been used.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_USED = LOGICAL( ARY__MXACB ) (Read and Write)
*           Whether a slot in the ACB has been used.

      INCLUDE 'ARY_MCB'          ! ARY_ Mapping Control Block
*        MCB_USED = LOGICAL( ARY__MXMCB ) (Read and Write)
*           Whether a slot in the MCB has been used.

      INCLUDE 'ARY_PCB'          ! ARY_ Placeholder Control Block
*        PCB_USED = LOGICAL( ARY__MXPCB ) (Read and Write)
*           Whether a slot in the PCB has been used.

*  Arguments Given:
      INTEGER BLOCK

*  Arguments Returned:
      INTEGER SLOT

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      EXTERNAL ARY1_INIT         ! Initialise common blocks

*  Local variables:
      INTEGER I                  ! Loop counter for slots

*.

*  Set an initial value for the SLOT argument.
      SLOT = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If requested, search for a free slot in the Data Control Block.
      IF ( BLOCK .EQ. ARY__DCB ) THEN
         DO 1 I = 1, ARY__MXDCB
            IF ( .NOT. DCB_USED( I ) ) THEN
               DCB_USED( I ) = .TRUE.
               SLOT = I
               GO TO 2
            END IF
1        CONTINUE

*  If no free slot could be found, then report an error.
         STATUS = ARY__EXSLT
         CALL MSG_SETI( 'NSLOT', ARY__MXDCB )
         CALL ERR_REP( 'ARY1_FFS_DCB',
     :   'All ^NSLOT slots allocated for entries in the ARY_ ' //
     :   'facility Data Control Block have been used up.', STATUS )
2        CONTINUE

*  If requested, search for a free slot in the Access Control Block.
      ELSE IF ( BLOCK .EQ. ARY__ACB ) THEN
         DO 3 I = 1, ARY__MXACB
            IF ( .NOT. ACB_USED( I ) ) THEN
               ACB_USED( I ) = .TRUE.
               SLOT = I
               GO TO 4
            END IF
3        CONTINUE

*  If no free slot could be found, then report an error.
         STATUS = ARY__EXSLT
         CALL MSG_SETI( 'NSLOT', ARY__MXACB )
         CALL ERR_REP( 'ARY1_FFS_ACB',
     :   'All ^NSLOT slots allocated for entries in the ARY_ ' //
     :   'facility Access Control Block have been used up.', STATUS )
4        CONTINUE

*  If requested, search for a free slot in the Mapping Control Block.
      ELSE IF ( BLOCK .EQ. ARY__MCB ) THEN
         DO 5 I = 1, ARY__MXMCB
            IF ( .NOT. MCB_USED( I ) ) THEN
               MCB_USED( I ) = .TRUE.
               SLOT = I
               GO TO 6
            END IF
5        CONTINUE

*  If no free slot could be found, then report an error.
         STATUS = ARY__EXSLT
         CALL MSG_SETI( 'NSLOT', ARY__MXMCB )
         CALL ERR_REP( 'ARY1_FFS_MCB',
     :   'All ^NSLOT slots allocated for entries in the ARY_ ' //
     :   'facility Mapping Control Block have been used up.', STATUS )
6        CONTINUE

*  If requested, search for a free slot in the Placeholder Control
*  Block.
      ELSE IF ( BLOCK .EQ. ARY__PCB ) THEN
         DO 7 I = 1, ARY__MXPCB
            IF ( .NOT. PCB_USED( I ) ) THEN
               PCB_USED( I ) = .TRUE.
               SLOT = I
               GO TO 8
            END IF
7        CONTINUE

*  If no free slot could be found, then report an error.
         STATUS = ARY__EXSLT
         CALL MSG_SETI( 'NSLOT', ARY__MXPCB )
         CALL ERR_REP( 'ARY1_FFS_PCB',
     :   'All ^NSLOT slots allocated for entries in the ARY_ ' //
     :   'facility Placeholder Control Block have been used up.',
     :   STATUS )
8        CONTINUE

*  If the block specified was invalid, then report an error.
      ELSE
         STATUS = ARY__FATIN
         CALL MSG_SETC( 'ROUTINE', 'ARY1_FFS' )
         CALL MSG_SETI( 'BADBLOCK', BLOCK )
         CALL ERR_REP( 'ARY1_FFS_BLOCK',
     :   'Routine ^ROUTINE called with an invalid BLOCK argument of ' //
     :   '^BADBLOCK (internal programming error).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_FFS', STATUS )

      END
