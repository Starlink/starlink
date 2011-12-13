      SUBROUTINE NDF1_RLS( BLOCK, SLOT, STATUS )
*+
*  Name:
*     NDF1_RLS

*  Purpose:
*     Release a slot in a common block.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_RLS( BLOCK, SLOT, STATUS )

*  Description:
*     The routine releases a slot which is currently in use in one of
*     the common blocks used by the NDF_ facility, making it available
*     for re-use.

*  Arguments:
*     INTEGER = BLOCK (Given)
*        The common block in which the slot is to be released. The
*        integer symbolic constants NDF__DCB, NDF__ACB and NDF__PCB are
*        available to identify these.
*     INTEGER = SLOT (Given and Returned)
*        The number of the slot to be released in the range 1 to
*        NDF__MXxxx, where xxx is DCB, ACB or PCB, as appropriate. A
*        value of zero is returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine attempts to execute even if STATUS is set on
*     entry, although no error report will be made if it subsequently
*     fails under these circumstances.

*  Algorithm:
*     -  Save the error context on entry.
*     -  Report an error if the slot number specified is invalid, or if
*     the slot is not in use.
*     -  Mark the appropriate slot in the specified common block as not
*     being in use.
*     -  Report an error if the common block specified is not valid.
*     -  Reset the SLOT argument to zero.
*     -  Restore the error context.

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
*     19-SEP-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     2-OCT-1989 (RFWS):
*        Minor corrections to prologue/comments.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_USED( NDF__MXDCB ) = LOGICAL (Read and Write)
*           Whether a slot in the DCB has been used.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_USED( NDF__MXACB ) = LOGICAL (Read and Write)
*           Whether a slot in the ACB has been used.

      INCLUDE 'NDF_PCB'          ! NDF_ Placeholder Control Block
*        PCB_USED( NDF__MXPCB ) = LOGICAL (Read and Write)
*           Whether a slot in the PCB has been used.

*  Arguments Given:
      INTEGER BLOCK

*  Arguments Given and Returned:
      INTEGER SLOT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL NDF1_INIT         ! Initialise common blocks

*  Local variables:
      INTEGER TSTAT              ! Temporary local status value

*.

*  Save the STATUS value and mark the error stack.
      TSTAT = STATUS
      CALL ERR_MARK

*  If requested, attempt to release a slot in the Data Control Block.
      STATUS = SAI__OK
      IF ( BLOCK .EQ. NDF__DCB ) THEN

*  If the slot number is not valid, then report an error.
         IF ( ( SLOT .LT. 1 ) .OR. ( SLOT .GT. NDF__MXDCB ) ) THEN
            STATUS = NDF__FATIN
            CALL MSG_SETC( 'ROUTINE', 'NDF1_RLS' )
            CALL MSG_SETI( 'SLOT', SLOT )
            CALL MSG_SETI( 'MXSLOT', NDF__MXDCB )
            CALL ERR_REP( 'NDF1_RLS_DCB',
     :      'Routine ^ROUTINE called with an invalid SLOT argument ' //
     :      'of ^SLOT; this value should lie between 1 and ^MXSLOT ' //
     :      'inclusive (internal programming error).', STATUS )

*  If the specified slot is not in use, then report an error.
         ELSE IF ( .NOT. DCB_USED( SLOT ) ) THEN
            STATUS = NDF__FATIN
            CALL MSG_SETC( 'ROUTINE', 'NDF1_RLS' )
            CALL MSG_SETI( 'SLOT', SLOT )
            CALL ERR_REP( 'NDF1_RLS_DCBU',
     :      'Routine ^ROUTINE called with a SLOT argument of ^SLOT; ' //
     :      'this Data Control Block slot is not currently in use ' //
     :      '(internal programming error).', STATUS )

*  Release the slot.
         ELSE
            DCB_USED( SLOT ) = .FALSE.
         END IF

*  If requested, attempt to release a slot in the Access Control Block.
      ELSE IF ( BLOCK .EQ. NDF__ACB ) THEN
         IF ( ( SLOT .LT. 1 ) .OR. ( SLOT .GT. NDF__MXACB ) ) THEN
            STATUS = NDF__FATIN
            CALL MSG_SETC( 'ROUTINE', 'NDF1_RLS' )
            CALL MSG_SETI( 'SLOT', SLOT )
            CALL MSG_SETI( 'MXSLOT', NDF__MXACB )
            CALL ERR_REP( 'NDF1_RLS_ACB',
     :      'Routine ^ROUTINE called with an invalid SLOT argument ' //
     :      'of ^SLOT; this value should lie between 1 and ^MXSLOT ' //
     :      'inclusive (internal programming error).', STATUS )

*  If the specified slot is not in use, then report an error.
         ELSE IF ( .NOT. ACB_USED( SLOT ) ) THEN
            STATUS = NDF__FATIN
            CALL MSG_SETC( 'ROUTINE', 'NDF1_RLS' )
            CALL MSG_SETI( 'SLOT', SLOT )
            CALL ERR_REP( 'NDF1_RLS_ACBU',
     :      'Routine ^ROUTINE called with a SLOT argument of ^SLOT; ' //
     :      'this Access Control Block slot is not currently in use ' //
     :      '(internal programming error).', STATUS )

*  Release the slot.
         ELSE
            ACB_USED( SLOT ) = .FALSE.
         END IF

*  If requested, attempt to release a slot in the Placeholder Control
*  Block.
      ELSE IF ( BLOCK .EQ. NDF__PCB ) THEN
         IF ( ( SLOT .LT. 1 ) .OR. ( SLOT .GT. NDF__MXPCB ) ) THEN
            STATUS = NDF__FATIN
            CALL MSG_SETC( 'ROUTINE', 'NDF1_RLS' )
            CALL MSG_SETI( 'SLOT', SLOT )
            CALL MSG_SETI( 'MXSLOT', NDF__MXPCB )
            CALL ERR_REP( 'NDF1_RLS_PCB',
     :      'Routine ^ROUTINE called with an invalid SLOT argument ' //
     :      'of ^SLOT; this value should lie between 1 and ^MXSLOT ' //
     :      'inclusive (internal programming error).', STATUS )

*  If the specified slot is not in use, then report an error.
         ELSE IF ( .NOT. PCB_USED( SLOT ) ) THEN
            STATUS = NDF__FATIN
            CALL MSG_SETC( 'ROUTINE', 'NDF1_RLS' )
            CALL MSG_SETI( 'SLOT', SLOT )
            CALL ERR_REP( 'NDF1_RLS_PCBU',
     :      'Routine ^ROUTINE called with a SLOT argument of ^SLOT; ' //
     :      'this Placeholder Control Block slot is not currently ' //
     :      'in use (internal programming error).', STATUS )

*  Release the slot.
         ELSE
            PCB_USED( SLOT ) = .FALSE.
         END IF

*  If the common block specified is not valid, then report an error.
      ELSE
         STATUS = NDF__FATIN
         CALL MSG_SETC( 'ROUTINE', 'NDF1_RLS' )
         CALL MSG_SETI( 'BADBLOCK', BLOCK )
         CALL ERR_REP( 'NDF1_RLS_BLOCK',
     :   'Routine ^ROUTINE called with an invalid BLOCK argument of ' //
     :   '^BADBLOCK (internal programming error).', STATUS )
      END IF

*  Reset the SLOT argument to zero.
      SLOT = 0

*  Annul any error if STATUS was previously bad, otherwise let the new
*  error report stand.
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( TSTAT .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            STATUS = TSTAT
         ELSE

*  Call error tracing routine if appropriate.
            CALL NDF1_TRACE( 'NDF1_RLS', STATUS )
         END IF
      ELSE
         STATUS = TSTAT
      END IF

*  Release the error stack.
      CALL ERR_RLSE

      END
