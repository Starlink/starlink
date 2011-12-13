      SUBROUTINE ARY1_ANL( IACB, STATUS )
*+
*  Name:
*     ARY1_ANL

*  Purpose:
*     Annul an array entry in the ACB.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_ANL( IACB, STATUS )

*  Description:
*     The routine annuls an array entry in the ACB. If the array is
*     currently mapped for access through this entry, then it is first
*     unmapped. The ACB entry is then annulled, i.e. the slot is
*     released and made available for re-use. If, as a result, the
*     reference count for the associated data object drops to zero,
*     then the object will be released from the ARY_ system and may be
*     deleted, according to its disposal mode.

*  Arguments:
*     IACB = INTEGER (Given and Returned)
*        The ACB entry to be annulled. A value of zero is returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine attempts to execute even if STATUS is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.

*  Algorithm:
*     -  Save the error context on entry.
*     -  If the array is currently mapped for access through the
*     specified ACB entry, then unmap it.
*     -  Annul the associated data object entry in the DCB and set the
*     DCB index to zero.
*     -  Release the ACB slot and reset the ACB index to zero.
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
*     1-AUG-1989 (RFWS):
*        Original version.
*     14-SEP-1989 (RFWS):
*        Changed call to ARY1_DANL to include new argument.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants

*  Global Variables:
      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_IDCB( ARY__MXACB ) = INTEGER (Read and Write)
*           Index to data object entry in the DCB.


*  Arguments Given and Returned:
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER TSTAT              ! Temporary status variable

*.

*  Save the STATUS value and mark the error stack.
      TSTAT = STATUS
      CALL ERR_MARK

*  If the array is currently mapped through this ACB entry, then unmap
*  it.
      STATUS = SAI__OK
      IF ( ACB_IMCB( IACB ) .GT. 0 ) THEN
         CALL ARY1_UMP( IACB, STATUS )
      END IF

*  Annul the associated data object and set the data object index in
*  the ACB to zero.
      CALL ARY1_DANL( .TRUE., ACB_IDCB( IACB ), STATUS )
      ACB_IDCB( IACB ) = 0

*  Release the ACB slot and reset the ACB index to zero.
      CALL ARY1_RLS( ARY__ACB, IACB, STATUS )
      IACB = 0

*  Annul any error if STATUS was previously bad, otherwise let the new
*  error report stand.
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( TSTAT .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            STATUS = TSTAT

*  Call error tracing routine if appropriate.
         ELSE
            CALL ARY1_TRACE( 'ARY1_ANL', STATUS )
         END IF
      ELSE
         STATUS = TSTAT
      END IF

*  Release error stack.
      CALL ERR_RLSE

      END
