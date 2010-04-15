      SUBROUTINE ARY1_DEL( IACB, STATUS )
*+
*  Name:
*     ARY1_DEL

*  Purpose:
*     Perform a deletion operation on an ACB entry.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_DEL( IACB, STATUS )

*  Description:
*     The routine performs a deletion operation on an entry in the ACB.
*     If the specified ACB entry describes a base array, then it and
*     all other ACB entries which refer to the same data object are
*     annulled, and the data object is erased.  If the ACB entry does
*     not describe a base array, then that ACB entry (alone) is
*     annulled, and the data object is not erased.

*  Arguments:
*     IACB = INTEGER (Given and Returned)
*        Index to the ACB entry on which the deletion operation is to
*        be performed. A value of zero is returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine attempts to execute even if STATUS is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.

*  Side Effects:
*     -   If a data object is erased, then all array identifiers which
*     refer to it become invalid.

*  Algorithm:
*     -  Save the error context on entry.
*     -  If the ACB entry specified does not describe a base array, then
*     simply annul the entry.
*     -  Otherwise, obtain the index of the data object entry in the DCB
*     and set its disposal mode to DELETE.
*     -  Loop through all active entries in the ACB and select those
*     which refer to the data object to be erased. Annull all these ACB
*     entries, causing the data object reference count to fall to zero
*     so that it is erased.
*     -  Reset the initial ACB index to zero.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-AUG-1989 (RFWS):
*        Original version.
*     {enter_changes_here}

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
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_DSP( ARY__MXDCB ) = CHARACTER * ( ARY__SZDSP ) (Write)
*           Data object disposal mode.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_CUT( ARY__MXACB ) = LOGICAL (Read)
*           Whether the ACB entry describes a cut.
*        ACB_IDCB( ARY__MXACB ) = INTEGER (Read)
*           Index to the data object entry in the DCB.

*  Arguments Given and Returned:
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACBA              ! ACB entry to be annulled
      INTEGER IACBT              ! ACB entry to be tested
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER NEXT               ! Next active ACB slot number
      INTEGER TSTAT              ! Temporary status variable

*.


*  Save the STATUS value and mark the error stack.
      TSTAT = STATUS
      CALL ERR_MARK

*  If the ACB entry does not refer to a base array, then simply annul
*  the entry.
      STATUS = SAI__OK
      IF ( ACB_CUT( IACB ) ) THEN
         CALL ARY1_ANL ( IACB, STATUS )

*  Otherwise, obtain the index of the data object entry in the DCB and
*  set its disposal mode to DELETE. The object will then be erased once
*  its reference count drops to zero.
      ELSE
         IDCB = ACB_IDCB( IACB )
         DCB_DSP( IDCB ) = 'DELETE'

*  Loop through all active entries in the ACB.
         IACBT = 0
         NEXT = 0
1        CONTINUE                ! Start of 'DO WHILE' loop
         CALL ARY1_NXTSL( ARY__ACB, IACBT, NEXT, STATUS )
         IF ( ( STATUS .EQ. SAI__OK ) .AND. ( NEXT .NE. 0 ) ) THEN
            IACBT = NEXT

*  Select those entries which refer to the data object to be erased and
*  annul them. This process eventually reduces the object's reference
*  count to zero, causing it to be erased.
            IF ( ACB_IDCB( IACBT ) .EQ. IDCB ) THEN
               IACBA = IACBT
               CALL ARY1_ANL( IACBA, STATUS )
            END IF
            GO TO 1
         END IF
      END IF

*  Reset the initial ACB index to zero.
      IACB = 0

*  Annul any error if STATUS was previously bad, otherwise let the new
*  error report stand.
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( TSTAT .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            STATUS = TSTAT

*  Call error tracing routine if appropriate.
         ELSE
            CALL ARY1_TRACE( 'ARY1_DEL', STATUS )
         END IF
      ELSE
         STATUS = TSTAT
      END IF

*  Release error stack.
      CALL ERR_RLSE

      END
