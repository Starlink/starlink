      SUBROUTINE ARY_DELET( IARY, STATUS )
*+
*  Name:
*     ARY_DELET

*  Purpose:
*     Delete an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_DELET( IARY, STATUS )

*  Description:
*     The routine deletes the specified array. If this is a base array,
*     then the associated data object is erased and all array
*     identifiers which refer to it (or to sections derived from it)
*     become invalid. If the array is mapped for access, then it is
*     first unmapped.  If an array section is specified, then this
*     routine is equivalent to calling ARY_ANNUL.

*  Arguments:
*     IARY = INTEGER (Given and Returned)
*        Identifier for the array to be deleted. A value of ARY__NOID
*        is returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine attempts to execute even if STATUS is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.
*     -  A value of ARY__NOID is always returned for the IARY argument,
*     even if the routine should fail. This constant is defined in the
*     include file ARY_PAR.

*  Algorithm:
*     -  Save the error context on entry.
*     -  Import the array identifier.
*     -  Check that DELETE access to the array is available.
*     -  If so, then perform a deletion operation on the ACB entry.
*     -  Reset the identifier value.
*     -  Restore the error context.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
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
*     2-AUG-1989 (RFWS):
*        Original version.
*     13-SEP-1989 (RFWS):
*        Added check that DELETE access to the array is available.
*     16-MAR-1990 (RFWS):
*        Ensure that the IARY value is reset even if the routine fails.
*     26-MAR-1990 (RFWS):
*        Improved prologue.
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

*  Arguments Given and Returned:
      INTEGER IARY

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to array entry in the ACB
      INTEGER TSTAT              ! Temporary status variable

*.

*  Save the STATUS value and mark the error stack.
      TSTAT = STATUS
      CALL ERR_MARK

*  Import the array identifier.
      STATUS = SAI__OK
      CALL ARY1_IMPID( IARY, IACB, STATUS )

*  Check that DELETE access to the array is available.
      CALL ARY1_CHACC( IACB, 'DELETE', STATUS )

*  If access is available, then perform a deletion operation on the ACB
*  entry. Reset the identifier value.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL ARY1_DEL( IACB, STATUS )
      END IF
      IARY = ARY__NOID

*  Annul any error if STATUS was previously bad, otherwise let the new
*  error report stand. Release the error stack.
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( TSTAT .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            STATUS = TSTAT

*  Report context information and call error tracing routine if
*  appropriate.
         ELSE
            CALL ERR_REP( 'ARY_DELET_ERR',
     :      'ARY_DELET: Error deleting an array.', STATUS )
            CALL ARY1_TRACE( 'ARY_DELET', STATUS )
         END IF
      ELSE
         STATUS = TSTAT
      END IF

*  Release error stack.
      CALL ERR_RLSE

      END
