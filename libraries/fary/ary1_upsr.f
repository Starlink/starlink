      SUBROUTINE ARY1_UPSR( COPY, MLOC, STATUS )
*+
*  Name:
*     ARY1_UPSR

*  Purpose:
*     Unmap a simple array component mapped for READ access.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_UPSR( COPY, MLOC, STATUS )

*  Description:
*     The routine unmaps a component of a simple array which has
*     previously been mapped for READ access.

*  Arguments:
*     COPY = LOGICAL (Given)
*        Whether mapped access is via a "copy" of the actual data; this
*        indicates whether or not the locator MLOC is associated with a
*        temporary object.
*     MLOC = CHARACTER * ( DAT__SZLOC ) (Given and Returned)
*        Locator to the HDS object mapped to provide memory locations
*        for the data. This locator will be annulled and reset to a
*        blank string by this routine. If it is associated with a
*        temporary object, then this will be erased.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine attempts to execute even if STATUS is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.
*     -  No entries in the ACB or MCB are updated by this routine.

*  Algorithm:
*     -  Save the error context on entry.
*     -  If mapped access is via a "copy", then annul the temporary
*     object containing the copy (causing it to be erased).
*     -  Otherwise, if access is direct via HDS, then annul the
*     associated locator (causing the data to be unmapped).
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
*     12-JUL-1989 (RFWS):
*        Original version.
*     16-AUG-1989 (RFWS):
*        Changed initialisation of locators to use global constant.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants

*  Arguments Given:
      LOGICAL COPY

*  Arguments Given and Returned:
      CHARACTER * ( * ) MLOC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER TSTAT              ! Temporary status value

*.

*  Save the STATUS value and mark the error stack.
      TSTAT = STATUS
      CALL ERR_MARK

*  If access is via a "copy" of the data, then annul the temporary
*  object containing the data (this erases the object).
      STATUS = SAI__OK
      IF ( COPY ) THEN
         CALL ARY1_ANTMP( MLOC, STATUS )

*  Otherwise, if access is direct via HDS, then annul the locator
*  (causing the data to be unmapped).
      ELSE
         CALL DAT_ANNUL( MLOC, STATUS )
         MLOC = ARY__NOLOC
      END IF

*  Annul any error if STATUS was previously bad, otherwise let the new
*  error report stand.
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( TSTAT .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            STATUS = TSTAT

*  Call error tracing routine if appropriate.
         ELSE
            CALL ARY1_TRACE( 'ARY1_UPSR', STATUS )
         END IF
      ELSE
         STATUS = TSTAT
      END IF

*  Release the error stack.
      CALL ERR_RLSE

      END
