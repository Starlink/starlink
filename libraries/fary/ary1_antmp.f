      SUBROUTINE ARY1_ANTMP( LOC, STATUS )
*+
*  Name:
*     ARY1_ANTMP

*  Purpose:
*     Annul a locator to a temporary object, thereby erasing the object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_ANTMP( LOC, STATUS )

*  Description:
*     The routine annuls a locator to a temporary object created by
*     ARY1_TEMP, thereby causing the associated object to be erased and
*     the file space associated with it to be released. If data are
*     mapped to the object via HDS, then they are first unmapped.

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given and Returned)
*        HDS locator to temporary object to be annulled. The character
*        variable supplied is reset to ARY__NOLOC by this routine.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine attempts to execute even if STATUS is set on
*     entry. However, no additional error report is made if it
*     subsequently fails under these circumstances.

*  Algorithm:
*     -  Save the error context on entry.
*     -  Find the parent of the object to be annulled.
*     -  Locate the object within its parent structure.
*     -  Annul the object's locator.
*     -  Erase the object.
*     -  Annul the parent's locator.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-JUN-1989  (RFWS):
*        Original version.
*     16-AUG-1989 (RFWS):
*        Changed initialisation of locators to use global constant.
*     9-OCT-1989 (RFWS):
*        Changed argument description to refer to ARY__NOLOC constant.
*     16-OCT-1990 (RFWS):
*        Changed to call ARY1_PAREN as a fix for problems with
*        DAT_PAREN.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants

*  Arguments Given:
      CHARACTER * ( * ) LOC

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      CHARACTER * ( DAT__SZLOC ) LOCP ! Locator to parent object
      CHARACTER * ( DAT__SZNAM ) NAME ! Name of object to be erased
      INTEGER TSTAT              ! Local temporary status variable

*.

*  Save the STATUS value and mark the error stack.
      TSTAT = STATUS
      CALL ERR_MARK

*  Find the temporary object's name.
      STATUS = SAI__OK
      NAME = ' '
      CALL DAT_NAME( LOC, NAME, STATUS )

*  Find its parent.
      LOCP = ARY__NOLOC
      CALL DAT_PAREN( LOC, LOCP, STATUS )

*  Annul the object's locator.
      CALL DAT_ANNUL( LOC, STATUS )
      LOC = ARY__NOLOC

*  Erase the object.
      CALL DAT_ERASE( LOCP, NAME, STATUS )

*  Annul the parent's locator.
      CALL DAT_ANNUL( LOCP, STATUS )
      LOCP = ARY__NOLOC

*  Annul any error if STATUS was previously bad, otherwise let the new
*  error report stand.
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( TSTAT .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            STATUS = TSTAT
         ELSE

*  Call error tracing routine if appropriate.
            CALL ARY1_TRACE( 'ARY1_ANTMP', STATUS )
         END IF
      ELSE
         STATUS = TSTAT
      END IF

*  Release the error stack.
      CALL ERR_RLSE

      END
