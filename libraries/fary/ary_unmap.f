      SUBROUTINE ARY_UNMAP( IARY, STATUS )
*+
*  Name:
*     ARY_UNMAP

*  Purpose:
*     Unmap an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_UNMAP( IARY, STATUS )

*  Description:
*     The routine unmaps an array which has previously been mapped for
*     READ, UPDATE or WRITE access.

*  Arguments:
*     IARY = INTEGER (Given)
*        Array identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine attempts to execute even if STATUS is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.
*     -  An error will result if the array has not previously been
*     mapped for access.

*  Algorithm:
*     -  Save the error context on entry.
*     -  Import the array identifier and unmap the array.
*     -  Restore the error context, reporting additional context
*     information if appropriate.

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
*     28-JUL-1989 (RFWS):
*        Original version.
*     9-OCT-1989 (RFWS):
*        Added STATUS check after call to ARY1_IMPID.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants

*  Arguments Given:
      INTEGER IARY

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER TSTAT              ! Temporary status variable
      INTEGER IACB               ! Index to array entry in the ACB

*.

*  Save the STATUS value and mark the error stack.
      TSTAT = STATUS
      CALL ERR_MARK

*  Import the array identifier and unmap the array.
      STATUS = SAI__OK
      CALL ARY1_IMPID( IARY, IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL ARY1_UMP( IACB, STATUS )
      END IF

*  Annul any error if STATUS was previously bad, otherwise let the new
*  error report stand.
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( TSTAT .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            STATUS = TSTAT

*  Report context information and call error tracing routine if
*  appropriate.
         ELSE
            CALL ERR_REP( 'ARY_UNMAP_ERR',
     :      'ARY_UNMAP: Error unmapping an array.', STATUS )
            CALL ARY1_TRACE( 'ARY_UNMAP', STATUS )
         END IF
      ELSE
         STATUS = TSTAT
      END IF

*  Release error stack.
      CALL ERR_RLSE

      END
