      SUBROUTINE TRN_CLOSE( STATUS )







*+
*  Name:
*     TRN_CLOSE

*  Purpose:
*     close the TRANSFORM facility.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN_CLOSE( STATUS )

*  Description:
*     The routine closes the TRANSFORM facility down, annulling all
*     compiled transformations and releasing all resources.  This
*     routine will still execute even if STATUS is set on entry,
*     although no error report will be made if it subsequently fails
*     under these circumstances.  If the facility is already closed,
*     the routine returns without action.

*  Arguments:
*     STATUS = INTEGER (given & returned)
*        Inherited error status.

*  Algorithm:
*     - Call the routine TRN1_SETUP, specifying a that the TRANSFORM
*       facility is to be deactivated.

*  Copyright:
*     Copyright (C) 1988 Science & Engineering Research Council.
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
*     R.F. Warren-Smith (DUVAD::RFWS)
*     {enter_new_authors_here}

*  History:
*     17-AUG-1988:  Original version (DUVAD::RFWS)
*     {enter_further_changes_here}

*  Bugs:
*     None known.
*     {note_new_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants


*  Status:
      INTEGER STATUS            ! Error status


*  Local Variables:
      INTEGER LSTAT             ! Local status variable


*.



*   Initialise the local status variable.
      LSTAT = STATUS


*   Perform a closedown, rendering the TRANSFORM facility inactive.
      CALL TRN1_SETUP( .FALSE., LSTAT )


*   If STATUS was not set on entry, return the local status value.
      IF( STATUS .EQ. SAI__OK ) STATUS = LSTAT


*   Exit routine.
      END
