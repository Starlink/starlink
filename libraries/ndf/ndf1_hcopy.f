      SUBROUTINE NDF1_HCOPY( LOC1, LOC2, STATUS )
*+
*  Name:
*     NDF1_HCOPY

*  Purpose:
*     Copy all the components of one HDS structure into another structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_HCOPY( LOC1, LOC2, STATUS )

*  Description:
*     The routine copies all the components in an HDS structure into
*     another structure preserving their names. Any components in the
*     destination structure are first deleted.

*  Arguments:
*     LOC1 = CHARACTER * ( * ) (Given)
*        Locator to input HDS structure.
*     LOC2 = CHARACTER * ( * ) (Given)
*        Locator to the HDS structure which is to receive the copied
*        components.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     DSB: David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     23-JAN-2009 (DSB):
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

*  Arguments Given:
      CHARACTER * ( * ) LOC1
      CHARACTER * ( * ) LOC2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) TLOC ! Temporary locator
      CHARACTER * ( DAT__SZNAM ) NAME ! Component name
      INTEGER I                  ! Component index
      INTEGER NCOMP              ! Number of components
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Empty the destination structure.
      CALL DAT_NCOMP( LOC2, NCOMP, STATUS )
      DO I = 1, NCOMP
         CALL DAT_INDEX( LOC2, I, TLOC, STATUS )
         CALL DAT_NAME( TLOC, NAME, STATUS )
         CALL DAT_ANNUL( TLOC, STATUS )
         CALL DAT_ERASE( LOC2, NAME, STATUS )
      END DO

*  Loop over all source components.
      CALL DAT_NCOMP( LOC1, NCOMP, STATUS )
      DO I = 1, NCOMP

*  Get the component's name.
         CALL DAT_INDEX( LOC1, I, TLOC, STATUS )
         CALL DAT_NAME( TLOC, NAME, STATUS )

*  Copy it into the destination structure.
         CALL DAT_COPY( TLOC, LOC2, NAME, STATUS )
         CALL DAT_ANNUL( TLOC, STATUS )
      END DO

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_HCOPY', STATUS )

      END
