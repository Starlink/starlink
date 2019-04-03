      SUBROUTINE NDF1_CPYNC( LOC1, NAME, LOC2, STATUS )
*+
*  Name:
*     NDF1_CPYNC

*  Purpose:
*     Copy a named HDS component from one structure to another.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_CPYNC( LOC1, NAME, LOC2, STATUS )

*  Description:
*     The routine copies a named HDS component (if it exists) from one
*     structure to another, preserving its name in the process. If the
*     component to be copied does not exist, then the routine completes
*     without action, but no error results. An error will be reported,
*     however, if a component exists to be copied but a component of
*     the same name already exists in the output structure.

*  Arguments:
*     LOC1 = CHARACTER * ( * ) (Given)
*        Locator to input HDS structure.
*     NAME = CHARACTER * ( * ) (Given)
*        HDS name of the component to be copied.
*     LOC2 = CHARACTER * ( * ) (Given)
*        Locator to the HDS structure which is to receive the copied
*        component.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Initialise.
*     -  See if the component to be copied exists.
*     -  If it does, then locate it and copy it.
*     -  Annul the locator when done.

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
*     12-OCT-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants

*  Arguments Given:
      CHARACTER * ( * ) LOC1
      CHARACTER * ( * ) NAME
      CHARACTER * ( * ) LOC2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL THERE              ! Whether the component exists
      CHARACTER * ( DAT__SZLOC ) TLOC ! Temporary locator

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      THERE = .TRUE.

*  See if the component to be copied exists.
      CALL DAT_THERE( LOC1, NAME, THERE, STATUS )
      IF ( ( STATUS .EQ. SAI__OK ) .AND. THERE ) THEN

*  If so, then locate it and copy it.
         CALL DAT_FIND( LOC1, NAME, TLOC, STATUS )
         CALL DAT_COPY( TLOC, LOC2, NAME, STATUS )

*  Annul the locator when done.
         CALL DAT_ANNUL( TLOC, STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_CPYNC', STATUS )

      END
