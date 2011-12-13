      SUBROUTINE IRA1_ASSET( LOC, STATUS )
*+
*  Name:
*     IRA1_ASSET

*  Purpose:
*     Set an astrometry structure to the DEFINED state.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_ASSET( LOC, STATUS )

*  Description:
*     The STATE component of the astrometry structure (AS) is given the
*     value "DEFINED". This will cause other IRA routines to accept
*     the values stored for the other AS components.

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        Locator to the astrometry structure.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990, 1991 Science & Engineering Research Council.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-DEC-1990 (DSB):
*        Original version.
*     24-APR-1991 (DSB):
*        Name changed from IRA_$ASSET to IRA1_ASSET
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants

*  Arguments Given:
      CHARACTER LOC*(*)

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the STATE component of the supplied AS, and set its value to
*  DEFINED.
      CALL CMP_PUT0C( LOC, 'STATE', 'DEFINED', STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRA1_ASSET_ERR1',
     : 'IRA1_ASSET: Unable to set an astrometry structure into a '//
     : 'defined state.', STATUS )
      END IF

      END
