      SUBROUTINE NDF_RESET( INDF, COMP, STATUS )
*+
*  Name:
*     NDF_RESET

*  Purpose:
*     Reset an NDF component to an undefined state.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_RESET( INDF, COMP, STATUS )

*  Description:
*     The routine resets a component of an NDF so that its value
*     becomes undefined. It may be used to remove unwanted optional NDF
*     components. Its use is also advisable before making format
*     changes to an NDF if retention of the existing values is not
*     required (e.g. before changing the data type of an array
*     component with the NDF_STYPE routine); this will avoid the cost
*     of converting the existing values.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the NDF component to be reset; any NDF component name
*        is valid. No error will result if the component is already
*        undefined.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A comma-separated list of component names may also be supplied
*     in which case each component will be reset in turn.
*     -  Specifying a component name of '*' will cause all components,
*     except for HISTORY and extensions, to be reset. The former may be
*     reset by specifying its name explicitly, while all extensions may
*     be removed by specifying a component name of 'EXTENSION'.
*     -  Individual extensions may be removed from an NDF with the
*     NDF_XDEL routine.
*     -  This routine may only be used to reset components of a base
*     NDF. If an NDF section is supplied, then it will return without
*     action. No error will result.
*     -  An array component of an NDF cannot be reset while it is
*     mapped for access. Neither can an NDF's axis component be reset
*     while any axis array is mapped for access. This routine will fail
*     if either of these conditions occurs.

*  Algorithm:
*     -  Import the NDF identifier.
*     -  Check that write access to the NDF is available.
*     -  Reset the NDF component(s).
*     -  If an error has occurred, then report context information.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     19-MAR-1990 (RFWS):
*        Derived from earlier version which was renamed as NDF1_RST, now
*        called by this routine.
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
      INTEGER INDF
      CHARACTER * ( * ) COMP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to NDF entry in the ACB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Check that WRITE access to the NDF is available.
      CALL NDF1_CHACC( IACB, 'WRITE', STATUS )

*  Reset the NDF component(s).
      CALL NDF1_RST( IACB, COMP, STATUS )

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_RESET_ERR',
     :   'NDF_RESET: Error resetting an NDF component to an ' //
     :   'undefined state.', STATUS )
         CALL NDF1_TRACE( 'NDF_RESET', STATUS )
      END IF

      END
