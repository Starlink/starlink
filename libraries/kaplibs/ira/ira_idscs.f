      SUBROUTINE IRA_IDSCS( IDA, SCS, STATUS )
*+
*  Name:
*     IRA_IDSCS

*  Purpose:
*     Gets the Sky Co-ordinate System associated with an IRA identifier.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_IDSCS( IDA, SCS, STATUS )

*  Description:
*     This routine returns the Sky Co-ordinate System associated with an
*     IRA identifier.

*  Arguments:
*     IDA = INTEGER (Given)
*        The IRA identifier for the astrometry information.
*     SCS = CHARACTER * ( * ) (Returned)
*        On exit, contains the full version of the sky co-ordinate system
*        associated with the IRA identifier. The supplied variable should
*        have a declared length given by symbolic constant IRA__SZSCS.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-OCT-2004 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_PAR'          ! IRA constants.

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common values.
*        ACM_SCS( IRA__MAX ) = CHARACTER (Read)
*           Full sky coordinate system (SCS) name from the associated
*           AS, with optional equinox specifier.

*  Arguments Given:
      INTEGER          IDA

*  Arguments Returned:
      CHARACTER SCS*(*)

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the IRA identifier is OK.
      CALL IRA1_CHECK( IDA, STATUS )
      IF( STATUS .EQ. SAI__OK ) THEN

*  Store the required SCS string.
         SCS = ACM_SCS( IDA )

*  If an error occurred, give a context message.
      ELSE
         CALL ERR_REP( 'IRA_IDSCS_ERR1', 'IRA_IDSCS: Unable to get '//
     :                 'the SCS for an IRA identifier', STATUS )
      END IF

      END
