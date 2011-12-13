      SUBROUTINE IRA_IDEPOCH( IDA, EPOCH, STATUS )
*+
*  Name:
*     IRA_IDEPOCH

*  Purpose:
*     Gets the epoch associated with an IRA identifier.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_IDEPOCH( IDA, EPOCH, STATUS )

*  Description:
*     This routine returns the epoch associated with an IRA identifier.

*  Arguments:
*     IDA = INTEGER (Given)
*        The IRA identifier for the astrometry information.
*     EPOCH = DOUBLE PRECISION (Returned)
*        Returned holding the epoch associated with the IRA identifier.
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
*        ACM_EPOCH( IRA__MAX ) = DOUBLE PRECISION (Read)
*           Julian epoch of observation from the associated AS.

*  Arguments Given:
      INTEGER          IDA

*  Arguments Returned:
      DOUBLE PRECISION EPOCH

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the IRA identifier is OK.
      CALL IRA1_CHECK( IDA, STATUS )
      IF( STATUS .EQ. SAI__OK ) THEN

*  Store the required epoch value.
         EPOCH = ACM_EPOCH( IDA )

*  If an error occurred, give a context message.
      ELSE
         CALL ERR_REP( 'IRA_IDEPOCH_ERR1', 'IRA_IDEPOCH: Unable to '//
     :                 'get the epoch for an IRA identifier', STATUS )
      END IF

      END
