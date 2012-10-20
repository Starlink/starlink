      SUBROUTINE GRP_ALARM( IGRP, EVENT, STATUS )
*+
*  Name:
*     GRP_ALARM

*  Purpose:
*     Notify the user of an event in the life of a watched group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_ALARM( IGRP, EVENT, STATUS )

*  Description:
*     This routine is called internally by the GRP library when a
*     significant event happens in the life of a group that is being
*     watched as a result of a call to GRP_WATCH. It is not intended
*     to be called by the user. It's main purpose is to provide a break
*     point target for debugging the creation and destruction of GRP
*     groups.

*  Arguments:
*     IGRP = INTEGER (Given)
*        An identifier for the created group. GRP__NOID is returned if
*        an error occurs.
*     EVENT = CHARACTER * ( * ) (Given)
*        This will be "CREATE" or "DELETE".
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - This routine attempts to run even if the global status value
*     indicates an error has already occurred.

*  Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
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
*     DSB: David Berry (JAC, Hawaii)
*     {enter_authors_here}

*  History:
*     19-OCT-2012 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER IGRP
      CHARACTER EVENT*(*)

*  Status:
      INTEGER STATUS             ! Global status

*.

      CALL ERR_BEGIN( STATUS )
      CALL MSG_SETC( 'E', EVENT )
      CALL MSG_SETI( 'I', IGRP )
      CALL MSG_OUT( ' ','Group: ^I   Event: ^E', STATUS )
      CALL ERR_END( STATUS )

      END
