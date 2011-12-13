      SUBROUTINE ATL_NOTIF( MSG, STATUS )
*+
*  Name:
*     ATL_NOTIF

*  Purpose:
*     Print a message to the screen if ATOOLS_VERBOSE is set.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL_NOTIF( MSG, STATUS )

*  Description:
*     Print a message to the screen if ATOOLS_VERBOSE is set.

*  Arguments:
*     MSG = CHARACTER * ( * ) (Given)
*        The message.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2003 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
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
*     6-JUN-2003 (DSB):
*        Original version.
*     30-MAY-2006 (DSB):
*        Moved into ATL library and changed prefix from "ATL1_" to "ATL_".
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER MSG*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER VERB
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Only display the message if environment variable ATOOLS_VERBOSE is defined.
      CALL PSX_GETENV( 'ATOOLS_VERBOSE', VERB, STATUS )
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
      ELSE
         CALL MSG_OUT( ' ', MSG, STATUS )
      END IF

      END
