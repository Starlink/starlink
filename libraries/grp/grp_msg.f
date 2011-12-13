      SUBROUTINE GRP_MSG( TOKEN, IGRP, INDEX )
*+
*  Name:
*     GRP_MSG

*  Purpose:
*     Assign an element of a group to a message token.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_MSG( TOKEN, IGRP, INDEX )

*  Description:
*     The routine assigns a specified element of a GRP group to a message
*     token for use in constructing messages with the ERR_ and MSG_
*     routines (see SUN/104).

*  Arguments:
*     TOKEN = CHARACTER * ( * ) (Given)
*        Name of the message token.
*     IGRP = INTEGER (Given)
*        A GRP identifier for the group.
*     INDEX = INTEGER (Given)
*        The index of the element to assign to the message token.

*  Notes:
*     - This routine has no inherited status argument. It will always
*     attempt to execute, and no error will be reported if it should
*     fail (although the message token will be assigned a blank string).

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
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
*     2-FEB-2011 (DSB):
*        Original version
*     2011-02-17 (TIMJ):
*        Remove lots of code cloned from GRP_GET and just use GRP_GET
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER TOKEN*(*)
      INTEGER IGRP
      INTEGER INDEX

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP public constants.

*  Local Variables:
      CHARACTER NAME*(GRP__SZNAM)
      INTEGER STATUS
*.

*  Initialise
      NAME = ' '

*  Mark the message stack, and initialise the local error status to OK.
      CALL ERR_MARK
      STATUS = SAI__OK

      CALL GRP_GET( IGRP, INDEX, 1, NAME, STATUS )

*  If an error has occurred, annull it and store a blank value for the
*  name.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         NAME = ' '
      END IF

* Release the message stack.
      CALL ERR_RLSE

*  Assign the string to the message token. Do it as the last act so that
*  there is no chance of the token being annulled.
      CALL MSG_SETC( TOKEN, NAME )

      END
