      SUBROUTINE GRP_WATCH( IGRP, STATUS )
*+
*  Name:
*     GRP_WATCH

*  Purpose:
*     Watch for events in the life of a specified group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_WATCH( IGRP, STATUS )

*  Description:
*     This routine causes messages to be reported when a group with the
*     given integer identifier is created or destroyed. It will also
*     list the integer identifiers for all currently active groups.
*
*     Each event in the life of the group is repoirted by routine
*     GRP_ALARM, and so a debugger break point can be set there to
*     investigate such events.

*  Arguments:
*     IGRP= INTEGER (Given)
*        The identifier for the group to be watched. If this is GRP__NOID,
*        the identifiers of all currently active groups are listed on
*        standard output. NOTE, in the C interface this should be an
*        integer (or GRP__NOID), not a pointer to a Grp structure.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

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
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'GRP_CONST'        ! GRP private constants

*  Global Variables:
      INCLUDE 'GRP_COM'

*  Arguments Given:
      INTEGER IGRP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL FIRST
      INTEGER SLOT
*.

*  Start a new error reporting environment.
      CALL ERR_BEGIN( STATUS )

*  If a sequence number of zero was supplied, list all the sequence
*  numbers of all active groups.
      IF( IGRP .EQ. GRP__NOID ) THEN

         FIRST = .TRUE.
         DO SLOT = 1, GRP__MAXG
            IF( CMN_USED( SLOT ) ) THEN
               IF( FIRST ) THEN
                  CALL MSG_OUT( ' ','Currently active GRP identifiers:',
     :                          STATUS )
                  FIRST = .FALSE.
               END IF
               CALL MSG_SETI( 'I', CMN_CHK( SLOT ) )
               CALL MSG_OUT( ' ','   ^I', STATUS )
            END IF
         END DO

         IF( FIRST ) THEN
            CALL MSG_OUT( ' ','There are currently no active GRP '//
     :                    'identifiers.', STATUS )
         END IF

         CALL MSG_BLANK( STATUS )

*  Store the identifier so that its events can be monitored.
      END IF

      CMN_WATCH = IGRP

*  End the error reporting environment.
      CALL ERR_END( STATUS )

      END
