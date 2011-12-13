      SUBROUTINE ADAM_RECEIVE ( PATH, CONTEXT, NAME, VALUE,
     :                          MESSID, STATUS )
*+
*  Name:
*     ADAM_RECEIVE

*  Purpose:
*     wait for incoming message.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ADAM_RECEIVE ( PATH, CONTEXT, NAME, VALUE, MESSID, STATUS )

*  Description:
*     Waits for a message to arrive at this task, and returns with the message
*     and associated parameters.
*     If any messages are already waiting on the message list they will be
*     picked up and returned in chronological order by calls to this
*     routine.

*  Arguments:
*     PATH=INTEGER (returned)
*        pointer to the path back to the originating task
*     CONTEXT=INTEGER (returned)
*        parameterised 'get, set, obey, cancel'
*     NAME=CHARACTER*(*) (given)
*        name of required function or parameter
*     VALUE=CHARACTER*(*) (returned)
*        received message
*     MESSID=INTEGER (given)
*        message number of received message

*  Algorithm:
*     Receive the message data structure using MESSYS_RECEIVE with
*     infinite timeout, and split it into its components.

*  Copyright:
*     Copyright (C) 1984, 1986, 1992, 1993 Science & Engineering Research Council.
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
*     John Cooke (REVAD::JAC) <date>
*     {enter_new_authors_here}

*  History:
*     3-MAY-1984  first insertion (REVAD::JAC)
*     22-MAY-1984  return "msg_status" as status (REVA::ADAM])
*     10-OCT-1984  change "normal" to "adam__OK" (REVA::ADAM)
*     09-JUN-1986  return message even when status bad (REVAD::BDK)
*     12-NOV-1992:  use SAI__OK not ADAM__OK (RLVAD::AJC)
*        8-MAR-1993: use MESSYS_PAR and MESSYS__INFINITE
*                    not MESDEFNS and INFINITE (RLVAD::AJC)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'MESSYS_PAR'

*  Arguments Returned:
      INTEGER PATH        !  pointer to the path back to the originating task
      INTEGER CONTEXT     !  parameterised 'get, set, obey, cancel'
      CHARACTER NAME*(*)  !  name of required function or parameter
      CHARACTER VALUE*(*) !  received value (usually in packed form)
      INTEGER MESSID      !  message number of received message

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER TIMEOUT
      PARAMETER ( TIMEOUT = MESSYS__INFINITE )
*    Data structures for ADAM:
      INCLUDE 'MESSYS_DD'

*.


      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF

      CALL MESSYS_RECEIVE ( TIMEOUT, PATH, MSG, MESSID, STATUS )

      CONTEXT = MSG_CONTEXT
      NAME = MSG_NAME

      IF ( MSG_LENGTH .GT. 0 ) THEN
         VALUE = MSG_VAL(1:MSG_LENGTH)
      ELSE
         VALUE = ' '
      ENDIF

      IF ( STATUS .EQ. SAI__OK ) THEN
         STATUS = MSG_STATUS
      ENDIF

      END

