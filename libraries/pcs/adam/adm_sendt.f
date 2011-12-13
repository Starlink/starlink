      SUBROUTINE ADAM_SENDT ( TIME, PATH, CONTEXT, NAME, INVAL, OUTVAL,
     :                       MESSID, STATUS )
*+
*  Name:
*     ADAM_SENDT

*  Purpose:
*     send message to another task with timeout on acknowledgment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ADAM_SENDT ( TIME, PATH, CONTEXT, NAME, INVAL, OUTVAL,
*     :  MESSID, STATUS )

*  Description:
*     Send a message to the task indicated by 'PATH' with given context
*     (get, set, obey, cancel). Return the value from the received
*     acknowledgment message and the message identifier in case any more
*     replies are expected from the communicating task.
*     This routine will only return with messages replying to the sent
*     message, ie other messages from other paths or with other ID's will
*     not cause this routine to return. It can be forced to return by an
*     AST signal or by the timeout expiring.
*     NOTE:
*     If a timeout occurson the reply and the reply subsequently arrives
*     it will be put on the message list and will remain there until the
*     list fills up (an error condition) or until an ADAM_RECEIVE (or a
*     GETREPLY looking for that PATH and MESSID) is done.

*  Arguments:
*     TIME=INTEGER (given)
*        timeout time in milliseconds
*     PATH=INTEGER (given)
*        pointer to the path to the required task
*     CONTEXT=INTEGER (given)
*        parameterised 'get, set, obey, cancel'
*     NAME=CHARACTER*(*) (given)
*        name of required function or parameter
*     INVAL=CHARACTER*(*) (given)
*        value string to be sent
*     OUTVAL=CHARACTER*(*) (returned)
*        contains return value
*     MESSID=INTEGER (returned)
*        message number issued by this task

*  Algorithm:
*     Construct the message data structure and use MESSYS_SEND to send
*     the message. This returns the MESSID. Then use MESSYS_GETREPLY with
*     given timeout to wait for a message from PATH with the same
*     MESSID.

*  Copyright:
*     Copyright (C) 1984, 1985, 1986, 1992, 1993 Science & Engineering Research Council.
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
*     Charlie Richardson (REVA::DCR) 5-Mar-85
*     {enter_new_authors_here}

*  History:
*     3-MAY-1984  first insertion (REVAD::JAC)
*     18-MAY-1984  "msg_status" added (REVA::ADAM])
*     24-MAY-1984  add status/context/name for receive (REVA::ADAM])
*     13-JUN-1984  context and name should not be altered on exit (REVA::ADAM])
*     10-OCT-1984  change "normal" to "adam__ok" (REVA::ADAM)
*     5-MAR-1985  put in use of GETREPLY (REVA::ADAM)
*     09-JUN-1986  return OUTVAL even on bad status (REVAD::BDK)
*     12-NOV-1992:  use SAI__OK not ADAM__OK (RLVAD::AJC)
*        8-MAR-1993: use MESSYS_PAR and MESSYS__MESSAGE
*                    not MESDEFNS and MESSAGE  (RLVAD::AJC)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'MESSYS_PAR'

*  Arguments Given:
      INTEGER TIME        !  timeout time in milliseconds
      INTEGER PATH        !  pointer to the path to the required task
      INTEGER CONTEXT     !  parameterised 'get, set, obey, cancel'
      CHARACTER NAME*(*)  !  name of required function or parameter
      CHARACTER INVAL*(*) !  value string to be sent

*  Arguments Returned:
      CHARACTER OUTVAL*(*) !  returned value
      INTEGER MESSID      !  message number issued by this task

*  Status:
      INTEGER STATUS
*    Data structures for ADAM:
      INCLUDE 'MESSYS_DD'

*.


      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF

      MSG_STATUS = SAI__OK
      MSG_FUNCTION = MESSYS__MESSAGE
      MSG_CONTEXT = CONTEXT
      MSG_NAME = NAME
      MSG_LENGTH = MIN ( LEN(INVAL), MSG_VAL_LEN )
      MSG_VAL = INVAL        !  truncated on right if too long

      CALL MESSYS_SEND ( PATH, MSG, MESSID, STATUS )
      CALL MESSYS_GETREPLY ( TIME, PATH, MESSID, MSG, STATUS )

      IF ( MSG_LENGTH .GT. 0 ) THEN
         OUTVAL = MSG_VAL(1:MSG_LENGTH)
      ELSE
         OUTVAL = ' '
      ENDIF

      IF ( STATUS .EQ. SAI__OK ) THEN
         STATUS = MSG_STATUS
      ENDIF

      END

