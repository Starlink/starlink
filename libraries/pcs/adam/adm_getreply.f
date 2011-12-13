      SUBROUTINE ADAM_GETREPLY ( PATH, MESSID, CONTEXT, NAME, VALUE,
     :                           STATUS )
*+
*  Name:
*     ADAM_GETREPLY

*  Purpose:
*     wait for incoming message from specified path & messid.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ADAM_GETREPLY ( PATH, MESSID, CONTEXT, NAME, VALUE, STATUS )

*  Description:
*     Waits for a message from a particular path and with a particular
*     ID to arrive at this task, and returns with the message and
*     associated parameters.

*  Arguments:
*     PATH=INTEGER (given)
*        required path of the received message
*     MESSID=INTEGER (given)
*        required message number of received message
*     CONTEXT=INTEGER (returned)
*        parameterised 'get, set, obey, cancel'
*     NAME=CHARACTER*(*) (returned)
*        name of function or parameter
*     VALUE=CHARACTER*(*) (returned)
*        received value
*     STATUS=INTEGER

*  Algorithm:
*     Call MESSYS_GETREPLY with infinite timeout and interpret the data
*     structure returned.

*  Copyright:
*     Copyright (C) 1985, 1986, 1992 Science & Engineering Research Council.
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
*     Charlie Richardson (REVA::DCR) 25-Feb-85
*     {enter_new_authors_here}

*  History:
*     26-FEB-1985  First insertion (REVA::ADAM)
*     06.06.1986:  return values even if bad status from MESSYS
*        (REVAD::BDK)
*     12.11.1992:  use SAI__OK not ADAM__OK (RLVAD::AJC)
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
      INTEGER PATH        !  required path of the received message
      INTEGER MESSID      !  required message number of received message

*  Arguments Returned:
      INTEGER CONTEXT     !  parameterised 'get, set, obey, cancel'
      CHARACTER NAME*(*)  !  name of function or parameter
      CHARACTER VALUE*(*) !  received value

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

      CALL MESSYS_GETREPLY ( TIMEOUT, PATH, MESSID, MSG, STATUS )

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
