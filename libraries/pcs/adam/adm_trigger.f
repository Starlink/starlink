      SUBROUTINE ADAM_TRIGGER ( PATH, CONTEXT, NAME, VALUE,
     :                          MESSID, STATUS )
*+
*  Name:
*     ADAM_TRIGGER

*  Purpose:
*     send triggering message without expecting acknowledgment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ADAM_TRIGGER ( PATH, CONTEXT, NAME, VALUE, MESSID, STATUS )

*  Description:
*     Sends a message to the task indicated in the path, but does not expect
*     an acknowledgment and so does not wait before returning.  The values
*     of PATH, CONTEXT, NAME and MESSID will usually have been copied
*     from the corresponding values obtained by a call to RECEIVE or
*     GETREPLY. This routine is the same as ADAM_REPLY except that it uses
*     message status MESSYS__TRIGGER.

*  Arguments:
*     PATH=INTEGER (given)
*        pointer to the path back to the originating task
*     CONTEXT=INTEGER (given)
*        parameterised 'get, set, obey, cancel'
*     NAME=CHARACTER*(*) (given)
*        name of required function or parameter
*     VALUE=CHARACTER*(*) (given)
*        value to be sent
*     MESSID=INTEGER (given)
*        message number of original received message

*  Algorithm:
*     Construct a message data structure and call MESSYS_REPLY.

*  Copyright:
*     Copyright (C) 1989, 1990, 1992, 1993 Science & Engineering Research Council.
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
*     W.F.Lupton (AAOEPP::WFL)
*     {enter_new_authors_here}

*  History:
*     01.05.1989: original (AAOEPP::WFL)
*     01.03.1990: correct comments (AAOEPP::WFL)
*     12.11.1992: use SAI__OK not ADAM__OK
*                   renamed MESERRS to MESSYS_ERR (RLVAD::AJC)
*        8.03.1993: use MESSYS_PAR and MESSYS__MESSAGE
*                   not MESDEFNS and MESSAGE (RLVAD::AJC)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'MESSYS_PAR'
      INCLUDE 'MESSYS_ERR'

*  Arguments Given:
      INTEGER PATH        !  pointer to the path back to the originating task
      INTEGER CONTEXT     !  parameterised 'get, set, obey, cancel'
      CHARACTER NAME*(*)  !  name of required function or parameter
      CHARACTER VALUE*(*) !  value to be sent
      INTEGER MESSID      !  message number of original received message

*  Status:
      INTEGER STATUS
*    Data structures for ADAM:
      INCLUDE 'MESSYS_DD'

*.


      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF

      MSG_STATUS = MESSYS__TRIGGER
      MSG_FUNCTION = MESSYS__MESSAGE
      MSG_CONTEXT = CONTEXT
      MSG_NAME = NAME
      MSG_LENGTH = MIN ( LEN(VALUE), MSG_VAL_LEN )
      MSG_VAL = VALUE      !  truncated on right if too long

      CALL MESSYS_REPLY ( PATH, MSG, MESSID, STATUS )

      END
