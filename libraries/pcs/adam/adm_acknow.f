      SUBROUTINE ADAM_ACKNOW ( PATH, MESSID, MESSTATUS, CONTEXT,
     :  NAME, VALUE, STATUS )
*+
*  Name:
*     ADAM_ACKNOW

*  Purpose:
*     acknowledge a message from another task.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ADAM_ACKNOW ( PATH, MESSID, MESSTATUS, CONTEXT,
*    :                   NAME, VALUE, STATUS

*  Description:
*     puts the given information into a message and returns it along the
*     indicated PATH.

*  Arguments:
*     PATH=INTEGER (given)
*        message path for the REPLY
*     MESSID=INTEGER (given)
*        message id of original incoming message
*     MESSTATUS=INTEGER (given)
*        message status to be set in the reply
*     CONTEXT=INTEGER (given)
*        whether original message was get/set/obey/cancel
*     NAME=CHARACTER*(*) (given)
*        name of the original requested action
*     VALUE=CHARACTER*(*) (given)
*        message string to be sent in reply
*     STATUS=INTEGER

*  Algorithm:
*     The given information is packed into a message structure and sent
*     along the indicated PATH using MESSYS_REPLY.

*  Implementation Deficiencies:
*     This can, in theory, truncate the VALUE string. In practice, this
*     should not happen. Ignore such an error - it is bad for this
*     routine to return a bad status.

*  Copyright:
*     Copyright (C) 1984, 1991 Science & Engineering Research Council.
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
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     24-NOV-1984 (REVA::ADAM):
*        Original version (REVA::ADAM)
*     25-APR-1991 (REVAD::BDK):
*        revise INCLUDE files (REVAD::BDK)
*     30-APR-1991 (REVAD::BDK):
*        use MESSYS_PAR (REVAD::BDK)
*     25-NOV-1991 (REVAD::BDK):
*        renamed from DTASK_ACKNOW (REVAD::BDK)
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
      INTEGER PATH              ! message path for the REPLY

      INTEGER MESSID            ! message id of original incoming message

      INTEGER MESSTATUS         ! message status to be set in the reply

      INTEGER CONTEXT           ! whether original message was
                                ! get/set/obey/cancel

      CHARACTER*(*) NAME        ! name of the original requested action

      CHARACTER*(*) VALUE       ! message string to be sent in reply


*  Status:
      INTEGER STATUS

*    Data structures for ADAM
      INCLUDE 'MESSYS_DD'

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Load the information into the 'message structure'
*
      MSG_STATUS = MESSTATUS
      MSG_FUNCTION = MESSYS__MESSAGE
      MSG_CONTEXT = CONTEXT
      MSG_NAME = NAME
      MSG_LENGTH = MIN ( LEN(VALUE), MSG_VAL_LEN )
      MSG_VAL = VALUE
*
*   Send the reply
*
      CALL MESSYS_REPLY ( PATH, MSG, MESSID, STATUS )

      END
