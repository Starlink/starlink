      SUBROUTINE DTASK_SET ( PATH, NAME, VALUE, MESSID, STATUS )
*+
*  Name:
*     DTASK_SET

*  Purpose:
*     Routine handling task set

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL DTASK_SET ( PATH, NAME, VALUE, MESSID, STATUS )

*  Description:
*     Sets the named task parameter to the given value.
*     Sends acknowledgment to initiating task.

*  Arguments:
*     PATH=INTEGER (given)
*           path to initiating task
*     NAME=CHARACTER*(*) (given)
*           name of parameter to be set. In a monolith this must be
*           ACTION_KEYWORD:PARAMETER_NAME
*     VALUE=CHARACTER*(*) (given)
*           value to be set
*     MESSID=INTEGER (given)
*           message identifier
*     STATUS=INTEGER

*  Algorithm:
*     Calls the ADAM parameter system primitives, so that the user is
*     not prompted for an HDS structure to put the parameter value in.

*  Copyright:
*     Copyright (C) 1984, 1987, 1991, 1993 Science & Engineering
*     Research Council. Copyright (C) 1995 Central Laboratory of the
*     Research Councils. All Rights Reserved.

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
*     John Cooke (REVS::JAC) 01May84
*     {enter_new_authors_here}

*  History:
*     08-MAY-1984 (REVAD::JAC):
*        First insertion
*     11-MAY-1984 (REVAD::JAC):
*        Test of autodoc
*     19-JUN-1984 (REVA::ADAM):
*        Changed from "put"; added acknowledgment
*     19-JUN-1984 (REVA::ADAM):
*        Repaired msg_value to msg_val !
*     02-NOV-1984 (REVAD::BDK):
*        Use full parameter system
*     16-NOV-1984 (REVA::ADAM):
*        New version with parameter system
*     11-JUN-1987 (REVAD::BDK):
*        Handle monoliths
*     15-JAN-1991 (RLVAD::AJC):
*        Check if task is monolith
*     25-APR-1991 (REVAD::BDK):
*        Revise INCLUDE files
*     30-APR-1991 (REVAD::BDK):
*        Revise INCLUDE files
*     09-MAY-1991 (REVAD::BDK):
*        Clear-out ERR and MSG systems
*     13-MAY-1991 (REVAD::BDK):
*        Use COMSHUT
*     04-JUN-1991 (ROE::BMC):
*        Remove reference to DDMSG
*     04-JUN-1991: Correct call to SUBPAR_FINDPAR to use NAMECODE when
*                  generating an error (ROE::BMC)
*     07-JUN-1991 (REVAD::BDK):
*        Insist COLPOS>1 for a monolith
*     08-MAR-1993 (RLVAD::AJC):
*        Remove include MESSYS_PAR
*     24-MAY-1995 (RLVAD::AJC):
*        Report on no action for monolith
*     04-AUG-1995 (RLVAD::AJC):
*        Allow non-monoliths to have action:name form
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DTASK_ERR'
      INCLUDE 'ADAM_DEFNS'

*  Arguments Given:
      INTEGER PATH         !  path to initiating task
      CHARACTER NAME*(*)   !  name of parameter to be set
      CHARACTER VALUE*(*)  !  value it is to be set to
      INTEGER MESSID       !  message identifier

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER NAMECODE     ! code number of parameter
      INTEGER COLPOS       ! position of ':' in NAME
      INTEGER ACODE        ! action pointer (returned by FINDACT
                           ! but not used)
      LOGICAL MONO         ! if the task is a monolith
      INTEGER MESSTATUS    ! status returned to initiating task
*.

      IF ( STATUS .NE. SAI__OK ) RETURN
*
*   If this is a monolith, the name must include the keyword of the
*   relevant action within the monolith in the form KEY:PARNAME.
*   If not a monolith this form is optional, any KEY part will be ignored.
*
*   Find any task/name separator ':'.
*   COLPOS will be 0 if there isn't one.
*
      COLPOS = INDEX ( NAME, ':' )

      CALL SUBPAR_MLITH ( MONO, STATUS )
      IF ( MONO ) THEN
*
*      It is a monolith
*
         IF ( COLPOS .GT. 1 ) THEN
*
*         Set-up the parameter system for the action.
*
            CALL SUBPAR_FINDACT ( NAME(1:COLPOS-1), ACODE, STATUS )

         ELSE
*
*         No action component in parameter specification
*
            STATUS = DTASK__ACTPAR
            NAMECODE = 0
            CALL ERR_REP ( ' ', 'DTASK: ' //
     :      'SET parameter not of form "task:parameter" for a monolith',
     :       STATUS )

         ENDIF

      ENDIF
*
*   Look-up the named parameter
*
      CALL SUBPAR_FINDPAR ( NAME(COLPOS+1:), NAMECODE, STATUS )
*
*   Put the value into it
*
      CALL SUBPAR_CMDPAR ( NAMECODE, VALUE, STATUS )
*
*   Acknowledge
*
      MESSTATUS = STATUS
      STATUS = SAI__OK
      CALL DTASK_COMSHUT ( PATH, MESSID, MESSTATUS, SET, NAME, VALUE,
     :  STATUS )

      END
