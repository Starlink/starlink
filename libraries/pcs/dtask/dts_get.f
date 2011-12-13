      SUBROUTINE DTASK_GET ( PATH, NAME, MESSID, STATUS )
*+
*  Name:
*     DTASK_GET

*  Purpose:
*     Routine handling d-task get

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL DTASK_GET ( PATH, NAME, MESSID, STATUS )

*  Description:
*     Obtains the value of the named d-task parameter.  If the parameter
*     does not exist, an appropriate status is returned.
*     Sends acknowledgment containing the parameter value to the
*     requesting task.

*  Arguments:
*     PATH=INTEGER (given)
*           path to requesting task
*     NAME=CHARACTER*(*) (given)
*           name of parameter to be got
*     MESSID=INTEGER (given)
*           message id
*     STATUS=INTEGER

*  Algorithm:
*     Stop the parameter system prompting by disabling its
*     communications. Ask the parameter system for the value as a
*     character string scalar, then re-enable the communications.
*     Finally, close the transaction.

*  Implementation Deficiencies:
*     Disabling and enabling communications is a frig to allow PAR_GET0C
*     to be used. The parameter system does not provide a call to carry
*     out the GET context properly.

*  Copyright:
*     Copyright (C) 1984, 1990-1991, 1993-1994 Science & Engineering
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
*     Dennis Kelly (REVAD::BDK)
*     Alan Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     08-MAY-1984 (REVAD::JAC):
*        First insertion
*     19-JUN-1984 (REVA::ADAM):
*        Added acknowledgment
*     16-NOV-1984 (REVA::ADAM):
*        Update documentation - new parameter system
*     17-JAN-1990 (RLVAD::AJC):
*        Increase VALUE 80 -> 132 characters
*     15-JAN-1991: handle monoliths - checking if task is
*                  monolith
*                  Also clear value initially (RLVAD::AJC)
*     25-APR-1991 (REVAD::BDK):
*        Revise INCLUDE files
*     30-APR-1991 (REVAD;;BDK):
*        Use MESSYS__MESSAGE
*     09-MAY-1991 (REVAD::BDK):
*        Flush ERR and MSG systems
*     13-MAY-1991 (REVAD::BDK):
*        Use COMSHUT
*     04-JUN-1991 (ROE::BMC):
*        Disable prompting on the PAR_GET0C
*     07-JUN-1991 (REVAD::BDK):
*        Change comments and status handling
*     08-MAR-1993 (RLVAD::AJC):
*        Use MESSYS__VAL_LEN - remove include DDMSG
*     27-JUL-1993 (RLVAD::AJC):
*        Use SUBPAR not PAR_GET0C
*     11-FEB-1994 (RLVAD::AJC):
*        Return only used length of VAL
*     17-AUG-1994 (RLVAD::AJC):
*        Call new SUBPAR_GET to get values of all types
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
      INCLUDE 'MESSYS_PAR'
*  Arguments Given:
      INTEGER PATH                  ! path to requesting task
      CHARACTER*(*) NAME            ! name of parameter to be got
      INTEGER MESSID                ! message id
*  Status:
      INTEGER STATUS
*    External routines:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER COLPOS                ! position of ':' in NAME
      INTEGER ACODE                 ! action pointer (returned by FINDACT
                                    ! but not used)
      LOGICAL MONO                  ! if the task is a monolith
      CHARACTER*(MESSYS__VAL_LEN) VALUE ! value obtained
      INTEGER VALLEN                ! used length of value
      INTEGER MESSTATUS             ! status returned to requesting task
      INTEGER NAMECODE              ! code number of parameter
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
         IF ( COLPOS .NE. 0 ) THEN
*
*         Set-up the parameter system for the task name
*
            CALL SUBPAR_FINDACT ( NAME(1:COLPOS-1), ACODE, STATUS )

         ELSE
*
*         No action component in parameter specification
*
            STATUS = DTASK__ACTPAR
            NAMECODE = 0
            CALL ERR_REP ( ' ', 'DTASK: ' //
     :      'GET parameter not of form "task:parameter" for a monolith',
     :       STATUS )

         ENDIF

      ENDIF
*
*   Get parameter value - initialise to blank to cover failure.
*

** frig
** to prevent prompting
      VALUE = ' '
      CALL SUBPAR_PUTPATH ( 0, 0, STATUS )
      CALL SUBPAR_FINDPAR ( NAME(COLPOS+1:), NAMECODE, STATUS )
      CALL SUBPAR_GET ( NAMECODE, VALUE, STATUS )
      MESSTATUS = STATUS
      STATUS = SAI__OK
      CALL SUBPAR_PUTPATH ( PATH, MESSID, STATUS )
** endfrig

*
*   Acknowledge.
*
      STATUS = SAI__OK
      VALLEN = MAX( 1, CHR_LEN( VALUE ) )
      CALL DTASK_COMSHUT ( PATH, MESSID, MESSTATUS, GET, NAME,
     : VALUE(1:VALLEN), STATUS )

      END
