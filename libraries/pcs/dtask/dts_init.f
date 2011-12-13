      SUBROUTINE DTASK_INIT ( NAME, NLENGTH, STATUS )
*+
*  Name:
*     DTASK_INIT

*  Purpose:
*     Initialise the main libraries

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL DTASK_INIT ( NAME, NLENGTH; STATUS )

*  Description:
*     Initialises the main ADAM libraries.

*  Arguments:
*     NAME=CHARACTER*(*) (given)
*           name of this task
*     NLENGTH=INTEGER (given)
*           length of task name
*     STATUS=INTEGER

*  Algorithm:
*     Initialise into the ADAM message system (MESSYS).
*     Initialise the parameter system (SUBPAR).
*     Initialise the task support library (TASK).

*  Copyright:
*     Copyright (C) 1984, 1989-1994 Science & Engineering Research
*     Council. Copyright (C) 2001 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     John Cooke (REVS::JAC) 22May84
*     {enter_new_authors_here}

*  History:
*     22-MAY-1984 (REVA::ADAM]):
*        First insertion
*     22-MAY-1984 (REVA::ADAM]):
*        Initialise action list common block
*     19-JUN-1984 (REVA::ADAM):
*        Change 'NORMAL' to 'ADAM__OK'
*     21-JUN-1984 (REVA::ADAM):
*        Add interrupt flag
*     26-OCT-1984: add NLENGTH to argument list and initialise parameter
*                  system (REVAD::BDK)
*     16-NOV-1984 (REVA::ADAM):
*        New version with parameter system
*     30-APR-1989: add call to TASK_INIT_MESSINFO to initialise list of
*                  active actions in subsidiary tasks (AAOEPP::WFL)
*     30-APR-1989 (AAOEPP::WFL):
*        Add call to DTASK_SETDUMP
*     01-MAR-1990 (AAOEPP::WFL):
*        Remove call to DTASK_SETDUMP
*     25-APR-1991 (REVAD::BDK):
*        Revise INCLUDE files
*     30-APR-1991 (REVAD::BDK):
*        Revise INCLUDE files
*     09-MAY-1991 (REVAD::BDK:
*        Initialise ERR and MSG
*     15-MAY-1991 (RLVAD::AJC):
*        Remove initialise INTRUPT_FLAG
*     28-MAY-1991 (REVAD::BDK):
*        Remove initialise ERR and MSG
*     07-JUN-1991 (REVAD::BDK):
*        Change comments
*     13-OCT-1992 (RLVAD::AJC):
*        Add INCLUDE 'PAR_PAR'
*     23-AUG-1993 (RLVAD::AJC):
*        Replace PAR_PAR with SUBPAR_SYS
*     04-AUG-1994 (RLVAD::AJC):
*        Pass used part of NAME to MESSYS
*     29-SEP-1994 (RLVAD::AJC):
*        Force DTASK_GETPATH to be loaded
*     11-JUN-2001 (AJC):
*        Call AMS (FAMS) directly
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'DTASK_SYS'
*  Arguments Given:
      CHARACTER*(*) NAME   !  name of this task
      INTEGER NLENGTH      !  length of task name
*  Status:
      INTEGER STATUS
*  Global Variables:
      INCLUDE 'DTASK_CMN'
*  Local Variables:
      INTEGER PATH         !  unused argument for DTASK_GETPATH
      INTEGER MESSID       !  unused argument for DTASK_GETPATH
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Initialise into message system.
*
      CALL FAMS_INIT( NAME(1:NLENGTH), STATUS )
*
*   Initialise action list common block.
*
      NACTS = 0
*
*   Initialise parameter system.
*
      CALL SUBPAR_ACTIV ( NAME, NLENGTH, STATUS )
*
*   Initialise the list of active actions in subsidiary tasks.
*
      CALL TASK_INIT_MESSINFO ( STATUS )
*
*   Put a dummy call to DTASK_GETPATH to force it to be loaded
*   in case TASK_TRIGGER needs it
*
      IF ( STATUS .NE. SAI__OK )
     :   CALL DTASK_GETPATH( NAME, PATH, MESSID, STATUS )

      END
