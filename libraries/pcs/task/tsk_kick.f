      SUBROUTINE TASK_KICK ( NAME, LENGTH, VALUE, STATUS )
*+
*  Name:
*     TASK_KICK

*  Purpose:
*     Signal another action to reschedule

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_KICK ( NAME, LENGTH, VALUE, STATUS )

*  Description:
*     This routine should be called by one action in a task to cause
*     another action in that task to be rescheduled.

*  Arguments:
*     NAME=CHARACTER (given)
*           name of action to be rescheduled
*     LENGTH=INTEGER (given)
*           number of significant bytes in VALUE
*     VALUE=CHARACTER*(*) (given)
*           a set of bytes to be passed to the main-line code
*     STATUS=INTEGER

*  Algorithm:
*     Use MESSYS_KICK.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     23-MAY-1991 (REVAD::BDK):
*        Original
*     27-MAY-1991 (REVAD::BDK):
*        Remove LIB$SIGNAL
*     15-JUN-2001 (AJC):
*        Use AMS (FAMS) _KICK not MESSYS_KICK
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER*(*) NAME     ! name of action to be rescheduled
      INTEGER LENGTH         ! number of significant bytes in VALUE
      CHARACTER*(*) VALUE    ! a set of bytes to be passed to the
                             ! main-line code
*  Status:
      INTEGER STATUS
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Inform the message system.
*
      CALL FAMS_KICK ( NAME, LENGTH, VALUE, STATUS )

      END
