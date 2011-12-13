      SUBROUTINE ADAM_PATH ( TASK_NAME, PATH, STATUS )
*+
*  Name:
*     ADAM_PATH

*  Purpose:
*     obtain path pointer to another task.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ADAM_PATH ( TASK_NAME, PATH, STATUS )

*  Description:
*     Returns a pointer for intertask communication to the task
*     described by TASK_NAME.

*  Arguments:
*     TASK_NAME=CHARACTER*(*) (given)
*        name of task to which path is required
*     PATH=INTEGER (returned)
*        pointer to the path

*  Algorithm:
*     Call MESSYS_PATH. If this succeeds it will return a status with
*     value ADAM__OK if a new path to the task has been set up, or
*     MESSYS_PATHOPEN if the path already existed.

*  Copyright:
*     Copyright (C) 1984, 1985, 1992 Science & Engineering Research Council.
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
*     10-OCT-1984  change "normal" to "adam__OK" (REVA::ADAM)
*     14-MAY-1985  turn PATHOPEN status into adam__ok (REVAD::BDK)
*     12.11.1992:  use SAI__OK not ADAM__OK
*        rename MESERRS MESSYS_ERR (RLVAD::AJC)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'MESSYS_ERR'

*  Arguments Given:
      CHARACTER TASK_NAME*(*)   !  name of task to which path is required

*  Arguments Returned:
      INTEGER PATH              !  pointer to the path

*  Status:
      INTEGER STATUS

*.


      IF ( STATUS .EQ. SAI__OK ) THEN

         CALL MESSYS_PATH ( TASK_NAME, PATH, STATUS )

         IF ( STATUS .EQ. MESSYS__PATHOPEN ) STATUS = SAI__OK

      ENDIF

      END

