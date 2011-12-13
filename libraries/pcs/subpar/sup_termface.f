      SUBROUTINE SUBPAR_TERMFACE ( UFACE, STATUS )
*+
*  Name:
*     SUBPAR_TERMFACE

*  Purpose:
*     set flag saying whether task connected to terminal.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_TERMFACE ( UFACE, STATUS )

*  Description:
*     Set a flag in the parameter system common blocks saying whether
*     the task is a 'normal' task, or whether, instead, it is being run
*     from a terminal. In the latter case, all output and prompting goes
*     directly to the terminal instead of being sent to another task.

*  Arguments:
*     UFACE=LOGICAL (given)
*        .TRUE. => attached to terminal
*        .FALSE. => controlled by another task
*     STATUS=INTEGER

*  Algorithm:
*     Set the COMMON variable RUNFACE.

*  Copyright:
*     Copyright (C) 1985, 1987, 1993 Science & Engineering Research Council.
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
*     BDK: B D Kelly (ROE)
*     {enter_new_authors_here}

*  History:
*     11-NOV-1985 (BDK):
*        Original
*     05-MAY-1987 (BDK):
*        Make RUNFACE an integer
*      1-MAR-1993 (AJC):
*        Add INCLUDE DAT_PAR
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'


*  Arguments Given:
      LOGICAL UFACE           ! .TRUE. => attached to terminal
                              ! .FALSE. => controlled by another task


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( UFACE ) THEN
         RUNFACE = SUBPAR__TERM
      ELSE
         RUNFACE = SUBPAR__TASK
      ENDIF

      END
