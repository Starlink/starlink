      SUBROUTINE TASK_ASTMSG ( NAME, LENGTH, VALUE, STATUS )
*+
*  Name:
*     TASK_ASTMSG

*  Purpose:
*     Used in user AST routine to signal to dtask

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_ASTMSG ( NAME, LENGTH, VALUE, STATUS )

*  Description:
*     This routine should be called inside a user AST routine invoked
*     initially by a d-task action, on completion of the AST routine.
*     It signals to the d-task system that completion has occurred and
*     the required action can now enter the next stage.

*  Arguments:
*     NAME=CHARACTER (given)
*           name of action to be staged
*     LENGTH=INTEGER (given)
*           number of significant bytes in VALUE
*     VALUE=CHARACTER*(*) (given)
*           a set of bytes to be passed to the main-line code
*     STATUS=INTEGER

*  Algorithm:
*     Signal the MESSYS that an AST interrupt has occurred.

*  Copyright:
*     Copyright (C) 1991-1993 Science & Engineering Research Council.
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
*     09-APR-1991 (REVAD::BDK):
*        Original
*     25-APR-1991 (REVAD::BDK):
*        Rearrange INCLUDE files
*     30-APR-1991 (REVAD::BDK):
*        Rearrange INCLUDE files
*     13-MAY-1991: move from DTASK library, don't disable ASTs
*                  (REVAD::BDK)
*     12-JUN-1991 (REVAD::BDK):
*        Remove lib$signal
*     04-OCT-1992 (RLVAD::AJC):
*        Add PAR_PAR for porting
*     24-AUG-1993 (RLVAD::AJC):
*        Use SUBPAR_SYS not PAR_PAR
*     15-JUN-2001 (AJC):
*        Use AMS (FAMS) _ASTMSG not MESSYS_ASTMSG
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'TASK_PAR'
      INCLUDE 'MESSYS_LEN'

*  Arguments Given:
      CHARACTER*(*) NAME     ! name of action to be staged
      INTEGER LENGTH         ! number of significant bytes in VALUE
      CHARACTER*(*) VALUE    ! a set of bytes to be passed to the
                             ! main-line code
*  Status:
      INTEGER STATUS
*  Global Variables:
      INCLUDE 'TASK_CMN'
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Set the interrupt flag in case an application wants to test for it.
*
      INTRUPT_FLAG = .TRUE.
*
*   Inform the message system.
*
      CALL FAMS_ASTMSG ( NAME, LENGTH, VALUE, STATUS )

      END
