      SUBROUTINE TASK_COUNT_MESSINFO ( ACTPTR, COUNT, STATUS )
*+
*  Name:
*     TASK_COUNT_MESSINFO

*  Purpose:
*     Count entries in list of active subsid actions

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_COUNT_MESSINFO ( ACTPTR, COUNT, STATUS )

*  Description:
*     Counts entries in the list of active actions in subsidiary tasks
*     corresponding to a given initiating action.

*  Arguments:
*     ACTPTR=INTEGER (given)
*           The action pointer for the action that initiated the actions in
*           the subsidiary task.
*     COUNT=INTEGER (given)
*           The number of actions initiated by this action that are still
*           active.
*     STATUS=INTEGER

*  Algorithm:
*     Count list entries associated with this action.

*  Copyright:
*     Copyright (C) 1989, 1991-1993 Science & Engineering Research
*     Council. All Rights Reserved.

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
*     W.F.Lupton (AAOEPP::WFL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-1989 (AAOEPP::WFL):
*        Original
*     23-APR-1991 (REVAD::BDK):
*        Rearrange INCLUDE files
*     06-MAY-1991 (REVAD::BDK):
*        Remove ADAMDEFNS
*     04-OCT-1992 (RLVAD::AJC):
*        Add PAR_PAR for porting
*     24-AUG-1993 (RLVAD::AJC):
*        Use SUBPAR_SYS not PAR_PAR
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'MESSYS_LEN'
      INCLUDE 'TASK_PAR'

*  Arguments Given:
      INTEGER ACTPTR      ! the action pointer for the initiating action

*  Arguments Returned:
      INTEGER COUNT       ! the number of active actions initiated by this one

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'TASK_CMN'

*  Local Variables:
      INTEGER I           ! counter
      LOGICAL DONE        ! whether have finished searching the list
*.
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Cycle through the list counting matching entries until hit end of list or
*    find a zero action pointer (which indicates end of list).
*
      I = 0
      DONE = .FALSE.
      COUNT = 0
      DO WHILE ( ( I .LT. TASK__MAXSUB ) .AND. ( .NOT. DONE ) )
         I = I + 1
*
*    If find a zero action pointer (end of list) exit from the loop.
*
         IF ( MESACTPTR(I) .EQ. 0 ) THEN
            DONE = .TRUE.
*
*    If find a matching entry, increment count.
*
         ELSE IF ( MESACTPTR(I) .EQ. ACTPTR ) THEN
            COUNT = COUNT + 1
         ENDIF
      ENDDO

      END
