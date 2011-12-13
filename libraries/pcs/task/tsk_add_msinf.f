      SUBROUTINE TASK_ADD_MESSINFO ( PATH, MESSID, STATUS )
*+
*  Name:
*     TASK_ADD_MESSINFO

*  Purpose:
*     Add to list of active subsidiary actions for an action

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_ADD_MESSINFO ( PATH, MESSID, STATUS )

*  Description:
*     Adds an entry to the list of active subsidiary actions for the current
*     action. The entry simply associates the action with the path and
*     message id corresponding to the action that it has just initiated
*     in a subsidiary task.

*  Arguments:
*     PATH=INTEGER (given)
*           The path identifying the subsidiary task
*     MESSID=INTEGER (given)
*           The message id identifying the action in the subsidiary task
*     STATUS=INTEGER

*  Algorithm:
*     Check that there is a current action (action pointer should be in COMMON).
*     Search through the list until find a free entry (action pointer <= 0).
*     Copy current action pointer, path and message id to the entry.

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
*     29-APR-1989 (AAOEPP::WFL):
*        Original
*     23-APR-1991 (REVAD::BDK):
*        Rearrange INCLUDE files
*     06-MAY-1991 (REVAD::BDK):
*        Remove ADAMDEFNS
*     04-OCT-1992 (RLVAD::AJC):
*        Add PAR_PAR
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
      INCLUDE 'TASK_ERR'
      INCLUDE 'TASK_PAR'

*  Arguments Given:
      INTEGER PATH        ! the path identifying the subsidiary task
      INTEGER MESSID      ! the message id'ing the action in the subsidiary task

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'TASK_CMN'

*  Local Variables:
      INTEGER I           ! counter
      LOGICAL DONE        ! whether have added entry to the list
*.
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Check that there is a current action.
*
      IF ( CURACTPTR .LE. 0 ) THEN
         STATUS = TASK__NOCURACT
*
*    Search for a free entry in the list.
*
      ELSE
         I = 0
         DONE = .FALSE.
         DO WHILE ( ( I .LT. TASK__MAXSUB ) .AND. ( .NOT. DONE ) )
            I = I + 1
            IF ( MESACTPTR(I) .LE. 0 ) THEN
               MESACTPTR(I) = CURACTPTR
               MESPATH(I) = PATH
               MESMESSID(I) = MESSID
               DONE = .TRUE.
            ENDIF
         ENDDO
         IF ( .NOT. DONE ) THEN
            STATUS = TASK__NOMESROOM
         ENDIF
      ENDIF

      END
