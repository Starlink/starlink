      SUBROUTINE TASK_SEARCH_MESSINFO ( PATH, MESSID, ACTPTR, STATUS )
*+
*  Name:
*     TASK_SEARCH_MESSINFO

*  Purpose:
*     Search for entry from list of active subsid actions

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_SEARCH_MESSINFO ( PATH, MESSID, ACTPTR, STATUS )

*  Description:
*     Searches for an entry corresponding to a specified path and message id in
*     the list of active subsidiary actions. If the entry is found, the action
*     pointer is returned. Otherwise, an action pointer of zero is returned
*     (but not a bad status).

*  Arguments:
*     PATH=INTEGER (given)
*           The path identifying the subsidiary task
*     MESSID=INTEGER (given)
*           The message id identifying the action in the subsidiary task
*     ACTPTR=INTEGER (returned)
*           The action pointer for the action that initiated the action in
*           the subsidiary task. Zero if the path and message are not found
*           in the list
*     STATUS=INTEGER

*  Algorithm:
*     Search list for entry with matching path and message id.
*     If found, return action pointer.
*     If not found, return action pointer of zero.

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
      INTEGER PATH        ! the path identifying the subsidiary task
      INTEGER MESSID      ! the message id'ing the action in the subsidiary task

*  Arguments Returned:
      INTEGER ACTPTR      ! the action pointer for the initiating action

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'TASK_CMN'

*  Local Variables:
      INTEGER I           ! counter
      LOGICAL DONE        ! whether have searchd entry to the list
*.
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Cycle through the list until find matching entry or find a zero action
*    pointer (which indicates end of list).
*
      I = 0
      DONE = .FALSE.
      ACTPTR = 0
      DO WHILE ( ( I .LT. TASK__MAXSUB ) .AND. ( .NOT. DONE ) )
         I = I + 1
*
*    If find a zero action pointer (end of list) exit from the loop.
*
         IF ( MESACTPTR(I) .EQ. 0 ) THEN
            DONE = .TRUE.
*
*    If find a matching entry, return action pointer and exit from the loop.
*
         ELSE IF ( ( MESPATH(I) .EQ. PATH ) .AND.
     :    ( MESMESSID(I) .EQ. MESSID ) ) THEN
            ACTPTR = MESACTPTR(I)
            DONE = .TRUE.
         ENDIF
      ENDDO

      END
