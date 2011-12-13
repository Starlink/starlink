      SUBROUTINE TASK_REMOVE_MESSINFO ( PATH, MESSID, STATUS )
*+
*  Name:
*     TASK_REMOVE_MESSINFO

*  Purpose:
*     Remove entry from list of active subsidiary actions

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_REMOVE_MESSINFO ( PATH, MESSID, STATUS )

*  Description:
*     Removes an entry from the list of active subsidiary actions. This simply
*     involves searching the list for an entry with a matching path and message
*     id and, if found, removing it. Entries are not checked for ownership
*     by the current action.

*  Arguments:
*     PATH=INTEGER (given)
*           The path identifying the subsidiary task
*     MESSID=INTEGER (given)
*           The message id identifying the action in the subsidiary task
*     STATUS=INTEGER

*  Algorithm:
*     Search list for entry with matching path and message id.
*     Set action pointer to -1 (unused).
*     If this was last used entry, set topmost -1's to 0 (end of list).

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
*     23-APR-1991: set MESACTPTR(I)=0 if the entry is at the end of the
*                  storage arrays (REVAD::BDK)
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

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'TASK_CMN'

*  Local Variables:
      INTEGER I           ! counter
      LOGICAL DONE        ! whether have removed entry to the list
*.
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Cycle through the list until find matching entry or find a zero action
*    pointer (which indicates end of list).
*
      I = 0
      DONE = .FALSE.
      DO WHILE ( ( I .LT. TASK__MAXSUB ) .AND. ( .NOT. DONE ) )
         I = I + 1
*
*    If find a zero action pointer, set adjacent -1 (unused) pointers to
*    zero too, and exit from the loop.
*
         IF ( MESACTPTR(I) .EQ. 0 ) THEN
            I = I - 1
            DO WHILE ( ( I .GT. 0 ) .AND. ( MESACTPTR(I) .EQ. -1 ) )
               MESACTPTR(I) = 0
               I = I - 1
            ENDDO
            DONE = .TRUE.
*
*    If find a matching entry, clear its action pointer to -1 (unused).
*    Continue, in case there are duplicate entries (and so that the above
*    logic gets a chance to operate).
*
         ELSE IF ( ( MESPATH(I) .EQ. PATH ) .AND.
     :    ( MESMESSID(I) .EQ. MESSID ) ) THEN
            IF ( I .EQ. TASK__MAXSUB ) THEN
*
*            Entry is at the very end of the arrays. Need to restart
*            high water mark. Decrement I to force going around the loop
*            again.
*
               MESACTPTR(I) = 0
               I = I - 1
            ELSE
               MESACTPTR(I) = -1
            ENDIF
         ENDIF
      ENDDO

      END
