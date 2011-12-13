      SUBROUTINE TASK_CLEAR_MESSINFO ( ACTPTR, STATUS )
*+
*  Name:
*     TASK_CLEAR_MESSINFO

*  Purpose:
*     Clear list of active subsidiary actions for an action

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_CLEAR_MESSINFO ( ACTPTR, STATUS )

*  Description:
*     Marks all entries in the list of active subsidiary actions for an action
*     as being unused. Entries are added to the list when user code calls
*     TASK_ADD_MESSINFO to indicate that it has initiated an action in a
*     subsidiary (lower-level) task. Entries are removed from the list when
*     fixed d-task routines determine that such an action has completed. It
*     is assumed that this routine is only called when the list is expected
*     to be empty and warning messages are output if any entries are found.

*  Arguments:
*     ACTPTR=INTEGER (given)
*           The pointer by which entries in the list are associated with
*           actions (normally the same as the action pointer that is used
*           in the DTASK_ routines but it doesn't have to be).
*     STATUS=INTEGER

*  Algorithm:
*     Search list for entries with matching action pointers.
*     If found, warn and set action pointer to -1 (unused).
*     If this was last used entry, set topmost -1's to 0 (end of list).

*  Copyright:
*     Copyright (C) 1989, 1991-1993 Science & Engineering Research
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
*     W.F.Lupton (AAOEPP::WFL)
*     B.D.Kelly (REVAD::BDK)
*     A.J.Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-APR-1989 (AAOEPP::WFL):
*        Original
*     23-APR-1991: rearrange INCLUDE files and set error status before
*                  calling ERR routines (REVAD::BDK)
*     06-MAY-1991 (REVAD::BDK):
*        Remove ADAMDEFNS
*     04-OCT-1992 (RLVAD::AJC):
*        Add PAR_PAR for porting
*     11-NOV-1992 (RLVAD::AJC):
*        Use ERR_REP and ERR_FLUSH not ERR_OUT
*     24-AUG-1993 (RLVAD::AJC):
*        Use SUBPAR_SYS not PAR_PAR
*     15-JUN-2001 (AJC):
*        Use AMS (FAMS) _PLOOKUP not MESSYS_PLOOKUP
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'MESSYS_PAR'
      INCLUDE 'MESSYS_LEN'
      INCLUDE 'TASK_PAR'

*  Arguments Given:
      INTEGER ACTPTR                  ! action pointer for which entries
                                      ! are to be cleared

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'TASK_CMN'

*  Local Variables:
      INTEGER I                       ! counter
      LOGICAL DONE                    ! whether have finished going
                                      ! through the list
      CHARACTER*(MESSYS__TNAME) TASK  ! task name used in error messages
*.
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Cycle through the list until exhaust it or find a zero action pointer
*    (which indicates end of list).
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
*    If find a matching pointer, output warning message and clear it to
*    -1 (unused).
*
         ELSE IF ( MESACTPTR(I) .EQ. ACTPTR ) THEN
            TASK = 'unknown'
            CALL FAMS_PLOOKUP ( MESPATH(I), TASK, STATUS )
            STATUS = SAI__ERROR
            CALL ERR_REP ( ' ', 'Programming error: active '//
     :               'subsidiary action at action completion', STATUS )
            CALL MSG_SETC ( 'TASK', TASK )
            CALL MSG_SETI ( 'MESSID', MESMESSID(I) )
            CALL ERR_REP ( ' ', 'Task ^TASK, message id ^MESSID',
     :                                                         STATUS )
            CALL ERR_FLUSH ( STATUS )
            MESACTPTR(I) = -1
         ENDIF
      ENDDO

      END
