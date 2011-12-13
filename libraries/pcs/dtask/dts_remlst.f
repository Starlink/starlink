      SUBROUTINE DTASK_REMLST ( NAME, STATUS )
*+
*  Name:
*     DTASK_REMLST

*  Purpose:
*     Remove named action from active action list

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL DTASK_REMLST ( NAME, STATUS )

*  Description:
*     Remove named action from active action list.

*  Arguments:
*     NAME=CHARACTER*(*) (given)
*           name of action to be removed
*     STATUS=INTEGER

*  Algorithm:
*     Searches action list for named ACTIVE action.  If found,
*     sets item state to "removed". If not found, returns error status.

*  Copyright:
*     Copyright (C) 1984, 1991-1993 Science & Engineering Research
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
*     John Cooke (REVS::JAC) 22May84
*     {enter_new_authors_here}

*  History:
*     22-MAY-1984 (REVA::ADAM]):
*        First insertion
*     22-MAY-1984 (REVA::ADAM]):
*        Dir
*     19-JUN-1984 (REVA::ADAM):
*        Change status symbol names
*     25-APR-1991 (REVAD::BDK):
*        Revise INCLUDE files
*     30-APR-1991 (REVAD::BDK):
*        Revise INCLUDE files
*     04-MAY-1991 (ROE::BMC):
*        Rename ACTSTATE to ACTSTATUS
*     13-OCT-1992 (RLVAD::AJC):
*        Add INCLUDE 'PAR_PAR'
*     23-AUG-1993 (RLVAD::AJC):
*        Replace PAR_PAR with SUBPAR_SYS
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
      INCLUDE 'DTASK_ERR'

*  Arguments Given:
      CHARACTER NAME*(*)  !   action name

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'DTASK_CMN'

*  Local Variables:
      INTEGER N
      INTEGER ACTPTR
      LOGICAL FOUND
      LOGICAL DONE
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

*   look for action of this name.....
      N = 1
      FOUND = .FALSE.
      DONE = .FALSE.

      DO WHILE ( ( .NOT. FOUND ) .AND. ( .NOT. DONE ) .AND.
     :  ( NACTS .GT. 0 ) )
         IF ( ACTNAME(N) .EQ. NAME ) THEN
            FOUND = .TRUE.
            ACTPTR = N
         ENDIF
         N = N + 1
         IF ( N .GT. NACTS ) THEN
            DONE = .TRUE.
         ENDIF
      ENDDO

*   check if active.....
      IF ( FOUND ) THEN
         IF ( ACTSTATE(ACTPTR) .EQ. DTASK__ACTIVE ) THEN
            ACTSTATE(ACTPTR) = DTASK__REMOVED
         ELSE
            STATUS = DTASK__ACTREMOVED
            ACTSTATE(ACTPTR) = DTASK__REMOVED
         ENDIF
      ELSE
         STATUS = DTASK__NOTFOUND
      ENDIF

      END
