      SUBROUTINE DTASK_SRCHLST ( NAME, ACTPTR, STATUS )
*+
*  Name:
*     DTASK_SRCHLST

*  Purpose:
*     Search action list for named action

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL DTASK_SRCHLST ( NAME, ACTPTR, STATUS )

*  Description:
*     Searches action list for named ACTIVE action.  If found,
*     returns pointer and sets status DTASK__ACTACTIVE. If not found or
*     action not active, returns ACTPTR=0 status DTASK__NOTFOUND.

*  Arguments:
*     NAME=CHARACTER*(*) (given)
*           action name
*     ACTPTR=INTEGER (returned)
*           pointer to this entry in the action list
*     STATUS=INTEGER

*  Algorithm:
*     Do a sequential search through the names in the common block.
*     NACTS is the number of actions currently in the list.

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
*        Repair to lookup
*     19-JUN-1984 (REVA::ADAM):
*        Changed status symbol names
*     25-APR-1991 (REVAD::BDK):
*        Revise INCLUDE files
*     30-APR-1991 (REVAD::BDK):
*        Revise INCLUDE files
*     04-MAY-1991 (ROE::BMC):
*        Rename ACTSTATE to ACTSTATUS
*     07-JUN-1991 (REVAD::BDK):
*        Change comments
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

*  Arguments Returned:
      INTEGER ACTPTR   !  pointer to this entry in the action list

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'DTASK_CMN'

*  Local Variables:
      INTEGER N        ! counter for searching list
      LOGICAL FOUND    ! flag for name found
      LOGICAL DONE     ! flag for end of list reached
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      ACTPTR = 0

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
            STATUS = DTASK__ACTACTIVE
         ELSE
            STATUS = DTASK__NOTFOUND
            ACTPTR = 0
         ENDIF
      ELSE
         STATUS = DTASK__NOTFOUND
      ENDIF

      END
