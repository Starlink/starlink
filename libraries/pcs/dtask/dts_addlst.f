      SUBROUTINE DTASK_ADDLST ( ANAME, AKEY, PATH, MESSID, SEQ, ACODE,
     :  ACTPTR, STATUS )
*+
*  Name:
*     DTASK_ADDLST

*  Purpose:
*     Add item to task action list

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL DTASK_ADDLST ( ANAME, AKEY, PATH, MESSID, SEQ, ACTPTR,
*     :  ACODE, STATUS )

*  Description:
*     Adds item to the list of current actions. If an entry
*     for the action already exists but is not active, it is re-used.
*     If an entry does not exist, one is created. If an entry already
*     exists and is active, an error status is returned.

*  Arguments:
*     ANAME=CHARACTER*(*) (given)
*           action name
*     AKEY=CHARACTER*(*) (given)
*           action keyword
*     PATH=INTEGER (given)
*           path pointer
*     MESSID=INTEGER (given)
*           transaction number
*     SEQ=INTEGER (given)
*           current sequence number of action
*     ACODE=INTEGER (given)
*           action code in the parameter system
*     ACTPTR=INTEGER (returned)
*           pointer to this entry in the action list
*
*     STATUS=INTEGER

*  Algorithm:
*     Search through the common-block arrays. When the slot has been
*     found (or allocated) for the action, store the details and
*     increment the action counter.

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
*     22-MAY-1984 (REVA::ADAM]):
*        Test debug
*     22-MAY-1984 (REVA::ADAM]):
*        Remove debug
*     19-JUN-1984 (REVA::ADAM):
*        Change status names
*     20-JUN-1984 (REVA::ADAM):
*        Added actcount
*     25-APR-1991 (REVAD::BDK):
*        Revise INCLUDE files
*     30-APR-1991 (REVAD::BDK):
*        Revise INCLUDE files, remove ACTVALUE
*     01-MAY-1991: remove the COUNT and VALUE import arguments
*                  (REVAD::BDK)
*     03-MAY-1991: only set ACTCOUNT for new action, initialise ACTTIM
*                  (REVAD::BDK)
*     09-MAY-1991 (REVAD::BDK):
*        Pass-in and store the action keyword
*     04-MAY-1991 (ROE::BMC):
*        Rename ACTSTATUS to ACTSTATE
*     07-JUN-1991 (REVAD::BDK):
*        Cange comments
*     28-FEB-1992 (AAO::TJF):
*        Add ACODE argument to set ACTCODE.
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
      CHARACTER*(*) ANAME ! action name
      CHARACTER*(*) AKEY  ! action keyword
      INTEGER PATH        ! path pointer
      INTEGER MESSID      ! message id for action setup
      INTEGER SEQ         ! current sequence status of action
      INTEGER ACODE       ! action code in the parameter system
*  Arguments Returned:
      INTEGER ACTPTR   !  pointer to this entry in the action list
*  Status:
      INTEGER STATUS
*  Global Variables:
      INCLUDE 'DTASK_CMN'
*  Local Variables:
      INTEGER N
      LOGICAL FOUND
      LOGICAL DONE
*.

      IF ( STATUS .NE. SAI__OK ) RETURN
*
*   See if this action name has already been allocated a slot.
*
      N = 1
      FOUND = .FALSE.
      DONE = .FALSE.

      DO WHILE ( ( .NOT. FOUND ) .AND. ( .NOT. DONE ) .AND.
     :  ( NACTS .GT. 0 ) )
         IF ( ACTNAME(N) .EQ. ANAME ) THEN
            FOUND = .TRUE.
            ACTPTR = N
         ENDIF
         N = N + 1
         IF ( N .GT. NACTS ) THEN
            DONE = .TRUE.
         ENDIF
      ENDDO
*
*   Then take appropriate action.
*
      IF ( FOUND ) THEN
         IF ( ACTSTATE(ACTPTR) .EQ. DTASK__REMOVED ) THEN
*
*         Space for the name already exists but is not active.
*
            CONTINUE
         ELSE IF ( ACTSTATE(ACTPTR) .EQ. DTASK__ACTIVE ) THEN
*
*         An entry already exists and is active.
*
            STATUS = DTASK__ACTACTIVE
         ELSE
*
*         An entry exists with an illegal status
*
            STATUS = DTASK__ILLACTSTATUS
         ENDIF

      ELSE
*
*      Allocate a new slot if possible.
*
         NACTS = NACTS + 1
         IF ( NACTS .LE. DTASK__MAXACT ) THEN
            ACTPTR = NACTS
            ACTNAME(ACTPTR) = ANAME
            ACTKEY(ACTPTR) = AKEY
            ACTCOUNT(ACTPTR) = 1
            ACTTIM(ACTPTR) = 0
            ACTCODE(ACTPTR) = ACODE
         ELSE
            STATUS = DTASK__ACTOVF
         ENDIF

      ENDIF
*
*   Add the details to the list if all is well.
*
      IF ( STATUS .EQ. SAI__OK ) THEN
         ACTSTATE(ACTPTR) = DTASK__ACTIVE
         ACTPATH(ACTPTR) = PATH
         ACTMESSID(ACTPTR) = MESSID
         ACTSEQ(ACTPTR) = SEQ
      ENDIF

      END
