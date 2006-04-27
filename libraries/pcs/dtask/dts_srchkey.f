      SUBROUTINE DTASK_SRCHKEY ( NAME, ACTPTR, STATUS )
*+
*  Name:
*     DTASK_SRCHKEY

*  Purpose:
*     Search action list for given action keyword

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL DTASK_SRCHKEY ( NAME, ACTPTR, STATUS )

*  Description:
*     Searches action list for the ACTIVE with the given keyword. If found,
*     returns pointer and sets status DTASK__ACTACTIVE. If not found or 
*     action not active returns ACTPTR=0 and status DTASK__NOTFOUND.
*     returns error status. 

*  Arguments:
*     NAME=CHARACTER*(*) (given)
*           action name
*     ACTPTR=INTEGER (returned)
*           pointer to this entry in the action list
*     STATUS=INTEGER

*  Algorithm:
*     Do a sequential search through the names in the common block.

*  Authors:
*     John Cooke (REVS::JAC) 22May84
*     {enter_new_authors_here}

*  History:
*     07-JUN-1991 (REVAD::BDK):
*        Copied from DTASK_SRCHLST
*     27-JUN-1991 (RLVAD::AJC):
*        Correct spellings in comments
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
      CHARACTER NAME*(*)  !   action keyword

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

      ACTPTR = 0

*   look for action with this keyword.....
      N = 1
      FOUND = .FALSE.
      DONE = .FALSE.

      DO WHILE ( ( .NOT. FOUND ) .AND. ( .NOT. DONE ) .AND. 
     :  ( NACTS .GT. 0 ) )
         IF ( ACTKEY(N) .EQ. NAME ) THEN
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
