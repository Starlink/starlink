*+  DTASK_SRCHLST - search action list for named action
      SUBROUTINE DTASK_SRCHLST ( NAME, ACTPTR, STATUS )
*    Description :
*     Searches action list for named ACTIVE action.  If found,
*     returns pointer and sets status DTASK__ACTACTIVE. If not found or 
*     action not active, returns ACTPTR=0 status DTASK__NOTFOUND.
*    Invocation :
*     CALL DTASK_SRCHLST ( NAME, ACTPTR, STATUS )
*    Parameters :
*     NAME=CHARACTER*(*) (given)
*           action name
*     ACTPTR=INTEGER (returned)
*           pointer to this entry in the action list
*     STATUS=INTEGER
*    Method :
*     Do a sequential search through the names in the common block.
*     NACTS is the number of actions currently in the list.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     John Cooke (REVS::JAC) 22May84
*    History :
*     date:  changes (institution::username)
*     22-MAY-1984  first insertion (REVA::ADAM])
*     22-MAY-1984  repair to lookup (REVA::ADAM])
*     19-JUN-1984  changed status symbol names (REVA::ADAM)
*     25.04.1991:  revise INCLUDE files (REVAD::BDK)
*     30.04.1991:  revise INCLUDE files (REVAD::BDK)
*     04.05.1991:  Rename ACTSTATE to ACTSTATUS (ROE::BMC)
*     07.06.1991:  change comments (REVAD::BDK)
*     13.10.1992:  add INCLUDE 'PAR_PAR' (RLVAD::AJC)
*     23.08.1993:  Replace PAR_PAR with SUBPAR_SYS  (RLVAD::AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'DTASK_SYS'
      INCLUDE 'DTASK_ERR'

*    Import :
      CHARACTER NAME*(*)  !   action name

*    Export :
      INTEGER ACTPTR   !  pointer to this entry in the action list

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'DTASK_CMN'

*    Local variables :
      INTEGER N        ! counter for searching list
      LOGICAL FOUND    ! flag for name found
      LOGICAL DONE     ! flag for end of list reached
*-

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
