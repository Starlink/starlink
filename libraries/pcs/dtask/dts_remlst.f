*+  DTASK_REMLST - remove named action from active action list
      SUBROUTINE DTASK_REMLST ( NAME, STATUS )
*    Description :
*     Remove named action from active action list.
*    Invocation :
*     CALL DTASK_REMLST ( NAME, STATUS )
*    Parameters :
*     NAME=CHARACTER*(*) (given)
*           name of action to be removed
*     STATUS=INTEGER
*    Method :
*     Searches action list for named ACTIVE action.  If found,
*     sets item state to "removed". If not found, returns error status.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     John Cooke (REVS::JAC) 22May84
*    History :
*     date:  changes (institution::username)
*     22-MAY-1984  first insertion (REVA::ADAM])
*     22-MAY-1984  dir (REVA::ADAM])
*     19-JUN-1984  change status symbol names (REVA::ADAM)
*     25.04.1991:  revise INCLUDE files (REVAD::BDK)
*     30.04.1991:  revise INCLUDE files (REVAD::BDK)
*     04.05.1991:  Rename ACTSTATE to ACTSTATUS (ROE::BMC)
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

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'DTASK_CMN'

*    Local variables :
      INTEGER N
      INTEGER ACTPTR
      LOGICAL FOUND
      LOGICAL DONE
*-

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
