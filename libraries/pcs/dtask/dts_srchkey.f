*+  DTASK_SRCHKEY - search action list for given action keyword
      SUBROUTINE DTASK_SRCHKEY ( NAME, ACTPTR, STATUS )
*    Description :
*     Searches action list for the ACTIVE with the given keyword. If found,
*     returns pointer and sets status DTASK__ACTACTIVE. If not found or 
*     action not active returns ACTPTR=0 and status DTASK__NOTFOUND.
*     returns error status. 
*    Invocation :
*     CALL DTASK_SRCHKEY ( NAME, ACTPTR, STATUS )
*    Parameters :
*     NAME=CHARACTER*(*) (given)
*           action name
*     ACTPTR=INTEGER (returned)
*           pointer to this entry in the action list
*     STATUS=INTEGER
*    Method :
*     Do a sequential search through the names in the common block.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     John Cooke (REVS::JAC) 22May84
*    History :
*     07.06.1991: copied from DTASK_SRCHLST (REVAD::BDK)
*     27.06.1991: correct spellings in comments (RLVAD::AJC)
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
      CHARACTER NAME*(*)  !   action keyword

*    Export :
      INTEGER ACTPTR   !  pointer to this entry in the action list

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'DTASK_CMN'

*    Local variables :
      INTEGER N
      LOGICAL FOUND
      LOGICAL DONE
*-

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
