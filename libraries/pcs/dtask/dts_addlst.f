*+  DTASK_ADDLST - add item to task action list
      SUBROUTINE DTASK_ADDLST ( ANAME, AKEY, PATH, MESSID, SEQ, ACODE,      
     :  ACTPTR, STATUS ) 
*    Description :
*     Adds item to the list of current actions. If an entry
*     for the action already exists but is not active, it is re-used.
*     If an entry does not exist, one is created. If an entry already
*     exists and is active, an error status is returned.
*    Invocation :
*     CALL DTASK_ADDLST ( ANAME, AKEY, PATH, MESSID, SEQ, ACTPTR,
*    :  ACODE, STATUS ) 
*    Parameters :
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
*    Method :
*     Search through the common-block arrays. When the slot has been 
*     found (or allocated) for the action, store the details and 
*     increment the action counter.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     John Cooke (REVS::JAC) 22May84
*    History :
*     22-MAY-1984  first insertion (REVA::ADAM])
*     22-MAY-1984  repair to lookup (REVA::ADAM])
*     22-MAY-1984  test debug (REVA::ADAM])
*     22-MAY-1984  remove debug (REVA::ADAM])
*     19-JUN-1984  change status names (REVA::ADAM)
*     20-JUN-1984  added actcount (REVA::ADAM)
*     25.04.1991:  revise INCLUDE files (REVAD::BDK)
*     30.04.1991:  revise INCLUDE files, remove ACTVALUE (REVAD::BDK)
*     01.05.1991:  remove the COUNT and VALUE import arguments
*                  (REVAD::BDK) 
*     03.05.1991:  only set ACTCOUNT for new action, initialise ACTTIM 
*                  (REVAD::BDK)
*     09.05.1991:  pass-in and store the action keyword (REVAD::BDK)
*     04.05.1991:  Rename ACTSTATUS to ACTSTATE (ROE::BMC)
*     07.06.1991:  cange comments (REVAD::BDK)
*     28.02.1992:  Add ACODE argument to set ACTCODE. (AAO::TJF)
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
      CHARACTER*(*) ANAME ! action name
      CHARACTER*(*) AKEY  ! action keyword
      INTEGER PATH        ! path pointer
      INTEGER MESSID      ! message id for action setup
      INTEGER SEQ         ! current sequence status of action
      INTEGER ACODE       ! action code in the parameter system
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
