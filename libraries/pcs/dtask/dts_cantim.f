*+  DTASK_CANTIM - Unix version: cancel the timer for an action
      SUBROUTINE DTASK_CANTIM ( ACTPTR, STATUS )
*    Description :
*     Cancel the timer for the specified action if there is one.
*    Invocation :
*     CALL DTASK_CANTIM ( ACTPTR, STATUS )
*    Parameters :
*     ACTPTR=INTEGER (given)
*           index to DTASK common blocks for the action
*     STATUS=INTEGER
*    Method :
*     Look-up the timer id for this action. If it is non-zero it has 
*     been used to declare a timer, so cancel the timer (even though it 
*     may have gone off), set the timer id to zero and increment the 
*     action counter so that the next timer id generated will be 
*     different.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     03.05.1991: original (REVAD::BDK)
*     13.10.1992:  add INCLUDE 'PAR_PAR' (RLVAD::AJC)
*     23.08.1993:  Replace PAR_PAR with SUBPAR_SYS  (RLVAD::AJC)
*     29.09.1993: Unix version (RLVAD::BKM)
*     27.06.1994: Version for Unix V2 message system (RLVAD::AJC)
*     19.08.1994: Ignore Unix cancel timer status (RLVAD::BKM)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'DTASK_SYS'

*    Import :
      INTEGER ACTPTR   ! index to DTASK common blocks for the action

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'DTASK_CMN'

*    Local variables
      INTEGER LSTAT
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( ACTTIM(ACTPTR) .NE. 0 ) THEN
*
*      A timer has been declared for this action. It may already have gone off.
*      This is ignored on VMS but the Unix cancel timer routine can return
*      ATIMER__NOTFOUND. Just use a local status at present.
*
         LSTAT = SAI__OK
         CALL FATIMER_CANTIM( ACTTIM(ACTPTR), LSTAT )
         ACTTIM(ACTPTR) = 0
         ACTCOUNT(ACTPTR) = ACTCOUNT(ACTPTR) + 1
         IF ( ACTCOUNT(ACTPTR) .GT. DTASK__MAXACTTOT ) THEN
            ACTCOUNT(ACTPTR) = 1
         ENDIF

      ENDIF

      END
