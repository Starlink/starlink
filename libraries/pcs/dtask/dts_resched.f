*+  DTASK_RESCHED - Unix version : sets a timer for rescheduling an action
      SUBROUTINE DTASK_RESCHED ( ACTPTR, ACTCNT, SCHEDTIME, STATUS )
*    Description :
*     Set a timer. When the timer completes the action specified by 
*     ACTPTR and ACTCNT will be rescheduled if it is still waiting.
*    Invocation :
*     CALL DTASK_RESCHED ( ACTPTR, ACTCNT, SCHEDTIME, STATUS )
*    Parameters :
*     ACTPTR=INTEGER (given)
*           pointer to required action in action list
*     ACTCNT=INTEGER (given)
*           timer counter for the action
*     SCHEDTIME=INTEGER (given)
*           time before re-scheduling ( in milliseconds )
*     STATUS=INTEGER
*    Method :
*     Use the Unix interval timer process accessed through FATIMER_SETTIMR
*     and DTASK_ASTHDLR to deliver a MESSYS format message to a task on its 
*     reschedule queue after a delay.
*     The values of ACTPTR and ACTCNT are passed to and returned from the
*     interval timer in the reschedule message by packing them into 4 bytes. 
*     Note that the VALUE of the packed result has to be passed - if the 
*     ADDRESS was used it would get overwritten by the next action to request 
*     a timed reschedule.
*     The timer parameter (c.f. AST value on VMS) doubles as the timer 
*     identifier, so it is stored to allow subsequent cancelling of the timer.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     John Cooke (REVS::JAC) 22May84
*    History :
*     date:  changes (institution::username)
*     22-MAY-1984  first insertion (REVA::ADAM])
*     22-MAY-1984  remove %val on AST parameter (REVA::ADAM])
*     25-MAY-1984  add %val on actptr in setimr (REVA::ADAM])
*     25-MAY-1984  remove %val again ! (REVA::ADAM])
*     25-MAY-1984  replace %val again for tests (REVA::ADAM])
*     20-JUN-1984  new error system; 10ms to 1ms ticks (REVA::ADAM)
*     25.04.1991:  revise INCLUDE files and add comments (REVAD::BDK)
*     03.05.1991:  store timer id (REVAD::BDK)
*     13.05.1991:  don't use LIB$SIGNAL (REVAD::BDK)
*     28.05.1991:  use ERR_REP (REVAD::BDK)
*     28.05.1991:  Correct ER_REP to ERR_REP (ROE::BMC)
*     05.06.1991:  Pack the AST parameter using arithmetic rather than
*                  an equivalence statement (ROE::BMC)
*     07.06.1991:  change comments (REVAD::BDK)
*     13.10.1992:  add INCLUDE 'PAR_PAR'
*                  get ^STATUS via DTASK_ESETK (RLVAD::AJC)
*     23.08.1993:  Replace PAR_PAR with SUBPAR_SYS  (RLVAD::AJC)
*     08.09.1993:  Unix version (RLVAD::BKM)
*     28.06.1994:  New Unix version (RAL::AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'DTASK_SYS'
*    Import :
      INTEGER ACTPTR          ! pointer to required action in action
                              ! list 
      INTEGER ACTCNT          ! timer counter for the action 
      INTEGER SCHEDTIME       ! time before re-scheduling in
                              ! milliseconds
*    Status :
      INTEGER STATUS

*    External references :
      EXTERNAL DTASK_CHDLR     ! Handler called directly from ATIMER service

*    Global variables :
      INCLUDE 'DTASK_CMN'

*    Local variables :
      INTEGER TIMERVAL         ! timer parameter
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Insert two 16-bit values in the timer parameter.
*
      TIMERVAL = ACTPTR * 65536 + ACTCNT
*
*   Set timer running
*
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL FATIMER_SETTIMR( SCHEDTIME, TIMERVAL, DTASK_CHDLR, 
     :                         STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP ( ' ', 'DTASK_RESCHED: failed to start timer', 
     :        STATUS )
         ELSE
            ACTTIM(ACTPTR) = TIMERVAL
         ENDIF
      ENDIF

      END
