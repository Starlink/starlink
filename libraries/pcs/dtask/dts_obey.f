*+  DTASK_OBEY - handle action for "obey" request 
      SUBROUTINE DTASK_OBEY ( DTASK_APPLIC, ACTPTR, VALUE, STATUS )
*    Description :
*     Carry out an OBEY, whether first-time in or as a result of a 
*     reschedule. After the application has returned, set-up any 
*     reschedule it may have requested.
*    Invocation :
*     CALL DTASK_OBEY ( DTASK_APPLIC, ACTPTR, VALUE, STATUS )
*    Parameters :
*     DTASK_APPLIC=EXTERNAL (given)
*           address of action routine
*     ACTPTR=INTEGER (given)
*           index for looking-up the action details
*     VALUE=CHARACTER*(*) (given and returned)
*           command line parameter string
*     STATUS=INTEGER
*    Method :
*     Call DTASK_APPLIC, check the returned status and set up any 
*     requested rescheduling. If the action has completed send the final
*     acknowledgment. 
*    Deficiencies :
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     John Cooke (REVA::JAC) 17May84
*    History :
*     date:  changes (institution::username)
*     22-MAY-1984  first insertion (REVA::ADAM])
*     22-MAY-1984  remove entry from action list once complete (REVA::ADAM])
*     22-MAY-1984  test debug (REVA::ADAM])
*     22-MAY-1984  ditto (REVA::ADAM])
*     22-MAY-1984  repair call to addlst (REVA::ADAM])
*     22-MAY-1984  remove debug (REVA::ADAM])
*     22-MAY-1984  increment actseq on "wait" or "stage" (REVA::ADAM])
*     24-MAY-1984  added msg_context - was missing (REVA::ADAM])
*     24-MAY-1984  handle rejected actions by not calling act (REVA::ADAM])
*     20-JUN-1984  changed error symbol names (REVA::ADAM)
*     20-JUN-1984  removed references to obstat (REVA::ADAM)
*     25-JUN-1984  added AST interrupt (REVA::ADAM)
*     25-JUN-1984  added seq increment for astint (REVA::ADAM)
*     17-AUG-1984  add "inform" return status (REVA::ADAM)
*     26-OCT-1984  add CHECKACT (REVAD::BDK)
*     14-NOV-1984  made 'SEQ' an import/export parameter (REVA::ADAM)
*     14-NOV-1984  allow seq to return as 0 (for first return!) (REVA::ADAM)
*     16-NOV-1984  new version with parameter system (REVA::ADAM)
*     16-NOV-1984  handle ACTCONSTR status (REVA::ADAM)
*     16-NOV-1984  handle constraint status correctly! (REVA::ADAM)
*     16-NOV-1984  try again! (REVA::ADAM)
*     24-NOV-1984  use DTASK_ACKNOW (REVAD::BDK)
*     24-NOV-1984  handle SUBPAR_FINDACT status correctly (BDK) (REVA::ADAM)
*     23.06.1985:  report name of parameter violating constraints 
*                      (REVAD::BDK)
*     09.10.1985:  trap OK and NORMAL status returns from ACT (REVAD::BDK)
*     25.03.1986:  trap ACTCANCEL return from ACT, clarify handling of 
*                  error conditions after CHECKACT (REVAD::BDK)
*      9.01.1987:  new command line parser added (AAOEPP::JAB)
*     26.05.1987:  use action keyword (REVAD::BDK)
*     30.04.1989:  call DTASK_APPLIC rather than ACT and surround with
*                  TASK_PUT_CURRINFO and TASK_GET_CURRINFO calls (AAOEPP::WFL)
*     30.04.1989:  call TASK_CLEAR_MESSINFO at start and end of action to
*                  clear records of active subsidiary actions (AAOEPP::WFL)
*     01.05.1989:  handle ACT__MESSAGE status; support timeout on ACT__ASTINT
*                  ACT__MESSAGE (AAOEPP::WFL)
*     14.11.1989:  remove include 'mesdefns'
*                  re-compile with MAXACTTOT = 32767 not 2**30
*                  (revised DTCOMMON) (RLVAD::AJC)
*     01.03.1990:  call DTASK_APPLIC all arguments required by ACT (and more);
*                  improve status checking, correct comments (AAOEPP::WFL)
*     02.03.1990:  restore include 'mesdefns' (defines INFINITE) (AAOEPP::WFL)
*     09.04.1991:  used passed-in PATH and MESSID for rescheduled 
*                  actions (REVAD::BDK)
*     25.04.1991:  revise INCLUDE files (REVAD::BDK)
*     30.04.1991:  revise INCLUDE files, reduce sizes of arguments to 
*                  SUBPAR_CHECKACT (REVAD::BDK)
*     01.05.1991:  remove ACTTOT, fit the TJF mod DTASK_ACT_SCHED 
*                  (REVAD::BDK)
*     03.05.1991:  cancel timers on reschedules (REVAD::BDK)
*     09.05.1991:  change order of arguments to this routine. Pass AKEY 
*                  to ADDLST, improve error trapping (REVAD::BDK)
*     10.05.1991:  close ERR and MSG whenever the transaction closes 
*                  don't bother trapping -ve values of SEQ (REVAD::BDK)
*     13.05.1991:  use ACTSHUT and COMSHUT (REVAD::BDK)
*     14.05.1991:  Remove action parameter constraint checking (ROE::BMC)
*     28.05.1991:  remove lib$cvt_dx_dx (REVAD::BDK)
*     28.05.1991:  Remove reference to NOWAIT and PARNAME (ROE::BMC)
*     07.06.1991:  change arguments to DTASK_APPLIC (REVAD::BDK)
*     10.06.1991:  sizeable rewrite, change subroutine arguments 
*                  (REVAD::BDK)
*     05.07.1991:  copy action keyword from COMMON (REVAD::BDK)
*     22.08.1991:  add REQUEST argument to DTASK_APPLIC (REVAD::BDK)
*     13.10.1992:  add INCLUDE 'PAR_PAR'
*                  use DTASK__SYSNORM instead of SS$_NORMAL (RLVAD::AJC)
*     27.07.1993:  remove unused VALID and ACTLEN (RLVAD::AJC)
*     23.08.1993:  Replace PAR_PAR with SUBPAR_SYS  (RLVAD::AJC)
*                  Replace PAR__SZNAM with SUBPAR__NAMELEN  (RLVAD::AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'ADAM_DEFNS'
      INCLUDE 'DTASK_SYS'
      INCLUDE 'DTASK_ERR'
      INCLUDE 'ACT_ERR'

*    Import :
      EXTERNAL DTASK_APPLIC      ! address of action routine
      INTEGER ACTPTR             ! index for looking-up the action
                                 ! details 
      CHARACTER*(*) VALUE        ! command line parameter string

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'DTASK_CMN'

*    Local variables :
      INTEGER SCHEDTIME              ! time in milliseconds for
                                     ! rescheduled action 
      INTEGER SEQ                    ! sequence number for stage of
                                     ! action 
      INTEGER ACODE                  ! pointer to the action in the 
                                     ! parameter system tables
      INTEGER MESSTATUS              ! status to return in
                                     ! acknowledgment 
      CHARACTER*(SUBPAR__NAMELEN) ANAME   ! action name
      CHARACTER*(SUBPAR__NAMELEN) AKEY    ! action keyword
      LOGICAL HANDLED                ! did DTASK_ACT_SCHED detect a
                                     ! reschedule action request from 
                                     ! the application
      INTEGER REQUEST                ! copy of request code returned 
                                     ! from the application
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Call the application handler.
*
      ACODE = ACTCODE(ACTPTR)
      ANAME = ACTNAME(ACTPTR)
      AKEY = ACTKEY(ACTPTR)
      SEQ = ACTSEQ(ACTPTR)

      CALL DTASK_APPLIC ( OBEY, ACODE, ANAME, ACTPTR, SEQ, VALUE,
     :  SCHEDTIME, REQUEST, STATUS ) 

      IF ( STATUS .EQ. SAI__OK ) THEN
*
*      Check for a reschedule request.
*
         CALL DTASK_ACT_SCHED ( REQUEST, ACTPTR, SEQ, SCHEDTIME,
     :     HANDLED, STATUS )

         IF ( HANDLED ) THEN
*
*         A reschedule was requested
*
            IF ( STATUS .NE. SAI__OK ) THEN
*
*            Attempted reschedule failed.
*
               MESSTATUS = STATUS
               STATUS = SAI__OK
               CALL DTASK_ACTSHUT ( ACTPATH(ACTPTR), ACTMESSID(ACTPTR),
     :           MESSTATUS, OBEY, ACTPTR, ANAME, AKEY, VALUE, STATUS )
            ENDIF

         ELSE
*
*         A reschedule was not requested.
*         Set message status depending on the application status return;
*         assume that entire action is complete if status is
*         not an expected status return.
*
             IF ( REQUEST .EQ. ACT__END ) THEN
                MESSTATUS = DTASK__ACTCOMPLETE
             ELSE IF ( REQUEST .EQ. ACT__UNIMP ) THEN
                MESSTATUS = DTASK__ACTUNIMP
             ELSE IF ( REQUEST .EQ. ACT__INFORM ) THEN
*
*             The value string contains message text for the user.
*
                MESSTATUS = DTASK__ACTINFORM
             ELSE
*
*             Return the unexpected status.
*
               IF ( REQUEST .EQ. SAI__OK ) THEN
                  MESSTATUS = DTASK__IVACTSTAT
                  CALL ERR_REP ( ' ', 
     :              'DTASK_OBEY: application returned illegal SAI__OK', 
     :              MESSTATUS )
               ELSE IF ( REQUEST .EQ. DTASK__SYSNORM ) THEN
                  MESSTATUS = DTASK__IVACTSTAT
                  CALL ERR_REP ( ' ', 
     :              'DTASK_OBEY: application returned illegal '/
     :              /'SS$_NORMAL', MESSTATUS )
               ELSE IF ( REQUEST .EQ. ACT__CANCEL ) THEN
                  MESSTATUS = DTASK__IVACTSTAT
                  CALL ERR_REP ( ' ', 
     :              'DTASK_OBEY: application returned illegal '/
     :              /'ACT__CANCEL', 
     :              MESSTATUS )
               ELSE
                  MESSTATUS = REQUEST
               ENDIF
            ENDIF
*
*         Shut down the action.
*
            STATUS = SAI__OK
            CALL DTASK_ACTSHUT ( ACTPATH(ACTPTR), ACTMESSID(ACTPTR),
     :        MESSTATUS, OBEY, ACTPTR, ANAME, AKEY, VALUE, STATUS )

         ENDIF

      ELSE
*
*      Error status returned from application
*
         MESSTATUS = STATUS
         STATUS = SAI__OK
         CALL DTASK_ACTSHUT ( ACTPATH(ACTPTR), ACTMESSID(ACTPTR),
     :     MESSTATUS, OBEY, ACTPTR, ANAME, AKEY, VALUE, STATUS )

      ENDIF

      END
