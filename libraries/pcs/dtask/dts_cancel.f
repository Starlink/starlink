*+  DTASK_CANCEL - cancel an action if active
      SUBROUTINE DTASK_CANCEL ( DTASK_APPLIC, PATH, MESSID, ACTPTR,
     :  VALUE, STATUS )
*    Description :
*     Tell the application that a request has been received to cancel an 
*     action which is currently waiting to be rescheduled.
*    Invocation :
*     CALL DTASK_CANCEL ( DTASK_APPLIC, PATH, MESSID, ACTPTR,
*    :  VALUE, STATUS )
*    Parameters :
*     DTASK_APPLIC=EXTERNAL (given)
*           address of action routine
*     PATH=INTEGER (given)
*           message path needed for reply
*     MESSID=INTEGER (given)
*           transaction number
*     ACTPTR=INTEGER (given)
*           action pointer
*     VALUE=CHARACTER*(*) (given and returned)
*           command line parameter string
*     STATUS=INTEGER
*    Method :
*     Check the named action is in progress.
*     Give the command-line parameter string to the parameter system.
*     Call DTASK_APPLIC to inform the application code what has 
*     happened.
*     Check the status returned from the application to see whether the 
*     action is to be terminated or whether it is to continue
*     rescheduling. 
*     In any case, send an acknowledgement to the task which requested
*     the CANCEL. 
*     Send an acknowledgement to the task which issued the obey if the 
*     action has ended.
*    Deficiencies :
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors 
*     John Cooke (REVS::JAC) 22May84
*    History :
*     22-MAY-1984  first insertion ( stub ) (REVA::ADAM])
*     25-MAY-1984  now implemented (REVA::ADAM])
*     25-MAY-1984  %val on actptr in cantim (REVA::ADAM])
*     25-MAY-1984  remove %val on actptr ! (REVA::ADAM])
*     25-MAY-1984  replace %val again for tests (REVA::ADAM])
*     20-JUN-1984  changed error symbol names (REVA::ADAM)
*     25-JUN-1984  added AST interrupts (REVA::ADAM)
*     02-OCT-1984  handle command-line parameters (REVAD::BDK)
*     16-NOV-1984  new version for parameter system (REVA::ADAM)
*     16-NOV-1984  handle CANCONSTR status (REVA::ADAM)
*     24-NOV-1984  use DTASK_ACKNOW (BDK) (REVA::ADAM)
*     16.04.1985:  send two acknowledgements if needed (REVAD::BDK)
*     21.06.1985:  return name of parameter violating constraints 
*                      (REVAD::BDK)
*     21.03.1986:  remove special handling of reschedules and ASTs
*                  and pass SEQ to ACT (REVAD::BDK)
*     25.03.1986:  don't send 2nd acknowledgement if action not active
*                      (REVAD::BDK)
*     22.01.1987:  New command line parser added (AAOEPP::JAB)
*     26.05.1987:  use action keyword (REVAD::BDK)
*     30.04.1989:  call DTASK_APPLIC rather than ACT and surround with
*                  TASK_PUT_CURRINFO and TASK_GET_CURRINFO calls (AAOEPP::WFL)
*     30.04.1989:  call TASK_CLEAR_MESSINFO on action completion (AAOEPP::WFL)
*     01.05.1989:  check for ACT__MESSAGE status returned (AAOEPP::WFL)
*     01.03.1990:  call DTASK_APPLIC all arguments required by ACT (and more);
*                  improve status checking, correct comments (AAOEPP::WFL)
*     09.04.1991:  always send two acknowledgements if the action was 
*                  actually cancelled, two ensure that both transactions 
*                  are closed-down (REVAD::BDK)
*     25.04.1991:  revise INCLUDE files (REVAD::BDK)
*     30.04.1991:  revise INCLUDE files, reduce sizes of arguments to 
*                  SUBPAR_CHECKACT (REVAD::BDK)
*     02.05.1991:  fit the TJF mod DTASK_ACT_SCHED (REVAD::BDK)
*     03.05.1991:  cancel timer if necessary (REVAD::BDK)
*     09.05.1991:  VALUE is given and returned, change order of 
*                  arguments (REVAD::BDK)
*     14.05.1991:  Remove action parameter constraint checking (ROE::BMC)
*     27.05.1991:  user ERR_REP and DTASK_COMSHUT (REVAD::BDK)
*     07.06.1991:  change arguments to DTASK_APPLIC, add extra error 
*                  reporting and change comments (REVAD::BDK)
*     11.06.1991:  change call arguments, assume the parameter system 
*                  has been set-up already (REVAD::BDK)
*     23.07.1991:  get action keyword from COMMON (REVAD::BDK)
*     22.08.1991:  add REQUEST argument to DTASK_APPLIC (REVAD::BDK)
*     25.11.1991:  use ADAM_ACKNOW (REVAD::BDK)
*     13.10.1992:  add INCLUDE 'PAR_PAR' 
*                  Get ^STATUS via DTASK_ESETK (RLVAD::AJC)
*     27.07.1993:  Remove unused ACTLEN (RLVAD::AJC)
*     23.08.1993:  Replace PAR_PAR with SUBPAR_SYS  (RLVAD::AJC)
*                  Replace PAR__SZNAM with SUBPAR__NAMELEN  (RLVAD::AJC)
*     11.06.2001:  Call AMS_REPLY (FAMS) directly (AJC):
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'ADAM_DEFNS'
      INCLUDE 'DTASK_SYS'
      INCLUDE 'DTASK_ERR'
      INCLUDE 'MESSYS_PAR'
      INCLUDE 'ACT_ERR'

*    Import :
      EXTERNAL DTASK_APPLIC  ! address of action routine
      INTEGER PATH           ! message path needed for reply
      INTEGER MESSID         ! transaction number
      INTEGER ACTPTR         ! action pointer

*    Import-export :
      CHARACTER*(*) VALUE    ! command line parameter string

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'DTASK_CMN'

*    Local variables :
      INTEGER SEQ                     ! sequence number for stage of
                                      ! action 
      INTEGER SCHEDTIME               ! requested reschedule time in 
                                      ! millisec
      INTEGER ACODE                   ! parameter system code number for
                                      ! the action 
      INTEGER MESSTATUS               ! status returned in acknowledgment
      INTEGER MESLEN                  ! length of VALUE
      INTEGER PATHOB                  ! path to task issuing original OBEY
      INTEGER MESSIDOB                ! messid of original OBEY
      CHARACTER*(SUBPAR__NAMELEN) ANAME    ! action name
      CHARACTER*(SUBPAR__NAMELEN) AKEY     ! action keyword
      LOGICAL HANDLED                 ! did DTASK_ACT_SCHED handle the 
                                      ! reschedule
      INTEGER REQUEST                 ! status signalling reschedule 
                                      ! type requested
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Extract the details of the OBEY transaction
*
      PATHOB = ACTPATH(ACTPTR)
      MESSIDOB = ACTMESSID(ACTPTR)
      SEQ = ACTSEQ(ACTPTR)
      ACODE = ACTCODE(ACTPTR)
      ANAME = ACTNAME(ACTPTR)
      AKEY = ACTKEY(ACTPTR)
*
*   Call the application.
*
      CALL DTASK_APPLIC ( CANCEL, ACODE, ANAME, ACTPTR, SEQ,
     :  VALUE, SCHEDTIME, REQUEST, STATUS ) 

      IF ( STATUS .EQ. SAI__OK ) THEN
*
*      Check for a reschedule request
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
*            Remove any outstanding timer
*            Clear list of active actions in subsidiary tasks.
*            Remove entry from the action list.
*
               MESSTATUS = STATUS
               CALL ERR_REP ( ' ',
     :           'failed to reschedule on receipt of cancel ' // 
     :           AKEY, STATUS )
               CALL DTASK_ESETK ( 'STAT', STATUS )
               CALL ERR_REP ( ' ', '^STAT', STATUS )
               STATUS = SAI__OK
               CALL DTASK_CANTIM ( ACTPTR, STATUS )
               CALL TASK_CLEAR_MESSINFO ( ACTPTR, STATUS )
               CALL DTASK_REMLST ( ANAME, STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL DTASK_ESETK ( 'STAT', STATUS )
                  CALL ERR_REP ( ' ', 'DTASK_CANCEL: ^STAT', 
     :              STATUS ) 
               ENDIF
*
*            Close down the original OBEY transaction.
*
               STATUS = SAI__OK
               MESLEN = MIN( LEN(VALUE), MESSYS__VAL_LEN )
               CALL FAMS_REPLY( PATHOB, MESSIDOB, MESSYS__MESSAGE,
     :           MESSTATUS, CANCEL, AKEY, MESLEN, VALUE, STATUS )

            ELSE
*
*         The action has been rescheduled to complete the cancellation. 
*         Tell the process which issued the cancel that everything is ok. 
*
               MESSTATUS = DTASK__ACTCANCEL

            ENDIF

         ELSE IF ( REQUEST .EQ. ACT__CANCEL ) THEN
*
*         No more rescheduling required.
*         Remove any outstanding timer
*         Clear list of active actions in subsidiary tasks.
*         Remove entry from the action list.
*
            CALL DTASK_CANTIM ( ACTPTR, STATUS )
            CALL TASK_CLEAR_MESSINFO ( ACTPTR, STATUS )
            CALL DTASK_REMLST ( ANAME, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               MESSTATUS = DTASK__ACTCANCEL
            ELSE
               MESSTATUS = STATUS
            ENDIF
*
*         Close down the original OBEY transaction.
*
            STATUS = SAI__OK
               MESLEN = MIN( LEN(VALUE), MESSYS__VAL_LEN )
               CALL FAMS_REPLY( PATHOB, MESSIDOB, MESSYS__MESSAGE,
     :           MESSTATUS, CANCEL, AKEY, MESLEN, VALUE, STATUS )

         ELSE IF ( REQUEST .EQ. ACT__END ) THEN
*
*         Invalid status.
*
            CALL ERR_REP ( ' ', 
     :        'the application returned ACT__END request', REQUEST )
            CALL ERR_REP ( ' ', 
     :        'this is invalid in response to a CANCEL command',
     :        REQUEST ) 

            MESSTATUS = DTASK__ACTNOTCANCEL

         ELSE
*
*         Invalid request returned from application
*
            IF ( REQUEST .EQ. SAI__OK ) THEN
               MESSTATUS = DTASK__ACTNOTCANCEL
            ELSE
               MESSTATUS = REQUEST
            ENDIF
            CALL ERR_REP ( ' ',
     :        'application returned invalid request', MESSTATUS )

         ENDIF

      ELSE
*
*      Error returned from application
*
         CALL ERR_REP ( ' ', 'CANCEL failed with bad status', STATUS )
         MESSTATUS = STATUS
      ENDIF
*
*   Close down the CANCEL transaction.
*
      STATUS = SAI__OK
      CALL DTASK_COMSHUT ( PATH, MESSID, MESSTATUS, CANCEL, AKEY, 
     :  VALUE, STATUS )

      END
