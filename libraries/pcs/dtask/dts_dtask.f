*+  DTASK_DTASK - Top subroutine for tasks run as separate processes
      SUBROUTINE DTASK_DTASK ( DEVINIT, DTASK_APPLIC, STATUS )
*    Description :
*     Initialise for this mode of operation, then wait for incoming 
*     messages. When one is received, identify the CONTEXT (GET, SET, 
*     OBEY, CANCEL or CONTROL) and pass the message information to the 
*     corresponding routine.
*     If at all possible, recover from any error conditions and revert 
*     to waiting for more messages.
*    Method :
*     Find the name of the process running the program, and use the name
*     while initialising into the ADAM message and parameter systems.
*     Then loop receiving instructions and acting on them.
*     Receipt of a GET/SET/OBEY/CANCEL/CONTROL command starts a new 
*     "transaction", identified by MESSID. The transaction and its 
*     associated communications are closed-down when this task returns 
*     the final message for the GET/SET/OBEY/CANCEL/CONTROL.
*    Deficiencies :
*
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     John Cooke (REVS::JAC) 01May84
*     BDK: B.D.Kelly (ROE)
*     WFL: W.F.Lupton (AAO)
*     AJC: A.J.Chipperfield (Starlink, RAL)
*     BKM: B.K.McIlwrath (Starlink, RAL)
*
*    History :
*     08May84:  added initial/final acknowledgment for OBEY
*     17May84:  dtask rescheduling started
*     19Jun84:  general rewrite started
*     26Oct84:  add full parameter system (REVAD::BDK)
*     04Dec84:  report error on exit (REVAD::BDK)
*     11Dec84:  report G/S/O/C errors without exiting (REVAD::BDK)
*     11Dec84:  use DTASK_ACKNOW (REVAD::BDK)
*     20.06.1985:  report bad status on message received without exiting
*                       (REVAD::BDK)
*     25.03.1986: change severity before signalling errors (REVAD::BDK)
*     22.08.1986: report failure in ACKNOW without exiting (REVAD::BDK)
*     27.08.1986: make into subroutine instead of main program 
*                 (REVAD::BDK)
*     27.08.1986:  moved illcontext from adam__ to dtask__ (REVA::ADAM)
*     27.08.1986:  forgot to include dterrs! (REVA::ADAM)
*     30.04.1989:  enable and disable reporting (AAOEPP::WFL)
*     30.04.1989:  receive and pass down EXTERNAL DTASK_APPLIC (AAOEPP::WFL)
*     01.05.1989:  use ERR_OUT rather than LIB$SIGNAL and TYPE (AAOEPP::WFL)
*     01.05.1989:  don't change severity of failure status (AAOEPP::WFL)
*     02.05.1989:  receive and call EXTERNAL DEVINIT (AAOEPP::WFL)
*     01.03.1990:  initialise VALUE, remove DONE, remove MSG_START and
*                  ERR_START calls and localise area where MSG and ERR
*                  routines can work, correct comments (AAOEPP::WFL)
*     25.04.1991:  revise INCLUDE files (REVAD::BDK)
*     30.04.1991:  use MESSYS__TNAME (REVAD::BDK)
*     09.05.1991:  tidy error reporting (REVAD::BDK)
*     09.05.1991:  change order of arguments to OBEY and CANCEL (REVAD::BDK) 
*     27.05.1991:  use ERR_REP, don't call ERR_ and MSG_STOP (REVAD::BDK)
*     04.06.1991:  Remove redundant variables (ROE::BMC)
*     04.06.1991:  Update/correct comments (ROE::BMC)
*     04.06.1991:  Use MYNAME length obtained instead of MSG_SETC (ROE::BMC)
*     04.06.1991:  Call ERR_CLEAR before DTASK_ACKNOW as the latter will
*                  terminate communications on this MESSID (ROE::BMC)
*     04.06.1991:  Use DTASK_COMSHUT (ROE::BMC)
*     07.06.1991:  move error reporting at end of task inside the DO 
*                  loop. Change comments, change AKEY to NAME
*                  (REVAD::BDK) 
*     10.06.1991:  rewrite message receiving and handling section 
*                  (REVAD::BDK)
*     07.04.1992:  Add CONTROL context to prologue and comments (RLVAD::BKM)
*     15.07.1992:  correct and split end messages (RLVAD::AJC)
*     14.10.1992:  Mods for portability
*                  Get ^STATUS via DTASK_ESETK
*                  INCLUDE PAR_PAR (RLVAD::AJC)
*     16.11.1992:  Remove unused declarations ISTAT and BADNAME (RLVAD::AJC)
*      8.03.1993:  Use MESSYS__VAL_LEN - remove include DDMSG (RLVAD::AJC)
*     23.08.1993:  Replace PAR_PAR with SUBPAR_SYS  (RLVAD::AJC)
*                  Replace PAR__SZNAM with SUBPAR__NAMELEN  (RLVAD::AJC)
*     22.09.1993:  Prevent attempts to use MESSYS after MSP errors (RLVAD::BKM)
*     25.09.1995:  Increase size of NAME to allow ACTNAME:PARNAME (RLVAD:AJC)
*     31.07.1995:    Add TEMPORARY call to DTASK_DECBUG to correct a compiler bug
*                  on Alpha/OSF Fortran. (BKM)
*     11.06.2001:  Call AMS_RECEIVE (FAMS) directly
*                  ADAM_PRCNAM now DTASK_PRCNAM (AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'MESSYS_PAR'
      INCLUDE 'MESSYS_ERR'
      INCLUDE 'ADAM_DEFNS'
      INCLUDE 'DTASK_ERR'

*    Import :
      EXTERNAL DEVINIT        ! application initialisation routine
      EXTERNAL DTASK_APPLIC   ! application calling routine

*    Status :
      INTEGER STATUS

*    Local variables :
      INTEGER MSGSTATUS                 ! status of received message
      INTEGER CONTEXT                   ! GET, SET, OBEY, CANCEL or CONTROL
      INTEGER MSGLEN                    ! Length of message value
      CHARACTER*( 2*SUBPAR__NAMELEN + 1 ) NAME       
                                        ! action keyword for OBEY, CANCEL and 
                                        ! CONTROL, parameter keyword for 
                                        ! GET and SET
      CHARACTER*(MESSYS__VAL_LEN) VALUE     ! command line parameter string
      INTEGER PATH                      ! path to commanding process
      INTEGER MESSID                    ! identifier for transaction
      CHARACTER*(MESSYS__TNAME) MYNAME  ! name of this task
      INTEGER NLENGTH                   ! actual length of MYNAME
*-

      IF ( STATUS .NE. SAI__OK ) RETURN
*
*   Initialise VALUE in case DEVINIT fails.
*
      VALUE = ' '
*
*   Obtain process name.
*
      CALL DTASK_PRCNAM ( MYNAME, NLENGTH, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP ( ' ', 'DTASK_DTASK: failed to get process name,'
     :    , STATUS )
         CALL DTASK_ESETK ( 'STAT', STATUS )
         CALL ERR_REP ( ' ', '^STAT', STATUS )
      ELSE
*
*      Initialise intertask communication (MESSYS), parameter system 
*      (SUBPAR) and tasking support library (TASK)
*
         CALL DTASK_INIT ( MYNAME, NLENGTH, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP ( ' ', MYNAME(1:NLENGTH) //
     :        ' task initialisation failed,', STATUS )
            CALL DTASK_ESETK ( 'STAT', STATUS )
            CALL ERR_REP ( ' ', '^STAT', STATUS )
         ELSE
*
*         Call the device-dependent initialisation routine.
*
            CALL DEVINIT ( STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP ( ' ', MYNAME(1:NLENGTH) //
     :           ' application initialisation failed,', STATUS )
               CALL DTASK_ESETK ( 'STAT', STATUS )
               CALL ERR_REP ( ' ', '^STAT', STATUS )
            ENDIF
         ENDIF
      ENDIF
*
*   Loop to receive and reply to messages.
*   The task is "never" expected to come out of this DO loop.
*   It is assumed the task is finally removed by being killed.
*
      DO WHILE ( STATUS .EQ. SAI__OK )
*
*      Switch error reporting to the terminal and error file
*
         CALL SUBPAR_PUTPATH ( 0, 0, STATUS )
*
*      Wait for a message
*
         MSGSTATUS = SAI__OK
         CALL FAMS_RECEIVE( MESSYS__INFINITE, MSGSTATUS, CONTEXT,
     :     NAME, MSGLEN, VALUE, PATH, MESSID, STATUS )
         IF( MSGLEN .LT. MESSYS__VAL_LEN )
     :     VALUE( MSGLEN+1: ) = ' '

         IF ( STATUS .NE. SAI__OK ) THEN
            CONTINUE

         ELSE IF ( MSGSTATUS .EQ. SAI__OK ) THEN
*
*         Incoming GET/SET/OBEY/CANCEL/CONTROL
*
            CALL DTASK_GSOC ( DTASK_APPLIC, PATH, MESSID, CONTEXT, NAME,
     :        VALUE, STATUS ) 
* TEMPORARY call (and routine) to fix a compiler bug on Alpha/OSF (31/07/95)
            CALL DTASK_DECBUG
         ELSE IF ( MSGSTATUS .EQ. MESSYS__EXTINT )  THEN
*
*         Should not happen in a task.
*
            CALL ERR_REP ( ' ', 'Invalid event EXTINT has occurred',
     :        MSGSTATUS )

         ELSE IF ( MSGSTATUS .EQ. MESSYS__RESCHED ) THEN
*
*         A timer has expired
*
            CALL DTASK_TIMEOUT ( DTASK_APPLIC, VALUE, STATUS ) 

         ELSE IF ( MSGSTATUS .EQ. MESSYS__ASTINT ) THEN
*
*         A message has arrived from an AST routine
*
            CALL DTASK_ASTINT ( DTASK_APPLIC, NAME, VALUE, STATUS ) 

         ELSE IF ( MSGSTATUS .EQ. MESSYS__KICK ) THEN
*
*         A message has come from this application
*
            CALL DTASK_KICK ( DTASK_APPLIC, NAME, VALUE, STATUS ) 


*         On Unix MSP errors are negative and may be reported in MSGSTATUS
         ELSE IF ( MSGSTATUS .GT. 0) THEN
*
*         Check if message is from a subsidiary task
*
            CALL DTASK_SUBSID ( DTASK_APPLIC, PATH, MESSID, CONTEXT,
     :        NAME, MSGSTATUS, VALUE, STATUS ) 

         ELSE
            STATUS = MSGSTATUS

         ENDIF

         IF ( STATUS .NE. SAI__OK ) THEN
*
*          Unrecoverable error, the task will exit.
*
            IF ( VALUE .EQ. ' ' ) THEN
               CALL ERR_REP ( ' ', MYNAME(1:NLENGTH) //
     :           ' unexpected failure,', STATUS )
               CALL DTASK_ESETK ( 'STAT', STATUS )
               CALL ERR_REP ( ' ', '^STAT', STATUS )
            ELSE
               CALL MSG_SETC ( 'VALUE', VALUE )
               CALL ERR_REP ( ' ', MYNAME(1:NLENGTH) //
     :           ' unexpected failure,', STATUS )
               CALL DTASK_ESETK ( 'STAT', STATUS )
               CALL ERR_REP ( ' ', '^STAT', STATUS )
               CALL MSG_SETC ( 'VALUE', VALUE )
               CALL ERR_REP ( ' ', 'VALUE: ^VALUE', STATUS )
            ENDIF
         ENDIF

      ENDDO

      END
