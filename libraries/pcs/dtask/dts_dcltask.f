*+  DTASK_DCLTASK - ADAM top subroutine for running tasks from DCL
      SUBROUTINE DTASK_DCLTASK ( DEVINIT, DTASK_APPLIC, STATUS )
*    Description :
*     ADAM tasks use this as their main subroutine when run directly at
*     a terminal using the DCL RUN command. They can also be declared as
*     DCL foreign commands, in which case the command-line parameters 
*     are picked-up.
*    Parameters :
*     DEVINIT=EXTERNAL (given)
*           application initialisation routine
*     DTASK_APPLIC=EXTERNAL (given)
*           application calling routine
*     STATUS=INTEGER
*    Method :
*     Initialize the parameter system, and inform it that the task is 
*     running at the terminal. Get the command-line and pass it to the 
*     command-line parser. Finally, call the application code.
*     The application routines are passed as EXTERNAL to allow the DTASK 
*     library to be in a shareable image.
*    Deficiencies :
*
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     11.11.1985:  Original (REVAD::BDK)
*     25.03.1986:  change severity before signalling error (REVAD::BDK)
*     27.08.1986:  make this a subroutine rather than the main program 
*                  (REVAD::BDK)
*     17.06.1987:  pass the action name 'RUN' in a variable (REVAD::BDK)
*     30.04.1989:  receive and pass on EXTERNAL DTASK_APPLIC (AAOEPP::WFL)
*     01.05.1989:  only output significant part of value string (AAOEPP::WFL)
*     01.05.1989:  use ERR_OUT more and LIB$SIGNAL less; don't change severity
*                  of status (AAOEPP::WFL)
*     02.05.1989:  receive and call EXTERNAL DEVINIT (AAOEPP::WFL)
*     25.04.1991:  revise INCLUDE files (REVAD::BDK)
*     30.04.1991:  revise INCLUDE files and improve error checking
*                  (REVAD::BDK) 
*     23.05.1991:  ensure STATUS is bad before calling ERR (REVAD::BDK)
*     27.05.1991:  user ERR_REP (REVAD::BDK)
*     04.05.1991:  Modify comments (ROE::BMC)
*     04.05.1991:  Modify calls to ERR_REP to be more meaningful (ROE::BMC)
*     04.05.1991:  Set status OK if action has completed (ROE::BMC)
*     07.06.1991:  change comments (REVAD::BDK)
*     14.10.1992:  Get command line via DTASK_GTCMD 
*                  Get ^STATUS via DTASK_ESETK
*                  INCLUDE PAR_PAR (RLVAD::AJC)
*      2.11.1992:  Get task name returned from SUBPAR_ACTDCL (RLVAD::AJC)
*      8.03.1993:  Use MESSYS__VAL_LEN not MSG_VAL_LEN (RLVAD::AJC)
*     23.08.1993:  Replace PAR_PAR with SUBPAR_SYS  (RLVAD::AJC)
*                  Replace PAR__SZNAM with SUBPAR__NAMELEN  (RLVAD::AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'DTASK_ERR'
      INCLUDE 'MESSYS_PAR'

*    Import :
      EXTERNAL DEVINIT         ! application initialisation routine
      EXTERNAL DTASK_APPLIC    ! application calling routine

*    Status :
      INTEGER STATUS

*    Local variables :
      CHARACTER VALUE*(MESSYS__VAL_LEN)   ! command-line parameter string
      CHARACTER*(SUBPAR__NAMELEN) NAME     ! action name. This is declared as 
                                      ! a variable to allow the symbolic 
                                      ! debugger to change the action 
                                      ! name
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Initialise the parameter system and HDS.
*
      CALL SUBPAR_ACTDCL ( NAME, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
*
*      Call the device-dependent initialisation routine in case this is
*      really a Dtask being debugged
*
         CALL DEVINIT ( STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
*
*         Get the command line
*
            CALL DTASK_GTCMD( VALUE, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
*
*           NAME has been set by the call to SUBPAR_ACTDCL
               CALL DTASK_OBEYDCL ( DTASK_APPLIC, NAME, VALUE, STATUS ) 
*
*            Check for bad completion status.
*
               IF ( STATUS .NE. DTASK__ACTCOMPLETE ) THEN
                  CALL DTASK_ESETK( 'STAT', STATUS )
                  CALL ERR_REP ( ' ', 'Application exit status ^STAT',
     :              STATUS )
                  IF ( VALUE .NE. ' ' ) THEN
                     CALL MSG_SETC ( 'VALUE', VALUE )
                     CALL ERR_REP ( ' ', '^VALUE', STATUS )
                  ENDIF
               ELSE
                  STATUS = SAI__OK
               ENDIF
            ENDIF

         ELSE
            CALL DTASK_ESETK( 'STAT', STATUS )
            CALL ERR_REP ( ' ', 'DTASK_DCLTASK: DEVINIT returned bad '//
     :        'status ^STAT', STATUS )
         ENDIF
      ELSE
         CALL DTASK_ESETK( 'STAT', STATUS )
         CALL ERR_REP ( ' ',
     :     'DTASK_DCLTASK: failed to start parameter system ^STAT',
     :     STATUS )
      ENDIF

      END
