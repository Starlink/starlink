*+  DTASK_OBEYDCL - obey action in DCL task
      SUBROUTINE DTASK_OBEYDCL ( DTASK_APPLIC, NAME, VALUE, STATUS )
*    Description :
*     Carry out an OBEY. This includes handling any command-line 
*     parameters which came with the OBEY.
*    Invocation :
*     CALL DTASK_OBEYDCL ( DTASK_APPLIC, NAME, VALUE, STATUS )
*    Parameters :
*     DTASK_APPLIC=EXTERNAL (given)
*           address of action routine
*     NAME=CHARACTER*(*) (given)
*           keyword of action required
*     VALUE=CHARACTER*(*) (given)
*           command line parameter string
*     STATUS=INTEGER
*    Method :
*     Check the given action in the list of declared actions for the 
*     task.
*     If everything is ok pass the command line parameter string to the
*     parameter system
*     If all this works, call DTASK_APPLIC (which is outside the
*     shareable image).
*    Deficiencies :
*     
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     14.11.1985:  Original (REVAD::BDK)
*      9.01.1987:  New command line parser added (AAOEPP::JAB)
*     26.05.1987:  use action keyword (REVAD::BDK)
*     09.02.1988:  return bad status from CMDLINE (REVAD::BDK)
*     30.04.1989:  call DTASK_APPLIC rather than ACT and surround with
*                  TASK_PUT_CURRINFO and TASK_GET_CURRINFO calls (AAOEPP::WFL)
*     01.03.1990:  call DTASK_APPLIC all arguments required by ACT (and more);
*                  improve status checking, correct comments (AAOEPP::WFL)
*     25.04.1991:  revise INCLUDE files (REVAD::BDK)
*     30.04.1991:  revise INCLUDE files, reduce sizes of arguments to 
*                  SUBPAR_CHECKACT (REVAD::BDK)
*     14.05.1991:  Remove action parameter constraint checking (ROE::BMC)
*     28.05.1991:  remove lib$cvt_dx_dx (REVAD::BDK)
*     04.05.1991:  Remove redundant variables (ROE::BMC)
*     04.05.1991:  Update/correct comments (ROE::BMC)
*     04.05.1991:  Don't modify bad status returns from sub-routines (ROE::BMC)
*     04.05.1991:  use ERR_REP to output errors rather than VALUE (ROE::BMC)
*     07.06.1991:  remove PATH and MESSID (REVAD::BDK)
*     22.08.1991:  add REQUEST argument to DTASK_APPLIC (REVAD::BDK)
*     13.10.1992:  add INCLUDE 'PAR_PAR' 
*                  use DTASK__SYSNORM to avoid SS$NORMAL (RLVAD::AJC)
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
      EXTERNAL DTASK_APPLIC   ! address of action routine
      CHARACTER*(*) NAME      ! keyword of action required
      CHARACTER*(*) VALUE     ! command line parameter string

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'DTASK_CMN'

*    Local variables :
      INTEGER ACTPTR                 ! pointer to the action in the
                                     ! action list 
      INTEGER SCHEDTIME              ! time in milliseconds for
                                     ! rescheduled action 
      INTEGER SEQ                    ! sequence number for stage of
                                     ! action 
      INTEGER ACODE                  ! code number for the action in the 
                                     ! parameter system
      CHARACTER*(SUBPAR__NAMELEN) ANAME   ! action name
      INTEGER ACTLEN                 ! length of ANAME
      INTEGER REQUEST                ! request code from the application
*-

      IF ( STATUS .NE. SAI__OK ) RETURN
*
*   look-up the action in the parameter system
*
      CALL SUBPAR_FINDACT ( NAME, ACODE, STATUS )
      CALL SUBPAR_ACTNAME ( ACODE, ANAME, ACTLEN, STATUS )
*
*   parse the command-line
*
      CALL SUBPAR_CMDLINE ( ACODE, OBEY, VALUE, STATUS )

*   still OK so call the EXTERNAL routine that is an interface to the
*   application (arbitrarily use zero for action pointer, path and
*   message id).....
      IF ( STATUS .EQ. SAI__OK ) THEN
         ACTPTR = 0
         SEQ = 0
         CALL DTASK_APPLIC ( OBEY, ACODE, ANAME, ACTPTR, SEQ, VALUE,
     :     SCHEDTIME, REQUEST, STATUS ) 

         IF ( STATUS .EQ. SAI__OK ) THEN
*
*         Translate known application request returns. 
*
            IF ( REQUEST .EQ. ACT__END ) THEN
               STATUS = DTASK__ACTCOMPLETE
            ELSE IF ( REQUEST .EQ. ACT__UNIMP ) THEN
               STATUS = DTASK__ACTUNIMP
            ELSE IF ( REQUEST .EQ. ACT__INFORM ) THEN
*
*            The value string contains message text for the user.
*
               STATUS = DTASK__ACTINFORM
            ELSE
*
*            Report the unexpected request.
*
               IF ( REQUEST .EQ. SAI__OK ) THEN
                  STATUS = DTASK__IVACTSTAT
                  CALL ERR_REP ( ' ', 'DTASK_OBEYDCL: application '//
     :              'returned illegal SAI__OK', STATUS )
               ELSE IF ( REQUEST .EQ. DTASK__SYSNORM ) THEN
                  STATUS = DTASK__IVACTSTAT
                  CALL ERR_REP ( ' ', 'DTASK_OBEYDCL: application '//
     :              'returned illegal SS$_NORMAL', STATUS )
               ELSE IF ( REQUEST .EQ. ACT__CANCEL ) THEN
                  STATUS = DTASK__IVACTSTAT
                  CALL ERR_REP ( ' ', 'DTASK_OBEYDCL: application '// 
     :              'returned illegal ACT__CANCEL', STATUS )
               ENDIF
            ENDIF

         ENDIF

      ENDIF

      END
