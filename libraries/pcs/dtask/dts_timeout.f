*+  DTASK_TIMEOUT - interpret a message from a timer
      SUBROUTINE DTASK_TIMEOUT ( DTASK_APPLIC, VALUE, STATUS ) 
*    Description :
*     Interpret a message from a timer. If necessary, activate the 
*     application.
*    Invocation :
*     CALL DTASK_TIMEOUT ( DTASK_APPLIC, VALUE, STATUS )
*    Parameters :
*     DTASK_APPLIC=EXTERNAL (given)
*           application calling routine
*     VALUE=CHARACTER*(*) (returned)
*           command-line parameter string
*     STATUS=INTEGER
*    Method :
*     Unpack the details of the timeout from the VALUE string. If the 
*     timeout matches a currently active action, restart the OBEY.
*    Deficiencies :
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     11.06.1991: developed from Adam v1 DTASK_INPUT (REVAD::BDK)
*     13.10.1992:  add INCLUDE 'PAR_PAR' (RLVAD::AJC)
*     23.08.1993:  Replace PAR_PAR with SUBPAR_SYS  (RLVAD::AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'DTASK_SYS'
      INCLUDE 'MESSYS_ERR'

*    Import :
      EXTERNAL DTASK_APPLIC  ! application calling routine
      CHARACTER*(*) VALUE    ! command-line parameter string

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'DTASK_CMN'

*    Local variables :
      INTEGER ACTPTR               ! action pointer
      INTEGER COUNT                ! requested action count number
      CHARACTER*4 RESVAL           ! copy of timer message VALUE
      INTEGER ASTVAL               ! for unpacking RESVAL

      EQUIVALENCE ( RESVAL, ASTVAL )
*-

      IF ( STATUS .NE. SAI__OK ) RETURN 
*
*   Unpack the timer identifier from the VALUE string.
*
      RESVAL = VALUE
      ACTPTR = ASTVAL / 65536
      COUNT =  ASTVAL - ( ACTPTR * 65536 )
      IF ( ( ACTPTR .GE. 1 ) .AND. ( ACTPTR .LE. DTASK__MAXACT ) ) THEN

         IF ( ( ACTSTATE(ACTPTR) .EQ. DTASK__ACTIVE ) .AND. 
     :     ( ACTCOUNT(ACTPTR) .EQ. COUNT ) .AND. 
     :     ( ACTTIM(ACTPTR) .NE. 0 ) ) THEN
*
*         The timer is expected.
*
            CALL SUBPAR_PUTPATH ( ACTPATH(ACTPTR), ACTMESSID(ACTPTR), 
     :        STATUS )
            CALL TASK_PUT_MESSINFO ( 0, 0, ' ', VALUE, 0,
     :        MESSYS__RESCHED ) 
*
*         Restart the OBEY.
*
            CALL DTASK_OBEY ( DTASK_APPLIC, ACTPTR, VALUE, STATUS )

         ELSE
*
*         it is a reschedule from a previous incarnation;
*         throw it away.....
*
            CONTINUE
         ENDIF
      ENDIF

      END
