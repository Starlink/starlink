*+  DTASK_ASTHDLR - AST handler for timed reschedules
      SUBROUTINE DTASK_ASTHDLR ( ASTPARM )
*    Description :
*     Signals that a timer has completed indicating that an action is 
*     due for rescheduling.
*    Invocation :
*     Invoked by timer completion
*     interrupts.
*    Parameters :
*     ASTPARM=INTEGER (given)
*           packed action number plus action counter passed by value
*    Method :
*     Interpret the AST parameter. This has been passed by VALUE, which 
*     is necessary to ensure it is unique to this timer event. The 
*     parameter contains two two-byte integers.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     John Cooke (REVS::JAC) 22May84
*    History :
*     date:  changes (institution::username)
*     22-MAY-1984  first insertion (REVA::ADAM])
*     25-MAY-1984  astparm access mode to %loc (REVA::ADAM])
*     20-JUN-1984  two 16-bit parameters; new error system (REVA::ADAM)
*     20-JUN-1984  added save of ast parm itself for reference (REVA::ADAM)
*     21-JUN-1984  added interrupt flag (REVA::ADAM)
*     25.04.1991:  rearrange INCLUDE files and add comments (REVAD::BDK)
*     30.04.1991:  revise INCLUDE files, remove REQASTPAR (REVAD::BDK)
*     13.05.1991:  dont disable ASTs, use MESSYS_RESMSG (REVAD::BDK)
*     07.06.1991:  change comments (REVAD::BDK)
*     28.06.1994:  Version for Unix Version 2 timer system (RAL::AJC)
*     11.06.2001:  Call AMS (FAMS) directly (AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER ASTPARM           ! the VMS AST parameter passed by value
*    Local variables :
      INTEGER ASTVAL            ! value of ASTPARM
      CHARACTER*4 VALUE         ! message sent to application
      INTEGER LENGTH            ! length of message sent
      INTEGER STATUS            ! local status
*    Data structures for ADAM:
      EQUIVALENCE ( ASTVAL, VALUE )
*-

*
*   Copy the timer identifier from the AST parameter.
*
      ASTVAL = ASTPARM
*
*   Inform the message system.
*
      STATUS = SAI__OK
      LENGTH = 4
      CALL FAMS_RESMSG( LENGTH, VALUE, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'DTASK_ASTHDLR1',
     :   'AST handler failed to send timed reschedule message', STATUS )
      ENDIF

      END
