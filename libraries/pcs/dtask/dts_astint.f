*+  DTASK_ASTINT - interpret a message from an AST routine
      SUBROUTINE DTASK_ASTINT ( DTASK_APPLIC, ANAME, VALUE, STATUS ) 
*    Description :
*     Interpret a message received from an AST routine. If necessary, 
*     activate the application.
*    Invocation :
*     CALL DTASK_ASTINT ( DTASK_APPLIC, ANAME, VALUE, STATUS ) 
*    Parameters :
*     DTASK_APPLIC=EXTERNAL (given)
*           application calling routine
*     ANAME=CHARACTER*(*) (given)
*           action name in message received
*     VALUE=CHARACTER*(*) (given)
*           command-line parameter string
*     STATUS=INTEGER
*    Method :
*     Check the named action is active. If it is, reactivate the OBEY.
*    Deficiencies :
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     11.06.1991: original, derived from Adam v1 DTASK_INPUT 
*                 (REVAD::BDK)
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
      INCLUDE 'MESSYS_ERR'

*    Import :
      EXTERNAL DTASK_APPLIC  ! application calling routine
      CHARACTER*(*) ANAME    ! action name in message received
      CHARACTER*(*) VALUE    ! command-line parameter string
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'DTASK_CMN'
*    Local variables :
      INTEGER ACTPTR               ! action pointer
*-

      IF ( STATUS .NE. SAI__OK ) RETURN 
*
*   The name carried in the message is the action name.
*
      STATUS = SAI__OK
      CALL DTASK_SRCHLST ( ANAME, ACTPTR, STATUS )

      IF ( STATUS .EQ. DTASK__ACTACTIVE ) THEN
*
*      Action is still there, enable communications.
*
         STATUS = SAI__OK
         CALL SUBPAR_PUTPATH ( ACTPATH(ACTPTR), ACTMESSID(ACTPTR),
     :     STATUS )
         CALL TASK_PUT_MESSINFO ( 0, 0, ' ', VALUE, 0, 
     :     MESSYS__ASTINT )
         CALL DTASK_OBEY ( DTASK_APPLIC, ACTPTR, VALUE, STATUS )
      ELSE
*
*      The action must have been cancelled
*      Throw it away.
*
         STATUS = SAI__OK

      ENDIF

      END
