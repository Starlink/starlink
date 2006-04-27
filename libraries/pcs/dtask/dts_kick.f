      SUBROUTINE  DTASK_KICK ( DTASK_APPLIC, ANAME, VALUE, STATUS ) 
*+
*  Name:
*     DTASK_KICK

*  Purpose:
*     Interpret a message from this application 

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL DTASK_KICK ( DTASK_APPLIC, ANAME, VALUE, STATUS ) 

*  Description:
*     Interpret a message received from this application. If necessary, 
*     activate the application.

*  Arguments:
*     DTASK_APPLIC=EXTERNAL (given)
*           application calling routine
*     ANAME=CHARACTER*(*) (given)
*           action name in message received
*     VALUE=CHARACTER*(*) (given)
*           command-line parameter string
*     STATUS=INTEGER

*  Algorithm:
*     Check the named action is active. If it is, reactivate the OBEY.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     11-JUN-1991: original, derived from Adam v1 DTASK_INPUT
*                 (REVAD::BDK)
*     13-OCT-1992 (RLVAD::AJC):
*        Add INCLUDE 'PAR_PAR'
*     23-AUG-1993 (RLVAD::AJC):
*        Replace PAR_PAR with SUBPAR_SYS
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'DTASK_SYS'
      INCLUDE 'DTASK_ERR'
      INCLUDE 'MESSYS_ERR'

*  Arguments Given:
      EXTERNAL DTASK_APPLIC  ! application calling routine
      CHARACTER*(*) ANAME    ! action name in message received
      CHARACTER*(*) VALUE    ! command-line parameter string
*  Status:
      INTEGER STATUS
*  Global Variables:
      INCLUDE 'DTASK_CMN'
*  Local Variables:
      INTEGER ACTPTR               ! action pointer
*.

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
     :     MESSYS__KICK )
         CALL DTASK_OBEY ( DTASK_APPLIC, ACTPTR, VALUE, STATUS )
      ELSE
*
*      The action must have been cancelled
*      Throw it away.
*
         STATUS = SAI__OK

      ENDIF

      END
