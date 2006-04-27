      SUBROUTINE DTASK_TIMEOUT ( DTASK_APPLIC, VALUE, STATUS ) 
*+
*  Name:
*     DTASK_TIMEOUT

*  Purpose:
*     Interpret a message from a timer

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL DTASK_TIMEOUT ( DTASK_APPLIC, VALUE, STATUS )

*  Description:
*     Interpret a message from a timer. If necessary, activate the 
*     application.

*  Arguments:
*     DTASK_APPLIC=EXTERNAL (given)
*           application calling routine
*     VALUE=CHARACTER*(*) (returned)
*           command-line parameter string
*     STATUS=INTEGER

*  Algorithm:
*     Unpack the details of the timeout from the VALUE string. If the 
*     timeout matches a currently active action, restart the OBEY.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     11-JUN-1991 (REVAD::BDK):
*        Developed from Adam v1 DTASK_INPUT
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
      INCLUDE 'MESSYS_ERR'

*  Arguments Given:
      EXTERNAL DTASK_APPLIC  ! application calling routine
      CHARACTER*(*) VALUE    ! command-line parameter string

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'DTASK_CMN'

*  Local Variables:
      INTEGER ACTPTR               ! action pointer
      INTEGER COUNT                ! requested action count number
      CHARACTER*4 RESVAL           ! copy of timer message VALUE
      INTEGER ASTVAL               ! for unpacking RESVAL

      EQUIVALENCE ( RESVAL, ASTVAL )
*.

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
