*+  AST_EXEC - Execute an ASTERIX command
      SUBROUTINE AST_EXEC( COMMAND, STATUS )
*    Description :
*     <description of what the subroutine does - for user info>
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*
*     David J. Allan (JET-X, University of Birmingham)
*
*    History :
*
*     21 May 93 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      CHARACTER*(*)		COMMAND			! Command string
*
*    Status :
*
      INTEGER 			STATUS
*
*    Local variables :
*
      CHARACTER*80		TASK

      INTEGER			IC

      LOGICAL			DONE
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise
      DONE = .FALSE.

*    Extract the command name
      IC = 1
      CALL CHR_FIWE( COMMAND, IC, STATUS )
      TASK = COMMAND(1:IC)
      CALL CHR_UCASE( TASK(1:IC) )

*    Try each monolith in turn. DONE means we found a routine, STATUS = -1
*    means it was a dummy.
      CALL CASW_GRF( TASK(1:IC), COMMAND, DONE, STATUS )
      CALL CASW_IMG( TASK(1:IC), COMMAND, DONE, STATUS )
      CALL CASW_HED( TASK(1:IC), COMMAND, DONE, STATUS )
      CALL CASW_UTIL( TASK(1:IC), COMMAND, DONE, STATUS )
      CALL CASW_TIM( TASK(1:IC), COMMAND, DONE, STATUS )
      CALL CASW_SRC( TASK(1:IC), COMMAND, DONE, STATUS )
      CALL CASW_SPEC( TASK(1:IC), COMMAND, DONE, STATUS )
      CALL CASW_TIM( TASK(1:IC), COMMAND, DONE, STATUS )
      CALL CASW_MENU( TASK(1:IC), COMMAND, DONE, STATUS )
      CALL CASW_WFC( TASK(1:IC), COMMAND, DONE, STATUS )
      CALL CASW_XRT( TASK(1:IC), COMMAND, DONE, STATUS )

*    Did we execute anything?
      IF ( DONE .AND. (STATUS.EQ.-1) ) THEN
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'TASK', TASK )
        CALL ERR_REP( ' ', 'AST_EXEC - task ^TASK has not been linked'/
     :                /' to this executable', STATUS )
      ELSE IF ( .NOT. DONE ) THEN
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'TASK', TASK )
        CALL ERR_REP( ' ', 'AST_EXEC - ^TASK is not a recognised'/
     :                /' ASTERIX task name', STATUS )
      END IF

      END
