*+  AST_UEXEC - Execute a task given its command string
      SUBROUTINE AST_UEXEC( TASKRTN, COMMAND, STATUS )
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
      EXTERNAL			TASKRTN			! Task routine
      CHARACTER*(*)		COMMAND			! Command string
*
*    Status :
*
      INTEGER 			STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Start new parameter context
      CALL USI0_MARK( 'CALLABLE', STATUS )

*    Parse the command string
      CALL USI0_PARSE( COMMAND, STATUS )

*    Invoke the task
      CALL TASKRTN( STATUS )

*    Restore parameter context
      CALL USI0_RLSE( STATUS )

      END
