*-----------------------------------------------------------------------
*+  IDTCON - Indicate if time has expired

      LOGICAL FUNCTION IDTCON

*    Description :
*     This function returns false if the timer has expired, otherwise
*     it returns true.
*
*    Invocation :
*     CALL IDTCON
*
*    Method :
*     <description of how the subroutine works>
*
*    Deficiencies :
*     Call VAX routine - LIB$STAT_TIMER
*
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     November 1989
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'

*    External references :
*     <declarations for external function references>
*    Global variables :
      INCLUDE 'IDIINC(IKN_COMTIM)'

*    Local Constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      INTEGER NSEC( 2 )

      REAL ETIME

*    Internal References :
*     <declarations for internal functions>
*    Local data :
*     <any DATA initialisations for local variables>
*-

*   Get the current elapse time
*   Note. It is returned as a negative number
      CALL LIB$STAT_TIMER( 1, NSEC )
      ETIME = -REAL( NSEC( 1 ) ) / 1.0E7

*   Indicate if the time has elapsed
      IF ( ETIME .GT. TDELAY ) THEN
         IDTCON = .FALSE.
      ELSE
         IDTCON = .TRUE.
      ENDIF

      END

