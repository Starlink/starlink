*-----------------------------------------------------------------------
*+  IDTBEG - Begin timer

      SUBROUTINE IDTBEG ( DTIME )

*    Description :
*     This starts the timer.
*
*    Invocation :
*     CALL IDTBEG ( DTIME )
*
*    Method :
*     <description of how the subroutine works>
*
*    Deficiencies :
*     VAX specific routine - LIB$INIT_TIMER, LIB$STAT_TIMER
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

*    Import :
*     Wait time
      REAL DTIME

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMTIM)'

*    Local Constants :
*     <local constants defined by PARAMETER>
*    Local variables :
*     <local variables> :
*    Internal References :
*     <declarations for internal functions>
*    Local data :
*     <any DATA initialisations for local variables>
*-

*   Set the timer flag to zero
      TONOFF = 0

*   Copy the requested time into the common block
      TDELAY = DTIME

*   Set up the timer
      CALL LIB$INIT_TIMER

      END

