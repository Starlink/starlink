      SUBROUTINE PERIOD_WRITEBELL()
*+
*  Name :
*     PERIOD_DEALL
*
*  Purpose
*     Write the bell character to the screen.
*
*  Invocation :
*     CALL PERIOD_WRITEBELL()
*
*  Description :
*     This routine writes the bell character to the screen, which for
*     most operating systems, causes the speaker to issue a beep.
*
*  Arguments :
*     None.
*
*  Authors :
*     BEC: Brad Cavanagh (JAC)
*
*  History :
*     13-APR-2006 (BEC):
*        Original version.
*-
*  Type Definitions:
      IMPLICIT NONE

*  Local variables:

      CHARACTER*1 BELL

*.

*  Initialise the bell.
      BELL = CHAR(7)

*  Write the bell.
      WRITE (*, *) BELL

*  Exit routine.
      END
