      SUBROUTINE PERIOD_DEALL( PNTR )
*+
*  Name :
*     PERIOD_DEALL
*
*  Purpose
*     Deallocate temporary storage.
*
*  Invocation :
*     CALL PEWRIOD_DEALL( PNTR )
*
*  Description :
*     This routine deallocates temporary storage which has previously
*     been allocated using PERIOD_ALLOC.
*     In the event of an error it will print a warning message in the
*     usual PERIOD way and return normally.
*
*  Arguments :
*     PNTR = INTEGER  (Given)
*        Pointer to storage returned by PERIOD_ALLOC.
*
*  Authors :
*     MBT: Mark Taylor (STARLINK) - originally for DAOPHOT
*
*  History :
*     7-JUN-2000 (MBT):
*        Original version.
*     14-Aug-2001 (KPD) version adapted for PERIOD
*     13-APR-2006 (BEC):
*        Replace direct BELL write with call to PERIOD_WRITEBELL.
*-
*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      INTEGER PNTR

*  Local variables :
      INTEGER STATUS


*.

*  Initialise the status variable.
      STATUS = 0

*  Deallocate memory.
      CALL PSX_FREE( PNTR, STATUS )

*  Deal with errors.
      IF ( STATUS .NE. 0 ) THEN
         CALL PERIOD_WRITEBELL()
         WRITE (*, *) '** WARNING: Failure to deallocate ' //
     :                'dynamically allocated memory'
      END IF

*   Exit routine.
      END
