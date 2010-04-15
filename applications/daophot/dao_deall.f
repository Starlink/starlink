      SUBROUTINE DAO_DEALL( PNTR )
*+
*  Name :
*     DAO_DEALL
*
*  Purpose
*     Deallocate temporary storage.
*
*  Invocation :
*     CALL DAO_ALLOC( PNTR )
*
*  Description :
*     This routine deallocates temporary storage which has previously
*     been allocated using DAO_ALLOC.
*     In the event of an error it will print a warning message in the
*     usual DAOPHOT way and return normally.
*
*  Arguments :
*     PNTR = INTEGER  (Given)
*        Pointer to storage returned by DAO_ALLOC.
*
*  Authors :
*     MBT: Mark Taylor (STARLINK)
*
*  History :
*     7-JUN-2000 (MBT):
*        Original version.
*-
*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER PNTR

*  Local variables :
      INTEGER STATUS            ! Starlink error status

*.

*  Initialise the status variable.
      STATUS = SAI__OK

*  Get a locator for a temporary object.
*  Deallocate the memory.
      CALL PSX_FREE( PNTR, STATUS )

*  Deal with errors.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL STUPID( 'Error in memory deallocation' )
         CALL ERR_ANNUL( STATUS )
      END IF

*   Exit routine.
      END
