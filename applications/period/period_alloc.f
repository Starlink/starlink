      SUBROUTINE PERIOD_ALLOC( TYPE, SIZE, PNTR )
*+
*  Name :
*     PERIOD_ALLOC
*
*  Purpose
*     Allocate temporary storage.
*
*  Invocation :
*     CALL PERIOD_ALLOC( TYPE, SIZE, PNTR )
*
*  Description :
*     This routine allocates temporary storage for use by a PERIOD
*     routine.  Storage allocated in this way should be freed by a
*     call to PERIOD_DEALL.
*     In the event of an error it will abort the PERIOD program in
*
*  Arguments :
*     TYPE = CHARACTER * ( * ) (Given)
*        HDS type of the storage to be allocated.
*     SIZE = INTEGER (Given)
*        Number of units of type TYPE to be allocated.
*     PNTR = INTEGER (Returned)
*        Pointer to the mapped workspace.
*
*  Notes :
*     This routine currently makes use of PSX_CALLOC to allocate memory
*     from the heap.  Historically, PERIOD has manipulated images in
*     memory so this is likely to be adequate.  However, if it became
*     necessary to use much more storage, this routine and PERIOD_DEALL
*     could be recoded to map memory backed with file storage, for
*     instance using AIF_TMP.
*
*  Authors :
*     MBT: Mark Taylor (STARLINK) - originally for DAOPHOT
*
*  History :
*     7-JUN-2000 (MBT):
*        Original version.
*     14-Aug-2001 (KPD)
*        Original (DAOPHOT) version adapted for PERIOD
*     13-APR-2006 (BEC)
*        Replace direct BELL write with call to PERIOD_WRITEBELL.
*-
*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      CHARACTER * ( * ) TYPE
      INTEGER SIZE

*  Arguments Returned:
      INTEGER PNTR

*  Local variables :
      INTEGER STATUS

*.

*  Initialise the status variable.
      STATUS = 0

*  Allocate memory.
      CALL PSX_CALLOC( SIZE, TYPE, PNTR, STATUS )

*  Deal with errors.
      IF ( STATUS .NE. 0 ) THEN
         CALL PERIOD_WRITEBELL()
         WRITE (*, *) '** ERROR: Failure in dynamic allocation ' //
     :                'of memory; aborting PERIOD'
         STOP
      END IF

*   Exit routine.
      END
