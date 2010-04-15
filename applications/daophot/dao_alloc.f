      SUBROUTINE DAO_ALLOC( TYPE, SIZE, PNTR )
*+
*  Name :
*     DAO_ALLOC
*
*  Purpose
*     Allocate temporary storage.
*
*  Invocation :
*     CALL DAO_ALLOC( TYPE, SIZE, PNTR )
*
*  Description :
*     This routine allocates temporary storage for use by a DAOPHOT
*     routine.  Storage allocated in this way should be freed by a
*     call to DAO_DEALL.
*     In the event of an error it will abort the program in the
*     usual DAOPHOT way.
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
*     from the heap.  Historically, DAOPHOT has manipulated images in
*     memory so this is likely to be adequate.  However, if it became
*     necessary to use much more storage, this routine and DAO_DEALL
*     could be recoded to map memory backed with file storage, for
*     instance using AIF_TMP.
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
      CHARACTER * ( * ) TYPE
      INTEGER SIZE

*  Arguments Returned:
      INTEGER PNTR

*  Local variables :
      INTEGER STATUS            ! Starlink error status
*.

*  Initialise the status variable.
      STATUS = SAI__OK

*  Allocate memory.
      CALL PSX_CALLOC( SIZE, TYPE, PNTR, STATUS )

*  Deal with errors.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL STUPID( 'Error in memory allocation' )
         CALL OOPS
      END IF

*   Exit routine.
      END
