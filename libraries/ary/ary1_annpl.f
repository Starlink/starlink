      SUBROUTINE ARY1_ANNPL( ERASE, IPCB, STATUS )
*+
*  Name:
*     ARY1_ANNPL

*  Purpose:
*     Annul a placeholder.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_ANNPL( ERASE, IPCB, STATUS )

*  Description:
*     The routine annuls a placeholder, releasing the PCB slot which it
*     occupies and optionally erasing the object associated with it.

*  Arguments:
*     ERASE = LOGICAL (Given)
*        Whether to erase the associated object.
*     IPCB = INTEGER (Given and Returned)
*        Index to placeholder entry in the PCB. A value of zero is
*        returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine attempts to execute even if STATUS is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.

*  Algorithm:
*     -  Save the error context on entry.
*     -  If required, erase the placeholder object, annulling its
*     locator in the process.
*     -  Otherwise, simply annul the locator.
*     -  Release the PCB slot and reset the IPCB argument to zero.
*     -  Restore the error context.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-SEP-1989 (RFWS):
*        Original version.
*     1-MAR-1990 (RFWS):
*        Removed un-referenced include file.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants

*  Global Variables:
      INCLUDE 'ARY_PCB'          ! ARY_ Placeholder Control Block
*        PCB_LOC( ARY__MXPCB ) = CHARACTER * ( DAT__SZLOC ) (Read and
*        Write)
*           Placeholder locator.

*  Arguments Given:
      INTEGER IPCB
      LOGICAL ERASE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER TSTAT              ! Temporary status variable

*.
       
*  Save the STATUS value and mark the error stack.
      TSTAT = STATUS
      CALL ERR_MARK
       
*  If required, erase the associated object, annulling it's locator in
*  the process.
      STATUS = SAI__OK
      IF ( ERASE ) THEN
         CALL ARY1_ANTMP( PCB_LOC( IPCB ), STATUS )

*  Otherwise, simply annul the locator.
      ELSE
         CALL DAT_ANNUL( PCB_LOC( IPCB ), STATUS )
         PCB_LOC( IPCB ) = ARY__NOLOC
      END IF

*  Release the PCB slot.
      CALL ARY1_RLS( ARY__PCB, IPCB, STATUS )
      IPCB = 0
       
*  Annul any error if STATUS was previously bad, otherwise let the new
*  error report stand.
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( TSTAT .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            STATUS = TSTAT
       
*  Call error tracing routine if appropriate.
         ELSE
            CALL ARY1_TRACE( 'ARY1_ANNPL', STATUS )
         END IF
      ELSE
         STATUS = TSTAT
      END IF

*  Release error stack.
      CALL ERR_RLSE

      END
