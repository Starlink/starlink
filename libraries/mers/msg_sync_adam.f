      SUBROUTINE MSG_SYNC( STATUS )
*+
*  Name:
*     MSG_SYNC

*  Purpose:
*     Synchronise message output via the user interface.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MSG_SYNC( STATUS )

*  Description:
*     This performs a synchronisation handshake with the user interface.
*     This is required if the current task has been outputting messages 
*     via the user interface and now wants to use a graphics cursor on the
*     command device. If a synchronisation error occurs, then an error 
*     report is made and the status value is returned set to MSG__SYNER.

*  Arguments: 
*     STATUS = INTEGER (Given and Returned)
*        The global status: it is returned set to MSG__SYNER on error.

*  Implementation Notes:
*     -  This subroutine is the ADAM version of MSG_SYNC.
*     -  This subroutine makes calls to SUBPAR_SYNC. 

*  Authors:
*     BDK: Dennis Kelly (ROE)
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-NOV-1985 (BDK):
*        Original version.
*     20-SEP-1989 (PCTR):
*        Converted to new prologue and layout.
*     13-MAR-1990 (PCTR):
*        Included error reporting.
*     26-JUN-1991 (PCTR):
*        Added mark and release to prevent message tokens being annulled
*        on error.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                      ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                  ! Standard SAE constants
      INCLUDE 'MSG_ERR'                  ! MGS_ error codes

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER ISTAT                      ! Local status

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the local status.
      ISTAT = SAI__OK

*  Create a new error context.
      CALL EMS_MARK

*  Perform a synchronisation handshake with the user interface.
      CALL SUBPAR_SYNC( ISTAT )

*  Check the returned status.
      IF ( ISTAT .NE. SAI__OK ) THEN

*     Annul the error context and report a message synchronisation error.
         CALL EMS_ANNUL( ISTAT )
         CALL EMS_MARK
         STATUS = MSG__SYNER
         CALL EMS_REP( 'MSG_SYNC_SYNER', 
     :   'MSG_SYNC: Error encountered during message synchronisation', 
     :   STATUS )
         CALL EMS_RLSE
      END IF

*  Release the current error context.
      CALL EMS_RLSE

      END
