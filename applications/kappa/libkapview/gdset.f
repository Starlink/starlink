      SUBROUTINE GDSET( STATUS )
*+
*  Name:
*     GDSET

*  Purpose:
*     Selects a current graphics device.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL GDSET( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Usage:
*     gdset device

*  Description:
*     This application selects a current graphics device.  This device
*     will be used for all applications requiring an image-display
*     until changed explicitly.

*  ADAM Parameters:
*     DEVICE = DEVICE (Read)
*        The graphics device to become the current graphics device.

*  Examples:
*     gdset xwindows
*        Makes the xwindows device the current graphics device.

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 June 27 (MJC):
*        Original version.
*     1991 October 17 (MJC):
*        Added an AGI_ANNUL to ensure that the global parameter is
*        updated every invocation.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! No implicit typing allowed

*  Global Constants:
      INCLUDE  'SAE_PAR'       ! Global SSE definitions

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER
     :  PICID                  ! AGI input picture identifier

*.

*    Check the inherited status on entry.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Open the AGI workstation to read the device.

      CALL AGI_ASSOC( 'DEVICE', 'READ', PICID, STATUS )

*    Annul the workstation.

      CALL AGI_ANNUL( PICID, STATUS )

      END
