      SUBROUTINE OVSET( STATUS )
*+
*  Name:
*     OVSET

*  Purpose:
*     Selects a current image-display overlay.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL OVSET( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Usage:
*     ovset device

*  Description:
*     This application selects a current image-display overlay. This
*     device will be used for all applications requiring an
*     image-display overlay until changed explicitly.

*  ADAM Parameters:
*     DEVICE = DEVICE (Read)
*        The image-display overlay to become the current image-display
*        overlay device.

*  Examples:
*     ovset xov
*        Makes the xov device the current image-display overlay.

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     DSB: David S Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 June 27 (MJC):
*        Original version.
*     1992 June 16 (MJC):
*        Made to work with WINDOW_OVERLAY class.  The restriction on the
*        number of colour indices has therefore been been relaxed.
*     4-OCT-2001 (DSB):
*        PGPLOT version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Global SSE definitions

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER MINCOL           ! Minimum number of colour indices on
      PARAMETER ( MINCOL = 1 ) ! device to be classed as an image display

*  Local Variables:
      INTEGER PICID            ! AGI input picture identifier
      INTEGER ZONE             ! SGS current zone
      LOGICAL DEVCAN           ! Cancel Image-display parameter?

*.

*  Check the inherited status on entry.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Open the AGI workstation to read the device, and activate PGPLOT.
      CALL AGP_ASSOC( 'DEVICE', 'READ', ' ', .FALSE., PICID, STATUS )

*  Check whether the chosen device is an 'image display overlay'
*  with a suitable minimum number of colour indices, colour and
*  a cursor.
      CALL KPG1_PQVID( 'DEVICE', 'IMAGE_OVERLAY,WINDOW_OVERLAY',
     :                 'COLOUR', MINCOL, STATUS )

*  AGI closedown sequence.
      DEVCAN = STATUS .NE. SAI__OK
      CALL AGP_DEASS( 'DEVICE', DEVCAN, STATUS )

      END
