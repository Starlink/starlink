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
*        overlay device.  The device must be in GNS categories
*        IMAGE_OVERLAY or WINDOW_OVERLAY.

*  Examples:
*     ovset xov
*        Makes the xov device the current image-display overlay.

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     {enter_new_authors_here}

*  hISTORY:
*     1991 June 27 (MJC):
*        Original version.
*     1992 June 16 (MJC):
*        Made to work with WINDOW_OVERLAY class.  The restriction on the
*        number of colour indices has therefore been been relaxed.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Global SSE definitions
      INCLUDE 'CTM_PAR'        ! Colour-table management constants

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER
     :  MINCOL                 ! Minimum number of colour indices on
                               ! device to be classed as an image
                               ! display
      PARAMETER ( MINCOL = 1 )

*  Local Variables:
      INTEGER
     :  PICID,                 ! AGI input picture identifier
     :  ZONE                   ! SGS current zone

      LOGICAL                  ! True if :
     :  DEVCAN                 ! Image-display parameter is to be
                               ! cancelled

*.

*    Check the inherited status on entry.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Open the AGI workstation to read the device, and activate SGS/GKS.

      CALL AGS_ASSOC( 'DEVICE', 'READ', ' ', PICID, ZONE, STATUS )

*    Check whether the chosen device is an 'image display overlay'
*    with a suitable minimum number of colour indices, colour and
*    a cursor.

      CALL KPG1_QVID( 'DEVICE', 'SGS', 'IMAGE_OVERLAY,WINDOW_OVERLAY',
     :                'COLOUR', MINCOL, STATUS )

*    AGI closedown sequence.

      DEVCAN = STATUS .NE. SAI__OK
      CALL AGS_DEASS( 'DEVICE', DEVCAN, STATUS )

      END
