      SUBROUTINE IDSET( STATUS )
*+
*  Name:
*     IDSET

*  Purpose:
*     Selects a current image-display device.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL IDSET( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Usage:
*     idset device

*  Description:
*     This application selects a current image-display device. This
*     device will be used for all applications requiring an
*     image-display until changed explicitly.

*  ADAM Parameters:
*     DEVICE = DEVICE (Read)
*        The image-display device to become the current image-display
*        device.  The device must be in one of the following GNS
*        categories: IMAGE_DISPLAY, IMAGE_OVERLAY, MATRIX_PRINTER, or
*        WINDOW, and have at least 24 colour indices or greyscale
*        intensities. 

*  Examples:
*     idset xwindows
*        Makes the xwindows device the current image-display device.

*  Related Applications:
*     KAPPA: GDSET, IDSTATE, OVSET; Figaro: SOFT.
				    
*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 June 27 (MJC):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Global SSE definitions
      INCLUDE 'PAR_ERR'        ! Parameter-system errors
      INCLUDE 'CTM_PAR'        ! Colour-table management constants

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER
     :  MINCOL                 ! Minimum number of colour indices on
                               ! device to be classed as an image
                               ! display
      PARAMETER ( MINCOL = 8 + CTM__RSVPN )

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

*    Check whether chosen device is an 'image display' with a suitable
*    minimum number of colour indices.

      CALL KPG1_QVID( 'DEVICE', 'SGS', 'IMAGE_DISPLAY,IMAGE_OVERLAY,'/
     :                /'WINDOW,MATRIX_PRINTER', ' ', MINCOL, STATUS )

*    AGI closedown sequence.

      DEVCAN = STATUS .NE. SAI__OK
      CALL AGS_DEASS( 'DEVICE', DEVCAN, STATUS )

      END
