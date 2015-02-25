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
*     TDCA: Tim Ash (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 June 27 (MJC):
*        Original version.
*     22-JUL-1999 (TDCA):
*        Modified to use PGPLOT.
*     30-SEP-1999 (DSB):
*        Tidied up.
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
      INTEGER MINCOL           ! Minimum number of colour indices on
                               ! device to be classed as an image
                               ! display
      PARAMETER ( MINCOL = 8 + CTM__RSVPN )

*  Local Variables:
      INTEGER PICID            ! AGI input picture identifier
      INTEGER UP               ! Highest available colour index available

*.

*  Check the inherited status on entry.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Open the AGI workstation to read the device, and activate PGPLOT.
      CALL KPG1_PGOPN( 'DEVICE', 'READ', PICID, STATUS )

*  Check whether chosen device is an 'image display' with a suitable
*  minimum number of colour indices.
      CALL KPG1_PQVID( 'DEVICE', 'IMAGE_DISPLAY,IMAGE_OVERLAY,'/
     :                 /'WINDOW,MATRIX_PRINTER', ' ', MINCOL,
     :                 UP, STATUS )

*  AGI closedown sequence.
      CALL KPG1_PGCLS('DEVICE', .FALSE., STATUS )

*  If an error occurred, add a context message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IDSET_ERR', 'IDSET: Unable to set current '//
     :                 'image display device.', STATUS )
      END IF

      END
