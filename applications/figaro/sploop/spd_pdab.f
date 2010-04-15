      SUBROUTINE SPD_PDAB( STATUS )
*+
*  Name:
*     SPD_PDAB

*  Purpose:
*     Display zoomed finder image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_PDAB( STATUS )

*  Description:
*     This routine looks up the viewport specification for the finder
*     image and displays the image.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29 Apr 1994 (hme):
*        Original version.
*     2005 June 2 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Global Variables:
      INCLUDE 'SPD_PCOM'         ! Specdre SPLOOP common block

*  Status:
      INTEGER STATUS             ! Global status

*  Local constants:
      REAL LEFT, RIGHT, BOTTOM, TOP
      PARAMETER ( LEFT = 0.05, RIGHT = 0.95, BOTTOM = 0.05, TOP = 0.95 )

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check device and finder image available.
      IF ( .NOT. DEVOPN .OR. .NOT. IMXST ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_PDAB_E01', 'SPD_PDAB: Error displaying ' //
     :      'finder image. Device or image not available.', STATUS )
         GO TO 500
      END IF

*  Clear the view surface.
      CALL PGPAGE

*  Set the viewport.
*  Where on the display the viewport is, is specified by local
*  constants. The world coordinates to be displayed in the viewport are
*  up to date in the common block.
      CALL PGSVP( LEFT, RIGHT, BOTTOM, TOP )
      CALL PGSWIN( IMWIN(1), IMWIN(2), IMWIN(3), IMWIN(4) )

*  Greyscale display.
*  All arguments are up to date in the common block.
      CALL PGGRAY( %VAL( CNF_PVAL(IMAGE) ), IMDIM(1), IMDIM(2), 1,
     :             IMDIM(1), 1, IMDIM(2), IMRNG(2), IMRNG(1), IMZOOM )

*  Draw box.
*  This draws a simple box with automatic outward ticks and NDF pixel index
*  labels. No text labels are drawn.
      CALL PGBOX( 'BCINTS', 0., 0, 'BCINTS', 0., 0 )

*  Return.
 500  CONTINUE
      END
