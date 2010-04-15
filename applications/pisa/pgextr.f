      SUBROUTINE PGEXTR( ID0, ID1, ID2, RDOWN, RUP, DDOWN, DUP, MDOWN,
     :                   MUP, STATUS )
*+
*  Name:
*     PGEXTR

*  Purpose:
*     To restore the view ports used in PISAFIT, prior to saving them in
*     the AGI database. The routine also closes down AGI and PGPLOT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PGEXTR( ID0, ID1, ID2, RDOWN, RUP, DDOWN, DUP, MDOWN, MUP
*                  STATUS )


*  Description:
*     The routine first sets up the view port associated with the
*     plotting of the radial fit. It then sets up the world coordinates
*     selects the first picture ( base viewport ) and saves the current
*     setup in AGI. The picture is then labelled. A repeat procedure is
*     followed for the residuals plot and then AGI and PGPLOT are closed
*     down.

*  Arguments:
*     ID0 = INTEGER (Given)
*        The AGI identifer for the first ( base ) viewport.
*     ID1 = INTEGER (Given)
*        The AGI identifer for the fit data picture.
*     ID2 = INTEGER (Given)
*        The AGI identifer for the residuals data picture.
*     RDOWN = REAL (Given)
*        The lower bound of the plotted radii ( not used passed for
*        completeness ).
*     RUP = REAL (Given)
*        The upper bound of the plotted radii.
*     DDOWN = REAL (Given)
*        The lower bound of the plotted fit data ( intensity ).
*     DUP = REAL (Given)
*        The upper bound of the plotted fit data ( intensity ).
*     MDOWN = REAL (Given)
*        The lower bound of the plotted residuals.
*     MUP = REAL (Given)
*        The lower bound of the plotted residuals.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-SEP-1990 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER ID0
      INTEGER ID1
      INTEGER ID2
      REAL RDOWN
      REAL RUP
      REAL DDOWN
      REAL DUP
      REAL MDOWN
      REAL MUP

*  Local Variables:
      INTEGER ID                 ! dummy for picture identifiers

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Select the data plot picture, set up the viewport in it
      CALL AGI_SELP( ID1, STATUS )
      CALL AGP_NVIEW( .FALSE., STATUS )

*  Set current world coordinates in this sub-picture
      CALL PGWINDOW( 0.0, RUP, DDOWN, 0.0 )

*  Save the current set-up within this
      CALL AGP_SVIEW( 'DATA', 'FIT', ID, STATUS )

*  Add a label
      CALL AGI_SLAB( -1, 'PISAFIT_FIT', STATUS )

*  Select the residuals sub-picture.
      CALL AGI_SELP( ID2, STATUS )
      CALL AGP_NVIEW( .FALSE., STATUS )
      CALL PGWINDOW( 0.0, RUP, MDOWN, MUP )

*  Save it
      CALL AGP_SVIEW( 'DATA', 'RESIDS', ID, STATUS )

*  Add a label
      CALL AGI_SLAB( -1, 'PISAFIT_RESIDS', STATUS )

*  Close PGPLOT
      CALL AGP_DEACT( STATUS )

*  End of AGI scope - annuls all ID's and set current ID to base picture
      CALL AGI_END( ID0, STATUS )
      CALL AGI_ANNUL( ID0, STATUS )
      CALL AGI_CANCL( 'DEVICE', STATUS )
      END
* $Id$
