      SUBROUTINE MAPCC7( XLO, YLO, XHI, YHI, RLO, RHI, PLO, PHI, POFFX,
     :                   POFFY, PWGSZX, PWGSZY, PWGRID, INSIDE, STATUS )
*+
*  Name:
*     MAPCC7

*  Purpose:
*     See if a sample lies within the output map area.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MAPCC7( XLO, YLO, XHI, YHI, RLO, RHI, PLO, PHI, POFFX, POFFY,
*                  PWGSZX, PWGSZY, PWGRID, INSIDE, STATUS )

*  Description:
*     Each non-zero pixel in the pixel weight grid is mapped into the
*     pixel coordinate system of the output map . If any of these pixels
*     fall within the bounds of the output map, then INSIDE is returned
*     true. Otherwise INSIDE is returned false.

*  Arguments:
*     XLO = INTEGER (Given)
*        The lower pixel index bound on the first (X) axis of the
*        output NDF.
*     YLO = INTEGER (Given)
*        The lower pixel index bound on the second (Y) axis of the
*        output NDF.
*     XHI = INTEGER (Given)
*        The upper pixel index bound on the first (X) axis of the
*        output NDF.
*     YHI = INTEGER (Given)
*        The upper pixel index bound on the second (Y) axis of the
*        output NDF.
*     RLO = INTEGER (Given)
*        The lower row index bound of the region of the pixel weight
*        grid containing non-zero weights.
*     RHI = INTEGER (Given)
*        The upper row index bound of the region of the pixel weight
*        grid containing non-zero weights.
*     PLO = INTEGER (Given)
*        The lower pixel index bound of the region of the pixel weight
*        grid containing non-zero weights.
*     PHI = INTEGER (Given)
*        The upper pixel index bound of the region of the pixel weight
*        grid containing non-zero weights.
*     POFFX = INTEGER (Given)
*        The offset between X indices in the output map and in the pixel
*        weight grid.
*     POFFY = INTEGER (Given)
*        The offset between Y indices in the output map and in the pixel
*        weight grid.
*     PWGSZX  = INTEGER (Given)
*        The total no. of pixels per row in each pixel weight grid.
*     PWGSZY  = INTEGER (Given)
*        The total no. of rows in each pixel weight grid.
*     PWGRID( PWGSZX, PWGSZY ) = REAL (Given)
*        The array of pixel weights. The
*     INSIDE = LOGICAL (Returned)
*        Returned true if the sample overlaps the output map area.
*        Returned false if the sample is outside the map area.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-FEB-1992 (DSB):
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
      INTEGER XLO
      INTEGER YLO
      INTEGER XHI
      INTEGER YHI
      INTEGER RLO
      INTEGER RHI
      INTEGER PLO
      INTEGER PHI
      INTEGER POFFX
      INTEGER POFFY
      INTEGER PWGSZX
      INTEGER PWGSZY
      REAL PWGRID( PWGSZX, PWGSZY )

*  Arguments Returned:
      LOGICAL INSIDE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! X index into the weight grid.
      INTEGER IX                 ! X index corresponding to the pixel
                                 ! closest to the centre of the current
                                 ! sector.
      INTEGER IY                 ! Y index corresponding to the pixel
                                 ! closest to the centre of the current
                                 ! sector.
      INTEGER J                  ! Y index into the weight grid.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise INSIDE to indicate that no part of the current sample
*  falls within the image area.
      INSIDE = .FALSE.

*  Loop round each used row in the supplied pixel weight grid which
*  falls within the output map.
      DO J = MAX( RLO, YLO - POFFY ), MIN( RHI, YHI - POFFY )

*  Find the corresponding row in the output map.
         IY = J + POFFY

*  Loop round each pixel within the current row of the selected pixel
*  weight grid which falls within the output map.
         DO I = MAX( PLO, XLO - POFFX ), MIN( PHI, XHI - POFFX )

*  Find the corresponding pixel in the output map.
            IX = I + POFFX

*  Unless the pixel has zero, weight, indicate that the current sample
*  overlaps the image area.
            IF( PWGRID( I, J ) .GT. 0.0 ) INSIDE = .TRUE.

         END DO

      END DO

      END
