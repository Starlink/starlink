      SUBROUTINE MAPCF0( C4, NZSECT, NYSECT, SECWGT, PWGSZX, PWGSZY,
     :                   RLO1, RHI1, PLO1, PHI1, RLO, RHI, PLO, PHI,
     :                   PWGRID, STATUS )
*+
*  Name:
*     MAPCF0

*  Purpose:
*     To create a single pixel weight grid assuming it will fit into the
*     current grid.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MAPCF0( C4, NZSECT, NYSECT, SECWGT, PWGSZX, PWGSZY, RLO1,
*                  RHI1, PLO1, PHI1, RLO, RHI, PLO, PHI, PWGRID,
*                  STATUS )

*  Description:
*     The region of the pixel weights grid specified by RLO1, RHI1,
*     PLO1 and PHI1 is filled with zeros (the pixels outside this
*     region are assumed already to hold zero).  The supplied sector
*     weights are mapped into the pixel weights grid using the supplied
*     transformation. If a sector lies within the grid then the pixel
*     within the grid which is closest to the sector centre is
*     increment by the sector weight. The bounds of the pixel region in
*     which non-zero weights lie is returned, and includes pixels which
*     may lie off the edge of the current pixel grid. If this happens,
*     the calling routine should replaced the current grid by a larger
*     grid and recalculate the weights.

*  Arguments:
*     C4( 6 ) = REAL (Given)
*        The coefficients of the linear transformation from sector
*        indices to pixel coordinates in the output map..
*     NZSECT = INTEGER (Given)
*        The number of sector in the cross-scan direction across a full
*        size detector.
*     NYSECT = INTEGER (Given)
*        The number of sector in the in-scan direction across a full
*        size detector.
*     SECWGT( NZSECT, NYSECT ) = REAL (Given)
*        The array of sector weights, normalized to a sum of unity.
*     PWGSZX = INTEGER (Given)
*        The size of the first dimension of the pixel weight grid.
*     PWGSZY = INTEGER (Given)
*        The size of the second dimension of the pixel weight grid.
*     RLO1 = INTEGER (Given)
*        The lower row bound of the region of the input grid containing
*        non-zero weights.
*     RHI1 = INTEGER (Given)
*        The upper row bound of the region of the input grid containing
*        non-zero weights.
*     PLO1 = INTEGER (Given)
*        The lower pixel bound of the region of the input grid
*        containing non-zero weights.
*     PHI1 = INTEGER (Given)
*        The upper pixel bound of the region of the input grid
*        containing non-zero weights.
*     RLO = INTEGER (Given and Returned)
*        The extreme lower row bound of the regions of all output grids
*        containing non-zero weights.
*     RHI = INTEGER (Given and Returned)
*        The extreme upper row bound of the regions of all output grids
*        containing non-zero weights.
*     PLO = INTEGER (Given and Returned)
*        The extreme lower pixel bound of the regions of all output
*        grids containing non-zero weights.
*     PHI = INTEGER (Given and Returned)
*        The extreme upper pixel bound of the regions of all output
*        grids containing non-zero weights.
*     PWGRID( PWGSZX, PWGSZY ) = REAL (Returned)
*        The pixel weights grid.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-FEB-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.

*  Arguments Given:
      REAL C4( 6 )
      INTEGER NZSECT
      INTEGER NYSECT
      REAL SECWGT( NZSECT, NYSECT )
      INTEGER PWGSZX
      INTEGER PWGSZY
      INTEGER RLO1
      INTEGER RHI1
      INTEGER PLO1
      INTEGER PHI1

*  Arguments Given and Returned:
      INTEGER RLO
      INTEGER RHI
      INTEGER PLO
      INTEGER PHI

*  Arguments Returned:
      REAL PWGRID( PWGSZX, PWGSZY )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Cross-scan sector index.
      INTEGER IX                 ! X index corresponding to the pixel
                                 ! closest to the centre of the current
                                 ! sector.
      INTEGER IY                 ! Y index corresponding to the pixel
                                 ! closest to the centre of the current
                                 ! sector.
      INTEGER J                  ! In-scan sector index.
      INTEGER PIXEL              ! Pixel index.
      INTEGER ROW                ! Row index.
      REAL    X                  ! Pixel X coordinate corresponding to
                                 ! the centre of the current sector.
      REAL    XIN                ! Contribution to the pixel X
                                 ! coordinate which is independent of I.
      REAL    Y                  ! Pixel Y coordinate corresponding to
                                 ! the centre of the current sector.
      REAL    YIN                ! Contribution to the pixel Y
                                 ! coordinate which is independent of I.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Zero the section of the grid containing non-zero values.
      DO ROW = RLO1, RHI1
         DO PIXEL = PLO1, PHI1
            PWGRID( PIXEL, ROW ) = 0.0
         END DO
      END DO

*  For each in-scan sector.
      DO J = 1, NYSECT

*  Find the terms which the in-scan sector index contribute to the pixel
*  coordinates.
         XIN = C4( 1 ) + C4( 3 )*J
         YIN = C4( 4 ) + C4( 6 )*J

*  For each cross-scan sector.
         DO I = 1, NZSECT

*  Find the corresponding pixel coordinates.
            X = XIN + C4( 2 )*I
            Y = YIN + C4( 5 )*I

*  Find the pixel indices of the closest pixel.
            IX = NINT( X + 0.5 )
            IY = NINT( Y + 0.5 )

*  If this pixel lies within the pixel grid, increment the value of the
*  closest pixel in the weight grid.
            IF( IX .GE. 1 .AND. IX .LE. PWGSZX .AND.
     :          IY .GE. 1 .AND. IY .LE. PWGSZY ) THEN

               PWGRID( IX, IY ) = PWGRID( IX, IY ) + SECWGT( I, J )

            END IF

*  Update the bounds of the used region.
            IF( IY .LT. RLO ) THEN
               RLO = IY
            ELSE IF( IY .GT. RHI ) THEN
               RHI = IY
            END IF

            IF( IX .LT. PLO ) THEN
               PLO = IX
            ELSE IF( IX .GT. PHI ) THEN
               PHI = IX
            END IF

         END DO

      END DO

      END
