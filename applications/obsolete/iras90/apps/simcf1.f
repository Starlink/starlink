      SUBROUTINE SIMCF1( C4, PXLO, PXHI, PYLO, PYHI, PSF, PWGSZX,
     :                   PWGSZY, RLO, RHI, PLO, PHI, PWGRID, WORK,
     :                   STATUS )
*+
*  Name:
*     SIMCF1

*  Purpose:
*     To create a single pixel weight grid assuming it will fit into the
*     current grid.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SIMCF1( C4, PXLO, PXHI, PYLO, PYHI, PSF, PWGSZX, PWGSZY,
*                  RLO, RHI, PLO, PHI, PWGRID, WORK, STATUS )

*  Description:
*     The entire grid is first filled with zeros.  The supplied PSF
*     values weights are then mapped into the pixel weights grid using
*     the supplied transformation. The pixel within the grid which is
*     closest to the a PSF pixel is increment by the pixel value (all
*     PSF values are assumed to lie within the pixel grid). The bounds
*     of the pixel region in which non-zero weights lie is returned.
*
*     An attempt is made to smooth out the noise introduced by the
*     nearest neightbour truncation applied to the pixel coordinates.
*     This is done by deciding on the nominal number of PSF values which
*     should contribute to each pixel weight value (determined by the
*     relative areas of the PSF and sky pixels), and then correcting
*     the calculated weight values if they actually had more or less
*     contributions than the nominal number.

*  Arguments:
*     C4( 6 ) = REAL (Given)
*        The coefficients of the linear transformation from PSF
*        indices to pixel coordinates in the sky map..
*     PXLO = INTEGER (Given)
*        The lower bound of the X axis of the PSFs.
*     PXHI = INTEGER (Given)
*        The upper bound of the X axis of the PSFs.
*     PYLO = INTEGER (Given)
*        The lower bound of the Y axis of the PSFs.
*     PYHI = INTEGER (Given)
*        The upper bound of the Y axis of the PSFs.
*     PSF( PXLO:PXHI, PYLO:PYHI ) = REAL (Given)
*        The PSF.
*     PWGSZX = INTEGER (Given)
*        The size of the first dimension of the pixel weight grid.
*     PWGSZY = INTEGER (Given)
*        The size of the second dimension of the pixel weight grid.
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
*        The pixel weights grid, normalised to a sum of unity.
*     WORK( PWGSZX, PWGSZY ) = REAL (Returned)
*        Work space.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-JAN-1993 (DSB):
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
      INTEGER PXLO
      INTEGER PXHI
      INTEGER PYLO
      INTEGER PYHI
      REAL PSF( PXLO:PXHI, PYLO:PYHI )
      INTEGER PWGSZX
      INTEGER PWGSZY

*  Arguments Given and Returned:
      INTEGER RLO
      INTEGER RHI
      INTEGER PLO
      INTEGER PHI

*  Arguments Returned:
      REAL PWGRID( PWGSZX, PWGSZY )
      REAL WORK( PWGSZX, PWGSZY )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! PSF pixel X index.
      INTEGER IX                 ! Sky pixel X index.
      INTEGER IY                 ! Sky pixel Y index.
      INTEGER J                  ! PSF pixel Y index.
      INTEGER PIXEL              ! Pixel index.
      INTEGER ROW                ! Row index.


      REAL ACTUAL                ! Actual no. of PSF pixels included in
                                 ! a particular weight.
      REAL NEWWGT                ! Corrected weight value.
      REAL NOM                   ! Nominal no. of PSF pixels per weight.
      REAL WGTSUM                ! Sum of weight values.
      REAL WTOT                  ! Total number of non-zero weights.
      REAL X                     ! Sky pixel X coordinate corresponding
                                 ! to the centre of the current PSF
                                 ! pixel.
      REAL XIN                   ! Contribution to the pixel X
                                 ! coordinate which is independent of I.
      REAL Y                     ! Sky pixel Y coordinate corresponding
                                 ! to the centre of the current PSF
                                 ! pixel.
      REAL YIN                   ! Contribution to the pixel Y
                                 ! coordinate which is independent of I.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Zero the whole grid (and workspace).
      DO ROW = 1, PWGSZY
         DO PIXEL = 1, PWGSZX
            PWGRID( PIXEL, ROW ) = 0.0
            WORK( PIXEL, ROW ) = 0.0
         END DO
      END DO

*  For each row in the PSF...
      DO J = PYLO, PYHI

*  Find the terms which the Y PSF index contribute to the sky pixel
*  coordinates.
         XIN = C4( 1 ) + C4( 3 )*J
         YIN = C4( 4 ) + C4( 6 )*J

*  For each column in the PSF...
         DO I = PXLO, PXHI

*  Find the corresponding sky pixel coordinates.
            X = XIN + C4( 2 )*I
            Y = YIN + C4( 5 )*I

*  Find the indices of the closest sky pixel.
            IX = NINT( X + 0.5 )
            IY = NINT( Y + 0.5 )

*  Increment the closest value in the weight grid. Also increment the
*  total data sum in the weight grid.
            PWGRID( IX, IY ) = PWGRID( IX, IY ) + PSF( I, J )

*  Increment the number of PSF pixels which have been added into this
*  weight grid value.
            WORK( IX, IY ) = WORK( IX, IY ) + 1.0

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

*  Count up the number of values with non-zero contributions.
      WTOT = 0.0
      DO IY = RLO, RHI
         DO IX = PLO, PHI
            IF( WORK( IX, IY ) .GT. 0.0 ) WTOT = WTOT + 1.0
         END DO
      END DO

*  Find the nominal number of PSF pixels per weight value.
      NOM = REAL( ( PXHI - PXLO + 1 )*( PYHI - PYLO + 1 ) )/WTOT

*  Apply a correction factor to each weight value which takes account
*  of the fact that some weights will by chance (due to the nearest
*  neighbour interpolation) contain more PSF values than others. This
*  reduces the noise in the pixel weight grids.
      WGTSUM = 0.0

      DO IY = RLO, RHI
         DO IX = PLO, PHI

            ACTUAL = WORK( IX, IY )
            IF( ACTUAL .GT. 0.0 ) THEN
               NEWWGT = PWGRID( IX, IY )*NOM/ACTUAL
               PWGRID( IX, IY ) = NEWWGT
               WGTSUM = WGTSUM + NEWWGT
            END IF

         END DO
      END DO

*  Report an error if the the total weight sum is zero.
      IF( WGTSUM .EQ. 0.0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SIMCF1_ERR1',
     :                 'SIMCF1: Zero data sum found in a supplied PSF',
     :                 STATUS )

*  Otherwise, divide the weight values by the total weight sum, to
*  ensure a total data sum of unity in the weight grid.
      ELSE

         DO IY = RLO, RHI
            DO IX = PLO, PHI
               PWGRID( IX, IY ) = PWGRID( IX, IY )/WGTSUM
            END DO
         END DO

      END IF

      END
