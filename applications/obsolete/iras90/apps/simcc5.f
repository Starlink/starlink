      SUBROUTINE SIMCC5( C2, PXLO, PXHI, PYLO, PYHI, PSF, NX, NY, C3,
     :                   PWGSZX, PWGSZY, RLO, RHI, PLO, PHI, IPPWG,
     :                   IPPWG2, GCXIND, GCYIND, STATUS )
*+
*  Name:
*     SIMCC5

*  Purpose:
*     To create a set of sky pixel weight grids.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SIMCC5( C2, PXLO, PXHI, PYLO, PYHI, PSF, NX, NY, C3, PWGSZX,
*                  PWGSZY, RLO, RHI, PLO, PHI, IPPWG, IPPWG2, GCXIND,
*                  GCYIND, STATUS )

*  Description:
*     The weights to use for each sky pixel depend on the relative
*     position of the detector centre within the central sky pixel. A
*     set of pixel weight grids is created for a range of different
*     relative positions. The central sky pixel is divided up into NX
*     by NY equal segments, and a weights grid is produced with the
*     detector centre coincident with the centre of each segment. Each
*     grid is produced by mapping the supplied PSF values into the
*     pixel weights grid. Because the PSF pixels are much smaller than
*     the sky pixels, each sky pixel weight will in general be made up
*     by summing several PSF values together.

*  Arguments:
*     C2( 6 ) = REAL (Given)
*        The coefficients of the linear transformation from PSF
*        indices to focal plane offsets from the detector centre.
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
*     NX = INTEGER (Given)
*        The number of segments into which to divide the range in X
*        coordinate covered by the sky pixel containing the detector
*        centre.
*     NY = INTEGER (Given)
*        The number of segments into which to divide the range in Y
*        coordinate covered by the sky pixel containing the detector
*        centre.
*     C3( 6 ) = REAL (Given and Returned)
*        On entry, C3 holds the coefficients of the linear
*        transformation from focal plane offsets from the detector
*        centre, to pixel coordinates in the sky map. Elements
*        ( 1 ) and ( 4 ) are changed by this routine.
*     PWGSZX = INTEGER (Given and Returned)
*        The size of the first dimension of each pixel weight grid. If
*        the supplied values of PWGSZX and PWGSZY are not large enough
*        to contain all the non-zero pixel weight, then the values are
*        increased and extra dynamic memory is allocated to store the
*        larger pixel grids.
*     PWGSZY = INTEGER (Given and Returned)
*        The size of the second dimension of each pixel weight grid
*        (see PWGSZX).
*     RLO = INTEGER (Given and Returned)
*        The lower row bound of the region of the grids containing
*        non-zero weights. Grid pixels outside the area supplied by
*        RLO, RHI, PLO and PHI on entry are assumed to be zero.
*     RHI = INTEGER (Given and Returned)
*        The upper row bound of the region of the grids containing
*        non-zero weights.
*     PLO = INTEGER (Given and Returned)
*        The lower pixel bound of the region of the grids containing
*        non-zero weights.
*     PHI = INTEGER (Given and Returned)
*        The upper pixel bound of the region of the grids containing
*        non-zero weights.
*     IPPWG( NX, NY ) = INTEGER (Given and Returned)
*        Pointers to the NX by NY pixel weight grids.
*     IPPWG2 = INTEGER (Given and Returned)
*        Pointer to a real work array of the same size as the pixel
*        weight grids.
*     GCXIND = INTEGER (Returned)
*        The X index of the grid pixel containing the detector centre.
*     GCYIND = INTEGER (Returned)
*        The Y index of the grid pixel containing the detector centre.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-JAN-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'PRM_PAR'          ! Standard Starlink constants.

*  Arguments Given:
      REAL C2( 6 )
      INTEGER PXLO
      INTEGER PXHI
      INTEGER PYLO
      INTEGER PYHI
      REAL PSF( PXLO:PXHI, PYLO:PYHI )
      INTEGER NX
      INTEGER NY

*  Arguments Given and Returned:
      REAL C3( 6 )
      INTEGER PWGSZX
      INTEGER PWGSZY
      INTEGER RLO
      INTEGER RHI
      INTEGER PLO
      INTEGER PHI
      INTEGER IPPWG( NX, NY )
      INTEGER IPPWG2

*  Arguments Returned:
      INTEGER GCXIND
      INTEGER GCYIND

      LOGICAL LIST

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL    C4( 6 )            ! Coefficients of linear
                                 ! transformation from PSF indices,
                                 ! to pixel coordinates.
      INTEGER II                 ! Segment index in X direction.
      INTEGER JJ                 ! Segment index in Y direction.
      INTEGER PHI1               ! Low pixel bound of region previously
                                 ! used.
      INTEGER PLO1               ! Low pixel bound of region previously
                                 ! used.
      INTEGER RHI1               ! Low row bound of region previously
                                 ! used.
      INTEGER RLO1               ! Low row bound of region previously
                                 ! used.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store the current size of the used region.
      RLO1 = RLO
      RHI1 = RHI
      PLO1 = PLO
      PHI1 = PHI

*  For the moment, assume that the non-zero weights will fit into the
*  currently mapped arrays. Set up the indices of the pixel within the
*  weights grids which contains the centre of the sample. This is
*  always set to the central pixel in the grid.
      GCXIND = ( PWGSZX + 1 )/2
      GCYIND = ( PWGSZY + 1 )/2

*  Initialise the returned bounds of the used region.
      RLO = VAL__MAXI
      RHI = VAL__MINI
      PLO = VAL__MAXI
      PHI = VAL__MINI

*  Loop round each of the evenly spaced offsets in Y  for which
*  seperate weight grids are required.
      DO JJ = 1, NY

*  Modify the transformation from focal plane offsets to pixel
*  coordinates to put the detector centre at the required offset from
*  the bottom of the central pixel of the weight grid.
         C3( 4 ) = REAL( GCYIND ) + ( REAL( JJ ) - 0.5 )/REAL( NY )
     :             - 1.0

*  Loop round each of the evenly spaced offsets in X for which
*  seperate weight grids are required.
         DO II = 1, NX

*  Modify the transformation from focal plane offsets to pixel
*  coordinates to put the detector centre at the required offset from
*  the left of the central pixel of the weight grid.
            C3( 1 ) = REAL( GCXIND ) + ( REAL( II ) - 0.5 )/REAL( NX )
     :                - 1.0

*  Concatenate the two transformations to get the coefficients of the
*  transformation from PSF indices to pixel coordinates in the weight
*  grids.
            CALL IRM_TRCON( C2, C3, C4, STATUS )


*  Attempt to update the pixel weight grids using the current mapped
*  arrays.
            CALL SIMCF0( C4, PXLO, PXHI, PYLO, PYHI, PSF, PWGSZX,
     :                   PWGSZY, RLO1, RHI1, PLO1, PHI1, RLO, RHI, PLO,
     :                   PHI, %VAL( IPPWG( II, JJ ) ), %VAL( IPPWG2 ),
     :                   STATUS )

         END DO

      END DO

*  If the current grid size was too small, extend the mapped arrays and
*  recalculate the pixel weights storing them in the enlarged arrays.
      IF( PHI .GT. PWGSZX .OR. PLO .LT. 1 .OR.
     :    RHI .GT. PWGSZY .OR. RLO .LT. 1) THEN

*  Return the new sizes (including a "safety margin" of 2 pixels on each
*  edge).
         PWGSZX = PHI - PLO + 5
         PWGSZY = RHI - RLO + 5

*  Set up the indices of the pixel within the weights grids which
*  contains the centre of the sample.
         GCXIND = ( PWGSZX + 1 )/2
         GCYIND = ( PWGSZY + 1 )/2

*  Initialise the returned bounds of the used region.
         RLO = VAL__MAXI
         RHI = VAL__MINI
         PLO = VAL__MAXI
         PHI = VAL__MINI

*  Extend the work array.
         CALL PSX_REALLOC( PWGSZX*PWGSZY*VAL__NBR, IPPWG2, STATUS )

*  Loop round each of the evenly spaced offsets in Y  for which
*  seperate weight grids are required.
         DO JJ = 1, NY

*  Modify the transformation from focal plane offsets to pixel
*  coordinates to put the detector centre at the required offset from
*  the bottom of the central pixel of the weight grid.
            C3( 4 ) = REAL( GCYIND ) + ( REAL( JJ ) - 0.5 )/REAL( NY )
     :                - 1.0

*  Loop round each of the evenly spaced offsets in X for which
*  seperate weight grids are required.
            DO II = 1, NX

*  Modify the transformation from focal plane offsets to pixel
*  coordinates to put the detector centre at the required offset from
*  the left of the central pixel of the weight grid.
               C3( 1 ) = REAL( GCXIND ) +
     :                   ( REAL( II ) - 0.5 )/REAL( NX ) - 1.0

*  Concatenate the two transformations to get the coefficients of the
*  transformation from PSF indices to pixel coordinates in the weight
*  grids.
               CALL IRM_TRCON( C2, C3, C4, STATUS )

*  Extend the array holding the current weight grid.
               CALL PSX_REALLOC( PWGSZX*PWGSZY*VAL__NBR,
     :                           IPPWG( II, JJ ), STATUS )

*  Store the pixel weight grids using the new mapped array.
               CALL SIMCF1( C4, PXLO, PXHI, PYLO, PYHI, PSF, PWGSZX,
     :                      PWGSZY, RLO, RHI, PLO, PHI,
     :                      %VAL( IPPWG( II, JJ ) ), %VAL( IPPWG2 ),
     :                      STATUS )

            END DO

         END DO

      END IF

      END
