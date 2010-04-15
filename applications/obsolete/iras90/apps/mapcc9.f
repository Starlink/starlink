      SUBROUTINE MAPCC9( XLO, YLO, XHI, YHI, RLO, RHI, PLO, PHI, POFFX,
     :                   POFFY, TOTWGT, PWGSZX, PWGSZY, PWGRID, DATVAL,
     :                   DATOUT, VAROUT, WGTOUT, INSIDE, STATUS )
*+
*  Name:
*     MAPCC9

*  Purpose:
*     Paste data, weight and squared data values for a single sample.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MAPCC9( XLO, YLO, XHI, YHI, RLO, RHI, PLO, PHI, POFFX, POFFY,
*                  TOTWGT, PWGSZX, PWGSZY, PWGRID, DATVAL, DATOUT,
*                  VAROUT, WGTOUT, INSIDE, STATUS )

*  Description:
*     This routine adds the data, variance and squared data values for
*     a single detector sample to the "running-sum" images, DATOUT,
*     VAROUT and WGTOUT.

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
*     TOTWGT = REAL (Given)
*        The total weight to assign to the sample.
*     PWGSZX  = INTEGER (Given)
*        The total no. of pixels per row in each pixel weight grid.
*     PWGSZY  = INTEGER (Given)
*        The total no. of rows in each pixel weight grid.
*     PWGRID( PWGSZX, PWGSZY ) = REAL (Given)
*        The array of pixel weights. The
*     DATVAL = REAL (Given)
*        The sample value to be mapped, in the required output units.
*     DATOUT( XLO:XHI, YLO:YHI ) = REAL (Given and Returned)
*        An image holding the sum of the weighted CRDD surface
*        brightness values. The input CRDD is added into this
*        array by this routine.
*     VAROUT( XLO:XHI, YLO:YHI ) = REAL (Given and Returned)
*        An image holding the sum of the weighted squared data values.
*        The weighted squared input data values are added into this
*        array by this routine.
*     WGTOUT( XLO:XHI, YLO:YHI ) = REAL (Given and Returned)
*        An image holding the sum of the weights used at each pixel in
*        DATOUT. The weights used for mapping the input CRDD are added
*        into this array by this routine.
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
      REAL TOTWGT
      INTEGER PWGSZX
      INTEGER PWGSZY
      REAL PWGRID( PWGSZX, PWGSZY )
      REAL DATVAL

*  Arguments Given and Returned:
      REAL DATOUT( XLO:XHI, YLO:YHI )
      REAL VAROUT( XLO:XHI, YLO:YHI )
      REAL WGTOUT( XLO:XHI, YLO:YHI )

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
      REAL    PWGT               ! Pixel weight, normalised to required
                                 ! total weight value.
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

*  Modify the pixel weight to give the required total weight.
            PWGT = TOTWGT*PWGRID( I, J )

*  Ignore the pixel if it has zero weight.
            IF( PWGT .GT. 0.0 ) THEN

*  Indicate that the current sample overlaps the image area.
               INSIDE = .TRUE.

*  Increment the value of the corresponding pixel in the image holding
*  the sum of the weighted surface brightness values.
               DATOUT( IX, IY ) = DATOUT( IX, IY ) + PWGT*DATVAL

*  Increment the value of the corresponding pixel in the image holding
*  the sum of the weights.
               WGTOUT( IX, IY ) = WGTOUT( IX, IY ) + PWGT

*  Increment the value of the corresponding pixel in the image holding
*  the sum of the weighted squared data values.
               VAROUT( IX, IY ) = VAROUT( IX, IY ) + PWGT*DATVAL*DATVAL

            END IF

         END DO

      END DO

      END
