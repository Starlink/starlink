      SUBROUTINE SIMCC6( XLO, YLO, XHI, YHI, RLO, RHI, PLO, PHI, POFFX,
     :                   POFFY, PWGSZX, PWGSZY, FACTOR, PWGRID, SKY,
     :                   NBAD, SMPVAL, INSIDE, STATUS )
*+
*  Name:
*     SIMCC6

*  Purpose:
*     Generate a single simulated sample value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SIMCC6( XLO, YLO, XHI, YHI, RLO, RHI, PLO, PHI, POFFX,
*                  POFFY, PWGSZX, PWGSZY, FACTOR, PWGRID, SKY, NBAD,
*                  SMPVAL, INSIDE, STATUS )

*  Description:
*     This routine multiplies each element of the pixel weight grid by
*     the corresponding sky pixel, and returns the sum of the products,
*     multiplied by the supplied FACTOR.  If any sky pixel is bad then
*     a bad value is returned. It also returns a flag saying if any
*     part of the sample area overlapped the edge of the sky image (in
*     this case a bad sample value is returned).

*  Arguments:
*     XLO = INTEGER (Given)
*        The lower pixel index bound on the first (X) axis of the
*        sky map.
*     YLO = INTEGER (Given)
*        The lower pixel index bound on the second (Y) axis of the
*        sky map.
*     XHI = INTEGER (Given)
*        The upper pixel index bound on the first (X) axis of the
*        sky map.
*     YHI = INTEGER (Given)
*        The upper pixel index bound on the second (Y) axis of the
*        sky map.
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
*        The offset between X indices in the sky map and in the pixel
*        weight grid, such that X[sky] = X[weight grid] + POFFX
*     POFFY = INTEGER (Given)
*        The offset between Y indices in the sky map and in the pixel
*        weight grid, such that Y[sky] = Y[weight grid] + POFFY
*     PWGSZX  = INTEGER (Given)
*        The total no. of pixels per row in each pixel weight grid.
*     PWGSZY  = INTEGER (Given)
*        The total no. of rows in each pixel weight grid.
*     FACTOR = REAL (Given)
*        The factor which converts the sum of the products of sky and
*        weights, to a CRDD sample value in the required output units.
*     PWGRID( PWGSZX, PWGSZY ) = REAL (Given)
*        The array of pixel weights.
*     SKY( XLO:XHI, YLO:YHI ) = REAL (Given)
*        The sky map.
*     NBAD = INTEGER (Given and Returned)
*        The total number of bad values simulated so far.
*     SMPVAL = REAL (Returned)
*        The sum of the products of the sky and weight values. Returned
*        bad if any bad sky values are encountered, or if the edge of
*        the sky map is encoutered.
*     INSIDE = LOGICAL (Returned)
*        Returned true if the sample does not touch the edge of the
*        sky map area (i.e. if it is wholly within the sky area).
*        Returned false otherwise.
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
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants

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
      REAL FACTOR
      REAL PWGRID( PWGSZX, PWGSZY )
      REAL SKY( XLO:XHI, YLO:YHI )

*  Arguments Given and Returned:
      INTEGER NBAD

*  Arguments Returned:
      REAL SMPVAL
      LOGICAL INSIDE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! X index into the weight grid.
      INTEGER IX                 ! X index into the sky map.
      INTEGER IY                 ! Y index into the sky map.
      INTEGER J                  ! Y index into the weight grid.

      REAL SKYVAL                ! Current sky value.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set INSIDE to true if no part of the current sample area falls
*  outside the sky area.
      INSIDE = ( RLO + POFFY .GE. YLO ) .AND. ( RHI + POFFY .LE. YHI )
     :         .AND.
     :         ( PLO + POFFX .GE. XLO ) .AND. ( PHI + POFFX .LE. XHI )

*  Return a bad value if the sample is not wholly inside the sky image.
      IF( .NOT. INSIDE ) THEN
         SMPVAL = VAL__BADR
         GO TO 999
      END IF

*  Initialise the simulated sample value to zero.
      SMPVAL = 0.0

*  Loop round each used row in the supplied pixel weight grid.
      DO J = RLO, RHI

*  Find the corresponding row in the sky map.
         IY = J + POFFY

*  Loop round each usable value within the current row of the selected
*  pixel weight grid.
         DO I = PLO, PHI

*  Find the corresponding pixel in the sky map.
            IX = I + POFFX

*  Return a bad value for the simulated sample value if the sky pixel is
*  bad.
            SKYVAL = SKY( IX, IY )
            IF( SKYVAL .EQ. VAL__BADR ) THEN
               SMPVAL = VAL__BADR
               NBAD = NBAD + 1
               GO TO 999
            END IF

*  Increment the simulated sample value.
            SMPVAL = SMPVAL + SKYVAL*PWGRID( I, J )

         END DO

      END DO

*  Scale the sample value into the correct units.
      SMPVAL = SMPVAL*FACTOR

*  Finish
 999  CONTINUE

      END
