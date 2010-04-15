      SUBROUTINE POL1_BLKWR( NX, NY, A, W, IBOXX, IBOXY, WLIM,
     :                       B, ASUM, WSUM, STATUS )
*+
*  Name:
*     POL1_BLKWR

*  Purpose:
*     Smooth a 2-dimensional image using a weighted rectangular box filter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_BLKWR( NX, NY, A, W, IBOXX, IBOXY, WLIM, B,
*                      ASUM, WSUM, STATUS )

*  Description:
*     The routine smooths a 2-dimensional image using a weighted rectangular
*     box filter; each pixel is replaced by the weighted mean of those good
*     neighbours which lie within a box of specified size.

*  Arguments:
*     NX = INTEGER (Given)
*        First dimension of the image to be smoothed.
*     NY = INTEGER (Given)
*        Second dimension of the image to be smoothed.
*     A( NX, NY ) = REAL (Given)
*        Input image to be smoothed.
*     W( NX, NY ) = REAL (Given)
*        The input weights. This array should not contain any VAL__BADR
*        values (use zero instead).
*     IBOXX = INTEGER (Given)
*        Half-size of the smoothing box in pixels in the X direction
*        (the actual size of the box used will be 2*IBOXX+1 pixels).
*     IBOXY = INTEGER (Given)
*        Half-size of the smoothing box in pixels in the Y direction
*        (the actual size of the box used will be 2*IBOXY+1 pixels).
*     WLIM = REAL (Given)
*        The total wieght in a smoothing box must be greater than WLIM in
*        order to produce a good smoothed output pixel value.
*     B( NX, NY ) = REAL (Returned)
*        The smoothed output image.
*     ASUM( NX ) = DOUBLE PRECISION (Returned)
*        Workspace for the pixel sums.
*     WSUM( NX ) = DOUBLE PRECISION (Returned)
*        Workspace for the weight sums.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     3-MAR-1999 (DSB):
*        Original version, based on KPG1_BLOCR by RFWS.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      REAL A( NX, NY )
      REAL W( NX, NY )
      INTEGER IBOXX
      INTEGER IBOXY
      REAL WLIM

*  Arguments Returned:
      REAL B( NX, NY )
      DOUBLE PRECISION ASUM( NX )
      DOUBLE PRECISION WSUM( NX )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IIX                ! Loop counter for summing over pixels
      INTEGER IIY                ! Loop counter for summing over lines
      INTEGER ILINE              ! Contributing line number
      INTEGER IPIX               ! Contributing pixel number
      INTEGER IX                 ! First array index
      INTEGER IY                 ! Second array index
      INTEGER NEWX               ! X position of new pixel
      INTEGER NEWY               ! Y position of new pixel
      INTEGER OLDX               ! X position of old pixel
      INTEGER OLDY               ! Y position of old line
      DOUBLE PRECISION ASUM0     ! Good pixel sum
      DOUBLE PRECISION WSUM0     ! Good pixel count
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the workspace arrays.
      DO 1 IX = 1, NX
         ASUM( IX ) = 0.0D0
         WSUM( IX ) = 0.0D0
 1    CONTINUE

*  Loop to initialise elements of the two workspace arrays so that they
*  contain sum for the good pixels in a box of size 1x(2*IBOXY+1)
*  centred on pixel location (IX,0). First loop over all the possible
*  contributing lines.
      DO 4 IIY = -IBOXY, IBOXY

*  If a line lies outside the input image, then use the boundary line
*  instead.
         IF ( IIY .LT. 1 ) THEN
            ILINE = 1
         ELSE IF ( IIY .GT. NY ) THEN
            ILINE = NY
         ELSE
            ILINE = IIY
         END IF

*  Accumulate sums for each box in the workspace arrays.
         DO 2 IX = 1, NX
            IF ( A( IX, ILINE ) .NE. VAL__BADR ) THEN
               ASUM( IX ) = ASUM( IX ) + A( IX, ILINE )*W( IX, ILINE )
               WSUM( IX ) = WSUM( IX ) + W( IX, ILINE )
            END IF
 2       CONTINUE
 4    CONTINUE

*  Smooth down the image.
*  =====================
*  Loop to form smoothed output values for each image line by moving the
*  accumulated sums for each initial box down the image.
      DO 9 IY = 1, NY

*  Find the position of the old input line which is lost from the boxes
*  as they are moved down the image and the position of the new line
*  which enters the boxes. Allow for the image boundaries.
         OLDY = IY - IBOXY - 1
         IF ( OLDY .LT. 1 ) OLDY = 1
         NEWY = IY + IBOXY
         IF ( NEWY .GT. NY ) NEWY = NY

*  Remove the pixels in the old line from the accumulated sums and add
*  the pixels in the new line.
         DO 5 IX = 1, NX
            IF ( A( IX, OLDY ) .NE. VAL__BADR ) THEN
               ASUM( IX ) = ASUM( IX ) - A( IX, OLDY )*W( IX, OLDY )
               WSUM( IX ) = WSUM( IX ) - W( IX, OLDY )
            END IF
            IF ( A( IX, NEWY ) .NE. VAL__BADR ) THEN
               ASUM( IX ) = ASUM( IX ) + A( IX, NEWY )*W( IX, NEWY )
               WSUM( IX ) = WSUM( IX ) + W( IX, NEWY )
            END IF
 5       CONTINUE

*  Smooth along each image line.
*  ============================
*  Form initial sums for a box of size (2*IBOXX+1)x1 centred on pixel
*  (0,IY).
         ASUM0 = 0.0D0
         WSUM0 = 0.0D0
         DO 7 IIX = -IBOXX, IBOXX

*  Allow for the image boundaries.
            IF ( IIX .LT. 1 ) THEN
               IPIX = 1
            ELSE IF ( IIX .GT. NX ) THEN
               IPIX = NX
            ELSE
               IPIX = IIX
            END IF

*  Use the current values stored in the workspace arrays to form these
*  initial sums.
            ASUM0 = ASUM0 + ASUM( IPIX )
            WSUM0 = WSUM0 + WSUM( IPIX )
 7       CONTINUE

*  Loop to move these sums along the current image line.
         DO 8 IX = 1, NX

*  Find the position of the old pixel which is lost from the box as it
*  moves along the line and the position of the new one which enters
*  it. Allow for the image boundaries.
            OLDX = IX - IBOXX - 1
            IF ( OLDX .LT. 1 ) OLDX = 1
            NEWX = IX + IBOXX
            IF ( NEWX .GT. NX ) NEWX = NX

*  Update the sums by subtracting the old pixel and adding the new one.
            ASUM0 = ASUM0 + ( ASUM( NEWX ) - ASUM( OLDX ) )
            WSUM0 = WSUM0 + ( WSUM( NEWX ) - WSUM( OLDX ) )

*  Calculate the smoothed output value.
            IF ( WSUM0 .GT. WLIM ) THEN
               B( IX, IY ) = ASUM0 / WSUM0

*  It is bad if the WLIM criterion is not met.
            ELSE
               B( IX, IY ) = VAL__BADR
            END IF

 8       CONTINUE
 9    CONTINUE

      END
