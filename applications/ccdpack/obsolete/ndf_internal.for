      SUBROUTINE NDF_$A2P( N, AX, LBND, UBND, HAVCEN, HAVWID, CENTRE,
     :                     WIDTH, INC, IPIX0, CENT0, SPACE0, INPIX,
     :                     IPIX1, CENT1, WIDTH1, STATUS )
*+
*  Name:
*     NDF_$A2P

*  Purpose:
*     Convert axis coordinates into pixel indices and related
*     information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_$A2P( N, AX, LBND, UBND, HAVCEN, HAVWID, CENTRE, WIDTH,
*                    INC, IPIX0, CENT0, SPACE0, INPIX, IPIX1, CENT1,
*                    WIDTH1, STATUS )

*  Description:
*     This routine converts a sequence of axis coordinate values for an
*     NDF axis into corresponding pixel index information. Since NDF
*     pixels may be discontiguous, any particular axis value may lie in
*     a single pixel, in several pixels or in no pixels at all. This
*     routine returns information about the index of the pixel lying
*     immediately "below" the axis coordinate and about a second
*     neighbouring pixel which is either the pixel in which the
*     coordinate lies, or the one whose centre lies nearest to the
*     coordinate supplied. A flag indicating whether the coordinate
*     lies within this pixel is also returned.

*  Arguments:
*     N = INTEGER (Given)
*        Number of axis coordinate values to be converted.
*     AX( N ) = DOUBLE PRECISION (Given)
*        Array of axis coordinate values.
*     LBND = INTEGER (Given)
*        Lower pixel index bound of the NDF dimension to which the axis
*        coordinates refer.
*     UBND = INTEGER (Given)
*        Upper pixel index bound of the NDF dimension to which the axis
*        coordinates refer.
*     HAVCEN = LOGICAL (Given)
*        Whether an array of pixel centre positions is available for the
*        NDF dimension.
*     HAVWID = LOGICAL (Given)
*        Whether an array of pixel width values is available for the NDF
*        dimension. This argument is not used unless HAVCEN is .TRUE..
*     CENTRE( LBND : UBND ) = DOUBLE PRECISION (Given)
*        Array of pixel centre positions, which should either increase
*        or decrease monotonically. This argument is not used unless
*        HAVCEN is set to .TRUE..
*     WIDTH( LBND : UBND ) = DOUBLE PRECISION (Given)
*        Array of non-negative pixel width values. This argument is not
*        used unless both HAVCEN and HAVWID are set to .TRUE..
*     INC = LOGICAL (Returned)
*        A value of .TRUE. is returned if the axis centre values (or
*        the defaults used in their place) increase monotonically with
*        pixel index and .FALSE. is returned if they decrease
*        monotonically. An error will be reported, and STATUS set, if a
*        non-monotonic set of centre values is detected.
*     IPIX0( N ) = INTEGER (Returned)
*        Returns the index of the NDF pixel "below" the corresponding
*        axis coordinate. If INC is .TRUE., this will be the index of
*        the pixel with the largest centre position less than or equal
*        to the axis coordinate. If INC is .FALSE., it will be the
*        index of the pixel with the smallest centre position greater
*        than or equal to the axis coordinate.
*     CENT0( N ) = DOUBLE PRECISION (Returned)
*        Returns the centre coordinate of the pixel with index IPIX0.
*     SPACE0( N ) = DOUBLE PRECISION (Returned)
*        Returns the signed difference between the centre coordinate of
*        the pixel with index IPIX0 + 1 and the centre position of the
*        pixel with index IPIX0.
*     INPIX( N ) = LOGICAL (Returned)
*        Returns .TRUE. if the axis coordinate lies within the bounds of
*        a pixel and .FALSE. if it does not.
*     IPIX1( N ) = INTEGER (Returned)
*        If INPIX is .TRUE., then IPIX1 returns the index of the pixel
*        which contains the axis coordinate. If the axis coordinate
*        lies inside more than one pixel, then this index identifies
*        which of these pixels has its centre position nearest to the
*        axis coordinate.  If INPIX is .FALSE., then IPIX1 returns the
*        index of the pixel whose centre coordinate is nearest to the
*        axis coordinate.
*     CENT1( N ) = DOUBLE PRECISION (Returned)
*        Returns the value of the centre coordinate for the pixel with
*        index IPIX1.
*     WIDTH1( N ) = DOUBLE PRECISION (Returned)
*        Returns a non-negative value for the width of the pixel with
*        index IPIX1.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine performs simple checks on the validity of the
*     values it returns.

*  Algorithm:
*     -  Check inherited global status.
*     -  If no pixel centre positions are available, then the pixels
*     are taken to be uniformly spaced, of unit width and contiguous.
*     Loop to calculate output values for each input axis coordinate.
*     -  Find the index of the pixel whose centre value equals or lies
*     immediately below the axis coordinate (ensure that half-integer
*     coordinate values are rounded upwards regardless of sign).
*     -  Obtain the centre position for this pixel and the distance to
*     the next pixel centre.
*     -  Find the index of the pixel containing the axis coordinate
*     (ensuring that if the coordinate lies on a boundary, then it lies
*     in the lower pixel, regardless of sign).
*     -  Obtain the centre position and width of this pixel.
*     -  If pixel centre positions are available, then check to see if
*     the centre value changes from one end of the axis to the other.
*     Report an error if it does not.
*     -  Determine whether axis centre positions increase or decrease
*     with pixel index. If there is a choice, then assume they
*     increase.
*     -  Set up an increment value to match the direction of increase
*     in the pixel centre positions.
*     -  Loop to process each input axis coordinate.
*     -  Set initial lower and upper pixel index limits. These will be
*     progressively adjusted until the axis coordinate is found to lie
*     between the centre positions of two adjacent pixels. Ensure that
*     these initial pixels are not adjacent.
*     -  Obtain the centre positions for the two limiting pixels.
*     -  Calculate the difference between the centre positions at the
*     current pixel limits and check this difference has the correct
*     sign. Report an error if it does not, as this indicates a
*     non-monotonic variation of centre position with pixel index.
*     -  Loop until the search converges on two adjacent pixels.
*     -  Determine if we are interpolating between the centre
*     coordinates of the two limiting pixels (as opposed to
*     extrapolating). We are interpolating only if the first pixel's
*     coordinate lies "below" the required value, while the second one
*     does not (interpolation must be achieved before convergence is
*     assured).
*     -  Interpolate (or extrapolate) linearly to estimate the pixel
*     index where the centre position matches the axis coordinate.
*     -  If we are extrapolating (not interpolating), then the
*     algorithm cannot converge on this iteration. Instead, we aim to
*     reach a position where we can interpolate on the next iteration
*     (and so eventually converge). Thus we aim to get one limiting
*     pixel lying "below" the required axis coordinate, and the other
*     lying "above".
*     -  If the new pixel index is the same as one of the old pixel
*     indices, then widen the search range by one pixel at the
*     appropriate end (otherwise there will be no change, hence an
*     infinite loop). Note that the pixels cannot become adjacent at
*     this point, so the convergence criterion will not be met on this
*     iteration.
*     -  Obtain the axis coordinate of the new pixel index and see if
*     it lies "below" the required value.
*     -  If so, and the new pixel lies below the initial search range,
*     then simply extend the lower pixel of the range. Otherwise move
*     the upper bound to this pixel and derive a new lower bound. Again
*     note that the limiting pixel indices cannot become adjacent.
*     -  Modify the range similarly if the new pixel lies above the
*     initial search range.
*     -  If we are interpolating, then convergence is now possible. To
*     ensure this, we ensure that the search range is narrowed by at
*     least one pixel on each iteration (otherwise there will be no
*     change, hence an infinite loop).
*     -  Obtain the new pixel centre location and see if it lies
*     "below" the required axis value.
*     -  Update the lower or upper pixel index limit according to the
*     outcome of the interpolation.
*     -  Return to perform the next iteration.
*     -  Return the index of the pixel lying immediately "below" the
*     axis coordinate, the centre position of this pixel, and the
*     distance to the next pixel.
*     -  Find the distance between the axis coordinate and the centre
*     positions of the two neighbouring pixels.
*     -  Determine if the axis coordinate lies inside each of these
*     pixels.
*     -  If it lies nearer the lower pixel, but does not lie inside it
*     and instead lies inside the upper pixel, then note this.
*     -  Otherwise, return values appropriate to the nearer (lower)
*     pixel.
*     -  If it lies nearer the upper pixel, but does not lie inside it
*     and instead lies inside the lower pixel, then note this.
*     -  Otherwise, return values appropriate to the nearer (upper)
*     pixel.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     6-MAR-1991 (RFWS):
*        Original version.
*     30-SEP-1991 (RFWS):
*        Added better security against non-convergence of the axis
*        coordinate search algorithm.
*     4-OCT-1991 (RFWS):
*        Moved error check outside loop.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes      

*  Arguments Given:
      INTEGER N
      DOUBLE PRECISION AX( N )
      INTEGER LBND
      INTEGER UBND
      LOGICAL HAVCEN
      LOGICAL HAVWID
      DOUBLE PRECISION CENTRE( LBND : UBND )
      DOUBLE PRECISION WIDTH( LBND : UBND )

*  Arguments Returned:
      LOGICAL INC
      INTEGER IPIX0( N )
      DOUBLE PRECISION CENT0( N )
      DOUBLE PRECISION SPACE0( N )
      LOGICAL INPIX( N )
      INTEGER IPIX1( N )
      DOUBLE PRECISION CENT1( N )
      DOUBLE PRECISION WIDTH1( N )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION CDIFF     ! Difference in pixel centre positions
      DOUBLE PRECISION CEN( 2 )  ! Pixel centre positions
      DOUBLE PRECISION CNEW( 1 ) ! Centre position of interpolated pixel
      DOUBLE PRECISION DIF( 2 )  ! Distance to neighbouring pixels
      DOUBLE PRECISION DINC      ! Direction of increasing centre values
      DOUBLE PRECISION VAR( 2 )  ! Variance values (unused)
      DOUBLE PRECISION VARIAN( 1 ) ! Dummy variance array
      DOUBLE PRECISION VNEW( 1 ) ! Variance value of interpolated pixel
      DOUBLE PRECISION WID( 2 )  ! Width of neighbouring pixels
      DOUBLE PRECISION WNEW( 1 ) ! Width value of interpolated pixel
      INTEGER I                  ! Loop counter for axis coordinates
      INTEGER INEW( 1 )          ! Interpolated pixel index
      INTEGER IRANGE( 2 )        ! Pixel index search range
      LOGICAL BELOW              ! Pixel lies "below" axis coordinate?
      LOGICAL BELOW1             ! First pixel "below" axis coordinate?
      LOGICAL BELOW2             ! Second pixel "below" axis coordinate?
      LOGICAL IN( 2 )            ! Coordinate inside neighbouring pixel?
      LOGICAL INTERP             ! Interpolation (not extrapolating)?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  No pixel centre positions are available.
*  =======================================
*  In this case, the pixels are taken to be uniformly spaced, of unit
*  width and contiguous. Loop to calculate output values for each input
*  axis coordinate.
      IF ( .NOT. HAVCEN ) THEN
         INC = .TRUE.
         DO 1 I = 1, N

*  Find the index of the pixel whose centre value equals or lies
*  immediately below the axis coordinate (ensure that half-integer
*  coordinate values are rounded upwards regardless of sign).
            IF ( AX( I ) .GE. 0.0D0 ) THEN
               IPIX0( I ) = NINT( AX( I ) )
            ELSE
               IPIX0( I ) = INT( AX( I ) - 0.5D0 )
               IF ( DBLE( IPIX0( I ) ) .EQ. AX( I ) - 0.5D0 )
     :            IPIX0( I ) = IPIX0( I ) + 1
            END IF

*  Obtain the centre position for this pixel and the distance to the
*  next pixel centre.
            CENT0( I ) = DBLE( IPIX0( I ) ) - 0.5D0
            SPACE0( I ) = 1.0D0

*  Find the index of the pixel containing the axis coordinate (ensuring
*  that if the coordinate lies on a boundary, then it lies in the lower
*  pixel, regardless of sign).
            INPIX( I ) = .TRUE.
            IF ( AX( I ) .GE. 0.0D0 ) THEN
               IPIX1( I ) = INT( AX( I ) )
               IF ( DBLE( IPIX1( I ) ) .NE. AX( I ) )
     :            IPIX1( I ) = IPIX1( I ) + 1
            ELSE
               IPIX1( I ) = INT( AX( I ) )
            END IF

*  Obtain the centre position and width of this pixel.
            CENT1( I ) = DBLE( IPIX1( I ) ) - 0.5D0
            WIDTH1( I ) = 1.0D0
 1       CONTINUE

*  Pixel centre positions are available.
*  ====================================
*  In this case, check to see if the centre value changes from one end
*  of the axis to the other. Report an error if it does not.
      ELSE
         IF ( UBND .NE. LBND ) THEN
            IF ( CENTRE( UBND ) .EQ. CENTRE( LBND ) ) THEN
               STATUS = NDF__AXVIN
               CALL ERR_REP( 'NDF_$A2P_ERR1',
     :                       'Axis CENTRE values do not increase or ' //
     :                       'decrease monotonically.',
     :                       STATUS )
               GO TO 99

*  Determine whether axis centre positions increase or decrease with
*  pixel index. If there is a choice, then assume they increase.
            ELSE
               INC = ( CENTRE( UBND ) .GT. CENTRE( LBND ) )
            END IF
         ELSE
            INC = .TRUE.
         END IF

*  Set up an increment value to match the direction of increase in the
*  pixel centre positions.
         IF ( INC ) THEN
            DINC = 1.0D0
         ELSE
            DINC = -1.0D0
         END IF

*  Loop to process each input axis coordinate.
         DO 3 I = 1, N

*  Set initial lower and upper pixel index limits. These will be
*  progressively adjusted until the axis coordinate is found to lie
*  between the centre positions of two adjacent pixels. Ensure that
*  these initial pixels are not adjacent.
            IRANGE( 1 ) = LBND
            IRANGE( 2 ) = MAX( UBND, LBND + 2 )

*  Obtain the centre positions for the two limiting pixels.
 2          CONTINUE             ! Start of 'DO WHILE' loop
            CALL NDF_$P2A( 2, IRANGE, LBND, UBND, .TRUE., HAVWID,
     :                     .FALSE., CENTRE, WIDTH, VARIAN, CEN,
     :                     WID, VAR, STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Calculate the difference between the centre positions at the current
*  pixel limits and check this difference has the correct sign. Report
*  an error if it does not, as this indicates a non-monotonic variation
*  of centre position with pixel index.
            CDIFF = CEN( 2 ) - CEN( 1 )
            IF ( DINC * CDIFF .LE. 0.0D0 ) THEN
               STATUS = NDF__AXVIN
               CALL ERR_REP( 'NDF_$A2P_ERR2',
     :                       'Axis CENTRE values do not increase ' //
     :                       'or decrease monotonically.', STATUS )
               GO TO 99
            END IF

*  Loop until the search converges on two adjacent pixels.
            IF ( IRANGE( 2 ) .GT. IRANGE( 1 ) + 1 ) THEN

*  Determine if we are interpolating between the centre coordinates of
*  the two limiting pixels (as opposed to extrapolating). We are
*  interpolating only if the first pixel's coordinate lies "below" the
*  required value, while the second one does not (interpolation must be
*  achieved before convergence is assured).
               BELOW1 = DINC * ( AX( I ) - CEN( 1 ) ) .GE. 0.0D0
               BELOW2 = DINC * ( AX( I ) - CEN( 2 ) ) .GE. 0.0D0
               INTERP = BELOW1 .AND. ( .NOT. BELOW2 )

*  Interpolate (or extrapolate) linearly to estimate the pixel index
*  where the centre position matches the axis coordinate.
               INEW( 1 ) = IRANGE( 1 ) + NINT( DBLE(
     :                        IRANGE( 2 ) - IRANGE( 1 ) ) *
     :                        ( ( AX( I ) - CEN( 1 ) ) / CDIFF ) )

*  If we are extrapolating (not interpolating), then the algorithm
*  cannot converge on this iteration. Instead, we aim to reach a
*  position where we can interpolate on the next iteration (and so
*  eventually converge). Thus we aim to get one limiting pixel lying
*  "below" the required axis coordinate, and the other lying "above".
               IF ( .NOT. INTERP ) THEN

*  If the new pixel index is the same as one of the old pixel indices,
*  then widen the search range by one pixel at the appropriate end
*  (otherwise there will be no change, hence an infinite loop). Note
*  that the pixels cannot become adjacent at this point, so the
*  convergence criterion will not be met on this iteration.
                  IF ( INEW( 1 ) .EQ. IRANGE( 1 ) ) THEN
                     INEW( 1 ) = INEW( 1 ) - 1
                  ELSE IF ( INEW( 1 ) .EQ. IRANGE( 2 ) ) THEN
                     INEW( 1 ) = INEW( 1 ) + 1
                  END IF

*  Obtain the axis coordinate of the new pixel index and see if it lies
*  "below" the required value.
                  CALL NDF_$P2A( 1, INEW, LBND, UBND, .TRUE., HAVWID,
     :                           .FALSE., CENTRE, WIDTH, VARIAN, CNEW,
     :                           WNEW, VNEW, STATUS )
                  IF ( STATUS .NE. SAI__OK ) GO TO 99
                  BELOW = DINC * ( AX( I ) - CNEW( 1 ) ) .GE. 0.0D0

*  If so, and the new pixel lies below the initial search range, then
*  simply extend the lower pixel of the range. Otherwise move the upper
*  bound to this pixel and derive a new lower bound. Again note that
*  the limiting pixel indices cannot become adjacent.
                  IF ( INEW( 1 ) .LT. IRANGE( 1 ) ) THEN
                     IF ( BELOW ) THEN
                        IRANGE( 1 ) = INEW( 1 )
                     ELSE
                        IRANGE( 2 ) = INEW( 1 )
                        IRANGE( 1 ) = INEW( 1 ) - 2
                     ENDIF

*  Modify the range similarly if the new pixel lies above the initial
*  search range.
                  ELSE IF ( INEW( 1 ) .GT. IRANGE( 2 ) ) THEN
                     IF ( BELOW ) THEN
                        IRANGE( 1 ) = INEW( 1 )
                        IRANGE( 2 ) = INEW( 1 ) + 2
                     ELSE
                        IRANGE( 2 ) = INEW( 1 )
                     END IF
                  END IF

*  If we are interpolating, then convergence is now possible. To ensure
*  this, we ensure that the search range is narrowed by at least one
*  pixel on each iteration (otherwise there will be no change, hence an
*  infinite loop).
               ELSE
                  IF ( INEW( 1 ) .EQ. IRANGE( 1 ) ) THEN
                     INEW( 1 ) = INEW( 1 ) + 1
                  ELSE IF ( INEW( 1 ) .EQ. IRANGE( 2 ) ) THEN
                     INEW( 1 ) = INEW( 1 ) - 1
                  END IF

*  Obtain the new pixel centre location and see if it lies "below" the
*  required axis value.
                  CALL NDF_$P2A( 1, INEW, LBND, UBND, .TRUE., HAVWID,
     :                           .FALSE., CENTRE, WIDTH, VARIAN, CNEW,
     :                           WNEW, VNEW, STATUS )
                  IF ( STATUS .NE. SAI__OK ) GO TO 99
                  BELOW = DINC * ( AX( I ) - CNEW( 1 ) ) .GE. 0.0D0

*  Update the lower or upper pixel index limit according to the outcome
*  of the interpolation.
                  IF ( BELOW ) THEN
                     IRANGE( 1 ) = INEW( 1 )
                  ELSE
                     IRANGE( 2 ) = INEW( 1 )
                  END IF

*  Return to perform the next iteration.
               END IF
               GO TO 2
            END IF                 

*  Return the index of the pixel lying immediately "below" the axis
*  coordinate, the centre position of this pixel, and the distance to
*  the next pixel.
            IPIX0( I ) = IRANGE( 1 )
            CENT0( I ) = CEN( 1 )
            SPACE0( I ) = CEN( 2 ) - CEN( 1 )

*  Find the distance between the axis coordinate and the centre
*  positions of the two neighbouring pixels.
            DIF( 1 ) = ABS( AX( I ) - CEN( 1 ) )
            DIF( 2 ) = ABS( AX( I ) - CEN( 2 ) )

*  Determine if the axis coordinate lies inside each of these pixels.
            IN( 1 ) = ( DIF( 1 ) .LE. ( 0.5D0 * WID( 1 ) ) )
            IN( 2 ) = ( DIF( 2 ) .LE. ( 0.5D0 * WID( 2 ) ) )

*  If it lies nearer the lower pixel, but does not lie inside it and
*  instead lies inside the upper pixel, then note this.
            IF ( DIF( 1 ) .LE. DIF( 2 ) ) THEN
               IF ( ( .NOT. IN( 1 ) ) .AND. IN( 2 ) ) THEN
                  INPIX( I ) = .TRUE.
                  IPIX1( I ) = IRANGE( 2 )
                  WIDTH1( I ) = WID( 2 )
                  CENT1( I ) = CEN( 2 )

*  Otherwise, return values appropriate to the nearer (lower) pixel.
               ELSE
                  INPIX( I ) = IN( 1 )
                  IPIX1( I ) = IRANGE( 1 )
                  WIDTH1( I ) = WID( 1 )
                  CENT1( I ) = CEN( 1 )
               END IF

*  If it lies nearer the upper pixel, but does not lie inside it and
*  instead lies inside the lower pixel, then note this.
            ELSE
               IF ( ( .NOT. IN( 2 ) ) .AND. IN( 1 ) ) THEN
                  INPIX( I ) = .TRUE.
                  IPIX1( I ) = IRANGE( 1 )
                  WIDTH1( I ) = WID( 1 )
                  CENT1( I ) = CEN( 1 )

*  Otherwise, return values appropriate to the nearer (upper) pixel.
               ELSE
                  INPIX( I ) = IN( 2 )
                  IPIX1( I ) = IRANGE( 2 )
                  WIDTH1( I ) = WID( 2 )
                  CENT1( I ) = CEN( 2 )
               END IF
            END IF
 3       CONTINUE           
      END IF

*  Arrive here if an error occurs.
 99   CONTINUE     
 
      END
      SUBROUTINE NDF_$CHXNM( XNAME, STATUS )
*+
*  Name:
*     NDF_$CHXNM

*  Purpose:
*     Check an NDF extension name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_$CHXNM( XNAME, STATUS )

*  Description:
*     The routine checks the name of an NDF extension for standard
*     form. A standard name must be no more than NDF__SZXNM characters
*     long, must begin with an alphabetic character and continue with
*     alphanumeric characters (including underscore) only. If this test
*     fails, then an error is reported and a STATUS value set.
*     Otherwise, the routine returns without action.

*  Arguments:
*     XNAME = CHARACTER * ( * ) (Given)
*        The extension name to be checked.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Check that the extension name is not too long and that it has
*     the standard form. Report an error if appropriate.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-SEP-1989 (RFWS):
*        Original version.
*     26-SEP-1989 (RFWS):
*        Finished prologue.
*     23-NOV-1989 (RFWS):
*        Changed to use the NDF__SZXNM constant.
*     29-JAN-1990 (RFWS):
*        Removed checks on name registration.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      CHARACTER * ( * ) XNAME

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_ISNAM          ! Whether a string is a standard name
      INTEGER CHR_LEN            ! Significant length of string

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the extension name is too long, or does not have the correct
*  standard form, then report an error.
      IF ( ( CHR_LEN( XNAME ) .GT. NDF__SZXNM ) .OR.
     :     ( .NOT. CHR_ISNAM( XNAME ) ) ) THEN
         STATUS = NDF__NSXNM
         CALL MSG_SETC( 'XNAME', XNAME )
         CALL ERR_REP( 'NDF_$CHXNM_NS',
     :   'Non-standard extension name ''^XNAME'' specified ' //
     :   '(possible programming error).', STATUS )
      END IF
       
*  Call error tracing routine and exit.
C      IF ( STATUS .NE. SAI__OK ) CALL NDF_$TRACE( 'NDF_$CHXNM', STATUS )

      END
      SUBROUTINE NDF_$FPARX( STR, F, L )
*+
*  Name:
*     NDF_$FPARX

*  Purpose:
*     Find a parenthesised expression in a character string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_$FPARX( STR, F, L )

*  Description:
*     The routine searches the string STR to identify a sub-string
*     containing a parenthesised expression and returns the character
*     positions of the opening and closing parentheses in the F and L
*     arguments. Allowance is made for nested parentheses. If a
*     parenthesised expression was not found, then the returned value
*     of F will be greater than the returned value of L.

*  Arguments:
*     STR = CHARACTER * ( * ) (Given)
*        String to be searched.
*     F = INTEGER (Returned)
*        Character position of the opening parenthesis.
*     L = INTEGER (Returned)
*        Character position of the closing parenthesis.

*  Algorithm:
*     -  Initialise.
*     -  Inspect each character in the string to identify the opening
*     parenthesis. If found, then note its position.
*     -  If an opening parentheses was found, then search subsequent
*     characters for the closing parenthesis.
*     -  Initialise the count of nested parentheses.
*     -  Count opening parentheses.
*     -  Count closing parentheses.
*     -  If the count of nested parentheses falls to zero, then the
*     closing parentheses has been found, so note its position.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-OCT-1989 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) STR

*  Arguments Returned:
      INTEGER F
      INTEGER L

*  Local Variables:
      INTEGER I                  ! Loop counter for characters
      INTEGER IPAR               ! Count of nested parentheses
      LOGICAL FOUND              ! Whether first character found

*.

*  Initialise.
      F = 1
      L = 0
      FOUND = .FALSE.

*  Inspect each character in the string, looking for an opening
*  parenthesis.
      DO 1 I = 1, LEN( STR )
         IF ( STR( I : I ) .EQ. '(' ) THEN

*  If found, then note its position.
            FOUND = .TRUE.
            F = I
            GO TO 2
         END IF
1     CONTINUE
2     CONTINUE

*  If the start of an expression has been found, then search for the
*  end.
      IF ( FOUND ) THEN

*  Initialise the count of nested parentheses.
         IPAR = 1

*  Loop to inspect subsequent characters in the string.
         DO 3 I = F + 1, LEN( STR )

*  Count opening parentheses.
            IF ( STR( I : I ) .EQ. '(' ) THEN
               IPAR = IPAR + 1

*  Count closing parentheses.
            ELSE IF ( STR( I : I ) .EQ. ')' ) THEN
               IPAR = IPAR - 1

*  If the number of nested parentheses falls to zero, then the final
*  character of the expression has been found. Note its position.
               IF ( IPAR .EQ. 0 ) THEN
                  L = I
                  GO TO 4
               END IF
            END IF
3        CONTINUE
4        CONTINUE
      END IF

      END
      INTEGER FUNCTION NDF_$INDXP( STR, CH )
*+
*  Name:
*     NDF_$INDXP

*  Purpose:
*     Find a character in a string, ignoring characters in parentheses.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = NDF_$INDXP( STR, CH )

*  Description:
*     The function returns the position of the first occurrence of the
*     character CH in the string STR, omitting any occurrences which
*     lie within parentheses '(...)'. Account is taken of nested
*     parentheses.

*  Arguments:
*     STR = CHARACTER * ( * ) (Given)
*        String to be searched.
*     CH = CHARACTER * ( 1 ) (Given)
*        Character to be found.

*  Returned Value:
*     NDF_$INDXP = INTEGER
*        The character position of the first un-parenthesised
*        occurrence of the character CH in the string STR. A value of
*        zero is returned if no such occurrence exists.

*  Algorithm:
*     -  Initialise.
*     -  Inspect each character in STR.
*     -  If the target character is found when not inside parentheses,
*     then return its position.
*     -  Count entries into each level of parenthesis.
*     -  Decrement the count when leaving each level of parenthesis.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-OCT-1989 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) STR
      CHARACTER * ( 1 ) CH

*  Local Variables:
      INTEGER I                  ! Loop counter for characters
      INTEGER PAR                ! Depth of nested parentheses

*.

*  Initialise.
      PAR = 0
      NDF_$INDXP = 0

*  Inspect each character in STR.
      DO 1 I = 1, LEN( STR )

*  If the target character is found when not inside parentheses, then
*  return its position.
         IF ( ( STR( I : I ) .EQ. CH ) .AND. ( PAR .EQ. 0 ) ) THEN
            NDF_$INDXP = I
            GO TO 2

*  Count entries into each level of nested parenthesis.
         ELSE IF ( STR( I : I ) .EQ. '(' ) THEN
            PAR = PAR + 1

*  Decrement the count when leaving each level of parenthesis. Ignore
*  missing left parentheses.
         ELSE IF ( ( STR( I : I ) .EQ. ')' ) .AND. ( PAR .GT. 0 ) ) THEN
            PAR = PAR - 1
         END IF
1     CONTINUE
2     CONTINUE

      END
      SUBROUTINE NDF_$P2A( N, IPIX, LBND, UBND, HAVCEN, HAVWID, HAVVAR,
     :                     CENTRE, WIDTH, VARIAN, CEN, WID, VAR,
     :                     STATUS )
*+
*  Name:
*     NDF_$P2A

*  Purpose:
*     Convert pixel indices into axis centre, width and variance
*     values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_$P2A( N, IPIX, LBND, UBND, HAVCEN, HAVWID, HAVVAR,
*                    CENTRE, WIDTH, VARIAN, CEN, WID, VAR, STATUS )

*  Description:
*     This routine converts a series of pixel indices for a single NDF
*     dimension into the corresponding axis coordinate system values,
*     namely: the pixel centre, width and variance. The routine will
*     provide the correct default values if the appropriate axis arrays
*     are not available, and will also extrapolate the axis coordinate
*     system in either direction if necessary.

*  Arguments:
*     N = INTEGER (Given)
*        Number of pixel index values to be converted.
*     IPIX( N ) = INTEGER (Given)
*        Array of pixel indices to be converted.
*     LBND = INTEGER (Given)
*        Lower pixel index bound for the NDF dimension.
*     UBND = INTEGER (Given)
*        Upper pixel index bound for the NDF dimension.
*     HAVCEN = LOGICAL (Given)
*        Whether an array of pixel centre positions is available.
*     HAVWID = LOGICAL (Given)
*        Whether an array of pixel width values is available.
*     HAVVAR = LOGICAL (Given)
*        Whether an array of pixel variance values is available.
*     CENTRE( LBND : UBND ) = DOUBLE PRECISION (Given)
*        Array of pixel centre positions. This is only used if HAVCEN is
*        .TRUE..
*     WIDTH( LBND : UBND ) = DOUBLE PRECISION (Given)
*        Array of pixel width values. This is only used if both HAVCEN
*        and HAVWID are .TRUE..
*     VARIAN( LBND : UBND ) = DOUBLE PRECISION (Given)
*        Array of pixel variance values. This is only used if both
*        HAVCEN and HAVVAR are .TRUE..
*     CEN( N ) = DOUBLE PRECISION (Returned)
*        Array of centre positions for the selected pixels.
*     WID( N ) = DOUBLE PRECISION (Returned)
*        Array of width values for the selected pixels.
*     VAR( N ) = DOUBLE PRECISION (Returned)
*        Array of variance values for the selected pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine performs checks on the validity of the centre,
*     width and variance values it returns and will report an error if
*     they are invalid.

*  Algorithm:
*     -  If no pixel centre coordinates are provided, then loop to
*     calculate all the centre positions, widths and variances
*     directly.
*     -  Otherwise loop to calculate these values from the array(s)
*     provided.
*     -  First test to see if the pixel index lies below the lower
*     bound of the pixel centre array.
*     -  If so, then obtain the pixel spacing to use for extrapolation
*     from the spacing of the two nearest pixel centres, if available.
*     -  Check that the spacing is not zero; this indicates invalid
*     centre values.
*     -  Use a spacing of unity if only one centre value is available.
*     -  Extrapolate to obtain the pixel centre position.
*     -  If pixel width values have been provided, then use the width
*     of the nearest pixel, checking it for validity.
*     -  Otherwise, use the pixel spacing to generate a width value.
*     -  The extrapolated variance value is zero.
*     -  If the pixel index lies above the upper bound of the pixel
*     centre array, then obtain the pixel spacing to use for
*     extrapolation from the spacing of the two nearest pixel centres.
*     -  Check that the spacing is not zero; this indicates invalid
*     centre values.
*     -  Use a spacing of unity if only one centre value is available.
*     -  Extrapolate to obtain the pixel centre position.
*     -  If pixel width values have been provided, then use the width
*     of the nearest pixel, checking it for validity.
*     -  Otherwise, use the pixel spacing to generate a width value.
*     -  The extrapolated variance value is zero.
*     -  If the pixel index lies within the bounds of the pixel centre
*     array, then extract the appropriate centre position.
*     -  If pixel width values have been provided, then extract the
*     matching pixel width, checking it for validity.
*     -  Otherwise, calculate the width from the centre spacing of
*     neighbouring pixels. Use both neighbours if available.
*     -  Use only one neighbour if the other lies outside the bounds of
*     the pixel centre array.
*     -  Use a width value of unity if only one centre position has
*     been supplied.
*     -  If an array of variance values has been provided, then extract
*     the appropriate value, checking it for validity.
*     -  Otherwise return a variance value of zero.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     8-MAR-1991 (RFWS):
*        Original version.
*     12-MAR-1991 (RFWS):
*        Added checks for invalid returned values.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes      

*  Arguments Given:
      INTEGER N
      INTEGER IPIX( N )
      INTEGER LBND
      INTEGER UBND
      LOGICAL HAVCEN
      LOGICAL HAVWID
      LOGICAL HAVVAR
      DOUBLE PRECISION CENTRE( LBND : UBND )
      DOUBLE PRECISION WIDTH( LBND : UBND )
      DOUBLE PRECISION VARIAN( LBND : UBND )

*  Arguments Returned:
      DOUBLE PRECISION CEN( N )
      DOUBLE PRECISION WID( N )
      DOUBLE PRECISION VAR( N )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION SPACE     ! Pixel spacing for extrapolation
      INTEGER I                  ! Loop counter for pixel indices

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If no pixel centre coordinates are provided, then loop to calculate
*  all the centre positions, widths and variances directly.
      IF ( .NOT. HAVCEN ) THEN
         DO 1 I = 1, N
            CEN( I ) = DBLE( IPIX( I ) ) - 0.5D0
            WID( I ) = 1.0D0
            VAR( I ) = 0.0D0
 1       CONTINUE        

*  Otherwise loop to calculate these values from the array(s) provided.
      ELSE
         DO 2 I = 1, N

*  First test to see if the pixel index lies below the lower bound of
*  the pixel centre array.
            IF ( IPIX( I ) .LT. LBND ) THEN

*  If so, then obtain the pixel spacing to use for extrapolation from
*  the spacing of the two nearest pixel centres, if available.
               IF ( UBND .GT. LBND ) THEN
                  SPACE = CENTRE( LBND + 1 ) - CENTRE( LBND )

*  Check that the spacing is not zero; this indicates invalid centre
*  values.
                  IF ( SPACE .EQ. 0.0D0 ) THEN
                     STATUS = NDF__AXVIN
                     CALL ERR_REP( 'NDF_$P2A_ERR1',
     :                             'Axis CENTRE values do not ' //
     :                             'increase or decrease ' //
     :                             'monotonically.',
     :                             STATUS )
                     GO TO 99
                  END IF

*  Use a spacing of unity if only one centre value is available.
               ELSE
                  SPACE = 1.0D0
               END IF

*  Extrapolate to obtain the pixel centre position.
               CEN( I ) =
     :            CENTRE( LBND ) - SPACE * DBLE( LBND - IPIX( I ) )

*  If pixel width values have been provided, then use the width of the
*  nearest pixel, checking it for validity.
               IF ( HAVWID ) THEN
                  WID( I ) = WIDTH( LBND )
                  IF ( WID( I ) .LT. 0.0D0 ) THEN
                     STATUS = NDF__AXVIN
                     CALL ERR_REP( 'NDF_$P2A_ERR2',
     :                             'Invalid negative axis WIDTH ' //
     :                             'value encountered.',
     :                             STATUS )
                     GO TO 99
                  END IF

*  Otherwise, use the pixel spacing to generate a width value.
               ELSE
                  WID( I ) = ABS( SPACE )
               END IF

*  The extrapolated variance value is zero.
               VAR( I ) = 0.0D0

*  If the pixel index lies above the upper bound of the pixel centre
*  array, then obtain the pixel spacing to use for extrapolation from
*  the spacing of the two nearest pixel centres.
            ELSE IF ( IPIX( I ) .GT. UBND ) THEN
               IF ( UBND .GT. LBND ) THEN
                  SPACE = CENTRE( UBND ) - CENTRE( UBND - 1 )

*  Check that the spacing is not zero; this indicates invalid centre
*  values.
                  IF ( SPACE .EQ. 0.0D0 ) THEN
                     STATUS = NDF__AXVIN
                     CALL ERR_REP( 'NDF_$P2A_ERR3',
     :                             'Axis CENTRE values do not ' //
     :                             'increase or decrease ' //
     :                             'monotonically.',
     :                             STATUS )
                     GO TO 99
                  END IF

*  Use a spacing of unity if only one centre value is available.
               ELSE
                  SPACE = 1.0D0
               END IF

*  Extrapolate to obtain the pixel centre position.
               CEN( I ) =
     :            CENTRE( UBND ) + SPACE * DBLE( IPIX( I ) - UBND )

*  If pixel width values have been provided, then use the width of the
*  nearest pixel, checking it for validity.
               IF ( HAVWID ) THEN
                  WID( I ) = WIDTH( UBND )
                  IF ( WID( I ) .LT. 0.0D0 ) THEN
                     STATUS = NDF__AXVIN
                     CALL ERR_REP( 'NDF_$P2A_ERR4',
     :                             'Invalid negative axis WIDTH ' //
     :                             'value encountered.',
     :                             STATUS )
                     GO TO 99
                  END IF

*  Otherwise, use the pixel spacing to generate a width value.
               ELSE
                  WID( I ) = ABS( SPACE )
               END IF

*  The extrapolated variance value is zero.
               VAR( I ) = 0.0D0

*  If the pixel index lies within the bounds of the pixel centre array,
*  then extract the appropriate centre position.
            ELSE
               CEN( I ) = CENTRE( IPIX( I ) )

*  If pixel width values have been provided, then extract the matching
*  pixel width, checking it for validity.
               IF ( HAVWID ) THEN
                  WID( I ) = WIDTH( IPIX( I ) )
                  IF ( WID( I ) .LT. 0.0D0 ) THEN
                     STATUS = NDF__AXVIN
                     CALL ERR_REP( 'NDF_$P2A_ERR5',
     :                             'Invalid negative axis WIDTH ' //
     :                             'value encountered.',
     :                             STATUS )
                     GO TO 99
                  END IF

*  Otherwise, calculate the width from the centre spacing of
*  neighbouring pixels. Use both neighbours if available.
               ELSE
                  IF ( ( IPIX( I ) .GT. LBND ) .AND.
     :                 ( IPIX( I ) .LT. UBND ) ) THEN
                     WID( I ) = 0.5D0 *
     :                  ABS ( CENTRE( IPIX( I ) + 1 ) -
     :                        CENTRE( IPIX( I ) - 1 ) )

*  Use only one neighbour if the other lies outside the bounds of the
*  pixel centre array.
                  ELSE IF ( IPIX( I ) .GT. LBND ) THEN
                     WID( I ) = ABS( CENTRE( IPIX( I ) ) -
     :                               CENTRE( IPIX( I ) - 1 ) )
                  ELSE IF ( IPIX( I ) .LT. UBND ) THEN
                     WID( I ) = ABS( CENTRE( IPIX( I ) + 1 ) -
     :                               CENTRE( IPIX( I ) ) )

*  Use a width value of unity if only one centre position has been
*  supplied.
                  ELSE
                     WID( I ) = 1.0D0
                  END IF
               END IF

*  If an array of variance values has been provided, then extract the
*  appropriate value, checking it for validity.
               IF ( HAVVAR ) THEN
                  VAR( I ) = VARIAN( IPIX( I ) )
                  IF ( VAR( I ) .LT. 0.0D0 ) THEN
                     STATUS = NDF__AXVIN
                     CALL ERR_REP( 'NDF_$P2A_ERR6',
     :                             'Invalid negative axis VARIANCE ' //
     :                             'value encountered.',
     :                             STATUS )
                     GO TO 99
                  END IF

*  Otherwise return a variance value of zero.
               ELSE
                  VAR( I ) = 0.0D0
               END IF
            END IF
 2       CONTINUE        
      END IF

*  Arrive here if an error occurs.
 99   CONTINUE     
 
      END
      SUBROUTINE NDF_$PSCPX( STR, MXEXTN, EXTN, NEXTN, CPF, STATUS )
*+
*  Name:
*     NDF_$PSCPX

*  Purpose:
*     Parse an NDF component propagation expression.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_$PSCPX( STR, MXEXTN, EXTN, NEXTN, CPF, STATUS )

*  Description:
*     The routine parses an expression specifying which components of
*     an NDF are to be propagated when a new NDF is created based on an
*     existing template. The expression should contain a comma
*     separated list of component names (optionally abbreviated) or
*     component names prefixed with 'NO' (to indicate that the
*     specified component should not be propagated). By default the
*     HISTORY, LABEL and TITLE components and all extensions are
*     propagated. Named extensions may be included or excluded by
*     specifying EXTENSION( ) or NOEXTENSION( ) as one of the list
*     items with a list of the extensions to be affected contained
*     within the parentheses. The same component name may appear more
*     than once in the list, and the effects of each occurrence are
*     cumulative (i.e. the latter occurrence takes precedence).  The
*     routine returns an array of logical component propagation flags
*     and a list of the names of extensions which are not to be
*     propagated.

*  Arguments:
*     STR = CHARACTER * ( * ) (Given)
*        The expression to be parsed.
*     MXEXTN = INTEGER (Given)
*        Maximum number of names to be returned in the EXTN array (i.e.
*        the declared size of this array).
*     EXTN( MXEXTN ) = CHARACTER * ( DAT__SZNAM ) (Returned)
*        List of the names of NDF extensions which are not to be
*        propagated.
*     NEXTN = INTEGER (Returned)
*        Number of extension names returned in the EXTN array.
*     CPF( NDF__MXCPF ) = LOGICAL (Returned)
*        Array of component propagation flags. Symbolic constants are
*        defined in the include file NDF_CONST to identify the elements
*        of this array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Initialise the component propagation flags.
*     -  Initialise other variables.
*     -  Loop to process each item in the list.
*     -  Find the next item and obtain the start and end character
*     positions of the component name.
*     -  Compare the component name with each permitted value in turn,
*     allowing abbreviations. Set the appropriate component propagation
*     flag values and note if the name was recognised.
*     -  If the name did not match, then it may be an EXTENSION or
*     NOEXTENSION specification. See if it contains a parenthesised
*     expression.
*     -  If so, then check that the characters in front of the opening
*     parenthesis are correct and make an appropraite call to parse the
*     extension list inside the parentheses.
*     -  If the list item was not recognised at all, then report an
*     error.
*     -  Increment the pointer to the start of the next item in the list
*     and return to process it.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-OCT-1989 (RFWS):
*        Original version.
*     22-FEB-1990 (RFWS):
*        Changed to prevent the UNITS component being propagated by
*        default.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS DAT constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      CHARACTER * ( * ) STR
      INTEGER MXEXTN

*  Arguments Returned:
      CHARACTER * ( DAT__SZNAM ) EXTN( MXEXTN )
      INTEGER NEXTN
      LOGICAL CPF( NDF__MXCPF )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER NDF_$INDXP         ! Position of unparenthesised character
      LOGICAL NDF_$SIMLR         ! String compare with abbreviation

*  Local Variables:
      INTEGER I1                 ! Character position of start of item
      INTEGER I2                 ! Character position of end of item
      INTEGER F                  ! Position of start of name to test
      INTEGER L                  ! Position of end of name to test
      INTEGER J1                 ! Position of opening parenthesis
      INTEGER J2                 ! Position of closing parenthesis
      LOGICAL RECOG              ! Whether item was recognised

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the component propagation flags.
      CPF( NDF__ACPF ) = .FALSE.
      CPF( NDF__DCPF ) = .FALSE.
      CPF( NDF__HCPF ) = .TRUE.
      CPF( NDF__LCPF ) = .TRUE.
      CPF( NDF__QCPF ) = .FALSE.
      CPF( NDF__TCPF ) = .TRUE.
      CPF( NDF__UCPF ) = .FALSE.
      CPF( NDF__VCPF ) = .FALSE.
      
*  Initialise the count of excluded extensions.
      NEXTN = 0

*  Initialise a pointer to the start of the "current" item in the
*  component list.
      I1 = 1

*  Loop to process each item in the list.
1     CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( ( I1 .LE. LEN( STR ) ) .AND. ( STATUS .EQ. SAI__OK ) ) THEN

*  Find the end of the current item (the last character before the next
*  unparenthesised comma or end of string).
         I2 = NDF_$INDXP( STR( I1 : ), ',' )
         IF ( I2 .EQ. 0 ) THEN
            I2 = LEN( STR )
         ELSE
            I2 = I2 + I1 - 2
         END IF

*  Find the first and last characters in the item (excluding surrounding
*  blanks).
         IF ( I1 .LE. I2 ) THEN
            CALL CHR_FANDL( STR( I1 : I2 ), F, L )

*  Check the item is not completely blank.
            IF ( F .LE. L ) THEN
               F = F + I1 - 1
               L = L + I1 - 1

*  Compare the item with each permitted value in turn, allowing
*  abbreviation. Set the appropriate component propagation flag values
*  and note if the item is recognised.
               RECOG = .FALSE.

*  ...AXIS.
               IF ( NDF_$SIMLR( STR( F : L ), 'AXIS',
     :                          NDF__MINAB ) ) THEN
                  CPF( NDF__ACPF ) = .TRUE.
                  RECOG = .TRUE.

*  ...NOAXIS.
               ELSE IF ( NDF_$SIMLR( STR( F : L ), 'NOAXIS',
     :                               NDF__MINAB + 2 ) ) THEN
                  CPF( NDF__ACPF ) = .FALSE.
                  RECOG = .TRUE.

*  ...DATA.
               ELSE IF ( NDF_$SIMLR( STR( F : L ), 'DATA',
     :                               NDF__MINAB ) ) THEN
                  CPF( NDF__DCPF ) = .TRUE.
                  RECOG = .TRUE.

*  ...NODATA.
               ELSE IF ( NDF_$SIMLR( STR( F : L ), 'NODATA',
     :                               NDF__MINAB + 2 ) ) THEN
                  CPF( NDF__DCPF ) = .FALSE.
                  RECOG = .TRUE.

*  ...HISTORY.
               ELSE IF ( NDF_$SIMLR( STR( F : L ), 'HISTORY',
     :                               NDF__MINAB ) ) THEN
                  CPF( NDF__HCPF ) = .TRUE.
                  RECOG = .TRUE.

*  ...NOHISTORY.
               ELSE IF ( NDF_$SIMLR( STR( F : L ), 'NOHISTORY',
     :                               NDF__MINAB + 2 ) ) THEN
                  CPF( NDF__HCPF ) = .FALSE.
                  RECOG = .TRUE.

*  ...LABEL.
               ELSE IF ( NDF_$SIMLR( STR( F : L ), 'LABEL',
     :                               NDF__MINAB ) ) THEN
                  CPF( NDF__LCPF ) = .TRUE.
                  RECOG = .TRUE.

*  ...NOLABEL.
               ELSE IF ( NDF_$SIMLR( STR( F : L ), 'NOLABEL',
     :                               NDF__MINAB + 2 ) ) THEN
                  CPF( NDF__LCPF ) = .FALSE.
                  RECOG = .TRUE.

*  ...QUALITY.
               ELSE IF ( NDF_$SIMLR( STR( F : L ), 'QUALITY',
     :                               NDF__MINAB ) ) THEN
                  CPF( NDF__QCPF ) = .TRUE.
                  RECOG = .TRUE.

*  ...NOQUALITY.
               ELSE IF ( NDF_$SIMLR( STR( F : L ), 'NOQUALITY',
     :                               NDF__MINAB + 2 ) ) THEN
                  CPF( NDF__QCPF ) = .FALSE.
                  RECOG = .TRUE.

*  ...TITLE.
               ELSE IF ( NDF_$SIMLR( STR( F : L ), 'TITLE',
     :                               NDF__MINAB ) ) THEN
                  CPF( NDF__TCPF ) = .TRUE.
                  RECOG = .TRUE.

*  ...NOTITLE.
               ELSE IF ( NDF_$SIMLR( STR( F : L ), 'NOTITLE',
     :                               NDF__MINAB + 2 ) ) THEN
                  CPF( NDF__TCPF ) = .FALSE.
                  RECOG = .TRUE.

*  ...UNITS.
               ELSE IF ( NDF_$SIMLR( STR( F : L ), 'UNITS',
     :                               NDF__MINAB ) ) THEN
                  CPF( NDF__UCPF ) = .TRUE.
                  RECOG = .TRUE.

*  ...NOUNITS.
               ELSE IF ( NDF_$SIMLR( STR( F : L ), 'NOUNITS',
     :                               NDF__MINAB + 2 ) ) THEN
                  CPF( NDF__UCPF ) = .FALSE.
                  RECOG = .TRUE.

*  ...VARIANCE.
               ELSE IF ( NDF_$SIMLR( STR( F : L ), 'VARIANCE',
     :                               NDF__MINAB ) ) THEN
                  CPF( NDF__VCPF ) = .TRUE.
                  RECOG = .TRUE.

*  ...NOVARIANCE.
               ELSE IF ( NDF_$SIMLR( STR( F : L ), 'NOVARIANCE',
     :                               NDF__MINAB + 2 ) ) THEN
                  CPF( NDF__VCPF ) = .FALSE.
                  RECOG = .TRUE.

*  If the item did not match any of the above, then it may be an
*  EXTENSION specification, followed by a parenthesised list of
*  extension names. Search for a parenthesesed expression.
               ELSE
                  CALL NDF_$FPARX( STR( F : L ), J1, J2 )

*  If found, then test the characters lying in front of the opening
*  parenthesis (if there are any).
                  IF ( J1 .LE. J2 ) THEN
                     J1 = J1 + F - 1
                     J2 = J2 + F - 1
                     IF ( J1 .GT. F ) THEN
                        IF ( NDF_$SIMLR( STR( F : J1 - 1 ), 'EXTENSION',
     :                                   NDF__MINAB ) ) THEN

*  If this is an EXTENSION specification, then update the list of
*  excluded extensions accordingly.
                           RECOG = .TRUE.
                           IF ( J1 + 1 .LE. J2 - 1 ) THEN
                              CALL NDF_$PXLST( .TRUE.,
     :                                         STR( J1 + 1 : J2 - 1 ),
     :                                         MXEXTN, EXTN, NEXTN,
     :                                         STATUS )
                           END IF

*  Perform the appropriate updating operation if this is a NOEXTENSION
*  specification.
                        ELSE IF ( NDF_$SIMLR( STR( F : J1 - 1 ),
     :                                        'NOEXTENSION',
     :                                        NDF__MINAB + 2 ) ) THEN
                           RECOG = .TRUE.
                           IF ( J1 + 1 .LE. J2 - 1 ) THEN
                              CALL NDF_$PXLST( .FALSE.,
     :                                         STR( J1 + 1 : J2 - 1 ),
     :                                         MXEXTN, EXTN, NEXTN,
     :                                         STATUS )
                           END IF
                        END IF
                     END IF
                  END IF
               END IF

*  If the list item was not ecognised, then report an error.
               IF ( .NOT. RECOG ) THEN
                  STATUS = NDF__CNMIN
                  CALL MSG_SETC( 'BADCOMP', STR( F : L ) )
                  CALL ERR_REP( 'NDF_$PSCPX_BAD',
     :                          'Invalid component name ' //
     :                          '''^BADCOMP'' specified (possible ' //
     :                          'programming error).', STATUS )
               END IF
            END IF
         END IF

*  Increment the pointer to the start of the next list item and return
*  to process it.
         I1 = I2 + 2
         GO TO 1
      END IF
       
*  Call error tracing routine and exit.
C      IF ( STATUS .NE. SAI__OK ) CALL NDF_$TRACE( 'NDF_$PSCPX', STATUS )

      END
      SUBROUTINE NDF_$PSNDB( STR, DEF, VALUE, ISPIX, STATUS )
*+
*  Name:
*     NDF_$PSNDB

*  Purpose:
*     Parse an NDF dimension bound.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_$PSNDB( STR, DEF, VALUE, ISPIX, STATUS )

*  Description:
*     The routine parses a string representing an upper or lower
*     dimension bound of an NDF section. If the string is blank, then a
*     default value is returned. Leading and trailing spaces are
*     ignored.

*  Arguments:
*     STR = CHARACTER * ( * ) (Given)
*        String to be parsed.
*     DEF = DOUBLE PRECISION (Given)
*        Default value to be returned if the string is blank.
*     VALUE = DOUBLE PRECISION (Returned)
*        Dimension bound value.
*     ISPIX = LOGICAL (Returned)
*        Whether the value returned is to be interpreted as a
*        pixel-index or an axis coordinate value; .TRUE. ==> pixel
*        index, .FALSE. ==> axis value. (The value is returned .TRUE.
*        if an integer format number is found and .TRUE. if floating
*        point. A value of .TRUE. is returned if the string is blank.)
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Find the first and last non-blank characters in the string.
*     -  If the input string is blank, then return the default value.
*     -  Otherwise, attempt to convert the string to a double precision
*     value.
*     -  If the attempt fails, then report an error message.
*     -  Otherwise, determine whether integer format was used.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     15-FEB-1991 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      CHARACTER * ( * ) STR
      DOUBLE PRECISION DEF

*  Arguments Returned:
      DOUBLE PRECISION VALUE
      LOGICAL ISPIX

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER F                  ! Position of first non-blank character
      INTEGER L                  ! Position of last non-blank character

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the first and last non-blank characters in the string.
      CALL CHR_FANDL( STR, F, L )

*  If the input string is blank, then return the default value.
      IF ( F .GT. L ) THEN
         VALUE = DEF
         ISPIX = .TRUE.

*  Otherwise, attempt to convert the string to a double precision
*  value.
      ELSE
         CALL CHR_CTOD( STR( F : L ), VALUE, STATUS )

*  If the attempt fails, then report an error message.
         IF ( STATUS .NE. SAI__OK ) THEN
            STATUS = NDF__BNDIN
            CALL MSG_SETC( 'BADBOUND', STR( F : L ) )
            CALL ERR_REP( 'NDF_$PSNDB_SYN',
     :                    'Invalid NDF dimension bound ' //
     :                    '''^BADBOUND'' specified; bad syntax.',
     :                    STATUS )

*  Otherwise, determine whether integer format was used.
         ELSE
            ISPIX = ( ( INDEX( STR( F : L ), '.' ) .EQ. 0 ) .AND.
     :                ( INDEX( STR( F : L ), 'E' ) .EQ. 0 ) .AND.
     :                ( INDEX( STR( F : L ), 'e' ) .EQ. 0 ) .AND.
     :                ( INDEX( STR( F : L ), 'D' ) .EQ. 0 ) .AND.
     :                ( INDEX( STR( F : L ), 'd' ) .EQ. 0 ) )
         END IF
      END IF
       
      END
      SUBROUTINE NDF_$PSNDE( STR, NDIM, LBND, UBND, VALUE1, VALUE2,
     :                       NVAL, ISPIX1, ISPIX2, ISBND, STATUS )
*+
*  Name:
*     NDF_$PSNDE

*  Purpose:
*     Parse an NDF dimension bounds expression.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_$PSNDE( STR, NDIM, LBND, UBND, VALUE1, VALUE2, NVAL,
*                      ISPIX1, ISPIX2, ISBND, STATUS )

*  Description:
*     The routine parses an NDF section bound expression (such as
*     '1:10,2', '3:,,~7' or '31~5,,6') and returns two values
*     specifying the section's bounds in each dimension, together with
*     additional information specifying how the bounds should be
*     calculated from the returned values. Suitable defaults are used
*     where appropriate.

*  Arguments:
*     STR = CHARACTER * ( * ) (Given)
*        String containing the expression to be parsed.
*     NDIM = INTEGER (Given)
*        Number of NDF bounds.
*     LBND( NDIM ) = INTEGER (Given)
*        NDF lower bounds (used to calculate defaults).
*     UBND( NDIM ) = INTEGER (Given)
*        NDF upper bounds (used to calculate defaults).
*     VALUE1( NDF__MXDIM ) = DOUBLE PRECISION (Returned)
*        First value specifying section bounds for each dimension.
*     VALUE2( NDF__MXDIM ) = DOUBLE PRECISION (Returned)
*        Second value specifying section bounds for each dimension.
*     NVAL = INTEGER (Returned)
*        Number of dimensions for which values are returned (cannot
*        exceed NDF__MXDIM).
*     ISPIX1( NDF__MXDIM ) = LOGICAL (Returned)
*        .FALSE. ==> the corresponding VALUE1 value is to be
*        interpreted as an axis coordinate value, .TRUE. ==> it is a
*        pixel index.
*     ISPIX2( NDF__MXDIM ) = LOGICAL (Returned)
*        .FALSE. ==> the corresponding VALUE2 value is to be
*        interpreted as an axis coordinate value, .TRUE. ==> it is a
*        pixel index.
*     ISBND( NDF__MXDIM ) = LOGICAL (Returned)
*        .TRUE. ==> the corresponding VALUE1 and VALUE2 values specify
*        the lower and upper bounds of the section directly, .FALSE.
*        ==> VALUE1 specifies the centre of the dimension's extent and
*        VALUE2 specifies the dimension's size.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The number of dimension bounds implied by the expression
*     supplied (one more than the number of separating commas which it
*     contains) must not exceed NDF__MXDIM. An error will be reported
*     if it does. It need not match the number of NDF dimensions
*     supplied.

*  Algorithm:
*     -  Initialise.
*     -  Loop to extract each dimension bound field from the
*     expression.
*     -  If we are still within the bounds of the expression string,
*     then search for the end of the next field (the last character
*     before a comma or end of string). Note if a comma did not
*     terminate this field.
*     -  If we are outside the bounds of the expression, but have to
*     make one more pass to process the (blank) field following a final
*     comma, then use the end of string as the end of the field.
*     -  Increment the count of dimension bounds and report an error if
*     this exceeds the maximum number of dimensions.
*     -  Set up values of the NDF lower and upper bounds for the
*     current dimension.
*     -  If the field does not exist (i.e. there are two consecutive
*     commas or a comma at the start or end of the string) then use the
*     default values for the current dimension.
*     -  Otherwise, find the first and last non-blank characters in the
*     current dimension field.
*     -  If the field is blank, then apply the default bounds.
*     -  Otherwise, parse the field to determine the values which
*     specify the dimension bounds.
*     -  Make a contextual error report if an error occurs.
*     -  Increment the pointer to the start of the next field and
*     return to process it.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     15-FEB-1991 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants      
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      CHARACTER * ( * ) STR
      INTEGER NDIM
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )

*  Arguments Returned:
      DOUBLE PRECISION VALUE1( NDF__MXDIM )
      DOUBLE PRECISION VALUE2( NDF__MXDIM )
      INTEGER NVAL
      LOGICAL ISPIX1( NDF__MXDIM )
      LOGICAL ISPIX2( NDF__MXDIM )
      LOGICAL ISBND( NDF__MXDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER F                  ! First non-blank character in field
      INTEGER I1                 ! First character position in field
      INTEGER I2                 ! Last character position in field
      INTEGER L                  ! Last non-blank character in field
      INTEGER LBND0              ! Default lower dimension bound
      INTEGER UBND0              ! Default upper dimension bound
      LOGICAL COMMA              ! Comma terminated a field?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      NVAL = 0
      I1 = 1
      COMMA = .TRUE.

*  Loop to extract each dimension bound field from the expression.
 1    CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( ( STATUS .EQ. SAI__OK ) .AND. COMMA ) THEN

*  If we are still within the bounds of the expression string, then
*  search for the end of the next field (the last character before a
*  comma or end of string). Note if a comma did not terminate this
*  field.
         IF ( I1 .LE. LEN( STR ) ) THEN
            I2 = INDEX( STR( I1 : ) , ',' )
            IF ( I2 .EQ. 0 ) THEN
               I2 = LEN( STR )
               COMMA = .FALSE.
            ELSE
               I2 = I2 + I1 - 2
            END IF

*  If we are outside the bounds of the expression, but have to make one
*  more pass to process the (blank) field following a final comma, then
*  use the end of string as the end of the field.
         ELSE
            I2 = LEN( STR )
            COMMA = .FALSE.
         END IF

*  Increment the count of dimension bounds and report an error if this
*  exceeds the maximum number of dimensions.
         NVAL = NVAL + 1
         IF ( NVAL .GT. NDF__MXDIM ) THEN
            STATUS = NDF__BNDIN
            CALL MSG_SETC( 'SECTION', STR )
            CALL MSG_SETI( 'NDIM', NVAL )
            CALL MSG_SETI( 'MXDIM', NDF__MXDIM )
            CALL ERR_REP( 'NDF_$PSNDE_XS',
     :                    'Too many dimensions given in the NDF ' //
     :                    'section expression ''(^SECTION)''; the ' //
     :                    'maximum number of NDF dimensions is ^MXDIM.',
     :                    STATUS )

*  Set up values of the NDF lower and upper bounds for the current
*  dimension.
         ELSE
            IF ( NVAL .LE. NDIM ) THEN
               LBND0 = LBND( NVAL )
               UBND0 = UBND( NVAL )
            ELSE
               LBND0 = 1
               UBND0 = 1
            END IF

*  If the field does not exist (i.e. there are two consecutive commas
*  or a comma at the start or end of the string) then use the default
*  values for the current dimension.
            IF ( I1 .GT. I2 ) THEN
               VALUE1( NVAL ) = DBLE( LBND0 )
               VALUE2( NVAL ) = DBLE( UBND0 )
               ISPIX1( NVAL ) = .TRUE.
               ISPIX2( NVAL ) = .TRUE.
               ISBND( NVAL ) = .TRUE.

*  Otherwise, find the first and last non-blank characters in the
*  current dimension field.
            ELSE
               CALL CHR_FANDL( STR( I1 : I2 ), F, L )

*  If the field is blank, then apply the default bounds.
               IF ( F .GT. L ) THEN
                  VALUE1( NVAL ) = DBLE( LBND0 )
                  VALUE2( NVAL ) = DBLE( UBND0 )
                  ISPIX1( NVAL ) = .TRUE.
                  ISPIX2( NVAL ) = .TRUE.
                  ISBND( NVAL ) = .TRUE.

*  Otherwise, parse the field to determine the values which specify the
*  dimension bounds.
               ELSE
                  F = F + I1 - 1
                  L = L + I1 - 1
                  CALL NDF_$PSNDF( STR( F : L ), LBND0, UBND0,
     :                             VALUE1( NVAL ), VALUE2( NVAL ),
     :                             ISPIX1( NVAL ), ISPIX2( NVAL ),
     :                             ISBND( NVAL ), STATUS )

*  Make a contextual error report if an error occurs.
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL MSG_SETI( 'NBND', NVAL )
                     CALL MSG_SETC( 'SECTION', STR )
                     CALL ERR_REP( 'NDF_$PSNDE_ERR',
     :                             'Error in dimension ^NBND of the ' //
     :                             'NDF section expression ' //
     :                             '''(^SECTION)''.', STATUS )
                  END IF
               END IF
            END IF
         END IF

*  Increment the pointer to the start of the next field and return to
*  process it.
         I1 = I2 + 2
         GO TO 1
      END IF     

      END
      SUBROUTINE NDF_$PSNDF( STR, LBND, UBND, VALUE1, VALUE2, ISPIX1,
     :                       ISPIX2, ISBND, STATUS )
*+
*  Name:
*     NDF_$PSNDF

*  Purpose:
*     Parse an NDF dimension bound field.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_$PSNDF( STR, LBND, UBND, VALUE1, VALUE2, ISPIX1, ISPIX2,
*                      ISBND, STATUS )

*  Description:
*     The routine parses a dimension bound field for an NDF to
*     determine two values which specify the bounds for a dimension
*     when selecting an NDF section. The lower and upper bounds may be
*     separated in the normal way by a colon (e.g. '10:20'), or by '~'
*     (e.g. '31~10'). The latter indicates that the bounds should be
*     centred on the first value and have a dimension size equal to the
*     second value. Suitable default values are returned if either or
*     both halves of the field are omitted (e.g. '100:', ':100', ':',
*     '~15', '33~' etc.). If no field separator is present, then the
*     upper bound is set to equal the lower bound (unless the string is
*     blank, which is equivalent to ':'). If the values of bounds are
*     supplied using integer format, then they are interpreted as pixel
*     indices. If floating-point format is used (including
*     double-precision) then they are interpreted as axis coordinate
*     system values.

*  Arguments:
*     STR = CHARACTER * ( * ) (Given)
*        The string to be parsed.
*     LBND = INTEGER (Given)
*        The NDF's lower pixel-index bound, to be used as the default
*        lower bound.
*     UBND = INTEGER (Given)
*        The NDF's upper pixel-index bound, to be used as the default
*        upper bound.
*     VALUE1 = DOUBLE PRECISION (Returned)
*        First value specifying the dimension bounds.
*     VALUE2 = DOUBLE PRECISION (Returned)
*        Second value specifying the dimension bounds.
*     ISPIX1 = LOGICAL (Returned)
*        Whether VALUE1 is a pixel index (as opposed to an axis value).
*     ISPIX2 = LOGICAL (Returned)
*        Whether VALUE2 is a pixel index (as opposed to an axis value).
*     ISBND = LOGICAL (Returned)
*        Whether VALUE1 and VALUE2 specify the lower and upper bounds
*        directly (i.e. .TRUE. ==> a ':' separator was given or
*        implied, whereas .FALSE. ==> a '~' separator was given).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The values obtained by parsing the string are not constrained
*     to lie within the NDF bounds. The lower bound returned may also
*     exceed the upper bound.

*  Algorithm:
*     -  Find the first and last non-blank characters in the string.
*     -  If the string is blank, then return the default field values
*     (a ':' separator being implied).
*     -  Otherwise, locate the separator between the two values.
*     -  Determine if the separator is a ':' either explicitly or by
*     implication.
*     -  Set up suitable defaults for each value, depending on which
*     separator was found.
*     -  If the separator appears at the start, then use the default
*     first value.
*     -  Otherwise, parse the string in front of the separator to
*     obtain the first bound, supplying the appropriate default.
*     -  If there is no separator present, then the second value equals
*     the first value.
*     -  Otherwise, if the separator appears at the end of the string,
*     then use the default second value.
*     -  Otherwise, parse the string which follows the separator to
*     determine the second value.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     15-FEB-1991 (RFWS):
*        Original version.
*     25-FEB-1991 (RFWS):
*        Fixed error in substring limits.
*     14-MAR-1991 (RFWS):
*        Added check that dimension extents are at least 1 pixel.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      CHARACTER * ( * ) STR
      INTEGER LBND
      INTEGER UBND

*  Arguments Returned:
      DOUBLE PRECISION VALUE1
      DOUBLE PRECISION VALUE2
      LOGICAL ISPIX1
      LOGICAL ISPIX2
      LOGICAL ISBND

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION DEF1      ! Default first value
      DOUBLE PRECISION DEF2      ! Default second value
      INTEGER F                  ! Position of first non-blank character
      INTEGER ISEP               ! Character position of separator
      INTEGER L                  ! Position of last non-blank character

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the first and last non-blank characters in the string.
      CALL CHR_FANDL( STR, F, L )

*  If the string is blank, then return the default field values (a ':'
*  separator being implied).
      IF ( F .GT. L ) THEN
         VALUE1 = DBLE( LBND )
         VALUE2 = DBLE( UBND )
         ISPIX1 = .TRUE.
         ISPIX2 = .TRUE.
         ISBND = .TRUE.

*  Otherwise, locate the separator between the two values.
      ELSE
         ISEP = INDEX( STR, ':' )
         IF ( ISEP .EQ. 0 ) ISEP = INDEX( STR, '~' )

*  Determine if the separator is a ':' either explicitly or by
*  implication.
         ISBND = .TRUE.
         IF ( ISEP .NE. 0 ) THEN
            ISBND = ( STR( ISEP : ISEP ) .EQ. ':' )
         ELSE
            ISEP = LEN( STR ) + 1
         END IF

*  Set up suitable defaults for each value, depending on which separator
*  was found.
         IF ( ISBND ) THEN
            DEF1 = DBLE( LBND )
            DEF2 = DBLE( UBND )
         ELSE
            DEF1 = DBLE( ( LBND + UBND ) / 2 )
            DEF2 = DBLE( UBND - LBND + 1 )
         END IF

*  If the separator appears at the start, then use the default first
*  value.
         IF ( ISEP .LE. F ) THEN
            VALUE1 = DEF1
            ISPIX1 = .TRUE.

*  Otherwise, parse the string in front of the separator to obtain the
*  first bound, supplying the appropriate default.
         ELSE
            CALL NDF_$PSNDB( STR( F : ISEP - 1 ), DEF1, VALUE1, ISPIX1,
     :                       STATUS )
         END IF

*  If there is no separator present, then the second value equals the
*  first value.
         IF ( ISEP .GT. L ) THEN
            VALUE2 = VALUE1
            ISPIX2 = ISPIX1

*  Otherwise, if the separator appears at the end of the string, then
*  use the default second value.
         ELSE IF ( ISEP .EQ. L ) THEN
            VALUE2 = DEF2
            ISPIX2 = .TRUE.

*  Otherwise, parse the string which follows the separator to determine
*  the second value.
         ELSE
            CALL NDF_$PSNDB( STR( ISEP + 1 : L ), DEF2, VALUE2, ISPIX2,
     :                       STATUS )
         END IF
      END IF

*  If no error has occurred and the second value obtained specifies the
*  extent of the dimension (rather than its upper bound), then check
*  that this extent is not negative.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( .NOT. ISBND ) THEN

*  If the extent is in pixels, then it must be positive.
            IF ( ISPIX2 .AND. ( NINT( VALUE2 ) .LE. 0 ) ) THEN
               STATUS = NDF__BNDIN
               CALL ERR_REP( 'NDF_$PSNDF_PEXT',
     :                       'Invalid dimension extent specified; ' //
     :                       'a positive number of pixels is required.',
     :                       STATUS )

*  If the extent is in axis units, then we can also allow a value of
*  zero (which translates into an extent of one pixel).
            ELSE IF ( ( .NOT. ISPIX2 ) .AND.
     :                ( VALUE2 .LT. 0.0D0 ) ) THEN
               STATUS = NDF__BNDIN
               CALL ERR_REP( 'NDF_$PSNDF_AEXT',
     :                       'Invalid dimension extent specified; ' //
     :                       'value must not be negative.',
     :                       STATUS )
            END IF
         END IF
      END IF

      END
      SUBROUTINE NDF_$PXLST( INCLUD, STR, MXEXTN, EXTN, NEXTN, STATUS )
*+
*  Name:
*     NDF_$PXLST

*  Purpose:
*     Parse an extension name list.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_$PXLST( INCLUD, STR, MXEXTN, EXTN, NEXTN, STATUS )

*  Description:
*     The routine parses a list of NDF extension names, extracting each
*     name from a comma separated list supplied and filling a character
*     array with validated extension names which specify which
*     extensions to exclude from an NDF copying operation. The comma
*     separated list may specify names for EXCLUSION (i.e. extensions
*     not to be copied) or INCLUSION (i.e. extensions to be copied,
*     over-riding a previous inclusion).

*  Arguments:
*     INCLUD = LOGICAL (Given)
*        Whether the extensions specified in the list supplied are to
*        be included (as opposed to excluded) from an NDF copying
*        operation.
*     STR = CHARACTER * ( * ) (Given)
*        The comma separated list of extension names.
*     MXEXTN = INTEGER (Given)
*        The maximum number of extensions which can be excluded (the
*        declared size of the EXTN array).
*     EXTN( MXEXTN ) = CHARACTER * ( DAT__SZNAM ) (Given and Returned)
*        The list of extensions to be excluded from an NDF copying
*        operation. This list may contain initial entries and will be
*        updated.
*     NEXTN = INTEGER (Given and Returned)
*        The number of significant elements in the EXTN array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Initialise pointers to the position of the "current" list
*     element.
*     -  Loop to identify each element in the list.
*     -  Find the next element and check it is not blank.
*     -  Extract the extension name, validate it, and convert to upper
*     case.
*     -  Search the existing excluded extension list to see if the name
*     is already there.
*     -  If it is there and extensions are being INCLUDED, then remove
*     the name from the list. Move following names down to close the
*     gap and decrement the count of excluded extensions.
*     -  If it is not there and extensions are being EXCLUDED, then add
*     the name to the excluded extension list, first checking that the
*     list will not overflow and reporting an error if it will.
*     -  Increment the pointer to the start of the next input list
*     element and return to process it.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-OCT-1989 (RFWS):
*        Original version.
*     2-JAN-1991 (RFWS):
*        Fixed illegal string concatenation problem.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS DAT constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      LOGICAL INCLUD
      CHARACTER * ( * ) STR
      INTEGER MXEXTN

*  Arguments Given and Returned:
      CHARACTER * ( DAT__SZNAM ) EXTN( MXEXTN )
      INTEGER NEXTN

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZNAM ) NAME ! Extension name
      INTEGER F                  ! First character position of name
      INTEGER I                  ! Loop counter for list entries
      INTEGER I1                 ! Position of start of list element
      INTEGER I2                 ! Position of end of list element
      INTEGER L                  ! Last character position of name
      INTEGER POSN               ! List position of pre-existing name
      LOGICAL THERE              ! Whether a name is in the list

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise a pointer to the character position of the start of the
*  "current" extension name.
      I1 = 1

*  Loop to identify each element in the extension name list.
1     CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( ( I1 .LE. LEN( STR ) ) .AND. ( STATUS .EQ. SAI__OK ) ) THEN

*  Find the end of the next extension name (the character before the
*  next comma or end of string).
         I2 = INDEX( STR( I1 : ), ',' )
         IF ( I2 .EQ. 0 ) THEN
            I2 = LEN( STR )
         ELSE
            I2 = I2 + I1 - 2
         END IF

*  If the next name was found, then find the first and last characters
*  in the name (excluding surrounding spaces).
         IF ( I1 .LE. I2 ) THEN
            CALL CHR_FANDL( STR( I1 : I2 ), F, L )

*  Check that the name is not all blank.
            IF ( F .LE. L ) THEN
               F = F + I1 - 1
               L = L + I1 - 1

*  Check the name for validity.
               CALL NDF_$CHXNM( STR( F : L ), STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  Extract the name and convert to upper case.
                  NAME = STR( F : L )
                  CALL CHR_UCASE( NAME )

*  Search the existing list of excluded extensions to see if this name
*  is already there.
                  THERE = .FALSE.
                  DO 2 I = 1, NEXTN
                     IF ( EXTN( I ) .EQ. NAME ) THEN
                        THERE = .TRUE.
                        POSN = I
                        GO TO 3
                     END IF
2                 CONTINUE
3                 CONTINUE

*  If present, and the name specifies an extension to be included, then
*  the name must be removed from the list.
                  IF ( THERE ) THEN
                     IF ( INCLUD ) THEN

*  Move following names in the list down to close the gap which this
*  name leaves and decrement the excluded extension count.
                        DO 4 I = POSN, NEXTN - 1
                           EXTN( I ) = EXTN( I + 1 )
4                       CONTINUE
                        NEXTN = NEXTN - 1
                     END IF

*  If the name is not in the list and it specifies an extension to be
*  excluded, then check that the list will not overflow. Report an error
*  if it will.
                  ELSE
                     IF ( .NOT. INCLUD ) THEN
                        IF ( NEXTN .GE. MXEXTN ) THEN
                           STATUS = NDF__XSEXT
                           CALL MSG_SETI( 'MXEXTN', MXEXTN )
                           CALL ERR_REP( 'NDF_$PXLST_XS',
     :                     'The maximum number of extension names ' //
     :                     '(^MXEXTN) has been exceeded.', STATUS )

*  Increment the excluded extension name count and store the new name in
*  the excluded extension list.
                        ELSE
                           NEXTN = NEXTN + 1
                           EXTN( NEXTN ) = NAME
                        END IF
                     END IF
                  END IF
               END IF
            END IF
         END IF

*  Increment the pointer to the start of the next element in the input
*  string and return to process it.
         I1 = I2 + 2
         GO TO 1
      END IF
       
*  Call error tracing routine and exit.
C      IF ( STATUS .NE. SAI__OK ) CALL NDF_$TRACE( 'NDF_$PXLST', STATUS )

      END
      LOGICAL FUNCTION NDF_$SIMLR( STR1, STR2, N )
*+
*  Name:
*     NDF_$SIMLR

*  Purpose:
*     Case insensitive string comparison, permitting abbreviation.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = NDF_$SIMLR( STR1, STR2, N )

*  Description:
*     The function returns a logical result indicating whether two
*     strings are the same apart from case. In assessing this, the
*     first string is allowed to be an abbreviation of the second
*     string, so long as it contains a specified minimum number of
*     characters.

*  Arguments:
*     STR1 = CHARACTER * ( * ) (Given)
*        The first string, which may be an abbreviation.
*     STR2 = CHARACTER * ( * ) (Given)
*        The second string.
*     N = INTEGER (Given)
*        The minimum number of characters to which the first string may
*        be abbreviated (although a smaller number will be accepted if
*        there are actually fewer than N characters in STR2).

*  Returned Value:
*     NDF_$SIMLR = LOGICAL
*        Whether the two strings match after allowing for case and
*        abbreviation of the first string to no less than N characters.

*  Algorithm:
*     -  Find how many characters there are in STR1, ignoring trailing
*     blanks, but using at least 1 character.
*     -  Determine how many characters from STR2 to compare it with.
*     -  Compare the selected regions of each string, ignoring case.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-SEP-1989 (RFWS):
*        Original version.
*     26-SEP-1989 (RFWS):
*        Minor spelling corrections.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) STR1
      CHARACTER * ( * ) STR2
      INTEGER N

*  External References:
      INTEGER CHR_LEN            ! Significant string length
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*  Local Variables:
      INTEGER L1                 ! No. characters to use from STR1
      INTEGER L2                 ! No. characters to use from STR2

*.

*  Find the number of characters in STR1, ignoring trailing blanks, but
*  using at least 1 character.
      L1 = MAX( 1, CHR_LEN( STR1 ) )

*  Find the number of characters from STR2 to compare with STR1. This
*  must include at least N characters (or more if present in STR1), but
*  cannot exceed the length of STR2.
      L2 = MIN( MAX( L1, N ), LEN( STR2 ) )

*  Compare the selected parts of the two strings, ignoring case.
      NDF_$SIMLR = CHR_SIMLR( STR1( : L1 ), STR2( : L2 ) )

      END
* $Id$
