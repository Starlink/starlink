      SUBROUTINE NDF1_A2P( N, AX, LBND, UBND, HAVCEN, HAVWID, CENTRE,
     :                     WIDTH, INC, IPIX0, CENT0, SPACE0, INPIX,
     :                     IPIX1, CENT1, WIDTH1, STATUS )
*+
*  Name:
*     NDF1_A2P

*  Purpose:
*     Convert axis coordinates into pixel indices and related
*     information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_A2P( N, AX, LBND, UBND, HAVCEN, HAVWID, CENTRE, WIDTH,
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

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

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
               CALL ERR_REP( 'NDF1_A2P_ERR1',
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
            CALL NDF1_P2A( 2, IRANGE, LBND, UBND, .TRUE., HAVWID,
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
               CALL ERR_REP( 'NDF1_A2P_ERR2',
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
                  CALL NDF1_P2A( 1, INEW, LBND, UBND, .TRUE., HAVWID,
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
                  CALL NDF1_P2A( 1, INEW, LBND, UBND, .TRUE., HAVWID,
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

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_A2P', STATUS )

      END
