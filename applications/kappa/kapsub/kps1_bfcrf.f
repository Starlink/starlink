      SUBROUTINE KPS1_BFCRF( MAP, IWCS, NAXC, NBEAM, NCOEF, PP, PSIGMA,
     :                       RP, RSIGMA, POLAR, POLSIG, STATUS )
*+
*  Name:
*     KPS1_BFCRF

*  Purpose:
*     Converts the pixel coefficients of the fit into the reference
*     Frame

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_BFCRF( MAP, IWCS, NAXC, NCOEF, PP, PSIGMA, RP,
*                      RSIGMA, POLAR, POLSIG, STATUS )

*  Description:
*     This converts the spatial coefficients and their errors from
*     PIXEL co-ordinates into the reporting Frame for the fitted beam
*     features.  In addition the standard deviation widths may be
*     swapped so that the third coefficent is the major axis and the
*     the fourth is the minor axis.  The orientation is made to lie
*     the range of 0 to pi radians by adding or subtracting pi
*     radians, and for SkyFrames it is measured from the North via East,
*     converted from X-axis through Y.  Data-value coefficients (e.g.
*     amplitude) are unchanged.
*
*     In addition it computes and returns the polar co-ordinates of the
*     secondary beams with respect to the primary in the reporting
*     Frame.  The orientation is measured North through East for a
*     SkyFrame, and from the Y axis anticlockwise for other Frames.

*     This routine makes it easier to write the results to a
*     logfile or to output parameters and is more efficient
*     avoiding duplication transformations caused by separate
*     outputting routines and flushed message tokens.

*  Arguments:
*     MAP = INTEGER (Given)
*        The AST Mapping from the PIXEL Frame of the NDF to the
*        reporting Frame.
*     IWCS = INTEGER (Given)
*        The FrameSet of two-dimensional frames associated with the NDF.
*     NAXC = INTEGER (Given)
*        The number of axes in CFRM.
*     NBEAM = INTEGER (Given)
*        The number of beam features fitted.
*     NCOEF = INTEGER (Given)
*        The number of coefficients per beam position.
*     PP( NCOEF, NBEAM ) = DOUBLE PRECISION (Given)
*        The fit parameters with spatial coefficients measured in the
*        PIXEL Frame.
*     PSIGMA( NCOEF, NBEAM ) = DOUBLE PRECISION (Given)
*        The errors in the fit parameters measuered in the PIXEL Frame.
*     RP( NCOEF, NBEAM ) = DOUBLE PRECISION (Returned)
*        The fit parameters with spatial coefficients measured in the
*        reporting Frame.
*     RSIGMA( NCOEF, NBEAM ) = DOUBLE PRECISION (Returned)
*        The errors in the fit parameters measuered in the reporting
*        Frame.
*     POLAR( 2, NBEAM ) =  DOUBLE PRECISION (Returned)
*         The polar co-ordinates of the beam features with respect to
*         the primary beam measured in the current co-ordinate Frame.
*         The orientation is a position angle in degrees, measured from
*         North through East if the current Frame is a Skyframe, or
*         anticlockwise from the Y axis otherwise.  The POLAR(*,1)
*         values of the primary beam are set to 0.0 and bad values.
*     POLSIG( 2, NBEAM ) =  DOUBLE PRECISION (Returned)
*         The standard-deviation errors associated with the polar
*          co-ordinates supplied in argument POLAR.  The POLSIG(*,1)
*         values of the primary beam are set to 0.0 and bad values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2007 Particle Physics and Astronomy Research
*     Council.  All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2007 May 21 (MJC):
*        Original version created from KPS1_BFLOG with the aim of
*        avoiding code duplication.
*     2007 May 25 (MJC):
*        Fixed repeated typo's such that the PSIGMA values are now
*        assigned.
*     2007 May 30 (MJC):
*        Add POLAR and POLSIG arguments and calculate polar co-ordinates
*        of secondary-beam features with error propagation.
*     2007 June 20 (MJC):
*        Correct the calculation of the orientation: allowing for flipped
*        longitude SKY axis and adjust non-SKY Frame angles to Y via
*        negative X.
*     2007 June 25 (MJC):
*        Used a purely AST approach to derive the position angle.  This
*        needed argument CFRM (current Frame identifier) to be replaced
*        by IWCS.
*     2007 August 6 (MJC):
*        Pass only single positions to AST_NORM.
*     2007 October 5 (MJC):
*        Use three points to derive the widths and their errors to allow
*        for rotation in the transformation.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! Message-system public constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      INTEGER MAP
      INTEGER IWCS
      INTEGER NAXC
      INTEGER NBEAM
      INTEGER NCOEF
      DOUBLE PRECISION PP( NCOEF, NBEAM )
      DOUBLE PRECISION PSIGMA( NCOEF, NBEAM )

*  Arguments Returned:
      DOUBLE PRECISION RP( NCOEF, NBEAM )
      DOUBLE PRECISION RSIGMA( NCOEF, NBEAM )
      DOUBLE PRECISION POLAR( 2, NBEAM )
      DOUBLE PRECISION POLSIG( 2, NBEAM )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      DOUBLE PRECISION SLA_DBEAR ! Bearing of one position from another

*  Local Constants:
      DOUBLE PRECISION PI
      PARAMETER ( PI = 3.1415926535898 )

      DOUBLE PRECISION R2D       ! Radians to degrees
      PARAMETER ( R2D = 180.0D0 / PI )

      DOUBLE PRECISION TWOPI
      PARAMETER ( TWOPI = 2.0D0 * PI )

*  Local Variables:
      DOUBLE PRECISION A( 2 )    ! Start of distance (A to B)
      DOUBLE PRECISION B( 2 )    ! End of distance (A to B)
      INTEGER CFRM               ! Current/reporting Frame
      DOUBLE PRECISION DATAN     ! Differentiating atan factor
      DOUBLE PRECISION DX        ! Increment along first axis
      DOUBLE PRECISION DY        ! Increment along second axis
      DOUBLE PRECISION GRAD      ! Gradient DY/DX
      INTEGER I                  ! Loop count
      INTEGER IB                 ! Beam loop count
      INTEGER IPIX               ! Index of PIXEL Frame in IWCS
      LOGICAL ISSKY              ! Is the current Frame a SkyFrame?
      DOUBLE PRECISION JUNK      ! Unused angle
      INTEGER LAT                ! Index to latitude axis in SkyFrame
      INTEGER LON                ! Index to longitude axis in SkyFrame
      INTEGER MAJOR              ! Index to major-FWHM value and error
      INTEGER MINOR              ! Index to minor-FWHM value and error
      INTEGER PFRM               ! PIXEL Frame
      DOUBLE PRECISION POS( 3, 2 ) ! Reporting positions
      DOUBLE PRECISION PIXPOS( 3, 2 ) ! Pixel positions
      DOUBLE PRECISION SPOS( 2 ) ! Single reporting position
      REAL THETA                 ! Orientation in radians
      DOUBLE PRECISION VAR       ! Variance
      DOUBLE PRECISION VARX      ! Sum of longitude-axis variances
      DOUBLE PRECISION VARY      ! Sum of latitude-axis variances
      DOUBLE PRECISION XIN( 2 )  ! X co-ordinate pair to convert
      DOUBLE PRECISION XOUT( 2 ) ! Converted X co-ordinate pair
      DOUBLE PRECISION YIN( 2 )  ! Y co-ordinate pair to convert
      DOUBLE PRECISION YOUT( 2 ) ! Converted Y co-ordinate pair

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the current Frame and PIXEL Frames from the FrameSet.
      CFRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )
      CALL KPG1_ASFFR( IWCS, 'PIXEL', IPIX, STATUS )
      PFRM = AST_GETFRAME( IWCS, IPIX, STATUS )

*  Find the latitude and longitude axes needed for the origin of the
*  orientation.
      ISSKY =  AST_ISASKYFRAME( CFRM, STATUS )
      IF ( ISSKY ) THEN
         LAT = AST_GETI( CFRM, 'LatAxis', STATUS )
         LON = AST_GETI( CFRM, 'LonAxis', STATUS )
      END IF

*  Report each fit in turn.
      DO IB = 1, NBEAM

*  Copy the input to output.  This handles any undefined values and the
*  data-value related coefficients.
         DO I = 1, NCOEF
            RP( I, IB ) = PP( I, IB )
            RSIGMA( I, IB ) = PSIGMA( I, IB )
         END DO

*  Beam position
*  =============

*  Centre
*  ------

*  Transform the fitted beam positions and errors from the PIXEL Frame
*  of the NDF to the reporting Frame.
         PIXPOS( 1, 1 ) = PP( 1, IB )
         PIXPOS( 1, 2 ) = PP( 2, IB )
         PIXPOS( 2, 1 ) = PP( 1, IB ) + PSIGMA( 1, IB )
         PIXPOS( 2, 2 ) = PP( 2, IB ) + PSIGMA( 2, IB )
         CALL AST_TRANN( MAP, 2, 2, 3, PIXPOS, .TRUE., NAXC, 3, POS,
     :                   STATUS )

*  Normalize the supplied current Frame position.  Need dummy array
*  because of AST_TRANN's requirement that the co-ordinates for the
*  point runs along the second dimension.
         SPOS( 1 ) = POS( 1, 1 )
         SPOS( 2 ) = POS( 1, 2 )
         CALL AST_NORM( CFRM, SPOS, STATUS )
         POS( 1, 1 ) = SPOS( 1 )
         POS( 1, 2 ) = SPOS( 2 )

         RP( 1, IB ) = POS( 1, 1 )
         RP( 2, IB ) = POS( 1, 2 )

         IF ( PSIGMA( 1, IB ) .NE. VAL__BADD ) THEN

*  Centre errors
*  -------------
*  Need to determine the distance for the error bars.  For convenience
*  we fudge it here if NAXC exceeds 2.  We're keeping higher axes
*  fixed anyway.
            DO I = 1, NAXC
               A( I ) = POS( 1, I )
               B( I ) = A( I )
            END DO
            B( 1 ) = POS( 2, 1 )

            RSIGMA( 1, IB )  = AST_DISTANCE( CFRM, A, B, STATUS )

*  Now we swap the path to the second axis.
            B( 1 ) = A( 1 )
            B( 2 ) = POS( 2, 2 )
            RSIGMA( 2, IB ) = AST_DISTANCE( CFRM, A, B, STATUS )
         END IF

*  FWHMs
*  =====

*  See which is major?  For simplicity this is determined in the
*  PIXEL domain, however, perhaps it should be done in the current
*  (reporting) Frame but what if the axes have different units?
*  We can review this if it's a problem in practice.
         IF ( PP( 3, IB ) .GE. PP( 4, IB ) ) THEN
            MAJOR = 3
            MINOR = 4
         ELSE
            MAJOR = 4
            MINOR = 3
         END IF

*  Widths
*  ------

*  Transform the fitted positions and widths from the PIXEL Frame
*  of the NDF to the reporting Frame.
         PIXPOS( 1, 1 ) = PP( 1, IB )
         PIXPOS( 1, 2 ) = PP( 2, IB )

         PIXPOS( 2, 1 ) = PP( 1, IB )
         PIXPOS( 2, 2 ) = PP( 2, IB ) + PP( MINOR, IB )

         PIXPOS( 3, 1 ) = PP( 1, IB ) + PP( MAJOR, IB )
         PIXPOS( 3, 2 ) = PP( 2, IB )

         CALL AST_TRANN( MAP, 3, 2, 3, PIXPOS, .TRUE., NAXC, 3, POS,
     :                   STATUS )

*  Normalize the supplied current Frame position.  Need dummy array
*  because of AST_TRANN's requirement that the co-ordinates for the
*  point runs along the second dimension.
         SPOS( 1 ) = POS( 1, 1 )
         SPOS( 2 ) = POS( 1, 2 )
         CALL AST_NORM( CFRM, SPOS, STATUS )
         POS( 1, 1 ) = SPOS( 1 )
         POS( 1, 2 ) = SPOS( 2 )

*  Need to determine the distances for the beam widths.  Use the
*  reporting axis.
         DO I = 1, NAXC
            A( I ) = POS( 1, I )
            B( I ) = POS( 2, I )
         END DO
         RP( 4, IB ) = AST_DISTANCE( CFRM, A, B, STATUS )

         DO I = 1, NAXC
            B( I ) = POS( 3, I )
         END DO
         RP( 3, IB ) = AST_DISTANCE( CFRM, A, B, STATUS )

*  Errors in width
*  ---------------
*  Bad values for the errors in the FWHM indicate no value exists.
         IF ( PSIGMA( MAJOR, IB ) .NE. VAL__BADD ) THEN

*  Transform the fitted positions and width errors from the PIXEL
*  Frame of the NDF to the reporting Frame.
            PIXPOS( 1, 1 ) = PP( 1, IB )
            PIXPOS( 1, 2 ) = PP( 2, IB )

            PIXPOS( 2, 1 ) = PP( 1, IB )
            PIXPOS( 2, 2 ) = PP( 2, IB ) + PSIGMA( MINOR, IB )

            PIXPOS( 3, 1 ) = PP( 1, IB ) + PSIGMA( MAJOR, IB )
            PIXPOS( 3, 2 ) = PP( 2, IB )

            CALL AST_TRANN( MAP, 3, 2, 3, PIXPOS, .TRUE., NAXC, 3, POS,
     :                      STATUS )

*  Normalize the supplied current Frame position.  Need dummy array
*  because of AST_TRANN's requirement that the co-ordinates for the
*  point runs along the second dimension.
            SPOS( 1 ) = POS( 1, 1 )
            SPOS( 2 ) = POS( 1, 2 )
            CALL AST_NORM( CFRM, SPOS, STATUS )
            POS( 1, 1 ) = SPOS( 1 )
            POS( 1, 2 ) = SPOS( 2 )

*  Need to determine the distance for the error bars.  For convenience
*  we fudge it here if NAXC exceeds 2.  We're keeping higher axes
*  fixed anyway.
*  Need to determine the distances for the beam widths.  Use the
*  reporting axis.
            DO I = 1, NAXC
               A( I ) = POS( 1, I )
               B( I ) = POS( 2, I )
            END DO
            RSIGMA( 4, IB ) = AST_DISTANCE( CFRM, A, B, STATUS )

            DO I = 1, NAXC
               B( I ) = POS( 3, I )
            END DO
            RSIGMA( 3, IB ) = AST_DISTANCE( CFRM, A, B, STATUS )

         END IF

*  Orientation
*  ===========

*  Find a PIXEL position that is offset from the centre by a small
*  amount (one pixel).  Take account of the different definition of the
*  fitted orientation and the angle needed by AST_OFFSET2.
         A( 1 ) = PP( 1, IB )
         A( 2 ) = PP( 2, IB )
         JUNK = AST_OFFSET2( PFRM, A, 0.5D0 * PI - PP( 5, IB ), 1.0D0,
     :                       B, STATUS )

*  Transform the central and offset pixel positions into the reporting
*  Frame.
         XIN( 1 ) = A( 1 )
         XIN( 2 ) = B( 1 )
         YIN( 1 ) = A( 2 )
         YIN( 2 ) = B( 2 )
         CALL AST_TRAN2( MAP, 2, XIN, YIN, .TRUE., XOUT, YOUT, STATUS )

*  Find the angle, as seen from the first point, between the positive
*  direction of the second current Frame axis, and the line joining the
*  two points.  Positive rotation is in the same sense as rotation from
*  the positive direction of the second current Frame axis to the
*  positive direction of the first current Frame axis.
         A( 1 ) = XOUT( 1 )
         A( 2 ) = YOUT( 1 )
         B( 1 ) = XOUT( 2 )
         B( 2 ) = YOUT( 2 )
         THETA = AST_AXANGLE( CFRM, A, B, 2, STATUS )

*  For a non-SkyFrame we want the angle measured from the second axis
*  through negative first axis (to mimic Skyframe's North via East).
         IF ( .NOT. ISSKY ) THETA = -THETA

*  Ensure that the result in the range 0 to 180 degrees.
         IF ( THETA .LT. 0 ) THEN
            THETA = ( THETA + PI )

         ELSE IF( THETA .GT. PI ) THEN
            THETA = ( THETA - PI )

         END IF

*  Note no need to revise the orientation error.
         RP( 5, IB ) = THETA

*  Polar co-ordinates
*  ==================
         IF ( IB .EQ. 1 ) THEN
            POLAR( 1, 1 ) = 0.0D0
            POLAR( 2, 1 ) = VAL__BADD
            POLSIG( 1, 1 ) = 0.0D0
            POLSIG( 2, 1 ) = VAL__BADD
         ELSE

*  Radius
*  ------
*  Transform the primary-beam and secondary position centre measured in
*  the PIXEL Frame of the NDF to the reporting Frame.
            PIXPOS( 1, 1 ) = PP( 1, 1 )
            PIXPOS( 1, 2 ) = PP( 2, 1 )
            PIXPOS( 2, 1 ) = PP( 1, IB )
            PIXPOS( 2, 2 ) = PP( 2, IB )
            CALL AST_TRANN( MAP, 2, 2, 3, PIXPOS, .TRUE., NAXC, 3, POS,
     :                      STATUS )

*  Normalize the supplied current Frame position.  Need dummy array
*  because of AST_TRANN's requirement that the co-ordinates for the
*  point runs along the second dimension.
            SPOS( 1 ) = POS( 1, 1 )
            SPOS( 2 ) = POS( 1, 2 )
            CALL AST_NORM( CFRM, SPOS, STATUS )
            POS( 1, 1 ) = SPOS( 1 )
            POS( 1, 2 ) = SPOS( 2 )

*  Need to determine the distances for the separation.  Use the
*  reporting axis.
            DO I = 1, NAXC
               A( I ) = POS( 1, I )
               B( I ) = POS( 2, I )
            END DO
            POLAR( 1, IB ) = AST_DISTANCE( CFRM, A, B, STATUS )

*  Use the partial derivatives of the polar radius function against the
*  four variables: (x,y) for the primary and secondary positions.  This
*  Taylor-expansion is good to first order.  One should evaluate JCJ^T
*  matrices where J is the Jacobian and C is the covariance matrix to
*  propagate the errors.

*  First assume that the primary beam position is exactly known.
*  Then that the secondary beam is exactly known, adding the variances.
            DX = ( POS( 2, 1 ) - POS( 1, 1 ) ) / POLAR( 1, IB )
            DY = ( POS( 2, 2 ) - POS( 1, 2 ) ) / POLAR( 1, IB )
            VAR = DX * DX * RSIGMA( 1, IB ) * RSIGMA( 1, IB ) +
     :            DY * DY * RSIGMA( 2, IB ) * RSIGMA( 2, IB ) +
     :            DX * DX * RSIGMA( 1, 1 ) * RSIGMA( 1, 1 ) +
     :            DY * DY * RSIGMA( 2, 1 ) * RSIGMA( 2, 1 )

            IF ( VAR .GT. 0.0D0 ) THEN
               POLSIG( 1, IB ) = SQRT( VAR )
            ELSE
               POLSIG( 1, IB ) = VAL__BADD
            END IF

*  Position angle
*  --------------
*  Use spherical geometry for a Skyframe.  We shall need the
*  differences to derive the position-angle error; for convenience
*  use the Euclidean notation.
             IF ( ISSKY ) THEN
               DX = POS( 2, LON ) - POS( 1, LON )
               DY = POS( 2, LAT ) - POS( 1, LAT )

*   Obtain the bearing of the vector between the centre and
*   the unit displacement.
               THETA = SLA_DBEAR( POS( 1, LON ), POS( 1, LAT ),
     :                            POS( 2, LON ), POS( 2, LAT ) )

*  If it's not a SkyFrame assume Euclidean geometry.  Also by
*  definition orientation is 0 degrees for a circular beam.  The
*  transformation can lead to small perturbation of the original
*  zero degrees.
            ELSE
               DX = POS( 2, 1 ) - POS( 1, 1 )
               DY = POS( 2, 2 ) - POS( 1, 2 )
               IF ( DX .NE. 0.0D0 .OR. DY .NE. 0.0D0 ) THEN

*  Switch from the customary X through Y to Y through negative X, i.e.
*  add pi/2 radians, but also the ATAN function returns values in the
*  range -pi to +pi and we require 0 to 2pi.
                  THETA = ATAN2( DY, DX ) + 0.5D0 * PI
               ELSE
                  THETA = 0.0D0
               END IF
            END IF

*  Ensure that the result in the range 0 to 360 degrees.
            IF ( THETA .LT. 0 ) THEN
               THETA = ( THETA + TWOPI )

            ELSE IF ( THETA .GT. TWOPI ) THEN
               THETA = ( THETA - TWOPI )

            END IF

            POLAR( 2, IB ) = THETA * R2D

*  Sum the variances along the two axes.
            VARX = RSIGMA( 1, 1 )  * RSIGMA( 1, 1 ) +
     :             RSIGMA( 1, IB ) * RSIGMA( 1, IB )
            VARY = RSIGMA( 2, 1 )  * RSIGMA( 2, 1 ) +
     :             RSIGMA( 2, IB ) * RSIGMA( 2, IB )

*  Use the partial derivatives of the polar position-angle function
*  against the four variables: (x,y) for the primary and secondary
*  positions.  First deal with the special cases.
            IF ( ABS( DX ) .LT. VAL__EPSD .AND.
     :           ABS( DY ) .LT. VAL__EPSD ) THEN
               POLSIG( 2, IB ) = VAL__BADD

*  PA = 0 or 180.  Combine the first axis errors, then determine the
*  corresponding small angle (hence the need for both ATAN2 arguemnts
*  to be positive.
            ELSE IF ( ABS( DX ) .LT. VAL__EPSD ) THEN
               POLSIG( 2, IB ) = ATAN2( ABS( B( 2 ) - A( 2 ) ),
     :                                  SQRT( VARX ) ) * R2D

*  Now deal with PA = 90 or 270.
            ELSE IF ( ABS( DY ) .LT. VAL__EPSD ) THEN
               POLSIG( 2, IB ) = ATAN2( SQRT( VARY ),
     :                                  ABS( B( 1 ) - A( 1 ) ) ) * R2D

            ELSE
               GRAD = DY / DX
               DATAN = 1.0D0 / ( 1 + GRAD * GRAD ) / DX
               VAR = DATAN * DATAN * ( GRAD * GRAD * VARX + VARY )

               IF ( VAR .GT. 0.0D0 ) THEN
                  POLSIG( 2, IB ) = SQRT( VAR ) * R2D
               ELSE
                  POLSIG( 2, IB ) = VAL__BADD
               END IF
            END IF
         END IF
      END DO

      END
