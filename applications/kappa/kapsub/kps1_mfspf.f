      SUBROUTINE KPS1_MFSPF( INTERP, MXKNOT, NKNOT, FKNOT, CMIN, CMAX,
     :                       SCALE, NWS, EL, MAXVAL, X, Z, W, PERM,
     :                       KNOT, COEFF, NCOEF, FACTOR, WS, IWS,
     :                       STATUS )

*+
*  Name:
*     KPS1_MFSPF

*  Purpose:
*     Fits a cubic B-spline curve by least squares.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MFSPF( INTERP, MXKNOT, NKNOT, FKNOT, CMIN, CMAX, SCALE,
*                      NWS, EL, MAXVAL, X, Z, W, PERM, KNOT, COEFF,
*                      NCOEF, FACTOR, WS, IWS, STATUS )

*  Description:
*     This routine fits a cubic B-spline curve for the given vectors of
*     values and weights at the specified co-ordinates, and returns the
*     coefficients of the fit.  There is a choice of interpolating or
*     smmothing spline (see INTERP).  The interpolation fit passes
*     through the supplied knots.  The smoothing spline fit is more
*     complex, but it generates knots automatically when the weighted
*     sum of squares of the residuals exceeds a smoothing factor.
*     See the Notes for more details.

*  Arguments:
*     INTERP = LOGICAL (Given)
*        If set to true an interpolating least-squares spline is found
*        and the NKNOT interior knots are equally spaced.  Otherwise a
*        smoothing spline is fit using a smoothing factor equal to the
*        number of data points; and the spline fit sets the knots.
*     MXKNOT = INTEGER (Given)
*        The maximum number of knots including exterior knots.  This
*        is for the case when INTERP is false where the fitting routine
*        can only assign up to MXKNOT knots automatically.  When
*        INTERP is true, this should be set to NKNOT+8.  For INTERP
*        set false, the maximum numvber of knots should be at least
*        the lesser of EL / 2 or 200, although only up to 28 will
*        actually be used in the final fit, the others are needed for
*        iteration to a reasonable smoothing factor than minimises the
*        residuals whilst not fitting to small-scale features and noise.
*     NKNOT = INTEGER (Given)
*        The number of interior knots.
*     FKNOT( NKNOT ) = REAL (Given)
*        Grid co-ordinates of the user-defined knot positions to be
*        applied to every trend when using an interpolating spline
*        (INTERP is true).  If the first element is negative, then
*        equally spaced knots are used.
*     CMIN = REAL (Given)
*        Minimum co-ordinate of the data bins (left edge, not the
*        centroid).
*     CMAX = REAL (Given)
*        Maximum co-ordinate of the data bins (right edge, not the
*        centroid).
*     SCALE = LOGICAL (Given)
*        If true the data values will be scaled to -1 to +1 range.
*     NWS = INTEGER*8 (Given)
*        The dimension of the WS work space which must be at least
*        4 * (MAXVAL + 4 * NKNOT).
*     EL = INTEGER*8 (Given)
*        The number of data points to be fitted by least-squares.
*     MAXVAL = INTEGER*8 (Given)
*        The maximum dimension of the data, weight and co-ordinate
*        vectors.  This should be no less than EL + 2.
*     X( MAXVAL ) = REAL (Given and Returned)
*        The co-ordinates of the points to be evaluated.
*     Z( MAXVAL ) = REAL (Given and Returned)
*        The values at the given positions.
*     W( MAXVAL ) = REAL (Given and Returned)
*        The weights at the given positions.
*     PERM( MAXVAL ) = INTEGER*8 (Given and Returned)
*        Workspace for permutation index need to sort the data into
*        ascending position.
*     KNOT( MXKNOT ) = REAL (Returned)
*        The positions of complete set of knots in the same co-ordinates
*        as array X.
*     COEFF( MXKNOT ) = REAL (Returned)
*        The cubic B-spline coefficients, defined at the knots.
*        There will be NCOEF=NKNOT+8 coefficients for an interpolating
*        spline.
*     NCOEF = INTEGER (Returned)
*        The number of spline coefficients.
*     WS( NWS ) = REAL (Returned)
*        Work space.
*     IWS( MXKNOT ) = INTEGER (Returned)
*        Work space.
*     FACTOR = REAL (Returned)
*        The scale factor applied to the data values before calculating
*        the spline.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The smoothing spline fit uses a smoothing factor S that must be
*     determined.  The routine first determines upper and lower limits
*     of S.  The upper is the weighted sum of the squares of the
*     residuals of the least-squares cubic polynomial fit.  The lower
*     limit comes from an estimation of the overall noise from averaging
*     the standard deviations in typically 50 segments and then combined
*     with a 3-sigma clip to exclude line features.  The purpose of the
*     segments is to exclude the trend from the calculation.
*     -  The value of S is decreased progressively more slowly until
*     the maximum number of knots or the miniumum value of S is
*     reached, or the residuals increase.  During iterations the initial
*     knots are used.
*     -  A final fit is made with the chosen S, but now finding the
*     knots afresh.
*     -  In some cases, the noise in the trend vector is much greater
*     than suggested by the weights supplied.  These are identified
*     from their upper limit being less than their lower limit for S.
*     An experimental fudge smooths the data and recalculates the lower
*     limit.  If the minimum still exceeds the upper limit, the lower
*     limits is set arbitrarily to 40% of the upper limit in order to
*     create some knots.  The smoothing appears not to work on data
*     tested thus far, and may need reworking, or simply removed,
*     leaving the user to fit these by another method or mask them
*     before fitting.  It may just be that the spline fitting
*     algorithm breaks down for data where the noise dominates any
*     trend.


*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     2008 May 14 (MJC):
*        Original version.
*     2008 May 21 (MJC):
*        Added FKNOT argument.
*     2008 May 27 (MJC):
*        Added iteration to smoothing factor.
*     20-FEB-2020 (DSB):
*        Support huge arrays.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Bad-pixel definitions
      INCLUDE 'MSG_PAR'          ! MSG__ constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL

*  Arguments Given:
      LOGICAL INTERP
      INTEGER MXKNOT
      INTEGER NKNOT              ! Number of interior knots
      REAL FKNOT( NKNOT )
      REAL CMIN, CMAX            ! X bounds of the fit
      LOGICAL SCALE              ! Data values are to be scaled
      INTEGER*8 NWS              ! Dimension of workspace
      INTEGER*8 EL               ! Number of points for evaluation
      INTEGER*8 MAXVAL           ! Dimension of the data vectors

*  Arguments Given and Returned:
      REAL X( MAXVAL )           ! Co-ordinates of the data
      REAL Z( MAXVAL )           ! Data values
      REAL W( MAXVAL )           ! Data weights

*  Arguments Returned:
      INTEGER*8 PERM( MAXVAL )   ! Workspace
      REAL KNOT( MXKNOT )        ! Positions of the knots in x
      REAL COEFF( MXKNOT )       ! B-spline coefficients
      INTEGER NCOEF              ! Number of spline coefficients
      REAL FACTOR                ! Data scale factor
      REAL WS( NWS )             ! Workspace
      INTEGER IWS( MXKNOT )      ! Workspace

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL FPFACT                ! Factor**1/iteration to scale estimate
      PARAMETER ( FPFACT = 0.8 ) ! of smoothing limit at each iteration
                                 ! must be 0 < FPFACT < 1.

*  Local Variables:
      LOGICAL BADOUT             ! Bad values present in smoothed data?
      LOGICAL BETTER             ! Better fit than the previous?
      REAL BSFACT                ! First smoothing factor in sequence of
                                 ! constant sum of squares of residuals
      INTEGER CONSEQ             ! Number of consecutive s giving
                                 ! constant sum of squares of residuals
      REAL ECR( 2 )              ! Effective x limits
      INTEGER*8 I                ! Loop counter
      INTEGER IFAIL              ! PDA error status
      INTEGER IOPT               ! PDA8_CURFIT flag
      REAL MAXV                  ! Maximum data value
      REAL MINV                  ! Minimum data value
      INTEGER*8 NDAT             ! Number of data, including clamps
      INTEGER NITER              ! Number of iterations
      REAL NOISE                 ! Clipped-mean noise in trend
      INTEGER*8 NSEG             ! Number of data, including clamps
      REAL PSFACT                ! Previous smoothing factor
      REAL PSIGMA                ! Previous sum of squares of residuals
      REAL SFACT                 ! Smoothing factor (s)
      REAL SIGMA                 ! Weighted sum of squares of residuals
      REAL SMAX                  ! Maximum smoothing factor
      REAL SMIN                  ! Minimum smoothing factor
      LOGICAL SMOOTH             ! Data smoothed
      INTEGER SMPTR              ! Pointer to smoothed trend
      INTEGER SPTR               ! Pointer to segment std. deviations
      INTEGER SUMPTR             ! Pointer to smoothing sums
      INTEGER TKNOT              ! Total number of knots required
      INTEGER WPTR               ! Pointer to segment/smoothing weights

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      SMOOTH = .FALSE.

*  If there were insufficient bins containing valid data report the
*  error and exit.  The threshold is the order plus two.
      IF ( EL .LT. 5 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETK( 'EL', EL )
         CALL ERR_REP( 'KPS1_MFSPF_INSFD',
     :     'KPS1_MFSPF: Insufficient data --- ^EL bins.', STATUS )
         GO TO 999
       END IF

*  Sort the data into increasing co-ordinate order.
*  ================================================

*  Add two more data points, with zero weight, at opposite ends of the
*  curve to ensure that the spline fit is valid over the entire range.
*  The extra values are normally set to zero, but for the special case
*  when the noise is greater than the maximum sunm of the residuals to
*  a cubic polyonimla fit, we need to smooth the data, and thus the
*  extra values duplicate the end values.
      X( EL + 1 ) = CMIN - 1.0
      Z( EL + 1 ) = Z( 1 )
      W( EL + 1 ) = VAL__EPSR * W( EL )
      X( EL + 2 ) = CMAX + 1.0
      Z( EL + 2 ) = Z( EL )
      W( EL + 2 ) = VAL__EPSR * W( EL )
      NDAT = EL + 2

*  The co-ordinate limits are slightly expanded.
      ECR( 1 ) = CMIN - 1.0
      ECR( 2 ) = CMAX + 1.0

*  PDA8_CURFIT demands that the positions be in ascending order.
*  First find the permutation of the co-ordinates, and then apply the
*  permutation to the co-ordinates.  This should be an inexpensive
*  operation as the supplied co-ordinates are expected to be in
*  ascending order already, so it should just place the (EL+1)th
*  at the start.
      IFAIL = 0
      CALL PDA8_QSIAR( NDAT, X, PERM )
      CALL PDA8_RINPR( PERM, NDAT, X, IFAIL )
      CALL PDA8_RINPR( PERM, NDAT, Z, IFAIL )
      CALL PDA8_RINPR( PERM, NDAT, W, IFAIL )
      IF ( IFAIL .NE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_MFSPF_SORTERR', 'Error sorting data '/
     :                 /'and weights, into ascending co-ordinate '/
     :                 /'order.', STATUS )
         GOTO 999
      END IF

*  Exit in case something has gone wrong before we attempt to use
*  PDA8_CURFIT.
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Scale the data to improve the fitting.
*  ======================================
      IF ( SCALE ) THEN

*  Find maximum and minimum data values.
         MAXV = VAL__MINR
         MINV = VAL__MAXR
         DO I = 1, NDAT
            MAXV = MAX( Z( I ), MAXV )
            MINV = MIN( Z( I ), MINV )
         END DO

*  Derive the scale factor.  Watch for pathological case.  Setting the
*  scale factor to be negative will prevent evaluation routines from
*  re-scaling.
         IF ( MAXV - MINV .LT. 1.E-6 * ABS( MINV ) ) THEN
            FACTOR = -1.0
         ELSE
            FACTOR = 1. / REAL( MAXV - MINV )

*  Scale data values to lie in the range -1 to +1 to improve
*  performance of the fitting routines.
             DO I = 1, NDAT
               Z( I ) = Z( I ) * FACTOR
            END DO
         END IF
      END IF

*  Fit interpolating spline.
*  =========================
      IF ( INTERP ) THEN

*  Set the interior knots an equal number of data points apart...
         IF ( NKNOT .GE. 1 .OR. FKNOT( 1 ) .LT. 0.0 ) THEN
            CALL KPS1_SUSKR( NDAT, X, NKNOT, KNOT( 5 ), STATUS )

*  or use the fixed knot positions.
         ELSE
            DO I = 1, NKNOT
               KNOT( 4 + I ) = FKNOT( I )
            END DO
         END IF

*  NCOEF is Given argument for an interpolation spline, but Returned
*  for a smoothing spline.
         NCOEF = MXKNOT
         SFACT = 0.0

*  Obtain the cubic spline coefficients of the least-squares fit.
*  The -1 means that we supply the interior knots and a least-squares
*  fit is performed.  The 3 is the order.
         CALL PDA8_CURFIT( -1, NDAT, X, Z, W, ECR( 1 ), ECR( 2 ), 3,
     :                     SFACT, MXKNOT, NCOEF, KNOT, COEFF, SIGMA, WS,
     :                     NWS, IWS, IFAIL )

*  Check for an error.
         IF ( IFAIL .GT. 0 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'IFAIL', IFAIL )
            CALL ERR_REP( 'KPS1_MFSPF_PDAS',
     :           'KPS1_MFSPF: Error ^IFAIL returned by PDA8_CURFIT '/
     :           /'fitting the interpolation spline curve.', STATUS )
            GO TO 999
         END IF

*  Total number of knots includes four exterior knots at each end.
         NCOEF = NKNOT + 8

*  Iterate to smoothing spline.
*  ============================
      ELSE

*  Find the smoothing factor upper limit.
*  --------------------------------------
*  Obtain the least-squares cubic polynomial fit and the upper limit
*  for the smoothing factor as given by SMAX.  This is achievde by
*  supplying a very large smoothing factor.
         SIGMA = 0.0
         NCOEF = 0
         CALL PDA8_CURFIT( 0, NDAT, X, Z, W, ECR( 1 ), ECR( 2 ), 3,
     :                     VAL__MAXR, MXKNOT, NCOEF, KNOT, COEFF, SMAX,
     :                     WS, NWS, IWS, IFAIL )

*  We want to apply at least some smoothing.
         SMAX = SMAX * 0.99

*  Find the smoothing factor lower limit.
*  --------------------------------------
*  Determine the approximate detrended noise in segments, removing
*  3-sigma outlier segments.  The number of segments to choose is
*  somewhat of a guess.  Looking at the more extreme curved ones,
*  about 50 looks right.  Constrain to have at least five elements
*  per segment to derive a standard deviation.  The noise is used to
*  set the lower limit for the residuals to avoid fitting to the noise.
*  Use workspace for the calculations, although we could declare some
*  50-element arrays.
         NSEG = MAX( 1, MIN( 50, EL / 5 ) )
         CALL PSX_CALLOC8( NSEG, '_REAL', SPTR, STATUS )
         CALL PSX_CALLOC8( NSEG, '_INTEGER', WPTR, STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 999

         CALL KPS1_MFNOR( NDAT, Z, X, NSEG, %VAL( CNF_PVAL( SPTR ) ),
     :                    %VAL( CNF_PVAL( WPTR ) ), NOISE, STATUS )

         CALL PSX_FREE( SPTR, STATUS )
         CALL PSX_FREE( WPTR, STATUS )
         SMIN = SQRT( REAL( NDAT ) ) * NOISE

*  Iterate to the smoothing factor.
*  ----------------------------------

*  Initialise for the first loop.
         SFACT = SMAX
         PSFACT = SFACT / FPFACT
         PSIGMA = VAL__MAXR
         BETTER = .TRUE.
         NITER = 0
         CONSEQ = 0

*  Fudge a better fit to noisy trends.
*  -----------------------------------
*  No iterations suggesting very noisy data and/or underestimated
*  errors used as weights.  In this case fudge matter by smoothing the
*  inputt data first. This may not work, and such cases may need to be
*  fit with a polynomial, or flagged to be excluded from the analysis.
         SMOOTH = SFACT .LT. SMIN
         IF ( SMOOTH ) THEN

*  Get workspace for the smoothing and smoothed data.
            CALL PSX_CALLOC8( NDAT, '_REAL', SMPTR, STATUS )
            CALL PSX_CALLOC8( NDAT, '_REAL', SUMPTR, STATUS )
            CALL PSX_CALLOC8( NDAT, '_INTEGER', WPTR, STATUS )
            IF ( STATUS .NE. SAI__OK ) GOTO 999

*  This assumes adjacent pixels are contiguous, which may not be the
*  case after line masking, but for noisy data masking is less likely
*  and this approximation should not matter across the broad fit.
*  We also assume that the smoothed trend does not have any bad
*  pixels, since the original data will not contain any.
            CALL KPG1_BLOC8R( .FALSE., .FALSE., .FALSE., NDAT, 1_8,
     :                        Z, 5, 1, 1, %VAL( CNF_PVAL( SMPTR ) ),
     :                        BADOUT, %VAL( CNF_PVAL( SUMPTR ) ),
     :                        %VAL( CNF_PVAL( WPTR ) ), STATUS )
            CALL PSX_FREE( SUMPTR, STATUS )
            CALL PSX_FREE( WPTR, STATUS )

*  Re-estimate the SMIN.
            CALL PSX_CALLOC8( NSEG, '_REAL', SPTR, STATUS )
            CALL PSX_CALLOC8( NSEG, '_INTEGER', WPTR, STATUS )
            IF ( STATUS .NE. SAI__OK ) GOTO 999

            CALL KPS1_MFNOR( NDAT, %VAL( CNF_PVAL( SMPTR ) ), X, NSEG,
     :                       %VAL( CNF_PVAL( SPTR ) ),
     :                       %VAL( CNF_PVAL( WPTR ) ), NOISE, STATUS )

            CALL PSX_FREE( SPTR, STATUS )
            CALL PSX_FREE( WPTR, STATUS )
            SMIN = SQRT( REAL( NDAT ) ) * NOISE

            IF ( SMAX .LE. SMIN ) SMIN = 0.4 * SMAX
         END IF

*  Determine a decent approximation for the smoothing factor by
*  a rough minimisation subject to constrains.
*  a) We do not want to fit to the noise hence the SMIN lower limit
*  for the smoothing factor.
*  b) The maximum number of knots in the calling routine is high to
*  avoid PDA8_CURFIT complaining that it does not have the workspace
*  capacity, but to avoid fitting to small scale features and noise
*  restrict the actual maximum number of knots.  About 20 interior
*  with the 8 exterior knots should be more than enough.
*  c) The maximum number of iterations.  In practice this should not
*  be reached.
*  d) PDA8_CURFIT does not return the error indiciating that the
*  smoothing factor is too low or the maximum number of knots is too
*  few.
         TKNOT = NKNOT + 8
         DO WHILE ( BETTER .AND. NITER .LT. 10 .AND. SFACT .GT. SMIN )
            NITER = NITER + 1

*  Increase the scale factor closer to unity at each iteration.
*  This assimes 0 < FPFACT < 1.
            SFACT = PSFACT * FPFACT ** ( 1.0 / REAL( NITER ) )
            NCOEF = 0

*  Use previously determined knots to save time on the second and
*  subsequent iterations.
            IOPT = 1
            IF ( NITER .EQ. 1 ) IOPT = 0

*  Obtain the cubic spline coefficients of the smoothing-spline fit.
            SIGMA = 0.0
            NCOEF = 0
            IF ( SMOOTH ) THEN
               CALL PDA8_CURFIT( IOPT, NDAT, X,
     :                           %VAL( CNF_PVAL( SMPTR ) ), W, ECR( 1 ),
     :                           ECR( 2 ), 3, SFACT, MXKNOT, NCOEF,
     :                           KNOT, COEFF, SIGMA, WS, NWS, IWS,
     :                           IFAIL )
            ELSE
               CALL PDA8_CURFIT( IOPT, NDAT, X, Z, W, ECR( 1 ),
     :                           ECR( 2 ), 3, SFACT, MXKNOT, NCOEF,
     :                           KNOT, COEFF, SIGMA, WS, NWS, IWS,
     :                           IFAIL )
            END IF

*  Attempt to find the best smoothing factor.  PDA8_CURFIT seems to
*  have quanta, where a series of smoothing factors give constant
*  fit residuals.  We want to the pick the largest factor for a
*  constant sum of the residuals, or until the maximum number of knots
*  is reached.
            BETTER = NCOEF .LE. TKNOT .AND. IFAIL .NE. 1

            IF ( SIGMA .EQ. PSIGMA ) THEN
               CONSEQ = CONSEQ + 1
               IF ( CONSEQ .EQ. 1 ) BSFACT = PSFACT
               PSFACT = SFACT
               PSIGMA = SIGMA

            ELSE IF ( SIGMA .GT. PSIGMA ) THEN
               BETTER = .FALSE.
               SFACT = PSFACT

            ELSE IF ( SIGMA .LT. PSIGMA ) THEN
               PSFACT = SFACT
               PSIGMA = SIGMA
               CONSEQ = 0
            END IF
         END DO

*  The number of knots seem to increase rapidly, but for well-behaved
*  trends this leads to IFAIL=1 because the storage space is too small.
*  However, we do not want to increase the estimate for the number of
*  knots, as it creates too many knots, and can fit to the noise in
*  curvy trends.  So go back to the previous estimate of the smoothing
*  factor, even if this means no interior knots.  It is worth attempting
*  some smoothing before fitting.
         IF ( IFAIL .EQ. 1 .OR. NCOEF .GT. TKNOT .OR.
     :        SFACT .LT. SMIN ) THEN
            IF ( NITER .GT. 1 )
     :         SFACT = SFACT / FPFACT ** ( 1.0 / REAL( NITER ) )
            IF ( CONSEQ .GT. 0 ) SFACT = BSFACT
         END IF

*  Ensure that we are not fitting to the noise.
         SFACT = MAX( SFACT, SMIN )

*  Final fit with new knots.
*  -------------------------
*  Now use the roughly determined smoothing factor with recalculated
*  knot positions.
         IF ( SMOOTH ) THEN
            CALL PDA8_CURFIT( 0, NDAT, X, %VAL( CNF_PVAL( SMPTR ) ), W,
     :                        ECR( 1 ), ECR( 2 ), 3, SFACT, TKNOT,
     :                        NCOEF, KNOT, COEFF, SIGMA, WS, NWS, IWS,
     :                        IFAIL )
         ELSE
            CALL PDA8_CURFIT( 0, NDAT, X, Z, W, ECR( 1 ), ECR( 2 ), 3,
     :                        SFACT, TKNOT, NCOEF, KNOT, COEFF, SIGMA,
     :                        WS, NWS, IWS, IFAIL )
         END IF

*  Check for an error.
         IF ( IFAIL .GT. 1 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'IFAIL', IFAIL )
            CALL ERR_REP( 'KPS1_MFSPF_PDA',
     :           'KPS1_MFSPF: Error ^IFAIL returned by PDA8_CURFIT '/
     :           /'fitting the smoothing spline curve.', STATUS )
            GO TO 999
         END IF
      END IF

  999 CONTINUE
      IF ( SMOOTH ) CALL PSX_FREE( SMPTR, STATUS )


      END
