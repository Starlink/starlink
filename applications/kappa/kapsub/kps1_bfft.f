      SUBROUTINE KPS1_BFFT( PIXPOS, FLBND, FUBND, FIXCON, AMPRAT,
     :                      POFSET, NP, P, SIGMA, RMS, STATUS )
*+
*  Name:
*     KPS1_BFFT

*  Purpose:
*     Finds two-dimensional generalised Gaussian parameter values that
*     are consistent with a supplied set of data values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_BFFT( PIXPOS, FLBND, FUBND, FIXCON, AMPRAT,
*                     NP, P, SIGMA, RMS, STATUS )

*  Description:
*     An initial guess is made at the fit parameters, and a PDA
*     routine is called to vary the parameter values until the sum of
*     the squared residuals between the supplied image and the Gaussian
*     is minimised.  If explicit values have been supplied for any of
*     the projection parameters, they are omitted from the optimisation.

*  Arguments:
*     PIXPOS( BF__MXPOS, BF__NDIM ) = DOUBLE PRECISION (Given)
*        The initial guess positions for the beams measured in pixel
*        co-ordinates.
*     FLBND( BF__NDIM ) = INTEGER (Given)
*        The lower pixel bounds of the data to be fitted.
*     FUBND( BF__NDIM ) = INTEGER (Given)
*        The upper pixel bounds of the data to be fitted.
*     FIXCON( BF__NCON ) = LOGICAL (Given)
*        Flags whether or not to apply constraints.  The elements are
*        the array relate to the following constraints.
*        1 -- Are the beam `source' amplitudes fixed?
*        2 -- Is the background level fixed?
*        3 -- Are the FWHM values of the beam fixed?
*        4 -- Are the beam positions fixed at the supplied co-ordinates?
*        5 -- Are the relative amplitudes fixed?
*        6 -- Are the separations to the secondary beam positions fixed?
*        7 -- Is the shape exponent fixed?
*        8 -- Is the beam fixed to be circular?
*        9 -- Is the orientation of the Gaussian fixed?
*     AMPRAT( BF__MXPOS - 1 ) = REAL (Given)
*        The ratios of the secondary beam 'sources' to the first beam.
*        These ratios contrain the fitting provided FIXCON(5) is .TRUE.
*     POFSET( BF__MXPOS - 1, BF__NDIM ) = DOUBLE PRECISION (Given)
*        The ratios of the secondary beam 'sources' to the first beam.
*        These ratios contrain the fitting provided FIXRAT is .TRUE.
*     NP = INTEGER (Given)
*        The size of array P.
*     P( NP ) = DOUBLE PRECISION (Given and Returned)
*        The Gaussian-fit parameters.  On entry, the array holds any
*        explicit parameter values requested by the user (as indicated
*        by the argument FIXCON).  Other elements of the array are
*        ignored.  On exit, such values are unchanged, but the other
*        elements are returned holding the best value of the
*        corresponding fit parameter.
*     SIGMA( NP ) = DOUBLE PRECISION (Returned)
*        The errors in the Gaussian-fit parameters.
*     RMS = DOUBLE PRECISION (Returned)
*        The RMS residual.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The supplied initial co-ordinates are not necessarily used
*     directly.  Instead they are used to form marginal profiles about
*     the location.  Filtering of those marginal profiles removes the
*     effect of neighbouring sources (see KPS1_CLNS) and from the
*     profiles the routine estimates of the initial Gaussian parameters.
*     This gives more consistent and robust values.  The
*     Levenberg-Marquardt method seems temperamental to the start
*     location.  While the centre position found for different start
*     points agree within their errors, it is somewhat disconcerting to
*     users.  This extra stage avoids that and appears to minimise those
*     errors further boosting the user's confidence in the fitted
*     coefficients.

*     Should there be a problem with this filtering and fitting the
*     initial co-ordinates as supplied are used by the main fit process.
*     Also the nominal initial FWHMs are set at 5.
*     -  The mean of the data is used to estimate the initial background
*     level.
*     -  The 4 and 96 percentiles in an 11-by-11-pixel region (or the
*     full fit area if it is smaller) provide a guess for the initial
*     amplitude.  The mean in the same region indicates which to use and
*     to discover whether the beam is positive or negative with respect
*     to the background.
*     -  If the fitting fails, the most common error code returned is
*     translated into a KAPPA code KAP__LMFOJ.  This code can be tested
*     and appropriate action taken in scripts.  The error arises when the
*     fitted functions from the Levenberg-Marquardt minimisation are
*     orthogonal to the Jacobian's columns (usually indicating that the
*     fitting area is too small).

*  Copyright:
*     Copyright (C) 2007 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2009-2010 Science & Technology Facilities Council.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2007 February 15 (MJC):
*        Original version.
*     2007 April 27 (MJC):
*        Added FIXAMP and FIXRAT arguments, and concurrent fitting of
*        multiple Gaussians,
*     2007 May 11 (MJC):
*        Pass constraint flags as an array to shorten the API.
*     2007 May 14 (MJC):
*        Support fixed separations.
*     2007 June 6 (MJC):
*        Back to using declared arrays for the unidimensional arrays
*        passed to PDA_LMERR.
*     2007 June 15 (MJC):
*        Use marginal profiles to estimate the initial Gaussian
*        parameters.  Add the Notes.
*     2007 June 25 (MJC):
*        Switch ORIENT to radians to be compatible with its use within
*        KPS1_STPAD.
*     2009 December 9 (MJC):
*        Loosened constraint on size of marginal profiles such that it
*        it now only restricts to the storage available in KPS1_STPAx.
*     2010 January 12 (TIMJ):
*        Ensure that the initial guess lies within the pixel bounds.
*     2010 July 5 (MJC):
*        Switched to generalised Gaussian fit by the introduction of
*        the shape exponent.
*     2010 October 26 (MJC):
*        Create a special error message for IFAIL=4, setting status to
*        the new error code.
*     2013 July 15 (MJC):
*        Fixed orientation now obtained through the COMMON block, set
*        in BEAMFIT itself.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'BF_PAR'           ! BEAMFIT constants
      INCLUDE 'KAP_ERR'          ! KAPPA error constants
      INCLUDE 'AST_PAR'          ! AST constants and functions

*  Global Variables:
      INCLUDE 'BF_COM'           ! Used for communicating with PDA
                                 ! routine
*        PC( BF__NCOEF, BF__MXPOS ) = DOUBLE PRECISION (Read and Write)
*           The initial-guess parameter values, including any fixed
*           values supplied by the user.
*        ARATIO( BF__MXPOS - 1 ) = DOUBLE PRECISION (Write)
*           The amplitude ratios of the secondary to the primary beam
*           positions.
*        PIXOFF( BF__MXPOS - 1, BF__NDIM ) = DOUBLE PRECISION (Write)
*           The pixel offsets of the secondary beam positions with
*           respect to the primary beam position.
*        NBEAMS = INTEGER (Read)
*           The number of beams to fit simultaneously
*        AMPC = LOGICAL (Write)
*           Amplitude of Gaussian fixed by user?
*        BACKC = LOGICAL (Write)
*           Background level fixed by user?
*        CIRC = LOGICAL (Write)
*           Circular beam fixed by user?
*        FWHMC = LOGICAL (Write)
*           FWHM(s) of the Gaussian(s) fixed by user?
*        IPWD = INTEGER (Read)
*           Pointer to work space for data values
*        ISTAT = INTEGER (Write)
*           Local status value.
*        LBND( BF__NDIM ) = INTEGER (Write)
*           The lower pixel bounds of the data and variance arrays.
*        ORIC = LOGICAL (Write)
*           Was the orientation fixed by the user?
*        POSC = LOGICAL (Write)
*           Were the pixel dimensions fixed by the user?
*        RATIOC = LOGICAL (Write)
*           Were the amnplitude ratios fixed?
*        SEPARC = LOGICAL (Write)
*           Are the separations fixed?
*        SHAPEC = LOGICAL (Write)
*           Shape exponent of the Gaussian fixed by user?
*        UBND( BF__NDIM ) = INTEGER (Write)
*           The upper pixel bounds of the data and variance arrays.

*  Arguments Given:
      DOUBLE PRECISION PIXPOS( BF__MXPOS, BF__NDIM )
      INTEGER FLBND( BF__NDIM )
      INTEGER FUBND( BF__NDIM )
      LOGICAL FIXCON( BF__NCON )
      REAL AMPRAT( NBEAMS - 1 )
      DOUBLE PRECISION POFSET( BF__MXPOS - 1, BF__NDIM )
      INTEGER NP

*  Arguments Given and Returned:
      DOUBLE PRECISION P( NP )

*  Arguments Returned:
      DOUBLE PRECISION SIGMA( NP )
      DOUBLE PRECISION RMS

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL KPS1_BFFN         ! Subroutine for evaluating the
                                 ! residuals

*  Local Constants:
      INTEGER MXCOEF             ! Maximum number of coefficients
      PARAMETER ( MXCOEF = BF__NCOEF * BF__MXPOS )

      DOUBLE PRECISION PI
      PARAMETER ( PI = 3.1415926535898 )

      INTEGER WIDTH              ! Width of region in which to estimate
      PARAMETER ( WIDTH = 5 )    ! amplitude

*  Local Variables:
      REAL AXRAT                 ! Initial beam axis ratio
      LOGICAL BAD                ! Array may contain bad values?
      INTEGER DIMS( BF__NDIM )   ! Dimensions
      LOGICAL FLAG( MXCOEF )     ! Problem parameter during inversion?
      DOUBLE PRECISION FS        ! Sum of squared residuals
      INTEGER GO                 ! Offset to current Gaussian's coeffs
      INTEGER I                  ! Loop count
      INTEGER IFAIL              ! PDA error status
      INTEGER IG                 ! Gaussian counter
      DOUBLE PRECISION INIT( BF__NDIM ) ! Refined initial co-ordinates
      INTEGER IPCORR             ! Pointer to correlation array
      INTEGER IPCOVA             ! Pointer to covariance array
      INTEGER IPCURV             ! Pointer to curvature array
      INTEGER IPDRES             ! Pointer to displaced-fit residuals
      INTEGER IPJAC              ! Pointer to Jacobian
      INTEGER IPREG              ! Pointer to small-region work array
      INTEGER IPWEF              ! Pointer to a work array for the
                                 ! functions evaluated at XC
      INTEGER IPWNA1             ! Pointer to a PDA work array
      INTEGER IPWNA2             ! Pointer to a PDA work array
      INTEGER J                  ! Array index
      INTEGER LW                 ! Size of a PDA work array
      INTEGER MAXPOS             ! Index of maximum value
      DOUBLE PRECISION MAXVAL    ! Maximum value of the data
      INTEGER MINPOS             ! Index of minimum value
      DOUBLE PRECISION MINVAL    ! Maximum value of the data
      DOUBLE PRECISION MEAN      ! Mean of the data
      DOUBLE PRECISION MEDIAN    ! Median of the small region
      DOUBLE PRECISION MODE      ! Mode of the small region
      INTEGER N                  ! Number of free parameters
      INTEGER NGOOD              ! Number of good values
      INTEGER NPOS               ! Number of data values
      REAL ORIENT                ! Initial orientation of beam (radians)
      REAL PERCNT( 2 )           ! The percentiles
      DOUBLE PRECISION PERVAL( 2 ) ! The values at the percentiles
      INTEGER PIVOT( MXCOEF )    ! Pivot indices
      INTEGER RLBND( BF__NDIM )  ! Lower bounds of small region
      INTEGER RDIMS( BF__NDIM )  ! Dimensions of small region about beam
      INTEGER REL                ! Number of elements in small region
      DOUBLE PRECISION RMEAN     ! Mean value ibn the small region
      INTEGER RUBND( BF__NDIM )  ! Upper bounds of small region
      DOUBLE PRECISION SD( MXCOEF ) ! Std. deviations of fitted params
      REAL SIGMIN                ! Initial minor-axis standard deviation
      DOUBLE PRECISION SUM       ! Sum of values in the small region
      DOUBLE PRECISION XC( MXCOEF ) ! Free parameters
      DOUBLE PRECISION DWORK( MXCOEF ) ! Work array
      REAL RWORK( MXCOEF )       ! Work array

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Transfer arguments to initial COMMON-block values.
*  ==================================================

*  Booleans
      AMPC = FIXCON( 1 )
      BACKC = FIXCON( 2 )
      FWHMC = FIXCON( 3 )
      POSC = FIXCON( 4 )
      RATIOC = FIXCON( 5 )
      SEPARC = FIXCON( 6 )
      SHAPEC = FIXCON( 7 )
      CIRC = FIXCON( 8 )
      ORIC = FIXCON( 9 )

*  Arrays
      DO I = 1, BF__NDIM
         LBND( I ) = FLBND( I )
         UBND( I ) = FUBND( I )
      END DO

      DO J = 1, BF__MXPOS - 1
         ARATIO( J ) = DBLE( AMPRAT( J ) )
         DO I = 1, BF__NDIM
            PIXOFF( J, I ) = POFSET( J, I )
         END DO
      END DO

      DO I = 1, NP
         IG = ( I - 1 ) / BF__NCOEF + 1
         J = I - ( IG - 1 ) * BF__NCOEF
         PC( J, IG ) = P( I )
      END DO

*  Number of functions is the number of pixels to fit.
      DIMS( 1 ) = FUBND( 1 ) - FLBND( 1 ) + 1
      DIMS( 2 ) = FUBND( 2 ) - FLBND( 2 ) + 1
      NPOS = DIMS( 1 ) * DIMS( 2 )

*  Obtain some initial values to guide the fitting.
*  ================================================

*  Use the mean as a first guess at the background.
      CALL KPG1_MEAND( NPOS, %VAL( CNF_PVAL( IPWD ) ), MEAN, STATUS )

*  Use the same percentile limits for all beams.
      PERCNT( 1 ) = 4.0
      PERCNT( 2 ) = 100.0 - PERCNT( 1 )

*  This is the index of the parameters to be fitted.
      N = 0
      DO IG = 1, NBEAMS

*  Use moments to estimate the fit parameters.  The INIT array
*  contains the fitted estimates for the centroid.  101 is the
*  dimension of the marginal arrays in KPS1_STPAx.
         CALL ERR_MARK
         INIT( 1 ) = PIXPOS( IG, 1 )
         INIT( 2 ) = PIXPOS( IG, 2 )
         CALL KPS1_STPAD( DIMS( 1 ), DIMS( 2 ),
     :                    %VAL( CNF_PVAL( IPWD ) ), FLBND, 1,
     :                    MIN( 101, DIMS( 1 ), DIMS( 2 ) ), INIT,
     :                    SIGMIN, AXRAT, ORIENT, NGOOD, RWORK, STATUS )

*  Something went wrong, say the array supplied is too small, and revert
*  to semi-arbitrary default initial values for the coefficients.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            SIGMIN = 5.0
            AXRAT = 1.0
            ORIENT = 0.0
            INIT( 1 ) = PIXPOS( IG, 1 )
            INIT( 2 ) = PIXPOS( IG, 2 )

         ELSE IF ( INIT( 1 ) .GT. DBLE( FUBND( 1 ) ) .OR.
     :             INIT( 2 ) .GT. DBLE( FUBND( 2 ) ) .OR.
     :             INIT( 1 ) .LT. DBLE( FLBND( 1 ) ) .OR.
     :             INIT( 2 ) .LT. DBLE( FLBND( 2 ) ) ) THEN
            SIGMIN = 5.0
            AXRAT = 1.0
            ORIENT = 0.0
            INIT( 1 ) = PIXPOS( IG, 1 )
            INIT( 2 ) = PIXPOS( IG, 2 )
         ELSE
            AXRAT = MAX( AXRAT, 0.1 )
         END IF
         CALL ERR_RLSE

*  Choose a region about the chosen point.
         RLBND( 1 ) = MAX( NINT( INIT( 1 ) + 0.5 ) - WIDTH, LBND( 1 ) )
         RLBND( 2 ) = MAX( NINT( INIT( 2 ) + 0.5 ) - WIDTH, LBND( 2 ) )
         RUBND( 1 ) = MIN( NINT( INIT( 1 ) + 0.5 ) + WIDTH, UBND( 1 ) )
         RUBND( 2 ) = MIN( NINT( INIT( 2 ) + 0.5 ) + WIDTH, UBND( 2 ) )

*  Obtain some workspace for the region.
         RDIMS( 1 ) = RUBND( 1 ) - RLBND( 1 ) + 1
         RDIMS( 2 ) = RUBND( 2 ) - RLBND( 2 ) + 1
         REL = RDIMS( 1 ) * RDIMS( 2 )
         CALL PSX_CALLOC( REL, '_DOUBLE', IPREG, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 999

         CALL KPG1_CPNDD( 2, LBND, UBND, %VAL( CNF_PVAL( IPWD ) ),
     :                    RLBND, RUBND,  %VAL( CNF_PVAL( IPREG ) ),
     :                    REL, STATUS )

*  Find the peak value around the nominal position.  There may be
*  noise that strongly biases the result, so instead of the actual
*  peak find the percentiles near the extremes.  Note that the
*  returned percentile values are for percentiles in ascending order.
*  We are fine since the first percentile is lower than the second.  We
*  can use the mean as a first guess at whether the beam is positive or
*  negative.
         BAD = .TRUE.
         CALL KPG1_HSTAD( BAD, REL, %VAL( CNF_PVAL( IPREG ) ), 2,
     :                    PERCNT, NGOOD, MINPOS, MINVAL, MAXPOS, MAXVAL,
     :                    SUM, RMEAN, MEDIAN, MODE, PERVAL, STATUS )

         CALL PSX_FREE( IPREG, STATUS )

*  Store initial values.
*  =====================

*  Store initial values for the parameters which the PDA routine will
*  search through, and count the number of parameters to fit.
         IF ( .NOT. ( POSC .OR. ( SEPARC .AND. IG .GT. 1 ) ) ) THEN
            N = N + 2
            XC( N - 1 ) = INIT( 1 )
            XC( N ) = INIT( 2 )
         END IF

*  Unlike the fixed FWHM and background, each fixed position is
*  determined after the initial values for the P array (copied to the
*  PC array in COMMON).
         PC( 1, IG ) = INIT( 1 )
         PC( 2, IG ) = INIT( 2 )

*  Just a guess for a circular beam.  PC( 3 ) to PC( 5 ) should
*  already be filled from BEAMFIT itself, if a fixed value is
*  wanted or initial values were supplied from the environment.
         IF ( .NOT. FWHMC ) THEN
            IF ( CIRC ) THEN
               N = N + 1
               XC( N ) = DBLE( SIGMIN / SQRT( AXRAT ) )
               PC( 3, IG ) = XC( N )
               PC( 4, IG ) = PC( 3, IG )
            ELSE
               N = N + 2
               XC( N - 1 ) = DBLE( SIGMIN / AXRAT )
               XC( N ) = DBLE( SIGMIN )
               PC( 3, IG ) = XC( N - 1 )
               PC( 4, IG ) = XC( N )
            END IF
         END IF

*  Orientation needs fitting when specified as free parameter, or
*  different fixed widths were supplied.
         IF ( .NOT. ORIC ) THEN
            N = N + 1
            XC( N ) = DBLE( ORIENT )
            PC( 5, IG ) = XC( N )
         END IF

         IF ( IG .GT. 1 .AND. RATIOC ) THEN
            PC( 6, IG ) = PC( 6, 1 ) * ARATIO( IG - 1 )

*  Use the appropriate estimate near the maximum, depending on whether
*  the beam is positive or negative.
         ELSE IF ( .NOT. AMPC ) THEN
            N = N + 1
            IF ( RMEAN .LT. 0.0D0 ) THEN
               XC( N ) = PERVAL( 1 )
            ELSE
               XC( N ) = PERVAL( 2 )
            END IF
            PC( 6, IG ) = XC( N )
         END IF

*  Use the same background for all beam positions.
         IF ( IG .GT. 1 ) THEN
            PC( 7, IG ) = PC( 7, 1 )

*  PC( 7, IG ) should already be filled from BEAMFIT itself, if a fixed
*  value is wanted.
         ELSE IF ( .NOT. BACKC ) THEN
            N = N + 1
            XC( N ) = MEAN
            PC( 7, IG ) = XC( N )
         END IF

*  Start near a Gaussian exponent.
         IF ( .NOT. SHAPEC ) THEN
            N = N + 1
            XC( N ) = 1.999
            PC( 8, IG ) = XC( N )
         END IF

      END DO

*  Perform the fitting.
*  ====================

*  Get work space for the PDA routine.  Since the number of residuals
*  is dynamic, also obtain workspace to store these.
      LW = ( NPOS + 5 ) * N + NPOS
      CALL PSX_CALLOC( LW, '_DOUBLE', IPWNA2, STATUS )
      CALL PSX_CALLOC( N, '_INTEGER', IPWNA1, STATUS )
      CALL PSX_CALLOC( NPOS, '_DOUBLE', IPWEF, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Initialise the status flag used by the PDA service routine
*  KPS1_BFFN.
      ISTAT = SAI__OK

*  Do the search.  The tolerance value is fairly arbitrary.
      CALL PDA_LMDIF1( KPS1_BFFN, NPOS, N, XC,
     :                 %VAL( CNF_PVAL( IPWEF ) ), 1.0D-4, IFAIL,
     :                 %VAL( CNF_PVAL( IPWNA1 ) ),
     :                 %VAL( CNF_PVAL( IPWNA2 ) ), LW )

      CALL PSX_FREE( IPWNA1, STATUS )
      CALL PSX_FREE( IPWNA2, STATUS )

*  If an error occurred in the PDA service routine (KPS1_BFFN), add a
*  context message, and then flush the error (so that other projections
*  will be attempted) unless the user has specified a specific
*  projection.
      IF ( STATUS .NE. SAI__OK ) GO TO 999
      IF ( ISTAT .NE. SAI__OK ) THEN
         STATUS = ISTAT
         CALL ERR_REP( 'KPS1_BFFT_ERR2', 'Error fitting generalised '/
     :                 /' Gaussian.', STATUS )
      ELSE

*  Report an error if the PDA routine could not find an answer.
         IF ( IFAIL .EQ. 0 .OR. IFAIL .GT. 4 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'IFAIL', IFAIL )
            CALL ERR_REP( 'KPS1_BFFT_ERR3', 'Routine '/
     :         /'PDA_LMDIF1 returned INFO = ^IFAIL.', STATUS )
            GO TO 999

*  Try to be more helpful with the most-common error.
         ELSE IF ( IFAIL .EQ. 4 ) THEN
            STATUS = KAP__LMFOJ
            CALL ERR_REP( 'KPS1_BFFT_ERR3', 'The returned fitted '/
     :         /'function from the Levenberg-Marquardt minimisation  '/
     :         /'is orthogonal to the columns of the Jacobian.  Try '/
     :         /'enlarging the fitting area.', STATUS )
         END IF
      END IF

*  Transfer the fit parameters, including the fixed values, from the
*  common to the argument.
      DO I = 1, NP
         IG = ( I - 1 ) / BF__NCOEF + 1
         J = I - ( IG - 1 ) * BF__NCOEF
         P( I ) = PC( J, IG )
      END DO

*  Obtain the rms of the fit.
*  ==========================

*  Compute the sum of the squared residuals.  Since the array of
*  residuals is dynamic it is obtained as workspace therefore a
*  subroutine must be called to evaluate its sum.
      FS = VAL__MAXD
      CALL KPG1_SQSUD( NPOS, %VAL( CNF_PVAL( IPWEF ) ),
     :                 FS, STATUS )

*  If no solution was found, report an error.
      IF ( STATUS .NE. SAI__OK ) GO TO 999
      IF ( FS .EQ. VAL__MAXD ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_BFFT_ERR4', 'Unable to find any '/
     :                 /'usable parameter values. ', STATUS )
         GO TO 999
      END IF

*  Return the RMS positional error.
      RMS = SQRT( MAX( 0.0D0, FS / DBLE( NPOS ) ) )

*  Determine fit errors
*  ====================
      CALL PSX_CALLOC( NPOS, '_DOUBLE', IPDRES, STATUS )
      CALL PSX_CALLOC( NPOS * N, '_DOUBLE', IPJAC, STATUS )
      CALL PSX_CALLOC( N * N, '_DOUBLE', IPCURV, STATUS )
      CALL PSX_CALLOC( N * N, '_DOUBLE', IPCORR, STATUS )
      CALL PSX_CALLOC( N * N, '_DOUBLE', IPCOVA, STATUS )

      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Initialise the errors.  This allows for parameters not fitted.
      DO I = 1, NP
         SIGMA( I ) = VAL__BADD
      END DO

*  Determine the parameter errors.
      CALL PDA_LMERR( KPS1_BFFN, NPOS, N, XC,
     :                %VAL( CNF_PVAL( IPWEF ) ), 5,
     :                %VAL( CNF_PVAL( IPDRES ) ),
     :                %VAL( CNF_PVAL( IPJAC ) ),
     :                %VAL( CNF_PVAL( IPCURV ) ), PIVOT, DWORK, SD,
     :                %VAL( CNF_PVAL( IPCORR ) ),
     :                %VAL( CNF_PVAL( IPCOVA ) ), FLAG, STATUS )

*  Place the fitted errors into the full array.  Non-fitted values
*  will have bad errors.
      N = 0
      DO IG = 1, NBEAMS
         GO = ( IG - 1 ) * BF__NCOEF

         IF ( .NOT. ( POSC .OR. ( SEPARC .AND. IG .GT. 1 ) ) ) THEN
            N = N + 2
            SIGMA( 1 + GO ) = SD( N - 1 )
            SIGMA( 2 + GO ) = SD( N )
         END IF

         IF ( .NOT. FWHMC ) THEN
            IF ( CIRC ) THEN
               N = N + 1
               SIGMA( 3 + GO ) = SD( N )
               SIGMA( 4 + GO ) = SIGMA( 3 + GO )
            ELSE
               N = N + 2
               SIGMA( 3 + GO ) = SD( N - 1 )
               SIGMA( 4 + GO ) = SD( N )
            END IF
         END IF

         IF ( .NOT. ORIC ) THEN
            N = N + 1
            SIGMA( 5 + GO ) = SD( N )
         END IF

         IF ( IG .GT. 1 .AND. RATIOC ) THEN
            SIGMA( 6 + GO ) = SIGMA( 6 ) * ABS( ARATIO( IG - 1 ) )

         ELSE IF ( .NOT. AMPC ) THEN
            N = N + 1
            SIGMA( 6 + GO ) = SD( N )
         END IF

         IF ( .NOT. BACKC .AND. IG .EQ. 1 ) THEN
            N = N + 1
            SIGMA( 7 + GO ) = SD( N )
         ELSE
            SIGMA( 7 + GO ) = SIGMA( 7 )
         END IF

         IF ( .NOT. SHAPEC ) THEN
            N = N + 1
            SIGMA( 8 + GO ) = SD( N )
         END IF

      END DO

*  Jump to here if an error occurs.
 999  CONTINUE

*  Free the work space.
      CALL PSX_FREE( IPDRES, STATUS )
      CALL PSX_FREE( IPJAC, STATUS )
      CALL PSX_FREE( IPCURV, STATUS )
      CALL PSX_FREE( IPCORR, STATUS )
      CALL PSX_FREE( IPCOVA, STATUS )
      CALL PSX_FREE( IPWEF, STATUS )

      END
