      SUBROUTINE KPS1_RPRFW( DIM1, DIM2, ARRAY, LBND, SIG0, AXISR,
     :                        THETA, RANGE, GAUSS, NXY, POS, WT, RSCALE,
     :                        RSCL2, NBIN1, NBIN2, BINPTS, DMODE, R,
     :                        LSTART, NPTS, DATA, IADR, NXTADR, PROFIL,
     :                        PROFR, PROFWT, FWHM, AMP, BACK, SIGMA,
     :                        GAMMA, AMP1, STATUS )
*+
*  Name:
*     KPS1_RPRF<X>

*  Purpose:
*     Fits a radial profile to a set of star images.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_RPRF<X>( DIM1, DIM2, ARRAY, LBND, SIG0, AXISR, THETA,
*                      RANGE, GAUSS, NXY, POS, WT, RSCALE, RSCL2, NBIN1,
*                      NBIN2, BINPTS, DMODE, R, LSTART, NPTS, DATA,
*                      IADR, NXTADR, PROFIL, PROFR, PROFWT, FWHM, AMP,
*                      BACK, SIGMA, GAMMA, AMP1, STATUS )

*  Description:
*     This routine combines the profiles of a number of stars whose
*     individual centroids are known, as well as the mean shape, sigma
*     and orientation for the entire sample.  It derives the two
*     parameters that define a Gaussian fit to the set of star images.

*  Arguments:
*     DIM1 = INTEGER (Given)
*        The number of pixels per line of the array.
*     DIM2 = INTEGER (Given)
*        The number of lines in the array.
*     ARRAY( DIM1, DIM2 ) = INTEGER*2 (Given)
*        The input array in which the star images lie.
*     LBND( 2 ) = INTEGER (Given)
*        The lower bounds of the input array.
*     SIG0 = REAL (Given)
*        The star 'sigma' across the minor axis.
*     AXISR = REAL (Given)
*        The star axis ratio.
*     THETA = REAL (Given)
*        The orientation of star major axis to the x axis in radians
*        (x through y positive).
*     RANGE = REAL (Given)
*        The number of star 'sigma's out to which the radial profile is
*        fitted.
*     GAUSS = LOGICAL (Given)
*        If .TRUE., the radial-fall-off parameter (gamma) is fixed to be
*        2; in other words the best-fitting two-dimensional Gaussian is
*        evaluated.  If .FALSE., gamma is a free parameter of the fit,
*        and the derived value is returned in argument GAMMA.
*     POS( NXY, 2 ) = DOUBLE PRECISION (Given)
*        Each line comprises the accurate x then y positions of a
*        star centre.
*     NXY = INTEGER (Given)
*        The number of stars to be fitted.
*     WT( NXY ) = REAL (Given)
*        The accurate star-centre y positions.
*     RSCALE = REAL (Given)
*        The scale factor for converting radial distance into bins
*        for individual point-spread functions.
*     RSCL2 = REAL (Given)
*        The scale factor for converting radial distance into bins
*        for the mean point-spread functions.
*     NBIN1 = INTEGER (Given)
*        The number of bins for an individual point-spread function.
*     NBIN2 = INTEGER (Given)
*        The number of bins for the mean point-spread function.
*     BINPTS = INTEGER (Given)
*        The number of pixels in the outermost radial bin.
*     DMODE( NBIN1 ) = REAL (Returned)
*        Workspace for the mode for each radial bin.
*     R( NBIN1 ) = REAL (Returned)
*        Workspace for the mean radii for each bin.
*     LSTART( NBIN1 ) = INTEGER (Returned)
*        Workspace for the linked-list starts for each bin.
*     NPTS( NBIN1 ) = INTEGER (Returned)
*        Workspace for the number of pixels in each bin.
*     DATA( BINPTS ) = ? (Returned)
*        Work array to hold the pixels of a radial bin.
*     IADR( BINPTS ) = INTEGER (Returned)
*        Work array to hold a link-list of pixel indices.
*     NXTADR( BINPTS ) = INTEGER (Returned)
*        Work array to hold a next pixel indices in a linked list.
*     PROFIL( NBIN2 ) = REAL (Returned)
*        The mean-profile value in each bin.
*     PROFR( NBIN2 ) = REAL (Returned)
*        The mean-profile radius in each bin
*     PROFWT( NBIN2 ) = REAL (Returned)
*        The weights of the mean profile in each bin.
*     FWHM = REAL (Returned)
*        An estimate of the full width at half maximum across the minor
*        axis of a star image.
*     AMP = REAL (Returned)
*        The Gaussian amplitude of the fitted mean point-spread
*        function.
*     BACK = REAL (Returned)
*        The background level of the mean point-spread function.
*     SIGMA = REAL (Returned)
*        The Gaussian width (sigma) of the fitted mean point-spread
*        function across the minor axis.
*     GAMMA = REAL (Returned)
*        Star radial-profile parameter, gamma, of the fitted mean
*        point-spread function.
*     AMP1 = REAL (Returned)
*        The Gaussian amplitude of the fitted point-spread function for
*        the first usable star.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  There is a routine for each numeric data type. The arrays ARRAY
*     and DATA supplied to the routine must have the data type specified.

*  Copyright:
*     Copyright (C) 1981, 1990-1992 Science & Engineering Research
*     Council. Copyright (C) 1998-2000 Central Laboratory of the
*     Research Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (Durham Univ.)
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1981 (RFWS):
*        Original version.
*     1990 September 20 (MJC):
*        Made generic, renamed from RADPRF, removed INVAL and ILEVEL,
*        passed extra arguments for the plotting and graphics database,
*        a log file, combined x and y positions into a single array,
*        commented the variables, and converted the prologue.
*     1991 July 5 (MJC):
*        Passed arrays from calling routine.  Removed plotting facility,
*        but returned extra parameters: the mean-profile values,
*        weights, and radii; the amplitude; Gaussian width; and the
*        background.
*     1992 April 2 (MJC):
*        Reordered the ARRAY argument to its normal location after the
*        dimensions.  Added LBND argument and corrected the x-y
*        positions as if the lower bounds were both one.
*     1998 May 26 (MJC):
*        Added GAUSS argument.  Used modern style of variable ordering.
*        Flipped the order of NXY and POS.
*     20-SEP-1999 (DSB):
*        Swapped order of POS indices and made it _DOUBLE for use with AST.
*     2-MAY-2000 (DSB):
*        Added argument AMP1.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Magic-value definitions

*  Arguments Given:
      INTEGER DIM1
      INTEGER DIM2
      INTEGER*2 ARRAY( DIM1 * DIM2 )
      INTEGER LBND( 2 )
      REAL SIG0
      REAL AXISR
      REAL THETA
      REAL RANGE
      LOGICAL GAUSS
      INTEGER NXY
      DOUBLE PRECISION POS( NXY, 2 )
      REAL WT( NXY )
      REAL RSCALE
      REAL RSCL2
      INTEGER NBIN1
      INTEGER NBIN2
      INTEGER BINPTS

*  Arguments Returned:
      REAL DMODE( 0:NBIN1-1 )
      REAL R( 0:NBIN1-1 )
      INTEGER LSTART( 0:NBIN1-1 )
      INTEGER NPTS( 0:NBIN1-1 )
      INTEGER*2 DATA( BINPTS )
      INTEGER IADR( BINPTS )
      INTEGER NXTADR( BINPTS )
      REAL PROFIL( 0:NBIN2-1 )
      REAL PROFR( 0:NBIN2-1 )
      REAL PROFWT( 0:NBIN2-1 )
      REAL FWHM
      REAL AMP
      REAL BACK
      REAL SIGMA
      REAL GAMMA
      REAL AMP1

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL PBAD                  ! Probability that a pixel is corrupt
      PARAMETER ( PBAD = 0.01 )
      INTEGER NITER              ! Maximum number of iterations during
                                 ! the cleaning
      PARAMETER ( NITER = 10 )
      INTEGER NITFIT             ! Maximum number of iterations during
                                 ! the fitting
      PARAMETER ( NITFIT = 20 )
      REAL TOLFIT                ! Tolerance of the fitting
      PARAMETER ( TOLFIT = 0.0005 )

*  Local Variables:
      INTEGER BIN                ! Bin counter for a star
      INTEGER BIN2               ! Bin counter for the mean star
      REAL C                     ! Cosine of the orientation
      REAL CONSTA                ! Constant for relating distance in
                                 ! elliptical star image to effective
                                 ! distance along the minor axis
      REAL CONSTB                ! Constant for relating distance in
                                 ! elliptical star image to effective
                                 ! distance along the minor axis
      REAL CONSTC                ! Constant for relating distance in
                                 ! elliptical star image to effective
                                 ! distance along the minor axis
      REAL DET                   ! Determinant
      REAL DSIG                  ! The sigma of a radial bin
      REAL DX                    ! Pixel displacement in x from the star
                                 ! centre
      REAL DX0                   ! X displacement of the first pixel in
                                 ! the box
      REAL DX2                   ! X pixel displacement squared
      REAL DXY                   ! X * y displacement
      REAL DY                    ! Pixel displacement in y from the star
                                 ! centre
      REAL DY2                   ! Y pixel displacement squared
      REAL FUNCT                 ! Gaussian function at a bin
      REAL FW                    ! Weighted Gaussian function at a bin
      INTEGER I                  ! Loop counter for pixels in the box
                                 ! around a star
      INTEGER I0                 ! X pixel index nearest the star centre
      INTEGER ICOUNT             ! Number of bin values in DATA
      INTEGER IMAX               ! X pixel upper index of region around
                                 ! a star
      INTEGER IMIN               ! X pixel lower index of region around
                                 ! a star
      INTEGER IMLOCN             ! Index to data vector
      INTEGER INDEX              ! Linked-list counter
      INTEGER ISHIFT             ! Half-width of the square around the
                                 ! star image
      INTEGER J                  ! Loop counter for pixels in the box
                                 ! around a star
      INTEGER J0                 ! Y pixel index nearest the star centre
      INTEGER JMAX               ! Y pixel upper index of region around
                                 ! a star
      INTEGER JMIN               ! Y pixel lower index of region around
                                 ! a star
      REAL RAXISR                ! Reciprocal minor-axis squared
      REAL RDASH                 ! Effective radius
      REAL RLIMIT                ! Radius limit in the minor axis
      REAL RSIG                  ! Reciprocal sigma
      REAL S                     ! Sine of the orientation
      INTEGER STAR               ! Number of the star
      DOUBLE PRECISION SUMD      ! Sum of weighted data values
      DOUBLE PRECISION SUMDF     ! Sum of Gaussian weighted mode
      DOUBLE PRECISION SUMF      ! Sum of Gaussian weights
      DOUBLE PRECISION SUMF2     ! Sum of Gaussian weights squared
      DOUBLE PRECISION SUMW      ! Sum of the number of points
      REAL XC                    ! X centre of the current star
                                 ! corrected for origin
      REAL YC                    ! Y centre of the current star
                                 ! corrected for origin

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Calculate the constants for relating radial distance in an
*  elliptical star image to effective distance along the minor axis.
      S = SIN( THETA )
      C = COS( THETA )
      RAXISR = ( 1.0 / AXISR ) ** 2
      CONSTA = S * S + C * C * RAXISR
      CONSTB = C * C + S * S * RAXISR
      CONSTC = 2.0 * C * S * ( RAXISR - 1.0 )

*  Calculate scale factors for converting radial distance to bins.
      RSIG = 1.0 / SIG0

*  Find the radius limit in the minor axis direction imposed by the
*  fitting limit 'range'.
      RLIMIT = RANGE * SIG0

*  Find the size of square to be scanned around each star to
*  accommodate the radial fitting range.
      ISHIFT = MAX( 1, NINT( RLIMIT * AXISR ) )

*  Initialise the arrays to hold the mean radial profile.
      DO BIN = 0, NBIN2 - 1
         PROFR( BIN ) = 0.0
         PROFIL( BIN ) = 0.0
         PROFWT( BIN ) = 0.0
      END DO

*  Indicate that the fitted amplitude of the first usable star is not yet
*  known.
      AMP1 = VAL__BADR

*  Consider each star which has positive weight.
      DO STAR = 1, NXY

         IF ( WT( STAR ) .GT. 1.0E-10 ) THEN

*  Find the centre of the search square.  The star positions are
*  supplied in world co-ordinates.  In this routine the array has lower
*  bounds which are both one, but the positions may not.  Therefore
*  correct the positions to what they would be given lower bounds of
*  one.
            XC = REAL( POS( STAR, 1 ) ) - REAL( LBND( 1 ) - 1 )
            YC = REAL( POS( STAR, 2 ) ) - REAL( LBND( 2 ) - 1 )

            I0 = NINT( MIN( MAX( -1.0E8, XC ), 1.0E8 ) )
            J0 = NINT( MIN( MAX( -1.0E8, YC ), 1.0E8 ) )

*  Find the edges of the search square centred on the star.
            IMIN = MAX( 1, I0 - ISHIFT )
            IMAX = MIN( DIM1, I0 + ISHIFT )
            JMIN = MAX( 1, J0 - ISHIFT )
            JMAX = MIN( DIM2, J0 + ISHIFT )

*  Initialise the arrays to point to the start of a linked list of all
*  the pixels in a given radial bin and to form the mean radius for
*  each bin.
            DO BIN = 0, NBIN1 - 1
               R( BIN ) = 0.0
               NPTS( BIN ) = 0
               LSTART( BIN ) = 0
            END DO

*  Scan the square around the star, calculating the x and y
*  displacements from the centre.  Note the use of a vector index.
            INDEX = 0
            DY = JMIN - YC - 1.0
            DX0 = IMIN - XC - 1.0

            DO J = JMIN, JMAX
               IMLOCN = ( J - 1 ) * DIM1 + IMIN - 1
               DY = DY + 1.0
               DY2 = DY * DY
               DX = DX0

               DO I = IMIN, IMAX
                  IMLOCN = IMLOCN + 1
                  DX = DX + 1.0

*  If the pixel is valid, calculate the effective radius.
                  IF ( ARRAY( IMLOCN ) .NE. VAL__BADW ) THEN
                     DX2 = DX * DX
                     DXY = DX * DY
                     RDASH = SQRT( CONSTA * DX2 + CONSTB * DY2 +
     :                             CONSTC * DXY )

*  Find the radial bin.
                     BIN = INT( RDASH * RSCALE )

                     IF ( BIN .LT. NBIN1 .AND. RDASH .LE. RLIMIT ) THEN

*  Form sums for the effective mean radius.
                        NPTS( BIN ) = NPTS( BIN ) + 1
                        R( BIN ) = R( BIN ) + RDASH

*  Form a linked list of all the pixels in this radial bin.
                        INDEX = INDEX + 1
                        IADR( INDEX ) = IMLOCN
                        NXTADR( INDEX ) = LSTART( BIN )
                        LSTART( BIN ) = INDEX
                     END IF

                  END IF

               END DO

            END DO

*  Initialise sums for forming a least-squares Gaussian fit to the star
*  radial profile and background.
            SUMW = 0.0D0
            SUMF = 0.0D0
            SUMF2 = 0.0D0
            SUMDF = 0.0D0
            SUMD = 0.0D0

*  Consider each bin with one or more points in it.
            DO BIN = 0, NBIN1 - 1

               IF ( LSTART( BIN ) .GT. 0 ) THEN

*  Extract the pixels in this bin from the linked list and store in the
*  array 'data'.  The linked list only points to good pixels and so
*  further bad-pixel checking is not required.
                  ICOUNT = 0
                  INDEX = LSTART( BIN )

    5             CONTINUE
                  IF ( INDEX .GT. 0 ) THEN
                     ICOUNT = ICOUNT + 1
                     DATA( ICOUNT ) = ARRAY( IADR( INDEX ) )
                     INDEX = NXTADR( INDEX )
                     GO TO 5

                  END IF

*  Find the most likely value in the bin.
                  IF ( ICOUNT .EQ. 1 ) THEN
                     DMODE( BIN ) = DATA( 1 )

                  ELSE
                     CALL KPG1_MODEW( DATA, ICOUNT, PBAD, NITER, 0.1,
     :                                  DMODE( BIN ), DSIG, STATUS )
                  END IF

*  Form the mean effective radius for the bin.
                  R( BIN ) = R( BIN ) / NPTS( BIN )

*  Form sums for fitting a Gaussian profile using the number of data
*  points as a weight.
                  FUNCT = EXP( - 0.5 * ( R( BIN ) * RSIG ) *  * 2 )
                  FW = FUNCT * NPTS( BIN )
                  SUMW = SUMW + NPTS( BIN )
                  SUMF = SUMF + FW
                  SUMF2 = SUMF2 + ( FUNCT * FW )
                  SUMDF = SUMDF + DMODE( BIN ) * FW
                  SUMD = SUMD + DMODE( BIN ) * NPTS( BIN )
               END IF

            END DO

*  Solve the normal equations for the Gaussian fit.
            DET = REAL( SUMF2 * SUMW - SUMF * SUMF )

            IF ( DET .NE. 0.0 ) THEN
               AMP = REAL( SUMW * SUMDF - SUMD * SUMF ) / DET
               BACK = REAL( SUMF2 * SUMD - SUMDF * SUMF ) / DET

*  Save the fitted amplitude of the first usable star.
               IF( AMP1 .EQ. VAL__BADR ) AMP1 = AMP

*  Insert the star profile into the mean-profile bins.
               DO BIN = 0, NBIN1 - 1

                  IF ( LSTART( BIN ) .GT. 0 ) THEN
                     BIN2 = INT( R( BIN ) * RSCL2 )

                     IF ( BIN2 .LE. NBIN2 - 1 ) THEN

*  Use the number of points and the star amplitude as weights during
*  the summations.
                        PROFIL( BIN2 ) = PROFIL( BIN2 ) + NPTS( BIN ) *
     :                                   ( DMODE( BIN ) - BACK )
                        PROFR( BIN2 ) = PROFR( BIN2 ) + R( BIN ) * AMP *
     :                                  NPTS( BIN )
                        PROFWT( BIN2 ) = PROFWT( BIN2 ) + AMP *
     :                                   NPTS( BIN )
                     END IF
                  END IF

               END DO

*  End of positive-determinant check.
            END IF

*  End of check to include star or not.
         END IF

*  End of the loop for each star.
      END DO

*  Having processed the individual stars we can now
*  compute a mean profile.
*  =======================

*  Calculate the mean radial profile and associated radii from the
*  binned data.
      DO BIN = 0, NBIN2 - 1

         IF ( PROFWT( BIN ) .GT. 0.0 ) THEN
            PROFIL( BIN ) = PROFIL( BIN ) / PROFWT( BIN )
            PROFR( BIN ) = PROFR( BIN ) / PROFWT( BIN )
         END IF

      END DO

*  Set initial estimates of profile parameters and call routine to
*  obtain a full least-squares fit to the profile.
      AMP = 1.0
      BACK = 0.0
      GAMMA = 2.0
      SIGMA = SIG0
      CALL KPS1_PGFTR( PROFIL, PROFWT, PROFR, NBIN2, NITFIT, GAUSS,
     :                 TOLFIT, AMP, BACK, SIGMA, GAMMA, STATUS )

      FWHM = VAL__BADR

*  If fit was not successful.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Calculate full-width-half-maximum seeing.
         FWHM = 2.0 * SIGMA * ( 1.38629 ** ( 1.0 / GAMMA ) )

      END IF

      END
