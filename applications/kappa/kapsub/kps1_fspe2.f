      SUBROUTINE KPS1_FSPE2( NBIN, X, Y, Z, XMIN, XMAX, YMIN, YMAX,
     :                       NXPAR, NYPAR, MCHOEF, CHCOEF, NCOEF, FIT,
     :                       RESID, RMS, STATUS )
*+
*  Name:
*     KPS1_FSPE2

*  Purpose:
*     Evaluates a bivariate Chebyshev polynomial series for a set of
*     x-y positions.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL KPS1_FSPE2( NBIN, X, Y, Z, XMIN, XMAX, YMIN, YMAX, NXPAR,
*                       NYPAR, MCHOEF, CHCOEF, NCOEF, FIT, RESID, RMS,
*                       STATUS )

*  Description:
*     This routine evaluates a bivariate Chebyshev polynomial at the
*     given vectors of x-y co-ordinates, and returns the fit and the
*     residuals.  The rms difference between the fitted and the original
*     values is also calculated.

*  Arguments:
*     NBIN = INTEGER (Given)
*        The number of points at which the polynomial is to be
*        evaluated.
*     X( * ) = DOUBLE PRECISION (Given)
*        The x co-ordinates of the points to be evaluated.
*     Y( * ) = DOUBLE PRECISION (Given)
*        The y co-ordinates of the points to be evaluated.
*     Z( * ) = DOUBLE PRECISION (Given)
*        The values at the given x-y positions before fitting.
*     XMIN = DOUBLE PRECISION (Given)
*        Lower end of the x range of the fit.  It must not be greater
*        than the x position of the first pixel in the data array.
*     YMIN = DOUBLE PRECISION (Given)
*        Lower end of the y range of the fit.  It must not be greater
*        than the y position of the first pixel in the data array.
*     XMAX = DOUBLE PRECISION (Given)
*        Upper end of the x range of the fit.  It must not be less
*        than the x position of the last pixel in the data array.
*     YMAX = DOUBLE PRECISION (Given)
*        Lower end of the y range of the fit.  It must not be less
*        than the y position of the last pixel in the data array.
*     NXPAR = INTEGER (Given)
*        The number of parameters of the FIT in the x direction, i.e
*        the degree of the polynomial plus one.
*     NYPAR = INTEGER (Given)
*        The number of parameters of the FIT in the y direction, i.e
*        the degree of the polynomial plus one.
*     MCHOEF = INTEGER (Given)
*        The dimension of the array of Chebyshev coefficients.
*     CHCOEF( MCHOEF ) = DOUBLE PRECISION (Given)
*        The Chebyshev polynomial coefficients, in the order increasing
*        x power for each increasing y power.  Thus coefficient Aij in
*        the standard convention is %CHCOEF(i*(%NYPAR)+j+1).  The array
*        may be rectangular, i.e. the highest x and y orders do not
*        have to be the same.
*     NCOEF = INTEGER (Given)
*        The number of Chebyshev coefficients.
*     FIT( * ) = DOUBLE PRECISION (Returned)
*        The fitted array.
*     RESID( * ) = DOUBLE PRECISION (Returned)
*        The residuals of the fit.
*     RMS = DOUBLE PRECISION (Returned)
*        The rms difference between the raw array and the fitted array.
*        The is returned as the bad value if there was no fit.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Uses the magic-value method for bad or undefined pixels.

*  Algorithm:
*     -  Initialise the rms sums.
*     -  Scan through the pixels.
*     -  Evaluate the Chebyshev surface at the pixel.
*     -  Form residuals and sums for the rms error of the fit.
*     -  At the end of the loop calculate the rms error.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1990 Jan 30 (MJC):
*        Original version.
*     1995 August 3 (MJC):
*        Renamed from PLY2EB.  Used a modern-style prologue and coding.
*        Made RMS argument double precision.  Made NBIN the first
*        argument.
*     1996 October 8 (MJC):
*        Removed NAG.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Bad-pixel definitions

*  Arguments Given:
      INTEGER NBIN               ! Number of points for evaluation
      DOUBLE PRECISION X( * )    ! X co-ordinates of the data
      DOUBLE PRECISION Y( * )    ! Y co-ordinates of the data
      DOUBLE PRECISION Z( * )    ! Data values
      DOUBLE PRECISION XMIN, XMAX ! X bounds of the fit
      DOUBLE PRECISION YMIN, YMAX ! Y bounds of the fit
      INTEGER NXPAR              ! X degree of the polynomial plus 1
      INTEGER NYPAR              ! Y degree of the polynomial plus 1
      INTEGER MCHOEF             ! Dimension of Chebyshev coeff. array
      DOUBLE PRECISION CHCOEF( MCHOEF ) ! Chebyshev coefficients
      INTEGER NCOEF             ! Number of Chebyshev coefficients

*  Arguments Returned:
      DOUBLE PRECISION FIT( * )  ! Fitted data
      DOUBLE PRECISION RESID( * ) ! Residuals
      DOUBLE PRECISION RMS       ! RMS difference between the fitted and
                                 ! unfitted arrays

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXPAR              ! Maximum number of parameters which
                                 ! can be handled in each direction
      PARAMETER ( MXPAR = 15 )

*  Local Variables:
      INTEGER I                  ! Loop counter
      INTEGER NPT                ! Number of points used to calculate
                                 ! the rms
      DOUBLE PRECISION PX( MXPAR ) ! Work array
      DOUBLE PRECISION SUMSQ     ! Sum of the square of differences

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise sums to form the rms error of the fit.
      RMS = VAL__BADD
      SUMSQ = 0.0D0
      NPT = 0

*  Scan through the pixels.
      DO I = 1, NBIN

*  Evaluate the fitted surface at all pixels.  This has to be a
*  pixel at a time because the following routine will only process
*  an array of x co-oridnates at a constant y co-ordinate.
         CALL KPG1_CHE2D( 1, XMIN, XMAX, X( I ), YMIN, YMAX, Y( I ),
     :                    NXPAR - 1, NYPAR - 1, MCHOEF, CHCOEF, MXPAR,
     :                    PX, FIT( I ), STATUS )

*  Form residuals and sums for the rms error of the fit.
         IF ( Z( I ) .NE. VAL__BADD ) THEN
            RESID( I ) = FIT( I ) - Z( I )
            SUMSQ = SUMSQ + RESID( I ) ** 2
            NPT = NPT + 1
         ELSE
            RESID( I ) = VAL__BADD
         END IF

*  End of the loop through the pixels.
      END DO

*  Calculate the rms error of the fit.
      IF ( NPT .GE. 1 ) RMS = SQRT( SUMSQ / DBLE( NPT ) )

      END
