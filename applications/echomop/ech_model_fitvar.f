      SUBROUTINE ECH_MODEL_FITVAR(
     :           XDATA,
     :           DATA,
     :           N_POINTS,
     :           FITTER,
     :           FIT,
     :           FIT_SIGMA,
     :           NPOLY,
     :           NREJ,
     :           REJ_THRESH,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_MODEL_FITVAR

*  Purpose:
*     Monte-carlo simulation to estimate variance on fits.

*  Description:
*     The polynomial fit is made to the sky data in xdata,ydata and this
*     fit is then subjected to a number of trial re-fits each consisting
*     of fit+gaussian noise. The mean and variance of the set of fits
*     is calculated for each point along the dataset and these values
*     are stored into fit_sigma. These variances on the fitted values
*     are then utilised as weights when fitting polynomials in the
*     other dimension (spatial). This method can easily be modified to
*     incorporate fits other than polynomials in the future and avoids the
*     need to work out the variance by inverting the chosen function
*     which is often difficult.

*  Invocation:
*     CALL ECH_MODEL_FITVAR(
*     :    xdata,
*     :    data,
*     :    n_points,
*     :    fitter,
*     :    fit,
*     :    fit_sigma,
*     :    npoly,
*     :    nrej,
*     :    rej_thresh,
*     :    status
*     :   )

*  Arguments:
*     XDATA = REAL (Given)
*        X data to be fitted.
*     DATA = REAL (Given)
*        Y (values)  data to be fitted.
*     N_POINTS = INTEGER (Given)
*        No of points to fit.
*     FIT = REAL (Given)
*        Fitted values.
*     FIT_SIGMA = REAL (Returned)
*        Variances on fitted values.
*     NPOLY = INTEGER (Given)
*        No of polynomial coefficients.
*     NREJ = INTEGER (Given)
*        No of reject cycles for fitting.
*     REJ_THRESH = REAL (Given)
*        Reject threshold for fitting.
*     FITTER = CHAR (Given)
*        Type of wavelength fitting function in use (POLY/SPLINE).
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     Fit the initial polynomial
*     Loop accumulating fit variance estimates
*        Fit a polynomial to simulated data
*     End loop
*     Calculate observed mean variance in simulation

*  Bugs:
*     None known.

*  Authors:
*     Dave Mills STARLINK (ZUVAD::DMILLS)

*  History:
*     1992 Sept 1 : Initial release

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_ENVIRONMENT.INC'

*  Arguments Given:
      INTEGER N_POINTS
      REAL FIT( N_POINTS )
      CHARACTER*( * ) FITTER
      INTEGER NPOLY
      INTEGER NREJ
      REAL REJ_THRESH

*  Arguments Returned:
      REAL XDATA( N_POINTS )
      REAL FIT_SIGMA( N_POINTS )

*  Workspace:
      REAL DATA( N_POINTS )

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER MAX_POINTS
      PARAMETER ( MAX_POINTS = 8192 )

*  Local Variables:
      DOUBLE PRECISION TEMP_COEFFS( MAX_FIT_COEFFS )

      REAL MEAN( MAX_POINTS )
      REAL MEANSQ( MAX_POINTS )
      REAL SIM_FIT( MAX_POINTS )
      REAL SIMUL( MAX_POINTS )
      REAL THRHI
      REAL HIT

      INTEGER I
      INTEGER J
      INTEGER MY_N_POINTS
      INTEGER LOOPS
      INTEGER NSREJ
      INTEGER SEED

      CHARACTER*80 WORK_STRING

*  Functions Called:
      REAL GAUSS_NOISE
      EXTERNAL GAUSS_NOISE
*.

*  Fit the initial polynomial.
      THRHI = REJ_THRESH
      MY_N_POINTS = MIN( N_POINTS, MAX_POINTS )

      DO J = 1, MY_N_POINTS
         FIT_SIGMA( J ) = 1.0
         MEAN( J ) = 0.0
         MEANSQ( J ) = 0.0
      END DO

      WORK_STRING = 'REAL-' // FITTER
      CALL ECH_FITTER( WORK_STRING, NPOLY, TEMP_COEFFS,
     :     MY_N_POINTS, XDATA, DATA, FIT_SIGMA,
     :     NREJ, THRHI, STATUS )
      CALL ECH_FEVAL( FITTER, NPOLY, TEMP_COEFFS,
     :     MY_N_POINTS, XDATA, FIT, STATUS )

*  Loop accumulating fit variance estimates.
      LOOPS = 20
      NSREJ = 0
      DO I = 1, LOOPS
         DO J = 1, MY_N_POINTS
            HIT = GAUSS_NOISE( SEED )
            SIMUL( J ) = FIT( J ) + HIT * SQRT( MAX( 1., FIT( J ) ) )
            FIT_SIGMA( J ) = 1.0
         END DO

*     Fit a polynomial to simulated data.
         WORK_STRING = 'REAL-' // FITTER
         CALL ECH_FITTER( WORK_STRING, NPOLY, TEMP_COEFFS,
     :        MY_N_POINTS, XDATA, SIMUL, FIT_SIGMA,
     :        NSREJ, THRHI, STATUS )
         CALL ECH_FEVAL( FITTER, NPOLY, TEMP_COEFFS,
     :        MY_N_POINTS, XDATA, SIM_FIT, STATUS )

         DO J = 1, MY_N_POINTS
            MEAN( J ) = MEAN( J ) + SIM_FIT( J )
            MEANSQ( J ) = MEANSQ( J ) + SIM_FIT( J ) * SIM_FIT( J )
         END DO
      END DO

*  Calculate observed mean variance in simulation.
      DO J = 1, MY_N_POINTS
         MEAN( J ) = MEAN( J ) / FLOAT( LOOPS )
         MEANSQ( J ) = MEANSQ( J ) / FLOAT( LOOPS )
         FIT_SIGMA( J ) = ABS( MEANSQ( J ) - MEAN( J ) * MEAN( J ) )
      END DO

      END


      REAL FUNCTION GAUSS_NOISE( DUMMY )
*+
*  Name:
*     ECHOMOP - GAUSS_NOISE

*  Purpose:
*     Returns gaussian noise with standarad deviation of 1.0.

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER DUMMY       ! Currently not used.

*  Local Variables:
      REAL SUM
      REAL FNUM

      INTEGER INUM
      INTEGER MAXNUM
      INTEGER I
      INTEGER STATUS
*.

      STATUS = SAI__OK

      SUM = 0.0
      DO I = 1, 12
         CALL PSX_RAND( INUM, MAXNUM, FNUM, STATUS )
         SUM = SUM + FNUM
      END DO

      GAUSS_NOISE = SUM - 6.0

      END
