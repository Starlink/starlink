      SUBROUTINE PON_STAT( X, Y, NDATA, SIGIN, MWT, XMIN, XMAX, YMIN,
     :                     YMAX, SIG, YLOG, XMEAN, YMEAN, XSIG, YSIG, A,
     :                     B, SIGA, SIGB, CHI2, Q, R, PROB, STATUS )
*+
*  Name:
*     PON_STAT

*  Purpose:
*     Perform a weighted least squares fit to a straight line.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PON_STAT( X, Y, NDATA, SIGIN, MWT, XMIN, XMAX, YMIN, YMAX,
*    :               SIG, YLOG, XMEAN, YMEAN, XSIG, YSIG, A, B, SIGA,
*    :               SIGB, CHI2, Q, R, PROB, STATUS )

*  Description:
*     Perform a weighted (if possible) least squares fit to a straight
*     line. The routine uses the algorithm presented by Bevington, P.R.,
*     1969, "Data Reduction and Error Analysis for the Physical Sciences",
*     McGraw-Hill. It has been adapted to return the Pearson correlation
*     coefficient as well.  The arguments Q and PROB are currently
*     returned as 0.

*  Arguments:
*     X( NDATA ) = DOUBLE PRECISION (Given)
*        X-axis data.
*     Y( NDATA ) = DOUBLE PRECISION (Given)
*        Y-axis data.
*     NDATA = INTEGER (Given)
*        Number of data.
*     SIGIN( NDATA ) = REAL (Given)
*        Given Y-axis uncertainties (standard deviation).
*     MWT = INTEGER (Given)
*        Mode: 0 - unweighted fit, 1 - weighted fit.
*     XMIN = REAL (Given)
*        Minimum X value to be used.
*     XMAX = REAL (Given)
*        Maximum X value to be used.
*     YMIN = REAL (Given)
*        Minimum Y value to be used.
*     YMAX = REAL (Given)
*        Maximum Y value to be used.
*     SIG( NDATA ) = REAL (Returned)
*        Y-axis uncertainties used in the fit (derived).
*     YLOG = LOGICAL (Given)
*        Whether logarithms of the Y-axis data are to be taken.
*     XMEAN = REAL (Returned)
*        Mean of the X-axis data.
*     YMEAN = REAL (Returned)
*        Mean of the Y-axis data.
*     XSIG = REAL (Returned)
*        Standard deviation of the X-axis data.
*     YSIG = REAL (Returned)
*        Standard deviation of the Y-axis data.
*     A = REAL (Returned)
*        Y-axis intercept of the straight line.
*     B = REAL (Returned)
*        Gradient of the straight line.
*     SIGA = REAL (Returned)
*        Standard deviation on the Y-axis intercept.
*     SIGB = REAL (Returned)
*        Standard deviation on the gradient.
*     CHI2 = REAL (Returned)
*        Fit chi-squared.
*     Q = REAL (Returned)
*        Incomplete gamma function derived from the fit chi-squared:
*        goodness of fit indicator.
*     R = REAL (Returned)
*        Pearson correlation coefficient.
*     PROB = REAL (Returned)
*        Probability of the Pearson correlation coefficient being due to
*        chance.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-JUN-1992 (JBVAD::PAH):
*        Original version.
*     24-JUN-1992 (PCTR):
*        Code tidy and prologue changes.
*     27-NOV-1992 (PCTR):
*        Rewritten the fit to stop it crashing.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NDATA

      DOUBLE PRECISION X( NDATA )
      DOUBLE PRECISION Y( NDATA )

      REAL SIGIN( NDATA )

      INTEGER MWT

      REAL XMIN
      REAL XMAX
      REAL YMIN
      REAL YMAX

      LOGICAL YLOG

*  Arguments Returned:
      REAL SIG( NDATA )
      REAL XMEAN
      REAL YMEAN
      REAL XSIG
      REAL YSIG
      REAL A
      REAL B
      REAL SIGA
      REAL SIGB
      REAL CHI2
      REAL Q
      REAL R
      REAL PROB

*  Status:
      INTEGER STATUS

*  Local Constants:
      REAL LOG10E                ! Log_10( e )
      PARAMETER ( LOG10E = 0.434294481903 )

*  Local Variables:
      INTEGER I                  ! Loop index
      INTEGER NDGOOD             ! Number of data used

      REAL DELTA                 ! Intermediate denominator
      REAL SIGDAT                ! Standard deviation of the fit
      REAL SS                    ! Sum of weights
      REAL SX                    ! Sum of X data
      REAL SX2                   ! Sum of X**2 data
      REAL SXY                   ! Sum of X*Y data
      REAL SY                    ! Sum of Y data
      REAL SY2                   ! Sum of Y**2 data
      REAL WT                    ! Statistical weight

*.

*  Check inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the accumulators.
      SX = 0.0
      SY = 0.0
      SX2 = 0.0
      SY2 = 0.0
      SXY = 0.0
      NDGOOD = 0

*  Check if the fit is to be weighted.
      IF ( MWT.EQ.0 ) THEN

*  Unweighted fit.
         DO 20 I = 1, NDATA
            IF ( REAL( X( I ) ) .GE. XMIN .AND.
     :           REAL( X( I ) ) .LE. XMAX .AND.
     :           REAL( Y( I ) ) .GE. YMIN .AND.
     :           REAL( Y( I ) ) .LE. YMAX
     :      ) THEN
               SX = SX + REAL( X( I ) )
               SY = SY + REAL( Y( I ) )
               SX2 = SX2 + REAL( X( I )**2 )
               SY2 = SY2 + REAL( Y( I )**2 )
               SXY = SXY + REAL( X( I ) * Y( I ) )
               NDGOOD = NDGOOD + 1
            END IF
 20      CONTINUE

         SS = REAL( NDGOOD )
      ELSE IF ( MWT .EQ. 1 ) THEN

*  Weighted fit.
         SS = 0.0

*  Calculate the summations.
         DO 10 I = 1, NDATA
            IF ( REAL( X( I ) ) .GE. XMIN .AND.
     :           REAL( X( I ) ) .LE. XMAX .AND.
     :           REAL( Y( I ) ) .GE. YMIN .AND.
     :           REAL( Y( I ) ) .LE. YMAX .AND.
     :           SIGIN( I ) .GT. 0.0 ) THEN

*  Check if the Y-axis data have been converted to logarithms and
*  convert their associated errors, if necessary.
               IF ( YLOG ) THEN
                  SIG( I ) = ABS( LOG10E * SIGIN( I ) / REAL( Y( I ) ) )
               ELSE
                  SIG( I ) = SIGIN( I )
               END IF

               WT = 1.0 / ( SIG( I )**2 )
               SS = SS + WT
               SX = SX + REAL( X( I ) ) * WT
               SY = SY + REAL( Y( I ) ) * WT
               SX2 = SX2 + REAL( X( I )**2 ) * WT
               SY2 = SY2 + REAL( Y( I )**2 ) * WT
               SXY = SXY + REAL( X( I ) * Y( I ) ) * WT
               NDGOOD = NDGOOD + 1
            END IF
 10      CONTINUE
      ELSE
         STATUS = SAI__ERROR
         CALL ERR_REP( 'PON_STAT_IVMOD', 'PON_STAT: Unknown ' //
     :        'weighting scheme.', STATUS )
         GO TO 999
      END IF

*  Check that a least squares fit is possible and abort if necessary.
      IF ( NDGOOD .LE. 2 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NDGOOD', NDGOOD )
         CALL ERR_REP( 'PON_STAT_BADND', 'PON_STAT: Too few data ' //
     :        'points for fit (^NDGOOD).', STATUS )
         GO TO 999
      END IF

*  Calculate the fit coefficients.
      DELTA = SX2 * SS - SX**2
      A = ( SX2 * SY - SX * SXY ) / DELTA
      B = ( SS * SXY - SX * SY ) / DELTA

*  Calculate the errors on the fit coefficients.
      SIGA = SQRT( SX2 / DELTA )
      SIGB = SQRT( SS / DELTA )
      XMEAN = SX / SS
      YMEAN = SY / SS
      XSIG = SQRT( ( SX2 - SX**2/SS ) * REAL( NDGOOD )
     :             / ( SS * REAL( NDGOOD - 1 ) ) )
      YSIG = SQRT( ( SY2 - SY**2/SS ) * REAL( NDGOOD )
     :             / ( SS * REAL( NDGOOD - 1 ) ) )

*  Calculate chi-squared.
      CHI2 = 0.0

      IF ( MWT .EQ. 0 ) THEN
         DO 50 I = 1, NDATA
            IF ( REAL( X( I ) ) .GE. XMIN .AND.
     :           REAL( X( I ) ) .LE. XMAX .AND.
     :           REAL( Y( I ) ) .GE. YMIN .AND.
     :           REAL( Y( I ) ) .LE. YMAX ) THEN
               CHI2 = CHI2 +
     :                ( REAL( Y( I ) ) - A - B * REAL( X( I ) ) )**2
            END IF
 50      CONTINUE

         SIGDAT = SQRT( CHI2 / REAL( NDGOOD - 2 ) )
         SIGA = SIGA * SIGDAT
         SIGB = SIGB * SIGDAT
      ELSE
         DO 60 I = 1, NDATA
            IF ( REAL( X( I ) ) .GE. XMIN .AND.
     :           REAL( X( I ) ) .LE. XMAX .AND.
     :           REAL( Y( I ) ) .GE. YMIN .AND.
     :           REAL( Y( I ) ) .LE. YMAX .AND.
     :           SIG( I ) .NE. 0.0 ) THEN
               CHI2 = CHI2 +
     :    ( ( REAL( Y( I ) ) - A - B * REAL( X( I ) ) ) / SIG( I ) )**2
            END IF
 60      CONTINUE
      END IF

*  Calculate the product moment correlation coefficient.
      R = ( SXY - SX*SY/SS ) /
     :     SQRT( ( SX2 - SX**2/SS ) * ( SY2 - SY**2/SS ) )

*  Return two dummy results for future use.
      Q = 0.0
      PROB = 0.0

*  Abort.
 999  CONTINUE

      END
* $Id$
