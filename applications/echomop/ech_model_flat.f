      SUBROUTINE ECH_MODEL_FLAT(
     :           IMAGE,
     :           QUALITY,
     :           ERRORS,
     :           NO_FLAT,
     :           NO_ERRORS,
     :           NX,
     :           NY,
     :           ORDER_NUMBER,
     :           MAX_SKY_PIXELS,
     :           FF_INTERACT,
     :           SUBSAMPLE,
     :           PREBALANCED,
     :           DEK_BELOW,
     :           DEK_ABOVE,
     :           FF_NPOLYSX,
     :           FF_NPOLYSY,
     :           NREJSX,
     :           NREJSY,
     :           THRESHS,
     :           FFLMED,
     :           FFLSMP,
     :           MAXIMUM_POLY,
     :           FITTER,
     :           TRACE_POLYNOMIAL,
     :           FLAT_MODEL,
     :           FLAT_MODEL_ERR,
     :           X_TRACE_COORD,
     :           Y_TRACE_COORD,
     :           XDATA,
     :           YDATA,
     :           YSIGMA,
     :           XFIT,
     :           YFIT,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_MODEL_FLAT

*  Purpose:
*     Calculate pixel-to-pixel balance factors from flat field.

*  Description:
*     (Adapted from routine MAKEBAL in PAMELA)
*
*     Creates a balance-factor frame from flat-field frame.  This is
*     done by fitting polynomials to the one-D profiles of the frame
*     in both X and Y and then dividing these by the frame itself.
*     Negative or zero pixels and those outside the region specified
*     are set to one.  The result when MULTIPLIED into a data frame
*     corrects for short-scale sensitivity variations.
*     Large-scale variation can be removed using either a flux-calibration
*     frame, or if unavailable, the ECH_FIT_ORDER_BLAZE routine will model
*     the large-scale variations.
*
*     This version adds the calculation of variances on the balance factors
*     and also allows the user to supply a frame of pre-calculated balance
*     factors as the input frame (prebalanced=YES).
*
*     It also allows separate activation of both types of polynomial
*     fits, interactive fitting, and the calculation of variances.

*  Invocation:
*     CALL ECH_MODEL_FLAT(
*     :    IMAGE,
*     :    QUALITY,
*     :    ERRORS,
*     :    NO_FLAT,
*     :    NO_ERRORS,
*     :    NX,
*     :    NY,
*     :    MAX_SKY_PIXELS,
*     :    FF_INTERACT,
*     :    SUBSAMPLE,
*     :    PREBALANCED,
*     :    DEK_BELOW,
*     :    DEK_ABOVE,
*     :    FF_NPOLYSX,
*     :    FF_NPOLYSY,
*     :    NREJSX,
*     :    NREJSY,
*     :    THRESHS,
*     :    FFLMED,
*     :    FFLSMP,
*     :    MAXIMUM_POLY,
*     :    FITTER,
*     :    TRACE_POLYNOMIAL,
*     :    FLAT_MODEL,
*     :    FLAT_MODEL_ERR,
*     :    X_TRACE_COORD,
*     :    Y_TRACE_COORD,
*     :    XDATA,
*     :    YDATA,
*     :    YSIGMA,
*     :    XFIT,
*     :    YFIT,
*     :    STATUS
*     :   )

*  Arguments:
*     IMAGE = REAL (Given)
*        Input frame image of dimensions nx columns and ny rows.
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     QUALITY = BYTE (Given)
*        Input frame quality image.
*     ERRORS = REAL (Given)
*        Input frame error image.
*     ORDER_NUMBER = INTEGER (Given)
*        Number of the order being processed.
*     MAX_SKY_PIXELS = INTEGER (Given)
*        Maximum number of spatial pixels.
*     FF_INTERACT = LOGICAL (Given)
*        TRUE if interactive fitting is wanted.
*     PREBALANCED = INTEGER (Given)
*        TRUE if frame already contains balance factors.
*     DEK_BELOW = INTEGER (Given)
*        Lower dekker offset in pixels.
*     DEK_ABOVE = INTEGER (Given)
*        Upper dekker offset in pixels.
*     FF_NPOLYSX = INTEGER (Given)
*        Number of polynomial coeffs in x direction.
*     FF_NPOLYSY = INTEGER (Given)
*        Number of polynomial coeffs in y direction.
*     NREJSX = INTEGER (Given)
*        Number of reject cycles for x fit.
*     NREJSY = INTEGER (Given)
*        Number of reject cycles for y fit.
*     THRESHS = REAL (Given)
*        Threshold for pixel rejection.
*     FFLMED = LOGICAL (Given)
*        TRUE if local median to be used.
*     FFLSMP = INTEGER (Given)
*        Number of samples for local mean/median.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum number of poly coeffs allowed.
*     TRACE_POLYNOMIAL = DOUBLE (Given)
*        Polynomial coeffs describing order trace.
*     FLAT_MODEL = REAL (Returned)
*        Modelled balance factors.
*     FLAT_MODEL_ERR = REAL (Returned)
*        Variances on  Modelled balance factors.
*     X_TRACE_COORD = DOUBLE PRECISION (Temporary Workspace)
*        X coordinates of order trace.
*     Y_TRACE_COORD = DOUBLE PRECISION (Temporary Workspace)
*        Y coordinates of order trace.
*     NO_FLAT = LOGICAL (Given)
*        Set TRUE if no flat field frame available.
*     NO_ERRORS = LOGICAL (Given)
*        TRUE if no error array available.
*     SUBSAMPLE = LOGICAL (Given)
*        Set TRUE if subsampling enabled for x fits.
*     FITTER = CHAR (Given)
*        Type of wavelength fitting function in use (POLY/SPLINE).
*     XDATA = REAL (Given and Returned)
*        X data for fit.
*     YDATA = REAL (Given and Returned)
*        Y data for fit.
*     YSIGMA = REAL (Given and Returned)
*        Deviations on fit.
*     XFIT = REAL (Given and Returned)
*        X values for fit.
*     YFIT = REAL (Given and Returned)
*        Fitted values.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     If order trace polynomial is active (order in use) then
*        Initialise flat field balance model to unity everywhere
*        Calculate order trace path across frame
*        If input frame already contains balance factors then
*           Copy balance factors into model arrays
*        Else if both polynomial num-coeffs are zero, or reject threshold is 0 thjen
*           Leave all balance factors set to unity
*        Else
*           If y polynomial fitting is enabled then
*              Loop through profile subsampling at 10 samples per pixel
*                 Sum nearest (good) pixels along order
*              End loop
*              Loop while re-fit required
*                 If not interactive clear re-fit flag now
*                 Fit the X polynomial to subsampled profile
*                 If interactive fitting then
*                    Plot fit and original data
*                    Get user input option
*                 Endif
*              End loop
*              Calculate xfit balance factors (from spatial profile fit)
*           Endif
*           If x polynomial fitting is enabled (degree>0) then
*              Loop Loading arrays for polynomial fit to spectral dependence
*              of flat field at offset 'ioff' from trace center
*                 Loop through x coords along order trace path at offset 'ioff'
*                    If pixel ok then add to sum
*                 End loop
*                 Loop while re-fit flag set
*                    If not interactive, then clear re-fit flag now
*                    If polynomial degree > 1 then
*                       Fit the wavelength direction data with a polynomial
*                    Else if poly degree is 1 then
*                       Model the flat field using local averages (NOT global average)
*                    Endif
*                    If interactive fitting is required then
*                       Plot data and fit with appropriate title
*                       Get user option input
*                    Endif
*                 End loop
*                 Calculate yfit balance factors
*                 Loop through x pixel coordinates
*                    If pixel within frame boundaries then
*                       If good pixel then
*                          Calculate balance factor from fits
*                          If flat field error was supplied then use it
*                          (scaled appropriately) else estimate variance
*                       Else set negative balance factor
*                       Endif
*                    Endif
*                 End loop
*                 Final check sets any zeros or negative values in balance factors to 1.
*              End loop
*              Report any non-positive pixels
*              Report any hugely deviant pixels
*           Endif
*        Endif
*     Else that warn order is disabled
*     Endif

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     12-JUN-1996 (MJC):
*       Handling for zeroes in YDATA array in the local mean or median
*       case.  Corrected the start point for local medians.
*     06-JUL-1996 (MJC):
*       Added parameter for the current order number.
*       Modified/fixed subsampling problems.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}


*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_ENVIRONMENT.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      REAL IMAGE( NX, NY )
      REAL THRESHS
      INTEGER ORDER_NUMBER
      INTEGER DEK_BELOW
      INTEGER DEK_ABOVE
      INTEGER MAX_SKY_PIXELS
      INTEGER MAXIMUM_POLY
      DOUBLE PRECISION TRACE_POLYNOMIAL( MAXIMUM_POLY )
      LOGICAL FF_INTERACT
      LOGICAL NO_FLAT
      LOGICAL NO_ERRORS
      LOGICAL FFLMED
      INTEGER FFLSMP
      CHARACTER*( * ) FITTER
      BYTE QUALITY( NX, NY )
      LOGICAL PREBALANCED
      REAL FLAT_MODEL( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
      REAL FLAT_MODEL_ERR( NX, -MAX_SKY_PIXELS / 2: MAX_SKY_PIXELS / 2 )
      INTEGER FF_NPOLYSX
      INTEGER FF_NPOLYSY
      INTEGER NREJSX
      INTEGER NREJSY
      LOGICAL SUBSAMPLE

*  Arguments Returned:
      REAL ERRORS( NX, NY )

*  Workspace:
      REAL XDATA( NX )
      REAL YDATA( NX )
      REAL YSIGMA( NX )
      REAL YFIT( NX )
      REAL XFIT( NX )
      DOUBLE PRECISION X_TRACE_COORD( NX )
      DOUBLE PRECISION Y_TRACE_COORD( NX )

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER MAX_POINTS
      PARAMETER ( MAX_POINTS = 8192 )

*  Local Variables:
      DOUBLE PRECISION TEMP_COEFFS( MAX_FIT_COEFFS )

      REAL COLFIT( MAX_POINTS )
      REAL XSUM( MAX_POINTS )
      REAL ESTIMATE( 4 )
      REAL RINC
      REAL SS
      REAL GX
      REAL RESULT
      REAL RSAMP
      REAL THRHI
      REAL SUM
      REAL MINEST
      REAL LSUM
      REAL MSUM
      REAL RSUM
      REAL NEAREST
      REAL FRACTION
      REAL XOFF
      REAL YFRAC

      INTEGER I
      INTEGER IY
      INTEGER IX
      INTEGER IOFF
      INTEGER IOFFST
      INTEGER IOFFEN
      INTEGER IIOFF
      INTEGER IMIN
      INTEGER OPTIONS
      INTEGER IWAVE
      INTEGER ISAMP
      INTEGER NSUM
      INTEGER YCOORD
      INTEGER FRAC
      INTEGER NXFIT
      INTEGER NYFIT
      INTEGER NZERO
      INTEGER NHUGE
      INTEGER IFAIL
      INTEGER IXFIT
      INTEGER IYFIT
      INTEGER IP
      INTEGER K
      INTEGER IWD
      INTEGER ICENT
      INTEGER USAMP
      INTEGER NCHAR1
      INTEGER NCHAR2
      INTEGER NCHAR3

      LOGICAL GOT_QUALITY
      LOGICAL GOT_BALANCE
      LOGICAL MENU
      LOGICAL REFIT

      CHARACTER*80 TITLE
      CHARACTER*32 REF_STR1
      CHARACTER*32 REF_STR2
      CHARACTER*32 REF_STR3
      CHARACTER*6 FORMAT

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
      INTEGER CHR_LEN
      INTEGER ECH_WORD_LEN
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Do nothing if order is disabled.
      IF  ( TRACE_POLYNOMIAL( 1 ) .EQ. ECH__BAD_DOUBLE ) THEN
         CALL ECH_REPORT( 0, ' Order is disabled: no modelling.' )
         GO TO 999
      END IF

*  Initialise.
      GOT_BALANCE = .NOT. NO_FLAT
      GOT_QUALITY = .TRUE.

*  Initialise flat-field balance model to unity everywhere.
      DO IOFF = -MAX_SKY_PIXELS / 2,  MAX_SKY_PIXELS / 2
         DO IWAVE = NX, 1, -1
            FLAT_MODEL( IWAVE, IOFF ) = 1.0
         END DO
      END DO

*  Initialise errors to zero.
      CALL ECH_ZERO_REAL( NX * MAX_SKY_PIXELS,
     :     FLAT_MODEL_ERR( 1, -MAX_SKY_PIXELS / 2 ) )

*  Calculate order trace path across frame.
      CALL ECH_CALC_TRACE( NX, MAXIMUM_POLY, TRACE_POLYNOMIAL,
     :     X_TRACE_COORD, Y_TRACE_COORD, STATUS )

*  Pre-supplied balance-factor frame:
*  =================================

*  If input frame already contains balance factors then
*  copy balance factors into model arrays.
      IF ( PREBALANCED ) THEN
         DO IWAVE = NX, 1, -1
            YCOORD = INT( Y_TRACE_COORD( IWAVE ) + 0.5 ) + DEK_BELOW
            IOFFST = DEK_BELOW
            IOFFEN = DEK_ABOVE
            IF ( YCOORD .LT. 1 ) THEN
               IOFFST = IOFFST - YCOORD + 1
               YCOORD = 1
            END IF
            IF ( YCOORD + IOFFEN - IOFFST .GT. NY ) THEN
               IOFFEN = IOFFST + NY - YCOORD
            END IF
            DO IOFF = IOFFST, IOFFEN
               IF ( GOT_QUALITY ) THEN
                  IF ( QUALITY( IWAVE, YCOORD ) .NE. 0 )
     :               GO TO 10
               END IF
               IF ( IMAGE( IWAVE, YCOORD ) .EQ. ECH__BAD_REAL )
     :            GO TO 10
               FLAT_MODEL( IWAVE, IOFF ) = IMAGE( IWAVE, YCOORD )
               IF ( .NOT. NO_ERRORS ) THEN
                  FLAT_MODEL_ERR( IWAVE, IOFF ) =
     :                  SQRT( ABS( ERRORS( IWAVE, YCOORD ) ) )
               END IF
  10           YCOORD = YCOORD + 1
            END DO
         END DO
         CALL ECH_REPORT( 0, ' Input balance factors copied.' )
         GO TO 998
      END IF

*  Local smooth:
*  ============

*  Calculate balance factors by local smooth.
      IF ( FITTER .EQ. 'SMOOTH' ) THEN
         SS = THRESHS * THRESHS * 2
         IF ( SS .EQ. 0.0 ) SS = 1.0
         IWD = ( FFLSMP - 1 ) / 2
         ICENT = IWD + 1
         DO IX = 1, IWD
            GX = EXP( -FLOAT( IX * IX ) / SS )
            XDATA( ICENT - IX ) = GX
            XDATA( ICENT + IX ) = GX
         END DO
         XDATA( ICENT ) = 1
         DO IWAVE = 1, NX
            YCOORD = INT( Y_TRACE_COORD( IWAVE ) + 0.5 ) + DEK_BELOW
            IOFFST = DEK_BELOW
            IOFFEN = DEK_ABOVE
            IF ( YCOORD .LT. 1 ) THEN
               IOFFST = IOFFST - YCOORD + 1
               YCOORD = 1
            END IF
            IF ( YCOORD + IOFFEN - IOFFST .GT. NY ) THEN
               IOFFEN = IOFFST + NY - YCOORD
            END IF
            DO IOFF = IOFFST, IOFFEN
               RESULT = 0.0
               SUM = 0.0
               K = IWAVE - IWD
               DO IX = 1, FFLSMP
                  IF ( K .GE. 1 .AND. K .LE. NX .AND.
     :                 IMAGE( K, YCOORD ) .GT. 0.0 ) THEN
                     RESULT = RESULT + XDATA( IX ) * IMAGE( K, YCOORD )
                     SUM = SUM + XDATA( IX )
                  END IF
                  K = K + 1
               END DO
               IF ( SUM .GT. 0.0 ) RESULT = RESULT / SUM
               IF ( IMAGE( IWAVE, YCOORD ) .GT. 0.0 ) THEN
                  FLAT_MODEL( IWAVE, IOFF ) = RESULT /
     :                  IMAGE( IWAVE, YCOORD )
                  FLAT_MODEL_ERR( IWAVE, IOFF ) = SQRT(
     :                  ABS( IMAGE( IWAVE, YCOORD ) ) ) /
     :                  IMAGE( IWAVE, YCOORD )
               END IF
               FLAT_MODEL_ERR( IWAVE, IOFF ) =
     :               FLAT_MODEL_ERR( IWAVE, IOFF ) *
     :               FLAT_MODEL_ERR( IWAVE, IOFF )
               YCOORD = YCOORD + 1
            END DO
         END DO
         CALL ECH_REPORT( 0,
     :       ' Balance factors calculated from local smoothing.' )
         GO TO 998
      END IF

*  Local slope:
*  ===========

*  Calculate balance factors from minimum local slope.
      IF ( FITTER .EQ. 'SLOPE' ) THEN
         DO IWAVE = 2, NX - 1
            YCOORD = INT( Y_TRACE_COORD( IWAVE ) + 0.5 ) + DEK_BELOW
            IOFFST = DEK_BELOW
            IOFFEN = DEK_ABOVE
            IF ( YCOORD .LT. 2 ) THEN
               IOFFST = IOFFST - YCOORD + 2
               YCOORD = 2
            END IF
            IF ( YCOORD + IOFFEN - IOFFST .GE. NY ) THEN
               IOFFEN = IOFFST + NY - YCOORD - 1
            END IF
            DO IOFF = IOFFST, IOFFEN
               IF ( IMAGE( IWAVE, YCOORD ) .GT. 0.0 ) THEN
                  IMIN = 0
                  IF ( IMAGE( IWAVE - 1, YCOORD ) .GT. 0.0 .AND.
     :                 IMAGE( IWAVE + 1, YCOORD ) .GT. 0.0 ) THEN
                     ESTIMATE( 1 ) = ( IMAGE( IWAVE - 1, YCOORD ) +
     :                     IMAGE( IWAVE + 1, YCOORD ) ) / 2.0
                     IMIN = 1

                  ELSE
                     ESTIMATE( 1 ) = 0.0
                  END IF

                  IF ( IMAGE( IWAVE - 1, YCOORD - 1 ) .GT. 0.0 .AND.
     :                 IMAGE( IWAVE + 1, YCOORD + 1 ) .GT. 0.0 ) THEN
                     ESTIMATE( 2 ) = ( IMAGE( IWAVE-1, YCOORD - 1 ) +
     :                     IMAGE( IWAVE + 1, YCOORD + 1 ) ) / 2.0
                     IMIN = 2

                  ELSE
                     ESTIMATE( 2 ) = 0.0
                  END IF

                  IF ( IMAGE( IWAVE, YCOORD - 1 ) .GT. 0.0 .AND.
     :                 IMAGE( IWAVE, YCOORD + 1 ) .GT. 0.0 ) THEN
                     ESTIMATE( 3 ) = ( IMAGE( IWAVE, YCOORD - 1 ) +
     :                     IMAGE( IWAVE, YCOORD + 1 ) ) / 2.0
                     IMIN = 3

                  ELSE
                     ESTIMATE( 3 ) = 0.0
                  END IF

                  IF ( IMAGE( IWAVE - 1, YCOORD + 1 ) .GT. 0.0 .AND.
     :                 IMAGE( IWAVE + 1, YCOORD - 1 ) .GT. 0.0 ) THEN
                     ESTIMATE( 4 ) = ( IMAGE( IWAVE-1, YCOORD + 1 ) +
     :                      IMAGE( IWAVE + 1, YCOORD - 1 ) ) / 2.0
                     IMIN = 4

                  ELSE
                     ESTIMATE( 4 ) = 0.0
                  END IF

*              If a slope is available...
                  IF ( IMIN .NE. 0 ) THEN

*                 Guess minimum slope value.
                     MINEST = ABS( IMAGE( IWAVE, YCOORD ) -
     :                     ESTIMATE( IMIN ) )

*                 Find minimum slope value.
                     DO I = 1, 4
                        IF ( ESTIMATE( I ) .NE. 0.0 .AND.
     :                       ABS( IMAGE( IWAVE, YCOORD ) -
     :                       ESTIMATE( I ) ) .LT. MINEST ) THEN
                           IMIN = I
                           MINEST = ABS( IMAGE( IWAVE, YCOORD ) -
     :                           ESTIMATE( I ) )
                        END IF
                     END DO

*                 Use smallest slope to find balance factor.
                     FLAT_MODEL( IWAVE, IOFF ) = ESTIMATE( IMIN ) /
     :                     IMAGE( IWAVE, YCOORD )

*                 Estimate variance.
                     FLAT_MODEL_ERR( IWAVE, IOFF ) = SQRT(
     :                     IMAGE( IWAVE, YCOORD ) ) /
     :                     IMAGE( IWAVE, YCOORD )
                     FLAT_MODEL_ERR( IWAVE, IOFF ) =
     :                     FLAT_MODEL_ERR( IWAVE, IOFF ) *
     :                     FLAT_MODEL_ERR( IWAVE, IOFF )
                  END IF
               END IF
               YCOORD = YCOORD + 1
            END DO
         END DO
         CALL ECH_REPORT( 0,
     :        ' Balance factors calculated from local slopes.' )
         GO TO 998
      END IF

*  Curve fit:
*  =========

*  If both polynomial num-coeffs are zero, or reject threshold is 0 then
*  leave all balance factors set to unity.
      IF ( .NOT. GOT_BALANCE .OR.
     :          ( FF_NPOLYSX .EQ. 0 .AND. FF_NPOLYSY .EQ. 0 ) .OR.
     :          THRESHS .LE. 0.0 ) THEN
         CALL ECH_REPORT( 0, ' Balance factors all set to unity.' )
         GO TO 999
      END IF

*  Curve fit in Y- and/or X-, initialise.
      IF ( FITTER .EQ. 'MEAN' ) THEN
         FF_NPOLYSX = 1
         FF_NPOLYSY = 0
         FFLMED = .FALSE.

      ELSE IF ( FITTER .EQ. 'MEDIAN' ) THEN
         FF_NPOLYSX = 1
         FF_NPOLYSY = 0
         FFLMED = .TRUE.
      END IF

      THRHI = THRESHS
      DO I = NX, 1, -1
         XFIT( I ) = 1.0
         YFIT( I ) = 1.0
         COLFIT( I ) = 1.0
      END DO

*  Find total flux in each X-increment.
      CALL ECH_ZERO_REAL( NX, XSUM )
      DO IWAVE = 1, NX
         YFRAC = Y_TRACE_COORD( IWAVE ) - FLOAT( INT(
     :         Y_TRACE_COORD( IWAVE ) + 0.5 ) )
         YCOORD = INT( Y_TRACE_COORD( IWAVE ) + 0.5 ) + DEK_BELOW
         IF ( YFRAC .GT. 0.0 ) THEN
            IOFFST = DEK_BELOW
            IOFFEN = DEK_ABOVE - 1

         ELSE
            IOFFST = DEK_BELOW + 1
            IOFFEN = DEK_ABOVE
            YCOORD = YCOORD + 1
         END IF
         IF ( YCOORD .LT. 1 ) THEN
            IOFFST = IOFFST - YCOORD + 1
            YCOORD = 1
         END IF
         IF ( YCOORD + IOFFEN - IOFFST .GT. NY ) THEN
            IOFFEN = IOFFST + NY - YCOORD
         END IF
         DO IOFF = IOFFST, IOFFEN
            IF ( GOT_QUALITY ) THEN
               IF ( QUALITY( IWAVE, YCOORD ) .NE. 0 )
     :            GO TO 20
            END IF
            IF ( IMAGE( IWAVE, YCOORD ) .EQ. ECH__BAD_REAL )
     :         GO TO 20
            FRACTION = 1.0
            IF ( YFRAC .GT. 0.0 ) THEN
               IF ( IOFF .EQ. DEK_BELOW ) THEN
                  FRACTION = YFRAC

               ELSE IF ( IOFF .EQ. DEK_ABOVE - 1 ) THEN
                  FRACTION = 1.0 - YFRAC
               END IF

            ELSE
               IF ( IOFF .EQ. DEK_ABOVE ) THEN
                  FRACTION = -YFRAC

               ELSE IF ( IOFF .EQ. DEK_BELOW + 1 ) THEN
                  FRACTION = 1.0 + YFRAC
               END IF
            END IF
            XSUM( IWAVE ) = XSUM( IWAVE ) +
     :            FRACTION * IMAGE( IWAVE, YCOORD )
   20       YCOORD = YCOORD + 1
         END DO
      END DO

*  If Y-polynomial fitting is enabled.
      IF ( FF_NPOLYSY .GT. 0 ) THEN
         CALL CHR_ITOC( FF_NPOLYSY, REF_STR1, NCHAR1 )
         REPORT_STRING = ' Fitting (coeffs=' //
     :         REF_STR1( :NCHAR1 ) // ') ' //
     :         FITTER( :ECH_WORD_LEN( FITTER ) ) //
     :         ' to flat-field spatial dependence.'
         CALL ECH_REPORT( 0, REPORT_STRING )
         IF ( .NOT. SUBSAMPLE .AND. ORDER_NUMBER .EQ. 1 ) THEN
            REPORT_STRING = ' (this usage is appropriate when the' //
     :            ' dekker is filled by bright pixels,'
            CALL ECH_REPORT( 0, REPORT_STRING )
            REPORT_STRING = ' if the dekker extends beyond the bright'//
     :       ' section then unwanted edge effects'
            CALL ECH_REPORT( 0, REPORT_STRING )
            REPORT_STRING = '  may cause problems.' //
     :            '  Use X-fit and TUNE_FFSUBSMP in this case.)'
            CALL ECH_REPORT( 0, REPORT_STRING )
         END IF

*     Generate a normalised version of the per-column fluxes
*     using a fifth-order polynomial curve.
         DO I = NX, 1, -1
            YSIGMA( I ) = 1.0
            XDATA( I ) = FLOAT( I )
         END DO
         CALL ECH_FITTER( 'REAL-POLY', 5, TEMP_COEFFS, NX, XDATA,
     :        XSUM, YSIGMA, NREJSY, THRHI, STATUS )
         CALL ECH_FEVAL( 'POLY', 5, TEMP_COEFFS, NX, XDATA, XFIT,
     :        STATUS )
         DO I = NX, 1, -1
            IF ( XSUM( I ) .EQ. 0.0 ) THEN
               XSUM( I ) = 1.0
            END IF
            COLFIT( I ) = XFIT( I ) / XSUM( I )
         END DO

*     Loop through profile subsampling at 10 samples per pixel.
         NXFIT = 0
         XOFF = REAL( DEK_BELOW )
         DO IOFF = 10 * DEK_BELOW, 10 * DEK_ABOVE
            NSUM = 0
            SUM = 0.0

*        Sum good pixels along order.
            DO IWAVE = 1, NX
               YCOORD = INT( Y_TRACE_COORD( IWAVE ) + XOFF + 0.5 )

*           If Y-coordinate is within image area.
               IF ( YCOORD .GT. 0 .AND. YCOORD .LE. NY ) THEN

*              Check Quality if present.
                  IF ( GOT_QUALITY ) THEN
                     IF ( QUALITY( IWAVE, YCOORD ) .NE. 0 )
     :                  GO TO 30
                  END IF

*              Check for bad data.
                  IF ( IMAGE( IWAVE, YCOORD ) .EQ. ECH__BAD_REAL )
     :               GO TO 30

*              Add to pixel count and sum.
                  NSUM = NSUM + 1
                  SUM = SUM + IMAGE( IWAVE, YCOORD ) / XSUM( IWAVE )
  30           END IF
            END DO
            NXFIT = NXFIT + 1
            XDATA( NXFIT ) = XOFF
            XOFF = XOFF + 0.1
            YDATA( NXFIT ) = SUM / REAL( MAX( 1, NSUM ) )
         END DO

*     Loop while re-fit required.
         REFIT = .TRUE.
         MENU = .TRUE.
         DO WHILE ( REFIT )

*        If not interactive clear re-fit flag now.
            IF ( .NOT. FF_INTERACT ) REFIT = .FALSE.

*        Fit a polynomial to subsampled spatial profile.
            DO I = NXFIT, 1, -1
               YSIGMA( I ) = 1.0
            END DO
            REF_STR1 = 'REAL-' // FITTER
            CALL ECH_FITTER( REF_STR1, FF_NPOLYSY,
     :           TEMP_COEFFS, NXFIT, XDATA, YDATA, YSIGMA,
     :           NREJSY, THRHI, STATUS )
            IFAIL = 0
            CALL ECH_FEVAL( FITTER, FF_NPOLYSY, TEMP_COEFFS,
     :           NXFIT, XDATA, XFIT, IFAIL )
            IF ( IFAIL .NE. 0 ) THEN
               GO TO 999
            END IF

*        If interactive fitting then.
            IF ( FF_INTERACT ) THEN

*           Plot fit and original data.
               CALL CHR_ITOC( ORDER_NUMBER, REF_STR1, NCHAR1 )
               CALL CHR_ITOC( FF_NPOLYSY, REF_STR2, NCHAR2 )
               TITLE = ' Order ' // REF_STR1( :NCHAR1 ) //
     :                 ', fitting (' //
     :                 FITTER( :ECH_WORD_LEN( FITTER ) ) // ':' //
     :                 REF_STR2( :NCHAR2 ) // ') to flat-field' //
     :                 ' spatial profile'
               OPTIONS = GRPH_CALC_MINMAX
               CALL ECH_PLOT_GRAPH( NXFIT, XDATA, YDATA,
     :              0., 0., 0., 0., 'Spatial offset across profile',
     :              'Average profile', TITLE, 0.0, 1.1, OPTIONS,
     :              'POINTS', STATUS )

               OPTIONS = GRPH_OVERLAY + GRPH_SET_COLOUR +
     :                   GRPH_SET_LINE_STYLE
               CALL ECH_PLOT_GRAPH( NXFIT, XDATA, XFIT, 0., 0.,
     :              0., 0., 'RED', 'DASH', ' ', 0., 0., OPTIONS,
     :              'LINES', STATUS )

*           Display menu.
   50          CONTINUE
               IF ( MENU ) THEN
                  CALL ECH_REPORT( 0, ' Options:' )
                  CALL ECH_REPORT( 0,
     :                 '   A - Continue non-interactively.' )
                  CALL ECH_REPORT( 0,
     :                 '   F - Change fitting function type.' )
                  CALL ECH_REPORT( 0,
     :                 '   Q - Quit this order.' )
                  CALL ECH_REPORT( 0,
     :                 '   E - Exit when fit is OK.' )
                  CALL ECH_REPORT( 0,
     :                 '   - - Decrement degree of fit used.' )
                  CALL ECH_REPORT( 0,
     :                 '   + - Increment degree of fit used.' )
                  CALL ECH_REPORT( 0,
     :                 '   M - Full menu display.' )
                  CALL ECH_REPORT( 0, ' ' )
                  MENU = .FALSE.

               ELSE
                  CALL ECH_REPORT( 0, ' Options [ A F Q E - + M ]' )
               END IF

*           Get users option.
               CALL ECH_READ_GRPH_CURSOR( STATUS )

*           Option 'M' - display full menu.
               IF ( USER_INPUT_CHAR .EQ. 'M' ) THEN
                  MENU = .TRUE.
                  GO TO 50

*           Option 'A' - auto.
               ELSE IF ( USER_INPUT_CHAR .EQ. 'A' ) THEN
                  REFIT = .FALSE.
                  FF_INTERACT = .FALSE.

*           Option 'Q' - quit order.
               ELSE IF ( USER_INPUT_CHAR .EQ. 'Q' ) THEN
                  REPORT_STRING = ' Further fits to this order'//
     :                 ' abandoned at user request.'
                  CALL ECH_REPORT( 0, REPORT_STRING )
                  GO TO 999

*           Option 'E' - exit order.
               ELSE IF ( USER_INPUT_CHAR .EQ. 'E' ) THEN
                  REFIT = .FALSE.

*           Option 'F' - change type of fit.
               ELSE IF ( USER_INPUT_CHAR .EQ. 'F' ) THEN
                 IF ( FITTER .EQ. 'MEDIAN' ) FITTER = 'SPLINE'
                 CALL ECH_FEVAL( FITTER, FF_NPOLYSY, TEMP_COEFFS, NX,
     :                XDATA, 0., ECH__NEXT_FITTER )

*           Option '+' - increment order of fit.
               ELSE IF ( USER_INPUT_CHAR .EQ. '+' ) THEN
                  CALL ECH_FEVAL( FITTER, FF_NPOLYSY, TEMP_COEFFS,
     :                 MAXIMUM_POLY, XDATA, 0., ECH__INC_NUMCOEFF )

*           Option '-' - decrement order of fit.
               ELSE IF ( USER_INPUT_CHAR .EQ. '-' ) THEN
                  CALL ECH_FEVAL( FITTER, FF_NPOLYSY, TEMP_COEFFS,
     :                 MAXIMUM_POLY, XDATA, 0., ECH__DEC_NUMCOEFF )

*           Unknown option.
               ELSE
                  REPORT_STRING = ' Unknown Option: "' //
     :                  USER_INPUT_CHAR // '".'
                  CALL ECH_REPORT( 0, REPORT_STRING )
                  GO TO 50
               END IF
            END IF
         END DO

*     Calculate X-fit balance factors (from spatial profile fit).
         DO IP = NXFIT, 1, -1
            IF ( YDATA( IP ) .GT. 0.0 )
     :         XFIT( IP ) = MIN( MAX( 0.1,
     :                      ABS( XFIT( IP ) / YDATA( IP ) ) ), 10.0 )
         END DO
      END IF

*  If X-polynomial fitting is enabled (degree>0).
      IF ( FF_NPOLYSX .GT. 0 ) THEN
         IF ( FF_NPOLYSX .EQ. 1 .AND. SUBSAMPLE )
     :      SUBSAMPLE = .FALSE.

*     Loop Loading arrays for polynomial fit to spectral dependence
*     of flat field at offset 'ioff' from trace center
         MENU = .TRUE.

         DO IIOFF = DEK_BELOW, DEK_ABOVE
            IOFF = IIOFF

*        Special option 'I' for manually selecting which increment
*        to process comes in here and avoids the DO loop incrementing
*        its counter.
  100       CONTINUE
            IF ( FF_NPOLYSX .GT. 1 ) THEN
               CALL CHR_ITOC( FF_NPOLYSX, REF_STR1, NCHAR1 )
               REPORT_STRING = ' Fitting (coeffs=' //
     :               REF_STR1( :NCHAR1 ) // ') ' //
     :               FITTER( :ECH_WORD_LEN( FITTER ) ) //
     :               ' to flat field at '

            ELSE IF ( FFLMED ) THEN
               CALL CHR_ITOC( FFLSMP, REF_STR1, NCHAR1 )
               REPORT_STRING = ' Using ' //
     :               REF_STR1( :NCHAR1 ) // '-pixel local medians at '

            ELSE
               REPORT_STRING = ' Using mean local flat-field at '
            END IF
            NCHAR1 = CHR_LEN( REPORT_STRING ) + 1
            IF ( IOFF .LT. -1 ) THEN
               CALL CHR_ITOC( -IOFF, REF_STR2, NCHAR2 )
               REPORT_STRING = REPORT_STRING( :NCHAR1 ) //
     :               REF_STR2( :NCHAR2 ) // ' pixels below trace.'

            ELSE IF ( IOFF .EQ. -1 ) THEN
               REPORT_STRING = REPORT_STRING( :NCHAR1 ) //
     :               '1 pixel below trace.'

            ELSE IF ( IOFF .EQ. 0 ) THEN
               REPORT_STRING = REPORT_STRING( :NCHAR1 ) //
     :               'centre of trace.'

            ELSE IF ( IOFF .EQ. 1 ) THEN
               REPORT_STRING = REPORT_STRING( :NCHAR1 ) //
     :               '1 pixel above trace.'

            ELSE
               CALL CHR_ITOC( IOFF, REF_STR2, NCHAR2 )
               REPORT_STRING = REPORT_STRING( :NCHAR1 ) //
     :               REF_STR2( :NCHAR2 ) // ' pixels above trace.'
            END IF
            CALL ECH_REPORT( 0, REPORT_STRING )

*        Loop through X-coords along order trace path at offset 'ioff'.
            NYFIT = 0
            DO IWAVE = 1, NX
               YCOORD = INT( Y_TRACE_COORD( IWAVE ) + 0.5 ) + IOFF

*           If pixel ok then add to sum.
               IF ( YCOORD .GT. 0 .AND. YCOORD .LE. NY ) THEN
                  IF ( GOT_QUALITY ) THEN
                     IF ( QUALITY( IWAVE, YCOORD ) .NE. 0 )
     :                  GO TO 40
                  END IF
                  IF ( IMAGE( IWAVE, YCOORD ) .LE. 0.0 )
     :               GO TO 40
                  NYFIT = NYFIT + 1
                  XDATA( NYFIT ) = REAL( IWAVE )

*              Apply subsample if needed.
                  IF ( SUBSAMPLE ) THEN
                     FRAC = REAL( Y_TRACE_COORD( IWAVE ) ) -
     :                   FLOAT( INT( Y_TRACE_COORD( IWAVE ) + 0.5 ) )
                     IF ( FRAC .GE. 0.0 ) THEN
                        IF ( YCOORD .GE. NY ) GO TO 39
                        IF ( GOT_QUALITY ) THEN
                           IF ( QUALITY( IWAVE, YCOORD + 1 ) .NE. 0 )
     :                        GO TO 39
                        END IF
                        IF ( IMAGE( IWAVE, YCOORD + 1 ) .LE. 0.0 )
     :                     GO TO 39
                        YDATA( NYFIT ) = ( IMAGE( IWAVE, YCOORD )
     :                        * ( 1.0 - FRAC ) +
     :                        IMAGE( IWAVE, YCOORD + 1 )
     :                        * FRAC ) / XSUM( IWAVE )

                     ELSE
                        IF ( YCOORD .LE. 1 ) GO TO 39
                        IF ( GOT_QUALITY ) THEN
                           IF ( QUALITY( IWAVE, YCOORD - 1 ) .NE. 0 )
     :                        GO TO 39
                        END IF
                        IF ( IMAGE( IWAVE, YCOORD ) .LE. 0.0 )
     :                     GO TO 39
                        YDATA( NYFIT ) = ( IMAGE( IWAVE, YCOORD )
     :                        * ( 1.0 + FRAC ) +
     :                        IMAGE( IWAVE, YCOORD - 1 )
     :                        * ( - FRAC ) ) / XSUM( IWAVE )
                     END IF

                  ELSE
   39                YDATA( NYFIT ) = IMAGE( IWAVE, YCOORD ) /
     :                     XSUM( IWAVE )
                  END IF
   40          END IF
            END DO

*        Loop while re-fit flag set.
            REFIT = .TRUE.
            DO WHILE ( REFIT .AND. NYFIT .GT. 0 )

*           If not interactive, then clear re-fit flag now.
               IF ( .NOT. FF_INTERACT ) REFIT = .FALSE.

*           If polynomial degree > 1 then fit the wavelength-direction
*           data with a polynomial.
               IF ( FF_NPOLYSX .GT. 1 ) THEN
                  DO I = NYFIT, 1, -1
                     YSIGMA( I ) = 1.0
                  END DO
                  REF_STR1 = 'REAL-' // FITTER
                  CALL ECH_FITTER( REF_STR1, FF_NPOLYSX,
     :                 TEMP_COEFFS, NYFIT, XDATA, YDATA, YSIGMA,
     :                 NREJSX, THRHI, STATUS )
                  IFAIL = 0
                  CALL ECH_FEVAL( FITTER, FF_NPOLYSX, TEMP_COEFFS,
     :                 NYFIT, XDATA, YFIT, STATUS )
                  IF ( IFAIL .NE. 0 ) THEN
                     GO TO 101
                  END IF

*           If polynomial fit degree is one then model the flat field
*           using local averages (not global average).  Using a global
*           averaged value would not be useful generally so we calculate
*           local averages based upon a pixel's left and right neighbours
*           and a combination of both sides.  The 'best' average is then
*           selected as the one which is closest to the value of the
*           pixel under study.  This method avoids the problems caused
*           at the edges of the order (spatially) by the very rapid
*           change in intensity as the dekker interrupts the light path.
               ELSE IF ( FF_NPOLYSX .EQ. 1 ) THEN
                  IF ( FFLMED ) THEN
                     DO IWAVE = 1, NX
                        ISAMP = MAX( 1, IWAVE - FFLSMP / 2 )
                        ISAMP = MIN( ISAMP, NX - FFLSMP - 1 )
                        CALL ECH_MEAN_MEDIAN( FFLSMP,
     :                       YDATA( ISAMP ), .TRUE., .FALSE.,
     :                       YFIT( IWAVE ), STATUS )

*                    Handle divide-by-zero cases.
                        IF ( YFIT( IWAVE ) .EQ. 0.0 .OR.
     :                       YDATA( IWAVE ) .EQ. 0.0 ) THEN
                           YFIT( IWAVE ) = 1.0

                        ELSE
                           YFIT( IWAVE ) = YFIT( IWAVE ) /
     :                                     YDATA( IWAVE )
                        END IF
                     END DO

                  ELSE
                     LSUM = YDATA( 1 )
                     USAMP = ( MAX( 1, FFLSMP ) / 2 ) * 2 + 1
                     RSAMP = FLOAT( USAMP )
                     RSUM = 0.0
                     MSUM = 0.0
                     DO IWAVE = 2, 2 + USAMP - 1
                        RSUM = RSUM + YDATA( IWAVE )
                        MSUM = MSUM + YDATA( MAX( 1, IWAVE - USAMP /
     :                         2 - 2 ) )
                     END DO
                     RSUM = RSUM / RSAMP
                     MSUM = MSUM / RSAMP
                     DO IWAVE = 1, NX
                        LSUM = ( RSAMP * LSUM -
     :                        YDATA( MAX( 1, IWAVE - USAMP ) ) +
     :                        YDATA( IWAVE ) ) /
     :                        RSAMP
                        RSUM = ( RSAMP * RSUM -
     :                        YDATA( MAX( 1, IWAVE - 1 ) ) +
     :                        YDATA( MIN( NX, IWAVE + USAMP ) ) ) /
     :                        RSAMP
                        MSUM = ( RSAMP * MSUM -
     :                        YDATA( MAX( 1, IWAVE - USAMP / 2 - 1
     :                        ) ) + YDATA( MIN( NX, IWAVE + USAMP / 2
     :                        + 1 ) ) ) / RSAMP
                        IF ( LSUM .EQ. 0.0 ) LSUM = 1.0
                        IF ( MSUM .EQ. 0.0 ) MSUM = 1.0
                        IF ( RSUM .EQ. 0.0 ) RSUM = 1.0
                        IF ( YDATA( IWAVE ) .EQ. 0.0 ) THEN
                           YFIT( IWAVE ) = 1.0

                        ELSE
                           NEAREST = LSUM / YDATA( IWAVE )
                           IF ( ABS( YDATA( IWAVE ) - MSUM ) .LT.
     :                          ABS( YDATA( IWAVE ) - LSUM ) ) THEN
                              NEAREST = MSUM / YDATA( IWAVE )
                              IF ( ABS( YDATA( IWAVE ) - RSUM ) .LT.
     :                             ABS( YDATA( IWAVE ) - MSUM ) )
     :                             THEN
                                 NEAREST = RSUM / YDATA( IWAVE )
                              END IF

                           ELSE IF ( ABS( YDATA( IWAVE ) - RSUM )
     :                               .LT. ABS( YDATA( IWAVE ) - LSUM
     :                               ) ) THEN
                              NEAREST = RSUM / YDATA( IWAVE )
                           END IF
                           YFIT( IWAVE ) = NEAREST
                        END IF
                     END DO
                  END IF
                  NYFIT = NX
               END IF

*              If interactive fitting is required.
               IF ( FF_INTERACT ) THEN

*              Plot data and fit with appropriate title.
                  IF ( FF_NPOLYSX .GT. 1 .OR.
     :                 ( FF_NPOLYSX .EQ. 1 .AND. FFLMED ) ) THEN
                     CALL CHR_ITOC( ORDER_NUMBER, REF_STR1, NCHAR1 )
                     CALL CHR_ITOC( IOFF, REF_STR2, NCHAR2 )
                     IF ( IOFF .GT. 0 ) THEN
                        REF_STR2 = '+' // REF_STR2( :NCHAR2 )
                        NCHAR2 = NCHAR2 + 1
                     END IF
                     IF ( FF_NPOLYSX .EQ. 1 .AND. FFLMED ) THEN
                        TITLE = ' Order ' // REF_STR1( :NCHAR1 ) //
     :                        '(' //
     :                        REF_STR2( :NCHAR2 ) // '):' //
     :                        ' flat-field using local median'

                     ELSE
                        CALL CHR_ITOC( FF_NPOLYSX, REF_STR3, NCHAR3 )
                        TITLE = ' Order ' // REF_STR1( :NCHAR1 ) //
     :                        '(' //
     :                        REF_STR2( :NCHAR2 ) // '): fitting ' //
     :                        FITTER( :ECH_WORD_LEN( FITTER ) ) //
     :                        ':' //
     :                        REF_STR3( :NCHAR3 ) // ' to flat-field'
                     END IF
                     OPTIONS = GRPH_CALC_MINMAX
                     CALL ECH_PLOT_GRAPH( NYFIT, XDATA, YDATA,
     :                    0., 0.0, 0., 0.0, 'X pixel along trace',
     :                    'Fractional intensity', TITLE, 0., 0.,
     :                    OPTIONS, 'POINTS', STATUS )

                     IF ( SUBSAMPLE ) THEN
                        FORMAT = 'LINES'

                     ELSE
                        FORMAT = 'POINTS'
                     END IF
                     OPTIONS = GRPH_OVERLAY +
     :                         GRPH_SET_COLOUR +
     :                         GRPH_SET_LINE_STYLE
                     CALL ECH_PLOT_GRAPH( NYFIT, XDATA, YFIT,
     :                    0., 0., 0., 0., 'RED', 'DASH', ' ',
     :                    0., 0., OPTIONS, FORMAT, STATUS )

                  ELSE
                     CALL CHR_ITOC( ORDER_NUMBER, REF_STR1, NCHAR1 )
                     CALL CHR_ITOC( IOFF, REF_STR2, NCHAR2 )
                     IF ( IOFF .GT. 0 ) THEN
                        REF_STR2 = '+' // REF_STR2( :NCHAR2 )
                        NCHAR2 = NCHAR2 + 1
                     END IF
                     TITLE = ' Order ' // REF_STR1( :NCHAR1 ) //
     :                        '(' //
     :                        REF_STR2( :NCHAR2 ) // '):' //
     :                        ' flat field using local mean'
                     OPTIONS = 0
                     CALL ECH_PLOT_GRAPH( NYFIT, XDATA, YFIT,
     :                    1.0, FLOAT( NX ), 0.5, 1.5,
     :                    'X pixel along trace', 'Balance factor',
     :                    TITLE, 0., 0., OPTIONS, 'BINS', STATUS )
                  END IF

*              Display menu.
  200             CONTINUE
                  IF ( MENU ) THEN
                     CALL ECH_REPORT( 0, ' Options:' )
                     CALL ECH_REPORT( 0,
     :                    '   A - Exit and continue automatically.' )
                     CALL ECH_REPORT( 0,
     :                    '   I - Select increment to fit.' )
                     IF ( FF_NPOLYSX .GT. 1 ) CALL ECH_REPORT( 0,
     :                    '   S - Toggle subsampling.' )
                     CALL ECH_REPORT( 0,
     :                    '   F - Change fitting function type.' )
                     CALL ECH_REPORT( 0,
     :                    '   Q - Quit this order.' )
                     CALL ECH_REPORT( 0,
     :                    '   E - Exit when fit is OK.' )
                     CALL ECH_REPORT( 0,
     :                    '   - - Decrement degree of fit used.' )
                     CALL ECH_REPORT( 0,
     :                    '   + - Increment degree of fit used.' )
                     CALL ECH_REPORT( 0,
     :                    '   M - Full menu display.' )
                  MENU = .FALSE.

                  ELSE
                     CALL ECH_REPORT( 0,
     :                    ' Options [ A I S F Q E - + M ]' )
                  END IF

*              Get option input.
                  CALL ECH_READ_GRPH_CURSOR( STATUS )

*              Option 'M' - display full menu.
                  IF ( USER_INPUT_CHAR .EQ. 'M' ) THEN
                     MENU = .TRUE.
                     GO TO 200

*              Option 'E' - exit.
                  ELSE IF ( USER_INPUT_CHAR .EQ. 'E' ) THEN
                     REFIT = .FALSE.

*              Option 'Q' - quit.
                  ELSE IF ( USER_INPUT_CHAR .EQ. 'Q' ) THEN
                     REPORT_STRING = ' Further fits to thi' //
     :                    's order abandoned at user request.'
                     CALL ECH_REPORT( 0, REPORT_STRING )
                     GO TO 999

*              Option 'A' - auto.
                  ELSE IF ( USER_INPUT_CHAR .EQ. 'A' ) THEN
                     REFIT = .FALSE.
                     FF_INTERACT = .FALSE.

*              Option 'S' - toggle subsampling.
                  ELSE IF ( USER_INPUT_CHAR .EQ. 'S' ) THEN
                     SUBSAMPLE = .NOT. SUBSAMPLE
                     IF ( SUBSAMPLE ) THEN
                        CALL ECH_REPORT( 0,
     :                       ' Subsampling enabled.' )

                     ELSE
                        CALL ECH_REPORT( 0,
     :                       ' Subsampling disabled.' )
                     END IF
                     GO TO 100

*              Option 'I' - select spatial pixel to fit.
                  ELSE IF ( USER_INPUT_CHAR .EQ. 'I' ) THEN
                     REFIT = .FALSE.
                     RINC = FLOAT( IOFF )
                     IOFF = 0
                     DO WHILE ( IOFF .EQ. 0 )
                       CALL ECH_GET_PARAMETER(
     :                     'INSTANT-PROMPT=Increment to fit',
     :                     'INT', RINC, .FALSE., ' ', 0, STATUS )
                       IF ( INT( RINC ) .LT. DEK_BELOW .OR.
     :                      INT( RINC ) .GT. DEK_ABOVE ) THEN
                          CALL CHR_ITOC( DEK_BELOW, REF_STR1, NCHAR1 )
                          CALL CHR_ITOC( DEK_ABOVE, REF_STR2, NCHAR2 )
                          REPORT_STRING = ' Increment must be in' //
     :                          ' dekker range: ' //
     :                          REF_STR1( :NCHAR1 ) // ' to ' //
     :                          REF_STR2( :NCHAR2 ) // '.'
                          CALL ECH_REPORT( 0, REPORT_STRING )
                          RINC = 0.0

                       ELSE
                          IOFF = INT( RINC )
                          GO TO 100
                       END IF
                     END DO

*              Option 'F' - change type of fit.
                  ELSE IF ( USER_INPUT_CHAR .EQ. 'F' ) THEN
                     CALL ECH_FEVAL( FITTER, FF_NPOLYSX,
     :                    TEMP_COEFFS, NX, XDATA, 0.,
     :                    ECH__NEXT_FITTER )

*              Option '+' - increment order of fit.
                  ELSE IF ( USER_INPUT_CHAR .EQ. '+' ) THEN
                     CALL ECH_FEVAL( FITTER, FF_NPOLYSX,
     :                    TEMP_COEFFS, MAXIMUM_POLY,
     :                    XDATA, 0., ECH__INC_NUMCOEFF )

*              Option '-' - decrement order of fit.
                  ELSE IF ( USER_INPUT_CHAR .EQ. '-' ) THEN
                     CALL ECH_FEVAL( FITTER, FF_NPOLYSX,
     :                    TEMP_COEFFS, MAXIMUM_POLY,
     :                    XDATA, 0., ECH__DEC_NUMCOEFF )

*              Unknown option.
                  ELSE
                     REPORT_STRING = ' Unknown Option: "' //
     :                     USER_INPUT_CHAR // '".'
                     CALL ECH_REPORT( 0, REPORT_STRING )
                     GO TO 200
                  END IF
               END IF
            END DO

*        Calculate Y-fit balance factors.
            IF ( FF_NPOLYSX .GT. 1 ) THEN
               DO IP = NYFIT, 1, -1
                  IF ( YDATA( IP ) .GT. 0.0 )
     :               YFIT( IP ) = MIN( 10.0, MAX( 0.1,
     :                     ABS( YFIT( IP ) / YDATA( IP ) ) ) )
               END DO
            END IF

*        Create balance factors for this Y-offset position.
*        Loop through X-pixel coordinates.
            IYFIT = 0
            IXFIT = ( IOFF - DEK_BELOW ) * 10 + 1
            DO IWAVE = 1, NX
               YCOORD = INT( Y_TRACE_COORD( IWAVE ) + 0.5 ) + IOFF

*           If pixel within frame boundaries then.
               IF ( YCOORD .GT. 0 .AND. YCOORD .LE. NY ) THEN
                  IF ( GOT_QUALITY ) THEN
                     IF ( QUALITY( IWAVE, YCOORD ) .NE.0) THEN
                        GO TO 98
                     END IF
                  END IF
                  IF ( IMAGE( IWAVE, YCOORD ) .EQ.
     :                 ECH__BAD_REAL ) GO TO 98
                  IYFIT = IYFIT + 1

*              Calculate balance factor from fits.
                  IF ( IMAGE( IWAVE, YCOORD ) .GT. 0.0 .OR.
     :                 ( FF_NPOLYSX .EQ. 1 .AND. .NOT. FFLMED ) )
     :                 THEN
                     FLAT_MODEL( IWAVE, IOFF ) = YFIT( IYFIT ) *
     :                     XFIT( IXFIT ) * COLFIT( IWAVE )
                     GO TO 99
                  END IF
   98             FLAT_MODEL( IWAVE, IOFF ) = -1.0
   99          END IF
            END DO
 101        CONTINUE
         END DO
      END IF

*  Calculate errors for balance frame or use supplied errors if available.
      DO IX = 1, NX
         YCOORD = INT( Y_TRACE_COORD( IX ) + 0.5 ) + DEK_BELOW
         IOFFST = DEK_BELOW
         IOFFEN = DEK_ABOVE
         IF ( YCOORD .LT. 1 ) THEN
            IOFFST = IOFFST - YCOORD + 1
            YCOORD = 1
         END IF
         IF ( YCOORD + IOFFEN - IOFFST .GT. NY ) THEN
            IOFFEN = IOFFST + NY - YCOORD
         END IF
         DO IY = IOFFST, IOFFEN
            IF ( .NOT. NO_ERRORS ) THEN
               IF ( ERRORS( IX, YCOORD ) .GT. 0.0
     :              .AND. IMAGE( IX, YCOORD ) .GT. 0.0 )
     :            FLAT_MODEL_ERR( IX, IY ) =
     :                  ERRORS( IX, YCOORD ) ** 2.0 /
     :                  IMAGE( IX, YCOORD )

            ELSE
               IF ( IMAGE( IX, YCOORD ) .GT. 0.0 )
     :            FLAT_MODEL_ERR( IX, IY ) = 1.0 /
     :                  ABS( IMAGE( IX, YCOORD ) )
            END IF
            YCOORD = YCOORD + 1
         END DO
      END DO

*  Check the balance model for deviant or negative values.
 998  CONTINUE
      NZERO = 0
      NHUGE = 0
      DO IX = 1, NX
         DO IY = DEK_BELOW, DEK_ABOVE
            IF ( FLAT_MODEL( IX, IY ) .LE. 0.0 ) THEN
               NZERO = NZERO + 1
               FLAT_MODEL( IX, IY ) = 1.0
               FLAT_MODEL_ERR( IX, IY ) = 0.0

            ELSE IF ( FLAT_MODEL( IX, IY ) .LE. 0.1 .OR.
     :           FLAT_MODEL( IX, IY ) .GT. 10.0 ) THEN
               NHUGE = NHUGE + 1
               FLAT_MODEL( IX, IY ) = 1.0
               FLAT_MODEL_ERR( IX, IY ) = 0.0
            END IF
         END DO
      END DO

*  Report any non-positive pixels.
      IF ( NZERO .NE. 0 ) THEN
         CALL CHR_ITOC( NZERO, REF_STR1, NCHAR1 )
         REPORT_STRING = ' Balance model had ' //
     :                   REF_STR1( :NCHAR1 ) //
     :                   ' non-positive pixels.'
         CALL ECH_REPORT( 0, REPORT_STRING )
         CALL ECH_REPORT( 0,
     :        ' These now have balance set to unity.' )
      ELSE
         CALL ECH_REPORT( 0,
     :        ' Balance model contained no non-positive pixels.' )
      END IF

*  Report any hugely deviant pixels.
      IF ( NHUGE .NE. 0 ) THEN
         CALL CHR_ITOC( NHUGE, REF_STR1, NCHAR1 )
         REPORT_STRING = ' Balance model had ' //
     :                   REF_STR1( :NCHAR1 ) //
     :                   ' very deviant pixels.'
         CALL ECH_REPORT( 0, REPORT_STRING )
         CALL ECH_REPORT( 0,
     :        ' These now have balance set to unity.' )
      ELSE
         CALL ECH_REPORT( 0,
     :        ' Balance model contained no very deviant pixels.' )
      END IF

  999 CONTINUE

      END
