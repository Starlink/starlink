      SUBROUTINE ECH_MODEL_SSKY(
     :           DATA,
     :           ERRORS,
     :           QUALITY,
     :           NX,
     :           NY,
     :           MAX_SKY_PIXELS,
     :           SKY_MASK,
     :           DEK_BELOW,
     :           DEK_ABOVE,
     :           SKY_NPOLY,
     :           SKYREJ,
     :           THRESH,
     :           VARI_SIM,
     :           XFITPOLY,
     :           XWIN,
     :           LINE_THRESH,
     :           READOUT,
     :           PHOTON,
     :           MAXIMUM_POLY,
     :           FITTER,
     :           TRACE_POLYNOMIAL,
     :           SKY_SPECTRUM,
     :           SKY_VARIANCE,
     :           SKY_MODEL,
     :           SKY_MODEL_ERR,
     :           XDATA,
     :           YDATA,
     :           YSIGMA,
     :           YDEVIA,
     :           SKYDATA,
     :           SKYSIGMA,
     :           SKYFIT,
     :           YFIT,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_MODEL_SSKY

*  Purpose:
*     Model sky background (2-D extraction).

*  Description:
*     Adapted from the PAMELA routine FITSKY.
*
*     This version of the routine expects an associated error array which
*     includes the contribution due to flat fielding to be included.
*     It will normally be used after 2d distortion fit scrunching has
*     been used.
*
*     This routine fits low order (TUNE_SKYPOLY=='sky_npoly' terms)
*     polynomials with 'skyrej' reject cycles and reject threshold 'thresh'
*     to every x position. It fits to the pixels specified
*     by 'sky_mask' (sky_mask(I) = 1 indicates that pixel I is sky.
*     The polynomials are then evaluated in the region between the
*     dekker limits.
*
*     A special case occurs if TUNE_SKYPOLY is set to zero, in which case
*     the sky model is set uniformly to ZERO everywhere.
*
*     This version of the module has additions to allow improved estimates
*     to be made by fitting polynomials in the x direction also. This
*     is enabled using the variable 'vari_sim' and is CPU intensive.
*     If enabled then each offset from the order trace has a seperate sky fit
*     done in the x direction. These fits are then subjected to a simulation
*     process to estimate the variance on the fitted sky values. These
*     variances and then used to weight the values used to fit in the
*     perpendicular direction (between dekker limits - spatial).
*     Regions suspected of containing sky lines are selected as those
*     where the observed value deviates from the fit by more than 'line_thresh'
*     sigma, and for such values (and their 'xwin' neighbours on either side)
*     the 'improved' estimates are not used in the per-x-increment fits.

*  Invocation:
*     CALL ECH_MODEL_SSKY(
*    :     DATA,
*    :     ERRORS,
*    :     QUALITY,
*    :     NX,
*    :     NY,
*    :     MAX_SKY_PIXELS,
*    :     SKY_MASK,
*    :     DEK_BELOW,
*    :     DEK_ABOVE,
*    :     SKY_NPOLY,
*    :     SKYREJ,
*    :     THRESH,
*    :     VARI_SIM,
*    :     XFITPOLY,
*    :     XWIN,
*    :     LINE_THRESH,
*    :     READOUT,
*    :     PHOTON,
*    :     MAXIMUM_POLY,
*    :     FITTER,
*    :     TRACE_POLYNOMIAL,
*    :     SKY_SPECTRUM,
*    :     SKY_VARIANCE,
*    :     SKY_MODEL,
*    :     SKY_MODEL_ERR,
*    :     XDATA,
*    :     YDATA,
*    :     YSIGMA,
*    :     YDEVIA,
*    :     SKYDATA,
*    :     SKYSIGM,
*    :     SKYFIT,
*    :     YFIT,
*    :     STATUS
*    :    )

*  Arguments:
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     GOT_QUALITY = LOGICAL (Given)
*        TRUE if quality data is available.
*     SKY_MASK = INTEGER (Given)
*        Status of each pixel across profile.
*     SKY_NPOLY = INTEGER (Given)
*        Degree of polynomial used to model sky profiles.
*     VARI_SIM = LOGICAL (Given)
*        TRUE if simulation to model variance.
*     XFITPOLY = INTEGER (Given)
*        Degree of polynomial used to model sky in x.
*     XWIN = INTEGER (Given)
*        Window for sky line width in pixels.
*     LINE_THRESH = REAL (Given)
*        Threshold for ignoring x fit for possible sky lines.
*     READOUT = REAL (Given)
*        Readout noise level in counts.
*     PHOTON = REAL (Given)
*        Photon to ADU conversion factor.
*     MAX_SKY_PIXELS = INTEGER (Given)
*        Maximum extent of sky and therefore dekker.
*     PFL_SUBSAMPLES = INTEGER (Given)
*        Number of subsamples across dekker.
*     DATA = REAL (Given)
*        Input frame image of dimensions nx columns and ny rows.
*     ERRORS = REAL (Given)
*        Input errors frame image of dimensions nx columns and ny rows.
*     QUALITY = BYTE (Given)
*        Input quality frame image of dimensions nx columns and ny rows.
*     SKY_MODEL = REAL (Given)
*        Modeled sky intensities at offsets from trace.
*     SKY_MODEL_ERR = REAL (Given)
*        Modeled sky errors at offsets from trace.
*     SUBSTEPS = INTEGER (Given)
*        Number of substeps per pixel for subsampling.
*     MODELED_PROFILE = REAL    ( WRITE )
*        Model profile formed using global averaging.
*     DEK_BELOW = INTEGER (Given)
*        Extent of dekker below order trace.
*     DEK_ABOVE = INTEGER (Given)
*        Extent of dekker above order trace.
*     SKYREJ = INTEGER (Given)
*
*     THRESH = REAL (Given)
*        Rejection threshold (sigma) for fit.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum number of fit coefficients allowed.
*     FITTER = CHAR (Given)
*        Type of wavelength fitting function in use (POLY/SPLINE).
*     TRACE_POLYNOMIAL = DOUBLE (Given)
*        Coefficients of fit describing order path across frame.
*     SKY_SPECTRUM = REAL (Given and Returned)
*        Extracted sky spectrum.
*     SKY_VARIANCE = REAL (Given and Returned)
*        Extracted sky spectrum variances.
*     XDATA = REAL (Given and Returned)
*        X data for fit.
*     YDATA = REAL (Given and Returned)
*        Y data for fit.
*     YSIGMA = REAL (Given and Returned)
*        Deviations on fit.
*     YDEVIA = REAL (Given and Returned)
*        Deivations from fit.
*     SKYDATA = REAL (Given and Returned)
*        Values to fit.
*     SKYSIGMA = REAL (Given and Returned)
*        Deviations on fitted values.
*     SKYFIT = REAL (Given and Returned)
*        Fitted valuers.
*     YFIT = REAL (Given and Returned)
*        Fitted values.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     If order has a valid trace then
*        Ensure photon conversion factor is reasonable
*        Zero sky model and variance arrays
*        If polynomial degree is < 0 then default is to leave sky=0 everywhere
*        Else
*           If x polynomials to be used then
*              Loop through offsets above/below order trace
*                 If a sky pixel offset then collect sky data
*                    Fit the wavelength polynomial
*                    Calculate deviations from fit
*                    Set initial sky model to fitted values
*                 Endif
*              End loop
*           Endif
*           Compute number of sky pixels available for fit
*           Initialise counters etc
*           Loop through increments in wavelength direction
*              Load sky data into arrays for poly-fit at this increment
*              Fit polynomial by least squares
*              Apply KDH scaling
*              Apply the pixel rejection criterion (Poisson)
*              Only one pixel at a time is rejected.
*              Evaluate and stow the sky polynomial over the object region
*              Report pixel rejects
*           End loop
*        Endif
*     Endif

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
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_ENVIRONMENT.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      INTEGER MAXIMUM_POLY
      CHARACTER*( * ) FITTER
      DOUBLE PRECISION TRACE_POLYNOMIAL( MAXIMUM_POLY )
      INTEGER DEK_BELOW
      INTEGER DEK_ABOVE
      REAL READOUT
      REAL PHOTON
      LOGICAL GOT_QUALITY
      LOGICAL VARI_SIM
      INTEGER MAX_SKY_PIXELS
      BYTE QUALITY( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2  )
      REAL THRESH
      REAL LINE_THRESH
      INTEGER XFITPOLY
      INTEGER XWIN

*  Arguments Returned:
      REAL SKY_SPECTRUM( NX )
      REAL SKY_VARIANCE( NX )
      REAL ERRORS( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
*          ! Supplied variance frame.
      REAL SKY_MODEL( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
*          ! Modelled sky intensities.
      REAL SKY_MODEL_ERR( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
*          ! Modelled sky variances.
      INTEGER SKY_MASK( -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2  )
*          ! Sky pixel positions mask.

*  Workspace variables used:
      REAL XDATA( NX )
      REAL YDATA( NX )
      REAL YSIGMA( NX )
      REAL YDEVIA( NX )
      REAL SKYDATA( NX )
      REAL SKYSIGMA( NX )
      REAL SKYFIT( NX )
      REAL YFIT( NX )
      REAL DATA( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
*          ! Image frame array.

*  Status:
      INTEGER status

*  Local Variables:
      DOUBLE PRECISION TEMP_COEFFS( MAX_FIT_COEFFS )

      REAL RDUMMY( 2 )
      REAL INC_VAL
      REAL INC_MEAN
      REAL INC_MEANSQ
      REAL SKYVAR
      REAL SPVAR
      REAL THRLO
      REAL THRHI
      REAL SIG
      REAL RMIN
      REAL CMIN
      REAL ALPHA
      REAL BETA
      REAL PROB
      REAL RESCALE
      REAL RATIO
      REAL RMS
      REAL VARI
      REAL VAR0
      REAL TOTAL
      REAL PIX
      REAL ETA
      REAL VMIN
      REAL AVGSKY
      REAL AVGSKY2
      REAL AVGVAR
      REAL SKYMEAN
      REAL RMSAVG

      INTEGER SKY_NPOLY
      INTEGER SKYREJ
      INTEGER I
      INTEGER NFIT
      INTEGER IWAVE
      INTEGER XCO
      INTEGER XCO2
      INTEGER IPROFILE
      INTEGER IREJ
      INTEGER NDF
      INTEGER ICYCLE
      INTEGER NPIXTOT
      INTEGER NFITS
      INTEGER SKYREJTOT
      INTEGER NYFIT
      INTEGER NPIX
      INTEGER NSOME
      INTEGER NCOUNT
      INTEGER K
      INTEGER IOFF
      INTEGER IOFF2
      INTEGER NCHAR1
      INTEGER NCHAR2
      INTEGER NCHAR3
      INTEGER NCHAR4
      INTEGER NCHAR5

      LOGICAL PIXEL_OK

      CHARACTER*32 WORK_STRING
      CHARACTER*16 REF_STR1
      CHARACTER*16 REF_STR2
      CHARACTER*8 REF_STR3
      CHARACTER*8 REF_STR4
      CHARACTER*8 REF_STR5

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
      REAL CHANCE
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  These variables may be changed to input/output parameters to allow
*  the caller to alter them at some stage.
      got_quality = .TRUE.

*  If order has no valid trace then do nothing.
      IF ( TRACE_POLYNOMIAL( 1 ) .EQ. ECH__BAD_DOUBLE ) THEN
         CALL ECH_REPORT( 0, ' Order disabled: no modelling.' )
         GO TO 999
      END IF

*  Factor for weighting low-count cases.  This is
*  derived by requiring that pixels cannot be
*  incorrectly weighted by more than a factor BETA
*  for an alpha sigma fluctuation.  For CCDs this should
*  be unimportant (likely to be zero) whereas for
*  photon counting detectors, it is important.
      ALPHA = 3.0
      BETA = 2.0
      VAR0 = READOUT * READOUT
      CMIN = ( ALPHA * BETA / ( BETA - 1.0 ) ) ** 2.0 / PHOTON -
     :       VAR0 * PHOTON
      CMIN = MAX( 0.0, CMIN )
      IF ( SKY_NPOLY .EQ. 0 ) CMIN = 1.0

*  Ensure photon conversion factor is reasonable.
      IF ( PHOTON .LE. 0.0 ) THEN
         PHOTON = 1.0
         CALL ECH_REPORT( 0,
     :        ' Defaulting ADU conversion factor to 1.' )
      ENDIF

*  Zero sky model and variance arrays.
      CALL ECH_ZERO_REAL( NX, SKY_SPECTRUM( 1 ) )
      CALL ECH_ZERO_REAL( NX, SKY_VARIANCE( 1 ) )
      CALL ECH_ZERO_REAL( NX * MAX_SKY_PIXELS,
     :     SKY_MODEL( 1, -MAX_SKY_PIXELS / 2 ) )
      CALL ECH_ZERO_REAL( NX * MAX_SKY_PIXELS,
     :     SKY_MODEL_ERR( 1, -MAX_SKY_PIXELS / 2 ) )

*  If polynomial degree is < 0 then default is to leave sky=0 everywhere.
      IF ( FITTER .EQ. 'NONE' ) SKY_NPOLY = -1
      IF ( FITTER .EQ. 'MEAN' ) THEN
         SKY_NPOLY = 0
         XFITPOLY = 0
      ENDIF
      IF ( SKY_NPOLY .EQ. 0 ) THEN
         CALL ECH_REPORT( 0,
     :        ' Sky calculation using average at each column.' )

      ELSE IF ( SKY_NPOLY .LT. 0 ) THEN
         CALL ECH_REPORT( 0,
     :        ' Sky polynomial order is negative.' )
         CALL ECH_REPORT( 0,
     :        ' Sky estimates set to zero everywhere.' )
         GO TO 999
      END IF

*  If X polynomials to be used then
      IF ( XFITPOLY .GT. 0 ) THEN

*     Loop through offsets above/below order trace.
         DO IOFF = DEK_BELOW, DEK_ABOVE
            NYFIT = 0

*        If a sky pixel offset then collect sky data.
            IF ( SKY_MASK( IOFF ) .EQ. 1 ) THEN
               CALL CHR_ITOC( IOFF, REF_STR1, NCHAR1 )
               REPORT_STRING = ' Wavelength-dependent model' //
     :               ' of spatial increment at offset ' //
     :               REF_STR1( :NCHAR1 ) // 'pixels.'
               CALL ECH_REPORT( 0, REPORT_STRING )
               IF ( GOT_QUALITY ) THEN
                  DO IWAVE = 1, NX
                    IF ( QUALITY( IWAVE, IOFF ) .NE. 0 )
     :                  GO TO 100
                     NYFIT = NYFIT + 1
                     XDATA( NYFIT ) = REAL( IWAVE ) / REAL( NX )
                     YDATA( NYFIT ) = DATA( IWAVE, IOFF )
                     YSIGMA( NYFIT ) = 1.0
  100             END DO

               ELSE
                  DO IWAVE = 1, NX
                     NYFIT = NYFIT + 1
                     XDATA( NYFIT ) = REAL( IWAVE ) / REAL( NX )
                     YDATA( NYFIT ) = DATA( IWAVE, IOFF )
                     YSIGMA( NYFIT ) = 1.0
                  END DO
               END IF

*           Fit the wavelength polynomial.
               THRHI = THRESH
               THRLO = - THRHI
               WORK_STRING = 'REAL-' // FITTER
               CALL ECH_FITTER( WORK_STRING,
     :              XFITPOLY, TEMP_COEFFS,
     :              NYFIT, XDATA, YDATA, YSIGMA,
     :              SKYREJ, THRHI, STATUS )
               CALL ECH_FEVAL( FITTER, XFITPOLY, TEMP_COEFFS,
     :              NYFIT, XDATA, YFIT, STATUS )

               IF ( vari_sim ) THEN
                  CALL ECH_REPORT( 0, ' Evaluating fit by simulation.' )
                  CALL ECH_MODEL_FITVAR( XDATA, YDATA, NYFIT, FITTER,
     :               YFIT, YSIGMA, XFITPOLY, SKYREJ, THRESH, STATUS )
               END IF

*           Calculate deviations from fit.
               DO I = 1, NYFIT
                  XCO = INT( XDATA( I ) * FLOAT( NX ) )
                  INC_VAL = ABS( DATA( XCO, IOFF )  - YFIT( I ) )
                  YDEVIA( I ) = INC_VAL
                  INC_MEAN = INC_MEAN + INC_VAL
                  INC_MEANSQ = INC_MEANSQ + INC_VAL * INC_VAL
               END DO
               INC_MEAN = INC_MEAN / FLOAT( NYFIT )

*           Set initial sky model to fitted values.
               DO i = 1, nyfit
                  XCO = INT( XDATA( I ) * FLOAT( NX ) )

*              If deviation of the fit from the observed value is less than
*              line_thresh rms deviation then set model to fitted value.
                  IF ( YDEVIA( I ) .LE. LINE_THRESH *
     :                 SQRT( MAX( 0.0, YFIT( I ) ) ) ) THEN
                     SKY_MODEL( XCO, IOFF ) = YFIT( I )
                     SKY_MODEL_ERR( XCO, IOFF ) =
     :                     YSIGMA( I ) * YSIGMA( I )

*              Otherwise ensure model is not set in surrounding
*              area either (suspected skyline region).
                  ELSE
                     DO IOFF2 = DEK_BELOW, DEK_ABOVE
                        DO XCO2 = MAX( XCO - XWIN, 1 ),
     :                            MIN( XCO + XWIN, NX )
                        SKY_MODEL( XCO2, IOFF2 ) = 0.0
                        SKY_MODEL_ERR( XCO2, IOFF2 ) = 0.0
                        END DO
                     END DO
                  END IF
               END DO
            ENDIF
         END DO
      ENDIF

*  Fit sky increment by increment. Adapted from PAMELA routine 'FITsky'.
      THRHI = ABS( THRESH )
      THRLO = -THRHI

*  Compute number of sky pixels available for fit.
      NPIX = 0
      DO IPROFILE = DEK_BELOW, DEK_ABOVE
         IF ( SKY_MASK( IPROFILE ) .GT. 0 ) NPIX = NPIX + 1
      END DO
      IF ( NPIX .LT. SKY_NPOLY ) THEN
         CALL ECH_REPORT( 0, ' Not enough sky pixels for fit.' )
         GO TO 999
      END IF

*  Initialise counters etc.
      NPIXTOT = 0
      SKYREJTOT = 0
      NFITS   = 0
      RMSAVG = 0.0
      NSOME = 0

*  Loop through increments in wavelength direction.
      DO IWAVE = 1, NX

*     Load sky data into arrays for poly-fit at this increment.
         NCOUNT = 0
         NPIX =   0
         AVGSKY = 0
         AVGSKY2 = 0
         AVGVAR = 0
         DO IPROFILE = DEK_BELOW, DEK_ABOVE

*        Use sky_mask to select sky pixels
*        On first cycle, weights are corrected
*        by CMIN factor from above to prevent excessive
*        weight being given to low count pixels. For low
*        signal photon counting data this gives almost uniform
*        weights, but it allows lower weights on CCD data.
            PIXEL_OK = .TRUE.
            IF ( GOT_QUALITY ) THEN
               IF ( QUALITY( IWAVE, IPROFILE ) .NE. 0 )
     :            PIXEL_OK = .FALSE.
            ENDIF

            IF ( SKY_MASK( IPROFILE ) .EQ. 1 .AND. PIXEL_OK ) THEN
               NPIX = NPIX + 1
               XDATA( NPIX ) = REAL( IPROFILE ) /
     :               REAL( DEK_ABOVE - DEK_BELOW )

*           If we have a fitted value from a x polynomial fit the
*           we can use the modeled variance
               IF ( SKY_MODEL( IWAVE, IPROFILE ) .GT. 0.0 ) THEN
                  SKYDATA( NPIX ) = SKY_MODEL( IWAVE, IPROFILE )
                  SKYSIGMA( NPIX ) = SKY_MODEL_ERR( IWAVE, IPROFILE )

*           Otherwise we have to just estimate variance according
*           to 'root N' statistics. These variances are used to
*           weight the per increment fits.
               ELSE
                  SKYDATA( NPIX ) = DATA( IWAVE, IPROFILE )

*              If user has provided variance frame along with
*              the data then use those values.
                  IF ( ERRORS( IWAVE, IPROFILE ) .GT. 0.0 ) THEN
                     SKYSIGMA( NPIX ) =
     :                     SQRT( MAX( 0.0, ( VAR0 + MAX(
     :                     ERRORS( IWAVE, IPROFILE ), CMIN ) /
     :                     MAX( 1.0, PHOTON ) ) ) )

*              Otherwise calculate a variance using the actual value.
                  ELSE
                     SKYSIGMA( NPIX ) =
     :                     SQRT( MAX( 0.0, ( VAR0 + MAX(
     :                     DATA( IWAVE, IPROFILE ), CMIN ) /
     :                     MAX( 1.0, PHOTON ) ) ) )
                  END IF
               END IF
               NCOUNT = NCOUNT + NINT( DATA( IWAVE, IPROFILE) )
               AVGSKY = AVGSKY + DATA( IWAVE, IPROFILE )
               AVGSKY2 = AVGSKY2 + DATA( IWAVE, IPROFILE ) *
     :                             DATA( IWAVE, IPROFILE )
            END IF
         END DO
         NPIXTOT = NPIXTOT + NPIX
         AVGSKY = AVGSKY / FLOAT( MAX( 1, NPIX ) )
         AVGSKY2 = AVGSKY2 / FLOAT( MAX( 1, NPIX ) )
         AVGVAR = ABS( AVGSKY2 - AVGSKY * AVGSKY )

*     Pixel reject cycle and refining the variances.
*     If no counts then skip all this. One extra
*     fit is forced to use the improved variance estimates.
         IF ( NCOUNT .GT. 0 ) THEN
            IF ( SKY_NPOLY .GT. 0 ) THEN
               NSOME = NSOME + 1
               ICYCLE = 0
               IREJ = 1
               DO WHILE ( ICYCLE .LE. 1 .OR.
     :                    ( IREJ .GT. 0 .AND. ICYCLE .LE. SKYREJ ) )
                  ICYCLE = ICYCLE + 1

*              Fit polynomial by least squares.
                  WORK_STRING = 'REAL-' // FITTER
                  CALL ECH_FITTER( WORK_STRING, SKY_NPOLY, TEMP_COEFFS,
     :                 NPIX, XDATA, SKYDATA, SKYSIGMA, 0, THRHI,
     :                 STATUS )
                  CALL ECH_FEVAL( FITTER, SKY_NPOLY, TEMP_COEFFS,
     :                 NPIX, XDATA, SKYFIT, STATUS )
                  NFITS = NFITS + 1

*              Revise sigma estimates using the polynomial sky model
*              First estimate total counts and therefore a floor to
*              the variance. This prevents overweighting taking into
*              account the uncertainty in the polynomial fit.
*              Only do this if a pixel has been rejected from previous cycle.
                  IF ( IREJ .GT. 0 ) THEN
                     TOTAL = 0.0
                     PIX = 0.0
                     DO K = 1, NPIX
                        IF ( SKYSIGMA( K ) .GT. 0.0 ) THEN
                           TOTAL = TOTAL + SKYDATA( K )
                           PIX = PIX + 1.0
                        END IF
                     END DO
                     VMIN = ALPHA * BETA / ( BETA - 1 ) *
     :                     SQRT( MAX( 0.0, REAL( SKY_NPOLY ) / PIX ) )
     :                     *  SQRT( MAX ( 0.0,
     :                     VAR0 + TOTAL / ( PIX * PHOTON ) ) ) -
     :                     VAR0 * PHOTON
                     RMS = 0.0
                     SKYMEAN = 0.0
                     NFIT = 0
                     DO K = 1, NPIX
                        IF ( SKYSIGMA( K ) .GT. 0.0 ) THEN
                           NFIT = NFIT + 1
                           VARI = VAR0 + MAX( VMIN, ABS( SKYFIT( K ) ) )
     :                           / MAX( 1.0, PHOTON )
                           SKYSIGMA ( K ) = SQRT( MAX( 0.0, VARI ) )
                           SKYMEAN = SKYMEAN + SKYFIT( K )
                           ETA = ( SKYDATA( K ) - SKYFIT( K ) ) /
     :                           MAX( 1.0, SKYSIGMA( K ) )
                           RMS = RMS + ETA * ETA

                        ELSE
                           VARI = VAR0 +
     :                           MAX( VMIN, ABS( SKYFIT( K ) ) ) /
     :                           MAX( 1., PHOTON )
                           SKYSIGMA( K ) = - ABS( SQRT(
     :                           MAX( 0.0, VARI ) ) )
                        END IF
                     END DO
                     NDF = NFIT - SKY_NPOLY
                     IF ( NFIT .GT. 0 ) THEN
                        SKYMEAN = SKYMEAN / REAL( NFIT )
                        RMS = SQRT( MAX( 0.0, RMS /
     :                          REAL( MAX( 1, NDF ) ) ) )
                        IF ( NDF .LE. 1 ) THEN
                           RMS = 1.0
                           NDF = 1
                        END IF

*                    Apply KDH scaling.
                        PROB = EXP( -REAL( NDF ) *
     :                            ( RMS - 1 ) ** 2 / 4. )
                        RMS = PROB + ( 1.0 - PROB ) * RMS

                     ELSE
                        CALL CHR_ITOC( IWAVE, REF_STR1, NCHAR1 )
                        REPORT_STRING =
     :                        ' All pixels rejected at' //
     :                        ' wavelength ' //
     :                        REF_STR1( :NCHAR1 ) // '.'
                        CALL ECH_REPORT( 0, REPORT_STRING )
                        SKY_SPECTRUM( IWAVE ) = ECH__BAD_REAL
                        SKY_VARIANCE( IWAVE ) = ECH__BAD_REAL
                        GO TO 800
                     END IF

*                 Apply the pixel rejection criterion (Poisson).
*                 Only one pixel at a time is rejected.
                     RESCALE = MAX( 0.01, RMS )
                     IF ( ICYCLE .LE. SKYREJ ) THEN
                        RMIN = 2.0
                        IREJ = 0
                        DO K = 1, NPIX
                           SIG = RESCALE * SKYSIGMA( K )
                           IF ( SIG .GT. 0 ) THEN

*                          CHANCE returns the probability of getting
*                          as bad as the observed value divided by the
*                          probabilty of getting as bad as the threshold.
*                          (Only for values less than 1 (i.e. rejects),
*                          otherwise = -1).
                              RATIO = CHANCE( SKYDATA( K ),
     :                              SKYFIT( K ),
     :                              SIG, THRLO, THRHI )
                              IF ( RATIO .GT. -0.5 .AND.
     :                             RATIO .LT. RMIN ) THEN
                                 RMIN = RATIO
                                 IREJ = K
                              END IF
                           END IF
                        END DO
                        IF ( IREJ .GT. 0 ) THEN
                           SKYSIGMA( IREJ ) =
     :                           -ABS( SKYSIGMA( IREJ ) )
                           SKYREJTOT = SKYREJTOT + 1
                        END IF
                     END IF
                  END IF
               END DO

*           Evaluate and stow the sky polynomial over the object region.
               SPVAR = 0.0
               DO K = 1, NPIX
                  SPVAR = SPVAR + SKYSIGMA( K ) * SKYSIGMA( K )
               END DO
               SPVAR = SPVAR / FLOAT( NPIX )
               SKYMEAN = 0.0
               SKYVAR = 0.0
               NPIX = 0
               DO IPROFILE = DEK_BELOW, DEK_ABOVE
                  IF ( SKY_MODEL( IWAVE, IPROFILE ) .EQ. 0.0 )
     :                 THEN
                     RDUMMY( 1 ) = REAL( IPROFILE ) /
     :                     REAL( DEK_ABOVE - DEK_BELOW )
                     CALL ECH_FEVAL( FITTER, SKY_NPOLY,
     :                    TEMP_COEFFS, 1, RDUMMY,
     :                    SKY_MODEL( IWAVE, IPROFILE ),
     :                    STATUS )
                     SKY_MODEL_ERR( IWAVE, IPROFILE ) = 0.0
                  END IF
                  SKYMEAN = SKYMEAN + SKY_MODEL( IWAVE, IPROFILE )
                  SKYVAR = SKYVAR + SPVAR
               END DO
               SKYMEAN = SKYMEAN /
     :               MAX( 1, DEK_ABOVE - DEK_BELOW + 1 )
               SKYVAR = SKYVAR /
     :               MAX( 1, DEK_ABOVE - DEK_BELOW + 1 )
               SKY_SPECTRUM( IWAVE ) = AVGSKY
               SKY_VARIANCE( IWAVE ) = AVGVAR

*           Report rejected pixels.
               IF ( MOD( IWAVE, 200 ) .EQ. 0 ) THEN
                  CALL CHR_ITOC( IWAVE, REF_STR1, NCHAR1 )
                  CALL CHR_RTOC( SKYMEAN, REF_STR2, NCHAR2 )
                  CALL CHR_RTOC( RMS, REF_STR3, NCHAR3 )
                  CALL CHR_ITOC( SKYREJTOT, REF_STR4, NCHAR4 )
                  CALL CHR_RTOC( 100.0 * REAL( SKYREJTOT ) /
     :                 REAL( NPIXTOT ), REF_STR5, NCHAR5 )
                  REPORT_STRING = ' X=' // REF_STR1( :NCHAR1 ) //
     :                  ' Sky=' // REF_STR2( :NCHAR2 ) //
     :                  ' RMS=' // REF_STR3( :NCHAR3 ) // ', ' //
     :                  REF_STR4( :NCHAR4 ) // ' rejects=' //
     :                  REF_STR5( :NCHAR5 ) // '%'
                  CALL ECH_REPORT( 0, REPORT_STRING )
               END IF
               RMSAVG = RMSAVG + RMS

            ELSE
               DO IPROFILE = DEK_BELOW, DEK_ABOVE
                  SKY_MODEL( IWAVE, IPROFILE ) = AVGSKY
                  SKY_MODEL_ERR( IWAVE, IPROFILE ) = AVGVAR
               END DO
               SKY_SPECTRUM( IWAVE ) = AVGSKY
               SKY_VARIANCE( IWAVE ) = AVGVAR
               IF ( MOD( IWAVE, 200 ) .EQ. 0 ) THEN
                  CALL CHR_ITOC( IWAVE, REF_STR1, NCHAR1 )
                  CALL CHR_RTOC( AVGSKY, REF_STR2, NCHAR2 )
                  REPORT_STRING = ' X=' // REF_STR1( :NCHAR1 ) //
     :                  ' Sky=' // REF_STR2( :NCHAR2 ) //'.'
                  CALL ECH_REPORT( 0, REPORT_STRING )
               END IF
            END IF
         END IF
  800    CONTINUE
      END DO

      IF ( SKY_NPOLY .GT. 0 ) THEN
         CALL CHR_RTOC( REAL( NFITS ) / REAL( NX ), REF_STR1, NCHAR1 )
         REPORT_STRING = ' Mean fits/Col: ' // REF_STR1( :NCHAR1 ) //'.'
         CALL ECH_REPORT( 0, REPORT_STRING )

         RMSAVG = RMSAVG / MAX( 1.0, REAL( NSOME ) )
         CALL CHR_RTOC( REAL( INT( 1000.0 * RMSAVG ) ) / 1000.0,
     :        REF_STR1, NCHAR1 )
         CALL CHR_ITOC( SKYREJTOT, REF_STR2, NCHAR2 )
         CALL CHR_RTOC( 100.0 * REAL( SKYREJTOT ) /
     :        MAX( 1.0, REAL( NPIXTOT ) ), REF_STR3, NCHAR3 )
         REPORT_STRING = ' Average RMS=' // REF_STR1( :NCHAR1 ) //
     :         ', ' // REF_STR2( :NCHAR2 ) // ' rejects=' //
     :         REF_STR3( :NCHAR3 ) // '%'
         CALL ECH_REPORT( 0, REPORT_STRING )
      END IF

  999 CONTINUE

      END
