      SUBROUTINE ECH_MODEL_SKY(
     :           DATA,
     :           QUALITY,
     :           ERRORS,
     :           NX,
     :           NY,
     :           ORDER_NUMBER,
     :           INTERACTIVE,
     :           NOFLAT,
     :           NO_ERRORS,
     :           FLAT_MODEL,
     :           FLAT_MODEL_ERR,
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
     :           X_TRACE_COORD,
     :           Y_TRACE_COORD,
     :           XDATA,
     :           YDATA,
     :           YSIGMA,
     :           YDEVIA,
     :           SKYDATA,
     :           SKYSIGMA,
     :           BALANCE,
     :           SKYFIT,
     :           YFIT,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_MODEL_SKY

*  Purpose:
*     Model sky background.

*  Description:
*     This routine is adapted from the PAMELA routine FITSKY.
*
*     This routine  fits low-order  (TUNE_SKYPOLY='sky_npoly' terms)
*     polynomials with  'skyrej' reject cycles  and reject threshold
*     'thresh' to every X-position.  It fits to the pixels specified
*     by 'sky_mask' (sky_mask(I) = 1 indicates that pixel I is sky).
*     The polynomials are then  evaluated in the  region between the
*     dekker limits.
*
*     A special case occurs if TUNE_SKYPOLY is set to zero, in which
*     case the sky model is set uniformly to ZERO everywhere.
*
*     This  version of the module  has additions  to allow  improved
*     estimates to be made by fitting polynomials in the X direction
*     also. This is enabled using the variable 'vari_sim' and is CPU
*     intensive.  If enabled  then each offset from  the order trace
*     has a seperate sky fit done in the X direction. These fits are
*     then subject to a simulation  process to estimate the variance
*     on the  fitted sky values.   These variances  and then used to
*     weight  the values used  to fit in the perpendicular direction
*     (between  dekker  limits  -  spatial).   Regions suspected  of
*     containing sky lines are selected  as those where the observed
*     value deviates from the fit  by more than 'line_thresh' sigma,
*     and  for such values  (and their  'xwin' neighbours on  either
*     side)  the  'improved' estimates are  not  used  in the per-x-
*     increment fits.

*  Invocation:
*     CALL ECH_MODEL_SKY(
*     :    DATA,
*     :    QUALITY,
*     :    ERRORS,
*     :    NX,
*     :    NY,
*     :    ORDER_NUMBER,
*     :    INTERACTIVE,
*     :    NOFLAT,
*     :    NO_ERRORS,
*     :    FLAT_MODEL,
*     :    FLAT_MODEL_ERR,
*     :    MAX_SKY_PIXELS,
*     :    SKY_MASK,
*     :    DEK_BELOW,
*     :    DEK_ABOVE,
*     :    SKY_NPOLY,
*     :    SKYREJ,
*     :    THRESH,
*     :    VARI_SIM,
*     :    XFITPOLY,
*     :    XWIN,
*     :    LINE_THRESH,
*     :    READOUT,
*     :    PHOTON,
*     :    MAXIMUM_POLY,
*     :    FITTER,
*     :    TRACE_POLYNOMIAL,
*     :    SKY_SPECTRUM,
*     :    SKY_VARIANCE,
*     :    SKY_MODEL,
*     :    SKY_MODEL_ERR,
*     :    X_TRACE_COORD,
*     :    Y_TRACE_COORD,
*     :    XDATA
*     :    YDATA
*     :    YSIGMA
*     :    YDEVIA,
*     :    SKYDATA
*     :    SKYSIGMA
*     :    BALANCE,
*     :    SKYFIT
*     :    YFIT,
*     :    STATUS
*     :   )

*  Arguments:
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     NOFLAT = LOGICAL (Given)
*        TRUE if no balance factors are available.
*     SKY_MASK( -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 ) = INTEGER (Given)
*        Status of each pixel across profile.
*     SKY_NPOLY = INTEGER (Given)
*        Degree of polynomial used to model sky profiles.
*     VARI_SIM = LOGICAL (Given)
*        TRUE if simulation to model variance.
*     XFITPOLY = INTEGER (Given)
*        Degree of polynomial used to model sky in X.
*     XWIN = INTEGER (Given)
*        Window for sky-line width in pixels.
*     LINE_THRESH = REAL (Given)
*        Threshold for ignoring X fit for possible sky lines.
*     READOUT = REAL (Given)
*        Readout noise level in counts.
*     PHOTON = REAL (Given)
*        Photon to ADU conversion factor.
*     MAX_SKY_PIXELS = INTEGER (Given)
*        Maximum extent of sky and therefore dekker.
*     DATA( NX, NY ) = REAL (Given)
*        Input frame image of dimensions NX by NY
*     ERRORS( NX, NY ) = REAL (Given)
*        Input errors frame image of dimensions NX by NY.
*     QUALITY( NX, NY ) = BYTE (Given)
*        Input quality frame image of dimensions NX by NY.
*     SKY_MODEL( NX, -MAX_SKY_PIXELS / 2: MAX_SKY_PIXELS / 2 ) = REAL (Given)
*        Modeled sky intensities at offsets from trace.
*     SKY_MODEL_ERR( NX, -MAX_SKY_PIXELS / 2: MAX_SKY_PIXELS / 2 ) = REAL (Given)
*        Modeled sky errors at offsets from trace.
*     FLAT_MODEL( NX, -MAX_SKY_PIXELS / 2: MAX_SKY_PIXELS / 2 ) = REAL (Given)
*        Balance factors at offsets from trace.
*     FLAT_MODEL_ERR( NX, -MAX_SKY_PIXELS / 2: MAX_SKY_PIXELS / 2 ) = REAL (Given)
*        Balance factor errors.
*     NO_ERRORS = LOGICAL (Given)
*        TRUE if no error array available.
*     DEK_BELOW = INTEGER (Given)
*        Extent of dekker below order trace.
*     DEK_ABOVE = INTEGER (Given)
*        Extent of dekker above order trace.
*     SKYREJ = INTEGER (Given)
*        Number of reject cycles.
*     THRESH = REAL (Given)
*        Rejection threshold (sigma) for fit.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum number of fit coefficients allowed.
*     FITTER*( * ) = CHAR (Given)
*        Type of wavelength fitting function in use (POLY/SPLINE).
*     TRACE_POLYNOMIAL( MAXIMUM_POLY ) = DOUBLE (Given)
*        Coefficients of fit describing order path across frame.
*     SKY_SPECTRUM( NX ) = REAL (Given and Returned)
*        Extracted sky spectrum.
*     SKY_VARIANCE( NX ) = REAL (Given and Returned)
*        Extracted sky spectrum variances.
*     X_TRACE_COORD( NX ) = DOUBLE (Temporary Workspace)
*        X co-ords of order trace path.
*     Y_TRACE_COORD( NX ) = DOUBLE (Temporary Workspace)
*        Y co-ords of order trace path.
*     XDATA( NX ) = REAL (Given and Returned)
*        X data for fit.
*     YDATA( NX ) = REAL (Given and Returned)
*        Y data for fit.
*     YSIGMA( NX ) = REAL (Given and Returned)
*        Deviations on fit.
*     YDEVIA( NX ) = REAL (Given and Returned)
*        Deivations from fit.
*     SKYDATA( NX ) = REAL (Given and Returned)
*        Values to fit.
*     SKYSIGMA( NX ) = REAL (Given and Returned)
*        Deviations on fitted values.
*     BALANCE( NX ) = REAL (Given)
*        Balance factors.
*     SKYFIT( NX ) = REAL (Given and Returned)
*        Fitted values.
*     YFIT( NX ) = REAL (Given and Returned)
*        Fitted values.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     {enter_further_changes_here}

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
      DOUBLE PRECISION TRACE_POLYNOMIAL( MAXIMUM_POLY ) ! Trace polynomial coefficients.
      INTEGER DEK_BELOW
      INTEGER DEK_ABOVE
      REAL READOUT
      REAL PHOTON
      CHARACTER*( * ) FITTER
      LOGICAL GOT_QUALITY
      LOGICAL NOFLAT
      LOGICAL NO_ERRORS
      LOGICAL GOT_BALANCE
      LOGICAL VARI_SIM
      INTEGER MAX_SKY_PIXELS
      BYTE QUALITY( NX, NY )
      REAL FLAT_MODEL( NX, -MAX_SKY_PIXELS / 2: MAX_SKY_PIXELS / 2 )
*          ! Modelled balance factors.
      REAL FLAT_MODEL_ERR( NX, -MAX_SKY_PIXELS / 2: MAX_SKY_PIXELS / 2 )
*          ! Modelled Balance factor variances.
      REAL THRESH
      REAL LINE_THRESH
      INTEGER XFITPOLY
      INTEGER XWIN

*  Arguments Returned:
      REAL SKY_SPECTRUM( NX )
      REAL SKY_VARIANCE( NX )
      REAL ERRORS( NX, NY )
      REAL SKY_MODEL( NX, -MAX_SKY_PIXELS / 2: MAX_SKY_PIXELS / 2 )
*          ! Modelled sky intensities.
      REAL SKY_MODEL_ERR( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
*          ! Modelled sky variances.
      INTEGER SKY_MASK( -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )
*          ! Sky pixel positions mask.

*  Workspace Variables:
      REAL XDATA( NX )
      REAL YDATA( NX )
      REAL YSIGMA( NX )
      REAL YDEVIA( NX )
      REAL SKYDATA( NX )
      REAL SKYSIGMA( NX )
      REAL DATA( NX, NY )
      REAL BALANCE( NX )
      REAL SKYFIT( NX )
      REAL YFIT( NX )
      DOUBLE PRECISION X_TRACE_COORD( NX )
      DOUBLE PRECISION Y_TRACE_COORD( NX )

*  Status:
      INTEGER STATUS

*  Local Variables:
      DOUBLE PRECISION TEMP_COEFFS( MAX_FIT_COEFFS )

      REAL X_GRAPH( 5000 )
      REAL Y_GRAPH( 5000 )
      REAL RDUMMY( 2 )
      REAL INC_VAL
      REAL INC_MEAN
      REAL INC_MEANSQ
      REAL INC_SIGMA
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
      REAL END_CLIP_MAXDEV
      REAL RMS_DEVIATION
      REAL SKYMEAN
      REAL RMSAVG
      REAL ASKY
      REAL AVGSKY
      REAL AVGSKY2
      REAL AVGVAR

      INTEGER YCOORD
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
      INTEGER IOFFST
      INTEGER IOFFEN
      INTEGER IOFF2
      INTEGER AUTO_CLIP_BY
      INTEGER CLIPPED
      INTEGER COUNT
      INTEGER IX
      INTEGER OPTIONS
      INTEGER ORDER_NUMBER
      INTEGER INC_COUNT
      INTEGER NCHAR1
      INTEGER NCHAR2
      INTEGER NCHAR3
      INTEGER NCHAR4
      INTEGER NCHAR5

      LOGICAL ACCEPTED
      LOGICAL ABANDONED
      LOGICAL PLOTTING
      LOGICAL TRACE_PLOT
      LOGICAL INTERACTIVE

      CHARACTER*80 TITLE
      CHARACTER*80 WORK_STRING
      CHARACTER*16 REF_STR1
      CHARACTER*16 REF_STR2
      CHARACTER*16 REF_STR3
      CHARACTER*16 REF_STR4
      CHARACTER*16 REF_STR5
      CHARACTER*8 LFITTER

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
      REAL CHANCE
      INTEGER CHR_LEN
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  These variables may be changed to input/output parameters to allow
*  the caller to alert them at some stage
      GOT_QUALITY = .TRUE.
      GOT_BALANCE = .NOT. NOFLAT

*  Check that order has a valid trace.
      IF ( TRACE_POLYNOMIAL( 1 ) .EQ. ECH__BAD_DOUBLE ) THEN
         CALL ECH_REPORT( 0, ' Order is disabled: no modelling.' )
         GO TO 999
      END IF

      LFITTER = FITTER
      IF ( FITTER .EQ. 'MEAN' ) LFITTER = 'POLY'

*  Ensure photon-conversion factor is reasonable.
      IF ( PHOTON .LE. 0.0 ) THEN
         PHOTON = 1.0
         CALL ECH_REPORT( 0,
     :        ' Defaulting ADU conversion factor to 1.0.' )
      END IF

*  Factor for weighting low-count cases (only for POLY fits).
*  This is derived by requiring that pixels cannot be
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

*  Zero sky-model and variance arrays.
      CALL ECH_ZERO_REAL( NX, SKY_SPECTRUM( 1 ) )
      CALL ECH_ZERO_REAL( NX, SKY_VARIANCE( 1 ) )
      CALL ECH_ZERO_REAL( NX * MAX_SKY_PIXELS,
     :     SKY_MODEL( 1, -MAX_SKY_PIXELS / 2 ) )
      CALL ECH_ZERO_REAL( NX * MAX_SKY_PIXELS,
     :     SKY_MODEL_ERR( 1, -MAX_SKY_PIXELS / 2 ) )

*  If polynomial degree is < 0 then default is to leave sky=0 everywhere.
      IF ( FITTER .EQ. 'NONE' ) THEN
         SKY_NPOLY = -1

      ELSE IF ( FITTER .EQ. 'MEAN' ) THEN
         SKY_NPOLY = 0
      END IF

      IF ( SKY_NPOLY .LT. 0 ) THEN
         CALL ECH_REPORT( 0,
     :        ' Sky fit coefficient count is negative.' )
         CALL ECH_REPORT( 0, ' Sky estimates set to 0 everywhere.' )
         GO TO 999
      END IF

      IF ( SKY_NPOLY .EQ. 0 ) THEN
         CALL ECH_REPORT( 0,
     :        ' Sky calculation using average at each column.' )
      END IF

*  Calculate order trace coordinates.
      CALL ECH_CALC_TRACE( NX, MAXIMUM_POLY, TRACE_POLYNOMIAL,
     :     X_TRACE_COORD, Y_TRACE_COORD, STATUS )

*  If X-polynomials to be used then.
      IF ( XFITPOLY .GT. 0 ) THEN

*     Loop through offsets above/below order trace.
         ABANDONED = .FALSE.
         DO IOFF = DEK_BELOW, DEK_ABOVE
            ACCEPTED = .FALSE.
            CLIPPED = 0
            AUTO_CLIP_BY = MAX( 1, NX / 50 )
            TRACE_PLOT = .FALSE.
            THRLO = -5.0
            THRHI = 5.0
            PLOTTING = .TRUE.
            NYFIT = 0

*        If a sky pixel offset then collect sky data.
            IF ( SKY_MASK( IOFF ) .EQ. 1 ) THEN
               REPORT_STRING = ' Wavelength-dependent' //
     :                         ' modelling of sky at '
               NCHAR2 = 42
               IF ( IOFF .LT. -1 ) THEN
                  CALL CHR_ITOC( -IOFF, REF_STR1, NCHAR1 )
                  REPORT_STRING = REPORT_STRING( :NCHAR2 ) //
     :                  REF_STR1( :NCHAR1 ) //
     :                  ' pixels below trace.'

               ELSE IF ( IOFF .EQ. -1 ) THEN
                  REPORT_STRING = REPORT_STRING( :NCHAR2 ) //
     :                  ' 1 pixel below trace.'

               ELSE IF ( IOFF .EQ. 0 ) THEN
                  REPORT_STRING = REPORT_STRING( :NCHAR2 ) //
     :                  ' trace centre.'

               ELSE IF ( IOFF .EQ. 1 ) THEN
                  REPORT_STRING = REPORT_STRING( :NCHAR2 ) //
     :                  ' 1 pixel above trace.'

               ELSE IF ( IOFF .GT. 1 ) THEN
                  CALL CHR_ITOC( IOFF, REF_STR1, NCHAR1 )
                  REPORT_STRING = REPORT_STRING( :NCHAR2 ) //
     :                  REF_STR1( :NCHAR1 ) //
     :                  ' pixels above trace.'
               END IF
               CALL ECH_REPORT( 0, REPORT_STRING )
               DO IWAVE = 1, NX, MAX( 1, NX / 400 )
                  YCOORD = INT( Y_TRACE_COORD( IWAVE ) + 0.5 ) + IOFF
                  IF ( YCOORD .GT. 0 .AND. YCOORD .LE. NY ) THEN
                     IF ( GOT_QUALITY ) THEN
                        IF ( QUALITY( IWAVE, YCOORD ) .NE. 0 ) THEN
                           GO TO 100
                        END IF
                     END IF
                     IF ( DATA( IWAVE, YCOORD ) .EQ. ECH__BAD_REAL )
     :                  GO TO 100
                     IF ( GOT_BALANCE ) THEN
                        BALANCE( IWAVE ) = FLAT_MODEL( IWAVE, IOFF )
                     IF ( BALANCE( IWAVE ) .LE. 0.0 )
     :                  BALANCE( IWAVE ) = 1.0

                     ELSE
                        BALANCE( IWAVE ) = 1.0
                     END IF
                     NYFIT = NYFIT + 1
                     XDATA( NYFIT ) = REAL( IWAVE )
                     YDATA( NYFIT ) = DATA( IWAVE, YCOORD )
     :                     * BALANCE( IWAVE )
                     YSIGMA( NYFIT ) = 1.0
  100                CONTINUE
                  END IF
               END DO

*           Loop determining the polynomial coefficients until OK.
               DO WHILE ( .NOT. ( ACCEPTED .OR. ABANDONED ) )

*              Fit the wavelength polynomial.
                  THRHI = THRESH
                  THRLO = - THRHI
                  WORK_STRING = 'REAL-' // LFITTER
                  CALL ECH_FITTER( WORK_STRING, XFITPOLY,
     :                 TEMP_COEFFS, NYFIT, XDATA, YDATA, YSIGMA,
     :                 SKYREJ, THRHI, STATUS )
                  DO IWAVE = 1, NYFIT
                     X_GRAPH( IWAVE ) = XDATA( IWAVE )
                  END DO
                  CALL ECH_FEVAL( LFITTER, XFITPOLY, TEMP_COEFFS,
     :                 NYFIT, XDATA, YFIT, STATUS )
                  IF ( VARI_SIM ) THEN
                     CALL ECH_REPORT( 0,
     :                    ' Evaluating fit by simulation.' )
                     CALL ECH_MODEL_FITVAR( XDATA, YDATA, NYFIT,
     :                    LFITTER, YFIT, YSIGMA, XFITPOLY, SKYREJ,
     :                    THRESH, STATUS )
                  END IF

*              Calculate deviations from fit.
                  INC_MEAN = 0.0
                  INC_MEANSQ = 0.0
                  COUNT = 0
                  DO I = 1, NYFIT
                     XCO = MIN( MAX( 1, INT( X_GRAPH( I ) ) ), NX )
                     YCOORD = INT( Y_TRACE_COORD( XCO ) + 0.5 ) + IOFF
                     IF ( YCOORD .GT. 0 .AND. YCOORD .LE. NY )
     :                    THEN
                        COUNT = COUNT + 1
                        INC_VAL = DATA( XCO, YCOORD ) - YFIT( I )
                        YDEVIA( COUNT ) = INC_VAL
                        Y_GRAPH( COUNT ) = INC_VAL
                        INC_MEAN = INC_MEAN + INC_VAL
                        INC_MEANSQ = INC_MEANSQ + INC_VAL *
     :                               INC_VAL
                     END IF
                  END DO
                  IF ( COUNT .EQ. 0 ) COUNT = 1
                  INC_MEAN = INC_MEAN / FLOAT( COUNT )
                  INC_SIGMA = SQRT( MAX( 0.0, INC_MEANSQ /
     :                   FLOAT( COUNT ) - INC_MEAN * INC_MEAN ) )

*              Determine RMS, max deviation, set acceptance if
*              fit is good enough.
                  RMS_DEVIATION = INC_SIGMA
                  END_CLIP_MAXDEV = THRESH * RMS_DEVIATION

*              Plot if enabled or automatically accepted.
                  IF ( ( .NOT. ACCEPTED ) .AND.
     :                NYFIT .GT. XFITPOLY .AND. INTERACTIVE ) THEN
 222                 CONTINUE
                     OPTIONS = GRPH_CALC_YMINMAX
                     CALL CHR_ITOC( ORDER_NUMBER, REF_STR1, NCHAR1 )
                     CALL CHR_ITOC( IOFF, REF_STR2, NCHAR2 )
                     IF ( IOFF .GT. 0 ) THEN
                        REF_STR2 = '+' // REF_STR2( :NCHAR2 )
                        NCHAR2 = NCHAR2 + 1
                     END IF
                     CALL CHR_ITOC( COUNT, REF_STR3, NCHAR3 )
                     CALL CHR_ITOC( CLIPPED, REF_STR4, NCHAR4 )
                     TITLE = 'Order ' // REF_STR1( :NCHAR1 ) //
     :                       '(' // REF_STR2( :NCHAR2 ) // ')' //
     :                       ': samples=' // REF_STR3( :NCHAR3 ) //
     :                       ', clipped=' // REF_STR4( :NCHAR4 )
                     IF ( FITTER .EQ. 'SPLINE' ) THEN
                        CALL CHR_ITOC( XFITPOLY / 2 - 7, REF_STR4,
     :                       NCHAR4 )
                        TITLE = TITLE( :CHR_LEN( TITLE ) ) //
     :                          ', knots=' // REF_STR4( :NCHAR4 )

                     ELSE
                        CALL CHR_ITOC( XFITPOLY - 1, REF_STR4,
     :                       NCHAR4 )
                        TITLE = TITLE( :CHR_LEN( TITLE ) ) //
     :                          ', degree=' // REF_STR4( :NCHAR4 )
                     END IF
                     IF ( .NOT. TRACE_PLOT ) THEN
                        IF ( PLOTTING ) THEN
                           CALL ECH_PLOT_GRAPH( COUNT, X_GRAPH,
     :                          Y_GRAPH, 1.0, FLOAT( NX ), 0.0,
     :                          0.0, 'X pixel ',  'Deviation ',
     :                          TITLE, 0.0, 0.0, OPTIONS, '+',
     :                          STATUS )
                        END IF

                     ELSE
                        CALL ECH_FEVAL( LFITTER, XFITPOLY,
     :                       TEMP_COEFFS, COUNT, X_GRAPH, Y_GRAPH,
     :                       STATUS )
                        IF ( PLOTTING ) THEN
                           OPTIONS = GRPH_SET_COLOUR
                           CALL ECH_PLOT_GRAPH( COUNT, X_GRAPH,
     :                          Y_GRAPH, 1.0, FLOAT( NX ), 0.0,
     :                          0.0, 'RED', ' ', ' ', 0.0, 1.0,
     :                          OPTIONS, 'LINES', STATUS )
                           OPTIONS = GRPH_CALC_YMINMAX
                           CALL ECH_PLOT_GRAPH( COUNT, X_GRAPH,
     :                          Y_GRAPH, 1.0, FLOAT( NX ), 0.0,
     :                          0.0, 'X pixels', 'Sky intensity',
     :                          TITLE, 0.0, 1.05, OPTIONS,
     :                          'LINES', STATUS )
                        END IF
                        COUNT = 0
                        DO I = 1, NYFIT
                           IF ( YFIT( I ) .NE. ECH__BAD_REAL )
     :                          THEN
                              COUNT = COUNT + 1
                              X_GRAPH( COUNT ) = XDATA( I )
                              Y_GRAPH( COUNT ) = YDATA( I )
                           END IF
                        END DO
                        IF ( PLOTTING ) THEN
                           OPTIONS = GRPH_OVERLAY + GRPH_SET_COLOUR
                           CALL ECH_PLOT_GRAPH( COUNT, X_GRAPH,
     :                          Y_GRAPH, 0.0, 0.0, 0.0, 0.0,
     :                          'BLACK', ' ', ' ', 0.0, 0.0,
     :                          OPTIONS, 'POINTS', STATUS )
                           OPTIONS = GRPH_SET_COLOUR
                           CALL ECH_PLOT_GRAPH( COUNT, X_GRAPH,
     :                          Y_GRAPH, 0.0, 0.0, 0.0, 0.0,
     :                          'BLACK', ' ', ' ', 0.0, 0.0,
     :                          OPTIONS, 'LINES', STATUS )
                        END IF
                     END IF
                  END IF
                  IF ( ACCEPTED .AND. .NOT. INTERACTIVE ) GO TO 333

*              Clip some more points from fit.
                  IF ( .NOT. ACCEPTED ) THEN
                     CALL ECH_CLIP_TRACE( COUNT, ORDER_NUMBER,
     :                    MAXIMUM_POLY, LFITTER, INTERACTIVE,
     :                    TRACE_PLOT, END_CLIP_MAXDEV,
     :                    AUTO_CLIP_BY, XFITPOLY, CLIPPED, YDATA,
     :                    YDEVIA, PLOTTING, .TRUE., ABANDONED,
     :                    ACCEPTED, STATUS )
                  END IF
                  IF ( STATUS .EQ. ECH__ABORT_OPTION ) THEN
                     CALL ECH_REPORT( 0,
     :                    ' Fitting aborted by user request.' )
                     STATUS = 0
                  END IF
                  IF ( ABANDONED ) GO TO 999
                  IF ( ACCEPTED .AND. .NOT. INTERACTIVE ) GO TO 222
                  NYFIT = 0
                  DO IX = 1, COUNT
                     IF ( YDEVIA( IX ) .NE. ECH__BAD_REAL ) THEN
                        NYFIT = NYFIT + 1
                        XDATA( NYFIT ) = INT( XDATA( IX ) )
                        YDATA( NYFIT ) = YDATA( IX )
                     END IF
                  END DO
               END DO
            END IF
 333        CONTINUE

            DO IWAVE = 1, NX
               XDATA( IWAVE ) = REAL( IWAVE )
            END DO
            CALL ECH_FEVAL( LFITTER, XFITPOLY, TEMP_COEFFS,
     :           NX, XDATA, YFIT, STATUS )
            INC_MEAN = 0.0
            INC_MEANSQ = 0.0
            INC_COUNT = 0
            DO I = 1, NX
               XCO = I
               YCOORD = INT( Y_TRACE_COORD( XCO ) + 0.5 ) + IOFF
               IF ( YCOORD .GT. 0 .AND. YCOORD .LE. NY ) THEN
                  IF ( GOT_QUALITY ) THEN
                     IF ( QUALITY( XCO, YCOORD ) .NE. 0 ) THEN
                        GO TO 400
                     END IF
                  END IF
                  IF ( DATA( XCO, YCOORD ) .EQ. ECH__BAD_REAL ) THEN
                     GO TO 400
                  END IF
                  INC_COUNT = INC_COUNT + 1
                  INC_VAL = ABS( DATA( XCO, YCOORD ) - YFIT( I ) )
                  X_GRAPH( I ) = XCO
                  Y_GRAPH( I ) = INC_VAL
                  YDEVIA( I ) = ABS( INC_VAL )
                  YSIGMA( I ) = ABS( INC_VAL )
                  INC_MEAN = INC_MEAN + INC_VAL
                  INC_MEANSQ = INC_MEANSQ + INC_VAL * INC_VAL
  400             CONTINUE
               END IF
            END DO
            IF ( INC_COUNT .EQ. 0 ) INC_COUNT = 1
            INC_MEAN = INC_MEAN / FLOAT( INC_COUNT )
            INC_SIGMA = SQRT( MAX( 0.0, INC_MEANSQ /
     :            FLOAT( INC_COUNT ) - INC_MEAN * INC_MEAN ) )

*        Set initial sky model to fitted values.
            DO I = 1, NX
               XCO = I
               YCOORD = INT( Y_TRACE_COORD( XCO ) + 0.5 ) + IOFF
               IF ( YCOORD .GT. 0 .AND. YCOORD .LE. NY ) THEN

*              If deviation of the fit from the observed value
*              is less than line_thresh rms deviation then set
*              model to fitted value.
                  IF ( LINE_THRESH .GT. 100.0 .OR.
     :                 YDEVIA( I ) .LE. LINE_THRESH *
     :                 SQRT( MAX( 0.0, YFIT( I ) ) ) ) THEN
                     SKY_MODEL( XCO, IOFF ) = YFIT( I )
                     SKY_MODEL_ERR( XCO, IOFF ) =
     :                               YSIGMA( I ) * YSIGMA( I )

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
               END IF
            END DO
         END DO
      END IF

*  Fit sky increment by increment.
*  Adapted from PAMELA routine 'FITsky'.
      THRLO = - ABS( THRESH )
      THRHI = ABS( THRESH )

*  Find number of sky pixels available for fit.
      NPIX = 0
      DO IPROFILE = DEK_BELOW, DEK_ABOVE
         IF ( SKY_MASK( IPROFILE ) .GT. 0 ) NPIX = NPIX + 1
      END DO
      IF ( NPIX .LT. SKY_NPOLY ) THEN
         CALL ECH_REPORT( 0, ' Not enough sky pixels for fit.' )
         RETURN
      END IF

*  Initialise counters etc.
      NPIXTOT = 0
      SKYREJTOT = 0
      NFITS = 0
      RMSAVG = 0.0
      NSOME = 0

*  Loop through increments in wavelength direction.
      DO IWAVE = 1, NX

*     Load sky data into arrays for poly-fit at this increment.
         NCOUNT = 0
         NPIX = 0
         AVGSKY = 0
         AVGSKY2 = 0
         AVGVAR = 0
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
         DO IPROFILE = IOFFST, IOFFEN
            IF ( SKY_MASK( IPROFILE ) .EQ. 1 ) THEN
               IF ( GOT_QUALITY ) THEN
                  IF ( QUALITY( IWAVE, YCOORD ) .NE. 0 ) THEN
                     GO TO 500
                  END IF
               END IF
               IF ( DATA( IWAVE, YCOORD ) .EQ. ECH__BAD_REAL ) THEN
                  GO TO 500
               END IF

               NPIX = NPIX + 1
               XDATA( NPIX ) = REAL( IPROFILE ) /
     :                         REAL( DEK_ABOVE - DEK_BELOW )
               IF ( GOT_BALANCE ) THEN
                  BALANCE( NPIX ) = FLAT_MODEL( IWAVE, IPROFILE )
                  IF ( BALANCE( NPIX ) .LE. 0.0 ) THEN
                     BALANCE( NPIX ) = 1.0
                  END IF

               ELSE
                  BALANCE( NPIX ) = 1.0
               END IF

*           If we have a fitted value from a x polynomial fit the
*           we can use the modeled variance.
               IF ( SKY_MODEL( IWAVE, IPROFILE ) .GT. 0.0 ) THEN
                  SKYDATA( NPIX ) = SKY_MODEL( IWAVE, IPROFILE )
                  SKYSIGMA( NPIX ) =
     :                  SKY_MODEL_ERR( IWAVE, IPROFILE )
                  ASKY = SKYDATA( NPIX )

*           Otherwise we have to just estimate variance according
*           to 'root N' statistics.  These variances are used to
*           weight the per increment fits.

               ELSE
                  SKYDATA( NPIX ) = BALANCE( NPIX ) *
     :                              DATA( IWAVE, YCOORD )
                  ASKY = SKYDATA( NPIX )

*              On first cycle, weights are corrected by CMIN factor
*              from above to prevent excessive weight being given to
*              low count pixels.  For low signal photon counting
*              data this gives almost uniform weights, but it allows
*              lower weights on CCD data.

*              If user has provided variance frame along with
*              the data then use those values.
                  IF ( .NOT. NO_ERRORS ) THEN
                     IF ( ERRORS( IWAVE,YCOORD ) .GT. 0.0 ) THEN
                        SKYSIGMA( NPIX ) = SQRT( MAX( 0.0,
     :                        BALANCE( NPIX ) * BALANCE( NPIX ) *
     :                        ( VAR0 + MAX(
     :                        ERRORS( IWAVE, YCOORD ) *
     :                        ERRORS( IWAVE, YCOORD ), CMIN ) /
     :                        PHOTON ) ) )
                     ELSE
                        SKYSIGMA( NPIX ) = SQRT( MAX( 0.0,
     :                       ( BALANCE( NPIX ) * BALANCE( NPIX ) *
     :                       ( VAR0 + MAX(
     :                       DATA( IWAVE, YCOORD ), CMIN ) /
     :                       PHOTON ) ) ) )
                     END IF

*              Otherwise calculate a variance using the actual value.
                  ELSE
                     SKYSIGMA( NPIX ) = SQRT( MAX( 0.0,
     :                   ( BALANCE( NPIX ) * BALANCE( NPIX ) *
     :                   ( VAR0 + MAX( DATA( IWAVE, YCOORD ),
     :                   CMIN ) / PHOTON ) ) ) )
                  END IF
               END IF
               NCOUNT = NCOUNT + NINT( ASKY )
               AVGSKY = AVGSKY + ASKY
               AVGSKY2 = AVGSKY2 + ASKY * ASKY
  500          CONTINUE
            END IF
            YCOORD = YCOORD + 1
         END DO
         NPIXTOT = NPIXTOT + NPIX
         AVGSKY = AVGSKY / FLOAT( MAX( 1, NPIX ) )
         AVGSKY2 = AVGSKY2 / FLOAT( MAX( 1, NPIX ) )
         AVGVAR = ABS( AVGSKY2 - AVGSKY * AVGSKY )

*     Pixel reject cycle and refining the variances
*     If no counts then skip all this.  One extra fit is
*     forced to use the improved variance estimates.
         IF ( NCOUNT .GT. 0 ) THEN
            IF ( SKY_NPOLY .GT. 0 ) THEN
               NSOME = NSOME + 1
               ICYCLE = 0
               IREJ = 1
               DO WHILE ( ICYCLE .LE. 1 .OR.
     :            ( IREJ .GT. 0 .AND. ICYCLE .LE. SKYREJ ) )
                  ICYCLE = ICYCLE + 1

*              Fit polynomial by least squares.
                  WORK_STRING = 'REAL-' // LFITTER
                  CALL ECH_FITTER( WORK_STRING, SKY_NPOLY,
     :                 TEMP_COEFFS, NPIX, XDATA, SKYDATA,
     :                 SKYSIGMA, 0, THRHI, STATUS )
                  CALL ECH_FEVAL( LFITTER, SKY_NPOLY, TEMP_COEFFS,
     :                 NPIX, XDATA, SKYFIT, STATUS )
                  NFITS = NFITS + 1

*              Revise sigma estimates using the polynomial sky model
*              First estimate total counts and therefore a floor to
*              the variance.  This prevents overweighting taking into
*              account the uncertainty in the polynomial fit.
*              Only do this if a pixel has been rejected from
*              previous cycle.
                  IF ( IREJ .GT. 0 ) THEN
                     TOTAL = 0.0
                     PIX = 0.0
                     DO K = 1, NPIX
                        IF ( SKYSIGMA( K ) .GT. 0.0 ) THEN
                           TOTAL = TOTAL + SKYDATA( K )
                           PIX = PIX + 1.0
                        END IF
                     END DO
                     IF ( PIX .EQ. 0.0 ) PIX = 1.0
                     VMIN = ALPHA * BETA / ( BETA - 1 ) *
     :                   SQRT( MAX( 0.0,
     :                         REAL( SKY_NPOLY ) / PIX ) )
     :                *  SQRT( MAX( 0.0,
     :                         VAR0 + TOTAL / ( PIX * PHOTON ) ) )
     :                         - VAR0 * PHOTON
                     RMS = 0.0
                     SKYMEAN = 0.0
                     NFIT = 0
                     DO K = 1, NPIX
                        IF ( SKYSIGMA( K ) .GT. 0. ) THEN
                           NFIT = NFIT + 1
                           VARI = BALANCE( K ) *
     :                            ( BALANCE( K ) * VAR0 +
     :                            MAX( VMIN,
     :                            ABS( SKYFIT( K ) ) ) /
     :                            PHOTON )
                           SKYSIGMA( K ) = SQRT( MAX( 0.0,VARI ) )
                           SKYMEAN = SKYMEAN + SKYFIT( K )
                           ETA = ( SKYDATA( K ) -
     :                           SKYFIT( K ) ) / MAX( 1.0,
     :                           SKYSIGMA( K ) )
                           RMS = RMS + ETA * ETA

                        ELSE
                           VARI = BALANCE( K ) * (
     :                            BALANCE( K ) * VAR0 +
     :                            MAX( VMIN, ABS( SKYFIT( K ) ) )
     :                            / PHOTON )
                           SKYSIGMA( K ) = - ABS( SQRT(
     :                                     MAX( 0.0, VARI ) ) )
                        END IF
                     END DO
                     ndf = nfit - sky_npoly
                     IF ( nfit .GT. 0 ) THEN
                        SKYMEAN = SKYMEAN / REAL( NFIT )
                        RMS = SQRT( MAX( 0.0, RMS /
     :                          REAL( MAX( 1, NDF ) ) ) )
                        IF ( NDF .LE. 1 ) THEN
                           RMS = 1.0
                           NDF = 1
                        END IF

*                    Apply KDH scaling.
                        PROB = EXP( REAL( -NDF ) *
     :                            ( RMS - 1 ) ** 2 / 4.0 )
                        RMS = PROB + ( 1.0 - PROB ) * RMS

                     ELSE
                        CALL CHR_ITOC( IWAVE, REF_STR1, NCHAR1 )
                        REPORT_STRING = ' All pixels rejected' //
     :                        ' at wavelength ' //
     :                        REF_STR1( :NCHAR1 ) // '.'
                        CALL ECH_REPORT( 0, REPORT_STRING )
                        SKY_SPECTRUM( IWAVE ) = ECH__BAD_REAL
                        SKY_VARIANCE( IWAVE ) = ECH__BAD_REAL
                        GO TO 998
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
                              RATIO = CHANCE( SKYDATA( K ),
     :                                SKYFIT( K ), SIG, THRLO,
     :                                THRHI )

*   CHANCE returns the Probability of getting as bad as the observed
*   value divided by the probabilty of getting as bad as the threshold.
*   (ONly for values less than 1 (i.e. rejects), otherwise = -1
                              IF ( RATIO .GT. -0.5 .AND.
     :                             RATIO .LT. RMIN ) THEN
                                 RMIN = RATIO
                                 IREJ = K
                              END IF
                           END IF
                        END DO
                        IF ( IREJ .GT. 0 ) THEN
                           SKYSIGMA( IREJ ) =
     :                                -ABS( SKYSIGMA( IREJ ) )
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
                  IF ( SKY_MODEL( IWAVE, IPROFILE ) .EQ. 0.0 ) THEN
                     RDUMMY( 1 ) = REAL( IPROFILE ) /
     :                     REAL( DEK_ABOVE - DEK_BELOW )
                     CALL ECH_FEVAL( LFITTER, SKY_NPOLY,
     :                    TEMP_COEFFS, 1, RDUMMY,
     :                    SKY_MODEL( IWAVE, IPROFILE ), STATUS )
                     SKY_MODEL_ERR( IWAVE, IPROFILE ) = 0.0
                  END IF
                  SKYMEAN = SKYMEAN + SKY_MODEL( IWAVE, IPROFILE )
                  SKYVAR = SKYVAR + SPVAR
               END DO
               SKYMEAN = SKYMEAN / MAX( 1, DEK_ABOVE-DEK_BELOW + 1 )
               SKYVAR = SKYVAR / MAX( 1, DEK_ABOVE-DEK_BELOW + 1 )
               SKY_SPECTRUM( IWAVE ) = AVGSKY
               SKY_VARIANCE( IWAVE ) = AVGVAR

*           Report pixel rejects.
               IF ( MOD( IWAVE, 200 ) .EQ. 0 ) THEN
                  CALL CHR_ITOC( IWAVE, REF_STR1, NCHAR1 )
                  CALL CHR_RTOC( FLOAT( INT( SKYMEAN * 1000.0 ) ) /
     :                 1000.0, REF_STR2, NCHAR2 )
                  CALL CHR_RTOC( FLOAT( INT( RMS * 1000.0 ) ) / 1000.0,
     :                 REF_STR3, NCHAR3 )
                  CALL CHR_ITOC( SKYREJTOT, REF_STR4, NCHAR4 )
                  CALL CHR_RTOC( 100.0 * REAL( SKYREJTOT ) /
     :                 MAX( 1.0, REAL( NPIXTOT ) ), REF_STR5,
     :                 NCHAR5 )
                  REPORT_STRING = ' At X=' //
     :                  REF_STR1( :NCHAR1 ) // ' sky=' //
     :                  REF_STR2( :NCHAR2 ) // ', RMS=' //
     :                  REF_STR3( :NCHAR3 ) // ', ' //
     :                  REF_STR4( :NCHAR4 ) // ' rejects=' //
     :                  REF_STR5( :NCHAR5 ) // '% of total.'
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
                  CALL CHR_RTOC( FLOAT( INT( AVGSKY * 1000.0 ) ) / 1000,
     :                 REF_STR2, NCHAR2 )
                  REPORT_STRING = ' At X=' //
     :                  REF_STR1( :NCHAR1 ) // ' sky=' //
     :                  REF_STR2( :NCHAR2 ) // '.'
                  CALL ECH_REPORT( 0, REPORT_STRING )
               END IF
            END IF
         END IF
  998    CONTINUE
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
     :         REF_STR3( :NCHAR3 ) // '% of total.'
         CALL ECH_REPORT( 0, REPORT_STRING )
      END IF

  999 CONTINUE

      END
