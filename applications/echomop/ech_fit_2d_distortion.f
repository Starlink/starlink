      SUBROUTINE ECH_FIT_2D_DISTORTION(
     :           NX,
     :           NY,
     :           IMAGE,
     :           INTERACTIVE,
     :           DEK_BELOW,
     :           DEK_ABOVE,
     :           ORDER_NUMBER,
     :           MAXIMUM_POLY,
     :           TRACE_POLYNOMIAL,
     :           WAVE_COEFFS,
     :           REF_LINE_FWHM,
     :           MAX_PERM_FTRS,
     :           IDENTIFIED_FTRS,
     :           IDEN_FTR_POSITION,
     :           IDEN_FTR_WAVELENGTH,
     :           INP_W2_NXPOLY,
     :           INP_W2_NYPOLY,
     :           W_POLY_2D,
     :           X_TRACE_COORD,
     :           Y_TRACE_COORD,
     :           DX_COORD,
     :           DY_COORD,
     :           F_OF_XY,
     :           FINAL_DEV,
     :           FITTED_F_OF_XY,
     :           XV,
     :           WEIGHTS,
     :           LINE_CENTERS,
     :           WORK,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_FIT_2D_DISTORTION

*  Purpose:
*     Fit 2-D polynomials to arc line shapes.

*  Description :
*     This routine fits a 2-D polynomial surface to the distortion present
*     in an echelle order.  The order trace is used as an origin axis; the
*     X-distance along it and Y-offset from it are the coordinates of the
*     fitted points.  The fitted quantity is the delta-wavelength of an
*     identified arc line.  The identified lines' centres are located
*     at all offsets above and below the trace.  These centres are all
*     assigned the identified wavelength for the line, and then their
*     offset (in wavelength) relative to the same line's position on the
*     origin axis calculated.  This provides the following dataset for input
*     in the surface fit:
*
*          X - coord of a line centre
*          Y - Offset from trace of the line centre
*          Difference in wavelength from 1-D fit prediction for coord X
*
*     Thus if no 2-D correction were necessary, all the Differences would
*     be zero.  Formulating the fit in this way allows maximum significance
*     of the quantity of interest.
*
*     Once  a fit has been obtained, the deviations from it of all sample
*     points (line-centres) are calculated.  Theses are then either
*     interactively or automatically clipped until an acceptable fit is
*     obtained.

*  Invocation:
*     CALL ECH_FIT_2D_DISTORTION(
*     :    NX,
*     :    NY,
*     :    IMAGE,
*     :    INTERACTIVE,
*     :    DEK_BELOW,
*     :    DEK_ABOVE,
*     :    ORDER_NUMBER,
*     :    MAXIMUM_POLY,
*     :    TRACE_POLYNOMIAL,
*     :    WAVE_COEFFS,
*     :    REF_LINE_FWHM,
*     :    MAX_PERM_FTRS,
*     :    IDENTIFIED_FTRS,
*     :    IDEN_FTR_POSITION,
*     :    IDEN_FTR_WAVELENGTH,
*     :    INP_W2_NXPOLY,
*     :    INP_W2_NYPOLY,
*     :    W_POLY_2D,
*     :    X_TRACE_COORD,
*     :    Y_TRACE_COORD,
*     :    DX_COORD,
*     :    DY_COORD,
*     :    F_OF_XY,
*     :    FINAL_DEV,
*     :    FITTED_F_OF_XY,
*     :    XV,
*     :    WEIGHTS,
*     :    LINE_CENTERS,
*     :    WORK,
*     :    STATUS
*     :   )

*  Arguments:
*     IMAGE = REAL (Given)
*        Input frame image of dimensions nx columns and ny rows.
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     END_CLIP_MAXDEV = REAL (Given)
*        Maximum deviation for clippable points.
*     AUTO_CLIP_BY = INTEGER (Given)
*        Number of points to auto clip before a re-fit.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum order of wavelength polynomial fit.
*     IDENTIFIED_FTRS = INTEGER (Given)
*        Count of identified features.
*     IDEN_FTR_POSITION = REAL (Given)
*        Positions identified features.
*     IDEN_FTR_WAVELENGTH = REAL (Given)
*        Wavelengths of identified features.
*     INTERACTIVE = LOGICAL (Given)
*        TRUE if interactive mode is used.
*     DEK_BELOW = INTEGER (Given)
*        Extent of dekker below order trace.
*     DEK_ABOVE = INTEGER (Given)
*        Extent of dekker above order trace.
*     ORDER_NUMBER = INTEGER (Given)
*        Number of order being processed.
*     TRACE_POLYNOMIAL = DOUBLE (Given)
*        Coefficients of fit describing order path across frame.
*     WAVE_COEFFS = DOUBLE (Given)
*        Wavelength polynomials for each order.
*     REF_LINE_FWHM = FLOAT (Given)
*        Arc line average fwhm in pixels.
*     MAX_PERM_FTRS = INTEGER (Given)
*        Maximum number of features per order.
*     INP_W2_NXPOLY = INTEGER (Given)
*        Number of X polynomial coefficients.
*     INP_W2_NYPOLY = INTEGER (Given)
*        Number of Y polynomial coefficients.
*     W_POLY_2D = DOUBLE (Given and Returned)
*        2-D chebyshev polynomial coefficients.
*     X_TRACE_COORD = DOUBLE (Temporary Workspace)
*        X coords of order trace path.
*     Y_TRACE_COORD = DOUBLE (Temporary Workspace)
*        Y coords of order trace path.
*     DX_COORD = REAL (Given and Returned)
*        X coords if fitted data.
*     DY_COORD = REAL (Given and Returned)
*        Y coords of fitted data.
*     F_OF_XY = DOUBLE (Given and Returned)
*        Dataset to be fitted (wavelength differences).
*     FINAL_DEV = REAL (Given and Returned)
*        Array of accepted deviations from polynomial fit.
*     FITTED_F_OF_XY = DOUBLE (Given and Returned)
*        Fitted values (wavlength differences) output by evaluating fit.
*     XV = REAL (Given and Returned)
*        X values.
*     WEIGHTS = REAL (Given)
*        The weights for the.
*     LINE_CENTERS = REAL (Given and Returned)
*        Centre positions of arc line profiles.
*     WORK = DOUBLE (Temporary Workspace)
*
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     If any identified features then
*        Copy polynomial degrees to local variables in case they are altered
*        Generate coordinates of order trace
*        Determine order of wavelength 1d polynomial
*        Loop through each 'line' located and identified
*           Determine line pixel-coords
*           Loop from lower dekker to upper dekker pixel by pixel
*              If y coord is on image then
*                Loop Sampling line-width+5 values around estimated line position
*                  If lowest value so far, remember it
*                End loop
*                Copy local values into data array for fitting
*                Try to fit a gaussian of width line_width to the data
*                If too far from first guess, reject it
*                If we have obtained at least a marginal fit, then
*              Endif
*           End loop
*           Normalise the centroids around the mean value
*        End loop
*        Compose list a points to be fitted in 2D fit
*        Loop until all points sorted into increasing order
*         Loop through all points
*           If pair of data triplets needs sorting then
*             Exchange the pairs of data X,Y,F(x,y)
*             Remember that at least one swap was made
*           Endif
*         End loop
*        End loop
*        Remove any 'bad' data points
*        Loop clipping and re-fitting
*           Remove any clipped points ( weights set to -ve values )
*           Remove all data for lines with Y=n for which there are less
*           than w2_nxpoly+2 contributing data points.
*           Fill the ancilliary arrays required by the surface fitting
*           routines.
*           Auto-reduce y degree of fit if necessary
*           If still at least a one fifth of the original set if points remaines then
*             Fit a 2D Chebyshev to the surface
*             Evaluate fit by calculating the residuals
*             Calculate deviations from fit
*             Determine RMS, max deviation, set acceptance if fit good enough
*             Plot if enabled or automatically accepted
*             Clip some more points from trace
*           Else
*              Abandon this orders' fit
*           Endif
*        End loop
*     Else
*        Report non event
*     Endif

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_DATA_CONSTRAINTS.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'
      INCLUDE 'ECH_ENVIRONMENT.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      REAL IMAGE( NX, NY )
      INTEGER MAXIMUM_POLY
      INTEGER MAX_PERM_FTRS
      DOUBLE PRECISION WAVE_COEFFS( MAXIMUM_POLY )
*          ! Polynomial coefficients for 1-D fit.
      DOUBLE PRECISION TRACE_POLYNOMIAL( MAXIMUM_POLY )
*          ! Polynomial coefficients of order trace.
      INTEGER IDENTIFIED_FTRS
      REAL REF_LINE_FWHM
      REAL END_CLIP_MAXDEV
      INTEGER AUTO_CLIP_BY
      INTEGER INP_W2_NXPOLY
      INTEGER INP_W2_NYPOLY
      INTEGER ORDER_NUMBER
      LOGICAL INTERACTIVE
      INTEGER DEK_BELOW
      INTEGER DEK_ABOVE
      DOUBLE PRECISION WEIGHTS( MAX_PERM_FTRS * MAX_SLICE_PIXELS )
*          ! Weights for fitted points.

*  Arguments Returned:
      DOUBLE PRECISION W_POLY_2D( MAXIMUM_POLY * MAXIMUM_POLY )
      REAL IDEN_FTR_POSITION( MAX_PERM_FTRS )
*          ! Positions of identified features.
      REAL IDEN_FTR_WAVELENGTH( MAX_PERM_FTRS )
*          ! Wavelengths of identified features.

*  Workspace:
      DOUBLE PRECISION DX_COORD( MAX_PERM_FTRS * MAX_SLICE_PIXELS )
*          ! X coordinates of points to fit.
      DOUBLE PRECISION DY_COORD( MAX_PERM_FTRS * MAX_SLICE_PIXELS )
*          ! Y offsets of points to fit.
      DOUBLE PRECISION F_OF_XY( MAX_PERM_FTRS * MAX_SLICE_PIXELS )
*          ! Value (delta-wavelength) to be fitted.
      REAL FINAL_DEV( MAX_PERM_FTRS * MAX_SLICE_PIXELS )
*          ! Deviations from fit.
      REAL XV( MAX_PERM_FTRS * MAX_SLICE_PIXELS )
*          ! Fit point index.
      DOUBLE PRECISION FITTED_F_OF_XY( MAX_PERM_FTRS*MAX_SLICE_PIXELS )
*          ! Fitted value predictions.
      REAL LINE_CENTERS( MAX_PERM_FTRS, -MAX_SLICE_PIXELS / 2:
     :                   MAX_SLICE_PIXELS / 2 )
*          ! Line centers for identified lines.
*  ECH version.
      DOUBLE PRECISION WORK( MAX_PERM_FTRS * MAX_SLICE_PIXELS * 5, 2 )
*  NAG version.
*      DOUBLE PRECISION WORK( MAX_PERM_FTRS * MAX_SLICE_PIXELS * 10 )
*          ! Workspace for NAG fitter.
      DOUBLE PRECISION X_TRACE_COORD( NX )
*          ! X coordinates of trace.
      DOUBLE PRECISION Y_TRACE_COORD( NX )
*          ! Y coordinates of trace.

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER MAX_PER_XY
      PARAMETER ( MAX_PER_XY = 1000 )

*  Local Variables:
      DOUBLE PRECISION LINE_CENTROID( -MAX_SLICE_PIXELS / 2 :
     :     MAX_SLICE_PIXELS / 2 )
*          ! Line centres at all offsets.
      DOUBLE PRECISION Y_LINE( MAX_PER_XY )
      DOUBLE PRECISION XMIN( MAX_PER_XY )
      DOUBLE PRECISION XMAX( MAX_PER_XY )
      DOUBLE PRECISION AXMIN( 2 )
      DOUBLE PRECISION AXMAX( 2 )
      DOUBLE PRECISION R8_TEMP
      DOUBLE PRECISION MEAN_CENTROID

      REAL SAMPLE( -MAX_SLICE_PIXELS/2 : MAX_SLICE_PIXELS/2 )
*          ! Y-Averaged samples of spectrum.
      REAL X2V( 5000 )
      REAL Y2V( 5000 )
      REAL YCENT( 100 )
      REAL CHANS( MAX_ALLOWED_RF_FEAT )   ! X coords for 1-D fit.
      REAL WAVES( MAX_ALLOWED_RF_FEAT )   ! Wavelengths for 1-D fit.
      REAL FIT_DATA( MAX_SLICE_PIXELS )   ! Line data to centre.
      REAL FIT_ERROR( MAX_SLICE_PIXELS )  ! Errors on fit_data.
      REAL FIT_PARS( 15 )                 ! Gaussian parameters.
      REAL FIT_VAR( 15 )                  ! Gaussian variances.
      REAL R4_TEMP
      REAL VALUE
      REAL PEAK_VALUE
      REAL X_AT_MAX
      REAL LOCAL_MIN
      REAL MAX_DEVIATION
      REAL RMS_DEVIATION

      INTEGER POINTS_PER_LINE( MAX_PER_XY ) ! Number of points for each Y.
      INTEGER DEGREE( 2 ) ! Order of fits in X and Y for fit routine.
      INTEGER I
      INTEGER II
      INTEGER YOFF
      INTEGER AVERAGING_EXTENT
      INTEGER MAX_COEFFS
      INTEGER IX
      INTEGER IY
      INTEGER istat
      INTEGER W2_NXPOLY
      INTEGER W2_NYPOLY
      INTEGER COUNT
      INTEGER ILINE_WIDTH
      INTEGER DCOUNT
      INTEGER CCOUNT
      INTEGER DUCOUNT
      INTEGER OPTIONS
      INTEGER LINES_THIS_ORDER
      INTEGER XBOX
      INTEGER Y_DELTA
      INTEGER X_DELTA
      INTEGER MAX_WORK
      INTEGER MAX_DATA_POINTS
      INTEGER LAST_Y
      INTEGER NUMBER_OF_Y_LINES
      INTEGER SURFACE_POINTS
      INTEGER IYSTART
      INTEGER IYEND
      INTEGER LOOPS
      INTEGER CLIPPED
      INTEGER POINTS_TO_FIT
      INTEGER INITIAL_SET
      INTEGER NCHAR1
      INTEGER NCHAR2
      INTEGER NCHAR3
      INTEGER NCHAR4
      INTEGER NCHAR5

      LOGICAL VALID( -MAX_SLICE_PIXELS/2 : MAX_SLICE_PIXELS/2 )
*          ! TRUE when line centre is OK.
      LOGICAL ACCEPTED
      LOGICAL ABANDONED
      LOGICAL EXCHANGED_PAIR
      LOGICAL SORTED
      LOGICAL CLIPPING
      LOGICAL MENU

      CHARACTER*64 TITLE
      CHARACTER*8 REF_STR1
      CHARACTER*8 REF_STR2
      CHARACTER*8 REF_STR3
      CHARACTER*8 REF_STR4
      CHARACTER*8 REF_STR5

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

      SURFACE_POINTS = 0
      MAX_COEFFS = MAXIMUM_POLY * MAXIMUM_POLY
      MAX_DATA_POINTS = MAX_PERM_FTRS * MAX_SLICE_PIXELS
      MAX_WORK = MAX_DATA_POINTS * 10
      AVERAGING_EXTENT = MAX( MIN( 3,
     :      ( DEK_ABOVE-DEK_BELOW + 1 ) / 8 ), 1 )

*  If no identified features, do nothing.
      IF ( IDENTIFIED_FTRS .LE. 0 ) THEN
         CALL CHR_ITOC( ORDER_NUMBER, REF_STR1, NCHAR1 )
         REPORT_STRING = ' No identified features in order ' //
     :         REF_STR1( :NCHAR1 ) // '.'
         CALL ECH_REPORT( 0, REPORT_STRING )
         GO TO 999
      END IF

*  Copy polynomial degrees to local variables in case they are altered.
      W2_NXPOLY = INP_W2_NXPOLY
      W2_NYPOLY = INP_W2_NYPOLY

*  Generate a series of X,Y coordinates using the polynomial
*  for order. The points will be seperated by 1 pixel
*  and will be located at X values of ???.5.

*  Generate coordinates of order trace.
      CALL ECH_CALC_TRACE( NX, MAXIMUM_POLY, TRACE_POLYNOMIAL,
     :     X_TRACE_COORD, Y_TRACE_COORD, STATUS )

*  Determine order of wavelength 1-D polynomial.
      ILINE_WIDTH = INT( MAX( REF_LINE_FWHM, 1.0 ) ) * 2 + 1
      XBOX = ILINE_WIDTH + 2

*  Loop through each 'line' located and identified.
      DO LINES_THIS_ORDER = 1, IDENTIFIED_FTRS

*     Resample line locality of line-width+5 pixels (in x).
*     Determine line pixel-coords.
         IX = INT( IDEN_FTR_POSITION( LINES_THIS_ORDER ) + 0.5 )
         IY = INT( Y_TRACE_COORD( IX ) + 0.5 )
         MEAN_CENTROID = 0.0
         COUNT = 0

*     Loop from lower dekker to upper dekker pixel-by-pixel.
         DO LOOPS = 1, 2
            IX = INT( IDEN_FTR_POSITION( LINES_THIS_ORDER ) + 0.5 )
            IF ( LOOPS .EQ. 1 ) THEN
               IYSTART = 0
               IYEND = DEK_ABOVE - AVERAGING_EXTENT

            ELSE
               IYSTART = DEK_BELOW + AVERAGING_EXTENT
               IYEND = -1
            ENDIF
            DO Y_DELTA = IYSTART, IYEND
               VALID ( Y_DELTA ) = .FALSE.

*           If Y-coord is on image then.
               IF ( IY + Y_DELTA .GT. 0 .AND. IY + Y_DELTA .LT. NY )
     :              THEN

*              Loop Sampling line-width+5 values around estimated
*              line position.
                  X_AT_MAX = 0.0
                  LOCAL_MIN = 1.0E20
                  DO X_DELTA = - XBOX, XBOX
                     SAMPLE( X_DELTA ) = 0.0
                     IF ( IX + X_DELTA .GT. 0 .AND.
     :                    IX + X_DELTA .LE. NX ) THEN

*                    If lowest value so far, remember it.
                        DCOUNT = 1
                        SAMPLE( X_DELTA ) =
     :                        IMAGE( IX + X_DELTA, IY + Y_DELTA )
                        DO YOFF = 1, AVERAGING_EXTENT
                           IF ( IY + Y_DELTA - YOFF .GT. 0 .AND.
     :                          IY + Y_DELTA - YOFF .LT. NY .AND.
     :                          IY + Y_DELTA + YOFF .GT. 0 .AND.
     :                          IY + Y_DELTA + YOFF .LT. NY ) THEN
                              DCOUNT = DCOUNT + 2
                              SAMPLE( X_DELTA ) =
     :                              SAMPLE( X_DELTA ) +
     :                              IMAGE( IX + X_DELTA,
     :                                     IY + Y_DELTA - YOFF ) +
     :                              IMAGE( IX + X_DELTA,
     :                                     IY + Y_DELTA + YOFF )
                           END IF
                        END DO
                        SAMPLE( X_DELTA ) = SAMPLE( X_DELTA ) /
     :                        FLOAT( DCOUNT )
                        IF ( IMAGE( IX + X_DELTA, IY + Y_DELTA ) .LT.
     :                       LOCAL_MIN )
     :                     LOCAL_MIN = SAMPLE( X_DELTA )
                     END IF
                  END DO

*              Copy local values into data array for fitting.
                  DCOUNT = 0
                  DUCOUNT = 0
                  PEAK_VALUE = -1.0E20
                  DO X_DELTA = - XBOX, XBOX
                     DCOUNT = DCOUNT + 1
                     FIT_DATA( DCOUNT ) = 0.0
                     IF ( IX + X_DELTA .GT. 0 .AND.
     :                    IX + X_DELTA .LE. NX ) THEN
                        IF ( ABS( X_DELTA ) .LE.
     :                       MAX( 3, ILINE_WIDTH ) ) THEN
                           FIT_DATA( DCOUNT ) =
     :                           SAMPLE( X_DELTA ) - LOCAL_MIN
                           FIT_ERROR( DCOUNT ) = 1.0 /
     :                           MAX( 1.0, FIT_DATA( DCOUNT ) )
                           DUCOUNT = DUCOUNT + 1
                           IF ( FIT_DATA( DCOUNT ) .GT.
     :                          PEAK_VALUE )
     :                        PEAK_VALUE = FIT_DATA( DCOUNT )
                       END IF
                    END IF
                  END DO
                  CCOUNT = ( DCOUNT + 1 ) / 2
                  PEAK_VALUE = MAX( FIT_DATA( CCOUNT - 1 ),
     :                  FIT_DATA( CCOUNT ),
     :                  FIT_DATA( CCOUNT + 1 ) )
                  DO X_DELTA = 2, XBOX
                     IF ( FIT_DATA( CCOUNT - X_DELTA ) .GT.
     :                    PEAK_VALUE )
     :                  FIT_DATA( CCOUNT - X_DELTA ) = LOCAL_MIN
                     IF ( FIT_DATA( CCOUNT + X_DELTA ) .GT.
     :                    PEAK_VALUE )
     :                  FIT_DATA( CCOUNT + X_DELTA ) = LOCAL_MIN
                  END DO

*              Try to fit a gaussian of width line_width to the data.
                  IF ( DUCOUNT .GT. 1 ) THEN
                     FIT_PARS( 1 ) =
     :                     FIT_DATA( INT ( ( DCOUNT + 1 ) / 2 ) )
                     FIT_PARS( 2 ) = FLOAT( DCOUNT + 1 ) / 2.0
                     FIT_PARS( 3 ) = 2 * REF_LINE_FWHM / 2.35
                     STATUS = 0
                     CALL ECH_FIT_GAUSSIAN( FIT_DATA, FIT_ERROR,
     :                    DCOUNT, FIT_PARS, FIT_VAR, 3, STATUS )

                  ELSE
                     STATUS = ECH__NO_DATA
                  END IF

*              If too far from first guess, reject it.
                  IF ( ABS( FLOAT( DCOUNT + 1 ) / 2.0 -
     :                 FIT_PARS( 2 ) ) .GT. 2.0 )
     :               STATUS = ECH__NO_DATA

*              If we have obtained at least a marginal fit, then.
                  IF ( STATUS .EQ. 0 .AND.
     :                 FIT_PARS( 3 ) .GT. 0.0 .AND.
     :                 FIT_PARS( 3 ) .LE. REF_LINE_FWHM * 3 ) THEN
                     VALID ( Y_DELTA ) = .TRUE.
                     X_AT_MAX = FLOAT( IX ) + FIT_PARS( 2 ) -
     :                  FLOAT( ( DCOUNT + 1 ) / 2 )
                     IX = INT( X_AT_MAX + 0.5 )
                     COUNT = COUNT + 1
                     MEAN_CENTROID = MEAN_CENTROID + X_AT_MAX
                     LINE_CENTROID( Y_DELTA ) = X_AT_MAX
                  END IF
               END IF
            END DO
         END DO

*     Normalise the centroids around the mean value.
         MEAN_CENTROID = MEAN_CENTROID / FLOAT( MAX( 1, COUNT ) )
         DO Y_DELTA = DEK_BELOW + AVERAGING_EXTENT,
     :                DEK_ABOVE - AVERAGING_EXTENT
            IF ( IY + Y_DELTA .GT. 0 .AND.
     :           IY + Y_DELTA .LT. NY ) THEN
               IF ( VALID( Y_DELTA ) .AND.
     :            ( SURFACE_POINTS .LT. MAX_DATA_POINTS ) ) THEN
                  SURFACE_POINTS = SURFACE_POINTS + 1
                  LINE_CENTERS( LINES_THIS_ORDER, Y_DELTA ) =
     :                  REAL( LINE_CENTROID( Y_DELTA ) )

               ELSE
                  LINE_CENTERS( LINES_THIS_ORDER, Y_DELTA ) = 0.0
               END IF
            END IF
         END DO
      END DO

*  Compose list of points to be used in 2-D fit.
      SURFACE_POINTS = 0
      DO Y_DELTA = DEK_BELOW + AVERAGING_EXTENT,
     :             DEK_ABOVE - AVERAGING_EXTENT
         POINTS_TO_FIT = 0

         DO II = 1, IDENTIFIED_FTRS
            IF ( LINE_CENTERS( II, Y_DELTA ) .NE. 0.0 ) THEN
               POINTS_TO_FIT = POINTS_TO_FIT + 1
               WAVES( POINTS_TO_FIT ) = IDEN_FTR_WAVELENGTH( II )
               CHANS( POINTS_TO_FIT ) = LINE_CENTERS( II, Y_DELTA )
               YCENT( POINTS_TO_FIT ) = IDEN_FTR_POSITION( II )
            END IF
         END DO
         DO I = 1, POINTS_TO_FIT
            IF ( SURFACE_POINTS .LT. MAX_DATA_POINTS ) THEN
               SURFACE_POINTS = SURFACE_POINTS + 1
               DY_COORD( SURFACE_POINTS ) = FLOAT( Y_DELTA )
               DX_COORD( SURFACE_POINTS ) = CHANS( I )
               CALL ECH_FEVAL( ' ', MAXIMUM_POLY, WAVE_COEFFS, 1,
     :              CHANS( I ), VALUE, STATUS )
               F_OF_XY( SURFACE_POINTS ) = WAVES( I ) - VALUE
               X2V( SURFACE_POINTS ) = YCENT( I ) +
     :               FLOAT( NX / 50 ) * ( CHANS( I ) - YCENT( I ) )
               Y2V( SURFACE_POINTS ) = FLOAT( Y_DELTA )
            END IF
         END DO
      END DO

*  Shell sort the three arrays containing the x,y coordinates
*  of the surface, and the height of the surface at these
*  coords (f_of_xy). The surface fitting routine expects
*  its data order primarily on increasing Y coord, with
*  embedded secondary ordering on increasing X coord.
      SORTED = .FALSE.

*  Loop until all points sorted into increasing order.
      DO WHILE ( .NOT. SORTED )

*     Loop through all points.
         EXCHANGED_PAIR = .FALSE.
         DO I = 2, SURFACE_POINTS

*        If pair of data triplets needs sorting then.
            IF ( DY_COORD( I ) .LT. DY_COORD( I - 1 ) .OR.
     :           ( DY_COORD( I ) .EQ. DY_COORD( I - 1 ) .AND.
     :           DX_COORD( I ) .LT. DX_COORD( I - 1 ) ) ) THEN

*           Exchange the pairs of data X,Y,F(x,y).
               R8_TEMP = DY_COORD( I )
               DY_COORD( I ) = DY_COORD( I - 1 )
               DY_COORD( I - 1 ) = R8_TEMP
               R8_TEMP = DX_COORD( I )
               DX_COORD( I ) = DX_COORD( I - 1 )
               DX_COORD( I - 1 ) = R8_TEMP
               R8_TEMP = F_OF_XY( I )
               F_OF_XY( I ) = F_OF_XY( I - 1 )
               F_OF_XY( I - 1 ) = R8_TEMP
               R4_TEMP = X2V( I )
               X2V( I ) = X2V( I - 1 )
               X2V( I - 1 ) = R4_TEMP
               R4_TEMP = Y2V( I )
               Y2V( I ) = Y2V( I - 1 )
               Y2V( I - 1 ) = R4_TEMP

*           Remember that at least one swap was made.
               EXCHANGED_PAIR = .TRUE.
            END IF
         END DO

         IF ( .NOT. EXCHANGED_PAIR ) SORTED = .TRUE.
      END DO

*  Remove any data flagged as unusable.
      DO I  = 1, SURFACE_POINTS
         IF ( DX_COORD( I ) .EQ. 0.0 .OR. DY_COORD( I ) .EQ. 0.0 .OR.
     :        F_OF_XY( I ) .EQ. 0.0 ) THEN
            WEIGHTS( I ) = -1.0
         END IF
      END DO
      INITIAL_SET = SURFACE_POINTS

*  Loop clipping and re-fitting.
      CLIPPED = 0
      CLIPPING = .TRUE.
      MENU = .TRUE.
      DO WHILE ( CLIPPING )

*     Remove any clipped points ( weights set to -ve values ).
         I = 1
         DO WHILE ( I .LE. SURFACE_POINTS )
            IF ( WEIGHTS( I ) .LT. 0.0 ) THEN
               DO II = I, SURFACE_POINTS - 1
                  DX_COORD( II ) = DX_COORD( II + 1 )
                  DY_COORD( II ) = DY_COORD( II + 1 )
                  F_OF_XY( II ) = F_OF_XY( II + 1 )
                  WEIGHTS( II ) = WEIGHTS( II + 1 )
                  X2V( II ) = X2V( II + 1 )
                  Y2V( II ) = Y2V( II + 1 )
               END DO
               SURFACE_POINTS = SURFACE_POINTS - 1

            ELSE
               I = I + 1
            END IF
         END DO

*     Remove all data for lines with Y=n for which there are less
*     than w2_nxpoly+2 contributing data points.
         LAST_Y = INT( DY_COORD( 1 ) )
         I = 1
         DO WHILE ( I .LE. SURFACE_POINTS )
            COUNT = 0
            DO WHILE ( INT( DY_COORD( I ) ) .EQ. LAST_Y .AND.
     :                 I .LE. SURFACE_POINTS )
               COUNT = COUNT + 1
               I = I + 1
            END DO
            IF ( COUNT .LT. W2_NXPOLY + 2 ) THEN
               DO II = I - COUNT, SURFACE_POINTS
                 DX_COORD( II ) = DX_COORD( II + COUNT )
                 DY_COORD( II ) = DY_COORD( II + COUNT )
                 F_OF_XY( II ) = F_OF_XY( II + COUNT )
                 X2V( II ) = X2V( II + COUNT )
                 Y2V( II ) = Y2V( II + COUNT )
               END DO
               SURFACE_POINTS = SURFACE_POINTS - COUNT
               I = MAX( 1, I - COUNT )
            END IF
            LAST_Y = INT( DY_COORD( I ) )
         END DO

*     Fill the ancilliary arrays required by the surface fitting
*     routines.
         NUMBER_OF_Y_LINES = 1
         Y_LINE( NUMBER_OF_Y_LINES ) = DY_COORD( 1 )
         XMIN( NUMBER_OF_Y_LINES ) = 0.0
         XMAX( NUMBER_OF_Y_LINES ) = DBLE( NX )
         POINTS_PER_LINE( NUMBER_OF_Y_LINES ) = 0
         AXMIN( 2 ) = DY_COORD( 1 )
         AXMAX( 2 ) = DY_COORD( 1 )
         DO I = 1, SURFACE_POINTS
            WEIGHTS( I ) = 1.0
            AXMIN( 2 ) = MIN( AXMIN( 2 ), DY_COORD( I ) )
            AXMAX( 2 ) = MAX( AXMAX( 2 ), DY_COORD( I ) )
            WORK( I, 1 ) = DX_COORD( I )
            WORK( I, 2 ) = DY_COORD( I )
            IF ( DY_COORD( I ) .GT. Y_LINE( NUMBER_OF_Y_LINES ) ) THEN
               NUMBER_OF_Y_LINES = NUMBER_OF_Y_LINES + 1
               POINTS_PER_LINE( NUMBER_OF_Y_LINES ) = 0
               Y_LINE( NUMBER_OF_Y_LINES ) = DY_COORD( I )
               XMIN( NUMBER_OF_Y_LINES ) = 0.0
               XMAX( NUMBER_OF_Y_LINES ) = DBLE( NX )
            END IF
            POINTS_PER_LINE( NUMBER_OF_Y_LINES ) =
     :            POINTS_PER_LINE( NUMBER_OF_Y_LINES ) + 1
         END DO

*     Auto-reduce degree of fit in Y-direction if necessary.
         IF ( NUMBER_OF_Y_LINES .LE. W2_NYPOLY + 2 ) THEN
            W2_NYPOLY = NUMBER_OF_Y_LINES - 2
            IF ( W2_NYPOLY .LT. 3 ) THEN
               CALL ECH_REPORT( 0, ' Not enough points for a 2-D fit.' )
               CALL ECH_REPORT( 0,
     :              ' More features must be identified.' )
               SURFACE_POINTS = 0

            ELSE
               CALL CHR_ITOC( W2_NYPOLY, REF_STR1, NCHAR1 )
               REPORT_STRING = ' Reducing Y-axis fit to degree ' //
     :               REF_STR1( :NCHAR1 ) // '.'
               CALL ECH_REPORT( 0, REPORT_STRING )
            END IF
         END IF

*     If still at least one fifth of the original set of
*     points remains we can still proceed.
         IF ( SURFACE_POINTS .GE. INITIAL_SET / 5 ) THEN

*        Fit a 2-D Chebyshev to the surface.
            ISTAT = 0

*        ECH version.
            DEGREE( 1 ) = W2_NXPOLY
            DEGREE( 2 ) = W2_NYPOLY
            AXMIN( 1 ) = 0.0
            AXMAX( 1 ) = DBLE( NX )
            CALL ECH_CHEF( SURFACE_POINTS, 2, F_OF_XY,
     :           WORK, AXMIN, AXMAX, DEGREE, MAX_COEFFS,
     :           W_POLY_2D, ISTAT )

*         NAG version.
*            CALL E02CAF( POINTS_PER_LINE, NUMBER_OF_Y_LINES,
*     :           W2_NXPOLY, W2_NYPOLY, DX_COORD, Y_LINE,
*     :           F_OF_XY, WEIGHTS, MAX_DATA_POINTS,
*     :           W_POLY_2D, MAX_COEFFS, XMIN,
*     :           XMAX, DY_COORD, 1, DY_COORD, 1,
*     :           WORK, MAX_WORK, ISTAT )

*        Evaluate fitted surface.
*        ECH version.
            CALL ECH_CHEV( SURFACE_POINTS, 2, WORK,
     :           AXMIN, AXMAX, DEGREE, MAX_COEFFS, W_POLY_2D,
     :           FITTED_F_OF_XY, ISTAT )

*        NAG version.
*            MI = 0
*            DO I = 1, NUMBER_OF_Y_LINES
*               ISTAT = 0
*               T = MI + 1
*               MI = MI + POINTS_PER_LINE( I )
*               YR = Y_LINE( I )
*               YMAX = Y_LINE( NUMBER_OF_Y_LINES )
*               IF ( NUMBER_OF_Y_LINES .EQ. 1 ) YMAX = YMAX + 1.0
*               CALL E02CBF( T, MI, W2_NXPOLY, W2_NYPOLY,
*     :            DX_COORD, XMIN( I ), XMAX( I ), YR,
*     :            DBLE( DEK_BELOW+AVERAGING_EXTENT ),
*     :            DBLE( DEK_ABOVE-AVERAGING_EXTENT ),
*     :            FITTED_F_OF_XY, W_POLY_2D, MAX_COEFFS,
*     :            WORK, MAX_WORK, ISTAT )
*            END DO

*        Calculate deviations from fit.
            MAX_DEVIATION = 0.0
            RMS_DEVIATION = 0.0
            DO I = 1, SURFACE_POINTS
               XV( I ) = FLOAT( I )
               FINAL_DEV( I ) = REAL(
     :               FITTED_F_OF_XY( I ) - F_OF_XY( I ) )
               IF ( ABS( FINAL_DEV( I ) ) .GT. MAX_DEVIATION )
     :            MAX_DEVIATION = ABS( FINAL_DEV( I ) )
               RMS_DEVIATION = RMS_DEVIATION +
     :               ABS( FINAL_DEV( I ) ) * ABS( FINAL_DEV( I ) )
            END DO
            COUNT = SURFACE_POINTS

*        Determine RMS, max deviation, set acceptance if fit good enough.
            RMS_DEVIATION = SQRT( RMS_DEVIATION /
     :            FLOAT( MAX(1, COUNT ) ) )
            IF ( MAX_DEVIATION .LE. END_CLIP_MAXDEV )
     :         ACCEPTED = .TRUE.

*        Plot if enabled or automatically accepted.
            IF ( ( INTERACTIVE .OR. ACCEPTED ) .AND.
     :         POINTS_TO_FIT .GT. W2_NXPOLY ) THEN
               OPTIONS = GRPH_CALC_MINMAX
               CALL CHR_ITOC( ORDER_NUMBER, REF_STR1, NCHAR1 )
               CALL CHR_ITOC( COUNT, REF_STR2, NCHAR2 )
               CALL CHR_ITOC( CLIPPED, REF_STR3, NCHAR3 )
               CALL CHR_ITOC( W2_NXPOLY, REF_STR4, NCHAR4 )
               CALL CHR_ITOC( W2_NYPOLY, REF_STR5, NCHAR5 )
               TITLE = 'Order ' // REF_STR1( :NCHAR1 ) //
     :               ': samples=' // REF_STR2( :NCHAR2 ) //
     :               ', clipped=' // REF_STR3( :NCHAR3 ) //
     :               ', degrees=' // REF_STR4( :NCHAR4 ) //
     :               ',' // REF_STR5( :NCHAR5 )
               CALL ECH_PLOT_GRAPH( COUNT, XV, FINAL_DEV,
     :              0, 0, 0, 0, '2-D distortion sample points',
     :              'Deviation (' // 'Angstroms' // ')',
*     :              'Deviation (' // WAVELENGTH_UNITS // ')',
     :              TITLE, 0, 0, OPTIONS, '+', STATUS )
            END IF

*        Clip some more points from trace.
            CALL ECH_CLIP_2D_FIT(
     :           NX, X2V, Y2V, COUNT, ORDER_NUMBER, INTERACTIVE,
     :           MENU, END_CLIP_MAXDEV, AUTO_CLIP_BY, MAXIMUM_POLY,
     :           CLIPPED, XV, FINAL_DEV, WEIGHTS, ABANDONED,
     :           ACCEPTED, W2_NXPOLY, W2_NYPOLY, STATUS )
            IF ( ACCEPTED .OR. ABANDONED ) CLIPPING = .FALSE.

*     Abandon this orders' fit.
         ELSE
            ABANDONED = .TRUE.
            ACCEPTED = .FALSE.
            CLIPPING = .FALSE.
         END IF
      END DO

  999 CONTINUE

      END
