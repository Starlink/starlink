      SUBROUTINE ECH_FIT_ORDER_BLAZE(
     :           NX,
     :           NY,
     :           IMAGE,
     :           QUALITY,
     :           ORDER_NUMBER,
     :           DEK_BELOW,
     :           DEK_ABOVE,
     :           MAXIMUM_POLY,
     :           FITTER,
     :           N_POLY,
     :           POLYNOMIAL,
     :           INTERACTIVE,
     :           FINAL_DEV,
     :           BLAZE,
     :           X_TRACE_COORD,
     :           Y_TRACE_COORD,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_FIT_ORDER_BLAZE

*  Purpose:
*     Fit function to order blaze (with clipping).

*  Description :
*     This routine attempts to fit a polynomial to the blaze of an order.
*     A variety of interactive/automatic clipping options are available
*     to help obtain a 'good' fit.  Options are all selected by a single
*     character stroke, a carriage return is not necessary.

*     The actual fitting is handled by the general-purpose routine
*     ECH_CLIP_TRACE.

*  Invocation:
*     CALL ECH_FIT_ORDER_BLAZE(
*     :    NX,
*     :    NY,
*     :    IMAGE,
*     :    QUALITY,
*     :    ORDER_NUMBER,
*     :    DEK_BELOW,
*     :    DEK_ABOVE,
*     :    MAXIMUM_POLY,
*     :    FITTER,
*     :    N_POLY,
*     :    POLYNOMIAL,
*     :    INTERACTIVE,
*     :    FINAL_DEV,
*     :    BLAZE,
*     :    X_TRACE_COORD,
*     :    Y_TRACE_COORD,
*     :    STATUS
*     :   )

*  Arguments:
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     IMAGE = REAL (Given)
*        Input image of dimensions NX columns by NY rows.
*     QUALITY = LOGICAL (Given)
*        Input data quality flags array.
*     ORDER_NUMBER = INTEGER (Given)
*        Number of order being processed.
*     DEK_BELOW = INTEGER (Given)
*        Extent of dekker below order trace.
*     DEK_ABOVE = INTEGER (Given)
*        Extent of dekker above order trace.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum degree of polynomial to attempt to fit.
*     FITTER = CHAR (Given)
*        Type of wavelength fitting function in use (POLY/SPLINE).
*     N_POLY = INTEGER (Given)
*        Maximum degree of polynomial to attempt to fit.
*     POLYNOMIAL = DOUBLE (Given)
*        Array of Polynomial coefficients (constant first).
*     INTERACTIVE = LOGICAL (Given)
*        TRUE if interactive clipping is to be used.
*     FINAL_DEV = REAL (Given)
*        Array of accepted deviations from polynomial fit.
*     BLAZE = REAL (Given)
*        Array of NX estimates of blaze intensity in each column.
*     X_TRACE_COORD = DOUBLE (Temporary Workspace)
*        Workspace array for X points to be fitted.
*     Y_TRACE_COORD = DOUBLE (Temporary Workspace)
*        Workspace array for Y points to be fitted.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     24-NOV-1994 (DMILLS):
*       Fixed bug causing crash when first order had been
*       disabled by user.  Action was to move the statement
*       `n_poly = poly_degree' inside the main IF-THEN clause.
*     17-APR-1997 (MJC):
*       Tidied up.  Fixed problem with spurious zeros in the
*       blaze spectrum.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

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
      REAL IMAGE( NX, NY )
      BYTE QUALITY( NX, NY )
      INTEGER ORDER_NUMBER  ! Lowest one on the image frame.
      INTEGER MAXIMUM_POLY
      INTEGER N_POLY
      CHARACTER*( * ) FITTER
      LOGICAL INTERACTIVE
      INTEGER AUTO_CLIP_BY
      INTEGER DEK_BELOW
      INTEGER DEK_ABOVE

*  Arguments Returned:
      REAL BLAZE( NX )
      REAL FINAL_DEV( NX )
      DOUBLE PRECISION POLYNOMIAL( MAXIMUM_POLY )

*  Workspace:
      DOUBLE PRECISION X_TRACE_COORD( NX )
      DOUBLE PRECISION Y_TRACE_COORD( NX )

*  Status:
      INTEGER STATUS

*  Local Variables:
      DOUBLE PRECISION TEMP_COEFFS( MAX_FIT_COEFFS )

      REAL X_GRAPH( 5000 )
      REAL Y_GRAPH( 5000 )
      REAL RVALUE( 2 )
      REAL MAXBLAZE
      REAL MAX_DEVIATION
      REAL RMS_DEVIATION
      REAL END_CLIP_MAXDEV
      REAL YFRAC
      REAL DATUM
      REAL LIMVAL
      REAL VMAX
      REAL THRHI

      INTEGER I
      INTEGER CLIPPED
      INTEGER COUNT
      INTEGER POLY_DEGREE
      INTEGER POINTS_TO_FIT
      INTEGER OPTIONS
      INTEGER IX
      INTEGER IY
      INTEGER III
      INTEGER NCHAR1
      INTEGER NCHAR2
      INTEGER NCHAR3
      INTEGER ICOUNT
      INTEGER START

      LOGICAL ACCEPTED
      LOGICAL ABANDONED
      LOGICAL PLOTTING
      LOGICAL TRACE_PLOT
      LOGICAL MENU

      CHARACTER*64 TITLE
      CHARACTER*8 REF_STR1
      CHARACTER*8 REF_STR2
      CHARACTER*8 REF_STR3

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
      INTEGER CHR_LEN
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Get blaze estimate.
      AUTO_CLIP_BY = MAX( 1, NX / 50 )
      TRACE_PLOT = .FALSE.
      THRHI = 5.0
      PLOTTING = .TRUE.
      MENU = .TRUE.

*  If this order is not active (signified by BAD polynomial(1)) then do nowt.
      IF ( POLYNOMIAL( 1 ) .EQ. ECH__BAD_DOUBLE ) THEN
         GO TO 999
      END IF

*  Calculate the order trace.
      CALL ECH_CALC_TRACE( NX, MAXIMUM_POLY, POLYNOMIAL,
     :     X_TRACE_COORD, Y_TRACE_COORD, STATUS )

*  Get blaze estimates.
      DO IX = 1, NX
         COUNT = 0
         VMAX = 0.0

*     Find fractional part of this pixel coord. in Y.
         YFRAC = REAL( Y_TRACE_COORD( IX ) ) -
     :           FLOAT( INT( Y_TRACE_COORD( IX ) + 0.5 ) )

*     Find Y-coord for first point.
         IY = INT( Y_TRACE_COORD( IX ) + 0.5 ) + DEK_BELOW - 1

*     Loop over extraction slit.
         DO III = DEK_BELOW, DEK_ABOVE
            IY = IY + 1
            IF ( IY .GT. 1 .AND. IY .LT. NY ) THEN
               IF ( QUALITY( IX, IY ) .EQ. 0 .AND.
     :              IMAGE( IX, IY ) .NE. ECH__BAD_REAL ) THEN

*              Use weighted mean of appropriate pixels.
                  IF ( YFRAC .GT. 0.0 .AND.
     :                 QUALITY( IX, IY + 1 ) .EQ. 0 .AND.
     :                 IMAGE( IX, IY + 1 ) .NE. ECH__BAD_REAL ) THEN
                     DATUM = IMAGE( IX, IY + 1 ) * YFRAC +
     :                       IMAGE( IX, IY ) * ( 1.0 - YFRAC )

                  ELSE IF ( QUALITY( IX, IY - 1 ) .EQ. 0 .AND.
     :                      IMAGE( IX, IY - 1 ) .NE.
     :                      ECH__BAD_REAL ) THEN
                     DATUM = IMAGE( IX, IY - 1 ) * ( - YFRAC ) +
     :                       IMAGE( IX, IY ) * ( 1.0 + YFRAC )

*              Set flag to indicate no value available at this point.
                  ELSE
                     DATUM = ECH__BAD_REAL
                  END IF

                  IF ( DATUM .NE. ECH__BAD_REAL ) THEN
                     COUNT = COUNT + 1
                     FINAL_DEV( COUNT ) = DATUM
                     IF ( DATUM .GT. VMAX ) THEN
                        VMAX = DATUM
                     END IF
                  END IF
               END IF
            END IF
         END DO
         ICOUNT = 0
         DO III = 1, COUNT
            IF ( FINAL_DEV( III ) .GT. VMAX * 0.9 ) THEN
               ICOUNT = ICOUNT + 1
               FINAL_DEV( ICOUNT ) = FINAL_DEV( III )
            END IF
         END DO
         COUNT = ICOUNT
         IF ( COUNT .GT. 0 ) THEN
            CALL ECH_MEAN_MEDIAN( COUNT, FINAL_DEV, .TRUE.,
     :           .FALSE., BLAZE( IX ), STATUS )

         ELSE
            BLAZE( IX ) = ECH__BAD_REAL
         END IF
      END DO

      ACCEPTED = .FALSE.
      ABANDONED = .FALSE.
      CLIPPED = 0
      POLY_DEGREE = N_POLY

*  Loop determining the polynomial coefficients until OK.
      DO WHILE ( .NOT. ( ACCEPTED .OR. ABANDONED ) )

*     Copy any good points' coordinates into work arrays for fitting.
         POINTS_TO_FIT = 0
         DO I = 1, NX
            IF ( BLAZE( I ) .GT. 0.0 ) THEN
               POINTS_TO_FIT = POINTS_TO_FIT + 1
               X_TRACE_COORD( POINTS_TO_FIT ) = DBLE( I )
               Y_TRACE_COORD( POINTS_TO_FIT ) = DBLE( BLAZE( I ) )
               Y_GRAPH( POINTS_TO_FIT ) = SQRT( BLAZE( I ) )
            END IF
         END DO

*     Do the polynomial fitting.
         IF ( POINTS_TO_FIT .GT. N_POLY ) THEN
            IF ( FITTER .EQ. 'MEDIAN' ) THEN
               CONTINUE

            ELSE
               CALL ECH_FITTER( FITTER, POLY_DEGREE, TEMP_COEFFS,
     :              POINTS_TO_FIT, X_TRACE_COORD, Y_TRACE_COORD,
     :              Y_GRAPH, 0, THRHI, STATUS )
            END IF

         ELSE
            ABANDONED = .TRUE.
            CALL ECH_REPORT( 0,
     :           ' Rejecting fit: not enough samples left to fit.' )
         END IF

*     If not abandoned due to lack of fitable points then.
         IF ( .NOT. ABANDONED ) THEN

*        Calculate deviations from fit.
            MAX_DEVIATION = 0.0
            RMS_DEVIATION = 0.0
            COUNT = 0
            DO I = 1, NX
               IF ( BLAZE( I ) .NE. ECH__BAD_REAL ) THEN
                  COUNT = COUNT + 1
                  X_TRACE_COORD( I ) = FLOAT( I )
                  X_GRAPH( COUNT ) = FLOAT( I )
                  IF ( FITTER .EQ. 'MEDIAN' ) THEN
                     START = I - POLY_DEGREE / 2
                     START = MAX( 1, START )
                     START = MIN( NX - POLY_DEGREE - 1, START )
                     CALL ECH_MEAN_MEDIAN( POLY_DEGREE,
     :                    BLAZE( START ), .TRUE., .FALSE., RVALUE,
     :                    STATUS )

                  ELSE
                     CALL ECH_FEVAL( FITTER, POLY_DEGREE,
     :                    TEMP_COEFFS, 1, X_GRAPH( COUNT ), RVALUE,
     :                    STATUS )
                  END IF
                  FINAL_DEV( I ) = BLAZE( I ) - RVALUE( 1 )
                  Y_GRAPH( COUNT ) = FINAL_DEV( I )
                  IF ( ABS( FINAL_DEV( I ) ) .GT. MAX_DEVIATION ) THEN
                     MAX_DEVIATION = ABS( FINAL_DEV( I ) )
                  END IF
                  IF ( FINAL_DEV( I ) .GT. 1E10 ) THEN
                     RMS_DEVIATION = 1E10

                  ELSE
                     RMS_DEVIATION = RMS_DEVIATION +
     :                     FINAL_DEV( I ) * FINAL_DEV( I )
                  END IF

               ELSE
                  FINAL_DEV( I ) = ECH__BAD_REAL
               END IF
            END DO

*        Determine RMS, max deviation, set acceptance if fit good enough.
            RMS_DEVIATION = SQRT( RMS_DEVIATION / FLOAT( COUNT + 1 ) )
            LIMVAL = REAL( SQRT( ABS( Y_TRACE_COORD( NX / 2 ) ) ) )
            END_CLIP_MAXDEV = MIN( 3.0 * RMS_DEVIATION, LIMVAL )
            IF ( MAX_DEVIATION .LE. END_CLIP_MAXDEV ) ACCEPTED = .TRUE.

*        Plot if enabled or automatically accepted.
            IF ( ( INTERACTIVE .OR. ACCEPTED ) .AND.
     :           POINTS_TO_FIT .GT. N_POLY ) THEN
               OPTIONS = GRPH_CALC_YMINMAX
               CALL CHR_ITOC( ORDER_NUMBER, REF_STR1, NCHAR1 )
               CALL CHR_ITOC( POINTS_TO_FIT, REF_STR2, NCHAR2 )
               CALL CHR_ITOC( CLIPPED, REF_STR3, NCHAR3 )
               TITLE = 'Order ' // REF_STR1( :NCHAR1 ) //
     :              ': samples=' // REF_STR2( :NCHAR2 ) //
     :              ', clipped=' // REF_STR3( :NCHAR3 )
               IF ( FITTER .EQ. 'SPLINE' ) THEN
                  CALL CHR_ITOC( POLY_DEGREE / 2 - 7, REF_STR3, NCHAR3 )
                  TITLE = TITLE( :CHR_LEN( TITLE ) ) //
     :                    ', knots=' // REF_STR3( :NCHAR3 )

               ELSE IF ( FITTER .EQ. 'MEDIAN' ) THEN
                  CALL CHR_ITOC( POLY_DEGREE, REF_STR3, NCHAR3 )
                  TITLE = TITLE( :CHR_LEN( TITLE ) ) //
     :                    ', median=' // REF_STR3( :NCHAR3 )

               ELSE
                  CALL CHR_ITOC( POLY_DEGREE - 1, REF_STR3, NCHAR3 )
                  TITLE = TITLE( :CHR_LEN( TITLE ) ) //
     :                    ', degree=' // REF_STR3( :NCHAR3 )
               ENDIF

               IF ( .NOT. TRACE_PLOT ) THEN
                  IF ( PLOTTING )
     :               CALL ECH_PLOT_GRAPH( POINTS_TO_FIT, X_GRAPH,
     :                    Y_GRAPH, 1.0, FLOAT( NX ), 0.0, 0.0,
     :                    'X pixel', 'Deviation', TITLE, 0.0, 0.0,
     :                    OPTIONS, '+', STATUS )

               ELSE
                  COUNT = 0
                  DO IX = 1, NX, MAX( 1, NX / 200 )
                     COUNT = COUNT + 1
                     X_GRAPH( COUNT ) = FLOAT( IX )
                  END DO
                  IF ( FITTER .EQ. 'MEDIAN' ) THEN
                     DO IX = 1, COUNT
                        START = INT( X_GRAPH( IX ) ) - POLY_DEGREE / 2
                        START = MAX( 1, START )
                        START = MIN( NX - POLY_DEGREE, START )
                        CALL ECH_MEAN_MEDIAN( POLY_DEGREE,
     :                       BLAZE( START ), .TRUE., .FALSE.,
     :                       Y_GRAPH( IX ), STATUS )
                     END DO

                  ELSE
                     CALL ECH_FEVAL( FITTER, POLY_DEGREE,
     :                    TEMP_COEFFS, COUNT, X_GRAPH, Y_GRAPH,
     :                    STATUS )
                  END IF
                  IF ( PLOTTING ) THEN
                     OPTIONS = GRPH_SET_COLOUR
                     CALL ECH_PLOT_GRAPH( COUNT, X_GRAPH, Y_GRAPH,
     :                    1.0, FLOAT( NX ), 0.0, 0.0, 'RED',
     :                    ' ', ' ', 0.0, 1.0, OPTIONS, 'LINES',
     :                    STATUS )
                     OPTIONS = GRPH_CALC_YMINMAX
                     CALL ECH_PLOT_GRAPH( COUNT, X_GRAPH, Y_GRAPH,
     :                    1.0, FLOAT( NX ), 0.0, 0.0, 'X pixels',
     :                    'Median intensity', TITLE,
     :                    0.0, 1.05, OPTIONS, 'LINES', STATUS )
                  END IF
                  COUNT = 0
                  DO I = 1, NX
                     IF ( BLAZE( I ) .NE. ECH__BAD_REAL ) THEN
                        COUNT = COUNT + 1
                        X_GRAPH( COUNT ) = FLOAT( I )
                        Y_GRAPH( COUNT ) = BLAZE( I )
                     END IF
                  END DO
                  IF ( PLOTTING ) THEN
                     OPTIONS = GRPH_OVERLAY + GRPH_SET_COLOUR
                     CALL ECH_PLOT_GRAPH( COUNT, X_GRAPH, Y_GRAPH,
     :                    0.0, 0.0, 0.0, 0.0, 'BLACK', ' ', ' ', 0.0,
     :                    0.0, OPTIONS, 'POINTS', STATUS )
                     OPTIONS = GRPH_SET_COLOUR
                     CALL ECH_PLOT_GRAPH( COUNT, X_GRAPH, Y_GRAPH,
     :                    1.0, FLOAT( NX ), 0.0, 0.0, 'BLACK',
     :                    ' ', ' ', 0.0, 1.0, OPTIONS, 'LINES',
     :                    STATUS )
                  END IF
               END IF
            END IF

*        Clip some more points from blaze fit.
            CALL ECH_CLIP_TRACE( NX, ORDER_NUMBER, MAXIMUM_POLY,
     :           FITTER, INTERACTIVE, TRACE_PLOT, END_CLIP_MAXDEV,
     :           AUTO_CLIP_BY, POLY_DEGREE, CLIPPED, BLAZE, FINAL_DEV,
     :           PLOTTING, MENU, ABANDONED, ACCEPTED, STATUS )
         END IF
      END DO

*  Save resulting blaze spectrum.
      IF ( ACCEPTED .AND. .NOT. ABANDONED ) THEN
         DO IX = 1, NX
            X_GRAPH( IX ) = FLOAT( IX )
         END DO
         IF ( FITTER .EQ. 'MEDIAN' ) THEN
            DO IX = 1, NX
               START = IX - POLY_DEGREE / 2
               START = MAX( 1, START )
               START = MIN( NX - POLY_DEGREE, START )
               CALL ECH_MEAN_MEDIAN( POLY_DEGREE, BLAZE( START ),
     :              .TRUE., .FALSE., Y_GRAPH( IX ), STATUS )
            END DO
            DO IX = 1, NX
               START = IX - POLY_DEGREE / 4
               START = MAX( 1, START )
               START = MIN( NX - POLY_DEGREE / 2, START )
               CALL ECH_MEAN_MEDIAN( POLY_DEGREE / 2, Y_GRAPH( START ),
     :              .FALSE., .FALSE., BLAZE( IX ), STATUS )
            END DO

         ELSE
            CALL ECH_FEVAL( FITTER, POLY_DEGREE, TEMP_COEFFS,
     :                      NX, X_GRAPH, BLAZE, STATUS )
         END IF
         CALL CHR_ITOC( ORDER_NUMBER, REF_STR1, NCHAR1 )
         CALL CHR_RTOC( MAX_DEVIATION, REF_STR2, NCHAR2 )
         CALL CHR_RTOC( RMS_DEVIATION, REF_STR3, NCHAR3 )
         REPORT_STRING = ' Order ' //
     :         REF_STR1( :NCHAR1 ) // ', blaze fit residuals: max=' //
     :         REF_STR2( :NCHAR2 ) // ' RMS=' //
     :         REF_STR3( :NCHAR3 ) // '.'
         CALL ECH_REPORT( 0, REPORT_STRING )

      ELSE IF ( ABANDONED .AND. .NOT. ACCEPTED ) THEN
         DO IX = 1, NX
            BLAZE( IX ) = 1.0
         END DO
         CALL ECH_REPORT( 0, ' Blaze function set to unity.' )
      END IF
      IF ( STATUS .EQ. ECH__ABORT_OPTION ) THEN
         CALL ECH_REPORT( 0, ' Fitting aborted by user request.' )
         STATUS = 0
      END IF

*  Scale blaze spectrum by its median.
      MAXBLAZE = 0.0
      DO IX = 1, NX
         IF ( BLAZE( IX ) .GT. MAXBLAZE ) MAXBLAZE = BLAZE( IX )
      END DO
      IF ( MAXBLAZE .GT. 0.0 ) THEN
         DO IX = 1, NX
            BLAZE( IX ) = BLAZE( IX ) / MAXBLAZE
         END DO

      ELSE
         DO IX = 1, NX
            BLAZE( IX ) = 1.0
         END DO
         CALL ECH_REPORT( 0,
     :        ' Blaze function set to unity.' )
      END IF
      N_POLY = POLY_DEGREE

      CALL CHR_RTOC( MAXBLAZE, REF_STR1, NCHAR1 )
      REPORT_STRING = ' Blaze normalised by division by ' //
     :      REF_STR1( : NCHAR1 ) // '.'
      CALL ECH_REPORT( 0, REPORT_STRING )

  999 CONTINUE

      END
