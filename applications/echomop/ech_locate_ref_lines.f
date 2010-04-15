      SUBROUTINE ECH_LOCATE_REF_LINES(
     :           NX,
     :           NY,
     :           N_ORDERS,
     :           MAXIMUM_POLY,
     :           TRACE_POLYNOMIAL,
     :           MAX_PERM_LINES,
     :           REF_LINE_THRESH,
     :           LINE_WID,
     :           LINE_POS,
     :           LINE_INTEN,
     :           EXTRACTED_REF,
     :           X_TRACE_COORD,
     :           Y_TRACE_COORD,
     :           REF_SPECTRUM,
     :           AVG_SPECTRUM,
     :           CONTINUUM,
     :           LINES,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_LOCATE_REF_LINES

*  Purpose:
*     Locate arc line candidates.

*  Description:
*     This routine locates (to the nearest X pixel), reference lines
*     to be used for wavelength calibration purposes.  Generally the
*     reference spectrum will be an ARC lamp spectrum, but in principle
*     this routine could more generally be used with a sky-line rich
*     spectrum, or even an object with strong emission lines (subject
*     to user provision of a suitable line list).  In the normal (ARC-
*     lamp) case, this routine will be called once for each data frame.
*     Usually there will be two ARC lamp frames (bracketing the object
*     exposure).

*  Invocation:
*     CALL ECH_LOCATE_REF_LINES(
*     :    NX,
*     :    NY,
*     :    N_ORDERS,
*     :    MAXIMUM_POLY,
*     :    TRACE_POLYNOMIAL,
*     :    MAX_PERM_LINES,
*     :    REF_LINE_THRESH,
*     :    LINE_WID,
*     :    LINE_POS,
*     :    LINE_INTEN,
*     :    EXTRACTED_REF,
*     :    X_TRACE_COORD,
*     :    Y_TRACE_COORD,
*     :    REF_SPECTRUM,
*     :    AVG_SPECTRUM,
*     :    CONTINUUM,
*     :    LINES,
*     :    STATUS
*     :   )

*  Arguments:
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     N_ORDERS = INTEGER (Given)
*        Number of orders in echellogram.
*     TRACE_POLYNOMIAL = DOUBLE (Given)
*        Coefficients of fit describing order path across frame.
*     MAX_PERM_LINES = INTEGER (Given)
*        Maximum number of lines per order to allow.
*     LINE_WID = REAL (Given)
*        Approximate line width in pixels.
*     LINE_POS = REAL (Returned)
*        X position of lines (peak value).
*     LINE_INTEN = REAL (Returned)
*        Average Peak intensity of lines.
*     EXTRACTED_REF = REAL (Returned)
*        Estimated reference spectrum.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum number of fit coefficients allowed.
*     REF_LINE_THRESH = FLOAT (Given)
*        Reference arc line threshold for identification.
*     X_TRACE_COORD = DOUBLE (Temporary Workspace)
*        X coords of order trace path.
*     Y_TRACE_COORD = DOUBLE (Temporary Workspace)
*        Y coords of order trace path.
*     REF_SPECTRUM = now (Given and Returned)
*        Arc line spectrum.
*     AVG_SPECTRUM = REAL (Temporary Workspace)
*        Workspace for spectrum averaging.
*     CONTINUUM = REAL (Given and Returned)
*        Estimated continuum intensity.
*     LINES = FLOAT (Given)
*        Arc lines.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

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
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'
      INCLUDE 'ECH_ENVIRONMENT.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      INTEGER N_ORDERS
      INTEGER MAXIMUM_POLY
      DOUBLE PRECISION TRACE_POLYNOMIAL( MAXIMUM_POLY, N_ORDERS )
      INTEGER MAX_PERM_LINES
      REAL REF_LINE_THRESH
      REAL LINE_WID
      REAL EXTRACTED_REF( NX, N_ORDERS )
      REAL LINES( NX )

*  Arguments Returned:
      REAL LINE_POS( MAX_PERM_LINES, N_ORDERS )
      REAL LINE_INTEN( MAX_PERM_LINES, N_ORDERS )

*  Workspace:
      REAL REF_SPECTRUM( NX )
      REAL CONTINUUM( NX )
      DOUBLE PRECISION X_TRACE_COORD( NX )
      DOUBLE PRECISION Y_TRACE_COORD( NX )
      REAL AVG_SPECTRUM( NX )

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL LEFT
      REAL RIGHT
      REAL TOTAL
      REAL PEAK_VALUE
      REAL XV
      REAL YV

      INTEGER ILINE_WID
      INTEGER I
      INTEGER IORD
      INTEGER LINE_COUNT
      INTEGER II
      INTEGER III
      INTEGER IV
      INTEGER X_AT_PEAK
      INTEGER IPOS
      INTEGER OPTIONS
      INTEGER NCHAR1
      INTEGER NCHAR2

      LOGICAL FOUND
      LOGICAL TOO_MANY

      CHARACTER*32 TITLE
      CHARACTER*8 REF_STR1
      CHARACTER*8 REF_STR2

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

      ILINE_WID = MAX( INT( LINE_WID ), 1 )
      TOO_MANY = .FALSE.

*  Loop through each of the orders in turn.
      DO IORD = 1, N_ORDERS

*     If a good trace polynomial is avaliable for this order.
         IF ( TRACE_POLYNOMIAL( 1, IORD ) .NE. ECH__BAD_DOUBLE ) THEN

*        Calculate the order trace.
            CALL ECH_CALC_TRACE( NX, MAXIMUM_POLY,
     :           TRACE_POLYNOMIAL( 1, IORD ), X_TRACE_COORD,
     :           Y_TRACE_COORD, STATUS )

*        Clear result and workspace arrays.
            DO I = 1, MAX_PERM_LINES
               LINE_POS( I, IORD ) = 0.0
               LINE_INTEN( I, IORD ) = 0.0
            END DO
            LINE_COUNT = 0
            DO I = 1, NX
               REF_SPECTRUM( I ) = 0.0
               CONTINUUM( I ) = 0.0
            END DO

*        Average central (trace +-1 Y-pixel) pixels for all X.
*        Loop through all X increments (following order trace).
            DO I = 1, NX
               REF_SPECTRUM( I ) = EXTRACTED_REF( I, IORD )
            END DO

*        Loop from 1 pixel below trace to 1 pixel above trace
*        If pixel is within frame limits then add value to average
*        Calc average of the 3 pixels sampled

*        Copy estimated reference spectrum into reduction object if empty.

*        Estimate continuum level of line spectrum by locating all local
*        minima (over 5 pixels)
*        Loop through reference spectrum.
            II = 0
            DO I = 3, NX - 2

*           If a local minimum at this pixel then store its value.
               IF ( ( REF_SPECTRUM( I - 2 ) .GE. REF_SPECTRUM( I - 1 )
     :              .OR. ILINE_WID .LT. 2 ) .AND.
     :              REF_SPECTRUM( I - 1 ) .GE. REF_SPECTRUM( I ) .AND.
     :              REF_SPECTRUM( I + 1 ) .GE. REF_SPECTRUM( I ) .AND.
     :              ( REF_SPECTRUM( I + 2 ) .GE. REF_SPECTRUM( I + 1 )
     :              .OR. ILINE_WID .LT. 2 ) ) THEN
                  CONTINUUM( I ) = REF_SPECTRUM( I )

*           Else store a flag.
               ELSE
                  CONTINUUM( I ) = ECH__BAD_REAL
               END IF
            END DO

*        Extend edge values for either end of the continuum.
            CONTINUUM( 1 ) = CONTINUUM( 3 )
            CONTINUUM( 2 ) = CONTINUUM( 3 )
            CONTINUUM( NX - 1 ) = CONTINUUM( NX - 2 )
            CONTINUUM( NX ) = CONTINUUM( NX - 2 )

*        Set pointer to beginning of continuum array.
            II = 2

*        Loop until end of array reached.
            DO WHILE ( II .LT. NX )

*           If current continuum entry is a FLAG then.
               IF ( CONTINUUM( II ) .EQ. ECH__BAD_REAL ) THEN

*              Remember start point and continue stepping through
*              continuum array until a non-FLAG value is reached.
                  III = II
                  DO WHILE ( CONTINUUM( III ) .EQ. ECH__BAD_REAL .AND.
     :                       III .LT. NX )
                     III = III + 1
                  END DO

*              Replace FLAG value by their nearest non-FLAG neighbour in
*              the continuum array.
                  LEFT = CONTINUUM( II - 1 )
                  RIGHT = CONTINUUM( III )
                  IF ( LEFT .EQ. ECH__BAD_REAL ) LEFT = RIGHT
                  IF ( RIGHT .EQ. ECH__BAD_REAL ) RIGHT = LEFT
                  DO IV = II, III - 1
                     IF ( IV .GT. ( III + II ) / 2 ) THEN
                        CONTINUUM( IV ) = RIGHT

                     ELSE
                        CONTINUUM( IV ) = LEFT
                     END IF
                  END DO
                  II = III

*           Else leave value as it is, and step to next entry.
               ELSE
                  II = II + 1
               END IF
            END DO

*        Smooth the continuum array with a 3 pixel average, replacing
*        any values which exceed the original sample value, with
*        the original sample. Repeat process ten times.
            CONTINUUM( 1 ) = CONTINUUM( 3 )
            CONTINUUM( 2 ) = CONTINUUM( 3 )
            CONTINUUM( NX - 1 ) = CONTINUUM( NX - 2 )
            CONTINUUM( NX ) = CONTINUUM( NX - 2 )
            DO I = 1, 10
               AVG_SPECTRUM( 1 ) = CONTINUUM( 1 )
               AVG_SPECTRUM( NX ) = CONTINUUM( NX )
               DO II = 2, NX - 1
                  AVG_SPECTRUM( II ) = ( CONTINUUM( II - 1 ) +
     :                  CONTINUUM( II ) + CONTINUUM( II + 1 ) ) / 3.0
               END DO
               DO II = 1, NX
                  IF ( AVG_SPECTRUM( II ) .LT. REF_SPECTRUM( II ) ) THEN
                     CONTINUUM( II ) = AVG_SPECTRUM( II )

                  ELSE
                     CONTINUUM( II ) = REF_SPECTRUM( II )
                  END IF
               END DO
            END DO

*        Multiply by reference threshold factor, continuum then holds
*        the level at each pixel which any line candidate must exceed.
            DO II = 1, NX
               CONTINUUM( II ) = CONTINUUM( II ) * REF_LINE_THRESH
            END DO

*        We now have a fair estimate of the underlying continuum of the
*        reference line spectrum. This is now used as a base when
*        calculating the intensities of individual lines.
*        Plot the spectrum, and the continuum.
            IF ( DIAGNOSTICS_ACTIVE ) THEN
               WRITE ( TITLE, '( A, I3 )' ) 'Line reference order ',
     :               IORD
               OPTIONS = GRPH_GEN_XAXIS + GRPH_CALC_MINMAX
               CALL ECH_PLOT_GRAPH( NX, REF_SPECTRUM, REF_SPECTRUM,
     :              0.0, 0.0, 0.0, 0.0, 'X PIXELS', 'SUM', TITLE,
     :              0.0, 1.2, OPTIONS, 'BINS', STATUS )
               OPTIONS = GRPH_GEN_XAXIS + GRPH_OVERLAY + GRPH_SET_COLOUR
               CALL ECH_PLOT_GRAPH( NX, CONTINUUM, CONTINUUM,
     :              0.0, 0.0, 0.0, 0.0, 'RED', ' ', ' ',
     :              0.0, 0.0, OPTIONS, 'BINS', STATUS )
            END IF

*        Smooth line spectrum in the X-direction by a 3-pixel average
*        This helps the algorithm in cases where the lines are undersampled
*        (i.e. all line intensity is in a single pixel), and doesn't affect
*        the estimate of line position which is only to the nearest pixel
*        at this stage.
            DO I = 2, NX - 1
               TOTAL = 0.0
               DO II = -1, 1
                  TOTAL = TOTAL + REF_SPECTRUM( I + II )
               END DO
               AVG_SPECTRUM( I ) = TOTAL / 3.0
            END DO
            DO I = 1, NX
               REF_SPECTRUM( I ) = AVG_SPECTRUM( I )
            END DO

*        Set FLAGs array, FLAG replaced by intensity when a line peak is
*        suspected.
            DO I = 1, NX
               LINES( I ) = ECH__BAD_REAL
            END DO
            REF_SPECTRUM( 1 ) = ECH__BAD_REAL
            REF_SPECTRUM( NX ) = ECH__BAD_REAL

*        Loop repeatedly locating maximum peak and replacing it (and its
*        neighbours) with a flag, until no more peaks can be found.
            ILINE_WID = MAX( INT( LINE_WID ), 1 )
            FOUND = .TRUE.
            DO WHILE ( FOUND )
               FOUND = .FALSE.
               PEAK_VALUE = 0.0

*           Loop through spectrum in X (ignoring end few pixels).
               DO I = ILINE_WID + 2, NX - ILINE_WID - 2

*              If no flagged values in local set of 5 then.
                  IF ( REF_SPECTRUM( I ) .NE. ECH__BAD_REAL .AND.
     :                 ( REF_SPECTRUM( I - 2 ) .NE. ECH__BAD_REAL .OR.
     :                   ILINE_WID .LT. 2 ) .AND.
     :                 ( REF_SPECTRUM( I + 2 ) .NE. ECH__BAD_REAL .OR.
     :                   ILINE_WID .LT. 2 ) .AND.
     :                 REF_SPECTRUM( I - 1 ) .NE. ECH__BAD_REAL .AND.
     :                 REF_SPECTRUM( I + 1 ) .NE. ECH__BAD_REAL ) THEN

*                 If local values form a smooth peaked set then.
                     IF ( REF_SPECTRUM( I ) .GT.
     :                    REF_SPECTRUM( I - 1 ) .AND.
     :                    REF_SPECTRUM( I ) .GT.
     :                    REF_SPECTRUM( I + 1 ) .AND.
     :                    ( REF_SPECTRUM( I + 1 ) .GT.
     :                      REF_SPECTRUM( I + 2 ) .OR.
     :                      ILINE_WID .LT. 2 ) .AND.
     :                    ( REF_SPECTRUM( I - 1 ) .GT.
     :                      REF_SPECTRUM( I - 2 ) .OR.
     :                      ILINE_WID .LT. 2 ) ) THEN

*                    If highest peak so far, record it.
                        IF ( REF_SPECTRUM( I ) .GT. PEAK_VALUE ) THEN
                           PEAK_VALUE = REF_SPECTRUM( I )
                           X_AT_PEAK = I
                        END IF
                     END IF
                  END IF
               END DO

*           If a peak has been located.
               IF ( PEAK_VALUE .GT. 0.0 ) THEN
                  FOUND = .TRUE.
                  LINE_COUNT = LINE_COUNT + 1
                  IF ( LINE_COUNT .LT. MAX_PERM_LINES ) THEN

*                 Loop through neighbouring entries, flagging them as 'used'.
                     DO II = -ILINE_WID, ILINE_WID
                        IF ( II .NE. 0 ) THEN
                           REF_SPECTRUM( X_AT_PEAK + II ) =
     :                           ECH__BAD_REAL

                        ELSE
                           LINES( X_AT_PEAK ) =
     :                           REF_SPECTRUM( X_AT_PEAK )
                           REF_SPECTRUM( X_AT_PEAK ) = ECH__BAD_REAL
                        END IF
                     END DO

                  ELSE
                     DO II = -ILINE_WID, ILINE_WID
                        REF_SPECTRUM( X_AT_PEAK + II ) = ECH__BAD_REAL
                     END DO
                  END IF
               END IF
            END DO

*        Loop through flag/values recording the remaining values,
*        each one being a local peak as established above.
            LINE_COUNT = 0
            DO I = ILINE_WID + 2, NX - ILINE_WID - 2

*           If entry is not a flag.
               IF ( LINES( I ) .NE. ECH__BAD_REAL ) THEN

*              If value exceeds line/continuum avg_spectrum (user tunable).
                  IF ( LINES( I ) .GT. CONTINUUM( I )  ) THEN
                     IF ( LINE_COUNT .LT. MAX_PERM_LINES - 1 ) THEN

*                    Increment located line counter, and save its
*                    position/intensity.
                        LINE_COUNT = LINE_COUNT + 1
                        LINE_POS( LINE_COUNT, IORD ) = I
                        LINE_INTEN( LINE_COUNT, IORD ) = LINES( I )

                     ELSE IF ( .NOT. TOO_MANY ) THEN
                        TOO_MANY = .TRUE.
                        CALL ECH_REPORT( 0, ' Too many line ' //
     :                       'candidates found, some will be ' //
     :                       'ignored.' )
                        CALL ECH_REPORT( 0, 'Use TUNE_MXRFLN to ' //
     :                       'increase the number allowed.' )
                     END IF
                  END IF
               END IF
            END DO

*        Fit gaussians to all candidates, rejecting the badly fitting ones.
            CALL ECH_FIT_REF_LINES( EXTRACTED_REF( 1, IORD ), NX,
     :           MAX_PERM_LINES, LINE_WID, LINE_POS( 1, IORD ),
     :           LINE_INTEN( 1, IORD ), CONTINUUM, IORD, STATUS )

*        Count number of successfully fitted lines.
            LINE_COUNT = 0
            DO WHILE ( LINE_POS( LINE_COUNT + 1, IORD ) .GT. 0.0 )
               LINE_COUNT = LINE_COUNT + 1
            END DO

*        Add positional indicators to graph for line candidates.
            IF ( DIAGNOSTICS_ACTIVE ) THEN
               OPTIONS = GRPH_OVERLAY
               DO II = 1, LINE_COUNT
                  WRITE ( TITLE, 1002 ) LINE_POS( II, IORD )
                  XV = LINE_POS( II, IORD ) + 5.0
                  IPOS = INT( LINE_POS( II, IORD ) + 0.5 )
                  YV = MAX( LINES( IPOS - 1 ), LINES( IPOS ),
     :                  LINES( IPOS + 1 ) )
                  CALL ECH_PLOT_GRAPH( 1, XV, YV, 0.0, 0.0, 0.0, 0.0,
     :                 ' ', ' ', TITLE( :10 ), 0.0, 0.0, OPTIONS,
     :                 'TEXT', STATUS )
               END DO
            END IF

*        Report number of lines found in this order.
            CALL CHR_ITOC( IORD, REF_STR1, NCHAR1 )
            CALL CHR_ITOC( LINE_COUNT, REF_STR2, NCHAR2 )
            REPORT_STRING = ' Order ' // REF_STR1( :NCHAR1 ) //
     :            ': located ' // REF_STR2( :NCHAR2 ) //
     :            ' line candidates.'
            CALL ECH_REPORT( 0, REPORT_STRING )
         END IF
      END DO

 1002 FORMAT ( 1X, ' - -', F6.0 )

      END
