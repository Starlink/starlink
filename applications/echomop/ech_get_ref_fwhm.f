      SUBROUTINE ECH_GET_REF_FWHM(
     :           IMAGE,
     :           NX,
     :           NY,
     :           N_ORDERS,
     :           MAXIMUM_POLY,
     :           TRACE_POLYNOMIAL,
     :           LINE_WIDTH,
     :           X_TRACE_COORD,
     :           Y_TRACE_COORD,
     :           AVG_SPECTRUM,
     :           REF_SPECTRUM,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_GET_REF_FWHM

*  Purpose:
*     Calculates average arc line FWHM in pixels.

*  Description:
*     This routine cross correlates all local peaks in all orders to create
*     an estimate of the profile of the reference lines. This profile is then
*     fitted with a simple gaussian, and the Full-Width-Half-Maximum (FWHM)
*     calculated. The FWHM is used by the reference line location and fitting
*     routines.

*  Invocation:
*     CALL ECH_GET_REF_FWHM(
*     :    IMAGE,
*     :    NX,
*     :    NY,
*     :    N_ORDERS,
*     :    MAXIMUM_POLY,
*     :    TRACE_POLYNOMIAL,
*     :    LINE_WIDTH,
*     :    X_TRACE_COORD,
*     :    Y_TRACE_COORD,
*     :    AVG_SPECTRUM,
*     :    REF_SPECTRUM,
*     :    STATUS
*     :   )

*  Arguments:
*     IMAGE = REAL (Given)
*        Input frame image of dimensions nx columns and ny rows.
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     N_ORDERS = INTEGER (Given)
*        Number of orders in echellogram.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum number of coefficients for trace polynomials.
*     TRACE_POLYNOMIAL = REAL (Given)
*        Trace polynomial coefficients.
*     LINE_WIDTH = REAL (Returned)
*        Estimated line full-width-half-max.
*     AVG_SPECTRUM = REAL (Temporary Workspace)
*        workspace for spectrum averaging.
*     X_TRACE_COORD = DOUBLE (Temporary Workspace)
*        X coords of order trace path.
*     Y_TRACE_COORD = DOUBLE (Temporary Workspace)
*        Y coords of order trace path.
*     REF_SPECTRUM = REAL (Given and Returned)
*        Arc line spectrum.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     04-JUN-1997 (MJC):
*       Added prologue, tidy-up.
*     09-JUN-1997 (MJC):
*       Added Y-pixel coordinate range checking in first loop.
*       Changed profile plot X-range to be four times estimated width
*       of profile, rather than 20 pixels.
*       Made size of internal profile array a constant parameter.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'

*  Local Constants:
      INTEGER PROF_DIM
      PARAMETER ( PROF_DIM = 100 )

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      INTEGER N_ORDERS
      INTEGER MAXIMUM_POLY
      REAL LINE_WIDTH
      DOUBLE PRECISION TRACE_POLYNOMIAL( MAXIMUM_POLY, N_ORDERS )
*           ! Trace polynomial coefficients.
      REAL IMAGE( NX, NY )

*  Workspace:
      REAL REF_SPECTRUM( NX )
      DOUBLE PRECISION X_TRACE_COORD( NX )
      DOUBLE PRECISION Y_TRACE_COORD( NX )
      REAL AVG_SPECTRUM( NX )

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL REF_PROFILE( -PROF_DIM : PROF_DIM )
      REAL XAXIS( -PROF_DIM : PROF_DIM )
      REAL HMAX_LOW_AT
      REAL HMAX_HI_AT
      REAL X_AT_PEAK
      REAL PEAK_VALUE
      REAL PROFILE_MEDIAN
      REAL XM
      REAL XH
      REAL YM
      REAL YH
      REAL MIN_VALUE

      INTEGER I
      INTEGER II
      INTEGER IGOOD
      INTEGER IORD
      INTEGER IORD_LOW
      INTEGER IORD_HI
      INTEGER NX_LOW
      INTEGER NX_HI
      INTEGER NEAREST_Y
      INTEGER Y_DELTA
      INTEGER XMI
      INTEGER X_AT_PEAKI
      INTEGER NCHAR1

      CHARACTER*50 TITLE
      CHARACTER*8 REF_STR1

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

      DO I = -PROF_DIM, PROF_DIM
         REF_PROFILE( I ) = 0.0
      END DO

*  Determine which orders to use, ignoring top/bottom if possible.
      IF ( N_ORDERS .GE. 3 ) THEN
         IORD_LOW = 2
         IORD_HI = N_ORDERS - 1
         NX_LOW = 2 * NX / 5
         NX_HI = 3 * NX / 5

      ELSE
         IORD_LOW = 1
         IORD_HI = 1
         NX_LOW = 10
         NX_HI = NX - 10
      END IF

*  Loop through each available order.
      DO IORD = IORD_LOW, IORD_HI

*     If a good trace polynomial is avaliable for this order then.
         IF ( TRACE_POLYNOMIAL( 1, IORD ) .NE. ECH__BAD_DOUBLE ) THEN

*        Calculate the order trace.
            CALL ECH_CALC_TRACE( NX, MAXIMUM_POLY,
     :           TRACE_POLYNOMIAL( 1, IORD ), X_TRACE_COORD,
     :           Y_TRACE_COORD, STATUS )

*        Initialise spectrum arrays.
            DO I = 1, NX
               AVG_SPECTRUM( I ) = 0.0
               REF_SPECTRUM( I ) = 0.0
            END DO

            MIN_VALUE = 1.0E20
            DO I = NX_LOW, NX_HI
               NEAREST_Y = INT( Y_TRACE_COORD( I ) + 0.5 )
               IF ( NEAREST_Y .GT. 0 .AND. NEAREST_Y .LE. NY ) THEN
                  IF ( IMAGE( I, NEAREST_Y ) .NE. ECH__BAD_REAL .AND.
     :                 IMAGE( I, NEAREST_Y ) .LT. MIN_VALUE ) THEN
                     MIN_VALUE = IMAGE( I, NEAREST_Y )
                  END IF
               END IF
            END DO

*        Loop through pixels on order +- 1 Y-pixel.
            DO I = NX_LOW, NX_HI

*           Calculate Y-pixel address and sample reference spectrum.
               NEAREST_Y = INT( Y_TRACE_COORD( I ) + 0.5 )
               IF ( NEAREST_Y .GT. 0 .AND. NEAREST_Y .LE. NY ) THEN
                  IF ( IMAGE( I, NEAREST_Y ) .NE. ECH__BAD_REAL ) THEN
                     REF_SPECTRUM( I ) = IMAGE( I, NEAREST_Y ) -
     :                     MIN_VALUE

                  ELSE
                     REF_SPECTRUM( I ) = 0.0
                  END IF
               END IF

*           Take 3-pixel (in Y) average spectrum too.
               IGOOD = 0
               DO Y_DELTA = -1, 1
                  IF ( NEAREST_Y + Y_DELTA .GT. 0 .AND.
     :                 NEAREST_Y + Y_DELTA .LE. NY ) THEN
                     IF ( IMAGE( I, NEAREST_Y + Y_DELTA ) .NE.
     :                    ECH__BAD_REAL ) THEN
                        AVG_SPECTRUM( I ) = AVG_SPECTRUM( I ) + MAX(
     :                        0.0, IMAGE( I, NEAREST_Y + Y_DELTA ) -
     :                        MIN_VALUE )
                        IGOOD = IGOOD + 1
                     END IF
                  END IF
               END DO
               IF ( IGOOD .GT. 0 ) THEN
                  AVG_SPECTRUM( I ) = AVG_SPECTRUM( I ) / REAL( IGOOD )

               ELSE
                  AVG_SPECTRUM( I ) = 0.0
               END IF
            END DO

*        Loop through average spectrum looking for local peaks (3 pixels wide).
            DO I = NX_LOW + 10, NX_HI - 10

*           If a local peak then.
               IF ( AVG_SPECTRUM( I ) .GT. AVG_SPECTRUM( I - 1 ) .AND.
     :              AVG_SPECTRUM( I - 1 ) .GT. AVG_SPECTRUM( I-2 ) .AND.
     :              AVG_SPECTRUM( I ) .GT. AVG_SPECTRUM( I + 1 ) .AND.
     :              AVG_SPECTRUM( I + 1 ) .GT. AVG_SPECTRUM( I + 2 ) )
     :             THEN

*              Get peak intensity from non-averaged version of spectrum.
                  PEAK_VALUE = MAX( 0.0, REF_SPECTRUM( I ) )

*              Estimate centre of peak.
                  X_AT_PEAK = 5.0
                  CALL ECH_FIND_CENTRE( 'B', 9, REF_SPECTRUM( I - 4 ),
     :                 X_AT_PEAK, STATUS )
                  X_AT_PEAK = X_AT_PEAK - 5.0

*              If centre is within +- 2 pixels of the peak pixel then.
                  IF ( ABS( X_AT_PEAK ) .LT. 2.0 .AND. PEAK_VALUE .GT.
     :                 0.0 ) THEN
                     PEAK_VALUE = SQRT( PEAK_VALUE )
                     X_AT_PEAKI = INT( X_AT_PEAK * 10 )

*                 Add subsampled 'centered' copy of reference spectrum
*                 into the accumulated profile array.
                     DO II = -90, -1
                        REF_PROFILE( II ) = REF_PROFILE( II ) +
     :                        REF_SPECTRUM( I + ( II - 5 + X_AT_PEAKI )
     :                        / 10 ) / PEAK_VALUE
                     END DO
                     DO II = 0, 90
                        REF_PROFILE( II ) = REF_PROFILE( II ) +
     :                        REF_SPECTRUM( I + ( II + 5 + X_AT_PEAKI )
     :                        / 10 ) / PEAK_VALUE
                     END DO
                  END IF
               END IF
            END DO
         END IF
      END DO

*  Calculate the median intensity of the composite profile.
      PEAK_VALUE = REF_PROFILE( 0 )
      MIN_VALUE = 1.0
      DO I = -90, 90
         REF_PROFILE( I ) = REF_PROFILE( I ) / PEAK_VALUE
         IF ( REF_PROFILE( I ) .LT. MIN_VALUE )
     :      MIN_VALUE = REF_PROFILE( I )
      END DO
      PROFILE_MEDIAN = MIN_VALUE

*  Step away from peak until intensity falls below median + peak-median/2.
      PEAK_VALUE = REF_PROFILE( 0 ) - PROFILE_MEDIAN
      HMAX_LOW_AT = 0.0
      HMAX_HI_AT = 0.0
      DO WHILE ( REF_PROFILE( INT( HMAX_LOW_AT ) ) - PROFILE_MEDIAN
     :           .GT. PEAK_VALUE / 2 )
         HMAX_LOW_AT = HMAX_LOW_AT - 1.0
      END DO
      DO WHILE ( REF_PROFILE( INT( HMAX_HI_AT ) ) - PROFILE_MEDIAN
     :           .GT. PEAK_VALUE / 2 )
         HMAX_HI_AT = HMAX_HI_AT + 1.0
      END DO

*  Calculate corresponding FWHM.
      LINE_WIDTH = ( HMAX_HI_AT - HMAX_LOW_AT + 1 ) / 10.0

*  Default value to 1.0 if calculated value is less than 1.
      IF ( LINE_WIDTH .LT. 1.0 ) THEN
         CALL ECH_REPORT( 0,
     :        ' Line width unobtainable: defaulted to 1.0.' )
         LINE_WIDTH = 1.0
      END IF

*  Plot a graph of composite profile.
      YM = 0.0
      YH = ( PEAK_VALUE + PROFILE_MEDIAN ) * 1.1
      XM = -2.0 * LINE_WIDTH
      XH = 2.0 * LINE_WIDTH
      XMI = - MIN( PROF_DIM, INT( 10.0 * XH + 0.5 ) )
      DO I = XMI, -XMI
         XAXIS( I ) = FLOAT( I ) / 10.0
      END DO
      CALL CHR_RTOC( FLOAT( INT( LINE_WIDTH * 10.0 ) ) / 10.0, REF_STR1,
     :     NCHAR1 )
      TITLE = ' Reference line FWHM estimated at ' //
     :      REF_STR1( :NCHAR1 ) // ' pixels.'
      CALL ECH_REPORT( 0, TITLE )
      CALL ECH_PLOT_GRAPH( ABS( 2 * XMI + 1 ), XAXIS( XMI + 1 ),
     :     REF_PROFILE( XMI + 1 ), XM, XH, YM, YH, 'Pixels',
     :     'Composite Profile', TITLE, 0.0, 0.0, 0, 'BINS', STATUS )

      END
