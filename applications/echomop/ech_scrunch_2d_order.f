      SUBROUTINE ECH_SCRUNCH_2D_ORDER(
     :           NX,
     :           NY,
     :           IMAGE,
     :           UERROR,
     :           GOT_ERRORS,
     :           NO_OF_BINS,
     :           MAX_SKY_PIXELS,
     :           READOUT_NOISE,
     :           NX_REBIN,
     :           W2_NX_POLY,
     :           W2_NY_POLY,
     :           W_POLY_2D,
     :           MAXIMUM_POLY,
     :           WAVE_COEFFS,
     :           SCRNCHD_WAVES,
     :           DEK_BELOW,
     :           DEK_ABOVE,
     :           TRACE_POLYNOMIAL,
     :           FLAT,
     :           EFLAT,
     :           REBINNED_IMAGE,
     :           REBINNED_ERRORS,
     :           X_TRACE_COORD,
     :           Y_TRACE_COORD,
     :           X_IN,
     :           CORRECTION,
     :           TWO_D,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_SCRUNCH_2D_ORDER

*  Purpose:
*     Re-bin 2-D order into 1-D spectrum.

*  Description:
*     This routine rebins a spectrum order from a wavelength polynomial
*     scale (as calibrated) to a linear wavelength scale.

*  Invocation:
*     CALL ECH_SCRUNCH_2D_ORDER(
*     :    NX,
*     :    NY,
*     :    IMAGE,
*     :    UERROR,
*     :    GOT_ERRORS,
*     :    NO_OF_BINS,
*     :    MAX_SKY_PIXELS,
*     :    READOUT_NOISE,
*     :    NX_REBIN,
*     :    W2_NX_POLY,
*     :    W2_NY_POLY,
*     :    W_POLY_2D,
*     :    MAXIMUM_POLY,
*     :    WAVE_COEFFS,
*     :    SCRNCHD_WAVES,
*     :    DEK_BELOW,
*     :    DEK_ABOVE,
*     :    TRACE_POLYNOMIAL,
*     :    FLAT,
*     :    EFLAT,
*     :    REBINNED_IMAGE,
*     :    REBINNED_ERRORS,
*     :    X_TRACE_COORD,
*     :    Y_TRACE_COORD,
*     :    X_IN,
*     :    CORRECTION,
*     :    TWO_D,
*     :    STATUS
*     :   )

*  Arguments:
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NX_REBIN = INTEGER (Given)
*        Number of bins in scrunched order.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum degree of polynomials used.
*     NO_OF_BINS = INTEGER (Given)
*        Number of bins in output scale.
*     WAVE_COEFFS = DOUBLE (Given)
*        Polynomial coefficients of fit.
*     SCRNCHD_WAVES = DOUBLE (Given)
*        Scale to scrunch into.
*     REBINNED_SPECTRUM = REAL (Returned)
*        Rebinned order spectrum.
*     REBINNED_VARIANCE = REAL (Returned)
*        Rebinned order spectrum variances.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     IMAGE = REAL (Given)
*        Input frame image of dimensions nx.
*     UERROR = REAL (Given and Returned)
*        Variance.
*     GOT_ERRORS = LOGICAL (Given)
*        TRUE if variances available.
*     MAX_SKY_PIXELS = INTEGER (Given)
*        Maximum number of pixels in dekker (max extractable).
*     READOUT_NOISE = REAL (Given)
*        Readout noise (sigma) in ocunts.
*     W2_NX_POLY = INTEGER (Given)
*        Number of x-polynomial coefficients.
*     W2_NY_POLY = INTEGER (Given)
*        Number of y-polynomial coefficients.
*     W_POLY_2D = DOUBLE (Given and Returned)
*        2D chebyshev polynomial coefficients.
*     DEK_BELOW = INTEGER (Given)
*        Extent of dekker below order trace.
*     DEK_ABOVE = INTEGER (Given)
*        Extent of dekker above order trace.
*     TRACE_POLYNOMIAL = DOUBLE (Given)
*        Coefficients of fit describing order path across frame.
*     FLAT = REAL (Given)
*        Flat field image.
*     EFLAT = REAL (Given)
*        Variance array for flat field image.
*     REBINNED_IMAGE = REAL (Given and Returned)
*        Output image frame.
*     REBINNED_ERRORS = REAL (Given)
*        Rebinned variances.
*     X_TRACE_COORD = DOUBLE (Temporary Workspace)
*        X coords of order trace path.
*     Y_TRACE_COORD = DOUBLE (Temporary Workspace)
*        Y coords of order trace path.
*     X_IN = REAL (Given and Returned)
*        X data array.
*     CORRECTION = REAL (Given and Returned)
*        Wavelength correction predicted by 2d chebyshev.
*     TWO_D = REAL (Given and Returned)
*        2D wavelength scale for order.

*  Method:
*     Initialise output arrays
*     Loop thru spatial increments
*        Calculate distortion fit offsets for each x position along order
*        Calculate 'corrected' wavelength for each x position along order
*     End loop
*     Loop thru scrunched output bins
*      If wavelength of bin known then
*       Loop thru spatial profile offsets
*           Determine start/end contributing input pixels
*           If good start and end input bins then
*             Loop thru input image pixels (in x) which contribute
*               Add contribution from input pixel
*             End loop
*             Set output pixels according to current trace offset
*           Endif
*       End loop
*      Endif
*     End loop

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
      INCLUDE 'ECH_DATA_CONSTRAINTS.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      INTEGER NO_OF_BINS
      INTEGER MAX_SKY_PIXELS
      INTEGER NX_REBIN
      INTEGER MAXIMUM_POLY
      REAL IMAGE ( NX, NY )
      LOGICAL GOT_ERRORS
      REAL REBINNED_ERRORS ( NX_REBIN, NY )
      DOUBLE PRECISION SCRNCHD_WAVES ( NX_REBIN )
      DOUBLE PRECISION WAVE_COEFFS ( MAXIMUM_POLY )
      REAL READOUT_NOISE
      INTEGER DEK_BELOW
      INTEGER DEK_ABOVE
      INTEGER W2_NX_POLY
      INTEGER W2_NY_POLY
      REAL FLAT(NX,-MAX_SKY_PIXELS/2:MAX_SKY_PIXELS/2)
      REAL EFLAT(NX,-MAX_SKY_PIXELS/2:MAX_SKY_PIXELS/2)
      DOUBLE PRECISION TRACE_POLYNOMIAL ( MAXIMUM_POLY )

*  Arguments Returned:
      REAL UERROR( NX, NY )
      REAL REBINNED_IMAGE( NX_REBIN, NY )
      DOUBLE PRECISION W_POLY_2D( MAXIMUM_POLY * MAXIMUM_POLY )

*  Workspace:
      DOUBLE PRECISION X_TRACE_COORD( NX )
      DOUBLE PRECISION Y_TRACE_COORD( NX )

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER MX2D_POLY
      DOUBLE PRECISION WORK( 8192, 2 )

*  Workspace variables used:
      REAL TWO_D( -MAX_SLICE_PIXELS / 2: MAX_SLICE_PIXELS / 2, NX )
      DOUBLE PRECISION X_IN( NX )
      DOUBLE PRECISION CORRECTION( NX )

*  Local Variables:
      DOUBLE PRECISION AXMIN( 2 )
      DOUBLE PRECISION AXMAX( 2 )

      REAL WAVE_BIN_START
      REAL WAVE_BIN_END
      REAL RAW_ERR
      REAL YOFFSET
      REAL AYOFFSET
      REAL VN
      REAL VA
      REAL VAR0
      REAL DN
      REAL ONE_D
      REAL ONE_DN

      INTEGER DEGREE( 2 )
      INTEGER I
      INTEGER J
      INTEGER IYOUT
      INTEGER JOFF
      INTEGER ISTAT
      INTEGER TRY
      INTEGER USE
      INTEGER START_BIN
      INTEGER END_BIN
      INTEGER START_SCAN
      INTEGER END_SCAN
      INTEGER YCOORD
      INTEGER NCHAR1

      CHARACTER*4 REF_STR1

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Do nothing for a disabled order.
      IF ( TRACE_POLYNOMIAL( 1 ) .EQ. ECH__BAD_DOUBLE ) THEN
         CALL ECH_REPORT( 0, ' Order disabled.' )
         GO TO 999
      END IF

      MX2D_POLY = MAXIMUM_POLY * MAXIMUM_POLY
      VAR0 = READOUT_NOISE * READOUT_NOISE

      CALL ECH_CALC_TRACE( NX, MAXIMUM_POLY, TRACE_POLYNOMIAL,
     :     X_TRACE_COORD, Y_TRACE_COORD, STATUS )
      IYOUT = INT( Y_TRACE_COORD( NX / 2 ) + 0.5 )

*  Initialise output arrays.
      DO I = 1, NX
         X_IN( I ) = DBLE( I )
         CORRECTION( I ) = 0.0
         WORK( I, 1 ) = DBLE( I )
      END DO
      RAW_ERR = 0.0
      AXMIN( 1 ) = 0.0
      AXMAX( 1 ) = DBLE( NX )
      AXMIN( 2 ) = DBLE( DEK_BELOW )
      AXMAX( 2 ) = DBLE( DEK_ABOVE )
      DEGREE( 1 ) = W2_NX_POLY
      DEGREE( 2 ) = W2_NY_POLY

*  Loop thru spatial increments.
      DO J = DEK_BELOW - 1, DEK_ABOVE + 1
         DO I = 1, NX
            WORK( I, 2 ) = DBLE( J )
         END DO

*     Calculate distortion fit offsets for each X-position along order.
*     ECH version.
         CALL ECH_CHEV( NX, 2, WORK, AXMIN, AXMAX, DEGREE,
     :        MX2D_POLY, W_POLY_2D, CORRECTION, ISTAT )

*     NAG version.
*         CALL E02CBF( 1, NX, W2_NX_POLY, W2_NY_POLY,
*     :        X_IN, 0.0, DBLE( NX ), DBLE( J ),
*     :        DBLE( DEK_BELOW - 1 ), DBLE( DEK_ABOVE + 1 ),
*     :        CORRECTION, W_POLY_2D,
*     :        MX2D_POLY, WORK, 20, ISTAT )

*     Calculate 'corrected' wavelength for each X position along order.
         DO I = 2, NX
            CALL ECH_FEVAL( ' ', MAXIMUM_POLY, WAVE_COEFFS, 1,
     :           FLOAT( I - 1 ), ONE_D, STATUS )
            CALL ECH_FEVAL( ' ', MAXIMUM_POLY, WAVE_COEFFS, 1,
     :           FLOAT( I ), ONE_DN, STATUS )
            TWO_D( J, I ) = ( ONE_D +
     :            REAL( CORRECTION( I - 1 ) ) +
     :            ONE_DN + REAL( CORRECTION( I ) ) ) / 2.0
         END DO
         TWO_D( J, 1 ) = 2.0 * TWO_D( J, 2 ) - TWO_D( J, 3 )
      END DO

      START_SCAN = 1
      END_SCAN = NX

*  Loop thru scrunched output bins.
      DO I = 1, NX_REBIN - 1
         IF ( I / 200 * 200 .EQ. I ) THEN
            CALL CHR_ITOC( I , REF_STR1, NCHAR1 )
            REPORT_STRING = ' Rebinning at X=' //
     :            REF_STR1( :NCHAR1 ) // '.'
            CALL ECH_REPORT( 0, REPORT_STRING )
         END IF

*     If wavelength of bin known then
         DO J = DEK_BELOW - 1, DEK_ABOVE + 1
            IF ( IYOUT + J .GT. 0 .AND. IYOUT + J .LE. NY ) THEN
               REBINNED_IMAGE( I, IYOUT + J ) = 0.0
            END IF
         END DO

         IF ( SCRNCHD_WAVES( I ) .GT. 0.0 ) THEN
            WAVE_BIN_START =  REAL( SCRNCHD_WAVES( I ) )
            WAVE_BIN_END =  REAL( SCRNCHD_WAVES( I + 1 ) )

*        Loop thru spatial profile offsets.
            DO J = DEK_BELOW - 1, DEK_ABOVE + 1
               START_BIN = 0
               END_BIN = 0
               JOFF =  J

*           Determine start/end contributing input pixels.
               DO TRY = MAX( 1, START_SCAN - 20 ),
     :                  MIN( END_SCAN + 20, NX - 1 )
                  IF ( TWO_D( JOFF,TRY ) .NE. 0.0 ) THEN
                     IF ( TWO_D( JOFF, TRY ) .LE. WAVE_BIN_START )
     :                  START_BIN = TRY
                     IF ( TWO_D( JOFF, TRY ) .LE. WAVE_BIN_END )
     :                  END_BIN = TRY
                  END IF
               END DO

               DN = 0.0
               VN = 0.0

*           If good start and end input bins then.
               IF ( START_BIN .NE. 0 .AND. END_BIN .NE. 0 ) THEN
                  START_SCAN = START_BIN
                  END_SCAN = END_BIN

*              Loop thru input image pixels (in X) which contribute.
                  DO USE = START_BIN, END_BIN
                     YCOORD = INT( Y_TRACE_COORD( USE ) + JOFF + 0.5 )

                     IF ( YCOORD .GT. 0 .AND. YCOORD .LE. NY ) THEN
                        YOFFSET = REAL( Y_TRACE_COORD( USE ) ) -
     :                        FLOAT( INT( Y_TRACE_COORD( USE ) ) )
                        AYOFFSET = ABS( YOFFSET )

*                   Add contribution from input pixel.
                        DN = DN + ( 1.0 - (
     :                        MAX( TWO_D( JOFF, USE + 1 ) -
     :                        WAVE_BIN_END, 0.0 ) +
     :                        MAX( WAVE_BIN_START -
     :                        TWO_D( JOFF, USE ), 0.0 ) ) /
     :                        ( TWO_D( JOFF, USE + 1 ) -
     :                        TWO_D( JOFF, USE ) ) ) *
     :                        IMAGE( USE, YCOORD )
                        IF ( GOT_ERRORS ) THEN
                           RAW_ERR = UERROR( USE,YCOORD )
                        END IF
                        VA = FLAT( USE, JOFF ) * (
     :                        FLAT( USE, JOFF ) * ( VAR0 +
     :                        RAW_ERR ** 2.0 ) )
                        IF ( EFLAT( USE, JOFF ) .GT. 0.0 ) THEN
                           VA = VA + (
     :                           EFLAT( USE, JOFF ) *
     :                           IMAGE( USE, YCOORD ) *
     :                           EFLAT( USE, JOFF ) *
     :                           IMAGE( USE, YCOORD ) )
                        END IF
                        VN = VN + ( 1.0 - (
     :                        MAX( TWO_D( JOFF, USE + 1 )  -
     :                        WAVE_BIN_END, 0. ) +
     :                        MAX( WAVE_BIN_START -
     :                        TWO_D( JOFF, USE ), 0.0 ) ) /
     :                        ( TWO_D( JOFF, USE + 1 ) -
     :                          TWO_D( JOFF, USE ) ) ) * VA
                     END IF
                  END DO

*              Set output pixels according to current trace offset.
                  IF ( YOFFSET .GE. 0.5 ) THEN
                     IF ( IYOUT + J .GT. 0 .AND.
     :                    IYOUT + J .LE. NY ) THEN
                        REBINNED_IMAGE( I, IYOUT + J ) =
     :                        REBINNED_IMAGE( I, IYOUT + J ) +
     :                        AYOFFSET * DN
                        REBINNED_ERRORS( I, IYOUT + J ) =
     :                        REBINNED_ERRORS( I, IYOUT + J ) +
     :                        AYOFFSET * VN
                     END IF
                     IF ( IYOUT + J + 1 .GT. 0 .AND.
     :                    IYOUT + J + 1 .LE. NY ) THEN
                        REBINNED_IMAGE( I, IYOUT + J + 1 ) =
     :                        REBINNED_IMAGE( I, IYOUT + J + 1 ) +
     :                        ( 1.0 - AYOFFSET ) * DN
                        REBINNED_ERRORS( I, IYOUT + J + 1 ) =
     :                        REBINNED_ERRORS( I, IYOUT + J + 1 ) +
     :                        ( 1.0 - AYOFFSET ) * VN
                     END IF

                  ELSE IF ( YOFFSET .NE. 0.0 ) THEN
                     IF ( IYOUT + J .GT. 0 .AND.
     :                    IYOUT + J .LE. NY ) THEN
                        REBINNED_IMAGE( I, IYOUT + J ) =
     :                        REBINNED_IMAGE( I, IYOUT + J ) +
     :                        ( 1.0 - AYOFFSET ) * DN
                        REBINNED_ERRORS( I, IYOUT+J ) =
     :                        REBINNED_ERRORS( I, IYOUT + J ) +
     :                        ( 1.0 - AYOFFSET ) * VN
                     END IF
                     IF ( IYOUT - 1 + J .GT. 0 .AND.
     :                    IYOUT - 1 + J .LE. NY ) THEN
                        REBINNED_IMAGE( I, IYOUT - 1 + J ) =
     :                        REBINNED_IMAGE( I, IYOUT - 1 + J ) +
     :                        AYOFFSET * DN
                        REBINNED_ERRORS( I, IYOUT - 1 + J ) =
     :                        REBINNED_ERRORS( I, IYOUT - 1 + J ) +
     :                        AYOFFSET * VN
                     END IF

                  ELSE
                     IF ( IYOUT + J .GT. 0 .AND.
     :                    IYOUT + J .LE. NY ) THEN
                        REBINNED_IMAGE( I, IYOUT + J ) = DN
                        REBINNED_ERRORS( I, IYOUT + J ) = VN
                     END IF
                  END IF
               END IF
            END DO
         END IF
      END DO

  999 CONTINUE

      END
