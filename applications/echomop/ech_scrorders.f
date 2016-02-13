      SUBROUTINE ECH_SCRUNCH_ORDERS(
     :           NX,
     :           NO_OF_BINS,
     :           ORDER_NUMBER,
     :           SCR_MODE,
     :           SET_SCALE,
     :           WAVELENGTH_SCALE,
     :           INPUT_SPECTRUM,
     :           INPUT_VARIANCE,
     :           BLZ_SPECTRUM,
     :           N_ORDERS,
     :           NX_REBIN,
     :           FRACTION,
     :           SSKEW,
     :           LOGR,
     :           FLUX,
     :           INTERPOLATE,
     :           QUAD,
     :           IMODE,
     :           NADD,
     :           MULTI_MERGE,
     :           BLZ_SCRUNCH,
     :           MAXIMUM_POLY,
     :           WAVE_COEFFS,
     :           SCRNCHD_SCALE,
     :           SPECTRUM,
     :           VARIANCE,
     :           REBINNED_SPECTRUM,
     :           REBINNED_VARIANCE,
     :           WAVE_SCALE_INDEX,
     :           REBIN_WORK,
     :           FILTERI,
     :           FILTERO,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_SCRUNCH_ORDERS

*  Purpose:
*     Scrunch orders in to 1-D spectrum (optionally merge).

*  Description:
*     This routine rebins a spectrum order from a wavelength polynomial
*     scale (as calibrated) to a linear wavelength scale.

*  Invocation:
*     CALL ECH_SCRUNCH_ORDERS(
*     :    NX,
*     :    NO_OF_BINS,
*     :    ORDER_NUMBER,
*     :    SCR_MODE,
*     :    SET_SCALE,
*     :    WAVELENGTH_SCALE,
*     :    INPUT_SPECTRUM,
*     :    INPUT_VARIANCE,
*     :    BLZ_SPECTRUM,
*     :    N_ORDERS,
*     :    NX_REBIN,
*     :    FRACTION,
*     :    SSKEW,
*     :    LOGR,
*     :    FLUX,
*     :    INTERPOLATE,
*     :    QUAD,
*     :    IMODE,
*     :    NADD,
*     :    MULTI_MERGE,
*     :    BLZ_SCRUNCH,
*     :    MAXIMUM_POLY,
*     :    WAVE_COEFFS,
*     :    SCRNCHD_SCALE,
*     :    SPECTRUM,
*     :    VARIANCE,
*     :    REBINNED_SPECTRUM,
*     :    REBINNED_VARIANCE,
*     :    WAVE_SCALE_INDEX,
*     :    REBIN_WORK,
*     :    FILTERI,
*     :    FILTERO,
*     :    STATUS
*     :   )

*  Arguments:
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     N_ORDERS = INTEGER (Given)
*        Number of orders in echellogram.
*     ORDER_NUMBER = INTEGER (Given)
*        Order in echellogram
*     NX_REBIN = INTEGER (Given)
*        Number of bins in scrunched order
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum degree of polynomials used
*     SSKEW = REAL (Given)
*
*     FRACTION = REAL (Given)
*
*     LOGR = LOGICAL (Given)
*
*     FLUX = LOGICAL (Given)
*
*     INTERPOLATE = LOGICAL (Given)
*
*     QUAD = LOGICAL (Given)
*
*     IMODE = INTEGER (Given)
*
*     NADD = INTEGER (Given)
*
*     NO_OF_BINS = INTEGER (Given)
*        Number of bins in output scale
*     INPUT_SPECTRUM = REAL (Given)
*        Input order spectrum
*     INPUT_VARIANCE = REAL (Given)
*        Input order spectrum variances
*     WAVE_COEFFS = DOUBLE (Given)
*        Polynomial coefficients of fit
*     SCRNCHD_SCALE = DOUBLE (Given)
*        Scale to scrunch into
*     WAVELENGTH_SCALE = DOUBLE (Given)
*        Wavelength Scale to scrunch into
*     SPECTRUM = REAL (Returned)
*        1D linear Rebinned and merged spectrum
*     VARIANCE = REAL (Returned)
*        1D linear Rebinned and merged variances
*     REBINNED_SPECTRUM = REAL (Returned)
*        Rebinned order spectrum
*     REBINNED_VARIANCE = REAL (Returned)
*        Rebinned order spectrum variances
*     WAVE_SCALE_INDEX = INTEGER (Temporary Workspace)
*        Indicies into full wavelength scale
*     REBIN_WORK = REAL (Temporary Workspace)
*        Workspace for rebinner
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions
*     SCR_MODE = CHAR (Given)
*        Scrunch type (object,arc)
*     SET_SCALE = LOGICAL (Given)
*        Set TRUE when global wavlength scale used.
*     BLZ_SPECTRUM = FLOAT (Given)
*        Blaze spectrum (normalised)
*     MULTI_MERGE = LOGICAL (Given)
*        TRUE if merging multiple frames data
*     BLZ_SCRUNCH = LOGICAL (Given)
*        TRUE if scrunching blaze also
*     FILTERI = FLOAT (Given and Returned)
*        Work array for merge
*     FILTERO = FLOAT (Given and Returned)
*        Work array for merge

*  Method:
*     Setup/Examine mode variables
*     Loop thru orders
*        Initialise output arrays
*        If interpolating between two wavelength scales then
*           Calculate interpolated wavelength scale
*        Else calculate wavelength scale
*        Endif
*        If not a global scale then fill up relevant section of
*              'wavelength_scale'
*        Initialise this orders' start/end bin indices
*          Loop thru wavelength scale array
*           If less than first scrunched wavelength, set min index
*           If less than last scrunched wavelength, set min index
*          End loop
*        Set wavelength scale indices for order
*        Rebin order spectrum into new wavelength scale
*        Re-initialise output arrays
*        Loop thru limits of wavelength scale for this order
*           Copy scrunched data/scale into output arrays
*        End loop
*        Re-initialise output arrays
*        Loop thru limits of wavelength scale for this order
*           Copy scrunched variance/scale into output arrays
*        End loop
*        Re-initialise output arrays
*        Loop thru limits of wavelength scale for this order
*           Copy scrunched variance/scale into output arrays
*        End loop
*        Merge scrunched order into 1d spectrum if enabled
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
      INCLUDE 'ECH_MAPPING.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER N_ORDERS
      INTEGER NO_OF_BINS
      INTEGER NX_REBIN
      INTEGER MAXIMUM_POLY
      LOGICAL SET_SCALE
      CHARACTER*( * ) SCR_MODE
      DOUBLE PRECISION WAVELENGTH_SCALE( NO_OF_BINS )
      REAL INPUT_SPECTRUM( NX, N_ORDERS )
      REAL BLZ_SPECTRUM( NX, N_ORDERS )
      REAL INPUT_VARIANCE( NX, N_ORDERS )
      DOUBLE PRECISION SCRNCHD_SCALE( NX_REBIN, N_ORDERS )
      DOUBLE PRECISION WAVE_COEFFS( MAXIMUM_POLY, N_ORDERS, 2 )
      LOGICAL BLZ_SCRUNCH
      INTEGER ORDER_NUMBER
      REAL SSKEW
      LOGICAL MULTI_MERGE
      LOGICAL LOGR
      LOGICAL FLUX
      LOGICAL INTERPOLATE
      LOGICAL QUAD
      INTEGER IMODE
      INTEGER NADD

*  Arguments Returned:
      REAL SPECTRUM( NO_OF_BINS )
      REAL VARIANCE( NO_OF_BINS )
      REAL REBINNED_SPECTRUM( NX_REBIN, N_ORDERS )
      REAL REBINNED_VARIANCE( NX_REBIN, N_ORDERS )
      REAL FRACTION

*  Workspace variables used:
      REAL FILTERI( NO_OF_BINS )
      REAL FILTERO( NO_OF_BINS )

*  Workspace:
      REAL REBIN_WORK( NO_OF_BINS, 2 )
      INTEGER WAVE_SCALE_INDEX( 2, N_ORDERS )

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL START_BIN_LAMBDA
      REAL END_BIN_LAMBDA
      REAL LAMBDA_SWAP
      REAL WW1
      REAL WW2
      REAL WW3
      REAL WW4

      INTEGER HI_ORDER
      INTEGER I
      INTEGER J
      INTEGER IORD
      INTEGER IQUAD
      INTEGER LOWLIM
      INTEGER HILIM
      INTEGER INDEX
      INTEGER LOW_ORDER
      INTEGER SORD
      INTEGER EORD
      INTEGER NCHAR1

      LOGICAL USDOWN

      CHARACTER*32 REF_STR1

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
      INTEGER CHR_LEN
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

      IF ( .NOT. SET_SCALE ) THEN
         IF ( NO_OF_BINS .NE. NX_REBIN * N_ORDERS ) THEN
            REPORT_STRING = ' Cannot scrunch: please regenerate' //
     :           ' wavelengths (Option 12.2).'
            CALL ECH_REPORT( 0, REPORT_STRING )
            RETURN
         ENDIF
      ENDIF

      CALL ECH_REPORT( 0, ' Generating wavelengths for scrunch.' )

*  Setup/Examine mode variables.
      IF ( QUAD ) THEN
         IQUAD = 1

      ELSE
         IQUAD = 0
      END IF

      IF ( IMODE .EQ. 0 ) THEN
         LOGR = .FALSE.
      END IF

*  Determine range of orders to process.
      IF ( ORDER_NUMBER .GT. 0 ) THEN
         SORD = ORDER_NUMBER
         EORD = ORDER_NUMBER

      ELSE
         SORD = 1
         EORD = N_ORDERS
      ENDIF

      LOW_ORDER = 0
      HI_ORDER = 0

*  Find first and last wavelength-calibrated orders.
      I = SORD
      DO WHILE ( I .LE. EORD .AND.
     :           ( LOW_ORDER .EQ. 0 .OR. HI_ORDER .EQ. 0 ) )
         IF ( LOW_ORDER .EQ. 0 .AND.
     :        WAVE_COEFFS( 1, I, 1 ) .NE. 0.0 ) THEN
            LOW_ORDER = I
         END IF
         J = N_ORDERS - I + 1
         IF ( HI_ORDER .EQ. 0 .AND.
     :        WAVE_COEFFS( 1, J, 1 ) .NE. 0.0 ) THEN
            HI_ORDER = J
         END IF
         I = I + 1
      END DO

*  Issume a warning if no orders are wavelength-calibrated and exit.
      IF ( LOW_ORDER .LE. 0 .OR. HI_ORDER .LE. 0 ) THEN
         REPORT_STRING = ' Unable to scrunch because wavelength' //
     :        ' scales not yet fitted.'
         CALL ECH_REPORT( 0, REPORT_STRING )
         GO TO 999
      END IF

*  Issume a warning if some of the orders are not calibrated.
      IF ( ORDER_NUMBER .LE. 0 ) THEN
         SORD = LOW_ORDER
         EORD = HI_ORDER
         IF ( HI_ORDER - LOW_ORDER + 1 .LT. N_ORDERS ) THEN
            REPORT_STRING = ' Unable to scrunch some orders' //
     :           ' because they are not yet wavelength calibrated.'
            CALL ECH_REPORT( 0, REPORT_STRING )
         ENDIF
      ENDIF

*  Determine the full range of wavelengths covered.
      CALL ECH_FEVAL( ' ', MAXIMUM_POLY, WAVE_COEFFS( 1, HI_ORDER, 1 ),
     :     1, 1.0, WW1, STATUS )
      CALL ECH_FEVAL( ' ', MAXIMUM_POLY, WAVE_COEFFS( 1, HI_ORDER, 1 ),
     :     1, FLOAT( NX ), WW2, STATUS )
      CALL ECH_FEVAL( ' ', MAXIMUM_POLY, WAVE_COEFFS( 1, LOW_ORDER, 1 ),
     :     1, 1.0, WW3, STATUS )
      CALL ECH_FEVAL( ' ', MAXIMUM_POLY, WAVE_COEFFS( 1, LOW_ORDER, 1 ),
     :     1, FLOAT( NX ), WW4, STATUS )
      START_BIN_LAMBDA = MIN( WW1, WW2, WW3, WW4 )
      END_BIN_LAMBDA = MAX( WW1, WW2, WW3, WW4 )

*  Fill the global wavelength scale if needed.
      IF ( SET_SCALE ) THEN
         IF ( USR_START_WAVE .GT. 0.0 )
     :      START_BIN_LAMBDA = USR_START_WAVE
         CALL FIG_WFILLD( DBLE( START_BIN_LAMBDA ),
     :        DBLE( END_BIN_LAMBDA ), LOGR, NO_OF_BINS,
     :        WAVELENGTH_SCALE )
      END IF

*  Initialise the output merged spectrum to zero.
      IF ( SCR_MODE .NE. 'ARC' .AND. ORDER_NUMBER .EQ. 0 .AND.
     :     .NOT. MULTI_MERGE ) THEN
         DO I = 1, NO_OF_BINS
            SPECTRUM( I ) = 0.0
            VARIANCE( I ) = 0.0
         END DO
      END IF

*  Loop through orders.
      DO IORD = SORD, EORD
         CALL CHR_ITOC( IORD, REF_STR1, NCHAR1 )
         REPORT_STRING = ' Scrunching ' //
     :                   SCR_MODE( :CHR_LEN( SCR_MODE ) ) //
     :                   ' spectrum, order ' //
     :                   REF_STR1( :NCHAR1 ) // '.'
         CALL ECH_REPORT( 0, REPORT_STRING )

*     Initialise output arrays.
*
*     NOTE, the array scrnchd_scale is used as temporary workspace to hold
*     the polynomial wavelength scale for input to the rebin routine.
*     In this context only the first 'nx' entries have values.
*     After rebinning has been done, the actual scrunched wavelengths
*     are copied into the array, utilising all 'nx_rebin' elements.
         IF ( WAVE_COEFFS( 1, IORD, 1 ) .NE. 0.0 ) THEN
            DO I = 1, NX_REBIN
               SCRNCHD_SCALE( I, IORD ) = 0.0
               REBINNED_SPECTRUM( I, IORD ) = 0.0
               REBINNED_VARIANCE( I, IORD ) = 0.0
            END DO

            DO I = 1, NO_OF_BINS
               REBIN_WORK( I, 1 ) = 0.0
               REBIN_WORK( I, 2 ) = 0.0
            END DO

*        Interpolate between two wavelength scales.
            IF ( INTERPOLATE ) THEN
               CALL FIG_WGEN2( IORD, NX, N_ORDERS, FRACTION,
     :              WAVE_COEFFS( 1, 1, 1 ), WAVE_COEFFS( 1, 1, 2 ),
     :              MAXIMUM_POLY, SCRNCHD_SCALE( 1, IORD ) )

*        Otherwise generate single wavelength scale.
            ELSE
               CALL FIG_WGEN( IORD, NX, N_ORDERS,
     :              WAVE_COEFFS( 1, 1, 1 ), MAXIMUM_POLY,
     :              SCRNCHD_SCALE( 1, IORD ) )
            END IF

*        If not a global scale then fill up relevant section of
*        'wavelength_scale'.
            IF ( .NOT. SET_SCALE ) THEN

*           Determine orientation  of the orders.
               CALL ECH_FEVAL( ' ', MAXIMUM_POLY,
     :              WAVE_COEFFS( 1, LOW_ORDER, 1 ), 1, 1.0,
     :              START_BIN_LAMBDA, STATUS )
               CALL ECH_FEVAL( ' ', MAXIMUM_POLY,
     :              WAVE_COEFFS( 1, HI_ORDER, 1 ), 1, FLOAT( NX ),
     :              END_BIN_LAMBDA, STATUS )
               IF ( START_BIN_LAMBDA .GT. END_BIN_LAMBDA ) THEN
                  USDOWN = .TRUE.

               ELSE
                  USDOWN = .FALSE.
               END IF

*           Find wavelength range covered by this order.
               CALL ECH_FEVAL( ' ', MAXIMUM_POLY,
     :              WAVE_COEFFS( 1, IORD, 1 ), 1, 1.0,
     :              START_BIN_LAMBDA, STATUS )
               CALL ECH_FEVAL( ' ', MAXIMUM_POLY,
     :              WAVE_COEFFS( 1, IORD, 1 ), 1, FLOAT( NX ),
     :              END_BIN_LAMBDA, STATUS )
               IF ( START_BIN_LAMBDA .GT. END_BIN_LAMBDA ) THEN
                  LAMBDA_SWAP = START_BIN_LAMBDA
                  START_BIN_LAMBDA = END_BIN_LAMBDA
                  END_BIN_LAMBDA = LAMBDA_SWAP
               END IF

*           Fill relevant part of global wavelength-scale.
               IF ( USDOWN ) THEN
                  CALL FIG_WFILLD( DBLE( START_BIN_LAMBDA ),
     :                 DBLE( END_BIN_LAMBDA ), LOGR, NX_REBIN,
     :                 WAVELENGTH_SCALE( ( N_ORDERS - IORD ) *
     :                 NX_REBIN + 1 ) )
                  LOWLIM = ( N_ORDERS - IORD ) * NX_REBIN + 1
                  HILIM = ( N_ORDERS - IORD + 1 ) * NX_REBIN - 1

               ELSE
                  CALL FIG_WFILLD( DBLE( START_BIN_LAMBDA ),
     :                 DBLE( END_BIN_LAMBDA ), LOGR, NX_REBIN,
     :                 WAVELENGTH_SCALE( ( IORD - 1 ) * NX_REBIN + 1 ) )
                  LOWLIM = ( IORD - 1 ) * NX_REBIN + 1
                  HILIM = IORD * NX_REBIN - 1
               END IF

*        Initialise this orders' start/end bin indices.
            ELSE
               LOWLIM = 1
               HILIM = 1

*           Loop through wavelength scale array.
               DO I = 1, NO_OF_BINS

*              If less than first scrunched wavelength, set min index.
                  IF ( WAVELENGTH_SCALE( I ) .GT. 0.0 ) THEN
                     IF ( LOWLIM .EQ. 1 ) LOWLIM = I
                     IF ( WAVELENGTH_SCALE( I ) .LT.
     :                    SCRNCHD_SCALE( 1, IORD ) ) LOWLIM = I + 1

*              If less than last scrunched wavelength, set max index.
                     IF ( WAVELENGTH_SCALE( I ) .LE.
     :                    SCRNCHD_SCALE( NX, IORD ) ) HILIM = I
                  END IF
               END DO
               LOWLIM = LOWLIM + 1 + NX / NX_REBIN
               HILIM = HILIM - 1 - NX / NX_REBIN
            END IF

*        Set wavelength scale indices for order.
            WAVE_SCALE_INDEX( 1, IORD ) = LOWLIM
            WAVE_SCALE_INDEX( 2, IORD ) = HILIM - LOWLIM + 1

*        Rebin order spectrum into new wavelength scale.
            CALL FIG_REBIND( IMODE, IQUAD, INPUT_SPECTRUM( 1, IORD ),
     :           NX, REBIN_WORK( 1, 1 ), NO_OF_BINS, NADD, SSKEW,
     :           FLUX, SCRNCHD_SCALE( 1, IORD ), WAVELENGTH_SCALE,
     :           LOGR, LOGR )

*        Re-initialise output arrays.
            DO INDEX = 1, NX_REBIN
               REBINNED_SPECTRUM( INDEX, IORD ) = 0.0
            END DO

*        Loop through wavelength scale for this order
*        copying scrunched data into output arrays.
            DO INDEX = LOWLIM, HILIM
               REBINNED_SPECTRUM( INDEX - LOWLIM + 1, IORD ) =
     :               REBIN_WORK( INDEX, 1 )
            END DO

*        Copy scrunched variance/scale into output arrays.
            IF ( interpolate ) THEN
               CALL FIG_WGEN2( IORD, NX, N_ORDERS, FRACTION,
     :              WAVE_COEFFS( 1, 1, 1 ), WAVE_COEFFS( 1, 1, 2 ),
     :              MAXIMUM_POLY, SCRNCHD_SCALE( 1, IORD ) )

            ELSE
               CALL FIG_WGEN( IORD, NX, N_ORDERS,
     :              WAVE_COEFFS( 1, 1, 1 ), MAXIMUM_POLY,
     :              SCRNCHD_SCALE( 1, IORD ) )
            END IF

            CALL FIG_REBIND( IMODE, IQUAD, INPUT_VARIANCE( 1, IORD ),
     :           NX, REBIN_WORK( 1, 2 ), NO_OF_BINS, NADD, SSKEW, FLUX,
     :           SCRNCHD_SCALE( 1, IORD ), WAVELENGTH_SCALE, LOGR,
     :           LOGR )

*        Re-initialise output arrays
            DO INDEX = 1, NX_REBIN
               SCRNCHD_SCALE( INDEX, IORD ) = 0.0
               REBINNED_VARIANCE( INDEX, IORD ) = 0.0
            END DO

*        Loop through limits of wavelength scale for this order.
            DO INDEX = LOWLIM, HILIM

*           Copy scrunched variance/scale into output arrays.
               SCRNCHD_SCALE( INDEX - LOWLIM + 1, IORD ) =
     :               WAVELENGTH_SCALE( INDEX )
               REBINNED_VARIANCE( INDEX - LOWLIM + 1, IORD ) =
     :               REBIN_WORK( INDEX, 2 )
            END DO

*        Merge scrunched order into 1d spectrum if enabled.
            IF ( SCR_MODE .NE. 'ARC' .AND. ORDER_NUMBER .EQ. 0 ) THEN
               CALL ECH_REPORT( 0, ' Merging order into 1-D spectrum.' )
               IF ( MULTI_MERGE ) THEN
                  REPORT_STRING = ' Co-adding 1-D spectrum to' //
     :                  ' previously  merged spectrum.'
                  CALL ECH_REPORT( 0, REPORT_STRING )
     :
               END IF
               CALL ECH_MERGE_ORDERS( NO_OF_BINS, 1,
     :              REBIN_WORK( 1, 1 ), REBIN_WORK( 1, 2 ), 51, 10.0,
     :              SPECTRUM, VARIANCE, FILTERI, FILTERO, STATUS  )
            END IF

         ELSE
            CALL ECH_REPORT( 0,
     :     ' Order has no wavelengths fitted: no scrunching done.' )
         ENDIF
      END DO

      IF ( SCR_MODE .NE. 'ARC' .AND. ORDER_NUMBER .EQ. 0 ) THEN
         IF ( .NOT. SET_SCALE ) THEN
            REPORT_STRING = ' Warning: merged spectrum' //
     :            ' does not have monotonic wavelengths.'
            CALL ECH_REPORT( 0, REPORT_STRING )
         END IF
      ENDIF

 999  CONTINUE

      END
