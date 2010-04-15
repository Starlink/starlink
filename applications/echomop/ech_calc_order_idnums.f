      SUBROUTINE ECH_CALC_ORDER_IDNUMS(
     :           NX,
     :           N_ORDERS,
     :           WVCAL_INTERACT,
     :           MAXIMUM_POLY,
     :           WAVE_COEFFS,
     :           START_SEARCH_WAVE,
     :           END_SEARCH_WAVE,
     :           ORDER_ID_NUMBER,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_CALC_ORDER_IDNUMS

*  Purpose:
*     Calculates order numbers on Echellogram.

*  Description:
*     This routine attempts to divine the order numbers corresponding to the
*     orders so far wavelength calibrated,and thus infer the full set of
*     order numbers for the frame. If sucessfull, then estimates of the
*     probable wavelength range of each order are made, and these may be
*     used by the wavelength calibrator to limit its search range and thus
*     speed its execution.

*  Invocation:
*      CALL ECH_CALC_ORDER_IDNUMS(
*     :     NX,
*     :     N_ORDERS,
*     :     WVCAL_INTERACT,
*     :     MAXIMUM_POLY,
*     :     WAVE_COEFFS,
*     :     START_SEARCH_WAVE,
*     :     END_SEARCH_WAVE,
*     :     ORDER_ID_NUMBER,
*     :     STATUS
*     :    )

*  Arguments:
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     N_ORDERS = INTEGER (Given)
*        Number of orders in echellogram.
*     WVCAL_INTERACT = LOGICAL (Given)
*        TRUE if interactive calibration selected.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum number of coefficients for wavelength calibrating polynomial.
*     WAVE_COEFFS = DOUBLE (Given)
*        Wavelength polynomials for each order.
*     START_SEARCH_WAVE = REAL (Given and Returned)
*        Start search wavelengths  for each order.
*     END_SEARCH_WAVE = REAL (Given and Returned)
*        End search wavelengths  for each order.
*     ORDER_ID_NUMBER = INTEGER (Given and Returned)
*        Order numbers for each order.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     Check if already got a good set of order numbers
*     Establish number of active wavelength coefficients for each order
*     Count number of active (already calibrated) orders
*     If NOT already calculated AND we have at least two calibrated orders then
*        Loop through orders up to penultimate
*           Zero counter for the order
*           Loop through remaining order (ie above on the frame)
*            If both of the orders are active then
*              Estimate start/end range for order
*              If range is acceptable then
*                 Calculate ratio of wavelengths at middle of each order
*                 Initialise order number estimate to first loop counter
*                 Check for inverted reference frame, and report suspicion
*                 Loop through all credible order numbers (1 to max_allowed_ordnum)
*                    Calculate expected ratio (as above), assuming first
*                         loops' order number is test order number
*                    If calculated ratio for test order number is closest
*                        yet to that calculated from actual calibrated orders then
*                       Save it as most likely candidate yet
*                    Endif
*                 End loop
*                 If we have an estimated order number and also a previous one then
*                    If estimate and previous value differ, report it
*                    Else remember it and increment counter of stable estimates
*                    Endif
*                 Else remember it and increment counter of stable estimates
*                 Endif
*                 Calculate corresponding 'first order' (ie lowest on frame) assuming
*                        the best estimate above was correct
*                 Loop through all orders calculating corresponding start/end wavelengths etc
*                 End loop
*              Endif
*            Endif
*           End loop
*        End loop
*        Report calculated ranges
*        If interactively calibrating then
*           Inquire wether the ranges are acceptable
*        Else accept them by default
*           BUT Check calculated order numbers are reasonable
*          Report if we had to disable the order numbers
*        Endif
*        If new order numbers have NOT been accepted then clear order_descrip array
*        Else interpolate wave scales for any previously unknown orders
*        Endif
*     Else return a too-few-calibrated status
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
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'
      INCLUDE 'ECH_DATA_CONSTRAINTS.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER N_ORDERS
      LOGICAL WVCAL_INTERACT
      INTEGER MAXIMUM_POLY
      DOUBLE PRECISION WAVE_COEFFS( MAXIMUM_POLY, N_ORDERS )
*            ! Wavelength coefficients.

*  Argments Given and Returned:
      REAL START_SEARCH_WAVE( N_ORDERS )
      REAL END_SEARCH_WAVE( N_ORDERS )
      INTEGER ORDER_ID_NUMBER( N_ORDERS )

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL WAVE_RATIO
      REAL TEMP
      REAL TEST_START1
      REAL TEST_START2
      REAL TEST_END1
      REAL TEST_END2
      REAL VALUE1
      REAL VALUE2
      REAL TEST_RATIO
      REAL MIN_DIFF
      REAL DELTA_LAMBDA
      REAL LEEWAY_FACTOR

      INTEGER COUNTS( MAX_ALLOWED_ORDERS ) ! Counts per order number candidate.
      INTEGER POSSIBLE_ORDER( MAX_ALLOWED_ORDERS )
      INTEGER I
      INTEGER II
      INTEGER III
      INTEGER COUNT
      INTEGER IT
      INTEGER DELTA_ORDER
      INTEGER FIRST_ORDER
      INTEGER NCHAR1
      INTEGER NCHAR2
      INTEGER NCHAR3
      INTEGER NCHAR4

      LOGICAL ALREADY_DONE
      LOGICAL ANSWER
      LOGICAL REVERSE

      CHARACTER*8 REF_STR1
      CHARACTER*8 REF_STR2
      CHARACTER*8 REF_STR3
      CHARACTER*8 REF_STR4

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

*  Check if already got a good set of order numbers.
      STATUS = 0
      ALREADY_DONE = .FALSE.
      LEEWAY_FACTOR = 5.0
      DO I = 1, N_ORDERS
         IF ( ORDER_ID_NUMBER( I ) .GT. 0 ) THEN
            ALREADY_DONE = .TRUE.
            GO TO 100
         END IF
      END DO
  100 CONTINUE

*  Count number of active (already calibrated) orders.
      COUNT = 0
      DO I = 1, N_ORDERS
         IF ( WAVE_COEFFS( 1, I ) .NE. ECH__BAD_DOUBLE .AND.
     :        WAVE_COEFFS( 1, I ) .NE. 0.0 ) THEN
            CALL ECH_FEVAL( ' ', MAXIMUM_POLY, WAVE_COEFFS( 1, I ),
     :           1, 1.0, TEST_START1, STATUS )
            CALL ECH_FEVAL( ' ', MAXIMUM_POLY, WAVE_COEFFS( 1, I ),
     :           1, FLOAT( NX ), TEST_END1, STATUS )
           IF ( TEST_START1 .GT. ABS_MIN_WAVELENGTH .AND.
     :          TEST_END1 .LT. ABS_MAX_WAVELENGTH  ) THEN
              COUNT = COUNT + 1
           END IF
         END IF
      END DO

*  Don't do anything if already done.
      IF ( ALREADY_DONE ) THEN
         GO TO 999
      END IF

*  Don't try to find order IDs if there aren't enough orders.
      IF ( COUNT .LT. 2 ) THEN
         IF ( .NOT. ALREADY_DONE ) STATUS = ECH__IDORD_TOOFEW
         GO TO 999
      END IF

*  Loop through orders up to penultimate.
      DO I = 1, N_ORDERS - 1

*     Zero counter for the order.
         COUNTS( I ) = 0
         MIN_DIFF = 1.0E20

*     Loop through remaining orders (i.e. above on the frame).
         DO II = I + 1, N_ORDERS

*        If both of the orders are active then.
            IF ( WAVE_COEFFS( 1, I ) .NE. ECH__BAD_DOUBLE .AND.
     :           WAVE_COEFFS( 1, I ) .NE. 0.0 .AND.
     :           WAVE_COEFFS( 1, II ) .NE. ECH__BAD_DOUBLE .AND.
     :           WAVE_COEFFS( 1, II ) .NE. 0.0 ) THEN

*           Estimate start/end range for order.
               CALL ECH_FEVAL( ' ', MAXIMUM_POLY, WAVE_COEFFS( 1, I ),
     :              1, 1.0, TEST_START1, STATUS )
               CALL ECH_FEVAL( ' ', MAXIMUM_POLY, WAVE_COEFFS( 1, I ),
     :              1, FLOAT( NX ), TEST_END1, STATUS )
               CALL ECH_FEVAL( ' ', MAXIMUM_POLY, WAVE_COEFFS( 1, II ),
     :              1, 1.0, TEST_START2, STATUS)
               CALL ECH_FEVAL( ' ', MAXIMUM_POLY, WAVE_COEFFS( 1, II ),
     :              1, FLOAT( NX ), TEST_END2, STATUS )

*           If range is acceptable then.
               IF ( TEST_START1 .GT. ABS_MIN_WAVELENGTH .AND.
     :              TEST_START2 .GT. ABS_MIN_WAVELENGTH .AND.
     :              TEST_END1 .LT. ABS_MAX_WAVELENGTH .AND.
     :              TEST_END2 .LT. ABS_MAX_WAVELENGTH ) THEN

*              Calculate ratio of wavelengths at middle of each order.
                  DELTA_ORDER = II - I
                  CALL ECH_FEVAL( ' ', MAXIMUM_POLY,
     :                 WAVE_COEFFS( 1, I ), 1, FLOAT( NX / 2 ),
     :                 VALUE1, STATUS )
                  CALL ECH_FEVAL( ' ', MAXIMUM_POLY,
     :                 WAVE_COEFFS( 1, II ), 1, FLOAT( NX / 2 ),
     :                 VALUE2, STATUS )
                  WAVE_RATIO = VALUE1 / VALUE2

*              Initialise order number estimate to first loop counter.
                  IT = I

*              Check for inverted reference frame, and report suspicion.
                  IF ( WAVE_RATIO .LT. 1.0 ) THEN
                     IF ( .NOT. REVERSE ) THEN
                        CALL ECH_REPORT( 0,
     :              ' Wavelength calibration image is upside-down.' )
                        REVERSE = .TRUE.
                     END IF
                     WAVE_RATIO = 1.0 / WAVE_RATIO
                     IT = II
                  END IF

*              Loop through all credible order numbers
*              (1 to max_allowed_ordnum).
                  DO III = 1, MAX_ALLOWED_ORDNUM

*                 Calculate expected ratio (as above), assuming first
*                 loops' order number is test order number
                     TEST_RATIO = FLOAT( III + DELTA_ORDER ) /
     :                            FLOAT( III )

*                 If calculated ratio for test order number is closest
*                 yet to that calculated from actual calibrated orders then
*                 save it as most likely candidate yet.
                     IF ( ABS( TEST_RATIO-WAVE_RATIO ) .LT.
     :                    MIN_DIFF ) THEN
                        MIN_DIFF = ABS( TEST_RATIO-WAVE_RATIO )
                        POSSIBLE_ORDER( IT ) = III
                     END IF
                  END DO

*              If we have an estimated order number and also
*              a previous one then.
                  IF ( ORDER_ID_NUMBER( IT ) .GT. 0 .AND.
     :                 POSSIBLE_ORDER( IT ) .GT. 0 ) THEN

*                 If estimate and previous value differ, report it.
                     IF ( POSSIBLE_ORDER( IT ) .NE.
     :                    ORDER_ID_NUMBER( IT ) ) THEN
                        CALL CHR_ITOC( IT, REF_STR1, NCHAR1 )
                        CALL CHR_ITOC( POSSIBLE_ORDER( IT ),
     :                       REF_STR2, NCHAR2 )
                        REPORT_STRING =
     :                        ' Revised order number: Order (' //
     :                        REF_STR1( :NCHAR1 ) // ')=' //
     :                        REF_STR2( :NCHAR2 ) // '.'
                        CALL ECH_REPORT( 0, REPORT_STRING )

*                 Else remember it and increment counter of stable estimates.
                     ELSE
                        ORDER_ID_NUMBER( IT ) = POSSIBLE_ORDER( IT )
                        COUNTS( IT ) = COUNTS( IT ) + 1
                     END IF

*              Else remember it and increment counter of stable estimates.
                  ELSE
                     ORDER_ID_NUMBER( IT ) = POSSIBLE_ORDER( IT )
                     COUNTS( IT ) = COUNTS( IT ) + 1
                  END IF

*              Calculate corresponding 'first order' (ie lowest on frame)
*              assuming the best estimate above was correct.
                  IF ( REVERSE ) THEN
                     FIRST_ORDER = POSSIBLE_ORDER( IT ) + IT - 1

                  ELSE
                     FIRST_ORDER = POSSIBLE_ORDER( IT ) - IT + 1
                  END IF

*              Loop through all orders calculating corresponding
*              start/end wavelengths etc.
                  DO III = 1, N_ORDERS
                     IF ( REVERSE ) THEN
                        POSSIBLE_ORDER( III ) = FIRST_ORDER - III + 1

                     ELSE
                        POSSIBLE_ORDER( III ) = FIRST_ORDER + III - 1
                     END IF
                     CALL ECH_FEVAL( ' ',
     :                      MAXIMUM_POLY, WAVE_COEFFS( 1, IT ),
     :                      1, 1.0, VALUE1, STATUS )
                     START_SEARCH_WAVE( III )  =
     :                      FLOAT( POSSIBLE_ORDER( IT ) ) /
     :                      FLOAT( POSSIBLE_ORDER( III ) ) * VALUE1
                     CALL ECH_FEVAL ( ' ',
     :                      MAXIMUM_POLY,WAVE_COEFFS(1,IT),
     :                      1, FLOAT(NX), VALUE1, STATUS)
                     END_SEARCH_WAVE( III )  =
     :                      FLOAT( POSSIBLE_ORDER( IT ) ) /
     :                      FLOAT( POSSIBLE_ORDER( III ) ) * VALUE1

                     IF ( START_SEARCH_WAVE ( III ) .GT.
     :                    END_SEARCH_WAVE ( III ) ) THEN
                        TEMP = START_SEARCH_WAVE( III )
                        START_SEARCH_WAVE( III ) =
     :                                  END_SEARCH_WAVE( III )
                        END_SEARCH_WAVE( III ) = TEMP
                     END IF
                     DELTA_LAMBDA = END_SEARCH_WAVE( III ) -
     :                            START_SEARCH_WAVE( III )
                     START_SEARCH_WAVE( III )  =
     :                     START_SEARCH_WAVE( III ) - DELTA_LAMBDA /
     :                     LEEWAY_FACTOR

                     END_SEARCH_WAVE( III )  =
     :                     END_SEARCH_WAVE( III ) + DELTA_LAMBDA /
     :                     LEEWAY_FACTOR
                  END DO
               END IF
            END IF
         END DO
      END DO

*  Report calculated ranges.
      CALL ECH_REPORT( 0,
     :     ' Calculated order search range specifications:' )
      DO I = 1, N_ORDERS
         CALL CHR_ITOC( I, REF_STR1, NCHAR1 )
         CALL CHR_ITOC( POSSIBLE_ORDER( I ), REF_STR2, NCHAR2 )
         CALL CHR_RTOC( START_SEARCH_WAVE( I ), REF_STR3, NCHAR3 )
         CALL CHR_RTOC( END_SEARCH_WAVE( I ), REF_STR4, NCHAR4 )
         REPORT_STRING = ' Order ' // REF_STR1( :NCHAR1 ) //
     :         ' is ' // REF_STR2( :NCHAR2 ) //
     :         ': Search range ' // REF_STR3( :NCHAR3 ) //
     :         '-' // REF_STR4( :NCHAR4 ) // ' ' //
     :         WAVELENGTH_UNITS( :CHR_LEN( WAVELENGTH_UNITS ) ) //
     :         '.'
         CALL ECH_REPORT( 0, REPORT_STRING )
         ORDER_ID_NUMBER( I ) = POSSIBLE_ORDER( I )
      END DO

*  If interactively calibrating then.
      IF ( WVCAL_INTERACT ) THEN

*     Inquire wether the ranges are acceptable.
         CALL ECH_GET_PARAMETER(
     :        'INSTANT-PROMPT=Are these acceptable', 'LOGICAL',
     :        0., ANSWER, ' ', 0, STATUS )

*  Else accept them by default.
      ELSE
         ANSWER = .TRUE.

*     BUT Check calculated order numbers are reasonable.
         DO I = 1, N_ORDERS
            IF ( POSSIBLE_ORDER( I ) .LT. 10 ) ANSWER = .FALSE.
            IF ( POSSIBLE_ORDER( I ) .GT.
     :         MAX_ALLOWED_ORDNUM ) ANSWER = .FALSE.
         END DO
         DO I = 1, N_ORDERS-1
            IF ( ABS( POSSIBLE_ORDER( I + 1 ) -
     :           POSSIBLE_ORDER( I ) ) .GT. 1 ) ANSWER = .FALSE.
         END DO

*     Report if we had to disable the order numbers
         IF ( .NOT. ANSWER  )
     :      CALL ECH_REPORT( 0,
     :           ' Inconsistent order numbers: not used.' )
      END IF

*  If new order numbers have NOT been accepted then clear order_descrip array.
      IF ( .NOT. ANSWER ) THEN
         DO I = 1, N_ORDERS
            ORDER_ID_NUMBER( I ) = 0
            START_SEARCH_WAVE( I ) = ECH__BAD_REAL
            END_SEARCH_WAVE( I ) = ECH__BAD_REAL
         END DO
         STATUS = ECH__IDORD_REJECTED

*  Else interpolate wave scales for any previously unknown orders.
      ELSE
         DO I = 1, N_ORDERS
            IF ( WAVE_COEFFS( 1,I ) .NE. ECH__BAD_DOUBLE .AND.
     :           WAVE_COEFFS( 1,I ) .NE. 0.0 ) THEN
               CONTINUE
            ELSE
               DO II = 1, MAXIMUM_POLY
                  WAVE_COEFFS( II, I ) = 0.0
               END DO
               DELTA_LAMBDA = END_SEARCH_WAVE( I ) -
     :               START_SEARCH_WAVE( I )
               WAVE_COEFFS( 1, I ) = START_SEARCH_WAVE( I ) +
     :               ( 1. / ( LEEWAY_FACTOR + 2 ) ) * DELTA_LAMBDA
               WAVE_COEFFS( 2, I ) = DELTA_LAMBDA /
     :               ( 1 + 2 / LEEWAY_FACTOR ) / FLOAT( NX )
            END IF
         END DO
      END IF

  999 CONTINUE

      END
