      SUBROUTINE ECH_MODEL_PROFILE(
     :           IMAGE,
     :           QUALITY,
     :           NX,
     :           NY,
     :           ORDER_NUMBER,
     :           N_ORDERS,
     :           MAXIMUM_POLY,
     :           TRACE_POLYNOMIAL,
     :           PFL_SUBSAMPLES,
     :           MAX_SKY_PIXELS,
     :           USE_NXF,
     :           DEK_BELOW,
     :           DEK_ABOVE,
     :           OBJ_MASK,
     :           SKY_MODEL,
     :           MODEL_PROFILE,
     :           X_TRACE_COORD,
     :           Y_TRACE_COORD,
     :           TOTAL,
     :           TCOUNT,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_MODEL_PROFILE

*  Purpose:
*     Calculate a model order profile (spatial).

*  Invocation:
*     CALL ECH_MODEL_PROFILE(
*     :    IMAGE,
*     :    QUALITY,
*     :    NX,
*     :    NY,
*     :    ORDER_NUMBER,
*     :    N_ORDERS,
*     :    MAXIMUM_POLY,
*     :    TRACE_POLYNOMIAL,
*     :    PFL_SUBSAMPLES,
*     :    MAX_SKY_PIXELS,
*     :    USE_NXF,
*     :    DEK_BELOW,
*     :    DEK_ABOVE,
*     :    OBJ_MASK,
*     :    SKY_MODEL,
*     :    MODEL_PROFILE,
*     :    X_TRACE_COORD,
*     :    Y_TRACE_COORD,
*     :    TOTAL,
*     :    TCOUNT,
*     :    STATUS
*     :   )

*  Arguments:
*     IMAGE = REAL (Given)
*        Input frame image of dimensions nx columns and ny rows.
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     TRACE_POLYNOMIAL = REAL (Given)
*        Polynomial coefficients tracing the orders.
*     DEK_BELOW = INTEGER (Given and Returned)
*        Lower dekker limit in pixels from trace.
*     DEK_ABOVE = INTEGER (Given and Returned)
*        Upper dekker limit in pixels from trace.
*     MAX_SKY_PIXELS = INTEGER (Given and Returned)
*        Size of mask arrays.
*     OBJ_MASK = INTEGER (Returned)
*        Non zero values denoting object presence.
*     SKY_MODEL = REAL (Given)
*        Modeled sky intensities at offsets from trace.
*     PFL_SUBSAMPLES = INTEGER (Given and Returned)
*        Number of subsamples to use for profiles.
*     MODEL_PROFILE = REAL (Given and Returned)
*        Profile.
*     FIT_WORK_XINT = INTEGER (Temporary Workspace)
*        Workspace array for NAG routine in fitter.
*     FIT_WORK_XDOUBLE = DOUBLE (Temporary Workspace)
*        Workspace array for NAG routine in fitter.
*     FIT_WORK_3XDOUBLE = DOUBLE (Temporary Workspace)
*        Workspace array for NAG routine in fitter.
*     QUALITY = LOGICAL (Given)
*        Input data frame quality flags array.
*     ORDER_NUMBER = INTEGER (Given)
*        Number of order being processed.
*     N_ORDERS = INTEGER (Given)
*        Number of orders in echellogram.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum number of fit coefficients allowed.
*     USE_NXF = FLOAT (Given)
*        Fraction of total columns to use
*        A value greater than one signifies that each order is to be
*        modelled separately.  The USE_NXF value is then the fractional part.
*     X_TRACE_COORD = DOUBLE (Temporary Workspace)
*        X coords of order trace path.
*     Y_TRACE_COORD = DOUBLE (Temporary Workspace)
*        Y coords of order trace path.
*     TOTAL = REAL (Given and Returned)
*        Total fluxes per order and column.
*     TCOUNT = INTEGER (Given and Returned)
*        Count of pixels used per increment.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     Estimate the spatial extent in pixels
*     Set appropriate subsampling rate
*     Report subsampling rate used
*     If specific order number provided then
*        Setup to process single order
*     Else process all orders together (global average)
*     Endif
*     Loop from lower dekker to upper dekker and count object pixels
*     Initialise profile to zero everywhere
*     Loop thru selected order(s)
*       If a good trace polynomial is avaliable for this order then
*         Calculate the order trace
*         Add good pixels to profile sum (for all x)
*       Endif
*     End loop
*     Calculate energies at substeps
*      Loop thru selected order(s)
*            Zero counts of contributing pixels
*            Loop thru image columns
*                Calculate exact y-coord (using trace polynomial)
*                Calculate offset from trace for pixel centre
*                Loop thru subsamples
*                   If a good pixel then calculate local profile and add to
*                   subsampled array
*                End loop
*            End loop
*            Normalise by dividing by counts of contributung pixels
*     End loop
*     If all order average then copy results into each orders profile

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

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      INTEGER N_ORDERS
      INTEGER MAXIMUM_POLY
      REAL IMAGE( NX, NY )
      BYTE QUALITY( NX, NY )
      DOUBLE PRECISION TRACE_POLYNOMIAL( MAXIMUM_POLY, N_ORDERS )
*           ! Polynomials to follow paths of orders.
      INTEGER DEK_BELOW
      INTEGER DEK_ABOVE
      INTEGER MAX_SKY_PIXELS
      REAL USE_NXF
      INTEGER ORDER_NUMBER
      INTEGER PFL_SUBSAMPLES

*  Arguments Returned:
      INTEGER OBJ_MASK( -MAX_SKY_PIXELS/2: MAX_SKY_PIXELS/2, N_ORDERS )
*          ! Non-zero where object pixels are.
      REAL SKY_MODEL( NX, -MAX_SKY_PIXELS/2:MAX_SKY_PIXELS/2, N_ORDERS )
*          ! Modelled sky intensities.
      REAL MODEL_PROFILE( -PFL_SUBSAMPLES/2:PFL_SUBSAMPLES/2, N_ORDERS )

*  Workspace variables used:
      REAL TOTAL( NX, N_ORDERS )
      INTEGER TCOUNT( NX, N_ORDERS )
      DOUBLE PRECISION X_TRACE_COORD( NX )
      DOUBLE PRECISION Y_TRACE_COORD( NX )

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER PROF_HWID
      PARAMETER ( PROF_HWID = 1000 )

*  Local Variables:
      REAL YCO
      REAL YCOSTEP
      REAL SAMPLE
      REAL VALUE

      INTEGER PCOUNT( -PROF_HWID : PROF_HWID )
      INTEGER CHECKY
      INTEGER I
      INTEGER II
      INTEGER IISTART
      INTEGER IIEND
      INTEGER NSTEP
      INTEGER SORD
      INTEGER EORD
      INTEGER ORDER_SIZE
      INTEGER SUBSTEPS
      INTEGER IOSTART
      INTEGER IOEND
      INTEGER IOFF
      INTEGER IORD
      INTEGER IY
      INTEGER OBJPIX
      INTEGER IIORD
      INTEGER OBJLOW
      INTEGER OBJHI
      INTEGER TTHRESH
      INTEGER IORDER_NUMBER
      INTEGER LORDER_NUMBER
      INTEGER UORDER_NUMBER
      INTEGER NCHAR

      CHARACTER*16 REF_STR1
      CHARACTER*16 REF_STR2

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
      INTEGER CHR_LEN
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Estimate the spatial extent in pixels, set appropriate subsampling rate.
      CALL ECH_CALC_PROFSAMP( NY, N_ORDERS, PFL_SUBSAMPLES, DEK_BELOW,
     :     DEK_ABOVE, ORDER_SIZE, SUBSTEPS, STATUS )
      YCOSTEP = 1.0 / FLOAT( SUBSTEPS )

      CALL CHR_ITOC( SUBSTEPS, REF_STR1, NCHAR )
      REPORT_STRING = ' Using ' // REF_STR1( :NCHAR ) //
     :      ' subsamples per pixel to sample profile.'
      CALL ECH_REPORT( 0, REPORT_STRING )

*  Zero-out profiles, sums and counts.
      CALL ECH_ZERO_REAL( N_ORDERS * PFL_SUBSAMPLES,
     :     MODEL_PROFILE( -PFL_SUBSAMPLES / 2, 1 ) )
      CALL ECH_ZERO_REAL( N_ORDERS * NX, TOTAL( 1, 1 ) )
      CALL ECH_ZERO_REAL( N_ORDERS * NX, TCOUNT( 1, 1 ) )

      IF ( ORDER_NUMBER .EQ. 0 .AND. USE_NXF .GE. 1.0 ) THEN
         LORDER_NUMBER = 1
         UORDER_NUMBER = N_ORDERS

      ELSE
         LORDER_NUMBER = 0
         UORDER_NUMBER = 0
      END IF

      DO IORDER_NUMBER = LORDER_NUMBER, UORDER_NUMBER
         IF ( IORDER_NUMBER .GT. 0 ) THEN
            SORD = IORDER_NUMBER
            EORD = IORDER_NUMBER
            CALL CHR_ITOC( SORD, REF_STR1, NCHAR )
            REPORT_STRING =
     :            ' Deriving independent profile for order ' //
     :            REF_STR1( :NCHAR ) // '.'
            CALL ECH_REPORT( 0, REPORT_STRING )

         ELSE
            SORD = 1
            EORD = N_ORDERS
            CALL ECH_REPORT( 0,
     :           ' Using all order information to derive profile.' )
         END IF

*     Find how many pixels in the spatial profile are "object".
         OBJPIX = 2
         OBJLOW = -999
         OBJHI = DEK_BELOW
         DO II = DEK_BELOW, DEK_ABOVE
            IF ( IORDER_NUMBER .GT. 0 ) THEN
               IF ( OBJ_MASK( II, SORD ) .NE. 0 ) THEN
                  OBJPIX = OBJPIX + 1
                  IF ( OBJLOW .EQ. -999 ) OBJLOW = II
                  IF ( II .GT. OBJHI ) OBJHI = II
               END IF

            ELSE
               DO IIORD = SORD, EORD
                  IF ( OBJ_MASK( II, IIORD ) .NE. 0 ) THEN
                     OBJPIX = OBJPIX + 1
                     IF ( OBJLOW .EQ. -999 ) OBJLOW = II
                     IF ( II .GT. OBJHI ) OBJHI = II
                     GO TO 100
                  END IF
               END DO
  100          CONTINUE
            END IF
         END DO
         OBJLOW = MAX( DEK_BELOW, OBJLOW - 1 )
         OBJHI = MIN( DEK_ABOVE, OBJHI + 1 )
         TTHRESH = OBJPIX * 2 / 3

*     Process each order in turn.
         DO IORD = SORD, EORD

*        Only process if a good trace polynomial is avaliable.
            IF ( TRACE_POLYNOMIAL( 1, IORD ) .NE. ECH__BAD_DOUBLE ) THEN

*           Calculate the order trace.
               CALL ECH_CALC_TRACE( NX, MAXIMUM_POLY,
     :              TRACE_POLYNOMIAL( 1, IORD ), X_TRACE_COORD,
     :              Y_TRACE_COORD, STATUS )

*           Find total flux in each X-step and number of
*           contributing pixels.
               DO I = NX, 1, -1
                  IY = INT( Y_TRACE_COORD( I ) + 0.5 ) + OBJLOW
                  IF ( IY .LT. 1 ) THEN
                     IISTART = OBJLOW + 1 - IY
                     IY = 1

                  ELSE
                     IISTART = OBJLOW
                  END IF
                  IF ( IY + OBJHI - OBJLOW + 1 .GT. NY ) THEN
                     IIEND = OBJHI + NY - IY - OBJHI + OBJLOW - 1

                  ELSE
                     IIEND = OBJHI
                  END IF
                  DO II = IISTART, IIEND
                     IF ( QUALITY( I, IY ) .EQ. 0 .AND.
     :                    IMAGE( I, IY ) .NE. ECH__BAD_REAL ) THEN
                        VALUE = IMAGE( I, IY ) -
     :                        SKY_MODEL( I, II, IORD )
                        IF ( VALUE .GT. 0.0 ) THEN
                           TOTAL( I, IORD ) = TOTAL( I, IORD ) +
     :                           VALUE
                           TCOUNT( I, IORD ) = TCOUNT( I, IORD ) + 1
                        END IF
                     END IF
                     IY = IY + 1
                  END DO
               END DO

               CALL ECH_ZERO_REAL( 2 * PROF_HWID + 1,
     :              PCOUNT( -PROF_HWID ) )

*           Calculate offset from trace for pixel centre at lower
*           edge of object.
               IOSTART = OBJLOW * SUBSTEPS
               IOEND = ( OBJHI - OBJLOW + 1 ) * SUBSTEPS

*           Sample the profile.
               DO I = NX, 1, -1
                  IF ( TCOUNT( I, IORD ) .GT. TTHRESH ) THEN

*                 Calculate exact Y-coord (using trace polynomial).
                     YCO = Y_TRACE_COORD( I ) + 0.5 + OBJLOW

*                 Check that pixels covered do not move off edge of
*                 image - limit range if they do.
                     IOFF = IOSTART
                     IIEND = IOEND
                     IF ( YCO .LT. 1.0 ) THEN
                        NSTEP = INT( ( 1.0 - YCO ) / YCOSTEP )
                        IF ( MOD( 1.0 - YCO, YCOSTEP ) .NE. 0.0 ) THEN
                           NSTEP = NSTEP + 1
                        END IF
                        IIEND = IIEND - NSTEP
                        YCO = YCO + NSTEP * YCOSTEP
                        IOFF = IOFF + NSTEP
                     END IF
                     IF ( YCO + YCOSTEP * IIEND - 1.0 .GE. NY ) THEN
                        NSTEP = INT( ( YCO + YCOSTEP * IIEND - 1.0
     :                        - NY ) / YCOSTEP ) + 1
                        IIEND = IIEND - NSTEP
                     END IF

*                 Set number of samples to process and initial Y-coord.
                     II = IIEND
                     IY = INT( YCO )

*                 Step along the profile until we reach a useable pixel.
                     DO WHILE ( ( QUALITY( I, IY ) .NE. 0 .OR.
     :                          IMAGE( I, IY ) .EQ. ECH__BAD_REAL )
     :                          .AND. II .GE. 0 )
                        YCO = YCO + YCOSTEP
                        IOFF = IOFF + 1
                        II = II - 1
                        IY = INT( YCO )
                     END DO
                     CHECKY = IY - 1

*                 Loop through subsamples.
                     DO WHILE ( II .GE. 0 )
                        IY = INT( YCO )

*                    Check for a new Y-coord.
                        GO TO ( 102 ), ( IY - CHECKY )

*                    (Fall through) it's the same Y-coord - process it.
  101                   SAMPLE = IMAGE( I, IY ) -
     :                        SKY_MODEL( I, IOFF / SUBSTEPS, IORD )
                        IF ( SAMPLE .GT. 0.0 ) THEN
                           MODEL_PROFILE( IOFF, IORD ) =
     :                           MODEL_PROFILE( IOFF, IORD ) +
     :                           SAMPLE / TOTAL( I, IORD )
                           PCOUNT( IOFF ) = PCOUNT( IOFF ) + 1
                        END IF
                        YCO = YCO + YCOSTEP
                        IOFF = IOFF + 1
                        II = II - 1
                        GO TO 103

*                    (New Y-coord) check this pixel.
  102                   IF ( QUALITY( I, IY ) .EQ. 0 .AND.
     :                       IMAGE( I, IY ) .NE. ECH__BAD_REAL ) THEN
                           CHECKY = IY
                           GO TO 101

*                    Move along to next whole pixel.
                        ELSE
                           YCO = YCO + 1.0
                           CHECKY = CHECKY + 1
                           IOFF = IOFF + SUBSTEPS
                           II = II - SUBSTEPS
                        END IF

  103                END DO
                  END IF
               END DO

*           Normalise by dividing by counts of contributing pixels.
               IOFF = IOSTART - 1
               DO I = ( OBJHI - OBJLOW + 1 ) * SUBSTEPS - 1, 1, -1
                  IOFF = IOFF + 1
                  MODEL_PROFILE( IOFF, IORD ) =
     :                  MODEL_PROFILE( IOFF, IORD ) /
     :                  FLOAT( MAX( 1, PCOUNT( IOFF ) ) )
               END DO

*           Display "fractional" profile.
               IF ( USE_NXF .LT. 1.0 ) THEN
                  IF ( IORD .EQ. SORD ) THEN
                     DO IOFF = OBJLOW, OBJHI
                        REPORT_STRING =
     :                     ' Average Object-pixel fraction at '
                        IF ( IOFF .LT. -1 ) THEN
                           CALL CHR_ITOC( -IOFF, REF_STR1, NCHAR )
                           REPORT_STRING = REPORT_STRING( :CHR_LEN(
     :                        REPORT_STRING ) + 1 ) //
     :                        REF_STR1( :NCHAR ) //
     :                        ' pixels below trace: '

                        ELSE IF ( IOFF .EQ. -1 ) THEN
                           REPORT_STRING = REPORT_STRING( :CHR_LEN(
     :                        REPORT_STRING ) ) //
     :                        ' 1 pixel below trace: '

                        ELSE IF ( IOFF .EQ. 0 ) THEN
                           REPORT_STRING = REPORT_STRING( :CHR_LEN(
     :                        REPORT_STRING ) ) //
     :                        ' trace centre: '

                        ELSE IF ( IOFF .EQ. 1 ) THEN
                           REPORT_STRING = REPORT_STRING( :CHR_LEN(
     :                        REPORT_STRING ) ) //
     :                        ' 1 pixel above trace: '
                        ELSE
                           CALL CHR_ITOC( IOFF, REF_STR1, NCHAR )
                           REPORT_STRING = REPORT_STRING( :CHR_LEN(
     :                        REPORT_STRING ) + 1 ) //
     :                        REF_STR1( :NCHAR ) //
     :                        ' pixels above trace: '
                        END IF
                        CALL CHR_RTOC( REAL( INT( 10000.0 *
     :                       MODEL_PROFILE( IOFF * SUBSTEPS, IORD ) ) )
     :                       / 100.0, REF_STR2, NCHAR )
                        REPORT_STRING = REPORT_STRING( :CHR_LEN(
     :                     REPORT_STRING ) + 1 ) //
     :                     REF_STR2( :NCHAR ) // '%'
                        CALL ECH_REPORT( 0, REPORT_STRING )
                     END DO
                  END IF

               ELSE
                  CALL CHR_RTOC( REAL( INT( 10000.0 *
     :                 MODEL_PROFILE( 0, IORD ) ) ) / 100.0, REF_STR1,
     :                 NCHAR )
                  REPORT_STRING =
     :             ' Average Object-pixel fraction at trace centre: ' //
     :             REF_STR1( :NCHAR ) // '%'
                  CALL ECH_REPORT( 0, REPORT_STRING )
               END IF

            ELSE
               CALL ECH_REPORT( 0, ' Order is disabled: no modelling.' )
            END IF
         END DO

*    If all-order average then copy results into each orders profile.
         IF ( ORDER_NUMBER .EQ. 0 .AND. USE_NXF .LT. 1.0 ) THEN
            DO IOFF = -PFL_SUBSAMPLES / 2, PFL_SUBSAMPLES / 2
               DO IORD = SORD + 1, EORD
                  MODEL_PROFILE( IOFF, SORD ) =
     :                  MODEL_PROFILE( IOFF, SORD ) +
     :                  MODEL_PROFILE( IOFF, IORD )
               END DO
               MODEL_PROFILE( IOFF, SORD ) =
     :               MODEL_PROFILE( IOFF, SORD ) /
     :               FLOAT( EORD - SORD + 1 )
            END DO
            DO IOFF = -PFL_SUBSAMPLES / 2, PFL_SUBSAMPLES / 2
               DO IORD = SORD + 1, EORD
                  MODEL_PROFILE( IOFF, IORD ) =
     :                  MODEL_PROFILE( IOFF, SORD )
               END DO
            END DO
         END IF
      END DO

      END
