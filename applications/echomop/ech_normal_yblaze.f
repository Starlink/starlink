      SUBROUTINE ECH_NORMAL_YBLAZE(
     :           NX,
     :           N_ORDERS,
     :           MEDIAN_BLAZE,
     :           BLZ_NPOLY,
     :           MAXIMUM_POLY,
     :           FITTED_WAVES,
     :           SPECTRUM,
     :           VARIANCE,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_NORMAL_YBLAZE

*  Purpose:
*     Normalise Y-blaze coefficients.

*  Description:
*     This routine normalises the Y-blaze coefficients.

*  Invocation:
*     CALL ECH_NORMAL_YBLAZE(
*     :    NX,
*     :    N_ORDERS,
*     :    MEDIAN_BLAZE,
*     :    BLZ_NPOLY,
*     :    MAXIMUM_POLY,
*     :    FITTED_WAVES,
*     :    SPECTRUM,
*     :    VARIANCE,
*     :    STATUS
*     :   )

*  Arguments:
*     N_ORDERS = INTEGER (Given)
*        Number of orders.
*     MEDIAN_BLAZE = REAL (Given and Returned)
*        Medians of X blazes.
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     BLZ_NPOLY = INTEGER (Given)
*        Number of coefficients for blaze fit.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum number of fit coefficients allowed.
*     FITTED_WAVES = FLOAT (Given)
*        Fitted wavelengths.
*     SPECTRUM = REAL (Given and Returned)
*        Input order spectrum.
*     VARIANCE = REAL (Given and Returned)
*
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
      INCLUDE 'ECH_DATA_CONSTRAINTS.INC'

*  Arguments Given:
      INTEGER N_ORDERS
      INTEGER NX
      INTEGER BLZ_NPOLY
      DOUBLE PRECISION FITTED_WAVES( NX, N_ORDERS )
      INTEGER MAXIMUM_POLY

*  Arguments Returned:
      REAL MEDIAN_BLAZE( N_ORDERS )
      REAL SPECTRUM( NX, N_ORDERS )
      REAL VARIANCE( NX, N_ORDERS )

*  Status:
      INTEGER STATUS

*  Local Variables:
      DOUBLE PRECISION X_DATA( MAX_ALLOWED_ORDERS * 2 )
      DOUBLE PRECISION SLOPE( MAX_ALLOWED_ORDERS * 2 )
      DOUBLE PRECISION COEFFS( 100 )

      REAL WEIGHT( MAX_ALLOWED_ORDERS * 2 )
      REAL THRESH
      REAL FITTED_SLOPE
      REAL FROM, TO
      REAL MEAN

      INTEGER I
      INTEGER II
      INTEGER N_COEFFS
      INTEGER NREJ
      INTEGER NCHAR1
      INTEGER NCHAR2

      LOGICAL STANDARD

      CHARACTER*32 REF_STR1
      CHARACTER*32 REF_STR2

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) )  RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT ( REPORT_MODE, ECH__MOD_ENTRY )

      IF ( BLZ_NPOLY .GT. 0 ) THEN
         CALL ECH_REPORT( 0,' Fitting splines to order slopes.' )
         CALL ECH_REPORT( 0,
     :        ' Set BLZ_NPOLY=0 to use measured slopes instead.' )

      ELSE
         CALL ECH_REPORT( 0,
     :        ' Using measured order slopes: no fitting.' )
      END IF

      STANDARD = .FALSE.
      IF ( N_ORDERS .GT. 1 ) THEN
         DO I = 1, N_ORDERS
            IF ( FITTED_WAVES( 1, I ) .EQ. 0.0 ) THEN
               CALL ECH_REPORT( 0,
     :              ' Not all orders have wavelength scales.' )
               CALL ECH_REPORT( 0,
     :              ' Assuming wavelength increases left to right,' )
               CALL ECH_REPORT ( 0,
     :              ' and that wavelength increases up the frame.' )
               STANDARD = .TRUE.
            END IF
         END DO

         IF ( .NOT. STANDARD ) THEN
            IF ( ABS( FITTED_WAVES( NX, 1 )- FITTED_WAVES( 1, 2 ) ) .LT.
     :           ABS( FITTED_WAVES( 1, 1 ) - FITTED_WAVES( NX, 2 ) ) )
     :           THEN
               STANDARD = .TRUE.

            ELSE
               STANDARD = .FALSE.
            END IF
         END IF

         IF ( FITTED_WAVES( NX, 1 ) .LT. FITTED_WAVES( 1, 1 ) )
     :      STANDARD = .NOT. STANDARD
         II = 0
         IF ( STANDARD ) THEN
            DO I = 2, N_ORDERS
               CALL ECH_MEAN_MEDIAN( 10, SPECTRUM( NX - 10, I - 1 ),
     :              .TRUE., .FALSE., FROM, STATUS )
               CALL ECH_MEAN_MEDIAN( 10, SPECTRUM( 1, I ), .TRUE.,
     :              .FALSE., TO, STATUS )
               IF ( FROM .EQ. 0.0 ) FROM = 1.0
               IF ( TO .EQ. 0.0 ) TO = 1.0
               MEAN = SQRT( FROM * TO )
               II = II + 1
               SLOPE( II ) = MEAN / FROM
               II = II + 1
               SLOPE( II ) = TO / MEAN
            END DO

         ELSE
            DO I = 2, N_ORDERS
               CALL ECH_MEAN_MEDIAN( 10, SPECTRUM( 1, I - 1 ), .TRUE.,
     :              .FALSE., FROM, STATUS )
               CALL ECH_MEAN_MEDIAN( 10, SPECTRUM( NX - 10, I ), .TRUE.,
     :              .FALSE., TO, STATUS )
               IF ( FROM .EQ. 0.0 ) FROM = 1.0
               IF ( TO .EQ. 0.0 ) TO = 1.0
               MEAN = SQRT( FROM * TO )
               II = II + 1
               SLOPE( II ) = MEAN / FROM
               II = II + 1
               SLOPE( II ) = TO / MEAN
            END DO
         END IF

         DO I = 1, II
            WEIGHT( I ) = ABS( REAL( SLOPE( I ) ) ) / 10.0
            X_DATA( I ) = FLOAT( I )
         END DO

         IF ( BLZ_NPOLY .GT. 0 ) THEN
            N_COEFFS = MIN( 20, BLZ_NPOLY )
            NREJ = 1
            THRESH = 4.0
            CALL ECH_FITTER( 'SPLINE', N_COEFFS, COEFFS, II, X_DATA,
     :           SLOPE, WEIGHT, NREJ, THRESH, STATUS )
            DO I = 1, N_ORDERS
               CALL ECH_FEVAL( 'SPLINE', N_COEFFS, COEFFS, 1,
     :              FLOAT( I ), FITTED_SLOPE, STATUS )
               CALL CHR_ITOC( I, REF_STR1, NCHAR1 )
               CALL CHR_RTOC( FITTED_SLOPE, REF_STR2, NCHAR2 )
               REPORT_STRING = ' Order ' // REF_STR1( :NCHAR1 ) //
     :               ' has a fitted slope of ' // REF_STR2( :NCHAR2 ) //
     :               '.'
               CALL ECH_REPORT( 0, REPORT_STRING )
               MEDIAN_BLAZE( I ) = FITTED_SLOPE
            END DO

         ELSE
            DO I = 1, N_ORDERS
               FITTED_SLOPE = REAL( SLOPE( 2 * I ) )
               CALL CHR_ITOC( I, REF_STR1, NCHAR1 )
               CALL CHR_RTOC( FITTED_SLOPE, REF_STR2, NCHAR2 )
               REPORT_STRING = ' Order ' // REF_STR1( :NCHAR1 ) //
     :               ' has a measured slope of ' //
     :               REF_STR2( :NCHAR2 ) // '.'
               CALL ECH_REPORT( 0, REPORT_STRING )
               MEDIAN_BLAZE( I ) = FITTED_SLOPE
            END DO
         END IF

      ELSE
         CALL ECH_REPORT( 0, ' Not enough orders to fit Y-blaze.' )
      END IF

      END
