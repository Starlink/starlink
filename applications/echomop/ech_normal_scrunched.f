      SUBROUTINE ECH_NORMAL_SCRUNCHED( NX, N_ORDERS, MEDIAN_BLAZE,
     :           FITTED_BLAZE, MAXIMUM_POLY, FITTED_WAVES, SPECTRUM,
     :           VARIANCE, STATUS )
*+
*  Name:
*     ECHOMOP - ECH_NORMAL_SCRUNCHED

*  Purpose:
*     Normlise Y-blaze coefficients.

*  Description:
*     This routine normalises the Y-blaze coefficients.

*  Invocation:
*      CALL ECH_NORMAL_SCRUNCHED( NX, N_ORDERS, MEDIAN_BLAZE,
*     :     FITTED_BLAZE, MAXIMUM_POLY, FITTED_WAVES, SPECTRUM,
*     :     VARIANCE, STATUS )

*  Arguments:
*     N_ORDERS = INTEGER (Given)
*        Number of orders.
*     MEDIAN_BLAZE = REAL (Given and Returned)
*        Medians of x blazes.
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     FITTED_BLAZE = FLOAT (Given)
*        Fitted blaze function.
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

*  Arguments Given:
      INTEGER N_ORDERS
      INTEGER NX
      DOUBLE PRECISION FITTED_WAVES( NX, N_ORDERS )
      INTEGER MAXIMUM_POLY
      REAL FITTED_BLAZE( NX, N_ORDERS )

*  Arguments Returned:
      REAL MEDIAN_BLAZE( N_ORDERS )
      REAL SPECTRUM( NX, N_ORDERS )
      REAL VARIANCE( NX, N_ORDERS )

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL FROM
      REAL TO
      REAL MEANB

      INTEGER I
      INTEGER IX

      LOGICAL STANDARD

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

      IF ( N_ORDERS .GT. 1 ) THEN
         DO I = 1, N_ORDERS
            IF ( FITTED_WAVES( 1, I ) .EQ. 0.0 ) THEN
               CALL ECH_REPORT( 0,
     :            ' All orders must have wavelength scales.' )
               RETURN
            END IF
         END DO

         IF ( ABS( FITTED_WAVES( NX, 1 ) - FITTED_WAVES( 1, 2 ) ) .LT.
     :        ABS( FITTED_WAVES( 1, 1 ) - FITTED_WAVES( NX, 2 ) ) ) THEN
            STANDARD = .TRUE.

         ELSE
            STANDARD = .FALSE.
         END IF

         IF ( FITTED_WAVES( NX, 1 ) .LT. FITTED_WAVES( 1, 1 ) ) THEN
            STANDARD = .NOT. STANDARD
         END IF
         IF ( STANDARD ) THEN
            DO I = 2, N_ORDERS
               CALL ECH_MEAN_MEDIAN( 10, SPECTRUM( NX - 10, I - 1 ),
     :              .TRUE., .FALSE., FROM, STATUS )
               CALL ECH_MEAN_MEDIAN( 10, SPECTRUM( 1, I ), .TRUE.,
     :              .FALSE., TO, STATUS )
               IF ( FROM .GT. 0.0 .AND. TO .GT. 0.0 ) THEN
                  MEANB = TO / FROM
                  DO IX = 1, NX
                     SPECTRUM( IX, I ) = SPECTRUM( IX, I ) / MEANB
                     VARIANCE( IX, I ) = VARIANCE( IX, I ) / MEANB
                  END DO
               END IF
            END DO

         ELSE
            DO I = 2, N_ORDERS
               CALL ECH_MEAN_MEDIAN( 10, SPECTRUM( 1, I - 1 ), .TRUE.,
     :              .FALSE., FROM, STATUS )
               CALL ECH_MEAN_MEDIAN( 10, SPECTRUM( NX - 10, I ), .TRUE.,
     :              .FALSE., TO, STATUS )
               IF ( FROM .GT. 0.0 .AND. TO .GT. 0.0 ) THEN
                  MEANB = TO / FROM
                  DO IX = 1, NX
                     SPECTRUM( IX, I ) = SPECTRUM( IX, I ) / MEANB
                     VARIANCE( IX, I ) = VARIANCE( IX, I ) / MEANB
                  END DO
               END IF
            END DO
         END IF

         CALL ECH_REPORT( 0,
     :        ' Y-blaze normalisation performed on merged spectrum.' )

      ELSE
         CALL ECH_REPORT( 0, ' Not enough orders to fit Y blaze.' )
      END IF

      END
