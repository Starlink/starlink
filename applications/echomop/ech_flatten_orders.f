      SUBROUTINE ECH_FLATTEN_ORDERS(
     :           NX,
     :           RESET,
     :           YBLAZE,
     :           BLAZE_SPECTRUM,
     :           NORMAL_MEDIAN,
     :           SPECTRUM,
     :           ERRORS,
     :           STATUS
     :           )
*+
*  Name:
*     ECHOMOP - ECH_FLATTEN_ORDERS

*  Purpose:
*     Divide fitted blaze function into object spectrum.
*
*  Description:
*     This routine flattens the orders for aesthetic effect by dividing by
*     a flat-field spectrum for the order.  The flat-field spectra will
*     usually be generated using the ECH_FIT_ORDER_BLAZE routine.

*  Invocation:
*     CALL ECH_FLATTEN_ORDERS(
*     :    NX,
*     :    RESET,
*     :    YBLAZE,
*     :    BLAZE_SPECTRUM,
*     :    NORMAL_MEDIAN,
*     :    SPECTRUM,
*     :    ERRORS,
*     :    STATUS
*     :   )

*  Arguments:
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     RESET = LOGICAL (Given)
*        TRUE if blaze funciton to be reset to unity.
*     YBLAZE = LOGICAL (Given)
*        TRUE if y-blaze correction required.
*     BLAZE_SPECTRUM = REAL (Returned)
*        Flattened order spectrum.
*     NORMAL_MEDIAN = REAL (Returned)
*        Normalised y-blaze coefficient.
*     SPECTRUM = REAL (Given and Returned)
*        Input order spectrum.
*     ERRORS = REAL (Given and Returned)
*        Input order spectrum errors.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     Divide each increment of order by blaze spectrum
*     Loop thru x values
*        If blaze at x is zero, set it = 1.0
*        Divide spectrum by blaze at x
*        Divide variance by blaze at x
*        If reset required, reset blaze to 1.0
*     End loop
*     If reset used then warn user

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
      INTEGER NX
      LOGICAL RESET
      LOGICAL YBLAZE

*  Arguments Returned:
      REAL SPECTRUM( NX )
      REAL BLAZE_SPECTRUM( NX )
      REAL NORMAL_MEDIAN
      REAL ERRORS( NX )

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER ii

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Divide each increment of order by blaze spectrum.
      DO II = 1, NX

*     If blaze at this X is zero, set it to unity.
         IF ( BLAZE_SPECTRUM( II ) .EQ. 0.0 ) THEN
            BLAZE_SPECTRUM( II ) = 1.0

         ELSE

*        Divide spectrum by blaze at this X.
            SPECTRUM( II ) = SPECTRUM( II ) / BLAZE_SPECTRUM( II )

*        Divide variance by blaze at X.
            ERRORS( II ) = ERRORS( II ) / BLAZE_SPECTRUM( II )

*        If reset required, reset blaze to 1.0
            IF ( RESET ) BLAZE_SPECTRUM( II ) = 1.0
         END IF
      END DO

*  If reset used then warn user.
      IF ( RESET ) THEN
           CALL ECH_REPORT( 0,
     :        ' Order de-blazed, blaze spectrum reset to unity.' )
      ELSE
         CALL ECH_REPORT( 0,
     :        ' Order de-blazed, blaze spectrum kept.' )
      END IF

      END
