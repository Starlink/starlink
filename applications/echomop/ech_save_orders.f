      SUBROUTINE ECH_SAVE_ORDERS(
     :           X_DIMEN,
     :           Y_DIMEN,
     :           TYPE_OF_SPECTRUM,
     :           DO_XAXIS,
     :           DO_YAXIS,
     :           X_AXIS,
     :           Y_AXIS,
     :           SPECTRUM,
     :           ERRORS,
     :           X_LABEL,
     :           Y_LABEL,
     :           Z_LABEL,
     :           X_DATA,
     :           Y_DATA,
     :           Z_DATA,
     :           DATA_ERRORS,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_SAVE_ORDERS

*  Purpose:
*     Save spectra to result file.

*  Description:
*     This routine handles saving spectra to results files.

*  Invocation:
*      CALL ECH_SAVE_ORDERS(
*     :     X_DIMEN,
*     :     Y_DIMEN,
*     :     TYPE_OF_SPECTRUM,
*     :     DO_XAXIS,
*     :     DO_YAXIS,
*     :     X_AXIS,
*     :     Y_AXIS,
*     :     SPECTRUM,
*     :     ERRORS,
*     :     X_LABEL,
*     :     Y_LABEL,
*     :     Z_LABEL,
*     :     X_DATA,
*     :     Y_DATA,
*     :     Z_DATA,
*     :     DATA_ERRORS,
*     :     STATUS
*     :    )

*  Arguments:
*     X_DIMEN = INTEGER (Given)
*        Number of columns in frame.
*     Y_DIMEN = INTEGER (Given)
*        Number of orders in echellogram.
*     X_UNITS = CHAR (Given)
*        Units for X axis data object.
*     X_LABEL = CHAR (Given)
*        Label for X axis data object.
*     Y_LABEL = CHAR (Given)
*        Label for Y axis data object.
*     Z_LABEL = CHAR (Given)
*        Label for Z axis data object.
*     X_DATA = REAL (Given)
*        Data for X object (wavelengths).
*     Y_DATA = INTEGER (Given)
*        Data for Y object (order numbers).
*     Z_DATA = REAL (Given)
*        Data for Z  object (arc spectrum).
*     TYPE_OF_SPECTRUM = CHAR (Given)
*        Type of spectrum to save (1d,2d,object,arc etc).
*     DO_XAXIS = LOGICAL (Given)
*        TRUE if x axis values required.
*     DO_YAXIS = LOGICAL (Given)
*        TRUE if y axis values required.
*     X_AXIS = DOUBLE PRECISION (Given and Returned)
*        X-axis values.
*     Y_AXIS = INTEGER (Given and Returned)
*        Y-axis values.
*     SPECTRUM = REAL (Given and Returned)
*        Input order spectrum.
*     ERRORS = REAL (Given and Returned)
*        Calculated object pixel errors.
*     DATA_ERRORS = FLOAT (Given and Returned)
*        Variances.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     17-APR-1997 (MJC):
*       Removed ASCII handling code to ECH_SAVE_ASCII.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'

*  Arguments Given:
      INTEGER X_DIMEN
      INTEGER Y_DIMEN
      LOGICAL DO_XAXIS
      LOGICAL DO_YAXIS
      CHARACTER*( * ) X_LABEL
      CHARACTER*( * ) Y_LABEL
      CHARACTER*( * ) Z_LABEL
      REAL X_DATA( X_DIMEN, Y_DIMEN )
      INTEGER Y_DATA( Y_DIMEN )
      CHARACTER*( * ) TYPE_OF_SPECTRUM
      REAL Z_DATA( X_DIMEN, Y_DIMEN )

*  Arguments Returned:
      INTEGER Y_AXIS( Y_DIMEN )
      REAL SPECTRUM( X_DIMEN, Y_DIMEN )
      REAL ERRORS( X_DIMEN, Y_DIMEN )
      REAL DATA_ERRORS( X_DIMEN, Y_DIMEN )
      DOUBLE PRECISION X_AXIS( X_DIMEN, Y_DIMEN )

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER I
      INTEGER IX

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

      IF ( DO_XAXIS ) THEN
         DO_XAXIS = .FALSE.
         DO I = 1, Y_DIMEN
            DO IX = 1, X_DIMEN
               IF ( INT( X_AXIS( IX, I ) ) .NE. 0 ) DO_XAXIS = .TRUE.
            END DO
         END DO
      END IF

      IF ( DO_XAXIS ) X_LABEL = 'WAVELENGTH'
      IF ( DO_YAXIS ) Y_LABEL = 'ORDER NUMBER'
      Z_LABEL = TYPE_OF_SPECTRUM

      DO I = 1, Y_DIMEN
         IF ( DO_YAXIS ) Y_DATA( I ) = I
         DO IX = 1, X_DIMEN
            Z_DATA( IX, I ) = SPECTRUM( IX, I )
            DATA_ERRORS( IX, I ) = ERRORS( IX, I )
            IF ( DO_XAXIS ) THEN
               X_DATA( IX, I ) = REAL( X_AXIS( IX, I ) )

            ELSE
               X_DATA( IX, I ) = REAL( IX )
            END IF
         END DO
      END DO

      END
