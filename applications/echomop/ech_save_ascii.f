      SUBROUTINE ECH_SAVE_ASCII(
     :           OUTPUTFILE,
     :           X_DIMEN,
     :           Y_DIMEN,
     :           DO_XAXIS,
     :           DO_YAXIS,
     :           SPECTRUM,
     :           ERRORS,
     :           X_DATA,
     :           Y_DATA,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_SAVE_ASCII

*  Purpose:
*     Save spectra to ASCII file.

*  Description:
*     This routine saves spectra to ASCII files.

*  Invocation:
*      CALL ECH_SAVE_ASCII(
*     :     OUTPUTFILE,
*     :     X_DIMEN,
*     :     Y_DIMEN,
*     :     DO_XAXIS,
*     :     DO_YAXIS,
*     :     SPECTRUM,
*     :     ERRORS,
*     :     X_DATA,
*     :     Y_DATA,
*     :     STATUS
*     :    )

*  Arguments:
*     OUTPUTFILE = CHARACTER*( * ) (Given)
*        Name of the file to be written.
*     X_DIMEN = INTEGER (Given)
*        Number of columns in frame.
*     Y_DIMEN = INTEGER (Given)
*        Number of orders in echellogram.
*     X_UNITS = CHAR (Given)
*        Units for X axis data object.
*     X_DATA = REAL (Given)
*        Data for X object (wavelengths).
*     Y_DATA = INTEGER (Given)
*        Data for Y object (order numbers).
*     DO_XAXIS = LOGICAL (Given)
*        TRUE if x axis values required.
*     DO_YAXIS = LOGICAL (Given)
*        TRUE if y axis values required.
*     SPECTRUM = REAL (Given and Returned)
*        Input order spectrum.
*     ERRORS = REAL (Given and Returned)
*        Calculated object pixel errors.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Authors:
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     17-APR-1997 (MJC):
*       Original version, based on ECH_SAVE_ORDERS.
*       Uses supplied filename rather than hard-wired name.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'

*  Arguments Given:
      CHARACTER*( * ) OUTPUTFILE
      INTEGER X_DIMEN
      INTEGER Y_DIMEN
      LOGICAL DO_XAXIS
      LOGICAL DO_YAXIS
      REAL X_DATA( X_DIMEN, Y_DIMEN )
      INTEGER Y_DATA( Y_DIMEN )
      REAL SPECTRUM( X_DIMEN, Y_DIMEN )
      REAL ERRORS( X_DIMEN, Y_DIMEN )

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER I
      INTEGER IX
      INTEGER RLUN

      CHARACTER*128 OPENED_NAME

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

      CALL ECH_OPEN_FILE( OUTPUTFILE, 'TEXT', 'NEW',
     :     .FALSE., RLUN, OPENED_NAME, STATUS )

      DO I = 1, Y_DIMEN
         IF ( DO_YAXIS ) THEN
            WRITE ( RLUN, 1000 ) Y_DATA( I )
         END IF
         DO IX = 1, X_DIMEN
            WRITE ( RLUN, 1001 ) X_DATA( IX, I ),
     :            SPECTRUM( IX, I ), ERRORS( IX, I )
         END DO
      END DO

      CALL ECH_OPEN_FILE( ' ', 'CLOSE', ' ', .FALSE., RLUN,
     :     OPENED_NAME, STATUS )
      REPORT_STRING = ' ASCII format data table saved in ' //
     :      OUTPUTFILE( :CHR_LEN( OUTPUTFILE ) ) // '.'
      CALL ECH_REPORT( 0, REPORT_STRING )

 1000 FORMAT ( 1X, I4 )
 1001 FORMAT ( 1X, 1PE14.6, 1X, E12.4, 1X, E12.4 )

      END
