      SUBROUTINE ECH_CALC_TRACE(
     :           NX,
     :           MAXIMUM_POLY,
     :           COEFFS,
     :           X_TRACE_COORD,
     :           Y_TRACE_COORD,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_CALC_TRACE

*  Purpose:
*     Calculate order path across frame.

*  Description:
*    This routine calculates the path of an order across the frame using
*    a fit (defined by coefficients). It fills the output arrays
*    with pairs of x and y coordinates.

*  Invocation:
*     CALL ECH_CALC_TRACE(
*     :    NX,
*     :    MAXIMUM_POLY,
*     :    COEFFS,
*     :    X_TRACE_COORD,
*     :    Y_TRACE_COORD,
*     :    STATUS
*     :    )

*  Arguments:
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum number of coeffs allowed.
*     COEFFS = DOUBLE (Given)
*        Fit coefficients.
*     X_TRACE_COORD = DOUBLE (Returned)
*        Calculated x coordinates.
*     Y_TRACE_COORD = DOUBLE (Returned)
*        Calculated y coordinates.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     Determine number of coeffs in active fit
*     If fit active then
*        Generate a series of x,y coordinates using the fit
*        for order 'iord'. The points will be seperated by 1 pixel
*        and will be located at x values of ???.5 .
*     Endif

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     10-JUL-1996 (MJC):
*       Tidied, added prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER MAXIMUM_POLY
      DOUBLE PRECISION COEFFS( MAXIMUM_POLY ) ! Fit coefficients.

*  Arguments Returned:
      DOUBLE PRECISION X_TRACE_COORD( NX )
      DOUBLE PRECISION Y_TRACE_COORD( NX )

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER NFTPOLY
      INTEGER I

      CHARACTER*6 FITTER

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Determine number of coeffs in active fit.
      FITTER = 'POLY'
      NFTPOLY = 0
      DO I = 1, MAXIMUM_POLY
         IF ( COEFFS( I ) .NE. 0.0 ) NFTPOLY = I
      END DO

*  If fit active.
      IF ( COEFFS( 1 ) .NE. ECH__BAD_DOUBLE .AND. NFTPOLY .GT. 0 ) THEN
         IF ( NFTPOLY .GT. 15 ) THEN
            FITTER = 'SPLINE'
         ELSE
            DO I = 1, NFTPOLY - 1
               IF ( ABS( COEFFS( I + 1 ) ) .GT.
     :              ABS( COEFFS( I ) ) ) FITTER = 'SPLINE'
            END DO
         ENDIF
         IF ( FITTER .EQ. 'SPLINE' ) NFTPOLY = NFTPOLY + 4

*     Initialise X array to pixel indices.
         DO I = 1, NX
            X_TRACE_COORD( I ) = DBLE( I )
         END DO

*     Generate Y-coordinates using the fit supplied.
         CALL ECH_DFEVAL( FITTER, NFTPOLY, COEFFS,
     :        NX, X_TRACE_COORD, Y_TRACE_COORD, STATUS )
      ENDIF

      END

      SUBROUTINE ECH_CALC_TRACE_AT_X(
     :           MAXIMUM_POLY,
     :           COEFFS,
     :           X_COORD,
     :           Y_COORD,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_CALC_TRACE_AT_X

*  Purpose:
*     Calculate order Y position at specific X.

*  Description:
*     This routine calculates the Y coordinate of an order trace
*     at a given X coordinate.

*  Invocation:
*     CALL ECH_CALC_TRACE_AT_X(
*     :    MAXIMUM_POLY,
*     :    COEFFS,
*     :    X_COORD,
*     :    Y_COORD,
*     :    STATUS
*     :    )

*  Arguments:
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum number of coeffs allowed.
*     COEFFS = DOUBLE (Given)
*        Fit coefficients.
*     X_COORD = DOUBLE (Given)
*        X-coordinate.
*     Y_COORD = DOUBLE (Returned)
*        Calculated Y-coordinate.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Authors:
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     24-MAR-1997 (MJC):
*       Original based on ECH_CALC_TRACE
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'

*  Arguments Given:
      INTEGER MAXIMUM_POLY
      DOUBLE PRECISION COEFFS( MAXIMUM_POLY ) ! Fit coefficients.
      DOUBLE PRECISION X_COORD

*  Arguments Returned:
      DOUBLE PRECISION Y_COORD

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER NFTPOLY
      INTEGER I

      CHARACTER*6 FITTER

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Determine number of coeffs in active fit.
      FITTER = 'POLY'
      NFTPOLY = 0
      DO I = 1, MAXIMUM_POLY
         IF ( COEFFS( I ) .NE. 0.0 ) NFTPOLY = I
      END DO

*  If fit active.
      IF ( COEFFS( 1 ) .NE. ECH__BAD_DOUBLE .AND. NFTPOLY .GT. 0 ) THEN
         IF ( NFTPOLY .GT. 15 ) THEN
            FITTER = 'SPLINE'

         ELSE
            DO I = 1, NFTPOLY - 1
               IF ( ABS( COEFFS( I + 1 ) ) .GT.
     :              ABS( COEFFS( I ) ) ) FITTER = 'SPLINE'
            END DO
         ENDIF
         IF ( FITTER .EQ. 'SPLINE' ) NFTPOLY = NFTPOLY + 4

*     Generate Y-coordinate using the fit supplied.
         CALL ECH_DFEVAL( FITTER, NFTPOLY, COEFFS, 1, X_COORD,
     :        Y_COORD, STATUS )
      ENDIF

      END
