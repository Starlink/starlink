      SUBROUTINE ECH_DFEVAL(
     :           TYPE,
     :           INP_COEFFS,
     :           COEFFS,
     :           N_POINTS,
     :           X_DATA,
     :           Y_FIT,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_DFEVAL

*  Purpose:
*     Select, apply and evaluate a fitting function (DOUBLE PRECISION result).

*  Invocation:
*     CALL ECH_DFEVAL(
*     :    TYPE,
*     :    INP_COEFFS,
*     :    COEFFS,
*     :    N_POINTS,
*     :    X_DATA,
*     :    Y_FIT,
*     :    STATUS
*     :   )

*  Arguments:
*    TYPE = CHAR (Given)
*       Type of fitting function.
*    INP_COEFFS = INTEGER (Given)
*       Number of input coefficients.
*    COEFFS = DOUBLE( INP_COEFFS ) (Given)
*       The coefficients.
*    N_POINTS = INTEGER (Given)
*       Number of points to evaluate fit at.
*    X_DATA = DOUBLE( N_POINTS ) (Given)
*       Array of data point for fit/plot.
*    Y_FIT = DOUBLE( N_POINTS ) (Returned)
*       Calculated data points (using fit evaluation).
*    STATUS = INTEGER (Given and Returned)
*       Input/Output status conditions.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     19-APR-1996 (MJC):
*       Added checking for BAD values in the polynomial coefficients
*       at entry.
*     11-APR-1997 (MJC):
*       Correct for PDA splines, rather than NAG splines.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'
      INCLUDE 'ECH_POLYSPLINE.INC'

*  Arguments Given:
      CHARACTER*( * ) TYPE
      INTEGER INP_COEFFS
      DOUBLE PRECISION COEFFS( INP_COEFFS )
      INTEGER N_POINTS
      DOUBLE PRECISION X_DATA( N_POINTS )

*  Arguments Returned:
      DOUBLE PRECISION Y_FIT( N_POINTS )

*  Status:
      INTEGER STATUS

*  Local Variables:
      DOUBLE PRECISION VALUE

      INTEGER I
      INTEGER II
      INTEGER N_COEFFS

      LOGICAL FOUND

      CHARACTER*6 FITTER

*  Functions Called:
      INTEGER CHR_LEN
*.

      IF ( TYPE .EQ. ' ' .OR. TYPE .EQ. '?' ) THEN

*     Determine number of coeffs in active fit.
         FITTER = 'POLY'
         N_COEFFS = 0
         FOUND = .FALSE.
         DO WHILE ( N_COEFFS .LT. INP_COEFFS .AND. .NOT. FOUND )
            IF ( COEFFS( N_COEFFS + 1 ) .NE. 0.0D0 .AND.
     :           COEFFS( N_COEFFS + 1 ) .NE. ECH__BAD_DOUBLE ) THEN
             N_COEFFS = N_COEFFS + 1

            ELSE
               FOUND = .TRUE.
            END IF
         END DO

*     If fit active then.
         IF ( COEFFS( 1 ) .NE. ECH__BAD_DOUBLE .AND.
     :        N_COEFFS .GT. 0 ) THEN

*        If there are MIN_SPLINE or more coefficients, assume SPLINE fit.
            IF ( N_COEFFS .GE. MIN_SPLINE ) THEN
               FITTER = 'SPLINE'

*        Otherwise assume POLY unless coefficents don't decrease.
            ELSE
               DO I = 1, N_COEFFS - 1
                   IF ( ABS( COEFFS( I + 1 ) ) .GT.
     :                  ABS( COEFFS( I ) ) ) FITTER = 'SPLINE'
               END DO
            END IF
            IF ( FITTER .EQ. 'SPLINE' ) N_COEFFS = N_COEFFS + 4
         ENDIF

      ELSE
         FITTER = TYPE
         N_COEFFS = INP_COEFFS
      ENDIF

*  If the fit type is unknown then assume guessed type.
      IF ( TYPE .EQ. '?' ) THEN
          INP_COEFFS = N_COEFFS
          IF ( N_COEFFS .GT. 0 ) TYPE = FITTER

      ELSE

*     Change type of fitting.
         IF ( STATUS .EQ. ECH__NEXT_FITTER ) THEN
            IF ( TYPE .EQ. 'POLY' ) THEN
               TYPE = 'SPLINE'
               IF ( N_COEFFS .LT. MIN_SPLINE ) THEN
                  N_COEFFS = ( N_COEFFS + 7 ) * 2
               ENDIF

            ELSE IF ( TYPE .EQ. 'SPLINE' ) THEN
               TYPE = 'POLY'
               IF ( N_COEFFS .GE. MIN_SPLINE ) THEN
                  N_COEFFS = N_COEFFS / 2 - 7
               END IF
            END IF
            REPORT_STRING = ' Fitting function type set to ' //
     :           TYPE( :CHR_LEN( TYPE ) ) // '.'
            CALL ECH_REPORT( 0, REPORT_STRING )
            WRITE ( REPORT_STRING, 1000 ) TYPE( :CHR_LEN( TYPE ) ),
     :                                    N_COEFFS
            CALL ECH_REPORT( 0, REPORT_STRING )
            INP_COEFFS = N_COEFFS

*     Decrement order of fit.
         ELSE IF ( STATUS .EQ. ECH__DEC_NUMCOEFF ) THEN
            IF ( TYPE .EQ. 'SPLINE' ) THEN
               N_COEFFS = MAX( MIN_SPLINE, N_COEFFS - 2 )

            ELSE
               N_COEFFS = MAX( 1, N_COEFFS - 1 )
            ENDIF
            WRITE ( REPORT_STRING, 1000 )
     :            TYPE( :CHR_LEN( TYPE ) ), N_COEFFS
            CALL ECH_REPORT( 0, REPORT_STRING )
            INP_COEFFS = N_COEFFS

*     Increment order of fit.
         ELSE IF ( STATUS .EQ. ECH__INC_NUMCOEFF ) THEN
            IF ( TYPE .EQ. 'SPLINE' ) THEN
               N_COEFFS = MIN( N_POINTS, N_COEFFS + 2 )

            ELSE
               N_COEFFS = MIN( N_POINTS, N_COEFFS + 1 )
            END IF
            WRITE ( REPORT_STRING, 1000 )
     :            TYPE( :CHR_LEN( TYPE ) ), N_COEFFS
            CALL ECH_REPORT( 0, REPORT_STRING )
            INP_COEFFS = N_COEFFS

*     Evaluate Polynomial.
         ELSE IF ( FITTER( :1 ) .EQ. 'P' ) THEN
            IF ( N_COEFFS .GT. 0 .AND. N_POINTS .GT. 0 ) THEN
               DO I = 1, N_POINTS
                  VALUE = COEFFS( N_COEFFS )
                  DO II = N_COEFFS-1, 1, -1
                     VALUE = VALUE * X_DATA( I ) + COEFFS( II )
                  END DO
                  Y_FIT( I ) = VALUE
               END DO
            END IF

*     Evaluate Spline.
         ELSE IF ( FITTER( :1 ) .EQ. 'S' ) THEN
            IF ( N_COEFFS .GT. 0 .AND. N_POINTS .GT. 0 ) THEN
               CALL SPLCALC2( N_COEFFS, COEFFS( 1 ),
     :              COEFFS( N_COEFFS / 2 + 1 ), N_POINTS, X_DATA, Y_FIT,
     :              STATUS )
            END IF
         END IF
      END IF

 1000 FORMAT ( 1X, 'Number of coefficients for ', A,
     :         ' fit set to ', I2, '.' )

      END
