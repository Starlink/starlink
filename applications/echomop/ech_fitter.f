      SUBROUTINE ECH_FITTER(
     :           TYPE,
     :           N_COEFFS,
     :           COEFFS,
     :           N_POINTS,
     :           X_DATA,
     :           Y_DATA,
     :           WEIGHT,
     :           NREJ,
     :           THRESH,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_FITTER

*  Purpose:
*     Selects and fits functions through 1-D datasets.

*  Invocation:
*     CALL ECH_FITTER(
*     :    TYPE,
*     :    N_COEFFS,
*     :    COEFFS,
*     :    N_POINTS,
*     :    X_DATA,
*     :    Y_DATA,
*     :    WEIGHT,
*     :    NREJ,
*     :    THRESH,
*     :    STATUS
*     :   )

*  Arguments:
*     TYPE = CHAR (Given)
*        Type of fitting function.
*     N_COEFFS = INTEGER (Given)
*        Number of coefficients in a fit.
*     COEFFS = DOUBLE (Given and Returned)
*        The number of coefficients used.
*     N_POINTS = INTEGER (Given)
*        Number of points to evaluate fit at.
*     X_DATA = DOUBLE (Given)
*        Array of data point for fit/plot.
*     Y_DATA = DOUBLE (Given)
*        Y data to fit.
*     WEIGHT = REAL (Given)
*        Weights to apply during fit.
*     NREJ = INTEGER (Given)
*        Number of reject cycles to use during fit.
*     THRESH = FLOAT (Given)
*        Rejection threshold (sigma) for fit.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     23-JUL-1996 (MJC):
*       Removed check for 'DOUBLE-' prefix in TYPE.
*       Removed unused common variables.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_ENVIRONMENT.INC'
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_POLYSPLINE.INC'

*  Arguments Given:
      INTEGER N_POINTS
      INTEGER N_COEFFS
      INTEGER NREJ
      REAL THRESH
      DOUBLE PRECISION X_DATA( * )
      DOUBLE PRECISION Y_DATA( * )
      REAL WEIGHT( * )
      CHARACTER*( * ) TYPE

*  Arguments Returned:
      DOUBLE PRECISION COEFFS( * )

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER USE_MAX_POINTS
      INTEGER W1, W2, W3, W3P2, W4, W5, W6, W7, WORKSIZE
      INTEGER I
      INTEGER NCHAR1

      LOGICAL GOT_WORKSPACE
      LOGICAL DOUBLE_DATA

      CHARACTER*4 REF_STR1
      CHARACTER*6 FITTER

      COMMON / FITTER_SPACE1 / W1, W2, W3, W3P2, W4, W5, W6, W7,
     :         WORKSIZE

      COMMON / FITTER_SPACE2 / GOT_WORKSPACE, USE_MAX_POINTS

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
      INTEGER CHR_LEN
*.
      DATA GOT_WORKSPACE / .FALSE. /

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

      STATUS = 0
      IF ( TYPE( :5 ) .EQ. 'REAL-' )  THEN
         FITTER = TYPE( 6:11 )
         DOUBLE_DATA = .FALSE.

      ELSE
         DOUBLE_DATA = .TRUE.
         FITTER = TYPE( :6 )
      ENDIF

      IF ( N_POINTS .GT. USE_MAX_POINTS .AND.
     :     USE_MAX_POINTS .GT. 0 ) THEN
         CALL PSX_FREE( W1, STATUS )
         CALL PSX_FREE( W2, STATUS )
         CALL PSX_FREE( W3, STATUS )
         CALL PSX_FREE( W4, STATUS )
         CALL PSX_FREE( W5, STATUS )
         CALL PSX_FREE( W6, STATUS )
         CALL PSX_FREE( W7, STATUS )
         GOT_WORKSPACE = .FALSE.
      END IF

      IF ( .NOT. GOT_WORKSPACE ) THEN
         CALL PSX_MALLOC( N_POINTS * 4 + 8, W1, STATUS  )
         CALL PSX_MALLOC( N_POINTS * 8 + 16, W2, STATUS  )
         CALL PSX_MALLOC( N_POINTS * 24 + 48, W3, STATUS  )
         W3P2 = W3 + N_POINTS * 8 + 24
         CALL PSX_MALLOC( N_POINTS * 8 + 16, W4, STATUS  )
         CALL PSX_MALLOC( 16 * MAX_FIT_COEFFS, W5, STATUS )
         CALL PSX_MALLOC( N_POINTS * 4 + 8, W6, STATUS )

*  NAG version allocation.
*         CALL PSX_MALLOC( N_POINTS * 8 + 16, W7, STATUS )
*  PDA version allocation (should be enough for NAG version also).
         WORKSIZE = 10 * ( MAX_FIT_COEFFS + 1 ) +
     :        2 * MAX( N_POINTS, MAX_FIT_COEFFS ) + MAX_FIT_COEFFS +
     :        16
         CALL PSX_MALLOC( 8 * WORKSIZE, W7, STATUS )
         GOT_WORKSPACE = .TRUE.
         USE_MAX_POINTS = N_POINTS
      END IF

      IF ( STATUS .NE. 0 ) THEN
          CALL ECH_REPORT( 0, ' Failed to obtain workspace for fit.' )

      ELSE
         IF ( FITTER( :6 ) .EQ. 'SPLINE' ) THEN
            IF ( N_COEFFS .LT. MIN_SPLINE ) THEN
               CALL ECH_REPORT( 0, ' Insufficient number of' //
     :              ' coefficients for spline fit.' )
               CALL CHR_ITOC( MIN_SPLINE, REF_STR1, NCHAR1 )
               REPORT_STRING =
     :               ' Increasing number of coefficients to ' //
     :               REF_STR1( :NCHAR1 ) // '.'
               CALL ECH_REPORT( 0, REPORT_STRING )
     :
               N_COEFFS = MIN_SPLINE
            END IF
            IF ( DOUBLE_DATA ) THEN
               CALL SPLFITD( N_POINTS, X_DATA, Y_DATA, WEIGHT,
     :              %VAL( W4 ), NREJ, THRESH, -THRESH, N_COEFFS,
     :              COEFFS( 1 ), COEFFS( N_COEFFS / 2 + 1 ),
     :              %VAL( W2 ), %VAL( W3 ), %VAL( W3P2 ), %VAL( W6 ),
     :              WORKSIZE, %VAL( W7 ), %VAL( W5 ), STATUS )

            ELSE
               CALL SPLFITA( N_POINTS, X_DATA, Y_DATA, WEIGHT,
     :              %VAL( W1 ), NREJ, THRESH, -THRESH, N_COEFFS,
     :              COEFFS( 1 ), COEFFS( N_COEFFS / 2 + 1 ),
     :              %VAL( W2 ), %VAL( W3 ), %VAL( W3P2 ), %VAL( W6 ),
     :              WORKSIZE, %VAL( W7 ), %VAL( W5 ), STATUS )
            ENDIF

         ELSE
            IF ( FITTER( :4 ) .NE. 'POLY' ) THEN
               CALL ECH_REPORT( 0,
     :              ' Unknown fitting function: ' //
     :              FITTER( : CHR_LEN( FITTER ) ) // '.' )
               CALL ECH_REPORT( 0, ' Using polynomial by default.' )
               TYPE = 'POLY'
            END IF
            IF ( N_COEFFS .GE. 10 ) THEN
               CALL CHR_ITOC( 9, REF_STR1, NCHAR1 )
               REPORT_STRING =
     :               ' Decresing number of coefficients to ' //
     :               REF_STR1( :NCHAR1 ) // '.'
               CALL ECH_REPORT( 0, REPORT_STRING )
               N_COEFFS = 9
            END IF
            DO I = 1, N_COEFFS
               COEFFS( I ) = 0.0
            END DO
            IF ( DOUBLE_DATA ) THEN
               CALL ECH_POLYFITD( N_POINTS, X_DATA, Y_DATA, WEIGHT,
     :              %VAL( W1 ), N_COEFFS, NREJ, THRESH, -THRESH,
     :              COEFFS, %VAL( W3 ), %VAL( W5 ), STATUS )

            ELSE
               CALL ECH_POLYFITA( N_POINTS, X_DATA, Y_DATA, WEIGHT,
     :              %VAL( W1 ), N_COEFFS, NREJ, THRESH, -THRESH,
     :              COEFFS, %VAL( W3 ), %VAL( W5 ), STATUS )
            ENDIF
         ENDIF
      ENDIF

      END
