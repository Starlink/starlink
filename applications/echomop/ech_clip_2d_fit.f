      SUBROUTINE ECH_CLIP_2D_FIT(
     :           NX,
     :           X2V,
     :           Y2V,
     :           NO_OF_POINTS,
     :           ORDER_NUMBER,
     :           INTERACTIVE,
     :           MENU,
     :           END_CLIP_MAXDEV,
     :           AUTO_CLIP_BY,
     :           MAXIMUM_POLY,
     :           CLIPPED,
     :           SAMPLES,
     :           FINAL_DEV,
     :           WEIGHTS,
     :           ABANDONED,
     :           ACCEPTED,
     :           POLYX_DEGREE,
     :           POLYY_DEGREE,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_CLIP_2D_FIT

*  Purpose:
*     Clip points from 2-D distortion fit (arc line centroids).

*  Description:
*     This routine handles the clipping of points from the set of 2D fitted
*     arc line samples. Each arc line being sampled at a variety of distances
*     from centre of the order trace.
*     A variety of interactive/automatic clipping options are available
*     to help attain a 'good' fit. Options are all selected by a single
*     character stroke and carriage return is not necessary.
*
*     A(utoclip), selects automatic clipping in which points will be
*     iteratively clipped and the set re-fitted. The number of points
*     to be clipped will be prompted for. A response of zero will
*     intitiate a cycle of clipping which terminates when the absolute
*     deviation of the worst samples has fallen below a tunable threshold
*     value (TUNE_CLPMAXDEV).
*
*     C(lip), is the most generally useful option as it simply clips
*     all points with absolute deviations greater than the current y-position of
*     the cursor.
*
*     D(isable), is used when it is clear that the  samples do not
*     follow a pattern at all and you wish to prevent any subsequent
*     processing of the order. Orders may be automatically disabled
*     if a small enough fraction of samples remain after auto-clipping
*     has been done.
*
*     E(xit), will leave 2D fitting for the current order and save the
*     latest 2D polynomial in the reduction database.
*
*     G(o), is used to switch from interactive clipping to entirely
*     automatic mode. Thus after manually clipping a couple of orders
*     it may be observed that the orders are well modeled and that few
*     points need be clipped, this can be left safely to auto-clipping.
*     The Go option selects auto-clipping for the current, and any
*     subsequent orders.
*
*     N(egative threshold), clips all samples with deviations which are
*     MORE NEGATIVE than the current y-position of the cursor.
*
*     P(ositive threshold), clips all samples with deviations which are
*     MORE POSITIVE than the current y-position of the cursor.
*
*     Q(uit), leaves the 2D fitting for this order without saving
*     the 2D polynomials in the reduction database.  It is therefore
*     LOST.
*
*     V(iew), the graph used for clipping shows the deviations of each
*     sampled point in terms of wavelength. The View option shows the
*     actual coordinates
*     of the arc line samples, providing an easy reference as to the agreement
*     with the deviation as expected from visual examination of the raw data.
*     Actually the deviation is much easier to see in View mode as it is
*     plotted on a greatly exaggerated scale. If no consistent pattern of
*     deviations is visible using the view option, then it unlikely that
*     it is worthwhile using the 2D extraction at all.
*     NOTE that View mode is mutually exclusive with all other operations
*     and must be exited (type any key) before clipping can be resumed.
*
*     (Plus key), increments the degree of X polynomial used to fit thru
*     the arc line samples. It may be increased up to the maximum specified
*     by the tunable parameter TUNE_MAXPOLY.
*
*     (Minus key), decrements the degree of X polynomial used to fit thru
*     the arc line samples.
*
*     (Greater-than key), increments the Y degree of polynomial used to fit thru
*     the arc line samples. It may be increased up to the maximum specified
*     by the tunable parameter TUNE_MAXPOLY.
*
*     (Less-than key), decrements the degree of Y polynomial used to fit thru
*     the arc line samples.
*
*     (Dot key), clips the sample nearest to the cursor when the key
*     is pressed.
*
*     NOTE after all clipping operations, the polynomial is re-fitted to
*     the remaining set of samples automatically.

*  Invocation:
*     CALL ECH_CLIP_2D_FIT(
*     :    NX,
*     :    X2V,
*     :    Y2V,
*     :    NO_OF_POINTS,
*     :    ORDER_NUMBER,
*     :    INTERACTIVE,
*     :    MENU,
*     :    END_CLIP_MAXDEV,
*     :    AUTO_CLIP_BY,
*     :    MAXIMUM_POLY,
*     :    CLIPPED,
*     :    SAMPLES,
*     :    FINAL_DEV,
*     :    WEIGHTS,
*     :    ABANDONED,
*     :    ACCEPTED,
*     :    POLYX_DEGREE,
*     :    POLYY_DEGREE,
*     :    STATUS
*     :   )

*  Arguments:
*    NO_OF_POINTS = INTEGER (Given)
*        Number of points in fit.
*    ORDER_NUMBER = INTEGER (Given)
*        Number of order being processed.
*    MAXIMUM_POLY = INTEGER (Given)
*        Maximum degree of polynomial to attempt to fit.
*    END_CLIP_MAXDEV = REAL (Given)
*        Maximum permitted deviation for autoclipping.
*    AUTO_CLIP_BY = INTEGER (Given)
*        Number of points to clip automatically before a re-fit.
*    INTERACTIVE = LOGICAL (Given)
*        TRUE if interactive clipping is to be used.
*    MENU = LOGICAL (Given and Returned)
*        TRUE if a full menu listing is to be made.
*    CLIPPED = INTEGER (Given and Returned)
*        Number of points clipped from set.
*    SAMPLES = REAL (Given and Returned)
*        Array of estimates of line centres.
*    FINAL_DEV = REAL (Given and Returned)
*        Array of accepted deviations from polynomial fit.
*    WEIGHTS = REAL (Given and Returned)
*        Array of weights.
*    ABANDONED = LOGICAL (Returned)
*        TRUE if fitting has been abandoned.
*    ACCEPTED = LOGICAL (Returned)
*        TRUE if fit is good enough and has been accepted.
*    POLYX_DEGREE = INTEGER (Given and Returned)
*        Current degree of polynomial to fit.
*    POLYY_DEGREE = INTEGER (Given and Returned)
*        Current degree of polynomial to fit.
*    NX = INTEGER (Given)
*        Number of columns in frame.
*    X2V = REAL (Given and Returned)
*        Workspace for graphics.
*    Y2V = REAL (Given and Returned)
*        Workspace for graphics.
*    STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     If 'interactive' then
*         Get users criteria to clip points
*     Else set automatic clipping
*     Endif
*     If any more points to be Clipped then
*     Endif
*     Accept automatically clipped fit if max deviation falls below threshold

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     09-AUG-1996 (MJC):
*       Some tidying, improved '.' option, added 'M' option.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_ENVIRONMENT.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER NO_OF_POINTS
      LOGICAL INTERACTIVE
      LOGICAL MENU
      INTEGER AUTO_CLIP_BY
      REAL END_CLIP_MAXDEV
      DOUBLE PRECISION WEIGHTS( NO_OF_POINTS )
      INTEGER MAXIMUM_POLY
      INTEGER ORDER_NUMBER

*  Arguments Returned:
      REAL X2V( 5000 )
      REAL Y2V( 5000 )
      REAL SAMPLES( NO_OF_POINTS )
      REAL FINAL_DEV( NO_OF_POINTS )
      INTEGER CLIPPED
      LOGICAL ABANDONED
      INTEGER POLYX_DEGREE
      INTEGER POLYY_DEGREE
      LOGICAL ACCEPTED

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL MAX_DEVIATION
      REAL LOWER_X_LIMIT
      REAL UPPER_X_LIMIT
      REAL LOWER_DEV_LIMIT
      REAL UPPER_DEV_LIMIT
      REAL TEMP
      REAL MIN_DISTANCE
      REAL DISTANCE
      REAL X1, X2, Y1, Y2
      REAL SCR_WID, SCR_HGT

      INTEGER I
      INTEGER POINTS_TO_CLIP
      INTEGER OPTIONS
      INTEGER LOOP_COUNT
      INTEGER JUST_CLIPPED
      INTEGER MAX_AT
      INTEGER IX
      INTEGER NEAR_IND
      INTEGER NCHAR1

      CHARACTER*48 TITLE
      CHARACTER*4 REF_STR1
      CHARACTER*4 CLIP_MODE

*  Functions Called:
      EXTERNAL ECH_FATAL_ERROR
      LOGICAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

      ACCEPTED = .FALSE.
      ABANDONED = .FALSE.

*  If 'interactive' display menu.
      IF ( INTERACTIVE ) THEN
  100    CONTINUE
         IF ( MENU ) THEN
            MENU = .FALSE.
            CALL ECH_REPORT( 0, ' Options:' )
            CALL ECH_REPORT( 0,
     :           '   R - Clip X-range ' )
            CALL ECH_REPORT( 0,
     :           '   . - Clip point nearest cursor' )
            CALL ECH_REPORT( 0,
     :           '   N - Clip points more negative' )
            CALL ECH_REPORT( 0,
     :           '   P - Clip point more positive' )
            CALL ECH_REPORT( 0,
     :           '   A - Auto-Clip points' )
            CALL ECH_REPORT( 0,
     :           '   G - GO, Auto-Clip this and subsequent orders' )
            CALL ECH_REPORT( 0,
     :           '   C - Clip points with > ABS deviation' )
            CALL ECH_REPORT( 0,
     :           '   V - View line skews (Exagerated)' )
            CALL ECH_REPORT( 0,
     :           '   D - Disable order' )
            CALL ECH_REPORT( 0,
     :           '   Q - Quit fit, do NOT save polynomials')
            CALL ECH_REPORT( 0,
     :           '   E - Exit - saving polynomial' )
            CALL ECH_REPORT( 0,
     :           '   - - Decrement X-degree of polynomial used' )
            CALL ECH_REPORT( 0,
     :           '   + - Increment X-degree of polynomial used' )
            CALL ECH_REPORT( 0,
     :           '   > - Decrement Y-degree of polynomial used' )
            CALL ECH_REPORT( 0,
     :           '   < - Increment Y-degree of polynomial used' )
            CALL ECH_REPORT( 0,
     :           '   M - Full menu display.' )
            CALL ECH_REPORT( 0, ' ' )

         ELSE
            CALL ECH_REPORT( 0,
     :           ' Options [ R . N P A G C V D Q E - + > < M ]' )
         END IF

*     Get user's criteria to clip points.
         CLIP_MODE = ' '
         LOWER_X_LIMIT = FLOAT( NO_OF_POINTS + 1 )
         UPPER_X_LIMIT = 0.0
         LOWER_DEV_LIMIT = -100.
         UPPER_DEV_LIMIT = 100.
         POINTS_TO_CLIP = 0

         CALL ECH_READ_GRPH_CURSOR( STATUS )

*     Option 'R': Clip X-range.
         IF ( USER_INPUT_CHAR .EQ. 'R' ) THEN
            CALL ECH_REPORT( 0,
     :           ' Now move cursor to range upper X-limit.' )
            lower_x_limit = MAX( 1.0, x_cursor )
            CALL ECH_READ_GRPH_CURSOR( status )
            upper_x_limit = MAX( 1.0, x_cursor )
            IF ( lower_x_limit .GT. upper_x_limit ) THEN
               temp = lower_x_limit
               lower_x_limit = upper_x_limit
               upper_x_limit = temp
            ENDIF
            lower_dev_limit = 100.0
            upper_dev_limit = -100.0
            points_to_clip = 1

*     Option 'V': View exaggerated skews.
         ELSE IF ( USER_INPUT_CHAR .EQ. 'V' ) THEN
            OPTIONS = GRPH_CALC_MINMAX
            CALL CHR_ITOC( NX / 50, REF_STR1, NCHAR1 )
            TITLE = ' Line centers (exaggerated ' //
     :            REF_STR1( :NCHAR1 ) // ' times)'
            CALL ECH_PLOT_GRAPH( NO_OF_POINTS, X2V, Y2V, 0, 0, 0, 0,
     :           'X position', 'Y offset', TITLE, 0, 10.0, OPTIONS,
     :           'POINTS', STATUS )
            CALL ECH_READ_GRPH_CURSOR( STATUS )

*     Option 'M': Display full menu.
         ELSE IF ( USER_INPUT_CHAR .EQ. 'M' ) THEN
            MENU = .TRUE.
            GO TO 100

*     Option '.': Clip point nearest cursor.
         ELSE IF ( USER_INPUT_CHAR .EQ. '.' ) THEN
            POINTS_TO_CLIP = 1

*     Option 'N': Clip points below cursor.
         ELSE IF ( USER_INPUT_CHAR .EQ. 'N' ) THEN
            POINTS_TO_CLIP = 1
            LOWER_DEV_LIMIT = Y_CURSOR
            UPPER_DEV_LIMIT = 100.0
            UPPER_X_LIMIT = FLOAT( NO_OF_POINTS + 1 )
            LOWER_X_LIMIT = 0.0

*     Options 'A' and 'G': Auto-clip points.
         ELSE IF ( USER_INPUT_CHAR .EQ. 'A' .OR.
     :             USER_INPUT_CHAR .EQ. 'G'   ) THEN
            IF ( AUTO_CLIP_BY .GT. 0 ) THEN
               POINTS_TO_CLIP = AUTO_CLIP_BY

            ELSE
               POINTS_TO_CLIP = MAX( 1, NO_OF_POINTS / 50 )
            ENDIF
            CLIP_MODE = 'AUTO'
            UPPER_DEV_LIMIT = 100.
            LOWER_DEV_LIMIT = -100.
            UPPER_X_LIMIT = FLOAT( NO_OF_POINTS + 1 )
            LOWER_X_LIMIT = 0.0
            IF ( USER_INPUT_CHAR .EQ. 'G' ) INTERACTIVE = .FALSE.

*     Option 'P': Clip points above cursor.
         ELSE IF ( USER_INPUT_CHAR .EQ. 'P' ) THEN
            POINTS_TO_CLIP = 1
            UPPER_DEV_LIMIT = Y_CURSOR
            LOWER_DEV_LIMIT = -100.
            UPPER_X_LIMIT = FLOAT( NO_OF_POINTS + 1 )
            LOWER_X_LIMIT = 0.0

*     Option 'C': Clip points with deviation greater than modulus
*     of cursor Y-position.
         ELSE IF ( USER_INPUT_CHAR .EQ. 'C' ) THEN
            POINTS_TO_CLIP = 1
            UPPER_DEV_LIMIT = ABS( Y_CURSOR )
            LOWER_DEV_LIMIT = - ABS( Y_CURSOR )
            UPPER_X_LIMIT = FLOAT( NO_OF_POINTS + 1 )
            LOWER_X_LIMIT = 0.0

*     Option '<': Increment order of Y fit.
         ELSE IF ( USER_INPUT_CHAR .EQ. '<' ) THEN
            POLYY_DEGREE = MIN( MAXIMUM_POLY, POLYY_DEGREE + 1 )
            CALL ECH_REPORT( 0,
     :           ' Re-fit: increasing Y-degree of polynomial.' )

*     Option '>': Decrement order of Y fit.
         ELSE IF ( USER_INPUT_CHAR .EQ. '>' ) THEN
            POLYY_DEGREE = MAX( 1, POLYY_DEGREE - 1 )
            CALL ECH_REPORT( 0,
     :           ' Re-fit: decreasing Y-degree of polynomial.' )

*     Option '+': Increment order of X fit.
         ELSE IF ( USER_INPUT_CHAR .EQ. '+' ) THEN
            POLYX_DEGREE = MIN( MAXIMUM_POLY, POLYX_DEGREE + 1 )
            CALL ECH_REPORT( 0,
     :           ' Re-fit: increasing X-degree of polynomial.' )

*     Option '-': Decrement order of X fit.
         ELSE IF ( USER_INPUT_CHAR .EQ. '-' ) THEN
            POLYX_DEGREE = MAX( 1, POLYX_DEGREE - 1 )
            CALL ECH_REPORT( 0,
     :           ' Re-fit: decreasing X-degree of polynomial.' )

*     Option 'Q': Quit.
         ELSE IF ( USER_INPUT_CHAR .EQ. 'Q' ) THEN
            ABANDONED = .TRUE.
            ACCEPTED = .TRUE.
            CALL ECH_REPORT( 0,
     :           ' Quitting fit, polynomials not saved.' )

*     Option 'D': Disable this order.
         ELSE IF ( USER_INPUT_CHAR .EQ. 'D' ) THEN
            ABANDONED = .TRUE.
            CALL ECH_REPORT( 0,
     :           ' Rejecting fit: order flagged as unusable.' )

*     Option 'E': Exit, accept fits.
         ELSE IF ( USER_INPUT_CHAR .EQ. 'E' ) THEN
            ACCEPTED = .TRUE.
            CALL ECH_REPORT( 0,
     :           ' Accepted fit: saving polynomials.' )

*     Unknown option.
         ELSE
            REPORT_STRING = ' Unknown Option: "' //
     :            USER_INPUT_CHAR // '".'
            CALL ECH_REPORT( 0, REPORT_STRING )
            GO TO 100
         ENDIF

*  Else set automatic clipping.
      ELSE
         IF ( auto_clip_by .GT. 0 ) THEN
            points_to_clip = auto_clip_by

         ELSE
            points_to_clip = MAX ( 1, no_of_points / 50 )
         ENDIF
         clip_mode = 'AUTO'
         upper_dev_limit = 100.
         lower_dev_limit = -100.
         upper_x_limit = FLOAT ( no_of_points + 1 )
         lower_x_limit = 0.0
      END IF

*  If any more points to be Clipped then.
      JUST_CLIPPED = 0
      IF ( POINTS_TO_CLIP .GT. 0 ) THEN
         IF ( USER_INPUT_CHAR .EQ. '.' ) THEN

*        Obtain conversion factors for screen position.
            CALL PGQVSZ( 2, X1, SCR_WID, Y1, SCR_HGT )
            CALL PGQWIN( X1, X2, Y1, Y2 )
            SCR_WID = SCR_WID / MAX( 1.0, ( X2 - X1 ) )
            SCR_HGT = SCR_HGT / MAX( 1.0, ( Y2 - Y1 ) )

*        Convert cursor coords to true units.
            X1 = X_CURSOR * SCR_WID
            Y1 = Y_CURSOR * SCR_HGT

*        Work through all non-clipped points.
            MIN_DISTANCE = 1.0E20
            DO I = 1, NO_OF_POINTS
               IF ( SAMPLES( I ) .NE. ECH__BAD_REAL ) THEN
                  X2 = FLOAT( I ) * SCR_WID
                  Y2 = FINAL_DEV( I ) * SCR_HGT

*              Calculate square of distance to this point.
                  DISTANCE = ( X2 - X1 ) * ( X2 - X1 ) +
     :                  ( Y2 - Y1 ) * ( Y2 - Y1 )
                  IF ( DISTANCE .LT. MIN_DISTANCE ) THEN
                     MIN_DISTANCE = DISTANCE
                     NEAR_IND = I
                  END IF
               END IF
            END DO

*        Clip the nearest point.
            JUST_CLIPPED = 1
            WEIGHTS( NEAR_IND ) = ECH__BAD_REAL
            SAMPLES( NEAR_IND ) = ECH__BAD_REAL

         ELSE
            LOOP_COUNT = 1
            IF ( CLIP_MODE .EQ. 'AUTO' ) LOOP_COUNT = POINTS_TO_CLIP
            DO I = 1, LOOP_COUNT
               MAX_DEVIATION = 0.0
               DO IX = MAX( 1, INT( LOWER_X_LIMIT ) ),
     :                MIN( NO_OF_POINTS, INT( UPPER_X_LIMIT ) )
                  IF ( SAMPLES( IX ) .NE. ECH__BAD_REAL ) THEN
                     IF ( ABS( FINAL_DEV( IX ) ) .GT.
     :                    MAX_DEVIATION ) THEN
                        MAX_DEVIATION = ABS( FINAL_DEV( IX ) )
                        MAX_AT = IX
                     END IF
                     IF ( CLIP_MODE .NE. 'AUTO' ) THEN
                        IF ( FINAL_DEV( IX ) .LT. LOWER_DEV_LIMIT .OR.
     :                       FINAL_DEV( IX ) .GT. UPPER_DEV_LIMIT ) THEN
                           SAMPLES( IX ) = ECH__BAD_REAL
                           FINAL_DEV( IX ) = ECH__BAD_REAL
                           WEIGHTS( IX ) = ECH__BAD_REAL
                           JUST_CLIPPED = JUST_CLIPPED + 1
                        END IF
                     END IF
                  END IF
               END DO

               IF ( clip_mode .EQ. 'AUTO' ) THEN
                  samples ( max_at ) = ECH__BAD_REAL
                  final_dev ( max_at ) = ECH__BAD_REAL
                  weights ( max_at ) = ECH__BAD_REAL
                  just_clipped = just_clipped + 1
               END IF
            END DO
         ENDIF
         CLIPPED = CLIPPED + JUST_CLIPPED
      ENDIF

*  Accept automatically clipped fit if max deviation falls below threshold.
      IF ( .NOT. INTERACTIVE .AND. MAX_DEVIATION .LT. END_CLIP_MAXDEV )
     :   ACCEPTED = .TRUE.

      END
