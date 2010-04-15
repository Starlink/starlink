      SUBROUTINE ECH_CLIP_TRACE(
     :           NX,
     :           ORDER_NUMBER,
     :           MAXIMUM_POLY,
     :           FITTER,
     :           INTERACTIVE,
     :           TRACE_PLOT,
     :           END_CLIP_MAXDEV,
     :           AUTO_CLIP_BY,
     :           POLY_DEGREE,
     :           CLIPPED,
     :           TRACE,
     :           FINAL_DEV,
     :           PLOTTING,
     :           MENU,
     :           ABANDONED,
     :           ACCEPTED,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_CLIP_TRACE

*  Purpose:
*     Clip points from a curve fit.

*  Description:
*     This routine handles the clipping of points from the sampled data.
*     A variety of interactive/automatic clipping options are available
*     to help attain a 'good' fit.  Options are all selected by a single
*     character stroke and carriage return in not necessary.
*
*     A(utoclip), selects automatic clipping in which points will be
*     iteratively clipped and the data re-fitted.  The number of points
*     to be clipped will be prompted for.   A response of zero will
*     intitiate a cycle of clipping which terminates when the absolute
*     deviation of the worst samples has fallen below a tunable threshold
*     value (TUNE_CLPMAXDEV).
*
*     C(lip), is the most generally useful option as it simply clips
*     all points with absolute deviations greater than the current
*     Y-position of the cursor.
*
*     D(isable), is used when it is clear that the samples are not
*     fittable.  Disables further processing ot the order effected.
*     Orders may be automatically disabled if a small enough fraction
*     of samples remain after auto-clipping has been done.
*
*     E(xit), will leave fitting for the current order and save the
*     latest fit in the reduction database.
*
*     G(o), is used to switch from interactive clipping to automatic
*     mode.  Thus, after manually clipping a couple of orders it may be
*     observed that the data are well modelled and that few points need
*     be clipped, this can be left safely to auto-clipping.
*     This option selects auto-clipping for the current, and any
*     subsequent orders.
*
*     N(egative threshold), clips all samples with deviations which are
*     MORE NEGATIVE than the current Y-position of the cursor.
*
*     P(ositive threshold), clips all samples with deviations which are
*     MORE POSITIVE than the current Y-position of the cursor.
*
*     Q(uit), leaves the fitter for this order without saving the fit
*     in the reduction database.  The fit is lost.
*
*     R(ange), allows the clipping of a set of samples denoted by a range
*     of values along the X-axis of the data.  This will generally be used
*     when the data is only partially present, or perhaps to remove
*     the contributions of a swath of bad points.  The starting point for
*     the range to be clipped is the current cursor X-position; the
*     end point is selected by moving the cursor and then hitting any key.
*
*     V(iew), the graph normally used for clipping shows the deviations
*     from the fit of each sample.  The View option shows the actual
*     samples, versus the fitted curve.  This provides a reference as to
*     the agreement of the fit with the data.  NOTE that View mode is
*     mutually exclusive with all other operations and must be lefted
*     (type any key) before clipping can be resumed.
*
*     (Plus key), increments the degree of fit used to fit through
*     the samples.  May be increased up to the maximum specified
*     by the parameter TUNE_MAXPOLY.
*
*     (Minus key), decrements the degree of fit used.
*
*     (Dot key), clips the sample nearest to the cursor when the key
*     is pressed.
*
*     (Exclamation mark key), leaves the fitter for this order
*     and all subsequent orders.  Will either abort the task or return to
*     the ECHOMOP main menu, depending on the context.
*
*     NOTE after all clipping operations, the curve is re-fitted to
*     the remaining samples automatically.

*  Invocation:
*      CALL ECH_CLIP_TRACE(
*     :     NX,
*     :     ORDER_NUMBER,
*     :     MAXIMUM_POLY,
*     :     FITTER,
*     :     INTERACTIVE,
*     :     TRACE_PLOT,
*     :     END_CLIP_MAXDEV,
*     :     AUTO_CLIP_BY,
*     :     POLY_DEGREE,
*     :     CLIPPED,
*     :     TRACE,
*     :     FINAL_DEV,
*     :     PLOTTING,
*     :     MENU,
*     :     ABANDONED,
*     :     ACCEPTED,
*     :     STATUS
*     :    )

*  Arguments:
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     ORDER_NUMBER = INTEGER (Given)
*        Number of order being processed.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum degree of polynomial to attempt to fit.
*     END_CLIP_MAXDEV = REAL (Given)
*        Maximum permitted deviation for autoclipping.
*     AUTO_CLIP_BY = INTEGER (Given)
*        Number of points to clip automatically before a re-fit.
*     INTERACTIVE = LOGICAL (Given)
*        TRUE if interactive clipping is to be used.
*     TRACE_PLOT = LOGICAL (Given)
*        TRUE if actual data and fit is to be plotted.
*     POLY_DEGREE = LOGICAL (Given and Returned)
*        Current degree of polynomial to fit.
*     CLIPPED = INTEGER (Given and Returned)
*        Number of points clipped from trace set.
*     TRACE = REAL (Given and Returned)
*        Array of nx estimates of order centre in each column.
*     FINAL_DEV = REAL (Given and Returned)
*        Array of accepted deviations from polynomial fit.
*     FITTER = CHAR (Given)
*        Type of wavelength fitting function in use (POLY/SPLINE).
*     PLOTTING = LOGICAL (Given and Returned)
*        Set TRUE if plotting enabled.
*     MENU = LOGICAL (Given and Returned)
*        Set TRUE if full command menu is displayed.
*     ABANDONED = LOGICAL (Returned)
*        TRUE if fitting has been abandoned.
*     ACCEPTED = LOGICAL (Returned)
*        TRUE if fit is good enough and has been accepted.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     18-JUL-1996 (MJC):
*       Improved '.' option.
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

*  Arguments:
      INTEGER NX
      INTEGER MAXIMUM_POLY
      LOGICAL INTERACTIVE
      LOGICAL TRACE_PLOT
      INTEGER AUTO_CLIP_BY
      REAL END_CLIP_MAXDEV
      CHARACTER*( * ) FITTER
      INTEGER ORDER_NUMBER
      LOGICAL PLOTTING
      LOGICAL MENU
      REAL TRACE( NX )
      REAL FINAL_DEV( NX )
      INTEGER CLIPPED
      LOGICAL ABANDONED
      INTEGER POLY_DEGREE
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
      INTEGER LOOP_COUNT
      INTEGER JUST_CLIPPED
      INTEGER MAX_AT
      INTEGER IX
      INTEGER NEAR_IND
      INTEGER NCHAR1

      LOGICAL LBOX

      CHARACTER*4 REF_STR1
      CHARACTER*4 CLIP_MODE

*  Functions called:
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

*  If 'interactive' then write informative messages and menu.
      IF ( INTERACTIVE ) THEN
*     Full menu.
  100    CONTINUE
         IF ( MENU ) THEN
            CALL CHR_ITOC( ORDER_NUMBER, REF_STR1, NCHAR1 )
            REPORT_STRING = ' Clipping order ' //
     :            REF_STR1( :NCHAR1 ) // '.'
            CALL ECH_REPORT( 0, REPORT_STRING )
            CALL ECH_REPORT( 0, ' Options:' )
            IF ( .NOT. TRACE_PLOT ) THEN
               CALL ECH_REPORT( 0,
     :              '   . - Clip point nearest cursor.' )
               CALL ECH_REPORT( 0,
     :              '   N - Clip points more negative.' )
               CALL ECH_REPORT( 0,
     :              '   P - Clip point more positive.' )
               CALL ECH_REPORT( 0,
     :              '   C - Clip points with > ABS deviation.' )
            END IF
            CALL ECH_REPORT( 0, '   R - Clip x range.' )
            CALL ECH_REPORT( 0, '   B - Clip x/y box region.' )
            CALL ECH_REPORT( 0, '   A - Auto-Clip points.' )
            CALL ECH_REPORT( 0,
     :           '   G - GO, Auto-clip this and subsequent orders.' )
            CALL ECH_REPORT( 0, '   D - Disable order.' )
            IF ( PLOTTING ) THEN
               CALL ECH_REPORT( 0,
     :              '   O - Off.  Toggles plotting (on/off).' )

            ELSE
               CALL ECH_REPORT( 0,
     :              '   O - On.  Toggles plotting (on/off).' )
            END IF
            IF ( FITTER .NE. 'MEDIAN' )
     :         CALL ECH_REPORT( 0,
     :              '   F - Change fitting function type.' )
            CALL ECH_REPORT( 0,
     :      '   V - Toggles view between Data and Deviations plots.' )
            CALL ECH_REPORT( 0,
     :              '   Q - Quit fit, do NOT save coefficients.' )
            CALL ECH_REPORT( 0,
     :              '   E - Exit saving coefficients.' )
            CALL ECH_REPORT( 0,
     :              '   ! - Abandon.  Exit fit option.' )
            CALL ECH_REPORT( 0,
     :              '   - - Decrement degree of fit used.' )
            CALL ECH_REPORT( 0,
     :              '   + - Increment degree of fit used.' )
            CALL ECH_REPORT( 0,
     :              '   M - Full menu display.' )
            CALL ECH_REPORT( 0, ' ' )
            MENU = .FALSE.

*     Single-line menu.
         ELSE
            CALL ECH_REPORT( 0,
     :           ' Options [ . N P C R B A G D O F V Q E ! - + M ]' )

         END IF

*     Get criteria to clip points.
         clip_mode = ' '
         lower_x_limit = FLOAT ( nx + 1 )
         upper_x_limit = 0.0
         lbox = .FALSE.
         lower_dev_limit = -100.
         upper_dev_limit = 100.
         points_to_clip = 0
         CALL ECH_READ_GRPH_CURSOR ( status )
         x_cursor = MAX( MIN( FLOAT( nx ), x_cursor ), 1.0 )
         IF ( user_input_char .EQ. 'R' ) THEN
            CALL ECH_REPORT( 0,
     :           ' Now move cursor to range upper X-limit.' )
            lower_x_limit = MAX( 1., x_cursor )
            CALL ECH_READ_GRPH_CURSOR ( status )
            upper_x_limit = MAX( 1., x_cursor )
            IF ( lower_x_limit .GT. upper_x_limit ) THEN
               temp = lower_x_limit
               lower_x_limit = upper_x_limit
               upper_x_limit = temp
            ENDIF
            lower_dev_limit = 100.
            upper_dev_limit = -100.
            points_to_clip = 1

         ELSE IF ( user_input_char .EQ. 'B' ) THEN
            CALL ECH_REPORT( 0,
     :           ' Now move cursor to opposite corner of region.' )
            lower_x_limit = MAX( 1., x_cursor )
            lower_dev_limit = y_cursor
            CALL ECH_READ_GRPH_CURSOR( status )
            upper_x_limit = MAX( 1., x_cursor )
            upper_dev_limit = y_cursor
            IF ( lower_x_limit .GT. upper_x_limit ) THEN
               temp = lower_x_limit
               lower_x_limit = upper_x_limit
               upper_x_limit = temp
            ENDIF
            IF ( lower_dev_limit .GT. upper_dev_limit ) THEN
               temp = lower_dev_limit
               lower_dev_limit = upper_dev_limit
               upper_dev_limit = temp
            ENDIF
            points_to_clip = 1
            lbox = .TRUE.

         ELSE IF ( TRACE_PLOT .AND. (
     :             USER_INPUT_CHAR .EQ. '.' .OR.
     :             USER_INPUT_CHAR .EQ. 'N' .OR.
     :             USER_INPUT_CHAR .EQ. 'P' .OR.
     :             USER_INPUT_CHAR .EQ. 'B' .OR.
     :             USER_INPUT_CHAR .EQ. 'C' ) ) THEN
            CALL ECH_REPORT( 0,
     :  ' Illegal option - Use V to toggle view to deviations plot.' )

         ELSE IF ( USER_INPUT_CHAR .EQ. 'M' ) THEN
            MENU = .TRUE.
            GO TO 100

         ELSE IF ( USER_INPUT_CHAR .EQ. '.' ) THEN
            POINTS_TO_CLIP = 1

         ELSE IF ( USER_INPUT_CHAR .EQ. 'O' ) THEN
            PLOTTING = .NOT. PLOTTING

         ELSE IF ( USER_INPUT_CHAR .EQ. 'V' ) THEN
            TRACE_PLOT = .NOT. TRACE_PLOT
            IF ( TRACE_PLOT ) THEN
               CALL ECH_REPORT( 0,
     :              ' Plotting of actual data and fit selected.' )

            ELSE
               CALL ECH_REPORT( 0,
     :              ' Plotting of deviations-from-fit selected.' )
            END IF

         ELSE IF ( USER_INPUT_CHAR .EQ. '!' ) THEN
            POINTS_TO_CLIP = 0
            ABANDONED = .TRUE.
            STATUS = ECH__ABORT_OPTION

         ELSE IF ( USER_INPUT_CHAR .EQ. 'F' ) THEN
            IF ( FITTER .NE. 'MEDIAN' )
     :          CALL ECH_FEVAL( FITTER, POLY_DEGREE, 0., 1,
     :               0., 0., ECH__NEXT_FITTER )

         ELSE IF ( USER_INPUT_CHAR .EQ. 'N' ) THEN
            POINTS_TO_CLIP = 1
            LOWER_DEV_LIMIT = Y_CURSOR
            UPPER_DEV_LIMIT = 100.
            UPPER_X_LIMIT = FLOAT( NX + 1 )
            LOWER_X_LIMIT = 0.0

         ELSE IF ( USER_INPUT_CHAR .EQ. 'A' .OR.
     :             USER_INPUT_CHAR .EQ. 'G' ) THEN
            IF ( AUTO_CLIP_BY .GT. 0 ) THEN
               POINTS_TO_CLIP = AUTO_CLIP_BY

            ELSE
               POINTS_TO_CLIP = MAX( 1, NX / 20 )
            END IF
            CLIP_MODE = 'AUTO'
            UPPER_DEV_LIMIT = 100.
            LOWER_DEV_LIMIT = -100.
            UPPER_X_LIMIT = FLOAT( NX + 1 )
            LOWER_X_LIMIT = 0.0
            IF ( USER_INPUT_CHAR .EQ. 'G' ) INTERACTIVE = .FALSE.

         ELSE IF ( USER_INPUT_CHAR .EQ. 'P' ) THEN
            POINTS_TO_CLIP = 1
            UPPER_DEV_LIMIT = Y_CURSOR
            LOWER_DEV_LIMIT = -100.
            UPPER_X_LIMIT = FLOAT( NX + 1 )
            LOWER_X_LIMIT = 0.0

         ELSE IF ( USER_INPUT_CHAR .EQ. 'C' ) THEN
            POINTS_TO_CLIP = 1
            UPPER_DEV_LIMIT = ABS( Y_CURSOR )
            LOWER_DEV_LIMIT = -ABS( Y_CURSOR )
            UPPER_X_LIMIT = FLOAT( NX + 1 )
            LOWER_X_LIMIT = 0.0

         ELSE IF ( USER_INPUT_CHAR .EQ. '+' ) THEN
            IF ( FITTER .NE. 'MEDIAN' ) THEN
               CALL ECH_FEVAL( FITTER, POLY_DEGREE, 0., MAXIMUM_POLY,
     :              0., 0., ECH__INC_NUMCOEFF )

            ELSE
               POLY_DEGREE = MIN( POLY_DEGREE * 3 / 2, MAXIMUM_POLY )
            END IF

         ELSE IF ( USER_INPUT_CHAR .EQ. '-' ) THEN
            IF ( FITTER .NE. 'MEDIAN' ) THEN
               CALL ECH_FEVAL( FITTER, POLY_DEGREE, 0., MAXIMUM_POLY,
     :              0., 0., ECH__DEC_NUMCOEFF )

            ELSE
               POLY_DEGREE = MAX( 5, POLY_DEGREE * 2 / 3 )
            ENDIF

         ELSE IF ( USER_INPUT_CHAR .EQ. 'Q' ) THEN
            ABANDONED = .TRUE.
            ACCEPTED = .TRUE.
            CALL ECH_REPORT( 0,
     :           ' Quitting fit, coefficients not saved.' )

         ELSE IF ( USER_INPUT_CHAR .EQ. 'D' ) THEN
            ABANDONED = .TRUE.
            CALL ECH_REPORT( 0,
     :           ' Rejected fit, Order flagged as unusable.' )

         ELSE IF ( USER_INPUT_CHAR .EQ. 'E' ) THEN
            ACCEPTED = .TRUE.
            CALL ECH_REPORT( 0, ' Accepted fit, saving coefficients.' )

         ELSE
            REPORT_STRING = ' Unknown Option: "' //
     :            USER_INPUT_CHAR // '".'
            CALL ECH_REPORT( 0, REPORT_STRING )
            GO TO 100
         END IF

*  Else set automatic clipping.
      ELSE
         IF ( AUTO_CLIP_BY .GT. 0 ) THEN
            POINTS_TO_CLIP = AUTO_CLIP_BY

         ELSE
            POINTS_TO_CLIP = MAX( 1, NX / 50 )
         END IF
         CLIP_MODE = 'AUTO'
         UPPER_DEV_LIMIT = 100.
         LOWER_DEV_LIMIT = -100.
         UPPER_X_LIMIT = FLOAT( NX + 1 )
         LOWER_X_LIMIT = 0.0
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
            DO I = 1, NX
               IF ( TRACE( I ) .NE. ECH__BAD_REAL ) THEN
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
            TRACE( NEAR_IND ) = ECH__BAD_REAL

         ELSE
            LOOP_COUNT = 1
            IF ( CLIP_MODE .EQ. 'AUTO' ) LOOP_COUNT = POINTS_TO_CLIP
            DO I = 1, LOOP_COUNT
               MAX_DEVIATION = 0.0
               DO IX = MAX( 1, INT( LOWER_X_LIMIT ) ),
     :                 MIN( NX, INT( UPPER_X_LIMIT ) )
                  IF ( TRACE( IX ) .NE. ECH__BAD_REAL ) THEN
                     IF ( ABS( FINAL_DEV( IX ) ) .GT.
     :                    MAX_DEVIATION ) THEN
                        MAX_DEVIATION = ABS( FINAL_DEV( IX ) )
                        MAX_AT = IX
                     END IF
                     IF ( CLIP_MODE .NE. 'AUTO' ) THEN
                        IF ( LBOX ) THEN
                           IF (
     :                       FINAL_DEV( IX ) .GT. LOWER_DEV_LIMIT .AND.
     :                       FINAL_DEV( IX ) .LT. UPPER_DEV_LIMIT ) THEN
                              TRACE( IX ) = ECH__BAD_REAL
                              FINAL_DEV( IX ) = ECH__BAD_REAL
                              JUST_CLIPPED = JUST_CLIPPED + 1
                           END IF

                        ELSE
                           IF (
     :                       FINAL_DEV( IX ) .LT. LOWER_DEV_LIMIT .OR.
     :                       FINAL_DEV( IX ) .GT. UPPER_DEV_LIMIT ) THEN
                              TRACE( IX ) = ECH__BAD_REAL
                              FINAL_DEV( IX ) = ECH__BAD_REAL
                              JUST_CLIPPED = JUST_CLIPPED + 1
                           END IF
                        END IF
                     END IF
                  END IF
               END DO
               IF ( CLIP_MODE .EQ. 'AUTO' ) THEN
                  TRACE( MAX_AT ) = ECH__BAD_REAL
                  FINAL_DEV( MAX_AT ) = ECH__BAD_REAL
                  JUST_CLIPPED = JUST_CLIPPED + 1
               END IF
            END DO
         END IF
         CLIPPED = CLIPPED + JUST_CLIPPED
      END IF

*  Accept automatically clipped fit if max deviation falls below threshold.
      IF ( .NOT. INTERACTIVE .AND. MAX_DEVIATION .LT. END_CLIP_MAXDEV )
     :    ACCEPTED = .TRUE.

      END
