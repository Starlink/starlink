      SUBROUTINE ECH_EDIT_PROFILE(
     :           NY,
     :           INTERACTIVE,
     :           PROFILING_MODE,
     :           N_ORDERS,
     :           PFL_SUBSAMPLES,
     :           SUBSTEPS,
     :           MAX_SKY_PIXELS,
     :           SUBSAMPLED_PROFILE,
     :           DEK_BELOW,
     :           DEK_ABOVE,
     :           OBJ_MASK,
     :           SKY_MASK,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_EDIT_PROFILE

*  Purpose:
*     Allow user to edit assignment of pixels as sky/object/dekker.

*  Description:
*     This routine plots an average spatial profile and provides for
*     interactive specification of the dekker / object / sky limits on
*     an order-by-order / or global basis.
*
*     Options are selected from a menu by striking single characters, the
*     carriage return is not required. The following options are available
*
*     A(nd), permits pixels to be specified as dual purpose for extraction
*     routines. The pixels may be both Object AND Sky, and each state may
*     be toggled serparately for each pixel in the spatial profile.
*     The AND and XOR options are mutually exclusive.
*
*     E(xit), leaves the profiling routine.
*
*     I(gnore), specifies that the pixel at the current x-position of the
*     graphics cursor be IGNORED in further processing. Pixels at this offset
*     will not be used for estimate the sky OR extracted as part of the
*     object.
*
*     L(ower dekker), sets the lower dekker limit to the current x-position
*     of the graphics cursor.
*
*     O(bject), specifies that the pixel at the current x-position of the
*     graphics cusror be treated as an Object pixel for the purposes of
*     extraction.
*
*     S(ky), specifies that the pixel at the current x-position of the
*     graphics cusror be treated as an Sky pixel for the purposes of
*     extraction.
*
*     U(pper dekker), sets the upper dekker limit to the current x-position
*     of the graphics cursor.
*
*     R(ange), sets the range of pixels between the current cursor position
*     and the centre of the order, to be object pixels. This is more
*     convenient than the 'O' option when there are many object pixels
*     which need specifying.
*
*     T(oggle replot mode), toggles the way in which re-plots are performed
*     after a pixels status has been edited. For some devices (eg TEK), it
*     is necessary to replot the entire graph,as graphics can not be switched
*     back on once they have been erased. Other devices may support changing
*     a display pixel state without re-plotting the whole screen. In this
*     case it is much quicker to replot only the part of the graph which
*     has actually changed. Typing R toggles between these two modes of
*     operation.
*
*     X(or), permits pixels to be specified exculsively as either Object
*     OR Sky pixel for the purposes of the  extraction routines. This state
*     is the default and will be most generally useful.
*     The AND and XOR options are mutually exclusive.
*
*     (Plus key), changes the selected order. The default is to edit ALL
*     orders at once, thus all orders will have the same sky/object pixel
*     allocations (in spatial dimension). But, in some cases it may be
*     advantageous to specify object pixels on an order by order basis.
*     Typing Plus from the All order situation will cause the first order
*     ONLY to be selected. In single order editing, the Plus key selects
*     the next order, except when the highest numbered order is the current
*     one, in which case it causes processing to revert to ALL order editing.
*
*     (Minus key), changes the selected order. As above except that the
*     order currently selected is decremented. Using the Minus key when
*     order 1 is selected  will revert to the ALL order editing situation.

*  Invocation:
*     CALL ECH_EDIT_PROFILE(
*    :     NY,
*    :     INTERACTIVE,
*    :     PROFILING_MODE,
*    :     N_ORDERS,
*    :     PFL_SUBSAMPLES,
*    :     SUBSTEPS,
*    :     MAX_SKY_PIXELS,
*    :     SUBSAMPLED_PROFILE,
*    :     DEK_BELOW,
*    :     DEK_ABOVE,
*    :     OBJ_MASK,
*    :     SKY_MASK,
*    :     STATUS
*    :     )

*  Arguments:
*     NY = INTEGER (Given)
*        Number of rows in input frame.
*     N_ORDERS = INTEGER (Given)
*        Number of orders in echellogram.
*     INTERACTIVE = LOGICAL (Given)
*        TRUE if user to edit profiles/limits.
*     PROFILING_MODE = CHAR (Given)
*        Mode selector: D-dekker limits, O-Object profile.
*     DEK_BELOW = INTEGER (Given and Returned)
*        Lower dekker limit in pixels from trace.
*     DEK_ABOVE = INTEGER (Given and Returned)
*        Upper dekker limit in pixels from trace.
*     MAX_SKY_PIXELS = INTEGER (Given and Returned)
*        Size of mask arrays.
*     OBJ_MASK = INTEGER (Given and Returned)
*        Non zero values denoting object presence.
*     SKY_MASK = INTEGER (Given and Returned)
*        Non zero values denoting object presence.
*     PFL_SUBSAMPLES = INTEGER (Given and Returned)
*        Number of subsamples to use for profiles.
*     SUBSAMPLED_PROFILE = REAL (Given and Returned)
*        Raw averaged profile intensities.
*     SUBSTEPS = INTEGER (Given and Returned)
*        Subsamples per pixel to use for profiles.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     Estimate the spatial extent in pixels
*     Set appropriate subsampling rate
*     Set plotting scope to twice the expected order profile size
*     Plot profile subsamples
*     If we are in interactive mode then
*        Loop while E(xit) option not selected yet
*           Display available choices and wait for selection
*           Process selected option
*           Option '+', steps to edit next order, or edit all orders again
*           Option 'M', steps to edit next order, or edit all orders again
*           Option '-', steps to edit previous order, or edit all orders again
*           Option 'A', enables each pixel to be flagged as both Object AND Sky
*           Option 'L', sets lower dekker limit to current cursor x position
*           Option 'U', sets upper dekker limit to current cursor x position
*           Option 'T', Toggles the graphics full screen refresh flag.
*           Option 'E', Exit, saving any changes made
*           Otherwise
*            If an Object profile is displayed then
*              Check for Object/Sky editing
*            For an Range Object selection, set the object mask and optionally clear
*            the Sky mask in the appropriate place
*              Calculate nearest whole pixel value corresponding to cursor position
*              Loop thru profile subsamples
*                 Calculate corresponding pixel value
*                 If its the same pixel as the one the user selected then
*                    For a Sky selection, set the sky mask and optionally clear
*                    the Object mask in the appropriate place
*                    For an Object selection, set the object mask and optionally clear
*                    the Sky mask in the appropriate place
*                    For an IGNORE selection, clear both masks appropriately
*                 Endif
*              Endloop
*            Endif
*           End of option processing
*           If still active then re-plot profile subsamples
*        End loop
*     Endif

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     29-APR-1997 (MJC):
*       Added prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_ENVIRONMENT.INC'

*  Arguments Given:
      INTEGER NY
      INTEGER N_ORDERS
      LOGICAL INTERACTIVE
      CHARACTER*( * ) PROFILING_MODE
      INTEGER DEK_BELOW( N_ORDERS )
      INTEGER DEK_ABOVE( N_ORDERS )
      INTEGER MAX_SKY_PIXELS
      INTEGER PFL_SUBSAMPLES

*  Arguments Returned:
      INTEGER OBJ_MASK( -MAX_SKY_PIXELS/2: MAX_SKY_PIXELS/2, N_ORDERS )
*          ! Non-zero where object pixels are.
      INTEGER SKY_MASK( -MAX_SKY_PIXELS/2: MAX_SKY_PIXELS/2, N_ORDERS )
*          ! Non-zero where sky pixels are.
      REAL SUBSAMPLED_PROFILE( -PFL_SUBSAMPLES/2 : PFL_SUBSAMPLES/2,
     :                         N_ORDERS )
*          ! Subsampled average profile intensities.

*  Status :
      INTEGER STATUS

*  Local Variables:
      INTEGER I
      INTEGER II
      INTEGER SPIXEL
      INTEGER SSTEP
      INTEGER ORDER_SIZE
      INTEGER DISP_SCALE
      INTEGER SUBSTEPS
      INTEGER DUMSTEPS
      INTEGER PIXEL
      INTEGER IORD
      INTEGER STORD
      INTEGER ENORD
      INTEGER NCHAR1
      INTEGER NCHAR2

      LOGICAL RANGE
      LOGICAL USER_EXIT
      LOGICAL NEEDS_REFRESH
      LOGICAL OBJ_AND_SKY
      LOGICAL FULL_MENU
      LOGICAL ALL_ORDERS

      CHARACTER*12 REF_STR1
      CHARACTER*8 REF_STR2
      CHARACTER*8 REF_STR3

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
      INTEGER CHR_LEN
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) )  RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :     CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Estimate the spatial extent in pixels, set appropriate subsampling rate.
      CALL ECH_CALC_PROFSAMP( NY, N_ORDERS, PFL_SUBSAMPLES, DEK_BELOW,
     :     DEK_ABOVE, ORDER_SIZE, DUMSTEPS, STATUS )
      IORD = MAX( 1, N_ORDERS / 2 )
      FULL_MENU = .TRUE.
      ALL_ORDERS = .TRUE.

*  Set plotting scope to twice the expected order profile size.
      DISP_SCALE = MIN( ORDER_SIZE * SUBSTEPS, PFL_SUBSAMPLES / 2 )
      DISP_SCALE = MIN( DISP_SCALE, MAX_SKY_PIXELS / 2 * SUBSTEPS )

*  Plot profile subsamples.
      NEEDS_REFRESH = .TRUE.
      CALL ECH_PLOT_PROFILE( NY, PROFILING_MODE, N_ORDERS, IORD,
     :     PFL_SUBSAMPLES, SUBSTEPS, MAX_SKY_PIXELS,
     :     SUBSAMPLED_PROFILE( -PFL_SUBSAMPLES / 2, IORD ),
     :     DEK_BELOW, DEK_ABOVE, OBJ_MASK, SKY_MASK, NEEDS_REFRESH,
     :     ALL_ORDERS, STATUS )

*  If we are not interactive the do nothing.
      IF ( .NOT. INTERACTIVE ) THEN
         GO TO 999
      END IF

      USER_EXIT = .FALSE.
      RANGE = .FALSE.

*  Loop while E(xit) option not selected yet.
      OBJ_AND_SKY = .FALSE.
      DO WHILE ( .NOT. USER_EXIT )

*     Display available choices and wait for selection.
         IF ( ALL_ORDERS ) THEN
            REPORT_STRING = ' Edit global profile information' //
     :            ' (all orders).'
            IORD = MAX( 1, N_ORDERS / 2 )

         ELSE
            CALL CHR_ITOC( IORD, REF_STR1, NCHAR1 )
            REPORT_STRING = ' Edit profile information for order ' //
     :            REF_STR1( :NCHAR1 ) // '.'
         ENDIF

  100    CONTINUE
         IF ( FULL_MENU ) THEN
            CALL ECH_REPORT( 0, REPORT_STRING )
            CALL ECH_REPORT( 0, ' Options:' )
            CALL ECH_REPORT( 0, '    + - Process next order.' )
            CALL ECH_REPORT( 0, '    - - Process previous order.' )
            CALL ECH_REPORT( 0, '    L - Set lower dekker limit.' )
            CALL ECH_REPORT( 0, '    U - Set upper dekker limit.' )
            CALL ECH_REPORT( 0, '    M - Full menu display.' )

*        Only display Sky/Object options when Object profile is being edited.
            IF ( PROFILING_MODE( :1 ) .NE. 'D' ) THEN
               CALL ECH_REPORT( 0,
     :              '    I - Set pixel(s) to be IGNORED.' )
               CALL ECH_REPORT( 0,
     :              '    S - Set pixel(s) to be used for SKY.' )
               CALL ECH_REPORT( 0,
     :              '    O - Set pixel(s) to be used for OBJECT.' )
               CALL ECH_REPORT( 0,
     :              '    R - Set start for a range of pixels.' )
               CALL ECH_REPORT( 0,
     :              '    X - Set Exclusive OR for SKY/OBJECT.' )
               CALL ECH_REPORT( 0,
     :              '    A - Allow pixels to be SKY AND OBJECT.' )
            END IF
            CALL ECH_REPORT( 0,
     :           '    T - Toggle full display refresh.' )
            CALL ECH_REPORT( 0, '    E - Exit.' )
            CALL ECH_REPORT( 0, ' ' )
            FULL_MENU = .FALSE.

         ELSE
            CALL ECH_REPORT( 0, REPORT_STRING )
            IF ( PROFILING_MODE( :1 ) .NE. 'D' ) THEN
               CALL ECH_REPORT( 0,
     :              ' Options [ + - U L M I S O X A R T E ]' )

            ELSE
               CALL ECH_REPORT( 0,
     :              ' Options [ + - U L M T E ]' )
            END IF
         ENDIF
         CALL ECH_READ_GRPH_CURSOR( STATUS )

*     Process selected option.
*     =======================

*     Option 'X', allows pixels to be both Sky and Object.
         IF ( USER_INPUT_CHAR .EQ. 'X' ) THEN
            OBJ_AND_SKY = .FALSE.
            CALL ECH_REPORT( 0,
     :      ' XOR set.  S option will toggle SKY and OBJECT flags.' )
            CALL ECH_REPORT( 0,
     :      ' XOR set.  O option will toggle SKY and OBJECT flags.' )

*     Option 'M', displays the full menu details..
         ELSE IF ( user_input_char .EQ. 'M' ) THEN
            FULL_MENU = .TRUE.
            REPORT_STRING = ' Displaying full menu...'
            GO TO 100

*     Option '+', steps to edit next order, or edit all orders again.
         ELSE IF ( USER_INPUT_CHAR .EQ. '+' ) THEN
            IF ( ALL_ORDERS ) THEN
               IORD = 1
               ALL_ORDERS = .FALSE.

            ELSE
               IORD = IORD + 1
               IF ( IORD .GT. N_ORDERS ) THEN
                  IORD = MAX( 1, N_ORDERS / 2 )
                  ALL_ORDERS = .TRUE.
               END IF
            END IF
            RANGE = .FALSE.

*     Option '-', steps to edit previous order, or edit all orders again.
         ELSE IF ( USER_INPUT_CHAR .EQ. '-' ) THEN
            IF ( ALL_ORDERS ) THEN
               IORD = N_ORDERS
               ALL_ORDERS = .FALSE.

            ELSE
               IORD = IORD - 1
               IF ( IORD .LT. 1 ) THEN
                  IORD = MAX( 1, N_ORDERS / 2 )
                  ALL_ORDERS = .TRUE.
               END IF
            END IF
            RANGE = .FALSE.

*     Option 'A', enables each pixel to be flagged as both Object AND Sky.
         ELSE IF ( USER_INPUT_CHAR .EQ. 'A' ) THEN
            OBJ_AND_SKY = .TRUE.
            CALL ECH_REPORT( 0,
     :          ' XOR set.  S option will toggle SKY mask only.' )
            CALL ECH_REPORT( 0,
     :          ' XOR set.  O option will toggle OBJECT mask only.' )

*     Options 'L' and 'U' set dekker limits to cursor X position.
         ELSE IF ( USER_INPUT_CHAR .EQ. 'L' .OR.
     :             USER_INPUT_CHAR .EQ. 'U' ) THEN

*        Calculate nearest pixel boundary corresponding to cursor position.
            IF ( X_CURSOR .GE. 0.0 ) THEN
               PIXEL = MIN( MAX_SKY_PIXELS / 2, INT( X_CURSOR ) )

            ELSE
               PIXEL = MAX( - MAX_SKY_PIXELS / 2, INT( X_CURSOR ) )
            ENDIF

            IF ( ALL_ORDERS ) THEN
               STORD = 1
               ENORD = N_ORDERS
               REF_STR1 = 'all orders'
               NCHAR1 = 10

            ELSE
               STORD = IORD
               ENORD = IORD
               CALL CHR_ITOC( IORD, REF_STR1, NCHAR1 )
               REF_STR1 = 'order ' // REF_STR1( :NCHAR1 )
               NCHAR1 = NCHAR1 + 6
            ENDIF

*        Option 'L', sets lower dekker limit to cursor X position.
            IF ( USER_INPUT_CHAR .EQ. 'L' ) THEN
               DO I = STORD, ENORD
                  DEK_BELOW( I ) = MIN( PIXEL, DEK_ABOVE( I ) )
               END DO
               PIXEL = MIN( PIXEL, DEK_ABOVE( STORD ) )
               REPORT_STRING = 'Lower'

*        Option 'U', sets upper dekker limit to current cursor x position.
            ELSE
               DO I = STORD, ENORD
                  DEK_ABOVE( I ) = MAX( PIXEL, DEK_BELOW( I ) )
               END DO
               PIXEL = MAX( PIXEL, DEK_BELOW( STORD ) )
               REPORT_STRING = 'Upper'
            END IF

            CALL CHR_ITOC( PIXEL, REF_STR2, NCHAR2 )
            IF ( PIXEL .GT. 0 ) THEN
               REF_STR2 = '+' // REF_STR2( :NCHAR2 )
               NCHAR2 = NCHAR2 + 1

            ELSE IF ( PIXEL .EQ. 0 ) THEN
               REF_STR2 = 'zero'
               NCHAR2 = 4
            END IF

            REPORT_STRING = ' ' // REPORT_STRING( :5 ) //
     :            ' dekker-limit (' // REF_STR1( :NCHAR1 ) //
     :            ') set at offset ' // REF_STR2( :NCHAR2 ) //
     :            ' from trace centre.'
            CALL ECH_REPORT( 0, REPORT_STRING )

*     Option 'T', Toggles the graphics full screen refresh flag.
*     (Some devices are not capable of erasing plotted pixels without
*     clearing the entire screen Eg. Tektronix 4010, other devices
*     CAN overwrite pixels, for these we may just replot the changed
*     part of the plot without having to clear the entire screen and
*     replot everythying).
         ELSE IF ( USER_INPUT_CHAR .EQ. 'T' ) THEN
            NEEDS_REFRESH = .NOT. NEEDS_REFRESH
            IF  ( NEEDS_REFRESH ) THEN
               CALL ECH_REPORT( 0,
     :            ' Assuming display requires full refresh.' )
            ELSE
               CALL ECH_REPORT( 0,
     :            ' Assuming display supports selective refresh.' )
            ENDIF

*     Option 'E', Exit, saving any changes made.
         ELSE IF ( USER_INPUT_CHAR .EQ. 'E' ) THEN
            CALL ECH_REPORT( 0, ' EXIT selected.' )
            USER_EXIT = .TRUE.

*     Otherwise, if an Object profile is displayed then
*     check for Object/Sky editing.
         ELSE IF ( PROFILING_MODE( :1 ) .NE. 'D' ) THEN

*        Calculate nearest whole pixel value corresponding to
*        cursor position - here we use pixel centres.
            IF ( X_CURSOR .GE. 0.0 ) THEN
               PIXEL = MIN( MAX_SKY_PIXELS / 2, INT( X_CURSOR + 0.5 ) )

            ELSE
               PIXEL = MAX( - MAX_SKY_PIXELS / 2, INT( X_CURSOR -
     :               0.5 ) )
            END IF

*        For a Range Object selection, set the object mask and optionally
*        clear the Sky mask in the appropriate place.
            IF ( USER_INPUT_CHAR .EQ. 'R' ) THEN
               SPIXEL = PIXEL
               RANGE = .TRUE.
               CALL CHR_ITOC( PIXEL, REF_STR1, NCHAR1 )
               REPORT_STRING = ' Range of pixels started at offset'//
     :               ' ' // REF_STR1( :NCHAR1 ) // '.'
               CALL ECH_REPORT( 0, REPORT_STRING )
               CALL ECH_REPORT( 0,
     :           ' Move cursor to end of range and hit O, S or I.' )

*        Determine start and end order to which any changes will apply
*        (At present this is just ONE, or ALL, but it could easily
*        be changed to allow editing of a range of orders profiles' at once)
            ELSE IF ( USER_INPUT_CHAR .EQ. 'S' .OR.
     :                USER_INPUT_CHAR .EQ. 'O' .OR.
     :                USER_INPUT_CHAR .EQ. 'I' ) THEN
               IF ( ALL_ORDERS ) THEN
                  STORD = 1
                  ENORD = N_ORDERS

               ELSE
                  STORD = IORD
                  ENORD = IORD
               ENDIF

               IF ( .NOT. RANGE ) THEN
                  SPIXEL = PIXEL
               END IF
               IF ( PIXEL .LT. SPIXEL ) THEN
                  SSTEP = 1

               ELSE
                  SSTEP = -1
               END IF
               DO I = STORD, ENORD

*              For a Sky selection, set the sky mask and optionally clear
*              the Object mask in the appropriate place.
                  IF ( USER_INPUT_CHAR .EQ. 'S' ) THEN
                     DO II = PIXEL, SPIXEL, SSTEP
                        SKY_MASK( II, I ) = 1
                        IF ( .NOT. OBJ_AND_SKY ) OBJ_MASK( II, I ) = 0
                     END DO

*              For an Object selection, set the object mask and optionally clear
*              the Sky mask in the appropriate place
                  ELSE IF ( USER_INPUT_CHAR .EQ. 'O' ) THEN
                     DO II = PIXEL, SPIXEL, SSTEP
                        OBJ_MASK( II, I ) = 1
                        IF ( .NOT. OBJ_AND_SKY ) SKY_MASK( II, I ) = 0
                     END DO

*              For an IGNORE selection, clear both masks appropriately
                  ELSE
                     DO II = PIXEL, SPIXEL, SSTEP
                        OBJ_MASK( II, I ) = 0
                        SKY_MASK( II, I ) = 0
                     END DO
                  END IF
               END DO

*           Report any problems.
               DO II = PIXEL, SPIXEL, SSTEP
                  IF ( II .LT. DEK_BELOW( STORD ) .OR.
     :                 II .GT. DEK_ABOVE( STORD ) ) THEN
                     CALL CHR_ITOC( II, REF_STR1, NCHAR1 )
                     IF ( II .GT. 0 ) THEN
                        REF_STR1 = '+' // REF_STR1( :NCHAR1 )
                        NCHAR1 = NCHAR1 + 1
                     END IF
                     REPORT_STRING = ' Warning: Pixel ' //
     :                     REF_STR1( :NCHAR1 ) // ' is' //
     :                     ' outside the dekker and will' //
     :                     ' not be used.'
                     CALL ECH_REPORT( 0, REPORT_STRING )
                  END IF
               END DO

*           Report action.
               CALL CHR_ITOC( PIXEL, REF_STR1, NCHAR1 )
               IF ( PIXEL .GT. 0 ) THEN
                  REF_STR1 = '+' // REF_STR1( :NCHAR1 )
                  NCHAR1 = NCHAR1 + 1

               ELSE IF ( PIXEL .EQ. 0 ) THEN
                  REF_STR1 = 'zero'
                  NCHAR1 = 4
               END IF
               IF ( RANGE ) THEN
                  CALL CHR_ITOC( SPIXEL, REF_STR2, NCHAR2 )
                  IF ( SPIXEL .GT. 0 ) THEN
                     REF_STR2 = '+' // REF_STR2( :NCHAR2 )
                     NCHAR2 = NCHAR2 + 1

                  ELSE IF ( SPIXEL .EQ. 0 ) THEN
                     REF_STR2 = 'zero'
                     NCHAR2 = 4
                  END IF
                  REPORT_STRING = ' Pixels with trace-offsets' //
     :                  ' from ' // REF_STR2( :NCHAR2 ) //
     :                  ' to ' // REF_STR1( :NCHAR1 ) //
     :                  ' set to '
                  RANGE = .FALSE.

               ELSE
                  REPORT_STRING = ' Pixels at offset ' //
     :                  REF_STR1( :NCHAR1 ) //
     :                  ' from trace centre set to '
               ENDIF
               IF ( USER_INPUT_CHAR .EQ. 'S' ) THEN
                  REF_STR3 = 'Sky.'

               ELSE IF ( USER_INPUT_CHAR .EQ. 'O' ) THEN
                  REF_STR3 = 'Object.'

               ELSE
                  REF_STR3 = 'be Ignored.'
               END IF
               REPORT_STRING = REPORT_STRING(
     :               :CHR_LEN( REPORT_STRING ) + 1 ) // REF_STR3
               CALL ECH_REPORT( 0, REPORT_STRING )

*        Unknown option.
            ELSE
               REPORT_STRING = ' Unknown Option: "' //
     :                USER_INPUT_CHAR // '".'
               GO TO 100
            END IF

*     Unknown option.
         ELSE
            REPORT_STRING = ' Unknown Option: "' //
     :            USER_INPUT_CHAR // '".'
            GO TO 100
         END IF

*     If still active then re-plot profile subsamples.
         IF ( .NOT. USER_EXIT .AND. USER_INPUT_CHAR .NE. 'R' ) THEN
            CALL ECH_PLOT_PROFILE( NY, PROFILING_MODE, N_ORDERS,
     :           IORD, PFL_SUBSAMPLES, SUBSTEPS, MAX_SKY_PIXELS,
     :           SUBSAMPLED_PROFILE( -PFL_SUBSAMPLES / 2, IORD ),
     :           DEK_BELOW, DEK_ABOVE, OBJ_MASK, SKY_MASK,
     :           NEEDS_REFRESH, ALL_ORDERS, STATUS )
         END IF
      END DO

  999 CONTINUE

      END
