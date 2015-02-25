*+  INRE - defines a sub-array or region within a 2-d array

      SUBROUTINE INRE( CURSOR, ZONEOV, ZONEO, XCEN, YCEN, LBND, DIMS,
     :                 XLOW, XHIGH, YLOW, YHIGH, PNLB, PNUB,
     :                 X1, X2, Y1, Y2, STATUS )
*
*    Description :
*
*     This subroutine defines a region of a 2-d array. If an image
*     display is available, then the cursor is used to define the
*     area, otherwise, the co-ordinates of the region come from the
*     environment.
*
*    Invocation :
*
*      CALL INRE( CURSOR, ZONEOV, ZONEO, XCEN, YCEN, LBND, DIMS,
*     :           XLOW, XHIGH, YLOW, YHIGH, PNLB, PNUB,
*     :           X1, X2, Y1, Y2, STATUS )
*
*    Arguments :
*
*     CURSOR = LOGICAL ( READ )
*         True if the cursor of the image is required
*     ZONEOV = INTEGER ( READ )
*         Frame zone identifier on the image-display overlay plane
*           (if %CURSOR is true)
*     ZONEO = INTEGER ( READ )
*         Data zone identifier on the image-display overlay plane
*           (if %CURSOR is true)
*     XCEN = REAL ( READ )
*         Column centre of the image (if %CURSOR is true)
*     YCEN = REAL ( READ )
*         Line centre of the image (if %CURSOR is true)
*     LBND( 2 ) = INTEGER ( READ )
*         Lower bounds of the array, thus the first element of the
*         array (1,1) is actually at (LBND(1),LBND(2)).
*     DIMS( 2 ) = INTEGER ( READ )
*         Dimensions of 2-d array
*     XLOW = REAL ( READ )
*         Minimum x-value that the cursor can take before being outside
*           the 2-d array displayed (if %CURSOR is true)
*     XHIGH = REAL ( READ )
*         Maximum x-value that the cursor can take before being outside
*           the 2-d array displayed (if %CURSOR is true)
*     YLOW = REAL ( READ )
*         Minimum y-value that the cursor can take before being outside
*           the 2-d array displayed (if %CURSOR is true)
*     YHIGH = REAL ( READ )
*         Maximum y-value that the cursor can take before being outside
*           the 2-d array displayed (if %CURSOR is true)
*     PNLB = CHARACTER ( READ )
*         ADAM parameter name used for the lower bound of the region
*         to be defined.  It is only used if CURSOR is false.
*     PNUB = CHARACTER ( READ )
*         ADAM parameter name used for the upper bound of the region
*         to be defined.  It is only used if CURSOR is false.
*     X1 = INTEGER ( READ, WRITE )
*         First x co-ordinate that defines a region (if %CURSOR is
*           false)
*     X2 = INTEGER ( READ, WRITE )
*         Second x co-ordinate that defines a region (if %CURSOR is
*           false)
*     Y1 = INTEGER ( READ, WRITE )
*         First y co-ordinate that defines a region (if %CURSOR is
*           false)
*     Y2 = INTEGER ( READ, WRITE )
*         Second y co-ordinate that defines a region (if %CURSOR is
*           false)
*     STATUS  = INTEGER( READ, WRITE )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     If cursor mode then
*        Select overlay frame zone and get bounds
*        Show functions of the choice device's buttons
*        Select overlay plane
*        Repeat until two final positions have been chosen or exit
*          button has been depressed or there is an error
*           Repeat until the first position has been chosen or there
*             is an error
*              Obtain a valid cursor position, i.e. within array
*              Reset last cursor position to its current position
*              If exit button has been pressed set repeat switch to off
*              Else if first button has been pressed then
*                 Display next functions of the choice device and
*                   locator in frame zone
*                 Mark the chosen point with a green cross
*                 Switch off first-point repeat
*              Else
*                 Display next functions of the choice device and
*                   locator in frame zone
*                 Mark the provisional point with a white cross
*              Endif
*           End repeat
*           Repeat until the second position has been chosen or there
*             is an error
*              Obtain a valid cursor position, i.e. within array
*              Reset last cursor position to its current position
*              If exit button has been pressed set repeat switch to
*                off and clear the overlay plane
*              Else if first button has been pressed then
*                 If the second position is different from the
*                   first then
*                    Erase previous line drawing
*                    Draw a green rectangle joining the two points
*                      which define the region
*                    Mark the chosen point with a green cross
*                    Convert cursor positions to pixels in the array
*                    Report the co-ordinates of the region
*                    Switch off second-point and main repeat switches
*                 Else
*                    Report the error and try again
*                 Endif
*              Else
*                 If the second position is different from the
*                   first then
*                    Draw a white rectangle joining the two points
*                      which define the region
*                 Else
*                    Try again
*                 Endif
*              Endif
*           End repeat
*        End repeat
*        Set region bounds to the correct polarity
*     Else
*        Obtain the two  bounds from the terminal such that
*          the two positions are different
*        Tidy parameters
*     Endif
*     End
*
*    Authors :
*
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*
*     1988 Apr 9 : Original extracted from INSPECT (RL.STAR::CUR)
*     1988 Jun 23: More error checking (RL.STAR::CUR).
*     1988 Sep 12 : Gave subroutine CROSS extra argument (RL.STAR::CUR).
*     1989 Apr 19 : Converted to graphics database (new argument list -
*                   ZONEOV for ZONEI, no WKIDOV) (RL.STAR::CUR).
*     1989 May 24 : Fixed bug so that region limits have correct polarity
*                   (RL.STAR::CUR).
*     1989 Oct 13 : Adjust for world co-ordinates being the picture
*                   extent rather than pixels (RL.STAR::CUR).
*     1990 Jan 9  : Corrected SGS status (RL.STAR::CUR).
*     1991 Jun 10 : Added LBND argument as temporary patch for INSPECT
*                   to use NDF.  Used lower and upper bounds parameters
*                   instead of 4 x and y parameters. (RAL::CUR).
*     1992 Mar  3: Replaced AIF parameter-system calls by the extended
*                  PAR library (RAL::CUR).
*
*    Type Definitions :

      IMPLICIT NONE

*    Global Constants :

      INCLUDE 'SAE_PAR'         ! SSE definitions

*    Status :

      INTEGER STATUS

*    Import :

      INTEGER
     :  LBND( 2 ),
     :  DIMS( 2 ),
     :  ZONEO,
     :  ZONEOV

      LOGICAL CURSOR

      REAL
     :  XCEN, YCEN,
     :  XLOW, YLOW,
     :  XHIGH, YHIGH

      CHARACTER*(*)
     :  PNLB,
     :  PNUB

*    Import-Export :

      INTEGER
     :  X1, Y1,
     :  X2, Y2

*    Local Variables :

      INTEGER
     :  LIML( 2 ),              ! Pixel limits below which the slice
                                ! may not terminate
     :  LIMU( 2 ),              ! Pixel limits above which the slice
                                ! may not terminate
     :  RELBND( 2 ),            ! Region lower bounds
     :  RELDEF( 2 ),            ! Suggestrd default region lower bounds
     :  REUBND( 2 ),            ! Region upper bounds
     :  REUDEF( 2 ),            ! Suggested default region upper bounds
     :  T,                      ! Work variable to swap region bounds
     :  UBND( 2 ),              ! Upper bounds of the array
     :  BUTTN1,                 ! Number of button pressed on the
                                ! trackerball device to get first x,y
     :  BUTTN2                  ! Number of trackerball button pressed
                                ! to get second x,y co-ordinate pair

      REAL
     :  A, B,                   ! General variables
     :  XCUR01,                 ! First x co-ordinate from cursor
                                ! that defines the region
     :  XCUR02,                 ! Second x co-ordinate from cursor
                                ! that defines the region
     :  XF1,                    ! Lower x bound of frame zone
     :  XF2,                    ! Upper x bound of frame zone
     :  XL, YL,                 ! Last valid cursor position
     :  XM,                     ! Frame zone size in x (dummy)
     :  YCUR01,                 ! First y co-ordinate from cursor
                                ! that defines the region
     :  YCUR02,                 ! Second y co-ordinate from cursor
                                ! that defines the region
     :  YF1,                    ! Lower y bound of frame zone
     :  YF2,                    ! Upper y bound of frame zone
     :  YM                      ! Frame zone size in y (dummy)

      LOGICAL                   ! true if :
     :  AGAIN, AGAIN1,          ! when a simulated REPEAT..UNTIL loop
                                ! is to continue
     :  AGAIN2, REPEAT          ! when a simulated REPEAT..UNTIL loop
                                ! is to continue

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .EQ. SAI__OK ) THEN

*       Find the upper bounds.

         UBND( 1 ) = LBND( 1 ) + DIMS( 1 ) - 1
         UBND( 2 ) = LBND( 2 ) + DIMS( 2 ) - 1

*       If an image display is available then use the cursor to define
*       two distinct, valid points.

         IF ( CURSOR ) THEN

            REPEAT = .TRUE.
            AGAIN1 = .TRUE.
            AGAIN2 = .TRUE.

*          Get bounds of frame zone

            CALL SGS_SELZ( ZONEOV, STATUS )
            CALL SGS_IZONE( XF1, XF2, YF1, YF2, XM, YM )

*          give function of trackerball buttons on overlay plane

            CALL BTTN3( 'ACCEPT', ' ', 'EXIT', '*', '*', .TRUE.,
     :                  STATUS )

            CALL SGS_SELZ( ZONEO, STATUS )

            XL = XCEN
            YL = YCEN

            DO WHILE ( REPEAT .AND. STATUS .EQ. SAI__OK )

*             Get the first co-ordinates.

               DO WHILE ( AGAIN1 .AND. STATUS .EQ. SAI__OK )

                  AGAIN = .TRUE.

                  DO WHILE ( AGAIN )

                     CALL SGS_SETCU( XL, YL )
                     CALL SGS_REQCU( XCUR01, YCUR01, BUTTN1 )

*                   Convert the `break' on a mouse (the middle button)
*                   for image displays to the GKS 7.2 behaviour, so that
*                   the interaction can be fully under mouse control.
                     IF ( BUTTN1 .EQ. 0 ) BUTTN1 = 2

*                   Check whether they lie within the array.

                     IF ( ( XCUR01 .GE. XLOW .AND. XCUR01 .LE. XHIGH )
     :                    .AND. ( YCUR01 .GE. YLOW .AND.
     :                            YCUR01 .LE. YHIGH ) ) THEN

                        AGAIN = .FALSE.

*                      Reset last cursor position

                        XL = XCUR01
                        YL = YCUR01

                     ELSE

*                      Choice may be negative under GKS 7.4.

                        IF ( BUTTN1 .GT. 0 ) THEN
                           CALL MSG_OUT( 'INRE_POI', 'Point lies '/
     :                                   /'outside the array.', STATUS )

*                         Reset the trackerball/mouse.

                           BUTTN1 = 0
                        ELSE

*                         Exit the loop as the exit trigger has been
*                         set.

                           AGAIN = .FALSE.
                        END IF
                     END IF
                  END DO

*                If the button is BREAK, then exit.

                  IF ( BUTTN1 .LE. 0 .OR. BUTTN1 .GT. 3 ) THEN

                     AGAIN1 = .FALSE.
                     AGAIN2 = .FALSE.
                     REPEAT = .FALSE.

*                If the button is CHOICE 1 or 3, then put a cross at
*                the point.  Allowing choice 3 is a temporary fudge to
*                allow the first button of an X-windows mouse to be
*                used.  It will mean that the right-hand white button
*                on an ARGS will behave like the red one (but is anyone
*                still using an ARGS?)

                  ELSE IF ( BUTTN1 .EQ. 1 .OR. BUTTN1 .EQ. 3 ) THEN

                     CALL SGS_SELZ( ZONEOV, STATUS )
                     CALL BTTN3( 'ACCEPT', 'SAMPLE', 'EXIT',
     :                           'BOX WIDTH', 'BOX HEIGHT', .TRUE.,
     :                           STATUS )
                     CALL SGS_SELZ( ZONEO, STATUS )

                     CALL SGS_SPEN ( 3 )
                     CALL KPG1_CROSS( XCUR01, YCUR01, 5., STATUS )
                     AGAIN1 = .FALSE.

*                Otherwise put a cross at the point return for another
*                point until the user presses the CHOICE 1 or 3 button.

                  ELSE
                     CALL SGS_SELZ( ZONEOV, STATUS )
                     CALL BTTN3( 'ACCEPT', 'SAMPLE', 'EXIT',
     :                           'BOX WIDTH','BOX HEIGHT', .TRUE.,
     :                           STATUS )

                     CALL SGS_SELZ( ZONEO, STATUS )
                     CALL SGS_SPEN( 1 )
                     CALL KPG1_CROSS( XCUR01, YCUR01, 5., STATUS )

*                End of which-button-pressed check

                  END IF

*             End of loop to get first point

               END DO

*             Get the second set of co-ordinates.

               A = XCUR01
               B = YCUR01
               XCUR02 = A
               YCUR02 = B
               XL = XCUR01
               YL = YCUR01

               DO WHILE ( AGAIN2 .AND. STATUS .EQ. SAI__OK )

                  AGAIN = .TRUE.

                  DO WHILE ( AGAIN )

*                   Set and get cursor position and button pressed

                     CALL SGS_SETCU( XL, YL )
                     CALL SGS_REQCU( XCUR02, YCUR02, BUTTN2 )

*                   Convert the no-choice into the middle button as in
*                   Convert the `break' on a mouse (the middle button)
*                   for image displays to the GKS 7.2 behaviour, so that
*                   the interaction can be fully under mouse control.
                     IF ( BUTTN2 .EQ. 0 ) BUTTN2 = 2

*                   Check whether they lie within the array.

                      IF ( ( XCUR02 .GE. XLOW .AND. XCUR02 .LE. XHIGH )
     :                    .AND. ( YCUR02 .GE. YLOW .AND.
     :                            YCUR02 .LE. YHIGH ) ) THEN

                        AGAIN = .FALSE.

*                      Reset last cursor position

                        XL = XCUR02
                        YL = YCUR02

                     ELSE

*                      There may be a negative button returned by
*                      GKS 7.4.

                        IF ( BUTTN2 .GT. 0 ) THEN
                           CALL MSG_OUT( 'INRE_POI', 'Point lies '/
     :                                   /'outside the array.', STATUS )

*                         Reset the trackerball/mouse.

                           BUTTN2 = 0
                           XCUR02 = A
                           YCUR02 = B
                        ELSE

*                         Exit the loop as the exit trigger has been
*                         set.

                           AGAIN = .FALSE.
                        END IF
                     END IF
                  END DO

*                If the button is BREAK, then exit.

                  IF ( BUTTN2 .LE. 0 .OR. BUTTN2 .GT. 3 ) THEN

                     CALL SGS_SELZ( ZONEOV, STATUS )
                     CALL SGS_CLRBL( XF1, XF2, YF1, YF2 )
                     CALL SGS_SELZ( ZONEO, STATUS )
                     AGAIN2 = .FALSE.
                     REPEAT = .FALSE.

*                If the button is CHOICE 1 or 3, then accept the region.
*                Allowing choice 3 is a temporary fudge to allow the
*                first button of an X-windows mouse to be used.  It
*                will mean that the right-hand white button on an ARGS
*                will behave like the red one (but is anyone still
*                using an ARGS?)

                  ELSE IF ( BUTTN2 .EQ. 1 .OR. BUTTN2 .EQ. 3 ) THEN

*                   Check that the points are distinct.

                     IF ( ( XCUR01 .NE. XCUR02 ) .OR.
     :                    ( YCUR01 .NE. YCUR02 ) ) THEN

*                      Draw the region in green after erasing
*                      any earlier region.

                        CALL SGS_CLRBL( XCUR01, A, YCUR01, B )
                        CALL SGS_SPEN ( 3 )
                        CALL KPG1_CROSS( XCUR01, YCUR01, 5., STATUS )
                        CALL SGS_BOX( XCUR01, XCUR02, YCUR01, YCUR02)
                        CALL SGS_FLUSH

                        X1 = MIN( UBND( 1 ), MAX( LBND( 1 ),
     :                       INT( XCUR01 ) + 1 ) )
                        Y1 = MIN( UBND( 2 ), MAX( LBND( 2 ),
     :                       INT( YCUR01 ) + 1 ) )
                        X2 = MIN( UBND( 1 ), MAX( LBND( 1 ),
     :                       INT( XCUR02 ) + 1 ) )
                        Y2 = MIN( UBND( 2 ), MAX( LBND( 2 ),
     :                       INT( YCUR02 ) + 1 ) )

*                      Report the pixel indices of the region.

                        CALL MSG_SETI( 'INRE_X1', X1 )
                        CALL MSG_SETI( 'INRE_Y1', Y1 )
                        CALL MSG_SETI( 'INRE_X2', X2 )
                        CALL MSG_SETI( 'INRE_Y2', Y2 )
                        CALL MSG_OUT( 'INRE_RES', 'Pixel bounds are '/
     :                    /'( ^INRE_X1, ^INRE_Y1 ) and '/
     :                    /'( ^INRE_X2, ^INRE_Y2 ).', STATUS )

*                      Switch off the repeat flags as we have valid
*                      bounds .

                        AGAIN2 = .FALSE.
                        REPEAT = .FALSE.

                     ELSE

*                      Otherwise, if the points are indistinct, then
*                      get new points.

                        CALL MSG_OUT( 'INRE_TPR', 'Two distinct '/
     :                                /'points required.', STATUS )
                        AGAIN1 = .TRUE.
                        AGAIN2 = .TRUE.

                     END IF

*                If any other button is pressed, draw a white region.

                  ELSE IF ( XCUR02 .NE. A .OR. YCUR02 .NE. B ) THEN

                     CALL SGS_CUVIS( .FALSE. )


*                   Draw the region after erasing old region.

                     CALL SGS_CLRBL( XCUR01, A, YCUR01, B )
                     CALL SGS_SPEN ( 3 )
                     CALL KPG1_CROSS( XCUR01, YCUR01, 5., STATUS )
                     CALL SGS_SPEN( 1 )
                     CALL SGS_BOX( XCUR01, XCUR02, YCUR01, YCUR02 )
                     CALL SGS_FLUSH

                     A = XCUR02
                     B = YCUR02
                     AGAIN2 = .TRUE.
                     CALL SGS_CUVIS( .TRUE. )

                  ELSE

                     A = XCUR02
                     B = YCUR02
                     AGAIN2 = .TRUE.

*                End of which-button-pressed check

                  END IF

*             End of repeat until loop for second co-ordinate pair

               END DO

*          End of main repeat until loop

            END DO

            CALL SGS_SPEN( 1 )

*          Make sure that X1/Y1 is the lower coordinate

            T = MAX( X1, X2 )
            X1 = MIN( X1, X2 )
            X2 = T
            T = MAX( Y1, Y2 )
            Y1 = MIN( Y1, Y2 )
            Y2 = T

*       the REgion is to be defined from the environment

         ELSE

*          Set the defaults as the current region.

            RELDEF( 1 ) = X1
            REUDEF( 1 ) = X2
            RELDEF( 2 ) = Y1
            REUDEF( 2 ) = Y2

*          Store the maximum and minimum values permitted for passing
*          to the PAR routine.

            LIML( 1 ) = LBND( 1 )
            LIML( 2 ) = LBND( 2 )
            LIMU( 1 ) = UBND( 1 ) - 1
            LIMU( 2 ) = UBND( 2 ) - 1

*          Obtain the lower bound of the region.

            CALL PAR_GRM1I( PNLB, 2, RELDEF, LIML, LIMU, .FALSE.,
     :                      RELBND, STATUS )

*          Revise minimum values permitted for the upper bound of the
*          region.  The limits ensure that there are at least two
*          pixels in either dimension.

            LIML( 1 ) = RELBND( 1 ) + 1
            LIML( 2 ) = RELBND( 2 ) + 1
            LIMU( 1 ) = UBND( 1 )
            LIMU( 2 ) = UBND( 2 )

*          Obtain the upper bound of the region.

            CALL PAR_GRM1I( PNUB, 2, REUDEF, LIML, LIMU, .FALSE.,
     :                      REUBND, STATUS )

*          Set the defaults as the current region.  Use of X1, X2, etc.
*          will be dispensed in the future.

            X1 = RELBND( 1 )
            X2 = REUBND( 1 )
            Y1 = RELBND( 2 )
            Y2 = REUBND( 2 )

*          Tidy up so that subroutine may be called again

            CALL PAR_CANCL( PNLB, STATUS )
            CALL PAR_CANCL( PNUB, STATUS )

*       end of cursor-mode check

         END IF

*    end of no-error-on-entry check

      END IF

      END
