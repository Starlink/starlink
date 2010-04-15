*+  INVA - reports the value of a pixel of a 2-d array

      SUBROUTINE INVA( CURSOR, ZONEOV, ZONEO, XCEN, YCEN, LBND, DIM1,
     :                 DIM2, ARRAY, XLOW, XHIGH, YLOW, YHIGH, PNV,
     :                 STATUS )
*
*    Description :
*
*     This subroutine reports the value of a pixel of a 2-d array. If an
*     image display is available, then the cursor is used to define the
*     point, otherwise, the co-ordinates of the pixel come from the
*     environment.  In the former case repeated values may be obtained
*     until the escape choice is selected.
*
*    Invocation :
*
*      CALL INVA( CURSOR, ZONEOV, ZONEO, XCEN, YCEN, LBND, DIM1, DIM2,
*     :           ARRAY, XLOW, XHIGH, YLOW, YHIGH, PNV, STATUS )
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
*         Column centre of the image display (if %CURSOR is true)
*     YCEN = REAL ( READ )
*         Line centre of the image display (if %CURSOR is true)
*     LBND( 2 ) = INTEGER ( READ )
*         Lower bounds of the array, thus the first element of the
*         array (1,1) is actually at (LBND(1),LBND(2)).
*     DIM1 = INTEGER( READ )
*         The first dimension of the 2-d array.
*     DIM2 = INTEGER( READ )
*         The second dimension of the 2-d array.
*     ARRAY( DIM1, DIM2 ) = REAL ( READ )
*         2-d array for which values are to be read
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
*     PNV = CHARACTER ( READ )
*         ADAM parameter name of x-y co-ordinate of pixel, if CURSOR is
*         false.
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
*        Repeat until a valid position has been chosen or exit button
*          has been depressed or there has been an error
*           If button is exit then
*              Switch repeat button off
*           Else if first button is pressed then
*              If cursor was placed in the picture then
*                 Reset the last cursor position
*                 Convert cursor position to pixel co-ordinates
*                 Report value
*              Else
*                 Tell user was has happened
*              Endif
*           Endif
*        End repeat
*     Else
*        Obtain the pixel co-ordinates from the environment
*        If there is an error then
*           If not abort, flush the error table
*        Else
*           Report the value
*        Endif
*     Tidy parameters
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
*     1989 Apr 19 : Converted to graphics database (new argument list -
*                   ZONEOV for ZONEI) (RL.STAR::CUR).
*     1989 Jul 27 : Passed the array dimensions as two variables
*                   (RL.STAR::CUR).
*     1989 Oct 13 : Adjust for world co-ordinates being the picture
*                   extent rather than pixels (RL.STAR::CUR).
*     1990 Jan 9  : Corrected SGS status (RAL::CUR).
*     1991 Jun 10 : Added LBND argument as temporary patch for INSPECT
*                   to use NDF.  Merged the two parameters into one
*                   x-y position (RAL::CUR).
*     1992 Mar  3: Replaced AIF parameter-system calls by the extended
*                  PAR library (RAL::CUR).
*
*    Type Definitions :

      IMPLICIT NONE

*    Global Constants :

      INCLUDE 'SAE_PAR'        ! Global SSE definitions
      INCLUDE 'PRM_PAR'        ! Magic-value definitions
      INCLUDE 'PAR_ERR'        ! Parameter-system error definitions

*    Status :

      INTEGER STATUS

*    Import :

      INTEGER
     :  LBND( 2 ),
     :  DIM1, DIM2,
     :  ZONEOV,
     :  ZONEO

      LOGICAL CURSOR

      REAL
     :  ARRAY( DIM1, DIM2 ),
     :  XCEN, YCEN,
     :  XLOW, YLOW,
     :  XHIGH, YHIGH

      CHARACTER*(*)
     :  PNV

*    Local Variables :

      INTEGER
     :  VAPIX( 2 ),             ! x-y pixel indices
     :  VAPIXD( 2 ),            ! Suggested default x-y pixel indices
     :  UBND( 2 ),              ! Upper bounds of the array
     :  XV, YV,                 ! X,y co-ordinate of selected pixel
     :  BUTTON                  ! Number of button pressed on the
                                ! trackerball device to get x,y pair

      REAL
     :  VALUE,                  ! Value of selected pixel
     :  XL, YL,                 ! Last valid cursor position
     :  XCUR1,                  ! X co-ordinate of point from cursor
     :  YCUR1                   ! Y co-ordinate of point from cursor

      LOGICAL                   ! true if :
     :  REPEAT                  ! when a simulated REPEAT..UNTIL loop
                                ! is to continue

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .EQ. SAI__OK ) THEN

*       Find the upper bounds.

         UBND( 1 ) = LBND( 1 ) + DIM1 - 1
         UBND( 2 ) = LBND( 2 ) + DIM2 - 1

*       If an image display is available, then use the cursor to define
*       the point whose value is required. Otherwise, obtain the
*       co-ordinates of the point from the environment.

         IF ( CURSOR ) THEN

            REPEAT = .TRUE.

*          give function of trackerball buttons

            CALL SGS_SELZ( ZONEOV, STATUS )
            CALL BTTN3( 'VALUE', ' ', 'EXIT', '*', '*', .TRUE., STATUS )

            CALL SGS_SELZ( ZONEO, STATUS )
            XL = XCEN
            YL = YCEN

            DO WHILE ( REPEAT .AND. STATUS .EQ. SAI__OK )

               CALL SGS_SETCU( XL, YL )

*             Read choice device

               CALL SGS_REQCU( XCUR1, YCUR1, BUTTON )

*             Choice may be negative under GKS 7.4.

               IF ( BUTTON .LE. 0 .OR. BUTTON .GT. 3 ) THEN

                  REPEAT = .FALSE.

*             Allowing choice 3 is a temporary fudge to allow the first
*             button of an X-windows mouse to be used.  It will mean
*             that the right-hand white button on an ARGS will behave
*             like the red one (but is anyone still using an ARGS?)

               ELSE IF ( BUTTON .EQ. 1 .OR. BUTTON .EQ. 3 ) THEN

*                Check whether the co-ordinates of the position
*                defined by the cursor are actually within the array
*                displayed.

                  IF ( ( XCUR1 .GE. XLOW .AND. XCUR1 .LE. XHIGH ) .AND.
     :                 ( YCUR1 .GE. YLOW .AND. YCUR1 .LE. YHIGH ) ) THEN

*                   Reset last cursor position

                     XL = XCUR1
                     YL = YCUR1

*                   Convert the cursor position to find the selected
*                   data pixel.

                     XV = MIN( UBND( 1 ), MAX( LBND( 1 ),
     :                    INT( XCUR1 ) + 1 ) )
                     YV = MIN( UBND( 2 ), MAX( LBND( 2 ),
     :                    INT( YCUR1 ) + 1 ) )

*                   Get the value of the pixel.

                     CALL GETV2( ARRAY, DIM1, DIM2, XV - LBND( 1 ) + 1,
     :                           YV - LBND( 2 ) + 1, VALUE, STATUS )

*                   Output the result allowing for invalid pixels.

                     CALL MSG_OUT( 'LINE', ' ', STATUS )
                     IF ( VALUE .EQ. VAL__BADR ) THEN
                        CALL MSG_SETC( 'VAL', 'INVALID' )
                     ELSE
                        CALL MSG_SETR( 'VAL', VALUE )
                     END IF

                     CALL MSG_SETI( 'X', XV )
                     CALL MSG_SETI( 'Y', YV )
                     CALL MSG_OUT( 'MSG1', '   VALUE of image at ^X, '/
     :                 /'^Y is ^VAL', STATUS )
                     CALL MSG_OUT( 'LINE', ' ', STATUS )

                  ELSE

                     CALL MSG_OUT( 'INVA_POI', 'Cursor value not'/
     :                             /' within array.', STATUS )

                  END IF

*             End of which-button-pressed check

               END IF

*          End of loop to get cursor positions

            END DO

         ELSE

*          Set the defaults as the centre of the array.

            VAPIXD( 1 ) = ( LBND( 1 ) + UBND( 1 ) ) / 2
            VAPIXD( 2 ) = ( LBND( 2 ) + UBND( 2 ) ) / 2

*          Obtain the pixel index whose value is required.

            CALL PAR_GRM1I( PNV, 2, VAPIXD, LBND, UBND, .FALSE., VAPIX,
     :                      STATUS )

*          Use of XV and YV will be dispensed in the future.

            XV = VAPIX( 1 )
            YV = VAPIX( 2 )

            IF ( STATUS .NE. SAI__OK ) THEN
               IF ( STATUS .NE. PAR__ABORT ) CALL ERR_FLUSH( STATUS )
            ELSE

               CALL GETV2( ARRAY, DIM1, DIM2, XV - LBND( 1 ) + 1,
     :                     YV - LBND( 2 ) + 1, VALUE, STATUS )

*             Output the result allowing for invalid pixels.

               CALL MSG_OUT( 'LINE', ' ', STATUS )
               IF ( VALUE .EQ. VAL__BADR ) THEN
                  CALL MSG_SETC( 'VAL', 'INVALID' )
               ELSE
                  CALL MSG_SETR( 'VAL', VALUE )
               END IF

               CALL MSG_SETI( 'X', XV )
               CALL MSG_SETI( 'Y', YV )
               CALL MSG_OUT( 'MSG1', '   VALUE of image at ^X, '/
     :           /'^Y is ^VAL', STATUS )
               CALL MSG_OUT( 'LINE', ' ', STATUS )

            END IF

*          Tidy up

            CALL PAR_CANCL( PNV, STATUS )

         END IF
      END IF

      END
