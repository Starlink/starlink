*+  INSL - defines, plots and stores a slice through a 2-d array

      SUBROUTINE INSL( CURSOR, GRAPHS, ZONEOV, ZONEO, ZONEG, XCEN, YCEN,
     :                 NDFI, LBND, DIM1, DIM2, ARRAY, XLOW, XHIGH, YLOW,
     :                 YHIGH, PNLB, PNUB, PNNAME, PNTITL, NULL, PLTITL,
     :                 ABSLAB, ORDLAB, MINTIC, MAJTIC, XLOG, YLOG,
     :                 OUTTIC, THICK, X1, X2, Y1, Y2, STATUS )
*
*    Description :
*
*     This subroutine obtains the end points of a cross-section through
*     a 2-d array.  From this a slice is calculated, which may be
*     plotted if a graphics device is available, and optionally stored
*     in an NDF.
*
*     If an image display is available, then the cursor is used to
*     define the two end points of the slice, otherwise, their
*     co-ordinates come from the environment.
*
*     The plotting style may be controlled: the axis tick marks can
*     be drawn inside or outside the axes; either or both axes may be
*     logarithmic; the numbers of major and minor tick marks per axis
*     may be adjusted for linear axes, or let the graphics package
*     decide, the number of minor tick marks per major tick is fixed
*     (8) for a logarithmic axis; and finally the whole plot may be
*     drawn with a thicker pen.
*
*    Invocation :
*
*      CALL INSL( CURSOR, GRAPHS, ZONEOV, ZONEO, ZONEG, XCEN, YCEN,
*     :           NDFI, LBND, DIM1, DIM2, ARRAY, XLOW, XHIGH, YLOW,
*     :           YHIGH, PNLB, PNUB, PNNAME, PNTITL, NULL, PLTITL,
*     :           ABSLAB, ORDLAB, MINTIC, MAJTIC, XLOG, YLOG, OUTTIC,
*     :           THICK, X1, X2, Y1, Y2, STATUS )
*
*    Arguments :
*
*     CURSOR = LOGICAL ( READ )
*         True if the cursor of the image is required
*     GRAPHS = LOGICAL ( READ )
*         True if a plot of the slice is required
*     ZONEOV = INTEGER ( READ )
*         Frame zone identifier of image-display overlay plane
*           (if %CURSOR is true)
*     ZONEO = INTEGER ( READ )
*         Data zone identifier of image-display overlay plane
*           (if %CURSOR is true)
*     ZONEG = INTEGER ( READ )
*         Zone identifier of graphics device (if %GRAPHS is true)
*     XCEN = REAL ( READ )
*         Column centre of the image display (if %CURSOR is true)
*     YCEN = REAL ( READ )
*         Line centre of the image display (if %CURSOR is true)
*     NDFI = INTEGER ( READ )
*         NDF identifier of the input NDF associated with the 2-d array.
*         It is used to propagate the data units and label to the
*         output slice NDF.
*     LBND( 2 ) = INTEGER ( READ )
*         Lower bounds of the array, thus the first element of the
*         array (1,1) is actually at (LBND(1),LBND(2)).
*     DIM1 = INTEGER( READ )
*         The first dimension of the 2-d array.
*     DIM2 = INTEGER( READ )
*         The second dimension of the 2-d array.
*     ARRAY( DIM1, DIM2 ) = REAL ( READ )
*         2-d array for which slices are to be computed
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
*         ADAM parameter name used for the lower bounds in pixel
*         indices of the slice.  It is only used if CURSOR is false.
*     PNUB = CHARACTER ( READ )
*         ADAM parameter name used for the upper bounds in pixel
*         indices of the slice.  It is only used if CURSOR is false.
*     PNNAME = CHARACTER ( READ )
*         Parameter name of file to store the slice.
*     PNTITL = CHARACTER ( READ )
*         Parameter name of the title of the stored slice.
*     NULL = REAL( READ )
*         Null value used by graphics package
*     PLTITL = CHAR( READ )
*         Title for the plot
*     ABSLAB = CHAR( READ )
*         Label for the x axis
*     ORDLAB = CHAR( READ )
*         Label for the y axis
*     XLOG = LOGICAL( READ )
*         If true the x axis is logarithmic
*     YLOG = LOGICAL( READ )
*         If true the y axis is logarithmic
*     MINTIC( 2 ) = REAL( READ )
*         The number of minor tick marks between each major tick mark
*           for the x and y axes.  A negative value forces the graphics
*           package to compute appropriate values.
*     MAJTIC( 2 ) = REAL( READ )
*         The parameter controlling the numbers of major tick marks
*           for the x and y axes.  (Number used is between MAJTIC+2 and
*           5*MAJTIC/2+4.) A negative value forces the graphics package
*           to compute appropriate values.
*     OUTTIC = LOGICAL( READ )
*         If true the axis tick marks are drawn outside the box
*     THICK = REAL( READ )
*         The line thickness in units of the default
*     X1 = INTEGER ( READ, WRITE )
*         First x co-ordinate that defines a slice (if %CURSOR is false)
*     X2 = INTEGER ( READ, WRITE )
*         Second x co-ordinate that defines a slice (if %CURSOR is
*           false)
*     Y1 = INTEGER ( READ, WRITE )
*         First y co-ordinate that defines a slice (if %CURSOR is false)
*     Y2 = INTEGER ( READ, WRITE )
*         Second y co-ordinate that defines a slice (if %CURSOR is
*           false)
*     STATUS  = INTEGER( READ, WRITE )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Compute maximum number of points in slice
*     If cursor mode then
*        Select overlay frame zone and get bounds
*        Show functions of the choice device's buttons
*        Select overlay plane
*        Repeat until two final positions have been chosen or exit
*          button has been depressed or there has been an error
*           Repeat until the first position has been chosen or there
*             is an error
*              Obtain a valid cursor position, i.e. within array
*                and convert to pixel co-ordinates
*              Reset last cursor position to its current position
*              If exit button has been pressed then
*                 Set repeat switch to off
*              Else if first button has been pressed then
*                 Display next functions of the choice device and
*                   locator on frame zone
*                 Mark the chosen point with a green cross
*                 Switch off first-point repeat
*              Else
*                 Display next functions of the choice device and
*                   locator on frame zone
*                 Mark the provisional point with a white cross
*              Endif
*           End repeat
*           Repeat until the second position has been chosen or there
*             is an error
*              Obtain a valid cursor position, i.e. within
*                array and convert to pixel co-ordinates
*              Reset last cursor position to its current position
*              If exit button has been pressed then
*                 Set repeat switch to off and clear the overlay plane
*              Else if first button has been pressed then
*                 If the second position is different from the
*                   first then
*                    Clear the overlay
*                    Draw a green line joining the two points which
*                      define the slice
*                    Report the co-ordinates of the slice
*                    Switch off second-point and main repeat switches
*                 Else
*                    Report the error and try again
*                 Endif
*              Else
*                 If the second position is different from the first
*                   then
*                    Draw a sample line in white joining the two
*                      points which define the slice
*                 Else
*                    Try again
*                 Endif
*              Endif
*           End repeat
*        End repeat
*     Else
*        Obtain the two ends of slice in pixels from the terminal such
*          that the two positions are different
*        Tidy parameters
*     Endif
*     If there was no exit from obtaining the slice (leave switch) then
*        If the slice is a part or whole of a column or line then just
*          extract the line
*        Else
*           Create and map workspace for computing the slice
*           If no error then
*              Calculate the slice via interpolation
*           Else
*              Report error and return
*           Endif
*        Endif
*        Tidy workspace
*        If graph mode then
*           If input title is the default then
*              Create a title using the region limits within it
*           Else
*              Use supplied title
*           Endif
*           Set Autograph to plot in the graphics zone
*           Create and map workspace for storing the pixel numbers
*           If no error then
*              Store the pixel numbers in the work array
*              If an error occurred report error context, tidy and exit
*           Else
*              Report error context, tidy and exit
*           Endif
*           If ordinate is logarithmic then
*              Create and map a scratch area in which any zero or
*                negative data are set to the graphics package's null
*                value
*              If no error then
*                 Do the substitutions by thresholding
*              Else
*                 Report error context, tidy and exit
*              Endif
*           Endif
*           Create and map workspace to store reflagged pixels
*           If no error then
*              Substitute the graphics package's null value for the
*                bad (magic-value) pixels
*           Else
*              Report error context, tidy and exit
*           Endif
*           Plot the slice on the graphics device
*           If status is not ok annul the error and report the
*              reason (invalid pixels)
*           Tidy the workspace
*        Endif
*        -  Create a primitive NDF.  Map its data array.  Obtain a
*        title for the NDF.  Copy the slice to the NDF.  Write an axis
*        label and centre array in pixels.  Copy the label and units
*        from the input NDF to the output.  Handle a null entry
*        transparently.  Cancel the parameters.
*     Endif
*     End
*
*    Authors :
*
*     Malcolm Currie  STARLINK (RAL::CUR)
*
*    History :
*
*     1988 Apr 11: Original extracted from INSPECT (RAL::CUR)
*     1988 Jun 23: More error reporting and tidying (RAL::CUR).
*     1988 Sep 12: Gave subroutine CROSS extra argument (RAL::CUR).
*     1988 Oct 26: Renamed the DATA_LABEL object in the output IMAGE
*                  structure to be TITLE for consistency (RAL::CUR).
*     1989 Feb  1: Removed SLPLT plot and place slice calculation
*                  outside of the cursor condition (RAL::CUR).
*     1989 Apr 19: Converted to graphics database (new argument list -
*                  ZONEOV for ZONEI, no WKIDOV) (RAL::CUR).
*     1989 Apr 24: Converted to NCAR graphics with many plot style
*                  options --- argument list has been extended; there
*                  are extra preparatory subroutine calls and default
*                  title has slice ends included (RAL::CUR).
*     1989 Jul 13: Control of temporary data via TRN routines to avoid
*                  HDS problem giving an access violation (RAL::CUR).
*     1989 Aug  7: Passed array dimensions as separate variables
*                  (RAL::CUR).
*     1989 Sep 18: Allowed for the special case when the slice is whole
*                  or part of a line or column (RAL::CUR).
*     1989 Oct 13: Adjust for world co-ordinates being the picture
*                  extent rather than pixels (RAL::CUR).
*     1989 Dec 21: Workspace managed by AIF_TEMP (RAL::CUR).
*     1990 Jan 9 : Corrected SGS status (RAL::CUR).
*     1990 Jan 11: Corrected the 1989 Sep 18 fix to access the correct
*                  slice (RAL::CUR).
*     1990 Apr  9: An abort response to the request for an output slice
*                  file is now not annulled (RAL::CUR).
*     1991 Jun 10 : Added LBND argument as temporary patch for INSPECT
*                   to use NDF.  Used lower and upper bounds parameters
*                   instead of 4 x and y parameters (RAL::CUR).
*     1991 Jun 13 : Added NDFI argument so propagate the label and
*                   units to the slice NDF.  Made LINE dynamic via
*                   workspace.  Made the workspace tidying neater
*                   (RAL::CUR).
*     1992 Mar  3: Replaced AIF parameter-system calls by the extended
*                  PAR library (RAL::CUR).
*
*    Type Definitions :

      IMPLICIT NONE

*    Global Constants :

      INCLUDE 'SAE_PAR'        ! Global SSE definitions
      INCLUDE 'DAT_PAR'        ! Data-system constants
      INCLUDE 'PRM_PAR'        ! Magic-value definitions
      INCLUDE 'PAR_ERR'        ! Parameter-system error definitions

*    Status :

      INTEGER STATUS

*    Import :

      INTEGER
     :  NDFI,
     :  LBND( 2 ),
     :  DIM1, DIM2,
     :  ZONEOV,
     :  ZONEG,
     :  ZONEO

      LOGICAL
     :  CURSOR,
     :  GRAPHS,
     :  OUTTIC,
     :  XLOG,
     :  YLOG

      REAL
     :  ARRAY( DIM1, DIM2 ),
     :  MINTIC( 2 ),
     :  MAJTIC( 2 ),
     :  NULL,
     :  THICK,
     :  XCEN, YCEN,
     :  XLOW, YLOW,
     :  XHIGH, YHIGH

      CHARACTER*(*)
     :  ABSLAB, ORDLAB,
     :  PLTITL,
     :  PNLB,
     :  PNUB,
     :  PNNAME,
     :  PNTITL

*    Export :

      INTEGER
     :  X1, Y1,
     :  X2, Y2

*    Local Variables:

      CHARACTER*(DAT__SZLOC)    ! Locator to:
     :  LINLOC,                 ! Work array used to store the slice
     :  SLLOC,                  ! Work arrays used to calculate and plot
                                ! slice
     :  SLLOC1,                 ! Work array used to plot slice
     :  SLLOC3,                 ! Scratch area for storing the data
                                ! values after changing any negative or
                                ! zero pixels when there is a
                                ! logarithmic ordinate
     :  SLLOC4                  ! Scratch area for storing the data
                                ! values after changing any bad pixels

      CHARACTER
     :  DIMSTR * 20,            ! List of start or end pixels of slice
     :  LABEL * 256,            ! Label of the NDF
     :  PTITLE * 56,            ! Plot title (56=16+2*20)
     :  UNITS * 256             ! Units of the NDF

      INTEGER
     :  BUTTN1,                 ! Number of button pressed on the
                                ! trackerball device to get first x,y
     :  BUTTN2,                 ! Number of trackerball button pressed
                                ! to get second x,y co-ordinate pair
     :  FLIMIT( 2 ),            ! First pixel in the slice
     :  IERR,                   ! Position of first error copying the
                                ! slice to the output NDF
     :  LIMITS( 2 ),            ! Limits of column or line slice
     :  LIML( 2 ),              ! Pixel limits below which the slice
                                ! may not terminate
     :  LIMU( 2 ),              ! Pixel limits above which the slice
                                ! may not terminate
     :  LIPNTR,                 ! Pointer to the slice array
     :  LLIMIT( 2 ),            ! Last pixel in the slice
     :  NCHAR,                  ! Running length of plot title
     :  NCHLIM,                 ! No. of characters in a slice limit
     :  NDFO,                   ! NDF identifier for output histogram
     :  NERR,                   ! Number of of conversion errors
                                ! copying the slice to the output NDF
     :  NPTS,                   ! Number of points in slice data
     :  NREP                    ! Number of replacements

      INTEGER
     :  SCPNTR,                 ! pointer to the sub-array of data
                                ! values after changing the value of
                                ! any bad pixels
     :  SLCPTR( 1 ),            ! Pointer to DATA_ARRAY of stored slice
     :  SLEDEF( 2 ),            ! Suggested default slice end bounds
     :  SLEND( 2 ),             ! Slice end bounds
     :  SLPNTR,                 ! Pointer to work arrays
     :  SLPTR1,                 ! Pointer to first work array
     :  SLSDEF( 2 ),            ! Suggested default slice start bounds
     :  SLSTAR( 2 ),            ! Slice start bounds
     :  SNPNTR,                 ! pointer to the sub-array of data
                                ! values after changing the value of
                                ! any zero or negative pixels when there
                                ! is a logaritmic ordinate
     :  SLDIMS( 2 ),            ! Dimensions of work arrays
     :  UBND( 2 )               ! Upper bounds of the input array

      REAL
     :  A, B,                   ! General variables
     :  XCUR1,                  ! First x co-ordinate from cursor
                                ! that defines the slice
     :  XCUR2,                  ! Second x co-ordinate from cursor
                                ! that defines the slice
     :  XF1,                    ! Lower x bound of frame zone
     :  XF2,                    ! Upper x bound of frame zone
     :  XL, YL,                 ! Last valid cursor position
     :  XM,                     ! Frame zone size in x (dummy)
     :  YCUR1,                  ! First y co-ordinate from cursor
                                ! that defines the slice
     :  YCUR2,                  ! Second y co-ordinate from cursor
                                ! that defines the slice
     :  YF1,                    ! Lower y bound of frame zone
     :  YF2,                    ! Upper y bound of frame zone
     :  YM                      ! Frame zone size in y (dummy)

      LOGICAL                   ! true if :
     :  AGAIN, AGAIN1,          ! a simulated REPEAT..UNTIL loop
                                ! is to continue
     :  AGAIN2, REPEAT,         ! a simulated REPEAT..UNTIL loop
                                ! is to continue
     :  BWORK,                  ! Work spaced obtained for re-flagging
                                ! bad pixels
     :  LEAVE,                  ! user does not wish to define any
                                ! further slices
     :  LWORK,                  ! Work spaced obtained for the slice
     :  PWORK,                  ! Work spaced obtained for pixel numbers
     :  THERE,                  ! Label or units defined in input NDF
     :  YLWORK                  ! Work spaced obtained for removing zero
                                ! or negative values for log-ordinate

*-
*    Check the inherited status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise workspace flags.

      LWORK = .FALSE.
      PWORK = .FALSE.
      BWORK = .FALSE.
      YLWORK = .FALSE.

      LEAVE = .FALSE.

*    Find the upper bounds.

      UBND( 1 ) = LBND( 1 ) + DIM1 - 1
      UBND( 2 ) = LBND( 2 ) + DIM2 - 1

*    If an image display is available, then use the cursor to define
*    the two ends of the slice

      IF ( CURSOR ) THEN

         REPEAT = .TRUE.
         AGAIN1 = .TRUE.
         AGAIN2 = .TRUE.

*       Get the bounds of the frame zone.

         CALL SGS_SELZ( ZONEOV, STATUS )
         CALL SGS_IZONE( XF1, XF2, YF1, YF2, XM, YM )

*       Display the choices.

         CALL BTTN3( 'ACCEPT', ' ', 'EXIT', '*', '*', .TRUE., STATUS )

         CALL SGS_SELZ( ZONEO, STATUS )
         XL = XCEN
         YL = YCEN

         DO WHILE ( REPEAT .AND. STATUS .EQ. SAI__OK )

*          Get the first set of co-ordinates.

            DO WHILE ( AGAIN1 .AND. STATUS .EQ. SAI__OK )

               AGAIN = .TRUE.

               DO WHILE ( AGAIN )

                  CALL SGS_SETCU( XL, YL )

*                Read the cursor.

                  CALL SGS_REQCU( XCUR1, YCUR1, BUTTN1 )

*                Convert the `break' on a mouse (the middle button) for
*                image displays to the GKS 7.2 behaviour, so that the
*                interaction can be fully under mouse control.
                  IF ( BUTTN1 .EQ. 0 ) BUTTN1 = 2

*                Make sure that the point lies within the array.

                  IF ( ( XCUR1 .GE. XLOW .AND. XCUR1 .LE. XHIGH )
     :              .AND. ( YCUR1 .GE. YLOW .AND. YCUR1 .LE. YHIGH) )
     :              THEN

*                   Reset the last cursor position.

                     XL = XCUR1
                     YL = YCUR1

*                   Convert the cursor position to find the selected
*                   data pixel.

                     X1 = MIN( UBND( 1 ), MAX( LBND( 1 ),
     :                    INT( XCUR1 ) + 1 ) )
                     Y1 = MIN( UBND( 2 ), MAX( LBND( 2 ),
     :                    INT( YCUR1 ) + 1 ) )
                     AGAIN = .FALSE.

                  ELSE

*                   Negative values are possible under GKS 7.4.

                     IF ( BUTTN1 .GT. 0 ) THEN
                        CALL MSG_OUT( 'INSL_MSG1', 'First point lies '/
     :                                /'outside the array.', STATUS )

*                      Reset the trackerball/mouse.

                        BUTTN1 = 0
                     ELSE

*                      Exit the loop as the exit trigger has been set.

                        AGAIN = .FALSE.
                     END IF
                  END IF
               END DO

*             If the button is BREAK, then exit.

               IF ( BUTTN1 .LE. 0 .OR. BUTTN1 .GT. 3 ) THEN

                  AGAIN1 = .FALSE.
                  AGAIN2 = .FALSE.
                  REPEAT = .FALSE.
                  LEAVE = .TRUE.

*             If the button is CHOICE 1 or 3, deposit a cross at the
*             point.  Allowing choice 3 is a temporary fudge to allow
*             the first button of an X-windows mouse to be used.  It
*             will mean that the right-hand white button on an ARGS
*             will behave like the red one (but is anyone still using
*             an ARGS?)

               ELSE IF ( BUTTN1 .EQ. 1 .OR. BUTTN1 .EQ. 3 ) THEN

                  CALL SGS_SELZ( ZONEOV, STATUS )
                  CALL BTTN3( 'ACCEPT', 'SAMPLE', 'EXIT',
     :                        'X-DIR', 'Y-DIR', .TRUE., STATUS )
                  CALL SGS_SELZ( ZONEO, STATUS )
                  CALL SGS_SPEN ( 3 )
                  CALL KPG1_CROSS( XCUR1, YCUR1, 5., STATUS )
                  AGAIN1 = .FALSE.

*             Otherwise, if any other button is pressed, then put a
*             cross at the point and return for another position
*             until CHOICE 1 or 3 is depressed.

               ELSE

                  CALL SGS_SELZ( ZONEOV, STATUS )
                  CALL BTTN3( 'ACCEPT', 'SAMPLE', 'EXIT',
     :                        'X-DIR', 'Y-DIR', .TRUE., STATUS )
                  CALL SGS_SELZ( ZONEO, STATUS )
                  CALL SGS_SPEN( 1 )
                  CALL KPG1_CROSS( XCUR1, YCUR1, 5., STATUS )

*             End of which-button-pressed check.

               END IF

*          End of loop to get first point.

            END DO

*          Get the second set of co-ordinates.

            A = XCUR1
            B = YCUR1
            XCUR2 = A
            YCUR2 = B
            XL = XCUR1
            YL = YCUR1

            DO WHILE ( AGAIN2 .AND. STATUS .EQ. SAI__OK )

               AGAIN = .TRUE.

               DO WHILE ( AGAIN )

                  CALL SGS_SETCU( XL, YL )

*                Read the cursor.

                  CALL SGS_REQCU( XCUR2, YCUR2, BUTTN2 )

*                Convert the `break' on a mouse (the middle button) for
*                image displays to the GKS 7.2 behaviour, so that the
*                interaction can be fully under mouse control.
                  IF ( BUTTN2 .EQ. 0 ) BUTTN2 = 2

*                Ensure that the point is within the array.

                  IF ( ( XCUR2 .GE. XLOW .AND. XCUR2 .LE. XHIGH )
     :              .AND.( YCUR2 .GE. YLOW .AND. YCUR2 .LE. YHIGH ) )
     :              THEN

*                   Reset the last cursor position.

                     XL = XCUR2
                     YL = YCUR2

*                   Convert the cursor position to find the selected
*                   data pixel.

                     X2 = MIN( UBND( 1 ), MAX( LBND( 1 ),
     :                    INT( XCUR2 ) + 1 ) )
                     Y2 = MIN( UBND( 2 ), MAX( LBND( 2 ),
     :                    INT( YCUR2 ) + 1 ) )
                     AGAIN = .FALSE.

                  ELSE

*                   Negative values are possible under GKS 7.4.

                     IF ( BUTTN2 .GT. 0 ) THEN
                        CALL MSG_OUT( 'INSL_MSG', 'Second point lies '/
     :                                /'outside the array.', STATUS )

*                      Reset the trackerball/mouse.

                        BUTTN2 = 0
                     ELSE

*                      Exit the loop as the exit trigger has been set.

                        AGAIN = .FALSE.
                     END IF
                  END IF
               END DO

*             If the button is BREAK, then exit.

               IF ( BUTTN2 .LE. 0 .OR. BUTTN2 .GT. 3 ) THEN

                  CALL SGS_SELZ( ZONEOV, STATUS )
                  CALL SGS_CLRBL( XF1, XF2, YF1, YF2 )
                  CALL SGS_SELZ( ZONEO, STATUS )
                  AGAIN2 = .FALSE.
                  REPEAT = .FALSE.
                  LEAVE = .TRUE.

*             If the button is CHOICE 1 or 3, then produce the slice.
*             Allowing choice 3 is a temporary fudge to allow the first
*             button of an X-windows mouse to be used.  It will mean
*             that the right-hand white button on an ARGS will behave
*             like the red one (but is anyone still using an ARGS?)

               ELSE IF ( BUTTN2 .EQ. 1 .OR. BUTTN2 .EQ. 3 ) THEN

*                Check whether the points are distinct.

                  IF ( ( XCUR1.NE.XCUR2 ) .OR. ( YCUR1.NE.YCUR2 ) ) THEN

*                   Clear the overlay plane.

                     CALL SGS_SELZ( ZONEOV, STATUS )
                     CALL SGS_CLRBL( XF1, XF2, YF1, YF2 )
                     CALL SGS_SELZ( ZONEO, STATUS )

*                   Draw the line in green.

                     CALL SGS_SPEN( 3 )
                     CALL SGS_LINE( XCUR1, YCUR1, XCUR2, YCUR2 )
                     CALL SGS_FLUSH

*                   Report the pixel indices of the ends of the slice.

                     CALL MSG_SETI( 'INSL_REX1', X1 )
                     CALL MSG_SETI( 'INSL_REY1', Y1 )
                     CALL MSG_SETI( 'INSL_REX2', X2 )
                     CALL MSG_SETI( 'INSL_REY2', Y2 )
                     CALL MSG_OUT( 'INSL_RES', 'The slice is from '/
     :                 /'pixel ( ^INSL_REX1, ^INSL_REY1 ) to '/
     :                 /'( ^INSL_REX2, ^INSL_REY2 ).', STATUS )

                     AGAIN2 = .FALSE.
                     REPEAT = .FALSE.

*                Otherwise, if not distinct then obtain new points.

                  ELSE

                     CALL MSG_OUT( 'INSL_DPR', 'Two distinct'/
     :                              /' points required.', STATUS )

                     AGAIN1 = .TRUE.
                     AGAIN2 = .TRUE.
                  END IF

*             If any other button was pressed, then produce a sample
*             line.

               ELSE IF ( XCUR2 .NE. A .OR. YCUR2 .NE. B ) THEN

                  CALL SGS_CUVIS( .FALSE. )

*                Draw a sample line.

                  CALL SGS_CLRBL( XCUR1, A, YCUR1, B )
                  CALL SGS_FLUSH
                  CALL SGS_SPEN( 1 )
                  CALL SGS_LINE( XCUR1, YCUR1, XCUR2, YCUR2 )

                  A = XCUR2
                  B = YCUR2
                  AGAIN2 = .TRUE.

                  CALL SGS_CUVIS( .TRUE. )
               ELSE

                  A = XCUR2
                  B = YCUR2
                  AGAIN2 = .TRUE.

*             End of which-button-pressed check.

               END IF

*          End of loop to obtain second point.

            END DO

*       End of main loop to obtain both points.

         END DO

*       Return to white.

         CALL SGS_SPEN( 1 )

*    Obtain two (X,Y) pairs.

      ELSE

*       Store the maximum and minimum values permitted for passing
*       to the PAR routine.

         LIML( 1 ) = LBND( 1 )
         LIML( 2 ) = LBND( 2 )
         LIMU( 1 ) = UBND( 1 )
         LIMU( 2 ) = UBND( 2 )

*       Loop to obtain two different pixels in the image.

         REPEAT = .TRUE.
         DO WHILE ( REPEAT .AND. STATUS .EQ. SAI__OK )

*          Set the defaults as the current region.

            SLSDEF( 1 ) = X1
            SLEDEF( 1 ) = X2
            SLSDEF( 2 ) = Y1
            SLEDEF( 2 ) = Y2

*          Obtain the start of the slice.

            CALL PAR_GRM1I( PNLB, 2, SLSDEF, LIML, LIMU, .FALSE.,
     :                      SLSTAR, STATUS )

*          Obtain the end of the slice.

            CALL PAR_GRM1I( PNUB, 2, SLEDEF, LIML, LIMU, .FALSE., SLEND,
     :                      STATUS )

*          Set the defaults as the current region.  Use of X1, X2,
*          etc. will be dispensed in the future.

            X1 = SLSTAR( 1 )
            X2 = SLEND( 1 )
            Y1 = SLSTAR( 2 )
            Y2 = SLEND( 2 )

*          Tidy up so that subroutine may be called again

            CALL PAR_CANCL( PNLB, STATUS )
            CALL PAR_CANCL( PNUB, STATUS )

*          Check for distinctness.

            REPEAT = X1 .EQ. X2 .AND. Y1 .EQ. Y2

*          Inform the user of the mistake.

            CALL ERR_MARK
            IF ( REPEAT ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'INSL_NOTDISTINCT',
     :           'INSL: Two distinct points are required.', STATUS )
               CALL ERR_FLUSH( STATUS )
            END IF
            CALL ERR_RLSE
         END DO

*    End of cursor-required check.

      END IF

*    Do not want to proceed if there was an error or only one end of the
*    slice is defined.

      IF ( .NOT. LEAVE .AND. STATUS .EQ. SAI__OK ) THEN

*       Check for the special cases when a single line or column in the
*       data array has been selected.  Not only should the following be
*       faster, it will also always give the correct answers, whereas
*       linear interpolation might generate some rounding errors.

*       First compute the length of the slice, so that work space may
*       be obtained to store the slice.

         IF ( X2 .EQ. X1 ) THEN
            LIMITS( 1 ) = Y1 - LBND( 2 ) + 1
            LIMITS( 2 ) = Y2 - LBND( 2 ) + 1
            NPTS = MAX( Y1, Y2 ) - MIN( Y1, Y2 ) + 1

         ELSE IF ( Y2 .EQ. Y1 ) THEN
            LIMITS( 1 ) = X1 - LBND( 1 ) + 1
            LIMITS( 2 ) = X2 - LBND( 1 ) + 1
            NPTS = MAX( X1, X2 ) - MIN( X1, X2 ) + 1

         ELSE

*          Compute the length of the slice in pixels.

            NPTS = INT( SQRT( REAL( ( Y2 - Y1 ) * ( Y2 - Y1 ) +
     :              ( X2 - X1 ) * ( X2 - X1 ) ) + 5.0E-3 ) ) + 1

         END IF

*       Create work space for storing the slice.  128 is a fudge
*       to prevent an HDS integrity check.

         CALL AIF_GETVM( '_REAL', 1, MAX( NPTS, 128 ), LIPNTR, LINLOC,
     :                   STATUS )
         LWORK = .TRUE.

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'INSL_WSLI',
     :        'INSL: Unable to get workspace to store the slice.',
     :        STATUS )
            CALL ERR_FLUSH( STATUS )
            GOTO 999
         END IF

         IF ( X2 .EQ. X1 ) THEN

*          Part or whole of a single line is extracted and put into the
*          line work array.

            CALL SLC2T1( 2, DIM1, DIM2, ARRAY, LIMITS,
     :                   X2 - LBND( 1 ) + 1, NPTS, %VAL( LIPNTR ),
     :                   STATUS )

         ELSE IF ( Y2 .EQ. Y1 ) THEN

*          Part or whole of a single column is extracted and put
*          into the line.

            CALL SLC2T1( 1, DIM1, DIM2, ARRAY, LIMITS,
     :                   Y2 - LBND( 2 ) + 1, NPTS, %VAL( LIPNTR ),
     :                   STATUS )

         ELSE

*          Create work space for the co-ordinates of the slice.

            SLDIMS( 1 ) = NPTS
            SLDIMS( 2 ) = 2
            CALL AIF_GETVM( '_REAL', 2, SLDIMS, SLPNTR, SLLOC, STATUS )

            IF ( STATUS .EQ. SAI__OK ) THEN

*             Calculate the slice.

               CALL LINSET( REAL( X1 - LBND( 1 ) + 1 ),
     :                      REAL( Y1 - LBND( 2 ) + 1 ),
     :                      REAL( X2 - LBND( 1 ) + 1 ),
     :                      REAL( Y2 - LBND( 2 ) + 1 ),
     :                      SLDIMS( 1 ), %VAL( SLPNTR ), NPTS, STATUS )
               CALL INPOL( %VAL( SLPNTR ), SLDIMS( 1 ), NPTS, ARRAY,
     :                     DIM1, DIM2, %VAL( LIPNTR ), STATUS )

            ELSE
               CALL ERR_REP( 'INSL_WSLC',
     :           'INSL: Unable to get workspace for calculating '/
     :           /'the slice.', STATUS )
               CALL ERR_FLUSH( STATUS )
               CALL AIF_ANTMP( SLLOC, STATUS )
               GOTO 999
            END IF

*          Tidy the workspace.

            CALL AIF_ANTMP( SLLOC, STATUS )
         END IF

         IF ( GRAPHS .AND. STATUS .EQ. SAI__OK ) THEN

            CALL ERR_MARK
            IF ( PLTITL .EQ. 'Slice plot' ) THEN

*             Define the region limits (arrays needed for subsequent
*             subroutine calls.)

               FLIMIT( 1 ) = X1
               LLIMIT( 1 ) = X2
               FLIMIT( 2 ) = Y1
               LLIMIT( 2 ) = Y2

*             Extend title, first by converting limits of the current
*             region to strings.

               CALL ERR_MARK
               CALL KPG_DIMLS( 2, FLIMIT, NCHLIM, DIMSTR, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  PTITLE = PLTITL( :10 )//': '/
     :                     /DIMSTR( :NCHLIM )//' to '
                  NCHAR = 16 + NCHLIM
                  CALL KPG_DIMLS( 2, LLIMIT, NCHLIM, DIMSTR, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     CALL CHR_APPND( DIMSTR( :NCHLIM ), PTITLE, NCHAR )
                  END IF

*                Ignore any error from KPG_DIMLS.

                  IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
               END IF
               CALL ERR_RLSE
            ELSE

*             Take the title literally.

               PTITLE = PLTITL
            END IF

*          Plot in the graph zone.

            CALL SGS_SELZ( ZONEG, STATUS )

*          Get AUTOGRAPH to use SGS zone

            CALL SNX_AGWV

*          Create work space for pixel numbers

            CALL AIF_GETVM( '_REAL', 1, NPTS, SLPTR1, SLLOC1, STATUS )
            PWORK = .TRUE.

            IF ( STATUS .EQ. SAI__OK ) THEN

*             Create the array of pixel numbers

               CALL KPG1_ELNMR( 1, NPTS, NPTS, %VAL( SLPTR1 ), STATUS )

               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_REP( 'INSL_PIXARR',
     :              'INSL: Error creating the array of pixel numbers.',
     :              STATUS )
                  CALL ERR_FLUSH( STATUS )
                  GO TO 999
               END IF
            ELSE
               CALL ERR_REP( 'INSL_WSLP',
     :           'INSL: Unable to get workspace for plotting slice.',
     :           STATUS )
               CALL ERR_FLUSH( STATUS )
               CALL ERR_RLSE
               GOTO 999
            END IF

            IF ( YLOG ) THEN

*             Create a scratch area in which the zero or negative
*             data are set to the NCAR null value.

               CALL AIF_GETVM( '_REAL', 1, NPTS, SNPNTR, SLLOC3,
     :                         STATUS )
               YLWORK = .TRUE.

               IF ( STATUS .NE. SAI__OK ) THEN

                  CALL ERR_REP( 'INSL_WSP3',
     :              'INSL: Unable to get workspace for changing the '/
     :              /'zero or negative values in the sub-array.',
     :              STATUS )
                  CALL ERR_FLUSH( STATUS )
               CALL ERR_RLSE
                  GOTO 999
               END IF

*             Set zero or negative numbers to the NCAR bad-pixel flag.

               CALL THRSR( %VAL( LIPNTR ), NPTS, 1./VAL__MAXR,
     :                     VAL__MAXR, NULL, VAL__MAXR,
     :                     %VAL( SNPNTR ), STATUS )
            END IF

*          Create a scratch area to store the reflagged bad pixels.

            CALL AIF_GETVM( '_REAL', 1, NPTS, SCPNTR, SLLOC4, STATUS )
            BWORK = .TRUE.

            IF ( STATUS .NE. SAI__OK ) THEN

               CALL ERR_REP( 'INSL_WSP4',
     :           'INSL: Unable to get workspace for changing the '/
     :           /'bad-pixel flagging in the slice.', STATUS )
               CALL ERR_FLUSH( STATUS )
               CALL ERR_RLSE
               GOTO 999
            END IF

*          Substitute the graphics package's null value for any bad
*          pixels.
            IF ( YLOG ) THEN
               CALL KPG1_CHVAR( NPTS, %VAL( SNPNTR ), VAL__BADR, NULL,
     :                          %VAL( SCPNTR ), NREP, STATUS )
            ELSE
               CALL KPG1_CHVAR( NPTS, %VAL( LIPNTR ), VAL__BADR, NULL,
     :                          %VAL( SCPNTR ), NREP, STATUS )
            END IF

*          Plot the slice.

            CALL LINPLT( %VAL(SLPTR1), %VAL(SCPNTR), NPTS, .FALSE.,
     :                   .FALSE., 0.0, 0.0, 0.0, 0.0, PTITLE, ABSLAB,
     :                   ORDLAB, MINTIC, MAJTIC, XLOG, YLOG, OUTTIC,
     :                   THICK, STATUS )

            CALL SGS_FLUSH
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'INSL_IVL',
     :           'INSL: Line is wholly bad.', STATUS )
               CALL ERR_FLUSH( STATUS )
            END IF

*          Release the error context.
            CALL ERR_RLSE

*       End of graphical-output required check.

         END IF

*       Create an output NDF.
*       =====================

*       Start a new error context.

         CALL ERR_MARK

*       Give some commentary.

         CALL MSG_OUT( 'NULLOP', 'Type the null character, !, if '/
     :     /'the slice is not to be saved.', STATUS )

*       Start a new NDF context.

         CALL NDF_BEGIN

*       Create a new NDF.

         CALL LPG_CREP( PNNAME, '_REAL', 1, NPTS, NDFO, STATUS )

*       Map the data array.

         CALL KPG1_MAP( NDFO, 'Data', '_REAL', 'WRITE', SLCPTR, NPTS,
     :                 STATUS )

*       Get the title for the NDF.

         CALL NDF_CINP( PNTITL, NDFO, 'TITLE', STATUS )

*       Write the slice to the NDF.

         CALL VEC_RTOR( .FALSE., NPTS, %VAL( LIPNTR ),
     :                  %VAL( SLCPTR( 1 ) ), IERR, NERR, STATUS )

*       Put a label in the axis structure, which is also created at
*       this point.  There is only one axis.  The axis is unitless.

         CALL NDF_ACPUT( 'Pixels', NDFO, 'Label', 1, STATUS )

*       Obtain the label from the input NDF and propagate it to the
*       slice NDF.

         CALL NDF_STATE( NDFI, 'Label', THERE, STATUS )
         IF ( THERE ) THEN
            CALL NDF_CGET( NDFI, 'Label', LABEL, STATUS )
            CALL NDF_CPUT( LABEL, NDFO, 'Label', STATUS )
         END IF

*       Obtain the units from the input NDF and propagate them to the
*       slice NDF.

         CALL NDF_STATE( NDFI, 'Units', THERE, STATUS )
         IF ( THERE ) THEN
            CALL NDF_CGET( NDFI, 'Units', UNITS, STATUS )
            CALL NDF_CPUT( UNITS, NDFO, 'Units', STATUS )
         END IF

*       Handle the null case invisibly.

         IF ( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*       Close down the NDF system.

         CALL NDF_END( STATUS )

*       Release the new error context.

         CALL ERR_RLSE

*       Cancel the parameters for the INSPECT loop.

         CALL PAR_CANCL( PNNAME, STATUS )
         CALL PAR_CANCL( PNTITL, STATUS )

      ELSE

         LEAVE = .FALSE.

*    End of not-leave check.

      END IF

 999  CONTINUE

*    Tidy any work space.

      IF ( BWORK ) CALL AIF_ANTMP( SLLOC4, STATUS )
      IF ( YLWORK ) CALL AIF_ANTMP( SLLOC3, STATUS )
      IF ( PWORK ) CALL AIF_ANTMP( SLLOC1, STATUS )
      IF ( LWORK ) CALL AIF_ANTMP( LINLOC, STATUS )

      END
