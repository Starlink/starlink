      SUBROUTINE KPG1_GRPHW( N, X, Y, NSIGMA, YSIGMA, XLAB, YLAB, TTL,
     :                       XSYM, YSYM, MODE, NULL, XL, XR, YB, YT, 
     :                       APP, QUIET, DX, DY, DBAR, IPLOT, STATUS )
*+
*  Name:
*     KPG1_GRPHW

*  Purpose:
*     Draws a line graph, using supplied work arrays.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_GRPHW( N, X, Y, NSIGMA, YSIGMA, XLAB, YLAB, TTL,
*                      XSYM, YSYM, MODE, NULL, XL, XR, YB, YT, APP, 
*                      QUIET, DX, DY, DBAR, IPLOT, STATUS )

*  Description:
*     Opens a graphics device and draws a graph displaying a supplied 
*     set of points. Each point is defined by an X and Y value, plus an 
*     optional error bar. An AST Plot is returned so that the calling 
*     application can add further graphics to the plot if needed. When 
*     complete, the calling application should annul the Plot, and close 
*     the workstation:
*
*       CALL AST_ANNUL( IPLOT, STATUS )
*       CALL AGP_DEASS( 'DEVICE', .FALSE., STATUS )

*  Environment Parameters:
*     The following envirnment parameter names are used by this routine,
*     to encourage uniformity in parameter naming, and behaviour:
*
*        AXES = _LOGICAL (Read)
*           TRUE if annotated axes are to be produced.
*        CLEAR = _LOGICAL (Read)
*           TRUE if the graphics device is to be cleared on opening. 
*        DEVICE = DEVICE (Read)
*           The plotting device. 
*        MARGIN( 4 ) = _REAL (Read)
*           The widths of the margins to leave for axis annotation, given 
*           as fractions of the corresponding dimension of the DATA picture. 
*           Four values may be given, in the order - bottom, right, top, left. 
*           If less than four values are given, extra values are used equal to 
*           the first supplied value. If these margins are too narrow any axis 
*           annotation may be clipped. The dynamic default is 0.18 (for all 
*           edges) if either annotated axes or a key are produced, and zero 
*           otherwise. 
*        MARKER = INTEGER )Read)
*           The PGPLOT marker type to use. Only accessed if MODE is 3 or 5.
*        STYLE = GROUP (Read)
*           A description of the plotting style required. The following 
*           synonyms for graphical elements may be used: 
*           "Err(Bars)" - Specifies colour, etc for error bars. Size(errbars)
*                         scales the size of the serifs (i.e. a size value of 
*                         1.0 produces a default size).
*           "Sym(bols)" - Specifies colour, etc for markers (used in modes 3
*                         and 5).
*           "Lin(es)"   - Specifies colour, etc for lines (used in modes 1, 2
*                         and 5).
*        XLEFT = _REAL (Read)
*           The axis value to place at the left hand end of the horizontal
*           axis. The dynamic default is specified by argument XL. The value 
*           supplied may be greater than or less than the value supplied for 
*           XRIGHT. 
*        XRIGHT = _REAL (Read)
*           The axis value to place at the right hand end of the horizontal
*           axis. The dynamic default is specified by argument XR. The value 
*           supplied may be greater than or less than the value supplied for 
*           XLEFT. 
*        YBOT = _REAL (Read)
*           The axis value to place at the bottom end of the vertical 
*           axis. The dynamic default is specified by argument YB. The value 
*           supplied may be greater than or less than the value supplied for 
*           YTOP. 
*        YTOP = _REAL (Read)
*           The axis value to place at the top end of the vertical axis. 
*           The dynamic default is specified by argument YT. The value 
*           supplied may be greater than or less than the value supplied 
*           for YBOT. 

*  Arguments:
*     N = INTEGER (Given)
*        No. of points
*     X( N ) = REAL (Given)
*        X value at each point.
*     Y( N ) = REAL (Given)
*        Y value at each point.
*     NSIGMA = REAL (Given)
*        Controls the length of the vertical error bars. A value of zero
*        suppresses error bars. Otherwise error bars are drawn which extend 
*        by from Y - NSIGMA*YSIGMA to Y + NSIGMA*YSIGMA.
*     YSIGMA( N ) = REAL (Given)
*        The standard deviation associated with each Y value. Not
*        accessed if NSIGMA is zero.
*     XLAB = CHARACTER * ( * ) (Given)
*        A default label for the X axis. Only used if the user does not
*        supply an alternative. Trailing spaces are ignored.
*     YLAB = CHARACTER * ( * ) (Given)
*        A default label for the Y axis. Only used if the user does not
*        supply an alternative. Trailing spaces are ignored.
*     TTL = CHARACTER * ( * ) (Given)
*        A default title for the plot. Only used if the user does not
*        supply an alternative.
*     XSYM = CHARACTER * ( * ) (Given)
*        The default symbol for the horizontal axis. Only used if the user 
*        does not supply an alternative. This will be stored with the Plot
*        in the AGI database and (for instance) used by CURSOR as axis 
*        symbols when displaying the curosir positions on the screen.
*     YSYM = CHARACTER * ( * ) (Given)
*        The default symbol for the horizontal axis. Only used if the user 
*        does not supply an alternative. This will be stored with the Plot
*        in the AGI database and (for instance) used by CURSOR as axis 
*        symbols when displaying the curosir positions on the screen.
*     MODE = INTEGER (Given)
*        Determines the way in which the data points are represented:
*            1 - A "staircase" histogram, in which each horizontal line is
*                centred on the X position.
*            2 - The points are joined by straight lines.
*            3 - A marker is placed at each point.
*            4 - (not used)
*            5 - A "chain" in which each point is marker by a marker and also 
*                join by straight lines to its neighbouring points.
*     NULL = LOGICAL (Given)
*        If .TRUE., then the user may supply a null (!) value for most of the
*        parameters accessed by this routine to indicate that nothing is to
*        be plotted. In this case, no error is returned. Otherwise, a
*        PAR__NULL error status is returned if a null value is supplied.
*     XL = REAL (Given)
*        The default value for the XLEFT parameter. If VAL__BADR is
*        supplied, the minimum of the X values is used (plus a small
*        margin).
*     XR = REAL (Given)
*        The default value for the XRIGHT parameter. If VAL__BADR is
*        supplied, the maximum of the X values is used (plus a small
*        margin).
*     YB = REAL (Given)
*        The default value for the YBOT parameter. If VAL__BADR is
*        supplied, the minimum of the low end of the Y error bars is 
*        used (plus a small margin).
*     YT = REAL (Given)
*        The default value for the YTOP parameter. If VAL__BADR is
*        supplied, the maximum of the high end of the Y error bars is 
*        used (plus a small margin).
*     APP = CHARACTER * ( * ) (Given)
*        The name of the application in the form "<package>_<application>".
*        E.g. "KAPPA_NORMALIZE".
*     QUIET = LOGICAL (Given)
*        If .FALSE., a message is displayed indicating the number of
*        points which were plotted. If .TRUE., nothing is displayed on
*        the alpha screen.
*     DX( N ) = DOUBLE PRECISION (Given and Returned)
*        Work space.
*     DY( N ) = DOUBLE PRECISION (Given and Returned)
*        Work space.
*     DBAR( N, 2 ) = DOUBLE PRECISION (Given and Returned)
*        Work space. Not accessed if NSIGMA is zero.
*     IPLOT = INTEGER (Returned)
*        The AST Plot used to do the drawing.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - If an error occurs, or if no graphics is produced because the
*     user supplied a null value for a parameter, IPLOT is returned equal
*     to AST__NULL, and PGPLOT is shut down.

*  Authors:
*     DSB: D.S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-JUN-1999 (DSB):
*        Original version.
*     17-SEP-1999 (DSB):
*        Modified to shutdown PGPLOT and return IPLOT=AST__NULL if
*        an error occurs. Swapped the order of drawing so that the axes
*        are drawn last (this looks better for instance, if a histogram is
*        drawn in which may bins have value zero and are therefore drawn 
*        on the bottom axis).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'PAR_ERR'          ! PAR error constants 
      INCLUDE 'AST_PAR'          ! AST constants and functions

*  Arguments Given:
      INTEGER N
      REAL X( N )
      REAL Y( N )
      REAL NSIGMA
      REAL YSIGMA( N )
      CHARACTER XLAB*(*)
      CHARACTER YLAB*(*)
      CHARACTER TTL*(*)
      CHARACTER XSYM*(*)
      CHARACTER YSYM*(*)
      INTEGER MODE
      LOGICAL NULL
      REAL XL
      REAL XR
      REAL YB
      REAL YT
      CHARACTER APP*(*)
      LOGICAL QUIET

*  Arguments Given and Returned:
      DOUBLE PRECISION DX( N )
      DOUBLE PRECISION DY( N )
      DOUBLE PRECISION DBAR( N, 2 )

*  Arguments Returned:
      INTEGER IPLOT

*  Status:
      INTEGER STATUS          ! Global status

*  External References:
      INTEGER CHR_LEN         ! Used length of a string.

*  Local Variables:
      DOUBLE PRECISION BOX(4) ! Bounding box for plot
      INTEGER AXMAPS( 2 )     ! 1-D Mappings for each axis
      INTEGER I               ! Loop index
      INTEGER IFRM            ! Pointer to defaults Frame 
      INTEGER IMARK           ! Marker type to use
      INTEGER IPICD           ! AGI picture id for DATA picture      
      INTEGER IPICF           ! AGI picture id for FRAME picture      
      INTEGER IVAL            ! Unused integer argument
      INTEGER NGOOD           ! No. of points to plot
      INTEGER NMARG           ! No. of supplied margin widths
      LOGICAL AXES            ! Draw axes?
      LOGICAL LVAL            ! Unused logical argument
      REAL DEFMAR             ! Default margin value
      REAL DELTA              ! Axis range
      REAL HI                 ! Y at high end of error bar
      REAL LO                 ! Y at low end of error bar
      REAL MARGIN( 4 )        ! Margins for plot annotation
      REAL XLEFT              ! X at left edge
      REAL XMAX               ! Maximum X in plot
      REAL XMIN               ! Minimum X in plot
      REAL XRIGHT             ! X at right edge
      REAL XX                 ! Central X value
      REAL YBOT               ! Y at bottom edge
      REAL YMAX               ! Maximum Y in plot
      REAL YMIN               ! Minimum Y in plot
      REAL YTOP               ! Y at top edge
      REAL YY                 ! Central Y value
*.

*  Check inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Start and AST context.
      CALL AST_BEGIN( STATUS )

*  If any of the plot limits have not been supplied, find the bounds of the 
*  supplied data (including error bars).
      IF( XL .EQ. VAL__BADR .OR. XR .EQ. VAL__BADR .OR. 
     :    YT .EQ. VAL__BADR .OR. YB .EQ. VAL__BADR ) THEN

         XLEFT = VAL__MAXR
         YBOT = VAL__MAXR
         XRIGHT = VAL__MINR
         YTOP = VAL__MINR

         IF( NSIGMA .GT. 0.0 ) THEN

            DO I = 1, N
               IF( X( I ) .NE. VAL__BADR .AND. Y( I ) .NE. VAL__BADR 
     :             .AND. YSIGMA( I ) .NE. VAL__BADR ) THEN
                  XLEFT = MIN( XLEFT, X( I ) )
                  YBOT = MIN( YBOT, Y( I ) - NSIGMA*YSIGMA( I ) )
                  XRIGHT = MAX( XRIGHT, X( I ) )
                  YTOP = MAX( YTOP, Y( I ) + NSIGMA*YSIGMA( I ) )
               END IF
            END DO

         ELSE

            DO I = 1, N
               IF( X( I ) .NE. VAL__BADR .AND. 
     :             Y( I ) .NE. VAL__BADR ) THEN
                  XLEFT = MIN( XLEFT, X( I ) )
                  YBOT = MIN( YBOT, Y( I ) )
                  XRIGHT = MAX( XRIGHT, X( I ) )
                  YTOP = MAX( YTOP, Y( I ) )
               END IF
            END DO

         END IF

*  Add 5% onto both end end of each axis.
         DELTA = 0.05*( XRIGHT - XLEFT )
         XLEFT = XLEFT - DELTA
         XRIGHT = XRIGHT + DELTA

         DELTA = 0.05*( YTOP - YBOT )
         YBOT = YBOT - DELTA
         YTOP = YTOP + DELTA

      END IF

*  Use any supplied limits in preference to the data limits found above.
      IF( XL .NE. VAL__BADR ) XLEFT = XL
      IF( XR .NE. VAL__BADR ) XRIGHT = XR
      IF( YB .NE. VAL__BADR ) YBOT = YB
      IF( YT .NE. VAL__BADR ) YTOP = YT

*  Get new bounds for the plot, using these as dynamic defaults.
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL PAR_DEF0R( 'XLEFT', XLEFT, STATUS )
         CALL PAR_GET0R( 'XLEFT', XLEFT, STATUS )
         IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )
      END IF            

      IF( STATUS .EQ. SAI__OK ) THEN
         CALL PAR_DEF0R( 'XRIGHT', XRIGHT, STATUS )
         CALL PAR_GET0R( 'XRIGHT', XRIGHT, STATUS )
         IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )
      END IF            

      IF( STATUS .EQ. SAI__OK ) THEN
         CALL PAR_DEF0R( 'YBOT', YBOT, STATUS )
         CALL PAR_GET0R( 'YBOT', YBOT, STATUS )
         IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )
      END IF            

      IF( STATUS .EQ. SAI__OK ) THEN
         CALL PAR_DEF0R( 'YTOP', YTOP, STATUS )
         CALL PAR_GET0R( 'YTOP', YTOP, STATUS )
         IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )
      END IF            

*  Store these as the bounds of the plotting box.
      BOX( 1 ) = DBLE( XLEFT )
      BOX( 2 ) = DBLE( YBOT )
      BOX( 3 ) = DBLE( XRIGHT )
      BOX( 4 ) = DBLE( YTOP )

*  Find the minimum and maximum data values to be displayed.
      XMIN = MIN( XLEFT, XRIGHT )
      XMAX = MAX( XLEFT, XRIGHT )
      YMIN = MIN( YTOP, YBOT )
      YMAX = MAX( YTOP, YBOT )

*  Copy the supplied data to the double precision work arrays, setting
*  any data outside the above limits bad.
      NGOOD = 0
      IF( NSIGMA .GT. 0.0 ) THEN

         DO I = 1, N
            XX = X( I )
            YY = Y( I )

            IF( XX .NE. VAL__BADR .AND. YY .NE. VAL__BADR ) THEN
               IF( XX .GE. XMIN .AND. XX .LE. XMAX .AND.
     :             YY .GE. YMIN .AND. YY .LE. YMAX ) THEN
                  DX( I ) = DBLE( XX )
                  DY( I ) = DBLE( YY )
                  DBAR( I, 1 ) = DBLE( YY - NSIGMA*YSIGMA( I ) )
                  DBAR( I, 2 ) = DBLE( YY + NSIGMA*YSIGMA( I ) )
                  NGOOD = NGOOD + 1
               ELSE
                  DX( I ) = AST__BAD
                  DY( I ) = AST__BAD
                  DBAR( I, 1 ) = AST__BAD
                  DBAR( I, 2 ) = AST__BAD
               END IF
            ELSE
               DX( I ) = AST__BAD
               DY( I ) = AST__BAD
               DBAR( I, 1 ) = AST__BAD
               DBAR( I, 2 ) = AST__BAD
            END IF

         END DO

      ELSE

         DO I = 1, N
            XX = X( I )
            YY = Y( I )

            IF( XX .NE. VAL__BADR .AND. YY .NE. VAL__BADR ) THEN
               IF( XX .GE. XMIN .AND. XX .LE. XMAX .AND.
     :             YY .GE. YMIN .AND. YY .LE. YMAX ) THEN
                  DX( I ) = DBLE( XX )
                  DY( I ) = DBLE( YY )
                  NGOOD = NGOOD + 1
               ELSE
                  DX( I ) = AST__BAD
                  DY( I ) = AST__BAD
               END IF
            ELSE
               DX( I ) = AST__BAD
               DY( I ) = AST__BAD
            END IF

         END DO

      END IF

*  If markers are to be drawn, get the marker type to use.
      IF( MODE .EQ. 3 .OR. MODE .EQ. 5 ) THEN
         CALL PAR_GDR0I( 'MARKER', 2, -31, 10000, .TRUE., IMARK, 
     :                   STATUS )
      ELSE
         IMARK = 0
      END IF

*  See if annotated axes are required. 
      CALL PAR_GET0L( 'AXES', AXES, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK) GO TO 999

*  Get the margin values, using a dynamic default of zero if no axes are 
*  being created (to avoid the unnecessary creation of FRAME pictures by 
*  KPG1_PLOT).
      IF( .NOT. AXES ) THEN
         DEFMAR = 0.0
      ELSE
         DEFMAR = 0.18
      END IF
      CALL PAR_DEF1R( 'MARGIN', 1, DEFMAR, STATUS )

      CALL PAR_GDRVR( 'MARGIN', 4, -0.49, 10.0, MARGIN, NMARG, STATUS )
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         MARGIN( 1 ) = DEFMAR
         NMARG = 1
      ELSE
         NMARG = MIN( 4, NMARG )
      END IF

*  Use the first MARGIN value for any unspecified edges.
      IF( STATUS .EQ. SAI__OK ) THEN 
         DO I = NMARG + 1, 4      
            MARGIN( I ) = MARGIN( 1 )
         END DO
      END IF


*  Create a FrameSet containing a single Frame in which the default values 
*  for labels, symbols, title, etc can be set.
      IFRM = AST_FRAMESET( AST_FRAME( 2, 'DOMAIN=DATAPLOT', STATUS ), 
     :                     ' ', STATUS )

*  Set the default value for the axis labels.
      CALL AST_SETC( IFRM, 'LABEL(1)', XLAB( : CHR_LEN( XLAB ) ), 
     :               STATUS )
      CALL AST_SETC( IFRM, 'LABEL(2)', YLAB( : CHR_LEN( YLAB ) ), 
     :               STATUS )

*  Set the default plot title.
      CALL AST_SETC( IFRM, 'TITLE', TTL, STATUS )

*  Set the default value for the axis symbols.
      CALL AST_SETC( IFRM, 'SYMBOL(1)', XSYM, STATUS )
      CALL AST_SETC( IFRM, 'SYMBOL(2)', YSYM, STATUS )

*  Atempt to open a graphics workstation, obtaining an AST Plot for 
*  drawing in a new DATA picture using PGPLOT.
      CALL KPG1_PLOT( IFRM, 'UNKNOWN', APP, ' ', MARGIN, 0, ' ', 
     :                ' ', 0.0, 0.0, 'DATAPLOT', BOX, IPICD, IPICF, 
     :                IVAL, IPLOT, IVAL, LVAL, STATUS )

*  If a null value was supplied for any graphics parameter, annul the
*  Plot, shut down PGPLOT and annull the error if allowed, and do not plot 
*  anything.
      IF( STATUS .EQ. PAR__NULL ) THEN
         IF( NULL ) THEN
            CALL AST_ANNUL( IPLOT, STATUS )
            CALL AGP_DEASS( 'DEVICE', .FALSE., STATUS )
            CALL ERR_ANNUL( STATUS )
         END IF

*  Otherwise, if the device was opened succesfully...
      ELSE IF( STATUS .EQ. SAI__OK ) THEN

*  Get the 1-D mappings which transform each of the Current Frame axes
*  onto the corresponding PGPLOT world co-ordinate axis.
         CALL KPG1_ASSPL( IPLOT, 2, AXMAPS, STATUS )

*  Map all the required axis values from Current Frame into PGPLOT world
*  co-ords.
         CALL AST_TRAN1( AXMAPS( 1 ), N, DX, .FALSE., DX, STATUS )
         CALL AST_TRAN1( AXMAPS( 2 ), N, DY, .FALSE., DY, STATUS )
         IF( NSIGMA .GT. 0.0 ) THEN
            CALL AST_TRAN1( AXMAPS( 2 ), 2*N, DBAR, .FALSE., DBAR, 
     :                      STATUS )
         END IF

*  Start a PGPLOT buffering context.
         CALL PGBBUF

*  Draw the points.
         CALL KPG1_PLTLN( N, 1, N, DX, DY, .FALSE., ( NSIGMA .GT. 0.0 ),
     :                    0.0D0, DBAR, 0.0D0, 'STYLE', IPLOT, MODE, 
     :                    IMARK, 1, 1, APP, STATUS )

*  Draw the axes if required.
         IF( AXES ) CALL KPG1_ASGRD( IPLOT, IPICF, .TRUE., STATUS )

*  End the PGPLOT buffering context.
         CALL PGEBUF

*  If required, display the number of points plotted.
         IF( .NOT. QUIET ) THEN

            IF( NGOOD .EQ. 0 ) THEN
               CALL MSG_OUT( 'KPG1_GRPHW_MSG1', '  No points plotted.'//
     :                       ' All the data was either bad or outside'//
     :                       ' the range of the axes.', STATUS )
            ELSE IF( NGOOD .EQ. 1 ) THEN
               CALL MSG_OUT( 'KPG1_GRPHW_MSG2', '  One point plotted.',
     :                       STATUS )
            ELSE 
               CALL MSG_SETI( 'NG', NGOOD )
               CALL MSG_OUT( 'KPG1_GRPHW_MSG3', '  ^NG points plotted.',
     :                       STATUS )
            END IF

         END IF

      END IF

*  Export the Plot pointer from the current AST context in case the calling 
*  routine wants to add anything else to the plot. This means the pointer
*  will not be annulled by the following call to AST_END.
      CALL AST_EXPORT( IPLOT, STATUS )

*  Tidy up.
 999  CONTINUE

*  If an error has occurred, annul the returned Plot and shutdown the
*  graphics system.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL AST_ANNUL( IPLOT, STATUS )
         CALL AGP_DEASS( 'DEVICE', .FALSE., STATUS )
      END IF

*  End AST context.
      CALL AST_END( STATUS )

      END
