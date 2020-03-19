      SUBROUTINE KPG1_GRPHW( N, X, Y, NSIGMA, YSIGMA, XLAB, YLAB, TTL,
     :                       XSYM, YSYM, MODE, NULL, XL, XR, YB, YT,
     :                       APP, QUIET, LMODE, BSCALE, DX, DY, DBAR,
     :                       IPLOT, STATUS )
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
*                      QUIET, LMODE, BSCALE, DX, DY, DBAR, IPLOT, STATUS )

*  Description:
*     Opens a graphics device and draws a graph displaying a supplied
*     set of points. Each point is defined by an X and Y value, plus an
*     optional error bar. An AST Plot is returned so that the calling
*     application can add further graphics to the plot if needed. When
*     complete, the calling application should annul the Plot, and close
*     the workstation:
*
*       CALL AST_ANNUL( IPLOT, STATUS )
*       CALL KPG_PGCLS( 'DEVICE', .FALSE., STATUS )
*
*     Various environment parameters are used to obtain options, etc.
*     The names of these parameters are hard-wired into this subroutine
*     in order to ensure conformity between applications.

*  Environment Parameters:
*        AXES = _LOGICAL (Read)
*           TRUE if annotated axes are to be produced.
*        CLEAR = _LOGICAL (Read)
*           TRUE if the graphics device is to be cleared on opening.
*        DEVICE = DEVICE (Read)
*           The plotting device.
*        LMODE = LITERAL (Read)
*           If the subroutine argument LMODE is .TRUE., then Parameter
*           LMODE is used to specify how the default values for YBOT and
*           YTOP are found. If argument LMODE is .FALSE. then "Extended"
*           mode is always used. This parameter can take the following
*           values:
*
*           - "Range" -- The lowest and highest supplied data values are
*           used (including error bars).
*
*           - "Extended" -- The lowest and highest supplied data values
*           are used (including error bars), extended to give a margin
*           of 2.5% of the total data range at each end.
*
*           - "Extended,10,5" -- Like "Extended", except the margins at
*           the two ends are specified as a pair of numerical value in
*           the second and third elements of the array. These values are
*           percentages of the total data range. So, "Extended,10,5"
*           includes a margin of 10% of the total data range in YBOT,
*           and 5% in YTOP. If only one numerical value is given, the
*           same value is used for both limits. If no value is given,
*           both limits default to 2.5. "Range" is equivalent to
*           "Extended,0,0".
*
*           - "Percentiles,5,95" -- The second and third elements of the
*           array are interpreted as percentiles. For instance,
*           "Perc,5,95" causes 5% of the data points (ignoring error
*           bars) to be below YBOT, and 10% to be above the YTOP. If
*           only one value (p1) is supplied, the other one, p2, defaults
*           to (100 - p1). If no values are supplied, p1 and p2 default
*           to 5 and 95.
*
*           - "Sigma,2,3" -- The second and third elements of the array
*           are interpreted as multiples of the standard deviation of
*           the data values (ignoring error bars). For instance, "S,2,3"
*           causes the YBOT to be the mean of the data values, minus two
*           sigma, and YTOP to be the mean plus three sigma. If only one
*           value is supplied, the same value is used for both limits.
*           If no values are supplied, both values default to 3.0.
*
*           The above strings can be abbreviated to one character.
*        MARGIN( 4 ) = _REAL (Read)
*           The widths of the margins to leave for axis annotation,
*           given as fractions of the corresponding dimension of the
*           current picture. Four values may be given in the order
*           bottom, right, top, left.  If fewer than four values are
*           given, extra values are used equal to the first supplied
*           value. If these margins are too narrow any axis
*           annotation may be clipped. The dynamic default is 0.15 (for
*           all edges) if either annotated axes or a key are produced,
*           and zero otherwise.
*        MARKER = _INTEGER (Read)
*           The PGPLOT marker type to use. Only accessed if MODE is 3 or
*           5.
*        STYLE = GROUP (Read)
*           A description of the plotting style required. The following
*           synonyms for graphical elements may be used.
*
*           - "Err(Bars)" -- Specifies colour, etc. for error bars.
*           Size(errbars) scales the size of the serifs (i.e. a size
*           value of 1.0 produces a default size).
*
*           - "Sym(bols)" -- Specifies colour, etc. for markers (used
*           in Modes 3 and 5).
*
*           - "Lin(es)" -- Specifies colour, etc. for lines (used in
*           Modes 1, 2, and 5).
*        TEMPSTYLE = GROUP (Read)
*           A description of plotting style required in addition to that
*           define by STYLE.  See STYLE for allowed synonyms.  Unlike
*           STYLE its values are not persistent between invocations.
*        XLEFT = _DOUBLE (Read)
*           The axis value to place at the left-hand end of the
*           horizontal axis. The dynamic default is specified by
*           argument XL. The value supplied may be greater than or less
*           than the value supplied for XRIGHT.
*        XRIGHT = _DOUBLE (Read)
*           The axis value to place at the right-hand end of the
*           horizontal axis. The dynamic default is specified by
*           argument XR. The value supplied may be greater than or less
*           than the value supplied for XLEFT.
*        YBOT = _DOUBLE (Read)
*           The axis value to place at the bottom end of the vertical
*           axis. The dynamic default is specified by argument YB. The
*           value supplied may be greater than or less than the value
*           supplied for YTOP.
*        YTOP = _DOUBLE (Read)
*           The axis value to place at the top end of the vertical axis.
*           The dynamic default is specified by argument YT. The value
*           supplied may be greater than or less than the value supplied
*           for YBOT.

*  Arguments:
*     N = INTEGER (Given)
*        Nuumber of points
*     X( N ) = REAL (Given)
*        X value at each point. These are scaled by the value supplied
*        in BSCALE(1) to generate the axis labels.
*     Y( N ) = REAL (Given)
*        Y value at each point. These are scaled by the value supplied
*        in BSCALE(2) to generate the axis labels.
*     NSIGMA = REAL (Given)
*        Controls the length of the vertical error bars. A value of zero
*        suppresses error bars. Otherwise error bars are drawn that
*        extend from Y - NSIGMA*YSIGMA to Y + NSIGMA*YSIGMA.
*     YSIGMA( N ) = REAL (Given)
*        The standard deviation associated with each Y value. Not
*        accessed if NSIGMA is zero.
*     XLAB = CHARACTER * ( * ) (Given)
*        A default label for the X axis. Only used if the user does not
*        supply an alternative. Trailing spaces are ignored. If a Plot
*        is supplied via IPLOT, then the "Label(1)" attribute in the
*        Plot is used as the default in preference to XLAB.
*     YLAB = CHARACTER * ( * ) (Given)
*        A default label for the Y axis. Only used if the user does not
*        supply an alternative. Trailing spaces are ignored. If a Plot
*        is supplied via IPLOT, then the "Label(2)" attribute in the
*        Plot is used as the default in preference to YLAB.
*     TTL = CHARACTER * ( * ) (Given)
*        A default title for the plot. Only used if the user does not
*        supply an alternative. If a Plot is supplied via IPLOT, then
*        the "Title" attribute in the Plot is used as the default in
*        preference to TTL.
*     XSYM = CHARACTER * ( * ) (Given)
*        The default symbol for the horizontal axis. Only used if the
*        user does not supply an alternative. This will be stored with
*        the Plot in the AGI database and (for instance) used by
*        KAPPA:CURSOR as axis symbols when displaying the cursor
*        positions on the screen. If a Plot is supplied via IPLOT, then
*        the "Symbol(1)" attribute in the Plot is used as the default
*        in preference to XSYM.
*     YSYM = CHARACTER * ( * ) (Given)
*        The default symbol for the horizontal axis. Only used if the
*        user does not supply an alternative. This will be stored with
*        the Plot in the AGI database and (for instance) used by
*        KAPPA:CURSOR as axis symbols when displaying the cursor
*        positions on the screen. If a Plot is supplied via IPLOT, then
*        the "Symbol(2)" attribute in the Plot is used as the default
*        in preference to YSYM.
*     MODE = INTEGER (Given)
*        Determines the way in which the data points are represented.
*           1 -- A "staircase" histogram, in which each horizontal line
*                is centred on the X position. Bad values are flanked by
*                vertical lines drawn down to the lower edge of the viewport.
*           2 -- The points are joined by straight lines.
*           3 -- A marker is placed at each point.
*           4 -- (not used)
*           5 -- A "chain" in which each point is indicated by a marker
*                and also join by straight lines to its neighbouring
*                points.
*           6 -- The same as Mode 1, except that bad values are not
*               flanked by vertical lines drawn down to the lower edge
*               of the viewport (a simple gap is left instead).
*           7 -- The data points are not drawn.
*     NULL = LOGICAL (Given)
*        If .TRUE., then the user may supply a null (!) value for most
*        of the parameters accessed by this routine to indicate that
*        nothing is to be plotted. In this case, no error is returned.
*        Otherwise, a PAR__NULL error status is returned if a null
*        value is supplied.
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
*        The name of the application in the form
*        "<package>_<application>", for instance "KAPPA_NORMALIZE".
*     QUIET = LOGICAL (Given)
*        If .FALSE., a message is displayed indicating the number of
*        points which were plotted. If .TRUE., nothing is displayed on
*        the alpha screen.
*     LMODE = LOGICAL (Given)
*        If .TRUE., then the user is given the chance to specify the
*        default vertical bounds for the plot using Parameter LMODE. If
*        .FALSE., the supplied bounds (YB, YT ) are used, and the
*        equivalent of "Extended" LMODE is used for any bounds that are
*        not supplied.
*     BSCALE( 2 ) = DOUBLE PRECISION (Given)
*        Scale factors which converts the supplied (x,y) values into the
*        values to label on the plot. A similar BZERO argument could be
*        added if there is ever a need.
*     DX( N ) = DOUBLE PRECISION (Given and Returned)
*        Work space.
*     DY( N ) = DOUBLE PRECISION (Given and Returned)
*        Work space.
*     DBAR( N, 2 ) = DOUBLE PRECISION (Given and Returned)
*        Work space. Not accessed if NSIGMA is zero.
*     IPLOT = INTEGER (Given and Returned)
*        On entry, this can either be AST_NULL or a pointer to a
*        two-dimensional Frame. If AST__NULL, the supplied values for
*        the XLAB, YLAB, TTL, XSYM and YSYM arguments are used without
*        change. If a Frame is supplied, the Label, Title, Units, and
*        Symbol attributes of the Frame are used in preference to XLAB,
*        YLAB, TTL, XSYM and YSYM (which are then ignored). Any
*        supplied Frame pointer is annulled before returning, and a
*        pointer to the Plot used to draw the axes is returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - If an error occurs, or if no graphics is produced because the
*     user supplied a null value for a parameter, IPLOT is returned
*     equal to AST__NULL, and PGPLOT is shut down.

*  Copyright:
*     Copyright (C) 1999, 2001 Central Laboratory of the Research
*     Councils.
*     Copyright (C) 2009 - 2011 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: D.S. Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-JUN-1999 (DSB):
*        Original version.
*     17-SEP-1999 (DSB):
*        Modified to shutdown PGPLOT and return IPLOT=AST__NULL if
*        an error occurs. Swapped the order of drawing so that the axes
*        are drawn last (this looks better for instance, if a histogram
*        is drawn in which may bins have value zero and are therefore
*        drawn on the bottom axis).
*     1-OCT-1999 (DSB):
*        Added argument LMODE. Attempt to draw all points, including
*        ones which are outside the plot.
*     26-OCT-1999 (DSB):
*        Made MARGIN a fraction of the current picture, not the DATA
*        picture.
*     1-MAR-2001 (DSB):
*        Retain good axis values if the value on the other axis is bad.
*     15-OCT-2009 (DSB):
*        Allow IPLOT to be used to supply default Frame attribute
*        values.
*     2010 August 15 (MJC):
*        Added TEMPSTYLE.
*     2010 October 11 (MJC):
*        Removed TEMPSTYLE, instead use STYLE for temporary attributes
*        via the + prefix notation.
*     16-JUN-2011 (DSB):
*        Added argument BSCALE.
*     20-JUL-2011 (DSB):
*        Set dynamic default for AXES so that by default axes are only
*        drawn if the existing picture is cleared.
*     20-JUL-2011 (DSB):
*        Ensure the DEVICE parameter is not cancelled if a null value is
*        supplied for it. This is so that multiple LPG invocations of
*        graphics apps such as NORMALIZE can work with "device=!".
*     11-FEB-2016 (DSB):
*        Added mode 7.
*     19-MAR-2020 (DSB):
*        Change screen report to give number of visible plotted points.
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
      LOGICAL LMODE
      DOUBLE PRECISION BSCALE( 2 )

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
      CHARACTER PLMODE*5      ! LMODE parameter name
      DOUBLE PRECISION BOX( 4 ) ! Bounding box for plot
      DOUBLE PRECISION DVAL   ! Parameter value
      INTEGER AXMAPS( 2 )     ! one-dimensional Mappings for each axis
      INTEGER FRAME           ! Current Frame for Plot
      INTEGER I               ! Loop index
      INTEGER IFRM            ! Pointer to defaults Frame
      INTEGER IMARK           ! Marker type to use
      INTEGER IPICD           ! AGI picture id for DATA picture
      INTEGER IPICF           ! AGI picture id for FRAME picture
      INTEGER IPLOT2          ! Plot with requested axis scalings
      INTEGER IVAL            ! Unused integer argument
      INTEGER NGOOD           ! No. of visible points
      INTEGER NMARG           ! No. of supplied margin widths
      LOGICAL AXES            ! Draw axes?
      LOGICAL CLEAR           ! Clear old picture first?
      LOGICAL LVAL            ! Unused logical argument
      LOGICAL VISIBLE         ! Is point visible?
      REAL DEFMAR             ! Default margin value
      REAL MARGIN( 4 )        ! Margins for plot annotation
      REAL XLEFT              ! X at left edge
      REAL XRIGHT             ! X at right edge
      REAL XX                 ! Central X value
      REAL YBOT               ! Y at bottom edge
      REAL YTOP               ! Y at top edge
      REAL YY                 ! Central Y value
*.

*  Check inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Start and AST context.
      CALL AST_BEGIN( STATUS )

*  Find the default values for Parameters XLEFT and XRIGHT. These are
*  the whole range extended by 2.5% at each end.
      XLEFT = XL
      XRIGHT = XR
      CALL KPG1_GRLM1( ' ', N, X, Y, 0.0, 0.0, XLEFT, XRIGHT,
     :                 STATUS )

*  Find the default values for Parameters YBOT and YTOP. Use Parameter
*  LMODE to determine the method to use if argument LMODE is .TRUE.,
*  otherwise a blank parameter name is used which results in "Extended"
*  mode being used.
      IF( LMODE ) THEN
         PLMODE = 'LMODE'
      ELSE
         PLMODE = ' '
      END IF

      YBOT = YB
      YTOP = YT
      CALL KPG1_GRLM1( PLMODE, N, Y, X, NSIGMA, YSIGMA, YBOT, YTOP,
     :                 STATUS )

*  Get new bounds for the plot, using these as dynamic defaults. The user
*  deals wants to see original unscaled data values, so remove the scaling
*  from the default value, and then put it back into the obtaiend value.
      IF( STATUS .EQ. SAI__OK ) THEN
         DVAL = XLEFT*BSCALE( 1 )
         CALL PAR_DEF0D( 'XLEFT', DVAL, STATUS )
         CALL PAR_GET0D( 'XLEFT', DVAL, STATUS )
         XLEFT = DVAL/BSCALE( 1 )
         IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )
      END IF

      IF( STATUS .EQ. SAI__OK ) THEN
         DVAL = XRIGHT*BSCALE( 1 )
         CALL PAR_DEF0D( 'XRIGHT', DVAL, STATUS )
         CALL PAR_GET0D( 'XRIGHT', DVAL, STATUS )
         XRIGHT = DVAL/BSCALE( 1 )
         IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )
      END IF

      IF( STATUS .EQ. SAI__OK ) THEN
         DVAL = YBOT*BSCALE( 2 )
         CALL PAR_DEF0D( 'YBOT', DVAL, STATUS )
         CALL PAR_GET0D( 'YBOT', DVAL, STATUS )
         YBOT = DVAL/BSCALE( 2 )
         IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )
      END IF

      IF( STATUS .EQ. SAI__OK ) THEN
         DVAL = YTOP*BSCALE( 2 )
         CALL PAR_DEF0D( 'YTOP', DVAL, STATUS )
         CALL PAR_GET0D( 'YTOP', DVAL, STATUS )
         YTOP = DVAL/BSCALE( 2 )
         IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )
      END IF

*  Store these as the bounds of the plotting box.
      BOX( 1 ) = DBLE( XLEFT )
      BOX( 2 ) = DBLE( YBOT )
      BOX( 3 ) = DBLE( XRIGHT )
      BOX( 4 ) = DBLE( YTOP )

*  Copy the supplied data to the double precision work arrays.
      NGOOD = 0
      IF( NSIGMA .GT. 0.0 ) THEN

         DO I = 1, N
            XX = X( I )
            YY = Y( I )

            IF( XX .NE. VAL__BADR ) THEN
               DX( I ) = DBLE( XX )
               VISIBLE = ( XX .GE. XLEFT .AND. XX .LE. XRIGHT )
            ELSE
               DX( I ) = AST__BAD
               VISIBLE = .FALSE.
            END IF

            IF( YY .NE. VAL__BADR ) THEN
               DY( I ) = DBLE( YY )
               DBAR( I, 1 ) = DBLE( YY - NSIGMA*YSIGMA( I ) )
               DBAR( I, 2 ) = DBLE( YY + NSIGMA*YSIGMA( I ) )
               VISIBLE = VISIBLE .AND. ( YY .GE. YBOT .AND.
     :                                   YY .LE. YTOP )
            ELSE
               DY( I ) = AST__BAD
               DBAR( I, 1 ) = AST__BAD
               DBAR( I, 2 ) = AST__BAD
               VISIBLE = .FALSE.
            END IF

            IF( VISIBLE ) NGOOD = NGOOD + 1

         END DO

      ELSE

         DO I = 1, N
            XX = X( I )
            YY = Y( I )

            IF( XX .NE. VAL__BADR ) THEN
               DX( I ) = DBLE( XX )
               VISIBLE = ( XX .GE. XLEFT .AND. XX .LE. XRIGHT )
            ELSE
               DX( I ) = AST__BAD
               VISIBLE = .FALSE.
            END IF

            IF( YY .NE. VAL__BADR ) THEN
               DY( I ) = DBLE( YY )
               VISIBLE = VISIBLE .AND. ( YY .GE. YBOT .AND.
     :                                   YY .LE. YTOP )
            ELSE
               DY( I ) = AST__BAD
               VISIBLE = .FALSE.
            END IF

            IF( VISIBLE ) NGOOD = NGOOD + 1

         END DO

      END IF

*  If markers are to be drawn, get the marker type to use.
      IF( MODE .EQ. 3 .OR. MODE .EQ. 5 ) THEN
         CALL PAR_GDR0I( 'MARKER', 2, -31, 10000, .TRUE., IMARK,
     :                   STATUS )
      ELSE
         IMARK = 0
      END IF

*  See if any current picture is to be cleared before plotting the new
*  one.
      CALL PAR_GET0L( 'CLEAR', CLEAR, STATUS )

*  Set the dynamic default for AXES so that by default axes are only
*  drawn if the picture is cleared.
      CALL PAR_DEF0L( 'AXES', CLEAR, STATUS )

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
         DEFMAR = 0.15
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

*  If no Frame was supplied, create one now and set its attributes using
*  the supplied arguments.
      IF( IPLOT .EQ. AST__NULL ) THEN
         FRAME = AST_FRAME( 2, 'DOMAIN=DATAPLOT', STATUS )

*  Set the default value for the axis labels.
         IF( XLAB .NE. ' ' ) CALL AST_SETC( FRAME, 'LABEL(1)',
     :                                      XLAB( : CHR_LEN( XLAB ) ),
     :                                      STATUS )
         IF( YLAB .NE. ' ' ) CALL AST_SETC( FRAME, 'LABEL(2)',
     :                                      YLAB( : CHR_LEN( YLAB ) ),
     :                                      STATUS )

*  Set the default plot title.
         IF( TTL .NE. ' ' ) CALL AST_SETC( FRAME, 'TITLE',
     :                                 TTL( : CHR_LEN( TTL ) ), STATUS )

*  Set the default value for the axis symbols.
         IF( XSYM .NE. ' ' ) CALL AST_SETC( FRAME, 'SYMBOL(1)',
     :                                   XSYM( : CHR_LEN( XSYM ) ),
     :                                   STATUS )
         IF( YSYM .NE. ' ' ) CALL AST_SETC( FRAME, 'SYMBOL(2)',
     :                                   YSYM( : CHR_LEN( YSYM ) ),
     :                                   STATUS )

*  If a Frame was supplied, use it and ensure it has the correct Domain.
*  Retain its other attribute values.
      ELSE
         FRAME = IPLOT
         CALL AST_SET( FRAME, 'DOMAIN=DATAPLOT', STATUS )
      END IF

*  Create a FrameSet containing this single Frame.
      IFRM = AST_FRAMESET( FRAME, ' ', STATUS )

*  Annul the Frame pointer (which may have been supplied by the user via
*  the IPLOT argument).
      CALL AST_ANNUL( FRAME, STATUS )

*  Attempt to open a graphics workstation, obtaining an AST Plot for
*  drawing in a new DATA picture using PGPLOT.
      CALL KPG1_PLOT( IFRM, 'UNKNOWN', APP, ' ', MARGIN, 0, ' ',
     :                ' ', 0.0, 0.0, 'DATAPLOT', BOX, IPICD, IPICF,
     :                IVAL, IPLOT, IVAL, LVAL, STATUS )

*  If a null value was supplied for any graphics parameter, annull any
*  Plot, annull the error, and shut down PGPLOT. Annull the error first
*  so that KPG1_PGCLS does not cancel  the 'DEVICE' parameter.
      IF( STATUS .EQ. PAR__NULL ) THEN
         IF( NULL ) THEN
            CALL AST_ANNUL( IPLOT, STATUS )
            CALL ERR_ANNUL( STATUS )
            CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )
         END IF

*  Otherwise, if the device was opened successfully...
      ELSE IF( STATUS .EQ. SAI__OK ) THEN

*  Get the one-dimensional mappings which transform each of the Current Frame axes
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

*  Draw the points with the STYLE parameter used to obtained the
*  persistent and/or temporary plotting attributes.
         CALL KPG1_PLTLN( N, 1, N, DX, DY, .FALSE., ( NSIGMA .GT. 0.0 ),
     :                    0.0D0, DBAR, 0.0D0, '+STYLE', IPLOT,
     :                    MODE, IMARK, 1, 1, APP, STATUS )

*  Draw the axes if required. We want to include the supplied bscale
*  factors in the labels, so take a copy of the Plot and rescale the
*  current Frame using a MatrixMap that applies the required scalings.
         IF( AXES ) THEN
            IPLOT2 = AST_COPY( IPLOT, STATUS )
            CALL AST_REMAPFRAME( IPLOT2, AST__CURRENT,
     :                           AST_MATRIXMAP( 2, 2, 1, BSCALE, ' ',
     :                                          STATUS ),
     :                           STATUS )
            CALL KPG1_ASGRD( IPLOT2, IPICF, .TRUE., STATUS )
         ENDIF

*  End the PGPLOT buffering context.
         CALL PGEBUF

*  If required, display the number of points plotted.
         IF( .NOT. QUIET ) THEN

            IF( NGOOD .EQ. 0 ) THEN
               CALL MSG_OUT( 'KPG1_GRPHW_MSG1', '  No points plotted.'//
     :                       ' All the data was either bad or outside'//
     :                       ' the range of the axes.', STATUS )
            ELSE IF( NGOOD .EQ. 1 ) THEN
               CALL MSG_OUT( 'KPG1_GRPHW_MSG2', '  One visible point '//
     :                       'plotted.', STATUS )
            ELSE
               CALL MSG_SETI( 'NG', NGOOD )
               CALL MSG_OUT( 'KPG1_GRPHW_MSG3', '  ^NG visible points'//
     :                       ' plotted.', STATUS )
            END IF

         END IF

      END IF

*  Export any Plot pointer from the current AST context in case the
*  calling routine wants to add anything else to the plot. This means
*  the pointer will not be annulled by the following call to AST_END.
      IF( IPLOT .NE. AST__NULL ) CALL AST_EXPORT( IPLOT, STATUS )

*  Tidy up.
 999  CONTINUE

*  If an error has occurred, annul the returned Plot and shutdown the
*  graphics system.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL AST_ANNUL( IPLOT, STATUS )
         CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )
      END IF

*  End AST context.
      CALL AST_END( STATUS )

      END
