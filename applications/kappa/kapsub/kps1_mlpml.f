      SUBROUTINE KPS1_MLPML( NDISP, USE, N, ILO, IHI, X, Y, XERROR,
     :                       YERROR, XBAR, YBAR, XSTEP, PARAM1, PARAM2,
     :                       IPLOT, MODE, MTYPE, ERSHAP, FREQ, APP,
     :                       STATUS )
*+
*  Name:
*     KPS1_MLPML

*  Purpose:
*     Draws a set of data curves.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MLPML( NDISP, USE, N, ILO, IHI, X, Y, XERROR, YERROR,
*                      XBAR, YBAR, XSTEP, PARAM1, PARAM2, IPLOT, MODE,
*                      MTYPE, ERSHAP, FREQ, APP, STATUS )

*  Description:
*     This routine draws a set of data curves (e.g. a data value and a
*     position, or two data values).  Errors in both data values may be
*     represented by error bars.  No annotated asxes are drawn.  The
*     calling routine should do this if required by passing the supplied
*     Plot (IPLOT) to routine KPG1_ASGRD.
*
*     PGPLOT should be active, and the viewport should correspond to
*     the DATA picture in which the plot is to be drawn.  PGPLOT world
*     co-ordinates within the viewport should be GRAPHICS co-ordinates
*     (millimetres from the bottom-left corner of the view surface).
*
*     The Plotting style is accessed using the environment parameter
*     specified by PARAM1, and may include the following synonyms for
*     graphical elements.
*        "Err(Bars)": Specifies colour, etc for error bars.
*                     Size(errbars) scales the size of the serifs used
*                     if ERSHAP=1 (i.e. a size value of 1.0 produces a
*                     default size).
*        "Sym(bols)": Specifies colour, etc. for markers (used in modes
*                     3 and 5).
*        "Lin(es)"  : Specifies colour, etc. for lines (used in modes
*                     1, 2, 4 and 5).

*  Arguments:
*     NDISP = INTEGER (Given)
*        Number of curves to be plotted.
*     USE( NDISP ) = LOGICAL (Given)
*        Set .FALSE. if the corresponding curve should not be drawn.
*     N = INTEGER (Given)
*        Number of points to be plotted.
*     ILO = INTEGER (Given)
*        The index of the first grid point to be used.
*     IHI = INTEGER (Given)
*        The index of the last grid point to be used.
*     X( N, NDISP ) = DOUBLE PRECISION (Given)
*        The X value at each point, in PGPLOT world co-ordinate (i.e.
*        millimetres from the bottom-left corner of the view surface).
*     Y( N, NDISP ) = DOUBLE PRECISION (Given)
*        The Y value at each point, in PGPLOT world co-ordinate (i.e.
*        millimetres from the bottom-left corner of the view surface).
*     XERROR = LOGICAL (Given)
*        Display X error bars?
*     YERROR = LOGICAL (Given)
*        Display Y error bars?
*     XBAR( N, 2, NDISP ) = DOUBLE PRECISION (Given)
*        Row 1 contains the lower limit and row 2 contains the upper
*        limit for each horizontal error bar, in PGPLOT world
*        co-ordinate (i.e. millimetres from the bottom-left corner of
*        the view surface).  Only  accessed if XERROR is .TRUE.
*     YBAR( N, 2, NDISP ) = DOUBLE PRECISION (Given)
*        Row 1 contains the lower limit and row 2 contains the upper
*        limit for each vertical error bar, in PGPLOT world co-ordinate
*        (i.e. millimetres from the bottom-left corner of the view
*        surface).  Only accessed if YERROR is .TRUE.
*     XSTEP( N, 2, NDISP ) = DOUBLE PRECISION (Given)
*        Row 1 contains the lower limit and row 2 contains the upper
*        limit for each horizontal step, in PGPLOT world co-ordinate
*        (i.e. millimetres from the bottom-left corner of the view
*        surface).  Only accessed if MODE is 4.
*     PARAM1 = CHARACTER * ( * ) (Given)
*        The name of the style parameter to be used when obtaining the
*        plotting style.
*     PARAM2 = CHARACTER * ( * ) (Given)
*        The name of the parameter to be used when obtaining the
*        individual pen defintions for each curve.
*     IPLOT = INTEGER (Given)
*        An AST Plot which can be used to do the drawing.  The Base
*        Frame should be GRAPHICS co-ordinates (millimetres from the
*        bottom-left corner of the PGPLOT view surface).  The Current
*        Frame should be the Frame in which annotation is required.
*     MODE = INTEGER (Given)
*        Determines the way in which the data points are represented.
*           1 - A "staircase" histogram, in which each horizontal line
*               is centred on the X position.
*           2 - The points are joined by straight lines.
*           3 - A marker is placed at each point (see MTYPE).
*           4 - Mark each point with a horizontal line of width given by
*               XW.
*           5 - A "chain" in which each point is marker by a marker and
*               also join by straight lines to its neighbouring points.
*     MTYPE = INTEGER (Given)
*        The PGPLOT marker type to use if MODE is 3 or 5.
*     ERSHAP = INTEGER (Given)
*        Determines the way in which error bars are drawn.
*           1 - X and Y errors are represented by horizontal and
*               vertical lines respectively.  Serifs are drawn at the
*               ends of each line.  The size of these serifs is
*               controlled by the Size(Errbar) plotting attribute.
*           2 - A cross is drawn joining the corners of the box
*               encompassing the X and Y errors.
*           3 - A Diamond is drawn joining the ends of the horizontal
*               and vertical error bars which would have been drawn if
*               ERSHAP had been 1.
*
*        These will all produce the same result (i.e. a single straight
*        line) if errors are available only on one axis (see XERROR and
*        YERROR).  Not accessed if XERROR and YERROR are both .FALSE.
*     FREQ = INTEGER (Given)
*        The frequency at which errors are to be plotted.  A value of 1
*        means "plot errors for every point", 2 means "plot errors for
*        every second point", etc.  Not accessed if XERROR and YERROR
*        are  both .FALSE.
*     APP = CHARACTER * ( * ) (Given)
*        The name of the calling application in the form
*        <package>_<application> (e.g. "KAPPA_DISPLAY").
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-AUG-1999 (DSB):
*        Original version.
*     2006 April 12 (MJC):
*        Remove unused variable and wrapped long lines.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function
                                 ! declarations
      INCLUDE 'GRP_PAR'          ! GRP constants

*  Arguments Given:
      INTEGER NDISP
      LOGICAL USE( NDISP )
      INTEGER N
      INTEGER ILO
      INTEGER IHI
      DOUBLE PRECISION X( N, NDISP )
      DOUBLE PRECISION Y( N, NDISP )
      LOGICAL XERROR
      LOGICAL YERROR
      DOUBLE PRECISION XBAR( N, 2, NDISP )
      DOUBLE PRECISION YBAR( N, 2, NDISP )
      DOUBLE PRECISION XSTEP( N, 2, NDISP )
      CHARACTER PARAM1*(*)
      CHARACTER PARAM2*(*)
      INTEGER IPLOT
      INTEGER MODE
      INTEGER MTYPE
      INTEGER ERSHAP
      INTEGER FREQ
      CHARACTER APP*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NAT                ! No. of AST attributes used
      PARAMETER ( NAT = 11 )

*  Local Variables:
      CHARACTER ATTNAM( NAT )*20 ! Names of attributes used by this
                                 ! routine
      CHARACTER PENDEF*( GRP__SZNAM ) ! AST attribute settings for
                                 ! current pen
      DOUBLE PRECISION ATTR( 20 )! Saved graphics attribute values
      INTEGER I                  ! Position index
      INTEGER IGRP               ! GRP identifier for pen definitions
      INTEGER IPEN               ! Index of current pen definition
      INTEGER J1                 ! Index at start of attribute setting
      INTEGER J2                 ! Index of comma at end of attribute
                                 ! setting
      INTEGER K                  ! Line index
      INTEGER NPEN               ! Number of pen defintions supplied in
                                 ! IGRP
      LOGICAL BADAT              ! Was attribute setting string invalid?
      LOGICAL DOWN               ! Is the pen down on the paper?
      LOGICAL DRAWC              ! Can line C be drawn?
      LOGICAL GOODX              ! Is current X value good?
      LOGICAL GOODX0             ! Was previous X value good?
      LOGICAL GOODY              ! Is current Y value good?
      LOGICAL GOODY0             ! Was previous Y value good?
      LOGICAL MIDX               ! Is middle X value good?
      REAL ATT0( NAT )           ! Original Plot attributes on entry
      REAL ATT1( NAT )           ! Plot attributes supplied by PARAM1
      REAL ERR                   ! Error bar limit value
      REAL RVAL                  ! General REAL variable
      REAL RX                    ! Single precision central X position
      REAL RX0                   ! Previous single-precision central X
                                 ! position
      REAL RXC                   ! X half way from previous to current
                                 ! position
      REAL RY                    ! Single precision central Y position
      REAL RY0                   ! Previous single-precision central Y
                                 ! position
      REAL SERIF                 ! Length of serif bar
      REAL WX1                   ! Lower X limit of PGPLOT window
      REAL WX2                   ! Higher X limit of PGPLOT window
      REAL WY1                   ! Lower Y limit of PGPLOT window
      REAL WY2                   ! Higher Y limit of PGPLOT window
      REAL XHI                   ! Upper X limit of error box
      REAL XLO                   ! Lower X limit of error box
      REAL YHI                   ! Upper Y limit of error box
      REAL YLO                   ! Lower Y limit of error box


      DATA ATTNAM /'COLOUR(AXES)',    'WIDTH(AXES)',    'STYLE(AXES)',
     :             'COLOUR(MARKERS)', 'WIDTH(MARKERS)', 'FONT(MARKERS)',
     :             'STYLE(MARKERS)',  'COLOUR(CURVES)', 'WIDTH(CURVES)',
     :             'STYLE(CURVES)',   'SIZE(AXES)' /

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the bounds of the current PGPLOT window.
      CALL PGQWIN( WX1, WX2, WY1, WY2 )

*  Ensure WX1 and WY1 are the minima.
      IF( WX1 .GT. WX2 ) THEN
         RVAL = WX1
         WX1 = WX2
         WX2 = RVAL
      END IF

      IF( WY1 .GT. WY2 ) THEN
         RVAL = WY1
         WY1 = WY2
         WY2 = RVAL
      END IF

*  Obtain the plotting attributes (colour, width, font, size, style)
*  to be used when drawing the lines, markers, and error bars.
*  =================================================================

*  Save the plotting attributes for the AST graphical elements which may
*  be changed by this routine, so that we can re-instate before we
*  return.  Clear any set attibute values so that new values can be
*  used later.
      DO I = 1, NAT
         ATT0( I ) = AST_GETR( IPLOT, ATTNAM( I ), STATUS )
         CALL AST_CLEAR( IPLOT, ATTNAM( I ), STATUS )
      END DO

*  Establish synonyms for AST graphical element names to be recognised
*  during the following call to KPG1_ASSET.  The symbols marking each
*  position are drawn as AST "markers" using AST_MARK.  The lines
*  joining the given positions are drawn as AST "Curves" using
*  AST_CURVE.   The error bars are also drawn using AST_CURVE and
*  therefore we need to use a different  element (i.e. not "Curves") to
*  represent them, since "Curves" is already being used to represent the
*  lines joining positions.  We arbitrarily use "Axes" to represent
*  error bars.  The Axes attributes will be copied to the Curves
*  attributes prior to drawing the error bars.
      CALL KPG1_ASPSY( '(ERR*BARS)', '(AXES)', STATUS )
      CALL KPG1_ASPSY( '(SYM*BOLS)', '(MARKERS)', STATUS )
      CALL KPG1_ASPSY( '(LIN*ES)', '(CURVES)', STATUS )

*  Set the attributes of the supplied Plot using the supplied parameter
*  to access a plotting style.  The above synonyms are recognised and
*  translated into the corresponding AST attribute names.  Colour names
*  are also translated into colour indices.
      CALL KPG1_ASSET( APP, PARAM1, IPLOT, STATUS )

*  Save the attributes to be used if no pen definitions have been
*  supplied in IGRP.
      DO I = 1, NAT
         ATT1( I ) = AST_GETR( IPLOT, ATTNAM( I ), STATUS )
      END DO

*  Get the drawing attributes to use for each contour.
      CALL KPS1_MLPPN( PARAM2, IGRP, STATUS )

*  Store the number of pen definitions supplied in the GRP group.  These
*  definitions will override the attributes set by the previous call to
*  KPG1_ASSET.
      IF( IGRP .NE. GRP__NOID ) THEN
         CALL GRP_GRPSZ( IGRP, NPEN, STATUS )
      ELSE
         NPEN = 0
      END IF

*  Initialise the pen number.
      IPEN = 0

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Loop round each used curve.
*  ===========================
      DO K = 1, NDISP

*  If any individual pen definitions were supplied...
         IF( NPEN .GT. 0 ) THEN

*  Increment the index of the next pen to use, cycling back to the start
*  when the end is reached.
            IPEN = IPEN + 1
            IF( IPEN .GT. NPEN ) IPEN = 1

         END IF

*  Continue if this curve is not to be drawn.
         IF( .NOT. USE( K ) ) GO TO 20

*  If different pens are being used, set the required attributes in the
*  Plot to produce the pen style for this curve.
         IF( NPEN .GT. 0 ) THEN

*  Get the next list of AST Attribute settings from the group.
            CALL GRP_GET( IGRP, IPEN, 1, PENDEF, STATUS )

*  Abort if an error has occurred.
            IF( STATUS .NE. SAI__OK ) GO TO 999

*  Loop round each comma-delimited attribute in the definitions,
*  translating colour names and any defined synonyms, and storing it in
*  the Plot.
            IF( PENDEF .NE. ' ' ) THEN
               J1 = 1
               DO WHILE( J1 .LE. GRP__SZNAM )
                  J2 = J1
                  CALL CHR_FIND( PENDEF, ',', .TRUE., J2 )
                  CALL KPG1_ASSTS( PENDEF( J1 : J2 - 1 ), .TRUE.,
     :                             .TRUE., IPLOT, BADAT, STATUS )
                  J1 = J2 + 1

*  Annul any error which occurred while setting the pen.
                  IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )

               END DO

            END IF

         END IF

*  Start a PGPLOT buffering context.
         CALL PGBBUF

*  First deal with "staircase" histograms.
*  =======================================
         IF( MODE .EQ. 1 ) THEN

*  Set up the correct PGPLOT attributes for the type of curve being
*  drawn.
            CALL KPG1_PGSTY( IPLOT, 'CURVES', .TRUE., ATTR, STATUS )

*  Indicate that the pen is initially up.
            DOWN = .FALSE.

*  Indicate we have no previous position.
            GOODX0 = .FALSE.
            GOODY0 = .FALSE.

*  Loop round each X/Y position in the given range.
            DO I = ILO, IHI

*  Save the single precision version of the current position.  Set flags
*  indicating if they are good.
               GOODX = ( X( I, K ) .NE. AST__BAD )
               IF( GOODX ) RX = REAL( X( I, K ) )

               GOODY = ( Y( I, K ) .NE. AST__BAD )
               IF( GOODY ) RY = REAL( Y( I, K ) )

*  See if the mid X position between this point and the previous point
*  is defined.  If so, store it.
               MIDX = GOODX .AND. GOODX0
               IF( MIDX ) RXC = 0.5*( RX + RX0 )

*  On each pass through this loop three lines may be drawn; A) a
*  horizontal line from the previous position (I-1), half way towards
*  the current position (I); B) a vertical line from the end of A) to
*  the Y value of the current position; C) a horizontal line from the
*  end of B) to the current position.

*  To draw C) the mid X position must be good and the current position
*  must have a good Y value.
               DRAWC = ( MIDX .AND. GOODY )

*  If possible draw line A). To draw A), the mid X position must be
*  defined and the previous position must have a good Y value.
               IF( MIDX .AND. GOODY0 ) THEN

*  If the pen is now down, put it down at the previous position.
                  IF( .NOT. DOWN ) THEN
                     CALL PGMOVE( RX0, RY0 )
                     DOWN = .TRUE.
                  END IF

*  Draw to the mid position.
                  CALL PGDRAW( RXC, RY0 )

*  If it is also possible to draw line C, then we can draw line B) now.
                  IF( DRAWC ) CALL PGDRAW( RXC, RY )

               END IF

*  If possible draw line C).
               IF( DRAWC ) THEN

*  If the pen is now down, put it down at the mid position.
                  IF( .NOT. DOWN ) THEN
                     CALL PGMOVE( RXC, RY )
                     DOWN = .TRUE.
                  END IF

*  Draw from the mid position to the current position.
                  CALL PGDRAW( RX, RY )

*  If we cannot draw line C), pick the pen up.
               ELSE
                  DOWN = .FALSE.
               END IF

*  Save information about the current position.
               GOODX0 = GOODX
               GOODY0 = GOODY
               RX0 = RX
               RY0 = RY

            END DO

*  Re-instate the original PGPLOT attributes.
            CALL KPG1_PGSTY( IPLOT, 'CURVES', .FALSE., ATTR, STATUS )

*  Now deal with points joined by straight lines.
*  ==============================================
         ELSE IF( MODE .EQ. 2 ) THEN

*  Set up the correct PGPLOT attributes for the type of curve being
*  drawn.
            CALL KPG1_PGSTY( IPLOT, 'CURVES', .TRUE., ATTR, STATUS )

*  Indicate the pen is initially up.
            DOWN = .FALSE.

*  Loop round each X/Y position in the given range.
            DO I = ILO, IHI

*  If the pen is currently down...
               IF( DOWN ) THEN

*  If both X and Y are good at the current position, draw a line to the
*  current position.
                  IF( X( I, K ) .NE. AST__BAD .AND.
     :                Y( I, K ) .NE. AST__BAD ) THEN
                     CALL PGDRAW( REAL( X( I, K ) ), REAL( Y( I, K ) ) )

*  If either X or Y are bad, pick the pen up.
                  ELSE
                     DOWN = .FALSE.

                  END IF

*  If the pen was originally up, and both X and Y are good at the
*  current position, put the pen down.
               ELSE IF( X( I, K ) .NE. AST__BAD .AND.
     :                  Y( I, K ) .NE. AST__BAD ) THEN
                  CALL PGMOVE( REAL( X( I, K ) ), REAL( Y( I, K ) ) )
                  DOWN= .TRUE.

               END IF

            END DO

*  Re-instate the original PGPLOT attributes.
            CALL KPG1_PGSTY( IPLOT, 'CURVES', .FALSE., ATTR, STATUS )

*  Now deal with a marker placed at each point.
*  ============================================
         ELSE IF( MODE .EQ. 3 ) THEN

*  Set up the correct PGPLOT attributes for the type of curve being
*  drawn.
            CALL KPG1_PGSTY( IPLOT, 'MARKERS', .TRUE., ATTR, STATUS )

*  Draw a marker at each good X/Y position in the given range.
            DO I = ILO, IHI
               IF( X( I, K ) .NE. AST__BAD .AND.
     :             Y( I, K ) .NE. AST__BAD ) THEN
                  CALL PGPT( 1, REAL( X( I, K ) ), REAL( Y( I, K ) ),
     :                       MTYPE )
               END IF
            END DO

*  Re-instate the original PGPLOT attributes.
            CALL KPG1_PGSTY( IPLOT, 'MARKERS', .FALSE., ATTR, STATUS )

*  Now deal with a horizontal line at each point.
*  ==============================================
         ELSE IF( MODE .EQ. 4 ) THEN

*  Set up the correct PGPLOT attributes for the type of curve being
*  drawn.
            CALL KPG1_PGSTY( IPLOT, 'CURVES', .TRUE., ATTR, STATUS )

*  Draw a line at each position which has good values for the upper and
*  lower X limits, and a good Y value, and is in the given range.
            DO I = ILO, IHI
               IF( XSTEP( I, 1, K ) .NE. AST__BAD .AND.
     :             XSTEP( I, 2, K ) .NE. AST__BAD .AND.
     :             Y( I, K ) .NE. AST__BAD ) THEN
                  RY = REAL( Y( I, K ) )
                  CALL PGMOVE( REAL( XSTEP( I, 1, K ) ), RY )
                  CALL PGDRAW( REAL( XSTEP( I, 2, K ) ), RY )
               END IF
            END DO

*  Re-instate the original PGPLOT attributes.
            CALL KPG1_PGSTY( IPLOT, 'CURVES', .FALSE., ATTR, STATUS )

*  Now deal with a lines joining points with a marker at each point.
*  =================================================================
         ELSE IF( MODE .EQ. 5 ) THEN

*  Set up the correct PGPLOT attributes for the type of curve being
*  drawn.  First the curves.
            CALL KPG1_PGSTY( IPLOT, 'CURVES', .TRUE., ATTR, STATUS )

*  Indicate the pen is initially up.
            DOWN = .FALSE.

*  Loop round each X/Y position in the given range.
            DO I = ILO, IHI

*  If the pen is currently down...
               IF( DOWN ) THEN

*  If both X and Y are good at the current position, draw a line to the
*  current position.
                  IF( X( I, K ) .NE. AST__BAD .AND.
     :                Y( I, K ) .NE. AST__BAD ) THEN
                     CALL PGDRAW( REAL( X( I, K ) ), REAL( Y( I, K ) ) )

*  If either X or Y are bad, pick the pen up.
                  ELSE
                     DOWN = .FALSE.

                  END IF

*  If the pen was originally up, and both X and Y are good at the
*  current position, put the pen down.
               ELSE IF( X( I, K ) .NE. AST__BAD .AND.
     :                  Y( I, K ) .NE. AST__BAD ) THEN
                  CALL PGMOVE( REAL( X( I, K ) ), REAL( Y( I, K ) ) )
                  DOWN = .TRUE.

               END IF

            END DO

*  Re-instate the original PGPLOT attributes.
            CALL KPG1_PGSTY( IPLOT, 'CURVES', .FALSE., ATTR, STATUS )

*  Now draw the markers.  Set PGPLOT attributes to match the plotting
*  style used by the Plot for drawing markers.  Save the current PGPLOT
*  attribute values in ATTR.
            CALL KPG1_PGSTY( IPLOT, 'MARKERS', .TRUE., ATTR, STATUS )

*  Draw a marker at each good X/Y position in the given range.
            DO I = ILO, IHI
               IF( X( I, K ) .NE. AST__BAD .AND.
     :             Y( I, K ) .NE. AST__BAD ) THEN
                  CALL PGPT( 1, REAL( X( I, K ) ), REAL( Y( I, K ) ),
     :                       MTYPE )
               END IF
            END DO

*  Re-instate the original PGPLOT attributes.
            CALL KPG1_PGSTY( IPLOT, 'MARKERS', .FALSE., ATTR, STATUS )

*  Report an error if the MODE value was illegal.
         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'MODE', MODE )
            CALL ERR_REP( 'KPS1_MLPML_ERR1', 'KPS1_MLPML: Illegal '//
     :                    'MODE value (^MODE) supplied (programming '//
     :                    'error).', STATUS )
         END IF

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Now draw the error bars if required.
*  ====================================
         IF( XERROR .OR. YERROR ) THEN

*  Set PGPLOT attributes to match the plotting style used by the Plot
*  for drawing Axes.  Save the current PGPLOT attribute values in ATTR.
            CALL KPG1_PGSTY( IPLOT, 'AXES', .TRUE., ATTR, STATUS )

*  Get the size of the serif for error bars.  This is scaled by the
*  "Size(Errbars)" attribute which is a synonym for "Size(Axes)".
            SERIF = AST_GETR( IPLOT, 'SIZE(AXES)', STATUS )*0.02*
     :                        MIN( ABS( WX2 - WX1 ), ABS( WY2 - WY1 ) )

*  Loop round positions which have good X and Y values.  Step over FREQ
*  positions each time.
            DO I = ILO + FREQ/2, IHI, FREQ
               IF( X( I, K ) .NE. AST__BAD .AND.
     :             Y( I, K ) .NE. AST__BAD ) THEN
                  RX = REAL( X( I, K ) )
                  RY = REAL( Y( I, K ) )

*  Find the X limits of the error box.
                  XLO = RX
                  XHI = RX
                  IF( XERROR ) THEN

                     ERR = XBAR( I, 1, K )
                     IF( ERR .NE. AST__BAD ) THEN
                         XHI = MAX( XHI, ERR )
                         XLO = MIN( XLO, ERR )
                     END IF

                     ERR = XBAR( I, 2, K )
                     IF( ERR .NE. AST__BAD ) THEN
                         XHI = MAX( XHI, ERR )
                         XLO = MIN( XLO, ERR )
                     END IF

                  END IF

*  Find the Y limits of the error box.
                  YLO = RY
                  YHI = RY
                  IF( YERROR ) THEN

                     ERR = YBAR( I, 1, K )
                     IF( ERR .NE. AST__BAD ) THEN
                         YHI = MAX( YHI, ERR )
                         YLO = MIN( YLO, ERR )
                     END IF

                     ERR = YBAR( I, 2, K )
                     IF( ERR .NE. AST__BAD ) THEN
                         YHI = MAX( YHI, ERR )
                         YLO = MIN( YLO, ERR )
                     END IF

                  END IF

*  If ERSHAP specifies a diagonal cross, draw a poly line from
*  the bottom-left on to centre, to top-right corner of the error box.
                  IF( ERSHAP .EQ. 2 ) THEN
                     IF( XERROR ) THEN
                        CALL PGMOVE( XLO, YLO )
                        CALL PGDRAW(  RX,  RY )
                        CALL PGDRAW( XHI, YHI )
                     END IF
                     IF( YERROR ) THEN
                        CALL PGMOVE( XLO, YHI )
                        CALL PGDRAW(  RX,  RY )
                        CALL PGDRAW( XHI, YLO )
                     END IF

*  If ERSHAP specifies a vertical cross, draw line between the limits at
*  through central X/Y position, and ad short bars across the ends.
                  ELSE IF( ERSHAP .EQ. 1 ) THEN

                     IF( XERROR ) THEN
                        CALL PGMOVE( XLO, RY )
                        CALL PGDRAW( XHI, RY )

                        CALL PGMOVE( XLO, RY - SERIF )
                        CALL PGDRAW( XLO, RY + SERIF )
                        CALL PGMOVE( XHI, RY - SERIF )
                        CALL PGDRAW( XHI, RY + SERIF )
                     END IF

                     IF( YERROR ) THEN
                        CALL PGMOVE( RX, YLO )
                        CALL PGDRAW( RX, YHI )

                        CALL PGMOVE( RX - SERIF, YLO )
                        CALL PGDRAW( RX + SERIF, YLO )
                        CALL PGMOVE( RX - SERIF, YHI )
                        CALL PGDRAW( RX + SERIF, YHI )
                     END IF

*  If ERSHAP specifies a diamond, draw a poly line around the error box.
                  ELSE IF( ERSHAP .EQ. 3 ) THEN
                     CALL PGMOVE( XLO, RY )
                     CALL PGDRAW( RX, YHI )
                     CALL PGDRAW( XHI, RY )
                     CALL PGDRAW( RX, YLO )
                     CALL PGDRAW( XLO, RY )

*  Report an error if the MODE value was illegal.
                  ELSE IF( STATUS .EQ. SAI__OK ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'SHAP', ERSHAP )
                     CALL ERR_REP( 'KPS1_MLPML_ERR2', 'KPS1_MLPML: '//
     :                             'Illegal ERSHAP value (^SHAP) '//
     :                             'supplied (programming error).',
     :                             STATUS )
                     GO TO 999

                  END IF

               END IF

            END DO

*  Re-instate the original PGPLOT attributes.
            CALL KPG1_PGSTY( IPLOT, 'AXES', .FALSE., ATTR, STATUS )

         END IF

*  End the PGPLOT buffering context.
         CALL PGEBUF

*  If separate pen definitions are being used, re-instate the plotting
*  attributes specified by the parameter PARAM1.
         IF( NPEN .GT. 0 ) THEN
            DO I = 1, NAT
               CALL AST_SETR( IPLOT, ATTNAM( I ), ATT1( I ), STATUS )
            END DO
         END IF

*  Jump here to continue with the next curve.
 20      CONTINUE

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

      END DO

*  Tidy up.
*  ========

 999  CONTINUE

*  Release any group holding pen defintions.
      IF( IGRP .NE. GRP__NOID ) CALL GRP_DELET( IGRP, STATUS )

*  Release resources used to store the synonyms.
      CALL KPG1_ASPSY( ' ', ' ', STATUS )

*  Re-instate the original plotting attributes for the AST graphical
*  elements that may have been changed by this routine.
      DO I = 1, NAT
         CALL AST_SETR( IPLOT, ATTNAM( I ), ATT0( I ), STATUS )
      END DO

      END
