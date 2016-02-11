      SUBROUTINE KPG1_PLTLN( N, ILO, IHI, X, Y, XERROR, YERROR, XBAR,
     :                       YBAR, XSTEP,PARAM, IPLOT, MODE, MTYPE,
     :                       ERSHAP, FREQ, APP, STATUS )
*+
*  Name:
*     KPG1_PLTLN

*  Purpose:
*     Produces a graphical representation of a set of points in two
*     dimensions

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PLTLN( N, ILO, IHI, X, Y, XERROR, YERROR, XBAR, YBAR,
*                      XSTEP, PARAM, IPLOT, MODE, MTYPE, ERSHAP, FREQ,
*                      APP, STATUS )

*  Description:
*     This routine produces a graphical representation of a set of
*     points in two-dimensional space (e.g. a data value and a position,
*     or two data values).  Errors in both data values may be
*     represented by error bars. No annotated axes are drawn. The
*     calling routine should do this if required by passing the supplied
*     Plot (IPLOT) to routine KPG1_ASGRD.
*
*     PGPLOT should be active, and the viewport should correspond to the
*     DATA picture in which the plot is to be drawn. PGPLOT world co-ordinates
*     within the viewport should be GRAPHICS co-ordinates (millimetres from the
*     bottom-left corner of the view surface).
*
*     The Plotting style is accessed using one or more environment parameters
*     specified by PARAM, and may include the following synonyms for graphical
*     elements:
*        "Err(Bars)" - Specifies colour, etc. for error bars. Size(errbars)
*                      scales the size of the serifs used if ERSHAP=1 (i.e.
*                      a size value of 1.0 produces a default size).
*        "Sym(bols)" - Specifies colour, etc. for markers (used in Modes 3
*                      and 5).
*        "Lin(es)" - Specifies colour, etc. for lines (used in Modes 1, 2,
*                    4 and 5).

*  Arguments:
*     N = INTEGER (Given)
*        Number of points to be plotted.
*     ILO = INTEGER (Given)
*        The index of the first grid point to be used.
*     IHI = INTEGER (Given)
*        The index of the last grid point to be used.
*     X( N ) = DOUBLE PRECISION (Given)
*        The X value at each point, in PGPLOT world co-ordinate (i.e.
*        millimetres from the bottom-left corner of the view surface).
*     Y( N ) = DOUBLE PRECISION (Given)
*        The Y value at each point, in PGPLOT world co-ordinate (i.e.
*        millimetres from the bottom-left corner of the view surface).
*     XERROR = LOGICAL (Given)
*        Display X error bars?
*     YERROR = LOGICAL (Given)
*        Display Y error bars?
*     XBAR( N, 2 ) = DOUBLE PRECISION (Given)
*        Row 1 contains the lower limit and Row 2 contains the upper limit
*        for each horizontal error bar, in PGPLOT world co-ordinate (i.e.
*        millimetres from the bottom left corner of the view surface). Only
*        accessed if XERROR is .TRUE.
*     YBAR( N, 2 ) = DOUBLE PRECISION (Given)
*        Row 1 contains the lower limit and Row 2 contains the upper limit
*        for each vertical error bar, in PGPLOT world co-ordinate (i.e.
*        millimetres from the bottom-left corner of the view surface). Only
*        accessed if YERROR is .TRUE.
*     XSTEP( N, 2 ) = DOUBLE PRECISION (Given)
*        Row 1 contains the lower limit and Row 2 contains the upper limit
*        for each horizontal step, in PGPLOT world co-ordinate (i.e.
*        millimetres from the bottom-left corner of the view surface). Only
*        accessed if MODE is 4.
*     PARAM = CHARACTER * ( * ) (Given)
*        The names of one or more style parameters to be used when obtaining
*        the plotting style. If more than one parameter is supplied, the
*        list should be separated by commas. The supplied parameters will
*        be used in the order supplied, with later parameters allowing
*        attributes obtained via earlier parameters to be assigned new values.
*     IPLOT = INTEGER (Given)
*        An AST Plot which can be used to do the drawing. The Base Frame
*        should be GRAPHICS co-ordinates (millimetres from the bottom-
*        left corner of the PGPLOT view surface). The Current Frame should
*        be the Frame in which annotation is required.
*     MODE = INTEGER (Given)
*        Determines the way in which the data points are represented.
*        The options are as follows.
*           1 - A "staircase" histogram, in which each horizontal line is
*               centred on the X position. Bad values are flanked by vertical
*               lines drawn down to the lower edge of the viewport.
*           2 - The points are joined by straight lines.
*           3 - A marker is placed at each point (see MTYPE).
*           4 - Mark each point with a horizontal line of width given by
*               XW.
*           5 - A "chain" in which each point is marker by a marker and also
*               join by straight lines to its neighbouring points.
*           6 - The same as Mode 1, except that bad values are not
*               flanked by vertical lines drawn down to the lower edge
*               of the viewport (a simple gap is left instead).
*           7 - The data points are not drawn.
*     MTYPE = INTEGER (Given)
*        The PGPLOT marker type to use if MODE is 3 or 5.
*     ERSHAP = INTEGER (Given)
*        Determines the way in which error bars are drawn:
*           1 - X and Y errors are represented by horizontal and vertical
*               lines respectively. Serifs are drawn at the ends of each
*               line. The size of these sreifs is controlled by the
*               size(errbar) plotting attribute.
*           2 - A cross is drawn joining the corners of the box
*               encompassing the X and Y errors.
*           3 - A Diamond is drawn joining the ends of the horizontal and
*               vertical error bars which would have been drawn if ERSHAP
*               had been 1.
*        These will all produce the same result (i.e. a single straight
*        line) if errors are available only on one axis (see XERROR and
*        YERROR). Not accessed if XERROR and YERROR are both .FALSE.
*     FREQ = INTEGER (Given)
*        The frequency at which errors are to be plotted. A value of 1
*        means "plot errors for every point", 2 means "plot errors for
*        every second point", etc. Not accessed if XERROR and YERROR are
*        both .FALSE.
*     APP = CHARACTER * ( * ) (Given)
*        The name of the calling application in the form
*        <package>_<application> (eg "KAPPA_DISPLAY").
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998, 1999, 2001 Central Laboratory of the Research Councils.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     DSB: David S. Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-SEP-1998 (DSB):
*        Original version.
*     9-DEC-1998 (DSB):
*        Check X and Y against AST__BAD before checking that there
*        is at least one point within the plot.
*     17-JUN-1999 (DSB):
*        Call to PGEBUF added to end the PGPLOT buffering context.
*     1-MAR-2001 (DSB):
*        Change Mode 1 so that vertical lines are drawn at the boundaries
*        of bad cells
*     10-JUL-2001 (DSB):
*        Corrected data type of ERR from REAL to DOUBLE PRECISION.
*     17-JUL-2001 (DSB):
*        Draw the error bars before the data points, rather than after
*        them, to avoid the data points being obscured by the error bars.
*     18-SEP-2001 (DSB):
*        Make bad error bars extend to the corresponding edge of the plot.
*     5-OCT-2005 (DSB):
*        Free resources used to hold attribute synonyms before returning.
*     20-MAR-2007 (DSB):
*        Allow more than one style parameter to be given via argument "PARAM".
*     2010 August 10 (MJC):
*        Added Mode=6.
*     11-AUG-2010 (DSB):
*        Added mode 6.
*     11-FEB-2016 (DSB):
*        Added mode 7.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Arguments Given:
      INTEGER N
      INTEGER ILO
      INTEGER IHI
      DOUBLE PRECISION X( N )
      DOUBLE PRECISION Y( N )
      LOGICAL XERROR
      LOGICAL YERROR
      DOUBLE PRECISION XBAR( N, 2 )
      DOUBLE PRECISION YBAR( N, 2 )
      DOUBLE PRECISION XSTEP( N, 2 )
      CHARACTER PARAM*(*)
      INTEGER IPLOT
      INTEGER MODE
      INTEGER MTYPE
      INTEGER ERSHAP
      INTEGER FREQ
      CHARACTER APP*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION ATTR( 20 )! Saved graphics attribute values
      DOUBLE PRECISION ERR       ! Error bar limit value
      INTEGER AXCOL0             ! Original Axis colour index
      INTEGER AXSTY0             ! Original Axis line style
      INTEGER COMMA              ! Index of first comma relative to START
      INTEGER CVCOL0             ! Original Curve Colour index
      INTEGER CVSTY0             ! Original Curve line style
      INTEGER END                ! Index of parameter name end
      INTEGER I                  ! Position index
      INTEGER MKCOL0             ! Original Marker Colour index
      INTEGER MKFNT0             ! Original Marker font
      INTEGER MKSTY0             ! Original Marker line style
      INTEGER START              ! Index of parameter name start
      LOGICAL DOWN               ! Is the pen down on the paper?
      LOGICAL DRAWC              ! Can line C be drawn?
      LOGICAL GOODX              ! Is current X value good?
      LOGICAL GOODX0             ! Was previous X value good?
      LOGICAL GOODY              ! Is current Y value good?
      LOGICAL GOODY0             ! Was previous Y value good?
      LOGICAL MIDX               ! Is middle X value good?
      LOGICAL MIDY               ! Is middle Y value good?
      LOGICAL MORE               ! Continue looping?
      LOGICAL OK                 ! Are there any points within the window?
      REAL AXWID0                ! Original Axis line width
      REAL CVWID0                ! Original Curve line width
      REAL MKWID0                ! Original Marker line width
      REAL RVAL                  ! General REAL variable
      REAL RX                    ! Single precision central X position
      REAL RX0                   ! Previous single precision central X position
      REAL RXC                   ! X half way from previous to current position
      REAL RY                    ! Single precision central Y position
      REAL RY0                   ! Previous single precision central Y position
      REAL SERIF                 ! Length of serif bar
      REAL WX1                   ! Lower X limit of PGPLOT window
      REAL WX2                   ! Higher X limit of PGPLOT window
      REAL WY1                   ! Lower Y limit of PGPLOT window
      REAL WY2                   ! Higher Y limit of PGPLOT window
      REAL XHI                   ! Upper X limit of error box
      REAL XLO                   ! Lower X limit of error box
      REAL YHI                   ! Upper Y limit of error box
      REAL YLO                   ! Lower Y limit of error box
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

*  Check there is at least one supplied position within the
*  PGPLOT window.
      OK = .FALSE.
      DO I = ILO, IHI
         IF( X( I ) .NE. AST__BAD .AND. Y( I ) .NE. AST__BAD ) THEN

            IF( REAL( X( I ) ) .GT. WX1 .AND.
     :          REAL( X( I ) ) .LT. WX2 .AND.
     :          REAL( Y( I ) ) .GT. WY1 .AND.
     :          REAL( Y( I ) ) .LT. WY2 ) THEN
               OK = .TRUE.
               GO TO 10
            END IF

         END IF
      END DO

 10   CONTINUE

*  Issue a warning if no positions fall within the window.
      IF( .NOT. OK ) THEN
         CALL MSG_BLANK( STATUS )
         CALL MSG_OUT( 'KPG1_PLTLN_MSG', 'No data points fall within '//
     :                 'the bounds of the plot.', STATUS )
      END IF

*  Start a PGPLOT buffering context.
      CALL PGBBUF

*  Obtain the plotting attributes (colour, width, font, size, style)
*  to be used when drawing the lines, markers, and error bars.
*  =================================================================

*  Save the plotting attributes for the AST graphical elements which may
*  be changed by this routine, so that we can re-instate them later.
      AXCOL0 = AST_GETI( IPLOT, 'COLOUR(AXES)', STATUS )
      AXWID0 = AST_GETR( IPLOT, 'WIDTH(AXES)', STATUS )
      AXSTY0 = AST_GETI( IPLOT, 'STYLE(AXES)', STATUS )
      MKCOL0 = AST_GETI( IPLOT, 'COLOUR(MARKERS)', STATUS )
      MKWID0 = AST_GETR( IPLOT, 'WIDTH(MARKERS)', STATUS )
      MKFNT0 = AST_GETI( IPLOT, 'FONT(MARKERS)', STATUS )
      MKSTY0 = AST_GETI( IPLOT, 'STYLE(MARKERS)', STATUS )
      CVCOL0 = AST_GETI( IPLOT, 'COLOUR(CURVES)', STATUS )
      CVWID0 = AST_GETR( IPLOT, 'WIDTH(CURVES)', STATUS )
      CVSTY0 = AST_GETI( IPLOT, 'STYLE(CURVES)', STATUS )

*  Establish synonyms for AST graphical element names to be recognised
*  during the following call to KPG1_ASSET. The symbols marking each position
*  are drawn as AST "markers" using AST_MARK. The lines joining the given
*  positions are drawn as AST "Curves" using AST_CURVE. The error bars are
*  also drawn using AST_CURVE and therefore we need to use a different
*  element (i.e. not "Curves") to represent them, since "Curves" is
*  already being used to represent the lines joining positions. We
*  arbitrarily use "Axes" to represent error bars. The Axes attributes
*  will be copied to the Curves attributes prior to drawing the error bars.
      CALL KPG1_ASPSY( '(ERR*BARS)', '(AXES)', STATUS )
      CALL KPG1_ASPSY( '(SYM*BOLS)', '(MARKERS)', STATUS )
      CALL KPG1_ASPSY( '(LIN*ES)', '(CURVES)', STATUS )

*  Set the attributes of the supplied Plot using each supplied parameter
*  in turn to access a plotting style. Values obtained using earlier
*  parameters in the list can be over-ridden using later parameters. The
*  above synonyms are recognised and translated into the corresponding
*  AST attribute names. Colour names are also translated into colour indices.
      START = 1
      MORE = .TRUE.
      DO WHILE( MORE )

*  Find the index of the first comma following the current START position.
         COMMA = INDEX( PARAM( START : ), ',' )

*  If no comma was found, assume the next parameter name ends at the end
*  of the PARAM string, and indicate that we will leave the DO loop after
*  processing the current parameter name.
         IF( COMMA .EQ. 0 ) THEN
            MORE = .FALSE.
            END = LEN( PARAM )

*  IF a comma was found, calculate the offset of the last character in
*  the parameter name.
         ELSE
            END = START - 1 + COMMA - 1
         END IF

*  Use current parameter name to get attribute values for the Plot,
*  over-writing any obtained using earlier parameters.
         CALL KPG1_ASSET( APP, PARAM( START : END ), IPLOT, STATUS )

*  Update the starting position, so that the search for the next comma
*  (if any) starts at the character following the current comma.
         START = END + 2

      END DO

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  First draw the error bars if required. This is done first so that the
*  data points are not obscured by the error bars.
*  =====================================================================
      IF( ( XERROR .OR. YERROR ) .AND. STATUS .EQ. SAI__OK ) THEN

*  Save the size of the serif for error bars. This is scaled by the
*  "size(errbars)" attribute which is a synonym for "size(axes)".
         SERIF = AST_GETR( IPLOT, 'SIZE(AXES)', STATUS )*0.02*
     :                     MIN( ABS( WX2 - WX1 ), ABS( WY2 - WY1 ) )

*  The plotting attributes to use for the error bars are currently
*  assigned to the "Axes" element in the Plot. We need to transfer these
*  to the "Curves" elements.
         CALL AST_SETI( IPLOT, 'COLOUR(CURVES)',
     :                  AST_GETI( IPLOT, 'COLOUR(AXES)', STATUS ),
     :                  STATUS )

         CALL AST_SETR( IPLOT, 'WIDTH(CURVES)',
     :                  AST_GETR( IPLOT, 'WIDTH(AXES)', STATUS ),
     :                  STATUS )

         CALL AST_SETI( IPLOT, 'STYLE(CURVES)',
     :                  AST_GETI( IPLOT, 'STYLE(AXES)', STATUS ),
     :                  STATUS )


*  Set PGPLOT attributes to match the plotting style used by the Plot for
*  drawing Curves. Save the current PGPLOT attribute values in ATTR.
         CALL KPG1_PGSTY( IPLOT, 'CURVES', .TRUE., ATTR, STATUS )

*  Loop round positions which have good X and Y values. Step over FREQ
*  positions each time.
         DO I = ILO + FREQ/2, IHI, FREQ
            IF( X( I ) .NE. AST__BAD .AND. Y( I ) .NE. AST__BAD ) THEN
               RX = REAL( X( I ) )
               RY = REAL( Y( I ) )

*  Find the X limits of the error box.
               XLO = RX
               XHI = RX
               IF( XERROR ) THEN

                  ERR = XBAR( I, 1 )
                  IF( ERR .EQ. AST__BAD ) ERR = WX1
                  XHI = MAX( XHI, REAL( ERR ) )
                  XLO = MIN( XLO, REAL( ERR ) )

                  ERR = XBAR( I, 2 )
                  IF( ERR .EQ. AST__BAD ) ERR = WX2
                  XHI = MAX( XHI, REAL( ERR ) )
                  XLO = MIN( XLO, REAL( ERR ) )

                  IF( XHI .EQ. WX2 .AND. XHI .EQ. WX1 ) THEN
                     XLO = RX
                     XHI = RX
                  END IF

               END IF

*  Find the Y limits of the error box.
               YLO = RY
               YHI = RY
               IF( YERROR ) THEN

                  ERR = YBAR( I, 1 )
                  IF( ERR .EQ. AST__BAD ) ERR = WY1
                  YHI = MAX( YHI, REAL( ERR ) )
                  YLO = MIN( YLO, REAL( ERR ) )

                  ERR = YBAR( I, 2 )
                  IF( ERR .EQ. AST__BAD ) ERR = WY2
                  YHI = MAX( YHI, REAL( ERR ) )
                  YLO = MIN( YLO, REAL( ERR ) )

                  IF( YHI .EQ. WY2 .AND. XHI .EQ. WY1 ) THEN
                     YLO = RY
                     YHI = RY
                  END IF

               END IF

*  If ERSHAP specifies a diagonal cross, draw a poly line from bottom left
*  to centre, to top-right corner of the error box,
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

                     IF( XLO .NE. WX1 ) THEN
                        CALL PGMOVE( XLO, RY - SERIF )
                        CALL PGDRAW( XLO, RY + SERIF )
                     END IF
                     IF( XHI .NE. WX2 ) THEN
                        CALL PGMOVE( XHI, RY - SERIF )
                        CALL PGDRAW( XHI, RY + SERIF )
                     END IF
                  END IF

                  IF( YERROR ) THEN
                     CALL PGMOVE( RX, YLO )
                     CALL PGDRAW( RX, YHI )

                     IF( YLO .NE. WY1 ) THEN
                        CALL PGMOVE( RX - SERIF, YLO )
                        CALL PGDRAW( RX + SERIF, YLO )
                     END IF
                     IF( YHI .NE. WY2 ) THEN
                        CALL PGMOVE( RX - SERIF, YHI )
                        CALL PGDRAW( RX + SERIF, YHI )
                     END IF
                  END IF

*  If ERSHAP specifies a diamond, draw a poly line around the error box.
               ELSE IF( ERSHAP .EQ. 3 ) THEN
                  CALL PGMOVE( XLO, RY )
                  CALL PGDRAW( RX, YHI )
                  CALL PGDRAW( XHI, RY )
                  CALL PGDRAW( RX, YLO )
                  CALL PGDRAW( XLO, RY )

*  Report an error if the ERSHAP value was illegal.
               ELSE IF( STATUS .EQ. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'SHAP', ERSHAP )
                  CALL ERR_REP( 'KPG1_PLTLN_ERR2', 'KPG1_PLTLN: '//
     :                          'Illegal ERSHAP value (^SHAP) '//
     :                          'supplied (programming error).',
     :                          STATUS )
                  GO TO 999

               END IF

            END IF

         END DO

*  Re-instate the original PGPLOT attributes.
         CALL KPG1_PGSTY( IPLOT, 'CURVES', .FALSE., ATTR, STATUS )

*  Re-instate the original plotting attributes for AST Curves.
         CALL AST_SETI( IPLOT, 'COLOUR(CURVES)', CVCOL0, STATUS )
         CALL AST_SETR( IPLOT, 'WIDTH(CURVES)', CVWID0, STATUS )
         CALL AST_SETI( IPLOT, 'STYLE(CURVES)', CVSTY0, STATUS )

      END IF

*  Now draw the data points on top of the error bars.

*  First deal with "staircase" histograms.
*  =======================================
      IF( MODE .EQ. 1 .OR. MODE .EQ. 6 ) THEN

*  Set PGPLOT attributes to match the plotting style used by the Plot for
*  drawing Curves. Save the current PGPLOT attribute values in ATTR.
         CALL KPG1_PGSTY( IPLOT, 'CURVES', .TRUE., ATTR, STATUS )

*  Indicate that the pen is initially up.
         DOWN = .FALSE.

*  Indicate we have no previous position.
         GOODX0 = .FALSE.
         GOODY0 = .FALSE.

*  Loop round each X/Y position in the given range.
         DO I = ILO, IHI

*  Save the single-precision version of the current position. Set flags
*  indicating if they are good.
            GOODX = ( X( I ) .NE. AST__BAD )
            IF( GOODX ) RX = REAL( X( I ) )

            GOODY = ( Y( I ) .NE. AST__BAD )
            IF( GOODY ) THEN
               RY = REAL( Y( I ) )
            ELSE
               RY = WY1
            END IF

*  See if the mid X position between this point and the previous point is
*  defined. If so, store it.
            MIDX = GOODX .AND. GOODX0
            IF( MIDX ) RXC = 0.5*( RX + RX0 )

*  On each pass through this loop three lines may be drawn; A) a horizontal
*  line from the previous position (I-1), half way towards the current
*  position (I); B) a vertical line from the end of A) to the Y value of
*  the current position; C) a horizontal line from the end of B) to the
*  current position.

*  To draw C) the mid X position must be good and the current position must
*  have a good Y value.
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

*  If we cannot draw line A), pick up the pen.
            ELSE
               DOWN = .FALSE.
            END IF

*  See if the mid Y value is defined. In Mode 1, bad values are drawn at
*  the bottom Y value, but in Mode 6 they are not drawn at all.
            MIDY = MODE .EQ. 1 .OR. ( GOODY .AND. GOODY0 )

*  Draw line B) so long as the mid X position is known. In Mode 1, bad Y
*  values are considered to be coincident with the bottom axis. In
*  Mode 6, bad Y values cause line B) to be omitted.
            IF( MIDX .AND. MIDY ) THEN

*  If the pen is now down, put it down at the mid x position.
               IF( .NOT. DOWN ) THEN
                  CALL PGMOVE( RXC, RY0 )
                  DOWN = .TRUE.
               END IF

*  Draw line B.
               CALL PGDRAW( RXC, RY )

*  If we cannot draw line B), pick up the pen.
            ELSE
               DOWN = .FALSE.
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

*  If we cannot draw line C), pick up the pen.
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

*  Set PGPLOT attributes to match the plotting style used by the Plot for
*  drawing Curves. Save the current PGPLOT attribute values in ATTR.
         CALL KPG1_PGSTY( IPLOT, 'CURVES', .TRUE., ATTR, STATUS )

*  Indicate the pen is initially up.
         DOWN = .FALSE.

*  Loop round each X/Y position in the given range.
         DO I = ILO, IHI

*  If the pen is currently down...
            IF( DOWN ) THEN

*  If both X and Y are good at the current position, draw a line to the
*  current position.
               IF( X( I ) .NE. AST__BAD .AND.
     :             Y( I ) .NE. AST__BAD ) THEN
                  CALL PGDRAW( REAL( X( I ) ), REAL( Y( I ) ) )

*  If either X or Y are bad, pick the pen up.
               ELSE
                  DOWN = .FALSE.

               END IF

*  If the pen was originally up, and both X and Y are good at the current
*  position, put the pen down.
            ELSE IF( X( I ) .NE. AST__BAD .AND.
     :               Y( I ) .NE. AST__BAD ) THEN
               CALL PGMOVE( REAL( X( I ) ), REAL( Y( I ) ) )
               DOWN= .TRUE.

            END IF

         END DO

*  Re-instate the original PGPLOT attributes.
         CALL KPG1_PGSTY( IPLOT, 'CURVES', .FALSE., ATTR, STATUS )

*  Now deal with a marker placed at each point.
*  ============================================
      ELSE IF( MODE .EQ. 3 ) THEN

*  Set PGPLOT attributes to match the plotting style used by the Plot for
*  drawing markers. Save the current PGPLOT attribute values in ATTR.
         CALL KPG1_PGSTY( IPLOT, 'MARKERS', .TRUE., ATTR, STATUS )

*  Draw a marker at each good X/Y position in the given range.
         DO I = ILO, IHI
            IF( X( I ) .NE. AST__BAD .AND. Y( I ) .NE. AST__BAD ) THEN
               CALL PGPT( 1, REAL( X( I ) ), REAL( Y( I ) ), MTYPE )
            END IF
         END DO

*  Re-instate the original PGPLOT attributes.
         CALL KPG1_PGSTY( IPLOT, 'MARKERS', .FALSE., ATTR, STATUS )

*  Now deal with a horizontal line at each point.
*  ==============================================
      ELSE IF( MODE .EQ. 4 ) THEN

*  Set PGPLOT attributes to match the plotting style
*  used by the Plot for drawing curves. Save the current PGPLOT attribute
*  values in ATTR.
         CALL KPG1_PGSTY( IPLOT, 'CURVES', .TRUE., ATTR, STATUS )

*  Draw a line at each position which has good values for the upper and
*  lower X limits, and a good Y value, and is in the given range.
         DO I = ILO, IHI
            IF( XSTEP( I, 1 ) .NE. AST__BAD .AND.
     :          XSTEP( I, 2 ) .NE. AST__BAD .AND.
     :          Y( I ) .NE. AST__BAD ) THEN
               RY = REAL( Y( I ) )
               CALL PGMOVE( REAL( XSTEP( I, 1 ) ), RY )
               CALL PGDRAW( REAL( XSTEP( I, 2 ) ), RY )
            END IF
         END DO

*  Re-instate the original PGPLOT attributes.
         CALL KPG1_PGSTY( IPLOT, 'CURVES', .FALSE., ATTR, STATUS )

*  Now deal with a lines joining points with a marker at each point.
*  =================================================================
      ELSE IF( MODE .EQ. 5 ) THEN

*  First, draw the lines. Set PGPLOT attributes to match the plotting
*  style used by the Plot for drawing Curves. Save the current PGPLOT
*  attribute values in ATTR.
         CALL KPG1_PGSTY( IPLOT, 'CURVES', .TRUE., ATTR, STATUS )

*  Indicate the pen is initially up.
         DOWN = .FALSE.

*  Loop round each X/Y position in the given range.
         DO I = ILO, IHI

*  If the pen is currently down...
            IF( DOWN ) THEN

*  If both X and Y are good at the current position, draw a line to the
*  current position.
               IF( X( I ) .NE. AST__BAD .AND.
     :             Y( I ) .NE. AST__BAD ) THEN
                  CALL PGDRAW( REAL( X( I ) ), REAL( Y( I ) ) )

*  If either X or Y are bad, pick the pen up.
               ELSE
                  DOWN = .FALSE.

               END IF

*  If the pen was originally up, and both X and Y are good at the current
*  position, put the pen down.
            ELSE IF( X( I ) .NE. AST__BAD .AND.
     :               Y( I ) .NE. AST__BAD ) THEN
               CALL PGMOVE( REAL( X( I ) ), REAL( Y( I ) ) )
               DOWN = .TRUE.

            END IF

         END DO

*  Re-instate the original PGPLOT attributes.
         CALL KPG1_PGSTY( IPLOT, 'CURVES', .FALSE., ATTR, STATUS )

*  Now draw the markers. Set PGPLOT attributes to match the plotting style
*  used by the Plot for drawing markers. Save the current PGPLOT attribute
*  values in ATTR.
         CALL KPG1_PGSTY( IPLOT, 'MARKERS', .TRUE., ATTR, STATUS )

*  Draw a marker at each good X/Y position in the given range.
         DO I = ILO, IHI
            IF( X( I ) .NE. AST__BAD .AND. Y( I ) .NE. AST__BAD ) THEN
               CALL PGPT( 1, REAL( X( I ) ), REAL( Y( I ) ), MTYPE )
            END IF
         END DO

*  Re-instate the original PGPLOT attributes.
         CALL KPG1_PGSTY( IPLOT, 'MARKERS', .FALSE., ATTR, STATUS )

*  Report an error if the MODE value was illegal.
      ELSE IF( MODE .NE. 7 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'MODE', MODE )
         CALL ERR_REP( 'KPG1_PLTLN_ERR1', 'KPG1_PLTLN: Illegal MODE '//
     :                 'value (^MODE) supplied (programming error).',
     :                 STATUS )
      END IF

*  Tidy up.
*  ========

 999  CONTINUE

*  Ensure that any previous synonyms for AST attributes are cleared.
      CALL KPG1_ASPSY( ' ', ' ', STATUS )

*  Re-instate the original plotting attributes for the AST graphical elements
*  which may have been changed by this routine.
      CALL AST_SETI( IPLOT, 'COLOUR(AXES)', AXCOL0, STATUS )
      CALL AST_SETR( IPLOT, 'WIDTH(AXES)', AXWID0, STATUS )
      CALL AST_SETI( IPLOT, 'STYLE(AXES)', AXSTY0, STATUS )
      CALL AST_SETI( IPLOT, 'COLOUR(MARKERS)', MKCOL0, STATUS )
      CALL AST_SETR( IPLOT, 'WIDTH(MARKERS)', MKWID0, STATUS )
      CALL AST_SETI( IPLOT, 'FONT(MARKERS)', MKFNT0, STATUS )
      CALL AST_SETI( IPLOT, 'STYLE(MARKERS)', MKSTY0, STATUS )
      CALL AST_SETI( IPLOT, 'COLOUR(CURVES)', CVCOL0, STATUS )
      CALL AST_SETR( IPLOT, 'WIDTH(CURVES)', CVWID0, STATUS )
      CALL AST_SETI( IPLOT, 'STYLE(CURVES)', CVSTY0, STATUS )

*  End the PGPLOT buffering context.
      CALL PGEBUF

      END
