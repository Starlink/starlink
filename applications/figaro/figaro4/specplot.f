      SUBROUTINE SPECPLOT( STATUS )
*+
*  Name:
*     SPECPLOT

*  Purpose:
*     Plot a spectrum.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SPECPLOT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine plots a spectrum (or any one-dimensional NDF section)
*     in the current (AGI) picture of the graphics device.
*
*     The plot can basically be an overlay over the most recent data
*     picture inside the current picture, or a new plot inside the
*     current picture. (The current picture after SPECPLOT is the
*     same as before.)
*
*     The screen contents of the current picture can be erased or not.
*
*     The plot location and size is governed by the outer and the
*     inner box. The inner box is the area where data are plotted,
*     the outer box contains the inner box and the plot labels.
*
*     In the overlay case the inner box and its world coordinates are
*     identified with the most recent data picture inside the current
*     picture. No labelling is done in the overlay case, so the outer
*     box has no meaning in this case.
*
*     In the case of a new plot, the outer box will be identified
*     with the current picture, although the plot labels are allowed
*     to extend beyond this area. Depending on the choice of
*     labelling, a sensible location for the inner box is offered.
*     After the inner box is specified, its world coordinates are
*     enquired. The prompt values correspond to the extreme values
*     found in the data. The location and world coordinates of the inner
*     box are saved as a data picture in the AGI data base.
*
*     The labelling consists of axes, axis ticks, numeric labels at
*     the major ticks, and text labels. The axes are counted from
*     bottom clockwise. Each axis can be drawn or not. Each
*     drawn axis can have ticks or not. Each axis can have numeric
*     labels or not. The left and right axes can have either
*     horizontal (orthogonal) or vertical (parallel) numeric labels.
*     Each axis can have a text label or not.
*
*     The kind of labelling is controlled by several 4-character
*     strings. Each character is the switch for axis 1, 2, 3, 4
*     respectively. "0" turns an option off for that axis, "+" turns
*     it on. For the ticks and for numeric labels of axes 2 and 4,
*     "-" is also allowed. It yields inward ticks and vertical
*     numeric labels.
*
*     The data can be plotted as a set of markers, as a line-style
*     polygon connecting the data points, or as a bin-style polygon.
*     In addition error bars or pixel width bars can be plotted. Each
*     of the options can be selected independent of the others, i.e.
*     several (or all) options can be selected at the same time. If
*     no variance information is available, error bars are de-selected
*     automatically. Bad data are omitted from the plot. If error
*     bars are selected, bad variances cause the corresponding data
*     also to be omitted.
*
*     The attributes of the plot can be selected. These are
*     -  colour
*     -  line thickness
*     -  character height (equivalent to marker size)
*     -  simple or roman font
*     -  dash pattern for polygon connections
*
*     Most parameters default to the last used value.

*  Usage:
*     specplot in overlay=? bottom=? left=? top=? right=?
*        labspc=? world=?

*  ADAM Parameters:
*     INFO = _LOGICAL (Read)
*        If false, the routine will issue only error messages and no
*        informational messages. [TRUE]
*     CLEAR = _LOGICAL (Read)
*        If true, the part of the graphics device corresponding to the
*        current (AGI) picture is erased before the plot is drawn.
*        [FALSE]
*     OVERLAY = _LOGICAL (Read)
*        If true, the plot will be an overlay on the most recent (AGI)
*        data picture within the current (AGI) picture.
*        If false, the plot will be user-defined, but the inner box is
*        restricted to the current (AGI) picture.
*     IN = NDF (Read)
*        The input NDF.
*     LIN = _LOGICAL (Read)
*        If true, the data points will be connected by a line-style
*        polygon. [TRUE]
*     BIN = _LOGICAL (Read)
*        If true, the data points will be connected by a bin-style (or
*        histogram-style) polygon. [FALSE]
*     MARK = _INTEGER (Read)
*        This parameter determines the kind of marker to be drawn at
*        each data point [0]:
*        -  0: No markers drawn,
*        -  1: Diagonal cross,
*        -  2: Asterisk,
*        -  3: Open circle,
*        -  4: Open square,
*        -  5: Filled circle,
*        -  6: Filled square.
*     ERROR = _LOGICAL (Read)
*        If true and variance information available, error bars will be
*        drawn. [FALSE]
*     WIDTH = _LOGICAL (Read)
*        If true, the pixel width will be indicated by horizontal bars.
*        [FALSE]
*     ROMAN = _LOGICAL (Read)
*        If true, PGPLOT's roman font is used for drawing text. If
*        false, the normal (single-stroke) font is used. [FALSE]
*     HEIGHT = _REAL (Read)
*        The height of the characters. This also affects the size of the
*        markers. Markers are about half the size of characters. The
*        height is measured in units of PGPLOT default text heights,
*        which is approximately 1/40 of the height of the (AGI) base
*        picture (i.e. 1/40 the height of the workstation window, screen
*        or paper). [1.]
*     COLOUR = _INTEGER (Read)
*        The PGPLOT colour index to be used for the plot. This can be
*        formally between 0 and 255, but not all devices support all
*        colours. The default colour representation is:
*        -  0: Background,           -  1: Foreground (default),
*        -  2: Red,                  -  3: Green,
*        -  4: Blue,                 -  5: Cyan,
*        -  6: Magenta,              -  7: Yellow,
*        -  8: Orange,               -  9: Green/Yellow,
*        - 10: Green/Cyan,           - 11: Blue/Cyan,
*        - 12: Blue/Magenta,         - 13: Red/Magenta,
*        - 14: Dark grey,            - 15: Light grey.
*     THICK = _INTEGER (Read)
*        The PGPLOT line thickness. Can be between 1 and 21. [1]
*     DASH = _INTEGER (Read)
*        The PGPLOT dash pattern [1]:
*        -  1: Full line,
*        -  2: Long dash,
*        -  3: Dash-dot-dash-dot,
*        -  4: Dotted,
*        -  5: Dash-dot-dot-dot.
*     AXES = _CHAR (Read)
*        Array of switches to turn on or off the drawing of either of
*        the four box sides. The sides are counted from bottom
*        clockwise: bottom, left, top, right. Any switch can be
*        "0" or "+". E.g. '00++' would switch off the bottom and left
*        axes and switch on the top and right axes. ['++++']
*     TICK = _CHAR (Read)
*        Array of switches to turn on or off the drawing of ticks along
*        either axis. Ticks are drawn only if the corresponding axis is
*        also drawn. The sides are counted from bottom
*        clockwise: bottom, left, top, right. Any switch can be
*        "0", "+" or "-". E.g. '00+-' would switch off the bottom and
*        left ticks and switch on the top and right ticks. The top axis
*        would have ticks outward, the right axis would have ticks
*        inward. ['----']
*     NUML = _CHAR (Read)
*        Array of switches to turn on or off the drawing of numeric
*        labels along either axis. The sides are counted from bottom
*        clockwise: bottom, left, top, right. Any switch can be
*        "0" or "+"; the second and fourth switch can also be "-". E.g.
*        '0+0-' would switch off the bottom and top labels and switch
*        on the left and right labels. The left axis would have labels
*        horizontal (orthogonal), the right axis would have labels
*        vertical (parallel). ['++00']
*     TEXT = _CHAR (Read)
*        Array of switches to turn on or off the drawing of text labels
*        along either axis. The sides are counted from bottom
*        clockwise: bottom, left, top, right. Any switch can be
*        "0" or "+". E.g. '0++0' would switch off the bottom and right
*        labels and switch on the left and top labels. ['+++0']
*     NORTHO = _REAL (Read)
*        If orthogonal numeric labels have been selected, you must
*        specify how much space there must be between the
*        axis and the text label, i.e. how long the longest numeric
*        label along the left or right axis will be. The unit is character
*        heights. [1]
*     MAJOR( 2 ) = _REAL (Read)
*        The distance in world coordinates between major tick marks. The
*        first element is for the horizontal direction, the second for
*        the vertical direction. This is also the distance along the
*        axis between numeric labels. Values of 0 cause PGPLOT to choose
*        the major tick interval automatically. [0.,0.]
*     MINOR( 2 ) = _INTEGER (Read)
*        The number of minor tick intervals per major tick interval. The
*        first element is for the horizontal direction, the second for
*        the vertical direction. Values of 0 for MINOR or MAJOR cause
*        PGPLOT to choose the minor tick interval automatically. [0,0]
*     BOTTOM = _CHAR (Read)
*        The text label for the first axis. Within the string, you can
*        use the following escape sequences:
*        - \fn Normal (single stroke) font,
*        - \fr Roman font,
*        - \fi Italic font,
*        - \fs Script font,
*        - \u  Superscript (use only paired with \d),
*        - \d  Subscript (use only paired with \u),
*        - \b  Backspace,
*        - \\  Backslash,
*        - \A  Danish umlaut (Angstroem),
*        - \g  Any greek letter.
*     LEFT = _CHAR (Read)
*        The text label for the second axis. Within the string, you can
*        use the following escape sequences:
*        - \fn Normal (single stroke) font,
*        - \fr Roman font,
*        - \fi Italic font,
*        - \fs Script font,
*        - \u  Superscript (use only paired with \d),
*        - \d  Subscript (use only paired with \u),
*        - \b  Backspace,
*        - \\  Backslash,
*        - \A  Danish umlaut (Angstroem),
*        - \g  Any greek letter.
*     TOP = _CHAR (Read)
*        The text label for the third axis. Within the string, you can
*        use the following escape sequences:
*        - \fn Normal (single stroke) font,
*        - \fr Roman font,
*        - \fi Italic font,
*        - \fs Script font,
*        - \u  Superscript (use only paired with \d),
*        - \d  Subscript (use only paired with \u),
*        - \b  Backspace,
*        - \\  Backslash,
*        - \A  Danish umlaut (Angstroem),
*        - \g  Any greek letter.
*     RIGHT = _CHAR (Read)
*        The text label for the fourth axis. Within the string, you can
*        use the following escape sequences:
*        - \fn Normal (single stroke) font,
*        - \fr Roman font,
*        - \fi Italic font,
*        - \fs Script font,
*        - \u  Superscript (use only paired with \d),
*        - \d  Subscript (use only paired with \u),
*        - \b  Backspace,
*        - \\  Backslash,
*        - \A  Danish umlaut (Angstroem),
*        - \g  Any greek letter.
*     DEVICE = DEVICE (Read)
*        The graphics device for the plot.
*     LABSPC( 4 ) = _REAL (Read)
*        The space between outer box (AGI current picture) and inner box
*        measured in units of character heights. The four numbers are
*        for the bottom, left, top, right labelling space in that order.
*        The dynamic default offered is based on the space requirements
*        for the axis labelling, and can in general be accepted.
*     WORLD( 4 ) = _REAL (Read)
*        The world coordinates that the left, right, bottom and top ends
*        of the inner box should correspond to.
*        The dynamic default is based on the coordinates of the first
*        and last pixel of the selected subset and on the extreme data
*        values of the selected subset. Reverse axes can be achieved by
*        giving WORLD(1) > WORLD(2) and/or WORLD(3) > WORLD(4).

*  Examples:
*     specplot spectrum accept
*        This is the simplest way to plot a 1-D data set in its full
*        length.
*     specplot imagerow(-100.:50.,15.) accept
*        This will take a 2-D data set IMAGEROW and plot part of the
*        row specified by the second coordinate being 15. The part of
*        the row plotted corresponds to the first coordinate being
*        between -100 and +50. Note that the decimal point forces use of
*        axis data. Omitting the period would force use of pixel
*        numbers.
*     specplot imagecol(15.,-100.:50.) accept
*        This will take a 2-D data set IMAGEROW and plot part of the
*        column specified by the first coordinate being 15. The part of
*        the row plotted corresponds to the second coordinate being
*        between -100 and +50. Note that the decimal point forces use of
*        axis data. Omitting the period would force use of pixel
*        numbers.
*     specplot spectrum lin=false bin=true accept
*        Replace direct connections between data points by bin-style
*        connections.
*     specplot spectrum mark=1 accept
*        Mark each data point by a diagonal cross.
*     specplot spectrum error=true width=true accept
*        Draw an error bar and a pixel width bar for each data point.
*     specplot spectrum roman=true height=1.5 colour=3 accept
*        Draw text with the roman font, draw text and makers 1.5 times
*        their normal size, and plot the whole thing in green colour.
*     specplot spectrum bottom=Weekday left="Traffic noise [dBA]" accept
*        Specify text labels on the command line instead of constructing
*        them from the file's axis and data info.
*     specplot spectrum overlay=true clear=false accept
*        The position and scale of the plot are determined by the
*        previous plot (which might have been produced by a different
*        application).
*     specplot spectrum world=[0.,1.,-1.,1.] accept
*        Use plot limits different from the extreme data values.

*  Notes:
*     This routine recognises the Specdre Extension v. 0.7.
*
*     This routine recognises and uses coordinate transformations in
*     AGI pictures.

*  Algorithm:
*     Get modal parameters INFO, CLEAR, OVERLAY.
*     Get layout parameters LIN, BIN, MARK, ERROR, WIDTH.
*     Access input, update ERROR switch.
*     Access given arrays plus label/unit strings.
*     Get workspaces and values for error bars, witdth bars.
*     Release error and width arrays.
*     Get attributes ROMAN, HEIGHT, COLOUR, THICK, DASH.
*     If not an overlay,
*        Get box layout AXES, TICK, NUML, TEXT, NORTHO, MAJOR, MINOR.
*        Get plot labels and work out how much space these need between
*        outer and inner box.
*     If overlay,
*        Open last AGI DATA picture as borderless PGPLOT view port.
*        Get workspaces for transformed x and y.
*        Transform all arrays.
*        Release original x and y.
*     Else (not an overlay),
*        Open current AGI picture as borderless PGPLOT view port.
*        Get user confirmation for space between inner and outer box.
*        Work out the inner box and change the PGPLOT view port.
*        Work out the PGPLOT window (parameter WORLD).
*     Do the actual plotting.
*     If not an overlay,
*        Save the view port as AGI DATA picture; its world coordinates
*        are 0...1 in each direction; the PGPLOT window used is stored
*        as a transformation with the picture.
*     Close down

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17 Sep 1991 (hme):
*        Original version.
*     21 Sep 1991 (hme):
*        Use DSA reference name different from parameter name. Otherwise
*        if the parameter has a global association, that structure in
*        ADAM_USER:GLOBAL is destroyed by DSA.
*     23 Sep 1991 (hme):
*        Avoid AGP_NVIEW to crash with world zero extent viewport. Avoid
*        divide-by-zero if current picture on entry has zero extent on
*        the screen/paper. Specify inner box position by labelling
*        space. SPPDO: Do not call PGBIN or PGLINE when only one point
*        to be plotted.
*     26 Nov 1991 (hme):
*        Bug fixing. Change reporting.
*     15 Dec 1991 (hme):
*        Suppress Starlink error messages arising from DSA calls.
*     14 Jul 1992 (hme):
*        Port to NDF and Unix.
*        Check status after AGI_ASSOC.
*     04 Jun 1993 (hme):
*        Major overhaul. The whole graphics access is new, using
*        AGP_ASSOC. No SGS any more. Support transformations. Support
*        reverse axes.
*     25 Jun 1993 (hme):
*        No longer include AGI_PAR and PAR_ERR. Use standard PGPLOT
*        calls (shorter names).
*     25 Nov 1994 (hme):
*        Use new libraries.
*     2005 June 1 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
*  NDF(1): IN
*  NDF(2): SPECVALS
*  NDF(3): SPECWIDS
*  NDF(4): Error bar workspace
*  NDF(5): Width bar workspace
*  NDF(6): X, Y work space
*  PNTR(1): Data (possibly copy of ZPTR(5))
*  PNTR(2): Errors
*  PNTR(3): Widths
*  PNTR(4): Centres (possibly copy of ZPTR(6))
*  ZPTR(1): Error bar bottom ends
*  ZPTR(2): Error bar top ends
*  ZPTR(3): Width bar left end
*  ZPTR(4): Width bar right end
*  ZPTR(5): Transformed data
*  ZPTR(6): Transformed centres
      LOGICAL INFO
      LOGICAL CLEAR              ! True if graphics device to be cleared
      LOGICAL OVER               ! True if overlay to data picture
      LOGICAL ERROR              ! True if errors to be plotted
      LOGICAL WIDTH              ! True if widths to be plotted
      LOGICAL LIN                ! True if line style connections
      LOGICAL BIN                ! True if bin style connections
      LOGICAL ROMAN              ! True for roman font
      INTEGER COLOUR             ! PGPLOT colour number
      INTEGER DASH               ! PGPLOT dash number
      INTEGER THICK              ! PGPLOT line thickness
      INTEGER MARK               ! Marker type
      INTEGER MINOR( 2 )         ! No. minor tick intervals per major
      REAL MAJOR( 2 )            ! Major tick intervals
      REAL CHIGHT                ! Character height in 1/40 view surface
      REAL NORTHO                ! Expexcted no. of chars in ortho label
      CHARACTER * ( 4 ) AXES     ! Switches to plot axes
      CHARACTER * ( 4 ) TICK     ! Switches to plot ticks in or out
      CHARACTER * ( 4 ) NUML     ! Switches to plot numeric labels
      CHARACTER * ( 4 ) TEXT     ! Switches to plot text labels
      CHARACTER * ( 64 ) XLABEL  ! Axis label
      CHARACTER * ( 64 ) XUNITS  ! Axis unit
      CHARACTER * ( 64 ) DLABEL  ! Data label
      CHARACTER * ( 64 ) DUNITS  ! Data unit
      CHARACTER * ( 64 ) LABEL( 4 ) ! Plot labels
      LOGICAL XTHERE             ! True if Extension exists
      INTEGER I                  ! Loop index
      INTEGER IERR               ! Returned by VEC routines
      INTEGER NERR               ! Returned by VEC routines
      INTEGER ACTVAL             ! No. of vector elements returned
      INTEGER PLACE              ! NDF placeholder
      INTEGER NDF( 6 )           ! NDF identifiers
      INTEGER PNTR( 4 )          ! Pointers to arrays
      INTEGER ZPTR( 6 )          ! Pointers to workspaces
      INTEGER NDIM               ! NDF dimensionality
      INTEGER ACTDIM             ! Non-degenerate dimensionality
      INTEGER NELM               ! NDF size
      INTEGER DIM( NDF__MXDIM )  ! NDF dimensions
      INTEGER AXIS               ! Number of the non-degenerate axis
      INTEGER PICCUR             ! Current picture on entry
      INTEGER PICDAT             ! Data picture
      INTEGER PGMARK( 6 )        ! PGPLOT marker numbers
      REAL CURR( 4 )             ! Current view port in NDC
      REAL BASE( 4 )             ! Base picture in pixels
      REAL DATA( 4 )             ! Data view port in NDC
      REAL WIND( 4 )             ! PGPLOT window
      REAL XYSCAL                ! Base picture aspect ratio
      REAL SPACE( 4 )            ! Spaces between inner and outer box
      CHARACTER * ( 6 ) AXNAME( 4 ) ! Parameter names for text labels
      CHARACTER * ( DAT__SZLOC ) XLOC ! Locator to Extension
      CHARACTER * ( 6 ) BOXOPT   ! Option string for PGBOX
      CHARACTER * ( DAT__SZMOD ) GRACC ! Graphics access mode
      CHARACTER * ( 80 ) FORW( 2 ) ! Forward coordinate transform
      CHARACTER * ( 80 ) INVS( 2 ) ! Inverse coordinate transform

*  Internal References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Data:
      DATA PGMARK / 5, 3, 4, 0, 17, 16 /
      DATA AXNAME / 'BOTTOM', 'LEFT', 'TOP', 'RIGHT' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Startup.
      CALL NDF_BEGIN

*  Modal parameters.
      CALL PAR_GET0L( 'INFO', INFO, STATUS )
      CALL PAR_GET0L( 'CLEAR',  CLEAR,  STATUS )
      CALL PAR_GET0L( 'OVERLAY', OVER, STATUS )
      IF ( CLEAR ) THEN
         GRACC = 'WRITE'
      ELSE
         GRACC = 'READ'
      END IF

*  Get the data layout.
      CALL PAR_GET0L( 'LIN', LIN, STATUS )
      CALL PAR_GET0L( 'BIN', BIN, STATUS )
      CALL PAR_GET0I( 'MARK', MARK, STATUS )
      CALL PAR_GET0L( 'ERROR', ERROR, STATUS )
      CALL PAR_GET0L( 'WIDTH', WIDTH, STATUS )

*  Access input.
*  Check that it is actually 1-D, find out which is the axis.
*  Update the error keyword, error bars are plotted if and only if
*  requested and variance component present.
      CALL NDF_ASSOC( 'IN', 'READ', NDF(1), STATUS )
      CALL NDF_DIM( NDF(1), NDF__MXDIM, DIM, NDIM, STATUS )
      ACTDIM = 0
      DO 1 I = 1, NDF__MXDIM
         IF ( DIM(I) .GT. 1 ) THEN
            AXIS = I
            ACTDIM = ACTDIM + 1
         END IF
 1    CONTINUE
      IF ( ACTDIM .NE. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPECPLOT_E01',
     :      'SPECPLOT: Error: Input NDF is not one-dimensional.',
     :      STATUS )
         GO TO 500
      END IF
      IF ( ERROR ) CALL NDF_STATE( NDF(1), 'VARIANCE', ERROR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Access centres, widths, data, errors.
*  This includes getting labels and units.
      XLABEL = ' '
      XUNITS = ' '
      DLABEL = ' '
      DUNITS = ' '
      CALL SPD_EAAA( NDF(1), 'READ', XTHERE, XLOC, STATUS )
      CALL SPD_EAEA( NDF(1), XLOC, AXIS, 'READ', '_REAL',
     :   XLABEL, XUNITS, PNTR(4), NDF(2), NELM, STATUS )
      IF ( WIDTH ) CALL SPD_EAFA( NDF(1), XLOC, AXIS, 'READ', '_REAL',
     :   PNTR(3), NDF(3), NELM, STATUS )
      CALL NDF_CGET( NDF(1), 'LABEL', DLABEL, STATUS )
      CALL NDF_CGET( NDF(1), 'UNITS', DUNITS, STATUS )
      CALL NDF_MAP( NDF(1), 'DATA', '_REAL', 'READ',
     :   PNTR(1), NELM, STATUS )
      IF ( ERROR ) CALL NDF_MAP( NDF(1), 'ERROR', '_REAL', 'READ',
     :   PNTR(2), NELM, STATUS )

*  Get workspace and values for error bar ends. Release error array.
      IF ( ERROR ) THEN
         CALL NDF_TEMP( PLACE, STATUS )
         CALL NDF_NEW( '_REAL', 1, 1, NELM, PLACE, NDF(4), STATUS )
         CALL NDF_MAP( NDF(4), 'DATA,VARIANCE', '_REAL', 'WRITE',
     :                 ZPTR(1), NELM, STATUS )
         CALL VEC_SUBR( .TRUE., NELM, %VAL( CNF_PVAL(PNTR(1)) ),
     :                  %VAL( CNF_PVAL(PNTR(2)) ),
     :                  %VAL( CNF_PVAL(ZPTR(1)) ), IERR, NERR, STATUS )
         CALL VEC_ADDR( .TRUE., NELM, %VAL( CNF_PVAL(PNTR(1)) ),
     :                  %VAL( CNF_PVAL(PNTR(2)) ),
     :                  %VAL( CNF_PVAL(ZPTR(2)) ), IERR, NERR, STATUS )
         CALL NDF_UNMAP( NDF(1), 'VARIANCE', STATUS )
      END IF

*  Get workspace and values for width bar ends. Release error array.
      IF ( WIDTH ) THEN
         CALL NDF_TEMP( PLACE, STATUS )
         CALL NDF_NEW( '_REAL', 1, 1, NELM, PLACE, NDF(5), STATUS )
         CALL NDF_MAP( NDF(5), 'DATA,VARIANCE', '_REAL', 'WRITE',
     :                 ZPTR(3), NELM, STATUS )
         CALL SPD_UAARR( .TRUE., NELM, 0.5, %VAL( CNF_PVAL(PNTR(3)) ),
     :                   %VAL( CNF_PVAL(ZPTR(4)) ), IERR, NERR, STATUS )
         CALL VEC_SUBR( .TRUE., NELM, %VAL( CNF_PVAL(PNTR(4)) ),
     :                  %VAL( CNF_PVAL(ZPTR(4)) ),
     :                  %VAL( CNF_PVAL(ZPTR(3)) ), IERR, NERR, STATUS )
         CALL VEC_ADDR( .TRUE., NELM, %VAL( CNF_PVAL(ZPTR(4)) ),
     :                  %VAL( CNF_PVAL(PNTR(4)) ),
     :                  %VAL( CNF_PVAL(ZPTR(4)) ), IERR, NERR, STATUS )
         IF ( NDF(3) .EQ. NDF__NOID ) THEN
            CALL NDF_AUNMP( NDF(1), 'WIDTH', AXIS, STATUS )
         ELSE
            CALL NDF_ANNUL( NDF(3), STATUS )
         END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Get attributes.
      IF ( .NOT. OVER ) CALL PAR_GET0L( 'ROMAN',  ROMAN,  STATUS )
      CALL PAR_GET0R( 'HEIGHT', CHIGHT, STATUS )
      CALL PAR_GET0I( 'COLOUR', COLOUR, STATUS )
      CALL PAR_GET0I( 'THICK',  THICK,  STATUS )
      CALL PAR_GET0I( 'DASH',   DASH,   STATUS )

*  Box layout and labels.
      IF ( .NOT. OVER ) THEN

*     Get the box layout.
         CONTINUE

*        The layout switches for each axis.
            CALL PAR_GET0C( 'AXES', AXES, STATUS )
            CALL PAR_GET0C( 'TICK', TICK, STATUS )
            CALL PAR_GET0C( 'NUML', NUML, STATUS )
            CALL PAR_GET0C( 'TEXT', TEXT, STATUS )

*        Check each element of each switch array.
            DO 2 I = 1, 4
               IF ( ( AXES(I:I) .NE. '0' .AND.
     :                AXES(I:I) .NE. '+' ) ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'INT', I )
                  CALL ERR_REP( 'SPECPLOT_E02',
     :               'SPECPLOT: Invalid AXES(^INT).', STATUS )
                  GO TO 500
               ELSE IF ( ( TICK(I:I) .NE. '0' .AND.
     :                     TICK(I:I) .NE. '+' .AND.
     :                     TICK(I:I) .NE. '-' ) ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'INT', I )
                  CALL ERR_REP( 'SPECPLOT_E03',
     :               'SPECPLOT: Invalid TICK(^INT).', STATUS )
                  GO TO 500
               ELSE IF ( ( NUML(I:I) .NE. '0' .AND.
     :                     NUML(I:I) .NE. '+' .AND.
     :                     NUML(I:I) .NE. '-' ) ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'INT', I )
                  CALL ERR_REP( 'SPECPLOT_E04',
     :               'SPECPLOT: Invalid NUML(^INT).', STATUS )
                  GO TO 500
               ELSE IF ( ( TEXT(I:I) .NE. '0' .AND.
     :                     TEXT(I:I) .NE. '+' ) ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'INT', I )
                  CALL ERR_REP( 'SPECPLOT_E05',
     :               'SPECPLOT: Invalid TEXT(^INT).', STATUS )
                  GO TO 500
               END IF
 2          CONTINUE

*        Complete the check for NUML(1:1) and NUML(3:3).
            IF ( NUML(1:1) .EQ. '-' ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'INT', 1 )
               CALL ERR_REP( 'SPECPLOT_E06',
     :            'SPECPLOT: Invalid NUML(^INT).', STATUS )
               GO TO 500
            ELSE IF ( NUML(3:3) .EQ. '-' ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'INT', 3 )
               CALL ERR_REP( 'SPECPLOT_E07',
     :            'SPECPLOT: Invalid NUML(^INT).', STATUS )
               GO TO 500
            END IF

*        If horizontal left or right (i.e. orthogonal) numeric label
*        requested, ask how long these might be.
            IF ( NUML(2:2) .EQ. '+' .OR. NUML(4:4) .EQ. '+' )
     :         CALL PAR_GET0R( 'NORTHO', NORTHO, STATUS )

*        If ticks requested, ask for distance between major ticks and
*        number of minor intervals per major interval. (Must be given for
*        both directions, even if ticks are requested for only one
*        direction.)
            IF ( TICK .NE. '0000' ) THEN
               CALL PAR_GET1R( 'MAJOR', 2, MAJOR, ACTVAL, STATUS )
               IF ( ACTVAL .NE. 2 ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'SPECPLOT_E08',
     :               'SPECPLOT: 2 values needed for MAJOR.', STATUS )
                  GO TO 500
               END IF
               CALL PAR_GET1I( 'MINOR', 2, MINOR, ACTVAL, STATUS )
               IF ( ACTVAL .NE. 2 ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'SPECPLOT_E09',
     :               'SPECPLOT: 2 values needed for MINOR.', STATUS )
                  GO TO 500
               END IF
            END IF

*     Get the text label prompt values from data.
         LABEL(1) = XLABEL
         IF ( XUNITS .NE. ' ' )
     :      LABEL(1) = XLABEL(:CHR_LEN(XLABEL)) //
     :         ' [' // XUNITS(:CHR_LEN(XUNITS)) // ']'
         LABEL(2) = DLABEL
         IF ( DUNITS .NE. ' ' )
     :      LABEL(2) = DLABEL(:CHR_LEN(DLABEL)) //
     :         ' [' // DUNITS(:CHR_LEN(DUNITS)) // ']'
         LABEL(3) = ' '
         CALL NDF_CGET( NDF(1), 'TITLE', LABEL(3), STATUS )
         LABEL(4) = LABEL(2)

*     Get ultimate labels and derive space necessary between inner and
*     outer box.
         DO 3 I = 1, 4

*        Text labels.
            IF ( TEXT(I:I) .EQ. '+' ) THEN
               CALL PAR_DEF0C( AXNAME(I), LABEL(I), STATUS )
               CALL PAR_GET0C( AXNAME(I), LABEL(I), STATUS )
            END IF

*        Need a blank space at least.
            SPACE(I) = CHIGHT

*        Need one character height for numeric label.
            IF ( NUML(I:I) .EQ. '+' )
     :         SPACE(I) = SPACE(I) + CHIGHT

*        If it is orthogonal, need more depending on length of label.
            IF ( NUML(I:I) .EQ. '+' .AND. (I.EQ.2 .OR. I.EQ.4) )
     :         SPACE(I) = SPACE(I) + (NORTHO-.5) * CHIGHT

*        If it is not orthogonal, need onle one character height.
            IF ( NUML(I:I) .EQ. '-' .AND. (I.EQ.2 .OR. I.EQ.4) )
     :         SPACE(I) = SPACE(I) + CHIGHT

*        Need one character height and a blank space for a text label.
            IF ( TEXT(I:I) .EQ. '+' )
     :         SPACE(I) = SPACE(I) + CHIGHT + CHIGHT
 3       CONTINUE
      END IF

*  Pre-plot processing for overlay.
      IF ( OVER ) THEN

*     We get hold of the most recent DATA picture and open it as a
*     PGPLOT view port without border.
*     Presumably the view surface is the base picture.
         CALL AGP_ASSOC( 'DEVICE', GRACC, 'DATA', .FALSE.,
     :      PICDAT, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 500

*     Since this is an overlay, we just use this view port. The PGPLOT
*     window will already be the picture world coordinates.
*     But there will in general be a transformation stored with the
*     picture. All our arrays must be transformed from file
*     coordinates (data coordinates) to window coordinates (world
*     coordinates). For the x and y arrays, we must also get workspaces,
*     because so far we have the original mapped read-only.
*     We happily swap pointers so that no one will know any
*     more which array is in which temporary NDF. But the ZPTR and
*     PNTR still point to the same (transformed) information.
         CONTINUE

*        Get additional workspaces.
            CALL NDF_TEMP( PLACE, STATUS )
            CALL NDF_NEW( '_REAL', 1, 1, NELM, PLACE, NDF(6), STATUS )
            CALL NDF_MAP( NDF(6), 'DATA,VARIANCE', '_REAL', 'WRITE',
     :         ZPTR(5), NELM, STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 500

*        Transform error bar bottoms and tops.
            IF ( ERROR ) THEN
               CALL AGI_TDTOW( PICDAT, NELM, %VAL( CNF_PVAL(PNTR(4)) ),
     :            %VAL( CNF_PVAL(ZPTR(1)) ), %VAL( CNF_PVAL(ZPTR(6)) ),
     :            %VAL( CNF_PVAL(ZPTR(5)) ), STATUS )
               I       = ZPTR(1)
               ZPTR(1) = ZPTR(5)
               ZPTR(5) = I
               CALL AGI_TDTOW( PICDAT, NELM, %VAL( CNF_PVAL(PNTR(4)) ),
     :                         %VAL( CNF_PVAL(ZPTR(2)) ),
     :                         %VAL( CNF_PVAL(ZPTR(6)) ),
     :                         %VAL( CNF_PVAL(ZPTR(5)) ), STATUS )
               I       = ZPTR(2)
               ZPTR(2) = ZPTR(5)
               ZPTR(5) = I
            END IF

*        Transform width bar ends.
            IF ( WIDTH ) THEN
               CALL AGI_TDTOW( PICDAT, NELM, %VAL( CNF_PVAL(ZPTR(3)) ),
     :                         %VAL( CNF_PVAL(PNTR(1)) ),
     :                         %VAL( CNF_PVAL(ZPTR(6)) ),
     :                         %VAL( CNF_PVAL(ZPTR(5)) ), STATUS )
               I       = ZPTR(3)
               ZPTR(3) = ZPTR(6)
               ZPTR(6) = I
               CALL AGI_TDTOW( PICDAT, NELM, %VAL( CNF_PVAL(ZPTR(4)) ),
     :                         %VAL( CNF_PVAL(PNTR(1)) ),
     :                         %VAL( CNF_PVAL(ZPTR(6)) ),
     :                         %VAL( CNF_PVAL(ZPTR(5)) ), STATUS )
               I       = ZPTR(4)
               ZPTR(4) = ZPTR(6)
               ZPTR(6) = I
            END IF

*        Transform data points themselves.
            CALL AGI_TDTOW( PICDAT, NELM, %VAL( CNF_PVAL(PNTR(4)) ),
     :                      %VAL( CNF_PVAL(PNTR(1)) ),
     :                      %VAL( CNF_PVAL(ZPTR(6)) ),
     :                      %VAL( CNF_PVAL(ZPTR(5)) ), STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 500

*        Release original data.
            CALL NDF_UNMAP( NDF(1), 'DATA', STATUS )
            IF ( NDF(2) .EQ. NDF__NOID ) THEN
               CALL NDF_AUNMP( NDF(1), 'CENTRE', AXIS, STATUS )
            ELSE
               CALL NDF_ANNUL( NDF(2), STATUS )
            END IF
            IF ( STATUS .NE. SAI__OK ) GO TO 500

*        Update pointers.
            PNTR(4) = ZPTR(6)
            PNTR(1) = ZPTR(5)

*  Pre-plot processing for non-overlay.
      ELSE

*     We get hold of the current picture and open it as a PGPLOT view
*     port - without border.
*     Presumably the view surface is the base picture.
         CALL AGP_ASSOC( 'DEVICE', GRACC, ' ', .FALSE., PICCUR, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 500

*     Since this is not an overlay, the current picture is to be the
*     outer box and we create an inner box (ultimate view port) inside
*     it. Betwwen outer and inner box is the labelling. And above we
*     have worked out a default spacing necessary between boxes. Here we
*     will ask the user for confirmation of that spacing and then get
*     that ultimate view port
         CONTINUE

*        Get confirmation for space between outer and inner box from
*        parameter system.
            CALL PAR_DEF1R( 'LABSPC', 4, SPACE, STATUS )
            CALL PAR_GET1R( 'LABSPC', 4, SPACE, ACTVAL, STATUS )
            IF ( ACTVAL .NE. 4 ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'SPECPLOT_E10',
     :            'SPECPLOT: 4 values needed for LABSPC.',
     :            STATUS )
               GO TO 500
            END IF

*        So we want a new view port inside the one we have now. We want
*        to go inside by SPACE * 1/40 the height of the base picture.
*        Now then. The view surface (base picture) goes from 0. to 1. in
*        either direction as measured in PGPLOT normalised device
*        coordinates. We first find out where in those coordinates our
*        outer box (current picture, current view port) is.
            CALL PGQVP( 0, CURR(1), CURR(2), CURR(3), CURR(4) )

*        Vertically we want to go inside by SPACE/40 in those units.
*        That's easy. Horizontally one such unit corresponds to more or
*        less pixels, it corresponds to the horizontal size of the base
*        picture. To find out the size ratio in pixels of the base
*        picture, we get the base picture (view surface) as the new
*        viewport and find out its coordinates in pixels.
            CALL PGSVP( 0., 1., 0., 1. )
            CALL PGQVP( 3, BASE(1), BASE(2), BASE(3), BASE(4) )

*        Check the viewport is finite.
            IF ( BASE(2) .LE. BASE(1) .OR. BASE(4) .LE. BASE(3) ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'SPECPLOT_E11', 'SPECPLOT: Error ' //
     :            'enquiring display size.', STATUS )
               GO TO 500
            END IF

*        Horizontally we want to go inside by SPACE/40 times XYSCAL.
            XYSCAL = ( BASE(4) - BASE(3) ) / ( BASE(2) - BASE(1) )

*        So the final viewport is this.
            DATA(1) = CURR(1) + XYSCAL * SPACE(2) / 40.
            DATA(2) = CURR(2) - XYSCAL * SPACE(4) / 40.
            DATA(3) = CURR(3) + SPACE(1) / 40.
            DATA(4) = CURR(4) - SPACE(3) / 40.
            IF ( DATA(2) .LE. DATA(1) .OR. DATA(4) .LE. DATA(3) ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'SPECPLOT_E12', 'SPECPLOT: Error ' //
     :            'finding data plot area: too much space reserved ' //
     :            'for labelling.', STATUS )
               GO TO 500
            END IF
            CALL PGSVP( DATA(1), DATA(2), DATA(3), DATA(4) )

*     Now work out the window (file coordinates). The default offered to
*     the user is derived from the actual ranges in the file.
         CALL SPD_UAAAR( .FALSE., NELM, %VAL( CNF_PVAL(PNTR(4)) ),
     :                   WIND(1), WIND(2), STATUS )
         CALL SPD_UAAAR( .TRUE., NELM, %VAL( CNF_PVAL(PNTR(1)) ),
     :                   WIND(3), WIND(4), STATUS )
         CALL PAR_DEF1R( 'WORLD', 4, WIND, STATUS )
         CALL PAR_GET1R( 'WORLD', 4, WIND, ACTVAL, STATUS )
         IF ( ACTVAL .NE. 4 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPECPLOT_ELMNUM',
     :         'SPECPLOT: 4 values needed for WORLD.', STATUS )
            GO TO 500
         ELSE IF ( STATUS .NE. SAI__OK ) THEN
            GO TO 500
         END IF
         IF ( WIND(2) .EQ. WIND(1) .OR. WIND(4) .EQ. WIND(3) ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPECPLOT_E13', 'SPECPLOT: Error setting ' //
     :         'world coordinates: Left equals right or top equals ' //
     :         'bottom coorinate.', STATUS )
            GO TO 500
         END IF
         CALL PGSWIN( WIND(1), WIND(2), WIND(3), WIND(4) )
      END IF

*  Do all the plotting.
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*     Set up the attributes for the whole plot.
         IF ( ROMAN ) CALL PGSCF( 2 )
         CALL PGSCH( CHIGHT )
         CALL PGSCI( COLOUR )
         CALL PGSLW( THICK )

*     Plot the labelling.
         IF ( .NOT. OVER ) THEN

*        Plot the box and numeric labels according to box layout
*        requests, separately for each side of the box.
            BOXOPT = '      '
            IF ( AXES(1:1) .EQ. '+' ) BOXOPT(1:1) = 'B'
            IF ( TICK(1:1) .NE. '0' ) BOXOPT(2:3) = 'TS'
            IF ( TICK(1:1) .EQ. '+' ) BOXOPT(4:4) = 'I'
            IF ( NUML(1:1) .EQ. '+' ) BOXOPT(5:5) = 'N'
            IF ( BOXOPT .NE. ' ' )
     :         CALL PGBOX( BOXOPT, MAJOR(1), MINOR(1),
     :            ' ', MAJOR(2), MINOR(2) )
            BOXOPT = '      '
            IF ( AXES(2:2) .EQ. '+' ) BOXOPT(1:1) = 'B'
            IF ( TICK(2:2) .NE. '0' ) BOXOPT(2:3) = 'TS'
            IF ( TICK(2:2) .EQ. '+' ) BOXOPT(4:4) = 'I'
            IF ( NUML(2:2) .NE. '0' ) BOXOPT(5:5) = 'N'
            IF ( NUML(2:2) .EQ. '+' ) BOXOPT(6:6) = 'V'
            IF ( BOXOPT .NE. ' ' )
     :         CALL PGBOX( ' ', MAJOR(1), MINOR(1),
     :            BOXOPT, MAJOR(2), MINOR(2) )
            BOXOPT = '      '
            IF ( AXES(3:3) .EQ. '+' ) BOXOPT(1:1) = 'C'
            IF ( TICK(3:3) .NE. '0' ) BOXOPT(2:3) = 'TS'
            IF ( TICK(3:3) .EQ. '+' ) BOXOPT(4:4) = 'I'
            IF ( NUML(3:3) .EQ. '+' ) BOXOPT(5:5) = 'M'
            IF ( BOXOPT .NE. ' ' )
     :         CALL PGBOX( BOXOPT, MAJOR(1), MINOR(1),
     :            ' ', MAJOR(2), MINOR(2) )
            BOXOPT = '      '
            IF ( AXES(4:4) .EQ. '+' ) BOXOPT(1:1) = 'C'
            IF ( TICK(4:4) .NE. '0' ) BOXOPT(2:3) = 'TS'
            IF ( TICK(4:4) .EQ. '+' ) BOXOPT(4:4) = 'I'
            IF ( NUML(4:4) .NE. '0' ) BOXOPT(5:5) = 'M'
            IF ( NUML(4:4) .EQ. '+' ) BOXOPT(6:6) = 'V'
            IF ( BOXOPT .NE. ' ' )
     :         CALL PGBOX( ' ', MAJOR(1), MINOR(1),
     :            BOXOPT, MAJOR(2), MINOR(2) )

*        Plot the text labels.
            IF ( TEXT(1:1) .EQ. '+' )
     :         CALL PGMTXT( 'B', (SPACE(1)-CHIGHT)/CHIGHT,
     :            .5, .5, LABEL(1) )
            IF ( TEXT(2:2) .EQ. '+' )
     :         CALL PGMTXT( 'L', (SPACE(2)-2.*CHIGHT)/CHIGHT,
     :            .5, .5, LABEL(2) )
            IF ( TEXT(3:3) .EQ. '+' )
     :         CALL PGMTXT( 'T', (SPACE(3)-2.*CHIGHT)/CHIGHT,
     :            .5, .5, LABEL(3) )
            IF ( TEXT(4:4) .EQ. '+' )
     :         CALL PGMTXT( 'R', (SPACE(4)-CHIGHT)/CHIGHT,
     :            .5, .5, LABEL(4) )
         END IF

*     Set character height for markers: The filled markers 5 and 6
*     would be smaller than others, unless we increase the character
*     height.
         IF ( MARK .GE. 5 .AND. MARK .LE. 6 )
     :      CALL PGSCH( 1.5*CHIGHT )

*     Do the data plotting.
         CALL SPD_WAAD( ERROR, WIDTH, LIN, BIN,
     :                  (MARK.GE.1.AND.MARK.LE.6), DASH, PGMARK(MARK),
     :                  NELM, %VAL( CNF_PVAL(PNTR(4)) ),
     :                  %VAL( CNF_PVAL(PNTR(1)) ),
     :                  %VAL( CNF_PVAL(ZPTR(1)) ),
     :                  %VAL( CNF_PVAL(ZPTR(2)) ),
     :                  %VAL( CNF_PVAL(ZPTR(3)) ),
     :                  %VAL( CNF_PVAL(ZPTR(4)) ), STATUS )

*     Reset character height.
         IF ( MARK .GE. 5 .AND. MARK .LE. 6 )
     :      CALL PGSCH( 1.5*CHIGHT )

*  Post-plot processing for non-overlay.
*  Save the ultimate window/view port as a DATA picture.
*  This should have picture world coordinates 0...1 and a transform
*  to the above window coordinates.
      IF ( .NOT. OVER ) THEN

*     The first forward function turns data file values into picture
*     world coordinates for X.
*     X = ( XL - WIND(1) ) / ( WIND(2) - WIND(1) )
         FORW(1) = 'X = ( XL - ( '
         I = 13
         CALL CHR_PUTR( WIND(1), FORW(1), I )
         FORW(1)(I+1:) = ' ) ) / ( '
         I = I + 9
         CALL CHR_PUTR( WIND(2)-WIND(1), FORW(1), I )
         FORW(1)(I+1:) = ' )'

*     The second forward function turns data file values into picture
*     world coordinates for Y.
*     Y = ( YL - WIND(3) ) / ( WIND(4) - WIND(3) )
         FORW(2) = 'Y = ( YL - ( '
         I = 13
         CALL CHR_PUTR( WIND(3), FORW(2), I )
         FORW(2)(I+1:) = ' ) ) / ( '
         I = I + 9
         CALL CHR_PUTR( WIND(4)-WIND(3), FORW(2), I )
         FORW(2)(I+1:) = ' )'

*     The first inverse function turns picture world coordinates into
*     data file values for X.
*     XL = X * ( WIND(2) - WIND(1) ) + WIND(1)
         INVS(1) = 'XL = X * ( '
         I = 11
         CALL CHR_PUTR( WIND(2)-WIND(1), INVS(1), I )
         INVS(1)(I+1:) = ' ) + ( '
         I = I + 7
         CALL CHR_PUTR( WIND(1), INVS(1), I )
         INVS(1)(I+1:) = ' )'

*     The second inverse function turns picture world coordinates
*     into data file values for Y.
*     YL = Y * ( WIND(4) - WIND(3) ) + WIND(3)
         INVS(2) = 'YL = Y * ( '
         I = 11
         CALL CHR_PUTR( WIND(4)-WIND(3), INVS(2), I )
         INVS(2)(I+1:) = ' ) + ( '
         I = I + 7
         CALL CHR_PUTR( WIND(3), INVS(2), I )
         INVS(2)(I+1:) = ' )'

*     For the sake of unity range in the picture set the window
*     coordinates accrodingly. Then save the viewport as a DATA
*     picture. Then store the transform.
         CALL PGSWIN( 0., 1., 0., 1. )
         CALL AGP_SVIEW( 'DATA', 'SPECDRE_SPECPLOT', PICDAT, STATUS )
         CALL AGI_TNEW( 2, 2, FORW, INVS, PICDAT, STATUS )
      END IF

*  Tidy up.
 500  CONTINUE

*  Deassign the device (makes the same current as was before opening
*  the device).
      CALL AGP_DEASS( 'DEVICE', .FALSE., STATUS )

*  Close down NDF.
      CALL NDF_END( STATUS )

*  Return.
      END
