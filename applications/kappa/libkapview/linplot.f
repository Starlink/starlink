      SUBROUTINE LINPLOT( STATUS )
*+
*  Name:
*     LINPLOT

*  Purpose:
*     Draws a line plot of the data values in a 1-dimensional NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL LINPLOT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a plot of array value against position for
*     a 1-dimensional NDF. The vertical axis of the plot represents array 
*     value (or the logarithm of the array value - see parameter YLOG),
*     and the horizontal axis represents position.
*
*     The plot may take several different forms such as a "join-the-dots"
*     plot, a "staircase" plot, a "chain" plot, etc, (see parameter MODE).
*     Errors on both the data values and the data positions may be represented 
*     in several different ways (see parameters ERRBAR and SHAPE). The
*     plotting style (colour, fonts, text size, etc) may be specified in
*     detail using parameter STYLE.
*
*     The bounds of the plot on both axes can be specified using
*     parameters XLEFT, XRIGHT, YBOT and YTOP. If not specified they take
*     default values which encompass the entire supplied data set. The
*     current picture is usually cleared before plotting the new picture,
*     but parameter CLEAR can be used to prevent this, allowing several
*     plots to be "stacked" together. If a new plot is drawn over an
*     existing plot, then there is an option to allow the bounds of the
*     new plot to be set to the bounds of the existing plot (see parameter
*     ALIGN).
*
*     The input NDF may, for instance, contain a spectrum of data values
*     against wavelength, or it may contain data values along a
*     1-dimensional profile through an NDF of higher dimensionality. In
*     the latter case, the current co-ordinate Frame of the NDF may have
*     more than 1 axis. Any of the axes in the current co-ordinate Frame
*     of the input NDF may be used to annotate the horizontal axis of the
*     plot produced by this application. Alternatively, the horizontal
*     axis may be annotated with offset from the first array element
*     measured within the current co-ordinate Frame of the NDF. For
*     instance, a 1-d slice through a 2-d image calibrated in RA/DEC could
*     be annotated with RA, or DEC, or offset from the first element (in
*     arc-minutes, degrees, etc). This offset is measured along the path
*     of the profile. The choice of annotation for the horizontal axis is
*     controlled by parameter USEAXIS.
*
*  Usage:
*     linplot ndf [comp] [mode] [xleft] [xright] [ybot] [ytop] [device]

*  ADAM Parameters:
*     ALIGN = _LOGICAL (Read)
*        Controls the way in which the bounds of a new plot are
*        determined if it is drawn over an existing plot. If ALIGN
*        is FALSE, then the bounds of the new plot are specified
*        using parameters XLEFT, XRIGHT, YBOT and YTOP as usual. If ALIGN
*        is TRUE, then the bounds of the new plot are set equal to 
*        the bounds of the existing plot (on both axes), and parameters 
*        XLEFT, XRIGHT, YBOT and YTOP are ignored. This parameter is only
*        accessed if parameter CLEAR is TRUE, and if there is another line 
*        plot within the current picture. The dynamic default is to align
*        the plots if the labels on the horizontal axes are the same, and
*        not to align the plots if the labels are different. []
*     AXES = _LOGICAL (Read)
*        TRUE if labelled and annotated axes are to be drawn around the
*        plot. The dynamic default is FALSE if the plot is being
*        aligned with an existing plot (see parameter ALIGN), and
*        TRUE otherwise. Parameters USEAXIS and YLOG determine the
*        quantities used to annotated the horizontal and vertical axes
*        respectively. The width of the margins left for the annotation 
*        may be controlled using parameter MARGIN. The appearance of the 
*        axes (colours, fonts, etc) can be controlled using the parameter
*        STYLE. []
*     CLEAR = _LOGICAL (Read)
*        If TRUE the current picture is cleared before the plot is 
*        drawn. If CLEAR is FALSE not only is the existing plot retained, 
*        but also the previous plot can be used to specify the axis 
*        limits (see parameter ALIGN). Thus you can generate a composite 
*        plot within a single set of axes, say using different colours or 
*        modes to distinguish data from different datasets.
*     COMP = LITERAL (Read)
*        The NDF component to be plotted.  It may be "Data", "Quality",
*        "Variance", or "Error" (where "Error" is an alternative to
*        "Variance" and causes the square root of the variance values
*        to be displayed).  If "Quality" is specified, then the quality
*        values are treated as numerical values (in the range 0 to
*        255). ["Data"]
*     DEVICE = DEVICE (Read)
*        The plotting device. [current graphics device]
*     ERRBAR = _LOGICAL (Read)
*        TRUE if error bars are to be drawn. The error bars can
*        comprise either or both of the data and axis-centre errors,
*        depending on what is available in the supplied dataset.  The
*        parameter SHAPE controls the appearance of the error bars, and
*        XSIGMA and YSIGMA control their lengths. The ERRBAR parameter is 
*        ignored unless the parameter COMP is set to "Data". [FALSE]
*     FREQ = _INTEGER (Read)
*        The frequency at which error bars are to be plotted.  For
*        instance, a value of 2 would mean that alternate points have
*        error bars plotted.  This lets some plots be less cluttered.
*        FREQ must lie in the range 1 to half of the number of points
*        to be plotted.  FREQ is only accessed when parameter ERRBAR is
*        TRUE.  [1]
*     KEY = _LOGICAL (Read)
*        TRUE if a key is to be plotted below the horizontal axis giving
*        the positions at the start and end of the plot, within the
*        current co-ordinate Frame of the NDF. If parameter USEAXIS is
*        null (i.e. if the horizontal axis is annotated with offset from 
*        the first array element), then the positions refer to the centres
*        of the first and last elements in the supplied NDF, whether or not
*        these elements are actually visible in the plot. If USEAXIS is not 
*        null (i.e. if the horizontal axis is annotated with the value on 
*        one of the axes of the NDFs current co-ordinate Frame), then the
*        displayed positions correspond to the two ends of the visible
*        section of the horizontal axis. The appearance of the key can be
*        controlled using parameter KEYSTYLE. The dynamic default is to 
*        produce a key if the current co-ordinate Frame of the supplied NDF 
*        has 2 or more axes, and not to if it only has 1 axis. []
*     KEYSTYLE = LITERAL (Read)
*        A group of attribute settings describing the plotting style to use 
*        for the key (see parameter KEY).
*
*        A comma-separated list of strings should be given in which each
*        string is either an attribute setting, or the name of a text file
*        preceded by an up-arrow character "^". Such text files should
*        contain further comma-separated lists which will be read and 
*        interpreted in the same manner. Attribute settings are applied in 
*        the order in which they occur within the list, with later settings
*        over-riding any earlier settings given for the same attribute.
*
*        Each individual attribute setting should be of the form:
*
*           <name>=<value>
*        
*        where <name> is the name of a plotting attribute, and <value> is
*        the value to assign to the attribute. Default values will be
*        used for any unspecified attributes. All attributes will be
*        defaulted if a null value (!) is supplied. See section "Plotting
*        Attributes" in SUN/95 for a description of the available
*        attributes. Any unrecognised attributes are ignored (no error is
*        reported). [current value] 
*     MARGIN( 4 ) = _REAL (Read)
*        The widths of the margins to leave for axis annotation, given 
*        as fractions of the corresponding dimension of the DATA picture. 
*        Four values may be given, in the order - bottom, right, top, left. 
*        If less than four values are given, extra values are used equal to 
*        the first supplied value. If these margins are too narrow any axis 
*        annotation may be clipped. The dynamic default is 0.18 (for all 
*        edges) if either annotated axes or a key are produced, and zero 
*        otherwise. []
*     MARKER = _INTEGER (Read)
*        This parameter is only accessed if parameter MODE is set to
*        "Chain" or "Mark". It specifies the symbol with which each
*        position should be marked, and should be given as an integer 
*        PGPLOT marker type. For instance, 0 gives a box, 1 gives a dot, 
*        2 gives a cross, 3 gives an asterisk, 7 gives a triangle. The 
*        value must be larger than or equal to -31. [current value]
*     MODE = LITERAL (Read)
*        Specifies the way in which data values are represented. MODE
*        can take the following values:
*        - "Histogram" -- An histogram of the points is plotted in the
*        style of a "staircase" (with vertical lines only joining the y 
*        values and not extending to the base of the plot).  The vertical 
*        lines are placed midway between adjacent x positions.
*
*        - "Line" -- The points are joined by straight lines.
*
*        - "Point" -- A dot is plotted at each point.
*
*        - "Mark" -- Each point is marker with a symbol specified by 
*        parameter MARKER.
*
*        - "Chain" -- A combination of "Line" and "Mark". 
*
*        - "Step" -- Each point is displayed as a horizontal line, whose 
*        length is specified by the axis width of the pixel.
*
*        [current value]
*     NDF = NDF (Read)
*        NDF structure containing the array to be plotted.
*     SHAPE = LITERAL (Read)
*        Specifies the way in which errors are represented. SHAPE
*        can take the following values:
*        - "Bars" -- Bars with serifs (i.e. cross pieces at each end) are
*        drawn joining the x error limits and the y error limits. The plotting 
*        attribute "Size(ErrBars)" (see parameter STYLE) can be used to 
*        control the size of these serifs (the attribute value should be
*        a magnification factor - 1.0 gives default serifs).
*
*        - "Cross" -- San-serif bars are drawn joining the x error limits and 
*        the y error limits.
*
*        - "Diamond" -- Adjacent error limits are joined to form an
*        error diamond.
*
*        The length of the error bars can be controlled using parameters
*        XSIGMA and YSIGMA. The colour, line width and line style used to 
*        draw them can be controlled using the plotting attributes 
*        "Colour(ErrBars)", "Width(ErrBars)" and "Style(ErrBars)" (see 
*        parameter STYLE). SHAPE is only accessed when parameter ERRBAR 
*        is TRUE.  [current value]
*     STYLE = LITERAL (Read)
*        A group of attribute settings describing the plotting style to use 
*        when drawing the annotated axes, data values, and error markers.
*
*        A comma-separated list of strings should be given in which each
*        string is either an attribute setting, or the name of a text file
*        preceded by an up-arrow character "^". Such text files should
*        contain further comma-separated lists which will be read and 
*        interpreted in the same manner. Attribute settings are applied in 
*        the order in which they occur within the list, with later settings
*        over-riding any earlier settings given for the same attribute.
*
*        Each individual attribute setting should be of the form:
*
*           <name>=<value>
*        
*        where <name> is the name of a plotting attribute, and <value> is
*        the value to assign to the attribute. Default values will be
*        used for any unspecified attributes. All attributes will be
*        defaulted if a null value (!) is supplied. See section "Plotting
*        Attributes" in SUN/95 for a description of the available
*        attributes. Any unrecognised attributes are ignored (no error is
*        reported). 
*
*        The appearance of the data values is controlled by the attributes
*        Colour(Curves), Width(Curves), etc (the synonym Lines may be used
*        in place of Curves). The appearance of markers used if parameter 
*        MODE is set to "Point", "Mark" or "Chain" is controlled by 
*        Colour(Markers), Width(Markers), etc (the synonym Symbols may be 
*        used in place of Markers). The appearance of the error symbols is 
*        controlled using Colour(ErrBars), Width(ErrBars), etc, (see 
*        parameter SHAPE). [current value]
*     USEAXIS = LITERAL (Read)
*        Specifies the quantity to be used to annotate the horizontal axis 
*        of the plot. If USEAXIS is given a null (!) value, then the 
*        distance along the profile from the centre of the first element in 
*        the supplied NDF will be used. This will be measured in the current 
*        co-ordinate Frame of the NDF. If USEAXIS is not null, then it
*        gives the index of the axis which is to be used, and must be less 
*        than or equal to the number of axes in the current co-ordinate 
*        Frame of the NDF. 
*
*        The quantity used to annotate the horizontal axis must have a
*        defined value at all points in the array, and must increase or 
*        decrease monotonically along the array. For instance, if RA is 
*        used to annotate the horizontal axis, then an error will be
*        reported if the profile passes through RA=0 because it will 
*        introduce a non-monotonic jump in axis value (from 0h to 24h, or 
*        24h to 0h). The dynamic default is 1 if the current co-ordinate 
*        Frame in the NDF is 1-dimensional an null (!) otherwise. []
*     XLEFT = LITERAL (Read)
*        The axis value to place at the left hand end of the horizontal
*        axis. The dynamic default is the value for the first element in the 
*        supplied NDF (with a margin to include any horizontal error bar).
*        The value supplied may be greater than or less than the value
*        supplied for XRIGHT. A formatted value for the quantity specified 
*        by parameter USEAXIS should be supplied. See also parameter ALIGN. []
*     XRIGHT = LITERAL (Read)
*        The axis value to place at the right hand end of the horizontal
*        axis. The dynamic default is the value for the last element in the 
*        supplied NDF (with a margin to include any horizontal error bar).
*        The value supplied may be greater than or less than the value
*        supplied for XLEFT. A formatted value for the quantity specified 
*        by parameter USEAXIS should be supplied. See also parameter ALIGN. []
*     XSIGMA = LITERAL (Read)
*        If horizontal error bars are produced (see parameter ERRBAR), then
*        XSIGMA gives the number of standard deviations which the error
*        bars are to represent. [current value]
*     YBOT = LITERAL (Read)
*        The axis value to place at the bottom end of the vertical
*        axis. The dynamic default is the lowest value within the supplied
*        array (with a margin to include any vertical error bars).
*        The value supplied may be greater than or less than the value
*        supplied for YTOP. The value should be supplied as a floating
*        point value for the quantity specified by parameter YLOG. See also
*        parameter ALIGN. []
*     YLOG = _LOGICAL (Read)
*        TRUE if the value displayed on the vertical axis is to be the
*        logarithm of the supplied data values. If TRUE, then the values
*        supplied for parameters YTOP and YBOT should be values for the
*        logarithm of the data value, not the data value itself. [FALSE]
*     YSIGMA = LITERAL (Read)
*        If vertical error bars are produced (see parameter ERRBAR), then
*        YSIGMA gives the number of standard deviations which the error
*        bars are to represent. [current value]
*     YTOP = LITERAL (Read)
*        The axis value to place at the top end of the vertical
*        axis. The dynamic default is the highest value within the supplied
*        array (with a margin to include any vertical error bars).
*        The value supplied may be greater than or less than the value
*        supplied for YBOT. The value should be supplied as a floating
*        point value for the quantity specified by parameter YLOG. See also
*        parameter ALIGN. []

*  Examples:
*     linplot spectrum 
*        Plots data values versus position for the whole of the
*        1-dimensional NDF called spectrum on the current graphics
*        device.  If the current co-ordinate Frame of the NDF is also
*        1-dimensional, then the horizontal axis will be labelled with 
*        values on axis 1 of the current co-ordinate Frame. Otherwise, it
*        will be labelled with offset from the first element.
*     linplot map(,100)
*        Plots data values versus position for row 100 in the 2-dimensional
*        NDF called map on the current graphics device.  
*     linplot spectrum(1:500) device=ps_l
*        Plots data values versus position for the first 500 elements
*        of the 1-dimensional NDF called spectrum. The output goes to a
*        text file which can be printed on a PostScript printer.
*     linplot ironarc v style="title=Fe Arc variance"
*        Plots variance values versus position for the whole of the 
*        1-dimensional NDF called ironarc on the current graphics device.  
*        The plot has a title of "Fe Arc variance".
*     linplot prof useaxis=dec xleft="23:30:22" xright="23:30:45"
*        This plots data values versus declination for those elements of the 
*        1-dimensional NDF called prof with declination value between 23d
*        30m 22s, and 23d 30m 45s. This assumes that the current
*        co-ordinate Frame in the NDF has an axis with symbol "dec".
*     linplot prof useaxis=2 ybot=1.0 ytop=3.0 ylog
*        This plots the logarithm (base 10) of the data values in the
*        entire 1-dimensional NDF called prof, against the value described 
*        by the second axis in the current co-ordinate Frame of the NDF.
*        The bottom of the vertical axis corresponds to a data value of 
*        10.0 and the top corresponds to a data value of 1000.0 (10 to
*        the power 3.0).
*     linplot xspec mode=p errbar xsigma=3 ysigma=3 shape=d style=^my_sty 
*        This plots the data values versus position for the dataset called 
*        xspec. Each pixel is plotted as a point surrounded by diamond-shaped 
*        error bars.  The error bars are 3 sigma error bars. The plotting 
*        style is read from text file my_sty. This could, for instance, 
*        contain strings such as; colour(err)=pink, colour(sym)=red, 
*        tickall=0, edge(2)=right. These cause the error bars to be drawn 
*        in pink, the points to be drawn in red, tick marks to be restricted 
*        to the labelled edges of the plot, and the vertical axis (axis 2) 
*        to be annotated on the right hand edge of the plot.
*     linplot ndf=spectrum noclear align
*        Plots data values versus pixel co-ordinate for the whole of
*        the 1-dimensional NDF called spectrum on the current graphics
*        device.  The plot is drawn over any existing plot and inherits
*        the bounds of the previous plot on both axes. A warning will be 
*        reported if the labels for the horizontal axes of the two plots
*        are different.

*  Notes:
*     -  The Title component in the NDF is used as the default title for 
*     the annotated axes. If the NDF does not have a Title component, then
*     the default title is taken from current co-ordinate Frame in the NDF. 
*     This default may be over-ridden by specifying a value for the Title
*     attribute using the STYLE parameter. 
*     -  Default axis errors and widths are used, if none are present in
*     the NDF. The defaults are the constants 0 and 1 respectively.
*     -  The application stores a number of pictures in the graphics
*     database in the following order: a FRAME picture containing the 
*     annotated axes, data plot, and optional key; a KEY picture to store 
*     the key if present; and a DATA picture containing just the data plot.
*     Note, the FRAME picture is only created if annotated axes or a key
*     has been drawn, or if non-zero margins were specified using parameter 
*     MARGIN. The world co-ordinates in the DATA picture will correspond
*     to offset along the profile on the horizontal axis, and data value
*     (or logarithm of data value) on the vertical axis. On exit the current 
*     database picture for the chosen device reverts to the input picture.

*  Related Applications:
*     KAPPA: PROFILE, MLINPLOT; Figaro: ESPLOT, IPLOTS, MSPLOT, SPLOT;
*     SPECDRE: SPECGRID.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, VARIANCE,
*     QUALITY, LABEL, TITLE, WCS and UNITS components of the NDF.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.  Only
*     double-precision floating-point data can be processed directly.
*     Other non-complex data types will undergo a type conversion
*     before the plot is drawn.

*  Authors:
*     Malcolm Currie STARLINK (RAL::CUR)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-OCT-1998 (DSB):
*        Original AST version, based on earlier version by MJC.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Global SSE definitions
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'PAR_ERR'          ! PAR error constants 

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN            ! Used length of a string
      LOGICAL CHR_SIMLR          ! Strings equal apart from case?

*  Local Constants:
      INTEGER NDIM               ! Dimensionality of input array
      PARAMETER( NDIM = 1 )      ! 1-d data

*  Local Variables:
      CHARACTER COMP*8         ! Component to be displayed
      CHARACTER KEYLN1*80      ! First line of key text
      CHARACTER KEYLN2*80      ! Second line of key text
      CHARACTER MCOMP*8        ! Component to be mapped
      CHARACTER NDFNAM*255     ! Full NDF specification 
      CHARACTER NEWLAB*80      ! X axis label from new Plot
      CHARACTER OLDLAB*80      ! X axis label from existing Plot
      CHARACTER TEXT*255       ! A general text string
      CHARACTER UNITS*20       ! Units of the data
      DOUBLE PRECISION BL( 2 ) ! "w.w. want" X/Y values at bottom left corner
      DOUBLE PRECISION BLG( 2 )! "uniform" X/Y values at bottom left corner
      DOUBLE PRECISION BOX( 4 )! Bounds of DATA picture 
      DOUBLE PRECISION DVAL    ! General double precision value
      DOUBLE PRECISION FN( NDF__MXDIM ) ! End position in current Frame
      DOUBLE PRECISION ST( NDF__MXDIM ) ! Start position in current Frame
      DOUBLE PRECISION TR( 2 ) ! "w.w. want" X/Y values at top right corner
      DOUBLE PRECISION TRG( 2 )! "uniform" X/Y values at top right corner
      DOUBLE PRECISION TXTPOS( 2 )! Key text centre position
      DOUBLE PRECISION XSMAX   ! Max X after inclusion of axis widths
      DOUBLE PRECISION XSMIN   ! Min X after inclusion of axis widths
      INTEGER AXMAP            ! Point to NDFs AXIS->GRID Mapping
      INTEGER AXMAPS( 2 )      ! Axis mappings for displayed data plot
      INTEGER DIM              ! Number of elements in the input array
      INTEGER EL               ! Number of mapped elements 
      INTEGER FREQ             ! Interval between error bars
      INTEGER FSET             ! Pointer to FrameSet 
      INTEGER I                ! General variable
      INTEGER IAT              ! No. of characters in a string
      INTEGER IAXFR            ! Index of effective AXIS Frame in IWCS
      INTEGER IAXIS            ! Index of axis used to annotate horiz. axis
      INTEGER ICURR0           ! Index of original current Frame in Plot
      INTEGER IHI              ! Index of last array element to use
      INTEGER ILO              ! Index of first array element to use
      INTEGER IMODE            ! Mode identifier
      INTEGER INDF             ! NDF identifier for input NDF
      INTEGER IPAWID           ! Pointer to supplied x axis widths
      INTEGER IPIC0            ! AGI id. for original current picture
      INTEGER IPICD            ! AGI id. for DATA picture
      INTEGER IPICD0           ! Existing picture ID
      INTEGER IPICF            ! AGI id. for new FRAME picture
      INTEGER IPICK            ! AGI id. for the KEY picture
      INTEGER IPLOT            ! Pointer to AST Plot for DATA picture
      INTEGER IPLOTK           ! Pointer to AST Plot for KEY picture
      INTEGER IPSTEP           ! Pointer to x axis widths in "w.w. want"
      INTEGER IPXBAR           ! Pointer to x error bar limits in "w.w. want"
      INTEGER IPXCEN           ! Pointer to x centres in "w.w. want" Frame
      INTEGER IPXDAT           ! Pointer to supplied x axis centres
      INTEGER IPXVAR           ! Pointer to supplied x centre variances
      INTEGER IPYBAR           ! Pointer to y error bar limits in "w.w. want"
      INTEGER IPYCEN           ! Pointer to y data values in "w.w. want" Frame
      INTEGER IPYDAT           ! Pointer to supplied y data values
      INTEGER IPYVAR           ! Pointer to supplied y data variances
      INTEGER ISHAPE           ! Identifier for error bar shape
      INTEGER IWCS             ! Pointer to the WCS FrameSet from the NDF
      INTEGER MAP              ! Pointer to "w.w. got"->"w.w. want" Mapping 
      INTEGER MONO             ! 0: Not monotonic, +1: increase. -1: decrease
      INTEGER MTYPE            ! PGPLOT marker type
      INTEGER NAX              ! No. of axes in current Frame
      INTEGER NAXNDF           ! No. of pixel axes in the base NDF
      INTEGER NC               ! No. of characters in NDFNAM
      INTEGER NCU              ! Number of characters in the units
      INTEGER NFRM             ! Frame index increment between IWCS and IPLOT
      INTEGER NMARG            ! No. of margin values given
      INTEGER PMAP             ! PermMap pointer
      INTEGER PERM( NDF__MXDIM ) ! Dummy axis permutation array
      INTEGER SDIM( NDIM )     ! The significant NDF axes
      INTEGER SLBND( NDIM )    ! Significant lower bounds of the image
      INTEGER SUBND( NDIM )    ! Significant upper bounds of the image
      INTEGER WCSMAP           ! Mapping from NDF GRID to current Frame
      INTEGER WWGOT            ! Index of "what we've got" Frame in FSET
      INTEGER WWWANT           ! Index of "what we want" Frame in FSET
      LOGICAL ALIGN            ! Should new X axis be aligned with existing X axis?
      LOGICAL AXES             ! Produce annotated axes?
      LOGICAL BAD              ! Bad values found?
      LOGICAL DIST             ! Show distance instead of axis value?
      LOGICAL ERRBAR           ! Display error bars?
      LOGICAL FRSTEP           ! Free pointer to x axis widths in "w.w. want"?
      LOGICAL FRXBAR           ! Free pointer to x error bar limits in "w.w. want"
      LOGICAL FRXCEN           ! Free pointer to x centres in "w.w. want" Frame
      LOGICAL FRYBAR           ! Free pointer to y error bar limits in "w.w. want"
      LOGICAL FRYCEN           ! Free ointer to y data values in "w.w. want" Frame
      LOGICAL KEY              ! Key of the contour heights to be produced?
      LOGICAL NEWPIC           ! Is new Plot aligned with existing DATA picture?
      LOGICAL NOINV            ! Did any mapping not have an inverse?
      LOGICAL OLDPIC           ! Was an existing DATA picture found?
      LOGICAL THERE            ! Does object exist?
      LOGICAL XLABEQ           ! Are x axis labels equal?
      LOGICAL XVAR             ! Display x axis centre variances?
      LOGICAL YLOG             ! Show log of data value?
      LOGICAL YVAR             ! Display y data variances?
      REAL HGT                 ! Height of text with horizontal baseline
      REAL KEYOFF              ! Offset to top of key 
      REAL LNSP                ! Line spacing in millimetres
      REAL MARGIN( 4 )         ! Width of margins round DATA picture
      REAL UP(2)               ! Up vector
      REAL X1, X2, Y1, Y2      ! Bounds of PGPLOT window
      REAL XCH                 ! Height of text with vertical baseline
      REAL XSIGMA              ! No. of std. devn's for X error bars
      REAL YSIGMA              ! No. of std. devn's for Y error bars

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Initialise pointers to allocated memory, to enable checks to be
*  performed when shutting down.
      FRSTEP = .FALSE.
      FRXBAR = .FALSE.
      FRXCEN = .FALSE.
      FRYBAR = .FALSE.
      FRYCEN = .FALSE.

*  Access the input NDF and get WCS information.
*  =============================================

*  Obtain the identifier of the NDF to be ploted.
      CALL NDF_ASSOC( 'NDF', 'READ', INDF, STATUS )

*  Find which component to plot.
      CALL KPG1_ARCOG( 'COMP', INDF, MCOMP, COMP, STATUS )

*  Obtain the units if present.  
      CALL KPG1_DAUNI( INDF, MCOMP, UNITS, NCU, STATUS )

*  Get an AST pointer to a FrameSet describing the co-ordinate Frames
*  present in the NDF's WCS component. Modify it to ensure that the Base,
*  and PIXEL frames have 1 dimension.  All current Frame axes are retained.
*  The NDF must have exactly 1 significant dimension (i.e. axis spanning 
*  more than 1 pixel). We do not check that the inverse Mapping is
*  defined since the inverse will be implemented within KPS1_LPLFS by a 
*  look up table (if possible).
      CALL KPG1_ASGET( INDF, NDIM, .TRUE., .FALSE., .FALSE., SDIM, 
     :                 SLBND, SUBND, IWCS, STATUS )

*  Save the length of the significant axis.
      DIM = SUBND( 1 ) - SLBND( 1 ) + 1

*  Save the number of current Frame axes.
      NAX = AST_GETI( IWCS, 'NAXES', STATUS )

*  Get the simplified Mapping from GRID Frame to current Frame.
      WCSMAP = AST_SIMPLIFY( AST_GETMAPPING( IWCS, AST__BASE, 
     :                                       AST__CURRENT, STATUS ),
     :                       STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  See which axis should be used for the horizontal axis annotation.
*  Allow null to be used to indicate that the horizontal axis should be
*  annotated with geodesic distance form the first point (measured along
*  the profile). The dynamic default is axis 1 if the input NDF is one
*  dimensional, and null otherwise. An axis index is returned.
      IF( NAX .EQ. 1 ) THEN
         IAXIS = 1
      ELSE
         IAXIS = 0
      END IF
      CALL KPG1_GTAXI( 'USEAXIS', IWCS, 1, IAXIS, STATUS )

*  If we got an axis, indicate that the horizontal axis will not be
*  annotated with distance frm the starting point.
      IF( STATUS .EQ. SAI__OK ) THEN
         DIST = .FALSE.

*  If a null (!) value was supplied for USEAXIS, annull the error and 
*  indicate that the horizontal axis should be annotated with distance
*  from the starting point.
      ELSE IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         DIST = .TRUE.

*  In general there is no way of knowing which axis to use to format
*  distance values, but a good guess will be to select the latitude 
*  axis of the current Frame, if there is a latitude axis. Otherwise
*  use the first axis. If this selection is inappropriate, the user can
*  always request a specific format using the STYLE parameter. See if there 
*  is an axis with the AsTime attribute set false. Annul any errors which 
*  occur because an axis does not have the AsTime attribute.
         IAXIS = 1

         DO I = 1, NAX
            TEXT = 'ASTIME('
            IAT = 7
            CALL CHR_PUTI( I, TEXT, IAT )
            CALL CHR_APPND( ')', TEXT, IAT )
            IF( .NOT. AST_GETL( IWCS, TEXT( : IAT ), STATUS ) ) THEN
               IF( STATUS .EQ. SAI__OK ) THEN
                  IAXIS = I
               ELSE
                  CALL ERR_ANNUL( STATUS )
               END IF
            END IF
         END DO
      END IF

*  If there is an axis structure in the NDF, get a mapping from the AXIS 
*  Frame to the GRID Frame. If no AXIS structure exists NDF AXIS routines 
*  will use a default one, but AST may use an AXIS Frame in the WCS 
*  component stored by a previous application. Therefore, we use the mapping 
*  from PIXEL Frame (the Frame corresponding to the default NDF AXIS structure)
*  to GRID Frame if no AXIS structure is present. This ensures that the
*  mapping can be used to map Axis values returned by NDF_AMAP into GRID
*  co-ordinates.
      CALL NDF_STATE( INDF, 'AXIS', THERE, STATUS ) 
      IF( THERE ) THEN
         CALL KPG1_ASFFR( IWCS, 'AXIS', IAXFR, STATUS )
      ELSE
         CALL KPG1_ASFFR( IWCS, 'PIXEL', IAXFR, STATUS )
      END IF

      AXMAP = AST_GETMAPPING( IWCS, IAXFR, AST__BASE, STATUS )

*  If the base NDF has more than 1 pixel axis (e.g. if a 1-d section from
*  a 2-d NDF was supplied), we need to modify the above mapping to have 1
*  input corresponding to the significant axis.
      NAXNDF = AST_GETI( AXMAP, 'NIN', STATUS )
      IF( NAXNDF .GT. 1 ) THEN

*  Create a PermMap with 1 input and an output for each NDF pixel axis.
*  Connect the one input to the output for the significant NDF dimension.
         DO I = 1, NAXNDF
            PERM( I ) = 0
         END DO
         PERM( SDIM( 1 ) ) = 1

         PMAP = AST_PERMMAP( 1, SDIM( 1 ), NAXNDF, PERM, 0.0D0, ' ',
     :                       STATUS )

*  Put this PermMap infront of the Mapping found above. The resulting
*  Mapping has one input (teh AXIS value on the significant NDF axis)
*  and one output (the 1-d GRID coordinate).
         AXMAP = AST_CMPMAP( PMAP, AXMAP, .TRUE., ' ', STATUS )

      END IF

*  See if the Y axis is to display logged data values.
      CALL PAR_GET0L( 'YLOG', YLOG, STATUS )      

*  Obtain a FrameSet containing three 2-D Frames. In the Frame 1, axis 1
*  is the GRID co-ordinate with the supplied 1-d array, and axis 2 is the 
*  raw data value. This Frame corresponds to "what we've got". In the Frame 3 
*  (the current Frame), axis 1 is the value on the selected axis from the 
*  NDF's current Frame (or distance along the profile if DIST is .TRUE.), and 
*  axis 2 is the raw or logged data value. This Frame corresponds to "what we
*  want" and is given the Domain "DATAPLOT". Frame 2 (the Base Frame) is 
*  spanned by the axes which are to mapped linearly onto the graphics 
*  screen (distance from the starting point on the first axis, and raw 
*  or logged data value on the second axis). This Frame corresponds to 
*  the "uniform" co-ordinate system. A flag is returned if any of the 
*  required Mappings do not have an inverse transformation. 
      CALL  KPS1_LPLFS( INDF, IWCS, DIST, IAXIS, DIM, YLOG, MCOMP,
     :                  UNITS( : NCU ), NOINV, FSET, STATUS )

*  Note the index of the "what we've got" and "what we want" Frames.
      WWGOT = 1
      WWWANT = 3

*  Get some other parameter values.
*  ================================
*  See if error bars are required.
      CALL PAR_GET0L( 'ERRBAR', ERRBAR, STATUS )

*  If so...
      IF( ERRBAR ) THEN

*  Obtain the shape of error bar to plot.
         CALL PAR_CHOIC( 'SHAPE', 'Bars', 'Bars,Cross,Diamond',
     :                   .TRUE., TEXT, STATUS )

*  Classify the value.
         IF( TEXT .EQ. 'BARS' ) THEN
            ISHAPE = 1
         ELSE IF( TEXT .EQ. 'CROSS' ) THEN
            ISHAPE = 2
         ELSE
            ISHAPE = 3
         END IF

*  Obtain the spacing between points showing the error bars.
         CALL PAR_GDR0I( 'FREQ', 1, 1, MAX( 1, DIM/2 ), .TRUE., FREQ, 
     :                   STATUS )

      END IF

*  Get the plotting mode.
      CALL PAR_CHOIC( 'MODE', 'Line', 
     :                'Histogram,Line,Point,Mark,Step,Chain',
     :                .FALSE., TEXT, STATUS )

*  Get an identifier for the mode, and get the marker type if required.
      IF( TEXT .EQ. 'HISTOGRAM' ) THEN
         IMODE = 1
      ELSE IF( TEXT .EQ. 'LINE' ) THEN
         IMODE = 2
      ELSE IF( TEXT .EQ. 'POINT' ) THEN
         IMODE = 3
         MTYPE = -1
      ELSE IF( TEXT .EQ. 'MARK' ) THEN
         IMODE = 3
         CALL PAR_GET0I( 'MARKER', MTYPE, STATUS )
      ELSE IF( TEXT .EQ. 'STEP' ) THEN
         IMODE = 4
      ELSE 
         IMODE = 5
         CALL PAR_GET0I( 'MARKER', MTYPE, STATUS )
      ENDIF

*  Ensure marker type (if used) is legal.
      MTYPE = MAX( -31, MTYPE )

*  Map the required NDF arrays, and get some work space.
*  =====================================================
*  Map the NDF AXIS centre values in double precision.
      CALL NDF_AMAP( INDF, 'CENTRE', 1, '_DOUBLE', 'READ', IPXDAT,
     :               EL, STATUS ) 

*  Allocate work space to hold the corresponding values to be displayed
*  on the horizontal axis of the Plot.
      CALL PSX_CALLOC( EL, '_DOUBLE', IPXCEN, STATUS )
      FRXCEN = (STATUS .EQ. SAI__OK )

*  We only use variances on the X axis if error bars are required, and if
*  axis variances are available.
      IF( .NOT. ERRBAR ) THEN
         XVAR = .FALSE.
      ELSE
         CALL NDF_ASTAT( INDF, 'VARIANCE', 1, XVAR, STATUS ) 
      END IF

*  If X axis variance values are required map them. 
      IF( XVAR ) THEN
         CALL NDF_AMAP( INDF, 'VARIANCE', 1, '_DOUBLE', 'READ', 
     :                  IPXVAR, EL, STATUS ) 

*  See how many standard deviations are to be used for a horizontal error bar.
         CALL PAR_GET0R( 'XSIGMA', XSIGMA, STATUS )
         XSIGMA = ABS( XSIGMA )

*  Tell the user what XSIGMA value we are using.
         CALL MSG_SETC( 'XS', XSIGMA )
         CALL MSG_OUT( 'LINPLOT_MSG1', '  Errors in position will be '//
     :                 'displayed as ^XS sigma errors.', STATUS )

*  Allocate work space to hold the upper and lower limits of each horizontal
*  error bar on the displayed horizontal axis.
         CALL PSX_CALLOC( EL*2, '_DOUBLE', IPXBAR, STATUS )
         FRXBAR = (STATUS .EQ. SAI__OK )

*  If X axis variance values are not required store safe pointer values.
      ELSE
         IPXVAR = IPXDAT
         IPXBAR = IPXDAT
      END IF

*  If the mode is "STEP"...
      IF( IMODE .EQ. 4 ) THEN

*  Map the axis widths.
         CALL NDF_AMAP( INDF, 'WIDTH', 1, '_DOUBLE', 'READ', IPAWID,
     :                  EL, STATUS ) 

*  Allocate work space to hold the upper and lower limits of each horizontal
*  step on the displayed horizontal axis.
         CALL PSX_CALLOC( EL*2, '_DOUBLE', IPSTEP, STATUS )
         FRSTEP = (STATUS .EQ. SAI__OK )

*  Abort if an error has occurred. 
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  If each point is not being drawn as a step, store safe pointer values.
      ELSE
         IPAWID = IPXDAT
         IPSTEP = IPXDAT
      END IF

*  Map the component of the NDF which is to be plotted on the vertical
*  axis, in double precision. 
      CALL NDF_MAP( INDF, MCOMP, '_DOUBLE', 'READ', IPYDAT, EL, 
     :              STATUS )

*  Allocate work space to hold the displayed Y axis values (these may be
*  logged data values or raw data values).
      CALL PSX_CALLOC( EL, '_DOUBLE', IPYCEN, STATUS )
      FRYCEN = (STATUS .EQ. SAI__OK )

*  We only use variances on the Y axis if the data values being displayed are 
*  from the NDF DATA component, and if error bars are required, and if
*  variances are available.
      IF( MCOMP .NE. 'Data' .OR. .NOT. ERRBAR ) THEN
         YVAR = .FALSE.
      ELSE
         CALL NDF_STATE( INDF, 'VARIANCE', YVAR, STATUS )
      END IF

*  If Y axis variance values are required map them. 
      IF( YVAR ) THEN
         CALL NDF_MAP( INDF, 'VARIANCE', '_DOUBLE', 'READ', IPYVAR, EL, 
     :                 STATUS )

*  See how many standard deviations are to be used for a vertical error bar.
         CALL PAR_GET0R( 'YSIGMA', YSIGMA, STATUS )
         YSIGMA = ABS( YSIGMA )

*  Tell the user what YSIGMA value we are using.
         CALL MSG_SETC( 'YS', YSIGMA )
         CALL MSG_OUT( 'LINPLOT_MSG2', '  Errors in data value will '//
     :                 'be displayed as ^YS sigma errors.', STATUS )

*  Allocate work space to hold the upper and lower limits of each vertical 
*  error bar.
         CALL PSX_CALLOC( EL*2, '_DOUBLE', IPYBAR, STATUS )
         FRYBAR = (STATUS .EQ. SAI__OK )

*  If Y axis variance values are not required store safe pointer values.
      ELSE
         IPYVAR = IPYDAT
         IPYBAR = IPYDAT
      END IF

*  Open the graphics device and database, and see if the plot is to be
*  produced over an existing DATA picture. If it is get a Plot for the
*  existing DATA picture and merge the FrameSet created above with it so 
*  that we have a Plot which can be used to draw the new data plot.
*  ===================================================================

*  Open the graphics device, allowing the user to choose whether or not
*  to clear it. If it is not cleared, the AGI identifier for any existing
*  DATA picture (within the current picture) is returned in IPICD, and
*  the corresponding AST Plot is returned in IPLOT.
      CALL KPG1_PLOTA( AST__NULL, 'UNKNOWN', ' ', IPIC0, IPICD0, IPLOT, 
     :                 STATUS )

*  Set a flag the new DATA picture should be created covering the same
*  area as an existing DATA picture.
      OLDPIC = ( IPLOT .NE. AST__NULL )

*  Indicate we have not yet created the new DATA picture.
      NEWPIC = .FALSE.

*  If we are drawing within an existing DATA picture...
      IF( OLDPIC ) THEN

*  Is the current Frame in the Plot associated with the existing DATA
*  picture a DATAPLOT Frame? If not, it was not created by this
*  application and so we cannot align the new picture with it.
         IF( AST_GETC( IPLOT, 'DOMAIN', STATUS ) .EQ. 'DATAPLOT' ) THEN

*  Get the X axis labels in the old and new DATAPLOTs, and remove leading 
*  blanks.
            OLDLAB = AST_GETC( IPLOT, 'LABEL(1)', STATUS )
            CALL CHR_LDBLK( OLDLAB )         

            NEWLAB = AST_GETC( FSET, 'LABEL(1)', STATUS )
            CALL CHR_LDBLK( NEWLAB )         

*  Issue a warning if the labels are different (apart from case).
            XLABEQ = CHR_SIMLR( OLDLAB, NEWLAB )

            IF( .NOT. XLABEQ ) THEN
               CALL MSG_BLANK( STATUS )
               CALL MSG_SETC( 'OLDLAB', OLDLAB )
               CALL MSG_SETC( 'NEWLAB', NEWLAB )
               CALL MSG_OUT( 'LINPLOT_MSG3', '  WARNING: The value '//
     :                       'being used to annotated the horizontal '//
     :                       'axis (^NEWLAB), is not the same as in '//
     :                       'the existing plot (^OLDLAB).', STATUS )
            END IF

*  We now decide whether to shift the X axis of the new DATAPLOT so that
*  it aligns with the existing DATAPLOT. First of all, choose a default
*  value on the basis of the axis labels. If the X axis labels are equal,
*  assume alignment is required.
            CALL PAR_DEF0L( 'ALIGN', XLABEQ, STATUS )

*  Allow the user to change this value.
            CALL PAR_GET0L( 'ALIGN', ALIGN, STATUS )

*  If they are to be aligned...
            IF( ALIGN ) THEN

*  Save the index of the current Frame in the Plot. 
               ICURR0 = AST_GETI( IPLOT, 'CURRENT', STATUS )

*  Save the number of Frames in the existing Plot.
               NFRM = AST_GETI( IPLOT, 'NFRAME', STATUS )

*  Merge the new FrameSet into the existing Plot, aligning them in the 
*  current Frame (i.e. the DATAPLOT Domain) using a UnitMap.
               CALL AST_ADDFRAME( IPLOT, AST__CURRENT, 
     :                            AST_UNITMAP( 2, ' ', STATUS ), 
     :                            FSET, STATUS ) 

*  Save the indices within the Plot of the "what we've got" and "what we 
*  want" Frames relating to the original NDF.
               WWGOT = WWGOT + NFRM
               WWWANT = WWWANT + NFRM

*  See if a key giving the start and end positions of the profile is
*  required. Default is no.
               CALL PAR_DEF0L( 'KEY', .FALSE., STATUS )
               CALL PAR_GET0L( 'KEY', KEY, STATUS )

*  See if annotated axes are required. The default is no.
               CALL PAR_DEF0L( 'AXES', .FALSE., STATUS )
               CALL PAR_GET0L( 'AXES', AXES, STATUS )

*  Get the margin values, using a dynamic default of zero if no key or
*  axes are being created (to avoid the unnecessary creation of FRAME 
*  pictures by KPG1_PLOTP).
               IF( .NOT. KEY .AND. .NOT. AXES ) THEN
                  CALL PAR_DEF1R( 'MARGIN', 1, 0.0, STATUS )
               ELSE
                  CALL PAR_DEF1R( 'MARGIN', 1, 0.18, STATUS )
               END IF

               CALL PAR_GDRVR( 'MARGIN', 4, -0.49, 10.0, MARGIN, NMARG,
     :                         STATUS )
               NMARG = MIN( 4, NMARG )

*  Abort if an error has occurred.
               IF( STATUS .NE. SAI__OK ) GO TO 999

*  Use the first value for any unspecified edges.
               DO I = NMARG + 1, 4      
                  MARGIN( I ) = MARGIN( 1 )
               END DO

*  Create a new DATA picture with the same shape, position and bounds as the 
*  existing one. Also create a KEY picture if necessary.
               IF( KEY ) THEN
                  CALL KPG1_PLOTP( IPICD0, 'LINPLOT', MARGIN, 1, 'KEY',
     :                             'B', 0.2, 0.0, 0.0D0, IPICD, IPICF, 
     :                             IPICK, STATUS )
               ELSE
                  CALL KPG1_PLOTP( IPICD0, 'LINPLOT', MARGIN, 0, ' ',
     :                             ' ', 0.0, 0.0, 0.0D0, IPICD, IPICF, 
     :                             IPICK, STATUS )
               END IF

*  Indicate we have created a new DATA picture.
               NEWPIC = .TRUE.

*  Get the Mapping from the "what we've got" Frame for the new data, to the
*  "what we want" Frame.
               MAP = AST_SIMPLIFY( AST_GETMAPPING( IPLOT, WWGOT, WWWANT,
     :                                             STATUS ),
     :                             STATUS )

*  Map all the "what we've got" values into "what we want" values...
               
*  Data values and vertical error bar limits.
               CALL KPS1_LPLLM( EL, 1, EL, %VAL( IPYDAT ), YVAR, 
     :                          .FALSE.,%VAL( IPYVAR ), YSIGMA, 
     :                          AST__NULL, MAP,2, %VAL( IPYCEN ), 
     :                          %VAL( IPYBAR ), TR( 2 ), BL( 2 ), 
     :                          MONO, BAD, STATUS )

*  The X axis central values and horizontal error bar limits.
               CALL KPS1_LPLLM( EL, 1, EL, %VAL( IPXDAT ), XVAR, 
     :                          .FALSE., %VAL( IPXVAR ), XSIGMA, AXMAP, 
     :                          MAP, 1, %VAL( IPXCEN ), %VAL( IPXBAR ), 
     :                          TR( 1 ), BL( 1 ), MONO, BAD, STATUS )
 
*  If the mode is "STEP", we also do X axis width limits.
               IF( IMODE .EQ. 4 ) THEN
                  CALL KPS1_LPLLM( EL, 1, EL, %VAL( IPXDAT ), .TRUE.,
     :                             .TRUE., %VAL( IPAWID ), 0.5, AXMAP, 
     :                             MAP, 1, %VAL( IPXCEN ), 
     :                             %VAL( IPSTEP ), XSMAX, XSMIN, MONO, 
     :                             BAD, STATUS )
               END IF

*  Indicate that the entire supplied array is to be plotted (only part of
*  it may actually be visible).
               ILO = 1
               IHI = DIM

            END IF

         END IF        

      END IF

*  If the above code did not produce a Plot which can be used to draw the
*  new data plot, we create one now by creating a new DATA picture within
*  the current picture, making it as large as possible. The bounds of this
*  picture are specified by the user, with the bounds of the supplied data
*  being used as the default picture bounds.
*  ======================================================================
      IF( .NOT. NEWPIC .AND. STATUS .EQ. SAI__OK ) THEN 

*  Ensure the original current AGI picture is re-instated.
         CALL AGI_SELP( IPIC0, STATUS )

*  We need to be able to transform points in both directions between the 
*  "what we want" Frame and the "uniform" Frame. This requires all
*  Mappings in the DATAPLOT FrameSet to be invertable. Report an error if
*  this is not the case.
         IF( NOINV ) THEN
            CALL MSG_SETC( 'LAB', AST_GETC( FSET, 'LABEL(1)', STATUS ) )

            STATUS = SAI__ERROR
            CALL ERR_REP( 'LINPLOT_ERR1', 'The horizontal axis value '//
     :                    '(^LAB) does not increase or decrease '//
     :                    'monotonically along the data array, and '//
     :                    'so cannot be used.', STATUS )

         END IF

*  Save the mapping from "what we've got" to "what we want" in the new
*  DATAPLOT FrameSet.
         MAP = AST_SIMPLIFY( AST_GETMAPPING( FSET, WWGOT, WWWANT, 
     :                                       STATUS ),
     :                       STATUS )

*  Find the limits of the axis 1 data value in the "what we want"
*  Frame, including any required error bars. This returns the central and
*  extreme X axis values for each error bar in the "what we want" Frame.
         CALL KPS1_LPLLM( EL, 1, EL, %VAL( IPXDAT ), XVAR, .FALSE.,
     :                    %VAL( IPXVAR ), XSIGMA, AXMAP, MAP, 1, 
     :                    %VAL( IPXCEN ), %VAL( IPXBAR ), TR( 1 ), 
     :                    BL( 1 ), MONO, BAD, STATUS )
 
*  Report an error if either limit is bad.
         IF( ( TR( 1 ) .EQ. AST__BAD .OR. BL( 1 ) .EQ. AST__BAD ) .AND.
     :       STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'LINPLOT_ERR3', 'No valid pixel positions '//
     :                    'found.' , STATUS )
         END IF

*  We now need to extend the x axis limits if each point is being
*  represented as a horizontal step equal to the AXIS WIDTH value.
*  If the mode is "STEP"...
         IF( IMODE .EQ. 4 ) THEN

*  Find the limits of the steps on axis 1 in the "what we want" Frame.
*  This returns the central and extreme X axis values for each step in the 
*  "what we want" Frame.
            CALL KPS1_LPLLM( EL, 1, EL, %VAL( IPXDAT ), .TRUE., .TRUE.,
     :                       %VAL( IPAWID ), 0.5, AXMAP, MAP, 1, 
     :                       %VAL( IPXCEN ), %VAL( IPSTEP ), XSMAX, 
     :                       XSMIN, MONO, BAD, STATUS )

*  Report an error if either limit is bad.
            IF( ( XSMAX .EQ. AST__BAD .OR. XSMIN .EQ. AST__BAD ) .AND.
     :          STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'LINPLOT_ERR4', 'No valid AXIS widths '//
     :                       'found.', STATUS )
            END IF

*  Ensure the X axis limits include the steps.
            TR( 1 ) = MAX( TR( 1 ), XSMAX )
            BL( 1 ) = MIN( BL( 1 ), XSMIN )
         END IF

*  If the annotated values on the horizontal axis is reversed, reverse the 
*  limits.
         IF( MONO .EQ. -1 ) THEN
            DVAL = BL( 1 )
            BL( 1 ) = TR( 1 ) 
            TR( 1 ) = DVAL
         END IF

*  Ensure the limits are not equal.
         IF( BL( 1 ) .EQ. TR( 1 ) ) THEN
            IF( BL( 1 ) .NE. 0.0 ) THEN
               TR( 1 ) = 2.0*BL( 1 )
            ELSE
               TR( 1 ) = 1.0D0
            END IF
         END IF

*  We now have the limits of the data on the X axis of the "what we want" 
*  Frame. Allow the user to over-ride these limits. 
         CALL KPG1_GTAXV( 'XLEFT', FSET, 1, BL( 1 ), STATUS )
         CALL KPG1_GTAXV( 'XRIGHT', FSET, 1, TR( 1 ), STATUS )

*  Find the corresponding range of GRID indices. Use an arbitrary value
*  of 0.0 for the Y axis when doing this Mapping (any Y value will do since
*  the axes are independent).
         BL( 2 ) = 0.0D0
         TR( 2 ) = 0.0D0
         CALL AST_TRANN( MAP, 1, 2, 1, BL, .FALSE., 2, 1, BLG, STATUS ) 
         CALL AST_TRANN( MAP, 1, 2, 1, TR, .FALSE., 2, 1, TRG, STATUS ) 
         ILO = MAX( 1, MIN( DIM, INT( MIN( BLG( 1 ), TRG( 1 ) ) ) ) )
         IHI = MAX( 1, MIN( DIM, INT( 0.5D0 + 
     :                                MAX( BLG( 1 ), TRG( 1 ) ) ) ) )

*  Find the limits of the Y axis (axis 2) data value in the "what we want"
*  Frame, including any required error bars. This returns the central and
*  extreme Y axis values for each error bar in the "what we want" Frame.
*  The returned limits only include the data in the grid index range 
*  covered by the X axis.
         CALL KPS1_LPLLM( EL, ILO, IHI, %VAL( IPYDAT ), YVAR, .FALSE.,
     :                    %VAL( IPYVAR ), YSIGMA, AST__NULL, MAP, 2, 
     :                    %VAL( IPYCEN ), %VAL( IPYBAR ), TR( 2 ), 
     :                    BL( 2 ), MONO, BAD, STATUS )

*  Report an error if either limit is bad.
         IF( ( TR( 2 ) .EQ. AST__BAD .OR. BL( 2 ) .EQ. AST__BAD ) .AND.
     :       STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'LINPLOT_ERR5', 'No valid data values found.',
     :                     STATUS )
         END IF

*  Expand the default Y limits by 3 percent at each end.
         DVAL = TR( 2 ) - BL( 2 )
         TR( 2 ) = TR( 2 ) + 0.03*DVAL
         BL( 2 ) = BL( 2 ) - 0.03*DVAL

*  Ensure the limits are not equal.
         IF( BL( 2 ) .EQ. TR( 2 ) ) THEN
            IF( BL( 2 ) .NE. 0.0 ) THEN
               TR( 2 ) = 2.0*BL( 2 )
            ELSE
               TR( 2 ) = 1.0D0
            END IF
         END IF

*  We now have the limits of the data on the Y axis of the "what we want" 
*  Frame. Allow the user to over-ride these limits. 
         CALL KPG1_GTAXV( 'YBOT', FSET, 2, BL( 2 ), STATUS )
         CALL KPG1_GTAXV( 'YTOP', FSET, 2, TR( 2 ), STATUS )

*  Map these positions into the Base (uniform) Frame.
         CALL AST_TRANN( FSET, 1, 2, 1, BL, .FALSE., 2, 1, BLG, STATUS ) 
         CALL AST_TRANN( FSET, 1, 2, 1, TR, .FALSE., 2, 1, TRG, STATUS ) 

*  See if a key giving the start and end positions of the profile is
*  required. Default is yes if the NDF has more than 1 Curretn Frame axis.
         CALL PAR_DEF0L( 'KEY', ( NAX .GT. 1 ), STATUS )
         CALL PAR_GET0L( 'KEY', KEY, STATUS )

*  See if annotated axes are required. The default is yes.
         CALL PAR_DEF0L( 'AXES', .TRUE., STATUS )
         CALL PAR_GET0L( 'AXES', AXES, STATUS )

*  Get the margin values, using a dynamic default of zero if no key or
*  axes are being created (to avoid the unnecessary creation of FRAME 
*  pictures by KPG1_PLOTP), and 0.18 otherwise.
         IF( .NOT. KEY .AND. .NOT. AXES ) THEN
            CALL PAR_DEF1R( 'MARGIN', 1, 0.0, STATUS )
         ELSE
            CALL PAR_DEF1R( 'MARGIN', 1, 0.18, STATUS )
         END IF

         CALL PAR_GDRVR( 'MARGIN', 4, -0.49, 10.0, MARGIN, NMARG,
     :                    STATUS )
         NMARG = MIN( 4, NMARG )

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Use the first value for any unspecified edges.
         DO I = NMARG + 1, 4      
            MARGIN( I ) = MARGIN( 1 )
         END DO

*  Create a new DATA picture. Make it as large as possible within the
*  bounds of the current AGI picture, and give it the AGI world co-ordinate 
*  bounds determined above. Also create a KEY picture if necessary. The
*  PGPLOT viewport is changed to represent the new DATA picture.
         BOX( 1 ) = BLG( 1 )
         BOX( 2 ) = BLG( 2 )
         BOX( 3 ) = TRG( 1 )
         BOX( 4 ) = TRG( 2 )

         IF( KEY ) THEN
            CALL KPG1_PLOTP( -1, 'LINPLOT', MARGIN, 1, 'KEY', 'B', 0.2, 
     :                       0.0, BOX, IPICD, IPICF, IPICK, STATUS )
         ELSE
            CALL KPG1_PLOTP( -1, 'LINPLOT', MARGIN, 0, ' ', ' ', 0.0, 
     :                       0.0, BOX, IPICD, IPICF, IPICK, STATUS )
         END IF

*  Create a Plot covering this viewport.
         CALL KPG1_ASPLT( FSET, BOX, ' ', IPLOT, STATUS )

*  Save the indices within the Plot of the "what we've got" and "what we 
*  want" Frames relating to the original NDF.
         WWGOT = WWGOT + 1
         WWWANT = WWWANT + 1

*  Save the index of its current Frame.
         ICURR0 = AST_GETI( IPLOT, 'CURRENT', STATUS )

      END IF

*  We now have a Plot and a new DATA picture. Prepare to produce the
*  graphical output.
*  =================================================================
*  Set the attributes of the Plot to give the required Plotting style.     
      CALL KPG1_ASSET( 'KAPPA_LINPLOT', 'STYLE', IPLOT, STATUS )

*  Generate a reference for the NDF to be stored in the graphics
*  database.
      CALL NDF_MSG( 'NDF', INDF )
      CALL MSG_LOAD( ' ', '^NDF', NDFNAM, NC, STATUS )

*  Save the Plot and data reference with the new DATA picture. The Frame
*  with index ICURR0 is made the current Frame for the Plot in AGI database.
      CALL KPG1_PLOTS( IPLOT, IPICD, NDFNAM( : NC ), ICURR0, STATUS )

*  If the user did not specify a Plot title (as indicated by the Plot title
*  being the same as the FSET title), make the NDF Title the default Title for 
*  the Plot. 
      IF( AST_GETC( FSET, 'TITLE', STATUS ) .EQ. 
     :    AST_GETC( IPLOT, 'TITLE', STATUS ) ) THEN

         TEXT = ' '
         CALL NDF_CGET( INDF, 'TITLE', TEXT, STATUS ) 

         IF( TEXT .NE. ' ' ) THEN
            CALL AST_SETC( IPLOT, 'TITLE', TEXT( : CHR_LEN( TEXT ) ), 
     :                     STATUS )
         END IF

      END IF

*  See if annotated axes are required. The default is YES unles we are 
*  drawing over an existing DATA picture.
      CALL PAR_DEF0L( 'AXES', .NOT. NEWPIC, STATUS )
      CALL PAR_GET0L( 'AXES', AXES, STATUS )

*  Produce the plot.
*  =================
*  Draw the grid if required.
      IF( AXES ) CALL KPG1_ASGRD( IPLOT, IPICF, .TRUE., STATUS )

*  Get the 1-D mappings which transform each of the GRAPHICS Frame axes
*  onto the corresponding "what we want" Frame axes.
      CALL KPG1_ASSPL( IPLOT, 2, AXMAPS, STATUS )

*  Map all the required axis values from "what we want" into GRAPHICS.
      CALL AST_TRAN1( AXMAPS( 1 ), DIM, %VAL( IPXCEN ), .FALSE., 
     :                %VAL( IPXCEN ), STATUS ) 

      CALL AST_TRAN1( AXMAPS( 2 ), DIM, %VAL( IPYCEN ), .FALSE., 
     :                %VAL( IPYCEN ), STATUS ) 

      IF( XVAR ) CALL AST_TRAN1( AXMAPS( 1 ), 2*DIM, %VAL( IPXBAR ), 
     :                           .FALSE., %VAL( IPXBAR ), STATUS ) 


      IF( YVAR ) CALL AST_TRAN1( AXMAPS( 2 ), 2*DIM, %VAL( IPYBAR ), 
     :                           .FALSE., %VAL( IPYBAR ), STATUS ) 

      IF( IMODE .EQ. 4 ) CALL AST_TRAN1( AXMAPS( 1 ), 2*DIM, 
     :                                       %VAL( IPSTEP ), .FALSE., 
     :                                       %VAL( IPSTEP ), STATUS ) 

*  Produce the data plot.
      CALL KPG1_PLTLN( DIM, ILO, IHI, %VAL( IPXCEN ), %VAL( IPYCEN ), 
     :                 XVAR, YVAR, %VAL( IPXBAR ), %VAL( IPYBAR ), 
     :                 %VAL( IPSTEP ), 'STYLE', IPLOT, IMODE, MTYPE, 
     :                 ISHAPE, FREQ, 'LINPLOT', STATUS )

*  Produce the Key (but only if we have not aligned the picture with an
*  existing DATAPLOT.
*  ====================================================================
      IF( KEY .AND. AXES .AND. STATUS .EQ. SAI__OK ) THEN

*  Get the bounds of the PGPLOT window (equivalent to the bounds of the 
*  DATA picture in the GRAPHICS Frame).
         CALL PGQWIN( X1, X2, Y1, Y2 )

*  If the horizontal axis is annotated with distance from the centre of
*  the first grid element in the supplied NDF, then the key indicates the 
*  position of the first and last grid elements within the n-D current 
*  Frame of the input NDF. Store the starting an ending GRID co-ordinates.
         IF( DIST ) THEN
            BLG( 1 ) = 1.0D0
            TRG( 1 ) = DBLE( DIM )

*  If the horizontal axis is annotated with value on a specified axis
*  then the key indicates the position of each end of the displayed
*  section of the Plot within the n-D current Frame of the input NDF.
         ELSE

*  Get the 2-d Mapping from GRAPHICS Frame to "What we've got" in the
*  supplied NDF.
            MAP = AST_SIMPLIFY( AST_GETMAPPING( IPLOT, AST__BASE, WWGOT, 
     :                                          STATUS ),
     :                          STATUS )

*  Find the corresponding GRID co-ordinates. Use an arbitrary value of 0.0 
*  for the Y axis when doing this Mapping (any Y value will do since
*  the axes are independent).
            BL( 1 ) = DBLE( X1 )
            BL( 2 ) = 0.0D0
            TR( 1 ) = DBLE( X2 )
            TR( 2 ) = 0.0D0
  
            CALL AST_TRANN( MAP, 1, 2, 1, BL, .TRUE., 2, 1, BLG, 
     :                      STATUS ) 
            CALL AST_TRANN( MAP, 1, 2, 1, TR, .TRUE., 2, 1, TRG, 
     :                      STATUS ) 

         END IF

*  Find the position in the current Frame of the supplied NDF corresponding
*  to these GRID co-ordinates.
         CALL AST_TRANN( WCSMAP, 1, 1, 1, BLG( 1 ), .TRUE., NAX, 1, ST, 
     :                   STATUS ) 
         CALL AST_TRANN( WCSMAP, 1, 1, 1, TRG( 1 ), .TRUE., NAX, 1, FN, 
     :                   STATUS ) 

*  Note the X value at left hand edge of the DATA picture. The key text
*  will be left justified here.
         TXTPOS( 1 ) = DBLE( X1 )

*  Activate the KEY picture. This returns a Plot for drawing in the
*  picture. 
         CALL KPG1_GDGET( IPICK, AST__NULL, .FALSE., IPLOTK, STATUS )

*  Add a copy of the current Frame from the NDF into the Plot so that
*  its axis attributes (Format, Label, etc) are available when formatting
*  the start and finish positions. The new Frame becomes the current Frame 
*  in the Plot.
         DO I = 1, NDF__MXDIM
            PERM( I ) = 1
         END DO

         CALL AST_ADDFRAME( IPLOTK, AST__BASE, 
     :                      AST_PERMMAP( 2, PERM, NAX, PERM, 0.0D0, 
     :                                   ' ', STATUS ),
     :                      AST_GETFRAME( IWCS, AST__CURRENT, STATUS ),
     :                      STATUS )

*  Set the style for plotting in the key picture. 
         CALL KPG1_ASSET( 'KAPPA_LINPLOT', 'KEYSTYLE', IPLOTK, STATUS )

*  Format the text for the first line of the key.
         KEYLN1 = 'From:  '
         IAT = 7
         CALL KPG1_ASPTP( IPLOTK, NAX, ST, .TRUE., '   ', KEYLN1, IAT, 
     :                    STATUS )

*  Format the text for the second line of the key.
         KEYLN2 = 'To:     '
         IAT = 8
         CALL KPG1_ASPTP( IPLOTK, NAX, FN, .TRUE., '   ', KEYLN2, IAT,
     :                    STATUS )

*  It's easier to specify positions in GRAPHICS co-ordinates (mm from
*  bottom left screen corner) since this is the PGPLOT world co-ordinate
*  system. Make the Base Frame the current Frame.
         CALL AST_SETI( IPLOTK, 'CURRENT', AST_GETI( IPLOTK, 'BASE',
     :                                               STATUS ), STATUS )

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Find the bounds of the picture in PGPLOT world co-ordinates, and store
*  the Y value at the centre of the top half of the picture.
         CALL PGQWIN( X1, X2, Y1, Y2 )
         TXTPOS( 2 ) = DBLE( 0.25*Y1 + 0.75*Y2 )

*  Get the current PGPLOT character heights in world coordinates.
         CALL PGQCS( 4, XCH, HGT )

*  Set the line spacing.
         LNSP = 1.2*HGT*AST_GETR( IPLOTK, 'SIZE(TEXT)', STATUS )

*  Use horizontal text.
         UP( 1 ) = 0.0D0
         UP( 2 ) = 1.0D0

*  Display the first line of text left justified within the top half of the 
*  key picture.
         CALL AST_TEXT( IPLOTK, KEYLN1, TXTPOS, UP, 'CL', STATUS )

*  Display the second line of text left justified on line space down.
         TXTPOS( 2 ) = TXTPOS( 2 ) - LNSP
         CALL AST_TEXT( IPLOTK, KEYLN2, TXTPOS, UP, 'CL', STATUS )

      END IF

*  Shutdown procedure.
*  ===================
 999  CONTINUE

*  Free any memory used.
      IF( FRSTEP ) CALL PSX_FREE( IPSTEP, STATUS )
      IF( FRXBAR ) CALL PSX_FREE( IPXBAR, STATUS )
      IF( FRXCEN ) CALL PSX_FREE( IPXCEN, STATUS )
      IF( FRYBAR ) CALL PSX_FREE( IPYBAR, STATUS )
      IF( FRYCEN ) CALL PSX_FREE( IPYCEN, STATUS )

*  Shutdown PGPLOT and the graphics database.
      CALL ERR_BEGIN( STATUS )
      CALL AGP_DEASS( 'DEVICE', .FALSE., STATUS )
      CALL ERR_END( STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Add a context report if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'LINPLOT_ERR6', 'LINPLOT: Failed to display a '//
     :                 'plot of a 1-dimensional data set.', STATUS )
      END IF

      END
