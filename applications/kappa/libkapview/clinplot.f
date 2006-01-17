      SUBROUTINE CLINPLOT( STATUS )
*+
*  Name:
*     CLINPLOT

*  Purpose:
*     Draws a spatial grid of line plots for an axis of a cube NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation
*     CALL CLINPLOT( STATUS )

*  Description:
*     This application displays a three-dimensional NDF as a series
*     of line plots of array value against position, arranged on a 
*     uniform spatial grid and plotted on the current graphics device.
*     The vertical axis of the plot represents array value, and the 
*     horizontal axis represents position along a chosen axis (see
*     parameter USEAXIS).  All the line plots have the same axis limits.
*     The values can be mapped in various ways on to the graphics 
*     surface (e.g. linearly, logarithmically); see parameters XMAP and
*     YMAP).

*     Each line plot is drawn in a box whose location and extent
*     corresponds to a spatial pixel.  It is like a magnified image
*     display, but instead of a pixel being a solid block of colour,
*     a spectrum is plotted.  Thus, for convenience, this documentation 
*     sometimes refers to the horizontal axis of the line plots as the
*     spectral axis; while this will often be the case, this application
*     is not restricted to plotting spectra.  Likewise the grid of line 
*     plots is also referred to as the image.
*
*     Annotated axes for the spatial co-ordinates may be drawn around 
*     the grid of line plots (see parameter EXTAXES).  The appearance of
*     these and the space they occupy may be controlled in detail (see
*     parameters STYLE and MARGIN).  

*     The image is produced within the current graphics database
*     picture.  The co-ordinates at the centre of the image, and the
*     scale of the image can be controlled using parameters CENTRE,
*     XMAGN and YMAGN.  Only the parts of the image that lie within the
*     current picture are visible; the rest is clipped.

*     Each line plot may take several different forms such as a
*     "join-the-dots" plot, a "staircase" plot, a "chain" plot, (see
*     parameter MODE).  Errors on both the data values and the data
*     positions may be represented in several different ways (see 
*     parameters ERRBAR and SHAPE).  The plotting style (colour, founts,
*     text size, etc) may be specified in detail using parameter 
*     LPSTYLE.  The lower-left plot has by default annotated axes, whose
*     style is controlled through parameter REFSTYLE.
*
*     The bounds of the plot on both axes can be specified using
*     parameters XLEFT, XRIGHT, YBOT and YTOP.  If not specified they
*     take default values which encompass the entire supplied data set.
*     The defaults for YBOT and YTOP can be selected in several ways
*     including percentiles (see parameter LMODE).
*
*     The current picture is usually cleared before plotting the new
*     picture, but parameter CLEAR can be used to prevent this, allowing
*     several plots to be `stacked' together.  If a new plot is drawn
*     over an existing plot, then there is an option to allow the new
*     plot to be aligned with the existing plot (see parameter ALIGN).
*

*  Usage:
*     clinplot ndf [comp] [mode] [xleft] [xright] [ybot] [ytop] [device]

*  ADAM Parameters:
*     ALIGN = _LOGICAL (Read)
*        Controls whether or not the new plot grid should be aligned 
*        with an existing grid plot.  If ALIGN is TRUE, the x-axis 
*        values of the new plot will be mapped into the co-ordinate 
*        system of the x axis in the existing plot before being used (if
*        this is not possible an error is reported).  In this case, the 
*        XLEFT,  XRIGHT, YBOT, and YTOP parameters are ignored and the
*        bounds of the existing plots are used instead.  If ALIGN is 
*        FALSE, the new x-axis values are used without change.  The 
*        bounds of each line plot are specified using parameters XLEFT,
*        XRIGHT, YBOT, and YTOP as usual, and these bounds are mapped
*        to the edges of the existing picture.  The ALIGN parameter is 
*        only accessed if parameter CLEAR is FALSE, and if there is 
*        another grid of line plots within the current picture.  
*
*        If a null (!) value is supplied, a value of TRUE will be used
*        if and only if a mapping can be found between the existing and 
*        the new plots.  A value of FALSE will be used otherwise. [!]
*     AXES = _LOGICAL (Read)
*        TRUE if labelled and annotated axes are to be drawn around the
*        line plots.  If a null (!) value is supplied, the value used
*        is FALSE regardless of whether the plot is being aligned with 
*        an existing plot (see parameter ALIGN) or not.  In general
*        axes only clutter the plots, although interior tick marks
*        can help read values.  Annotated axes are normally drawn
*        about the exterior axes of the lower-left plot through the
*        REFAXES parameter.
*
*        Parameter USEAXIS determines the quantity used to annotate the
*        horizontal axis.  The width of the margins left for the 
*        annotation may be controlled using parameter MARGIN.  The
*        appearance of the axes (colours, founts, etc.) can be 
*        controlled using the parameter LPSTYLE. [!]
*     CENTRE = LITERAL (Read)
*        The co-ordinates of the data pixel to be placed at the centre
*        of the grid of line plots, in the current co-ordinate Frame of
*        the NDF (supplying a colon ":" will display details of the 
*        current co-ordinate Frame).  The position should be supplied as
*        a list of formatted axis values separated by spaces or commas. 
*        See also parameter USEAXIS.  A null (!) value causes the centre
*        of the spatial image to be used. [!]
*     CLEAR = _LOGICAL (Read)
*        If TRUE the current picture is cleared before the plot is 
*        drawn.  The initial default is TRUE.  If CLEAR is FALSE not
*        only is the existing plot retained, but also the previous plot
*        can be used to specify the axis limits (see parameter ALIGN). 
*        Thus you can generate a composite plot within a single set of
*        axes, say using different colours or modes to distinguish the
*        different datasets. 
*
*        Note, alignment between the two plots is controlled by the
*        AlignSystem attribute of the data being displayed.  For
*        instance, if you have an existing plot showing the spectra in a
*        cube plotted against radio velocity and you overlay the
*        spectra from another cube, also in radio velocity but with a 
*        different rest frequency, the appearance of the final plot will
*        depend on the value of the AlignSystem attribute of the second
*        spectrum.  If AlignSystem is "Wavelen" (this is the default),
*        then the two spectra will be aligned in wavelength, but if
*        AlignSystem is "vrad" they will be aligned in radio velocity. 
*        There will be no difference in effect between these two forms
*        of alignment unless the rest frequency is different in the two
*        cubes.  Likewise, the AlignStdOfRest attribute of the second
*        cube's spectral axis controls the standard of rest in which
*        alignment occurs.  These attributes (like all other attributes)
*        may be examined and modified using WCSATTRIB.  [current value]
*     COMP = LITERAL (Read)
*        The NDF array component to be displayed.  It may be "Data",
*        "Quality", "Variance", or "Error" (where "Error" is an
*        alternative to "Variance" and causes the square root of the
*        variance values to be displayed).  If "Quality" is specified,
*        then the quality values are treated as numerical values (in
*        the range 0 to 255). ["Data"]
*     DEVICE = DEVICE (Read)
*        The name of the graphics device used to display the cube.
*        [current graphics device]
*     ERRBAR = _LOGICAL (Read)
*        TRUE if error bars are to be drawn in the line plots.  The
*        error bars can comprise either or both of the data and
*        axis-centre errors, depending on what is available in the
*        supplied dataset.  The parameter SHAPE controls the appearance
*        of the error bars, and XSIGMA and YSIGMA control their lengths.
*        The ERRBAR parameter is ignored unless the parameter COMP is
*        set to "Data".  The initial default is FALSE.  [current value]
*     EXTAXES = _LOGICAL (Read)
*        TRUE if labelled and annotated axes are to be drawn around the
*        spatial grid (i.e. external axes).  These display co-ordinates
*        in the current co-ordinate Frame of the supplied NDF, and may
*        be changed using application WCSFRAME (see also parameter 
*        USEAXIS).  The width of the margins left for the annotation may
*        be controlled using parameter MARGIN.  The appearance of the 
*        axes (colours, founts, etc.) can be controlled using the 
*        STYLE parameter.  [TRUE]
*     FILL = _LOGICAL (Read)
*        If FILL is set to TRUE, then the image will be `stretched' to 
*        fill the current picture in both directions.  This can be 
*        useful to elongate the spectra to reveal more detail by using
*        more of the display surface at the cost of different spatial
*        scales, and when the spatial axes have markedly different
*        dimensions.  The dynamic default is TRUE if either of the
*        spatial diensions is one. and FALSE otherwise. []
*     FREQ = _INTEGER (Read)
*        The frequency at which error bars are to be plotted.  For
*        instance, a value of 2 would mean that alternate points have
*        error bars plotted.  This lets some plots be less cluttered.
*        FREQ must lie in the range 1 to half of the number of points
*        to be plotted.  FREQ is only accessed when parameter ERRBAR is
*        TRUE.  [1]
*     LMODE = LITERAL (Read)
*        LMODE specifies how the defaults for parameters YBOT and YTOP
*        (the  lower and upper limit of the vertical axis of the plot) 
*        should be found.  The supplied string should consist of up to 
*        three sub-strings, separated by commas.  The first sub-string 
*        must specify the method to use.  If supplied, the other two 
*        sub-strings should be numerical values as described below 
*        (default values will be used if these sub-strings are not 
*        provided).  The following methods are available.
*
*        - "Range" -- The minimum and maximum data values (including any
*        error bars) are used as the defaults for YBOT and YTOP.  No
*        other sub-strings are needed by this option.
*
*        - "Extended" -- The minimum and maximum data values (including
*        error bars) are extended by percentages of the data range,
*        specified by the second and third sub-strings.  For instance, 
*        if the value "Ex,10,5" is supplied, then the default for YBOT
*        is set to the minimum data value minus 10% of the data range, 
*        and the default for YTOP is set to the maximum data value plus 
*        5% of the data range.  If only one value is supplied, the
*        second value defaults to the supplied value.  If no values are
*        supplied, both values default to "2.5".  Care should be taken
*        with this mode if YMAP is set to "Log" since the extension to
*        the data range caused by this mode may result in the axis
*        encompassing the value zero.
*
*        - "Percentile" -- The default values for YBOT and YTOP are set 
*        to  the specified percentiles of the data (excluding error
*        bars).  For  instance, if the value "Per,10,99" is supplied, 
*        then the default for YBOT is set so that the lowest 10% of the
*        plotted points are  off the bottom of the plot, and the default
*        for YTOP is set so that the highest 1% of the points are off 
*        the top of the plot.  If only one value, p1, is supplied, the
*        second value, p2, defaults to (100 - p1).  If no values are 
*        supplied, the values default to "5,95".
*
*        - "Sigma" -- The default values for YBOT and YTOP are set to
*        the specified numbers of standard deviations below and above 
*        the mean of the data.  For instance, if the value "sig,1.5,3.0"
*        is supplied, then the default for YBOT is set to the mean of
*        the data minus 1.5 standard deviations, and the default for
*        YTOP is set to the mean plus 3 standard deviations.  If only
*        one value is supplied, the second value defaults to the
*        supplied value.  If no values are provided both default to
*        "3.0".
*
*        The method name can be abbreviated to a single character, and
*        is case insensitive.  The initial value is "Extended". 
*        [current value]
*     LPMARGIN( 4 ) = _REAL (Read)
*        The widths of the margins to leave for axis annotation around
*        the line plots, given as fractions of a single plot for
*        a spatial pixel.  Four values may be given, in the order: 
*        bottom, right, top, left.  If fewer than four values are given,
*        extra values are used equal to the first supplied value.  If 
*        these margins are too narrow any axis annotation may be 
*        clipped.  If a null (!) value is supplied, the value used is 
*        0.5 (for all edges) if annotated axes are produced around the
*        grid, 0.1 if spatial axes are present (parameter AXES), and
*        0.0 otherwise.  The initial value is null.  [current value]
*     LPSTYLE = LITERAL (Read)
*        A group of attribute settings describing the plotting style to 
*        use when drawing the annotated axes, data values, and error
*        markers in the line plots.
*
*        A comma-separated list of strings should be given in which each
*        string is either an attribute setting, or the name of a text 
*        file preceded by an up-arrow character "^".  Such text files
*        should contain further comma-separated lists which will be read
*        and interpreted in the same manner.  Attribute settings are 
*        applied in the order in which they occur within the list, with
*        later settings overriding any earlier settings given for the
*        same attribute.
*
*        Each individual attribute setting should be of the form:
*
*           <name>=<value>
*        
*        where <name> is the name of a plotting attribute, and <value> 
*        is the value to assign to the attribute.  Default values will
*        be used for any unspecified attributes.  All attributes will
*        be defaulted if a null value (!)---the initial default---is
*        supplied.  See section "Plotting Attributes" in SUN/95 for a
*        description of the available attributes.  Any unrecognised 
*        attributes are ignored (no error is reported). 
*
*        The appearance of the data values is controlled by the 
*        attributes Colour(Curves), Width(Curves), etc. (the synonym 
*        Lines may be used in place of Curves).  The appearance of
*        markers used if parameter MODE is set to "Point", "Mark" or
*        "Chain" is controlled by Colour(Markers), Width(Markers), etc.
*        (the synonym Symbols may be used in place of Markers).  The 
*        appearance of the error symbols is controlled using
*        Colour(ErrBars), Width(ErrBars), etc. (see parameter SHAPE). 
*        [current value]
*     MARGIN( 4 ) = _REAL (Read)
*        The widths of the margins to leave around the image for axis 
*        annotations, given as fractions of the corresponding dimension 
*        of the current picture.  The actual margins used may be 
*        increased to preserve the aspect ratio of the data.  Four 
*        values may be given, in the order: bottom, right, top, left.
*        If fewer than four values are given, extra values are used 
*        equal to the first supplied value.  If these margins are too 
*        narrow any axis annotation may be clipped.  If a null (!) value
*        is supplied, the value used is (for all edges); 0.15 if 
*        annotated axes are being produced; and 0.0 otherwise.  The
*        initial default is null. [current value]
*     MARKER = _INTEGER (Read)
*        This parameter is only accessed if parameter MODE is set to
*        "Chain" or "Mark".  It specifies the symbol with which each
*        position should be marked, and should be given as an integer 
*        PGPLOT marker type.  For instance, 0 gives a box, 1 gives a 
*        dot, 2 gives a cross, 3 gives an asterisk, 7 gives a triangle.
*        The value must be larger than or equal to -31.  The initial
*        default is 11.  [current value]
*     MODE = LITERAL (Read)
*        Specifies the way in which data values are represented.  MODE
*        can take the following values:
*
*        - "Histogram" -- An histogram of the points is plotted in the
*        style of a "staircase" (with vertical lines only joining the y 
*        values and not extending to the base of the plot).  The
*        vertical lines are placed midway between adjacent x positions.
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
*        The input NDF structure containing the data to be displayed.
*        It should have three significant axes, i.e. whose dimensions
*        are greater than 1.
*     REFAXES = _LOGICAL (Read)
*        TRUE if labelled and annotated axes are to be drawn around the
*        bottom-left line plot that acts as a reference.  If AXES is
*        set TRUE, a cluttered plot can ensue, especially if the chosen
*        STYLE parameters include axis annotations.  The normal option
*        is to have annotated axes only for the lower-left line plot.
*        This parameter overrides the value of AXES for this special 
*        key plot.  If a null (!) value is supplied, the value used is 
*        FALSE if the plot is being aligned with an existing plot (see 
*        parameter ALIGN), and TRUE otherwise.  Parameter USEAXIS 
*        determines the quantity used to annotated the horizontal axis. 
*        The width of the margins left for the annotation may be 
*        controlled using parameter LPMARGIN.  The appearance of the 
*        axes (colours, founts, etc.) can be controlled using the 
*        parameter REFSTYLE. [!]
*     REFSTYLE = LITERAL (Read)
*        A group of attribute settings describing the plotting style to 
*        use when drawing the annotated axes in the bottom-left line 
*        plot.  It overrides the values supplied by the STYLE
*        parameter that is applicable to all the line plots.
*
*        A comma-separated list of strings should be given in which each
*        string is either an attribute setting, or the name of a text 
*        file preceded by an up-arrow character "^".  Such text files
*        should contain further comma-separated lists which will be read
*        and interpreted in the same manner.  Attribute settings are 
*        applied in the order in which they occur within the list, with
*        later settings overriding any earlier settings given for the
*        same attribute.
*
*        Each individual attribute setting should be of the form:
*
*           <name>=<value>
*        
*        where <name> is the name of a plotting attribute, and <value> 
*        is the value to assign to the attribute. Default values will be
*        used for any unspecified attributes.  All attributes will be
*        defaulted if a null value (!)---the initial default---is
*        supplied.  See section "Plotting Attributes" in SUN/95 for a
*        description of the available attributes.  Any unrecognised
*        attributes are ignored (no error is reported).
*
*        The application adjusts the text heights of the labels and
*        numerical annotations with the density of the plots.  So
*        try the default heights first before attempting any adjustments
*        with the Size(NumLab) and Size(TextLab) attributes.
*        [current value]
*     SHAPE = LITERAL (Read)
*        Specifies the way in which errors are represented.  SHAPE
*        can take the following values:
*
*        - "Bars" -- Bars with serifs (i.e. cross pieces at each end) 
*        are drawn joining the x error limits and the y error limits. 
*        The plotting attribute "Size(ErrBars)" (see parameter STYLE) 
*        can be used to control the size of these serifs (the attribute
*        value should be a magnification factor; 1.0 gives default 
*        serifs).
*
*        - "Cross" -- San-serif bars are drawn joining the x-error 
*        limits and the y-error limits.
*
*        - "Diamond" -- Adjacent error limits are joined to form an
*        error diamond.
*
*        The length of the error bars can be controlled using parameters
*        XSIGMA and YSIGMA.  The colour, line width and line style used
*        to draw them can be controlled using the plotting attributes 
*        "Colour(ErrBars)", "Width(ErrBars)" and "Style(ErrBars)" (see 
*        parameter STYLE).  SHAPE is only accessed when parameter ERRBAR
*        is TRUE.  The initial value is "Bars".  [current value]
*     STYLE = GROUP (Read)
*        A group of attribute settings describing the plotting style to
*        use for the annotated external spatial axes (see parameter
*        EXTAXES). 
*
*        A comma-separated list of strings should be given in which each
*        string is either an attribute setting, or the name of a text 
*        file preceded by an up-arrow character "^".  Such text files
*        should contain further comma-separated lists which will be 
*        read and interpreted in the same manner.  Attribute settings 
*        are applied in the order in which they occur within the list, 
*        with later settings over-riding any earlier settings given for 
*        the same attribute.
*
*        Each individual attribute setting should be of the form:
*
*           <name>=<value>
*        
*        where <name> is the name of a plotting attribute, and <value>
*        is the value to assign to the attribute.  Default values will
*        be used for any unspecified attributes.  All attributes will be
*        defaulted if a null value (!) is supplied.  See section 
*        "Plotting Attributes" in SUN/95 for a description of the 
*        available attributes.  Any unrecognised attributes are ignored 
*        (no error is reported). [current value] 
*     USEAXIS = LITERAL (Read)
*        The NDF axis that will appear along the abscissa of the line 
*        plots.  This can be specified by its integer index within the 
*        current Frame of the NDF (in the range 1 to 3 in the current 
*        Frame), or by its symbol string.  A list of acceptable values 
*        is displayed if an illegal value is supplied.  If the axes of 
*        the current Frame are not parallel to the NDF pixel axes, then 
*        the pixel axis which is most nearly parallel to the specified 
*        current Frame axis will be used.
*
*        The null (!) value requests that the spectral axis in a
*        SpecFrame be used.  If there is no SpecFrame present in the
*        NDF, the value 3 is used.
*
*        The remaining two significant axes are therefore deemed to be
*        spatial, the lower dimension corresponding to the horizontal
*        axis on the exterior spatial axes and the first value for 
*        parameter CENTRE.  
*
*        In some WCS Frames it may not be possible to display the data,
*        for instance, if you select USEAXIS to be one axis of a SKY
*        Frame.  In such cases switch to a different Frame, such as
*        PIXEL with application WCSFRAME to present the desired axis
*        in the line plots.  [!]
*     XLEFT = LITERAL (Read)
*        The axis value to place at the left-hand end of the horizontal
*        axis of each line plot.  If a null (!) value is supplied, the
*        value used is the value for the first element in the supplied
*        NDF (with a margin to include any horizontal error bar).  The
*        value supplied may be greater than or less than the value 
*        provided for XRIGHT.  A formatted value for the quantity
*        specified by parameter USEAXIS should be supplied.  See also
*        parameter ALIGN. [!]
*     XMAGN = _REAL (Read)
*        The horizontal magnification for the image.  The default
*        value of 1.0 corresponds to 'normal' magnification in which the
*        the image fills the available space in at least one dimension.
*        A value larger than 1.0 makes each data pixel wider.  If this
*        results in the image being wider than the available space then
*        the the image will be clipped to display fewer pixels.  See
*        also parameters YMAGN, CENTRE and FILL. [1.0]
*     XMAP = LITERAL (Read)
*        Specifies how the quantity represented by the line plot's x 
*        axis is mapped on to the plot.  The options are:
*
*        - "Pixel" -- The mapping is such that pixel index within the
*        input NDF increases linearly across the plot.
*
*        - "Distance" -- The mapping is such that distance along the 
*        curve within the current WCS Frame of the input NDF increases
*        linearly across the plot.
*
*        - "Log" -- The mapping is such that the logarithm (base 10) of
*        the value used to annotate the axis increases linearly across
*        the plot.  An error will be reported if the dynamic range of
*        the axis is less than 100, or if the range specified by XLEFT
*        and XRIGHT encompasses the value zero.
*
*        - "Linear" -- The mapping is such that the value used to
*        annotate the axis increases linearly across the plot.
*
*        - "Default" -- One of "Linear" or "Log" is chosen
*        automatically, depending on which one produces a more-even 
*        spread of values on the plot. 
*        ["Default"]
*     XRIGHT = LITERAL (Read)
*        The axis value to place at the right-hand end of the horizontal
*        axis of each line plot.  If a null (!) value is supplied, the 
*        value used is the value for the last element in the supplied
*        NDF (with a margin to include any horizontal error bar).  The 
*        value supplied may be greater than or less than the value 
*        provided for XLEFT.  A formatted value for the quantity
*        specified by parameter USEAXIS should be supplied.  See also
*        parameter ALIGN. [!]
*     XSIGMA = LITERAL (Read)
*        If horizontal error bars are produced (see parameter ERRBAR), 
*        then XSIGMA gives the number of standard deviations that the 
*        error bars are to represent.  The initial value is 1.0.
*        [current value]
*     YBOT = LITERAL (Read)
*        The axis value to place at the bottom end of the vertical
*        axis of each line plot.  If a null (!) value is supplied, the
*        value used is determined by parameter LMODE.  The value of YBOT
*        may be greater than or less than the value supplied for YTOP. 
*        If parameter YMAP is set to "ValueLog", then the supplied value
*        should be the logarithm (base 10) of the bottom data value.
*        See also parameter ALIGN. [!]
*     YMAGN = _REAL (Read)
*        The vertical magnification for the image.  The default
*        value of 1.0 corresponds to 'normal' magnification in which the
*        the image fills the available space in at least one dimension.
*        A value larger than 1.0 makes each data pixel taller.  If this
*        results in the image being taller than the available space then
*        the image will be clipped to display fewer pixels.  See also
*        parameters XMAGN, CENTRE and FILL.  If a null (!) value is
*        supplied, the value used is the value supplied for XMAGN. [!]
*     YMAP = LITERAL (Read)
*        Specifies how the quantity represented by the line plot's y 
*        axis is mapped on to the screen.  The options are:
*
*        - "Linear" -- The data values are mapped linearly on to the
*        screen.
*
*        - "Log" -- The data values are logged logarithmically on to the
*        screen.  An error will be reported if the dynamic range of
*        the axis is less than 100, or if the range specified by YTOP 
*        and YBOT encompasses the value zero.  For this reason, care 
*        should be taken over the choice of value for parameter LMODE,
*        since some choices could result in the y range being extended
*        so far that it encompasses zero. 
*
*        - "ValueLog" -- This is similar to "Log" except that, instead 
*        of mapping the data values logarithmically on to the screen,
*        this option maps the log (base 10) of the data values linearly 
*        on to the screen.  If this option is selected, the values 
*        supplied for parameters YTOP and YBOT should be values for the 
*        logarithm of the data value, not the data value itself. 
*        ["Linear"]
*     YSIGMA = LITERAL (Read)
*        If vertical error bars are produced (see parameter ERRBAR), 
*        then YSIGMA gives the number of standard deviations that the 
*        error bars are to represent.  The initial value is 1.0.
*        [current value]
*     YTOP = LITERAL (Read)
*        The axis value to place at the top end of the vertical axis for
*        each line plot.  If a null (!) value is supplied, the value
*        used is determined by parameter LMODE.  The value of LTOP may
*        be greater than or less than the value supplied for YBOT.  If
*        parameter YMAP is set to "ValueLog", then the supplied value
*        should be the logarithm (base 10) of the bottom data value.
*        See also parameter ALIGN. [!]

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Examples:
*     clinplot cube useaxis=3
*        Plots a set of line plots of data values versus position
*        along the third axis for the whole of the three-dimensional
*        NDF called cube on the current graphics device.  Axes are drawn
*        around the grid of plots indicating the spatial positions in
*        the current co-ordinate Frame.  The third axis may not be
*        spectral and the otther two axes need not be spatial.
*     clinplot cube margin=0.1
*        As above, but if a search locates a spectral axis in the
*        world co-ordinate system, this is plotted along the horizontal
*        of the line plots, and the other axes are deemed to be spatial.
*        The spectral axis is specified either by a WCS Domain.  Also
*        the margin for the spatial axes is reduced to 0.1 to allow more
*        room for the grid of line plots.
*     clinplot map(~5,~5,) useaxis=3 noextaxes
*        Plots data values versus position for the central 5-by-5 pixel
*        region of the three-dimensional NDF called map on the current
*        graphics device.  No spatial axes are drawn.
*     clinplot map(~5,~5,) useaxis=3 noextaxes device=ps_l
*        As the previous example but now the output goes to a text file 
*        which can be printed on a PostScript printer.
*     clinplot nearc v style="'title=Ne Arc variance'" useaxis=1 
*              refaxes=f
*        Plots variance values versus position along axis 1, for each
*        spatial pixel in dimensions two and three, for the whole of the
*        three-dimensional NDF called nearc on the current graphics 
*        device.  The plot has a title of "Ne Arc variance".  No
*        annotated axes are drawn around the lower-left line plot.
*     clinplot nebula useaxis=offsetvel xleft=-10 xright=30 lpmargin=0.3
*        This plots data values versus radial velocity for those
*        elements of the three-dimensional NDF called nebula with
*        velocity values between -10 and 30.  This assumes that the
*        current co-ordinate Frame in the NDF has an axis with symbol
*        "offsetvel".  Set the margins around the grid to be 0.3 of
*        the dimensions of one of the line plots.
*     clinplot ngc3504 useaxis=2 ybot=10 ytop=1000.0 ymap=log xmap=log
*        This plots the data values in the entire three-dimensional NDF
*        called ngc3504, against the value described by the second axis
*        in the current co-ordinate Frame of the NDF at each pixel in
*        the first and third axes.  The values represented by both 
*        spectral axes are mapped logarithmically on to the screen.  The
*        bottom of each vertical axis corresponds to a data value of
*        10.0 and each top corresponds to a data value of 1000.0.
*     clinplot speccube mode=p errbar xsigma=3 ysigma=3 shape=d 
*              lpstyle=^my_sty 
*        This plots the data values versus position at ech spatial
*        position for the dataset called speccube.  Each spectral pixel
*        is plotted as a point surrounded by diamond-shaped error bars,
*        indicating 3-sigma errors.  The line-plot plotting style is
*        read from text file my_sty.  This could, for instance, contain
*        strings such as: colour(err)=pink, colour(sym)=red, tickall=0.
*        These cause the error bars to be drawn in pink, the points to
*        be drawn in red, and tick marks to be restricted to the
*        labelled edges of the plot.
*     clinplot ndf=speccube noclear align lpstyle="colour(curves)=blue"
*        Plots data values versus pixel co-ordinate at each spatial
*        position for the whole of the three-dimensional NDF called 
*        speccube on the current graphics device.  The plot is drawn
*        over any existing plot and inherits the bounds of the previous
*        plot on all axes.  A warning will be reported if the labels
*        for the line plot horizontal axes or either spatial axis of the
*        two plots are different.  The data are drawn in blue, probably
*        to distinguish it from the previous plot drawn in a different
*        colour.
*     clinplot speccube system='system(1)=freq,unit(1)=GHz'
*        This example assumes that the current co-ordinate Frame of NDF 
*        speccube is a SpecFrame.  The horizontal axis (axis "1") is 
*        labelled with frequency values, in units of GHz.  If the 
*        SpecFrame represents some other system (such as wavelength, 
*        velocity, or energy), or has some other units, then the 
*        conversion is done automatically.  Note, a SpecFrame is a 
*        specialised class of Frame which knows how to do these 
*        conversions; the above command will fail if the current
*        co-ordinate Frame in the NDF is a simple Frame (such as the
*        AXIS Frame).  A SpecFrame can be created from an AXIS Frame 
*        using application WCSADD.
*     clinplot taurus centre="12:23:34 -22:12:23" xmagn=2 accept
*        Displays the data values versus spectral position for the NDF
*        called taurus, for spatial pixels centred on the position 
*        RA=12:23:34, DEC=-22:12:23.  This assumes that the current
*        co-ordinate Frame in the NDF is an equatorial (RA/DEC) Frame.
*        The image is displayed with a magnification of 2 so that each
*        data pixel appears twice as large (on each axis) as normal. 
*        Fewer data pixels may be displayed to ensure the image fits 
*        within the available space in the current picture.

*  Notes:
*     -  For large cubes or spatial sections, the resolution of the 
*     graphics device may allow only a fraction of the detail in the 
*     data to be plotted.   So only small sections are recommended. 
*     -  The Title component in the NDF is used as the default title 
*     for the exterior annotated axes.  If the NDF does not have a Title
*     component, then the default title is taken from current 
*     co-ordinate Frame in the NDF.  This default may be overridden by 
*     specifying a value for the Title attribute using the STYLE
*     parameter. 
*     -  If all the data values at a spatial position are bad, no line
*     plot is drawn at that location.  Where the combination of CENTRE,
*     XMAGN, and YMAGN parameter values require image padding, a gap 
*     will therefore be present in the grid of plots.
*     -  Some combinations of CENTRE, XMAGN, YMAGN, and LPMARGIN values
*     will cause the line plots' true spatial positions and extents to
*     appear slightly displaced.  However, the true spatial co-ordinates
*     of a given spectrum always lies within the boundaries of its line
*     plot, and they are recorded in the AGI picture comment.
*     -  By default the lower-left plot only has annotated axes at its
*     foot and left side.  Therefore a DSBSPECTRUM Domain upper sideband
*     will only be drawn if the visible grid has one row.
*     -  The application stores a number of pictures in the graphics
*     database in the following order: a FRAME picture containing the 
*     annotated spatial axes and line plot grid and its margins; 
*     a DATA picture containing just the plot-grid area; and one DATA
*     pictures for each spatial pixel plotted.  Note, the FRAME picture
*     is only created if annotated axes have been drawn, or if non-zero
*     margins were specified using parameter MARGIN.  If the last
*     visible plot is not at the lowest-left spatial pixel, there will
*     be a duplicate DATA picture for the last visible plot, the
*     second being enclosed in a FRAME picture.
*
*     The world co-ordinates in the first DATA picture will be pixels,
*     and millimetres from the lower-left of the graphics surface for 
*     the grid DATA pictures.  A reference to the supplied NDF, together
*     with a copy of the WCS information in the NDF are stored with
*     every DATA picture.  The comment associated with each grid DATA
*     pictures is a list of the formatted spatial co-ordinates in the 
*     current WCS Frame. 
*
*     On exit the current database picture for the chosen device
*     reverts to the input picture.

*  Related Applications:
*     KAPPA: DISPLAY, LINPLOT, MLINPLOT; Figaro: SPECGRID; SPLAT.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     VARIANCE, LABEL, TITLE, WCS and UNITS components of the input NDF.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  This application will handle data in all numeric types, though
*     type conversion to integer will occur for unsigned byte and word
*     images.  However, when there is no scaling only integer data will
*     not be type converted, but this is not expensive for the expected
*     byte-type data.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)      
*     {enter_new_authors_here}

*  History:
*     2005 December 15 (MJC):
*        Original version based upon DISPLAY and LINPLOT.
*     2005 December 20 (MJC):
*        Duplicate the co-ordinate array into KPG1_GRLM2 to match the
*        number of data elements.  Place annotated axes around the last
*        _visible_ plot.
*     2005 December 21 (TIMJ):
*        Minor tweak in XMAGN determination for g95 (both args to
*        MAX() must be the same type).
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Global SSE definitions
      INCLUDE 'NDF_PAR'        ! NDF_ constants
      INCLUDE 'PRM_PAR'        ! NUM_ constants
      INCLUDE 'DAT_PAR'        ! DAT__ constants
      INCLUDE 'AST_PAR'        ! AST constants and function declarations
      INCLUDE 'PAR_ERR'        ! Parameter-system error definitions
      INCLUDE 'PAR_PAR'        ! Parameter-system constants
      INCLUDE 'CNF_PAR'        ! For CNF_PVAL function

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN          ! Used length of a string
      INTEGER KPG1_CEIL        ! Smallest integer .GE. a given floating 
                               ! value
      INTEGER KPG1_FLOOR       ! Largest integer .LE. a given floating
                               ! value

*  Local Constants:
      INTEGER NDIM             ! Dimensionality required
      PARAMETER ( NDIM = 3 )
      INTEGER NSPDIM           ! Dimensionality of spatial plane
      PARAMETER ( NSPDIM = 2 )
      REAL NUMHT               ! NumLab height scale factor
      PARAMETER ( NUMHT = 0.8 )
      REAL NHTMIN              ! NumLab height minimum
      PARAMETER ( NHTMIN = 0.2 )
      REAL NHTMAX              ! NumLab height maximum
      PARAMETER ( NHTMAX = 0.65 )
      REAL TEXTHT              ! TextLab height scale factor
      PARAMETER ( TEXTHT = 0.9 )
      REAL THTMIN              ! TextLab height minimum
      PARAMETER ( THTMIN = 0.23 )
      REAL THTMAX              ! TextLab height maximum
      PARAMETER ( THTMAX = 0.72 )

*  Local Variables:
      INTEGER AEL              ! No. of elements in mapped co-ord array
      LOGICAL ALIGN            ! DATA picture aligned with a previous
                               ! picture?
      REAL ASP0                ! Aspect ratio of the available space
      REAL ASPD                ! Aspect ratio of the data array
      REAL ASPECT              ! Aspect ratio of the DATA picture
      LOGICAL AXES             ! Annotated axes are to be drawn?
      INTEGER AXMAP            ! Pointer to NDF's AXIS->GRID Mapping
      INTEGER AXMAPS( 2 )      ! Axis mappings for displayed data plot
      LOGICAL BAD              ! Bad pixels are present in the cube?
      LOGICAL BADAT            ! Bad attribute?
      INTEGER BFRM             ! Original Base-Frame pointer
      INTEGER BFRMI            ! Image Base-Frame pointer
      INTEGER BFRMS            ! Spectral Base-Frame pointer
      DOUBLE PRECISION BL( 2 ) ! "W.w. want" X/Y values at bottom-left 
                               ! corner
      INTEGER BLBND( NDIM + 1 ) ! Lower bounds of the error-bar array
      DOUBLE PRECISION BLG( 2 )! "Uniform" X/Y values at bottom-left 
                               ! corner
      DOUBLE PRECISION BOX( 4 )! Bounds of image in pixel co-ordinates
      INTEGER BUBND( NDIM + 1 ) ! Upper bounds of the error-bar array
      DOUBLE PRECISION CC( NSPDIM ) ! Current Frame co-ords at image 
                               ! centre
      INTEGER CFRM             ! Original current-Frame pointer
      INTEGER CFRMI            ! Image current-Frame pointer
      INTEGER CFRMS            ! Spectral current-Frame pointer
      LOGICAL CLEAR            ! Clear plotting surface?
      CHARACTER COMENT*( 42 )  ! Picture comment (42=2+XCOTXT+YCOTXT)
      CHARACTER COMP*( 8 )     ! Component to be displayed
      REAL DEFMAR              ! Default MARGIN value
      LOGICAL DEVCAN           ! Cancel DEVICE parameter?
      INTEGER DIMS( NDIM )     ! Dimensions of input array
      CHARACTER* ( 9 ) DOMAIN  ! Domain element
      INTEGER DPFS             ! FrameSet connecting old and new 
                               ! DATAPLOT Frames
      DOUBLE PRECISION DVAL    ! General double precision value
      INTEGER EL               ! Number of elements in the mapped array
      LOGICAL ERRBAR           ! Display error bars?
      INTEGER FAXES( NDF__MXDIM ) ! Frame axes of split mapping
      INTEGER FREQ             ! Interval between error bars
      LOGICAL FRSTEG           ! Free pointer to x-axis widths in
                               ! GRAPHICS Frame?
      LOGICAL FRSTEP           ! Free pointer to x-axis widths in "w.w.
                               ! want"?
      LOGICAL FRXBAG           ! Free pointer to x-error-bar limits in 
                               ! GRAPHICS Frame?
      LOGICAL FRXBAR           ! Free pointer to x-error-bar limits in 
                               ! "w.w. want"?
      LOGICAL FRXCEF           ! Free pointer to x centres in "w.w. 
                               ! want" Frame, duplicated?
      LOGICAL FRXCEG           ! Free pointer to x centres in GRAPHICS
                               ! Frame?
      LOGICAL FRXCEN           ! Free pointer to x centres in "w.w. 
                               ! want" Frame?
      LOGICAL FRYBAA           ! Free pointer to all NDF y error-bar
                               ! limits in "w.w. want"?
      LOGICAL FRYBAG           ! Free pointer to y error-bar limits
                               ! in GRAPHICS Frame?
      LOGICAL FRYBAR           ! Free pointer to y section error-bar 
                               ! limits in "w.w. want"
      LOGICAL FRYCEA           ! Free pointer to all NDF y data values
                               ! in "w.w. want" Frame?
      LOGICAL FRYCEG           ! Free pointer to spectrum y values in 
                               ! GRAPHICS Frame?
      LOGICAL FRYCEN           ! Free pointer to spectrum y values in 
                               ! "w.w. want" Frame?
      INTEGER FSET             ! Pointer to FrameSet used for plotting
      DOUBLE PRECISION GC( NDIM ) ! GRID co-ords at image centre
      REAL GLBND( NSPDIM )     ! Low grid co-ord bounds of PGPLOT window
      REAL GUBND( NSPDIM )     ! High grid co-ord bounds of PGPLOT
                               ! window
      INTEGER I                ! General variable
      INTEGER IAXFR            ! Index of effective AXIS Frame in IWCS
      INTEGER ICURR            ! Index of current Frame
      INTEGER ICURR0           ! Index of original current Frame in Plot
      INTEGER IHI              ! Index of last array element to use
      INTEGER ILO              ! Index of first array element to use
      INTEGER IMODE            ! Mode identifier
      INTEGER INDF             ! NDF identifier for input NDF
      INTEGER INDFI            ! NDF identifier for image section
                               ! two-dimensional section
      INTEGER INDFS            ! NDF identifier for specrum section
      INTEGER IPAWID           ! Pointer to supplied x-axis widths
      INTEGER IPIC             ! Horizontal index of current spectrum
      INTEGER IPLOTI           ! Pointer to AST spatial Plot DATA
                               ! picture
      INTEGER IPLOTL           ! Previous picture's pointer to AST
                               ! spectral Plot picture
      INTEGER IPLOTS           ! Pointer to AST spectral Plot picture
      INTEGER IPSTEG           ! Pointer to x-axis widths in GRAPHICS
      INTEGER IPSTEP           ! Pointer to x-axis widths in "w.w. want"
      INTEGER IPXBAG           ! Pointer to x-error-bar limits in
                               ! GRAPHICS Frame
      INTEGER IPXBAR           ! Pointer to x-error-bar limits in "w.w.
                               ! want"
      INTEGER IPXCEF           ! Pointer to x centres in "w.w. want" 
                               ! Frame, duplicated to match the number of
                               ! data values
      INTEGER IPXCEG           ! Pointer to x centres in GRAPHICS Frame
      INTEGER IPXCEN           ! Pointer to x centres in "w.w. want" 
                               ! Frame
      INTEGER IPXDAT           ! Pointer to supplied x-axis centres
      INTEGER IPXVAR           ! Pointer to supplied x-centre variances
      INTEGER IPYBAA           ! Pointer to all NDF y-error-bar limits 
                               ! in "w.w. want"
      INTEGER IPYBAG           ! Pointer to spectrum y-error-bar limits 
                               ! in GRAPHICS Frame
      INTEGER IPYBAR           ! Pointer to spectrum y-error-bar limits 
                               ! in "w.w. want"
      INTEGER IPYCEA           ! Pointer to all NDF y data values in 
                               ! "w.w. want" Frame
      INTEGER IPYCEG           ! Pointer to spectrum y data values in 
                               ! GRAPHICS Frame
      INTEGER IPYCEN           ! Pointer to spectrum y data values in 
                               ! "w.w. want" Frame
      INTEGER IPYDAT           ! Pointer to supplied y-data values
      INTEGER IPYVAR           ! Pointer to supplied y-data variances
      INTEGER ISHAPE           ! Identifier for error-bar shape
      INTEGER IWCS             ! Pointer to WCS FrameSet from the NDF
      INTEGER IWCSI            ! Pointer to WCS image FrameSet
      INTEGER IWCSS            ! Pointer to WCS spectral FrameSet
      INTEGER J                ! General variable
      INTEGER JPIC             ! Vertical index of current spectrum
      REAL LABSIZ              ! Plot Size for axis labels
      LOGICAL LAST             ! Processing last line plot?
      INTEGER LBND( NDF__MXDIM )! Lower pixel-index bounds of the NDF
      INTEGER LIPIC            ! Horizontal index of last visible plot
      INTEGER LJPIC            ! Vertical index of last visible plot
      LOGICAL LPAXES           ! Axes in line plots?
      INTEGER LPAXIS           ! Axis index for line plots' abscissae
      INTEGER LPDIM            ! Number of elements in input array along
                               ! spectral axis
      REAL LPMARG( 4 )         ! Width of margins around grid of
                               ! line plots
      INTEGER LBNDS( NDF__MXDIM ) ! Lower bounds of spectrum
                               ! section to use for a line plot
      INTEGER MAP              ! Pointer to "w.w. got"->"w.w. want"
                               ! Mapping 
      INTEGER MAPC2B           ! Mapping from current to base frames
      INTEGER MAPI             ! 1-1 mapping from current to base frames
                               ! for image axes
      INTEGER MAPP             ! 1-1 mapping from PIXEL to base frames
                               ! for image axes
      INTEGER MAPP2B           ! Mapping from PIXEL to base frames
      INTEGER MAPS             ! 1-1 mapping from current to base frames
                               ! for spectral axis
      REAL MARGIN( 4 )         ! Width of margins around DATA picture
      REAL MAXMAG              ! Maximum allowed magnification
      INTEGER MONO             ! 0: Not monotonic, +1: increase. -1: 
                               ! decrease
      INTEGER MTYPE            ! PGPLOT marker type
      INTEGER NAXC             ! Original number of current Frame axes
      INTEGER NAXNDF           ! No. of pixel axes in the base NDF
      CHARACTER MCOMP *( 8 )   ! Component to be mapped
      INTEGER NBAD             ! Number of bad values in a spectrum
      INTEGER NC               ! Number of characters in NDFNAM
      INTEGER NCU              ! Number of characters in the units
      CHARACTER NDFNAM*( 255 ) ! Full NDF specification 
      INTEGER NDIMS            ! Total number of NDF dimensions
      INTEGER NDUP             ! Number of rows to duplicate
      INTEGER NEL              ! No. of elements returned by KPG1_CPNDD
      LOGICAL NEWPIC           ! Is new Plot aligned with existing DATA
                               ! picture?
      INTEGER NFRM             ! Frame index increment between IWCS and
                               ! IPLOTS
      INTEGER NLPMAR           ! No. of line-plot margin values given
      INTEGER NMARG            ! No. of margin values given
      LOGICAL NOINV            ! Did any mapping not have an inverse?
      INTEGER NPIC             ! Ordinal number of current grid picture
      REAL NUMSIZ              ! Plot size for axis numbering
      INTEGER NVAL             ! No. of axis values supplied
      LOGICAL OLDPIC           ! Was an existing DATA picture found?
      REAL OPLBND( NSPDIM )    ! Low pixel co-ord bounds of NDF overlap
      REAL OPUBND( NSPDIM )    ! High pixel co-ord bounds of NDF overlap
      REAL PCLBND( NSPDIM )    ! Low pixel co-ord bounds of PGPLOT
                               ! window
      REAL PCUBND( NSPDIM )    ! High pixel co-ord bounds of PGPLOT
                               ! window
      INTEGER PERM( NDF__MXDIM ) ! Dummy axis permutation array
      INTEGER PICID0           ! AGI id. for original current picture
      INTEGER PICIDA           ! AGI id. for DATA picture generated by
                               ! spatial display in which grid appears
      INTEGER PICIDD           ! AGI id. for DATA picture
      INTEGER PICIDE           ! Existing picture ID
      INTEGER PICIDN           ! AGI id. for null picture
      INTEGER PICIDF           ! AGI id. for new FRAME picture
      INTEGER PICIDG           ! AGI id. for current picture in the grid
      INTEGER PICIDK           ! AGI id. for the KEY picture (dummy)
      INTEGER PIXFID           ! PIXEL Frame index in input WCS
      INTEGER PFRM             ! Original PIXEL-Frame pointer
      INTEGER PFRMI            ! Image PIXEL-Frame pointer
      INTEGER PMAP             ! PermMap pointer
      LOGICAL RFAXES           ! Axes in lower-left line plot?
      REAL RHOPIC              ! Grid density fudge factor
      INTEGER SDIM( NDF__MXDIM ) ! The significant NDF axes
      INTEGER SLBND( NDIM )    ! Significant lower bounds of the cube
      INTEGER SPAXIS( NSPDIM ) ! The spatial axes in the plot
      INTEGER STATE            ! State of a parameter
      INTEGER SUBND( NDIM )    ! Significant upper bounds of the cube
      CHARACTER TEXT*( 255 )   ! A general text string
      LOGICAL THERE            ! Does object exist?
      CHARACTER TITLE*( 255 )  ! Default title for the plot
      DOUBLE PRECISION TR( 2 ) ! "W.w. want" X/Y values at top-right 
                               ! corner
      DOUBLE PRECISION TRG( 2 )! "Uniform" X/Y values at top-right 
                               ! corner
      INTEGER UBND( NDF__MXDIM )! Upper pixel-index bounds of the NDF
      INTEGER UBNDS( NDF__MXDIM ) ! Upper bounds of spectrum
      CHARACTER UNITS * ( 30 ) ! Units of the data
      LOGICAL VISIBL           ! Data not all bad, hence plot visible?
      INTEGER WDIM( NSPDIM )   ! Dimensions in pixels of PGPLOT window
      INTEGER WILBND( NSPDIM ) ! Lower pixel-index bounds of NDF section
      INTEGER WIUBND( NSPDIM ) ! Upper pixel-index bounds of NDF section
      REAL WPLBND( NSPDIM )    ! Low pixel co-ord bounds of NDF section
      REAL WPUBND( NSPDIM )    ! High pixel co-ord bounds of NDFsection
      INTEGER WWGOT            ! Index of "what we've got" Frame in FSET
      INTEGER WWWANT           ! Index of "what we want" Frame in FSET
      REAL X1, X2              ! X bounds of current picture viewport in
                               ! millimetres
      REAL X1DATA, X2DATA      ! X bounds of the spatial DATA picture
      CHARACTER*( 20 ) XCOTXT  ! Formatted x co-ordinate of a spatial
                               ! pixel
      REAL XLMARG              ! Sum of the inner margins along x-axis
      REAL XMAGN               ! X magnification
      REAL XMARG               ! Sum of the outer margins along x-axis
      CHARACTER XMAP*( 8 )     ! How to map the x axis on to line plot
      INTEGER XPIC             ! Number of line plots in x direction
      DOUBLE PRECISION XSMAX   ! Max x after inclusion of axis widths
      DOUBLE PRECISION XSMIN   ! Min x after inclusion of axis widths
      REAL XSIGMA              ! No. of std. devn's for x error bars
      REAL XTENT               ! Extent in x of the PGPLOT window in 
                               ! NDF pixels
      LOGICAL XVAR             ! Display x axis centre variances?
      REAL XVMAG               ! X magnification factor for viewport
      REAL XWMAG               ! X magnification factor for window
      REAL Y1, Y2              ! Y bounds of current picture viewport in
                               ! millimetres
      REAL Y1DATA, Y2DATA      ! Y bounds of the spatial DATA picture
      CHARACTER*( 20 ) YCOTXT  ! Formatted y co-ordinate of a spatial
                               ! pixel
      REAL YLMARG              ! Sum of the inner margins along y-axis
      REAL YMAGN               ! Y magnification
      REAL YMARG               ! Sum of the outer margins along y-axis
      CHARACTER YMAP*( 8 )     ! How to map the y axis on to line plot
      INTEGER YPIC             ! Number of line plots in y direction
      REAL YSIGMA              ! No. of std. devn's for y error bars
      REAL YTENT               ! Extent in y of the PGPLOT window in 
                               ! NDF pixels
      LOGICAL YVAR             ! Display y data variances?
      REAL YVMAG               ! Y magnification factor for viewport
      REAL YWMAG               ! Y magnification factor for window

* Local Constants:
      REAL INTMAR( 4 )         ! Margins for interior line plots
      DATA INTMAR / 0.01, 0.01, 0.01, 0.01 /

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Initialise pointers to allocated memory, to enable checks to be
*  performed when shutting down.
      FRSTEG = .FALSE.
      FRSTEP = .FALSE.
      FRXBAG = .FALSE.
      FRXBAR = .FALSE.
      FRXCEF = .FALSE.
      FRXCEG = .FALSE.
      FRXCEN = .FALSE.
      FRYBAA = .FALSE.
      FRYBAG = .FALSE.
      FRYBAR = .FALSE.
      FRYCEA = .FALSE.
      FRYCEG = .FALSE.
      FRYCEN = .FALSE.

*  Get the main parameters of the data to be displayed.
*  ====================================================

*  Obtain the identifier of the NDF to be displayed.
      CALL LPG_ASSOC( 'NDF', 'READ', INDF, STATUS )

*  Find which component to display.  MCOMP is for use with NDF_MAP and 
*  may be set to 'Error'.  COMP is for use with all other NDF routines 
*  (which do not accept 'Error' as an NDF component name), and has
*  'Variance' in place of 'Error'.
      CALL KPG1_ARCOG( 'COMP', INDF, MCOMP, COMP, STATUS )

*  Get an AST pointer to a FrameSet describing the co-ordinate Frames
*  present in the NDF's WCS component.  Modify it to ensure that the 
*  Base, PIXEL and Current frames all have three dimensions.  The NDF 
*  must have exactly NDIM significant dimensions (i.e. axes
*  spanning more than one pixel).  This routine accesses the 'USEAXIS'
*  parameter.
      CALL KPG1_ASGET( INDF, NDIM, .FALSE., .TRUE., .TRUE., SDIM, 
     :                 SLBND, SUBND, IWCS, STATUS )

      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Store the size of each significant dimension.
      DO I = 1, NDIM
         DIMS( I ) = SUBND( I ) - SLBND( I ) + 1
      END DO
      DO I = NDIM + 1, NDF__MXDIM
         SDIM( I ) = I
      END DO

*  Get the name of the NDF.  This is later stored in the graphics
*  database.
      CALL NDF_MSG( 'NDF', INDF )
      CALL MSG_LOAD( ' ', '^NDF', NDFNAM, NC, STATUS )

*  Obtain the units if present.  
      CALL KPG1_DAUNI( INDF, MCOMP, UNITS, NCU, STATUS )

*  Obtain the axis to plot along the line plots' abscissae.
*  ========================================================

*  Extract the current, PIXEL, and base Frames, and get the number of
*  axes in the current Frame.
      CFRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )
      BFRM = AST_GETFRAME( IWCS, AST__BASE, STATUS )
      CALL KPG1_ASFFR( IWCS, 'PIXEL', PIXFID, STATUS )
      PFRM = AST_GETFRAME( IWCS, PIXFID, STATUS )
      NAXC = AST_GETI( CFRM, 'NAXES', STATUS )

*  Get the index of the current Frame axis defining the plotting
*  axis.  Allow a null value to cause the routine to exit by
*  supplying 0 for the axis index.
      CALL ERR_MARK
      LPAXIS = 0
      DO I = 1, NDIM
         WRITE( DOMAIN, '(''DOMAIN('',I1,'')'')' ) SDIM( I )
         IF ( AST_GETC( IWCS, DOMAIN, STATUS ) .EQ. 'SPECTRUM' .OR.
     :        AST_GETC( IWCS, DOMAIN, STATUS ) .EQ. 'DSBSPECTRUM') THEN
            LPAXIS = SDIM( I )
         END IF
      END DO

*  There was no SPECTRUM Domain; not sure that this can occur, but
*  just in case use the de facto standard for a spectral cube.
*  A non-zero LPAXIS on input becomes the dynamic default.
      CALL KPG1_GTAXI( 'USEAXIS', CFRM, 1, LPAXIS, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

*  There was no spectral Frame and a null supplied, so use the de facto
*  standard for a spectral cube.
         LPAXIS = 3
      END IF
      CALL ERR_RLSE

      LPDIM = DIMS( LPAXIS )

*  Get a one-dimensional spectral FrameSet.
*  ========================================

*  Remember the index of the current Frame.
      ICURR = AST_GETI( IWCS, 'CURRENT', STATUS )

*  Obtain a one-dimensional mapping from the current to the base
*  for the spectral axis.
      MAPC2B = AST_GETMAPPING( IWCS, AST__CURRENT, AST__BASE, STATUS )
      CALL AST_MAPSPLIT( MAPC2B, 1, LPAXIS, FAXES, MAPS, STATUS )

      IF (  ( MAPS .EQ. AST__NULL .OR.
     :      AST_GETI( MAPS, 'Nout', STATUS ) .NE. 1 ) .AND.
     :      STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CLINPLOT_BADAXIS', 'The spectral axis in the '/
     :                 /'supplied NDF is not independent of the '/
     :                 /'spatial axes.', STATUS )
         GOTO 999
      END IF

*  Construct a FrameSet suitable to be passed to KPS1_LPLFS.  This has a
*  one-dimensional base Frame corresponding to the GRID axis [FAXES(1)]
*  already found, and a one-dimensional current Frame corresponding to
*  the SpecFrame or nominal spectral Frame.
      BFRMS = AST_PICKAXES( BFRM, 1, FAXES( 1 ), PMAP, STATUS )
      CFRMS = AST_PICKAXES( CFRM, 1, FAXES( 1 ), PMAP, STATUS )
      IWCSS = AST_FRAMESET( BFRMS, ' ', STATUS )
      CALL AST_INVERT( MAPS, STATUS )
      CALL AST_ADDFRAME( IWCSS, AST__BASE, MAPS, CFRMS, STATUS )

*  Get a two-dimensional image FrameSet.
*  =====================================

*  Assign the spatial axes in their current order.
      J = 0
      DO I = 1, NDIM
         IF ( SDIM( I ) .NE. LPAXIS ) THEN
            J = J + 1
            SPAXIS( J ) = SDIM( I )
         END IF
      END DO

*  Obtain a two-dimensional mapping from the current to the base
*  for the image axes.
      CALL AST_MAPSPLIT( MAPC2B, NSPDIM, SPAXIS, FAXES, MAPI, STATUS )

      IF (  ( MAPI .EQ. AST__NULL .OR.
     :      AST_GETI( MAPI, 'Nout', STATUS ) .NE. NSPDIM ) .AND.
     :      STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CLINPLOT_BADAXIS', 'The spectral axis in the '/
     :                 /'supplied NDF is not independent of the '/
     :                 /'spatial axes.', STATUS )
         GOTO 999
      END IF

*  Obtain a two-dimensional mapping from the pixel to the base
*  for the image axes.
      MAPP2B = AST_GETMAPPING( IWCS, PIXFID, AST__BASE, STATUS )
      CALL AST_MAPSPLIT( MAPP2B, NSPDIM, SPAXIS, FAXES, MAPP, STATUS )

*  Construct a FrameSet suitable to be passed to KPG1_GTPOS.  This has a
*  two-dimensional base Frame corresponding to the GRID axes (FAXES)
*  already found, and a two-dimensional current Frame corresponding to
*  the SkyFrame or nominal sky Frame.
      BFRMI = AST_PICKAXES( BFRM, NSPDIM, FAXES, PMAP, STATUS )
      CFRMI = AST_PICKAXES( CFRM, NSPDIM, FAXES, PMAP, STATUS )
      PFRMI = AST_PICKAXES( PFRM, NSPDIM, FAXES, PMAP, STATUS )
      IWCSI = AST_FRAMESET( BFRMI, ' ', STATUS )
      CALL AST_INVERT( MAPP, STATUS )
      CALL AST_ADDFRAME( IWCSI, AST__BASE, MAPP, PFRMI, STATUS )
      CALL AST_INVERT( MAPI, STATUS )
      CALL AST_ADDFRAME( IWCSI, AST__BASE, MAPI, CFRMI, STATUS )

*  Associate AXIS information.
*  ===========================
*
*  If there is an axis structure in the NDF, get a mapping from the AXIS
*  Frame to the GRID Frame.  If no AXIS structure exists NDF AXIS 
*  routines will use a default one, but AST may use an AXIS Frame in the
*  WCS component stored by a previous application.  Therefore, we use 
*  the mapping from PIXEL Frame (the Frame corresponding to the default
*  NDF AXIS structure) to GRID Frame if no AXIS structure is present.  
*  This ensures that the mapping can be used to map Axis values returned
*  by NDF_AMAP into GRID co-ordinates.
      CALL NDF_STATE( INDF, 'AXIS', THERE, STATUS ) 
      IF ( THERE ) THEN
         CALL KPG1_ASFFR( IWCS, 'AXIS', IAXFR, STATUS )
      ELSE
         CALL KPG1_ASFFR( IWCS, 'PIXEL', IAXFR, STATUS )
      END IF

      AXMAP = AST_GETMAPPING( IWCS, IAXFR, AST__BASE, STATUS )

*  If the base NDF has more than one pixel axis (e.g. if a 
*  one-dimensional section from a two-dimensional NDF was supplied), 

*  We need to modify the above mapping to have one input corresponding 
*  to the line-plot horizontal axis.

*  Create a PermMap with one input and an output for each NDF pixel 
*  axis.  Connect the one input to the output for the significant NDF 
*  dimension.
      NAXNDF = AST_GETI( AXMAP, 'NIN', STATUS )
      DO I = 1, NAXNDF
         PERM( I ) = 0
      END DO
      PERM( LPAXIS ) = 1

      PMAP = AST_PERMMAP( 1, LPAXIS, NAXNDF, PERM, 0.0D0, ' ', STATUS )

*  Put this PermMap in front of the Mapping found above.  The resulting
*  Mapping has one input (the AXIS value on the significant NDF axis)
*  and NAXNDF outputs (the three-dimensional GRID co-ordinate).
      AXMAP = AST_CMPMAP( PMAP, AXMAP, .TRUE., ' ', STATUS )

*  Create a PermMap with NAXNDF inputs and one output for the spectral
*  axis. 
      DO I = 1, NAXNDF
         PERM( I ) = I
      END DO

      PMAP = AST_PERMMAP( NAXNDF, PERM, 1, LPAXIS, 0.0D0, ' ', STATUS )

*  Put this PermMap after the Compound Mapping found above.  The 
*  resulting Mapping has one input (the AXIS value on the significant 
*  NDF axis) and one output (the one-dimensional GRID co-ordinate).
      AXMAP = AST_CMPMAP( AXMAP, PMAP, .TRUE., ' ', STATUS )

* Check the axis map is defined in both directions. 
      IF ( .NOT. AST_GETL( AXMAP, 'TRANFORWARD', STATUS ) .OR.
     :     .NOT. AST_GETL( AXMAP, 'TRANINVERSE', STATUS ) ) THEN
         IF ( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'CLINPLOT_ERR', 'One or more of the input '/
     :                    /'AXIS arrays is non-monotonic.', STATUS )
            GO TO 999
         END IF
      END IF

*  See how the x and y axes are to be mapped on to the screen.
      CALL PAR_CHOIC( 'XMAP', 'Default', 'Default,Linear,Log,Pixel,'/
     :                /'Distance', .TRUE., XMAP, STATUS )
      CALL PAR_CHOIC( 'YMAP', 'Linear', 'Linear,Log,ValueLog', .TRUE., 
     :                YMAP, STATUS )

*  Obtain a FrameSet containing three two-dimensional Frames.  In the
*  Frame 1, axis 1 is the GRID co-ordinate with the supplied 
*  one-dimensional array, and axis 2 is the raw data value.  This Frame 
*  corresponds to "what we've got".  In the Frame 3 (the current Frame),
*  axis 1 is the value on the selected axis from the NDF's current Frame
*  and axis 2 is the raw (or logged if ymap=ValueLog) data value.  This 
*  Frame corresponds to "what we want to see" and is given the Domain 
*  "DATAPLOT".  Frame 2 (the Base Frame) is spanned by the axes which 
*  are to be mapped linearly or logarithmically on to the graphics
*  surface.  Axis 1 will be determined by the setting of parameter XMAP,
*  and axis 2 by the setting of YMAP.  This Frame corresponds to the 
*  "uniform" co-ordinate system, and is given the Domain AGI_WORLD.  A
*  flag is returned if any of the required Mappings do not have an 
*  inverse transformation.
      CALL KPS1_LPLFS( INDF, IWCSS, .FALSE., 1, LPDIM, XMAP, YMAP, 
     :                 MCOMP, UNITS( : NCU ), NOINV, FSET, STATUS )

*  Note the index of the "what we've got" and "what we want" Frames.
      WWGOT = 1
      WWWANT = 3

*  Get some error-bar parameter values.
*  ====================================

*  See if error bars are required.
      CALL PAR_GET0L( 'ERRBAR', ERRBAR, STATUS )

*  If so...
      XVAR = .FALSE.
      YVAR = .FALSE.
      IF ( ERRBAR ) THEN

*  Shape and Frequency
*  -------------------
*  Obtain the shape of error bar to plot.
         CALL PAR_CHOIC( 'SHAPE', 'Bars', 'Bars,Cross,Diamond',
     :                   .TRUE., TEXT, STATUS )

*  Classify the value.
         IF ( TEXT .EQ. 'BARS' ) THEN
            ISHAPE = 1
         ELSE IF ( TEXT .EQ. 'CROSS' ) THEN
            ISHAPE = 2
         ELSE
            ISHAPE = 3
         END IF

*  Obtain the spacing between points showing the error bars.
         CALL PAR_GDR0I( 'FREQ', 1, 1, MAX( 1, LPDIM/2 ), .TRUE., FREQ, 
     :                   STATUS )

*  Number of standard deviations
*  -----------------------------
*  We only use variances on the x axis if error bars are required, and 
*  if axis variances are available.
         CALL NDF_ASTAT( INDF, 'VARIANCE', LPAXIS, XVAR, STATUS ) 

*  See how many standard deviations are to be used for a horizontal 
*  error bar.
         IF ( XVAR ) THEN
            CALL PAR_GET0R( 'XSIGMA', XSIGMA, STATUS )
            XSIGMA = ABS( XSIGMA )

*  Tell the user what XSIGMA value we are using.
            CALL MSG_BLANK( STATUS )
            CALL MSG_SETR( 'XS', XSIGMA )
            CALL MSG_OUT( 'CLINPLOT_MSG1', '  Errors in position will '/
     :                    /'be displayed as ^XS sigma errors.', STATUS )
         END IF

*  We only use variances on the y axis if the data values being 
*  displayed are from the NDF DATA component, and if error bars are
*  required, and if variances are available.
         IF ( MCOMP .EQ. 'Data' ) THEN
            CALL NDF_STATE( INDF, 'VARIANCE', YVAR, STATUS )
         END IF

*  See how many standard deviations are to be used for a vertical error 
*  bar.
         IF ( YVAR ) THEN
            CALL PAR_GET0R( 'YSIGMA', YSIGMA, STATUS )
            YSIGMA = ABS( YSIGMA )

*  Tell the user what YSIGMA value we are using.
            CALL MSG_BLANK( STATUS )
            CALL MSG_SETR( 'YS', YSIGMA )
            CALL MSG_OUT( 'CLINPLOT_MSG2', '  Errors in data value '/
     :                    /'will be displayed as ^YS sigma errors.',
     :                    STATUS )
         END IF
      END IF


*  Get the plotting mode.
*  ======================
      CALL PAR_CHOIC( 'MODE', 'Line', 
     :                'Histogram,Line,Point,Mark,Step,Chain',
     :                .FALSE., TEXT, STATUS )

*  Get an identifier for the mode, and get the marker type if required.
      IF ( TEXT .EQ. 'HISTOGRAM' ) THEN
         IMODE = 1
      ELSE IF ( TEXT .EQ. 'LINE' ) THEN
         IMODE = 2
      ELSE IF ( TEXT .EQ. 'POINT' ) THEN
         IMODE = 3
         MTYPE = -1
      ELSE IF ( TEXT .EQ. 'MARK' ) THEN
         IMODE = 3
         CALL PAR_GET0I( 'MARKER', MTYPE, STATUS )
      ELSE IF ( TEXT .EQ. 'STEP' ) THEN
         IMODE = 4
      ELSE
         IMODE = 5
         CALL PAR_GET0I( 'MARKER', MTYPE, STATUS )
      END IF

*  Ensure marker type (if used) is legal.
      MTYPE = MAX( -31, MTYPE )

      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Determine the width of the margins around the spatial picture.
*  ==============================================================

*  See if annotated axes are required. 
      CALL PAR_GET0L( 'EXTAXES', AXES, STATUS )

*  Set the dynamic default for the margins to place around the DATA
*  picture (a single value is used for all edges), and then get the
*  margins to use.  Negative margins can be used, but the sum of the
*  two margins in one any dimension must be greater than -1.0. 
*  Therefore limit each margin to be greater than -0.49.
      IF ( AXES ) THEN
         DEFMAR = 0.15
      ELSE
         DEFMAR = 0.0
      END IF

      CALL PAR_DEF1R( 'MARGIN', 1, DEFMAR, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

      CALL PAR_GDRVR( 'MARGIN', 4, -0.49, 10.0, MARGIN, NMARG, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         NMARG = 1
         MARGIN( 1 ) = DEFMAR
      END IF

      NMARG = MIN( 4, NMARG )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Use the first value for any unspecified edges.
      DO I = NMARG + 1, 4      
         MARGIN( I ) = MARGIN( 1 )
      END DO

*  Compact the code to make easier to read by defining combined
*  x and y margins.
      XMARG = MARGIN( 2 ) + MARGIN( 4 )
      YMARG = MARGIN( 1 ) + MARGIN( 3 )

*  Report an error if the margins do not leave any room for the DATA
*  picture.
      IF ( ( 1.0 - YMARG .LE. 0.005 .OR.
     :       1.0 - XMARG .LE. 0.005 ) .AND. 
     :     STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR

         CALL ERR_REP( 'CLINPLOT_ERR2', 'No room left for the DATA '//
     :                 'picture (try reducing the size of the '//
     :                 'margins - see parameter MARGIN).', STATUS )
         GO TO 999

      END IF

*  Determine the width of the margins around the grid of line plots.
*  =================================================================

*  See if annotated axes are required.  The default is no.
      CALL PAR_DEF0L( 'AXES', .FALSE., STATUS )
      CALL PAR_GET0L( 'AXES', LPAXES, STATUS )

*  Use the dynamic default if a null value was supplied.
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         LPAXES = .FALSE.
      END IF

*  Set the dynamic default for the margins to place around the grid
*  of line plots (a single value is used for all edges), and then get
*  the margins to use.  For an inererior axes allow half a pixel margin.
*  If exterior axes just allow a little room for any tick marks to not
*  overprint the line plot. If there are no interior or exterior axes,
*  using a dynamic default of zero avoids the unnecessary creation of
*  FRAME pictures by KPG1_PLOTP.
      IF ( LPAXES ) THEN
         DEFMAR = 0.5
      ELSE IF ( AXES ) THEN
         DEFMAR = 0.1
      ELSE
         DEFMAR = 0.0
      END IF

      CALL PAR_DEF1R( 'LPMARGIN', 1, DEFMAR, STATUS )

* Obtain up to four values in the range 0 to 1 pixel.
      CALL PAR_GDRVR( 'LPMARGIN', 4, 0.0, 1.0, LPMARG, NLPMAR, STATUS )

      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         NLPMAR = 1
         LPMARG( 1 ) = DEFMAR
      END IF

      NLPMAR = MIN( 4, NLPMAR )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Use the first value for any unspecified edges.
      DO I = NLPMAR + 1, 4  
         LPMARG( I ) = LPMARG( 1 )
      END DO

*  Convert the fractions of pixels into fractions of the
*  array dimensions.
      XLMARG = ( LPMARG( 2 ) + LPMARG( 4 ) ) / 
     :         REAL( DIMS( SPAXIS( 1 ) ) )
      YLMARG = ( LPMARG( 1 ) + LPMARG( 3 ) ) / 
     :         REAL( DIMS( SPAXIS( 2 ) ) )

*  Determine the section of the supplied NDF to be displayed.
*  ==========================================================

*  First get the aspect ratio of the available space in which the image
*  can be displayed.  This is the current picture, minus any requested
*  margins.  To do this we need to temporarily open the AGI database and
*  PGPLOT plotting package, so that we can have a look at the current
*  picture.  Do not clear the picture (it may be cleared when it is next
*  opened within KPG1_PLOT).
      CALL AGP_ASSOC( 'DEVICE', 'UPDATE', ' ', .FALSE., PICID0, STATUS )

*  If the device could not be opened, indicate that the parameter
*  association should be cancelled when the device is closed so that a
*  new device name will be obtained when the parameter is next accessed.
      DEVCAN = .FALSE.

      IF ( STATUS .NE. SAI__OK .AND. STATUS .NE. PAR__ABORT .AND.
     :     STATUS .NE. PAR__NULL ) THEN
         DEVCAN = .TRUE.
     
*  If successful, store the bounds of the viewport for the current
*  picture (in millimetres).
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
         CALL PGQVP( 2, X1, X2, Y1, Y2 )

*  Check the viewport does not have zero area.
         IF ( X2 .EQ. X1 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'CLINPLOT_3', 'Current AGI picture has '//
     :                    'zero width.', STATUS )

         ELSE IF ( Y2 .EQ. Y1 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'CLINPLOT_4', 'Current AGI picture has '//
     :                    'zero height.', STATUS )
         END IF

      END IF

*  Close down the graphics device and AGI database.
      CALL ERR_BEGIN( STATUS ) 
      CALL AGP_DEASS( 'DEVICE', DEVCAN, STATUS )
      CALL ERR_END( STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Now find the aspect ratio of the available space (i.e. the current
*  picture minus the margins). 
      ASP0 = ( ( Y2 - Y1 ) * ( 1.0 - YMARG ) * ( 1.0 - YLMARG ) ) /
     :       ( ( X2 - X1 ) * ( 1.0 - XMARG ) * ( 1.0 - XLMARG ) ) 

*  Get the aspect ratio of the supplied data array.
      ASPD = REAL( DIMS( SPAXIS( 2 ) ) )/ REAL( DIMS( SPAXIS( 1 ) ) )

*  If there would be space at left and right when the array fills the
*  height of the available space, then the viewport used for the data
*  array can be expanded in x but not in y.  Store the maximum allowable
*  magnifications of the viewport used to display the data in x and y.
      IF ( ASPD .GT. ASP0 ) THEN
         XVMAG = ASPD / ASP0
         YVMAG = 1.0

*  If there would be space at top and bottom when the array fills the
*  width of the available space, then the viewport used for the data
*  array can be expanded in Y but not in X.
      ELSE
         XVMAG = 1.0
         YVMAG = ASP0 / ASPD
      END IF

*  Get the positive magnifications required for both axes.  Use a 
*  dynamic default for YMAGN equal to the value supplied for XMAGN.  A 
*  magnification of 1.0 results in the whole image being displayed 
*  within the current picture so that it fills the available space in
*  at least one dimension.
      MAXMAG = REAL( DIMS( 1 ) )
      CALL PAR_GDR0R( 'XMAGN', 0.0, 1.0E-6, MAXMAG, .FALSE., XMAGN, 
     :                STATUS )
      MAXMAG = MAX( XMAGN, REAL( DIMS( 2 ) ) )
      CALL PAR_GDR0R( 'YMAGN', XMAGN, 1.0E-6, MAXMAG, .TRUE., YMAGN, 
     :                STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  If the magnification in x is greater than the maximum allowed by just
*  expanding the width of the viewport, then we also need to reduce the
*  size of the window (so that fewer pixels are visible within the
*  expanded viewport).
      IF ( XMAGN .GT. XVMAG ) THEN
         XWMAG = XVMAG / XMAGN

*  Otherwise, we can achieve the requested magnification without
*  reducing the window.
      ELSE
         XVMAG = XMAGN
         XWMAG = 1.0
      END IF

*  Do the same for the y axis.
      IF ( YMAGN .GT. YVMAG ) THEN
         YWMAG = YVMAG/YMAGN
      ELSE
         YVMAG = YMAGN
         YWMAG = 1.0
      END IF

*  Determine the aspect ratio of the viewport for the DATA picture.
      ASPECT = ASPD * YVMAG / XVMAG

*  Determine the extent of the corresponding window in pixels.
      XTENT = REAL( DIMS( SPAXIS( 1 ) ) ) * XWMAG
      YTENT = REAL( DIMS( SPAXIS( 2 ) ) ) * YWMAG

*  Get the GRID co-ordinates at the centre of the supplied NDF.  GRID
*  co-ordinates are (1.0,1.0) at the centre of the first pixel.
      GC( SPAXIS( 1 ) ) = ( 1.0D0 + DBLE( DIMS( SPAXIS( 1 ) ) ) ) / 
     :                    2.0D0
      GC( SPAXIS( 2 ) ) = ( 1.0D0 + DBLE( DIMS( SPAXIS( 2 ) ) ) ) /
     :                    2.0D0
      GC( LPAXIS ) = ( 1.0D0 + DBLE( DIMS( LPAXIS ) ) ) / 2.0D0

*  Convert these into the Current Frame of the NDF. 
      CALL AST_TRANN( IWCSI, 1, NSPDIM, 1, GC, .TRUE., NSPDIM,
     :                1, CC, STATUS )

*  Try to convert these back to grid.  The current Frame is not suitable
*  for specifying a centre position if any of the returned values are
*  bad.
      CALL AST_TRANN( IWCSI, 1, NSPDIM, 1, CC, .FALSE., NSPDIM,
     :                1, GC, STATUS )

      IF ( GC( SPAXIS( 1 ) ) .EQ. AST__BAD .OR. 
     :     GC( SPAXIS( 2 ) ) .EQ. AST__BAD ) THEN
         IF ( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'CLINPLOT_ERR4B', 'The Mapping from the '//
     :                    'current WCS co-ordinate Frame to pixel '//
     :                    'co-ordinates is undefined at the image '//
     :                    'centre.', STATUS )
            CALL ERR_REP( 'CLINPLOT_ERR4C',  'Try displaying a '//
     :                    'section of the image, or changing the '//
     :                    'current Frame to PIXEL using the KAPPA '//
     :                    'WCSFRAME task.', STATUS )
         END IF
         GO TO 999
      END IF

*  If the centre pixel of the supplied NDF has no defined position then 
*  we only access the CENTRE parameter if a value was supplied on the
*  command line.  Otherwise, just use the centre of the GRID frame as 
*  the centre for the displayed image.
      CALL LPG_STATE( 'CENTRE', STATE, STATUS )
      IF ( ( CC( SPAXIS( 1 ) ) .NE. AST__BAD .AND.
     :       CC( SPAXIS( 2 ) ) .NE. AST__BAD ) .OR.
     :     STATE .EQ. PAR__ACTIVE ) THEN

*  Obtain the Current Frame co-ordinates (returned in CC) to put at the 
*  centre of the picture using parameter CENTRE.  Use the Current Frame 
*  co-ordinates at the centre of the image as the dynamic default (they 
*  will be ignored if they are bad).  KPG1_GTPOS loops until 
*  co-ordinates are obtained that are valid in the Base Frame (i.e. 
*  GRID Frame in our case) of the supplied FrameSet.  These GRID 
*  co-ordinates are returned in GC.
         CALL KPG1_GTPOS( 'CENTRE', IWCSI, .TRUE., CC, GC, STATUS )
      END IF

*  Find the corresponding upper and lower bounds in GRID co-ordinates.
      GLBND( 1 ) = REAL( GC( SPAXIS( 1 ) ) ) - 0.5 * XTENT
      GUBND( 1 ) = GLBND( 1 ) + XTENT
      GLBND( 2 ) = REAL( GC( SPAXIS( 2 ) ) ) - 0.5 * YTENT
      GUBND( 2 ) = GLBND( 2 ) + YTENT

*  Find the equivalent bounds in pixel co-ordinates. 
      PCLBND( 1 ) = GLBND( 1 ) - 1.5 + REAL( SLBND( SPAXIS( 1 ) ) )
      PCUBND( 1 ) = GUBND( 1 ) - 1.5 + REAL( SLBND( SPAXIS( 1 ) ) )
      PCLBND( 2 ) = GLBND( 2 ) - 1.5 + REAL( SLBND( SPAXIS( 2 ) ) )
      PCUBND( 2 ) = GUBND( 2 ) - 1.5 + REAL( SLBND( SPAXIS( 2 ) ) )

*  Find the pixel index bounds of the NDF section.  Now we only want
*  whole pixels, so the floor and ceilings are switched from their
*  normal inclusive sense.  However, this can lead to inverted bounds;
*  abort in this case of excesive magnification.
      WILBND( 1 ) = KPG1_CEIL( PCLBND( 1 ) ) + 1
      WIUBND( 1 ) = KPG1_FLOOR( PCUBND( 1 ) )
      WILBND( 2 ) = KPG1_CEIL( PCLBND( 2 ) ) + 1
      WIUBND( 2 ) = KPG1_FLOOR( PCUBND( 2 ) )

      IF ( WILBND( 1 ) .GT. WIUBND( 1 ) .OR.
     :     WILBND( 2 ) .GT. WIUBND( 2 ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CLINPLOT_5', 'The values supplied for '/
     :                 /'parameters %CENTRE, %XMAGN and %YMAGN would '/
     :                 /'result in no part of the image being '/
     :                 /'displayed.', STATUS )
         GOTO 999
      END IF

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  These bounds are for the two significant spatial axes which will, in 
*  general, correspond to different axes in the supplied NDF.  Store the
*  bounds of the section to be displayed within the supplied NDF.
      CALL NDF_BOUND( INDF, NDF__MXDIM, LBND, UBND, NDIMS, STATUS )
      LBND( SPAXIS( 1 ) ) = WILBND( 1 )
      UBND( SPAXIS( 1 ) ) = WIUBND( 1 )
      LBND( SPAXIS( 2 ) ) = WILBND( 2 )
      UBND( SPAXIS( 2 ) ) = WIUBND( 2 )
      LBND( LPAXIS ) = SLBND( LPAXIS )
      UBND( LPAXIS ) = SUBND( LPAXIS )

*  Obtain an NDF identifier for this section of the supplied NDF.
      CALL NDF_SECT( INDF, NDIMS, LBND, UBND, INDFI, STATUS ) 

*  Store the significant spatial dimensions of this NDF section.
      WDIM( 1 ) = WIUBND( 1 ) - WILBND( 1 ) + 1
      WDIM( 2 ) = WIUBND( 2 ) - WILBND( 2 ) + 1

*  Store the pixel co-ordinate bounds of the section (this may be
*  different to PCLBND/PCUBND due to rounding of the pixel indices).
      WPLBND( 1 ) = REAL( WILBND( 1 ) ) - 1.0
      WPUBND( 1 ) = REAL( WIUBND( 1 ) )
      WPLBND( 2 ) = REAL( WILBND( 2 ) ) - 1.0
      WPUBND( 2 ) = REAL( WIUBND( 2 ) )

*  Find the pixel co-ordinate bounds of the section of the original NDF
*  that falls within the DATA picture.
      OPLBND( 1 ) = MIN( REAL( SUBND( SPAXIS( 1 ) ) ), 
     :                   MAX( REAL( SLBND( SPAXIS( 1 ) ) ) - 1.0, 
     :                        WPLBND( 1 ) ) )
      OPUBND( 1 ) = MIN( REAL( SUBND( SPAXIS( 1 ) ) ), 
     :                   MAX( REAL( SLBND( SPAXIS( 1 ) ) ) - 1.0,
     :                        WPUBND( 1 ) ) )
      OPLBND( 2 ) = MIN( REAL( SUBND( SPAXIS( 2 ) ) ), 
     :                   MAX( REAL( SLBND( SPAXIS( 2 ) ) ) - 1.0,
     :                        WPLBND( 2 ) ) )
      OPUBND( 2 ) = MIN( REAL( SUBND( SPAXIS( 2 ) ) ), 
     :                   MAX( REAL( SLBND( SPAXIS( 2 ) ) ) - 1.0, 
     :                        WPUBND( 2 ) ) )

*  Report an error if there is no overlap.
      IF ( OPLBND( 1 ) .EQ. OPUBND( 1 ) .OR. 
     :     OPLBND( 2 ) .EQ. OPUBND( 2 ) .AND.
     :     STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CLINPLOT_5', 'The values supplied for '/
     :                 /'parameters %CENTRE, %XMAGN and %YMAGN would '/
     :                 /'result in no part of the image being '/
     :                 /'displayed.', STATUS )
      END IF

*  Start the graphics system.
*  ==========================

*  If the array being displayed is one-dimensional, set a dynamic 
*  default of TRUE for parameter FILL.
      IF ( WDIM( 1 ) .EQ. 1 .OR. WDIM( 2 ) .EQ. 1 ) THEN
         CALL PAR_DEF0L( 'FILL', .TRUE., STATUS )
      ELSE
         CALL PAR_DEF0L( 'FILL', .FALSE., STATUS )
      END IF

*  Store the pixel co-ordinate bounds to be stored with the new DATA
*  picture in the AGI database.  Note these allow for the margins
*  within which the grid of line plots, one for each spatial pixel,
*  will be drawn.
      BOX( 1 ) = DBLE( PCLBND( 1 ) - LPMARG( 4 ) )
      BOX( 2 ) = DBLE( PCLBND( 2 ) - LPMARG( 1 ) )
      BOX( 3 ) = DBLE( PCUBND( 1 ) + LPMARG( 2 ) )
      BOX( 4 ) = DBLE( PCUBND( 2 ) + LPMARG( 3 ) )

*  Start up the graphics system again.  This stores a new DATA picture 
*  in the AGI database with the given pixel co-ordinate bounds
*  (enclosing a FRAME picture that may also be created).  The 
*  PGPLOT viewport is set so that it matches the area of the DATA 
*  picture.  World co-ordinates within the PGPLOT window are set to 
*  millimetres from the bottom-left corner of the view surface.  An AST 
*  Plot is returned for drawing in the DATA picture.  The Base 
*  (GRAPHICS) Frame in the Plot corresponds to millimetres from the 
*  bottom-left corner of the view surface, and the Current Frame is 
*  inherited form the NDF's WCS FrameSet.

*  Start up the graphics system.  This routine accesses the following
*  parameters:  'CLEAR', 'DEVICE', 'FILL', and 'STYLE'.
      CALL KPG1_PLOT( IWCSI, 'NEW', 'KAPPA_CLINPLOT', NDFNAM( : NC ), 
     :                MARGIN, 0, ' ', ' ', 0.0, ASPECT, 'PIXEL', 
     :                BOX, PICIDD, PICIDF, PICIDN, IPLOTI, NFRM, ALIGN, 
     :                STATUS )

*  It would be useful to know if the device was cleared or not.  So
*  inquire of the parameter again.
      CALL PAR_GET0L( 'CLEAR', CLEAR, STATUS )

*  If the user did not specify a Plot title (as indicated by the Plot 
*  title being the same as the WCS title), make the NDF Title the
*  default Title for the Plot.  We have to be careful about the timing 
*  of this change to the Title.  If we did it before KPG1_PLOT (i.e. if 
*  we set the Title in IWCS) it may prevent alignment occurring within 
*  KPG1_PLOT since alignment fails if the Title of two Frames differ.
      IF ( AST_GETC( IWCS, 'TITLE', STATUS ) .EQ. 
     :     AST_GETC( IPLOTI, 'TITLE', STATUS ) ) THEN

         TITLE = ' '
         CALL NDF_CGET( INDF, 'TITLE', TITLE, STATUS ) 

         IF ( TITLE .NE. ' ' ) THEN
            CALL AST_SETC( IPLOTI, 'TITLE', 
     :                     TITLE( : CHR_LEN( TITLE ) ), STATUS )
         END IF

      END IF

      IF ( STATUS .EQ. PAR__ABORT ) GOTO 999

*  Produce the exterior plot.
*  ==========================

*  Draw the outer axes grid, if required.
      IF ( AXES ) CALL KPG1_ASGRD( IPLOTI, PICIDF, .TRUE., STATUS )

*  We're done with the exterior plot now.
      CALL AST_ANNUL( IPLOTI, STATUS )

*  Restore the original current picture.
      CALL AST_SETI( IWCS, 'CURRENT', ICURR, STATUS )

*  Inquire the current picture.
      CALL AGI_ICURP( PICIDA, STATUS )

*  Need to store its world co-ordinates changed by KPG1_PLOT,
*  as they're not in the AGI database associated with this picture.
      CALL PGQWIN( X1DATA, X2DATA, Y1DATA, Y2DATA )

*  Map the required NDF arrays, and get some work space.
*  =====================================================

*  Map the NDF AXIS-centre values along the spectral axis in double
*  precision.
      CALL NDF_AMAP( INDF, 'CENTRE', LPAXIS, '_DOUBLE', 'READ', IPXDAT,
     :               AEL, STATUS ) 

*  Allocate work space to hold the corresponding values to be displayed
*  on the horizontal axis of the Plot.
      CALL PSX_CALLOC( AEL, '_DOUBLE', IPXCEN, STATUS )
      FRXCEN = ( STATUS .EQ. SAI__OK )

*  We only use variances on the x axis if error bars are required, and 
*  if axis variances are available.
      IF ( .NOT. ERRBAR ) THEN
         XVAR = .FALSE.
      ELSE
         CALL NDF_ASTAT( INDF, 'VARIANCE', LPAXIS, XVAR, STATUS )
      END IF

*  If line-plot x-axis variance values are required, map them. 
      IF ( XVAR ) THEN
         CALL NDF_AMAP( INDF, 'VARIANCE', LPAXIS, '_DOUBLE', 
     :                  'READ', IPXVAR, AEL, STATUS ) 

*  Allocate work space to hold the upper and lower limits of each 
*  horizontal error bar on the displayed horizontal axis.
         CALL PSX_CALLOC( AEL * 2, '_DOUBLE', IPXBAR, STATUS )
         FRXBAR = (STATUS .EQ. SAI__OK )

*  If x-axis variance values are not required store safe pointer values.
      ELSE
         IPXVAR = IPXDAT
         IPXBAR = IPXDAT
      END IF

*  If the mode is "STEP"...
      IF ( IMODE .EQ. 4 ) THEN

*  Map the spectral axis widths.
         CALL NDF_AMAP( INDF, 'WIDTH', LPAXIS, '_DOUBLE', 'READ', 
     :                  IPAWID, AEL, STATUS ) 

*  Allocate work space to hold the upper and lower limits of each
*  horizontal step on the displayed horizontal axis.
         CALL PSX_CALLOC( AEL * 2, '_DOUBLE', IPSTEP, STATUS )
         FRSTEP = ( STATUS .EQ. SAI__OK )

*  Abort if an error has occurred. 
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  If each point is not being drawn as a step, store safe pointer
*  values.
      ELSE
         IPAWID = IPXDAT
         IPSTEP = IPXDAT
      END IF

*  Map the component of the NDF which is to be plotted on the vertical
*  axis, in double precision.  Here we map the whole array as we require
*  the same vertical range applied to every line plot.  Hence we need
*  statistics for all values (and error bar limits).  Note the different
*  number of mapped elements compared with the axis limits.
      CALL NDF_MAP( INDF, MCOMP, '_DOUBLE', 'READ', IPYDAT, EL, STATUS )

*  Allocate work space to hold the displayed y-axis values (these may be
*  logged data values or raw data values).
      CALL PSX_CALLOC( EL, '_DOUBLE', IPYCEA, STATUS )
      FRYCEA = ( STATUS .EQ. SAI__OK )

*  If y-axis variance values are required map them. 
      IF ( YVAR ) THEN
         CALL NDF_MAP( INDF, 'VARIANCE', '_DOUBLE', 'READ', IPYVAR, EL,
     :                 STATUS )

*  Allocate work space to hold the upper and lower limits of each
*  vertical error bar.
         CALL PSX_CALLOC( EL * 2, '_DOUBLE', IPYBAA, STATUS )
         FRYBAA = ( STATUS .EQ. SAI__OK )

*  If y-axis variance values are not required store safe pointer values.
      ELSE
         IPYVAR = IPYDAT
         IPYBAA = IPYDAT
      END IF

*  See if the plot is to be produced over an existing DATA picture.  
*  If it is get a Plot for the existing DATA picture and merge the 
*  FrameSet created above with it so that we have a Plot which can be
*  used to draw the new data plot.
*  ==================================================================

*  If the plotting device wasn't cleared, the AGI identifier for any 
*  existing DATA picture (within the current picture) is returned in
*  PICIDE, and the corresponding AST Plot is returned in IPLOTS.
      CALL KPS1_CLIPA( AST__NULL, 'UNKNOWN', ' ', CLEAR, PICIDG,
     :                 PICIDE, IPLOTS, STATUS )

*  Record the world co-ordinates of the spatial data picture.
      CALL PGQWIN( X1DATA, X2DATA, Y1DATA, Y2DATA )

*  Set the dimensions of the grid of pictures with more-memorable names.
      XPIC = WDIM( 1 )
      YPIC = WDIM( 2 )

*  Obtain the picture to enclose the top-right spatial pixel.  Neither
*  an outline nor a FRAME is required for this test plot.
      CALL KPS1_CLIPG( PICIDA, XPIC, YPIC, XPIC, YPIC, LPMARG, BOX,
     :                 1, 'CLINPLOT', .FALSE., .FALSE., PICIDG, PICIDF,
     :                 STATUS )

*  Make that the current picture.
      CALL AGI_SELP( PICIDG, STATUS )

*  Set a flag to indicate the new DATA picture should be created
*  covering the same area as an existing DATA picture.
      OLDPIC = ( IPLOTS .NE. AST__NULL )

*  Indicate we have not yet created the new DATA picture.
      NEWPIC = .FALSE.

*  If we are drawing within an existing DATA picture...
      IF ( OLDPIC ) THEN

*  Is the current Frame in the Plot associated with the existing DATA
*  picture a DATAPLOT Frame?  If not, it was not created by this
*  application and so we cannot align the new picture with it.
         IF ( AST_GETC( IPLOTS, 'DOMAIN', STATUS ) .EQ. 
     :        'DATAPLOT' ) THEN

*  Attempt to find a Mapping from the DATAPLOT Frame in the existing 
*  picture to the DATAPLOT Frame in the new picture.
            DPFS = AST_CONVERT( AST_GETFRAME( IPLOTS, AST__CURRENT,
     :                                        STATUS ),
     :                          AST_GETFRAME( FSET, AST__CURRENT,
     :                                        STATUS ),
     :                          'DATAPLOT', STATUS )

*  Abort if an error has occurred.
            IF ( STATUS .NE. SAI__OK ) GO TO 999

*  We now decide whether to shift the x axis of the new DATAPLOT so that
*  it aligns with the existing DATAPLOT.  Set a default of TRUE if a
*  Mapping was found above, and FALSE if not.  Then allow the user to 
*  change the default.
            CALL PAR_DEF0L( 'ALIGN', ( DPFS .NE. AST__NULL ), STATUS )
            CALL PAR_GET0L( 'ALIGN', ALIGN, STATUS )

*  Use the dynamic default if a null value was supplied.
            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )
               ALIGN = ( DPFS .NE. AST__NULL )
            END IF

*  If they are to be aligned...
            IF ( ALIGN ) THEN

*  Report an error if no Mapping is available.
               IF ( DPFS .EQ. AST__NULL .AND. STATUS .EQ. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'CLINPLOT_ERR', 'Cannot convert '/
     :                          /'positions between the existing plot '/
     :                          /'and the new plot.', STATUS )
                  GO TO 999
               END IF

*  Save the index of the current Frame in the Plot. 
               ICURR0 = AST_GETI( IPLOTS, 'CURRENT', STATUS )

*  Save the number of Frames in the existing Plot.
               NFRM = AST_GETI( IPLOTS, 'NFRAME', STATUS )

*  Merge the new FrameSet into the existing Plot, aligning them in the 
*  current Frame (i.e. the DATAPLOT Domain) using a UnitMap.
               CALL AST_ADDFRAME( IPLOTS, AST__CURRENT, DPFS, FSET,
     :                           STATUS ) 

*  Save the indices within the Plot of the "what we've got" and "what 
*  we want" Frames relating to the original NDF.
               WWGOT = WWGOT + NFRM
               WWWANT = WWWANT + NFRM

*  Abort if an error has occurred.
               IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Create a new DATA picture with the same shape, position and bounds
*  as the existing one.  This routine accesses the 'FILL' parameter.
               CALL KPG1_PLOTP( PICIDE, 'CLINPLOT', INTMAR, 0, ' ', ' ',
     :                          0.0, 0.0, 0.0D0, PICIDD, PICIDF, PICIDK,
     :                          STATUS )

*  Indicate we have created a new DATA picture.
               NEWPIC = .TRUE.

*  Get the Mapping from the "what we've got" Frame for the new data, to
*  the "what we want" Frame.
               MAP = AST_SIMPLIFY( AST_GETMAPPING( IPLOTS, WWGOT, 
     :                                             WWWANT, STATUS ),
     :                             STATUS )

*  Map all the "what we've got" values into "what we want" values...
               
*  Data values and vertical error bar limits for pixels.
               CALL KPS1_LPLLM( EL, 1, EL, %VAL( CNF_PVAL( IPYDAT ) ), 
     :                          YVAR, .FALSE., 
     :                          %VAL( CNF_PVAL( IPYVAR ) ), YSIGMA, 
     :                          AST__NULL, MAP, 2, 
     :                          %VAL( CNF_PVAL( IPYCEA ) ),
     :                          %VAL( CNF_PVAL( IPYBAA ) ), 
     :                          TR( 2 ), BL( 2 ), MONO, BAD, STATUS )

*  The x-axis central values and horizontal error bar limits.
               CALL KPS1_LPLLM( AEL, 1, AEL, 
     :                          %VAL( CNF_PVAL( IPXDAT ) ), XVAR, 
     :                          .FALSE., %VAL( CNF_PVAL( IPXVAR ) ), 
     :                          XSIGMA, AXMAP, MAP, 1, 
     :                          %VAL( CNF_PVAL( IPXCEN ) ), 
     :                          %VAL( CNF_PVAL( IPXBAR ) ),
     :                          TR( 1 ), BL( 1 ), MONO, BAD, STATUS )
 
*  If the mode is "STEP", we also do x-axis width limits.
               IF ( IMODE .EQ. 4 ) THEN
                  CALL KPS1_LPLLM( AEL, 1, AEL, 
     :                             %VAL( CNF_PVAL( IPXDAT ) ), .TRUE.,
     :                             .TRUE., %VAL( CNF_PVAL( IPAWID ) ), 
     :                             0.5, AXMAP, MAP, 1, 
     :                             %VAL( CNF_PVAL( IPXCEN ) ),
     :                             %VAL( CNF_PVAL( IPSTEP ) ), 
     :                             XSMAX, XSMIN, MONO, BAD, STATUS )
               END IF

*  Indicate that the entire supplied array is to be plotted (only part 
*  of it may actually be visible).
               ILO = 1
               IHI = LPDIM
            END IF
         END IF        
      END IF

*  If the above code did not produce a Plot which can be used to draw
*  the new data plot, we create one now by creating a new DATA picture
*  within the current picture, making it as large as possible.  The
*  bounds of this picture are specified by the user, with the bounds of
*  the supplied data being used as the default picture bounds.
*  ====================================================================

      IF ( .NOT. NEWPIC .AND. STATUS .EQ. SAI__OK ) THEN 


*  Ensure the original current AGI picture is re-instated.
         CALL AGI_SELP( PICIDG, STATUS )

*  We need to be able to transform points in both directions between the
*  "what we want" Frame and the "uniform" Frame.  This requires all
*  Mappings in the DATAPLOT FrameSet to be invertable.  Report an error 
*  if this is not the case.
         IF ( NOINV ) THEN
            CALL MSG_SETC( 'LAB', AST_GETC( FSET, 'LABEL(1)', STATUS ) )

            STATUS = SAI__ERROR
            CALL ERR_REP( 'CLINPLOT_ERR1', 'The horizontal axis value '/
     :                    /'(^LAB) does not increase or decrease '/
     :                    /'monotonically along the data array, and '/
     :                    /'so cannot be used.', STATUS )

         END IF

*  Save the mapping from "what we've got" to "what we want" in the new
*  DATAPLOT FrameSet.
         MAP = AST_SIMPLIFY( AST_GETMAPPING( FSET, WWGOT, WWWANT, 
     :                                       STATUS ),  STATUS )

*  Find the limits of the axis 1 data value in the "what we want"
*  Frame, including any required error bars.  This returns the central 
*  and extreme x-axis values for each error bar in the "what we want"
*  Frame.
         CALL KPS1_LPLLM( AEL, 1, AEL, %VAL( CNF_PVAL( IPXDAT ) ), XVAR,
     :                    .FALSE., %VAL( CNF_PVAL( IPXVAR ) ), XSIGMA,
     :                    AXMAP, MAP, 1, %VAL( CNF_PVAL( IPXCEN ) ), 
     :                    %VAL( CNF_PVAL( IPXBAR ) ), TR( 1 ), BL( 1 ),
     :                    MONO, BAD, STATUS )
 
*  Report an error if either limit is bad.
         IF ( ( TR( 1 ) .EQ. AST__BAD .OR. BL( 1 ) .EQ. AST__BAD ) .AND.
     :          STATUS .EQ. SAI__OK ) THEN 
            STATUS = SAI__ERROR
            CALL ERR_REP( 'CLINPLOT_ERR3', 'No valid pixel '/
     :                    /'positions found.' , STATUS )
         END IF

*  We now need to extend the x-axis limits if each point is being
*  represented as a horizontal step equal to the AXIS WIDTH value.
*  If the mode is "STEP"...
         IF ( IMODE .EQ. 4 ) THEN

*  Find the limits of the steps on axis 1 in the "what we want" Frame.
*  This returns the central and extreme x-axis values for each step in 
*  the "what we want" Frame.
            CALL KPS1_LPLLM( AEL, 1, AEL, %VAL( CNF_PVAL( IPXDAT ) ), 
     :                       .TRUE., .TRUE., %VAL( CNF_PVAL( IPAWID ) ),
     :                       0.5, AXMAP, MAP, 1, 
     :                       %VAL( CNF_PVAL( IPXCEN ) ), 
     :                       %VAL( CNF_PVAL( IPSTEP ) ), XSMAX,
     :                       XSMIN, MONO, BAD, STATUS )

*  Report an error if either limit is bad.
            IF ( ( XSMAX .EQ. AST__BAD .OR. XSMIN .EQ. AST__BAD ) .AND.
     :           STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'CLINPLOT_ERR4', 'No valid AXIS '/
     :                       /'widths found.', STATUS )
            END IF

*  Ensure the x-axis limits include the steps.
            TR( 1 ) = MAX( TR( 1 ), XSMAX )
            BL( 1 ) = MIN( BL( 1 ), XSMIN )
         END IF

*  If the annotated values on the horizontal axis is reversed, reverse
*  the limits.
         IF ( MONO .EQ. -1 ) THEN
            DVAL = BL( 1 )
            BL( 1 ) = TR( 1 ) 
            TR( 1 ) = DVAL
         END IF

*  Ensure the limits are not equal.
         IF ( BL( 1 ) .EQ. TR( 1 ) ) THEN
            IF ( BL( 1 ) .NE. 0.0 ) THEN
               TR( 1 ) = 2.0 * BL( 1 )
            ELSE
               TR( 1 ) = 1.0D0
            END IF
         END IF

*  We now have the limits of the data on the x axis of the "what we 
*  want" Frame.  Allow the user to override these limits. 
         CALL KPG1_GTAXV( 'XLEFT', 1, .TRUE., FSET, 1, BL( 1 ), 
     :                    NVAL, STATUS )
         CALL KPG1_GTAXV( 'XRIGHT', 1, .TRUE., FSET, 1, TR( 1 ),
     :                    NVAL, STATUS )

*  Find the corresponding range of GRID indices.  Use an arbitrary value
*  of 0.0 for the y axis when doing this Mapping (any Y value will do 
*  since the axes are independent).
         BL( 2 ) = 0.0D0
         TR( 2 ) = 0.0D0
         CALL AST_TRANN( MAP, 1, 2, 1, BL, .FALSE., 2, 1, BLG, STATUS ) 
         CALL AST_TRANN( MAP, 1, 2, 1, TR, .FALSE., 2, 1, TRG, STATUS ) 
         ILO = MAX( 1, MIN( LPDIM, INT( MIN( BLG( 1 ), TRG( 1 ) ) ) ) )
         IHI = MAX( 1, MIN( LPDIM, INT( 0.5D0 + MAX( BLG( 1 ), 
     :         TRG( 1 ) ) ) ) )

*  Map the trimmed NDF arrays.
*  ===========================

*  Now we know the limits of the selected data in both the spatial and
*  spectral regions.  If these are interior to the widest limits, we
*  need the section visible in the plot in which to determine the y-axis
*  limits.
         IF ( ILO .GT. 1 .OR. IHI .LT. AEL .OR.
     :        LBND( SPAXIS( 1 ) ) .GT. SLBND( SPAXIS( 1 ) ) .OR.
     :        UBND( SPAXIS( 1 ) ) .LT. SUBND( SPAXIS( 1 ) ) .OR.
     :        LBND( SPAXIS( 2 ) ) .GT. SLBND( SPAXIS( 2 ) ) .OR.
     :        UBND( SPAXIS( 2 ) ) .LT. SUBND( SPAXIS( 2 ) ) ) THEN

*  Unmap the data and axes.
            CALL NDF_UNMAP( INDFI, '*', STATUS )
            CALL NDF_AUNMP( INDFI, '*', LPAXIS, STATUS )

*  Define limits of new section.  The spatial bounds are already
*  assigned.  We just need to adjust the spectral limits, converting
*  from GRID to PIXEL with the origin
            LBND( LPAXIS ) = ILO + SLBND( LPAXIS ) - 1
            UBND( LPAXIS ) = IHI + SLBND( LPAXIS ) - 1

*  Obtain an NDF identifier for this section of the supplied NDF.
            CALL NDF_SECT( INDF, NDIMS, LBND, UBND, INDFI, STATUS ) 

*  Map the required NDF arrays.
*  ----------------------------

*  Map the NDF AXIS-centre values along the spectral axis in double
*  precision.
            CALL NDF_AMAP( INDFI, 'CENTRE', LPAXIS, '_DOUBLE', 'READ',
     :                     IPXDAT, AEL, STATUS ) 

*  If line-plot x-axis variance values are required, map them.   We
*  have already tested for the presence of the VARIANCE array.
            IF ( XVAR ) THEN
               CALL NDF_AMAP( INDFI, 'VARIANCE', LPAXIS, '_DOUBLE', 
     :                        'READ', IPXVAR, AEL, STATUS ) 

            END IF

*  If the mode is "STEP"...
            IF ( IMODE .EQ. 4 ) THEN

*  Map the spectral axis widths.
               CALL NDF_AMAP( INDFI, 'WIDTH', LPAXIS, '_DOUBLE', 'READ',
     :                        IPAWID, AEL, STATUS )

*  Abort if an error has occurred. 
               IF ( STATUS .NE. SAI__OK ) GO TO 999
            END IF

*  Map the component of the NDF which is to be plotted on the vertical
*  axis, in double precision.  Here we map the whole array as we require
*  the same vertical range applied to every line plot.  Hence we need
*  statistics for all values (and error bar limits).  Note the different
*  number of mapped elements compared with the axis limits.
            CALL NDF_MAP( INDFI, MCOMP, '_DOUBLE', 'READ', IPYDAT, EL,
     :                    STATUS )

*  If y-axis variance values are required map them. 
            IF ( YVAR ) THEN
               CALL NDF_MAP( INDFI, 'VARIANCE', '_DOUBLE', 'READ',
     :                       IPYVAR, EL, STATUS )

            END IF
            
*  Reset the number of pictures.
            XPIC = UBND( SPAXIS( 1 ) ) - LBND( SPAXIS( 1 ) ) + 1      
            YPIC = UBND( SPAXIS( 2 ) ) - LBND( SPAXIS( 2 ) ) + 1      

         END IF

*  Find the limits of the y axis (axis 2) data value in the "what we 
*  want" Frame, including any required error bars.  This returns the 
*  central and extreme y-axis values for each error bar in the "what we 
*  want" Frame.  The returned limits only include the data in the grid
*  index range covered by the x axis.
         CALL KPS1_LPLLM( EL, 1, EL, %VAL( CNF_PVAL( IPYDAT ) ),
     :                    YVAR, .FALSE., %VAL( CNF_PVAL( IPYVAR ) ), 
     :                    YSIGMA, AST__NULL, MAP, 2, 
     :                    %VAL( CNF_PVAL( IPYCEA ) ), 
     :                    %VAL( CNF_PVAL( IPYBAA ) ), TR( 2 ),
     :                    BL( 2 ), MONO, BAD, STATUS )

*  Report an error if either limit is bad.
         IF ( ( TR( 2 ) .EQ. AST__BAD .OR. BL( 2 ) .EQ. AST__BAD ) .AND.
     :        STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'CLINPLOT_ERR5',
     :                    'No valid data values found.', STATUS )
         END IF

*  Because the number of elements in the array and the co-ordinates will
*  generally not be the same---routine KPS1_GRLM2 was written for LINPLOT
*  where they are the same length---we first have to duplicate the mask.
*  First obtain some workspace, then duplicate the values.
         NDUP = EL / AEL
         CALL PSX_CALLOC( EL, '_DOUBLE', IPXCEF, STATUS )

         CALL KPG1_PXDPD( AEL, %VAL( CNF_PVAL( IPXCEN ) ), NDUP, EL,
     :                    %VAL( CNF_PVAL( IPXCEF ) ), STATUS )

*  Find suitable default values for YTOP and YBOT.
         BL( 2 ) = VAL__BADD
         TR( 2 ) = VAL__BADD
         CALL KPG1_GRLM2( 'LMODE', EL, %VAL( CNF_PVAL( IPYCEA ) ),
     :                    %VAL( CNF_PVAL( IPXCEF ) ), YVAR,
     :                    %VAL( CNF_PVAL( IPYBAA ) ), BL( 2 ), TR( 2 ),
     :                    STATUS )

*  Ensure the limits are not equal.
         IF ( BL( 2 ) .EQ. TR( 2 ) ) THEN
            IF ( BL( 2 ) .NE. 0.0 ) THEN
               TR( 2 ) = 2.0 * BL( 2 )
            ELSE
               TR( 2 ) = 1.0D0
            END IF
         END IF

*  We now have the limits of the data on the y axis of the "what we
*  want" Frame.  Allow the user to override these limits. 
         CALL KPG1_GTAXV( 'YBOT', 1, .TRUE., FSET, 2, BL( 2 ), 
     :                    NVAL, STATUS )
         CALL KPG1_GTAXV( 'YTOP', 1, .TRUE., FSET, 2, TR( 2 ),
     :                    NVAL, STATUS )

*  If the y axis will be logarithmic, ensure that the limits are of the
*  same sign.
         IF ( YMAP .EQ. 'LOG' ) THEN
            IF ( TR( 2 ) * BL( 2 ) .LE. 0.0 ) THEN            
               CALL MSG_BLANK( STATUS )
               CALL MSG_OUT( ' ', '  Cannot use logarithmic mapping '/
     :                       /'for the y-axis (parameter YMAP) since '/
     :                       /'the axis range specified by YTOP and '/
     :                       /'YBOT includes zero.  Try a different '/
     :                       /'value for parameter LMODE.', STATUS )
            END IF
         END IF

*  Map these positions into the Base (uniform) Frame.
         CALL AST_TRANN( FSET, 1, 2, 1, BL, .FALSE., 2, 1, BLG, STATUS )
         CALL AST_TRANN( FSET, 1, 2, 1, TR, .FALSE., 2, 1, TRG, STATUS )

*  Abort if an error has occurred.
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Create a new DATA picture.  Make it as large as possible within the
*  bounds of the current AGI picture, and give it the AGI world
*  co-ordinate bounds determined above.  The PGPLOT viewport is changed 
*  to represent the new DATA picture.
         BOX( 1 ) = BLG( 1 )
         BOX( 2 ) = BLG( 2 )
         BOX( 3 ) = TRG( 1 )
         BOX( 4 ) = TRG( 2 )

*  Save the indices within the Plot of the "what we've got" and "what we
*  want" Frames relating to the original NDF.
         WWGOT = WWGOT + 1
         WWWANT = WWWANT + 1

      END IF

*  Get workspace needed to plot a single spectrum in the GRAPHICS Frame.
*  =====================================================================

*  Allocate work space to hold the displayed y-axis values (these may be
*  logged data values or raw data values).  Here we only want the length
*  of a single spectrum.
      CALL PSX_CALLOC( AEL, '_DOUBLE', IPYCEN, STATUS )
      FRYCEN = ( STATUS .EQ. SAI__OK )

*  Allocate work space to hold the y-axis centres values to be displayed
*  on the vertical axis of the Plot in GRAPHICS co-ordinates.
      CALL PSX_CALLOC( AEL, '_DOUBLE', IPYCEG, STATUS )
      FRYCEG = ( STATUS .EQ. SAI__OK )

*  If y-axis variance values are required...
      IF ( YVAR ) THEN

*  Allocate work space to hold the upper and lower limits of each
*  vertical error bar.
         CALL PSX_CALLOC( AEL * 2, '_DOUBLE', IPYBAR, STATUS )
         FRYBAR = ( STATUS .EQ. SAI__OK )

*  If y-axis variance values are not required store safe pointer values.
      ELSE
         IPYBAR = IPYDAT
      END IF

*  Allocate work space to hold the x-axis centres values to be displayed
*  on the horizontal axis of the Plot in GRAPHICS co-ordinates.
      CALL PSX_CALLOC( AEL, '_DOUBLE', IPXCEG, STATUS )
      FRXCEG = ( STATUS .EQ. SAI__OK )

      IF ( XVAR ) THEN

*  Allocate work space to hold the upper and lower limits of each 
*  horizontal error bar on the displayed horizontal axis, again in
*  GRAPHICS co-ordinates.
         CALL PSX_CALLOC( AEL * 2, '_DOUBLE', IPXBAG, STATUS )
         FRXBAG = (STATUS .EQ. SAI__OK )

*  If x-axis variance values are not required store safe pointer values.
      ELSE
         IPXBAG = IPXCEN
      END IF

*  If the mode is "STEP"...
      IF ( IMODE .EQ. 4 ) THEN

*  Allocate work space to hold the upper and lower limits of each
*  horizontal step on the displayed horizontal axis in GRAPHIC
*  co-ordinates.
         CALL PSX_CALLOC( AEL * 2, '_DOUBLE', IPSTEG, STATUS )
         FRSTEG = ( STATUS .EQ. SAI__OK )

*  If each point is not being drawn as a step, store safe pointer
*  values.
      ELSE
         IPSTEP = IPXDAT
      END IF

*  See if annotated axes are required.  The default is no.
      CALL PAR_DEF0L( 'REFAXES', .NOT. NEWPIC, STATUS )
      CALL PAR_GET0L( 'REFAXES', RFAXES, STATUS )

*  Use the dynamic default if a null value was supplied.
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         RFAXES = .TRUE.
      END IF

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Create default sections for each line plot.
*  ===========================================

*  Specify the bounds by making all but the spectral axis degenerate.
      DO I = 1, NDF__MXDIM
         LBNDS( I ) = 1
         UBNDS( I ) = 1
      END DO
      LBNDS( LPAXIS ) = LBND( LPAXIS )
      UBNDS( LPAXIS ) = UBND( LPAXIS )

*  Specify the bounds of the error-bar array.  This is effectively
*  the same as data value bounds except it has an extra two-element
*  dimension. 
      IF ( YVAR ) THEN
         DO I = 1, NDIM
            BLBND( I ) = LBND( I )
            BUBND( I ) = UBND( I )
         END DO
         BLBND( NDIM + 1 ) = 1
         BUBND( NDIM + 1 ) = 2
         UBNDS( NDIM + 1 ) = 2
      END IF

*  Loop around the grid of spectra.
*  ================================

*  Want to record the grid indices of the last plotted picture in case
*  there are regions of bad orr undefined data within the cube.
      LIPIC = XPIC
      LJPIC = YPIC

*  Keep a tally of the spatial pixels processed. 
      NPIC = XPIC * YPIC + 1
      IPLOTL = -1

*  Loop in reverse order such that the last plot is the lower-left.
*  Derive the `grid' indices of the pictures.
      DO J = UBND( SPAXIS( 2 ) ), LBND( SPAXIS( 2 ) ), -1
         JPIC = J - LBND( SPAXIS( 2 ) ) + 1
         DO I = UBND( SPAXIS( 1 ) ), LBND( SPAXIS( 1 ) ), -1
            IPIC = I - LBND( SPAXIS( 1 ) ) + 1

            NPIC = NPIC - 1
            LAST = NPIC .EQ. 1

*  Extract the w.w.want Y centres and error bars.
*  ==============================================

*  We've already obtained the mapped y centres for the whole NDF.  Now 
*  we need to extract those for the current spatial pixel (i.e. 
*  spectrum).  Set the spatial bounds for the current pixel/spectrum.
            LBNDS( SPAXIS( 1 ) ) = I
            UBNDS( SPAXIS( 1 ) ) = I
            LBNDS( SPAXIS( 2 ) ) = J
            UBNDS( SPAXIS( 2 ) ) = J

*  Retrieve the current spectrum's y centres.  Note that LBND and UBND
*  include all dimensions, not just the significant ones, hence the
*  NDF__MXDIM.
            CALL KPG1_CPNDD( NDF__MXDIM, LBND, UBND, 
     :                       %VAL( CNF_PVAL( IPYCEA ) ), LBNDS,
     :                       UBNDS, %VAL( CNF_PVAL( IPYCEN ) ), NEL,
     :                       STATUS )

*  Check that there are good values present.  By definition AEL is NEL.
            CALL KPG1_NBADD( AEL, %VAL( CNF_PVAL( IPYCEN ) ), NBAD,
     :                       STATUS )
            VISIBL = NBAD .LT. AEL
            IF ( VISIBL .OR. ( LAST .AND. RFAXES ) ) THEN

*  If y-axis variance values are required...
               IF ( YVAR .AND. VISIBL ) THEN

*  We've already obtained the mapped y error bars for the whole NDF.
*  Now we need to extract those for the current spatial pixel (i.e. 
*  spectrum).  Retrieve the current spectrum's y error bars.
                  CALL KPG1_CPNDD( NDIM + 1, BLBND, BUBND, 
     :                             %VAL( CNF_PVAL( IPYBAA ) ), LBNDS,
     :                             UBNDS, %VAL( CNF_PVAL( IPYBAR ) ),
     :                             NEL, STATUS )
               END IF

*  Create the DATA picture in the interior grid.
*  =============================================

*  The lower-left spatial pixel/line plot will normally require
*  labelled and annotated axes.  So we also create a FRAME picture that 
*  will enclose the lower-left line plot and extend beyond the line
*  plot only to the left and below (unless there is only one line plot),
*  reaching the edge of the image DATA picture.  For the lower-left
*  picture we may also create a FRAME picture in which to write the axis
*  annotations.

*  Restore the former current picture (the DATA picture from the
*  spatial display), and its world co-ordinates.
               CALL AGI_SELP( PICIDA, STATUS )
               CALL AGP_NVIEW( .FALSE., STATUS )
               CALL PGSWIN( X1DATA, X2DATA, Y1DATA, Y2DATA )

*  Obtain the spatial co-ordinates of the current spatial pixel
*  by transforming from pixel to the current Frame's co-ordinates.
*  IPIC and JPIC refer to the visible section, but the grid co-ordinates
*  are in terms of the input array bounds.
               IF ( VISIBL ) THEN
                  GC( SPAXIS( 1 ) ) = DBLE( I - SLBND( SPAXIS( 1 ) )
     :                                      + 1 )
                  GC( SPAXIS( 2 ) ) = DBLE( J - SLBND( SPAXIS( 2 ) ) 
     :                                      + 1 )
               ELSE

*  LIPIC and LJPIC refer to the visible section, but the
*  grid co-ordinates are in terms of the input array bounds.
                  GC( SPAXIS( 1 ) ) = DBLE( LIPIC - 
     :                                      SLBND( SPAXIS( 1 ) ) +
     :                                      LBND( SPAXIS( 1 ) ) )
                  GC( SPAXIS( 2 ) ) = DBLE( LJPIC -
     :                                      SLBND( SPAXIS( 2 ) ) +
     :                                      LBND( SPAXIS( 2 ) ) )
               END IF

               CALL AST_TRANN( IWCSI, 1, NSPDIM, 1, GC, .TRUE., NSPDIM,
     :                         1, CC, STATUS )

*  Convert these co-ordinates to text strings.
               IF ( VISIBL ) THEN
                  XCOTXT = AST_FORMAT( IWCS, SPAXIS( 1 ), CC( 1 ),
     :                                 STATUS )
                  YCOTXT = AST_FORMAT( IWCS, SPAXIS( 2 ), CC( 2 ),
     :                                 STATUS )
               END IF

*  Assign the DATA pictures' world co-ordinate limits.
               BOX( 1 ) = BLG( 1 )
               BOX( 2 ) = BLG( 2 )
               BOX( 3 ) = TRG( 1 )
               BOX( 4 ) = TRG( 2 )

*  Create a DATA picture in the grid of line-plot pictures.  This is
*  within the outer spatial plot with the interior margins.  First
*  create a FRAME picture enclosing the DATA picture, when it's needed.
*  The FRAME isn't required when RFAXES is FALSE, as there are no axes
*  to draw, or when there is no room, i.e. it's not the last picture.
*  Draw an outline.
               COMENT = XCOTXT( :CHR_LEN( XCOTXT ) )//',  '//YCOTXT
               IF ( VISIBL ) THEN
                  CALL KPS1_CLIPG( PICIDA, XPIC, YPIC, IPIC, JPIC, 
     :                             LPMARG, BOX, 1, COMENT, 
     :                             LAST .AND. RFAXES, 
     :                             .TRUE., PICIDG, PICIDF, STATUS )
               ELSE
                  CALL KPS1_CLIPG( PICIDA, XPIC, YPIC, LIPIC, LJPIC, 
     :                             LPMARG, BOX, 1, COMENT, 
     :                             LAST .AND. RFAXES, 
     :                             .TRUE., PICIDG, PICIDF, STATUS )
               END IF

*  Select the created DATA picture, and set the viewport from it.
               CALL AGI_SELP( PICIDG, STATUS )
               CALL AGP_NVIEW( .FALSE., STATUS )

*  Obtain a Plot for the new DATA picture if we do not yet have one
*  (i.e. if the new DATA picture was not aligned with an existing DATA
*  picture).
               IF ( .NOT. ALIGN ) THEN
                  CALL KPG1_PLOTN( AST__NULL, 'AGI_WORLD', PICIDG,
     :                             .TRUE., IPLOTS, NFRM, STATUS )
               END IF

*  Create a Plot covering this viewport.
*               CALL KPG1_ASPLT( FSET, BOX, ' ', IPLOTS, STATUS )

*  Save the index of its current Frame.
               ICURR0 = AST_GETI( IPLOTS, 'CURRENT', STATUS )

*  Set the additional style for plotting in the bottom-left reference
*  line-plot picture. 
               IF ( LAST .AND. RFAXES ) THEN

*  The height of the text needs adjustment as the density of the grid
*  increases.  The density, limits and scaling are something of a guess.
                  RHOPIC = SQRT( REAL( MAX( XPIC, YPIC ) ) )
                  LABSIZ = MAX( THTMIN, MIN( THTMAX, TEXTHT / RHOPIC ) )
                  NUMSIZ = MAX( NHTMIN, MIN( NHTMAX, NUMHT / RHOPIC ) )
                  CALL AST_SETR( IPLOTS, 'Size(TextLab)', 
     :                           LABSIZ, STATUS )
                  CALL AST_SETR( IPLOTS, 'Size(NumLab)', 
     :                           NUMSIZ, STATUS )

*  Obtain used-defined attributes for the lower-left plot.
                  CALL KPG1_ASSET( 'KAPPA_CLINPLOT', 'REFSTYLE',
     :                             IPLOTS, STATUS )
               END IF

*  Save the Plot and data reference with the new DATA picture.  The 
*  Frame with index ICURR0 is made the current Frame for the Plot in
*  the AGI database.
               CALL KPG1_PLOTS( IPLOTS, PICIDG, NDFNAM( : NC ), ICURR0, 
     :                          'AGI_WORLD', 'DATAPLOT', STATUS )

*  Abort if an error has occurred.
               IF ( STATUS .NE. SAI__OK ) GO TO 999

*  We now have a Plot and a new DATA picture.  Prepare to produce the
*  graphical output.
*  ==================================================================

*  Set the LogPlot attributes in the Plot appropriately.
               CALL AST_SETL( IPLOTS, 'LOGPLOT(1)', ( XMAP .EQ. 'LOG' ),
     :                        STATUS )
               CALL AST_SETL( IPLOTS, 'LOGPLOT(2)', ( YMAP .EQ. 'LOG' ),
     :                        STATUS )

*  Get the one-dimensional mappings which transform each of the 
*  GRAPHICS Frame axes on to the corresponding "what we want" Frame
*  axes.
               CALL KPG1_ASSPL( IPLOTS, 2, AXMAPS, STATUS )

               IF ( VISIBL ) THEN

*  Map all the required axis values from "what we want" into GRAPHICS.
                  CALL AST_TRAN2( IPLOTS, AEL,
     :                            %VAL( CNF_PVAL( IPXCEN ) ),
     :                            %VAL( CNF_PVAL( IPYCEN ) ), .FALSE., 
     :                            %VAL( CNF_PVAL( IPXCEG ) ), 
     :                            %VAL( CNF_PVAL( IPYCEG ) ), STATUS )

                  IF ( XVAR ) CALL AST_TRAN1( AXMAPS( 1 ), 2 * AEL, 
     :                                      %VAL( CNF_PVAL( IPXBAR ) ),
     :                                      .FALSE.,
     :                                      %VAL( CNF_PVAL( IPXBAG ) ), 
     :                                      STATUS )

                  IF ( YVAR ) CALL AST_TRAN1( AXMAPS( 2 ), 2 * AEL, 
     :                                      %VAL( CNF_PVAL( IPYBAA ) ),
     :                                      .FALSE., 
     :                                      %VAL( CNF_PVAL( IPYBAG ) ), 
     :                                      STATUS )

                  IF ( IMODE .EQ. 4 ) THEN
                     CALL AST_TRAN1( AXMAPS( 1 ), 2 * AEL,
     :                               %VAL( CNF_PVAL( IPSTEP ) ),
     :                               .FALSE.,
     :                               %VAL( CNF_PVAL( IPSTEG ) ),
     :                               STATUS )
                  END IF
               END IF

*  Allow the user to specify the units for either axis.
               CALL AST_SETACTIVEUNIT( IPLOTS, .TRUE., STATUS )

*  Set the attributes of the Plot to give the required Plotting style
*  in the line-plot pictures.
               CALL KPG1_ASSET( 'KAPPA_CLINPLOT', 'LPSTYLE', IPLOTS,
     :                          STATUS )

*  Produce the plot.
*  =================

*  Produce the data plot.
               IF ( VISIBL ) THEN
                  CALL KPG1_PLTLN( LPDIM, ILO, IHI,
     :                             %VAL( CNF_PVAL( IPXCEG ) ),
     :                             %VAL( CNF_PVAL( IPYCEG ) ),
     :                             XVAR, YVAR,
     :                             %VAL( CNF_PVAL( IPXBAG ) ),
     :                             %VAL( CNF_PVAL( IPYBAG ) ),
     :                             %VAL( CNF_PVAL( IPSTEG ) ), 
     :                             'LPSTYLE', IPLOTS, IMODE, MTYPE,
     :                             ISHAPE, FREQ, 'KAPPA_CLINPLOT',
     :                             STATUS )

*  If the plot was fine, record its indices in case its the last visible
*  plot.
                   IF ( STATUS .EQ. SAI__OK ) THEN
                      LIPIC = IPIC
                      LJPIC = JPIC
                   END IF
               END IF
            END IF

*  Draw the grid if required.  KPG1_ASGRD normally draws two spectral
*  axes for a DSBSpectrum, one sideband at the foot and the other
*  sideband at top.  This will cluttered the second unless.  So we use
*  a KAPPA `pseudo-attribute' to disable the top axis when there
*  is more than one row in the visible line-plot grid, or the last
*  visible plot lies in the top row.  The later is the case where the
*  lower-left spatial position only contains bad values and hence is
*  not plotted, and hence we need to annotate the last visible plot.
            IF ( ( VISIBL .AND. LPAXES ) .OR. 
     :           ( RFAXES .AND. LAST ) ) THEN

               IF ( ( VISIBL .AND. YPIC .GT. 1 ) .OR. 
     :              ( LAST .AND. LJPIC .LT. YPIC ) ) THEN
                  CALL KPG1_ASSTS( 'DrawDSB=0', .FALSE., .TRUE.,
     :                             IPLOTS, BADAT, STATUS )
               END IF
               IF ( LPAXES .OR. ( RFAXES .AND. LAST ) ) THEN

*  Draw the grid/axes.
                  CALL KPG1_ASGRD( IPLOTS, PICIDF, .TRUE., STATUS )
               END IF

*  Free up the previous Plot.
               IF ( VISIBL ) THEN
                  IF ( IPLOTL .NE. -1 ) CALL AST_ANNUL( IPLOTL, STATUS )
                  IPLOTL = IPLOTS
               END IF
            END IF

*  Free-up precious AGI resources.
            CALL AGI_ANNUL( PICIDG, STATUS )

         END DO
      END DO

*  Shutdown procedure.
*  ===================

 999  CONTINUE

*  Free any memory used.
      IF ( FRSTEG ) CALL PSX_FREE( IPSTEG, STATUS )
      IF ( FRSTEP ) CALL PSX_FREE( IPSTEP, STATUS )
      IF ( FRXBAG ) CALL PSX_FREE( IPXBAG, STATUS )
      IF ( FRXBAR ) CALL PSX_FREE( IPXBAR, STATUS )
      IF ( FRXCEF ) CALL PSX_FREE( IPXCEF, STATUS )
      IF ( FRXCEG ) CALL PSX_FREE( IPXCEG, STATUS )
      IF ( FRXCEN ) CALL PSX_FREE( IPXCEN, STATUS )
      IF ( FRYBAA ) CALL PSX_FREE( IPYBAA, STATUS )
      IF ( FRYBAG ) CALL PSX_FREE( IPYBAG, STATUS )
      IF ( FRYBAR ) CALL PSX_FREE( IPYBAR, STATUS )
      IF ( FRYCEA ) CALL PSX_FREE( IPYCEA, STATUS )
      IF ( FRYCEG ) CALL PSX_FREE( IPYCEG, STATUS )
      IF ( FRYCEN ) CALL PSX_FREE( IPYCEN, STATUS )

*  Shutdown PGPLOT and the graphics database.
      CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Add a context report if anything went wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CLINPLOT_ERR', 'CLINPLOT: Failed to plot '//
     :                 'line plots of a three-dimensional data set.', 
     :                 STATUS )
      END IF

      END
