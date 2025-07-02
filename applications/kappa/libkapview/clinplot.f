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

*  Invocation:
*     CALL CLINPLOT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application displays a three-dimensional NDF as a series of
*     line plots of array value against position, arranged on a
*     uniform spatial grid and plotted on the current graphics device.
*     The vertical axis of each line plot represents array value, and
*     the horizontal axis represents position along a chosen axis (see
*     Parameter USEAXIS).  All the line plots have the same axis
*     limits.
*
*     This application will typically be used to display a grid of
*     spectra taken from a cube in which the current WCS Frame
*     includes one spectral axis (e.g. frequency) and two spatial axes
*     (e.g. RA and Dec). For this reason the following documentation
*     refers to the "spectral axis" and the "spatial axes". However,
*     cubes containing other types of axes can also be displayed, and
*     references to "spectral" and "spatial" axes should be
*     interpreted appropriately.
*
*     A rectangular grid of NX by NY points (see Parameters NX and NY)
*     is defined over the spatial extent of the cube, and a spectrum
*     is drawn at each such point. If NX and NY equal the spatial
*     dimensions of the cube (which is the default for spatial axes of
*     fewer than 31 pixels), then one spectrum is drawn for every
*     spatial pixel in the cube. For speed, the spectrum will be
*     binned up so that the number of elements in the spectrum does
*     not exceed the horizontal number of device pixels available for
*     the line plot.
*
*     Annotated axes for the spatial co-ordinates may be drawn around
*     the grid of line plots (see Parameter AXES).  The appearance of
*     these and the space they occupy may be controlled in detail (see
*     Parameters STYLE and MARGIN).
*
*     The plot may take several different forms such as a
*     "join-the-dots" plot, a "staircase" plot, a "chain" plot (see
*     Parameter MODE).  The plotting style (colour, founts, text size,
*     etc.) may be specified in detail using Parameter SPECSTYLE.
*
*     The data value at the top and bottom of each line plot can be
*     specified using Parameters YBOT and YTOP.  The defaults can be
*     selected in several ways including percentiles (see Parameter
*     LMODE).
*
*     The current picture is usually cleared before plotting the new
*     picture, but Parameter CLEAR can be used to prevent this,
*     allowing the plot (say) to be drawn over the top of a previously
*     displayed grey scale image.
*
*     The range and nature of the vertical and horizontal axes in each
*     line plot can be displayed in a key to the right of the main
*     plot (see Parameter KEY).  Also, an option exists to add
*     numerical labels to the first (i.e. bottom left) line plot, see
*     Parameter REFLABEL.  However, due to the nature of the plot, the
*     text used may often be too small to read.

*  Usage:
*     clinplot ndf [useaxis] [device] [nx] [ny]

*  ADAM Parameters:
*     ALIGN = _LOGICAL (Read)
*        Controls whether or not the spectra should be aligned spatially
*        with an existing data plot.  If ALIGN is TRUE, each spectrum
*        will be drawn in a rectangular cell that is centred on the
*        corresponding point on the sky.  This may potentially cause the
*        spectra to overlap, depending on their spatial separation.  If
*        ALIGN is FALSE, then the spectra are drawn in a regular grid of
*        equal-sized cells that cover the entire picture.  This may cause
*        them to be drawn at spatial positions that do not correspond to
*        their actual spatial positions within the supplied cube.  The
*        dynamic default is TRUE if Parameter CLEAR is TRUE and there is
*        an existing DATA picture on the graphics device. []
*     AXES = _LOGICAL (Read)
*        TRUE if labelled and annotated axes describing the spatial
*        are to be drawn around the outer edges of the plot. The
*        appearance of the axes can be controlled using the STYLE
*        parameter. The dynamic default is to draw axes only if the
*        CLEAR parameter indicates that the graphics device is not
*        being cleared.  []
*     BLANKEDGE = _LOGICAL (Read)
*        If TRUE then no tick marks or labels are placed on the edges
*        of line plots that touch the outer spatial axes (other edges
*        that do not touch the outer axes will still be annotated).
*        This can avoid existing tick marks being over-written when
*        drawing a grid of spectra over the top of a picture that
*        includes annotated axes. The dynamic default is TRUE if and
*        only if the graphics device is not being cleared (i.e.
*        Parameter CLEAR is FALSE) and no spatial axes are being drawn
*        (i.e. Parameter AXES is FALSE). []
*     CLEAR = _LOGICAL (Read)
*        If TRUE the current picture is cleared before the plot is
*        drawn.  IF FALSE, then the display is left uncleared and an
*        attempt is made to align the spatial axes of the new plot
*        with any spatial axes of the existing plot.  Thus, for
*        instance, a while light image may be displayed using DISPLAY,
*        and then spectra drawn over the top of the image using this
*        application.  [TRUE]
*     COMP = LITERAL (Read)
*        The NDF array component to be displayed.  It may be "Data",
*        "Quality", "Variance", or "Error" (where "Error" is an
*        alternative to "Variance" and causes the square root of the
*        variance values to be displayed).  If "Quality" is specified,
*        then the quality values are treated as numerical values (in
*        the range 0 to 255).  ["Data"]
*     DOWNSAMPLE = _LOGICAL (Read)
*        If TRUE each spectrum is downsampled prior to plotting so that
*        there are no more samples than device pixels.  If MODE is
*        "Histogram" or "GapHistogram" there should be four pixels
*        per sample.  [TRUE]
*     DEVICE = DEVICE (Read)
*        The name of the graphics device used to display the cube.
*        [current graphics device]
*     FILL = _LOGICAL (Read)
*        If FILL is set to TRUE, then the display will be `stretched'
*        to fill the current picture in both directions.  This can be
*        useful to elongate the spectra to reveal more detail by using
*        more of the display surface at the cost of different spatial
*        scales, and when the spatial axes have markedly different
*        dimensions.  The dynamic default is TRUE if either of the
*        spatial diensions is one. and FALSE otherwise.  []
*     KEY = _LOGICAL (Read)
*        If TRUE, then a "key" will be drawn to the right of the plot.
*        The key will include information about the vertical and
*        horizontal axes of the line plots, including the maximum and
*        minimum value covered by the axis and the quantity
*        represented by the axis. The appearance of this key can be
*        controlled using Parameter KEYSTYLE, and its position can be
*        controlled using Parameter KEYPOS.  [TRUE]
*     KEYPOS() = _REAL (Read)
*        Two values giving the position of the key.  The first value
*        gives the gap between the right-hand edge of the grid plot
*        and the left-hand edge of the key (0.0 for no gap, 1.0 for
*        the largest gap).  The second value gives the vertical
*        position of the top of the key (1.0 for the highest position,
*        0.0 for the lowest).  If the second value is not given, the
*        top of the key is placed level with the top of the grid
*        plot.  Both values should be in the range 0.0 to 1.0.  If a
*        key is produced, then the right-hand margin specified by
*        Parameter MARGIN is ignored.  [current value]
*     KEYSTYLE = GROUP (Read)
*        A group of attribute settings describing the plotting style
*        to use for the key (see Parameter KEY).
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
*        be used for any unspecified attributes.  All attributes will be
*        defaulted if a null value (!)---the initial default---is
*        supplied.  To apply changes of style to only the current
*        invocation, begin these attributes with a plus sign.  A mixture
*        of persistent and temporary style changes is achieved by
*        listing all the persistent attributes followed by a plus sign
*        then the list of temporary attributes.
*
*        See section "Plotting Attributes" in SUN/95 for a description
*        of the available attributes.  Any unrecognised attributes are
*        ignored (no error is reported).
*
*        The appearance of the text in the key can be changed by
*        setting new values for the attributes Colour(Strings),
*        Font(Strings), etc.  [current value]
*     LMODE = LITERAL (Read)
*        LMODE specifies how the defaults for Parameters YBOT and YTOP
*        (the lower and upper limit of the vertical axis of each line
*        plot) should be found.  The supplied string should consist of
*        up to three sub-strings, separated by commas.  The first
*        sub-string must specify the method to use.  If supplied, the
*        other two sub-strings should be numerical values as described
*        below (default values will be used if these sub-strings are
*        not provided).  The following methods are available.
*
*        - "Range" -- The minimum and maximum data values in the
*        supplied cube are used as the defaults for YBOT and YTOP.  No
*        other sub-strings are needed by this option.
*
*        - "Extended" -- The minimum and maximum data values in the
*        cube are extended by percentages of the data range, specified
*        by the second and third sub-strings.  For instance, if the
*        value "Ex,10,5" is supplied, then the default for YBOT is set
*        to the minimum data value minus 10% of the data range, and
*        the default for YTOP is set to the maximum data value plus 5%
*        of the data range.  If only one value is supplied, the second
*        value defaults to the supplied value.  If no values are
*        supplied, both values default to "2.5".
*
*          - "Percentile" -- The default values for YBOT and YTOP are
*        set to the specified percentiles of the data in the supplied
*        cube. For instance, if the value "Per,10,99" is supplied,
*        then the default for YBOT is set so that the lowest 10% of
*        the plotted points are off the bottom of the plot, and the
*        default for YTOP is set so that the highest 1% of the points
*        are off the top of the plot.  If only one value, p1, is
*        supplied, the second value, p2, defaults to (100 - p1).  If
*        no values are supplied, the values default to "5,95".
*
*        - "Sigma" -- The default values for YBOT and YTOP are set to
*        the specified numbers of standard deviations below and above
*        the mean of the data.  For instance, if the value
*        "sig,1.5,3.0" is supplied, then the default for YBOT is set
*        to the mean of the data minus 1.5 standard deviations, and
*        the default for YTOP is set to the mean plus 3 standard
*        deviations.  If only one value is supplied, the second value
*        defaults to the supplied value.  If no values are provided
*        both default to "3.0".
*
*        The method name can be abbreviated to a single character, and
*        is case insensitive.  The initial value is "Range".
*        [current value]
*     MARGIN( 4 ) = _REAL (Read)
*        The widths of the margins to leave around the outer spatial
*        axes for axis annotations, given as fractions of the
*        corresponding dimension of the current picture.  The actual
*        margins used may be increased to preserve the aspect ratio of
*        the data.  Four values may be given, in the order: bottom,
*        right, top, left. If fewer than four values are given, extra
*        values are used equal to the first supplied value.  If these
*        margins are too narrow any axis annotation may be clipped.
*        If a null (!) value is supplied, the value used is (for all
*        edges); 0.15 if annotated axes are being produced; and 0.0
*        otherwise.  The initial default is null.  [current value]
*     MARKER = _INTEGER (Read)
*        This parameter is only accessed if Parameter MODE is set to
*        "Chain" or "Mark".  It specifies the symbol with which each
*        position should be marked, and should be given as an integer
*        PGPLOT marker type.  For instance, 0 gives a box, 1 gives a
*        dot, 2 gives a cross, 3 gives an asterisk, 7 gives a triangle.
*        The value must be larger than or equal to -31.  [current value]
*     MODE = LITERAL (Read)
*        Specifies the way in which data values are represented.  MODE
*        can take the following values.
*
*        - "Histogram" -- An histogram of the points is plotted in the
*        style of a "staircase" (with vertical lines only joining the
*        y-axis values and not extending to the base of the plot).  The
*        vertical lines are placed midway between adjacent x-axis
*        positions.  Bad values are flanked by vertical lines to the
*        lower edge of the plot.
*
*        - "GapHistogram" -- The same as the "Histogram" option except
*        bad values are not flanked by vertical lines to the lower edge
*        of the plot, leaving a gap.
*
*        - "Line" -- The points are joined by straight lines.
*
*        - "Point" -- A dot is plotted at each point.
*
*        - "Mark" -- Each point is marker with a symbol specified by
*        Parameter MARKER.
*
*        - "Chain" -- A combination of "Line" and "Mark".
*
*        The initial default is "Line".
*        [current value]
*     NDF = NDF (Read)
*        The input NDF structure containing the data to be displayed.
*        It should have three significant axes, i.e. whose dimensions
*        are greater than 1.
*     NX = _INTEGER (Read)
*        The number of spectra to draw in each row. The spectra will
*        be equally spaced over the bounds of the x pixel axis. The
*        dynamic default is the number of pixels along the x axis of
*        the NDF, so long as this value is no more than 30. If the x
*        axis spans more than 30 pixels, then the dynamic default is
*        30 (meaning that some spatial pixels will be ignored).  []
*     NY = _INTEGER (Read)
*        The number of spectra to draw in each column. The spectra
*        will be equally spaced over the bounds of the y pixel axis.
*        The dynamic default is the number of pixels along the y axis
*        of the NDF, so long as this value is no more than 30. If the
*        y axis spans more than 30 pixels, then the dynamic default is
*        30 (meaning that some spatial pixels will be ignored).  []
*     REFLABEL = _LOGICAL (Read)
*        If TRUE then the first line plot (i.e. the lower left
*        spectrum) will be annotated with numerical and textual labels
*        describing the two axes. Note, due to the small size of the
*        line plot, such text may be too small to read on some
*        graphics devices.  [current value]
*     SPECAXES = _LOGICAL (Read)
*        TRUE if axes are to be drawn around each spectrum. The
*        appearance of the axes can be controlled using the SPECSTYLE
*        parameter. [TRUE]
*     SPECSTYLE = LITERAL (Read)
*        A group of attribute settings describing the plotting style
*        to use when drawing the axes and data values in the spectrum
*        line plots.
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
*        be used for any unspecified attributes.  All attributes will be
*        defaulted if a null value (!)---the initial default---is
*        supplied.  To apply changes of style to only the current
*        invocation, begin these attributes with a plus sign.  A mixture
*        of persistent and temporary style changes is achieved by
*        listing all the persistent attributes followed by a plus sign
*        then the list of temporary attributes.
*
*        See section "Plotting Attributes" in SUN/95 for a description
*        of the available attributes.  Any unrecognised attributes are
*        ignored (no error is reported).
*
*        By default the axes have interior tick marks, and are without
*        labels and a title to avoid overprinting on adjacent plots.
*
*        The appearance of the data values is controlled by the
*        attributes Colour(Curves), Width(Curves), etc. (the synonym
*        Lines may be used in place of Curves).  [current value]
*     STYLE = GROUP (Read)
*        A group of attribute settings describing the plotting style to
*        use for the annotated outer spatial axes (see Parameter AXES).
*
*        A comma-separated list of strings should be given in which each
*        string is either an attribute setting, or the name of a text
*        file preceded by an up-arrow character "^".  Such text files
*        should contain further comma-separated lists which will be
*        read and interpreted in the same manner.  Attribute settings
*        are applied in the order in which they occur within the list,
*        with later settings overriding any earlier settings given for
*        the same attribute.
*
*        Each individual attribute setting should be of the form:
*
*           <name>=<value>
*
*        where <name> is the name of a plotting attribute, and <value>
*        is the value to assign to the attribute.  Default values will
*        be used for any unspecified attributes.  All attributes will be
*        defaulted if a null value (!)---the initial default---is
*        supplied.  To apply changes of style to only the current
*        invocation, begin these attributes with a plus sign.  A mixture
*        of persistent and temporary style changes is achieved by
*        listing all the persistent attributes followed by a plus sign
*        then the list of temporary attributes.
*
*        See section "Plotting Attributes" in SUN/95 for a description
*        of the available attributes.  Any unrecognised attributes are
*        ignored (no error is reported).  [current value]
*     USEAXIS = LITERAL (Read)
*        The WCS axis that will appear along the horizontal axis of
*        each line plot (the other two axes will be used as the spatial
*        axes). The axis can be specified using one of the following
*        options.
*
*         -  Its integer index within the current Frame of the NDF (in
*         the range 1 to 3 in the current frame).
*
*         -  Its symbol string such as "RA", or "VRAD".
*
*         - A generic option where "SPEC" requests the spectral axis,
*         "TIME" selects the time axis, "SKYLON" and "SKYLAT" picks the
*         sky longitude and latitude axes respectively.  Only those axis
*         domains present are available as options.
*
*        A list of acceptable values is displayed if an illegal value is
*        supplied.  The dynamic default is the index of any spectral
*        axis found in the current Frame of the NDF.  []
*     YBOT = _REAL (Read)
*        The data value for the bottom edge of each line plot. The
*        dynamic default is chosen in a manner determined by Parameter
*        LMODE.  []
*     YTOP = _REAL (Read)
*        The data value for the top edge of each line plot. The dynamic
*        default is chosen in a manner determined by Parameter LMODE. []

*  Examples:
*     clinplot cube useaxis=3
*        Plots a set of line plots of data values versus position
*        along the third axis for the three-dimensional NDF called
*        cube on the current graphics device.  Axes are drawn around
*        the grid of plots indicating the spatial positions in the
*        current co-ordinate Frame.  The third axis may not be
*        spectral and the other two axes need not be spatial.
*     clinplot cube margin=0.1
*        As above, but if a search locates a spectral axis in the
*        world co-ordinate system, this is plotted along the
*        horizontal of the line plots, and the other axes are deemed
*        to be spatial. Also the margin for the spatial axes is
*        reduced to 0.1 to allow more room for the grid of line plots.
*     clinplot map(~5,~5,) useaxis=3 noaxes
*        Plots data values versus position for the central 5-by-5
*        pixel region of the three-dimensional NDF called map on the
*        current graphics device.  No spatial axes are drawn.
*     clinplot map(~5,~5,) useaxis=3 noaxes device=ps_l mode=hist
*        As the previous example but now the output goes to a text
*        file (pgplot.ps) which can be printed on a PostScript
*        printer and the data are plotted in histogram form.
*     clinplot nearc v style="'title=Ne Arc variance'" useaxis=1
*               reflabel=f
*        Plots variance values versus position along Axis 1, for each
*        spatial position in dimensions two and three, for the three
*        dimensional NDF called nearc on the current graphics device.
*        The plot has a title of "Ne Arc variance".  No labels are
*        drawn around the lower-left line plot.
*     clinplot ndf=speccube noclear specstyle="colour(curves)=blue"
*        Plots data values versus pixel co-ordinate at each spatial
*        position for the three-dimensional NDF called speccube on the
*        current graphics device.  The plot is drawn over any existing
*        plot and inherits the spatial bounds of the previous plot.
*        The data are drawn in blue, probably to distinguish it from
*        the previous plot drawn in a different colour.

*   Notes:
*      -  If no Title is specified via the STYLE parameter, then the
*      Title component in the NDF is used as the default title for the
*      annotated axes.  If the NDF does not have a Title component, then
*      the default title is taken from current co-ordinate Frame in the
*      NDF.  If this has not been set explicitly, then the name of the
*      NDF is used as the default title.
*      -  If all the data values at a spatial position are bad, no line
*      plot is drawn at that location.
*
*  Related Applications:
*     KAPPA: DISPLAY, LINPLOT, MLINPLOT; Figaro: SPECGRID; SPLAT.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     VARIANCE, LABEL, TITLE, WCS and UNITS components of the input NDF.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics & Astronomy Research
*     Council.  (C) 2008-2010 Science & Technology Facilities Council.
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
*     MJC: Malcolm J. Currie (RAL)
*     DSB: David S. Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     7-JUN-2006 (DSB):
*        Original version, based upon previous CLINPLOT by MJC.
*     22-JUN-2006 (DSB):
*        Key position changed to take account of horizontal expansion of
*        the spatial axes.
*     5-DEC-2006 (DSB):
*        Allow input NDFs to have degenerate pixel axes.
*     2008 November 12 (MJC):
*        Added MODE and MARKER parameters, and mention SPECSTYLE in the
*        Description.  This required the viewport and window limits to
*        be set for each cell.
*     17-JUL-2009 (DSB):
*        Added ALIGN and SPECAXES parameters.
*     2010 August 10 (MJC):
*        Added Gapped MODE.
*     2010 August 13 (MJC):
*        The new mode renamed to GapHistogram.
*     2010 October 13 (MJC):
*        Permit temporary style attributes.
*     2025 July 1 (GSB):
*        Add DOWNSAMPLE parameter and correct logic regarding histogram
*        modes.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PRM_PAR'          ! VAL constants
      INCLUDE 'CNF_PAR'          ! CNF constants

*  Status:
      INTEGER STATUS

*  External References:
      EXTERNAL KPG1_ASPLN

*  Local Constants:
      INTEGER NDIM               ! Dimensionality of input array
      PARAMETER( NDIM = 3 )

      INTEGER MXSPEC             ! Max. no. of spectra per row or column
      PARAMETER( MXSPEC = 100 )

      REAL KW                    ! Width of KEY picture as a fraction of
      PARAMETER( KW = 0.18 )     ! current picture width

*  Local Variables:
      CHARACTER ATTR*20          ! AST attribute name
      CHARACTER COMP*8           ! Component to be displayed
      CHARACTER LABEL*40         ! NDF Label component
      CHARACTER MAPKEY*20        ! Key for next polyline description
      CHARACTER MCOMP*8          ! Component to be mapped
      CHARACTER NDFNAM*255       ! Full NDF specification
      CHARACTER TEXT*255         ! A general text string
      CHARACTER UNIT*20          ! NDF Unit component
      DOUBLE PRECISION ATTRS( 5 )! Original plotting attribute values
      DOUBLE PRECISION BBOX( 4 ) ! Bounds in base Frame of new Plot
      DOUBLE PRECISION BOX( 4 )  ! Bounds of image in pixel co-ordinates
      DOUBLE PRECISION CON( 2 )  ! Constants for axis permutations
      DOUBLE PRECISION DCGX      ! X GRID coord at spectrum
      DOUBLE PRECISION DCGY      ! Y GRID coord at spectrum
      DOUBLE PRECISION DGLB( 2 ) ! Lower bounds of GRAPHICS region
      DOUBLE PRECISION DGUB( 2 ) ! Upper bounds of GRAPHICS region
      DOUBLE PRECISION DX        ! Width of each spectrum cell in mm
      DOUBLE PRECISION DY        ! Height of each spectrum cell in mm
      DOUBLE PRECISION GRX       ! X GRAPHICS coord at spectrum
      DOUBLE PRECISION GRY       ! Y GRAPHICS coord at spectrum
      DOUBLE PRECISION IN( 2 )   ! GRID coords
      DOUBLE PRECISION INA( 2 )  ! Corner A of window in input coords
      DOUBLE PRECISION INB( 2 )  ! Corner B of window in input coords
      DOUBLE PRECISION KEYX      ! Horizontal key offset (in mm)
      DOUBLE PRECISION OUTA( 2 ) ! Corner A of window in output coords
      DOUBLE PRECISION OUTB( 2 ) ! Corner B of window in output coords
      DOUBLE PRECISION SHIFTS( 2 ) ! Shifts from G2D Frame to P2D Frame
      DOUBLE PRECISION SPBND( 2 )! Bounds of spctral WCS value
      INTEGER BFRM               ! Pointer to base Frame in Plot
      INTEGER CBMAP              ! Pointer to current->base Mapping
      INTEGER CFRM               ! Pointer to current Frame in Plot
      INTEGER CGTOGR             ! NDF GRID to GRAPHICS Mapping
      INTEGER CGX                ! X GRID index at spectrum
      INTEGER CGY                ! Y GRID index at spectrum
      INTEGER CMAP( MXSPEC*MXSPEC )! GRAPHICS->GRID Mapping for a cell
      INTEGER CPM                ! A CmpMap
      INTEGER CREG( MXSPEC*MXSPEC )! GRAPHICS Interval for a cell
      INTEGER DATF               ! The data value Frame
      INTEGER DIM( NDIM )        ! The pixel NDF axis dimensions
      INTEGER DPF                ! DATAPLOT Frame
      INTEGER DPMAP              ! 1st cell GRAPHICS->DATAPLOT Mapping
      INTEGER EL                 ! No. of mapped elements
      INTEGER FS                 ! FrameSet describing cell coords
      INTEGER G2D                ! Pointer to 2D celestial GRID Frame
      INTEGER GDMAP              ! Mapping from GRAPHICS to DPF
      INTEGER GF                 ! Data value / grid Frame
      INTEGER GRFRM              ! GRAPHICS Frame
      INTEGER I                  ! Loop count
      INTEGER IAT                ! Used length of a string
      INTEGER INDF               ! NDF id. for input NDF
      INTEGER INP( 2 )           ! Input axis permutation array
      INTEGER IPD                ! Pointer to NDF data array
      INTEGER IPICD              ! AGI id. for DATA picture
      INTEGER IPICF              ! AGI id. for new FRAME picture
      INTEGER IPICK              ! AGI id. for the KEY picture
      INTEGER IPLOT              ! Pointer to AST Plot for DATA picture
      INTEGER IPLOT2             ! Plot for current cell
      INTEGER IPLOT3             ! Plot for plotting line curves
      INTEGER IPLOT4             ! Plot for spatial axes
      INTEGER IPLOTD             ! Plot stored with DATA picture
      INTEGER IPLOTK             ! Pointer to AST Plot for KEY picture
      INTEGER IPN                ! P'nter to no. of p'nters per polyline
      INTEGER IPW1               ! Pointer to work array 1
      INTEGER IPW2               ! Pointer to work array 2
      INTEGER IPW3               ! Pointer to work array 3
      INTEGER IPX                ! Pointer to array of graphics X values
      INTEGER IPY                ! Pointer to array of graphics Y values
      INTEGER IWCS               ! Pointer to the WCS FrameSet from NDF
      INTEGER IX                 ! Index of cell in row
      INTEGER IY                 ! Index of cell in column
      INTEGER KM                 ! Subsiduary KeyMap for polyline info
      INTEGER MODE               ! Mode identifier
      INTEGER MTYPE              ! PGPLOT marker type
      INTEGER NCELL              ! Number of cells in plot
      INTEGER NCU                ! Number of characters in the units
      INTEGER NFRM               ! Increment in Frame index
      INTEGER NK                 ! Number of celestial axes
      INTEGER NKP                ! No. of values supplied for KEYPOS
      INTEGER NMARG              ! No. of margin values given
      INTEGER NP                 ! No. of points in polyline
      INTEGER NPOLY              ! No. of poly lines described in KeyMap
      INTEGER NPTOT              ! Total no. of points in all tick marks
      INTEGER NS                 ! Number of spectral axes
      INTEGER NSAMP              ! No. of samples along a spectrum
      INTEGER NX                 ! No. of spectra per row
      INTEGER NY                 ! No. of spectra per column
      INTEGER OUTP( 4 )          ! Output axis permutation array
      INTEGER P2D                ! 2D PIXEL Frame
      INTEGER PM                 ! A PermMap
      INTEGER PMAP               ! Mapping from 2D GRID to PIXEL Frame
      INTEGER SDIM( NDIM )       ! The significant NDF axes
      INTEGER SKAX( NDF__MXDIM ) ! Indices of celestial WCS axes
      INTEGER SKBAX( NDF__MXDIM )! Indices of celestial GRID axes
      INTEGER SKMAP              ! Celestial WCS->grid Mapping
      INTEGER SKWCS              ! Pointer to WCS FrameSet for sky axes
      INTEGER SKYF               ! Pointer to 2D celestial WCS Frame
      INTEGER SLBND( NDIM )      ! Significant lower bounds of the image
      INTEGER SLM                ! A SelectorMap
      INTEGER SPAX               ! Index of spectral WCS axis
      INTEGER SPBAX( NDF__MXDIM )! Indices of spectral GRID axes
      INTEGER SPFRM              ! Pointer to spectral WCS Frame
      INTEGER SPMAP              ! Spectral WCS->grid Mapping
      INTEGER SUBND( NDIM )      ! Significant upper bounds of the image
      INTEGER SWM                ! A SwitchMap
      INTEGER TICKMAP            ! KeyMap holding details of tick marks
      INTEGER TMAP               ! Temporary Mapping
      INTEGER WCF                ! Final Frame
      INTEGER WCM                ! Final Mapping
      INTEGER WM                 ! A WinMap
      LOGICAL ALIGN              ! DATA pic aligned with a previous pic?
      LOGICAL AXES               ! Draw annotated spatial axes?
      LOGICAL BLEDGE             ! Leaves edge spec plots bare?
      LOGICAL CGOOD( MXSPEC, MXSPEC )! Was a spectrum drawn in the cell?
      LOGICAL CLEAR              ! Is screen to be cleared on opening?
      LOGICAL DOWNSAMP           ! Downsample spectrum for plotting?
      LOGICAL FIRST              ! Is first cell yet to be annotated?
      LOGICAL KEY                ! Make a key of the grid co-ordinates?
      LOGICAL REFLAB             ! Draw labels around first spectrum?
      LOGICAL SPAXES             ! Draw annotated spectral axes?
      REAL ASPECT                ! Aspect ratio of the input array
      REAL AX                    ! GRAPHICS->NDC slope (X)
      REAL AY                    ! GRAPHICS->NDC slope (Y)
      REAL BX                    ! GRAPHICS->NDC offset (X)
      REAL BY                    ! GRAPHICS->NDC offset (Y)
      REAL DUMMY                 ! Un-required argument value
      REAL DX1                   ! Unused
      REAL DX2                   ! Width of viewport in device pixels
      REAL DY1                   ! Unused
      REAL DY2                   ! Height of viewport in device pixels
      REAL GBOX( 4 )             ! Bounds in GRAPHICS Frame of new Plot
      REAL GBOXFX                ! Lower X GRAPHICS bound of first cell
      REAL GBOXFY                ! Lower Y GRAPHICS bound of first cell
      REAL GLB( 2 )              ! Lower bounds of GRAPHICS region
      REAL GUB( 2 )              ! Upper bounds of GRAPHICS region
      REAL KEYOFF                ! Offset to top of key
      REAL KEYPOS( 2 )           ! Key position
      REAL MARGIN( 4 )           ! Width of margins round DATA picture
      REAL MINDIM                ! Minimum dimension of plot in mm
      REAL OFFX                  ! X offset from 1st to current cell
      REAL OFFY                  ! Y offset from 1st to current cell
      REAL RHOPIC                ! Plot density for scaling ref. axes
      REAL SMARGX                ! X margin used by spectral annotation
      REAL SMARGY                ! Y margin used by spectral annotation
      REAL TL                    ! MajTickLen value
      REAL VBOX( 4 )             ! Bounds in NDC of new Plot
      REAL VX1                   ! NDC X at left hand of current cell
      REAL VX2                   ! NDC X at right hand of current cell
      REAL VY1                   ! NDC Y at bottom of current cell
      REAL VY2                   ! NDC Y at top of current cell
      REAL X1                    ! GRAPHICS X at left hand of Plot
      REAL X2                    ! GRAPHICS X at right hand of Plot
      REAL Y1                    ! GRAPHICS Y at bottom of Plot
      REAL Y2                    ! GRAPHICS Y at top of Plot
      REAL YBOT                  ! Min. data value to be displayed
      REAL YTOP                  ! Max. data value to be displayed

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise things.
      IPX = 0
      IPY = 0
      IPN = 0

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain the NDF and extract the required information from it.
*  ============================================================

*  Obtain the identifier of the NDF to be plotted.
      CALL LPG_ASSOC( 'NDF', 'READ', INDF, STATUS )

*  Find which component to display.  MCOMP is for use with NDF_MAP and
*  may be set to 'Error'.  COMP is for use with all other NDF routines
*  (which do not accept 'Error' as an NDF component name), and has
*  'Variance' in place of 'Error'.
      CALL KPG1_ARCOG( 'COMP', INDF, MCOMP, COMP, STATUS )

*  Map the required component.
      CALL NDF_MAP( INDF, MCOMP, '_REAL', 'READ', IPD, EL,
     :              STATUS )

*  Get the Label and Unit from the NDF.
      UNIT = ' '
      CALL KPG1_DAUNI( INDF, MCOMP, UNIT, NCU, STATUS )
       LABEL = ' '
      CALL NDF_CGET( INDF, 'Label', LABEL, STATUS )

*  Get an AST pointer to a FrameSet describing the co-ordinate Frames
*  present in the NDF's WCS component.  Modify it to ensure that the
*  Base, PIXEL and Current frames all have three dimensions.  The NDF
*  must have exactly three significant dimensions (i.e. axes spanning
*  more than one pixel).
      CALL KPG1_ASGET( INDF, NDIM, .FALSE., .TRUE., .TRUE., SDIM, SLBND,
     :                 SUBND, IWCS, STATUS )

*  Get pointers for the base Frame, the currrent Frame, and the Mapping
*  from current to base.
      CFRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )
      BFRM = AST_GETFRAME( IWCS, AST__BASE, STATUS )
      CBMAP = AST_GETMAPPING( IWCS, AST__CURRENT, AST__BASE, STATUS )

*  Identify the axis that is to drawn as a spectrum, and get individual
*  Mappings and Frames for the spectral and spatial axes.
*  ====================================================================

*  Locate the spectral axis in the WCS current Frame, if any.
      CALL ATL_FSPEC( CFRM, SPAX, SPFRM, STATUS )

*  Use WCS axis 3 if no spectral axis was found.
      IF( SPAX .EQ. 0 ) SPAX = 3

*  See which axis the user wants to use as the spectral axis, using the
*  above as the default.
      CALL KPG1_GTAXI( 'USEAXIS', CFRM, 1, SPAX, STATUS )

*  Get the corresponding single-axis Frame.
      SPFRM = AST_PICKAXES( CFRM, 1, SPAX, TMAP, STATUS )

*  Locate the corresponding pixel axis, if possible. If not possible,
*  report an error.
      CALL AST_MAPSPLIT( CBMAP, 1, SPAX, SPBAX, SPMAP, STATUS )

      IF( SPMAP .EQ. AST__NULL ) THEN
         NS = 0
      ELSE
         NS = AST_GETI( SPMAP, 'Nout', STATUS )
      END IF

      IF( STATUS .EQ. SAI__OK .AND. NS .NE. 1 ) THEN
         ATTR = 'Label('
         IAT = 6
         CALL CHR_PUTI( SPAX, ATTR, IAT )
         CALL CHR_APPND( ')', ATTR, IAT )

         STATUS = SAI__ERROR
         CALL MSG_SETC( 'AX', AST_GETC( CFRM, ATTR( : IAT ),
     :                                  STATUS ) )
         CALL ERR_REP( 'CLINPLOT_ERR1', 'The ^AX axis is not '//
     :                 'parallel to a pixel axis.', STATUS )

      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Find the left and right value on the spectral WCS axis.
      IN( 1 ) = 1.0D0
      IN( 2 ) = DBLE( SUBND( SPBAX( 1 ) ) - SLBND( SPBAX( 1 ) ) + 1 )
      CALL AST_TRAN1( SPMAP, 2, IN, .FALSE., SPBND, STATUS )

*  Identify the pixel axes associated with the two non-spectral axes.
      IF( SPAX .EQ. 1 ) THEN
         SKAX( 1 ) = 2
         SKAX( 2 ) = 3
      ELSE IF( SPAX .EQ. 2 ) THEN
         SKAX( 1 ) = 3
         SKAX( 2 ) = 1
      ELSE
         SKAX( 1 ) = 1
         SKAX( 2 ) = 2
      END IF

      CALL AST_MAPSPLIT( CBMAP, 2, SKAX, SKBAX, SKMAP, STATUS )

      IF( SKMAP .EQ. AST__NULL ) THEN
         NK = 0
      ELSE
         NK = AST_GETI( SKMAP, 'Nout', STATUS )
      END IF

      IF( STATUS .EQ. SAI__OK .AND. NK .NE. 2 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CLINPLOT_ERR2', 'Cannot associate the '//
     :                 'non-spectral axes with a pair of pixel axes.',
     :                 STATUS )
      END IF

*  Create a FrameSet describing the spatial axes. This has a 2D GRID
*  Frame (G2D) as the base Frame, and the 2 non-spectral axes as the
*  current Frame (SKYF), and also includes a 2D PIXEL Frame (P2D).
      G2D = AST_PICKAXES( BFRM, 2, SKBAX, TMAP, STATUS )
      SKWCS = AST_FRAMESET( G2D, ' ', STATUS )

      P2D = AST_FRAME( 2, 'Domain=PIXEL', STATUS )
      SHIFTS( 1 ) = DBLE( SLBND( SKBAX( 1 ) ) ) - 1.5D0
      SHIFTS( 2 ) = DBLE( SLBND( SKBAX( 2 ) ) ) - 1.5D0
      PMAP = AST_SHIFTMAP( 2, SHIFTS, ' ', STATUS )
      CALL AST_ADDFRAME( SKWCS, AST__BASE, PMAP, P2D, STATUS )

      SKYF = AST_PICKAXES( CFRM, 2, SKAX, TMAP, STATUS )
      CALL AST_INVERT( SKMAP, STATUS )
      CALL AST_ADDFRAME( SKWCS, AST__BASE, SKMAP, SKYF, STATUS )

*  Get the dimensions of the two spatial pixel axes.
      DIM( 1 ) = SUBND( SKBAX( 1 ) ) - SLBND( SKBAX( 1 ) ) + 1
      DIM( 2 ) = SUBND( SKBAX( 2 ) ) - SLBND( SKBAX( 2 ) ) + 1
      DIM( 3 ) = SUBND( SPBAX( 1 ) ) - SLBND( SPBAX( 1 ) ) + 1

*  Set up the graphics system
*  ==========================

*  See if the screen will be cleared.
      CALL PAR_GET0L( 'CLEAR', CLEAR, STATUS )

*  See if annotated spatial axes are need.
      CALL PAR_DEF0L( 'AXES', CLEAR, STATUS )
      CALL PAR_GET0L( 'AXES', AXES, STATUS )

*  See if annotated spectral axes are need.
      CALL PAR_GET0L( 'SPECAXES', SPAXES, STATUS )

*  See if a key to vector length is required.
      CALL PAR_GET0L( 'KEY', KEY, STATUS )

*  See the edges of the line plot that touch the edges of the main plot
*  are to be left bare.
      CALL PAR_DEF0L( 'BLANKEDGE', .NOT. CLEAR .AND. .NOT. AXES,
     :                STATUS )
      CALL PAR_GET0L( 'BLANKEDGE', BLEDGE, STATUS )

*  See if numerical anmd text labels are to drawn around the first line
*  plot.
      CALL PAR_GET0L( 'REFLABEL', REFLAB, STATUS )

*  Set the dynamic default for MARGIN.
      IF ( AXES ) THEN
         MARGIN( 1 ) = 0.15
      ELSE
         MARGIN( 1 ) = 0.0
      END IF

      CALL PAR_DEF1R( 'MARGIN', 1, MARGIN( 1 ), STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get new values.
      CALL PAR_GDRVR( 'MARGIN', 4, -0.49, 10.0, MARGIN, NMARG, STATUS )

*  Use the default if a null value was supplied.
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         NMARG = 1
      END IF

*  Ignore any suplus values.
      NMARG = MIN( 4, NMARG )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Use the first value for any unspecified edges.
      DO I = NMARG + 1, 4
         MARGIN( I ) = MARGIN( 1 )
      END DO

*  Get the plotting mode.
      CALL PAR_CHOIC( 'MODE', 'Line',
     :                'Histogram,GapHistogram,Line,Point,Mark,Step,'/
     :                /'Chain', .FALSE., TEXT, STATUS )

*  Get an identifier for the mode, and get the marker type if required.
      IF( TEXT .EQ. 'HISTOGRAM' ) THEN
         MODE = 1
      ELSE IF( TEXT .EQ. 'GAPHISTOGRAM' ) THEN
         MODE = 6
      ELSE IF( TEXT .EQ. 'LINE' ) THEN
         MODE = 2
      ELSE IF( TEXT .EQ. 'POINT' ) THEN
         MODE = 3
         MTYPE = -1
      ELSE IF( TEXT .EQ. 'MARK' ) THEN
         MODE = 3
         CALL PAR_GET0I( 'MARKER', MTYPE, STATUS )
      ELSE
         MODE = 5
         CALL PAR_GET0I( 'MARKER', MTYPE, STATUS )
      END IF

*  Ensure marker type (if used) is legal.
      MTYPE = MAX( -31, MTYPE )

*  Store the pixel co-ordinates bounds for the new DATA picture.  These
*  are only used if the new DATA picture is not based on an existing
*  DATA picture.  Note, the corresponding PGPLOT window created by
*  KPG1_PLOT will have world co-ordinates of millimetres from the
*  bottom-left corner of the view surface, NOT pixels.  This box is only
*  used to define the bounds of the picture within the AGI database for
*  the benefit of non-AST applications.
      BOX( 1 ) = DBLE( SLBND( SKBAX( 1 ) ) ) - 1.0D0
      BOX( 2 ) = DBLE( SLBND( SKBAX( 2 ) ) ) - 1.0D0
      BOX( 3 ) = DBLE( SUBND( SKBAX( 1 ) ) )
      BOX( 4 ) = DBLE( SUBND( SKBAX( 2 ) ) )

*  Store the aspect ratio of the data array, assuming square pixels.
      ASPECT = ( BOX( 4 ) - BOX( 2 ) )/( BOX( 3 ) - BOX( 1 ) )

*  Generate a reference for the NDF to be stored in the graphics
*  database.
      CALL NDF_MSG( 'NDF', INDF )
      CALL MSG_LOAD( ' ', '^NDF', NDFNAM, IAT, STATUS )

*  Start up the graphics system.  This stores a new DATA picture in the
*  AGI database with the given pixel co-ordinate bounds (a KEY picture
*  is also created if necessary, together with an enclosing FRAME
*  picture ). The PGPLOT viewport is set so that it matches
*  the area of the DATA picture.  World co-ordinates within the PGPLOT
*  window are set to millimetres from the bottom-left corner of the
*  view surface.  An AST Plot is returned for drawing in the DATA
*  picture.  The Base (GRAPHICS) Frame in the Plot corresponds to
*  millimetres from the bottom-left corner of the viewport, and the
*  Current Frame is inherited from the NDF's WCS FrameSet.  A Plot is
*  stored in the AGI database that has SKY coords as the current Frame.
*  This will be extended later by adding a SKY-SPECTRUM Frame in to it
*  (which will be left as the current Frame).

*  First deal with cases where a key is required...
      IF( KEY ) THEN

*  Get the position required for the key.  The margin between DATA and
*  KEY Frames is determined by the horizontal position requested for the
*  key.
         CALL PAR_GDRVR( 'KEYPOS', 2, -1.0, 1.0, KEYPOS, NKP, STATUS )
         IF( KEYPOS( 1 ) .GE. 0.0 ) THEN
            MARGIN( 2 ) = KEYPOS( 1 )
         ELSE
            MARGIN( 2 ) = KEYPOS( 1 ) - KW
         END IF

*  Start up the graphics system, creating a KEY picture.
         CALL KPG1_PLOT( SKWCS, 'UNKNOWN', 'KAPPA_CLINPLOT',
     :                   NDFNAM( : IAT ), MARGIN, 1, 'KEY ', 'R', KW,
     :                   ASPECT, 'PIXEL', BOX, IPICD, IPICF, IPICK,
     :                   IPLOT, NFRM, ALIGN, STATUS )

*  Otherwise, start up the graphics system, creating no KEY picture.
      ELSE
         CALL KPG1_PLOT( SKWCS, 'UNKNOWN', 'KAPPA_CLINPLOT',
     :                   NDFNAM( : IAT ), MARGIN, 0, ' ', ' ', 0.0,
     :                   ASPECT, 'PIXEL', BOX, IPICD, IPICF, IPICK,
     :                   IPLOT, NFRM, ALIGN, STATUS )

      END IF

*  See if the cells of the plot should be aligned spatially with an
*  existing data plot. Otherwise, they form a regular grid covering the
*  whole picture.
      CALL PAR_DEF0L( 'ALIGN', ALIGN, STATUS )
      CALL PAR_GET0L( 'ALIGN', ALIGN, STATUS )

*  Get the Mapping from spatial GRID coords in the NDF to GRAPHICS coords in
*  the Plot.
      CGTOGR = AST_GETMAPPING( IPLOT, 1 + NFRM, AST__BASE, STATUS )

*  Ensure the Title attribute of the Plot has a useful value.
      CALL KPG1_ASTTL( IPLOT, SKWCS, INDF, STATUS )

*  Define the extent of each cell in the grid of line plots.
*  =========================================================

*  See how many spectra are to be included in the grid.
      NX = MIN( 30, DIM( 1 ) )
      NY = MIN( 30, DIM( 2 ) )
      CALL PAR_GDR0I( 'NX', NX, 1, MIN( MXSPEC, DIM( 1 ) ), .FALSE.,
     :                 NX, STATUS )
      CALL PAR_GDR0I( 'NY', NY, 1, MIN( MXSPEC, DIM( 2 ) ), .FALSE.,
     :                 NY, STATUS )

*  Get the bounds of the current PGPLOT window (this is the same as the
*  bounds of the Plot in the GRAPHICS Frame).
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL PGQWIN( X1, X2, Y1, Y2 )

*  Find the width and height of each spectrum's cell in the GRAPHICS
*  Frame.
         DX = DBLE( X2 - X1 )/DBLE( NX )
         DY = DBLE( Y2 - Y1 )/DBLE( NY )

*  Obtain the viewport bounds in NDC.
         CALL PGQVP( 0, VX1, VX2, VY1, VY2 )

*  Find the constants that connect GRAPHICS coords to NDC.
         AX = ( VX1 - VX2 )/( X1 - X2 )
         BX = ( VX1*X2 - VX2*X1 )/( X2 - X1 )

         AY = ( VY1 - VY2 )/( Y1 - Y2 )
         BY = ( VY1*Y2 - VY2*Y1 )/( Y2 - Y1 )

*  Note the minimum dimension of the plotting area.
         MINDIM = X2 - X1
         IF( Y2 - Y1 .LT. MINDIM ) MINDIM = Y2 - Y1

*  Find suitable default values for YTOP and YBOT.
         YBOT = VAL__BADR
         YTOP = VAL__BADR
         CALL KPG1_GRLM3( 'LMODE', EL, %VAL( CNF_PVAL( IPD ) ),
     :                    %VAL( CNF_PVAL( IPD ) ), .FALSE.,
     :                    0.0, YBOT, YTOP, STATUS )

*  Ensure the limits are not equal.
         IF( YBOT .EQ. YTOP ) THEN
            IF( YBOT .NE. 0.0 ) THEN
               YTOP = 2.0*YBOT
            ELSE
               YTOP = 1.0D0
            END IF
         END IF

*  Find the max and min data values to display.
         CALL PAR_DEF0R( 'YTOP', YTOP, STATUS )
         CALL PAR_GET0R( 'YTOP', YTOP, STATUS )
         CALL PAR_DEF0R( 'YBOT', YBOT, STATUS )
         CALL PAR_GET0R( 'YBOT', YBOT, STATUS )

*  Get the number of device pixels across a single line plot
         CALL PGQVSZ( 3, DX1, DX2, DY1, DY2 )

*  Decide on the number of spectral samples to use for each line plot.
*  There is no point in using more than the number of device pixels
*  across a single cell.  Reduce it even further for histogram-style
*  to demand at least four device pixels per sample point.
         NSAMP = DIM( 3 )
         CALL PAR_GET0L( 'DOWNSAMPLE', DOWNSAMP, STATUS )
         IF ( DOWNSAMP ) THEN
            IF ( MODE .EQ. 1 .OR. MODE .EQ. 6 ) THEN
               IF ( NSAMP .GT. NINT( DX2 / NX / 4 ) )
     :           NSAMP = NINT( DX2 / NX / 4 )
            ELSE IF( NSAMP .GT. NINT( DX2/NX ) ) THEN
               NSAMP = NINT( DX2/NX )
            END IF
         END IF

*  Draw all the spectra (but not the axes or borders).
*  ===================================================

*  Create three work arrays to hold a single displayed spectrum.
         CALL PSX_CALLOC( NSAMP, '_DOUBLE', IPW1, STATUS )
         CALL PSX_CALLOC( NSAMP, '_DOUBLE', IPW2, STATUS )
         CALL PSX_CALLOC( NSAMP, '_INTEGER', IPW3, STATUS )

*  Create a temporary copy of the Plot so that we can set its style
*  using the SPECSTYLE parameter without modifying its appearance.
         IPLOT3 = AST_COPY( IPLOT, STATUS )

*  Get a pointer to the GRAPHICS Frame.
         GRFRM = AST_GETFRAME( IPLOT3, AST__BASE, STATUS )

*  Draw the line plots. Loop round each cell finding the indices on the
*  spatial grid axes at which the spectrum is to be extracted, and the
*  GRAPHICS co-ordinates of the lower-left corner of the cell.
         NCELL = 0
         DO IY = 1, NY
            CGY = NINT( 0.5 + ( REAL( IY ) - 0.5 )*
     :                        REAL( DIM( 2 ) )/REAL( NY ) )

            CALL PGBBUF

            DO IX = 1, NX
               CGX = NINT( 0.5 + ( REAL( IX ) - 0.5 )*
     :                           REAL( DIM( 1 ) )/REAL( NX ) )

*  If the spectra are being aligned spatially with an existing data plot...
               IF( ALIGN ) THEN

*  Transform the spatial grid (x,y) for the spectrum into graphics coords.
                  DCGX = DBLE( CGX )
                  DCGY = DBLE( CGY )
                  CALL AST_TRAN2( CGTOGR, 1, DCGX, DCGY, .TRUE., GRX,
     :                            GRY, STATUS )

*  Get the bounds of the spectrum's cell in GRAPHICS coords. The cell is
*  centred on the spectrum position found above.
                  GBOX( 1 ) = REAL( GRX - 0.5D0*DX )
                  GBOX( 2 ) = REAL( GRY - 0.5D0*DY )
                  GBOX( 3 ) = GBOX( 1 ) + REAL( DX )
                  GBOX( 4 ) = GBOX( 2 ) + REAL( DY )

*  If the spectra are being placed on a regular grid covering the data
*  picture, the GRAPHICS bounds of the cell are formed by linear
*  subdivision of the plotting region.
               ELSE
                  GBOX( 1 ) = X1 + REAL( IX - 1 ) * REAL( DX )
                  GBOX( 2 ) = Y1 + REAL( IY - 1 ) * REAL( DY )
                  GBOX( 3 ) = GBOX( 1 ) + REAL( DX )
                  GBOX( 4 ) = GBOX( 2 ) + REAL( DY )
               END IF

*  Get the double precision version of the cell bounds.
               INA( 1 ) = DBLE( GBOX( 1 ) )
               INA( 2 ) = DBLE( GBOX( 2 ) )
               INB( 1 ) = INA( 1 ) + DX
               INB( 2 ) = INA( 2 ) + DY

*  Get the bounds of the cell in NDC.
               VBOX( 1 ) = AX*GBOX( 1 ) + BX
               VBOX( 2 ) = AY*GBOX( 2 ) + BY
               VBOX( 3 ) = AX*GBOX( 3 ) + BX
               VBOX( 4 ) = AY*GBOX( 4 ) + BY

*  KPG1_PLTLN assumes that it is dealing with the full viewport and
*  corresponding GRAPHICS co-ordinates.  So we have to reset the
*  viewport to match the current cell.
               CALL PGSVP( VBOX( 1 ), VBOX( 3 ), VBOX( 2 ), VBOX( 4 ) )
               CALL PGSWIN( GBOX( 1 ), GBOX( 3 ), GBOX( 2 ), GBOX( 4 ) )

*  Copy the required samples from the spectral axis into the work
*  arrays.
               CALL KPS1_CLPCP( SLBND, SUBND, SKBAX, SPBAX( 1 ), CGX,
     :                          CGY, NSAMP, %VAL( CNF_PVAL( IPD ) ),
     :                          INA( 1 ), INA( 2 ),
     :                          DX, DY, YTOP, YBOT, CGOOD( IX, IY ),
     :                          %VAL( CNF_PVAL( IPW1 ) ),
     :                          %VAL( CNF_PVAL( IPW2 ) ),
     :                          %VAL( CNF_PVAL( IPW3 ) ), STATUS )

*  Draw the curve if it contains any good values.  The plus requests
*  support of temporary SPECSTYLE attributes.  Also allow for more than
*  one call to KPG1_ASSET for SPECSTYLE by using the delimiter suffix.
               IF( CGOOD( IX, IY ) ) THEN
                  CALL KPG1_PLTLN( NSAMP, 1, NSAMP ,
     :                            %VAL( CNF_PVAL( IPW1 ) ),
     :                            %VAL( CNF_PVAL( IPW2 ) ),
     :                            .FALSE., .FALSE., 0.0D0, 0.0D0, 0.0D0,
     :                            '+SPECSTYLE+', IPLOT3, MODE, MTYPE, 0,
     :                            0, 'KAPPA_CLINPLOT', STATUS )

*  Increment the number of cells done so far.
                  NCELL = NCELL + 1

*  We now create a Mapping from 2-D GRAPHICS to 4-D
*  (GRID1,GRID2,GRID3,DATA) within the current cell that we will use
*  later when constructing the FrameSet to be stored with the DATA
*  picture in the AGI database. First, produce a WinMap that maps the
*  GRAPHICS co-ordinate box covered by this cell on to the corresponding
*  ranges of GRID co-ordinates (on the SPBAX axis) and data value.
                  OUTA( 1 ) = 0.5D0
                  OUTA( 2 ) = YBOT
                  OUTB( 1 ) = DIM( 3 ) + 0.5D0
                  OUTB( 2 ) = YTOP
                  WM = AST_WINMAP( 2, INA, INB, OUTA, OUTB, ' ',
     :                             STATUS )

*  Now produce a PermMap that copies the spectral GRID axis value and
*  data value, and introduces constant values for the spatial GRID axes.
                  INP( 1 ) = SPBAX( 1 )
                  INP( 2 ) = 4

                  OUTP( SKBAX( 1 ) ) = -1
                  OUTP( SKBAX( 2 ) ) = -2
                  OUTP( SPBAX( 1 ) ) = 1
                  OUTP( 4 ) = 2

                  CON( 1 ) = CGX
                  CON( 2 ) = CGY

                  PM = AST_PERMMAP( 2, INP, 4, OUTP, CON, ' ', STATUS )

*  Combine these two Mappings in series to get a Mapping from 2D
*  Graphics co-ordinates to (GRID1,GRID2,GRID3,DATA) within the current
*  cell.
                  CMAP( NCELL ) = AST_CMPMAP( WM, PM, .TRUE., ' ',
     :                                        STATUS )

*  Produce an Interval that covers the cell.
                  CREG( NCELL ) = AST_INTERVAL( GRFRM, INA, INB,
     :                                          AST__NULL, ' ', STATUS )
               END IF
            END DO

            CALL PGEBUF
          END DO

*  Free the work arrays
         CALL PSX_FREE( IPW1, STATUS )
         CALL PSX_FREE( IPW2, STATUS )
         CALL PSX_FREE( IPW3, STATUS )

*  Reset the viewport and window to the full area.
         CALL PGSVP( VX1, VX2, VY1, VY2 )
         CALL PGSWIN( X1, X2, Y1, Y2 )

*  Now draw all annotated axes, borders, etc.
*  ==========================================

*  Create a 2D CmpFrame describing the data axis and spectral axis. The
*  data axis is described by a simple 1D Frame, and the spectral axis is
*  described by the Frame extracted earlier from the NDF WCS FrameSet.
         DATF = AST_FRAME( 1, 'Title=Data value,Symbol=Data',
     :                     STATUS )
         IF( LABEL .NE. ' ' ) THEN
            CALL AST_SETC( DATF, 'Label', LABEL, STATUS )
         ELSE
            CALL AST_SETC( DATF, 'Label', 'Data value', STATUS )
         END IF

         IF( UNIT .NE. ' ' ) CALL AST_SETC( DATF, 'Unit', UNIT,
     :                                       STATUS )

         DPF = AST_CMPFRAME( SPFRM, DATF,
     :                    'Title=Data value versus spectral position,'//
     :                    'Domain=DATAPLOT', STATUS )

*  Create a 2D Frame representing data value and the position on the
*  spectral GRID axis.
         GF = AST_FRAME( 2, 'Title=Data value versus spectral '//
     :                   'grid position', STATUS )

*  Get a Mapping from GF to DPF. This is a UnitMap on the data (second)
*  axis, and is the SPMAP found earlier on the spectral axis.
         CALL AST_INVERT( SPMAP, STATUS )
         GDMAP = AST_CMPMAP( SPMAP, AST_UNITMAP( 1, ' ', STATUS ),
     :                       .FALSE., ' ', STATUS )

*  Create a FrameSet from these in which GF is the base Frame and DPF is
*  the current Frame.
         FS = AST_FRAMESET( GF, ' ', STATUS )
         CALL AST_ADDFRAME( FS, AST__BASE, GDMAP, DPF, STATUS )

*  Store the bounds of the area within the GF Frame that is to be mapped
*  on to each spectrum's cell.
         BBOX( 1 ) = 0.5
         BBOX( 2 ) = YBOT
         BBOX( 3 ) = DBLE( SUBND( SPBAX( 1 ) ) -
     :                     SLBND( SPBAX( 1 ) ) ) + 1.5D0
         BBOX( 4 ) = YTOP

*  Indicate we have not yet draw a spectrum.
         FIRST = .TRUE.

*  Loop round each spectrum cell. CGX and CGY are the spatial GRID indices
*  of the spectrum in the supplied cube.
         DO IX = 1, NX
            CGX = NINT( 0.5 + ( REAL( IX ) - 0.5 )*
     :                       REAL( DIM( 1 ) )/REAL( NX ) )
            CALL PGBBUF
            DO IY = 1, NY

*  Skip if no spectrum was drawn in this cell.
               IF( CGOOD( IX, IY ) ) THEN
                  CGY = NINT( 0.5 + ( REAL( IY ) - 0.5 )*
     :                        REAL( DIM( 2 ) )/REAL( NY ) )

*  If the spectra are being aligned spatially with an existing data plot...
                  IF( ALIGN ) THEN

*  Transform the spatial grid (x,y) for the spectrum into graphics coords.
                     DCGX = DBLE( CGX )
                     DCGY = DBLE( CGY )
                     CALL AST_TRAN2( CGTOGR, 1, DCGX, DCGY, .TRUE., GRX,
     :                               GRY, STATUS )

*  Get the bounds of the spectrum's cell in GRAPHICS coords. The cell is
*  centred on the spectrum position found above.
                     GBOX( 1 ) = REAL( GRX - 0.5D0*DX )
                     GBOX( 2 ) = REAL( GRY - 0.5D0*DY )
                     GBOX( 3 ) = GBOX( 1 ) + REAL( DX )
                     GBOX( 4 ) = GBOX( 2 ) + REAL( DY )

*  If the spectra are being placed on a regular grid covering the data
*  picture, the GRAPHICS bounds of the cell are formed by linear
*  subdivision of the plotting region.
                  ELSE
                     GBOX( 1 ) = X1 + REAL( IX - 1 ) * REAL( DX )
                     GBOX( 2 ) = Y1 + REAL( IY - 1 ) * REAL( DY )
                     GBOX( 3 ) = GBOX( 1 ) + REAL( DX )
                     GBOX( 4 ) = GBOX( 2 ) + REAL( DY )
                  END IF

*  If this is the first spectrum, we draw a grid around it using
*  AST_GRID.
                  IF( FIRST ) THEN
                     FIRST = .FALSE.

*  Record the bounds of the first cell in GRAPHICS co-ordinates.
                     GBOXFX = GBOX( 1 )
                     GBOXFY = GBOX( 2 )

*  Create a new Plot covering just the current cell.
                     IPLOT2 = AST_PLOT( FS, GBOX, BBOX, ' ', STATUS )

*  Save the Mapping from GRAPHICS to the DATAPLOT Frame. We will later
*  add the DATAPLOT Frame into the Plot stored in the AGI database, so
*  that subsequent calls to LINPLOT can align with it.
                     DPMAP = AST_GETMAPPING( IPLOT2, AST__BASE,
     :                                       AST__CURRENT, STATUS )

*  Set the style for plotting in the line plot.  The plus requests
*  support of temporary SPECSTYLE attributes.  This the final attribute
*  setting for SPECSTYLE hence there is no plus-sign suffix.
                     CALL KPG1_ASSET( 'KAPPA_CLINPLOT', '+SPECSTYLE',
     :                                 IPLOT2, STATUS )

*  Ensure no title or minor tick marks are produced.
                     CALL AST_SET( IPLOT2, 'DrawTitle=0,minticklen=0',
     :                             STATUS )

*  If the edges are to be left blank, or if no annotation is required,
*  supress annotation.
                     IF( BLEDGE .OR. .NOT. REFLAB ) THEN
                        CALL AST_SET( IPLOT2, 'TextLab=0,NumLab=0',
     :                             STATUS )
                     END IF

*  Reduce the text size (this is normally done by KPG1_PLOT, but the
*  Plot we are using here was not created by KPG1_PLOT).  The
*  non-linear scaling gives better results in practise than the linear.
                     RHOPIC = SQRT( REAL( MAX( NX, NY ) ) )
                     CALL AST_SETR( IPLOT2, 'Size(NumLab1)',
     :                              AST_GETR( IPLOT2, 'Size(NumLab1)',
     :                              STATUS )/ RHOPIC, STATUS )
                     CALL AST_SETR( IPLOT2, 'Size(NumLab2)',
     :                              AST_GETR( IPLOT2, 'Size(NumLab2)',
     :                              STATUS )/ RHOPIC, STATUS )
                     CALL AST_SETR( IPLOT2, 'Size(TextLab1)',
     :                              AST_GETR( IPLOT2, 'Size(TextLab1)',
     :                              STATUS )/ RHOPIC, STATUS )
                     CALL AST_SETR( IPLOT2, 'Size(TextLab2)',
     :                              AST_GETR( IPLOT2, 'Size(TextLab2)',
     :                              STATUS )/ RHOPIC, STATUS )

*  Draw lines using KPG1_ASPLN. This records details of the lines
*  drawn in the AST KeyMap (TICKMAP) specified when calling KPG1_ASPLG.
                     TICKMAP = AST_KEYMAP( ' ', STATUS )
                     CALL KPG1_ASPLG( TICKMAP, BLEDGE, X1, X2, Y1, Y2 )
                     CALL AST_GRFSET( IPLOT2, 'Line', KPG1_ASPLN,
     :                                STATUS )
                     CALL AST_SETL( IPLOT2, 'Grf', .TRUE., STATUS )

*  Ensure DSBSpecFrames don't produce alternative axis ticks on the top
*  edge.
                     CALL KPG1_SETASTDSB( .FALSE. )

*  Draw the grid if required.
                     IF( SPAXES ) CALL KPG1_ASGRD( IPLOT2, IPICF,
     :                                             .TRUE., STATUS )

*  Get the bounding box of the graphics produced by the above call to
*  AST_GRID.
                     CALL AST_BOUNDINGBOX( IPLOT2, GLB, GUB, STATUS )

*  Determine how far outside the cell the axis annotation extended.
                     SMARGX = GBOX( 1 ) - GLB( 1 )
                     SMARGY = GBOX( 2 ) - GLB( 2 )

*  Note the number of entries in the TICKMAP KeyMap.
                     NPOLY = AST_MAPSIZE( TICKMAP, STATUS )

*  Get the total number of points used to draw the tick marks.
                     NPTOT = 0
                     DO I = 1, NPOLY
                        MAPKEY = 'POLYLINE'
                        IAT = 8
                        CALL CHR_PUTI( I, MAPKEY, IAT )
                        IF( AST_MAPGET0A( TICKMAP, MAPKEY, KM,
     :                                    STATUS ) ) THEN
                           IF( AST_MAPGET0I( KM, 'N', NP,
     :                                       STATUS ) ) THEN
                              NPTOT = NPTOT + NP
                           END IF
                           CALL AST_ANNUL( KM, STATUS )
                        END IF
                     END DO

*  Allocate work space for arrays containing this number of points.
                     CALL PSX_CALLOC( NPTOT, '_REAL', IPX, STATUS )
                     CALL PSX_CALLOC( NPTOT, '_REAL', IPY, STATUS )
                     CALL PSX_CALLOC( NPOLY, '_INTEGER', IPN, STATUS )

*  Establish the graphical attributes that AST_GRID uses to draw the
*  tick marks.
                     CALL KPG1_PGSTY( IPLOT2, 'TICKS', .TRUE., ATTRS,
     :                               STATUS )

*  For subsequent cells, draw the tick marks by translating the
*  polylines stored in TICKMAP from the first cell to the current cell.
*  This is faster than drawing the ticks using AST_GRID.
                  ELSE IF( SPAXES ) THEN

*  Find the offset in graphics co-ordinates from the bottom-left corner
*  of the first cell to be annotated to the current cell.
                     OFFX = GBOX( 1 ) - GBOXFX
                     OFFY = GBOX( 2 ) - GBOXFY

*  Draw the tick marks for this cell.
                     CALL KPS1_CLPTM( BLEDGE, OFFX, OFFY, NPOLY,
     :                                TICKMAP, %VAL( CNF_PVAL( IPX ) ),
     :                                %VAL( CNF_PVAL( IPY ) ),
     :                                %VAL( CNF_PVAL( IPN ) ),
     :                                STATUS )
                   END IF
               END IF
            END DO
            CALL PGEBUF
         END DO

*  Reset the original graphical attributes.
         CALL KPG1_PGSTY( IPLOT2, 'TICKS', .FALSE., ATTRS, STATUS )

*  Initialise the horizontal offset to apply to the key.
         KEYX = 0.0

*  Draw the spatial axes if required.
         IF( AXES ) THEN

*  We will need to expand the area enclosed within the spatial axes to
*  allow room for the annotation associated with the spectral plots, and
*  for the interior tick marks.
            TL = AST_GETR( IPLOT, 'MajTickLen', STATUS )

            IF( TL .GT. 0.0 ) THEN
               SMARGX = SMARGX + TL*MINDIM
               SMARGY = SMARGY + TL*MINDIM
            END IF

            DGLB( 1 ) = X1 - SMARGX
            DGLB( 2 ) = Y1 - SMARGY
            DGUB( 1 ) = X2 + SMARGX
            DGUB( 2 ) = Y2 + SMARGY
            CALL ATL_CUTPL( IPLOT, AST__BASE, DGLB, DGUB, IPLOT4,
     :                      STATUS )

*  Draw the axes.
            CALL KPG1_ASGRD( IPLOT4, IPICF, .TRUE., STATUS )

*  If a key is being drawn, move it to the right to take account of the
*  expansion to the spatial axes above.
            IF( KEY ) KEYX = SMARGX

         END IF

*  Now store a suitable Plot with the DATA picture in the AGI database.
*  ====================================================================

*  Create a SelectorMap that identifies which cell any given GRAPHICS
*  position is in.
         SLM = AST_SELECTORMAP( NCELL, CREG, AST__BAD, ' ', STATUS )

*  Create a SwitchMap that transforms 2D Graphics co-ordinates into 4D
*  (GRID1,GRID2,GRID3,DATA) co-ordinates.
         SWM = AST_SWITCHMAP( SLM, AST__NULL, NCELL, CMAP, ' ', STATUS )

*  Create a Mapping that transforms (GRID1,GRID2,GRID3,DATA) into the
*  current Frame of the NDF's WCS FrameSet (with a fourth DATA axis).
         CALL AST_INVERT( CBMAP, STATUS )
         CPM = AST_CMPMAP( CBMAP, AST_UNITMAP( 1, ' ', STATUS ),
     :                     .FALSE., ' ', STATUS )
         WCM = AST_CMPMAP( SWM, CPM, .TRUE., ' ', STATUS )

*  Create a corresponding 4D Frame.
         WCF = AST_CMPFRAME( CFRM, DATF, ' ', STATUS )

*  Get the Plot stored with the DATA picture when KPG1_PLOT was called
*  earlier.
         CALL KPG1_GDGET( IPICD, AST__NULL, .FALSE., IPLOTD, STATUS )

*  Add in the DATAPLOT Frame (that describes the data value/frequency
*  axes for the first valid cell). LINPLOT can align with this Frame.
         CALL AST_ADDFRAME( IPLOTD, AST__BASE, DPMAP, DPF, STATUS )

*  Add in the SKY Frame (which describes the celestial axes of the outer
*  grid). DISPLAY/CONTOUR can align with this. The GRAPHICS to SKY
*  Mapping is obtained from the Plot used to draw the celestial axes.
         CALL AST_ADDFRAME( IPLOTD, AST__BASE,
     :                      AST_GETMAPPING( IPLOT, AST__BASE,
     :                                      AST__CURRENT, STATUS ),
     :                      SKYF, STATUS )

*  Add in the (GRID1,GRID2,GRID3,DATA) Frame using the WCM Mapping to
*  connect it to the GRAPHICS (base) Frame.
         CALL AST_ADDFRAME( IPLOTD, AST__BASE, WCM, WCF, STATUS )

*  Store the modified Plot back in the AGI database
         CALL KPG1_GDPUT( IPICD, 'PIXEL', ' ', IPLOTD, STATUS )

*  Plot the key if necessary.
*  ==========================
         IF ( KEY ) THEN

*  If no value was supplied for the vertical position of the KEY using
*  Parameter KEYPOS, find the value which puts the top of the key level
*  with the top of the DATA picture.
            IF ( NKP .LT. 2 ) THEN

*  Report an error if there is insufficient room within the current
*  picture for the key.
               IF ( IPICK .EQ. -1 .AND. STATUS .EQ. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'CLINPLOT_KEY', 'There is '//
     :                          'insufficient room in the current '//
     :                          'picture for a key.', STATUS )
                  GO TO 999
               END IF

*  We need to know the position of the top of the DATA picture so that
*  the top of the key can be put at the same height on the screen.  Get
*  the bounds of the current PGPLOT viewport, in mm.  Only the vertical
*  position at the top is needed.
               CALL PGQVP( 2, DUMMY, DUMMY, DUMMY, KEYOFF )

*  Activate the KEY picture.  This returns a pointer to an AST Plot
*  which can be used to draw in the KEY picture, and sets the current
*  PGPLOT viewport so that it corresponds to the KEY picture.
               CALL KPG1_GDGET( IPICK, AST__NULL, .FALSE., IPLOTK,
     :                          STATUS )
               IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Find the vertical position in the key picture which corresponds to
*  the top of the DATA picture, as a fraction of the height of the key
*  picture.
               CALL PGQVP( 2, DUMMY, DUMMY, Y1, Y2 )
               KEYOFF = ( KEYOFF - Y1 )/( Y2 - Y1 )

*  If the horizontal positions was given using Parameter KEYPOS, just
*  activate the KEY picture.
            ELSE
               KEYOFF = KEYPOS( 2 )
               CALL KPG1_GDGET( IPICK, AST__NULL, .FALSE., IPLOTK,
     :                          STATUS )
               IF ( STATUS .NE. SAI__OK ) GO TO 999
            END IF

*  Set the style for plotting in the key picture.  The plus requests
*  support of temporary KEYSTYLE attributes.
            CALL KPG1_ASSET( 'KAPPA_CLINPLOT', '+KEYSTYLE', IPLOTK,
     :                       STATUS )

*  Draw the key to the right of the grid plot and aligned with
*  the top axis.
            CALL KPS1_CLPKY( IPLOTK, YTOP, YBOT, SPBND, SPFRM,
     :                       LABEL, UNIT, KEYOFF, KEYX, STATUS )

*  Report a context message if anything went wrong.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'CLINPLOT_NOKEY', 'Error while creating '//
     :                       'the key.', STATUS )
            END IF

         END IF
      END IF

*  Tidy up.
*  ========
 999  CONTINUE

*  Shutdown PGPLOT and the graphics database.
      CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )

*  Free workspace.
      IF( IPX .NE. 0 ) CALL PSX_FREE( IPX, STATUS )
      IF( IPY .NE. 0 ) CALL PSX_FREE( IPY, STATUS )
      IF( IPN .NE. 0 ) CALL PSX_FREE( IPN, STATUS )

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
