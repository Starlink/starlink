      SUBROUTINE LINPLOT( STATUS )
*+
*  Name:
*     LINPLOT

*  Purpose:
*     Draws a line plot of a 1-d NDF's data values against their axis
*     co-ordinates.

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
*     This application creates a line plot of an array versus its
*     co-ordinates for a 1-dimensional NDF on the current graphics
*     device.  Thus it could be used to display a spectrum.  The array
*     may be part or whole of the data array, but also the error,
*     variance, or quality can be shown.  The plot is situated within
*     the current graphics-database picture.
*
*     The graph of the x-y data points can take one of a number of
*     forms:
*
*        o straight lines connecting successive points;
*        o histogram, where the y co-ordinate is the histogram height;
*        o symbols (from a selection of five) drawn at each point; and
*        o horizontal lines, whose length is specified by the axis
*          width of each pixel.
*
*     Error bars may be plotted too.  There is control of the colour
*     of the components of the plot.
*
*     The graph resides within labelled and enumerated axes
*     corresponding to pixel or data co-ordinates and data value
*     respectively.  The data co-ordinates are derived from the NDF's
*     axis component.  A title and axis labels may be specified.

*  Usage:
*     linplot ndf [comp] [mode] [pltitl] [abslab] [ordlab] [device]

*  ADAM Parameters:
*     ABSLAB = LITERAL (Read)
*        Label for the plot abscissa, in which NCAR fancy founts may be
*        embedded when FONT = "NCAR".  If axis information is present
*        the suggested default is the NDF's axis label followed by the
*        units, in parentheses.  If an error occurs obtaining the label 
*        the default is "Pixel co-ordinates". []
*     ABSLIM( 2 ) = _REAL (Read)
*        The abscissa limits when AXLIM is TRUE.  The limits may extend
*        beyond the extreme axis values present in the dataset, however,
*        the values must be distinct, and positive when XLOG=TRUE.  The
*        order you supply the limits is unimportant; the first value
*        will always have the lower pixel co-ordinate, and the second
*        will always have the larger pixel co-ordinate.  (This
*        restriction arises because graphics database only permits this
*        order.  Use the FLIP command if you want to reverse the axis.)
*
*        The suggested defaults are the values that would have been used
*        had AXLIM=FALSE, and depend on the value of parameter CLEAR.
*        See parameter AXLIM for details.
*     AXLIM = _LOGICAL (Read)
*        If TRUE, you decide the upper and lower limits of the axes via
*        parameters ABSLIM and ORDLIM.  Otherwise, LINPLOT calculates
*        sensible limits as described below.
*
*        When CLEAR=TRUE the limits of the ordinate are derived after
*        applying a margin of 3-percent of the data range to the
*        maximum combined data value and error, and from the minimum
*        combined value and error.  The default abscissa limits are the
*        axis co-ordinate limits after correction for pixel width and
*        axis error.  There may also be up to a half-pixel enlargement
*        if MODE="Histogram".
*
*        When CLEAR=FALSE the plot limits are derived from the previous
*        plot (DATA picture), thus allowing composite pictures to be
*        formed.
*        [FALSE]
*     CLEAR = _LOGICAL (Read)
*        If TRUE the current picture is to be cleared before the
*        line plot is drawn.  If CLEAR is FALSE not only is the existing
*        plot retained, but also the previous plot (DATA picture) is
*        deemed to specify the axis limits when AXLIM=FALSE, and the
*        suggested defaults for parameters ABSLIM and ORDLIM when
*        AXLIM=TRUE.  Thus you can generate a composite plot within
*        a single set of axes, say using different colours or modes to
*        distinguish data from different datasets.
*     COMP = LITERAL (Read)
*        The NDF component to be plotted.  It may be "Data", "Quality",
*        "Variance", or "Error" (where "Error" is the alternative to
*        "Variance" and causes the square root of the variance values
*        to be displayed).  If "Quality" is specified, then the quality
*        values are treated as numerical values (in the range 0 to
*        255). ["Data"]
*     COSYS = LITERAL (Read)
*        The co-ordinate system to be used.  This can be either "World"
*        or "Data".  "World" makes pixel co-ordinates to appear on axes
*        and the bounds are obtained in pixel indices.  If COSYS =
*        "Data" the NDF's axis information is used to annotate axes and
*        the bounds are specified in that co-ordinate system.
*        [Current co-ordinate system]
*     DEVICE = DEVICE (Read)
*        The plotting device. [Current graphics device]
*     ERRBAR = _LOGICAL (Read)
*        TRUE if error bars are to be drawn.  The error bars can
*        comprise either or both of the data and axis-centre errors,
*        depending on what is available in the supplied dataset.  See
*        parameter SHAPE to control the appearance of the error bars.
*        This parameter is ignored unless COMP="Data". [FALSE]
*     ERRCOL = LITERAL (Read)
*        The colour of the error bars (on devices that support colour).
*        See parameter LINCOL for the available options and their
*        meanings.  ERRCOL is only accessed when ERRBAR=TRUE.  [The
*        current value, but equals "1" (the foreground colour) if there
*        is no current value.]
*     FONT = LITERAL (Read)
*        The fount to be used for the line graphics.  It can be either
*        "NCAR" for the NCAR fancy characters and "GKS" for the
*        standard GKS san-serif fount.   The former is intended for
*        hardcopy publication-quality plots, since it is relatively
*        slow; the latter is intended for normal interactive graphics
*        requiring rapid plotting, and it is clearer on small plots.
*        The suggested default is the current value. ["GKS"]
*     FREQ = _INTEGER (Read)
*        The frequency at which error bars are to be plotted.  For
*        instance, a value of 2 would mean that alternative points have
*        error bars plotted.  This lets some plots be less cluttered.
*        FREQ must lie in the range 1 to half of the number of points
*        to be plotted.  FREQ is only accessed when ERRBAR=TRUE.  [1]
*     LINCOL = LITERAL (Read)
*        The colour of the lines showing the data (so this excludes the
*        annotated axes) on devices that support colour.  The options
*        are described below.
*
*          "MAX"          - The maximum colour index in the image 
*                           display colour lookup table.
*          "MIN"          - The minimum (non-reserved) colour index in 
*                           the image-display colour lookup table.
*          An integer     - The actual colour index.  It is constrained
*                           between 0 and the maximum colour index
*                           available on the device. 
*          A named colour - Uses the named colour from the palette, and
*                           if it is not present, the nearest colour
*                           from the palette is selected.
*
*        If the colour is to remain unaltered as the lookup table is
*        manipulated choose an integer between 0 and 15, or a named
*        colour.  This parameter will be ignored if symbols are plotted.
*        [The current value, but equals "1" (the foreground colour) if
*        there is no current value.] 
*     MAJTIC( 2 ) = _REAL (Read)
*        The parameter controlling the numbers of major tick marks
*        for the x and y axes.  (Number used is between MAJTIC+2 and
*        5*MAJTIC/2+4.) [4.,4.]
*     MINTIC( 2 ) = _REAL (Read)
*        The number of minor tick marks between each major tick mark
*        for the x and y axes.  A negative value forces the graphics
*        package to compute appropriate values.  The number of
*        minor tick marks per major tick is fixed (8) for a logarithmic
*        axis. [-1.,-1.]
*     MODE = LITERAL (Read)
*        The type of the line plot.  This can be one of the following
*        values.
*
*           "Histogram"   - An histogram of the points is plotted (with
*                           vertical lines only joining the y values and
*                           not extending to the base of the plot).  The
*                           vertical lines are placed midway between
*                           adjacent x positions.
*           "Line"        - The points are joined by straight lines.
*           "Point"       - A dot is plotted at each point.
*           "Step"        - Each point is displayed as a horizontal
*                           line, whose length is specified by the axis
*                           width of the pixel.
*           1             - A synonym for "Point".
*           2--5          - These are similar to "Point", but give
*                           different symbols.  2 gives plus signs,  3
*                           generates asterisks, 4 produces circles, and
*                           5 creates multiplication crosses.
*
*        Where colour is available the lines of the "Histogram", "Line",
*        and "Step" options are plotted in the colour defined by
*        parameter LINCOL; likewise the symbols of options "Point" and
*        the integers is plotted in the colour specified through
*        parameter SYMCOL.
*
*        MODE is defaulted to the current value, which is initially
*        "Line". []
*     NDF = NDF (Read)
*        NDF structure containing the array to be plotted.
*     ORDLAB = LITERAL (Read)
*        Label for the plot ordinate, in which NCAR fancy founts may be
*        embedded when FONT = "NCAR".  The suggested default is the
*        NDF's label followed by the units, if present, in parentheses.  
*        If an error occurs obtaining the label the default is the
*        component name followed by " values". []
*     ORDLIM( 2 ) = _REAL (Read)
*        The ordinate limits when AXLIM is TRUE.  The limits may extend
*        beyond the extreme data values present in the dataset, however,
*        the values must be distinct, and positive when YLOG=TRUE.
*        It is usually best to allow some margin to prevent the plotted
*        values hitting the axes.  The order you supply the limits is
*        unimportant; the smaller value will become the lower limit,
*        and the larger the upper limit.
*
*        The suggested defaults are the values that would have been used
*        had AXLIM=FALSE, and depend on the value of parameter CLEAR.
*        See parameter AXLIM for details.
*     OUTTIC = _LOGICAL (Read)
*        TRUE if the axis tick marks are to appear on the outside of
*        the axes instead of inside. By default, the tick marks are
*        drawn inside the plot region.  [FALSE]
*     PLTITL = LITERAL (Read)
*        The title of the plot.  Up to about 40 characters can be
*        accommodated, and NCAR fancy founts may be embedded when
*        FONT = "NCAR".  The suggested default is the title of the NDF.
*        If an error occurs obtaining the title, it is defaulted to
*        "Line plot".  []
*     PXSIZE = _REAL (Read)
*        The horizontal size of the display in metres. If a value less
*        than the default is requested, the display will appear at
*        the bottom left of the current device.  There is an upper
*        limit given by the x size of the current picture. [Maximum
*        that can fit in the current picture]
*     PYSIZE = _REAL (Read)
*        The vertical size of the display in metres. If a value less
*        than the default is requested, then the display will appear at
*        the bottom left of the current device.  There is an upper
*        limit given by the y size of the current picture. [Maximum
*        that can fit in the current picture]
*     SHAPE = LITERAL (Read)
*        The way the errors are to be represented graphically.  SHAPE
*        can take the following values.
*
*           "Bars"     - A cross with serifs is plotted joining the x
*                        error limits and then the y error limits.
*           "Cross"    - A san-serif cross is plotted joining the x
*                        error limits and then the y error limits.
*           "Diamond"  - Adjacent error limits are joined to form an
*                        error diamond.
*
*        SHAPE is defaulted to the current value, which is initially
*        "Bars".  SHAPE is only accessed when ERRBAR=TRUE.  []
*     SYMCOL = LITERAL (Read)
*        The colour of the plotted symbols (on devices that support
*        colour).  See parameter LINCOL for the available options and
*        their meanings.  SYMCOL is only accessed when MODE="Point"
*        or has a integer value.   [The current value, but equals "1"
*        (the foreground colour) if there is no current value.] 
*     THICK = _REAL (Read)
*        The thickness of the axes and annotations in the plot, where
*        1.0 is the normal thickness.  Currently, this is only available
*        on a few devices.  When FONT="GKS", axis annotations are
*        unaffected by THICK.  It must take a value in the range
*        0.5--10.0.  [1.0]
*     XLOG = _LOGICAL (Read)
*        TRUE if the abscissa (pixel number) is to be logarithmic.  It
*        is unlikely that you would want to do this.  The chosen
*        array may be truncated to include only positive co-ordinates.
*        [FALSE]
*     YLOG = _LOGICAL (Read)
*        TRUE if the ordinate (data value) is to be logarithmic.  This
*        is useful when the data have wide dynamic range. [FALSE]

*  Examples:
*     linplot spectrum cosys=d
*        Plots data values versus data co-ordinate for the whole of the
*        1-dimensional NDF called spectrum on the current graphics
*        device.  The data co-ordinates will be in pixels if spectrum
*        does not have an axis component (this remark applies to all
*        the examples save the last where world co-ordinates are
*        plotted).
*     linplot spectrum(1:500) device=graphon
*        Plots data values versus the data or pixel co-ordinates
*        (whichever is the current system) for the first 500 elements
*        of the 1-dimensional NDF called spectrum on the Graphon
*        device.
*     linplot ironarc v pltitl="Fe Arc variance"
*        Plots variance values versus data or pixel co-ordinate for the
*        whole of the 1-dimensional NDF called ironarc on the current
*        graphics device.  The plot has a title of "Fe Arc variance".
*     linplot rscvn(3000.42:3994.) noclear abslab="Epoch" cosys=d
*        This plots on the current graphics device data values versus
*        data co-ordinates for those elements of the 1-dimensional NDF
*        called rscvn whose axis values lie between 3000.42 and 3994.0.
*        If the current co-ordinate system is already "Data", the COSYS
*        parameter may be dispensed with.  The device is not cleared so
*        the plot will overlay the existing picture.  The abscissa has
*        label "Epoch".
*     linplot frequencies mode=h lincol=red ylog
*        This draws a red histogram of the 1-dimensional dataset called
*        frequencies.  The red colour will continue to be used in
*        subsequent plots.  The data values will be plotted
*        logarithmically.  
*     linplot xspec mode=p errbar shape=d errcol=pink symcol=1 xlog
*        This plots the data values versus the logarithm of the axis
*        centres for the dataset called xspec.  Each pixel is plotted
*        as a point surrounded by diamond-shaped error bars.  The
*        points are drawn in the foreground colour and the error
*        diamonds are displayed in pink.
*     linplot ndf=spectrum axlim ordlim=[100,250] cosys=w accept
*        Plots data values versus pixel co-ordinate for the whole of
*        the 1-dimensional NDF called spectrum on the current graphics
*        device.  The limits of the ordinate axis are 100 and 250, so
*        data values outside this range will be clipped.
*     linplot ndf=spectrum2 lincol=yellow noclear
*        This plots data values versus pixel co-ordinate, overlaid on
*        the previous example, for the 1-dimensional NDF called
*        spectrum2.  The new locus is plotted in yellow.

*  Notes:
*     -  The application stores a number of pictures in the graphics
*     database in the following order: a FRAME of the specified size
*     containing the title, annotated axes, and line plot; and a DATA
*     picture, which has world co-ordinates for linear axes measured in
*     pixels along the x axis and data values along y, and their
*     logarithms if a logarithmic axis is selected.  The DATA picture
*     also has data co-ordinates stored; for a linear axis this
*     requires that the NDF's axis units are not pixel co-ordinates;
*     for a logarithmic axis the actual data co-ordinate or value is
*     recorded.  If there is no NDF axis information and a logarithmic
*     abscissa, the DATA co-ordinates are pixel co-ordinates.  The NDF
*     associated with the plot is stored by reference with the DATA
*     picture.  On exit the current database picture for the chosen
*     device reverts to the input picture.
*     -  In a logarithmic plot only positive data along each
*     logarithmic axis can be displayed, therefore negative data are 
*     excluded.  Likewise any error bar which has a non-positive
*     limit is not plotted, even though the data point itself can
*     appear if it has positive co-ordinates.  A logarithmic axis will
*     always increase from left to right, and from bottom to top.
*     -  Bad pixels are excluded from the plot, and they do not affect
*     the limits of the ordinate.  The same applies to zero or negative
*     data values if the plot is to have a logarithmic ordinate.
*     -  When COSYS="World", default axis errors and widths are used,
*     if needed.  The defaults are the constants 0 and 1 respectively.
*     -  If you wish to make a composite plot, ensure that parameters
*     ABSLAB, ORDLAB, COSYS, XLOG, and YLOG do not change between plots.
*     For COSYS="Data" and XLOG=FALSE, the data co-ordinates must
*     be linear.

*  Algorithm:
*     -  Find the type of plot.
*     -  Find which component to display, obtain an identifier to the
*     NDF and check that the component is present. Find the data type
*     for processing.  Get the NDF bounds, the number of significant
*     dimensions and inquire the bad-pixel flag.  Determine which
*     co-ordinate system is to be used.
*     -  Ascertain the type of axes requested.
*     -  Get the bounds of the slice in the significant dimension
*     between define bound limits.  Convert from data co-ordinates if
*     necessary.  Allow for the logarithmic x axis by constraining the
*     bounds to be positive.  Create and map the slice.
*     -  Determine whether error bars are required and their frequency
*     and shape.
*     -  Obtain the plot title and axis labels from the NDF where
*     that is possible.
*     -  Obtain the ordinate limits via parameter or by calculation
*     depending on the plot type and whether or not error bars are
*     used.  Recalculate the limits to have a margin.
*     -  Get the display device and open the database for it with the
*     appropriate access mode. Get the current SGS zone.
*     -  Create the frame picture and store it in the database.
*     -  Obtain the remaining plot attributes, including the colours
*     of the lines, symbols, and error bars.
*     -  For data co-ordinates obtain the axis co-ordinates.  Constrain
*     the co-ordinate bounds to be positive in a logarithmic plot.
*     Find the minimum positive value needed for the plot allowing for
*     errors and the type of plot.  Recalculate the limits to have a
*     margin.  If using world co-ordinates also make a smaller section
*     if there are negative bounds, again allowing some margin.  Then
*     get some work space which is filled with pixel co-ordinates.
*     -  Draw the line plot, and error bars where requested.  Record the
*     plot as the data picture in the database.  Record the NDF
*     reference in the database.  Store the transformations from world
*     to data co-ordinates evaluating a scale and offset except for
*     logarithmic axes when there is no NDF axis structure, or the
*     abscissa is logarithmic and the data co-ordinate system is
*     plotted.
*     -  Tidy any workspace used, reset the pen attributes, the
*     AGI database and the NDF system.

*  Related Applications:
*     KAPPA: INSPECT, MLINPLOT; Figaro: ESPLOT, IPLOTS, MSPLOT, SPLOT;
*     SPECDRE: SPECGRID.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, VARIANCE,
*     QUALITY, LABEL, TITLE, and UNITS components of the NDF.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.  Only
*     single-precision floating-point data can be processed directly.
*     Other non-complex data types will undergo a type conversion
*     before the line plot is drawn.

*  Implementation Deficiencies:
*     Some missing options to tailor the plot.  NCAR does strange
*     things if the picture is too small.  A method to circumvent the
*     problem is being investigated.

*  Authors:
*     Malcolm Currie STARLINK (RAL::CUR)
*     {enter_new_authors_here}

*  History:
*     1991 February 11 (MJC):
*        Original NDF version.
*     1991 April 27 (MJC):
*        Stored both world and data co-ordinates in the graphics
*        database.
*     1991 May 1 (MJC):
*        Renamed IN parameter to NDF for consistency.
*     1991 May 24 (MJC):
*        Added world co-ordinates option.
*     1991 July 31 (MJC):
*        No longer redefines colours of SGS pens to predefined state if
*        workstation has dynamic colour representation, now there is
*        palette control.
*     1991 August 20 (MJC):
*        Added FONT parameter.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1992 April 14 (MJC):
*        Handles arbitrary user-defined sections from non-1-dimensional
*        NDFs.  Removed XLIMIT parameter.
*     1992 November 30 (MJC):
*        Does not use non-monotonic axis centres.
*     1995 October 19 (MJC):
*        Supports Error component.
*     1996 October 3 (MJC):
*        Added several new facilities: error-bar option with choice of
*        styles, choice of plot styles via parameter MODE, use of
*        different colours for the lines, symbols, and error bars.
*     1997 January 12 (MJC):
*        Used PSX to obtain workspace more efficiently.  Made thick
*        lines work for all parts of the plot.
*     1997 April 7 (MJC):
*        Redesigned to allow easy production of composite plots,
*        controlled using the CLEAR parameter.  Parameter AXLIM
*        introduced to replace ORDLIM.  ORDLIM is now the ordinate
*        limits, replacing ORDLOW and ORDUPP.  Added control of the
*        x-limits via parameter ABSLIM.  Used dynamic defaults for
*        ABSLIM and ORDLIM.  Increased the maximum line thickness.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Global SSE definitions
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'NDF_PAR'          ! Magic-value definitions
      INCLUDE 'NDF_ERR'          ! NDF_ error definitions
      INCLUDE 'PRM_PAR'          ! PRIMDAT definitions
      INCLUDE 'PAR_ERR'          ! Parameter-system error definitions
      INCLUDE 'GKS_PAR'          ! GKS constants (e.g. GSET)

*  Status:
      INTEGER STATUS

*  External References:
      EXTERNAL SNX_AGGUX
      REAL SNX_AGGUX             ! Converts NCAR x grid co-ordinate to
                                 ! world co-ordinate
      EXTERNAL SNX_AGGUY
      REAL SNX_AGGUY             ! Converts NCAR y grid co-ordinate to
                                 ! world co-ordinate

*  Local Constants:
      REAL ANCLP1                ! Fraction of the frame zone in which
      REAL ANCLP2                ! the line plot will appear when there
      REAL ANCLP3                ! are axes.  Negative requests the
      REAL ANCLP4                ! default, so here just allows more
                                 ! room for the plot title at the top.
      PARAMETER ( ANCLP1 = -1.0, ANCLP2 = -1.0,
     :            ANCLP3 = -1.0, ANCLP4 = 0.91 )

      INTEGER ERRPEN             ! SGS pen number used to plot error 
      PARAMETER ( ERRPEN = 4 )   ! bars

      INTEGER LINPEN             ! SGS pen number used to plot graph
      PARAMETER ( LINPEN = 2 )   ! bars

      INTEGER NDIM               ! Dimensionality of input array
      PARAMETER( NDIM = 1 )      ! 1-d data

      INTEGER SYMPEN             ! SGS pen number used to draw symbols
      PARAMETER ( SYMPEN = 3 )

*  Local Variables:
      CHARACTER * ( 72 ) ABSLAB  ! Label for the abscissa (pixels)
      REAL ABSLIM( 2 )           ! Abscissa (axis-centre) limits
      REAL ABSRNG                ! Abscissa range
      CHARACTER * ( 17 ) ACOMP   ! List of axis arrays to be mapped
      INTEGER ACTDIM             ! Actual number of dimensions in the
                                 ! NDF
      INTEGER ADIM               ! The dimension along which data are
                                 ! plotted
      REAL ANCLIP( 4 )           ! Fraction of the frame zone in which the
                                 ! plot will appear when there are axes.
                                 ! (Needed as parameter cannot be an
                                 ! array.)
      CHARACTER * ( NDF__SZTYP ) ATYPE ! Processing type of the axis centres
      LOGICAL AXES               ! Axes to be plotted?
      LOGICAL AXLIM              ! The user to set the axis limits?
      LOGICAL BAD                ! May bad pixels be present in the
                                 ! image
      LOGICAL CLEAR              ! Is the graphics device is to be
                                 ! cleared before display of the array?
      CHARACTER * ( 8 ) COMP     ! Component to be displayed
      CHARACTER * ( 5 ) COSYS    ! Co-ordinate system
      LOGICAL DACOOR             ! Data co-ordinates are to be stored
                                 ! in the database?
      LOGICAL DATACO             ! Axes are given in data co-ordinates?
      DOUBLE PRECISION DB( 2 )   ! Bounds of the slice in pixels or data
                                 ! co-ordinates
      LOGICAL DEVCAN             ! Graphics-device parameter is to be
                                 ! cancelled?
      DOUBLE PRECISION DOFSET( NDIM + 1 ) ! Offsets in the world-to-data
      LOGICAL DPAXIS             ! Axis centres are double precision?
      DOUBLE PRECISION DSCALE( NDIM + 1 ) ! Scale factors in the world-to-data
                                 ! co-ordinate transformations
      CHARACTER * ( NDF__SZTYP ) DTYPE ! Type of the image after
                                 ! processing (not used)
      INTEGER DXPNTR( 2 )        ! Pointers to the d.p. axis centre and
                                 ! and error arrays
      INTEGER ECOLI              ! Original colour index of pen used for
                                 ! plotting error bars
      INTEGER EL                 ! Number of elements in the input array
      INTEGER ELNTYP             ! Line type for current error-bar SGS
                                 ! pen
      REAL ELWIDT                ! Width of current error-bar SGS pen
      REAL EMAX                  ! Maximum data co-ordinate allowing for
                                 ! error bars
      REAL EMIN                  ! Minimum data co-ordinate allowing for
                                 ! error bars
      LOGICAL EPLR               ! Polyline representation used for
                                 ! error bars to be reset?
      LOGICAL ERRBAR             ! Error bars to be plotted?
      INTEGER ERRCI              ! Colour index required for error bars
      LOGICAL ERWORK             ! Workspace for pixel (zero) errors
                                 ! obtained?
      CHARACTER * ( 9 ) ESHAPE   ! Shape of error bars
      INTEGER ESPACE             ! Spacing between points with error
                                 ! bars
      LOGICAL FWORK              ! Workspace for flipped axis centres
                                 ! obtained?
      LOGICAL FEWORK             ! Workspace for flipped axis error
                                 ! obtained?
      CHARACTER * ( 4 ) FOUNT    ! Fount type
      LOGICAL FWWORK             ! Workspace for flipped pixel widths
                                 ! obtained?
      INTEGER FXPNTR( 3 )        ! Pointers to the flipped x-axis
                                 ! co-ordinates and errors
      INTEGER GSTAT              ! GKS status
      REAL HXMAX                 ! Maximum data co-ordinate in histogram
      REAL HXMIN                 ! Minimum data co-ordinate in histogram
      INTEGER IERR               ! GKS error indicator
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Processing type of the image
      INTEGER IWKID              ! GKS workstation identifier
      INTEGER LASF( 13 )         ! GKS list of aspect source flags
      INTEGER LCOLI              ! Original colour index of pen used for
                                 ! plotting the lines
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of the NDF
      INTEGER LINCI              ! Colour index required for lines
      INTEGER LLNTYP             ! Line type for current lines SGS pen
      REAL LLWIDT                ! Width of current lines SGS pen
      REAL MAJTIC( 2 )           ! Parameters controlling the numbers of
                                 ! major tick marks along x and y axes
                                 ! respectively
      INTEGER MAXPOS             ! Index of maximum (not used)
      CHARACTER * ( 8 ) MCOMP    ! Component to be mapped
      INTEGER MINPOS             ! Index of minimum (not used)
      REAL MINTIC( 2 )           ! Numbers of minor tick marks along x and
                                 ! y axes respectively
      CHARACTER * ( 9 ) MODE     ! Type of the plot
      LOGICAL MONOTO             ! Axis is monotonic?
      INTEGER NDF                ! NDF identifier
      INTEGER NDFC               ! Identifier for input section
      INTEGER NDFS               ! NDF identifier of the section
      INTEGER NINVAL             ! Number of bad axis values (not used)
      REAL OFFSET( NDIM + 1 )    ! Offsets in the world-to-data
      CHARACTER * ( 72 ) ORDLAB  ! Label for the ordinate (data values)
      REAL ORDLIM( 2 )           ! Ordinate limits
      REAL ORDRNG                ! Ordinate range
      LOGICAL OUTTIC             ! Axis tick marks are to be placed
                                 ! outside the box instead of inside?
      INTEGER PICID1             ! Graphics' database identifier on
                                 ! input
      INTEGER PICID2             ! Graphics' database identifier for
                                 ! the frame picture
      INTEGER PICID3             ! Graphics' database identifier for
                                 ! the displayed line plot picture
      INTEGER PICIDC             ! Current graphics' database identifier
      INTEGER PICIDL             ! Graphics' database identifier for the
                                 ! last DATA picture
      REAL PIXCO( 2 )            ! Abscissa pixel co-ordinate limits
      CHARACTER * ( 72 ) PLTITL  ! Title of the plot
      INTEGER PNTRI( 2 )         ! Pointers to the NDF data and error
                                 ! arrays
      LOGICAL PWORK              ! Workspace for pixel co-ordinates
                                 ! obtained?
      INTEGER PXPNTR( 3 )        ! Pointers to the x-axis co-ordinates
                                 ! and errors
      REAL RMAXV                 ! Maximum data value
      REAL RMINV                 ! Maximum data value
      REAL SCALE( NDIM + 1 )     ! Scale factors in the world-to-data
                                 ! co-ordinate transformations
      INTEGER SCOLI              ! Original colour index of pen
                                 ! used for plotting symbols
      INTEGER SIGDIM( NDF__MXDIM ) ! The significant dimensions of the NDF
      INTEGER SLNTYP             ! Line type for current symbol SGS pen
      REAL SLWIDT                ! Width of the current symbol SGS pen
      LOGICAL SPLR               ! Polyline representation used for
                                 ! symbols to be reset?
      INTEGER SYMBOL             ! Symbol number in point plot
      INTEGER SYMCI              ! Colour index required for symbols
      LOGICAL TCKCTR             ! The numbers of tick marks cannot be
                                 ! controlled?
      REAL TEMP                  ! Workspace
      LOGICAL THERE              ! A nominated data object is present ?
      REAL THICK                 ! The line thickness (standard is 1.0)
      REAL THRESH( 2 )           ! Thresholds for YLOG option
      REAL TICDEF( 2 )           ! Suggested default axis-tick values
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of the NDF
      LOGICAL USEWID             ! Use axis widths?
      LOGICAL WIWORK             ! Workspace for pixel widths obtained?
      INTEGER WPNTR              ! Pointer to workspace for histogram
      REAL WX( 2 )               ! DATA picture's limiting abscissa
                                 ! world co-ordinates
      REAL WY( 2 )               ! DATA picture's limiting ordinate
                                 ! world co-ordinates
      INTEGER XBOUND( 2 )        ! Index bounds of the x axis
      REAL XLDEF( 2 )            ! Dynamic defaults for abscissa limits
      LOGICAL XLOG               ! Abscissa will be logarithmic?
      LOGICAL XREVER             ! X-axis has reversed data
                                 ! co-ordinates?
      REAL YLDEF( 2 )            ! Dynamic defaults for ordinate limits
      LOGICAL YLOG               ! Ordinate will be logarithmic?
      INTEGER ZONE1              ! Initial SGS zone identifier
      INTEGER ZONEF              ! SGS frame-zone identifier

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      DEVCAN = .FALSE.
      EPLR = .FALSE.
      SPLR = .FALSE.

*  Obtain the plot style.
*  ======================  

*  Determine the type of the plot.
      CALL PAR_MIX0I( 'MODE', 'Line', 1, 5, 'Line,Histogram,Point,Step',
     :                .FALSE., MODE, STATUS )

*  See if the value is numeric.
      IF ( MODE .NE. 'LINE' .AND. MODE .NE. 'HISTOGRAM' .AND.
     :     MODE .NE. 'POINT' .AND. MODE .NE. 'STEP' ) THEN

*  Convert the output numeric string to its numeric value.  The values
*  have already been validated by the PAR routine.
         CALL CHR_CTOI( MODE, SYMBOL, STATUS )

*  A numeric value means use the point mode with the numbered symbol.
*  So reassign the mode to be a point type of plot.
         MODE = 'POINT'

*  A point has symbol 1.
      ELSE IF ( MODE .EQ. 'POINT' ) THEN
         SYMBOL = 1
      END IF

*  Set up a useful variable
      USEWID = MODE .EQ. 'STEP'

*  See whether picture is to be refreshed or not.
      CALL PAR_GTD0L( 'CLEAR', .TRUE., .TRUE., CLEAR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Obtain and map the array to be plotted.
*  =======================================

*  Find which component to plot.
      CALL PAR_CHOIC( 'COMP', 'Data', 'Data,Quality,Error,Variance',
     :                .FALSE., COMP, STATUS )

*  Most NDF routines with a component argument don't recognise 'ERROR',
*  so we need two variables.  Thus convert 'ERROR' into 'VARIANCE' in
*  the variable needed for such routines.  The original value is held
*  in a variable with the prefix M for mapping, as one of the few
*  routines that does support 'ERROR' is NDF_MAP; it is also needed for
*  plot annotations using any NDF units.
      MCOMP = COMP
      IF ( COMP .EQ. 'ERROR' ) COMP = 'VARIANCE'

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain the identifier of the NDF to be plotted.
      CALL NDF_ASSOC( 'NDF', 'READ', NDF, STATUS )

*  There must be a data array, but for other components check that
*  requested component is present.
      IF ( COMP .NE. 'DATA' ) THEN
         CALL NDF_STATE( NDF, COMP, THERE, STATUS )

*  The component is not present or not defined.
         IF ( .NOT. THERE ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'COMP', COMP )
            CALL ERR_REP( 'LINPLOT_NOCOMP',
     :        'LINPLOT: ^COMP component is not defined.', STATUS )
            GO TO 980
         END IF
      END IF

*  This application can only process real components directly.
*  Therefore for the given type of the image find in which type it
*  should be processed.  Currently, it is obvious since only one type
*  is supported, but this acts as a placeholder when this is no longer
*  true.  It may still be possible to handle d.p. data provided the
*  dynamic range is not too small.
      CALL ERR_MARK
      CALL NDF_MTYPE( '_REAL', NDF, NDF, COMP, ITYPE, DTYPE, STATUS )
      IF ( STATUS .EQ. NDF__TYPNI ) THEN
         CALL ERR_FLUSH( STATUS )
         CALL MSG_OUT( 'PRECLOSS', 'The loss of precision may not be '/
     :     /'serious so continuing to process in _REAL.', STATUS )
         ITYPE = '_REAL'
      END IF
      CALL ERR_RLSE

*  Ensure that the number of significant dimensions is correct.
      CALL KPG1_SGDIM( NDF, NDIM, SIGDIM, STATUS )

*  Get the bounds of the NDF.
      CALL NDF_BOUND( NDF, NDF__MXDIM, LBND, UBND, ACTDIM, STATUS )

*  Use a shorthand for the significant dimension.
      ADIM = SIGDIM( NDIM )

*  Set upper insignificant bounds to one.  We have to make a section so
*  that trailing insignificant bounds may be shifted when the user has
*  specified the whole NDF.  This cannot be done for the base NDF.
      CALL NDF_SECT( NDF, ACTDIM, LBND, UBND, NDFC, STATUS )
      CALL KPG1_SECSH( NDFC, ADIM, STATUS )

*  Check whether or not bad pixels may be present.
      CALL NDF_BAD( NDF, COMP, .FALSE., BAD, STATUS )

*  Get the type of co-ordinates to place on axes.
*  ==============================================

*  Is there an axis system?
      CALL NDF_STATE( NDF, 'Axis', DACOOR, STATUS )

*  Obtain the desired co-ordinate system.
      CALL PAR_CHOIC( 'COSYS', 'Data', 'Data,World', .FALSE., COSYS,
     :                STATUS )

*  Find the effective co-ordinate system.
      DATACO = DACOOR .AND. COSYS .EQ. 'DATA'

*  Find the implementation type of the axis structure.
*  ===================================================

*  Integer needs d.p. because it potentially has ten significant
*  digits.  The implementation type is only required when there is an
*  axis structure present.
      DPAXIS = .FALSE.
      IF ( DACOOR ) THEN
         CALL NDF_ATYPE( NDF, 'Centre', ADIM, ATYPE, STATUS )
         IF ( ATYPE .EQ. '_INTEGER' .OR. ATYPE .EQ. '_DOUBLE' ) THEN
            ATYPE = '_DOUBLE'

*  Initialise the flag to indicate the type.
            DPAXIS = .TRUE.

         ELSE
            ATYPE = '_REAL'
            DPAXIS = .FALSE.
         END IF
      END IF

*  Are the axes to be logarithmic?
      CALL PAR_GTD0L( 'XLOG', .FALSE., .TRUE., XLOG, STATUS )
      CALL PAR_GTD0L( 'YLOG', .FALSE., .TRUE., YLOG, STATUS )
      TCKCTR = XLOG .AND. YLOG

*  Decide whether error bars should be plotted.
      ERRBAR = .FALSE.
      IF ( COMP .EQ. 'DATA' ) THEN
         CALL PAR_GET0L( 'ERRBAR', ERRBAR, STATUS )

         IF ( ERRBAR ) THEN

*  Obtain the shape of error bar to plot.
            CALL PAR_CHOIC( 'SHAPE', 'Bars', 'Bars,Cross,Diamond',
     :                      .TRUE., ESHAPE, STATUS )

         END IF
      END IF
  
*  Proceed no further as something has gone wrong.
      IF ( STATUS .NE. SAI__OK ) GOTO 980

*  Get the bounds of the slice.
*  ============================

*  Map the axis centres.  Use double precision to prevent loss of
*  precision.  Access the error array when data co-ordinates are
*  available and error bars are requested.
      CALL NDF_AMAP( NDF, 'Centre', ADIM, '_DOUBLE', 'READ', DXPNTR, EL,
     :               STATUS )

*  Is the axis monotonic?  Start a new error context so that the error
*  reports concerning a non-monotonic axis may be annulled.  Instead we
*  issue a warning message so that the application can continue by
*  using world co-ordinates.
      CALL ERR_MARK
      CALL KPG1_MONOD( .TRUE., EL, %VAL( DXPNTR( 1 ) ), MONOTO, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         MONOTO = .FALSE.
      END IF
      CALL ERR_RLSE

*  Issue the warning.  Change the emphasis depending on whether the
*  co-ordinate system is DATA.
      IF ( .NOT. MONOTO ) THEN
         CALL MSG_SETI( 'IAXIS', ADIM )
         IF ( DATACO ) THEN
            CALL MSG_OUT( 'LINPLOT_NOTMONO1',
     :        'LINPLOT: Axis ^IAXIS is not monotonic.  Will '/
     :        /'use world co-ordinates instead.', STATUS )
         ELSE
            CALL MSG_OUT( 'LINPLOT_NOTMONO2',
     :        'LINPLOT: Axis ^IAXIS is not monotonic.  Will not '/
     :        /'record axis bounds in the graphics database.', STATUS )
         END IF

*  Reset the co-ordinate system and axis-present flags.
         DATACO = .FALSE.
         DACOOR = .FALSE.
      END IF

*  Derive the pixel-index bounds, and their corresponding axis values
*  (when data co-ordinates are required).  The bounds are positive for
*  a logarithmic plot axis.  This routine assumes a monotonic axis.
      CALL KPS1_LIXLM( LBND( ADIM ), UBND( ADIM ), %VAL( DXPNTR( 1 ) ),
     :                 DATACO, XLOG, XBOUND, DB, STATUS )

*  Unmap the axis centres.
      CALL NDF_AUNMP( NDF, 'Centre', ADIM, STATUS )

*  Define new bounds.
      LBND( ADIM ) = XBOUND( 1 )
      UBND( ADIM ) = XBOUND( 2 )

*  Create and map the slice.
*  =========================

*  Create the sub-array.
      CALL NDF_SECT( NDFC, ADIM, LBND, UBND, NDFS, STATUS )

*  Map the array with read access.  For error bars the data and error
*  arrays are required.
      IF ( ERRBAR ) THEN
         CALL NDF_MAP( NDFS, 'Data,Error', ITYPE, 'READ', PNTRI, EL,
     :                 STATUS )
      ELSE
         CALL NDF_MAP( NDFS, MCOMP, ITYPE, 'READ', PNTRI, EL, STATUS )
      END IF

*  Obtain the spacing between points showing the error bars no that the
*  number of elements is known.
      IF ( ERRBAR ) CALL PAR_GDR0I( 'FREQ', 1, 1, MAX( 1, EL/2 ),
     :  .TRUE., ESPACE, STATUS )

*  Proceed no further as something has gone wrong.
      IF ( STATUS .NE. SAI__OK ) GOTO 980

*  Obtain the plot title and axis labels.
*  ======================================

*  Get the title.
      CALL KPG1_GNTIT( NDF, 'PLTITL', 'Line plot', PLTITL, STATUS )

      IF ( DATACO ) THEN

*  Get the abscissa label suggesting the value in the NDF axis
*  structure, if present, as the default.
         CALL KPG1_GAXLB( NDF, ADIM, 'ABSLAB', 'Pixel co-ordinates',
     :                    ABSLAB, STATUS )
      ELSE

*  Get the abscissa label without consulting the NDF's axis structure
*  to prevent the wrong label being associated with the world
*  co-ordinates.
         CALL PAR_DEF0C( 'ABSLAB', 'Pixel co-ordinates', STATUS )
         CALL PAR_GET0C( 'ABSLAB', ABSLAB, STATUS )
      END IF

*  Get the ordinate label suggesting the value in the NDF label and
*  units components, if present, as the default, otherwise the
*  component name followed by values is used.
      CALL KPG1_GNLBU( NDF, 'ORDLAB', MCOMP, ORDLAB, STATUS )

*  Proceed no further as something has gone wrong.
      IF ( STATUS .NE. SAI__OK ) GOTO 960

*  Start the graphics system.
*  ==========================

*  Associate a graphics device in the database and obtain the
*  zone that matches the current picture.
      IF ( CLEAR ) THEN
         CALL AGS_ASSOC( 'DEVICE', 'WRITE', ' ', PICID1, ZONE1, STATUS )
      ELSE
         CALL AGS_ASSOC( 'DEVICE', 'UPDATE', ' ', PICID1, ZONE1,
     :                   STATUS )
      END IF

*  Create the frame picture.
*  =========================
      CALL KPG1_FRPIC( 'PXSIZE', 'PYSIZE', 'KAPPA_LINPLOT', .FALSE.,
     :                 ZONEF, PICID2, STATUS )

*  Reset the input picture as current in case of an accident.
      CALL AGI_SELP( PICID1, STATUS )

*  Set the pen attributes for plotting.
*  ====================================

*  Get the line thickness.
      CALL PAR_GDR0R( 'THICK', 1.0, 0.5, 10.0, .TRUE., THICK, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 960

*  Note that this must be done before plotting to avoid plot
*  regeneration (i.e. clear the plot when the device is closed).

*  Want solid lines for the plotting.
      CALL KPG1_SOLIN( STATUS )

*  Obtain the colour index for the desired colour of the error bars.
*  Don't restrict the colours to the palette to give the user more
*  control.  There are instructions in the documentation on the
*  benefits of choosing a palette colour.
      CALL KPG1_IVCI( 'DEVICE', 'LINCOL', .FALSE., LINCI, STATUS )

*  Inquire the workstation identifier for GKS inquiries.
      CALL SGS_ICURW( IWKID )

*  Inquire the current colour index of this pen (it will be restored
*  after all plotting is complete).
      CALL GQPLR( IWKID, LINPEN, GSET, IERR, LLNTYP, LLWIDT, LCOLI )

*  Store the new colour index and line thickness for this pen.  However,
*  the latter appears not to work (probably due to NCAR resetting
*  something.)
      CALL GSPLR( IWKID, LINPEN, LLNTYP, THICK, LINCI )

*  Obtain the colour index for the desired colour of the error bars.
*  Don't restrict the colours to the palette to give the user more
*  control.  There are instructions in the documentation on the
*  benefits of choosing a palette colour.
      IF ( ERRBAR ) THEN
         CALL KPG1_IVCI( 'DEVICE', 'ERRCOL', .FALSE., ERRCI, STATUS )

*  Inquire the current colour index of this pen (it will be restored
*  after all plotting is complete).
         CALL GQPLR( IWKID, ERRPEN, GSET, IERR, ELNTYP, ELWIDT, ECOLI )

*  Store the new colour index and width for this pen.  However,
*  the latter appears not to work (probably due to NCAR resetting
*  something).
         CALL GSPLR( IWKID, ERRPEN, ELNTYP, THICK, ERRCI )
         EPLR = .TRUE.
      END IF

*  Obtain the colour index for the desired colour of the error bars.
*  Don't restrict the colours to the palette to give the user more
*  control.  There are instructions in the documentation on the
*  benefits of choosing a palette colour.
      IF ( MODE .EQ. 'POINT' ) THEN
         CALL KPG1_IVCI( 'DEVICE', 'SYMCOL', .FALSE., SYMCI, STATUS )

*  Inquire the current colour index of this pen (it will be restored
*  after all plotting is complete).
         CALL GQPLR( IWKID, SYMPEN, GSET, IERR, SLNTYP, SLWIDT, SCOLI )

*  Store the new colour index and line width for this pen.  However,
*  the latter appears not to work (probably due to NCAR resetting
*  something).
         CALL GSPLR( IWKID, SYMPEN, SLNTYP, THICK, SYMCI )
         SPLR = .TRUE.
      END IF

*  Obtain the ordinate limits.
*  ===========================

*  Determine whether or not to define the axis limits.
      CALL PAR_GTD0L( 'AXLIM', .FALSE., .TRUE., AXLIM, STATUS )

*  Proceed no further as something has gone wrong.
      IF ( STATUS .NE. SAI__OK ) GOTO 960

      IF ( AXLIM ) THEN

*  Obtain the ordinate-limit defaults.
*  ===================================
*
*  The defaults depend on whether the picture is being overlaid or not,
*  and hence the value of parameter CLEAR.  Overlaid pictures
*  (CLEAR=FALSE) offer the bounds used for the previous picture, so
*  that composites can be constructed.  New plots derive limits from
*  the dataset, with a little border.
         IF ( CLEAR ) THEN

*  Obtain the bounds of the data array.
            CALL KPG1_MXMNR( .TRUE., EL, %VAL( PNTRI( 1 ) ), NINVAL,
     :                       RMAXV, RMINV, MAXPOS, MINPOS, STATUS )

*  Derived the limits, allowing a 5-percent border.  Constrain the
*  logarithmic values to be positive.
            IF ( YLOG ) THEN
               RMINV = MAX( VAL__SMLR, RMINV )
               RMAXV = MAX( VAL__SMLR, RMAXV )
               ORDRNG = LOG10( RMAXV ) - LOG10( RMINV )
               YLDEF( 2 ) = 10.0 ** ( LOG10( RMAXV ) + 0.03 * ORDRNG )
               YLDEF( 1 ) = 10.0 ** ( LOG10( RMINV ) - 0.02 * ORDRNG )
            ELSE
               ORDRNG = RMAXV - RMINV
               YLDEF( 1 ) = RMINV - 0.02 * ORDRNG
               YLDEF( 2 ) = RMAXV + 0.03 * ORDRNG
            END IF

*  The plotting surface is not cleared.
         ELSE

*  Obtain the identifier to the current picture.
            CALL AGI_ICURP( PICIDC, STATUS )

*  Access the last DATA picture.
            CALL KPG1_AGFND( 'DATA', PICIDL, STATUS )

*  Obtain its world co-ordinate bounds.
            CALL AGI_IWOCO( WX( 1 ), WX( 2 ), WY( 1 ), WY( 2 ), STATUS )

*  Convert from world to data co-ordinates if using the data
*  co-ordinate system.
            IF ( DATACO ) THEN
               CALL AGI_TWTOD( PICIDL, 2, WX, WY, XLDEF, YLDEF, STATUS )

*  Just transfer the values to the defaults.
            ELSE
               XLDEF( 1 ) = WX( 1 )
               XLDEF( 2 ) = WX( 2 )
               YLDEF( 1 ) = WY( 1 )
               YLDEF( 2 ) = WY( 2 )
            END IF

         END IF

*  Obtain the ordinate limits.
*  ===========================
         ORDLIM( 1 ) = 0.
         ORDLIM( 2 ) = 0.

*  Start an error context.
         CALL ERR_MARK

*  Get the upper and lower limits, which must be different.
         DO WHILE ( ABS( ORDLIM( 2 ) - ORDLIM( 1 ) ) .LE. VAL__EPSR *
     :              ( 0.5 * ABS( ORDLIM( 2 ) ) +
     :                0.5 * ABS( ORDLIM( 1 ) ) ) )

*  Obtain the limits.  Note the different lower limit for a logarithmic
*  ordinate.
            IF ( YLOG ) THEN
               CALL PAR_GDR1R( 'ORDLIM', 2, YLDEF, VAL__SMLR,
     :                         VAL__MAXR, .TRUE., ORDLIM, STATUS )

            ELSE
               CALL PAR_GDR1R( 'ORDLIM', 2, YLDEF, VAL__MINR,
     :                         VAL__MAXR, .TRUE., ORDLIM, STATUS )
            END IF

            IF ( STATUS .EQ. PAR__ABORT ) THEN
               CALL ERR_RLSE
               GOTO 960
            END IF

*  Report an error immediately if the values are the same.
            IF ( ABS( ORDLIM( 2 ) - ORDLIM( 1 ) ) .LT. VAL__SMLR *
     :           ( 0.5 * ABS( ORDLIM( 2 ) ) +
     :             0.5 * ABS( ORDLIM( 1 ) ) ) ) THEN

               STATUS = SAI__ERROR
               CALL ERR_REP( 'ERR_LINPLOT_EQLIM2',
     :           'LINPLOT: Maximum ordinate limit equals minimum. Try '/
     :           /'again', STATUS )
               CALL ERR_FLUSH( STATUS )

*  Cancel the parameter to enable a re-try when something has gone
*  wrong.
               CALL PAR_CANCL( 'ORDLIM', STATUS )
            END IF

*  Axis cannot be reversed, so ensure that the first value is the lower
*  limit and the second is the upper limit.
            TEMP = ORDLIM( 1 )
            ORDLIM( 1 ) = MIN( TEMP, ORDLIM( 2 ) )
            ORDLIM( 2 ) = MAX( TEMP, ORDLIM( 2 ) )

         END DO

*  Release the error context.
         CALL ERR_RLSE

*  The ordinate limits are to be calculated by the programme.
      ELSE

*  Find the extreme values from the array.  We define the lower
*  threshold for a log axis, hence locate the smallest positive value or
*  value less one standard deviation, depending on whether or not error
*  bars are required.
         IF ( CLEAR ) THEN

            IF ( YLOG ) THEN
               THRESH( 1 ) = VAL__SMLR
               THRESH( 2 ) = VAL__MAXR
            ELSE
               THRESH( 1 ) = VAL__MINR
               THRESH( 2 ) = VAL__MAXR
            END IF

            IF ( ERRBAR ) THEN

*  Find the limits of the values allowing for their 1-sigma errors.
               CALL KPG1_MXMER( .TRUE., EL, %VAL( PNTRI( 1 ) ),
     :                          %VAL( PNTRI( 2 ) ), 1.0, THRESH, NINVAL,
     :                          ORDLIM( 2 ), ORDLIM( 1 ), MAXPOS,
     :                          MINPOS, STATUS )
            ELSE
               CALL KPG1_MMTHR( .TRUE., EL, %VAL( PNTRI( 1 ) ), THRESH,
     :                          NINVAL, ORDLIM( 2 ), ORDLIM( 1 ),
     :                          MAXPOS, MINPOS, STATUS )
            END IF

*  Derive the range.  Allow a 5-percent margin.  Constrain the
*  logarithmic values to be positive.
            IF ( YLOG ) THEN
               ORDRNG = LOG10( ORDLIM( 2 ) ) - LOG10( ORDLIM( 1 ) )
               ORDLIM( 2 ) = 10.0 ** ( LOG10( ORDLIM( 2 ) ) +
     :                       0.03 * ORDRNG )
               ORDLIM( 1 ) = 10.0 ** ( LOG10( ORDLIM( 1 ) ) -
     :                       0.02 * ORDRNG )
            ELSE
               ORDRNG = ORDLIM( 2 ) - ORDLIM( 1 )
               ORDLIM( 2 ) = ORDLIM( 2 ) + 0.03 * ORDRNG
               ORDLIM( 1 ) = ORDLIM( 1 ) - 0.02 * ORDRNG
            END IF

*  The plotting surface is not cleared.
         ELSE

*  Obtain the identifier to the current picture.
            CALL AGI_ICURP( PICIDC, STATUS )

*  Access the last DATA picture.
            CALL KPG1_AGFND( 'DATA', PICIDL, STATUS )

*  Obtain its world co-ordinate bounds.
            CALL AGI_IWOCO( WX( 1 ), WX( 2 ), WY( 1 ), WY( 2 ), STATUS )

*  Convert from world to data co-ordinates if using the data
*  co-ordinate system.
            IF ( DATACO ) THEN
               CALL AGI_TWTOD( PICIDL, 2, WX, WY, ABSLIM, ORDLIM,
     :                         STATUS )

*  Just transfer the values to the limits.
            ELSE
               ABSLIM( 1 ) = WX( 1 )
               ABSLIM( 2 ) = WX( 2 )
               ORDLIM( 1 ) = WY( 1 )
               ORDLIM( 2 ) = WY( 2 )
            END IF

         END IF

*  If the AGI database was used to define the limits or default limits.
         IF ( .NOT. CLEAR ) THEN

*  Return to the previous picture. 
            CALL AGI_SELP( PICIDC, STATUS )

*  Release the DATA picture.
            CALL AGI_ANNUL( PICIDL, STATUS )
         END IF
      END IF

*  Obtain the axis co-ordinates and their limits.
*  ==============================================
*
*  Initialise the flags to say that no workspace has been obtained.
*  They will be used later for tidying.
      PWORK = .FALSE.
      ERWORK = .FALSE.
      WIWORK = .FALSE.
      FWORK = .FALSE.
      FEWORK = .FALSE.
      FWWORK = .FALSE.

      IF ( DATACO ) THEN

*  Use data co-ordinates.
*  ----------------------

*  Define which axis components to map.
         IF ( ERRBAR ) THEN
            IF ( USEWID ) THEN
               ACOMP = 'Centre,Error,Width'
            ELSE
               ACOMP = 'Centre,Error'
            END IF
         ELSE
            IF ( USEWID ) THEN
               ACOMP = 'Centre,Width'
            ELSE
               ACOMP = 'Centre'
            END IF
         END IF

*  Map axis centres this time in single precision for the NCAR/GKS
*  co-ordinate system.  Also map the axis errors if error bars are to be
*  plotted, and the widths if a step plot is to be produced.
         CALL NDF_AMAP( NDFS, ACOMP, ADIM, '_REAL', 'READ',
     :                  PXPNTR, EL, STATUS )

*  To reduce later coding, always use the same pointer for the widths.
*  When the widths are used without error bars the widths would be
*  accessed by the second element of the pointer.  When using pixel
*  co-ordinates the default widths will always be accessed by the third
*  element of the array of pointers.
         IF ( USEWID .AND. .NOT. ERRBAR ) PXPNTR( 3 ) = PXPNTR( 2 )

*  Note that the defaults or limits derived from AGI are already known
*  when CLEAR is FALSE.  We just need to find the limits of defaults
*  for a completely new plot.
         IF ( CLEAR ) THEN

*  Logarithmic bounds have already been accounted for.
            ABSLIM( 1 ) = REAL( DB( 1 ) )
            ABSLIM( 2 ) = REAL( DB( 2 ) )

            IF ( ERRBAR .OR. USEWID ) THEN

*  Find the limits of the axis values allowing for their 1-sigma errors
*  and/or their widths.  We can use the existing ABSLIM( 1 ) and
*  ABSLIM( 2 ) otherwise.
               CALL KPG1_AXEXR( EL, %VAL( PXPNTR( 1 ) ), ERRBAR,
     :                          %VAL( PXPNTR( 2 ) ), USEWID,
     :                          %VAL( PXPNTR( 3 ) ), 1.0, EMIN, EMAX,
     :                          STATUS )

*  Constrain the limits to be positive for a logarithmic axis.  Not that
*  the sense of the axis must be considered.
               IF ( XLOG ) THEN
                  IF ( ABSLIM( 1 ) .LT. ABSLIM( 2 ) ) THEN
                     IF ( EMIN .GT. 0.0 ) ABSLIM( 1 ) = EMIN
                     ABSLIM( 2 ) = EMAX
                  ELSE
                     IF ( EMAX .GT. 0.0 ) ABSLIM( 2 ) = EMAX
                     ABSLIM( 1 ) = EMIN
                  END IF

*  Just adopt the new limits that allow for the error bars.
               ELSE
                  ABSLIM( 1 ) = EMIN
                  ABSLIM( 2 ) = EMAX
               END IF
            END IF

*  Find the limits of the histogram-style of plot.
            IF ( MODE .EQ. 'HISTOGRAM' ) THEN
               CALL KPS1_LIHEX( EL, %VAL( PXPNTR( 1 ) ), HXMIN, HXMAX,
     :                          STATUS )

*  Find the overall x limits for the histogram plot. Allow for the
*  sense of the x axis.  Constrain the limits to be positive for a
*  logarithmic axis.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( ABSLIM( 1 ) .LT. ABSLIM( 2 ) ) THEN
                     IF ( XLOG ) THEN
                        IF ( HXMIN .GT. 0.0 )
     :                    ABSLIM( 1 ) = MIN( ABSLIM( 1 ), HXMIN )
                     ELSE
                        ABSLIM( 1 ) = MIN( ABSLIM( 1 ), HXMIN )
                     END IF
                     ABSLIM( 2 ) = MAX( ABSLIM( 2 ), HXMAX )
                  ELSE
                     ABSLIM( 1 ) = MAX( ABSLIM( 1 ), HXMIN )
                     IF ( XLOG ) THEN
                        IF ( HXMAX .GT. 0.0 )
     :                    ABSLIM( 2 ) = MIN( ABSLIM( 2 ), HXMAX )
                     ELSE
                        ABSLIM( 2 ) = MIN( ABSLIM( 2 ), HXMAX )
                     END IF
                  END IF
               END IF
            END IF
         END IF

*  Use pixel co-ordinates.
*  =======================
      ELSE

*  Deal with pixel co-ordinates where the lower limits are known.
*  Allow more room for the histogram locus which extends beyond the
*  pixel centres by 0.5 pixels.  It is also convenient to use the same
*  extension to ensure that the first and last point or line is
*  plotted.  It may be supressed if it starts or ends at the x bounds
*  of the region (ZONTS-like rounding problem?).  However, the lower
*  limit must be positive for a logarithmic axis.  Set the lower limit
*  to a semi-arbitrary 0.5 though the line goes to minus infinity.
*  Note the LBND( ADIM ) will always be at least 1 for a logarithmic
*  axis, thanks to KPS1_LIXLM.
         IF ( CLEAR ) THEN
            IF ( XLOG ) THEN
               ABSLIM( 1 ) = MAX( REAL( LBND( ADIM ) ) - 1.0, 0.5 )
            ELSE
               ABSLIM( 1 ) = REAL( LBND( ADIM ) ) - 1.0
            END IF
            ABSLIM( 2 ) = REAL( UBND( ADIM ) )
         END IF
      END IF

*  Create the pixel axis annotations.
*  ==================================
*
*  We need to create the axis arrays if using world co-ordinates as
*  opposed to the data co-ordinates taken from the NDF.
      IF ( .NOT. DATACO ) THEN

*  Obtain workspace.  Note that the same pointer is used as for data
         CALL PSX_CALLOC( EL, '_REAL', PXPNTR( 1 ), STATUS )
         PWORK = .TRUE.

*  Fill the array with pixel co-ordinates.
         CALL KPG1_SSCOF( EL, 1.0D0, DBLE( LBND( ADIM ) ) - 0.5D0,
     :                    %VAL( PXPNTR( 1 ) ), STATUS )

*  Supply default axis error bars if required.
         IF ( ERRBAR ) THEN

*  Obtain workspace.  Note that the same pointer is used as for the axis
*  errors.
            CALL PSX_CALLOC( EL, '_REAL', PXPNTR( 2 ), STATUS )
            ERWORK = .TRUE.

*  Fill the work array with a constant error.  These have value 0 (by
*  definition for pixel co-ordinates).
            CALL KPG1_FILLR( 0.0, EL, %VAL( PXPNTR( 2 ) ), STATUS )
         END IF

*  Supply default axis widths if required.
         IF ( USEWID ) THEN

*  Obtain workspace.  Note that the same pointer is used as for the axis
*  widths (when there are no error bars).
            CALL PSX_CALLOC( EL, '_REAL', PXPNTR( 3 ), STATUS )
            WIWORK = .TRUE.

*  Fill the work array with constant widths.  These have value 1 (by
*  definition for pixel co-ordinates).
            CALL KPG1_FILLR( 1.0, EL, %VAL( PXPNTR( 3 ) ), STATUS )
         END IF

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 940

*  Finally allow some margin so the first and last points or lines can
*  be seen.  Derive the range and expand it.  Note the this only applies
*  to a completely new plot.
      IF ( CLEAR ) THEN
         IF ( MODE .NE. 'HISTOGRAM' ) THEN
            IF ( XLOG ) THEN
               ABSRNG = LOG10( ABSLIM( 2 ) ) - LOG10( ABSLIM( 1 ) )
               ABSLIM( 2 ) = 10.0 ** ( LOG10( ABSLIM( 2 ) ) +
     :                       0.02 * ABSRNG )
               ABSLIM( 1 ) = 10.0 ** ( LOG10( ABSLIM( 1 ) ) -
     :                       0.02 * ABSRNG )
            ELSE
               ABSRNG = ABSLIM( 2 ) - ABSLIM( 1 )
               ABSLIM( 2 ) = ABSLIM( 2 ) + 0.02 * ABSRNG
               ABSLIM( 1 ) = ABSLIM( 1 ) - 0.02 * ABSRNG
            END IF
         END IF
      END IF

*  Obtain the abscissa-limit defaults.
*  ===================================
*
*  The defaults depend on whether the picture is being overlaid or not,
*  and hence the value of parameter CLEAR.  Overlaid pictures
*  (CLEAR=FALSE) offer the bounds used for the previous picture, so
*  that composites can be constructed.  New plots derive limits from
*  the dataset.  The CLEAR=FALSE defaults have already been determined
*  and assigned when the ordinate limits were found.
      IF ( CLEAR ) THEN

*  Set the defaults to the limiting values calculated from the data.
         XLDEF( 1 ) = ABSLIM( 1 )
         XLDEF( 2 ) = ABSLIM( 2 )
      END IF

*  Obtain the abscissa limits.
*  ===========================
      IF ( AXLIM ) THEN
         ABSLIM( 1 ) = 0.
         ABSLIM( 2 ) = 0.

*  Limits correspond to the order lower then upper pixel co-ordinate.
         XREVER =  XLDEF( 1 ) .GT. XLDEF( 2 )

*  Start an error context.
         CALL ERR_MARK

*  Get the upper and lower limits, which must be different.
         DO WHILE ( ABS( ABSLIM( 2 ) - ABSLIM( 1 ) ) .LE. VAL__EPSR *
     :              ( 0.5 * ABS( ABSLIM( 2 ) ) +
     :                0.5 * ABS( ABSLIM( 1 ) ) ) )

*  Obtain the limits.  Note the different lower limit for a logarithmic
*  abscissa.
            IF ( YLOG ) THEN
               CALL PAR_GDR1R( 'ABSLIM', 2, XLDEF, VAL__SMLR,
     :                         VAL__MAXR, .TRUE., ABSLIM, STATUS )

            ELSE
               CALL PAR_GDR1R( 'ABSLIM', 2, XLDEF, VAL__MINR,
     :                         VAL__MAXR, .TRUE., ABSLIM, STATUS )
            END IF

            IF ( STATUS .EQ. PAR__ABORT ) THEN
               CALL ERR_RLSE
               GOTO 940
            END IF

*  Report an error immediately if the values are the same.
            IF ( ABS( ABSLIM( 2 ) - ABSLIM( 1 ) ) .LT. VAL__SMLR *
     :           ( 0.5 * ABS( ABSLIM( 2 ) ) +
     :             0.5 * ABS( ABSLIM( 1 ) ) ) ) THEN

               STATUS = SAI__ERROR
               CALL ERR_REP( 'ERR_LINPLOT_EQLIM1',
     :           'LINPLOT: Maximum abscissa limit equals minimum.  '/
     :           /'Try again', STATUS )
               CALL ERR_FLUSH( STATUS )

*  Cancel the parameter to enable a re-try when something has gone
*  wrong.
               CALL PAR_CANCL( 'ABSLIM', STATUS )
            END IF

*  Has the user swapped the order, thus violating the pixel co-ordinate
*  system.  If so swap the values.
            IF (    ( XREVER .AND. ABSLIM( 1 ) .LT. ABSLIM( 2 ) ) .OR.
     :        ( .NOT. XREVER .AND. ABSLIM( 1 ) .GT. ABSLIM( 2 ) ) ) THEN
               TEMP = ABSLIM( 1 )
               ABSLIM( 1 ) = ABSLIM( 2 )
               ABSLIM( 2 ) = TEMP
            END IF
         END DO

*  Release the error context.
         CALL ERR_RLSE
      END IF

*  Reverse x-axis arrays.
*  ======================

*  The SGS window must have world co-ordinates that increase from left
*  to right.  However, the x-axis may be drawn in reverse by NCAR when
*  the lower limit is greater than the upper limit.  In this case
*  reverse all of the axis arrays in workspace.  For a logarithmic the
*  axis cannot be reversed despite the sense of the co-ordinates in the
*  dataset.
      IF ( ABSLIM( 1 ) .GT. ABSLIM( 2 ) .AND. .NOT. XLOG ) THEN

         CALL PSX_CALLOC( EL, '_REAL', FXPNTR( 1 ), STATUS )
         FWORK = .TRUE.
         CALL KPG1_FLIPR( 1, EL, %VAL( PXPNTR( 1 ) ), 1,
     :                    %VAL( FXPNTR( 1 ) ), STATUS )

         IF ( ERRBAR ) THEN
            CALL PSX_CALLOC( EL, '_REAL', FXPNTR( 2 ), STATUS )
            FEWORK = .TRUE.
            CALL KPG1_FLIPR( 1, EL, %VAL( PXPNTR( 2 ) ), 1,
     :                       %VAL( FXPNTR( 2 ) ), STATUS )
         END IF

         IF ( USEWID ) THEN
            CALL PSX_CALLOC( EL, '_REAL', FXPNTR( 3 ), STATUS )
            FWWORK = .TRUE.
            CALL KPG1_FLIPR( 1, EL, %VAL( PXPNTR( 3 ) ), 1,
     :                       %VAL( FXPNTR( 3 ) ), STATUS )
         END IF
      END IF
 
*  Obtain the axis style.
*  ======================  

*  Tick numbers are not altered for a logarithmic axis.
      IF ( .NOT. TCKCTR ) THEN

*  Get the number of minor ticks, assigning the dynamic defaults.
         TICDEF( 1 ) = -1.
         TICDEF( 2 ) = -1.
         CALL PAR_GDR1R( 'MINTIC', 2, TICDEF, -1., VAL__MAXR, .FALSE.,
     :                   MINTIC, STATUS )

*  Get the parameter controlling the number of major ticks per axis,
*  assigning the dynamic defaults.
         TICDEF( 1 ) = 4.
         TICDEF( 2 ) = 4.
         CALL PAR_GDR1R( 'MAJTIC', 2, TICDEF, -1., VAL__MAXR, .FALSE.,
     :                   MAJTIC, STATUS )
      END IF

*  Are the tick marks on the outside of the axes?
      CALL PAR_GTD0L( 'OUTTIC', .FALSE., .TRUE., OUTTIC, STATUS )

*  Get the fount.  Although NCAR is the default, either must be
*  selected to prevent persistence from earlier invocations.
      CALL PAR_CHOIC( 'FONT', 'GKS', 'GKS,NCAR', .TRUE., FOUNT, STATUS )
      IF ( FOUNT .EQ. 'GKS ' ) THEN
         CALL AGPWRT( 0.0, 0.0, ' ', 0, 0, 0, -100 )
      ELSE IF ( FOUNT .EQ. 'NCAR' ) THEN
         CALL AGPWRT( 0.0, 0.0, ' ', 0, 0, 0, 100 )
      END IF

      CALL GKS_GSTAT( STATUS )

*  Define the location of the axes and draw them.
*  ==============================================
      ANCLIP( 1 ) = ANCLP1
      ANCLIP( 2 ) = ANCLP2
      ANCLIP( 3 ) = ANCLP3
      ANCLIP( 4 ) = ANCLP4

      AXES = .TRUE.
      IF ( AXES ) THEN

*  Get AUTOGRAPH to use the SGS zone.
         CALL SNX_AGWV

*  Draw annotated axes in graph window with the grid positioned as
*  defined above.  This revises some of the NCAR defaults to make the
*  axes clearer.  Note there may be problems for d.p. data co-ordinates
*  due to GKS's use of single precision.
         CALL KPG1_NCAXS( ABSLIM( 1 ), ORDLIM( 1 ), ABSLIM( 2 ),
     :                    ORDLIM( 2 ), ANCLIP, PLTITL, ABSLAB, ORDLAB,
     :                    XLOG, YLOG, MINTIC, MAJTIC, OUTTIC, THICK,
     :                    .FALSE., STATUS )

      END IF

*  Set the line width.
*  ===================

*  For some reason to do with NCAR, this has to appear after NCAR has
*  done its work to make thickness graph lines.  Therefore, FONT=GKS
*  can never be drawn thicker.
*  
*  See if the line thickness is to be altered for all lines.
      IF ( ABS( THICK - 1.0 ) .GT. VAL__EPSR ) THEN

*  Inquire the GKS aspect source flags.
         CALL GQASF( GSTAT, LASF )

*  Set the line width scale factor source flags to individual.
         LASF( 2 ) = 1
         CALL GSASF( LASF )

*  Now actually set the line-width scale factor.
         CALL GSLWSC( THICK )

*  Watch out for any error.
         CALL GKS_GSTAT( STATUS )
      END IF

      IF ( STATUS .NE. SAI__OK ) GOTO 940

*  Draw the line plot.
*  ===================

*  Obtain workspace for the histogram option.
      IF ( MODE .EQ. 'HISTOGRAM' ) 
     :    CALL PSX_CALLOC( EL, '_INTEGER', WPNTR, STATUS )

*  Deal with an unreversed abscissa first.  For a logarithmic the axis
*  cannot be reversed despite the sense of the co-ordinates in the
*  dataset.
      IF ( ABSLIM( 1 ) .LT. ABSLIM( 2 ) .OR. XLOG ) THEN

*  Draw the locus of the line plot.
         CALL KPS1_LINPL( MODE, EL, %VAL( PXPNTR( 1 ) ),
     :                    %VAL( PXPNTR( 3 ) ), %VAL( PNTRI( 1 ) ),
     :                    ABSLIM( 1 ), ABSLIM( 2 ), ORDLIM( 1 ),
     :                    ORDLIM( 2 ), XLOG, YLOG, LINPEN, SYMBOL,
     :                    SYMPEN, %VAL( WPNTR ), STATUS )

         IF ( ERRBAR ) THEN

*  Draw the error bars.
            CALL KPG1_ERBAR( ESHAPE, ERRPEN, ESPACE, EL,
     :                       %VAL( PXPNTR( 1 ) ), %VAL( PXPNTR( 2 ) ),
     :                       %VAL( PNTRI( 1 ) ), %VAL( PNTRI( 2 ) ),
     :                       ABSLIM( 1 ), ABSLIM( 2 ), ORDLIM( 1 ),
     :                       ORDLIM( 2 ), XLOG, YLOG, STATUS )
         END IF

*  Deal with a reversed x-axis.
      ELSE

*  Draw the locus of the line plot.
         CALL KPS1_LINPL( MODE, EL, %VAL( FXPNTR( 1 ) ),
     :                    %VAL( FXPNTR( 3 ) ), %VAL( PNTRI( 1 ) ),
     :                    ABSLIM( 1 ), ABSLIM( 2 ), ORDLIM( 1 ),
     :                    ORDLIM( 2 ), XLOG, YLOG, LINPEN, SYMBOL,
     :                    SYMPEN, %VAL( WPNTR ), STATUS )

         IF ( ERRBAR ) THEN

*  Draw the error bars.
            CALL KPG1_ERBAR( ESHAPE, ERRPEN, ESPACE, EL,
     :                       %VAL( FXPNTR( 1 ) ), %VAL( FXPNTR( 2 ) ),
     :                       %VAL( PNTRI( 1 ) ), %VAL( PNTRI( 2 ) ),
     :                       ABSLIM( 1 ), ABSLIM( 2 ), ORDLIM( 1 ),
     :                       ORDLIM( 2 ), XLOG, YLOG, STATUS )

         END IF
      END IF

*  Free the workspace used for the histogram option.
      IF ( MODE .EQ. 'HISTOGRAM' ) CALL PSX_FREE( WPNTR, STATUS )

      CALL GKS_GSTAT( STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ERR_LINPLOT_PLOT',
     :     'LINPLOT: Error removing bad pixels or during the plotting.',
     :     STATUS )
      ELSE

*  Map the axis centres with the correct implementation type if not
*  already done so.
*  =================================================================

*  When world co-ordinates have been requested, but the NDF contains
*  axis information a transformation from world to data co-ordinates is
*  possible.  However, there is no axis array mapped at this point,
*  since the x co-ordinates are stored in workspace.  Therefore free
*  this workspace, since it is no longer required, record this fact,
*  and map the axis centres using the implementation data type.
         IF ( PWORK .AND. DACOOR ) THEN
            CALL PSX_FREE( PXPNTR( 1 ), STATUS )
            PWORK = .FALSE.
            CALL NDF_AMAP( NDFS, 'Centre', ADIM, ATYPE, 'READ',
     :                     PXPNTR, EL, STATUS )

*  Real axis centres are already mapped for GKS/NCAR, but if the data
*  co-ordinates require double precision, unmap the real version and
*  remap in '_DOUBLE'.
         ELSE IF ( DATACO .AND. DPAXIS ) THEN
            CALL NDF_AUNMP( NDFS, '*', ADIM, STATUS )
            CALL NDF_AMAP( NDFS, 'Centre', ADIM, '_DOUBLE', 'READ',
     :                     PXPNTR, EL, STATUS )
         END IF

*  Determine the scale and offset of the transformation between world
*  and data positional co-ordinates.
*  ==================================================================

*  Note that unflipped co-ordinates are used, hence could not be freed
*  earlier.
         IF ( DACOOR ) THEN
            IF ( DPAXIS ) THEN

*  Find the transformation for the x axis.
               CALL KPG1_DWSOD( LBND( ADIM ), UBND( ADIM ),
     :                          %VAL( PXPNTR( 1 ) ), DSCALE( 1 ),
     :                          DOFSET( 1 ), STATUS )

*  Since the ordinate units are not co-ordinates, but data values, the
*  identity transformation is established.
               DSCALE( 2 ) = 1.0D0
               DOFSET( 2 ) = 0.0D0

*  Single-precision is sufficient.
            ELSE

*  Find the transformation for the x axis.
               CALL KPG1_DWSOR( LBND( ADIM ), UBND( ADIM ),
     :                          %VAL( PXPNTR( 1 ) ), SCALE( 1 ),
     :                          OFFSET( 1 ), STATUS )

*  Since the ordinate units are not co-ordinates, but data values, the
*  identity transformation is established.
               SCALE( 2 ) = 1.0
               OFFSET( 2 ) = 0.0

            END IF
         END IF

*  Define the DATA picture's world co-ordinates if not already set.
*  ================================================================

*  Set the world co-ordinates that will be stored in the database to
*  pixels along the abscissa to follow the KAPPA convention.  This
*  cannot be done for logarithmic plots, so log( world co-ordinates )
*  are stored.  ***XLOG case may prevent a composite plot.***

*  Get the lower and upper abscissa limits at the left and right of the
*  NCAR grid.
         IF ( .NOT. XLOG ) THEN
            ABSLIM( 1 ) = SNX_AGGUX( 0.0 )
            ABSLIM( 2 ) = SNX_AGGUX( 1.0 )

*  Get the lower and upper ordinate limits at the bottom and top of the
*  NCAR grid.
            IF ( YLOG ) THEN
               ORDLIM( 1 ) = LOG10( SNX_AGGUY( 0.0 ) )
               ORDLIM( 2 ) = LOG10( SNX_AGGUY( 1.0 ) )
            ELSE
               ORDLIM( 1 ) = SNX_AGGUY( 0.0 )
               ORDLIM( 2 ) = SNX_AGGUY( 1.0 )
            END IF

*  Convert the abscissa data co-ordinates to pixels using the linear
*  transformation.
            IF ( DATACO ) THEN
               IF ( DPAXIS ) THEN
                  PIXCO( 1 ) = ( ABSLIM( 1 ) - REAL( DOFSET( 1 ) ) ) /
     :                         REAL( DSCALE( 1 ) )
                  PIXCO( 2 ) = ( ABSLIM( 2 ) - REAL( DOFSET( 1 ) ) ) /
     :                         REAL( DSCALE( 1 ) )
               ELSE
                  PIXCO( 1 ) = ( ABSLIM( 1 ) - OFFSET( 1 ) ) /
     :                         SCALE( 1 )
                  PIXCO( 2 ) = ( ABSLIM( 2 ) - OFFSET( 1 ) ) /
     :                         SCALE( 1 )
               END IF

*  Assign the world co-ordinates.  Note that for pixel co-ordinates the
*  limits returned by SNX_AGGUX are already pixel co-ordinates.
               CALL SGS_SW( PIXCO( 1 ), PIXCO( 2 ), ORDLIM( 1 ),
     :                      ORDLIM( 2 ), STATUS )

            ELSE
               CALL SGS_SW( ABSLIM( 1 ), ABSLIM( 2 ), ORDLIM( 1 ),
     :                      ORDLIM( 2 ), STATUS )
            END IF
         END IF

*  Record the data picture in the database.
*  ========================================
*
         CALL KPG1_SDTRN( 'KAPPA_LINPLOT', NDF, PICID3, STATUS )

*  World co-ordinates for data are log( data co-ordinates ), therefore
*  taking the anti-log will result in data positions.  When there is no
*  axis information a logarithmic abscissa or ordinate has world
*  co-ordinates which are log( data co-ordinates) or log( data values )
*  respectively.  Therefore, taking the anti-log will produced the
*  desired result.
         IF ( ( XLOG .AND. ( DATACO .OR. .NOT. DACOOR ) ) .OR.
     :        ( YLOG .AND. .NOT. DACOOR ) ) THEN

*  Store the transformation for logarithmic world co-ordinates.
            CALL KPG1_LGTRN( XLOG, YLOG, STATUS )

         ELSE IF ( DACOOR ) THEN

*  Store the transformation between world and data positional
*  co-ordinates.
*  ==========================================================
            IF ( DPAXIS ) THEN

*  Since the ordinate units are not co-ordinates, but data values, the
*  identity transformation is established.
               DSCALE( 2 ) = 1.0D0
               DOFSET( 2 ) = 0.0D0

*  Only store the transformation for data co-ordinates not in pixel
*  co-ordinates.
               IF ( ABS( DSCALE( 1 ) - 1.0D0 ) .GT. VAL__EPSD  .OR.
     :              ABS( DOFSET( 1 ) - 0.0D0 ) .GT. VAL__EPSD ) THEN
                  CALL KPG1_LLTRD( XLOG, YLOG, DSCALE, DOFSET, STATUS )

               ELSE IF ( XLOG ) THEN

*  Store the transformation for logarithmic world co-ordinates.  This
*  allows for the case where the axis array contains merely pixel
*  co-ordinates.
                  CALL KPG1_LGTRN( XLOG, YLOG, STATUS )
               END IF

*  Single-precision is sufficient.
            ELSE

*  Find the transformation for the x axis.
               CALL KPG1_DWSOR( LBND( ADIM ), UBND( ADIM ),
     :                          %VAL( PXPNTR( 1 ) ), SCALE( 1 ),
     :                          OFFSET( 1 ), STATUS )

*  Since the ordinate units are not co-ordinates, but data values, the
*  identity transformation is established.
               SCALE( 2 ) = 1.0
               OFFSET( 2 ) = 0.0

*  Only store the transformation for data co-ordinates not in pixel
*  co-ordinates.
               IF ( ABS( SCALE( 1 ) - 1.0 ) .GT. VAL__EPSR .OR.
     :              ABS( OFFSET( 1 ) - 0.0 ) .GT. VAL__EPSR ) THEN
                  CALL KPG1_LLTRR( XLOG, YLOG, SCALE, OFFSET, STATUS )

               ELSE IF ( XLOG ) THEN

*  Store the transformation for logarithmic world co-ordinates.  This
*  allows for the case where the axis array contains merely pixel
*  co-ordinates.
                  CALL KPG1_LGTRN( XLOG, YLOG, STATUS )
               END IF
            END IF
        END IF
      END IF

*  Tidy the workspace.
*  ===================
 940  CONTINUE
      IF ( PWORK ) CALL PSX_FREE( PXPNTR( 1 ), STATUS )
      IF ( ERWORK ) CALL PSX_FREE( PXPNTR( 2 ), STATUS )
      IF ( WIWORK ) CALL PSX_FREE( PXPNTR( 3 ), STATUS )
      IF ( FWORK ) CALL PSX_FREE( FXPNTR( 1 ), STATUS )
      IF ( FEWORK ) CALL PSX_FREE( FXPNTR( 2 ), STATUS )
      IF ( FWWORK ) CALL PSX_FREE( FXPNTR( 3 ), STATUS )

*  Graphics closedown sequence.
*  ============================
 960  CONTINUE

*  Reset the line width.
      IF ( ABS( THICK - 1.0 ) .GT. VAL__EPSR ) THEN

*  Set the line width scale factor source flags to bundled.
         LASF( 2 ) = 0
         CALL GSASF( LASF )

*  Watch out for any error.
         CALL GKS_GSTAT( STATUS )
      END IF

*  Restore pens to their former state.
      CALL GSPLR( IWKID, LINPEN, LLNTYP, LLWIDT, LCOLI )
      IF ( EPLR ) CALL GSPLR( IWKID, ERRPEN, ELNTYP, ELWIDT, ECOLI )
      IF ( SPLR ) CALL GSPLR( IWKID, SYMPEN, SLNTYP, SLWIDT, SCOLI )

*  Close the device.
      CALL AGS_DEASS( 'DEVICE', DEVCAN, STATUS )

*  Unmap and annul NDF data.
*  =========================
 980  CONTINUE
      CALL NDF_END( STATUS )

 999  CONTINUE

      END
