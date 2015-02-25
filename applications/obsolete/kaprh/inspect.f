      SUBROUTINE INSPECT( STATUS )
*+
*  Name:
*     INSPECT

*  Purpose:
*     Inspects a 2-d NDF in a variety of ways.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL INSPECT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application provides an interactive facility to inspect the
*     details of whole or part of the 2-dimensional data array in an
*     input NDF.  Briefly, the inspection options permit: a region to
*     be selected for which statistics may be calculated, its values
*     written to a text file, or an histogram be plotted and saved in
*     an NDF; the region itself may be saved in an NDF; the value of a
*     pixel or a region of pixels to be viewed; a slice between two
*     pixels may be calculated, plotted and saved in an NDF; text
*     files containing x-y-value of selected pixels may be created and
*     extended, and chosen pixels marked.
*
*     The application has two modes of interaction: cursor and
*     interface.  In cursor mode the selection of pixels, and the
*     definition of the region are made by moving a graphics cursor
*     over a previously displayed image or contour plot.  Since
*     instructional text showing the function of the mouse or
*     trackerball buttons is shown, the graphics device providing the
*     cursor must be an image-display overlay.  Also the name of the
*     NDF used to display the image or contour plot is known and need
*     not be entered.  This is the recommended interaction mode.  The
*     alternative, interface, means that the pixel indices of pixels
*     and regions to be inspected are specified in response to prompts.

*     The application is composed of two parts.  First the preliminaries
*     obtains the mode, the input NDF and graphics devices.  In cursor
*     mode this usually amounts to a single prompt, and but two in
*     interface mode.  The second stage is a loop where the inspection
*     option is selected and performed.

*  Usage:
*     inspect in [mode] gdevice option [overlay]
*        { numbin=? hirep=? histogram=? hititle=?
*        { filename=?
*        { peind=?
*        { lbound=? ubound=?
*        { out=?
*        { slstart=? slend=? slice=? sltitle=?
*        { vaind=?
*        { xycont=? xyfile=? xytitle=?
*        option
*
*  ADAM Parameters:
*     ABSLAB  =  LITERAL (Read)
*        Label for the line-plot abscissa, in which NCAR fancy founts
*        may be embedded when FONT = "NCAR".  Note Slice and Histogram
*        have different defaults and these are stored separately.
*
*        For a slice plot the suggested default is the current value,
*        which is initially "Pixels".  If an error occurs obtaining the
*        label the default is "Pixels".
*
*        If axis information is present the suggested default for a
*        plot of an histogram is the NDF's axis label followed by the
*        units, in parentheses.  If an error occurs obtaining the label
*        or there is no axis information, the label takes its current
*        value, which initially is "Values".
*
*        For the first plot ABSLAB is defaulted to the suggested value
*        unless PLOTSTYLE is included on the command line, and
*        subsequently will only be obtained whenever PLOTSTYLE is TRUE.
*        []
*     FILENAME = FILENAME (Write)
*        Name of the text file to contain the Listing of image values.
*        The suggested default is inspect_list.lis.  This is only
*        required for the "List" option.
*     FONT = LITERAL (Read)
*        The fount to be used for the line graphics.  It can be either
*        "NCAR" for the NCAR fancy characters and "GKS" for the standard
*        GKS san-serif fount.   The former is intended for hardcopy
*        publication-quality plots, since it is relatively slow; the
*        latter is intended for normal interactive graphics requiring
*        rapid plotting, and it is clearer on small plots. The
*        suggested default is the current value. ["GKS"]
*
*        For the first plot FONT is defaulted to "GKS", and
*        subsequently will only be obtained whenever PLOTSTYLE is TRUE.
*        []
*     GDEVICE = DEVICE (Read)
*        The name of the graphics device for line plots produced by the
*        "Histogram" and "Slice" options.  The device should not be the
*        image display, but it may be the image-display overlay plane
*        used in the cursor-interaction mode, i.e. the same value as
*        parameter OVERLAY (though this is not advisable for
*        X-windows).  In the latter case plotting occurs in the same
*        picture as the overlay annotations, namely the current
*        picture.  If the existing plot on the base plane of the image
*        display has text, e.g. annotated axes, a mess can of confused
*        lines can appear.  To avoid this the current picture should be
*        made the DATA picture rather than the FRAME around it.  If
*        null, !, is given no line plots will be drawn unless the
*        "Device" option is selected.
*     HIREP = _LOGICAL (Read)
*        TRUE if the full Histogram is to be reported to you.  A large
*        number of bins may be required for the plot but need not be
*        listed in full.  This parameter provides a way of preventing
*        unwanted, tedious and long output.  The suggested default is
*        FALSE.  HIREP is only required for the "Histogram" option.
*     HISTOGRAM = NDF (Read)
*        Name of the NDF structure to save the Histogram in its data
*        array.  If null, !, is entered, the histogram NDF is not
*        created.  This parameter is only required for the "Histogram"
*        option.  The suggested default is !.
*     HITITLE = LITERAL (Read)
*        Title for the output NDF containing the Histogram.  For the
*        first histogram saved this defaults to
*        "KAPPA - Inspect_Histogram", and subsequently this becomes
*        the suggested default.   This parameter is only required for
*        the "Histogram" option. []
*     IN = NDF (Read)
*        NDF structure containing the 2-dimensional data array to be
*        inspected.
*     LBOUND( 2 ) = _INTEGER (Read)
*        Lower bounds in pixel indices of the Region.  The chosen pixel
*        must be different from that at the lower bound.  It is only
*        used in Interface mode with the "Region" option.
*     MAJTIC( 2 ) = _REAL (Read)
*        The parameter controlling the numbers of major tick marks
*        for the x and y axes in the slice or histogram plot.
*        (The number used is between MAJTIC+2 and 5*MAJTIC/2+4.).
*
*        By default, it is [4.,4.].  For the first plot MAJTIC is
*        defaulted, and subsequently will only be obtained whenever
*        PLOTSTYLE is TRUE.  []
*     MINTIC( 2 ) = _REAL (Read)
*        The number of minor tick marks between each major tick mark
*        for linear x and y axes in the slice or histogram plot.  A
*        negative value forces the graphics package to compute
*        appropriate values.  The number of minor tick marks per major
*        tick is fixed (8) for a logarithmic axis.
*
*        By default, it is [-1.,-1.].  For the first plot MINTIC is
*        defaulted, and subsequently will only be obtained whenever
*        PLOTSTYLE is TRUE.  []
*     MODE = LITERAL (Read)
*        The interaction mode.  The options are "Cursor" to use a
*        graphics cursor to select regions and pixels to inspect,
*        or "Interface" where prompted ADAM parameters are used to
*        define those parts of the image to inspect. [Current
*        interaction mode]
*     NUMBIN = _INTEGER (Read)
*        Number of bins needed for the Histogram.  The suggested default
*        is the current value, which is 100 initially.  A value is the
*        range 2--5000 is required.  This parameter is only required
*        for the "Histogram" option.
*     OPTION = LITERAL (Read)
*        Current inspection mode.  The options are:
*
*         "Device"     - This allows the selection and opening of a new
*                        line-plot graphics device, at the same time
*                        closing down the old one, whose last plot is
*                        stored in the graphics database.
*         "Exit"       - Exit the application.
*         "Histogram"  - This calculates the histogram of the current
*                        region. A summary and plot (if there is a
*                        graphics device available) of the histogram
*                        is produced.  The style of the plot may be
*                        adjusted via several parameters.  The full
*                        histogram may also be reported. The histogram
*                        data can be stored in a 1-d NDF.
*         "List"       - This produces a formatted and headed listing
*                        of the chosen region to a text file.
*         "Peep"       - Obtain a formatted listing of the 7x7 section
*                        of the array data, centred on a pixel
*                        specified using the cursor or via prompting.
*         "Region"     - To define the region of the array to be used
*                        by other options. If the image display
*                        is available, then the cursor is used to
*                        define the area, otherwise, the pixel bounds
*                        of the region come from the environment. Using
*                        the cursor, the functions of the choice-device
*                        buttons are drawn on the overlay.
*         "Save"       - Writes the current region to a new NDF,
*                        propagating all the components.
*         "Slice"      - Two points are defined via the cursor or from
*                        parameter prompting between which a slice (i.e.
*                        cross-section) is calculated.  Using the
*                        cursor, the functions of the trackerball or
*                        mouse buttons are drawn on the overlay. A plot
*                        is made to the graphics device if available.
*                        The style of the plot may be adjusted via
*                        several parameters.  The slice can be stored
*                        in a 1-d NDF.  The slice abscissa has units in
*                        true pixels (assuming pixels are square), thus
*                        a 45-degree slice would have a length root 2
*                        times its projected length in x or y.
*         "Statistics" - The key statistical parameters of the current
*                        region are determined and reported.
*         "Value"      - Obtain the value of a pixel at a point selected
*                        via the cursor or via prompting.
*         "XYcur"      - A list of the co-ordinates and values of pixels
*                        selected by the image-display cursor are
*                        written to a text file with Fortran carriage
*                        control. The functions of the trackerball or
*                        mouse buttons are displayed.  Optionally, an
*                        existing file in the same format as produced
*                        by XYcur can be appended to, for example, when
*                        a session has been interrupted.  These stored
*                        pixels are displayed on the overlay plane as
*                        if there had been no interruption.  XYcur
*                        requires cursor mode.
*
*        The suggested default is "Region".
*
*        If the option is specified on the command line a single
*        inspection may be undertaken, i.e.  there is no looping.  This
*        feature is intended for command procedures.
*     ORDLAB  =  LITERAL (Read)
*        Label for the line-plot ordinate, in which NCAR fancy founts
*        may be embedded.  Note Slice and Histogram have different
*        defaults and these are stored separately.
*
*        For an histogram plot the suggested default is the current
*        value, which is initially "Number".  If an error occurs
*        obtaining the label the default is "Number".
*
*        If axis information is present the suggested default for a
*        plot of a slice is the NDF's axis label followed by the
*        units, in parentheses.  If an error occurs obtaining the label
*        or there is no axis information, the label takes its current
*        value, which initially is "Data values".
*
*        For the first plot ORDLAB is defaulted to the suggested value
*        unless PLOTSTYLE is included on the command line, and
*        subsequently will only be obtained whenever PLOTSTYLE is TRUE.
*        []
*     OUT = NDF (Read)
*        Name of the NDF structure to contain the Saved Region.  This
*        is only used in the "Save" option.
*     OUTTIC = _LOGICAL (Read)
*        TRUE if the axis tick marks are to appear on the outside of
*        the axes instead of inside in the slice or histogram plots.
*        This eliminates intersections of ticks with the data locus.
*
*        By default, the tick marks are drawn inside the plot region.
*        For the first plot OUTTIC is defaulted, and subsequently will
*        only be obtained whenever PLOTSTYLE is TRUE.  []
*     OVERLAY = DEVICE (Read)
*        Name of the overlay-plane device used in the cursor interaction
*        mode.  It must have class IMAGE_OVERLAY or WINDOW_OVERLAY and
*        support colour.  It is ignored when MODE is not "Cursor".
*        [Current image-display-overlay device]
*     PEIND( 2 ) = _INTEGER (Read)
*        x-y pixel index of the pixel about which the Peep is required.
*        The values must lie within their respective bounds of the
*        input image.  The suggested default is the image centre.  It
*        is only used in Interface mode with the "Peep" option.
*     PLOTSTYLE = _LOGICAL (Read)
*        If TRUE, the plotting style of line plots is to be altered from
*        the default for the first plot, or the existing values for
*        subsequent graphs.  Initially, it is defaulted to FALSE, then
*        the suggested value is the current value.  Therefore to
*        override the plotting-style parameters on the first plot, new
*        values should be given on the command line, and along with the
*        PLOTSTYLE keyword for ABSLAB, ORDLAB and PLTITL.
*        Subsequently, the plotting style may be retained or modified
*        via prompts.  []
*     PLTITL = LITERAL (Read)
*        The title of a line plot, in which NCAR fancy founts may be
*        embedded.  Note Slice and Histogram have different defaults
*        and these are stored separately.  Both attempt to use the NDF's
*        title if present and no error occurs, otherwise the current
*        value becomes the suggested default.  For the histogram plot
*        this is initially "Histogram of current region" and for the
*        slice plot it is initially "Slice plot".
*
*        For the first plot PLTITL is defaulted to the suggested value
*        unless PLOTSTYLE is included on the command line, and
*        subsequently will only be obtained whenever PLOTSTYLE is TRUE.
*        []
*     SLEND( 2 ) = _INTEGER (Read)
*        The x-y pixel index defining the end of the Slice.  It
*        must lie within the bounds of the array and be distinct from
*        the start of the slice.  The suggested default is the upper
*        bound of the input NDF.  It is only used in Interface mode
*        with the "Slice" option.
*     SLICE = NDF (Read)
*        Name of the NDF structure to save the Slice in its data array.
*        If null, !, is entered, the slice NDF is not created.  It is
*        only required in the "Slice" option.  The suggested default is
*        !.
*     SLSTART( 2 ) = _INTEGER (Read)
*        The x-y pixel index defining the start of the Slice.  It must
*        lie within the bounds of the array.  The suggested default is
*        the lower bound of the input NDF.  It is only used in
*        Interface mode with the "Slice" option.
*     SLTITLE = LITERAL (Read)
*        Title for the Slice NDF.  Title for the Region NDF.  For the
*        first region saved this defaults to "KAPPA - Inspect_Slice",
*        and subsequently this becomes the suggested default.  It is
*        only required in the "Slice" option. []
*     THICK = _REAL (Read)
*        The thickness of the axes and annotations in the histogram and
*        line plots, where 1.0 is the normal thickness.  Currently,
*        this is only available on a few devices.  It must take a value
*        in the range 0.5--5.0.
*
*        By default the line thickness is 1.0.  For the first plot
*        THICK is defaulted to the suggested value, and subsequently
*        will only be obtained whenever PLOTSTYLE is TRUE.  []
*     TITLE = LITERAL (Read)
*        Title for the Region NDF.  For the first region saved this
*        defaults to "KAPPA - Inspect", and subsequently this becomes
*        the suggested default.  It is only used in the "Save" option.
*        []
*     UBOUND( 2 ) = _INTEGER (Read)
*        Upper bounds in pixel indices of the Region.  The chosen pixel
*        must be different from that at the lower bound.  It is only
*        used in Interface mode with the "Region" option.
*     VAIND( 2 ) = _INTEGER (Read)
*        x-y pixel index of pixel whose Value is required.  The values
*        must lie within their respective bounds of the input image.
*        The suggested default is the image centre.  It is only used in
*        Interface mode with the "Value" option.
*     XLOG = _LOGICAL (Read)
*        TRUE if the line-plot abscissa is to be logarithmic.  Note, for
*        Slice and Histogram options each has its own independent
*        switch.  It is unlikely that you would want to do this.  By
*        default, the abscissa is linear.  For the first plot XLOG is
*        defaulted, and subsequently will only be obtained whenever
*        PLOTSTYLE is TRUE.  []
*     YLOG = _LOGICAL (Read)
*        TRUE if the line-plot ordinate is to be logarithmic. Note, for
*        Slice and Histogram options each has its own independent
*        switch.  By default, the ordinate is linear.  For the first
*        plot YLOG is defaulted, and subsequently will only be obtained
*        whenever PLOTSTYLE is TRUE.  []
*     XYCONT = _LOGICAL (Read)
*        If TRUE, an existing file is appended to in the XYcur option.
*        The suggested default is FALSE.  It is only available in
*        cursor mode with the "XYcur" option.
*     XYFILE = FILENAME (Update)
*        Name of the text file to which pixel data are written by
*        XYcur option.  The suggested default is xylist.lis.  It is
*        only available in cursor mode with the "XYcur" option.
*     XYTITLE = LITERAL (Read)
*        Title for the text file in XYcur option.  For the first file
*        created this defaults to "# KAPPA - Inspect_XYcur", and
*        subsequently this becomes the suggested default. It is not
*        accessed if XYCONT is TRUE.   It is only available in cursor
*        mode with the "XYcur" option. []

*  Examples:
*     Notes:
*        Since INSPECT is an interacting, graphical and self-contained
*        monolith of applications it is not straightforward to give
*        command-line examples.  Generally, the best way to run INSPECT
*        is in cursor mode after having displayed an image.  The
*        following examples use the prompting mode.
*     inspect rulupi i canon_l sl slstart=[3,10] slend=[9,42] slice=!
*        Plots a slice from pixel (3,10) to (9,42) of the NDF called
*        rulupi to the CANON_L graphics device.
*     inspect rulupi i gdevice=x2w option=hi numbin=100 histogram=ru_hg
*        Calculates the histogram of the NDF called rulupi, reporting
*        a summary to you, and plots the histogram to the x2w device.
*        The histogram has one hundred bins and is stored in an NDF
*        called ru_hg.
*     inspect rulupi i ! option=hi numbin=100 hirep \
*        As above except no plot is made, no NDF is created, and
*        the full one hundred histogram values are reported.  In this
*        particular example the second parameter could equally well be
*        C for cursor mode since no co-ordinate information is
*        obtained.

*  Notes:
*     -  In cursor mode there must be an existing DATA picture for the
*     chosen image display stored in the graphics database.  Valid
*     cursor positions are bounded by the DATA picture.
*     -  On exit the input picture, if there was one, is made the
*     current picture on the overlay; and the last graphics plot is
*     stored in the database as a FRAME picture.  Also, if the "Device"
*     option is used a FRAME picture is stored for that device.
*     -  The Histogram NDF has an AXIS component whose the LABEL and
*     UNITS are those of the input NDF's data array; its centres are
*     in data value of the bin centre.  The NDF LABEL is "Number".
*     -  The Slice NDF has an AXIS component whose LABEL is "Pixel" and
*     centres are pixel co-ordinates from 0.5; its LABEL and UNITS are
*     propagated from the input NDF.
*     -  The current palette entries 1 to 4 associated with the OVERLAY
*     are used as follows in the cursor mode for IMAGE_OVERLAY devices.
*     A sample slice or region, and the associated button, are drawn
*     with palette index 1.  Similarly, index 3 is used to indicate an
*     accepted slice or region.  The exit button is drawn in the colour
*     of index 2.  Index 4 is used to draw the boxes representing the
*     mouse or trackerball buttons.  Use the PAL* commands to select
*     suitable complementary colours for the image's colour table,
*     especially for palette indices 1 and 3.
*
*     For WINDOW_OVERLAY devices, all the above are drawn with the
*     colour of palette index 1, but the various colours are replaced
*     by different dashed-line patterns.  Use PALENTRY to change the
*     colour of the lines.

*  Algorithm:
*     -  Get the mode of operation.
*     -  In cursor mode obtain the overlay device, start database
*     activity, activate SGS and get the zone associated with the input
*     picture.  Validate the class and attributes (colour) of the
*     device.  Check it has sufficient choices and a cursor.  Obtain
*     the last DATA picture and a zone for it.  Display information
*     about the DATA picture, and attempt to obtain a locator to the
*     NDF referenced in the picture.
*     -  Obtain the NDF via the command line, the graphics database or
*     prompting.  Get its bounds, dimensions and check the
*     dimensionality.  Map the data array.
*     -  Set the region to the whole array.
*     -  Obtain the bounds of valid cursor positions from DATA picture
*     in the database.
*     -  Obtain the line-graphics workstation, activating database
*     activity if not already done so, and obtain a zone identifier for
*     the current picture.
*     -  Initialise variables for the main loop.
*     -  Determine whether or not the option was specified on the
*     command line.  If it was set a flag to prevent looping.  Obtain
*     the option.  Clear the overlay frame zone when about to be reused
*     Clear the line-plot zone when not exiting or changing the
*     line-plot device.  If line graphics is required and slice or
*     histogram option has been selected, then see whether the plotting
*     style is to be changed.  If it is obtain the plot title, and
*     axis labels with the appropriate defaults, the number of tick
*     marks and their polarity, whether either or both axes are to be
*     logarithmic, and the line thickness.  Enter a case-like structure
*     to perform the required option.  Most are just simply a
*     subroutine call, but the following have additional method to
*     note:
*        o  Region---the flags to indicate that the histogram and
*        statistics have been calculated are both set to false.
*        o  Save---create an NDF section with the region bounds and
*        propagate the other components.
*        o  Device---if there is a current line-plot device select
*        the input zone and picture, then save a frame zone.  Reselect
*        the input picture.  In cursor mode, release the zone otherwise
*        deactivate the database.  Cancel the device and reselect the
*        overlay picture if in cursor mode.  Open the new device,
*        activating the database if necessary, and obtain a zone
*        identifier to the next picture.  If an error occurs (other than
*        abort) switch of line-plotting function and report this to the
*        user.  Again reselect the overlay picture if in cursor mode.
*        o  Statistics---Only compute them if not already derived for
*        the current region.  The standard statistics routines work on
*        the whole array, therefore create a new NDF section for the
*        region and map it.  Convert the vector positions returned by
*        the statistical subroutine to 2-d pixel indices.  Record that
*        the statistics have been calculated. Display the results.
*        o  Histogram---Use an NDF section of the region. Record that
*        the histogram is computed for the region.  Increment the plot
*        count.
*     -  Tidy the data locators and NDF system.
*     -  Tidy the line-graphics and overlay devices, resetting the
*     current picture to its input state.

*   Related Applications:
*      KAPPA: CURSOR, HISTOGRAM, ELPROF, LOOK, NDFCOPY, STATS; ESP:
*      SECTOR; Figaro: HIST, ICUR, IGCUR, ILIST, ISTAT, SLICE.

*  Implementation Status:
*     -  Only real data can be processed directly.  Other data types
*     will undergo a type conversion before processing occurs.
*     -  The routine correctly processes the AXIS, DATA, QUALITY,
*     VARIANCE, LABEL, TITLE, UNITS, WCS and HISTORY components of an NDF,
*     and propagates all extensions to the output Region NDF.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.  Bad pixels are are excluded from statistics and are
*     indicated in reports of data values by the word INVALID.  In the
*     slice plot they appear as gaps, and they do not affect the limits
*     of the ordinate.  The same applies to zero or negative data
*     values if the plot is to have a logarithmic ordinate.  Similarly,
*     for the histogram abscissa.
*
*  Implementation Deficiencies:
*     -  Only pixel indices may be used---data co-ordinates are not
*     supported, though the statistics option will report the position
*     of the extreme values in the data co-ordinates if axis data are
*     available.  Thus the current co-ordinate system global parameter
*     is ignored.
*     -  Uses GKS7.2 in which sample mode is not implemented, and using
*     the trackerball or mouse is clunkier than using 'rubber bands'.
*     There is no zooming for inspection of fine detail.
*     -  There are lots of missing options to tailor line plots. NCAR
*     does strange things if the picture is too small.  A method to
*     circumvent the problem is being investigated.

*  Authors:
*     MJC: Malcolm Currie  (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 June 11 (MJC)
*        Original NDF version, based on the pre-V0.8 version.
*     1991 July 5 (MJC):
*        Passed a section to the histogram routine.
*     1991 August 20 (MJC):
*        Added FONT parameter.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1992 April 21 (MJC):
*        Made to handle significant dimensions for user-defined
*        sections.
*     1992 June 16 (MJC):
*        Cursor mode made to work with WINDOW_OVERLAY class.  Since
*        these devices only have one colour, the normal coloured
*        lines appear as dashed lines.
*     1992 December 4 (MJC):
*        Leaves the palette of the overlay device unchanged.  Added
*        a note describing the roles of the palette colours used.
*        Inserted the "Save" option into the description of OPTION.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     2007 May 18 (MJC):
*        Used revised API for KPG1_STDSR.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE definitions
      INCLUDE 'DAT_PAR'        ! Data-system constants
      INCLUDE 'PRM_PAR'          ! Magic-value definitions
      INCLUDE 'SUBPAR_PAR'       ! SUBPAR_ constants
      INCLUDE 'PAR_ERR'          ! Parameter-system errors
      INCLUDE 'NDF_PAR'          ! NDF definitions
      INCLUDE 'NDF_ERR'          ! NDF error definitions

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER NDIM               ! Dimensionality of the data
      PARAMETER ( NDIM = 2 )

      INTEGER NGDOPT             ! Number of graph drawing options
      PARAMETER ( NGDOPT = 2 )

      INTEGER NLUTST             ! Minimum number of entries in colour
                                 ! set for a device to be classed as an
                                 ! image display
      PARAMETER ( NLUTST = 1 )

      INTEGER NPRICL             ! Number of primary colours
      PARAMETER ( NPRICL = 3 )

*  Local Variables:
      REAL
     :  CLIP( 1 ),               ! Clipping std. deviations
     :  MINTIC( 2 ),             ! Numbers of minor tick marks along x
                                 ! and y axes respectively for a plot
     :  MAJTIC( 2 ),             ! Parameters controlling the numbers of
                                 ! major tick marks along x and y axes
                                 ! respectively for a plot
     :  THICK,                   ! The line thickness (standard is 1.0)
     :  TICDEF( 2 ),             ! Suggested default axis-tick values
     :  XCEN,                    ! Column centre of image display
     :  XHIGH,                   ! Maximum x-value that cursor can take
                                 ! before being outside the array
                                 ! displayed
     :  XLOW,                    ! Minimum x-value that cursor can take
                                 ! before being outside the array
                                 ! displayed
     :  YCEN,                    ! Line centre of image display
     :  YHIGH,                   ! Maximum y-value that the cursor
                                 ! can take before being outside
                                 ! the array displayed.
     :  YLOW                     ! Minimum y-value that the cursor
                                 ! can take before being outside
                                 ! the array displayed

      LOGICAL                    ! True if:
     :  BAD,                     ! Bad pixels may be present in the
                                 ! array
     :  CMLOPT,                  ! The option was given on the command
                                 ! line
     :  CURAVL,                  ! Cursor is available
     :  CURSOR,                  ! Cursor mode is operative
     :  GOTLOC,                  ! Locator to the NDF has been obtained
     :  GOTNAM,                  ! A reference name of the NDF has been
                                 ! obtained
     :  GRAPHS,                  ! Graphics device is available
     :  HSTGRM,                  ! An histogram has already been
                                 ! calculated
     :  IMGDIS,                  ! Device is nominally an image display
     :  OUTTIC,                  ! Axis tick marks are to be placed
                                 ! outside the box instead of inside
     :  PLOTTD,                  ! There has been a plot on the current
                                 ! line graphics device
     :  PLTSTY,                  ! Plot options for graphics to be
                                 ! changed
     :  STACAL,                  ! Statistics have been calculated for
                                 ! the current region
     :  TCKCTR,                  ! The numbers of tick marks cannot be
                                 ! controlled
     :  VALID,                   ! NDF identifier is valid
     :  XLOG( NGDOPT ),          ! Plot abscissa will be logarithmic
     :  YLOG( NGDOPT )           ! Plot ordinate will be logarithmic

      CHARACTER*72
     :  ABSLAB( NGDOPT ),        ! Label for the abscissa of the plot
     :  OPLIST,                  ! List of available options
     :  ORDLAB( NGDOPT ),        ! Label for the ordinate of the plot
     :  PLTITL( NGDOPT )         ! Title of the plot

      CHARACTER
     :  COMP * 8,                ! Component to inspect
     :  DTYPE * ( NDF__SZFTP ),  ! Type of the image after processing
                                 ! (not used)
     :  FOUNT * 4,               ! Fount type
     :  IMGMES( 6 ) * 80,        ! Informational messages if device is
                                 ! an image display
     :  ITYPE * ( NDF__SZTYP ),  ! Processing type of the image
     :  MAXWCS * 255,            ! Formatted maximum WCS position
     :  MINWCS * 255,            ! Formatted minimum WCS position
     :  MODE * 10,               ! Interaction mode
     :  OPTDEF * 10,             ! Option default
     :  OPTION * 10,             ! Chosen option
     :  REFNAM * 256,            ! Reference name
     :  TERMES( 5 ) * 80         ! Informational messages if device is
                                 ! a terminal

      CHARACTER * ( DAT__SZLOC ) ! Locator to:
     :  LOCI                     ! Input NDF structure file

      DOUBLE PRECISION
     :  DMAX,                    ! Max. value of pixels in array
     :  DMAXC,                   ! Max. pixel value after clipping
     :  DMIN,                    ! Min. value of pixels in array
     :  DMINC,                   ! Min. pixel value after clipping
     :  MAXC( NDF__MXDIM ),      ! Co-ordinates of max. pixel
     :  MAXCC( NDF__MXDIM ),     ! Max. pixel co-ords (clipped)
     :  MEAN,                    ! Mean of pixels in array
     :  MEANC,                   ! Mean of pixels after clipping
     :  MINC( NDF__MXDIM ),      ! Co-ordinates of min. pixel
     :  MINCC( NDF__MXDIM ),     ! Min. pixel co-ords (clipped)
     :  STDEV,                   ! Standard devn. of pixels in array
     :  STDEVC,                  ! Std. devn. of pixels after clipping
     :  SUM,                     ! Sum of pixels in array
     :  SUMC                     ! Sum of pixels after clipping

      INTEGER
     :  ACTOPT,                  ! The state of the OPTION parameter
     :  DIMS( NDF__MXDIM ),      ! Array containing the dimensions of
                                 ! the of the array passed back from
                                 ! the environment
     :  EL,                      ! Number of pixels in the input NDF
     :  GLOOP,                   ! Number of graphics options chosen so
                                 ! far
     :  I,                       ! General variables
     :  IMAX( 1 ),               ! Vector index of max. pixel
     :  IMAXC( 1 ),              ! Vector index of max. clipped pixel
     :  IMIN( 1 ),               ! Vector index of min. pixel
     :  IMINC( 1 ),              ! Vector index of min. clipped pixel
     :  IPIXX,                   ! Maximum number of columns of pixels
                                 ! of the image-display overlay
     :  IPIXY,                   ! Maximum number of lines of pixels of
                                 ! the image-display overlay
     :  LBND( NDF__MXDIM )       ! Lower bounds of the input NDF

      INTEGER
     :  MAXP( NDF__MXDIM ),      ! Indices of maximum-valued pixel
     :  MAXPC( NDF__MXDIM ),     ! Maximum pixel indices after clipping
     :  MINP( NDF__MXDIM ),      ! Indices of minimum-valued pixel
     :  MINPC( NDF__MXDIM ),     ! Minimum pixel indices after clipping
     :  NCLIP,                   ! No. of clipping cycles for statistics
     :  NDF,                     ! Identifier for input NDF
     :  NDFO,                    ! Identifier for output NDF
     :  NDFR,                    ! Identifier for NDF section
     :  NDIMS,                   ! Actual number of dimensions of the
                                 ! NDF
     :  NGOOD,                   ! No. valid pixels in array
     :  NGOODC,                  ! No. valid pixels after clipping
     :  NIMGMS,                  ! Number of lines of image-display
                                 ! messages
     :  NINTS,                   ! Number of greyscale intensities
                                 ! available on the chosen device
     :  NTERMS                   ! Number of lines of terminal messages

      INTEGER
     :  PICIDG,                  ! Picture identifier for graphics plots
     :  PICIDI,                  ! Picture identifier for image area
     :  PICID,                   ! Input picture identifier
     :  PICIDP,                  ! Picture identifier for stored line
                                 ! plot
     :  PNTRI( 1 )               ! Pointer to the array data

      INTEGER
     :  REDIMS( NDIM ),          ! Dimensions of the of the region
     :  REL,                     ! Number of pixels in the region
     :  RELBND( NDF__MXDIM ),    ! Lower bounds of the region chosen
     :  REPNTR( 1 ),             ! Pointer to the region
     :  REUBND( NDF__MXDIM ),    ! Upper bounds of the region chosen
     :  RNUMB,                   ! Number of histogram bins
     :  SDIM( NDF__MXDIM ),      ! Significant dimensions of the NDF
     :  SLBND( NDIM ),           ! Significant lower bounds of the image
     :  SRLBND( NDIM ),          ! Significant lower bounds of the
                                 ! region
     :  SRUBND( NDIM ),          ! Significant upper bounds of the
                                 ! region
     :  SUBND( NDIM ),           ! Significant upper bounds of the image
     :  TSTAT                    ! Dummy status

      INTEGER
     :  UBND( NDF__MXDIM ),      ! Upper bounds of the input NDF
     :  WKIDOV,                  ! GKS workstation identifier for the
                                 ! overlay planes
     :  X1,                      ! First x co-ordinate from keyboard for
                                 ! REGION
     :  X2,                      ! Second x co-ordinate from keyboard
                                 ! for REGION
     :  XC1,                     ! First x co-ordinate from keyboard
                                 ! for SLICE
     :  XC2,                     ! Second x co-ordinate from keyboard
                                 ! for SLICE
     :  Y1,                      ! First y co-ordinate from keyboard
                                 ! for REGION
     :  Y2,                      ! Second y co-ordinate from keyboard
                                 ! for REGION
     :  YC1,                     ! First y co-ordinate from keyboard for
                                 ! SLICE
     :  YC2,                     ! Second y co-ordinate from keyboard
                                 ! for SLICE
     :  ZONEG,                   ! Zone identifier for graphics device
                                 ! chosen for plots
     :  ZONEO,                   ! SGS zone identifier - overlay plane
     :  ZONEOV                   ! SGS zone identifier - overlay plane

*.

*    Check the inherited status.

      IF ( STATUS .NE. SAI__OK ) RETURN

      CURAVL = .FALSE.
      GRAPHS = .FALSE.
      GOTLOC = .FALSE.
      GOTNAM = .FALSE.

*    Find which mode of operation is to be employed.

      CALL PAR_CHOIC( 'MODE', 'Cursor', 'Cursor,Interface', .TRUE.,
     :                MODE, STATUS )
      CURSOR = MODE .EQ. 'CURSOR'

*    If answer is illegal, then end.

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*    Obtain overlay device.
*    ======================

*    If answer is true, open image-display overlay.

      IF ( CURSOR ) THEN

*       Associate image display and start database activity

         CALL AGS_ASSOC( 'OVERLAY', 'UPDATE', ' ', PICID, ZONEOV,
     :                   STATUS )

*       If it was not possible to open the image display for use then
*       proceed using non-cursor mode, i.e. type in co-ordinates of
*       data to be inspected.  Tidy the graphics system.  Since a
*       parameter was obtained an abort may have been issued.

         IF ( STATUS .EQ. PAR__ABORT ) GOTO 999

*       Validate the class and attributes of the device.
*       ================================================

*       Check whether the chosen device is an 'image display overlay'
*       with a suitable minimum number of colour indices, colour and
*       a cursor.

         CALL KPG1_QVID( 'OVERLAY', 'SGS', 'IMAGE_OVERLAY,'/
     :                   /'WINDOW_OVERLAY', 'COLOUR,CURSOR', NLUTST,
     :                   STATUS )

*       Obtain the number of colour indices and the maximum display
*       surface.

         CALL KPG1_QIDAT( 'OVERLAY', 'SGS', NINTS, IPIXX, IPIXY,
     :                    STATUS )

         IF ( STATUS .NE. SAI__OK ) GOTO 990

*       Validate the number of choices and prepare the cursor.
*       ======================================================

*       Inquire GKS identifier of the image display overlay.

         CALL SGS_ICURW( WKIDOV )

*       Create some commentary describing how the functions displayed
*       in boxes on the screen relate to keyboard when the device
*       is a graphics terminal, or to mouse keys when the device is an
*       image display.

         TERMES( 1 ) = 'For certain options, boxes will appear...'
         TERMES( 2 ) = '   Press keyboard "1" or the space bar to '/
     :                 /'select the operation shown to the left.'
         TERMES( 3 ) = '   Press keyboard "2" to select the operation '/
     :                 /'shown in the middle box.'
         TERMES( 4 ) = '   Press keyboard "." to select the operation '/
     :                 /'shown in the right box.'
         TERMES( 5 ) = ' '
         NTERMS = 5

         IMGMES( 1 ) = 'For certain options, boxes will appear.  The '/
     :                 /'functions are controlled '
         IMGMES( 2 ) = 'by the mouse/trackerball buttons and '/
     :                 /' keyboard...'
         IMGMES( 3 ) = '   Press left button to select the operation '/
     :                 /'shown in the left box.'
         IMGMES( 4 ) = '   Press middle button/keyboard "2" to select '/
     :                 /'the operation shown in the middle box.'
         IMGMES( 5 ) = '   Press right button to select the operation '/
     :                 /'shown in the right box.'
         IMGMES( 6 ) = ' '
         NIMGMS = 6

*       Prepare the cursor. Specifically, does it exist and have the
*       correct attributes?

         CALL KPG1_PRCUR( 2, TERMES, NTERMS, IMGMES, NIMGMS, '12 .',
     :                    CURAVL, IMGDIS, STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 990

*       Is there no cursor or does it not have any choices?

         IF ( .NOT. CURAVL ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'INSPECT_NOCURSOR',
     :        'INSPECT: The workstation $OVERLAY does not have a '/
     :        /'suitable cursor.', STATUS )
            GOTO 990
         END IF
         CALL MSG_BLANK( STATUS )

*       There are a number of possibilities about where, or whether or
*       not plotting will occur:
*
*         Firstly, if the input picture is a FRAME, overlay annotations
*         may be drawn within the FRAME picture, provided there is a
*         DATA picture.
*
*         Secondly, if the input picture is a DATA picture, that
*         picture is used for both overlay annotations and interaction
*         with the data.
*
*         Thirdly, there must be a DATA picture otherwise the
*         application will exit.

         CALL KPG1_AGFND( 'DATA', PICIDI, STATUS )

*       Get a zone id for the data picture on the image-display device

         CALL AGI_SELP( PICIDI, STATUS )
         CALL AGS_NZONE( ZONEO, STATUS )

*       Display information about the current picture.
*       ==============================================

         CALL KPG1_AGATC( STATUS )

*       Obtain a reference to the NDF.
*       ==============================

         CALL KPG1_AGREF( PICIDI, 'READ', GOTNAM, REFNAM, STATUS )

*       See whether the reference is a name or locator.  The latter should be
*       phased out, but there may be some old databases and software
*       in circulation.

         CALL DAT_VALID( REFNAM, GOTLOC, STATUS )
         IF ( GOTLOC ) LOCI = REFNAM

*    End of the main cursor-mode check.

      END IF

*    Obtain the NDF.
*    ===============

*    Begin an NDF context.

      CALL NDF_BEGIN
      VALID = .FALSE.

*    Obtain the NDF.  If the name is given on the command line
*    it will be used.  If not, the database data reference is used,
*    if there is one.  Otherwise, the user is prompted.

      CALL KPG1_ASREF( 'IN', 'READ', GOTNAM, REFNAM, NDF, STATUS )

*    Check that the identifier is valid.  This is needed for the tidying
*    operations, where we want to know whether to annul the identifier
*    and status is bad.  The application could have failed to obtain
*    a data picture or locator, so there may not be a valid identifier.
*    The two operations of obtaining the locator and accessing the
*    NDF are intertwined so we can't have different closedown sequences.

      CALL NDF_VALID( NDF, VALID, STATUS )

*    This application supports only the real type directly.  Therefore
*    for the given type of the image find in which type it should be
*    processed.  It may still be possible to handle d.p. data provided
*    the dynamic range is not too small.

      CALL ERR_MARK
      CALL NDF_MTYPE( '_REAL', NDF, NDF, 'Data', ITYPE, DTYPE, STATUS )
      IF ( STATUS .EQ. NDF__TYPNI ) THEN
         CALL ERR_FLUSH( STATUS )
         CALL MSG_OUT( 'PRECLOSS', 'The loss of precision may not be '/
     :     /'serious so continuing to process in _REAL.', STATUS )
         ITYPE = '_REAL'
      END IF
      CALL ERR_RLSE

*    Find whether or not there are but two significant dimensions and
*    which ones they are.

      CALL KPG1_SGDIM( NDF, NDIM, SDIM, STATUS )

*    Obtain the bounds of the image.  These will be stored in the
*    graphics database once the cell-array is displayed.

      CALL NDF_BOUND( NDF, NDF__MXDIM, LBND, UBND, NDIMS, STATUS )

*    Must have a 2-d.  A bad status will be generated by NDF_BOUND
*    if there are greater than 2 significant dimensions.

      IF ( STATUS .EQ. NDF__XSDIM ) THEN
         CALL NDF_MSG( 'NDF', NDF )
         CALL ERR_REP( 'INSPECT_IVDIM',
     :     'INSPECT: NDF ^NDF is not two-dimensional.', STATUS )
         GOTO 980
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 980

*    Compute the dimensions.

      DO I = 1, NDIM
         SLBND( I ) = LBND( SDIM( I ) )
         SUBND( I ) = UBND( SDIM( I ) )
         DIMS( I ) = SUBND( I ) - SLBND( I ) + 1
      END DO

*    Only component supported is the data array.

      COMP = 'Data'

*    Map the input data array.

      CALL KPG1_MAP( NDF, COMP, ITYPE, 'READ', PNTRI, EL, STATUS )

      IF ( STATUS .NE. SAI__OK ) GOTO 980

*    Set area to the whole array.
*    ============================

*    Define the bounds of the region.  There are two pixel-index schemes
*    operating in this hybrid application for historical reasons.
*    Ideally, the X1, X2, Y1, Y2 should be removed.

      X1 = SLBND( 1 )
      Y1 = SLBND( 2 )
      X2 = SUBND( 1 )
      Y2 = SUBND( 2 )

*    Repeat for the slice bounds.

      XC1 = X1
      YC1 = Y1
      XC2 = X2
      YC2 = Y2

*    Also the region bounds and dimensions.

      DO I = 1, NDIMS
         IF ( I .EQ. SDIM( I ) ) THEN
            RELBND( I ) = SLBND( I )
            REUBND( I ) = SUBND( I )
         ELSE
            RELBND( I ) = LBND( I )
            REUBND( I ) = UBND( I )
         END IF
      END DO
      REDIMS( 1 ) = REUBND( SDIM( 1 ) ) - RELBND( SDIM( 1 ) ) + 1
      REDIMS( 2 ) = REUBND( SDIM( 2 ) ) - RELBND( SDIM( 2 ) ) + 1

*    Obtain the bounds of valid cursor positions.
*    ============================================

      IF ( CURSOR ) THEN

*       Obtain bounds of the data picture from the graphics database.

         CALL AGI_SELP( PICIDI, STATUS )
         CALL AGI_IWOCO( XLOW, XHIGH, YLOW, YHIGH, STATUS )

*       Find co-ordinates of the central pixel of the image.

         XCEN = 0.5 * ( XHIGH - XLOW )
         YCEN = 0.5 * ( YHIGH - YLOW )

*       Select input picture as the current picture in case the graphs
*       are to be plotted on the overlay already in use.

         CALL AGI_SELP( PICID, STATUS )
         CALL SGS_SELZ( ZONEOV, STATUS )
      END IF

*    Get the line-graphics workstation.
*    ==================================

*    Start a new error context.

      CALL ERR_MARK

*    Associate the graphics workstation and start database activity.

      CALL AGI_ASSOC( 'GDEVICE', 'WRITE', PICIDG, STATUS )
      IF ( .NOT. CURSOR ) CALL AGS_ACTIV( STATUS )

*    Get the SGS zone associated with the picture.

      CALL AGS_NZONE( ZONEG, STATUS )

*    If it was not possible to open the graphics device for use then
*    proceed using non-graphs mode.

      IF ( STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_RLSE
         GOTO 980

      ELSE IF ( STATUS .NE. SAI__OK ) THEN

         CALL ERR_REP( 'ERR_INSPECT_DVS',
     :     'INSPECT: Error in device specification.', STATUS )
         CALL ERR_FLUSH( STATUS )
         CALL MSG_OUT( 'NOGRAPHS', 'No plots will be made.', STATUS )

*    Otherwise, set the graphics flag to true.

      ELSE
         GRAPHS = .TRUE.
         PLTSTY = .FALSE.
         PLOTTD = .FALSE.
      END IF

*    Release the new error context.

      CALL ERR_RLSE

*    Select input picture as the current picture in case of an accident.

      IF ( CURSOR ) CALL AGI_SELP( PICID, STATUS )

*    Initialise variables for the main loop.
*    =======================================

*    Set a flag to indicate no histogram and no statistics calculated.

      HSTGRM = .FALSE.
      STACAL = .FALSE.

      RNUMB = 100

*    Number of line plots.

      GLOOP = 0

*    Plotting style.

      PLTITL( 1 ) = 'Histogram of current region'
      ABSLAB( 1 ) = 'Values'
      ORDLAB( 1 ) = 'Number'
      XLOG( 1 ) = .FALSE.
      YLOG( 1 ) = .FALSE.
      PLTITL( 2 ) = 'Slice plot'
      ABSLAB( 2 ) = 'Pixels'
      ORDLAB( 2 ) = 'Data values'
      XLOG( 2 ) = .FALSE.
      YLOG( 2 ) = .FALSE.

*    Specify the list of available options.

      IF ( CURSOR ) THEN
         OPLIST = 'Region,Device,Exit,Histogram,'/
     :            /'List,Peep,Save,Slice,Statistics,Value,XYcur'
      ELSE
         OPLIST = 'Region,Device,Exit,Histogram,'/
     :            /'List,Peep,Save,Slice,Statistics,Value'
      END IF
      OPTDEF = 'Region'

*    See whether the option is specified on the command line.
*    ========================================================

*    If the inspection option is specified on the command line then
*    there will be no looping.

      CALL LPG_STATE( 'OPTION', ACTOPT, STATUS )
      CMLOPT = ACTOPT .EQ. SUBPAR__ACTIVE

*    The main loop.
*    ==============

 100  CONTINUE

*    Obtain an option.
*    =================

      CALL PAR_CHOIC( 'OPTION', OPTDEF, OPLIST, .FALSE., OPTION,
     :                STATUS )

*    Exit if something has gone wrong.

      IF ( STATUS .NE. SAI__OK ) THEN
         OPTION = 'EX'
      END IF

      OPTDEF = OPTION

*    Cancel the parameter as we may loop.

      CALL PAR_CANCL( 'OPTION', STATUS )

*    Clear the graphics devices.
*    ===========================

*    Clear the overlay plane for those options that are going to reuse
*    it or for tidy up on exit.

      IF ( ( OPTION( 1:2 ) .EQ. 'RE' .OR. OPTION( 1:2 ) .EQ. 'EX' .OR.
     :       OPTION( 1:2 ) .EQ. 'PE' .OR. OPTION( 1:2 ) .EQ. 'VA' .OR.

     :       OPTION( 1:2 ) .EQ. 'SL' ) .AND. CURSOR ) THEN
         CALL SGS_SELZ( ZONEOV, STATUS )
         CALL SGS_CLRZ
         CALL SGS_FLUSH
      END IF

*    Clear the graphics device from last option unless exiting.

      IF ( OPTION( 1:2 ) .NE. 'EX' .AND. OPTION( 1:2 ) .NE. 'DE' .AND.
     :     GRAPHS ) THEN
         CALL SGS_SELZ( ZONEG, STATUS )
         CALL SGS_CLRZ
         CALL SGS_FLUSH
      END IF

*    Obtain the attributes of the slice or histogram line plot.
*    ==========================================================

      IF ( GRAPHS ) THEN

*       The graph plot styles may be changed via the command line
*       for the first plot only, telling the application that they
*       should be modified for subequent plots.

         IF ( OPTION(1:2) .EQ. 'HI' .OR. OPTION(1:2) .EQ. 'SL' ) THEN
            CALL PAR_GTD0L( 'PLOTSTYLE', .TRUE., .TRUE., PLTSTY,
     :                      STATUS )
            CALL PAR_CANCL( 'PLOTSTYLE', STATUS )

*          Get the plot annotations.

            IF ( PLTSTY ) THEN

*             Obtain the plot title and axis labels.
*             ======================================

               IF ( GLOOP .GT. 0 )
     :           CALL MSG_OUT( 'COMMENT', 'The original, standard '/
     :             /'title or axis label can be restored by entering '/
     :             /'the null character, !, in response to the prompt.',
     :             STATUS )

*             Annotations depend on the plot type.

               IF ( OPTION( 1:2 ) .EQ. 'HI' ) THEN

*                Obtain a title for the plot.

                  CALL KPG1_GNTIT( NDF, 'PLTITL',
     :                             'Histogram of current region',
     :                             PLTITL( 1 ), STATUS )

*                Obtain the abscissa axis label.

                  CALL KPG1_GNLBU( NDF, 'ABSLAB', COMP, ABSLAB( 1 ),
     :                             STATUS )

*                Obtain the ordinate axis label.

                  CALL PAR_DEF0C( 'ORDLAB', ORDLAB(1), STATUS )
                  CALL PAR_GET0C( 'ORDLAB', ORDLAB(1), STATUS )
                  IF ( STATUS .NE. SAI__OK .AND.
     :                 STATUS .NE. PAR__ABORT ) THEN
                     CALL ERR_ANNUL( STATUS )
                     ORDLAB(1) = 'Number'
                  ELSE IF ( STATUS .EQ. PAR__ABORT ) THEN
                     GOTO 980
                  END IF

               ELSE IF ( OPTION( 1:2 ) .EQ. 'SL' ) THEN

*                Obtain a title for the plot.

                  CALL KPG1_GNTIT( NDF, 'PLTITL', 'Slice plot',
     :                             PLTITL( 2 ), STATUS )

*                Obtain the abscissa axis label.

                  CALL PAR_DEF0C( 'ABSLAB', ABSLAB(2), STATUS )
                  CALL PAR_GET0C( 'ABSLAB', ABSLAB(2), STATUS )
                  IF ( STATUS .NE. SAI__OK .AND.
     :                 STATUS .NE. PAR__ABORT ) THEN
                     CALL ERR_ANNUL( STATUS )
                     ABSLAB(2) = 'Pixels'
                  END IF

*                Obtain the ordinate axis label.

                  CALL KPG1_GNLBU( NDF, 'ORDLAB', COMP, ORDLAB( 2 ),
     :                             STATUS )

                  IF ( STATUS .EQ. PAR__ABORT ) GOTO 980

*             End of check for option to get appropriate annotations

               END IF

*             Cancel parameters in the loop.

               CALL PAR_CANCL( 'PLTITL', STATUS )
               CALL PAR_CANCL( 'ABSLAB', STATUS )
               CALL PAR_CANCL( 'ORDLAB', STATUS )

*          End of check for plotting style to be changed.

            END IF

*          Get the plotting style options.
*          ===============================

*          Note that the defaults will be obtained for the first plot
*          from the interface file.

            IF ( PLTSTY .OR. GLOOP .EQ. 0 ) THEN

               IF ( OPTION( 1:2 ) .EQ. 'HI' ) THEN

*                Are the axes logarithmic?

                  CALL PAR_GTD0L( 'XLOG', .FALSE., .TRUE., XLOG( 1 ),
     :                            STATUS )
                  CALL PAR_GTD0L( 'YLOG', .FALSE., .TRUE., YLOG( 1 ),
     :                            STATUS )
                  TCKCTR = XLOG( 1 ) .AND. YLOG( 1 )
               ELSE
                  CALL PAR_GTD0L( 'XLOG', .FALSE., .TRUE., XLOG( 2 ),
     :                            STATUS )
                  CALL PAR_GTD0L( 'YLOG', .FALSE., .TRUE., YLOG( 2 ),
     :                            STATUS )
                  TCKCTR = XLOG( 2 ) .AND. YLOG( 2 )
               END IF

*             Tick numbers are not altered for a logarithmic axis

               IF ( .NOT. TCKCTR ) THEN

*                Get the number of minor ticks, assigning the dynamic
*                defaults.

                  TICDEF( 1 ) = -1.
                  TICDEF( 2 ) = -1.
                  CALL PAR_GDR1R( 'MINTIC', 2, TICDEF, -1., VAL__MAXR,
     :                            .FALSE., MINTIC, STATUS )

*                Get the parameter controlling the number of major
*                ticks per axis, assigning the dynamic defaults.

                  TICDEF( 1 ) = 4.
                  TICDEF( 2 ) = 4.
                  CALL PAR_GDR1R( 'MAJTIC', 2, TICDEF, -1., VAL__MAXR,
     :                            .FALSE., MAJTIC, STATUS )

                  CALL PAR_CANCL( 'MINTIC', STATUS )
                  CALL PAR_CANCL( 'MAJTIC', STATUS )
               END IF

*             Are the tick marks on the outside of the axes?

               CALL PAR_GTD0L( 'OUTTIC', .FALSE., .TRUE., OUTTIC,
     :                         STATUS )

*             Get the line thickness.

               CALL PAR_GDR0R( 'THICK', 1.0, 0.5, 5.0, .TRUE., THICK,
     :                         STATUS )

*             Get the fount.  Although NCAR is the default, either must
*             be selected to prevent persistence from earlier
*             invocations.

               CALL PAR_CHOIC( 'FONT', 'GKS', 'GKS,NCAR', .TRUE., FOUNT,
     :                         STATUS )
               IF ( FOUNT .EQ. 'GKS ' ) THEN
                  CALL AGPWRT( 0.0, 0.0, ' ', 0, 0, 0, -100 )
               ELSE IF ( FOUNT .EQ. 'NCAR' ) THEN
                  CALL AGPWRT( 0.0, 0.0, ' ', 0, 0, 0, 100 )
               END IF

*             Cancel the parameters in the loop.

               CALL PAR_CANCL( 'XLOG', STATUS )
               CALL PAR_CANCL( 'YLOG', STATUS )
               CALL PAR_CANCL( 'OUTTIC', STATUS )
               CALL PAR_CANCL( 'THICK', STATUS )
               CALL PAR_CANCL( 'FONT', STATUS )

*             Report context of an error

               IF ( STATUS .NE. SAI__OK ) THEN
                  IF ( STATUS .NE. PAR__ABORT ) THEN
                     CALL ERR_REP( 'ERR_INSPECT_PAR',
     :                 'INSPECT: Error obtaining a parameter defining '/
     :                 /'the plot', STATUS )
                  END IF
                  GOTO 980
               END IF

*          End of change-plotting-styles check.

            END IF
         END IF

*    End of graphics check.

      END IF


*    REGION option.
*    ==============

      IF ( OPTION( 1:2 ) .EQ. 'RE' ) THEN

         CALL INRE( CURSOR, ZONEOV, ZONEO, XCEN, YCEN, SLBND, DIMS,
     :              XLOW, XHIGH, YLOW, YHIGH, 'LBOUND', 'UBOUND',
     :              X1, X2, Y1, Y2, STATUS )

*       Define the bounds of the region for use by Histogram, Save, and
*       Statistics options.

         RELBND( SDIM( 1 ) ) = X1
         REUBND( SDIM( 1 ) ) = X2
         RELBND( SDIM( 2 ) ) = Y1
         REUBND( SDIM( 2 ) ) = Y2
         REDIMS( 1 ) = REUBND( SDIM( 1 ) ) - RELBND( SDIM( 1 ) ) + 1
         REDIMS( 2 ) = REUBND( SDIM( 2 ) ) - RELBND( SDIM( 2 ) ) + 1

*       A new region so the statistics and the histogram for it have
*       yet to be calculated.

         HSTGRM = .FALSE.
         STACAL = .FALSE.

*    SAVE option.
*    ============

      ELSE IF ( OPTION( 1:2 ) .EQ. 'SA' ) THEN

*       Start a new NDF context as we want to output the region
*       immediately.

         CALL NDF_BEGIN

*       Create a new section with the bounds of the region.

         CALL NDF_SECT( NDF, NDIMS, RELBND, REUBND, NDFR, STATUS )

*       Create a new NDF of the section, propagating all other
*       components.

         CALL LPG_PROP( NDFR, 'Data,Quality,Variance,Axis,Units,WCS',
     :                  'OUT', NDFO, STATUS )

*       Get the title for the NDF.

         CALL NDF_CINP( 'TITLE', NDFO, 'TITLE', STATUS )

*       Close down the NDF system.

         CALL NDF_END( STATUS )

*       Cancel the output parameters to enable a further region to be
*       saved.
         CALL PAR_CANCL( 'TITLE', STATUS )
         CALL PAR_CANCL( 'OUT', STATUS )

*    DEVICE option.
*    ==============

      ELSE IF ( OPTION( 1:2 ) .EQ. 'DE' ) THEN

*       If a graphics device has been selected...

         IF ( GRAPHS ) THEN
            CALL ERR_MARK

*          Store the last picture (if any) in the database --- not
*          ideal as plot may get overwritten later, and its world
*          co-ordinates are those provided by NCAR and are not in
*          useful user units like pixels or the number in a bin.
*          Note that the saved picture must lie within the current
*          line-graphics device's zone and picture for the save to
*          work.

            IF ( PLOTTD ) THEN
               CALL SGS_SELZ( ZONEG, STATUS )
               CALL AGI_SELP( PICIDG, STATUS )
               CALL AGS_SZONE( 'FRAME', 'KAPPA_INSPECT', PICIDP,
     :                         STATUS )

               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_REP( 'ERR_INSPECT_DBSF',
     :              'INSPECT: Error while storing the frame in the '/
     :              /'graphics database.', STATUS )
                  CALL ERR_FLUSH( STATUS )
               END IF

*             Finished with the new picture so release it.

               CALL AGI_ANNUL( PICIDP, STATUS )
            END IF

            CALL ERR_RLSE

*          The default line-graphics picture must be the same as on
*          input for the loop to work.

            CALL AGI_SELP( PICIDG, STATUS )

*          Close down SGS if there is no overlay using SGS, or reset
*          the current picture in case of an accident; and then close
*          the old device.  The line-graphics zone identifier cannot
*          be released (in order to avoid exhausting their quota)
*          without selecting another zone---you cannot release the
*          current zone.

            IF ( CURSOR ) THEN
               CALL SGS_SELZ( ZONEOV, STATUS )
               CALL SGS_RELZ( ZONEG )
            ELSE
               CALL AGS_DEACT( STATUS )
            END IF
            CALL AGI_CANCL( 'GDEVICE', STATUS )

            IF ( CURSOR ) THEN
               CALL AGI_SELP( PICID, STATUS )
            END IF
         END IF

*       Start a new error context so that an error can be flushed and
*       processing can continue without a graphics device.

         CALL ERR_MARK

*       Open the new device.

         CALL AGI_ASSOC( 'GDEVICE', 'WRITE', PICIDG, STATUS )

*       Fire up SGS if not already active.

         IF ( .NOT. CURSOR ) CALL AGS_ACTIV( STATUS )

*       Get SGS zone associated with the picture.

         CALL AGS_NZONE( ZONEG, STATUS )

*       If it was not possible to open the graphics device for use then
*       proceed using non-graphs mode.

         IF ( STATUS .EQ. PAR__ABORT ) THEN
            GRAPHS = .FALSE.
            CALL ERR_RLSE
            GOTO 980

         ELSE IF ( STATUS .NE. SAI__OK ) THEN

            CALL ERR_REP( 'ERR_INSPECT_DVS',
     :       'INSPECT: Error in device specification.', STATUS )
            CALL ERR_FLUSH( STATUS )
            CALL MSG_OUT( 'NOGRAPHS', 'No plots will be made.', STATUS )

            GRAPHS = .FALSE.
            CALL AGI_CANCL( 'GDEVICE', STATUS )

*       Otherwise, set the graphics flag to true.

         ELSE
            GRAPHS = .TRUE.
            PLOTTD = .FALSE.
         END IF

         CALL ERR_RLSE

*       Select input picture as the current picture in case of an
*       accident.

         IF ( CURSOR ) CALL AGI_SELP( PICID, STATUS )


*    VALUE option.
*    =============

      ELSE IF ( OPTION( 1:2 ) .EQ. 'VA' ) THEN

         CALL INVA( CURSOR, ZONEOV, ZONEO, XCEN, YCEN, SLBND, DIMS( 1 ),
     :              DIMS( 2 ), %VAL( PNTRI( 1 ) ), XLOW, XHIGH, YLOW,
     :              YHIGH, 'VAIND', STATUS )

*    PEEP option.
*    ============

      ELSE IF ( OPTION( 1:2 ) .EQ. 'PE' ) THEN

*       Report the data values about a selected pixel.

         CALL INPE( CURSOR, ZONEOV, ZONEO, XCEN, YCEN, SLBND, DIMS( 1 ),
     :              DIMS( 2 ), %VAL( PNTRI( 1 ) ), XLOW, XHIGH, YLOW,
     :              YHIGH, 'PEIND', STATUS )

*    LIST option.
*    ============

      ELSE IF ( OPTION( 1:2 ) .EQ. 'LI' ) THEN

         CALL ERR_MARK

*       Write the listing file.

         CALL IMLST( %VAL( PNTRI( 1 ) ), SLBND, DIMS( 1 ), DIMS( 2 ),
     :               X1, Y1, X2, Y2, 'FILENAME', 1.0, STATUS )

*       Cancel the parameter as we are in a loop.

         CALL PAR_CANCL( 'FILENAME', STATUS )

*       Display and clear errors.

         IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
         CALL ERR_RLSE

*    STATS option.
*    =============

      ELSE IF ( OPTION( 1:2 ) .EQ. 'ST' ) THEN

*       If the user is just redisplaying the statistics on the same
*       region there is no need to re-calculate them.

         IF ( .NOT. STACAL ) THEN

*          Start a new NDF context because the standard statistics
*          routine expects to be given the whole array to process.

            CALL NDF_BEGIN

*          Create a new section.

            CALL NDF_SECT( NDF, NDIMS, RELBND, REUBND, NDFR, STATUS )

*          Map the component in that section.

            CALL KPG1_MAP( NDFR, COMP, '_REAL', 'READ', REPNTR, REL,
     :                    STATUS )

*          Compute the statistics of the current region.

            BAD = .TRUE.
            NCLIP = 0
            CALL KPG1_STATR( BAD, REL, %VAL( REPNTR( 1 ) ), NCLIP, CLIP,
     :                       NGOOD, IMIN( 1 ), DMIN, IMAX( 1 ), DMAX,
     :                       SUM, MEAN, STDEV, NGOODC, IMINC( 1 ),
     :                       DMINC, IMAXC( 1 ), DMAXC, SUMC, MEANC,
     :                       STDEVC, STATUS )

*          If available, convert the minimum and maximum pixel locations
*          into N-dimensional indices and then into co-ordinate values.

            IF ( NGOOD .NE. 0 ) THEN
               CALL KPG1_VEC2N( 1, IMIN, NDIMS, RELBND, REUBND, MINPC,
     :                          STATUS )
               CALL KPG1_VEC2N( 1, IMAX, NDIMS, RELBND, REUBND, MAXPC,
     :                          STATUS )
               CALL KPG1_PX2AX( NDIMS, MINPC, NDFR, MINCC, STATUS )
               CALL KPG1_PX2AX( NDIMS, MAXPC, NDFR, MAXCC, STATUS )

               DO  I = 1, NDIMS
                  MINP( I ) = MINPC( I )
                  MAXP( I ) = MAXPC( I )
                  MINC( I ) = MINCC( I )
                  MAXC( I ) = MAXCC( I )
               END DO
            END IF

*          Record that the statistics have been calculated for the
*          region.

            STACAL = .TRUE.

*          End the NDF context as we want to free the identifier and
*          unmap the section.

            CALL NDF_END( STATUS )
         END IF

*       Display the statistics to single precision.

         CALL KPG1_STDSR( NDIMS, REL, NGOOD, DMIN, MINP, MINC, DMAX,
     :                    MAXP, MAXC, SUM, MEAN, STDEV, VAL__BADD,
     :                    VAL__BADD, 1, VAL__BADR, VAL__BADD,
     :                    MAXWCS, MINWCS, STATUS )

*    HIST option.
*    ============

      ELSE IF ( OPTION( 1:2 ) .EQ. 'HI' ) THEN

*       Start a new NDF context because the standard statistics
*       routine expects to be given the whole array to process.

         CALL NDF_BEGIN

*       Create a new section.

         CALL NDF_SECT( NDF, NDIMS, RELBND, REUBND, NDFR, STATUS )

*       Map the component in that section.

         CALL KPG1_MAP( NDFR, COMP, '_REAL', 'READ', REPNTR, REL,
     :                 STATUS )

*       Compute the histogram and plot it.

         DO I = 1, NDIM
            SRLBND( I ) = RELBND( SDIM( I ) )
            SRUBND( I ) = REUBND( SDIM( I ) )
         END DO

         CALL INHI( GRAPHS, ZONEG, NDFR, SRLBND, REDIMS( 1 ),
     :              REDIMS( 2 ), %VAL( REPNTR( 1 ) ), 'NUMBIN',
     :              'HISTOGRAM', 'HITITLE', 'HIREP', 1.E36, PLTITL( 1 ),
     :              ABSLAB( 1 ), ORDLAB( 1 ), MINTIC, MAJTIC, XLOG( 1 ),
     :              YLOG( 1 ), OUTTIC, THICK, RNUMB, HSTGRM, STATUS )

*       End the NDF context as we want to free the identifier and
*       unmap the section.

         CALL NDF_END( STATUS )

         IF ( STATUS .NE. SAI__OK ) GOTO 980

         GLOOP = GLOOP + 1
         IF ( GRAPHS ) PLOTTD = .TRUE.

*    SLICE option.
*    =============

      ELSE IF ( OPTION( 1:2 ) .EQ. 'SL' ) THEN

*       Define, compute and plot a slice throught the array.

         CALL INSL( CURSOR, GRAPHS, ZONEOV, ZONEO, ZONEG, XCEN, YCEN,
     :              NDF, SLBND, DIMS( 1 ), DIMS( 2 ),
     :              %VAL( PNTRI( 1 ) ), XLOW, XHIGH, YLOW, YHIGH,
     :              'SLSTART', 'SLEND', 'SLICE', 'SLTITLE', 1.E36,
     :              PLTITL( 2 ), ABSLAB( 2 ), ORDLAB( 2 ), MINTIC,
     :              MAJTIC, XLOG( 2 ), YLOG( 2 ), OUTTIC, THICK,
     :              XC1, XC2, YC1, YC2, STATUS )

         GLOOP = GLOOP + 1
         IF ( GRAPHS ) PLOTTD = .TRUE.

*    XYCUR option.
*    =============

      ELSE IF ( OPTION( 1:2 ) .EQ. 'XY' ) THEN

*       Obtain x-y positions and write them to an text file.  Cancel
*       the parameter for the filename in order to loop.

         CALL INXY( ZONEOV, ZONEO, XCEN, YCEN, SLBND, DIMS( 1 ),
     :              DIMS( 2 ), %VAL( PNTRI( 1 ) ), XLOW, XHIGH,
     :              YLOW, YHIGH, 'XYCONT', 'XYFILE', 'XYTITLE', STATUS )

         CALL PAR_CANCL( 'XYFILE', STATUS )

*    EXIT option.
*    ============

      ELSE IF ( OPTION( 1:2 ) .EQ. 'EX' ) THEN

         GOTO 980

*    End of option conditional.

      END IF

*    Return for another option.
*    ==========================

*    Will not loop if the option was given on the command line.

      IF ( CMLOPT ) THEN

*       Clear the overlay plane.

         IF ( CURSOR ) THEN
            CALL SGS_SELZ( ZONEOV, STATUS )
            CALL SGS_CLRZ
            CALL SGS_FLUSH
         END IF

*    Loop as the option was not given on the command line.

      ELSE
         GOTO 100
      END IF


*    Tidy up.
*    ========

 980  CONTINUE

*    Tidy the data.
*    ==============

*    Unmap and annul the data explicitly as NDF_ will be confused if
*    the locator by reference is annulled first.

      IF ( VALID ) CALL NDF_ANNUL( NDF, STATUS )

*    Tidy up the input data structure.

      IF ( CURSOR ) THEN
         IF ( GOTLOC ) CALL REF_ANNUL( LOCI, STATUS )
         CALL DAT_VALID( LOCI, GOTLOC, STATUS )
         IF ( GOTLOC ) CALL DAT_ANNUL( LOCI, STATUS )
      END IF

*    End the NDF context.

      CALL NDF_END( STATUS )

 990  CONTINUE

*    Tidy the line graphics.
*    =======================

      IF ( GRAPHS ) THEN

*       Want to be sure that the select picture works.

         TSTAT = SAI__OK
         IF ( STATUS .NE. SAI__OK ) THEN
            TSTAT = STATUS
            STATUS = SAI__OK
         ELSE
            TSTAT = SAI__OK
         END IF

*       Only save a picture if there has been one drawn.

         IF ( GLOOP .GT. 0 ) THEN
            CALL SGS_SELZ( ZONEG, STATUS )

*          Store last picture (if any) in the database --- not ideal as
*          its world co-ordinates are those provided by NCAR and are not
*          in useful user units like pixels or the number in a bin.

            CALL AGI_SELP( PICIDG, STATUS )
            CALL AGS_SZONE( 'FRAME', 'KAPPA_INSPECT', PICIDP, STATUS )

            IF ( STATUS .NE. PAR__ABORT .AND. STATUS .NE. PAR__NULL
     :          .AND. STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'ERR_INSPECT_DBSF',
     :           'INSPECT: Error while storing the line plot frame '/
     :           /'in the graphics database.', STATUS )
            END IF
         END IF

*       Reset the current picture to the input picture.

         CALL AGI_SELP( PICIDG, STATUS )

*       Restore the bad status if present.

         STATUS = TSTAT

         IF ( .NOT. CURSOR ) CALL AGS_DEACT( STATUS )
         CALL AGI_ANNUL( PICIDG, STATUS )
      END IF

*    Tidy Overlay device.
*    ====================

*    AGI closedown including re-instating the input picture as current.

      IF ( CURSOR ) CALL AGS_DEASS( 'OVERLAY', .FALSE., STATUS )

 999  CONTINUE

      END
