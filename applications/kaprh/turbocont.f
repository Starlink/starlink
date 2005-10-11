      SUBROUTINE TURBOCONT( STATUS )
*+
*  Name:
*     TURBOCONT

*  Purpose:
*     Contours a 2-d NDF quickly.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation
*     CALL TURBOCONT( STATUS )

*  Description:
*     This application draws a contour plot of a 2-dimensional NDF on
*     the current graphics device via an efficient algorithm.  The
*     image may be part or whole of the data array, but also the
*     variance or quality can be shown.  The plot is situated within
*     the current graphics-database picture.

*     The contour plot resides within optional, annotated and enumerated
*     axes.  An optional, but recommended, key may be drawn to the
*     right of the contour plot.  It reports the NDF's units if there
*     are any, and only contour heights actually plotted are included.
*     There are seven methods for selecting contours.

*  Usage:
*     turbocont ndf [comp] mode ncont [key] [device]
*        { firstcnt=? stepcnt=?
*        { heights=?
*        { percentiles=?
*        mode

*  ADAM Parameters:
*     ABSLAB  =  LITERAL (Read)
*        Label for the plot abscissa, in which NCAR fancy founts may be
*        embedded when FONT = "NCAR".  If axis information is present
*        the suggested default is the NDF's axis label followed by the
*        units, in parentheses.  If an error occurs obtaining the label
*        the suggested default is "X". []
*     ANNOTA = _LOGICAL (Read)
*        If TRUE the contour lines will be annotated with a contour
*        number corresponding to the key entry.  It is ignored and there
*        are no annotations when KEY = FALSE.  [FALSE]
*     AXES = _LOGICAL (Read)
*        TRUE if labelled and annotated axes are to be drawn around the
*        contour plot.  The annotations are either the data
*        co-ordinates from the NDF axis components, provided these are
*        present and linear and COSYS = "Data"; otherwise pixel
*        co-ordinates are used.  [TRUE]
*     BORDER = _LOGICAL (Read)
*        BORDER is TRUE if a box is to be drawn about the contour plot.
*        This is only accessed when there are no axes required. [TRUE]
*     CLEAR = _LOGICAL (Read)
*        TRUE if the graphics device is to be cleared before display
*        of the array. [TRUE]
*     COMP = LITERAL (Read)
*        The NDF component to be contoured.  It may be "Data",
*        "Quality", "Variance", or "Error" (where "Error" is the
*        alternative to "Variance" and causes the square root of the
*        variance values to be taken before plotting contours).  If
*        "Quality" is specified, then the quality values are treated as
*        numerical values (in the range 0 to 255). ["Data"]
*     CONCOL = LITERAL (Read)
*        The colour of the contour lines on devices that support colour.
*        The options are described below.
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
*        colour.  This parameter will be ignored if PENROT = TRUE.
*        [The current value, but equals "1" (the foreground colour) if
*        there is no current value.]
*     COSYS = LITERAL (Read)
*        The co-ordinate system to be used.  This can be either "World"
*        or "Data".  "World" makes pixel co-ordinates to appear on axes.
*        If COSYS = "Data" the NDF's axis information is used to
*        annotate axes.  [Current co-ordinate system]
*     DASHED = _REAL (Read)
*        The height below which the contours will be drawn with dashed
*        lines.  A null value (!) means all contours are drawn with
*        solid lines.  This facility is only available when ANNOTA =
*        FALSE. [!]
*     DEVICE = DEVICE (Read)
*        The plotting device. [Current image-display device]
*     FILL = _LOGICAL (Read)
*        The contour plot normally has square pixels, in other words
*        a length along each axis corresponds to the same number of
*        pixels.  However, for images with markedly different
*        dimensions this default behaviour may not be suitable or give
*        the clearest plot.  When FILL is TRUE, the square-pixel
*        constraint is relaxed and the contour plot is the largest
*        possible within the current picture.  When FILL is FALSE, the
*        pixels are square.  The suggested default is the current value.
*        [FALSE]
*     FIRSTCNT = _REAL (Read)
*        Height of the first contour (Linear and Magnitude modes).
*        The suggested value is the current value.
*     FONT = LITERAL (Read)
*        The fount to be used for the line graphics.  It can be either
*        "NCAR" for the NCAR fancy characters and "GKS" for the standard
*        GKS san-serif fount.  The former is intended for hardcopy
*        publication-quality plots, since it is relatively slow; the
*        latter is intended for normal interactive graphics requiring
*        rapid plotting, and it is clearer on small plots.  The
*        suggested default is the current value. ["GKS"]
*     HEIGHTS() = _REAL (Read)
*        Contour levels (Free mode).  The suggested default is the
*        current value.
*     KEY = _LOGICAL (Read)
*        When KEY is TRUE, a key of the contour level versus pixel
*        value is to be produced. [TRUE]
*     LABELFREQ = _INTEGER (Read)
*        The frequency with which contour levels are annotated.  1
*        means every level will be labelled.  This may be excessive in
*        plots where the contours are closely packed.  This parameter
*        is ignored unless contour annotation has been selected.  It
*        must be between one and the number of contour heights. [1]
*     MAJTIC( 2 ) = _REAL (Read)
*        The parameter controlling the numbers of major tick marks
*        for the x and y axes.  (Number used is between MAJTIC+2 and
*        5*MAJTIC/2+4.) [3.,3.]
*     MAXRES = _LOGICAL (Read)
*        If TRUE the contours are interpolated to the resolution of the
*        plotting device, i.e. provides sub-pixel resolution, otherwise
*        straight-line segments at pixel resolution are drawn.  The
*        latter does not give smooth contours, but this makes the
*        processing much faster.  The former draws smoother contours to
*        the resolution of the graphics workstation, but they still
*        have vertices.  If you require smooth well-rounded contours try
*        the slower CONTOUR. [FALSE]
*     MINTIC( 2 ) = _REAL (Read)
*        The number of minor tick marks between each major tick mark
*        for the x and y axes.  A negative value forces the graphics
*        package to compute appropriate values. [-1.,-1.]
*     MODE = LITERAL (Read)
*        The method used to select the contour levels.  The options are
*        described below.
*
*          "Area"        - The contours enclose areas of the array for
*                          which the equivalent radius increases by
*                          equal increments.  You specify the number of
*                          levels.
*          "Automatic"   - The contour levels are equally spaced between
*                          the maximum and minimum pixel values in the
*                          array.  You supply the number of contour
*                          levels.
*          "Equalised"   - You define the number of equally spaced
*                          percentiles.
*          "Free"        - You specify a series of contour values
*                          explicitly.
*          "Linear"      - You define the number of contours, the start
*                          contour level and linear step between
*                          contours.
*          "Magnitude"   - You define the number of contours, the start
*                          contour level and step between contours.  The
*                          step size is in magnitudes so the nth contour
*                          is dex(-0.4*(n-1)*step) times the start
*                          contour level.
*          "Percentiles" - You specify a series of percentiles.
*
*        The suggested default is the current value, which is initially
*        "Free".
*     NCONT = _INTEGER (Read)
*        The number of contours required (all modes except Free and
*        Percentiles).  It must be between 1 and 50.  If the number is
*        large, the plot may be cluttered and take longer to produce.
*        6, the initial suggested default, gives reasonable results.
*        The current value becomes the suggested default.
*     NDF = NDF (Read)
*        NDF structure containing the 2-dimensional image to be
*        contoured.
*     NOISY = _LOGICAL (Read)
*        If TRUE the contour lines will alternately be annotated with
*        a contour number corresponding to the key entry, but at
*        twice the frequency.  It is ignored unless annotated contours
*        have been selected. [FALSE]
*     ORDLAB  =  LITERAL (Read)
*        Label for the plot ordinate, in which NCAR fancy founts may be
*        embedded when FONT = "NCAR".  If axis information is present
*        the suggested default is the NDF's axis label followed by the
*        units, in parentheses.  If an error occurs obtaining the label
*        the suggested default is "Y". []
*     OUTTIC = _LOGICAL (Read)
*        TRUE if the axis tick marks are to appear on the outside of
*        the axes instead of inside.  By default, the tick marks are
*        drawn outside the contouring region to eliminate
*        intersections of ticks with the contours. [TRUE]
*     PENROT = _LOGICAL (Read)
*        If TRUE, the plotting pens are cycled through the contours to
*        aid identification of the contour heights.  It is ignored
*        when annotation is selected. [FALSE]
*     PERCENTILES() = _REAL (Read)
*        Contour levels given as percentiles.  The values must lie
*        between 0.0 and 100.0. (Percentiles mode).  The suggested
*        default is the current value.
*     PLTITL = LITERAL (Read)
*        The title of the plot.  Up to about 40 characters can be
*        accommodated, and NCAR fancy founts may be embedded when FONT =
*        "NCAR".  If an error occurs obtaining the title, it is
*        defaulted to "Contour plot".  [The NDF title]
*     PXSIZE = _REAL (Read)
*        The length (x axis) of the plot in metres.  There is an upper
*        limit given by the x size of the current picture. [Maximum that
*        can fit in the current picture whilst preserving square pixels]
*     PYSIZE = _REAL (Read)
*        The length (y axis) of the plot in metres.  There is an upper
*        limit given by the y size of the current picture. [Maximum that
*        can fit in the current picture whilst preserving square pixels]
*     RESOLUTION = _REAL (Read)
*        The resolution factor.  The actual plotting resolution is this
*        times the x and y theoretical resolutions in world
*        co-ordinates.  In GKS, whether or not a given "lamp" is
*        illuminated or pen position is marked with ink cannot be
*        determined, so a factor of unity is too small for the most
*        efficient processing.  It must lie between 2.0 and 10.0. [2.0]
*     STEPCNT = _REAL (Read)
*        Separation between contour levels, linear for Linear mode
*        and in magnitudes for Magnitude mode.  The suggested value is
*        the current value.
*     THICK = _REAL (Read)
*        The thickness of the lines and NCAR-fount characters in the
*        plot, where 1.0 is the normal thickness.  Currently, this is
*        only available on a few devices.  It must take a value in the
*        range 0.5--10.0.  [1.0]

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Examples:
*     turbocont myfile d \
*        Contours the data array in the NDF called myfile on the current
*        graphics device.  All other settings are defaulted, so for
*        example the current method for determining heights is used, and
*        a key is plotted.
*     turbocont taurus1(100:199,150:269,4) \
*        Contours a 2-dimensional portion of current array component in
*        the NDF cube called taurus1 on the current graphics device.
*        The portion extends from pixel (100,150,4) to pixel
*        (199,269,4).  All other settings are defaulted, so for example
*        coarse contours are drawn, using the current mode for
*        determining heights, and a key is plotted.
*     turbocont ndf=ngc6872 mode=au ncont=5 device=ps_l concol=white
*        Contours the data array in the NDF called ngc6872 on the
*        ps_l graphics device.  Five equally spaced contours between
*        the maximum and minimum data values are drawn in white.  The
*        NDF's title adorns the plot.  A key is plotted.
*     turbocont ngc6872 mode=au ncont=5 annota labelfreq=2 cosys=w
*     device=ps_l concol=white
*        As above.  In addition the contours are annotated at
*        alternate heights.  The axes are annotated with pixel
*        co-ordinates.
*     turbocont ngc6872 mode=li firstcnt=10 stepcnt=2 ncont=4 noaxes
*        Contours the data array in the NDF called ngc6872 on the
*        current graphics device.  Four contours at heights 10, 12, 14,
*        and 16 are drawn.  A key is plotted, but no axes surround the
*        contour plot.
*     turbocont ss443 mode=pe percentiles=[80,90,95,98,99,99.9] annota
*        Contours the data array in the NDF called ss443 on the
*        current graphics device.  Annotated contours at heights
*        corresponding to the 80, 90, 95, 98, 99, and 99.9 percentiles
*        are drawn.  A key is plotted.
*     turbocont mode=eq ncont=5 dashed=0 pencol=red ndf=skyflux
*        Contours the data array in the NDF called skyflux on the
*        current graphics device.  Contours at heights corresponding to
*        the 10, 30, 50, 70 and 90 percentiles are drawn in red.  Those
*        contours whose values are negative will appear as dashed
*        lines.  A key is plotted.
*     turbocont comp=d nokey penrot \
*        Contours the portion of the data array in the current NDF on
*        the current graphics device using the current method for height
*        selection.  The NDF's title adorns the plot.  No key is drawn.
*        The appearance of the contours cycles every third contour.
*     turbocont comp=v mode=fr heights=[10,20,40,80] title=Variance
*        Contours the variance array in the current NDF on the
*        current graphics device.  Contours at 10, 20, 40 and 80 are
*        drawn.  "Variance" is the title of the plot.

*  Notes:
*     -  The application stores a number of pictures in the graphics
*     database in the following order: a FRAME of the specified size
*     containing the title, annotated axes, contours and key; a DATA
*     picture which is stored with world co-ordinates in units of data
*     pixels; and a KEY picture to store the key if present.  The DATA
*     picture also may have double-precision data co-ordinates derived
*     from the NDF axis components provided these are linear and
*     different from pixel co-ordinates; the data co-ordinates are
*     stored via a linear transformation.  The NDF associated with the
*     plot is stored by reference with the DATA picture.  On exit the
*     current database picture for the chosen device reverts to the
*     input picture.
*     -  There are some options for setting the characteristics of the
*     contour lines.  By default, solid lines are drawn with the same
*     colour as the axes and key, namely the foreground colour.  The
*     colour will depend on the graphics device chosen, but it is often
*     black for printers or white for terminals.  The alternatives to
*     override this default behaviour are listed below.
*
*        1. Set a colour for all contours using parameter CONCOL.
*        2. Request dashed contours below some threshold given by
*           parameter DASHED and solid lines for other heights.  All
*           contours have either the foreground colour or that
*           prescribed by parameter CONCOL.
*        3. Cycle the pens modulo 3 for each contour height actually
*           plotted by setting PENROT = TRUE.  The characteristics of
*           the second and third line styles will depend on the chosen
*           graphics device.  An image display or pen plotter will draw
*           coloured lines using palette entries 1 to 3; whereas a
*           window overlay, or monochrome laser printer or terminal
*           will draw a variety of dashed or thicker lines.
*        4. Combine options 2 and 3.  However, palette colours 1 to 3
*           will always be used and CONCOL ignored.  The contours below
*           the threshold continue the cycle through the three colours.
*           There may be some confusion on devices that already use
*           dashed lines, so this is only suitable for devices
*           supporting at least three colours simultaneously.
*        5. Annotate the contours using the number of the contour height
*           corresponding to the key entries rather than the values
*           themselves.  (Set parameter ANNOTA = TRUE.)  The frequency
*           of labelling may be defined (LABELFREQ).  The key option
*           must be chosen (KEY = TRUE) in conjunction with annotated
*           contours.  Annotation is not recommended should the data
*           array have a large number of bad pixels, or if the contours
*           are closely packed.  There is an additional parameter
*           (NOISY) to select double annotations for short or noisy
*           contours in option 2.
*
*     Annotation takes precedence over pen rotation, which in turn
*     overrides colour control through CONCOL.
*  Algorithm:
*     -  Find which component to display, obtain an identifier to the
*     NDF and check that the component is present.  Find the data type
*     for processing.  Get the NDF bounds and inquire the bad-pixel
*     flag.  Get the units.  Obtain the sub-image bounds.  Create and
*     the section.  Determine which co-ordinate system to use.
*     -  Get the display device and open the database for it with the
*     appropriate device status.  Get the current SGS zone.
*     -  Save the frame picture in the database.
*     -  Obtain the plotting style, title, axis labels and plot size.
*     -  Create a new zone of the requested size.
*     -  Define frame, image and key zones.
*     -  Select the contour heights and sort them in ascending order.
*     -  Get the contour-style parameters.
*     -  Obtain the data co-ordinate transformation and axis bounds.
*     -  Draw axes in a smaller than default NCAR zone or a box in the
*     full image zone.
*     -  Find the position of the top of the key.
*     -  Obtain lots of work arrays, redefine the co-ordinates to that
*     of the image and plot the contours in the NCAR grid region.  Clear
*     workspace.  Store the image picture in the database, plus the data
*     reference, and the transformation for data co-ordinates.
*     -  Draw the key, recording it in the database and report the
*     heights.
*     -  Tidy graphics and NDF.

*  Related Applications:
*     KAPPA: CONTOUR; Figaro: ICONT; SPECDRE: SPECCONT.

*  Implementation Status:
*     -  Only real data can be processed directly.  Other non-complex
*     numeric data types will undergo a type conversion before the
*     contour plot is drawn.
*     -  Bad pixels and automatic quality masking are supported.

*  Implementation Deficiencies:
*     Lots of missing options to tailor the plot.  NCAR does strange
*     things if the picture is too small.  A method to circumvent the
*     problem is being investigated.

*  Authors:
*     MJC: Malcolm J. Currie  STARLINK
*     PWD: Peter W. Draper (JAC, Durham University)
*     {enter_new_authors_here}

*  History:
*     1989 Aug 25 (MJC):
*        Original based on CONTOUR.
*     1989 Oct 17 (MJC):
*        Revised the world co-ordinates of the contour picture to the
*        Starlink standard instead of pixel number, and corrected the
*        application name in database pictures and error messages.
*     1989 Oct 27 (MJC):
*        Improved positioning of the top of the key.
*     1989 Dec 21 (MJC):
*        Workspace managed by AIF_TEMP.
*     1990 Jan 9  (MJC):
*        Corrected SGS status.
*     1990 Mar 30 (MJC):
*        Added axis annotation parameters and reports the chosen
*        contour heights to the user.
*     1990 May 22 (MJC):
*        Corrected aspect ratio of the contour region to give the same
*        scale factor in x and y.
*     1990 Aug 29 (MJC):
*        NDF version supporting variance and quality, bad-pixel checks,
*        units and title to annotate the plot.
*     1991 February 7 (MJC):
*        Added NDF reference into the database, AGI context control and
*        removed fuzzy-picture fudge.
*     1991 April 9 (MJC):
*        Added data co-ordinate transformation and optional axes.
*        Re-organised world co-ordinates so that CNTTUR no longer
*        handles the full array, merely the section.
*     1991 May 1 (MJC):
*        Renamed IN parameter to NDF for consistency.
*     1991 July 31 (MJC):
*        No longer redefines colours of SGS pens to predefined state if
*        workstation has dynamic colour representation, now there is
*        palette control.
*     1991 August 20 (MJC):
*        Added FONT parameter.
*     1991 October 22 (MJC):
*        Fixed a bug so that when annotating and using data co-ordinates
*        the contours now appear at the correct locations.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1992 April 4 (MJC):
*        Made to handle significant dimensions for user-defined
*        sections.
*     1992 November 30 (MJC):
*        Does not use non-monotonic axis centres.
*     1992 December 17 (MJC):
*        Added the FILL option.
*     1993 January 19 (MJC):
*        Added the BORDER option.
*     1995 October 19 (MJC):
*        Supports Error component.
*     1996 August 29 (MJC):
*        Increased tessellation cell to 256 pixels square.
*     1997 May 12 (MJC):
*        Added percentiles and equalised options for the MODE.  New
*        parameter PERCENTILES.  Increased tessellation cell to 512
*        pixels square.
*     1997 May 19 (MJC):
*        Added CONCOL and DASHED parameters, and further examples.
*        Improved efficiency by using PSX to obtain workspace.
*        Increased the maximum thickness from 5 to 10.  Rewrote the
*        Notes on contour colour and line style.
*     2005 October 11 (PWD):
*        Use CNF_PVAL in %VAL calls.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! global SSE definitions
      INCLUDE 'DAT_PAR'        ! Data-system constants
      INCLUDE 'PRM_PAR'        ! Magic-value definitions
      INCLUDE 'NDF_PAR'        ! NDF_ public constants
      INCLUDE 'NDF_ERR'        ! NDF_ error definitions
      INCLUDE 'PAR_ERR'        ! Parameter-system error definitions
      INCLUDE 'GKS_PAR'        ! GKS constants (e.g. GSET)
      INCLUDE 'CNF_PAR'        ! CNF functions.

*  Status:
      INTEGER STATUS

*  Local Constants:
      REAL
     :  ANCLP1, ANCLP2,        ! Fraction of the frame zone in which the
     :  ANCLP3, ANCLP4         ! image will appear when there are axes.
                               ! Note aspect ratio is preserved.
      PARAMETER ( ANCLP1 = 0.19, ANCLP2 = 0.95,
     :            ANCLP3 = 0.15, ANCLP4 = 0.91 )

      INTEGER

     :  CELDIM,                ! Tessellation cell dimension
     :  CUNITS,                ! Maximum number of characters in units
                               ! that will be visible in the plot

     :  NDIM,                  ! Dimensionality of input array
     :  MXCONT,                ! Maximum number of contour heights
     :  MXSBPX                 ! Maximum number of sub-pixels
      PARAMETER( CELDIM = 512 )
      PARAMETER( CUNITS = 14 )
      PARAMETER( NDIM = 2 )    ! Default to 2-dimensional
      PARAMETER( MXCONT = 50 )
      PARAMETER( MXSBPX = 200 )

      REAL
     :  ASPKEY                 ! Fractional aspect ratio (of that of
                               ! the input array) for the key
      PARAMETER ( ASPKEY = 0.2 )

      INTEGER CONPEN             ! SGS pen number used to plot unrotated
      PARAMETER ( CONPEN = 2 )   ! contour

*  Local Variables:
      LOGICAL                  ! True if :
     :  ANNOTA,                ! (Alternate) contour lines are annotated
     :  AXES,                  ! Annotated axes are to be drawn
     :  BAD,                   ! Bad pixels may be present in the image
     :  BORDER,                ! A border about the plot is to be drawn
     :  CLEAR,                 ! the graphics device is to be cleared
                               ! before display of the array
     :  CNTUSD( MXCONT ),      ! A contour has been plotted at the
                               ! corresponding height in CNTLEV
     :  COLOUR,                ! Workstation supports colour
     :  DACOOR,                ! Data co-ordinates are to be stored
                               ! in the database
     :  DATACO,                ! Axes are given in data co-ordinates
     :  DATEMP,                ! Either of the axes is non-monotonic
     :  DEVCAN,                ! Graphics-device parameter is to be
                               ! cancelled
     :  DPAXIS                 ! Axis centres are double precision

      LOGICAL                  ! True if :
     :  FILL,                  ! Plotting area is filled
     :  KEY,                   ! A key of the contour heights
                               ! is to be produced
     :  MAXRES,                ! Sub-pixel contouring should be
                               ! attempted, plotting detail to the
                               ! x,y resolutions
     :  MONOTO,                ! Axis is monotonic
     :  NOISY,                 ! The contours are noisy and double the
                               ! number of annotations is provided
     :  OUTTIC,                ! Axis tick marks are to be placed
                               ! outside the box instead of inside
     :  PENROT,                ! The graphics pens are to be cycled
     :  SCLINE,                ! Non-standard thickness lines needed
     :  THERE                  ! NDF array component is present

      DOUBLE PRECISION
     :  DXLBND( NDIM ),        ! Axis lower bounds
     :  DXUBND( NDIM ),        ! Axis upper bounds
     :  DOFSET( NDIM ),        ! Offsets in the world-to-data
                               ! co-ordinate transformations
     :  DSCALE( NDIM )         ! Scale factors in the world-to-data
                               ! co-ordinate transformations

      REAL
     :  ANCLIP( 4 ),           ! Fraction of the frame zone in which the
                               ! image will appear when there are axes.
                               ! Can't give array directly as parameter
     :  AREA( MXCONT ),        ! Work array for storing areas in CNTSEL
     :  ASP,                   ! Aspect ratio of the input array
     :  AXLBND( NDIM ),        ! Axis lower bounds
     :  AXUBND( NDIM ),        ! Axis upper bounds
     :  CLWIDT,                ! Width of lines of current SGS pen
     :  CNTLEV( MXCONT ),      ! Contour heights
     :  DX,                    ! x resolution of the plotting in world
                               ! co-ordinates
     :  DY,                    ! y resolution of the plotting in world
                               ! co-ordinates
     :  GRID( 4 ),             ! Current AUTOGRAPH grid offsets
     :  KEYOFF,                ! Fractional y position of the top of the
                               ! key
     :  MINTIC( 2 ),           ! Numbers of minor tick marks along x and
                               ! y axes respectively
     :  MAJTIC( 2 )            ! Parameters controlling the numbers of
                               ! major tick marks along x and y axes
                               ! respectively
      REAL
     :  OFFSET( NDIM ),        ! Offsets in the world-to-data
                               ! co-ordinate transformations
     :  PERCNT( MXCONT ),      ! Contour heights as percentiles
                               ! (actually fractions)
     :  RESFAC,                ! Minimum length of a line segment in
                               ! units of the device resolution
     :  SCALE( NDIM ),         ! Scale factors in the world-to-data
                               ! co-ordinate transformations
     :  THICK,                 ! The line thickness (standard is 1.0)
     :  THRESH,                ! Threshold for dashed contours
     :  TICDEF( 2 ),           ! Suggested default axis-tick values
     :  TK,                    ! Co-ordinate transformed x (not used)
     :  X1, X2, Y1, Y2,        ! Zone size in world co-ordinates
     :  XM, YM,                ! Zone size in metres
     :  XRES, YRES             ! x,y resolution of the display device in
                               ! world co-ordinates

      CHARACTER*72
     :  ABSLAB,                ! Label for the abscissa of the plot
     :  ATYPE * ( NDF__SZTYP ),! Processing type of the axis centres
     :  COMP * 8,              ! Component to be displayed
     :  COSYS * 5,             ! Co-ordinate system
     :  DTYPE * ( NDF__SZFTP ),! Type of the image after processing (not
                               ! used)
     :  FOUNT * 4,             ! Fount type
     :  ITYPE * ( NDF__SZTYP ),! Processing type of the image
     :  ORDLAB,                ! Label for the ordinate of the plot
     :  MCOMP * 8,             ! Component to be mapped
     :  MODE * 20,             ! Contour mode
     :  PLTITL,                ! Title of the plot
     :  UNITS * ( CUNITS+5 )   ! Units of the data

      INTEGER
     :  AEL,                   ! Number of elements in a mapped axis
     :  AXPNTR( 1 ),           ! Pointer to a mapped axis
     :  CCOLI,                 ! Original colour index of pen used for
                               ! plotting the lines
     :  CFPNTR,                ! Pointer to the cell-flag work array
     :  CLNTYP,                ! Line type for current lines SGS pen
     :  CONCI,                 ! Colour index required for contours
     :  DIMS( NDIM ),          ! Dimensions of input array
     :  EL,                    ! Number of elements in the input array
     :  GSTAT,                 ! GKS status
     :  I,                     ! General variables
     :  IERR,                  ! GKS error indicator
     :  IWKID,                 ! GKS workstation identifier
     :  LABFRQ,                ! The frequency with which contours are
                               ! to be labelled
     :  LASF( 13 ),            ! GKS list of aspect source flags
     :  LBND( NDF__MXDIM )     ! Lower bounds of the image

      INTEGER
     :  LLPNTR,                ! Pointer to the linked-list work array
     :  MAXPEN,                ! Maximum pen number for undashed pens
     :  NCU,                   ! Number of characters in the units
     :  NCONT,                 ! Number of contour heights
     :  NDF,                   ! NDF identifier
     :  NDFC,                  ! Identifier for input section
     :  NDFS,                  ! NDF identifier of the section
     :  NDIMS,                 ! Total number of NDF dimensions
     :  NLOCUS,                ! Size of the x,y locus work arrays
     :  NWORK                  ! Number of work arrays obtained
                               ! successfully

      INTEGER
     :  PICID,                 !
     :  PICID1,                ! Graphics' database identifier on input
     :  PICID2,                ! Graphics' database identifier for
                               ! the frame (contour + key) picture
     :  PICID3,                ! Graphics' database identifier for
                               ! the displayed contour picture
     :  PICID4,                ! Graphics' database identifier for
                               ! the picture of the key
     :  PNTRI( 1 ),            ! Pointer to image data
     :  SCRPNT,                ! Pointer to the work array
     :  SDIM( NDF__MXDIM ),    ! Significant dimensions of the NDF
     :  SLBND( NDIM ),         ! Significant lower bounds of the image
     :  SLIST( MXCONT ),       ! Work array
     :  SUBND( NDIM ),         ! Significant upper bounds of the image
     :  UBND( NDF__MXDIM )     ! Upper bounds of the image

      INTEGER
     :  WPNTR( 5 ),            ! Pointers to workspace
     :  XPNTR,                 ! Pointer to the x locus work array
     :  YPNTR,                 ! Pointer to the y locus work array
     :  ZONE1,                 ! Initial SGS zone identifier
     :  ZONE2,                 ! SGS zone identifier
     :  ZONEK,                 ! SGS zone identifier for the key
     :  ZONEIF,                ! SGS zone identifier for the image area
     :  ZONEI                  ! SGS zone identifier for the image area

*.

*    Check the inherited status.

      IF ( STATUS .NE. SAI__OK ) GOTO 999

      DEVCAN = .FALSE.

*    Obtain and map the array to be contoured.
*    =========================================

*    Find which component to contour, converting 'ERROR' into
*    'VARIANCE'.

      CALL PAR_CHOIC( 'COMP', 'Data', 'Data,Quality,Error,Variance',
     :                .FALSE., COMP, STATUS )
      MCOMP = COMP
      IF ( COMP .EQ. 'ERROR' ) COMP = 'VARIANCE'

*    Begin an NDF context.

      CALL NDF_BEGIN

*    Obtain the identifier of the NDF to be contoured.

      CALL LPG_ASSOC( 'NDF', 'READ', NDF, STATUS )

*    There must be a data array, but for other components check that
*    requested component is present.

      IF ( COMP .NE. 'DATA' ) THEN
         CALL NDF_STATE( NDF, COMP, THERE, STATUS )

*       The component is not present or not defined.

         IF ( .NOT. THERE ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'COMP', COMP )
            CALL ERR_REP( 'TURBOCONT_NOCOMP',
     :        'TURBOCONT: ^COMP component is not defined.', STATUS )
            GO TO 980
         END IF
      END IF

*    This application can only process real components directly.
*    Therefore for the given type of the image find in which type
*    it should be processed.  Currently, it is obvious since only
*    one type is supported, but this acts as a placeholder when this
*    is no longer true.  It may still be possible to handle d.p.
*    data provided the dynamic range is not too small.

      CALL ERR_MARK
      CALL NDF_MTYPE( '_REAL', NDF, NDF, COMP, ITYPE, DTYPE, STATUS )
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
*    graphics database once the image is contoured.

      CALL NDF_BOUND( NDF, NDF__MXDIM, LBND, UBND, NDIMS, STATUS )

*    Set upper insignificant bounds to one.  We have to make a section
*    so that trailing insignificant bounds may be shifted when the
*    user has specified the whole NDF.  This cannot be done for the base
*    NDF.

      CALL NDF_SECT( NDF, NDIMS, LBND, UBND, NDFC, STATUS )
      CALL KPG1_SECSH( NDFC, SDIM( NDIM ), STATUS )

*    Must have a 2-d.  A bad status will be generated by NDF_BOUND
*    if there are greater than 2 significant dimensions.

      IF ( STATUS .EQ. NDF__XSDIM ) THEN
         CALL ERR_REP( 'TURBOCONT_IVDIM',
     :     'TURBOCONT: Input NDF must be two-dimensional.', STATUS )
         GOTO 980
      END IF

*    Compute the dimensions and the significant bounds.

      SLBND( 1 ) = LBND( SDIM( 1 ) )
      SLBND( 2 ) = LBND( SDIM( 2 ) )
      SUBND( 1 ) = UBND( SDIM( 1 ) )
      SUBND( 2 ) = UBND( SDIM( 2 ) )
      DIMS( 1 ) = SUBND( 1 ) - SLBND( 1 ) + 1
      DIMS( 2 ) = SUBND( 2 ) - SLBND( 2 ) + 1

*    Check whether or not bad pixels may be present.

      CALL NDF_BAD( NDF, COMP, .FALSE., BAD, STATUS )

*    Obtain the units if present.  A null units field does not
*    cause a blank line to appear in the key.  Quality has no units.

      CALL KPG1_DAUNI( NDF, MCOMP, UNITS, NCU, STATUS )

      IF ( STATUS .NE. SAI__OK ) GOTO 980

*    Create a section from the input (full-sized) NDF section that
*    has any insignificant bounds shifted; otherwise it is possible
*    for the array to be filled with bad values, simply because the
*    chosen section lies outside the bounds of the original section.

      CALL NDF_SECT( NDFC, SDIM( NDIM ), LBND, UBND, NDFS, STATUS )

*    Map the image.

      CALL KPG1_MAP( NDFS, MCOMP, ITYPE, 'READ', PNTRI, EL, STATUS )

*    Get the type of co-ordinates to place on axes.
*    ==============================================

*    Is there an axis system?

      CALL NDF_STATE( NDF, 'Axis', DACOOR, STATUS )

*    Obtain the desired co-ordinate system.

      CALL PAR_CHOIC( 'COSYS', 'Data', 'Data,World', .FALSE., COSYS,
     :                STATUS )

*    Find the effective co-ordinate system.

      DATACO = DACOOR .AND. COSYS .EQ. 'DATA'

*    Find the implementation type of the axis structure.
*    ===================================================

*    Integer needs d.p. because it potentially has ten significant
*    digits.  Record the fact for compactness and efficiency.

      IF ( DATACO ) THEN
         CALL KPG1_AXTYP( NDF, 'Centre', ATYPE, STATUS )
         DPAXIS = ATYPE .EQ. '_DOUBLE'
      END IF

*    Test axes to determine monotonicity.
*    ====================================

      DATEMP = .TRUE.
      IF ( DACOOR ) THEN

*       Obtain the axes calling the appropriate routine depending on the
*       axis implementation type.

         IF ( DPAXIS ) THEN
            DO  I = 1, NDIM

*             Map the axis.

               CALL NDF_AMAP( NDF, 'Centre', SDIM( I ), '_DOUBLE',
     :                        'READ', AXPNTR, AEL, STATUS )

*             Are all the axes monotonic?  Start a new error context so
*             that the error reports concerning a non-monotonic axis
*             may be annulled.  Instead we issue a warning message
*             so that the application can continue by using world
*             co-ordinates.

               MONOTO = .TRUE.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  CALL ERR_MARK
                  CALL KPG1_MONOD( .TRUE., AEL,
     :                             %VAL( CNF_PVAL( AXPNTR( 1 ) ) ),
     :                             MONOTO, STATUS )
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL ERR_ANNUL( STATUS )
                     MONOTO = .FALSE.
                  END IF
                  CALL ERR_RLSE
               END IF

*             Issue the warning.  Change the emphasis depending on
*             whether the co-ordinate system is DATA.

               IF ( .NOT. MONOTO ) THEN
                  CALL MSG_SETI( 'IAXIS', SDIM( I ) )
                  IF ( DATACO ) THEN
                     CALL MSG_OUT( 'TURBOCONT_NOTMONO1',
     :                'TURBOCONT: Axis ^IAXIS is not monotonic.  Will '/
     :                /'use world co-ordinates instead.', STATUS )
                  ELSE
                     CALL MSG_OUT( 'TURBOCONT_NOTMONO2',
     :                'TURBOCONT: Axis ^IAXIS is not monotonic.  Will '/
     :                /'not record axis bounds in the graphics '/
     :                /'database.', STATUS )
                  END IF

*                Record the fact.
                  DATEMP = .FALSE.
               END IF

*             Unmap the axis since we have finished with it.

               CALL NDF_AUNMP( NDF, 'Centre', SDIM( I ), STATUS )
            END DO
         ELSE
            DO  I = 1, NDIM

*             Map the axis centres.

               CALL NDF_AMAP( NDF, 'Centre', SDIM( I ), '_REAL',
     :                        'READ', AXPNTR, AEL, STATUS )

*             Are all the axes monotonic?  Start a new error context so
*             that the error reports concerning a non-monotonic axis
*             may be annulled.  Instead we issue a warning message
*             so that the application can continue by using world
*             co-ordinates.

               MONOTO = .TRUE.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  CALL ERR_MARK
                  CALL KPG1_MONOR( .TRUE., AEL,
     :                             %VAL( CNF_PVAL( AXPNTR( 1 ) ) ),
     :                             MONOTO, STATUS )
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL ERR_ANNUL( STATUS )
                     MONOTO = .FALSE.
                  END IF
                  CALL ERR_RLSE
               END IF

*             Issue the warning.  Change the emphasis depending on
*             whether the co-ordinate system is DATA.

               IF ( .NOT. MONOTO ) THEN
                  CALL MSG_SETI( 'IAXIS', SDIM( I ) )
                  IF ( DATACO ) THEN
                     CALL MSG_OUT( 'TURBOCONT_NOTMONO1',
     :                'TURBOCONT: Axis ^IAXIS is not monotonic.  Will '/
     :                /'use world co-ordinates instead.', STATUS )
                  ELSE
                     CALL MSG_OUT( 'TURBOCONT_NOTMONO2',
     :                'TURBOCONT: Axis ^IAXIS is not monotonic.  Will '/
     :                /'not record axis bounds in the graphics '/
     :                /'database.', STATUS )
                  END IF

*                Record the fact.
                  DATEMP = .FALSE.
               END IF

*             Unmap the axis since we have finished with it.

               CALL NDF_AUNMP( NDF, 'Centre', SDIM( I ), STATUS )
            END DO
         END IF
      END IF

*    Reset the co-ordinate system and axis flags when either of the axes
*    is non-monotonic.

      IF ( .NOT. DATEMP ) THEN
         DATACO = .FALSE.
         DACOOR = .FALSE.
      END IF

*    Start the graphics system.
*    ==========================

*    See whether picture is to be refreshed or not.

      CALL PAR_GTD0L( 'CLEAR', .TRUE., .TRUE., CLEAR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 980

*    Open the database for the device.

      IF ( CLEAR ) THEN
         CALL AGS_ASSOC( 'DEVICE', 'WRITE', ' ', PICID1, ZONE1, STATUS )
      ELSE
         CALL AGS_ASSOC( 'DEVICE', 'UPDATE', ' ', PICID1, ZONE1,
     :                   STATUS )
      END IF

*    Set up the workstation polyline representations.
*    ================================================

*    If the device supports colour we want solid lines to be drawn.

      CALL KPG1_QCOL( COLOUR, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         DEVCAN = .TRUE.
         GOTO 960
      END IF

*    Find whether a key is required.
*    ===============================

      CALL PAR_GTD0L( 'KEY', .TRUE., .TRUE., KEY, STATUS )

*    Are annotated axes required?
*    ============================

      CALL PAR_GTD0L( 'AXES', .TRUE., .TRUE., AXES, STATUS )

*    Is a border required?
*    =====================

      IF ( AXES ) THEN
         BORDER = .TRUE.
      ELSE
         CALL PAR_GTD0L( 'BORDER', .TRUE., .TRUE., BORDER, STATUS )
      END IF

*    Create and store the frame picture.
*    ===================================

      CALL KPG1_FRPIC( 'PXSIZE', 'PYSIZE', 'KAPPA_TURBOCONT', .FALSE.,
     :                 ZONE2, PICID2, STATUS )

*    Reset input picture as current in case of an accident.

      CALL AGI_SELP( PICID1, STATUS )

*    Obtain a title for the plot.
*    ============================

      CALL KPG1_GNTIT( NDF, 'PLTITL', 'Contour Plot', PLTITL,
     :                 STATUS )

      IF ( AXES ) THEN

*       Obtain axis labels.
*       ===================

         IF ( DATACO ) THEN

*          A null value causes the default to be chosen, namely, 'X' for
*          the abscissa...

*          Get the abscissa and ordinate labels suggesting the value in
*          the NDF axis structure, if present, as the default.

            CALL KPG1_GAXLB( NDF, SDIM( 1 ), 'ABSLAB', 'X', ABSLAB,
     :                       STATUS )
            CALL KPG1_GAXLB( NDF, SDIM( 2 ), 'ORDLAB', 'Y', ORDLAB,
     :                       STATUS )

         ELSE

*          Get the abscissa and ordinate labels without consulting the
*          NDF's axis structure to preevent the wrong label being
*          associated with the world co-ordinates.  The suggested
*          defaults are 'X' and 'Y'.

            CALL PAR_DEF0C( 'ABSLAB', 'X', STATUS )
            CALL PAR_GET0C( 'ABSLAB', ABSLAB, STATUS )
            CALL PAR_DEF0C( 'ORDLAB', 'Y', STATUS )
            CALL PAR_GET0C( 'ORDLAB', ORDLAB, STATUS )
         END IF

*       ^^^^^^^^^^^^^^^^^^^

         ANCLIP( 1 ) = ANCLP1
         ANCLIP( 2 ) = ANCLP2
         ANCLIP( 3 ) = ANCLP3
         ANCLIP( 4 ) = ANCLP4

*       Obtain the plotting style.
*       ==========================

*       Get the number of minor ticks, assigning the dynamic defaults.

         TICDEF( 1 ) = -1.
         TICDEF( 2 ) = -1.
         CALL PAR_GDR1R( 'MINTIC', 2, TICDEF, -1., VAL__MAXR, .FALSE.,
     :                   MINTIC, STATUS )

*       Get the parameter controlling the number of major ticks per
*       axis, assigning the dynamic defaults.

         TICDEF( 1 ) = 3.
         TICDEF( 2 ) = 3.
         CALL PAR_GDR1R( 'MAJTIC', 2, TICDEF, -1., VAL__MAXR, .FALSE.,
     :                   MAJTIC, STATUS )

*       Are the tick marks on the outside of the axes?

         CALL PAR_GTD0L( 'OUTTIC', .TRUE., .TRUE., OUTTIC, STATUS )
      END IF

*    Get the line thickness.
      CALL PAR_GDR0R( 'THICK', 1.0, 0.5, 10.0, .TRUE., THICK, STATUS )
      SCLINE = THICK .LT. 0.99999 .OR. THICK .GT. 1.00001

*    Set the fount.  This is only needed if there is some annotation.
*    Inline annotation is only available if there is a key.
      IF ( AXES .OR. KEY ) THEN

*    Get the fount.  Although NCAR is the default, either must be
*    selected to prevent persistence from earlier invocations.
         CALL PAR_CHOIC( 'FONT', 'GKS', 'GKS,NCAR', .TRUE., FOUNT,
     :                   STATUS )

*    Use the fast and clean san-serif fount.
         IF ( FOUNT .EQ. 'GKS ' ) THEN
            CALL AGPWRT( 0.0, 0.0, ' ', 0, 0, 0, -100 )

*    Crude attempt to make GKS text thicker by using the bold fount.
            IF ( THICK .GT. 1.5 ) CALL SGS_SFONT( 102 )

         ELSE IF ( FOUNT .EQ. 'NCAR' ) THEN
            CALL AGPWRT( 0.0, 0.0, ' ', 0, 0, 0, 100 )
         END IF
      END IF

*    Determine if square pixels are required.
*    ========================================
      CALL PAR_GTD0L( 'FILL', .FALSE., .TRUE., FILL, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 960

*    Create zones.
*    =============

*    Define the aspect ratio of the plot.  For the filled area just
*    find the aspect ratio of the frame. (This assumes symmetric
*    trimming of each axis for the annotated axes.)  Create a frame
*    zone for the plot and a key zone (when required).  The subroutine
*    recognises the negative ASP value, and will calculate an effective
*    aspect ratio and return this value.

      IF ( FILL ) THEN
         ASP = -1.0
      ELSE
         ASP = REAL( DIMS( 1 ) ) / REAL( DIMS( 2 ) )
      END IF
      CALL KPG1_KEYZO( KEY, ASPKEY, ASP, ZONEIF, ZONEK, STATUS )

*    Obtain the contour heights.
*    ===========================

*    Select the method of defining contour heights and evaluate them.

      CALL KPS1_CNSER( 'MODE', 'NCONT', 'FIRSTCNT', 'STEPCNT',
     :                 'HEIGHTS', 'PERCENTILES', BAD, EL,
     :                 %VAL( CNF_PVAL( PNTRI( 1 ) ) ), MXCONT, CNTLEV,
     :                 PERCNT, AREA, NCONT, MODE, STATUS )

*    Sort the contour heights into increasing order.

      IF ( STATUS .EQ. SAI__OK .AND. NCONT .GT. 0 )
     :  CALL KPG1_QSRTR( NCONT, 1, NCONT, CNTLEV, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( STATUS .NE. PAR__ABORT ) THEN
            CALL ERR_REP( 'TURBOCONT_GTLEV',
     :        'TURBOCONT: Error obtaining or sorting the contour '/
     :        /'levels', STATUS )
         END IF
         GOTO 960
      END IF

*    Obtain the level below which the contours will be dashed.  A null
*    value means no contours are dashed.
      CALL ERR_MARK
      CALL PAR_GET0R( 'DASHED', THRESH, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         THRESH = VAL__BADR
      END IF
      CALL ERR_RLSE

*    Obtain the line characteristics.
*    ================================

*    Initialise the flags used to define the contour style.
      ANNOTA = .FALSE.
      MAXRES = .FALSE.
      NOISY = .FALSE.
      PENROT = .FALSE.
      LABFRQ = 0

*    There must be a key so that the annotations can be converted
*    into actual values.

      IF ( KEY ) THEN

*       Determine whether contours are to be annotated or not.

         CALL PAR_GTD0L( 'ANNOTA', .FALSE., .TRUE., ANNOTA, STATUS )
      END IF

*    There are additional qualifying parameters once annotation is
*    selected.

      IF ( ANNOTA ) THEN

*       See whether or not double annotations required.

         CALL PAR_GTD0L( 'NOISY', .FALSE., .TRUE., NOISY, STATUS )

*       Get the frequency of contour-level annotations.

         CALL PAR_GDR0I( 'LABELFREQ', 1, 1, NCONT, .TRUE., LABFRQ,
     :                   STATUS )

      ELSE

*       Determine whether or not pens are to be cycled through the
*       contour levels.

         CALL PAR_GTD0L( 'PENROT', .FALSE., .TRUE., PENROT, STATUS )

      END IF

*    Determine whether or not contouring is computed for the maximum
*    resolution.

      CALL PAR_GTD0L( 'MAXRES', .FALSE., .TRUE., MAXRES, STATUS )

*    Get the resolution factor. The actual plotting resolution is this
*    times the x and y theoretical resolutions in world co-ordinates.
*    In GKS the whether or not a given "lamp" is illuminated or pen
*    quantum has ink within it cannot be determined, so a factor of
*    unity is too small.

      CALL PAR_GDR0R( 'RESOLUTION', 2.0, 2.0, 10.0, .TRUE., RESFAC,
     :                STATUS )

*    Report error context.

      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( STATUS .NE. PAR__ABORT ) THEN
            CALL ERR_REP( 'TURBOCONT_LINTYP',
     :        'TURBOCONT: Error obtaining line characteristics',
     :        STATUS )
         END IF
         GOTO 960
      END IF

*  Set the pen colours and line styles.
*  ====================================

*  Note that this must be done before plotting to avoid plot
*  regeneration (i.e. clear the plot when the device is closed).

      IF ( .NOT. PENROT ) THEN

*  Obtain the colour index for the desired colour of the contours.
*  Don't restrict the colours to the palette to give the user more
*  control.  There are instructions in the documentation on the benefits
*  of choosing a palette colour.
         CALL KPG1_IVCI( 'DEVICE', 'CONCOL', .FALSE., CONCI, STATUS )

      END IF

*  In order to preserve pen 1 for the axes and key, contouring uses pen
*  2.  So we shift the properties but default the properties of pen 1
*  to pen 2.  If there is a threshold, we need to transfer pens 2 and 3
*  to 3 and 4 respectively.  Therefore we start at the highest pen and
*  work backwards.

*  Inquire the workstation identifier for GKS inquiries.
      CALL SGS_ICURW( IWKID )

*  When there is pen rotation we want dashed lines for pens 5 to 7 in
*  the same colours as pens 2 to 4.  Otherwise only pen 5 need be
*  dashed.
      MAXPEN = CONPEN
      IF ( PENROT ) MAXPEN = CONPEN + 2
      DO I = MAXPEN, CONPEN, -1

*  Inquire the current colour index of this pen (it will be restored
*  after all plotting is complete).
         CALL GQPLR( IWKID, I - 1, GSET, IERR, CLNTYP, CLWIDT, CCOLI )

*  Decide whether to use the CONCOL index or the existing colour.
         IF ( .NOT. PENROT ) CCOLI = CONCI

*  Decide whether to use the existing line type or the existing type.
*  It is solid when there is no pen rotation.  This is because in the
*  case of pen rotation, we need to preserve the line style in case the
*  device only supports a variety of dashed-line patterns.
         IF ( .NOT. PENROT ) CLNTYP = 1

*  Store the new colour index, line style, and line thickness for this
*  pen and its complementary dashed form.  However, the line thickness
*  appears not to change (probably due to NCAR resetting something, as
*  it works when NCAR calls are absent).
         IF ( THRESH .NE. VAL__BADR ) THEN
            CALL GSPLR( IWKID, I, CLNTYP, THICK, CCOLI )
            CALL GSPLR( IWKID, I + 3, 2, THICK, CCOLI )

*  Just shift the pen to have the attributes of the next higher pen,
*  except perhaps the colour when there is no pen rotation.
         ELSE
            CALL GSPLR( IWKID, I, CLNTYP, THICK, CCOLI )

         END IF
      END DO

*    Obtain the axis co-ordinates.
*    =============================

      IF ( DACOOR ) THEN

*       To plot axes and to be able to store their data co-ordinates
*       the application requires a linear axis.  Determine whether or
*       not the data co-ordinates derived from the NDF axes are linear
*       and not identical to world (pixel) co-ordinates, and if so find
*       the linear transformation from world to data co-ordinates and
*       the axis bounds.

         IF ( DPAXIS ) THEN
            CALL KPG1_DCLID( NDIM, NDFS, DXLBND, DXUBND, DSCALE, DOFSET,
     :                       DACOOR, STATUS )
            DO I = 1, NDIM
               AXLBND( I ) = REAL( DXLBND( I ) )
               AXUBND( I ) = REAL( DXUBND( I ) )
            END DO
         ELSE
            CALL KPG1_DCLIR( NDIM, NDFS, AXLBND, AXUBND, SCALE, OFFSET,
     :                       DACOOR, STATUS )
         END IF
      END IF

      IF ( AXES .AND. .NOT. DATACO ) THEN

*       Just use the pixel bounds to define the annotated-axis limits
*       as there is no axis information or the user want pixel indices
*       on the axes.

         DO I = 1, NDIM
            AXLBND( I ) = REAL( SLBND( I ) - 1 )
            AXUBND( I ) = REAL( SUBND( I ) )
         END DO
      END IF

*    Define the location of the axes and draw them.
*    ==============================================

*    Plot in the image area, which includes the border (graph window)
*    if axes are required.

      CALL SGS_SELZ( ZONEIF, STATUS )

      IF ( AXES ) THEN

*       Get AUTOGRAPH to use the SGS zone.

         CALL SNX_AGWV

*       Store the current NCAR grid values.

         CALL AGGETF( 'GRID/LEFT.', GRID( 1 ) )
         CALL AGGETF( 'GRID/RIGHT.', GRID( 2 ) )
         CALL AGGETF( 'GRID/BOTTOM.', GRID( 3 ) )
         CALL AGGETF( 'GRID/TOP.', GRID( 4 ) )

*       Set the current NCAR grid values.

         CALL AGSETF( 'GRID/LEFT.', ANCLIP( 1 ) )
         CALL AGSETF( 'GRID/RIGHT.', ANCLIP( 2 ) )
         CALL AGSETF( 'GRID/BOTTOM.', ANCLIP( 3 ) )
         CALL AGSETF( 'GRID/TOP.', ANCLIP( 4 ) )

*       Draw annotated axes in graph window with the grid positioned as
*       defined above.  Note the NDF-axis bounds are used to define the
*       bounds of the annotated axes.  In other words a regularly
*       spaced array is assumed.  Note there may be problems for d.p.
*       data co-ordinates due to GKS's use of single precision.

         CALL NCRAXS( AXLBND( 1 ), AXLBND( 2 ), AXUBND( 1 ),
     :                AXUBND( 2 ), PLTITL, ABSLAB, ORDLAB, MINTIC,
     :                MAJTIC, OUTTIC, THICK, .FALSE., STATUS )

*       The plot of contours will in be in the NCAR grid window.  When
*       NCAR is not required to draw with annotation the equivalent SGS
*       zone has to be defined.  If annotation is required the new zone
*       is not used for plotting, but it is created merely to define
*       the location of the top of the key.  The NCAR grid window is
*       defined to lie between world co-ordinates 0--1 along each axis.

         CALL SNX_AGCS
         CALL SGS_ZONE( 0.0, 1.0, 0.0, 1.0, ZONEI, STATUS )

*       Restore the input NCAR grid values.

         CALL AGSETF( 'GRID/LEFT.', GRID( 1 ) )
         CALL AGSETF( 'GRID/RIGHT.', GRID( 2 ) )
         CALL AGSETF( 'GRID/BOTTOM.', GRID( 3 ) )
         CALL AGSETF( 'GRID/TOP.', GRID( 4 ) )

*    No axes.
*    ========

      ELSE

*       Make the image/frame zone the image zone as there need not be
*       space for axes.  So clone the image zone from the frame zone.

         CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )
         CALL SGS_ZONE( X1, X2, Y1, Y2, ZONEI, STATUS )

*       Define an world co-ordinate system for the image zone.  The unit
*       square may be needed to compute the position of the top of the
*       key.

         CALL SGS_SW( 0.0, 1.0, 0.0, 1.0, STATUS )
      END IF

*    Set the line width.
*    ===================

      IF ( SCLINE ) THEN

*       Inquire the GKS aspect source flags.
         CALL GQASF( GSTAT, LASF )

*       Set the line width scale factor source flags to individual.
         LASF( 2 ) = 1
         CALL GSASF( LASF )

*       Now actually set the line width scale factor.
         CALL GSLWSC( THICK )

*       Watch out for any error.
         CALL GKS_GSTAT( STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 960
      END IF

*    Draw a box around the plot when required.

      IF ( BORDER .AND. .NOT. AXES ) CALL SGS_BOX( 0.0, 1.0, 0.0, 1.0 )

*    Find the fractional position of the top axis, to be used later
*    for positioning the key.
*    ==============================================================

      IF ( KEY ) THEN

*       Again there are two cases.  If the aspect ratio of the data
*       array region to be contoured is less than one, the frame zone
*       of the contour plot spans the full height of the current
*       picture.  Therefore just use the fractional position of the top
*       of the grid from NCAR.  If there are no axes, it is top of the
*       second dimension of the image zone.  (Note at this point the
*       co-ordinates are normalised for the frame, whereas the top of
*       the plot will be the inverse aspect ratio in this system.) The
*       key-drawing routine will calculate an offset from the height of
*       the text if necessary.

         IF ( ASP .LT. 1.0 .AND. AXES ) THEN
            KEYOFF = ANCLIP( 4 )
         ELSE

*          The top of the grid is a line y=1.0 in ZONEI. Transform
*          this to the key zone.

            IF ( AXES .OR. FILL ) THEN
               CALL SGS_TPZ( ZONEI, 0.0, 1.0, ZONEK, TK, KEYOFF,
     :                       STATUS )

*          The top of the grid is a line at the ratio of the zone and
*          image aspect ratios in ZONEI. (Since these are in normalised
*          co-ordinates).  Transform this to the key zone.

            ELSE
               CALL SGS_TPZ( ZONEI, 0.0, XM / YM / ASP, ZONEK, TK,
     :                       KEYOFF, STATUS )
            END IF
         END IF
      END IF

*    Define the world co-ordinates in the image zone.  This permits
*    KPS1_CNTUR to work on the NDF section as if it were the whole
*    array.
      CALL SGS_SW( 0.0, REAL( DIMS( 1 ) ), 0.0, REAL( DIMS( 2 ) ),
     :             STATUS )

      IF ( ANNOTA ) THEN

*       Want to plot with NCAR, so return to the previous zone
*       associated with the grid window.

         CALL SGS_SELZ( ZONEIF, STATUS )

*       The NCAR grid may have data co-ordinates, whereas the contouring
*       routine expects pixel co-ordinates.  In this case the grid
*       co-ordinates should be changed to pixels. Also make the grid
*       limits match the existing one (i.e. not applicable when there
*       are no axes).  Also if the origin of the pixel co-ordinates is
*       not at (1,1), there needs to be a shift of origin.

         IF ( AXES ) THEN

*          Set the current NCAR grid values.

            CALL AGSETF( 'GRID/LEFT.', ANCLIP( 1 ) )
            CALL AGSETF( 'GRID/RIGHT.', ANCLIP( 2 ) )
            CALL AGSETF( 'GRID/BOTTOM.', ANCLIP( 3 ) )
            CALL AGSETF( 'GRID/TOP.', ANCLIP( 4 ) )

         ELSE IF ( .NOT. AXES ) THEN

*          Store the current NCAR grid values.

            CALL AGGETF( 'GRID/LEFT.', GRID( 1 ) )
            CALL AGGETF( 'GRID/RIGHT.', GRID( 2 ) )
            CALL AGGETF( 'GRID/BOTTOM.', GRID( 3 ) )
            CALL AGGETF( 'GRID/TOP.', GRID( 4 ) )

*          Set the NCAR grid values to encompass the whole plotting
*          region.

            CALL AGSETF( 'GRID/LEFT.', 0.01 )
            CALL AGSETF( 'GRID/RIGHT.', 0.99 )
            CALL AGSETF( 'GRID/BOTTOM.', 0.01 )
            CALL AGSETF( 'GRID/TOP.', 0.99 )
         END IF

*       Create the invisible axes to enable world co-ordinates to be
*       recognised by NCAR.

         CALL KPS1_NCUCO( 0.0, REAL( DIMS( 1 ) ), 0.0,
     :                    REAL( DIMS( 2 ) ), STATUS )

*       Reset the NCAR grid limits.

         CALL AGSETF( 'GRID/LEFT.', GRID( 1 ) )
         CALL AGSETF( 'GRID/RIGHT.', GRID( 2 ) )
         CALL AGSETF( 'GRID/BOTTOM.', GRID( 3 ) )
         CALL AGSETF( 'GRID/TOP.', GRID( 4 ) )

      END IF

      IF ( MAXRES ) THEN

*       Inquire the plotting resolution in world co-ordinates so that
*       only visible contours will be plotted.

         CALL SGS_IDUN( XRES, YRES )

*       Find the minimum offsets for a contour to be plotted.

         DX = MAX( 1.E-3, RESFAC * XRES )
         DY = MAX( 1.E-3, RESFAC * YRES )
      ELSE

*       This gives pixel resolution implicitly.

         DX = 0.0
         DY = 0.0
      END IF

*    Obtain work space to compute the contours.
*    ==========================================

*    Create a scratch area in which to log pixels contoured.

      CALL PSX_CALLOC( DIMS( 1 ) * DIMS( 2 ), '_LOGICAL', WPNTR( 1 ),
     :                 STATUS )

*    Record the number of work arrays obtained and assign an alias.

      NWORK = 1
      SCRPNT = WPNTR( 1 )
      IF ( STATUS .NE. SAI__OK ) GOTO 950

*    Create a scratch area in which to store the linked list of cells.

      CALL PSX_CALLOC( CELDIM * CELDIM, '_INTEGER', WPNTR( 2 ), STATUS )

*    Increment the count of workspace obtained and assign as alias.

      NWORK = NWORK + 1
      LLPNTR = WPNTR( 2 )
      IF ( STATUS .NE. SAI__OK ) GOTO 950

*    Create a scratch area in which to flag cells.

      CALL PSX_CALLOC( CELDIM * CELDIM, '_INTEGER', WPNTR( 3 ), STATUS )

*    Increment the count of workspace obtained and assign as alias.

      NWORK = NWORK + 1
      CFPNTR = WPNTR( 3 )
      IF ( STATUS .NE. SAI__OK ) GOTO 950

*    Find the size of the buffers to store the contours.  The factor
*    is somewhat arbitrary, but should cope with very twisty contours
*    spanning the whole of the cell or image.  DX and DY must be
*    positive so, no checking!

      IF ( MAXRES ) THEN
         NLOCUS = 8 * MIN( 2 * CELDIM, DIMS( 1 ) + DIMS( 2 ) ) *
     :            MIN( MXSBPX, MAX( INT( 1.0 / DX ), INT( 1.0 / DY ) )
     :            + 1 )
      ELSE
         NLOCUS = 8 * MIN( 2 * CELDIM, DIMS( 1 ) + DIMS( 2 ) )
      END IF

      IF ( NOISY ) NLOCUS = NLOCUS * 2

*    Create a scratch area in which to store the x locus of a contour.

      CALL PSX_CALLOC( NLOCUS, '_REAL', WPNTR( 4 ), STATUS )

*    Increment the count of workspace obtained and assign an alias.

      NWORK = NWORK + 1
      XPNTR = WPNTR( 4 )
      IF ( STATUS .NE. SAI__OK ) GOTO 950

*    Create a scratch area in which to store the y locus of a contour.

      CALL PSX_CALLOC( NLOCUS, '_REAL', WPNTR( 5 ), STATUS )

*    Increment the count of workspace obtained and assign an alias.

      NWORK = NWORK + 1
      YPNTR = WPNTR( 5 )
      IF ( STATUS .NE. SAI__OK ) GOTO 950

*    Draw the contour plot itself.
*    =============================

      CALL KPS1_CNTUR( DIMS( 1 ), DIMS( 2 ),
     :                 %VAL( CNF_PVAL( PNTRI( 1 ) ) ), 0, 0,
     :                 DIMS( 1 ), DIMS( 2 ), CELDIM, CELDIM,
     :                 NCONT, CNTLEV, DX, DY, ANNOTA, NOISY, LABFRQ,
     :                 PENROT, THRESH, MAXRES, NLOCUS,
     :                 %VAL( CNF_PVAL( XPNTR ) ),
     :                 %VAL( CNF_PVAL( YPNTR ) ), SLIST,
     :                 %VAL( CNF_PVAL( LLPNTR ) ),
     :                 %VAL( CNF_PVAL( CFPNTR ) ), CNTUSD, STATUS )
      CALL SGS_FLUSH

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'TURBOCONT_SCA',
     :     'TURBOCONT: Error contouring the array.', STATUS )
      END IF

*    Re-define world co-ordinates of the contouring zone to the normal
*    convention.

      CALL SGS_SW( REAL( SLBND( 1 ) ) - 1.0, REAL( SUBND( 1 ) ),
     :             REAL( SLBND( 2 ) ) - 1.0, REAL( SUBND( 2 ) ),
     :             STATUS )

*    Reset the line width.
*    =====================

      IF ( SCLINE ) THEN

*       Set the line width scale factor source flags to bundled.

         LASF( 2 ) = 0
         CALL GSASF( LASF )

*       Watch out for any error.

         CALL GKS_GSTAT( STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 950
      END IF

*    Unmap and annul workspace.
*    ==========================

      DO I = 1, NWORK
         CALL PSX_FREE( WPNTR( I ), STATUS )
      END DO

      IF ( STATUS .NE. SAI__OK ) GOTO 960

*    Record the data picture in the database.
*    ========================================
*
*    Switch back to the frame picture.

      CALL AGI_SELP( PICID2, STATUS )

*    Record the picture and a reference to the NDF in the database.

      CALL KPG1_SDTRN( 'KAPPA_TURBOCONT', NDF, PICID3, STATUS )

*    Store a transformation from data co-ordinates to world co-ordinates
*    where they are different and the data co-ordinates are linear.

      IF ( DACOOR ) THEN
         IF ( DPAXIS ) THEN
            CALL KPG1_LITRD( DSCALE, DOFSET, STATUS )
         ELSE
            CALL KPG1_LITRR( SCALE, OFFSET, STATUS )
         END IF
      END IF

*    Plot the key.
*    =============

*    Reset input picture as current in case of an accident.

      CALL AGI_SELP( PICID1, STATUS )

*    If a key of contours are to be plotted...

      IF ( KEY .AND. STATUS .EQ. SAI__OK ) THEN

*       Return to the key zone and frame picture.

         CALL AGI_SELP( PICID2, STATUS )
         CALL SGS_SELZ( ZONEK, STATUS )

*       Get AUTOGRAPH to use SGS zones.

         CALL SNX_AGWV

*       Draw the key, aligned with the top axis.

         CALL CNTKEY( NCONT, CNTLEV, CNTUSD, KEYOFF, UNITS, THICK,
     :                STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'TURBOCONT_NOKEY',
     :        'TURBOCONT: Error while plotting the key.', STATUS )
         ELSE

*          Record the key in the database.

            CALL AGS_SZONE( 'KEY', 'KAPPA_TURBOCONT', PICID4, STATUS )

            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'TURBOCONT_DBSK',
     :           'TURBOCONT: Error while storing the key picture in '/
     :           /'the graphics database.', STATUS )
            END IF
         END IF

*    End of plot-key check.

      END IF

*    Report the heights to the user as they are not always clearly
*    visible on small plots and/or on low-resolution workstations.

      CALL CNTHLT( NCONT, CNTLEV, CNTUSD, STATUS )

      GOTO 960

*    Something has gone wrong obtaining work space, so report error
*    context and tidy the workspace obtained successfully.

 950  CONTINUE
      CALL ERR_REP( 'TURBOCONT_WSP',
     :  'TURBOCONT: Unable to get workspace for plotting contours',
     :  STATUS )

      DO I = 1, NWORK
         CALL PSX_FREE( WPNTR( I ), STATUS )
      END DO

*    AGI closedown sequence.
*    =======================

 960  CONTINUE

*  Deactivate SGS and close the workstation.  If the workstation was
*  not activated an error results. >>> THIS IS COMMENTED OUT SINCE
*  IT CAUSES THE SCREEN TO BE CLEARED! 24/5/01 DSB
*      CALL AGS_DEACT( STATUS )

*  Close the AGI context and reinstate the input current picture.  If
*  there is no current picture an error results.
      CALL AGI_END( -1, STATUS )

*  Inquire the input picture identifier so that it may be annulled
*  and the database closed.
      CALL AGI_ICURP( PICID, STATUS )
      CALL AGI_ANNUL( PICID, STATUS )

*    Unmap and annul NDF data.
*    =========================

 980  CONTINUE
      CALL NDF_END( STATUS )

 999  CONTINUE

      END
