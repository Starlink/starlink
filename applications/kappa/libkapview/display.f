      SUBROUTINE DISPLAY( STATUS )
*+
*  Name:
*     DISPLAY

*  Purpose:
*     Displays a 1-d or 2-d NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation
*     CALL DISPLAY( STATUS )

*  Description:
*     This application displays an image of a 1- or 2-dimensional NDF
*     on the current image-display device.  The image may be the data
*     array, but also variance or quality can be shown.  The image is
*     situated within the current picture with the maximum
*     magnification without clipping or distorting the image, though
*     the exact positioning and magnification can be controlled.  The
*     colour mapping has several scaling methods described below.  All
*     the available colour indices are used save a few reserved for
*     annotations.
*
*     Only the parts of the displayed image that lie within the current
*     picture are visible;  the rest is clipped.  Should the image be
*     too large to fit onto the current picture at unit magnification
*     (if you demand this magnification), then there is an option to
*     squash the array in order to make it just fit; otherwise the
*     portion of the data array visible within the current picture is
*     displayed as originally requested.

*     Annotated axes and a title, or a coloured border may be drawn
*     around the displayed image. 

*  Usage:
*     display in [comp] clear [device] mode [centre] [xmagn] [ymagn]
*        [out] { low=? high=?
*              { percentiles=?
*              { sigmas=?
*              mode

*  ADAM Parameters:
*     ABSLAB  =  LITERAL (Read)
*        Label for the plot abscissa, in which NCAR fancy founts 
*        may be embedded when FONT = "NCAR".  This parameter is only
*        used when the axes option is selected.  If axis information is
*        present the suggested default is the NDF's axis label followed
*        by the units, in parentheses.  If an error occurs obtaining
*        the label the suggested default is "X". []
*     AXES = _LOGICAL (Read)
*        TRUE if labelled and annotated axes are to be drawn around the
*        displayed image.  The annotations are either the data
*        co-ordinates from the NDF axis components, provided these are
*        present and linear and COSYS = "Data"; otherwise pixel
*        co-ordinates are used.  [FALSE]
*     BADCOL = LITERAL (Read)
*        The colour to give a bad pixel in the display.  There are a
*        number of options described below.
*
*          "MAX"          - The maximum colour index used for the
*                           display of the image.
*          "MIN"          - The minimum colour index used for the
*                           display of the image.
*          An integer     - The actual colour index.  It is constrained
*                           between 0 and the maximum colour index
*                           available on the device. 
*          A named colour - Uses the named colour from the palette, and
*                           if it is not present, the nearest colour
*                           from the palette is selected.
*
*        If the colour is to remain unaltered as the lookup table is
*        manipulated choose an integer between 0 and 15, or a named
*        colour.  The suggested default is the current value. [The
*        current value, but equals "MIN" if there is no current value.]
*     BCOLOUR = LITERAL (Read)
*        The colour of the border.  It is only accessed if BORDER is
*        TRUE.  There are a number of options described below.
*
*          "MAX"          - The maximum palette colour index.
*          "MIN"          - The background colour.
*          An integer     - The actual colour index in the palette.  It
*                           is constrained to be between 0 and 15.
*          A named colour - Uses the named colour from the palette, and
*                           if it is not present, the nearest colour
*                           from the palette is selected.
*
*        The suggested default is the current value. [The current value,
*        but equals "Yellow" if there is no current value.]
*     BORDER = _LOGICAL (Read)
*        TRUE if a coloured border is to be drawn around the
*        displayed image.  If AXES is TRUE the value of BORDER will
*        be ignored and no border will be drawn.  The colour and width
*        of the border is controlled by parameters BCOLOUR and
*        BWIDTH. [FALSE]
*     BWIDTH( 2 ) = _REAL (Read)
*        The width of the border along each axis in device pixels.  It
*        is only obtained when BORDER is TRUE.  If only a single value
*        is given it is duplicated to the second dimension.  The
*        suggested default is the current value.  The widths must lie
*        in the range 1.0--20.0. [4.0]
*     CENTRE( 2 ) = _DOUBLE (Read)
*        These two values control the position of the displayed image.
*        Specifically, if COSYS = "World" they are the pixel indices of
*        the NDF image that are to lie at the centre of the current
*        picture, but they are not limited to the bounds of the NDF
*        array.  If COSYS = "Data" they are the data co-ordinates to
*        lie at the centre of the picture, and are limited by the
*        bounds of the NDF array.  The CENTRE parameters permit you to
*        display a portion of an NDF about a specified pixel at high
*        magnification.  The application attempts to display as much of
*        the NDF array it can at the magnification, so do not expect a
*        symmetric image about the chosen centre.  If you do not
*        specify a magnification with centering, it may result in a
*        small displayed image.  Further it may not be possible to have
*        precisely the pixel you want at the centre of the image; the
*        displacement decreases as the magnification is increased.
*        CENTRE is disabled when FILL is TRUE.  [Centre of the image]
*     CLEAR = _LOGICAL (Read)
*        TRUE if the current picture is to be cleared before the
*        display of the image. [FALSE]
*     COMP = LITERAL (Read)
*        The NDF component to be displayed.  It may be "Data",
*        "Quality", "Variance", or "Error" (where "Error" is the
*        alternative to "Variance" and causes the square root of the
*        variance values to be displayed).  If "Quality" is specified,
*        then the quality values are treated as numerical values (in
*        the range 0 to 255). ["Data"]
*     COSYS = LITERAL (Read)
*        The co-ordinate system to be used.  This can be either "World"
*        or "Data".  "World" makes pixel co-ordinates to appear on axes
*        and the centering is defined in pixels.  If COSYS = "Data" the
*        NDF's axis information is used to annotate axes and to control
*        the position of the displayed image.  [Current co-ordinate
*        system]
*     DEVICE = DEVICE (Read)
*        The name of the graphics device used to display the image.
*        The device must be in one of the following GNS categories:
*        IMAGE_DISPLAY, IMAGE_OVERLAY, MATRIX_PRINTER, or WINDOW, and
*        have at least 24 colour indices or greyscale intensities.
*        [Current image-display device]
*     FILL = _LOGICAL (Read)
*        The display normally has square pixels, in other words a
*        length along each axis corresponds to the same number of
*        pixels.  However, for images with markedly different
*        dimensions, such as two-dimensional spectra, this default
*        behaviour may not be suitable or give the clearest plot.  When
*        FILL is TRUE, the square-pixel constraint is relaxed and the
*        displayed image is the largest possible within the current
*        picture.  When FILL is FALSE, the pixels are square.  When
*        FILL is TRUE it disables the CENTRE, XMAGN, and YMAGN
*        parameters.  The suggested default is the current value.
*        [FALSE]
*     FONT = LITERAL (Read)
*        The fount to be used for the line graphics.  It can be either
*        "NCAR" for the NCAR fancy characters and "GKS" for the standard
*        GKS san-serif fount.  The former is intended for hardcopy
*        publication-quality plots, since it is relatively slow; the
*        latter is intended for normal interactive graphics requiring
*        rapid plotting, and it is clearer on small plots. The
*        suggested default is the current value. ["GKS"]
*     HIGH = _DOUBLE (Read)
*        The array value that scales to the highest pen in the colour 
*        table.  All larger array values are set to the highest colour
*        index when HIGH is greater than LOW, otherwise all array values
*        greater than HIGH are set to the lowest colour index.  The
*        dynamic default is the maximum data value.  There is an
*        efficiency gain when both LOW and HIGH are given on the
*        command line, because the extreme values need not be computed.
*        (Scale mode)
*     IN = NDF (Read)
*        Input NDF data structure containing the image to be displayed.
*     LOW = _DOUBLE (Read)
*        The array value that scales to the lowest pen in the colour 
*        table.  All smaller array values are set to the lowest colour
*        index when LOW is less than HIGH, otherwise all array values
*        smaller than LOW are set to the highest colour index.   The
*        dynamic default is the minimum data value.  There is an
*        efficiency gain when both LOW and HIGH are given on the
*        command line, because the extreme values need not be computed.
*        (Scale mode)
*     LUT = NDF (Read)
*        Name of the NDF containing a lookup table as its data array;
*        the lookup table is written to the image-display's colour
*        table.  The purpose of this parameter is to provide a means of
*        controlling the appearance of the image on certain devices,
*        such as colour printers, that do not have a dynamic colour
*        table, i.e. the colour table is reset when the device is
*        opened.  If used with dynamic devices, such as windows or
*        Ikons, the new colour table remains after this application has
*        completed. A null, !, means that the existing colour table will
*        be used.
*
*        The LUT must be 2-dimensional, the first dimension
*        being 3, and the second being arbitrary.  The method used to
*        compress or expand the colour table if the second dimension is
*        different from the number of unreserved colour indices is
*        controlled by parameter NN.  Also the LUT's values must lie in 
*        the range 0.0--1.0. [!]
*     MAJTIC( 2 ) = _REAL (Read)
*        The parameter controlling the numbers of major tick marks
*        for the x and y axes.  (Number used is between MAJTIC+2 and
*        5*MAJTIC/2+4.)   A negative value for an axis makes the
*        graphics package decide an appropriate value.  This parameter
*        is only used when the axes option is selected. [3.,3.]
*     MINTIC( 2 ) = _REAL (Read)
*        The number of minor tick marks between each major tick mark
*        for the x and y axes.  A negative value forces the graphics
*        package to compute appropriate values.   This parameter is
*        only used when the axes option is selected. [-1.,-1.]
*     MODE = LITERAL (Read)
*        The type of scaling to be applied to the array.  The options
*        are described below.
*          "Faint"       - The image is scaled from the mean minus one
*                          standard deviation to the mean plus seven
*                          standard deviations.  The scaling values are
*                          reported so that the faster Scale mode may be
*                          utilised later. 
*          "Flash"       - The image is flashed onto the screen without
*                          any scaling at all.  This is the fastest
*                          option.
*          "Percentiles" - The image is scaled between the values
*                          corresponding to two percentiles.  The
*                          scaling values are reported so that the
*                          faster Scale mode may be utilised later. 
*          "Range"       - The image is scaled between the minimum and
*                          maximum data values.
*          "Scale"       - You define the upper and lower limits
*                          between which the image is to be scaled.  The
*                          application reports the maximum and the
*                          minimum values for reference and makes these 
*                          defaults respectively.
*          "Sigmas"      - The image is scaled between two standard-
*                          deviation limits.  The scaling values used
*                          are reported so that the faster Scale mode
*                          may be utilised later. 
*     NN = _LOGICAL (Read)
*        If TRUE the input lookup table is mapped to the colour table by
*        using the nearest-neighbour method.  This preserves sharp
*        edges and is better for lookup tables with blocks of colour.
*        If NN is FALSE linear interpolation is used, and this is
*        suitable for smoothly varying colour tables.  NN is ignored
*        unless LUT is not null. [FALSE]
*     NUMBIN  =  _INTEGER (Read)
*        The number of histogram bins used to compute percentiles for
*        scaling. (Percentiles mode) [2048]
*     ORDLAB  =  LITERAL (Read)
*        Label for the plot ordinate, in which NCAR fancy founts 
*        may be embedded when FONT = "NCAR".   This parameter is only
*        used when the axes option is selected.  If axis information is
*        present the suggested default is the NDF's axis label followed
*        by the units, in parentheses.  If an error occurs obtaining
*        the label the suggested default is "Y". []
*     OUT = NDF (Write)
*        The scaled section of the NDF displayed; it also does not have
*        values that equal the reserved portion of the colour table.
*        The output NDF is intended to be used as the input data in
*        conjunction with SCALE=FALSE.  It will be vertically inverted
*        with respect to the input array because of GKS convention.  If
*        it has a null value (!) no output NDF will be created.  This
*        parameter is ignored when SCALE=FALSE. [!]
*     OUTTIC = _LOGICAL (Read)
*        TRUE if the axis tick marks are to appear on the outside of
*        the axes instead of inside.  This parameter is only used
*        when the axes option is selected. [TRUE]
*     PERCENTILES( 2 ) = _REAL (Read)
*        The percentiles that define the scaling limits. For example,
*        [25,75] would scale between the quartile values. (Percentile
*        mode)
*     PLTITL = LITERAL (Read)
*        The title of the plot.  Up to about 40 characters can be
*        accommodated, and NCAR fancy founts may be embedded (cf.
*        SUN/90) when FONT = "NCAR".  This parameter is only used when
*        the axes option is selected. [The NDF title]
*     SCAHIGH = _DOUBLE (Write)
*        The array value scaled to the maximum colour index for display.
*        In Flash mode or when there is no scaling the highest colour
*        index is used.  The current display linear-scaling maximum is
*        set to this value.
*     SCALE = _LOGICAL (Read)
*        If TRUE the input array is scaled according to the value of
*        parameter MODE.  If it is FALSE, MODE is ignored, and the input
*        array is displayed as is.  There is no scaling, inversion
*        or avoidance of annotation pens.  SCALE = FALSE is intended to
*        be used with arrays previously scaled by this or similar
*        applications which have already performed the scaling,
*        inversion and exclusion.  It provides the quickest method of
*        image display within this application. [TRUE]
*     SCALOW = _DOUBLE (Write)
*        The array value scaled to the minimum colour index for display.
*        In Flash mode or when there is no scaling the lowest colour
*        index is used.  The current display linear-scaling minimum is
*        set to this value.
*     SIGMAS( 2 ) = _REAL (Read)
*        The standard-deviation bounds that define the scaling limits.
*        To obtain values either side of the mean both a negative and
*        a positive value are required.  Thus [-2,3] would scale
*        between the mean minus two and the mean plus three standard
*        deviations.  [3,-2] would give the negative of that.
*     SQUASH = _LOGICAL (Read)
*        TRUE if the array is to be squashed otherwise it is displayed
*        as is with clipping.  This parameter is only used when the x
*        and y magnifications are both one, and the image would be
*        clipped.  It is not used in Flash mode.
*     THICK = _REAL (Read)
*        The thickness of the axes and annotations in the plot, where
*        1.0 is the normal thickness.  It should be between 0.5 and 5.
*        This feature is only available on some devices.   This
*        parameter is only used when the axes option is selected. [1.0]
*     XMAGN = _REAL (Read)
*        The magnification (zooming) in the x direction.  Unit 
*        magnification means that one NDF pixel maps to one
*        display-device pixel.  It is ignored when FILL is TRUE.
*        [Maximum that gives square pixels and just fills the current
*        database picture]
*     YMAGN = _REAL (Read)
*        The magnification (zooming) in the y direction.  Unit 
*        magnification means that one NDF pixel maps to one
*        display-device pixel.  It is ignored when FILL is TRUE.
*        [%XMAGN]

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Examples:
*     display ngc6872 mode=p percentiles=[10,90]
*        Displays the NDF called ngc6872 on the current image-display
*        device.  The scaling is between the 10 and 90 per cent
*        percentiles of the image.
*     display vv256 mode=flash border bwidth=6.0 badcol="Red"
*        Displays the NDF called vv256 on the current image-display
*        device.  There is no scaling of the data; instead the modulus
*        of each pixel with respect to the number of colour-table
*        indices is shown.  Any bad data will be displayed in red.  A
*        coloured border, of width six device pixels, is drawn around
*        the image; it will have the current border colour.
*     display mode=fa axes clear out=video cosys=d \
*        Displays the current NDF data component with annotated axes
*        after clearing the current picture on the current image-display
*        device.  The axes take the axis labels and title from the NDF,
*        and are annotated in data co-ordinates.  The scaling is
*        between the -1 and +7 standard deviations of the image around
*        its mean. The scaled data are stored in an NDF called video.
*     display video noscale \
*        Displays the data component of the NDF called video (created
*        in the previous example) without scaling within the current
*        picture on the current image-display device.
*     display in=cgs4a comp=v mode=sc low=1 high=5.2 device=xwindows
*        Displays the variance component of NDF cgs4a on the xwindows
*        device, scaling between 1 and 5.2.
*     display redrectangle xmagn=4 centre=[300,200] \
*        Displays the redrectangle NDF with a magnification of four
*        times, so that four device pixels corresponds to one image
*        pixel, on the current device.  The exact portion of the image
*        visible will depend on the size and location of the current 
*        picture, however the displayed portion will have pixel
*        (300,200) at the centre of the current picture.  The current
*        scaling is used.
*     display ngc6872 mode=ra device=lj250 lut=pizza
*        Displays the NDF called ngc6872 on the LJ250 device. The
*        lookup table in the NDF called pizza is mapped on the LJ250's
*        colour table.  The scaling is between the minimum and maximum
*        of the image.

*  Notes:
*     -  The application stores a number of pictures in the graphics
*     database in the following order: a FRAME of the specified size
*     containing the title, annotated axes, and the image area
*     (provided AXES is TRUE) or the border (if BORDER is TRUE), whose
*     world co-ordinates are in device pixels; a DATA picture with
*     world co-ordinates in pixel co-ordinates.  The DATA picture also
*     may have data co-ordinates derived from the NDF axis components
*     provided these are linear and different from pixel co-ordinates;
*     the data co-ordinates are stored via a double-precision linear
*     transformation.  The NDF associated with the plot is stored by
*     reference with the DATA picture.  On exit the current database
*     picture for the chosen device reverts to the input picture.
*     -  When axes are requested the axis annotations are defined by 
*     their lower and upper bounds, i.e. a regular array is assumed.
*     The bounds are derived from the part of NDF being displayed, and
*     will be in pixel or data co-ordinates.
*     -  The data type of the output NDF depends on the number of colour
*     indices: _UBYTE for no more than 256, _UWORD for 257 to 65535,
*     and _INTEGER otherwise.   The output NDF will not contain any
*     extensions, UNITS, QUALITY, and VARIANCE; but LABEL, TITLE, and
*     AXIS information are propagated from the input NDF.  The output
*     NDF does not become the new current data array.  It is a Simple
*     NDF (because the bad-pixel flag is set to false in order to
*     access the maximum colour index, and to handle sections),
*     therefore only NDF-compliant applications can process it.
*     -  For images much larger than the current picture size measured
*     in device pixels, the resolution of the device will allow only a
*     fraction of the detail in the array to be plotted.  Therefore,
*     the application compresses the image by block averaging when it
*     can do so without loss of resolution when displayed.  This saves
*     time scaling the data and transmitting them to the image display.
*     Note that the default values for parameters LOW and HIGH are
*     the minimum and maximum values in the compressed floating-point
*     array.

*  Algorithm:
*     - Determine whether scaling is required.
*     - Find which component to display, obtain an identifier to the
*     NDF and check that the component is present. Find the data type
*     for processing (integer if no scaling).  Get the NDF bounds and
*     inquire the bad-pixel flag.  Determine the co-ordinate system to
*     use.
*     -  Get the display device and open the database for it with the
*     appropriate device status. Get the current SGS zone. Inquire the
*     number of colour indices that are available on the image display
*     and check that device is indeed an image display.
*     -  Define world co-ordinates in device-pixel units, the method
*     depending on whether or not the current picture is the base.
*     -  Obtain the bounds of the image zone, with special allowances
*     made for axes, if required. Get co-ordinates of centre and
*     magnifications of the array within computed limits. Obtain the
*     scaling method.
*     -  Determine the limiting co-ordinates in device and array pixels.
*     -  Define the frame zone and axes, storing the frame picture in
*     in the database.  Get the style of the axes.
*     -  Obtain and map only those parts of the requested component
*     that are visible. Define new cell-array limits for the section.
*     Determine whether or not block averaging is required.  If it us
*     use work space to perform the calculations, adjusting the array
*     dimensions accordingly.
*     -  Create and map a scratch area for array scaling.  Scale via
*     the requested method and data type, performing any necessary
*     preliminary calculations, e.g. histogram for percentiles.  Reset
*     the bad-pixel flag if it has been determined that none exist 
*     during the preliminaries to save processing.
*     -  Plot the scaled cell array. Reset the world co-ordinates to be
*     in units of the unblocked data-array pixels and record the data
*     picture in the database.
*     -  Write the scaling limits.
*     -  Obtain the axis values from the NDF if present or when axes
*     and data co-ordinates are required.  Determine whether or not the
*     axes are linear.  If they are, compute the linear transformation.
*     -  Plot the axes if requested returning to the image zone.
*     -  Reset the world co-ordinates to be in units of the unblocked
*     data-array pixels and record the data picture in the database,
*     and a reference to the image data.  Record the linear
*     transformation if present.
*     -  Store the scaled data in an output NDF if required, performing
*     type conversion where necessary.
*     -  Free the workspace.
*     -  Reset the current picture to the input current picture.
*     -  Tidy the data and graphics.

*  Related Applications:
*     KAPPA: GREYPLOT; Figaro: IGREY, IMAGE; SPECDRE: MOVIE.

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
*     {enter_new_authors_here}

*  History:
*     1990 July 12 (MJC):
*        Original version based on the pre-V0.7 DISPLAY.
*     1990 August 19 (MJC):
*        Only the visible portion is scaled and displayed.  Redefined
*        CENTRE (ex XCENTR and YCENTR) in terms of image pixels.
*     1990 November 9 (MJC):
*        Block averaging added.
*     1991 February 7 (MJC):
*        Added NDF reference into the database, AGI context control and
*        removed fuzzy-picture fudge.
*     1991 March 19 (MJC):
*        Added output parameters for the scaling limits.
*     1991 April 4 (MJC):
*        Added data co-ordinate transformation.
*     1991 May 7 (MJC):
*        Added the input and output of NDFs containing scaled data.
*     1991 May 14 (MJC):
*        Added direct processing of _BYTE and _WORD data.
*     1991 July 22 (MJC):
*        Added coloured-border option, and user-controlled colouring of
*        bad pixels.
*     1991 July 31 (MJC):
*        No longer redefines first SGS pen to white if workstation has
*        dynamic colour representation, now there is palette control.
*     1991 August 20 (MJC):
*        Added FONT parameter.
*     1991 August 22 (MJC):
*        Added input lookup-table facility via LUT and NN parameters,
*        and an illustrative example of same.
*     1992 January 22 (MJC):
*        Fixed a bug that could cause the NOSCALE option to scale.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1992 March 30 (MJC):
*        Revised defaulting behaviour of BADCOL and BCOLOUR, and
*        corrected the notes.  Added an example of their use.  Handles
*        arbitrary user-defined 2-d sections.
*     1992 November 27 (MJC):
*        Does not use non-monotonic axis centres.
*     1992 December 17 (MJC):
*        Added the FILL option.
*     1995 October 19 (MJC):
*        Supports Error component.
*     1997 May 28 (MJC):
*        QUALITY and HISTORY no longer propagated to the OUT NDF.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     14-JUL-1998 (DSB):
*        Use '_DOUBLE' instead of 'DOUBLE' in call to NDF_MTYPE.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Global SSE definitions
      INCLUDE 'DAT_PAR'        ! Data-system constants
      INCLUDE 'PRM_PAR'        ! Magic-value definitions
      INCLUDE 'NDF_PAR'        ! NDF_ public constants
      INCLUDE 'NDF_ERR'        ! NDF_ error constants
      INCLUDE 'SUBPAR_PAR'     ! SUBPAR constants
      INCLUDE 'PAR_ERR'        ! Parameter-system error definitions
      INCLUDE 'CTM_PAR'        ! Colour-table management constants

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER MINCOL           ! Minimum number of colour indices on
                               ! device to be classed as an image
                               ! display
      PARAMETER ( MINCOL = 8 + CTM__RSVPN )

      REAL
     :  ANCLP1, ANCLP2,        ! Fraction of the frame zone in which the
     :  ANCLP3, ANCLP4         ! image will appear when there are axes.
                               ! Note aspect ratio is preserved.
      PARAMETER ( ANCLP1 = 0.19, ANCLP2 = 0.95,
     :            ANCLP3 = 0.15, ANCLP4 = 0.91 )

      INTEGER MAXBIN           ! Maximum number of histogram bins
      PARAMETER( MAXBIN = 2048 )! should be enough

      INTEGER MXLUTE           ! Maximum lookup table entry
      PARAMETER ( MXLUTE = CTM__MXPEN )

      INTEGER NDIM             ! Dimensionality required
      PARAMETER( NDIM = 2 )

      INTEGER NPRCTL           ! Maximum number of percentiles
      PARAMETER( NPRCTL = 2 )

      INTEGER NPRICL           ! Number of primary colours
      PARAMETER ( NPRICL = 3 )

      REAL MXMAGN              ! Maximum magnification factor
      PARAMETER ( MXMAGN = 32.0 )

*  Local Variables:
      LOGICAL                  ! True if :
     :  ANS,                   ! The array is to be squashed
     :  AXES,                  ! Annotated axes are to be drawn
     :  BAD,                   ! The array may contain bad pixels and
                               ! therefore testing should occur
     :  BLOCK,                 ! The image was averaged before display
     :  BORDER,                ! A border is to be drawn
     :  CLEAR,                 ! The image display is to be cleared
                               ! before display of the array
     :  DACOOR,                ! Data co-ordinates are to be stored
                               ! in the database
     :  DATACO                 ! Input and axes are given in data
                               ! co-ordinates

      LOGICAL                  ! True if:
     :  DATEMP,                ! Either of the axes is non-monotonic
     :  DEVCAN,                ! Image-display parameter is to be
                               ! cancelled
     :  DPAXIS,                ! Axis centres are double precision
     :  FILL,                  ! Plotting area is filled
     :  FNDRNG,                ! Find the data range for scaling
     :  INVERT,                ! The array is to inverted for display
     :  MONOTO,                ! Axis is monotonic
     :  NN,                    ! Mapping the input LUT via
                               ! nearest-neighbour method
     :  OUTTIC,                ! Axis tick marks are to be placed 
                               ! outside the box instead of inside
     :  POSTIV,                ! The scaling of the array is to be
                               ! positive
     :  SCALE,                 ! The input array will be scaled
     :  THERE                  ! Requested component is defined

      REAL
     :  ANCLIP( 4 ),           ! Fraction of the frame zone in which the
                               ! image will appear when there are axes.
                               ! Can't give array directly as parameter
     :  ASP,                   ! Aspect ratio of the current picture
     :  AXLBND( NDIM ),        ! Axis lower bounds
     :  AXUBND( NDIM ),        ! Axis upper bounds
     :  BORWID( NDIM ),        ! Border widths
     :  CONTRC,                ! Linear fractions of the frame zone that
                               ! contain the image zone (axes only)
     :  DEFMAG,                ! Dynamic default magnification
     :  DUMMY,                 ! Used to swap percentiles
     :  FACTOR,                ! Ratio of the image-display screen
                               ! width to maximum dimension of the
                               ! array
     :  GRID( 4 ),             ! Current AUTOGRAPH grid offsets
     :  HWX, HWY               ! Half widths of the current picture in
                               ! world co-ordinates

      REAL
     :  LSCALE( NDIM ),        ! Scale factors in the world-to-data
                               ! co-ordinate transformations
     :  LUT( NPRICL, 0:MXLUTE ),
                               ! Lookup table
     :  MINTIC( 2 ),           ! Numbers of minor tick marks along x and
                               ! y axes respectively
     :  MAJTIC( 2 ),           ! Parameters controlling the numbers of
                               ! major tick marks along x and y axes
                               ! respectively
     :  OFFSET( NDIM ),        ! Offsets in the world-to-data
                               ! co-ordinate transformations
     :  RMAXV,                 ! Minimum value in the array
     :  RMINV,                 ! Maximum value in the array
     :  PERCNT( NPRCTL ),      ! Percentiles
     :  PERDEF( NPRCTL ),      ! Suggested values for percentiles
     :  PERVAL( NPRCTL ),      ! Values at the percentiles
     :  PIXX,                  ! Maximum number of columns of pixels
                               ! of the image display
     :  PIXY                   ! Maximum number of lines of pixels
                               ! of the image display

      REAL
     :  RIMHI,                 ! Upper limit used for scaling the array
     :  RIMLO,                 ! Lower   "     "   "     "     "    "
     :  ROUNDL,                ! Rounding error in co-ordinate 
                               ! transformation (lower bounds)
     :  ROUNDU,                ! Rounding error in co-ordinate 
                               ! transformation (upper bounds)
     :  SIGDEF( 2 ),           ! Suggested default standard-deviation
                               ! limits
     :  SIGRNG( 2 ),           ! Standard-deviation limits
     :  T1, T2,                ! Dummy variables for rescaling image
     :  THICK,                 ! The line thickness (standard is 1.0)
     :  TICDEF( 2 ),           ! Suggested default axis-tick values
     :  VIEWP( 4 ),            ! NDC for input picture
     :  WINDOW( 4 )            ! Dummy for GKS inquiry

      REAL
     :  X1, X2, Y1, Y2,        ! Zone size in world co-ordinates
     :  XCEN, YCEN,            ! Centre of the image zone in world
                               ! co-ordinates
     :  XCENTR, YCENTR,        ! World co-ordinates for the centre of
                               ! displayed array
     :  XCTLE,                 ! Distance from centre of the picture to 
                               ! the left edge of the image
     :  XCTRE,                 ! Distance from centre of the picture to 
                               ! the right edge of the image
     :  XL, XU, YL, YU,        ! Boundary of cell array in world
                               ! co-ordinates
     :  XLC, XUC, YLC, YUC,    ! Boundary of zone to contain cell array
                               ! in world co-ordinates (image-display
                               ! pixels)
     :  XLF, XUF, YLF, YUF,    ! Boundary of zone to contain axes
                               ! in world co-ordinates (image-display
                               ! pixels)
     :  XLP, XUP, YLP, YUP,    ! Boundary of zone to contain cell array
                               ! in world co-ordinates (data-array
                               ! pixels)
     :  XLT, XUT, YLT, YUT,    ! Work variables to circumvent compiler
                               ! bug
     :  XM, YM,                ! Zone size in metres
     :  XMAGN, YMAGN,          ! x,y magnifications
     :  YCTLE,                 ! Distance from centre of the picture to 
                               ! the bottom edge of the image
     :  YCTUE                  ! Distance from centre of the picture to 
                               ! the top edge of the image

      CHARACTER*72
     :  ABSLAB,                ! Label for the abscissa of the plot
     :  ATYPE * ( NDF__SZTYP ),! Processing type of the axis centres
     :  COMP * 8,              ! Component to be displayed
     :  COSYS * 5,             ! Co-ordinate system
     :  DTYPE * ( NDF__SZFTP ),! Type of the image after processing (not
                               ! used)
     :  FORM * ( NDF__SZFRM ), ! Form of the output data array
     :  FOUNT * 4,             ! Fount type
     :  ITYPE * ( NDF__SZTYP ),! Processing type of the image
     :  MCOMP * 8,             ! Component to be mapped
     :  MODE,                  ! Manner in which the array is to be
                               ! scaled
     :  ORDLAB,                ! Label for the ordinate of the plot
     :  OTYPE * ( NDF__SZTYP ),! Processing type of the output image
     :  PLTITL,                ! Title of the plot
     :  PNAME * (DAT__SZNAM)   ! Name of the input picture

      INTEGER
     :  ACTHIG,                ! The HIGH parameter state
     :  ACTLOW,                ! The LOW parameter state
     :  AEL( NDF__MXDIM ),     ! Number of elements in a mapped axis
     :  AXPNTR( NDF__MXDIM ),  ! Pointers to the mapped axes
     :  BLAVF( NDIM ),         ! Block averaging factors
     :  BORCI,                 ! Border colour index
     :  BPCI,                  ! Bad-pixel colour index
     :  CDIMS( NDIM ),         ! Dimensions of compressed array
     :  CEL,                   ! Number of elements in compressed array
     :  DIMS( NDIM ),          ! Dimensions of input array
     :  EL,                    ! Number of elements in the input and 
                               ! cell arrays
     :  GSTAT,                 ! Graphics status
     :  HIST( MAXBIN )         ! Array containing histogram

      INTEGER
     :  I,                     ! General variables
     :  IERR,                  ! Position of first conversion error
     :  IIMHI,                 ! Upper limit used for scaling the array
     :  IIMLO,                 ! Lower   "     "   "     "     "    "
     :  IMAXV,                 ! Minimum value in the array
     :  IMINV,                 ! Maximum value in the array
     :  IPIXX,                 ! Maximum number of columns of pixels
                               ! of the image display
     :  IPIXY,                 ! Maximum number of lines of pixels
                               ! of the image display
     :  LBND( NDF__MXDIM ),    ! Lower bounds of the image
     :  LDIMS( NDIM ),         ! Dimensions of LUT arrays
     :  LEL,                   ! Number of elements in the input and 
                               ! output LUT arrays
     :  LP,                    ! Lowest pen with which to display the
                               ! the image
     :  LPNTR( 1 ),            ! Pointer to input colour table
     :  MAXPOS,                ! Position of the maximum (not used)
     :  MINPOS                 ! Position of the minimum (not used)

      INTEGER
     :  NDF,                   ! Identifier for input NDF
     :  NDFC,                  ! Identifier for input section
     :  NDFL,                  ! Identifier for LUT
     :  NDFO,                  ! Identifier for output NDF
     :  NDFS,                  ! NDF identifier for the visible section
                               ! of the image
     :  NDIMS,                 ! Total number of NDF dimensions
     :  NERR,                  ! Number of conversion errors
     :  NINTS,                 ! Number of greyscale intensities
                               ! available on the chosen device
     :  NINVAL,                ! Number of bad values in the input array
     :  NUMBIN,                ! Number of bins in histogram
     :  NTR,                   ! GKS normalisation transformation no.
     :  NVAL,                  ! Number of border widths
     :  OPNTR( 1 )             ! Pointer to output array data

      INTEGER
     :  PICID1,                ! Graphics' database identifier on input
     :  PICID2,                ! Graphics' database identifier for
                               ! the displayed picture
     :  PICIDF,                ! Graphics' database identifier for
                               ! the frame including axes
     :  PNTRI( 1 ),            ! Pointer to image data
     :  SAPNT,                 ! Pointer to the scratch area in which
                               ! a compressed array is kept
     :  SAPNT1,                ! Pointer to a scratch array used to
                               ! compute a compressed image
     :  SAPNT2,                ! Pointer to a scratch array used to
                               ! compute a compressed image
     :  SCRPNT,                ! Pointer to the scratch area in which
                               ! the scaled array is kept
     :  SDIM( NDF__MXDIM ),    ! Significant dimensions of the NDF
     :  SLBND( NDIM ),         ! Significant lower bounds of the image
     :  SUBND( NDIM ),         ! Significant upper bounds of the image
     :  UBND( NDF__MXDIM ),    ! Upper bounds of the image
     :  WKID,                  ! Work station identification
     :  ZONE1,                 ! SGS zone identifier
     :  ZONE2,                 ! SGS zone identifier
     :  ZONEF,                 ! SGS zone identifier for the frame
     :  ZONEI                  ! SGS zone identifier for the picture
                               ! itself

      DOUBLE PRECISION
     :  CDUMMY,                ! Dummy for co-ordinate conversion
     :  CENDEF( NDIM ),        ! Suggested default for the image centre
     :  CENMAX( NDIM ),        ! Maximum indices for the image centre
     :  CENMIN( NDIM ),        ! Minimum indices for the image centre
     :  CENTRE( NDIM ),        ! Indices of the image pixel at the
                               ! centre of the display
     :  DIMHI,                 ! Upper limit used for scaling the array
     :  DIMLO,                 ! Lower   "     "   "     "     "    "
     :  DMAXV,                 ! Minimum value in the array
     :  DMINV,                 ! Maximum value in the array
     :  DXLBND( NDIM ),        ! Axis lower bounds
     :  DXUBND( NDIM ),        ! Axis upper bounds
     :  DOFSET( NDIM ),        ! Offsets in the world-to-data
                               ! co-ordinate transformations
     :  DSCALE( NDIM )         ! Scale factors in the world-to-data
                               ! co-ordinate transformations

      BYTE
     :  BIMHI,                 ! Upper limit used for scaling the array
     :  BIMLO,                 ! Lower   "     "   "     "     "    "
     :  BMAXV,                 ! Minimum value in the array
     :  BMINV                  ! Maximum value in the array

      INTEGER * 2
     :  WIMHI,                 ! Upper limit used for scaling the array
     :  WIMLO,                 ! Lower   "     "   "     "     "    "
     :  WMAXV,                 ! Minimum value in the array
     :  WMINV                  ! Maximum value in the array

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'    ! NUM declarations for conversions
      INCLUDE 'NUM_DEF_CVT'    ! NUM definitions for conversions

*.

*    Check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*    Determine whether scaling is required.
*    ======================================
*
*    This parameter is needed immediately as this affects the
*    implementation data type.

      CALL PAR_GTD0L( 'SCALE', .TRUE., .TRUE., SCALE, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*    Associate the data to be displayed.
*    ===================================

*    Find which component to display.

      CALL PAR_CHOIC( 'COMP', 'Data', 'Data,Quality,Error,Variance',
     :                .FALSE., COMP, STATUS )

*    Most NDF routines with a component argument don't recognise
*    'ERROR', so we need two variables.  Thus convert 'ERROR' into
*    'VARIANCE' in the variable needed for such routines.  The original
*    value is held in a variable with the prefix M for mapping, as one
*    of the few routines that does support 'ERROR' is NDF_MAP; it is
*    also needed for plot annotations using any NDF units.

      MCOMP = COMP
      IF ( COMP .EQ. 'ERROR' ) COMP = 'VARIANCE'

*    Begin an NDF context.

      CALL NDF_BEGIN

*    Obtain the identifier of the NDF to be displayed.

      CALL NDF_ASSOC( 'IN', 'READ', NDF, STATUS )

*    There must be a data array, but for other components check that
*    requested component is present.

      IF ( COMP .NE. 'DATA' ) THEN
         CALL NDF_STATE( NDF, COMP, THERE, STATUS )

*       The component is not present or not defined, so report an error
*       giving the NDF's name.

         IF ( .NOT. THERE ) THEN
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', NDF )
            CALL MSG_SETC( 'COMP', COMP )
            CALL ERR_REP( 'DISPLAY_NOCOMP',
     :        'DISPLAY: ^COMP component is not defined in NDF ^NDF.',
     :        STATUS )
            GO TO 980
         END IF
      END IF

*    This application can only process real, double precision, word,
*    integer and byte components directly. Therefore for the given type
*    of the image find in which type it should be processed.

      IF ( SCALE ) THEN
         CALL NDF_MTYPE( '_BYTE,_WORD,_INTEGER,_REAL,_DOUBLE', NDF, NDF,
     :                   COMP, ITYPE, DTYPE, STATUS )

*    If there is no scaling the GKS cell array routine demands an
*    integer array.

      ELSE
         CALL NDF_MTYPE( '_INTEGER', NDF, NDF, COMP, ITYPE, DTYPE,
     :                   STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 980

*    Find whether or not there are but two significant dimensions and
*    which ones they are.  If there is only one (significant) dimension
*    the other will be a dummy dimension.

      CALL KPG1_SDIMP( NDF, NDIM, SDIM, STATUS )

*    Obtain the bounds of the image.  These will be stored in the
*    graphics database once the cell-array is displayed.

      CALL NDF_BOUND( NDF, NDF__MXDIM, LBND, UBND, NDIMS, STATUS )

*    Set upper insignificant bounds to one.  We have to make a section
*    so that trailing insignificant bounds may be shifted when the
*    user has specified the whole NDF.  This cannot be done for the base
*    NDF.  The section is also needed to permit a 1-dimensional NDF to
*    be displayed, as it allows NDF_ to make a dummy second axis.

      CALL NDF_SECT( NDF, MAX( NDIM, NDIMS ), LBND, UBND, NDFC, STATUS )
      CALL KPG1_SECSH( NDFC, SDIM( NDIM ), STATUS )

*    Must have a 2-d.  A bad status will be generated by NDF_BOUND
*    if there are greater than 2 significant dimensions.

      IF ( STATUS .EQ. NDF__XSDIM ) THEN
         CALL NDF_MSG( 'NDF', NDF )
         CALL ERR_REP( 'DISPLAY_IVDIM',
     :     'DISPLAY: NDF ^NDF is not two-dimensional.', STATUS )
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

*    Get the type of co-ordinates to input and place on axes.
*    ========================================================

*    Is there an axis system?

      CALL NDF_STATE( NDFC, 'Axis', DACOOR, STATUS )

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
         CALL KPG1_AXTYP( NDFC, 'Centre', ATYPE, STATUS )
         DPAXIS = ATYPE .EQ. '_DOUBLE'
      END IF

*    Map axes for data co-ordinates.
*    ===============================

      DATEMP = .TRUE.
      IF ( DACOOR ) THEN

*       Obtain the axes calling the appropriate routine depending on the
*       axis implementation type.

         IF ( DPAXIS ) THEN
            DO  I = 1, NDIM
               CALL NDF_AMAP( NDFC, 'Centre', SDIM( I ), '_DOUBLE',
     :                        'READ', AXPNTR( I ), AEL( I ), STATUS )

               IF ( AEL( I ) .GT. 1 ) THEN

*               Are all the axes monotonic?  Start a new error context
*               so that the error reports concerning a non-monotonic
*               axis may be annulled.  Instead we issue a warning
*               message so that the application can continue by using
*               world co-ordinates.

                  MONOTO = .TRUE.
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     CALL ERR_MARK
                     CALL KPG1_MONOD( .TRUE., AEL( I ),
     :                                %VAL( AXPNTR( I ) ), MONOTO,
     :                                STATUS )
                     IF ( STATUS .NE. SAI__OK ) THEN
                        CALL ERR_ANNUL( STATUS )
                        MONOTO = .FALSE.
                     END IF
                     CALL ERR_RLSE
                  END IF

*                Issue the warning.  Change the emphasis depending on
*                whether the co-ordinate system is DATA.

                  IF ( .NOT. MONOTO ) THEN
                     CALL MSG_SETI( 'IAXIS', SDIM( I ) )
                     IF ( DATACO ) THEN
                        CALL MSG_OUT( 'DISPLAY_NOTMONO1',
     :                    'DISPLAY: Axis ^IAXIS is not monotonic.  '/
     :                    /'Will use world co-ordinates instead.',
     :                    STATUS )
                     ELSE
                        CALL MSG_OUT( 'DISPLAY_NOTMONO2',
     :                    'DISPLAY: Axis ^IAXIS is not monotonic.  '/
     :                    /'Will not record axis bounds in the '/
     :                    /'graphics database.', STATUS )
                     END IF

*                   Record the fact.
                     DATEMP = .FALSE.

*                   Unmap the axis since we have finished with it.

                     CALL NDF_AUNMP( NDFC, 'Centre', SDIM( I ), STATUS )
                  END IF
               END IF

*             Axis has one element or is monotonic.

               IF ( DATEMP ) THEN

*                Find the range of the axis co-ordinates.

                  CALL KPG1_AXBND( AEL( I ), %VAL( AXPNTR( I ) ),
     :                             DXLBND( I ), DXUBND( I ), STATUS )

               END IF
            END DO

         ELSE
            DO  I = 1, NDIM
               CALL NDF_AMAP( NDFC, 'Centre', SDIM( I ), '_REAL',
     :                        'READ', AXPNTR( I ), AEL( I ), STATUS )

               IF ( AEL( I ) .GT. 1 ) THEN

*                Are all the axes monotonic?  Start a new error context
*                so that the error reports concerning a non-monotonic
*                axis may be annulled.  Instead we issue a warning
*                message so that the application can continue by using
*                world co-ordinates.

                  MONOTO = .TRUE.
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     CALL ERR_MARK
                     CALL KPG1_MONOR( .TRUE., AEL( I ),
     :                                %VAL( AXPNTR( I ) ), MONOTO,
     :                                STATUS )
                     IF ( STATUS .NE. SAI__OK ) THEN
                        CALL ERR_ANNUL( STATUS )
                        MONOTO = .FALSE.
                     END IF
                     CALL ERR_RLSE
                  END IF

*                Issue the warning.  Change the emphasis depending on
*                whether the co-ordinate system is DATA.

                  IF ( .NOT. MONOTO ) THEN
                     CALL MSG_SETI( 'IAXIS', SDIM( I ) )
                     IF ( DATACO ) THEN
                        CALL MSG_OUT( 'DISPLAY_NOTMONO1',
     :                   'DISPLAY: Axis ^IAXIS is not monotonic.  '/
     :                   /'Will use world co-ordinates instead.',
     :                   STATUS )
                     ELSE
                        CALL MSG_OUT( 'DISPLAY_NOTMONO2',
     :                   'DISPLAY: Axis ^IAXIS is not monotonic.  '/
     :                   /'Will not record axis bounds in the '/
     :                   /'graphics database.', STATUS )
                     END IF

*                   Record the fact.
                     DATEMP = .FALSE.
                  END IF
               END IF

*             Axis has one element or is monotonic.
               IF ( DATEMP ) THEN

*                Find the range of the axis co-ordinates.

                  CALL KPG1_AXBNR( AEL( I ), %VAL( AXPNTR( I ) ),
     :                             AXLBND( I ), AXUBND( I ), STATUS )
               END IF

*             Unmap the axis since we have finished with it. (Will use
*             double precision later.)

               CALL NDF_AUNMP( NDFC, 'Centre', SDIM( I ), STATUS )
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

      DEVCAN = .FALSE.

*    See whether current picture is to be refreshed or not.

      CALL PAR_GTD0L( 'CLEAR', .TRUE., .TRUE., CLEAR, STATUS )

*    Abort if something has gone wrong to avoid unrelated error
*    messages.

      IF ( STATUS .NE. SAI__OK ) GOTO 980

*    Associate a graphics device in the database.

      IF ( CLEAR ) THEN
         CALL AGS_ASSOC( 'DEVICE', 'WRITE', ' ', PICID1, ZONE1, STATUS )
      ELSE
         CALL AGS_ASSOC( 'DEVICE', 'UPDATE', ' ', PICID1, ZONE1,
     :                   STATUS )
      END IF

*    Check whether chosen device is an 'image display' with a suitable
*    minimum number of colour indices.

      CALL KPG1_QVID( 'DEVICE', 'SGS', 'IMAGE_DISPLAY,IMAGE_OVERLAY,'/
     :                /'WINDOW,MATRIX_PRINTER', ' ', MINCOL, STATUS )

*    Obtain the number of colour indices and the maximum display
*    surface.

      CALL KPG1_QIDAT( 'DEVICE', 'SGS', NINTS, IPIXX, IPIXY, STATUS )

*    Define the lowest pen number for display of the image.  0 is
*    reserved for the background.  Others are reserved for annotations.

      LP = CTM__RSVPN

*    Abort if the device is not suitable or something has gone wrong
*    associating the device.

      IF ( STATUS .NE. SAI__OK ) THEN

*       The device name is to be cancelled.

         DEVCAN = .TRUE.
         GOTO 960
      END IF

      PIXX = REAL( IPIXX )
      PIXY = REAL( IPIXY )

*    Obtain a lookup table and write it to the colour table.
*    =======================================================

*    Start a new error context.

      CALL ERR_MARK

*    Start an NDF context.

      CALL NDF_BEGIN
      
*    Obtain the NDF identifier and pointer of the input lookup table.
*    Validate the LUT.

      CALL KPG1_AVLUT( 'LUT', NDFL, LPNTR, LEL, STATUS )

*    Obtain the array dimensions.

      CALL NDF_DIM( NDFL, NDIM, LDIMS, NDIMS, STATUS )

*    Null status means do not read a lookup table.

      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*       Read the LUT.
*       =============

*       Map the lookup table to the colour table by interpolation or by
*       nearest neighbour?

         CALL PAR_GTD0L( 'NN', .FALSE., .TRUE., NN, STATUS )

*       If the structure was found then read in the lookup table.

         CALL KPG1_LUTIN( LDIMS( 2 ), %VAL( LPNTR( 1 ) ),
     :                    NINTS - CTM__RSVPN, NN, LUT, STATUS )

*       Install the lookup table into image-display colour table.
*       =========================================================

*       The lookup table follows the palette, hence the offset in the
*       colour index.

         CALL SGS_ICURW( WKID )
         DO  I = 0, NINTS - 1 - CTM__RSVPN, 1
            CALL GSCR( WKID, I + CTM__RSVPN, LUT( 1, I ), LUT( 2, I ),
     :                 LUT( 3, I ) )
         END DO
      END IF

*    End the NDF context.

      CALL NDF_END( STATUS )

*    End error context.

      CALL ERR_RLSE
      IF ( STATUS .NE. SAI__OK ) GOTO 960
 
*    Define the world co-ordinate system in device pixels.
*    =====================================================

*    Is the input picture the base zone?

      CALL AGI_INAME( PNAME, STATUS )
      CALL CHR_UCASE( PNAME )
      IF ( PNAME( 1:4 ) .EQ. 'BASE' ) THEN

*       It is, so some extra work is needed to define a suitable
*       world-co-ordinate system.

*       Inquire zone size.

         CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )

*       Adjust aspect ratio to avoid shrinkage.

         ASP = ( PIXX - 1.0 ) / ( PIXY - 1.0 )

*       Create and select a zone of the specified aspect ratio, centred
*       for nearly square devices.

         IF ( XM/YM .GT. 0.5 .AND. XM/YM .LT. 2.0 ) THEN
            CALL SGS_ZSHAP( ASP, 'CC', ZONE2, STATUS )
         ELSE
            CALL SGS_ZSHAP( ASP, 'BL', ZONE2, STATUS )
         END IF

*       Specify world co-ordinates so that data pixels map onto device
*       pixels.

         CALL SGS_SW( 0.0, PIXX, 0.0, PIXY, STATUS )

*       Inquire the new zone size.

         CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )

*       Inquire whether GKS/SGS has reported an error.

         CALL GKS_GSTAT( STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'DISPLAY_ZONE',
     :        'DISPLAY: Error while defining the shape or '/
     :        /'co-ordinates of the graphics zone on the '/
     :        /'image-display surface.', STATUS )

            CALL SGS_RELZ( ZONE2 )

*          Device name to be cancelled to prevent an invalid device
*          being stored as the current value.

            DEVCAN = .TRUE.
            GOTO 960

         END IF

*    The current picture is not the base zone, and so it will have its
*    own world co-ordinates.  In this application the pixels on the
*    display surface defines the standard co-ordinate system.
*    Therefore, the extents of the current picture in device pixels must
*    be found.

      ELSE

*       Need to find the normalised device co-ordinates of the current
*       picture...

         CALL GQCNTN( GSTAT, NTR )
         CALL GQNT( NTR, GSTAT, WINDOW, VIEWP )

*       to derive the bounds of the current picture in device pixels.

         X1 = VIEWP( 1 ) * PIXX
         X2 = VIEWP( 2 ) * PIXX
         Y1 = VIEWP( 3 ) * PIXX
         Y2 = VIEWP( 4 ) * PIXX

*       Redefine the world co-ordinates for the zone associated with the
*       input picture.

         CALL SGS_SW( X1, X2, Y1, Y2, STATUS )

*       Inquire whether GKS/SGS has reported an error.

         CALL GKS_GSTAT( STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'DISPLAY_CRPDC',
     :        'DISPLAY: Error while deriving the device co-ordinates '/
     :        /'of the current picture zone or resetting the world '/
     :        /'co-ordinates to them.', STATUS )

*          Device name to be cancelled to prevent an invalid device
*          being stored as the current value

            DEVCAN = .TRUE.
            GOTO 960

         END IF

*       Use the same zone identifier as for the base-picture case, for
*       later reference.

         ZONE2 = ZONE1
      END IF

*    Determine the type of scaling.
*    ==============================

*    Get the type of scaling to be done on the array. This digression
*    is because this can effect the bounds of the image zone (when
*    squashing is performed).

      IF ( SCALE ) THEN
         CALL PAR_CHOIC( 'MODE', 'Scale',
     :                   'Scale,Flash,Faint,Percentiles,Range,Sigma',
     :                   .TRUE., MODE, STATUS )
         CALL CHR_UCASE( MODE )
      ELSE
         MODE = ' '
      END IF

*    The array is to be inverted because the GKS convention is for the
*    origin to be at the top.

      INVERT = .TRUE.

*    Are annotated axes required?

      CALL PAR_GTD0L( 'AXES', .FALSE., .TRUE., AXES, STATUS )

*    Obtain parameters for an optional border.
*    =========================================

      IF ( AXES ) THEN

*       Axes and a border are mutually exclusive.

         BORDER = .FALSE.
      ELSE

*       Is a border required?

         CALL PAR_GTD0L( 'BORDER', .FALSE., .TRUE., BORDER, STATUS )
         IF ( BORDER ) THEN

*          It is so obtain their widths in device pixels.  If only one
*          value is supplied it applies in both dimensions, so must be
*          copied.

            CALL PAR_DEF1R( 'BWIDTH', 1, 4.0, STATUS )
            CALL PAR_GDRVR( 'BWIDTH', NDIM, 1.0, 20.0, BORWID, NVAL,
     :                      STATUS )
            IF ( NVAL .LT. NDIM ) THEN
               DO  I = NVAL + 1, NDIM
                  BORWID( I ) = BORWID( 1 )
               END DO
            END IF

*          Obtain the colour for the border as a colour index in the
*          palette.  

            CALL KPG1_MACOL( 'BCOLOUR', 0, CTM__RSVPN - 1, BORCI,
     :                       STATUS )
         END IF
      END IF

*    Obtain the colour for bad pixels as a colour index in the palette.

      CALL KPG1_MACOL( 'BADCOL', LP, NINTS - 1, BPCI, STATUS )

      IF ( STATUS .EQ. PAR__ABORT ) GOTO 960

*    Obtain the bounds of the image zone.
*    ====================================
*
*    Note all image-zone calculations must make some allowances for
*    the frame zone containing the axes or the border. However, the
*    default central pixel is the same.  This makes the display smaller
*    than default.

      IF ( AXES .OR. BORDER ) THEN

*       Just copy the NCAR borders for axes.

         IF ( AXES ) THEN
            ANCLIP( 1 ) = ANCLP1
            ANCLIP( 2 ) = ANCLP2
            ANCLIP( 3 ) = ANCLP3
            ANCLIP( 4 ) = ANCLP4
      
*          Find the scaling factor for cell array given axes compared
*          with no axes.  The difference between elements 2 and 1
*          (upper and lower x respectively) should be the same as that
*          between elements 4 and 3 (upper and lower y respectively) to
*          preserve square pixels.

            CONTRC = ANCLIP( 2 ) - ANCLIP( 1 )

*       The effect with borders depends on the dimensions of the
*       current zone, the widths of the borders and the aspect ratio
*       of the zone.  In order to prevent clipping of the borders ensure
*       that the new frame zone has at least sufficient room about it
*       and within the old frame zone to accommodate the border.

         ELSE
            ANCLIP( 1 ) = BORWID( 1 ) / ( X2 - X1 )
            ANCLIP( 2 ) = 1.0 - ANCLIP( 1 )
            ANCLIP( 3 ) = BORWID( 2 ) / ( Y2 - Y1 )
            ANCLIP( 4 ) = 1.0 - ANCLIP( 3 )
         END IF

*       When axes or borders are present room must be made for them,
*       and so the default magnification is reduced.  Make a new
*       "initial" image zone, but with an allowance for the axes or
*       borders.

         CALL KPS1_IMZBO( ANCLIP, ZONE1, STATUS )

*       Obtain the bounds of this new zone.

         CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )
      END IF

*    Find the half widths of the picture.

      HWX = 0.5 * ( X2 - X1 )
      HWY = 0.5 * ( Y2 - Y1 )

*    Find the centre of the picture in world co-ordinates.

      XCEN = 0.5 * ( X1 + X2 )
      YCEN = 0.5 * ( Y1 + Y2 )

*    Find the centre of the image in pixel co-ordinates.

      CENTRE( 1 ) = 0.5D0 * DBLE( SUBND( 1 ) + SLBND( 1 ) - 1 )
      CENTRE( 2 ) = 0.5D0 * DBLE( SUBND( 2 ) + SLBND( 2 ) - 1 )

*    Determine if the array is to fill the image zone.
      CALL PAR_GTD0L( 'FILL', .FALSE., .TRUE., FILL, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 960

*    Just use the centre of the image for the FILL option.

      IF ( .NOT. FILL ) THEN

*       Find the limiting positions for the centre of the displayed image.

         IF ( DATACO ) THEN

*       Since we are using data co-ordinates define the centre limits as
*       the axis bounds, converting to double precision where necessary,
*       as the parameter to be obained is d.p.

            DO  I = 1, NDIM
               IF ( DPAXIS ) THEN
                  CENMIN( I ) = MIN( DXLBND( I ), DXUBND( I ) )
                  CENMAX( I ) = MAX( DXLBND( I ), DXUBND( I ) )
               ELSE
                  CENMIN( I ) = DBLE( MIN( AXLBND( I ), AXUBND( I ) ) )
                  CENMAX( I ) = DBLE( MAX( AXLBND( I ), AXUBND( I ) ) )

*                Need the axis to be remapped as double precision for
*                the section.

                  CALL NDF_AMAP( NDFC, 'Centre', SDIM( I ), '_DOUBLE',
     :                          'READ', AXPNTR( I ), AEL( I ), STATUS )
               END IF

*             Find the centre of the image in data co-ordinates.

               CDUMMY = CENTRE( I )
               CALL KPG1_AXVLD( SLBND( I ), SUBND( I ),
     :                          %VAL( AXPNTR( I ) ), 1, CDUMMY,
     :                          CENTRE( I ), STATUS )
            END DO

         ELSE

*          As far as the user is concerned the centre is in pixel
*          indices.  However internally the application uses image
*          co-ordinates.  (The 0.5's are to convert to pixel indices.)
*          The default centre of the image zone will be the centre of
*          the image.  Since the magnification has to be known to
*          define the limits for the centre position, and to compute
*          the default magnification the centre has to be known, we
*          have to make a compromise.  Therefore the limits for the
*          centre assume maximum magnification.

            CENMIN( 1 ) = CENTRE( 1 ) - MXMAGN * HWX + 0.5
            CENMIN( 2 ) = CENTRE( 2 ) - MXMAGN * HWY + 0.5
            CENMAX( 1 ) = CENTRE( 1 ) + MXMAGN * HWX + 0.5
            CENMAX( 2 ) = CENTRE( 2 ) + MXMAGN * HWY + 0.5
         END IF

*    Set the suggested default to be the centre of the picture.

         CENDEF( 1 ) = CENTRE( 1 )
         CENDEF( 2 ) = CENTRE( 2 )

*    Ask where the plot is to be positioned.

         CALL PAR_GRM1D( 'CENTRE', NDIM, CENDEF, CENMIN, CENMAX,
     :                   .FALSE., CENTRE, STATUS )

*    If the centre was given in data co-ordinates, these must be
*    converted to pixel co-ordinates for each dimension.  First convert
*    them to pixel indices.  Note the special case of a single-pixel
*    axis as the CENTRE value is known by definition, and it must be
*    treated separately to avoid an error from KPG1_AINDD.

         IF ( DATACO ) THEN
            DO  I = 1, NDIM
               IF ( SLBND( I ) .EQ. SUBND( I ) ) THEN
                  CENTRE( I ) = DBLE( SLBND( I ) )

               ELSE IF ( STATUS .EQ. SAI__OK ) THEN
                  CDUMMY = CENTRE( I )
                  CALL KPG1_AINDD( SLBND( I ), SUBND( I ),
     :                             %VAL( AXPNTR( I ) ),
     :                             1, CDUMMY, CENTRE( I ), STATUS )

*             Add a contextual error to clarify what might be a puzzling
*             message from the above routine.  Puzzling because the user
*             might not be aware of using data co-ordinates.

                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL ERR_REP( 'DISPLAY_PECAXS',
     :                 'DISPLAY:  A peculiar set of axis centres was '/
     :                 /'encountered, that makes it impossible to '/
     :                 /'convert the data co-ordinates of the centre '/
     :                 /'to pixel indices.', STATUS )
                  END IF
               END IF

*             Unmap the axis since we have finished with it.  A section
*             of the axis will be obtained later once the size of the
*             section to display is known.

               CALL NDF_AUNMP( NDFC, 'Centre', SDIM( I ), STATUS )
            END DO

*    Exit if there was an error performing the conversion.
            IF ( STATUS .NE. SAI__OK ) GOTO 960
         END IF
      END IF

*    Convert to image co-ordinates from pixel indices.  With the fill
*    option, the centre is already given pixel co-ordinates.
      IF ( FILL ) THEN
         XCENTR = REAL( CENTRE( 1 ) )
         YCENTR = REAL( CENTRE( 2 ) )
      ELSE
         XCENTR = REAL( CENTRE( 1 ) ) - 0.5
         YCENTR = REAL( CENTRE( 2 ) ) - 0.5
      END IF
 
*    Find distances to the edge of the image from the chosen image
*    centre in image co-ordinates.

      XCTLE = XCENTR - REAL( SLBND( 1 ) - 1 )
      XCTRE = REAL( SUBND( 1 ) ) - XCENTR
      YCTLE = YCENTR - REAL( SLBND( 2 ) - 1 )
      YCTUE = REAL( SUBND( 2 ) ) - YCENTR

*   Derive the magnifications when the plot is filled. 
      IF ( FILL ) THEN
         XMAGN = 2.0 * HWX / REAL( DIMS( 1 ) )
         YMAGN = 2.0 * HWY / REAL( DIMS( 2 ) )

*    Obtain the magnifications.

      ELSE

*       Set the default magnification so that the whole picture is
*       displayed, and the picture just fills the display surface in
*       one dimension.

*       First check for singularities whereupon set the magnification
*       to an arbitrary value.  Negative offsets are allowed, i.e. the
*       centre pixel can lie outside the actual array.  This is needed
*       to give full positional control, though may give an empty
*       picture if the user does not select the centre pixel carefully.

         IF ( ABS( XCTLE ) .LT. 1./MXMAGN .OR.
     :        ABS( YCTLE ) .LT. 1./MXMAGN .OR.
     :        ABS( XCTRE ) .LT. 1./MXMAGN .OR.
     :        ABS( YCTUE ) .LT. 1./MXMAGN ) THEN
            DEFMAG = 1.0

         ELSE

*          Evaluate the default magnification by finding the side of
*          the image that will touch an edge of the current picture.
*          It must be protected against negative offsets when the
*          centre lies outside the array.  Note clipping can occur due
*          to rounding, so the default magnification is slightly
*          reduced.

            DEFMAG = MXMAGN
            IF ( XCTLE .GT. 0 ) DEFMAG = MIN( HWX / XCTLE, DEFMAG )
            IF ( YCTLE .GT. 0 ) DEFMAG = MIN( HWY / YCTLE, DEFMAG )
            IF ( XCTRE .GT. 0 ) DEFMAG = MIN( HWX / XCTRE, DEFMAG )
            IF ( YCTUE .GT. 0 ) DEFMAG = MIN( HWY / YCTUE, DEFMAG )
            DEFMAG = DEFMAG * 0.999998
         END IF

*    Ask how much the plot is to be magnified.

         CALL PAR_GDR0R( 'XMAGN', DEFMAG, 1./MXMAGN, MXMAGN, .TRUE.,
     :                   XMAGN, STATUS )
         CALL PAR_GDR0R( 'YMAGN', XMAGN, 1./MXMAGN, MXMAGN, .TRUE.,
     :                   YMAGN, STATUS )
      END IF

      IF ( STATUS .EQ. PAR__ABORT ) GOTO 960

*    Define the default limiting positions of the cell array in world
*    co-ordinates.

      XL = XCEN - XMAGN * XCTLE
      YL = YCEN - YMAGN * YCTLE
      XU = XCEN + XMAGN * XCTRE
      YU = YCEN + YMAGN * YCTUE

*    If the array is too large to fit onto the screen and the
*    magnification is unity then ask whether the array is to be
*    squashed.

      IF ( ( ( DIMS( 1 ) .GT. 2.0 * HWX .AND.
     :         ABS(XMAGN - 1.0) .LT. VAL__SMLR )
     :   .OR. ( DIMS( 2 ) .GT. 2.0 * HWY .AND.
     :         ABS(YMAGN - 1.0) .LT. VAL__SMLR ) )
     :   .AND. MODE(1:2) .NE. 'FL' ) THEN

*       Get squash option.

         CALL PAR_GTD0L( 'SQUASH', .TRUE., .TRUE., ANS, STATUS )

*       If the user wants a squashed array, then display a
*       squashed array by defining the corners such that there
*       is no distortion.

         IF ( ANS ) THEN
            FACTOR = 2.0 * MAX( HWX, HWY ) /
     :               REAL(  MAX( DIMS( 1 ), DIMS( 2 ) )  )

            XL = XCEN - FACTOR * XCTLE
            YL = YCEN - FACTOR * YCTLE
            XU = XCEN + FACTOR * XCTRE
            YU = YCEN + FACTOR * YCTUE
         END IF
      END IF

*    Create a new zone to contain the picture.  Allow for rounding 
*    errors.

      XLC = MAX( 1.000001 * X1, XL )
      YLC = MAX( 1.000001 * Y1, YL )
      XUC = MIN( 0.999999 * X2, XU )
      YUC = MIN( 0.999999 * Y2, YU )
      CALL SGS_ZONE( XLC, XUC, YLC, YUC, ZONEI, STATUS )
      CALL SGS_SW( XLC, XUC, YLC, YUC, STATUS )

*    Inquire whether GKS/SGS has reported an error

      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( STATUS .NE. PAR__ABORT .AND. STATUS .NE. PAR__NULL ) THEN
            CALL ERR_REP( 'DISPLAY_ZONEI',
     :       'DISPLAY: Error while defining the shape or co-ordinates '/
     :        /'of the picture zone.  This is probably due to an '/
     :        /'incorrect combination of magnification and centering.',
     :        STATUS )
            CALL ERR_REP( 'DISPLAY_ZONEIC',
     :        'If the centering is correct, try increasing the '/
     :        /'magnification so that some of the image is visible in '/
     :        /'the current picture, or define a new current picture.',
     :        STATUS )
         END IF

*       Device name to be cancelled to prevent an invalid device being
*       stored as the current value

         DEVCAN = .TRUE.
         GOTO 960
      END IF

*    Find the bounds of the zone in data-array pixels.
*    =================================================
*
*    Compute the boundaries of the zone, but instead of being in
*    image-display pixels, define them in the reference frame
*    of the data-array elements.  When the current zone is stored
*    in the database it will have the data-array co-ordinates.  Thus
*    any offset or magnification is included.  Explicitly check for
*    the special cases when edges of the whole image are visible to
*    avoid rounding errors.

*    Find semi-arbitrary rounding correction.  It allows for the
*    number of mathematical operations performed.

      ROUNDL = 1 - 4.0 * VAL__EPSR
      ROUNDU = 1 + 4.0 * VAL__EPSR

*    Lower x bound.

      IF ( XL .GT. XLC * ROUNDL ) THEN
         XLP = REAL( SLBND( 1 ) ) - 1.0
      ELSE
         XLP = XCENTR - ( XCEN - XLC ) / XMAGN
      END IF

*    Upper x bound.

      IF ( XU .LE. XUC * ROUNDU ) THEN
         XUP = REAL( SUBND( 1 ) )
      ELSE
         XUP = XCENTR + ( XUC - XCEN ) / XMAGN
      END IF

*    Lower y bound.

      IF ( YL .GE. YLC * ROUNDL ) THEN
         YLP = REAL( SLBND( 2 ) ) - 1.0
      ELSE
         YLP = YCENTR - ( YCEN - YLC ) / YMAGN
      END IF

*    Upper y bound.

      IF ( YU .LE. YUC * ROUNDU ) THEN
         YUP = REAL( SUBND( 2 ) )
      ELSE
         YUP = YCENTR + ( YUC - YCEN ) / YMAGN
      END IF

*    Define frame zone and axes.
*    ===========================

      IF ( AXES .OR. BORDER ) THEN

*       Select the initial zone as the new zone will be larger than
*       the current image zone. (The initial zone is the input zone
*       when the input picture is not the base.)

         CALL SGS_SELZ( ZONE2, STATUS )

         IF ( AXES ) THEN

*          Define its bounds in the grid reference frame, i.e. the
*          reduced image zone.  First the lower-left corner.  For axes
*          the contraction will be the same for both dimensions.

            T1 = XLC - ANCLIP( 1 ) / CONTRC * ( XUC - XLC )
            T2 = YLC - ANCLIP( 3 ) / CONTRC * ( YUC - YLC )

*          Now convert these to the co-ordinates in the "initial" zone.

            CALL SGS_TPZ( ZONEI, T1, T2, ZONE2, XLF, YLF, STATUS )

*          Similarly for top-right corner.

            T1 = XUC + ( 1.0 - ANCLIP( 2 ) ) / CONTRC * ( XUC - XLC )
            T2 = YUC + ( 1.0 - ANCLIP( 4 ) ) / CONTRC * ( YUC - YLC )
            CALL SGS_TPZ( ZONEI, T1, T2, ZONE2, XUF, YUF, STATUS )

*       Make the frame zone a border width bigger than the image zone.

         ELSE IF ( BORDER ) THEN
            XLF = XLC - BORWID( 1 )
            XUF = XUC + BORWID( 1 )
            YLF = YLC - BORWID( 2 )
            YUF = YUC + BORWID( 2 )
         END IF

*       Create the zone (remember initial zone has device-pixel
*       co-ordinates.)

         CALL SGS_ZONE( XLF, XUF, YLF, YUF, ZONEF, STATUS )
         CALL SGS_SW( XLF, XUF, YLF, YUF, STATUS )

*       Store the current zone (which is already the correct shape) as
*       the frame.  It will have unnormalised device (pixel)
*       co-ordinates.

         CALL AGS_SZONE( 'FRAME', 'KAPPA_DISPLAY', PICIDF, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'DISPLAY_DBSF',
     :        'DISPLAY: Error while storing the frame in the graphics '/
     :        /'database.', STATUS )
            GOTO 960
         END IF
      END IF

      IF ( AXES ) THEN

*       Obtain a title for the plot.
*       ============================

         CALL KPG1_GNTIT( NDF, 'PLTITL', ' ', PLTITL, STATUS )

*       Obtain axis labels.
*       ===================

         IF ( DATACO ) THEN

*          A null value causes the default to be chosen, namely, 'X' for
*          the abscissa...

*          Get the abscissa and ordinate labels suggesting the value in
*          the NDF axis structure, if present, as the default.

            CALL KPG1_GAXLB( NDFC, SDIM( 1 ), 'ABSLAB', 'X', ABSLAB,
     :                       STATUS )
            CALL KPG1_GAXLB( NDFC, SDIM( 2 ), 'ORDLAB', 'Y', ORDLAB,
     :                       STATUS )

         ELSE

*          Get the abscissa and ordinate labels without consulting the
*          NDF's axis structure to prevent the wrong label being
*          associated with the world co-ordinates.  The suggested
*          defaults are 'X' and 'Y'.

            CALL PAR_DEF0C( 'ABSLAB', 'X', STATUS )
            CALL PAR_GET0C( 'ABSLAB', ABSLAB, STATUS )
            CALL PAR_DEF0C( 'ORDLAB', 'Y', STATUS )
            CALL PAR_GET0C( 'ORDLAB', ORDLAB, STATUS )
         END IF

*       Get the style of the axes.
*       ==========================

*       Get the number of minor ticks, assigning the dynamic defaults.

         TICDEF( 1 ) = -1.
         TICDEF( 2 ) = -1.
         CALL PAR_GDR1R( 'MINTIC', 2, TICDEF, -1., VAL__MAXR, .FALSE.,
     :                   MINTIC, STATUS )

*       Get the parameter controlling the number of major ticks per
*       axis.

*       Get the parameter controlling the number of major ticks per
*       axis, assigning the dynamic defaults.

         TICDEF( 1 ) = 3.
         TICDEF( 2 ) = 3.
         CALL PAR_GDR1R( 'MAJTIC', 2, TICDEF, -1., VAL__MAXR, .FALSE.,
     :                   MAJTIC, STATUS )

*       Are the tick marks on the outside of the axes?  Default is 
*       outside so that they do not hide any of the image.

         CALL PAR_GTD0L( 'OUTTIC', .TRUE., .TRUE., OUTTIC, STATUS )

*       Get the line thickness.

         CALL PAR_GDR0R( 'THICK', 1.0, 0.5, 5.0, .TRUE., THICK, STATUS )

*       Get the fount.  Although NCAR is the default, either must be
*       selected to prevent persistence from earlier invocations.

         CALL PAR_CHOIC( 'FONT', 'GKS', 'GKS,NCAR', .TRUE., FOUNT,
     :                   STATUS )
         IF ( FOUNT .EQ. 'GKS ' ) THEN
            CALL AGPWRT( 0.0, 0.0, ' ', 0, 0, 0, -100 )
         ELSE IF ( FOUNT .EQ. 'NCAR' ) THEN
            CALL AGPWRT( 0.0, 0.0, ' ', 0, 0, 0, 100 )
         END IF

*       Abort if something has gone wrong.

         IF ( STATUS .NE. SAI__OK ) GOTO 960

*       End of the section to define the axis style and annotations.
*       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

*       We would like to plot the axes at this point, but if we
*       did, the border and interior tick marks would be overwritten
*       by the cell-array image.

*       Return to the image zone to plot the cell array.

         CALL SGS_SELZ( ZONEI, STATUS )
      END IF

*    Abort if something has gone wrong.

      IF ( STATUS .NE. SAI__OK ) GOTO 960

*    Access only the parts of the image that will be visible.
*    ========================================================

*    Compute new array bounds.

      LBND( SDIM( 1 ) ) = MAX( SLBND( 1 ), INT( XLP ) )
      LBND( SDIM( 2 ) ) = MAX( SLBND( 2 ), INT( YLP ) )
      UBND( SDIM( 1 ) ) = MIN( SUBND( 1 ), INT( XUP ) + 1 )
      UBND( SDIM( 2 ) ) = MIN( SUBND( 2 ), INT( YUP ) + 1 )

      SLBND( 1 ) = LBND( SDIM( 1 ) )
      SLBND( 2 ) = LBND( SDIM( 2 ) )
      SUBND( 1 ) = UBND( SDIM( 1 ) )
      SUBND( 2 ) = UBND( SDIM( 2 ) )

      XLT = REAL( SLBND( 1 ) - 1 )
      XUT = REAL( SUBND( 1 ) )
      YLT = REAL( SLBND( 2 ) - 1 )
      YUT = REAL( SUBND( 2 ) )

*    Compute the new dimensions.

      DIMS( 1 ) = SUBND( 1 ) - SLBND( 1 ) + 1
      DIMS( 2 ) = SUBND( 2 ) - SLBND( 2 ) + 1

*    Create a section from the input (full-sized) NDF section that
*    has any insignificant bounds shifted; otherwise it is possible
*    for the array to be filled with bad values, simply because the
*    chosen section lies outside the bounds of the original section.

      CALL NDF_SECT( NDFC, SDIM( NDIM ), LBND, UBND, NDFS, STATUS )

*    Block average the array to be displayed.
*    ========================================
*
*    For images much larger than the current picture size (in device
*    pixels) the resolution of the device will allow only a fraction
*    of the detail in the array to be plotted.  If the ratio of the
*    array size to picture size is greater than two the array can be
*    compressed by averaging.  This saves time scaling the data and
*    transmitting them to the image display.
*
*    First find whether compression is possible.  The compression factor
*    depends on the inverse magnification, and is given by the next
*    lowest positive integer.   Compression is possible when the factor
*    is two or greater.

      BLAVF( 1 ) = INT( MIN ( 1.0 / XMAGN, 1.0 / YMAGN ) )
      BLAVF( 2 ) = BLAVF( 1 )
      BLOCK = .FALSE.

      IF ( BLAVF( 1 ) .GT. 1 ) THEN

*       The compression routine can only cope with floating-point
*       data, so the image section may be converted during mapping.
*       Find the implementation type to be used.

         CALL ERR_MARK
         CALL NDF_MTYPE( '_REAL,_DOUBLE', NDFS, NDFS, COMP, ITYPE,
     :                   DTYPE, STATUS )
         IF ( STATUS .EQ. NDF__TYPNI ) CALL ERR_ANNUL( STATUS )
         CALL ERR_RLSE

*       Map the input image.

         CALL KPG1_MAP( NDFS, MCOMP, ITYPE, 'READ', PNTRI, EL, STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 960

*       Find the size of the output array.  For the moment don't
*       worry about rounding effects, which could make the array's
*       aspect ratio change after compression.

         CDIMS( 1 ) = DIMS( 1 ) / BLAVF( 1 )
         CDIMS( 2 ) = DIMS( 2 ) / BLAVF( 2 )
         CEL = CDIMS( 1 ) * CDIMS( 2 )

*       Create workspace: three are needed---one for the compressed
*       array, and the other two are needed to perform the averaging 
*       calculations.

         CALL PSX_CALLOC( CEL, ITYPE, SAPNT, STATUS )
         CALL PSX_CALLOC( DIMS( 1 ), ITYPE, SAPNT1, STATUS )
         CALL PSX_CALLOC( DIMS( 1 ), '_INTEGER', SAPNT2, STATUS )

*       Something has gone wrong, so let the user know the context.
*       Tidy the workspace.

         IF ( STATUS .NE. SAI__OK ) THEN

            CALL ERR_REP( 'DISPLAY_WSP2',
     :        'DISPLAY: Unable to get workspace to compress the '/
     :        /'array', STATUS )

            CALL PSX_FREE( SAPNT2, STATUS )
            CALL PSX_FREE( SAPNT1, STATUS )
            CALL PSX_FREE( SAPNT, STATUS )
            GOTO 960
         END IF

*       Do the compression calling the appropriate routine for the 
*       implementation type.  Any bad pixels are not necessarily
*       propagated to the averaged array unless all pixels in a
*       BLAVF*BLAVF-sized bin are bad.

         IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPG1_CMAVR( NDIM, DIMS, %VAL( PNTRI( 1 ) ), BLAVF, 1,
     :                       %VAL( SAPNT ), %VAL( SAPNT1 ),
     :                       %VAL( SAPNT2 ), STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG1_CMAVD( NDIM, DIMS, %VAL( PNTRI( 1 ) ), BLAVF, 1,
     :                       %VAL( SAPNT ), %VAL( SAPNT1 ),
     :                       %VAL( SAPNT2 ), STATUS )

         END IF

*       Tidy the workspace.

         CALL PSX_FREE( SAPNT2, STATUS )
         CALL PSX_FREE( SAPNT1, STATUS )

*       Unmap the input array.

         CALL NDF_UNMAP( NDFS, COMP, STATUS )

*       Fool the rest of the application into thinking the work array
*       of averaged pixels is the input array.  However, a flag is
*       retained so that the work array may be tidied at the end.

         PNTRI( 1 ) = SAPNT
         BLOCK = .TRUE.

*       The number of elements in compressed array also has to be 
*       adjusted for the trick.

         EL = CDIMS( 1 ) * CDIMS( 2 )
         DIMS( 1 ) = CDIMS( 1 )
         DIMS( 2 ) = CDIMS( 2 )

*    No compression.

      ELSE

*       Map the image with the original implementation type.

         CALL KPG1_MAP( NDFS, MCOMP, ITYPE, 'READ', PNTRI, EL, STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 960
      END IF

*    Create a scratch area in which to put the scaled array.
*    =======================================================

*    Not required if the input array is to be used.

      IF ( SCALE ) THEN
         CALL PSX_CALLOC( EL, '_INTEGER', SCRPNT, STATUS )

*       Something has gone wrong, so let the user know the context.

         IF ( STATUS .NE. SAI__OK ) THEN

            CALL ERR_REP( 'DISPLAY_WSP',
     :        'DISPLAY: Unable to get workspace to scale the array.',
     :        STATUS )

            CALL PSX_FREE( SCRPNT, STATUS )
            GOTO 940
         END IF
      END IF


*    Faint (positive) display of the array.
*    ======================================

      IF ( MODE(1:2) .EQ. 'FA' .OR. MODE(1:2) .EQ. 'SI' ) THEN

*       Obtain the standard-deviation limits if not predefined.  There
*       is no dynamic default.

         IF ( MODE(1:2) .EQ. 'SI' ) THEN
            SIGDEF( 1 ) = VAL__BADR
            SIGDEF( 2 ) = VAL__BADR
            CALL PAR_GDR1R( 'SIGMAS', 2, SIGDEF, -1000., 10000.,
     :                      .FALSE., SIGRNG, STATUS )

         ELSE IF ( MODE(1:2) .EQ. 'FA' ) THEN

*           Fixed range in standard-deviation units.

             SIGRNG( 1 ) = -1.0
             SIGRNG( 2 ) = 7.0
         END IF

*       Select appropriate routine for the data type chosen and scale
*       the image between the standard-deviation limits into the cell
*       array.  The cell array has values between the colour-index
*       limits LP and the largest colour index for the device.  (This
*       will change when a colour-table management scheme is
*       introduced.)
*       =============================================================

         IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPS1_FAINR( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), SIGRNG, LP, NINTS-1,
     :                       BPCI, INVERT, %VAL( SCRPNT ), RIMLO,
     :                       RIMHI, STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPS1_FAIND( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), SIGRNG, LP, NINTS-1,
     :                       BPCI, INVERT, %VAL( SCRPNT ), DIMLO,
     :                       DIMHI, STATUS )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
            CALL KPS1_FAINI( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), SIGRNG, LP, NINTS-1,
     :                       BPCI, INVERT, %VAL( SCRPNT ), IIMLO,
     :                       IIMHI, STATUS )

         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
            CALL KPS1_FAINW( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), SIGRNG, LP, NINTS-1,
     :                       BPCI, INVERT, %VAL( SCRPNT ), WIMLO,
     :                       WIMHI, STATUS )

         ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN
            CALL KPS1_FAINB( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), SIGRNG, LP, NINTS-1,
     :                       BPCI, INVERT, %VAL( SCRPNT ), BIMLO,
     :                       BIMHI, STATUS )

         END IF

*    Do scaling with extreme values in the array as the limits.
*    ==========================================================

      ELSE IF ( MODE(1:2) .EQ. 'RA' ) THEN

*       Select appropriate routine for the data type chosen and scale
*       the image between user-defined limits.   The cell array
*       has values between the colour-index limits LP and the largest
*       colour index for the device.
*       =============================================================

         IF ( ITYPE .EQ. '_REAL' ) THEN

*          Obtain the maximum and minimum values.

            CALL KPG1_MXMNR( BAD, EL, %VAL( PNTRI( 1 ) ), NINVAL,
     :                       RIMHI, RIMLO, MAXPOS, MINPOS, STATUS )

*          The number of bad pixels has been counted so it might be
*          possible to save future processing.

            BAD = BAD .OR. ( NINVAL .EQ. 0 )

*          Report the scaling limits for future use.

            CALL MSG_SETR( 'MINVAL', RIMLO )
            CALL MSG_SETR( 'MAXVAL', RIMHI )
            CALL MSG_OUT( 'PVLO', 'Data will be scaled from ^MINVAL '/
     :                    /'to ^MAXVAL.', STATUS )

*          Scale the data values using the extreme values into the
*          cell array.

            CALL KPG1_ISCLR( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), INVERT, RIMLO, RIMHI,
     :                       LP, NINTS-1, BPCI, %VAL( SCRPNT ), STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN

*          Obtain the maximum and minimum values.

            CALL KPG1_MXMND( BAD, EL, %VAL( PNTRI( 1 ) ), NINVAL,
     :                       DIMHI, DIMLO, MAXPOS, MINPOS, STATUS )

*          The number of bad pixels has been counted so it might be
*          possible to save future processing.

            BAD = BAD .OR. ( NINVAL .EQ. 0 )

*          Report the scaling limits for future use.

            CALL MSG_SETD( 'MINVAL', DIMLO )
            CALL MSG_SETD( 'MAXVAL', DIMHI )
            CALL MSG_OUT( 'PVLO', 'Data will be scaled from ^MINVAL '/
     :                    /'to ^MAXVAL.', STATUS )

*          Scale the data values using the extreme values into the
*          cell array.

            CALL KPG1_ISCLD( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), INVERT, DIMLO, DIMHI,
     :                       LP, NINTS-1, BPCI, %VAL( SCRPNT ), STATUS )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN

*          Obtain the maximum and minimum values.

            CALL KPG1_MXMNI( BAD, EL, %VAL( PNTRI( 1 ) ), NINVAL,
     :                       IIMHI, IIMLO, MAXPOS, MINPOS, STATUS )

*          The number of bad pixels has been counted so it might be
*          possible to save future processing.

            BAD = BAD .OR. ( NINVAL .EQ. 0 )

*          Report the scaling limits for future use.

            CALL MSG_SETI( 'MINVAL', IIMLO )
            CALL MSG_SETI( 'MAXVAL', IIMHI )
            CALL MSG_OUT( 'PVLO', 'Data will be scaled from ^MINVAL '/
     :                    /'to ^MAXVAL.', STATUS )

*          Scale the data values using the extreme values into the
*          cell array.

            CALL KPG1_ISCLI( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), INVERT, IIMLO, IIMHI,
     :                       LP, NINTS-1, BPCI, %VAL( SCRPNT ), STATUS )

         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN

*          Obtain the maximum and minimum values.

            CALL KPG1_MXMNW( BAD, EL, %VAL( PNTRI( 1 ) ), NINVAL,
     :                       WIMHI, WIMLO, MAXPOS, MINPOS, STATUS )

*          The number of bad pixels has been counted so it might be
*          possible to save future processing.

            BAD = BAD .OR. ( NINVAL .EQ. 0 )

*          Report the scaling limits for future use.

            CALL MSG_SETI( 'MINVAL', NUM_WTOI( WIMLO ) )
            CALL MSG_SETI( 'MAXVAL', NUM_WTOI( WIMHI ) )
            CALL MSG_OUT( 'PVLO', 'Data will be scaled from ^MINVAL '/
     :                    /'to ^MAXVAL.', STATUS )

*          Scale the data values using the extreme values into the
*          cell array.

            CALL KPG1_ISCLW( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), INVERT, WIMLO, WIMHI,
     :                       LP, NINTS-1, BPCI, %VAL( SCRPNT ), STATUS )

         ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN

*          Obtain the maximum and minimum values.

            CALL KPG1_MXMNB( BAD, EL, %VAL( PNTRI( 1 ) ), NINVAL,
     :                       BIMHI, BIMLO, MAXPOS, MINPOS, STATUS )

*          The number of bad pixels has been counted so it might be
*          possible to save future processing.

            BAD = BAD .OR. ( NINVAL .EQ. 0 )

*          Report the scaling limits for future use.

            CALL MSG_SETI( 'MINVAL', NUM_BTOI( BIMLO ) )
            CALL MSG_SETI( 'MAXVAL', NUM_BTOI( BIMHI ) )
            CALL MSG_OUT( 'PVLO', 'Data will be scaled from ^MINVAL '/
     :                    /'to ^MAXVAL.', STATUS )

*          Scale the data values using the extreme values into the
*          cell array.

            CALL KPG1_ISCLB( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), INVERT, BIMLO, BIMHI,
     :                       LP, NINTS-1, BPCI, %VAL( SCRPNT ), STATUS )
         END IF

*    Do scaling with limits from the environment.
*    ============================================

      ELSE IF ( MODE(1:2) .EQ. 'SC' ) THEN

*       Determine whether or not the scaling parameters have been
*       found, to avoid finding the maximum and minimum values when
*       they are not required.

         CALL PAR_STATE( 'LOW', ACTLOW, STATUS )
         CALL PAR_STATE( 'HIGH', ACTHIG, STATUS )
         FNDRNG = ACTLOW .EQ. SUBPAR__ACTIVE .AND.
     :            ACTHIG .EQ. SUBPAR__ACTIVE

*       Select appropriate routine for the data type chosen and scale
*       the image between user-defined limits.  The cell array
*       has values between the colour-index limits LP and the largest
*       colour index for the device.
*       =============================================================

         IF ( ITYPE .EQ. '_REAL' ) THEN

*          Set the scaling limits to the extreme values.

            IF ( FNDRNG ) THEN
               RMINV = VAL__MINR
               RMAXV = VAL__MAXR
            ELSE

*             Obtain the maximum and minimum values to be used as
*             dynamic defaults.

               CALL KPG1_MXMNR( BAD, EL, %VAL( PNTRI( 1 ) ), NINVAL,
     :                          RMAXV, RMINV, MAXPOS, MINPOS, STATUS )

*             The number of bad pixels has been counted so it might be
*             possible to save future processing.

               BAD = BAD .OR. ( NINVAL .EQ. 0 )
            END IF

*          Obtain the scaling limits from the environment and do the
*          scaling into the cell array.

            CALL KPS1_DSCLR( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), INVERT, 'LOW', 'HIGH',
     :                       LP, NINTS-1, BPCI, RMINV, RMAXV, .TRUE.,
     :                       %VAL( SCRPNT ), RIMLO, RIMHI, STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN

*          Set the scaling limits to the extreme values.

            IF ( FNDRNG ) THEN
               DMINV = VAL__MIND
               DMAXV = VAL__MAXD
            ELSE

*             Obtain the maximum and minimum values to be used as
*             dynamic defaults.

               CALL KPG1_MXMND( BAD, EL, %VAL( PNTRI( 1 ) ), NINVAL,
     :                          DMAXV, DMINV, MAXPOS, MINPOS, STATUS )

*             The number of bad pixels has been counted so it might be
*             possible to save future processing.

               BAD = BAD .OR. ( NINVAL .EQ. 0 )
            END IF

*          Obtain the scaling limits from the environment and do the
*          scaling into the cell array.

            CALL KPS1_DSCLD( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), INVERT, 'LOW', 'HIGH',
     :                       LP, NINTS-1, BPCI, DMINV, DMAXV, .TRUE.,
     :                       %VAL( SCRPNT ), DIMLO, DIMHI, STATUS )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN

*          Set the scaling limits to the extreme values.

            IF ( FNDRNG ) THEN
               IMINV = VAL__MINI
               IMAXV = VAL__MAXI
            ELSE

*             Obtain the maximum and minimum values to be used as
*             dynamic defaults.

               CALL KPG1_MXMNI( BAD, EL, %VAL( PNTRI( 1 ) ), NINVAL,
     :                          IMAXV, IMINV, MAXPOS, MINPOS, STATUS )

*             The number of bad pixels has been counted so it might be
*             possible to save future processing.

               BAD = BAD .OR. ( NINVAL .EQ. 0 )
            END IF

*          Obtain the scaling limits from the environment and do the
*          scaling into the cell array.

            CALL KPS1_DSCLI( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), INVERT, 'LOW', 'HIGH',
     :                       LP, NINTS-1, BPCI, IMINV, IMAXV, .TRUE.,
     :                       %VAL( SCRPNT ), IIMLO, IIMHI, STATUS )

         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN

*          Set the scaling limits to the extreme values.

            IF ( FNDRNG ) THEN
               WMINV = VAL__MINW
               WMAXV = VAL__MAXW
            ELSE

*             Obtain the maximum and minimum values to be used as
*             dynamic defaults.

               CALL KPG1_MXMNW( BAD, EL, %VAL( PNTRI( 1 ) ), NINVAL,
     :                          WMAXV, WMINV, MAXPOS, MINPOS, STATUS )

*             The number of bad pixels has been counted so it might be
*             possible to save future processing.

               BAD = BAD .OR. ( NINVAL .EQ. 0 )
            END IF

*          Obtain the scaling limits from the environment and do the
*          scaling into the cell array.

            CALL KPS1_DSCLW( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), INVERT, 'LOW', 'HIGH',
     :                       LP, NINTS-1, BPCI, WMINV, WMAXV, .TRUE.,
     :                       %VAL( SCRPNT ), WIMLO, WIMHI, STATUS )

         ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN

*          Set the scaling limits to the extreme values.

            IF ( FNDRNG ) THEN
               BMINV = VAL__MINB
               BMAXV = VAL__MAXB
            ELSE

*             Obtain the maximum and minimum values to be used as
*             dynamic defaults.

               CALL KPG1_MXMNB( BAD, EL, %VAL( PNTRI( 1 ) ), NINVAL,
     :                          BMAXV, BMINV, MAXPOS, MINPOS, STATUS )

*             The number of bad pixels has been counted so it might be
*             possible to save future processing.

               BAD = BAD .OR. ( NINVAL .EQ. 0 )
            END IF

*          Obtain the scaling limits from the environment and do the
*          scaling into the cell array.

            CALL KPS1_DSCLB( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), INVERT, 'LOW', 'HIGH',
     :                       LP, NINTS-1, BPCI, BMINV, BMAXV, .TRUE.,
     :                       %VAL( SCRPNT ), BIMLO, BIMHI, STATUS )
         END IF

*    Do scaling with percentile limits.
*    ==================================

      ELSE IF ( MODE(1:2) .EQ. 'PE' ) THEN

*       Find the percentiles required.  There is no dynamic default.

         DO I = 1, NPRCTL
            PERDEF( I ) = VAL__BADR
         END DO
         CALL PAR_GDR1R( 'PERCENTILES', NPRCTL, PERDEF, 0.0, 100.0,
     :                   .FALSE., PERCNT, STATUS )

*       Convert percentiles to fractions.

         DO  I = 1, NPRCTL
            PERCNT( I ) = PERCNT( I ) * 0.01
         END DO

*       Record the polarity.

         POSTIV = PERCNT( 2 ) .GT. PERCNT( 1 )

*       Also get the number of histogram bins to be used - suggest
*       the maximum allowable as the default.

         CALL PAR_GDR0I( 'NUMBIN', MAXBIN, 1, MAXBIN, .TRUE., NUMBIN,
     :                   STATUS )

*       Since NUMBIN is passed as an adjustable-array dimension we have
*       to check it has been obtained.

         IF ( STATUS .EQ. SAI__OK ) THEN

*          Select appropriate routine for the data type chosen and scale
*          the image between user-defined limits.  The cell array
*          has values between the colour-index limits LP and the largest
*          colour index for the device.
*          =============================================================

            IF ( ITYPE .EQ. '_REAL' ) THEN

*             Obtain the maximum and minimum values to define the bounds
*             of the histogram.

               CALL KPG1_MXMNR( BAD, EL, %VAL( PNTRI( 1 ) ), NINVAL,
     :                          RMAXV, RMINV, MAXPOS, MINPOS, STATUS )

*             The number of bad pixels has been counted so it might be
*             possible to save future processing.

               BAD = BAD .OR. ( NINVAL .EQ. 0 )

*             Generate the histogram between those bounds.

               CALL KPG1_GHSTR( BAD, EL, %VAL( PNTRI( 1 ) ),
     :                          NUMBIN, RMAXV, RMINV, HIST, STATUS )

*             Estimate the values at the percentiles.

               CALL KPG1_HSTFR( NUMBIN, HIST, RMAXV, RMINV, NPRCTL,
     :                          PERCNT, PERVAL, STATUS )

*             Swap the percentiles back if they were flipped.

               IF ( .NOT. POSTIV ) THEN
                  DUMMY = PERVAL( 1 )
                  PERVAL( 1 ) = PERVAL( 2 )
                  PERVAL( 2 ) = DUMMY
               END IF

*             Report the scaling limits for future use.

               CALL MSG_SETR( 'MINVAL', PERVAL( 1 ) )
               CALL MSG_SETR( 'MAXVAL', PERVAL( 2 ) )
               CALL MSG_OUT( 'PVLO', 'Data will be scaled from '/
     :                       /'^MINVAL to ^MAXVAL.', STATUS )

*             Scale the data values using the percentile values.
*             Copy the scaling values for output.

               RIMLO = PERVAL( 1 )
               RIMHI = PERVAL( 2 )
               CALL KPG1_ISCLR( BAD, DIMS( 1 ), DIMS( 2 ),
     :                          %VAL( PNTRI( 1 ) ), INVERT, RIMLO,
     :                          RIMHI, LP, NINTS-1, BPCI,
     :                          %VAL( SCRPNT ), STATUS )

            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN

*             Obtain the maximum and minimum values to define the bounds
*             of the histogram.

               CALL KPG1_MXMND( BAD, EL, %VAL( PNTRI( 1 ) ), NINVAL,
     :                          DMAXV, DMINV, MAXPOS, MINPOS, STATUS )

*             The number of bad pixels has been counted so it might be
*             possible to save future processing.

               BAD = BAD .OR. ( NINVAL .EQ. 0 )

*             Generate the histogram between those bounds.

               CALL KPG1_GHSTD( BAD, EL, %VAL( PNTRI( 1 ) ),
     :                          NUMBIN, DMAXV, DMINV, HIST, STATUS )

*             Estimate the values at the percentiles.

               CALL KPG1_HSTFR( NUMBIN, HIST, SNGL( DMAXV ),
     :                          SNGL( DMINV ), NPRCTL, PERCNT, PERVAL,
     :                          STATUS )

*             Swap the percentiles back if they were flipped.

               IF ( .NOT. POSTIV ) THEN
                  DUMMY = PERVAL( 1 )
                  PERVAL( 1 ) = PERVAL( 2 )
                  PERVAL( 2 ) = DUMMY
               END IF

*             Report the scaling limits for future use.

               CALL MSG_SETR( 'MINVAL', PERVAL( 1 ) )
               CALL MSG_SETR( 'MAXVAL', PERVAL( 2 ) )
               CALL MSG_OUT( 'PVLO', 'Data will be scaled from '/
     :                       /'^MINVAL to ^MAXVAL.', STATUS )

*             Scale the data values using the percentile values.
*             Copy the scaling values for output.

               DIMLO = DBLE( PERVAL( 1 ) )
               DIMHI = DBLE( PERVAL( 2 ) )
               CALL KPG1_ISCLD( BAD, DIMS( 1 ), DIMS( 2 ),
     :                          %VAL( PNTRI( 1 ) ), INVERT, DIMLO,
     :                          DIMHI, LP, NINTS-1, BPCI, 
     :                          %VAL( SCRPNT ), STATUS )

            ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN

*             Obtain the maximum and minimum values to define the bounds
*             of the histogram.

               CALL KPG1_MXMNI( BAD, EL, %VAL( PNTRI( 1 ) ), NINVAL,
     :                          IMAXV, IMINV, MAXPOS, MINPOS, STATUS )

*             The number of bad pixels has been counted so it might be
*             possible to save future processing.

               BAD = BAD .OR. ( NINVAL .EQ. 0 )

*             Generate the histogram between those bounds.

               CALL KPG1_GHSTI( BAD, EL, %VAL( PNTRI( 1 ) ),
     :                          NUMBIN, IMAXV, IMINV, HIST, STATUS )

*             Estimate the values at the percentiles.

               CALL KPG1_HSTFR( NUMBIN, HIST, REAL( IMAXV ),
     :                          REAL( IMINV ), NPRCTL, PERCNT, PERVAL,
     :                          STATUS )

*             Swap the percentiles back if they were flipped.

               IF ( .NOT. POSTIV ) THEN
                  DUMMY = PERVAL( 1 )
                  PERVAL( 1 ) = PERVAL( 2 )
                  PERVAL( 2 ) = DUMMY
               END IF

*             Report the scaling limits for future use.

               CALL MSG_SETI( 'MINVAL', IFIX( PERVAL( 1 ) ) )
               CALL MSG_SETI( 'MAXVAL', IFIX( PERVAL( 2 ) ) )
               CALL MSG_OUT( 'PVLO', 'Data will be scaled from '/
     :                       /'^MINVAL to ^MAXVAL.', STATUS )

*             Scale the data values using the percentile values.
*             Copy the scaling values for output.

               IIMLO = IFIX( PERVAL( 1 ) )
               IIMHI = IFIX( PERVAL( 2 ) )
               CALL KPG1_ISCLI( BAD, DIMS( 1 ), DIMS( 2 ),
     :                          %VAL( PNTRI( 1 ) ), INVERT, IIMLO,
     :                          IIMHI, LP, NINTS-1, BPCI,
     :                          %VAL( SCRPNT ), STATUS )

            ELSE IF ( ITYPE .EQ. '_WORD' ) THEN

*             Obtain the maximum and minimum values to define the bounds
*             of the histogram.

               CALL KPG1_MXMNW( BAD, EL, %VAL( PNTRI( 1 ) ), NINVAL,
     :                          WMAXV, WMINV, MAXPOS, MINPOS, STATUS )

*             The number of bad pixels has been counted so it might be
*             possible to save future processing.

               BAD = BAD .OR. ( NINVAL .EQ. 0 )

*             Generate the histogram between those bounds.

               CALL KPG1_GHSTW( BAD, EL, %VAL( PNTRI( 1 ) ),
     :                          NUMBIN, WMAXV, WMINV, HIST, STATUS )

*             Estimate the values at the percentiles.

               CALL KPG1_HSTFR( NUMBIN, HIST, NUM_WTOR( WMAXV ),
     :                          NUM_WTOR( WMINV ), NPRCTL, PERCNT,
     :                          PERVAL, STATUS )

*             Swap the percentiles back if they were flipped.

               IF ( .NOT. POSTIV ) THEN
                  DUMMY = PERVAL( 1 )
                  PERVAL( 1 ) = PERVAL( 2 )
                  PERVAL( 2 ) = DUMMY
               END IF

*             Report the scaling limits for future use.

               CALL MSG_SETI( 'MINVAL', IFIX( PERVAL( 1 ) ) )
               CALL MSG_SETI( 'MAXVAL', IFIX( PERVAL( 2 ) ) )
               CALL MSG_OUT( 'PVLO', 'Data will be scaled from '/
     :                       /'^MINVAL to ^MAXVAL.', STATUS )

*             Scale the data values using the percentile values.
*             Copy the scaling values for output.

               WIMLO = NUM_RTOW( PERVAL( 1 ) )
               WIMHI = NUM_RTOW( PERVAL( 2 ) )
               CALL KPG1_ISCLW( BAD, DIMS( 1 ), DIMS( 2 ),
     :                          %VAL( PNTRI( 1 ) ), INVERT, WIMLO,
     :                          WIMHI, LP, NINTS-1, BPCI,
     :                          %VAL( SCRPNT ), STATUS )

            ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN

*             Obtain the maximum and minimum values to define the bounds
*             of the histogram.

               CALL KPG1_MXMNB( BAD, EL, %VAL( PNTRI( 1 ) ), NINVAL,
     :                          BMAXV, BMINV, MAXPOS, MINPOS, STATUS )

*             The number of bad pixels has been counted so it might be
*             possible to save future processing.

               BAD = BAD .OR. ( NINVAL .EQ. 0 )

*             Generate the histogram between those bounds.

               CALL KPG1_GHSTB( BAD, EL, %VAL( PNTRI( 1 ) ),
     :                          NUMBIN, BMAXV, BMINV, HIST, STATUS )

*             Estimate the values at the percentiles.

               CALL KPG1_HSTFR( NUMBIN, HIST, NUM_BTOR( BMAXV ),
     :                          NUM_BTOR( BMINV ), NPRCTL, PERCNT,
     :                          PERVAL, STATUS )

*             Swap the percentiles back if they were flipped.

               IF ( .NOT. POSTIV ) THEN
                  DUMMY = PERVAL( 1 )
                  PERVAL( 1 ) = PERVAL( 2 )
                  PERVAL( 2 ) = DUMMY
               END IF

*             Report the scaling limits for future use.

               CALL MSG_SETI( 'MINVAL', IFIX( PERVAL( 1 ) ) )
               CALL MSG_SETI( 'MAXVAL', IFIX( PERVAL( 2 ) ) )
               CALL MSG_OUT( 'PVLO', 'Data will be scaled from '/
     :                       /'^MINVAL to ^MAXVAL.', STATUS )

*             Scale the data values using the percentile values.
*             Copy the scaling values for output.

               BIMLO = NUM_RTOB( PERVAL( 1 ) )
               BIMHI = NUM_RTOB( PERVAL( 2 ) )
               CALL KPG1_ISCLB( BAD, DIMS( 1 ), DIMS( 2 ),
     :                          %VAL( PNTRI( 1 ) ), INVERT, BIMLO,
     :                          BIMHI, LP, NINTS-1, BPCI,
     :                          %VAL( SCRPNT ), STATUS )
            END IF

*       End of status check.

         END IF

*    Flash (as positive) the array without scaling.
*    ==============================================

      ELSE IF ( MODE(1:2) .EQ. 'FL' ) THEN

*       Select appropriate routine for the data type chosen and flash
*       the image via modulo arithmetic.  The cell array has values
*       between the colour-index limits LP and the largest colour
*       index for the device.
*       =============================================================

         IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPG1_FLASR( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), LP, NINTS - 1, INVERT,
     :                       .TRUE., %VAL( SCRPNT ), STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG1_FLASD( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), LP, NINTS - 1, INVERT,
     :                       .TRUE., %VAL( SCRPNT ), STATUS )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
            CALL KPG1_FLASI( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), LP, NINTS - 1, INVERT,
     :                       .TRUE., %VAL( SCRPNT ), STATUS )

         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
            CALL KPG1_FLASW( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), LP, NINTS - 1, INVERT,
     :                       .TRUE., %VAL( SCRPNT ), STATUS )

         ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN
            CALL KPG1_FLASB( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), LP, NINTS - 1, INVERT,
     :                       .TRUE., %VAL( SCRPNT ), STATUS )
         END IF

*    End of the section to compute the cell array by the various
*    methods.

      END IF

      IF ( STATUS .EQ. SAI__OK ) THEN

*       Display the array.
*       ==================
       
*       Select the appropriate pointer to the input data if no scaling
*       has been performed, otherwise to the scaled data in workspace.

         IF ( SCALE ) THEN
            CALL KPG1_GCA( XLC, YLC, XUC, YUC, DIMS( 1 ), DIMS( 2 ),
     :                     DIMS( 1 ), DIMS( 2 ), %VAL( SCRPNT ),
     :                     STATUS )
         ELSE
            CALL KPG1_GCA( XLC, YLC, XUC, YUC, DIMS( 1 ), DIMS( 2 ),
     :                     DIMS( 1 ), DIMS( 2 ), %VAL( PNTRI( 1 ) ),
     :                     STATUS )
         END IF

*       Finished with the input array so unmap it.

         IF ( .NOT. BLOCK ) CALL NDF_UNMAP( NDFS, COMP, STATUS )

*       Store the scaling values.
*       =========================

*       There is no scaling in flash mode.

         IF ( MODE( 1:2 ) .NE. 'FL' .AND. SCALE ) THEN

*          Select appropriate calls depending on the implementation
*          type.

            IF ( ITYPE .EQ. '_REAL' ) THEN
               CALL PAR_PUT0D( 'SCALOW', DBLE( RIMLO ), STATUS )
               CALL PAR_PUT0D( 'SCAHIGH', DBLE( RIMHI ), STATUS )

            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
               CALL PAR_PUT0D( 'SCALOW', DIMLO, STATUS )
               CALL PAR_PUT0D( 'SCAHIGH', DIMHI, STATUS )

            ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
               CALL PAR_PUT0D( 'SCALOW', DBLE( IIMLO ), STATUS )
               CALL PAR_PUT0D( 'SCAHIGH', DBLE( IIMHI ), STATUS )

            ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
               CALL PAR_PUT0D( 'SCALOW', NUM_WTOD( WIMLO ), STATUS )
               CALL PAR_PUT0D( 'SCAHIGH', NUM_WTOD( WIMHI ), STATUS )

            ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN
               CALL PAR_PUT0D( 'SCALOW', NUM_BTOD( BIMLO ), STATUS )
               CALL PAR_PUT0D( 'SCAHIGH', NUM_BTOD( BIMHI ), STATUS )
            END IF

*          Abort to prevent spurious error messages.

            IF ( STATUS .NE. SAI__OK ) GOTO 940

*       Use the pen limits for flash mode.

         ELSE
            CALL PAR_PUT0D( 'SCALOW', DBLE( LP ), STATUS )
            CALL PAR_PUT0D( 'SCAHIGH', DBLE( NINTS - 1 ), STATUS )
         END IF

*       Obtain the axis co-ordinates.
*       =============================

         IF ( DACOOR ) THEN

*          To plot axes and to be able to store their data co-ordinates
*          the application requires a linear axis.  Determine whether
*          or not the data co-ordinates derived from the NDF axes are
*          linear and not identical to world (pixel) co-ordinates, and
*          if so find the linear transformation from world to data
*          co-ordinates and the axis bounds.  The negative number of
*          dimensions tells the subroutine not to worry if there are
*          fewer than 2 significant dimensions.

            IF ( DPAXIS ) THEN
               CALL KPG1_DCLID( -NDIM, NDFS, DXLBND, DXUBND, DSCALE,
     :                          DOFSET, DACOOR, STATUS )
               DO I = 1, NDIM
                  AXLBND( I ) = REAL( DXLBND( I ) )
                  AXUBND( I ) = REAL( DXUBND( I ) )
               END DO
            ELSE
               CALL KPG1_DCLIR( -NDIM, NDFS, AXLBND, AXUBND, LSCALE,
     :                          OFFSET, DACOOR, STATUS )
            END IF
         END IF

         IF ( AXES .AND. .NOT. DATACO ) THEN

*          Just use the pixel bounds to define the annotated-axis
*          limits as there is no axis information or the user wants
*          pixel indices on the axes.

            DO  I = 1, NDIM
               AXLBND( I ) = REAL( SLBND( I ) - 1 )
               AXUBND( I ) = REAL( SUBND( I ) )
            END DO
         END IF

*       Draw axes.
*       ==========

         IF ( AXES ) THEN

*          Select the frame zone.

            CALL SGS_SELZ( ZONEF, STATUS )

*          Get AUTOGRAPH to use the SGS zone.

            CALL SNX_AGWV

*          Store the current NCAR grid values. 

            CALL AGGETF( 'GRID/LEFT.', GRID( 1 ) )
            CALL AGGETF( 'GRID/RIGHT.', GRID( 2 ) )
            CALL AGGETF( 'GRID/BOTTOM.', GRID( 3 ) )
            CALL AGGETF( 'GRID/TOP.', GRID( 4 ) )

*          Store the current NCAR grid values. 

            CALL AGSETF( 'GRID/LEFT.', ANCLIP( 1 ) )
            CALL AGSETF( 'GRID/RIGHT.', ANCLIP( 2 ) )
            CALL AGSETF( 'GRID/BOTTOM.', ANCLIP( 3 ) )
            CALL AGSETF( 'GRID/TOP.', ANCLIP( 4 ) )

*          Draw annotated axes in graph window with the grid positioned
*          as defined above.  Note the NDF-axis bounds are used to
*          define the bounds of the annotated axes.  In other words a
*          regularly spaced array is assumed.  Note there may be
*          problems for d.p. data co-ordinates due to GKS's use of
*          single precision.

            CALL NCRAXS( AXLBND( 1 ), AXLBND( 2 ), AXUBND( 1 ),
     :                   AXUBND( 2 ), PLTITL, ABSLAB, ORDLAB, MINTIC,
     :                   MAJTIC, OUTTIC, THICK, .FALSE., STATUS )

*          Restore the input NCAR grid values. 

            CALL AGSETF( 'GRID/LEFT.', GRID( 1 ) )
            CALL AGSETF( 'GRID/RIGHT.', GRID( 2 ) )
            CALL AGSETF( 'GRID/BOTTOM.', GRID( 3 ) )
            CALL AGSETF( 'GRID/TOP.', GRID( 4 ) )

*          Return to the image zone.

            CALL SGS_SELZ( ZONEI, STATUS )

*       Draw border.
*       ============

         ELSE IF ( BORDER ) THEN

*          Select the frame zone.

            CALL SGS_SELZ( ZONEF, STATUS )

*          Plot the border about the edge of the cell array.

            CALL KPS1_DSBOR( XLC, XUC, YLC, YUC, BORWID, BORCI, STATUS )

*          Return to the image zone.

            CALL SGS_SELZ( ZONEI, STATUS )
         END IF

*       Make sure the output is complete as soon as possible.

         CALL SGS_FLUSH

*       ^^^^^^^^^^

*       Reset the world co-ordinates to pixels in the array.  The
*       world co-ordinates are needed by other applications.

         CALL SGS_SW( XLT, XUT, YLT, YUT, STATUS )

*       Record the data picture in the database.
*       ========================================
*
*       Record the picture and a reference to the NDF in the database.

         CALL KPG1_SDTRN( 'KAPPA_DISPLAY', NDF, PICID2, STATUS )

*       Store a transformation from data co-ordinates to world
*       co-ordinates, where they are different and the data
*       co-ordinates are linear.

         IF ( DACOOR ) THEN
            IF ( DPAXIS ) THEN
               CALL KPG1_LITRD( DSCALE, DOFSET, STATUS )
            ELSE
               CALL KPG1_LITRR( LSCALE, OFFSET, STATUS )
            END IF
         END IF

      ELSE IF ( STATUS .NE. PAR__ABORT ) THEN
         CALL ERR_REP( 'DISPLAY_SCA',
     :     'DISPLAY: Error scaling the picture.', STATUS )
      END IF

      IF ( SCALE ) THEN
      
*       Create the output NDF.
*       ======================

*       Begin an NDF context.

         CALL NDF_BEGIN

*       Begin a new error context.

         CALL ERR_MARK

*       Create the output NDF structure based on the input NDF.

         CALL NDF_PROP( NDFS, 'AXIS,NOQUALITY,NOHISTORY,NOVARIANCE,'/
     :                  /'NOEXTENSION(),WCS', 'OUT', NDFO, STATUS )

         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
         ELSE

*          Define the output data type.  Note bad-pixel flag should be
*          switched off once KAPPA is fully converted to the NDF.

            IF ( NINTS-1 .LE. NUM__MAXUB ) THEN
               OTYPE = '_UBYTE'
            ELSE IF ( NINTS-1 .LE. NUM__MAXUW ) THEN
               OTYPE = '_UWORD'
            ELSE
               OTYPE = '_INTEGER'
            END IF

*          Set the data type of the data component to be one of the
*          above rather than the type of the input NDF.

            CALL NDF_STYPE( OTYPE, NDFO, 'Data', STATUS )

*          Map the NDF's data component for WRITE access.

            CALL KPG1_MAP( NDFO, 'Data', OTYPE, 'WRITE', OPNTR, EL,
     :                    STATUS )

*          There are no bad values by definition.

            BAD = .FALSE.

*          Output the bad pixel flag value accordingly unless the
*          output NDF is primitive.

            CALL NDF_FORM( NDFO, 'Data', FORM, STATUS )

            IF ( FORM .NE. 'PRIMITIVE' ) THEN
               CALL NDF_SBAD( BAD, NDFO, 'Data', STATUS )
            END IF

*          Move the contents from the scaled array to the NDF data
*          component. Since there can be no conversion errors by
*          definition the count returned by the conversion routine is
*          ignored.

            IF ( OTYPE .EQ. '_UBYTE' ) THEN
               CALL VEC_ITOUB( BAD, EL, %VAL( SCRPNT ),
     :                         %VAL( OPNTR( 1 ) ), IERR, NERR, STATUS )
            ELSE IF ( OTYPE .EQ. '_UWORD' ) THEN
               CALL VEC_ITOUW( BAD, EL, %VAL( SCRPNT ),
     :                         %VAL( OPNTR( 1 ) ), IERR, NERR, STATUS )
            ELSE IF ( OTYPE .EQ. '_INTEGER' ) THEN
               CALL VEC_ITOI( BAD, EL, %VAL( SCRPNT ),
     :                        %VAL( OPNTR( 1 ) ), IERR, NERR, STATUS )
            END IF

*          Unmap the data component.

            CALL NDF_UNMAP( NDFO, 'Data', STATUS )
         END IF

*       End the error context.

         CALL ERR_RLSE

*       End the NDF context.

         CALL NDF_END( STATUS )

*       Unmap and annul the workspace used for scaling.

         CALL PSX_FREE( SCRPNT, STATUS )
      END IF

 940  CONTINUE

*    Unmap and annul the workspace used for blocking.

      IF ( BLOCK ) CALL PSX_FREE( SAPNT, STATUS )

*    AGI closedown sequence.
*    =======================

 960  CONTINUE
      CALL AGS_DEASS( 'DEVICE', DEVCAN, STATUS )

*    Unmap and annul NDF data.
*    =========================

 980  CONTINUE
      CALL NDF_END( STATUS )

 999  CONTINUE

      END
