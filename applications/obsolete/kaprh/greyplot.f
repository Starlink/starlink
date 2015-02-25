      SUBROUTINE GREYPLOT( STATUS )
*+
*  Name:
*     GREYPLOT

*  Purpose:
*     Produces a greyscale plot of a 1-d or 2-d NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL GREYPLOT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application produce a greyscale plot of a 1- or
*     2-dimensional NDF, especially for a hardcopy device.  The image
*     is usually the data array, but may also be the variance or
*     quality.  The plot appears in the current graphics-database
*     picture.

*     The greyscale plot resides within optional, annotated and
*     enumerated axes. An optional key may be drawn to the right of the
*     greyscale plot comprising a title and grey blocks annotated with
*     the corresponding array value.  There are a number of scaling
*     methods to map array values to grey levels in the plot.

*     The time to output to hardcopy devices can be quite lengthy
*     and generally depends on the size of the greyscale plot.
*     Therefore, there are parameters for controlling the size of
*     the plot.

*  Usage:
*     greyplot in [comp] key [device] mode [pxsize] [pysize] [out]
*        { white=? black=?
*        { percentiles=?
*        { sigmas=?
*        mode

*  ADAM Parameters:
*     ABSLAB  =  LITERAL (Read)
*        Label for the plot abscissa, in which NCAR fancy founts
*        may be embedded when FONT = "NCAR".  This parameter is only
*        used when the axes option is selected.  If axis information is
*        present the suggested default is the NDF's axis label followed
*        by the units, in parentheses.  If an error occurs obtaining
*        the label the suggested default is "X". []
*     AXES = _LOGICAL (Read)
*        TRUE if annotated axes are to be drawn around the displayed
*        image.  The annotations are either the data co-ordinates from
*        the NDF axis components, provided these are present and linear
*        and COSYS = "Data"; otherwise pixel co-ordinates are used.
*        [FALSE]
*     BADCOL = LITERAL (Read)
*        The grey level to give a bad pixel in the display.  There are
*        a number of options described below.
*
*          "MAX"       - Black
*          "MIN"       - White
*          An integer  - The actual 'colour' index.  It is constrained
*                        to be between 0 and the maximum colour index
*                        available on the device, and so gives a shade
*                        of grey.  0 gives the background colour.
*          A named     - Uses the named `colour' from the palette, and
*          `colour'      if it is not present, the nearest colour from
*                        the palette is selected.  The palette contains
*                        grey levels at percentages from black to white,
*                        e.g. GREY50 is midway between black and white.

*        If this application is run on a device that supports colour
*        it is possible to mark the bad pixels in colour on the
*        greyscale provided BADCOL is an integer between 0 and 15, or
*        a named colour.  The bad pixels will remain unaltered if the
*        lookup table is manipulated.  The suggested default is the
*        current value. [The current value, but equals "MIN" if there
*        is no current value.]
*     BLACK = _DOUBLE (Read)
*        The array value that scales to the black in the greyscale
*        colour table.  All smaller array values appear black when
*        BLACK is less than WHITE, otherwise all array values
*        smaller than BLACK appear white.  The dynamic default is the
*        minimum data value.   There is an efficiency gain when both
*        BLACK and WHITE are given on the command line, because the
*        extreme values need not be computed.  (Scale mode)
*     COMP = LITERAL (Read)
*        The NDF component to be displayed.  It may be "Data",
*        "Quality", "Variance", or "Error" (where "Error" is the
*        alternative to "Variance" and causes the square root of the
*        variance values to be displayed).  If "Quality" is specified,
*        then the quality values are treated as numerical values (in
*        the range 0 to 255). ["Data"]
*     COSYS = LITERAL (Read)
*        The co-ordinate system to be used.  This can be either "World"
*        or "Data".  "World" makes pixel co-ordinates to appear on axes.
*        If COSYS = "Data" the NDF's axis information is used to
*        annotate axes.  [Current co-ordinate system]
*     DEVICE = DEVICE (Read)
*        The name of the graphics device used to display the image.
*        The device must be in one of the following GNS categories:
*        IMAGE_DISPLAY, IMAGE_OVERLAY, MATRIX_PRINTER, or WINDOW, and
*        have at least 24 greyscale intensities.
*        [Current image-display device]
*     FILL = _LOGICAL (Read)
*        The greyscale plot normally has square pixels, in other words
*        a length along each axis corresponds to the same number of
*        pixels.  However, for images with markedly different
*        dimensions, such as two-dimensional spectra, this default
*        behaviour may not be suitable or give the clearest plot.  When
*        FILL is TRUE, the square-pixel constraint is relaxed and the
*        greyscale plot is the largest possible within the current
*        picture.  When FILL is FALSE, the pixels are square.  The
*        suggested default is the current value.  [FALSE]
*     FONT = LITERAL (Read)
*        The fount to be used for the line graphics.  It can be either
*        "NCAR" for the NCAR fancy characters and "GKS" for the standard
*        GKS san-serif fount.   The former is intended for hardcopy
*        publication-quality plots, since it is relatively slow; the
*        latter is intended for normal interactive graphics requiring
*        rapid plotting, and it is clearer on small plots. The
*        suggested default is the current value. ["GKS"]
*     FULL = _LOGICAL (Read)
*        If TRUE, the whole greyscale lookup table for the device is
*        used including the normally reserved pens.  Thus all but two
*        of the available intensities participate in the greyscale,
*        which improves the photographic quality of the image.  The
*        remaining pens are for the background, and for the key and
*        axes.  This option is only available for devices that reset
*        their `colour' tables when the device is opened, such as
*        Laserprinters. (This restriction prevents problems on devices
*        that retain their "colour tables", where using the normally
*        reserved pens would mean that either part of the greyscale
*        would be emphemeral---departing when the application
*        completes if the reserved pens are stored and reinstated on
*        completion, or earlier plots drawn by other applications would
*        be altered.)
*
*        If FULL = FALSE, only non-reserved intensities will form the
*        greyscale.  If a null (!) value is supplied, the value used is
*        TRUE when the non-reserved pens is less than 32, and FALSE
*        otherwise.  (This figure was chosen because it is roughly the
*        number of grey levels at which the eye will clearly detect the
*        quantisation in a complex scene.) [!]
*     IN = NDF (Read)
*        Input NDF data structure containing the image to be displayed.
*     KEY = _LOGICAL (Read)
*        A key of the greyscale versus pixel value and title is to be
*        produced when this is TRUE. [TRUE]
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
*        The type of scaling to be applied to the array.  There are a
*        number of options described below.
*          "Faint"       - The image is scaled from the mean minus one
*                          standard deviation to the mean plus seven
*                          standard deviations.  The scaling values are
*                          reported so that the faster Scale mode may be
*                          utilised later.
*          "Flash"       - The image is flashed onto the screen without
*                          any scaling at all.  This is the fastest
*                          option.  Since there is no longer a
*                          one-to-one mapping between data values and
*                          grey levels this scaling mode is not
*                          available with a key.
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
*        The scaled NDF that is displayed; it also does not have
*        values that equal the reserved portion of the colour table.
*        The output NDF is intended to be used as the input data in
*        conjunction with SCALE=FALSE.  It will be vertically
*        inverted with respect to the input array because of GKS
*        convention.  If it has a null value (!) no output NDF will be
*        created.  This parameter is ignored when SCALE=FALSE. [!]
*     OUTTIC = _LOGICAL (Read)
*        True if the axis tick marks are to appear on the outside of
*        the axes instead of inside.   This parameter is only used
*        when the axes option is selected. [TRUE]
*     PERCENTILES( 2 ) = _REAL (Read)
*        The percentiles that define the scaling limits. For example,
*        [75,25] would scale between the quartile values. (Percentile
*        mode)
*     PLTITL = LITERAL (Read)
*        The title of the plot.  Up to about 25 characters can be
*        accommodated when there are no axes, and about 40 otherwise.
*        NCAR fancy founts may be embedded (cf. SUN/90) when FONT =
*        "NCAR".  This parameter is only used when either the axes or
*        key option is selected. [The NDF title]
*     PXSIZE = _REAL (Read)
*        The length (x axis) of the plot in metres.  [Maximum that can
*        fit in the current picture whilst preserving square pixels.]
*     PYSIZE = _REAL (Read)
*        The length (y axis) of the plot in metres.  [Maximum that can
*        fit in the current picture whilst preserving square pixels.]
*     REDUCT = _REAL (Read)
*        The reduction factor of the array, and must be in the range
*        1/MAX(NX,NY)--1.0, where NX and NY are the number of pixels
*        in the image along the x and y directions.  Since the output
*        to the hardcopy device may be long, the size of the image
*        with respect to the key can be reduced by lowering REDUCT.
*        The spooling time goes approximately proportional to the
*        square of REDUCT.  1.0 means the array fills the plotting zone.
*        [1.0]
*     SCAHIGH = _DOUBLE (Write)
*        The array value scaled to white in the greyscale display.
*        In Flash mode or when there is no scaling the colour index of
*        white is used.  The current display linear-scaling maximum is
*        set to this value.
*     SCALE = _LOGICAL (Read)
*        If TRUE the input array is scaled according to the value of
*        parameter MODE.  If it is FALSE, MODE is ignored, and the input
*        array is displayed as is.  There is no scaling, inversion
*        or avoidance of annotation pens.  SCALE=FALSE is intended to
*        be used with arrays previously scaled by this or similar
*        applications which have already performed the scaling,
*        inversion and exclusion.  It provides the quickest method of
*        image display within this application. [TRUE]
*     SCALOW = _DOUBLE (Write)
*        The array value scaled to black in the greyscale display.
*        In Flash mode or when there is no scaling the colour index of
*        black is used.  The current display linear-scaling minimum is
*        set to this value.
*     SIGMAS( 2 ) = _REAL (Read)
*        The standard-deviation bounds that define the scaling limits.
*        To obtain values either side of the mean both a negative and
*        a positive value are required.  Thus [-2,3] would scale
*        between the mean minus two and the mean plus three standard
*        deviations.  [3,-2] would give the negative of that. (Sigmas
*        mode).
*     THICK = _REAL (Read)
*        The thickness of the axes and annotations in the plot, where
*        1.0 is the normal thickness.  It should be between 0.5 and 5.
*        This feature is only available on some devices.   This
*        parameter is only used when the axes option is selected. [1.0]
*     WHITE = _DOUBLE (Read)
*        The array value that scales to white in the greyscale
*        colour table.  All larger array values appear white when
*        WHITE is greater than BLACK, otherwise all array values
*        larger than WHITE appear black.  The dynamic default is the
*        maximum data value.   There is an efficiency gain when both
*        BLACK and WHITE are given on the command line, because the
*        extreme values need not be computed.  (Scale mode)

*  Examples:
*     greyplot sdor key mode=sc black=1 white=5.2
*        Makes a greyscale display of the data component of the NDF
*        called sdor on the current image-display device, scaling
*        between 1 and 5.2.  Values up to 1.0 in the data array will
*        appear black in the plot, and values larger than 5.2 will be
*        white.  Intermediate values will a grey level determined by
*        linear interpolation.  A key is drawn to the right of the
*        greyscale.
*     greyplot in=sdor nokey mode=p percentiles=[10,90] badcol="Black"
*        This makes a greyscale plot of the NDF called sdor on the
*        current image-display device. The scaling is between the 10 and
*        90 per cent percentiles of the image.  No key is drawn.  Bad
*        data appear black in the plot.
*     greyplot mode=fa axes out=video cosys=d \
*        Displays a greyscale of the current NDF data component with
*        annotated axes on the current image-display device.  The axes
*        take the axis labels and title from the NDF, and are annotated
*        in data co-ordinates.  The scaling is between the -1 and +7
*        standard deviations of the image around its mean.  A key is
*        drawn.  The scaled data are stored in an NDF called video.
*     greyplot video noscale \
*        Displays the data component of the NDF called video (created
*        in the previous example) without scaling within the current
*        picture on the current image-display device.
*     greyplot cgs4k v key mode=ra device=canon_l
*        Makes a greyscale display of the variance component of NDF
*        cgs4k on the Canon_l device, scaling between the minimum and
*        maximum variance values.

*  Notes:
*     -  The application stores a number of pictures in the graphics
*     database in the following order: a FRAME of the specified size
*     containing the title, annotated axes, and the image area
*     (provided AXES is TRUE), whose world co-ordinates are in pixels;
*     a DATA picture with world co-ordinates in units of data pixels;
*     and a KEY.  The DATA picture also may have double-precision data
*     co-ordinates derived from the NDF axis component provided these
*     are linear and different from pixel co-ordinates; the data
*     co-ordinates are stored via a linear transformation.  The NDF
*     associated with the plot is stored by reference with the DATA
*     picture.  On exit the current database picture for the chosen
*     device reverts to the input picture.
*     -  When axes are requested the axis annotations are defined by
*     their lower and upper bounds, i.e. a regular array is assumed.
*     The bounds are in pixel or data co-ordinates.
*     -  The data type of the output NDF depends on the number of colour
*     indices: _UBYTE for no more than 256, _UWORD for 257 to 65535,
*     and _INTEGER otherwise.   The output NDF will not contain any
*     extensions, UNITS, QUALITY, and VARIANCE; but LABEL, TITLE, and
*     AXIS information are propagated from the input NDF.  The output
*     NDF does not become the new current data array.  It is a Simple
*     NDF (because the bad-pixel flag is set to false in order to
*     access the maximum grey level), therefore only NDF-compliant
*     applications can process it.

*  Algorithm:
*     - Determine whether scaling is required.
*     - Find which component to display, obtain an identifier to the
*     NDF and check that the component is present. Find the data type
*     for processing (integer if no scaling).  Get the NDF bounds and
*     inquire the bad-pixel flag.  Determine the co-ordinate system to
*     use.
*     - Get the display device and open the database for it with the
*     appropriate device status. Get the current SGS zone. Inquire the
*     number of colour indices that are available on the image display
*     and check that device is indeed an image display.
*     -  Create the frame and key zones.
*     -  Set up the workstation greyscale.
*     -  Define the frame zone and key zones, storing the frame picture
*     in the database.  If axes are required get their style and
*     annotation, and then draw them.
*     -  Obtain the bounds of the image zone, with special allowances
*     made for axes, if required.  Obtain the scaling method.
*     -  Create and map a scratch area for array scaling.  Scale via
*     the requested method and data type, performing any necessary
*     preliminary calculations, e.g. histogram for percentiles.  Reset
*     the bad-pixel flag if it has been determined that none exist
*     during the preliminaries to save processing.
*     -  Plot the scaled cell array.
*     -  Write the scaling limits.
*     -  Obtain the axis values from the NDF if present or when axes
*     and data co-ordinates are required.  Determine whether or not the
*     axes are linear.  If they are, compute the linear transformation.
*     -  Plot the axes if requested returning to the image zone.
*     -  Record the data picture in the database, and a reference to
*     the image data.  Record the linear transformation if present.
*     -  Plot a key axes, if requested, and save it in the database.
*     -  Store the scaled data in an output NDF if required, performing
*     type conversion where necessary.
*     -  Reset the current picture to the input current picture.
*     -  Tidy the data and graphics.

*  Related Applications:
*     KAPPA: DISPLAY; Figaro: IGREY, IMAGE; SPECDRE: MOVIE.

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
*     1991 February 7 (MJC):
*        NDF version based on the pre-0.8 version.
*     1991 March 19 (MJC):
*        Added output parameters for the scaling limits.
*     1991 April 5 (MJC):
*        Added data co-ordinate transformation and optional axes.
*     1991 May 7 (MJC):
*        Added the input and output of NDFs containing scaled data.
*     1991 May 14 (MJC):
*        Added direct processing of _BYTE and _WORD data.
*     1991 July 22 (MJC):
*        Added user-controlled appearance of bad pixels.
*     1991 August 20 (MJC):
*        Added FONT parameter.
*     1992 January 29 (MJC):
*        Removed line that resets the background to black.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1992 March 30 (MJC):
*        Revised defaulting behaviour of BADCOL and corrected the notes.
*        Included BADCOL in one of the examples.
*     1992 April 13 (MJC):
*        Made to handle significant dimensions for user-defined
*        sections.
*     1992 November 28 (MJC):
*        Does not use non-monotonic axis centres.
*     1992 December 17 (MJC):
*        Added the FILL option.
*     1995 October 19 (MJC):
*        Supports Error component.
*     1997 May 28 (MJC):
*        QUALITY and HISTORY no longer propagated to the OUT NDF.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Global SSE definitions
      INCLUDE 'DAT_PAR'        ! Data-system constants
      INCLUDE 'PAR_ERR'        ! Parameter-system error definitions
      INCLUDE 'SUBPAR_PAR'     ! SUBPAR constants
      INCLUDE 'PRM_PAR'        ! PRIMDAT definitions
      INCLUDE 'NDF_PAR'        ! NDF definitions
      INCLUDE 'NDF_ERR'        ! NDF_ error constants
      INCLUDE 'CTM_PAR'        ! Colour-table management constants
      INCLUDE 'GNS_PAR'        ! GNS constants

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER MAXBIN           ! Maximum number of histogram bins
                               ! for percentiles
      PARAMETER ( MAXBIN = 2048 )

      INTEGER MAXCOL           ! Maximum number of grey levels to be
                               ! drawn in the key
      PARAMETER ( MAXCOL = 16 )

      INTEGER MINCOL           ! Minimum number of colours on device to
                               ! be classed as a suitable device for
                               ! the greyplot
      PARAMETER ( MINCOL = 4 + CTM__RSVPN )

      INTEGER NDIM             ! Dimensionality required
      PARAMETER( NDIM = 2 )

      INTEGER NPRICL           ! Number of primary colours
      PARAMETER ( NPRICL = 3 )

      INTEGER NPRCTL           ! Maximum number of percentiles
      PARAMETER( NPRCTL = 2 )

      REAL ASPKEY              ! Fractional aspect ratio (of that of
                               ! the input array) for the key
      PARAMETER ( ASPKEY = 0.2 )

      REAL MREDUC              ! Minimum reduction factor
      PARAMETER ( MREDUC = 1./32.0 )

      REAL
     :  ANCLP1, ANCLP2,        ! Fraction of the frame zone in which the
     :  ANCLP3, ANCLP4         ! image will appear when there are axes.
                               ! Note aspect ratio is preserved.
      PARAMETER ( ANCLP1 = 0.19, ANCLP2 = 0.95,
     :            ANCLP3 = 0.15, ANCLP4 = 0.91 )

*  Local Variables:
      LOGICAL                  ! True if :
     :  AXES,                  ! Annotated axes required
     :  BAD,                   ! The array may contain bad pixels and
                               ! therefore testing should occur
     :  DACOOR,                ! Data co-ordinates are to be stored
                               ! in the database
     :  DATACO,                ! Axes are given in data co-ordinates
     :  DATEMP,                ! Either of the axes is non-monotonic
     :  DEVCAN                 ! Image-display parameter is to be
                               ! cancelled

      LOGICAL
     :  DPAXIS,                ! Axis centres are double precision
     :  FILL,                  ! Plotting area is filled
     :  FULL,                  ! Use maximum-length greyscale
     :  FNDRNG,                ! Find the data range for scaling
     :  INVERT,                ! The array is to inverted for display
     :  KEY,                   ! A key of the grey values versus pixel
                               ! is to be produced
     :  MONOTO,                ! Axis is monotonic
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
     :  AXLBND( NDIM ),        ! Axis lower bounds
     :  AXUBND( NDIM ),        ! Axis upper bounds
     :  ASP,                   ! Aspect ratio of the input array
     :  DUMMY,                 ! Used to swap percentiles, range limits
     :  GKSCOL( NPRICL ),      ! Colour of one pen for transfer to
                               ! device lookup table
     :  GRID( 4 ),             ! Current AUTOGRAPH grid offsets
     :  LSCALE( NDIM ),        ! Scale factors in the world-to-data
                               ! co-ordinate transformations
     :  MAJTIC( 2 ),           ! Parameters controlling the numbers of
                               ! major tick marks along x and y axes
                               ! respectively
     :  MINTIC( 2 ),           ! Numbers of minor tick marks along x and
                               ! y axes respectively
     :  MINRED,                ! Minimum reduction factor
     :  OFFSET( NDIM ),        ! Offsets in the world-to-data
                               ! co-ordinate transformations
     :  PERCNT( NPRCTL ),      ! Percentiles
     :  PERDEF( NPRCTL ),      ! Suggested defaults for the percentiles
     :  PERVAL( NPRCTL )       ! Values at the percentiles

      REAL
     :  RMAXV,                 ! Minimum value in the array
     :  RMINV,                 ! Maximum value in the array
     :  REDUCT,                ! Reduction factor for the cell array
     :  RIMHI,                 ! Upper limit used for scaling the array
     :  RIMLO,                 ! Lower   "     "   "     "     "    "
     :  SIGDEF( 2 ),           ! Suggested defaults for standard
                               ! deviations
     :  SIGRNG( 2 ),           ! Standard-deviation limits
     :  THICK,                 ! Line thickness for axes
     :  TICDEF( 2 ),           ! Suggested default axis-tick values
     :  XL, XU, YL, YU         ! Boundary of cell array in world
                               ! co-ordinates

      CHARACTER*(DAT__SZLOC)   ! Locator for :
     :  SCRLOC                 ! Scratch area in which the scaled
                               ! array is kept

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
     :  MODE*10,               ! Manner in which the array is to be
                               ! scaled
     :  ORDLAB,                ! Label for the ordinate of the plot
     :  OTYPE * ( NDF__SZTYP ),! Processing type of the output image
     :  PLTITL,                ! Title of the plot
     :  WKOPEN * ( GNS__SZKEY )! Workstation reset flag

      INTEGER
     :  ACTBLA,                ! The BLACK parameter is active
     :  ACTWHI,                ! The WHITE parameter is active
     :  AEL( NDF__MXDIM ),     ! Number of elements in a mapped axis
     :  AXPNTR( NDF__MXDIM ),  ! Pointers to the mapped axes
     :  BPCI,                  ! Bad-pixel colour index
     :  DIMS( NDIM ),          ! Dimensions of input array
     :  EL,                    ! Number of elements in the input and
                               ! cell arrays
     :  HIST( MAXBIN ),        ! Histogram
     :  I, J,                  ! Counters
     :  IDUMMY,                ! Used to swap range limits
     :  IERR,                  ! Position of first conversion error
     :  IIMHI,                 ! Upper limit used for scaling the array
     :  IIMLO,                 ! Lower   "     "   "     "     "    "
     :  IMAXV,                 ! Minimum value in the array
     :  IMINV                  ! Maximum value in the array

      INTEGER
     :  IPIXX,                 ! Maximum number of columns of pixels
                               ! of the image display
     :  IPIXY,                 ! Maximum number of lines of pixels
                               ! of the image display
     :  LBND( NDF__MXDIM ),    ! Lower bounds of the image
     :  LP,                    ! Lowest pen with which to display the
                               ! the image
     :  MAXPOS,                ! Position of the maximum (not used)
     :  MINPOS,                ! Position of the minimum (not used)
     :  NDF,                   ! Identifier for input NDF
     :  NDFC,                  ! Identifier for section copy of NDF
     :  NDFO,                  ! Identifier for output NDF
     :  NDIMS,                 ! Actual number of dimensions
     :  NERR,                  ! Number of conversion errors
     :  NLEVK,                 ! Number of grey levels in the key
     :  NINTS,                 ! Number of greyscale intensities
                               ! available on the chosen device
     :  NINVAL,                ! Number of bad values in input array
     :  NUMBIN                 ! Number of bins in the histogram

      INTEGER
     :  OPNTR( 1 ),            ! Pointer to output array data
     :  PICID1,                ! Graphics' database identifier on input
     :  PICID2,                ! Graphics' database identifier for
                               ! the displayed picture and key
     :  PICID3,                ! Graphics' database identifier for
                               ! the displayed greyscale
     :  PNTRI( 1 ),            ! Pointer to input array data
     :  SCRPNT,                ! Pointer to the scratch area in which
                               ! the scaled array is kept
     :  SDIM( NDF__MXDIM ),    ! Significant dimensions of the NDF
     :  SLBND( NDIM ),         ! Significant lower bounds of the image
     :  SUBND( NDIM ),         ! Significant upper bounds of the image
     :  UBND( NDF__MXDIM ),    ! Upper bounds of the image
     :  WKID,                  ! GKS work station identifier
     :  ZONE1,                 ! Initial SGS zone identifier
     :  ZONE2,                 ! SGS zone identifier for the frame
     :  ZONEK,                 ! SGS zone identifier for the key
     :  ZONEIF,                ! SGS zone identifier for the image/frame
     :  ZONEI                  ! SGS zone identifier for the image area
                               ! when there are axes

      DOUBLE PRECISION
     :  DDUMMY,                ! Used to swap range limits
     :  DIMHI,                 ! Upper limit used for scaling the array
     :  DIMLO,                 ! Lower   "     "   "     "     "    "
     :  DMAXV,                 ! Minimum value in the array
     :  DMINV,                 ! Maximum value in the array
     :  DSCALE( NDIM ),        ! Scale factors in the world-to-data
                               ! co-ordinate transformations
     :  DOFSET( NDIM ),        ! Offsets in the world-to-data
                               ! co-ordinate transformations
     :  DXLBND( NDIM ),        ! Axis lower bounds
     :  DXUBND( NDIM )         ! Axis upper bounds

      BYTE
     :  BDUMMY,                ! Used to swap range limits
     :  BIMHI,                 ! Upper limit used for scaling the array
     :  BIMLO,                 ! Lower   "     "   "     "     "    "
     :  BMAXV,                 ! Minimum value in the array
     :  BMINV                  ! Maximum value in the array

      INTEGER * 2
     :  WDUMMY,                ! Used to swap range limits
     :  WIMHI,                 ! Upper limit used for scaling the array
     :  WIMLO,                 ! Lower   "     "   "     "     "    "
     :  WMAXV,                 ! Minimum value in the array
     :  WMINV                  ! Maximum value in the array

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'    ! NUM declarations for conversions
      INCLUDE 'NUM_DEF_CVT'    ! NUM definitions for conversions

*.

*    Check the inherited status on entry.

      IF ( STATUS .NE. SAI__OK ) RETURN

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

      CALL LPG_ASSOC( 'IN', 'READ', NDF, STATUS )

*    There must be a data array, but for other components check that
*    requested component is present.

      IF ( COMP .NE. 'DATA' ) THEN
         CALL NDF_STATE( NDF, COMP, THERE, STATUS )

*       The component is not present or not defined, so make an error
*       report including the NDF's name.

         IF ( .NOT. THERE ) THEN
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', NDF )
            CALL MSG_SETC( 'COMP', COMP )
            CALL ERR_REP( 'GREYPLOT_NOCOMP',
     :        'GREYPLOT: ^COMP component is not defined in NDF ^NDF.',
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
*    NDF.  The section is needed to permit a 1-dimensional NDF to be
*    displayed, as it allows NDF_ to make a dummy second axis.

      CALL NDF_SECT( NDF, MAX( NDIM, NDIMS ), LBND, UBND, NDFC, STATUS )
      CALL KPG1_SECSH( NDFC, SDIM( NDIM ), STATUS )

*    Must have a 2-d.  A bad status will be generated by NDF_BOUND
*    if there are greater than 2 significant dimensions.

      IF ( STATUS .EQ. NDF__XSDIM ) THEN
         CALL NDF_MSG( 'NDF', NDF )
         CALL ERR_REP( 'GREYPLOT_IVDIM',
     :     'GREYPLOT: NDF ^NDF is not two-dimensional.', STATUS )
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

*    Get the type of co-ordinates to place on axes.
*    ==============================================

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
                        CALL MSG_OUT( 'GREYPLOT_NOTMONO1',
     :                    'GREYPLOT: Axis ^IAXIS is not monotonic.  '/
     :                    /'Will use world co-ordinates instead.',
     :                    STATUS )
                     ELSE
                        CALL MSG_OUT( 'GREYPLOT_NOTMONO2',
     :                    'GREYPLOT: Axis ^IAXIS is not monotonic.  '/
     :                    /'Will not record axis bounds in the '/
     :                    /'graphics database.', STATUS )
                     END IF

*                   Record the fact.
                     DATEMP = .FALSE.

                  END IF
               END IF

*             Axis has one element or is monotonic.

               IF ( DATEMP ) THEN

*                Find the range of the axis co-ordinates.

                  CALL KPG1_AXBND( AEL( I ), %VAL( AXPNTR( I ) ),
     :                             DXLBND( I ), DXUBND( I ), STATUS )
               END IF

*             Unmap the axis since we have finished with it.

               CALL NDF_AUNMP( NDFC, 'Centre', SDIM( I ), STATUS )
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
                        CALL MSG_OUT( 'GREYPLOT_NOTMONO1',
     :                   'GREYPLOT: Axis ^IAXIS is not monotonic.  '/
     :                   /'Will use world co-ordinates instead.',
     :                   STATUS )
                     ELSE
                        CALL MSG_OUT( 'GREYPLOT_NOTMONO2',
     :                   'GREYPLOT: Axis ^IAXIS is not monotonic.  '/
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

*             Unmap the axis since we have finished with it.

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

*    Find whether a key is required or not.
*    ======================================

      CALL PAR_GTD0L( 'KEY', .TRUE., .TRUE., KEY, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 980

*    Start the graphics system.
*    ==========================

      DEVCAN = .FALSE.

*    Associate a graphics device in the database.

      CALL AGS_ASSOC( 'DEVICE', 'WRITE', ' ', PICID1, ZONE1, STATUS )

*    Check whether chosen device is an 'image display' with a suitable
*    minimum number of colour indices.

      CALL KPG1_QVID( 'DEVICE', 'SGS', 'IMAGE_DISPLAY,IMAGE_OVERLAY,'/
     :                /'WINDOW,MATRIX_PRINTER', ' ', MINCOL, STATUS )

*    Obtain the number of colour indices and the maximum display
*    surface.

      CALL KPG1_QIDAT( 'DEVICE', 'SGS', NINTS, IPIXX, IPIXY, STATUS )

*    Abort if the device is not suitable or something has gone wrong
*    associating the device.

      IF ( STATUS .NE. SAI__OK ) THEN

*       The device name is to be cancelled.

         DEVCAN = .TRUE.
         GOTO 960
      END IF

*    Create the frame picture.
*    =========================

      CALL KPG1_FRPIC( 'PXSIZE', 'PYSIZE', 'KAPPA_GREYPLOT', .FALSE.,
     :                 ZONE2, PICID2, STATUS )

*    Reset the input picture as current in case of an accident.

      CALL AGI_SELP( PICID1, STATUS )

*    ^^^^^^^^^^^^^^^^^^^^^^^^^


*    Get the shape and size of the pixels.
*    =====================================

*    Determine if square pixels are required.
      CALL PAR_GTD0L( 'FILL', .FALSE., .TRUE., FILL, STATUS )

*    Get the reduction factor for the plot - default is no reduction,
*    the cell array fills the whole zone.

      MINRED = MAX( MREDUC, 1.0 / REAL( MAX( DIMS( 1 ), DIMS( 2 ) ) ) )
      CALL PAR_GDR0R( 'REDUCT', 1.0, MINRED, 1.0, .TRUE., REDUCT,
     :                STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 960

*    Create the various zones.
*    =========================

*    Define the aspect ratio of the image.  For the filled area just
*    find the aspect ratio of the frame. (This assumes symmetric
*    trimming of each axis for the annotated axes.)  Create a frame
*    zone for the greyscale and a key zone (when required).  The
*    subroutine recognises the negative ASP value, and will calculate
*    an effective aspect ratio and return this value.

      IF ( FILL ) THEN
         ASP = -1.0
      ELSE
         ASP = REAL( DIMS( 1 ) ) / REAL( DIMS( 2 ) )
      END IF
      CALL KPG1_KEYZO( KEY, ASPKEY, ASP, ZONEIF, ZONEK, STATUS )

*    Select the image/frame zone.

      CALL SGS_SELZ( ZONEIF, STATUS )

*    Normalise its world co-ordinates.

      CALL SGS_SW( 0.0, 1.0, 0.0, 1.0, STATUS )

*    Define a smaller zone to allow for the reduction factor.

      CALL SGS_ZONE( 0.0, REDUCT, 0.0, REDUCT, ZONEIF, STATUS )

*    Inquire whether GKS/SGS has reported an error.

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GREYPLOT_ZONE',
     :     'GREYPLOT: Error while defining the shape or '/
     :     /'co-ordinates of the graphics zone on the '/
     :     /'image-display surface.', STATUS )

*       The device name is to be cancelled.

         DEVCAN = .TRUE.
         GOTO 960
      END IF

*    Define default limiting positions of the cell array.
*    ====================================================

      XL = REAL( SLBND( 1 ) - 1 )
      YL = REAL( SLBND( 2 ) - 1 )
      XU = REAL( SUBND( 1 ) )
      YU = REAL( SUBND( 2 ) )

*    See whether the device resets.
*    ==============================

*    Obtain GKS workstation identifier

      CALL SGS_ICURW( WKID )

*    Start the GNS system for GKS.  Find the reset keyword of the
*    workstation.  Then stop the GNS system for GKS.

      CALL GNS_START( 'GKS', STATUS )
      CALL GNS_IWCG( WKID, 'OPEN', WKOPEN, STATUS )
      CALL GNS_STOP( 'GKS', STATUS )

*    Set up annotation pens and greyscale.
*    =====================================

*    See whether the device resets.  If it does not the full colour
*    table may not be used.

      IF ( WKOPEN .EQ. 'NORESET' ) THEN
         FULL = .FALSE.
      ELSE

*       Find whether the maximum number of grey levels is required or
*       not.  The default is true when there are less than 32 available
*       intensities.

         CALL PAR_GTD0L( 'FULL', NINTS .LT. CTM__RSVPN + 32, .TRUE.,
     :                   FULL, STATUS )
      END IF

      IF ( FULL ) THEN

*       Set the lower pen to be used.  Minimum is 2 because one pen for
*       the background and one for the annotations.

         LP = 2
      ELSE

*       Set the lower pen to the normal number of reserved pens.

         LP = CTM__RSVPN
      END IF

*    Make the greyscale between the pen limits, and write it to the
*    colour table.

      DO  I = LP, NINTS-1, 1
         DO  J = 1, NPRICL, 1
            GKSCOL( J ) = REAL( I - LP ) / REAL( NINTS - LP - 1 )
         END DO

         CALL GSCR( WKID, I, GKSCOL( 1 ), GKSCOL( 2 ), GKSCOL( 3 ) )
      END DO

*    Obtain the colour for bad pixels as a colour index in the palette.

      CALL KPG1_MACOL( 'BADCOL', LP, NINTS - 1, BPCI, STATUS )

*    Draw axes.
*    ==========

*    Are annotated axes required?

      CALL PAR_GTD0L( 'AXES', .FALSE., .TRUE., AXES, STATUS )

      IF ( AXES ) THEN
         ANCLIP( 1 ) = ANCLP1
         ANCLIP( 2 ) = ANCLP2
         ANCLIP( 3 ) = ANCLP3
         ANCLIP( 4 ) = ANCLP4

*       When axes are present room must be made for them, and so the
*       default magnification is reduced.  Make a new "initial" image
*       zone, but with an allowance for the axes.

         CALL KPS1_IMZBO( ANCLIP, ZONEI, STATUS )

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
*          NDF's axis structure to preevent the wrong label being
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

      ELSE IF ( KEY ) THEN

*       Obtain a title for the key.
*       ===========================

         CALL KPG1_GNTIT( NDF, 'PLTITL', 'Greyplot ', PLTITL, STATUS )
      END IF

      IF ( AXES .OR. KEY ) THEN

*       Get the fount.  Although NCAR is the default, either must be
*       selected to prevent persistence from earlier invocations.

         CALL PAR_CHOIC( 'FONT', 'GKS', 'GKS,NCAR', .TRUE., FOUNT,
     :                   STATUS )
         IF ( FOUNT .EQ. 'GKS ' ) THEN
            CALL AGPWRT( 0.0, 0.0, ' ', 0, 0, 0, -100 )
         ELSE IF ( FOUNT .EQ. 'NCAR' ) THEN
            CALL AGPWRT( 0.0, 0.0, ' ', 0, 0, 0, 100 )
         END IF
      END IF

*    Set the world co-ordinates of the data zone to be the bounds of
*    the image.

      CALL SGS_SW( XL, XU, YL, YU, STATUS )

*    Determine the type of scaling.
*    ==============================

*    Get the type of scaling to be performed on the array.  Note that
*    there is no one-to-one mapping between the grey levels and a
*    flashed array, since modulo arithmetic is used.

      IF ( SCALE ) THEN
         IF ( KEY ) THEN
            CALL PAR_CHOIC( 'MODE', 'Scale',
     :                      'Scale,Faint,Percentiles,Range,Sigma',
     :                      .TRUE., MODE, STATUS )
         ELSE
            CALL PAR_CHOIC( 'MODE', 'Scale',
     :                      'Scale,Flash,Faint,Percentiles,Range,Sigma',
     :                      .TRUE., MODE, STATUS )
         END IF
         CALL CHR_UCASE( MODE )
      END IF

*    The array is to be inverted because the GKS convention is for the
*    origin to be at the top.

      INVERT = .TRUE.
      IF ( STATUS .NE. SAI__OK ) GOTO 960

*    Map the NDF.

      CALL KPG1_MAP( NDF, MCOMP, ITYPE, 'READ', PNTRI, EL, STATUS )

*    Create a scratch area in which to put the scaled array.
*    =======================================================

*    Not required if the input array is to be used.

      IF ( SCALE ) THEN
         CALL AIF_GETVM( '_INTEGER', 2, DIMS, SCRPNT, SCRLOC, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN

            CALL ERR_REP( 'ERR_GREYPLOT_WSP',
     :        'GREYPLOT: Unable to get workspace to scale the array.',
     :        STATUS )
            GOTO 960
         END IF
      END IF

*     Negative pictures look better.

      POSTIV = .FALSE.

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

*           Fixed range in standard-deviation units.  It is a negative
*           picture.

             SIGRNG( 1 ) = 7.0
             SIGRNG( 2 ) = -1.0
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
*       colour index for the device.  Note that the extreme values
*       are reversed to give a negative picture.
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
            CALL MSG_OUT( 'PVLO', 'Data will be scaled from ^MAXVAL '/
     :                    /'to ^MINVAL.', STATUS )

*          Scale the data values using the extreme values into the
*          cell array.

            CALL KPG1_ISCLR( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), INVERT, RIMHI, RIMLO,
     :                       LP, NINTS-1, BPCI, %VAL( SCRPNT ), STATUS )

*          The high and low values have been swapped in order to scale
*          correctly, i.e.  minimum is white and maximum is black.
*          Reswap them for later use.

            DUMMY = RIMHI
            RIMHI = RIMLO
            RIMLO = DUMMY

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
            CALL MSG_OUT( 'PVLO', 'Data will be scaled from ^MAXVAL '/
     :                    /'to ^MINVAL.', STATUS )

*          Scale the data values using the extreme values into the
*          cell array.

            CALL KPG1_ISCLD( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), INVERT, DIMLO, DIMHI,
     :                       LP, NINTS-1, BPCI, %VAL( SCRPNT ), STATUS )

*          The high and low values have been swapped in order to scale
*          correctly, i.e.  minimum is white and maximum is black.
*          Reswap them for later use.

            DDUMMY = DIMHI
            DIMHI = DIMLO
            DIMLO = DDUMMY

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
            CALL MSG_OUT( 'PVLO', 'Data will be scaled from ^MAXVAL '/
     :                    /'to ^MINVAL.', STATUS )

*          Scale the data values using the extreme values into the
*          cell array.

            CALL KPG1_ISCLI( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), INVERT, IIMLO, IIMHI,
     :                       LP, NINTS-1, BPCI, %VAL( SCRPNT ), STATUS )

*          The high and low values have been swapped in order to scale
*          correctly, i.e.  minimum is white and maximum is black.
*          Reswap them for later use.

            IDUMMY = IIMHI
            IIMHI = IIMLO
            IIMLO = IDUMMY

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
            CALL MSG_OUT( 'PVLO', 'Data will be scaled from ^MAXVAL '/
     :                    /'to ^MINVAL.', STATUS )

*          Scale the data values using the extreme values into the
*          cell array.

            CALL KPG1_ISCLW( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), INVERT, WIMLO, WIMHI,
     :                       LP, NINTS-1, BPCI, %VAL( SCRPNT ), STATUS )

*          The high and low values have been swapped in order to scale
*          correctly, i.e.  minimum is white and maximum is black.
*          Reswap them for later use.

            WDUMMY = WIMHI
            WIMHI = WIMLO
            WIMLO = WDUMMY

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
            CALL MSG_OUT( 'PVLO', 'Data will be scaled from ^MAXVAL '/
     :                    /'to ^MINVAL.', STATUS )

*          Scale the data values using the extreme values into the
*          cell array.

            CALL KPG1_ISCLB( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), INVERT, BIMLO, BIMHI,
     :                       LP, NINTS-1, BPCI, %VAL( SCRPNT ), STATUS )

*          The high and low values have been swapped in order to scale
*          correctly, i.e.  minimum is white and maximum is black.
*          Reswap them for later use.

            BDUMMY = BIMHI
            BIMHI = BIMLO
            BIMLO = BDUMMY
         END IF

*    Do scaling with limits from the environment.
*    ============================================

      ELSE IF ( MODE(1:2) .EQ. 'SC' ) THEN

*       Determine whether or not the scaling parameters have been
*       found, to avoid finding the maximum and minimum values when
*       they are not required.

         CALL LPG_STATE( 'BLACK', ACTBLA, STATUS )
         CALL LPG_STATE( 'WHITE', ACTWHI, STATUS )
         FNDRNG = ACTBLA .EQ. SUBPAR__ACTIVE .AND.
     :            ACTWHI .EQ. SUBPAR__ACTIVE

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
*          scaling into the cell array.  It is a negative picture by
*          default.

            CALL KPS1_DSCLR( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), INVERT, 'BLACK',
     :                       'WHITE', LP, NINTS-1, BPCI, RMINV, RMAXV,
     :                       .FALSE., %VAL( SCRPNT ), RIMLO,
     :                       RIMHI, STATUS )

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
*          scaling into the cell array.  It is a negative picture by
*          default.

            CALL KPS1_DSCLD( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), INVERT, 'BLACK',
     :                       'WHITE', LP, NINTS-1, BPCI, DMINV, DMAXV,
     :                       .FALSE., %VAL( SCRPNT ), DIMLO,
     :                       DIMHI, STATUS )

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
*          scaling into the cell array.  It is a negative picture by
*          default.

            CALL KPS1_DSCLI( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), INVERT, 'BLACK',
     :                       'WHITE', LP, NINTS-1, BPCI, IMINV, IMAXV,
     :                       .FALSE., %VAL( SCRPNT ), IIMLO,
     :                       IIMHI, STATUS )

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
*          scaling into the cell array.  It is a negative picture by
*          default.

            CALL KPS1_DSCLW( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), INVERT, 'BLACK',
     :                       'WHITE', LP, NINTS-1, BPCI, WMINV, WMAXV,
     :                       .FALSE., %VAL( SCRPNT ), WIMLO,
     :                       WIMHI, STATUS )

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
*          scaling into the cell array.  It is a negative picture by
*          default.

            CALL KPS1_DSCLB( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), INVERT, 'BLACK',
     :                       'WHITE', LP, NINTS-1, BPCI, BMINV, BMAXV,
     :                       .FALSE., %VAL( SCRPNT ), BIMLO,
     :                       BIMHI, STATUS )
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

*       Also get the number of histogram bins to be used---suggest
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
*       index for the device.  It is a negative picture by default.
*       =============================================================

         IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPG1_FLASR( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), LP, NINTS - 1, INVERT,
     :                       .FALSE., %VAL( SCRPNT ), STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG1_FLASD( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), LP, NINTS - 1, INVERT,
     :                       .FALSE., %VAL( SCRPNT ), STATUS )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
            CALL KPG1_FLASI( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), LP, NINTS - 1, INVERT,
     :                       .FALSE., %VAL( SCRPNT ), STATUS )

         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
            CALL KPG1_FLASW( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), LP, NINTS - 1, INVERT,
     :                       .FALSE., %VAL( SCRPNT ), STATUS )

         ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN
            CALL KPG1_FLASB( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), LP, NINTS - 1, INVERT,
     :                       .FALSE., %VAL( SCRPNT ), STATUS )
         END IF

*    End of the section to compute the cell array by the various
*    methods.

      END IF

      IF ( STATUS .EQ. SAI__OK ) THEN

*       Display the array.
*       ==================

*       Display the array, selecting the appropriate pointer to the
*       input data if no scaling has been performed, otherwise to the
*       scaled data in workspace.

         IF ( SCALE ) THEN
            CALL KPG1_GCA( XL, YL, XU, YU, DIMS( 1 ), DIMS( 2 ),
     :                     DIMS( 1 ), DIMS( 2 ), %VAL( SCRPNT ),
     :                     STATUS )
         ELSE
            CALL KPG1_GCA( XL, YL, XU, YU, DIMS( 1 ), DIMS( 2 ),
     :                     DIMS( 1 ), DIMS( 2 ), %VAL( PNTRI( 1 ) ),
     :                     STATUS )
         END IF

         CALL SGS_FLUSH

*       Finished with the input array so unmap it.

         CALL NDF_UNMAP( NDF, COMP, STATUS )

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
               CALL KPG1_DCLID( -NDIM, NDFC, DXLBND, DXUBND, DSCALE,
     :                          DOFSET, DACOOR, STATUS )
               DO I = 1, NDIM
                  AXLBND( I ) = REAL( DXLBND( I ) )
                  AXUBND( I ) = REAL( DXUBND( I ) )
               END DO
            ELSE
               CALL KPG1_DCLIR( -NDIM, NDFC, AXLBND, AXUBND, LSCALE,
     :                          OFFSET, DACOOR, STATUS )
            END IF

         END IF

         IF ( AXES .AND. .NOT. DATACO ) THEN

*          Just use the pixel bounds to define the annotated-axis
*          limits as there is no axis information or the user want
*          pixel indices on the axes.

            DO I = 1, NDIM
               AXLBND( I ) = REAL( SLBND( I ) - 1 )
               AXUBND( I ) = REAL( SUBND( I ) )
            END DO
         END IF

*       Draw the annotated axes.
*       ========================

         IF ( AXES ) THEN

*          Return to the image/frame zone.  This is in effect a frame
*          zone now since axes are going to be drawn within it.

            CALL SGS_SELZ( ZONEIF, STATUS )

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
         END IF

*       Make sure the output is complete as soon as possible.

         CALL SGS_FLUSH

*       Record the data picture in the database.
*       ========================================
*
*       Revert back to the frame picture.

         CALL AGI_SELP( PICID2, STATUS )

*       Record the picture and a reference to the NDF in the database.

         CALL KPG1_SDTRN( 'KAPPA_GREYPLOT', NDF, PICID3, STATUS )

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

*       Abort to prevent spurious error messages.

         IF ( STATUS .NE. SAI__OK ) GOTO 940

*       Draw the key.
*       =============

         IF ( KEY ) THEN

*          Select the full zone.

            CALL SGS_SELZ( ZONEK, STATUS )

*          The number of levels in the key must not exceed the number of
*          available pens.

            NLEVK = MIN( MAXCOL, NINTS - LP - 1 )

*          Draw the key and title, choosing the appropriate routine for
*          the implementation data type.  Note that the key is scaled
*          between the lower and upper colour indices used.

            IF ( ITYPE .EQ. '_REAL' ) THEN
               CALL KPG1_GKEYR( NLEVK, LP, NINTS-1, ASPKEY, RIMLO,
     :                          RIMHI, PLTITL, STATUS )

            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
               CALL KPG1_GKEYD( NLEVK, LP, NINTS-1, ASPKEY, DIMLO,
     :                          DIMHI, PLTITL, STATUS )

            ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
               CALL KPG1_GKEYI( NLEVK, LP, NINTS-1, ASPKEY, IIMLO,
     :                          IIMHI, PLTITL, STATUS )

            ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
               CALL KPG1_GKEYW( NLEVK, LP, NINTS-1, ASPKEY, WIMLO,
     :                          WIMHI, PLTITL, STATUS )

            ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN
               CALL KPG1_GKEYB( NLEVK, LP, NINTS-1, ASPKEY, BIMLO,
     :                          BIMHI, PLTITL, STATUS )
            END IF

*          Display the key in full immediately.

            CALL SGS_FLUSH

            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'ERR_GREYPLOT_KEY',
     :           'GREYPLOT: Error drawing the key.', STATUS )
            END IF
         END IF
      ELSE
         CALL ERR_REP( 'ERR_GREYPLOT_SCA',
     :     'GREYPLOT: Error scaling the picture.', STATUS )
      END IF

      IF ( SCALE ) THEN

*       Create the output NDF.
*       ======================

*       Begin an NDF context.

         CALL NDF_BEGIN

*       Begin a new error context.

         CALL ERR_MARK

*       Create the output NDF structure based on the input NDF.

         CALL LPG_PROP( NDF, 'AXIS,NOHISTORY,NOQUALITY,NOVARIANCE,'/
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

      END IF

 940  CONTINUE

*    Unmap and annul the workspace used for scaling.

      IF ( SCALE ) CALL AIF_ANTMP( SCRLOC, STATUS )

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

