      SUBROUTINE CRELUT( STATUS )
*+
*  Name:
*     CRELUT

*  Purpose:
*     Creates or manipulates an image-display lookup table using a
*     palette.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CRELUT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application allows a lookup table to be created or modified
*     interactively on a chosen image display from a palette of
*     colours.  All plotting is performed within the current
*     graphics-database picture for that device.  The phases in the
*     creation or manipulation of the lookup table are enumerated below.
*     1. The initial colour table is read from an NDF lookup-table file
*        or a greyscale used if there is no input lookup table.
*     2. The name of an NDF containing a 2-dimensional array is obtained
*        and the array is scaled and displayed in the top half of the
*        picture at the largest magnification without distortion.  Below
*        this an histogram of the values between the scaling limits is
*        drawn with the colour index of each bin corresponding to the
*        bin's scaled value.  Thus colours in the image and the
*        histogram match.   Axes of number versus data value are plotted
*        about the histogram.  If a null character, !, is given then no NDF
*        array is read and a ramp is produced instead of the histogram.
*        An axis of pen numbers in the lookup table is drawn around the
*        ramp.
*     3. A numbered palette is drawn below the histogram.  A palette
*        created in an earlier run of CRELUT may be restored from an
*        NDF.  Otherwise the palette comprises eight coloured blocks
*        (black, white, red, green, blue, yellow, magenta and cyan)
*        with palette numbers 0--7, an eight-level greyscale (8--15)
*        and a sixteen-level greyscale (16--31).  Palette numbers
*        16--31 may be replaced randomly by colours you define.  The
*        colours are specified by either the giving the red, green, blue
*        intensities; or by name.  The loop is terminated by a null.
*     4. Inside a loop you select the palette colour(s) to be assigned
*        to the first and last pen numbers of a band within the lookup
*        table.  For convenience, where there is an image and histogram
*        the equivalent data values are entered rather than pen numbers
*        directly, though they are converted to the nearest pens in the
*        lookup table.  Linear interpolation between the two palette
*        colours yields the lookup-table colours inside the band.
*        Should only one colour be given then all the pens in the
*        requested range are set to that colour.  Pen numbers may be
*        re-used indefinitely and assigned new colours if the desired
*        effect is not obtained.  (The histogram of the array is
*        produced to assist in a sensible choice).  The loop is
*        terminated by a null in response to either of the prompts.
*     5. The lookup table may be saved in an NDF.  A null response, !,
*        to the request for the name of the file in which the table is
*        to be stored will result in the table not being saved.
*        Likewise the palette may be saved in an NDF.

*  Usage:
*     crelut inlut outlut ndf [comp] low high [inpal] [outpal] [device]

*  ADAM Parameters:
*     COLOUR() = LITERAL (Read)
*        A colour to be added to the palette at the entry given by
*        parameter PALNUM.  It is specified in one of two ways.

*          o  A named colour from the standard colour set, which may
*          be abbreviated.  If the abbreviated name is ambiguous the
*          first match is selected.  The case of the name is ignored.
*          Some examples are "Seagreen", "Violet", and "Orchid".
*
*          o  Normalised red, green, and blue intensities separated by
*          commas or spaces.  Each value must lie in the range 0.0--1.0.
*          For example, "1.0,1.0,0.5" would give a pale yellow.

*        To exit the loop that obtains new palette colours enter a null
*        character (!) in response to the prompt.
*     COLRANGE() = _INTEGER (Read)
*        The numbers of the palette colours to be allocated to a range
*        of pens within the lookup table.  One or two palette colours
*        may be entered.  If only one is given all the range of pens
*        are assigned that colour.  If two palette colours are given
*        the colour of a pen is obtained by linear interpolation
*        between the two colours at the fractional position of the pen
*        in the range of colour indices.  Allowed values are 0--31.
*     COMP = LITERAL (Read)
*        The NDF component to be displayed.  It may be "Data",
*        "Quality", "Variance", or "Error" (where "Error" is the
*        alternative to "Variance" and causes the square root of the
*        variance values to be displayed).  If "Quality" is specified,
*        then the quality values are treated as numerical values (in
*        the range 0 to 255). ["Data"]
*     DEVICE = DEVICE (Read)
*        Name of the image display to be used.  The device must be in
*        one of the following GNS categories: IMAGE_DISPLAY,
*        IMAGE_OVERLAY, or WINDOW, and have at least 48 colour indices.
*        At least 120 colour indices is recommended.  The device must
*        also not reset when the device is opened (since the new colour
*        table would be lost).  [Current image-display device]
*     HIGH = _DOUBLE (Read)
*        This is the highest value in the 2-dimensional data array used
*        for scaling and computing the histogram.  All larger array
*        values are set to the highest colour index when HIGH is
*        greater than LOW, otherwise all array values greater than HIGH
*        are set to the lowest colour index.  The dynamic default is
*        the maximum data value.
*     INLUT = NDF (Read)
*        Name of the NDF containing the initial lookup table as its data
*        array.  The LUT must be 2-dimensional, the first dimension
*        being 3, and the second being arbitrary.  The method used to
*        compress or expand the colour table if the second dimension is
*        different from the number of unreserved colour indices is
*        controlled by parameter NN.  Also the LUT's values must lie in
*        the range 0.0--1.0.  If INLUT is null (!) a greyscale is used.
*     INPAL = NDF (Read)
*        Name of the NDF containing the initial palette as its data
*        array.  The palette must be 2-dimensional, the first dimension
*        being 3, and the second 32.  If the second dimension is
*        greater than 32 only the first 32 colours are used; if it has
*        less than 32 just fill as much of the palette as is possible
*        starting from the first colour.  The palette's values must lie
*        in the range 0.0--1.0.  If INPAL is null (!) the default
*        palette is loaded.
*     LOW = _DOUBLE (Read)
*        The array value that scales to the lowest pen in the colour
*        table, and the minimum value to be included in the histogram.
*        All smaller array values are set to the lowest colour
*        index when LOW is less than HIGH, otherwise all array values
*        smaller than LOW are set to the highest colour index.   The
*        dynamic default is the minimum data value.
*     NDF = NDF (Read)
*        Input NDF data structure containing the image to be displayed
*        to show the effect of the created colour table.
*     NN = _LOGICAL (Read)
*        If TRUE, the input lookup table is mapped to the colour table by
*        using the nearest-neighbour method.  This preserves sharp
*        edges and is better for lookup tables with blocks of colour.
*        If NN is FALSE, linear interpolation is used, and this is
*        suitable for smoothly varying colour tables. [FALSE]
*     OK = _LOGICAL (Read)
*        TRUE when the palette colour just produced is acceptable.
*     OUTLUT = NDF (Write)
*        The output lookup table.
*     OUTPAL = NDF (Write)
*        The palette used to create the lookup table.
*     PALNUM = _INTEGER (Read)
*        The number of the palette entry whose colour is to be
*        modified.  (The numbers are plotted on the palette.) It is
*        used within a loop to modify up to sixteen entries in the
*        palette.  Entering a null, !, will end that loop.  The
*        suggested default is the next palette number.  PALNUM must lie
*        in the range 16--31.
*     PENRANGE() = _INTEGER (Read)
*        The range of pen numbers in the lookup table which is about to
*        be allocated a set of colours from the palette.  PENRANGE is
*        only used when there is no image and histogram plotted.  The
*        pen number can be read from the axis below the ramp.  If one
*        pen number is given, only this pen is altered, and it is given
*        the first palette colour of COLRANGE.  If two are supplied,
*        the first pen number entered will take the first palette
*        colour entered, and the second pen is assigned the second
*        palette colour.  The pens must lie in the range zero to the
*        maximum number of available pens.
*     PTITLE = LITERAL (Read)
*        Title for the output palette NDF. ["KAPRH - Crelut"]
*     TITLE = LITERAL (Read)
*        Title for the output lookup table NDF. ["KAPRH - Crelut"]
*     VALRANGE() = _DOUBLE (Read)
*        The range of data values in the histogram/image which is to
*        be allocated a set of colours from the palette, and hence be
*        assigned to a part of the lookup table.  VALRANGE is only used
*        when there is an image and histogram plotted.  The data value
*        may be read from the axis below the histogram.  If one data
*        value is given, only the single pen in the lookup table
*        corresponding to the value is altered, and it is given the
*        first palette colour of COLRANGE.  If two values are supplied,
*        the first data value entered will take the first palette
*        colour entered, and the second data value is assigned the
*        second palette colour.  The data values must lie in the range
*        PVLO--PVHI.

*  Examples:
*     Note:
*        Since the application is highly interactive and contains loops
*        it is not possible to give one-line commands to perform a
*        complete operation.  Therefore the examples show how to
*        control the input and output data and not the interactive
*        manipulation of the colour table.
*
*     crelut heat bizarre hh12 \
*        Reads a lookup table in an NDF called heat.  If resampling of
*        the lookup table is required it achieved via linear
*        interpolation. The lookup table after the manipulation is
*        stored in NDF bizarre.  The data array in NDF hh12 is scaled
*        between its minimum and maximum values and displayed in the
*        top half of the current picture on the current image-display
*        device.  Also drawn is an histogram of the intensities.
*     crelut heat bizarre hh12 inpal=mypal \
*        As above except a palette created previously via the OUTPAL
*        parameter.  This palette is in an NDF called mypal.
*     crelut inlut=! deluxe hh12 v low=100 high=400 \
*        A greyscale lookup table is manipulated and the result
*        is stored in NDF deluxe.  The variance array in NDF hh12 is
*        scaled between 100 and 400, and displayed in the top half of
*        the current picture on the current image-display device.  Also
*        drawn is an histogram of the intensities between those limits.
*     crelut heat bizarre ndf=! device=xwindows \
*        Reads a lookup table in an NDF called heat.  If resampling of
*        the lookup table is required it achieved via the
*        nearest-neighbour method.  The lookup table after the
*        manipulation is stored in NDF bizarre.  A linear ramp is
*        displayed in the lower half of the current picture on the
*        xwindows device.

*  Notes:
*     -  The application stores, in the order given, the following
*     pictures in the graphics database: a frame comprising the data
*     picture, the histogram or ramp and the palette; the data-array
*     picture with world co-ordinates in units of data pixels; the
*     histogram/ramp frame picture including the histogram/ramp plus
*     the annotated axes; and the histogram with world co-ordinates in
*     units of data values and number, or the ramp with units of pen
*     numbers and normalised frequency.   The NDF associated with the
*     image/histogram plots is stored by reference with the DATA
*     picture.  On exit the current database picture for the chosen
*     device reverts to the input picture.
*     -  Bad pixels will appear with the lowest colour index in the
*     plot.

*  Algorithm:
*     -  Start the graphics system.  Check the workstation is a valid
*     image display.  Obtain the number of colour indices the size of
*     the display surface in pixels.  Find the lowest and number of
*     non-reserved colour indices.
*     -  Set up the workstation polyline colours for palette annotation.
*     -  Mimic opening with write access to prevent regeneration via
*     clearing the current zone.
*     -  Define the frame zone and two of three zones for each part of
*     the display retaining square pixels in displayed image.  Save the
*     frame in the graphics database.
*     -  Set up a preliminary lookup table.  Find which type of
*     resampling of the LUT is required.  Start NDF_, associate,
*     validate and map the input LUT.  Copy it to the working LUT.
*     Load a greyscale if the above failed.
*     -  Save the reserved colour indices.  Attempt to read a palette
*     from an NDF. If successful Start NDF_, associate, validate and
*     map the input LUT.  Copy it to the palette.  Otherwise create the
*     pre-defined palette of colours and greyscales.  Install the
*     palette and initial LUT into the image-display colour table.
*     -  Find which NDF component to display, obtain an identifier to
*     the NDF and check that the component is present.  Find the data
*     type for processing.  Get the NDF bounds, check that the NDF is
*     2-dimensional and inquire the bad-pixel flag.
*     -  If null was entered obtain workspace and fill it with a ramp
*     of the non-reserved colour indices.
*     -  Otherwise define the limiting positions of the cell array.
*     Define the zone for the image.  Obtain workspace and scale the
*     data between defined limits therein.  Display the array and free
*     the workspace.  Get more workspace for the histogram display and
*     the histogram itself.  Create them.  Find the height of the
*     histogram bin with the maximum count.  Record the image picture
*     in the database.  Tidy the NDF system.
*     -  Save the histogram/ramp frame picture in the database. Draw
*     annotated axes for the histogram or ramp.  Draw the histogram or
*     ramp cell array in the grid region.  Reset the world co-ordinates
*     to more-useful units: counts versus data value for the histogram,
*     and pen number for the ramp.  Store the histogram/ramp picture in
*     the database.  Define the scale and offset for converting between
*     pen number and dat value.
*     -  Set up the palette array and display it.  Write yellow or black
*     integral annotations depending on the colour of the palette entry.
*     -  Give some commentary to the user.  Obtain new palette entries,
*     via RGB intensities for a palette number, reporting the palette's
*     current RGB.  The annotation changes colour if necessary.  The new
*     palette colour may be rejected, and the old is replaced.  Loop
*     until null is entered.
*     -  Give some commentary to the user.  In a loop obtain pen or
*     data-value and colour-index ranges.  Convert data values to pen
*     numbers. For a single pen a block of colour is written to the
*     colour table.  For two pens linearly interpolate between them for
*     each colour index to be changed.  Loop until an error or null is
*     entered.
*     -  Save the created lookup table in an NDF with a title.
*     -  Restore the input, reserved pens.
*     -  Close the graphics system.

*  Implementation Deficiencies:
*     The method of obtaining colours is far from ideal, and will
*     require revision, probably via a GUI.

*  Related Applications:
*     KAPPA: LUTABLE, LUTFLIP, LUTREAD, LUTROT, LUTSAVE, LUTTWEAK,
*     LUTVIEW, PALREAD, PALSAVE; Figaro: COLOUR.

*  Implementation Status:
*     -  The magic-value method is used for processing bad data.
*     -  This application will handle data in all numeric types, though
*     type conversion to integer will occur for unsigned byte and word
*     images.

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 May 10 (MJC):
*        Original NDF version based on the pre-0.8 version.
*     1991 June 27 (MJC):
*        Improved the clarity of the documentation and instructions,
*        especially concerning the meaning of "pen".  Improved the user
*        interface by permitting nominated palette entries to be
*        revised, and showing the value of the existing entry.  Added
*        facilities to save and restore the palette.  Used full axes
*        including a correctly scaled ordinate, and data values on the
*        abscissa of the histogram.  LUT pens can be specified via data
*        values.  Stored data co-ordinates with the DATA picture
*        associated with the image.
*     1991 July 22 (MJC):
*        Replaced the RGB parameter by the more-powerful COLOUR.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1992 April 23 (MJC):
*        Made to handle significant dimensions for user-defined
*        sections of the image.
*     1992 November 30 (MJC):
*        Does not use non-monotonic axis centres.
*     1995 October 19 (MJC):
*        Supports Error component.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Standard SAE definitions
      INCLUDE 'DAT_PAR'        ! Data-system constants
      INCLUDE 'PAR_ERR'        ! Parameter-system errors
      INCLUDE 'SUBPAR_PAR'     ! SUBPAR constants
      INCLUDE 'CTM_PAR'        ! Colour-table management constants
      INCLUDE 'NDF_PAR'        ! NDF definitions
      INCLUDE 'PRM_PAR'        ! Magic-value definitions

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER HEIGHT           ! Height of ramp or histogram
      PARAMETER ( HEIGHT = 100 )

      INTEGER MXLUTE           ! Maximum lookup table entry
      PARAMETER ( MXLUTE = CTM__MXPEN )

      INTEGER NBLINE           ! Number of lines of coloured blocks
      PARAMETER ( NBLINE = 2 )

      INTEGER NDIM             ! Dimensionality required
      PARAMETER( NDIM = 2 )

      INTEGER NBLOCK           ! Number of coloured blocks.  Must be an
                               ! integer multiple of NBLINE.
      PARAMETER ( NBLOCK = 16 * NBLINE )

      INTEGER NPRICL           ! Number of primary colours
      PARAMETER ( NPRICL = 3 )

      REAL PALHGT              ! Palette-height scaling
      PARAMETER ( PALHGT = 0.125 )

      REAL PENTHR              ! Threshold of sum of RGB intensities
                               ! of the coloured blocks below which a
                               ! brightly-coloured annotation is drawn
                               ! so that label is visible
      PARAMETER ( PENTHR = 1.0 )

      REAL YHIGH               ! Fractional height of histogram/ramp and
                               ! palette so image appears in the top
                               ! 1 - YHIGH fraction of the picture
      PARAMETER ( YHIGH = 0.5 )

*  Local Variables:
      INTEGER
     :  ACTHIG,                ! The state of the HIGH parameter
     :  ACTLOW,                ! The state of the LOW parameter
     :  AEL,                   ! Number of elements in a mapped axis
     :  ANINTS,                ! Number of colour indices excluding
                               ! reserved pens
     :  AXPNTR( 1 ),           ! Pointer to a mapped axis
     :  BLOCKS( NBLOCK/NBLINE, NBLINE ),
                               ! Array containing the pen numbers of the
                               ! palette
     :  CDIMS( 2 ),            ! Dimensions of blocks of colours
     :  COLRNG( 2 ),           ! Range of colours to be fitted to pens
     :  DEFPNM,                ! Default palette number
     :  DIMS( 2 ),             ! Dimensions of input array
     :  EL,                    ! Number of elements in the input and
                               ! image array
     :  GSTAT                  ! Graphics status

      INTEGER
     :  HDIMS( 2 ),            ! Dimensions of ramp/histogram displayed
     :  HSTPTR,                ! Pointer to histogram
     :  I, J, K,               ! General variables
     :  IERR,                  ! Position of first conversion error
     :  IIMHI,                 ! Upper limit used for scaling the array
     :  IIMLO,                 ! Lower   "     "   "     "     "    "
     :  IMAXV,                 ! Minimum value in the image array
     :  IMINV                  ! Maximum value in the image array

      INTEGER
     :  IPIXX,                 ! Maximum number of columns of pixels
                               ! of the image display
     :  IPIXY,                 ! Maximum number of lines of pixels
                               ! of the image display
     :  LBND( NDF__MXDIM ),    ! Lower bounds of the image
     :  LDIMS( 2 ),            ! Dimensions of LUT arrays
     :  LEL,                   ! Number of elements in the input and
                               ! output LUT arrays
     :  LP,                    ! Lowest pen with which to display the
                               ! the image and histogram, or ramp
     :  LPNTR( 1 ),            ! Pointer to input colour table
     :  MAXPOS,                ! Dummy, returned by max-min routine
     :  MINPOS,                ! Dummy, returned by max-min routine
     :  MXVAL,                 ! Maximum number in an histogram bin
     :  MKPEN                  ! Text colour

      INTEGER
     :  NDF,                   ! Identifier for image NDF
     :  NDFI,                  ! Identifier for input LUT NDF
     :  NDFPI,                 ! Identifier for input palette NDF
     :  NDIMS,                 ! Actual number of dimensions
     :  NERR,                  ! Number of conversion errors
     :  NINTS,                 ! Number of greyscale intensities
                               ! available on the chosen device
     :  NINVAL,                ! Number of bad values in input array
     :  NVAL                   ! Number of values return from a
                               ! parameter system

      INTEGER
     :  PALNUM,                ! Palette number of the latest colour
                               ! supplied by the environment
     :  PDIMS( 2 ),            ! Dimensions of NDF palette arrays
     :  PEL,                   ! Number of elements in the input and
                               ! output palette NDF arrays
     :  PENRNG( 2 ),           ! Range of pens over which the
                               ! chosen colours to go
     :  PICID1,                ! Initial picture identifier
     :  PICID2,                ! Picture identifier for the frame
     :  PICID3,                ! Picture identifier for data array
     :  PICID4,                ! Picture identifier for histogram/ramp
     :  PICIDH,                ! Picture identifier for histogram/ramp
                               ! frame
     :  PNTRI( 1 ),            ! Pointer to the image array
     :  PPNTR( 1 )             ! Pointer to input colour table

      INTEGER
     :  RAMPTR,                ! Pointer to ramp/histogram cell array
     :  SCRPNT,                ! Pointer to work space used to invert
                               ! scale arrays for display
     :  SDIM( NDF__MXDIM ),    ! Significant dimensions of the image
     :  UBND( NDF__MXDIM ),    ! Upper bounds of the image
     :  WKID,                  ! Work station identification
     :  ZONEID,                ! Initial zone identification
     :  ZONEH,                 ! Histogram frame-zone identification
     :  ZONEHC,                ! Histogram/ramp cell-array-zone
                               ! identification
     :  ZONEI,                 ! Image zone identification
     :  ZONEP,                 ! Palette zone identification
     :  ZONET,                 ! Temporary zone identification
     :  ZONID2                 ! Zone identification

      LOGICAL                  ! True if :
     :  ANS,                   ! The specified colour is alright
     :  BAD,                   ! The array may contain bad pixels and
                               ! therefore testing should occur
     :  DACOOR,                ! Data co-ordinates are to be stored
                               ! in the database
     :  DEVCAN,                ! Image-display parameter is to be
                               ! cancelled
     :  DPAXIS,                ! Axis centres are double precision
     :  FNDRNG,                ! Find the data range for scaling
     :  IMAGE,                 ! An array is read in successfully
     :  INVERT,                ! The array is to inverted for display
     :  MONOTO( NDIM ),        ! Axis is monotonic
     :  NN,                    ! Mapping of input LUT to the colour
                               ! table uses nearest-neighbour method
                               ! (i.e. not linear interpolation)
     :  PALRED,                ! Palette has been read from an NDF
     :  REVSCA,                ! The data-to-pen scaling is negative
     :  THERE                  ! Requested component is defined

      CHARACTER * 72
     :  ATYPE * ( NDF__SZTYP ),! Processing type of the axis centres
     :  COMP * 8,              ! Component to be displayed
     :  DATTYP,                ! Histogram or ramp suffix for graphics
                               ! database description
     :  DTYPE * ( NDF__SZFTP ),! Type of the image after processing (not
                               ! used)
     :  ITYPE * ( NDF__SZTYP ),! Processing type of the image
     :  MCOMP * 8,             ! Component to be mapped
     :  POS*2                  ! Position-of-plot code

      CHARACTER*(DAT__SZLOC)   ! Locators for:
     :  HSTLOC,                ! Histogram
     :  RMPLOC,                ! Histogram/ramp cell array
     :  SCRLOC                 ! Scratch space for inversion and
                               ! scaling during display of cell arrays

      REAL
     :  AXLBND( NDIM ),        ! Axis lower bounds
     :  AXUBND( NDIM ),        ! Axis upper bounds
     :  BASCOL( NPRICL ),      ! Intensities of the primary colours of
                               ! the first colour used during fit of
                               ! colours to pens
     :  FAC( NPRICL ),         ! Rate of change of intensity of the
                               ! primary colours with pen number
     :  FI,                    ! Fraction of the display surface
                               ! reserved for the histogram/ramp and
                               ! palette
     :  GKSCOL( NPRICL ),      ! Temporary array used to deposit
                               ! intensities of the three primaries
                               ! when using the GKS pen-setting routine
     :  LUT( NPRICL, 0:MXLUTE ),
                               ! Lookup table
     :  MAJTIC( 2 ),           ! Parameters controlling the numbers of
                               ! major tick marks along x and y axes
                               ! respectively
     :  MAXVAL,                ! Floating MXVAL
     :  MINTIC( 2 ),           ! Numbers of minor tick marks along x and
                               ! y axes respectively
     :  NORM,                  ! Normalisation factor for initial
                               ! palette greyscales
     :  OFFSET( 2 ),           ! Offset for converting world to data
                               ! co-ordinates
     :  PALETT( NPRICL, 0:NBLOCK - 1 ),
                               ! Background lookup table
                               ! containing the complete lookup table
                               ! without the special entries
     :  PIXX,                  ! Number of columns of device pixels
                               ! in the input picture
     :  PIXY                   ! Number of lines of device pixels
                               ! in the input picture

      REAL
     :  PVLO,                  ! Low pixel value
     :  PVHI,                  ! High pixel value
     :  RESPEN( NPRICL, 0:CTM__RSVPN - 1 ),
                               ! Reserved pens on input
     :  RIMHI,                 ! Upper limit used for scaling the array
     :  RIMLO,                 ! Lower   "     "   "     "     "    "
     :  RMAXV,                 ! Minimum value in the image array
     :  RMINV,                 ! Maximum value in the image array
     :  RNINTS,                ! Scaling factor to convert lookup table
                               ! to range 0 to 1 for GKS
     :  SCALE( 2 ),            ! Scale for converting data values to pen
                               ! number
     :  X1, X2, Y1, Y2,        ! Zone size in world co-ordinates
     :  XL, XU, YL, YU,        ! Boundary of IMAGE cell array in world
                               ! co-ordinates
     :  XM, YM,                ! Zone size in metres
     :  Y,                     ! Y co-ordinate of label
     :  YRANGE                 ! Scale length used for the lower
                               ! of the plotting, e.g. to calculate
                               ! text height and positioning

      DOUBLE PRECISION
     :  DIMHI,                 ! Upper limit used for scaling the array
     :  DIMLO,                 ! Lower   "     "   "     "     "    "
     :  DMAXV,                 ! Minimum value in the image array
     :  DMINV,                 ! Maximum value in the image array
     :  DOFSET( 2 ),           ! Offset for converting world to data
                               ! co-ordinates
     :  DSCALE( 2 ),           ! Scale for converting data values to pen
                               ! number
     :  DXLBND( NDIM ),        ! Axis lower bounds
     :  DXUBND( NDIM ),        ! Axis upper bounds
     :  HOFSET,                ! Offset for converting data values to
                               ! pen number
     :  HSCALE,                ! Scale for converting data values to pen
                               ! number
     :  VALRNG( 2 )            ! Range of data values over which the
                               ! chosen colours go

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

*    Check the inherited status.

      IF ( STATUS .NE. SAI__OK ) RETURN

      DEVCAN = .FALSE.

*    Start the graphics system.
*    ==========================

*    Open up SGS (update mode rather write as some additional SGS pens
*    must be defined before clearing the zone).

      CALL AGS_ASSOC( 'DEVICE', 'UPDATE', ' ', PICID1, ZONEID, STATUS )

*    Check whether chosen device is an 'image display'.  It must have
*    a suitable minimum number of colour indices, and will not reset when opened.

      CALL KPG1_QVID( 'DEVICE', 'SGS', 'IMAGE_DISPLAY,IMAGE_OVERLAY,'/
     :                /'WINDOW', 'COLOUR,RESET', NBLOCK + 16, STATUS )

*    Obtain the number of colour indices and the maximum display
*    surface.

      CALL KPG1_QIDAT( 'DEVICE', 'SGS', NINTS, IPIXX, IPIXY, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN

*       The device name is to be cancelled to prevent an invalid device
*       being stored as the current value.

         DEVCAN = .TRUE.
         GOTO 999
      END IF

*    Find the lowest non-reserved colour index and the number of
*    non-reserved colour indices.

      LP = MAX( NBLOCK, CTM__RSVPN )
      ANINTS = NINTS - LP
      RNINTS = REAL( ANINTS - 1 )

*    Set up the workstation polyline colours.
*    ========================================

*    Obtain GKS workstation identifier.

      CALL SGS_ICURW( WKID )

*    Set special pens for SGS. 5 is black, 6 is yellow.  Adjust the
*    polyline representation.  Yellow is reserved pen number 5.  Black
*    is pen 0.

      J = 5
      MKPEN = J + 1

      CALL GSPLR( WKID, MKPEN, 1, 1, 5 )
      CALL GSPLR( WKID, J, 1, 1, 0 )

*    Mimic opening with write access.
*    ================================

*    Inquire the zone size.

      CALL SGS_IZONE( X1, Y1, X2, Y2, XM, YM )

*    Now clear the zone --- a fudge to prevent GKS regenerating
*    the device, in conjunction with UPDATE mode rather than opening
*    AGI with WRITE access as required.

      CALL SGS_BOX( X1, X2, Y1, Y2 )
      CALL SGS_FLUSH
      CALL SGS_CLRZ

*    Define the zones.
*    =================

*    Create and select a zone, centred for nearly square devices.

      IF ( XM/YM .GT. 0.5 .AND. XM/YM .LT. 2.0 ) THEN
         POS = 'CC'
      ELSE
         POS = 'BL'
      END IF

*    Define the picture sizes and some useful parameters.

      PIXX = REAL( IPIXX ) * XM
      PIXY = REAL( IPIXX ) * YM

      YRANGE = MIN( PIXY , 2.0 * YHIGH * PIXY )
      FI = ( 0.5 * YRANGE ) / PIXY

*    Obtain the largest zone whilst retaining square pixels.

      CALL SGS_ZSHAP( XM/YM, POS, ZONID2, STATUS )

*    Set the world co-ordinates of the image display.

      CALL SGS_SW( 0.0, PIXX, 0.0, PIXY, STATUS )

*    Create a zone for the histogram, first creating a temporary zone.
*    Its world co-ordinates are arbitrary as only axes will be drawn
*    within in via SNX/NCAR.

      CALL SGS_ZSHAP( PIXX/PIXY/FI, 'BC', ZONET, STATUS )
      CALL SGS_ZSHAP( PIXX/(0.5 - PALHGT)/YRANGE, 'TC', ZONEH, STATUS )
      CALL SGS_RELZ( ZONET )
      CALL SGS_SW( 0.0, PIXX, YRANGE * PALHGT, FI * PIXY, STATUS )

*    Create a zone for the palette.

      CALL SGS_SELZ( ZONID2, STATUS )
      CALL SGS_ZSHAP( PIXX/PALHGT/YRANGE, 'BC', ZONEP, STATUS )

*    Define its world co-ordinates in terms of pixels.

      CALL SGS_SW( 0.0, PIXX, 0.0, YRANGE*PALHGT, STATUS )

*    Inquire whether GKS/SGS has reported an error.

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CRELUT_ZONE',
     :     'CRELUT: Error while defining the shape or co-ordinates '/
     :     /'of a graphics zone on the image-display surface.',
     :     STATUS )

*       The device name is to be cancelled to prevent an invalid device
*       being stored as the current value.

         DEVCAN = .TRUE.
         GOTO 999
      END IF

*    Move back to the frame zone.

      CALL SGS_SELZ( ZONID2, STATUS )

*    Store this zone in the graphics database.

      CALL AGS_SZONE( 'FRAME', 'KAPRH_CRELUT', PICID2, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CRELUT_DBSF',
     :     'CRELUT: Error while storing the frame in the graphics '/
     :     /'database.', STATUS )
         GOTO 999
      END IF

*    Reset input picture as current in case of an accident

      CALL AGI_SELP( PICID1, STATUS )

*    Set up a preliminary lookup table.
*    ==================================

*    Decide whether the nearest-neighbour method or interpolation is
*    required to map the input lookup table to the device's colour
*    table.

      CALL PAR_GTD0L( 'NN', .FALSE., .TRUE., NN, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*    Start a new error context.

      CALL ERR_MARK

*    Start an NDF context.

      CALL NDF_BEGIN

*    Obtain the NDF identifier and pointer of the input
*    lookup table.  Validate the LUT.

      CALL KPG1_AVLUT( 'INLUT', NDFI, LPNTR, LEL, STATUS )

*    Obtain the array dimensions.

      CALL NDF_DIM( NDFI, NDIM, LDIMS, NDIMS, STATUS )

*    Transfer the input data to the lookup table.

      CALL KPG1_LUTIN( LDIMS( 2 ), %VAL( LPNTR( 1 ) ), ANINTS, NN, LUT,
     :                 STATUS )

*    The normal case when a null response is given indicates a
*    greyscale is to be used.

      IF ( STATUS .EQ. PAR__NULL ) THEN

*       Annul the error in this context.

         CALL ERR_ANNUL( STATUS )

*       Load the greyscale ramp ignoring the reserved pens.

         DO  J = 0, ANINTS-1, 1
            DO  I = 1, NPRICL, 1
               LUT( I, J ) = REAL( J ) / RNINTS
            END DO
         END DO

*    A fatal error occurred.

      ELSE IF ( STATUS .NE. SAI__OK ) THEN
         IF ( STATUS .NE. PAR__ABORT ) THEN

*          Report the error context.

            CALL NDF_MSG( 'INLUT', NDFI )
            CALL ERR_REP( 'CRELUT_NOTR',
     :        'CRELUT: Error reading the lookup table ^INLUT.', STATUS )
         END IF
         CALL ERR_RLSE

*       End the NDF context.

         CALL NDF_END( STATUS )
         GOTO 999
      END IF

*    End the NDF context.

      CALL NDF_END( STATUS )

*    End error context.

      CALL ERR_RLSE

*    Save the reserved colour indices.
*    =================================

*    The reserved colour indices will be overwritten by the palette,
*    therefore save them so they can be restored on completion.  Note
*    that the realised values are requied, otherwise the predefined
*    colours will be returned. Check for an error.

      DO  J = 0, CTM__RSVPN - 1
         CALL GQCR( WKID, J, 1, GSTAT, RESPEN( 1, J ), RESPEN( 2, J ),
     :              RESPEN( 3, J ) )
      END DO
      CALL GKS_GSTAT( STATUS )

*    Read an input palette.
*    ======================

*    The palette has yet to be read.

      PALRED = .FALSE.

*    Find dimensions of the coloured-block array.

      CDIMS( 2 ) = NBLINE
      CDIMS( 1 ) = NBLOCK / CDIMS( 2 )

*    Start a new error context.

      CALL ERR_MARK

*    Start an NDF context.

      CALL NDF_BEGIN

*    Obtain the NDF identifier and pointer of the input palette.
*    Validate the palette.  Actually this may not be good enough.
*    If the NDF's second dimension is greater than 32 we can just use
*    the first 32 entries.  However, if it has less than 32 just
*    fill what we can.

      CALL KPG1_AVLUT( 'INPAL', NDFPI, PPNTR, PEL, STATUS )

*    Obtain the array dimensions.

      CALL NDF_DIM( NDFPI, NDIM, PDIMS, NDIMS, STATUS )

*    Transfer the input data to the palette.  There will be no
*    conversion errors as we are merely copying data to the same type.

      CALL VEC_RTOR( .FALSE., 3 * MIN( PDIMS( 2 ), NBLOCK ),
     :               %VAL( PPNTR( 1 ) ), PALETT, IERR, NERR, STATUS )

*    Fill the remainder of the palette with a greyscale.

      IF ( STATUS .EQ. SAI__OK .AND. PDIMS( 2 ) .LT. NBLOCK ) THEN

         NORM = REAL( NBLOCK - PDIMS( 2 ) - 1 )
         DO  I = PDIMS( 2 ), NBLOCK - 1, 1
            DO  J = 1, NPRICL
               PALETT( J, I ) = REAL( I - PDIMS( 2 ) ) / NORM
            END DO
         END DO
      END IF

*    The normal case when a null response is given indicates that the
*    standard palette is to be used.

      IF ( STATUS .EQ. PAR__NULL ) THEN

*       Annul the error in this context.

         CALL ERR_ANNUL( STATUS )

*    A fatal error occurred.

      ELSE IF ( STATUS .NE. SAI__OK ) THEN
         IF ( STATUS .NE. PAR__ABORT ) THEN

*          Report the error context.

            CALL NDF_MSG( 'INPAL', NDFPI )
            CALL ERR_REP( 'CRELUT_NOTPAL',
     :        'CRELUT: Error reading the palette in ^INPAL.', STATUS )
         END IF
         CALL ERR_RLSE

*       End the NDF context.

         CALL NDF_END( STATUS )
         GOTO 999

      ELSE

*       The palette was read successfully.

         PALRED = .TRUE.
      END IF

*    End the NDF context.

      CALL NDF_END( STATUS )

*    End error context.

      CALL ERR_RLSE

*    Create the pre-defined palette colours.
*    =======================================

*    There are CDIMS( 2 ) lines of standard colour indices that form a
*    palette.  These are only used for the palette and not for the
*    image, histogram or ramp.  The palette overwrites the original
*    reserved pens.

      IF ( .NOT. PALRED )
     :  CALL KPS1_CLPAL( CDIMS( 1 ), CDIMS( 2 ), PALETT, STATUS )

*    Install the palette into image-display colour table.
*    ====================================================

      DO  I = 0, NBLOCK - 1, 1
         CALL GSCR( WKID, I, PALETT( 1, I ), PALETT( 2, I ),
     :              PALETT( 3, I ) )
      END DO

*    Install the initial lookup table into image-display colour table.
*    =================================================================

*    The lookup table follows the palette, hence the offset in the
*    colour index.

      DO  I = 0, ANINTS - 1, 1
         CALL GSCR( WKID, I + NBLOCK, LUT( 1, I ), LUT( 2, I ),
     :              LUT( 3, I ) )
      END DO

*    Make the change immediately.  Check for an error within GKS.
*    Aborts will now restore the input reserved colour indices.

      CALL SGS_FLUSH
      CALL GKS_GSTAT( STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 980

*    Get the NDF for which the colour-table is being designed.
*    =========================================================

*    Start a new error context.

      CALL ERR_MARK

*    If no NDF is displayed a linear ramp will be plotted instead of
*    the histogram.  The IMAGE keeps a record of whether an NDF image
*    has been displayed.  Assume there is no image unless and until
*    one is read.

      IMAGE = .FALSE.

*    Find which component to display, converting 'ERROR' into
*    'VARIANCE'.

      CALL PAR_CHOIC( 'COMP', 'Data', 'Data,Quality,Error,Variance',
     :                .FALSE., COMP, STATUS )
      MCOMP = COMP
      IF ( COMP .EQ. 'ERROR' ) COMP = 'VARIANCE'

*    Begin an NDF context.

      CALL NDF_BEGIN

*    Obtain the identifier of the NDF to be displayed.

      CALL LPG_ASSOC( 'NDF', 'READ', NDF, STATUS )

*    There must be a data array, but for other components check that
*    requested component is present.

      IF ( COMP .NE. 'DATA' .AND. STATUS .EQ. SAI__OK ) THEN
         CALL NDF_STATE( NDF, COMP, THERE, STATUS )

*       The component is not present or not defined, so make an error
*       report containing the NDF's name.

         IF ( .NOT. THERE ) THEN
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', NDF )
            CALL MSG_SETC( 'COMP', COMP )
            CALL ERR_REP( 'CRELUT_NOCOMP',
     :        'CRELUT: ^COMP component is not defined in NDF ^NDF.',
     :        STATUS )
         END IF
      END IF

*    This application can only process real, double precision and
*    integer components directly.  Therefore for the given type of the
*    image find in which type it should be processed.

      CALL NDF_MTYPE( '_BYTE,_WORD,_INTEGER,_REAL,_DOUBLE', NDF, NDF,
     :                COMP, ITYPE, DTYPE, STATUS )

*    Find whether or not there are but two significant dimensions and
*    which ones they are.

      CALL KPG1_SGDIM( NDF, NDIM, SDIM, STATUS )

*    Obtain the bounds of the image.  These will be stored in the
*    graphics database once the cell-array is displayed.

      CALL NDF_BOUND( NDF, NDF__MXDIM, LBND, UBND, NDIMS, STATUS )

*    Watch for an error to avoid a spurious error message and a clean
*    closedown.

      IF ( STATUS .EQ. SAI__OK ) THEN

*       Compute the dimensions.

         DIMS( 1 ) = UBND( SDIM( 1 ) ) - LBND( SDIM( 1 ) ) + 1
         DIMS( 2 ) = UBND( SDIM( 2 ) ) - LBND( SDIM( 2 ) ) + 1

*       Check that the dimensions are significant.  This should
*       disappear with the next version of NDF.

         IF ( DIMS( 1 ) .LT. 2 .OR. DIMS( 2 ) .LT. 2 ) THEN
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', NDF )
            CALL ERR_REP( 'CRELUT_IVDIM2',
     :        'CRELUT: NDF ^NDF does not have two significant '/
     :        /'dimensions.', STATUS )
         END IF
      END IF

*    Check whether or not bad pixels may be present.

      CALL NDF_BAD( NDF, COMP, .FALSE., BAD, STATUS )

*    Map the NDF.

      CALL KPG1_MAP( NDF, MCOMP, ITYPE, 'READ', PNTRI, EL, STATUS )

*    If the user wants to abort, then leave the program.

      IF ( STATUS .NE. SAI__OK ) THEN

*       Tidy the error and NDF contexts and exit whenever an error has
*       occurred.  A null response is not an error, as this means plot
*       a linear ramp, so annul it.

         IF ( STATUS .NE. PAR__NULL ) THEN
            CALL ERR_RLSE
            CALL NDF_END( STATUS )
            GOTO 980
         END IF

         CALL ERR_ANNUL( STATUS )

*       Obtain workspace for a ramp.
*       ============================

         HDIMS( 1 ) = ANINTS
         HDIMS( 2 ) = 1

*       Create work space for ramp display.

         CALL AIF_GETVM( '_INTEGER', 1, ANINTS, RAMPTR, RMPLOC, STATUS )

         IF ( STATUS .EQ. SAI__OK ) THEN

*           Fill the array with element numbers less one to give colour
*           indices.  Omit the reserved pens.  Note this is may not be
*           the usual number.

             CALL KPG1_ELNMI( LP, NINTS-1, ANINTS, %VAL( RAMPTR ),
     :                        STATUS )

         ELSE

*          Report the error and tidy.

            CALL ERR_REP( 'CRELUT_WSPR',
     :        'CRELUT: Unable to get workspace for forming a ramp.',
     :        STATUS )
            CALL ERR_RLSE
            CALL AIF_ANTMP( RMPLOC, STATUS )
            GOTO 980
         END IF

         DATTYP = 'Ramp'

      ELSE

*       Record that the image is to be displayed.

         IMAGE = .TRUE.

*       Define default limiting positions of the cell array.
*       ====================================================

         XL = REAL( LBND( SDIM( 1 ) ) - 1 )
         YL = REAL( LBND( SDIM( 2 ) ) - 1 )
         XU = REAL( UBND( SDIM( 1 ) ) )
         YU = REAL( UBND( SDIM( 2 ) ) )

*       Define the zone in which the image is to be plotted.
*       ====================================================

*       Select the frame picture.

         CALL AGI_SELP( PICID2, STATUS )
         CALL SGS_SELZ( ZONID2, STATUS )

*       Create a zone for the picture, first creating a temporary zone.

         CALL SGS_ZSHAP( PIXX / PIXY / ( 1. - FI ), 'TL', ZONET,
     :                   STATUS )
         CALL SGS_ZSHAP( REAL( DIMS( 1 ) )/ REAL( DIMS( 2 ) ), 'CC',
     :                   ZONEI, STATUS )
         CALL SGS_RELZ( ZONET )

*       Switch to the image zone and set the world co-ordinates to the
*       array bounds following normal convention.

         CALL SGS_SELZ( ZONEI, STATUS )
         CALL SGS_SW( XL, XU, YL, YU, STATUS )

*       Draw the image, suitably scaled.
*       ================================

*       The array is to be inverted vertically because the GKS
*       convention is for the origin to be at the top.

         INVERT = .TRUE.

*       Create work space for inversion and scaling.

         CALL AIF_GETVM( '_INTEGER', 2, DIMS, SCRPNT, SCRLOC, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'CRELUT_WSPI',
     :        'CRELUT: Unable to get workspace for displaying the NDF.',
     :        STATUS )
            CALL AIF_ANTMP( SCRLOC, STATUS )
            CALL ERR_RLSE
            CALL NDF_END( STATUS )
            GOTO 980
         END IF

*       Determine whether or not the scaling parameters have been
*       found, to avoid finding the maximum and minimum values when
*       they are not required.

         CALL LPG_STATE( 'LOW', ACTLOW, STATUS )
         CALL LPG_STATE( 'HIGH', ACTHIG, STATUS )
         FNDRNG = ACTLOW .EQ. SUBPAR__ACTIVE .AND.
     :            ACTHIG .EQ. SUBPAR__ACTIVE

*       Select appropriate routine for the data type chosen and scale
*       the image between user-defined limits.  The cell array has
*       values between the colour-index limits LP and the largest
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
*          scaling into the cell array.  It is a positive picture by
*          default.

            CALL KPS1_DSCLR( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), INVERT, 'LOW', 'HIGH',
     :                       LP, NINTS-1, CTM__RSVPN, RMINV, RMAXV,
     :                       .TRUE., %VAL( SCRPNT ), RIMLO, RIMHI,
     :                       STATUS )

*          Define the zone world co-ordinates and the sign of the
*          scaling.

            REVSCA = RIMLO .GT. RIMHI
            PVLO = MIN( RIMLO, RIMHI )
            PVHI = MAX( RIMLO, RIMHI )

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
*          scaling into the cell array.  It is a positive picture by
*          default.

            CALL KPS1_DSCLD( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), INVERT, 'LOW', 'HIGH',
     :                       LP, NINTS-1, CTM__RSVPN, DMINV, DMAXV,
     :                       .TRUE., %VAL( SCRPNT ), DIMLO, DIMHI,
     :                       STATUS )

*          Define the zone world co-ordinates and the sign of the
*          scaling.

            REVSCA = DIMLO .GT. DIMHI
            PVLO = SNGL( MIN( DIMLO, DIMHI ) )
            PVHI = SNGL( MAX( DIMLO, DIMHI ) )

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
*          scaling into the cell array.  It is a positive picture by
*          default.

            CALL KPS1_DSCLI( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), INVERT, 'LOW', 'HIGH',
     :                       LP, NINTS-1, CTM__RSVPN, IMINV, IMAXV,
     :                       .TRUE., %VAL( SCRPNT ), IIMLO, IIMHI,
     :                       STATUS )

*          Define the zone world co-ordinates and the sign of the
*          scaling.

            REVSCA = IIMLO .GT. IIMHI
            PVLO = REAL( MIN( IIMLO, IIMHI ) )
            PVHI = REAL( MAX( IIMLO, IIMHI ) )

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
*          scaling into the cell array.  It is a positive picture by
*          default.

            CALL KPS1_DSCLW( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), INVERT, 'LOW', 'HIGH',
     :                       LP, NINTS-1, CTM__RSVPN, WMINV, WMAXV,
     :                       .TRUE., %VAL( SCRPNT ), WIMLO, WIMHI,
     :                       STATUS )

*          Define the zone world co-ordinates and the sign of the
*          scaling.

            REVSCA = WIMLO .GT. WIMHI
            PVLO = MIN( NUM_WTOR( WIMLO ), NUM_WTOR( WIMHI ) )
            PVHI = MAX( NUM_WTOR( WIMLO ), NUM_WTOR( WIMHI ) )

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
*          scaling into the cell array.  It is a positive picture by
*          default.

            CALL KPS1_DSCLB( BAD, DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( PNTRI( 1 ) ), INVERT, 'LOW', 'HIGH',
     :                       LP, NINTS-1, CTM__RSVPN, BMINV, BMAXV,
     :                       .TRUE., %VAL( SCRPNT ), BIMLO, BIMHI,
     :                       STATUS )

*          Define the zone world co-ordinates and the sign of the
*          scaling.

            REVSCA = BIMLO .GT. BIMHI
            PVLO = MIN( NUM_BTOR( BIMLO ), NUM_BTOR( BIMHI ) )
            PVHI = MAX( NUM_BTOR( BIMLO ), NUM_BTOR( BIMHI ) )
         END IF

*       Display the array.
*       ==================

*       Display the array, selecting the appropriate pointer to the
*       input data if no scaling has been performed, otherwise to the
*       scaled data in workspace.

         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL KPG1_GCA( XL, YL, XU, YU, DIMS( 1 ), DIMS( 2 ),
     :                     DIMS( 1 ), DIMS( 2 ), %VAL( SCRPNT ),
     :                     STATUS )
            CALL SGS_FLUSH
         END IF

*       Check that GCA has not reported an error.

         CALL GKS_GSTAT( STATUS )

*       Tidy the workspace.

         CALL AIF_ANTMP( SCRLOC, STATUS )

*       Tidy and exit following an error.

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_RLSE
            CALL NDF_END( STATUS )
            GOTO 980
         END IF

*       Draw the histogram.
*       ===================

         HDIMS( 1 ) = ANINTS
         HDIMS( 2 ) = HEIGHT

*       Create work space for the histogram display.

         CALL AIF_GETVM( '_INTEGER', 2, HDIMS, RAMPTR, RMPLOC, STATUS )

*       Create work space for the histogram itself.

         CALL AIF_GETVM( '_INTEGER', 1, HDIMS( 1 ), HSTPTR, HSTLOC,
     :                   STATUS )

         IF ( STATUS .EQ. SAI__OK ) THEN

*          Create the histogram of the array and a cell-array
*          histogram.  In the image of the histogram each column has
*          the appropriate pen corresponding to the data value.   The
*          height of the column is proprotional to the number of values
*          in that bin.   Choose the appropriate routine for each
*          possible data type.

            IF ( ITYPE .EQ. '_REAL' ) THEN
               CALL KPS1_HSTCR( BAD, EL, %VAL( PNTRI( 1 ) ), RIMLO,
     :                          RIMHI, LP, INVERT, HDIMS( 1 ),
     :                          HDIMS( 2 ), MXVAL, %VAL( HSTPTR ),
     :                          %VAL( RAMPTR ), STATUS )

            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
               CALL KPS1_HSTCD( BAD, EL, %VAL( PNTRI( 1 ) ), DIMLO,
     :                          DIMHI, LP, INVERT, HDIMS( 1 ),
     :                          HDIMS( 2 ), MXVAL, %VAL( HSTPTR ),
     :                          %VAL( RAMPTR ), STATUS )

            ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
               CALL KPS1_HSTCI( BAD, EL, %VAL( PNTRI( 1 ) ), IIMLO,
     :                          IIMHI, LP, INVERT, HDIMS( 1 ),
     :                          HDIMS( 2 ), MXVAL, %VAL( HSTPTR ),
     :                          %VAL( RAMPTR ), STATUS )

            ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
               CALL KPS1_HSTCW( BAD, EL, %VAL( PNTRI( 1 ) ), WIMLO,
     :                          WIMHI, LP, INVERT, HDIMS( 1 ),
     :                          HDIMS( 2 ), MXVAL, %VAL( HSTPTR ),
     :                          %VAL( RAMPTR ), STATUS )

            ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN
               CALL KPS1_HSTCB( BAD, EL, %VAL( PNTRI( 1 ) ), BIMLO,
     :                          BIMHI, LP, INVERT, HDIMS( 1 ),
     :                          HDIMS( 2 ), MXVAL, %VAL( HSTPTR ),
     :                          %VAL( RAMPTR ), STATUS )

            END IF

*          Get the maximum bin so the zone world co-ordinates for the
*          graphics database and the axes of the histogram scaling may
*          be correctly normalised.

            MAXVAL = REAL( MXVAL )

*       Report the error context.  Tidy the workspace and the image NDF.

         ELSE
            CALL ERR_REP( 'CRELUT_WSH',
     :        'CRELUT: Unable to get workspace for creating the '/
     :        /'histogram.', STATUS )
            CALL ERR_RLSE
            CALL AIF_ANTMP( HSTLOC, STATUS )
            CALL AIF_ANTMP( RMPLOC, STATUS )
            CALL NDF_END( STATUS )
            GOTO 980
         END IF

*       Tidy the workspace for the histogram.

         CALL AIF_ANTMP( HSTLOC, STATUS )

*       Record the data picture in the database.
*       ========================================

         CALL KPG1_SDTRN( 'KAPRH_CRELUT_Picture', NDF, PICID3,
     :                    STATUS )

*       Record for messages the type of the plot.

         DATTYP = 'Histogram'

*       Find the implementation type of the axis structure.
*       ===================================================

*       Is there an axis system?

         CALL NDF_STATE( NDF, 'Axis', DACOOR, STATUS )

*       Integer needs d.p. because it potentially has ten significant
*       digits.  Record the fact for compactness and efficiency.

         IF ( DACOOR ) THEN
            CALL KPG1_AXTYP( NDF, 'Centre', ATYPE, STATUS )
            DPAXIS = ATYPE .EQ. '_DOUBLE'

*       Obtain the axis co-ordinates.
*       =============================

*       To be able to store the NDF data co-ordinates in the database
*       the application requires a linear axis.
*
            IF ( DPAXIS ) THEN
               DO  I = 1, NDIM

*                Map the axis.

                  CALL NDF_AMAP( NDF, 'Centre', SDIM( I ), '_DOUBLE',
     :                           'READ', AXPNTR, AEL, STATUS )

*                Are all the axes monotonic?  Start a new error context
*                so that the error reports concerning a non-monotonic
*                axis may be annulled.  Instead we issue a warning
*                message so that the application can continue by using
*                world co-ordinates.

                  IF ( STATUS .EQ. SAI__OK ) THEN
                     CALL ERR_MARK
                     CALL KPG1_MONOD( .TRUE., AEL, %VAL( AXPNTR( 1 ) ),
     :                                MONOTO( I ), STATUS )
                     IF ( STATUS .NE. SAI__OK ) THEN
                        CALL ERR_ANNUL( STATUS )
                        MONOTO( I ) = .FALSE.
                     END IF
                     CALL ERR_RLSE
                  END IF

*                Issue the warning.

                  IF ( .NOT. MONOTO( I ) ) THEN
                     CALL MSG_SETI( 'IAXIS', SDIM( I ) )
                     CALL MSG_OUT( 'CRELUT_NOTMONO',
     :                 'CRELUT: Axis ^IAXIS is not monotonic.  Will '/
     :                 /'not record axis bounds in the graphics '/
     :                 /'database.', STATUS )
                  END IF

*                Unmap the axis.

                  CALL NDF_AUNMP( NDF, 'Centre', SDIM( I ), STATUS )
               END DO
            ELSE

               DO  I = 1, NDIM

*                Map the axis.

                  CALL NDF_AMAP( NDF, 'Centre', SDIM( I ), '_REAL',
     :                           'READ', AXPNTR, AEL, STATUS )

*                Are all the axes monotonic?  Start a new error context
*                so that the error reports concerning a non-monotonic
*                axis may be annulled.  Instead we issue a warning
*                message so that the application can continue by using
*                world co-ordinates.

                  IF ( STATUS .EQ. SAI__OK ) THEN
                     CALL ERR_MARK
                     CALL KPG1_MONOR( .TRUE., AEL, %VAL( AXPNTR( 1 ) ),
     :                                MONOTO( I ), STATUS )
                     IF ( STATUS .NE. SAI__OK ) THEN
                        CALL ERR_ANNUL( STATUS )
                        MONOTO( I ) = .FALSE.
                     END IF
                     CALL ERR_RLSE
                  END IF

*                Issue the warning.

                  IF ( .NOT. MONOTO( I ) ) THEN
                     CALL MSG_SETI( 'IAXIS', SDIM( I ) )
                     CALL MSG_OUT( 'CRELUT_NOTMONO',
     :                 'CRELUT: Axis ^IAXIS is not monotonic.  Will '/
     :                 /'not record axis bounds in the graphics '/
     :                 /'database.', STATUS )
                  END IF

*                Unmap the axis.

                  CALL NDF_AUNMP( NDF, 'Centre', SDIM( I ), STATUS )
               END DO
            END IF

            IF ( MONOTO( 1 ) .AND. MONOTO( 2 ) ) THEN

*          The axis is monotonic, determine whether or not the data
*          co-ordinates derived from the NDF axes are linear and not
*          identical to world (pixel) co-ordinates, and if so find the
*          linear transformation from world to data co-ordinates and
*          the axis bounds.

               IF ( DPAXIS ) THEN
                  CALL KPG1_DCLID( NDIM, NDF, DXLBND, DXUBND, DSCALE,
     :                             DOFSET, DACOOR, STATUS )
                  AXLBND( 1 ) = REAL( DXLBND( 1 ) )
                  AXLBND( 2 ) = REAL( DXLBND( 2 ) )
               ELSE
                  CALL KPG1_DCLIR( NDIM, NDF, AXLBND, AXUBND, SCALE,
     :                             OFFSET, DACOOR, STATUS )
               END IF
         END IF

*       Set the flag to indicate no axis centres are present, when
*       either axis is non-monotonic.

         ELSE
            DACOOR = .FALSE.
         END IF

*       Store a transformation from data co-ordinates to world
*       co-ordinates where they are different and the data co-ordinates
*       are linear.

         IF ( DACOOR ) THEN
            IF ( DPAXIS ) THEN
               CALL KPG1_LITRD( DSCALE, DOFSET, STATUS )
            ELSE
               CALL KPG1_LITRR( SCALE, OFFSET, STATUS )
            END IF
         END IF

*       End the NDF context for displaying the image, and computing its
*       histogram.

         CALL NDF_END( STATUS )

*       Reset the input picture as current in case of an accident.

         CALL AGI_SELP( PICID1, STATUS )

*    FIRST plot and use of the input image ends here..

      END IF

*    Release the error context.

      CALL ERR_RLSE

*    SECOND plot (histogram or ramp).
*    ================================

*    Move to the frame zone for the histogram or ramp.

      CALL AGI_SELP( PICID2, STATUS )
      CALL SGS_SELZ( ZONEH, STATUS )

*    Store the information in the database.

      CALL AGS_SZONE( 'FRAME', 'KAPRH_CRELUT_'//DATTYP, PICIDH, STATUS )

*    Get AUTOGRAPH to use the SGS zone.

      CALL SNX_AGWV

      IF ( IMAGE ) THEN

*       Draw annotated axes in the graph window for the histogram.
*       Note the NDF values are used to define the abscissa bounds of
*       the annotated axes.  Note also that there may be problems for
*       d.p. data co-ordinates due to GKS's use of single precision.
*       The lines have normal thickness and tick marks are drawn outside
*       the grid window.

         MINTIC( 1 ) = -1.0
         MINTIC( 2 ) = -1.0
         MAJTIC( 1 ) = 4.0
         MAJTIC( 2 ) = 4.0
         CALL NCRAXS( PVLO, 0.0, PVHI, MAXVAL, DATTYP, 'Data values',
     :                'Number', MINTIC, MAJTIC, .TRUE., 1.0, .FALSE.,
     :                STATUS )

      ELSE

*       Draw annotated axes in the graph window for the ramp.

         MINTIC( 1 ) = -1.0
         MINTIC( 2 ) = -1.0
         MAJTIC( 1 ) = 4.0
         MAJTIC( 2 ) = 1.0
         CALL NCRAXS( 0.0, 0.0, RNINTS + 1.0, 1.0, DATTYP, 'LUT pens',
     :                ' ', MINTIC, MAJTIC, .TRUE., 1.0, .FALSE.,
     :                STATUS )
      END IF

*    The histogram or ramp cell array will in be in the NCAR grid
*    window.  The NCAR grid window is defined to lie between world
*    co-ordinates 0--1 along each axis.

      CALL SNX_AGCS
      CALL SGS_ZONE( 0.0, 1.0, 0.0, 1.0, ZONEHC, STATUS )
      CALL SGS_SW( 0.0, 1.0, 0.0, 1.0, STATUS )

*    The histogram or ramp cell-array is already scaled to the allowed
*    range of colour indices, therefore just display the image of the
*    histogram or ramp.

      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL KPG1_GCA( 0.0, 0.0, 1.0, 1.0, HDIMS( 1 ), HDIMS( 2 ),
     :                  HDIMS( 1 ), HDIMS( 2 ), %VAL( RAMPTR ),
     :                  STATUS )
         CALL SGS_BOX( 0.0, 1.0, 0.0, 1.0 )
         CALL SGS_FLUSH
      END IF

*    Check that GCA has not reported an error.

      CALL GKS_GSTAT( STATUS )

*    Tidy the work space for the ramp or histogram.

      CALL AIF_ANTMP( RMPLOC, STATUS )

*    Report the context and abort.

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'DATTYP', DATTYP )
         CALL ERR_REP( 'CRELUT_HSTDSP',
     :     'CRELUT: Error displaying the ^DATTYP.', STATUS )
         GOTO 980
      END IF

*    Define the world co-ordinates of the histogram or ramp zone.
*    ============================================================
*
*    Now redefine the world co-ordinates to be something more useful
*    for the database.

      IF ( IMAGE ) THEN

*       Define the world co-ordinate system for the histogram.
*       It refers to actual values, rather than pen numbers.

         CALL SGS_SW( PVLO, PVHI, 0.0, MAXVAL, STATUS )

      ELSE

*       The ramp's abscissa world co-ordinates are in terms of pen
*       numbers.

         CALL SGS_SW( 0.0, RNINTS, 0.0, REAL( HDIMS( 2 ) ), STATUS )
      END IF

*    Store the ramp or histogram picture in the database.
*    ====================================================

      CALL AGS_SZONE( 'DATA', 'KAPRH_CRELUT_'//DATTYP, PICID4, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'DATTYP', DATTYP )
         CALL ERR_REP( 'CRELUT_DBSH',
     :     'CRELUT: Error while storing the ^DATTYP zone in '/
     :     /'the graphics database.', STATUS )
         GOTO 980
      END IF

      IF ( IMAGE ) THEN

*       A linear transformation is needed for converting data values to
*       pen numbers.  Find the scale and offset, using the maximum
*       precision.  Watch for the case with reverse scaling when using
*       the axis co-ordinates.  (This is done to avoid a long if type =
*       conditional section.)

         IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            HSCALE = ( DIMHI - DIMLO ) / DBLE( RNINTS )
            HOFSET = DIMLO
         ELSE IF ( REVSCA ) THEN
            HSCALE = DBLE( PVLO - PVHI ) / DBLE( RNINTS )
            HOFSET = DBLE( PVHI )
         ELSE
            HSCALE = DBLE( PVHI - PVLO ) / DBLE( RNINTS )
            HOFSET = DBLE( PVLO )
         END IF
      END IF

*    Reset the input picture as current in case of an accident.

      CALL AGI_SELP( PICID1, STATUS )

*    Second plot ends.

*    THIRD plot (palette).
*    =====================

*    Set up the available colours, and vertically inverting ready for
*    display.

      K = 0
      DO  I = CDIMS( 2 ), 1, -1
         DO  J = 1, CDIMS( 1 ), 1
            BLOCKS( J, I ) = K
            K = K + 1
         END DO
      END DO

*    Switch to the palette zone.

      CALL AGI_SELP( PICID2, STATUS )
      CALL SGS_SELZ( ZONEP, STATUS )

*    Display the available colours in the palette.

      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL KPG1_GCA( 0.0, 0.0, PIXX, YRANGE * PALHGT, CDIMS( 1 ),
     :                  CDIMS( 2 ), CDIMS( 1 ), CDIMS( 2 ), BLOCKS,
     :                  STATUS )
         CALL SGS_FLUSH
      END IF

*    Report the error context and abort.

      CALL GKS_GSTAT( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CRELUT_PALDSP',
     :     'CRELUT: Error displaying the palette.', STATUS )
         GOTO 980
      END IF

*    Reset the input picture as current in case of an accident.

      CALL AGI_SELP( PICID1, STATUS )

*    Set up the text size and relative position for writing the labels
*    on the available colours.

      CALL SGS_STXJ( 'BL' )
      CALL SGS_SHTX( YRANGE / 64. )

*    Write the labels.

      DO  I = 1, CDIMS( 2 ), 1

*       Set the y co-ordinate of the labels.

         Y = YRANGE / 128. + REAL( I-1 ) * YRANGE / 16.0

         DO  J = 1, CDIMS( 1 ), 1

*          Get red, green and blue intensities for the current block.

            CALL GQCR( WKID, ( J-1 + ( I-1 ) * CDIMS( 1 ) ), 1, GSTAT,
     :                 GKSCOL( 1 ), GKSCOL( 2 ), GKSCOL( 3 ) )

*          The pen has to be coloured (yellow) when the colour is dark.
*          The others are written onto coloured areas and are thus done
*          in black.

            IF ( GKSCOL(1) + GKSCOL(2) + GKSCOL(3) .LT. PENTHR ) THEN
               CALL SGS_SPEN( MKPEN )
            ELSE
               CALL SGS_SPEN( 5 )
            END IF

*          Write out the next label.

            CALL SGS_BTEXT( PIXX / 128. + REAL( J-1 ) * PIXX / 16.0, Y )
            CALL SGS_ATXI( ( I-1 ) * CDIMS( 1 ) + ( J-1 ), 0 )

         END DO
      END DO

*    THIRD plot ends

      CALL SGS_FLUSH

*    Obtain used-defined colours in the palette.
*    ===========================================

*    Start a new error context.

      CALL ERR_MARK

*    Give commentary for the user.

      CALL MSG_OUT( 'BLANK', ' ', STATUS )
      CALL MSG_SETI( 'PLOW', CDIMS( 1 ) )
      CALL MSG_SETI( 'PHIGH', NBLOCK - 1 )
      CALL MSG_OUT( 'CRELUT_INFO1', 'Now you may add to the predefined'/
     :  /' palette. Numbers ^PLOW to ^PHIGH, are available. ', STATUS )
      CALL MSG_SETI( 'PHIGH', NBLOCK - 1 )
      CALL MSG_OUT( 'CRELUT_INFO1B', 'Type ! to complete the '/
     :  /'modifications.', STATUS )

*    Extend the palette to include user-defined colours.
*    ===================================================

      PALNUM = 15
      DO WHILE ( STATUS .EQ. SAI__OK )

*       Obtain the palette number, setting the default for the next one
*       in the available palette entries, cycling at the end to the
*       first.

         DEFPNM = MOD( PALNUM + 1 - CDIMS( 1 ), CDIMS( 1 ) ) +
     :            CDIMS( 1 )
         CALL PAR_GDR0I( 'PALNUM', DEFPNM, CDIMS( 1 ), NBLOCK - 1,
     :                   .FALSE., PALNUM, STATUS )
         CALL PAR_CANCL( 'PALNUM', STATUS )

*       Inquire the current seetings in case the user just wishes to
*       tweak the colour slightly.

         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL GQCR( WKID, PALNUM, 1, GSTAT, GKSCOL( 1 ), GKSCOL( 2 ),
     :                 GKSCOL( 3 ) )
            CALL MSG_SETR( 'RED', GKSCOL( 1 ) )
            CALL MSG_SETR( 'GREEN', GKSCOL( 2 ) )
            CALL MSG_SETR( 'BLUE', GKSCOL( 3 ) )
            CALL MSG_OUT( 'CURCOL',
     :        'The current RGB is ^RED,^GREEN,^BLUE.', STATUS )
         END IF

*       Get one colour for the palette number.

         CALL KPG1_GPCOL( 'COLOUR', GKSCOL, STATUS )
         CALL PAR_CANCL( 'COLOUR', STATUS )

*       If the colour is alright then try it out, and see if it is
*       to be kept, otherwise leave the loop.

         IF ( STATUS .EQ. SAI__OK ) THEN

            CALL GSCR( WKID, PALNUM, GKSCOL(1), GKSCOL(2), GKSCOL(3) )

*          Determine the colour of the label.

            IF ( GKSCOL(1) + GKSCOL(2) + GKSCOL(3) .LT. PENTHR ) THEN
               CALL SGS_SPEN( MKPEN )
            ELSE
               CALL SGS_SPEN( 5 )
            END IF

*          Find the palette number with in the line.

            K = MOD( PALNUM, CDIMS( 1 ) )

*          Set the y co-ordinate of the labels.

            Y = YRANGE / 128. + REAL( PALNUM / CDIMS( 1 ) ) *
     :          YRANGE / 16.0

*          Write out the next label.

            CALL SGS_BTEXT( PIXX / 128. + REAL( K ) * PIXX / 16.0, Y )
            CALL SGS_ATXI( PALNUM, 0 )

            CALL SGS_FLUSH

*          See if the new colour is acceptable.

            CALL PAR_GTD0L( 'OK', .FALSE., .TRUE., ANS, STATUS )

*          If the user wants to abort, then leave the program.

            IF ( STATUS .EQ. PAR__ABORT ) THEN
               CALL ERR_RLSE
               GOTO 980
            END IF

*          Annul parameter OK.

            CALL PAR_CANCL( 'OK', STATUS )

*          If the colour is liked then save it.

            IF ( ANS ) THEN

               DO  I = 1, NPRICL, 1
                  PALETT( I, PALNUM ) = GKSCOL( I )
               END DO

*          Otherwise restore the previous colours for this element
*          of the palette.

            ELSE

               DO  I = 1, NPRICL, 1
                  GKSCOL( I ) = PALETT( I, PALNUM )
               END DO

               CALL GSCR( WKID, PALNUM, GKSCOL(1), GKSCOL(2),
     :                    GKSCOL(3) )

*             Determine the colour of the label.

               IF ( GKSCOL(1) + GKSCOL(2) + GKSCOL(3) .LT. PENTHR ) THEN
                  CALL SGS_SPEN( MKPEN )
               ELSE
                  CALL SGS_SPEN( 5 )
               END IF

*             Write out the next label.

               CALL SGS_BTEXT( PIXX / 128. + REAL( K ) * PIXX / 16.0,
     :                         Y )
               CALL SGS_ATXI( PALNUM, 0 )

               CALL SGS_FLUSH

*             Decrement the palette number so the default will be the
*             same number as this time.

               PALNUM = PALNUM - 1

*          End of colour-to-be-saved check.

            END IF

*       End of no-error-getting-RGB-intensities check.

         END IF

      END DO

*    If the user wants to abort, then leave the application.

      IF ( STATUS .NE. SAI__OK .AND. STATUS .NE. PAR__NULL ) THEN
         CALL ERR_RLSE
         GOTO 980
      END IF

*    A null status is expected to end the loop that adds colours to the
*    palette, so annul the status.

      IF ( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*    Release the error context.

      CALL ERR_RLSE

*    Interactively adjust the colour table.
*    ======================================

*    Start a new error context.

      CALL ERR_MARK

*    Give commentary for the user.

      CALL MSG_OUT( 'BLANK', ' ', STATUS )
      CALL MSG_OUT( 'CRELUT_INFO2', 'Now you may interactively change '/
     :  /'the lookup table (LUT). You give ranges of ', STATUS )
      IF ( IMAGE ) THEN
         CALL MSG_OUT( 'CRELUT_INFO3', 'data values (that map to '/
     :     /'LUT pens) to be assigned colours obtained ', STATUS )
         CALL MSG_OUT( 'CRELUT_INFO4', 'by interpolation of pairs of '/
     :     /'numbered colours selected from the palette. ', STATUS )
      ELSE
         CALL MSG_OUT( 'CRELUT_INFO3', 'LUT pens to store colours '/
     :     /'obtained by interpolation of pairs of colours ', STATUS )
         CALL MSG_OUT( 'CRELUT_INFO4', 'selected from the palette. '/
     :     /'Type ! to complete the creation of the LUT.', STATUS )
      END IF
      CALL MSG_OUT( 'CRELUT_INFO5', 'Type ! to complete the creation '/
     :  /'of the LUT.', STATUS )

*    Interactively produce a coloured image.

      DO WHILE ( STATUS .EQ. SAI__OK )

         CALL MSG_OUT( 'BLANK', ' ', STATUS )

*       Get a range of pens to which some colours are to be fitted.
*       ===========================================================

         IF ( IMAGE ) THEN

*          Obtain data values first.  A single value may be given to
*          change a single pen. Cancel the parameter to enable further
*          values to be obtained.

            CALL PAR_GDRVD( 'VALRANGE', 2, DBLE( PVLO ),
     :                      DBLE( PVHI ), VALRNG, NVAL, STATUS )
            CALL PAR_CANCL( 'VALRANGE', STATUS )

*          Convert the data-value range to a pen range.

            PENRNG( 1 ) = NINT( ( VALRNG( 1 ) - HOFSET ) / HSCALE )
            IF ( NVAL .EQ. 1 ) THEN
               PENRNG( 2 ) = PENRNG( 1 )
            ELSE
               PENRNG( 2 ) = NINT( ( VALRNG( 2 ) - HOFSET ) / HSCALE )
            END IF

         ELSE

*          Obtain the range of pens.  A single value may be given to
*          change that pen alone.

            CALL PAR_GDRVI( 'PENRANGE', 2, 0, ANINTS-1, PENRNG, NVAL,
     :                      STATUS )
            IF ( NVAL .EQ. 1 ) PENRNG( 2 ) = PENRNG( 1 )
            CALL PAR_CANCL( 'PENRANGE', STATUS )
         END IF

*       Get a range of palette colours.  However, a single value may
*       be entered for a solid block of colour.

         CALL PAR_GDRVI( 'COLRANGE', 2, 0, NBLOCK-1, COLRNG, NVAL,
     :                   STATUS )

*       If only one colour was entered then create a range starting
*       and ending with the same colour (i.e. a block of uniform
*       colour)

         IF ( NVAL .EQ. 1 ) COLRNG( 2 ) = COLRNG( 1 )

*       Cancel the parameter to enable further values to be obtained.

         CALL PAR_CANCL( 'COLRANGE', STATUS )

*       Provided there was no error in the input, the lookup table is
*       updated.

         IF ( STATUS .EQ. SAI__OK ) THEN

*          If the first pen in the range appears after the second then
*          swap the pens around so that the fit goes from a pen with a
*          lower number to one with a higher number.  Also swap the
*          colour range to maintain correspondence.

            IF ( PENRNG( 2 ) .LT. PENRNG( 1 ) ) THEN
               I = PENRNG( 1 )
               PENRNG( 1 ) = PENRNG( 2 )
               PENRNG( 2 ) = I
               I = COLRNG( 1 )
               COLRNG( 1 ) = COLRNG( 2 )
               COLRNG( 2 ) = I
            END IF

*          If the same pen appears at both ends of the range
*          then only change the one entry in the lookup table.

            IF ( PENRNG( 1 ) .EQ. PENRNG( 2 ) ) THEN

               DO  I = 1, NPRICL, 1
                  LUT( I, PENRNG( 1 ) ) = PALETT( I, COLRNG( 1 ) )
                  GKSCOL( I ) = LUT( I, PENRNG( 1 ) )
               END DO

*             Change the colour representation for the first pen.

               CALL GSCR( WKID, PENRNG( 1 ) + NBLOCK, GKSCOL( 1 ),
     :                    GKSCOL( 2 ), GKSCOL( 3 ) )

               CALL SGS_FLUSH

*          Create a block of constant colour between the range of pens.

            ELSE IF ( COLRNG( 1 ) .EQ. COLRNG( 2 ) ) THEN

               DO  J = PENRNG( 1 ), PENRNG( 2 ), 1
                  DO  I = 1, NPRICL, 1
                     LUT( I, J ) = PALETT( I, COLRNG( 1 ) )
                     GKSCOL( I ) = LUT( I, J )
                  END DO

*               Change the colour representation for the pen.

                  CALL GSCR( WKID, J + NBLOCK, GKSCOL( 1 ),
     :                       GKSCOL( 2 ), GKSCOL( 3 ) )
               END DO
               CALL SGS_FLUSH

            ELSE

*             For an ordinary range of pens, the pens are set to the
*             colour which is calculated by linear interpolation
*             between the two limits.

               DO  I = 1, NPRICL, 1

*                Set up colour of first pen.

                  BASCOL( I ) = PALETT( I, COLRNG( 1 ) )

*                Set up the rate of change of colour with pen number.

                  FAC( I ) = ( PALETT( I, COLRNG( 2 ) )
     :                       - PALETT( I, COLRNG( 1 ) ) )
     :                       / REAL( PENRNG( 2 ) - PENRNG( 1 ) )
               END DO

*             Load up the lookup table with the calculated colours.

               DO  J = PENRNG( 1 ), PENRNG( 2 ), 1

                  DO  I = 1, NPRICL, 1

*                Linearly interpolate to derive the colours.

                     LUT( I, J ) = MIN( MAX( BASCOL( I ) + FAC( I ) *
     :                             REAL( J - PENRNG( 1 ) ), 0.0 ), 1.0 )
                     GKSCOL( I ) = LUT( I, J )
                  END DO

*                Change the colour representation for the pen.

                  CALL GSCR( WKID, J + NBLOCK, GKSCOL( 1 ),
     :                       GKSCOL( 2 ), GKSCOL( 3 ) )

               END DO
               CALL SGS_FLUSH

*          End of pens-are-the-same-at-both-ends-of-the-range check.

            END IF

*       End of no-error-getting-pen-and-colour-ranges check.

         END IF

      END DO

*    If the user wants to abort, then leave the application.

      IF ( STATUS .NE. SAI__OK .AND. STATUS .NE. PAR__NULL ) THEN
         CALL ERR_RLSE
         GOTO 980
      END IF

*    A null status is expected to end the loop that adds colours to the
*    palette, so annul the status.

      IF ( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*    Release the error context.

      CALL ERR_RLSE

*    Save the colour table in an NDF.
*    ================================

*    Give some commentary.

      CALL MSG_OUT( 'NULLOP', 'Type the null character, !, if the '/
     :  /'created LUT is not to be saved.', STATUS )

*    Specify the dimensions of the new NDF.

      LDIMS( 1 ) = NPRICL
      LDIMS( 2 ) = ANINTS

*    Create a new primitive NDF containing the lookup table and a title.
*    A null response will be handled transparently.

      CALL KPG1_CPNTR( 'OUTLUT', 'TITLE', NDIM, LDIMS, LUT, .TRUE.,
     :                 STATUS )

*    Save the colour table in an NDF.
*    ================================

*    Specify the dimensions of the new NDF.

      PDIMS( 1 ) = NPRICL
      PDIMS( 2 ) = NBLOCK

*    Create a new primitive NDF containing the colour table and a title.
*    A null response will be handled transparently.

      CALL KPG1_CPNTR( 'OUTPAL', 'PTITLE', NDIM, PDIMS, PALETT, .TRUE.,
     :                 STATUS )

 980  CONTINUE

*    Restore the input, reserved colour indices.
*    ===========================================

*    The reserved colour indices have been overwritten by the palette,
*    therefore restore them to their former values.  Check for an
*    error.

      DO  J = 0, CTM__RSVPN - 1
         CALL GSCR( WKID, J, RESPEN( 1, J ), RESPEN( 2, J ),
     :              RESPEN( 3, J ) )
      END DO
      CALL GKS_GSTAT( STATUS )

*    AGI closedown sequence.
*    =======================

 999  CONTINUE
      CALL AGS_DEASS( 'DEVICE', DEVCAN, STATUS )

      END
