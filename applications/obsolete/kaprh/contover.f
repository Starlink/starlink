      SUBROUTINE CONTOVER( STATUS )
*+
*  Name:
*     CONTOVER

*  Purpose:
*     Contours a 2-d NDF overlaid on an image displayed previously.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CONTOVER( STATUS )

*  Description:
*     This application draws a contour plot of a 2-dimensional NDF
*     using an efficient algorithm.  The array may be part or whole of
*     the data array, but also the variance or quality can be shown.

*     The contour plot is drawn over an existing image that is
*     displayed on the chosen graphics workstation or its overlay,
*     provided the displayed image has been recorded in the graphics
*     database.  (This will be the case for other display routines in
*     KAPPA.)  The contour plotting occurs within the current picture
*     only if it is a DATA picture, otherwise contours are overlaid in
*     the last DATA picture within the current picture.  This
*     application assumes that the world co-ordinate systems of the data
*     array and the displayed image are both in pixel units, but not to
*     the same origins.  Pixel x-y offsets may be given to match the
*     contour plot with the image, provided some contouring will be
*     visible.  These displacements are in the sense image co-ordinate
*     minus the data-array co-ordinate for an arbitrary fiducial point.

*     The contouring algorithm has only pixel resolution, and
*     so the contours are not smooth, but this makes the processing
*     much faster.  There are seven methods for selecting contours.

*     The best way to use this application is to first display an image
*     on the base plane of an image display, make this the current
*     picture, and then plot contours on the overlay plane, clearing
*     the overlay picture each time. This enables more than one attempt
*     at getting the correct contour heights.  The underlying image will
*     not be erased. (Note that if you do not make the underlying image
*     the current picture, the contour plot becomes the last DATA
*     picture, and so any subsequent x-y offsets should be set to 0,0 to
*     prevent successive contour plots being incorrectly located.)

*  Usage:
*     contover ndf [comp] offset mode ncont [device]
*        { firstcnt=? stepcnt=?
*        { heights=?
*        { percentiles=?
*        mode

*  ADAM Parameters:
*     CLEAR = _LOGICAL (Read)
*        True if the graphics device is to be cleared before display
*        of the array.  It should only be true for an overlay device.
*        [TRUE]
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
*     DASHED = _REAL (Read)
*        The height below which the contours will be drawn with dashed
*        lines.  A null value (!) means all contours are drawn with
*        solid lines.  This facility is only available when ANNOTA =
*        FALSE. [!]
*     DEVICE = DEVICE (Read)
*        The plotting device.  The device must be in one of the
*        following GNS categories: IMAGE_DISPLAY, IMAGE_OVERLAY,
*        WINDOW, WINDOW_OVERLAY, or MATRIX_PRINTER.
*        [Current image-display-overlay device]
*     FIRSTCNT = _REAL (Read)
*        Height of the first contour (Linear and Magnitude modes).
*     HEIGHTS() = _REAL (Read)
*        Contour levels (Free mode).  The suggested default is the
*        current value.
*     MODE = LITERAL (Read)
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
*     OFFSET( 2 ) = _INTEGER (Read)
*        X-y offsets of the input data-array with respect to the
*        displayed image (i.e. x_data - x_image followed by
*        y_data - y_image for any fiducial point).   These are
*        constrained so that some part of the contour plot will be
*        overlaid on the displayed image.  The suggested default is
*        [0,0], i.e. no shift.
*     PENROT = _LOGICAL (Read)
*        If TRUE, the plotting pens are cycled through the contours to
*        aid identification of the contour heights.  It is ignored
*        when annotation is selected. [FALSE]
*     PERCENTILES() = _REAL (Read)
*        Contour levels given as percentiles.  The values must lie
*        between 0.0 and 100.0. (Percentiles mode).  The suggested
*        default is the current value.
*     STEPCNT = _REAL (Read)
*        Separation between contour levels, linear for Linear mode
*        and in magnitudes for Magnitude mode.
*     THICK = _REAL (Read)
*        The thickness of the contours in the plot, where 1.0 is the
*        normal thickness.  Currently, this is only available on a few
*        devices.  It must take a value in the range 0.5--10.0.  [1.0]

*  Examples:
*     contover myfile d [-20,7] \
*        Contours the data array in the NDF called myfile on the current
*        image-display overlay device; the overlay is displaced such
*        that pixel (i,j) in myfile corresponds to pixel (i-20,j+7) in
*        the displayed image.  All other settings are defaulted, so for
*        example the current method for determining heights is used,
*        and as much of myfile will be contoured that fits into the
*        current picture.
*     contover ndf=ngc6872 mode=au ncont=5 offset=[0,0]
*        Contours the data array in the NDF called ngc6872 on the
*        current image-display overlay device.  Five equally spaced
*        contours between the maximum and minimum data values are
*        drawn.  There is no offset between the contour plot and the
*        displayed image; this can be useful for comparing an NDF
*        before and after some processing, e.g. smoothing.
*     contover iras60(200:300,100:350) comp=d offset=[3,5] \
*        Contours the portion of the data array in the NDF called iras60
*        on the current image-display overlay using the current method
*        for height selection.  The maximum portion of the data array
*        that can be contoured goes from pixel (200,100) to (300,350).
*        The overlay is displaced such that pixel (i,j) in the data
*        array corresponds to pixel (i+3,j+5) in the displayed image.
*     contover comp=v mode=fr heights=[10,20,40,80] device=xov \
*        Contours the variance array in the current NDF on the xov
*        device.  Contours at 10, 20, 40 and 80 are drawn.  There is no
*        displacement between the variance contour plot and the
*        displayed image.
*     contover mode=eq ncont=5 dashed=15 pencol=blue ndf=skyflux
*        Contours the data array in the NDF called skyflux on the
*        current image-overlay device.  Contours at heights
*        corresponding to the 10, 30, 50, 70 and 90 percentiles are
*        drawn in blue (if available).  Those contours whose values
*        less than 15 will appear as dashed lines.  There is no
*        displacement between the contour plot and the displayed image.
*     contover xx1 mode=pe percentiles=[90,95,98,99] pencol=white
*     noclear device=epsf_l
*        Contours the data array in the NDF called xx1 on the epsf_l
*        device.  White contours at heights corresponding to the 90, 95,
*        98, and 99 percentiles are drawn.  The display is not cleared.
*        There is no displacement.  The output file could be combined
*        with a DISPLAY plot (using PSMERGE) to make a hardcopy of a
*        contour plot on a dark image.

*  Notes:
*     -  The application records the contour plot as a DATA picture
*     with world co-ordinates in units of data pixels in the graphics
*     database.  The DATA picture may also may have double-precision
*     data co-ordinates derived from the NDF axis components provided
*     these are linear and different from pixel co-ordinates; the data
*     co-ordinates are stored via a linear transformation.  The NDF
*     associated with the plot is stored by reference with the DATA
*     picture.  On exit the current database picture for the chosen
*     device reverts to the input picture.
*     -  There are some options for setting the characteristics of the
*     contour lines.  By default, solid lines are drawn with the same
*     colour as the axes and key, namely the foreground colour.  The
*     colour will depend on the graphics device chosen, but it is often
*     black for printers or white for terminals.  The alternatives to
*     override this default behaviour are listed below.
*
*        1. Set a colour for all contours using parameter CONCOL.
*           The choices may be quite restrictive on certain devices,
*           for example a window overlay only has one colour.  Use
*           the PALENTRY command to change this colour.
*        2. Request dashed contours below some threshold given by
*           parameter DASHED and solid lines for other heights.  All
*           contours have either the foreground colour or that
*           prescribed by parameter CONCOL.
*        3. Cycle the pens modulo 3 for each contour height actually
*           plotted by setting PENROT = TRUE.  The characteristics of
*           the second and third line styles will depend on the chosen
*           graphics device.  An image display or pen plotter will draw
*           coloured lines using palette entries 1 to 3; whereas a
*           window overlay, monochrome laser printer or terminal will
*           draw a variety of dashed or thicker lines.
*        4. Combine options 2 and 3.  However, palette colours 1 to 3
*           will always be used and CONCOL ignored.  The contours below
*           the threshold continue the cycle through the three colours.
*           There may be some confusion on devices that already use
*           dashed lines, so this is only suitable for devices
*           supporting at least three colours simultaneously.
*
*     Pen rotation takes precedence over colour control through CONCOL.

*  Algorithm:
*     -  Find which component to display, obtain an identifier to the
*     NDF and check that the component is present. Find the data type
*     for processing.  Get the NDF bounds and inquire the bad-pixel
*     flag.
*     -  Get the display device and open the database for it with the
*     appropriate device status. Get the current SGS zone.  Check that
*     the device is an image-display.  Obtain the the last DATA picture
*     and report its name, comment and label.
*     -  Obtain the zone's pixel sizes, and obtain the offsets using
*     the pixel sizes as constraints.  Find the region of the displayed
*     image that will have contours overlaid, and make it a new zone.
*     Create and map the section to be contoured.  Redefine the
*     zone's co-ordinates to those of the NDF sections' size.
*     -  Select the contour heights and sort them in ascending order.
*     -  Get the contour-style parameter.
*     -  Obtain the data co-ordinate transformation and axis bounds.
*     -  Obtain lots of work arrays, redefine the co-ordinates to that
*     of the array and plot the contours.  Clear workspace.  Store the
*     contour picture in the database, plus the data reference, and the
*     transformation for data co-ordinates.
*     -  Report the heights drawn.
*     -  Tidy graphics and NDF.

*  Related Applications:
*     KAPPA: CONTOUR, TURBOCONT; Figaro: ICONT; SPECDRE: SPECCONT.

*  Implementation Status:
*     -  Only real data can be processed directly.  Other data types
*     will undergo a type conversion before the contour plot is drawn.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.

*  Implementation Deficiencies:
*     Lots of missing options to tailor the plot.  The co-ordinate
*     system of the underlying image and the overlay contour plot is
*     limited to pixels and you have to compute it yourself.  The
*     overlay will appear over any obscured portion of the image.

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1989 Jun 21 (MJC):
*        Original version.
*     1989 Aug  8 (MJC):
*        Passed array dimensions as separate variables
*        to CNTDRW and CNTSEL.
*     1989 Sep  1 (MJC):
*        Converted to use turbo-contouring.
*     1989 Dec 21 (MJC):
*        Workspace managed by AIF_TEMP.
*     1990 Jan 9 (MJC):
*        Corrected SGS status.
*     1990 Mar 30:
*        Reports the chosen contour heights to the user.
*     1991 April 13 (MJC):
*        First NDF version supporting variance and quality.  Added data
*        co-ordinate transformation, NDF reference into the database,
*        and AGI context control.  Re-organised world co-ordinates so
*        that CNTTUR no longer handles the full array, merely the
*        section.
*     1991 May 1 (MJC):
*        Renamed IN parameter to NDF for consistency.
*     1991 July 31 (MJC):
*        No longer redefines colours of SGS pens to predefined state if
*        workstation has dynamic colour representation, now there is
*        palette control.
*     1991 October 17 (MJC):
*        Fixed typo' in calculation of allowed shifts.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1992 April 4 (MJC):
*        Made to handle significant dimensions for user-defined
*        sections.  Bounds parameters removed.
*     1992 June 16 (MJC):
*        Made to work with WINDOW_OVERLAY class.  The restriction on the
*        number of colour indices has therefore been been relaxed.
*     1992 November 30 (MJC):
*        Does not use non-monotonic axis centres.
*     1995 October 19 (MJC):
*        Supports Error component.
*     1997 May 21 (MJC):
*        Added percentiles and equalised options for the MODE.  New
*        parameter PERCENTILES.  Increased tessellation cell to 512
*        pixels square.  Added CONCOL, DASHED, and THICK parameters,
*        and further examples.  Improved efficiency by using PSX to
*        obtain workspace.  Rewrote the Notes on contour colour and line
*        style.
*     6-MAY-1998 (DSB):
*        Update the GKS workstation after changing polyline
*        representations. This prevents the screen being cleared when the
*        workstation is closed.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! No default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Global SSE definitions
      INCLUDE 'DAT_PAR'        ! Data-system constants
      INCLUDE 'PAR_ERR'        ! Parameter-system error definitions
      INCLUDE 'PRM_PAR'        ! PRIMDAT public constants
      INCLUDE 'NDF_PAR'        ! NDF_ public constants
      INCLUDE 'NDF_ERR'        ! NDF_ error definitions
      INCLUDE 'GKS_PAR'        ! GKS constants (e.g. GSET)

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER
     :  CELDIM,                ! Tessellation cell dimension
     :  NDIM,                  ! Dimensionality of input array
     :  MXCONT                 ! Maximum number of contour heights
      PARAMETER( CELDIM = 512 )
      PARAMETER( NDIM = 2 )    ! Default to 2-d
      PARAMETER( MXCONT = 50 )

      INTEGER CONPEN             ! SGS pen number used to plot unrotated
      PARAMETER ( CONPEN = 2 )   ! contour

*  Local Variables:
      LOGICAL                  ! True if :
     :  BAD,                   ! Bad pixels may be present in the image
     :  CLEAR,                 ! The graphics device is to be cleared
                               ! before display of the array
     :  CNTUSD( MXCONT ),      ! A contour has been plotted at the
                               ! corresponding height in CNTLEV
     :  COLOUR,                ! Workstation supports colour
     :  DACOOR,                ! Data co-ordinates are to be stored
                               ! in the database
     :  DEVCAN,                ! Graphics-device parameter is to be
                               ! cancelled
     :  DPAXIS,                ! Axis centres are double precision
     :  MONOTO( NDIM ),        ! Axis is monotonic
     :  PENROT,                ! The graphics pens are to be cycled
     :  REFOBJ,                ! There is a reference object
     :  SCLINE,                ! Non-standard thickness lines needed
     :  THERE,                 ! NDF array component is present
     :  VALID                  ! Reference object is a locator

      REAL
     :  AREA( MXCONT ),        ! Work array for storing areas in CNTSEL
     :  AXLBND( NDIM ),        ! Axis lower bounds
     :  AXUBND( NDIM ),        ! Axis upper bounds
     :  CLWIDT,                ! Width of current lines SGS pen
     :  CNTLEV( MXCONT ),      ! Contour heights
     :  OFFSET( NDIM ),        ! Offsets in the world-to-data
                               ! co-ordinate transformations
     :  PERCNT( MXCONT ),      ! Contour heights as percentiles
                               ! (actually fractions)
     :  SCALE( NDIM ),         ! Scale factors in the world-to-data
                               ! co-ordinate transformations
     :  THICK,                 ! The line thickness (standard is 1.0)
     :  THRESH,                ! Threshold for dashed contours
     :  X1, X2, Y1, Y2,        ! Zone size in world co-ordinates
     :  XM, YM                 ! Zone size in metres

      DOUBLE PRECISION
     :  DXLBND( NDIM ),        ! Axis lower bounds
     :  DXUBND( NDIM ),        ! Axis upper bounds
     :  DOFSET( NDIM ),        ! Offsets in the world-to-data
                               ! co-ordinate transformations
     :  DSCALE( NDIM )         ! Scale factors in the world-to-data
                               ! co-ordinate transformations

      CHARACTER*( DAT__SZLOC ) ! Locator for :
     :  WKLOC( 5 )             ! Work arrays for computing the contours

      CHARACTER
     :  ATYPE * ( NDF__SZTYP ),! Processing type of the axis centres
     :  COMP * 8,              ! Component to be displayed
     :  DTYPE * ( NDF__SZFTP ),! Type of the image after processing (not
                               ! used)
     :  ITYPE * ( NDF__SZTYP ),! Processing type of the image
     :  MCOMP * 8,             ! Component to be mapped
     :  MODE * 20,             ! Height choosing mode
     :  REFNAM * ( 132 )       ! Reference data associated with the last
                               ! DATA picture

      INTEGER
     :  AEL,                   ! Number of elements in a mapped axis
     :  AXPNTR( 1 ),           ! Pointer to a mapped axis
     :  CCOLI,                 ! Original colour index of pen used for
                               ! plotting the lines
     :  CFPNTR,                ! Pointer to the cell-flag work array
     :  CLNTYP,                ! Line type for current lines SGS pen
     :  CONCI,                 ! Colour index required for contours
     :  DEFOFF( NDIM ),        ! Default offsets of the data-array w.r.t
                               ! the displayed image
     :  DIMS( NDIM ),          ! Dimensions of input array
     :  EL,                    ! Number of elements in the input array
     :  GSTAT,                 ! GKS status
     :  I,                     ! General variable
     :  IERR,                  ! GKS error indicator
     :  IWKID,                 ! GKS workstation identifier
     :  LASF( 13 ),            ! GKS list of aspect source flags
     :  LBND( NDF__MXDIM ),    ! Lower bounds of the array
     :  LIML( NDIM ),          ! Minimum permitted offsets
     :  LIMU( NDIM )           ! Maximum permitted offsets

      INTEGER
     :  LLPNTR,                ! Pointer to the linked-list work array
     :  MAXPEN,                ! Maximum pen number for undashed pens
     :  NCONT,                 ! Number of contour heights
     :  NDF,                   ! NDF identifier
     :  NDFC,                  ! Identifier for input section
     :  NDFS,                  ! NDF identifier of the section
     :  NDIMS,                 ! Total number of NDF dimensions
     :  NLOCUS,                ! Size of the x-y locus work arrays
     :  NWORK                  ! Number of work arrays obtained
                               ! successfully

      INTEGER
     :  OFFS( NDIM ),          ! Offsets of the data-array w.r.t
                               ! the displayed image
     :  PICID1,                ! Graphics' database identifier on input
     :  PICID2,                ! Graphics' database identifier in which
                               ! the contour plot will be drawn
     :  PICID3,                ! Graphics' database identifier for
                               ! the displayed contour picture
     :  PNTRI( 1 ),            ! Pointer to array data
     :  SCRPNT,                ! Pointer to the work array
     :  SDIM( NDF__MXDIM ),    ! Significant dimensions of the NDF
     :  SLBND( NDIM ),         ! Significant lower bounds of the image
     :  SLIST( MXCONT ),       ! Work array
     :  SUBND( NDIM ),         ! Significant upper bounds of the image
     :  UBND( NDF__MXDIM ),    ! Upper bounds of the array
     :  WPNTR( 5 )             ! Pointers to workspace

      INTEGER
     :  XHI,                   ! x co-ordinate of end of sub-array
                               ! in current picture's co-ordinates
     :  XLI,                   ! x co-ordinate of start of sub-array
                               ! in current picture's co-ordinates
     :  XPNTR,                 ! Pointer to the x locus work array
     :  XRANGI,                ! x size of the displayed image
     :  YHI,                   ! y co-ordinate of end of sub-array
                               ! in current picture's co-ordinates
     :  YLI,                   ! y co-ordinate of start of sub-array
                               ! in current picture's co-ordinates
     :  YPNTR,                 ! Pointer to the y locus work array
     :  YRANGI,                ! y size of the displayed image
     :  ZONE1,                 ! Initial SGS zone identifier
     :  ZONED,                 ! SGS zone identifier for the contouring
                               ! region (i.e. sub-array visible in the
                               ! current picture)
     :  ZONEI                  ! SGS zone identifier for the image area

*.

*    Check the global inherited status.

      IF ( STATUS .NE. SAI__OK ) RETURN



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
            CALL ERR_REP( 'CONTOVER_NOCOMP',
     :        'CONTOVER: ^COMP component is not defined.', STATUS )
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

*    Compute the dimensions and the significant bounds.

      SLBND( 1 ) = LBND( SDIM( 1 ) )
      SLBND( 2 ) = LBND( SDIM( 2 ) )
      SUBND( 1 ) = UBND( SDIM( 1 ) )
      SUBND( 2 ) = UBND( SDIM( 2 ) )
      DIMS( 1 ) = SUBND( 1 ) - SLBND( 1 ) + 1
      DIMS( 2 ) = SUBND( 2 ) - SLBND( 2 ) + 1

      IF ( STATUS .NE. SAI__OK ) GOTO 980


*    Start the graphics system.
*    ==========================

*    See whether picture is to be refreshed

      CALL PAR_GTD0L( 'CLEAR', .TRUE., .TRUE., CLEAR, STATUS )
      IF ( STATUS .EQ. PAR__ABORT ) GOTO 980

*    Open graphics database for required device.

      IF ( CLEAR ) THEN
         CALL AGS_ASSOC( 'DEVICE', 'WRITE', ' ', PICID1, ZONE1, STATUS )
      ELSE
         CALL AGS_ASSOC( 'DEVICE', 'UPDATE', ' ', PICID1, ZONE1,
     :                   STATUS )
      END IF

*    If the graphics device was not available and leave the application.

      IF ( STATUS .NE. SAI__OK ) THEN

*       The device name is to be cancelled.

         DEVCAN = .TRUE.
         GOTO 960
      END IF

*    Check whether chosen device is or could be an 'image display'
*    with a suitable minimum number of colour indices.

      CALL KPG1_QVID( 'DEVICE', 'SGS', 'IMAGE_DISPLAY,IMAGE_OVERLAY,'/
     :                /'WINDOW,WINDOW_OVERLAY,MATRIX_PRINTER', ' ', 1,
     :                STATUS )

*    Abort if the workstation is not suitable.

      IF ( STATUS .NE. SAI__OK ) THEN
         DEVCAN = .TRUE.
         GO TO 960
      END IF

*    There are a number of possibilities about where, or whether or not
*    plotting will occur:
*
*    If the input picture is a DATA picture, that picture is used for
*    overlaying the contour plot. If not, the database is searched for
*    the most-recent DATA picture, and uses that for the contour plot.
*    There must be a DATA picture otherwise the application will exit.

      CALL KPG1_AGFND( 'DATA', PICID2, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         DEVCAN = .TRUE.
         GOTO 960
      END IF

*    Get a zone identifier for the data picture on the image-display
*    device.

      CALL AGS_NZONE( ZONEI, STATUS )

*    Report the name, comment, and label, if one exists, for the
*    current picture.

      CALL KPG1_AGATC( STATUS )

*    Report the object associated with the DATA picture.
*    ===================================================
*
*    Determine whether or not there is a reference object
*    associated with the current picture.

      CALL KPG1_AGREF( PICID2, 'READ', REFOBJ, REFNAM, STATUS )

*    If one exists translate its locator reference to a token containing
*    the path name and file name, and tidy the reference locator; or
*    just use the reference name.

      IF ( REFOBJ ) THEN
         CALL DAT_VALID( REFNAM( :DAT__SZLOC ), VALID, STATUS )
         IF ( VALID ) THEN
            CALL KPG1_HMSG( 'NAME', REFNAM( :DAT__SZLOC ) )
            CALL REF_ANNUL( REFNAM( :DAT__SZLOC ), STATUS )
         ELSE
            CALL MSG_SETC( 'NAME', REFNAM )
         END IF
         CALL MSG_OUT( 'NAME', '   Reference data object: ^NAME',
     :                 STATUS )
      END IF

*    Set up the workstation polyline representations.
*    ================================================

*    If the device supports colour we want solid lines to be drawn.

      CALL KPG1_QCOL( COLOUR, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         DEVCAN = .TRUE.
         GOTO 960
      END IF

*    Define the zone size and bounds.
*    ================================

*    Inquire zone size.

      CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )

*    Assume that the world co-ordinates are in pixels---the normal
*    convention to derive the dimensions of the displayed image.

      XRANGI = NINT( X2 ) - NINT( X1 )
      YRANGI = NINT( Y2 ) - NINT( Y1 )

*    Inquire whether GKS/SGS has reported an error

      CALL GKS_GSTAT( STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 960

*    Get the offsets.
*    ================
*
*    Revise minimum values permitted for the upper bound of the region.
*    The limits ensure that there are at least two pixels in either
*    dimension as this is the minimum needed for contouring.  There also
*    may be an odd pixel due to a rounding error.  The limits allow for
*    the displacements of lower and upper bounds between the input array
*    and the displayed image.

      LIML( 1 ) = 3 - DIMS( 1 ) + NINT( X1 ) - SLBND( 1 )
      LIML( 2 ) = 3 - DIMS( 2 ) + NINT( Y1 ) - SLBND( 2 )
      LIMU( 1 ) = XRANGI + NINT( X1 ) - SLBND( 1 ) - 1
      LIMU( 2 ) = YRANGI + NINT( X2 ) - SLBND( 2 ) - 1

*    Make the default have no shift.

      DEFOFF( 1 ) = 0
      DEFOFF( 2 ) = 0

*    Get offsets of current data sub-array with respect to the displayed
*    image.

      CALL PAR_GRM1I( 'OFFSET', NDIM, DEFOFF, LIML, LIMU, .FALSE.,
     :                OFFS, STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN

*       Now find the region of the displayed-image picture that will
*       have contours overlaid, and redefine a zone.

         XLI = MAX( SLBND( 1 ) - 1 + OFFS( 1 ), NINT( X1 ) )
         XHI = MIN( SUBND( 1 ) + OFFS( 1 ), NINT( X2 ) )
         YLI = MAX( SLBND( 2 ) - 1 + OFFS( 2 ), NINT( Y1 ) )
         YHI = MIN( SUBND( 2 ) + OFFS( 2 ), NINT( Y2 ) )
         CALL SGS_ZONE( REAL( XLI ), REAL( XHI ), REAL( YLI ),
     :                  REAL( YHI ), ZONED, STATUS )

*       Find the new dimensions and bounds of the section.

         DIMS( 1 ) = XHI - XLI
         DIMS( 2 ) = YHI - YLI
         LBND( SDIM( 1 ) ) = XLI - OFFS( 1 ) + 1
         LBND( SDIM( 2 ) ) = YLI - OFFS( 2 ) + 1
         UBND( SDIM( 1 ) ) = XHI - OFFS( 1 )
         UBND( SDIM( 2 ) ) = YHI - OFFS( 2 )
         SLBND( 1 ) = LBND( SDIM( 1 ) )
         SLBND( 2 ) = LBND( SDIM( 2 ) )
         SUBND( 1 ) = UBND( SDIM( 1 ) )
         SUBND( 2 ) = UBND( SDIM( 2 ) )

*       Define the world co-ordinates in the image zone.  This permits
*       CNTTUR to work on the NDF section as if it were the whole array.

         CALL SGS_SW( 0.0, REAL( DIMS( 1 ) ), 0.0, REAL( DIMS( 2 ) ),
     :                STATUS )

      END IF

*    Obtain the required section of the NDF.
*    =======================================

*    Create the section of the NDF array to be contoured from the input
*    (full-sized) NDF section that has any insignificant bounds
*    shifted; otherwise it is possible for the array to be filled with
*    bad values, simply because the chosen section lies outside the
*    bounds of the original section.

      CALL NDF_SECT( NDFC, SDIM( NDIM ), LBND, UBND, NDFS, STATUS )

*    Map the image.

      CALL KPG1_MAP( NDFS, MCOMP, ITYPE, 'READ', PNTRI, EL, STATUS )

*    Obtain the contour heights.
*    ===========================

*    Check whether or not bad pixels are present.

      CALL NDF_BAD( NDFS, COMP, .FALSE., BAD, STATUS )

*    Select the method of defining contour heights and evaluate them.

      CALL KPS1_CNSER( 'MODE', 'NCONT', 'FIRSTCNT', 'STEPCNT',
     :                 'HEIGHTS', 'PERCENTILES', BAD, EL,
     :                 %VAL( PNTRI( 1 ) ), MXCONT, CNTLEV, PERCNT,
     :                 AREA, NCONT, MODE, STATUS )

      IF ( STATUS .NE. SAI__OK ) GOTO 960

*    Sort the contour heights into increasing order.

      IF ( STATUS .EQ. SAI__OK .AND. NCONT .GT. 0 )
     :  CALL KPG1_QSRTR( NCONT, 1, NCONT, CNTLEV, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( STATUS .NE. PAR__ABORT ) THEN
            CALL ERR_REP( 'CONTOVER_GTLEV',
     :        'CONTOVER: Error obtaining or sorting the contour levels',
     :        STATUS )
         END IF
      END IF

*    Define the contour-line characteristics.
*    ========================================

*    Get the line thickness.
      CALL PAR_GDR0R( 'THICK', 1.0, 0.5, 10.0, .TRUE., THICK, STATUS )
      SCLINE = THICK .LT. 0.99999 .OR. THICK .GT. 1.00001

*    Obtain the level below which the contours will be dashed.  A null
*    value means no contours are dashed.
      CALL ERR_MARK
      CALL PAR_GET0R( 'DASHED', THRESH, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         THRESH = VAL__BADR
      END IF
      CALL ERR_RLSE

*    Determine whether or not pens are to be cycled through the
*    contour levels.

      CALL PAR_GTD0L( 'PENROT', .FALSE., .TRUE., PENROT, STATUS )

*  Set the pen colours and line styles.
*  ====================================

*  Note that this must be done before plotting to avoid plot
*  regeneration (i.e. clear the plot when the device is closed).

      IF ( .NOT. PENROT ) THEN

*  Obtain the colour index for the desired colour of the contours.
*  Don't restrict the colours to the palette to give the user more
*  control.  There are instructions in the documentation on the benefits
*  of choosing a palette colour.  For windows overlays, there isn't
*  much choice anyway.
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

*  Ensure that the pen changes have been applied. This may cause GKS to
*  redraw or clear the screen. It must be done now because otherwise, it
*  would be done when the workstation is closed, resulting in the newly
*  drawn graphics being erased.
      CALL GUWK( IWKID, 1 )

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

*    Obtain the axis co-ordinates.
*    =============================

*    Is there an axis system?

      CALL NDF_STATE( NDFS, 'Axis', DACOOR, STATUS )

*    Integer needs d.p. because it potentially has ten significant
*    digits.  Record the fact for compactness and efficiency.

      IF ( DACOOR ) THEN

*       Find the implementation type of the axis structure.

         CALL KPG1_AXTYP( NDF, 'Centre', ATYPE, STATUS )
         DPAXIS = ATYPE .EQ. '_DOUBLE'

*       To be able to store the NDF data co-ordinates in the database
*       the application requires a linear axis.

         IF ( DPAXIS ) THEN

            DO  I = 1, NDIM

*             Map the axis.

               CALL NDF_AMAP( NDFS, 'Centre', SDIM( I ), '_DOUBLE',
     :                        'READ', AXPNTR, AEL, STATUS )

*             Are all the axes monotonic?  Start a new error context so
*             that the error reports concerning a non-monotonic axis
*             may be annulled.  Instead we issue a warning message so
*             that the application can continue by using world
*             co-ordinates.

               IF ( STATUS .EQ. SAI__OK ) THEN
                  CALL ERR_MARK
                  CALL KPG1_MONOD( .TRUE., AEL, %VAL( AXPNTR( 1 ) ),
     :                             MONOTO( I ), STATUS )
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL ERR_ANNUL( STATUS )
                     MONOTO( I ) = .FALSE.
                  END IF
                  CALL ERR_RLSE
               END IF

*             Issue the warning.

               IF ( .NOT. MONOTO( I ) ) THEN
                  CALL MSG_SETI( 'IAXIS', SDIM( I ) )
                  CALL MSG_OUT( 'CONTOVER_NOTMONO',
     :              'CONTOVER: Axis ^IAXIS is not monotonic.  Will '/
     :              /'not record axis bounds in the graphics '/
     :              /'database.', STATUS )
               END IF

*             Unmap the axis.

               CALL NDF_AUNMP( NDFS, 'Centre', SDIM( I ), STATUS )
            END DO
         ELSE

            DO  I = 1, NDIM

*             Map the axis.

               CALL NDF_AMAP( NDFS, 'Centre', SDIM( I ), '_REAL',
     :                        'READ', AXPNTR, AEL, STATUS )

*             Are all the axes monotonic?  Start a new error context so
*             that the error reports concerning a non-monotonic axis
*             may be annulled.  Instead we issue a warning message so
*             that the application can continue by using world
*             co-ordinates.

               IF ( STATUS .EQ. SAI__OK ) THEN
                  CALL ERR_MARK
                  CALL KPG1_MONOR( .TRUE., AEL, %VAL( AXPNTR( 1 ) ),
     :                             MONOTO( I ), STATUS )
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL ERR_ANNUL( STATUS )
                     MONOTO( I ) = .FALSE.
                  END IF
                  CALL ERR_RLSE
               END IF

*             Issue the warning.

               IF ( .NOT. MONOTO( I ) ) THEN
                  CALL MSG_SETI( 'IAXIS', SDIM( I ) )
                  CALL MSG_OUT( 'CONTOVER_NOTMONO',
     :              'CONTOVER: Axis ^IAXIS is not monotonic.  Will '/
     :              /'not record axis bounds in the graphics '/
     :              /'database.', STATUS )
               END IF

*             Unmap the axis.

               CALL NDF_AUNMP( NDFS, 'Centre', SDIM( I ), STATUS )
            END DO
         END IF

         IF ( MONOTO( 1 ) .AND. MONOTO( 2 ) ) THEN

*       The axis is monotonic, determine whether or not the data
*       co-ordinates derived from the NDF axes are linear and not
*       identical to world (pixel) co-ordinates, and if so find the
*       linear transformation from world to data co-ordinates and the
*       axis bounds.

            IF ( DPAXIS ) THEN
               CALL KPG1_DCLID( NDIM, NDFS, DXLBND, DXUBND, DSCALE,
     :                          DOFSET, DACOOR, STATUS )
               AXLBND( 1 ) = REAL( DXLBND( 1 ) )
               AXLBND( 2 ) = REAL( DXLBND( 2 ) )
            ELSE
               CALL KPG1_DCLIR( NDIM, NDFS, AXLBND, AXUBND, SCALE,
     :                          OFFSET, DACOOR, STATUS )
            END IF

*       Set the flag to indicate no axis centres are present, when
*       either axis is non-monotonic.

         ELSE
            DACOOR = .FALSE.
         END IF
      END IF

      IF ( STATUS .NE. SAI__OK ) GOTO 960

*    Obtain the workspace needed to compute the contours.
*    ====================================================

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
*    spanning the whole of the cell or image.

      NLOCUS = 8 * MIN( 2 * CELDIM, DIMS( 1 ) + DIMS( 2 ) )

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

*    Draw the contours.
*    ==================

*    Draw the contour plot itself.  Note DX,DY are set to 0.0 so there
*    is no resolution restraint except imposed by pixel-resolution
*    contouring.  Also there are no annotations, therefore the ANNOTA
*    and NOISY arguments are .FALSE., and the labelling frequency is set
*    to an arbitrary 1.  The final .FALSE. is MAXRES.

      CALL KPS1_CNTUR( DIMS( 1 ), DIMS( 2 ), %VAL( PNTRI( 1 ) ), 0, 0,
     :                 DIMS( 1 ), DIMS( 2 ), CELDIM, CELDIM,
     :                 NCONT, CNTLEV, 0.0, 0.0, .FALSE., .FALSE., 1,
     :                 PENROT, THRESH, .FALSE., NLOCUS, %VAL( XPNTR ),
     :                 %VAL( YPNTR ), SLIST, %VAL( LLPNTR ),
     :                 %VAL( CFPNTR ), CNTUSD, STATUS )
      CALL SGS_FLUSH

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CONTOVER_SCA',
     :     'CONTOVER: Error contouring the array.', STATUS )
      END IF

*    Re-define world co-ordinates of the contouring zone to the normal
*    convention.

      CALL SGS_SW( REAL( SLBND( 1 ) ) - 1.0, REAL( SUBND( 1 ) ),
     :             REAL( SLBND( 2 ) ) - 1.0, REAL( SUBND( 2 ) ),
     :             STATUS )

*    ^^^^^^^^^^^^^^^^^^

*    Unmap and annul workspace.

      DO I = 1, NWORK
         CALL PSX_FREE( WPNTR( I ), STATUS )
      END DO

      IF ( STATUS .NE. SAI__OK ) GOTO 960

*    Record the data picture in the database.
*    ========================================
*
*    Switch back to the input picture.

      CALL AGI_SELP( PICID1, STATUS )

*    Record the picture and a reference to the NDF in the database.

      CALL KPG1_SDTRN( 'KAPPA_CONTOVER', NDF, PICID3, STATUS )

*    Store a transformation from data co-ordinates to world co-ordinates
*    where they are different and the data co-ordinates are linear.

      IF ( DACOOR ) THEN
         IF ( DPAXIS ) THEN
            CALL KPG1_LITRD( DSCALE, DOFSET, STATUS )
         ELSE
            CALL KPG1_LITRR( SCALE, OFFSET, STATUS )
         END IF
      END IF

*    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

*    Report the heights to the user as there is no key.

      CALL CNTHLT( NCONT, CNTLEV, CNTUSD, STATUS )

*    Reset the line width.
*    =====================

      IF ( SCLINE ) THEN

*       Set the line width scale factor source flags to bundled.

         LASF( 2 ) = 0
         CALL GSASF( LASF )

*       Watch out for any error.

         CALL GKS_GSTAT( STATUS )
      END IF

      GOTO 960

*    Something has gone wrong obtaining work space, so report error
*    context and tidy the workspace obtained successfully.

*    Clear up the workspace.
*    =======================

 950  CONTINUE
      CALL ERR_REP( 'CONTOVER_WSP',
     :  'CONTOVER: Unable to get workspace for plotting contours',
     :  STATUS )

      DO I = 1, NWORK
         CALL PSX_FREE( WPNTR( I ), STATUS )
      END DO

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
