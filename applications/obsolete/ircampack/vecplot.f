      SUBROUTINE VECPLOT( STATUS )
*+
*  Name:
*     VECPLOT

*  Purpose:
*     Plots a 2-d vector map

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL VECPLOT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application plots vectors defined by the values contained
*     within a pair of 2-d NDFs, the first holding the magnitude of the
*     vector quantity at each pixel, and the second holding the
*     corresponding vector orientations. The number of vectors in the
*     plot is kept to a managable value by only plotting vectors for
*     pixels on a sparse regular matrix. The increment (in pixels)
*     between plotted vectors is given by parameter STEP. Zero
*     orientation may be fixed at any position angle within the plot by
*     specifying an appropriate value for parameter ANGROT. Each vector
*     may be represented either by an arrow or by a simple line, as
*     selected by parameter VTYPE.
*
*     The plot is situated within the current graphics-database
*     picture, and may reside within optional, annotated and enumerated
*     axes.  An optional, but recommended, key may be drawn to the
*     right of the plot.  It reports the data units if there are any
*     (taken from the NDF associated with parameter NDF1) and gives the
*     scale used for drawing the vectors in data units per centimetre.
*     It also displays a typical vector and the corresponding data
*     value. The justification of the vector is indicated by a small
*     circle placed at the position of the corresponding pixel centre.
*
*     If the current graphics-database picture is a DATA picture or
*     contains a DATA picture (such as created by the applications
*     DISPLAY, CONTOUR, etc), then the vector plot is overlayed on
*     top of the existing DATA plot. In this case any requested
*     annotation and key are drawn outside the DATA picture but within
*     the current picture. If there is insufficient room within the
*     currrent picture, then the annotation and/or key may not be
*     drawn. The user is warned if this happens but the application
*     continues. If no DATA picture can be found within the current
*     picture then the user specifies ther total size of the plot frame
*     using parameters PXSIZE and PYSIZE, but the application itself
*     chooses how to position the vector plot and key within this
*     frame.

*  Usage:
*     VECPLOT NDF1 NDF2 [COMP] [STEP] [VSCALE] [VTYPE] [JUST] [KEY]
*             [DEVICE]

*  ADAM Parameters:
*     ABSLAB  =  LITERAL (Read)
*        Label for the plot abscissa, in which NCAR fancy founts may be
*        embedded when FONT = "NCAR".  If axis information is present
*        in NDF1 the suggested default is the axis label from NDF1
*        followed by the units, in parentheses.  If an error occurs
*        obtaining the label the suggested default is "X". []
*     ANGROT = _REAL (Read)
*        A rotation angle in degrees to be added to each vector
*        orientation before plotting the vectors (see parameter NDF2).
*        [0.0]
*     AXES = _LOGICAL (Read)
*        True if labelled and annotated axes are to be drawn around the
*        plot.  The annotations are either the data co-ordinates from
*        the axis components of NDF1, provided these are present and
*        linear and COSYS = "DATA"; otherwise pixel co-ordinates are
*        used.
*     BORDER = _LOGICAL (Read)
*        True if a box is to be drawn about the plot. This is only
*        accessed if no axes are drawn. [TRUE]
*     CLEAR = _LOGICAL (Read)
*        True if the graphics device is to be cleared before display
*        of the array. [TRUE]
*     COMP = LITERAL (Read)
*        The component of NDF1 which is to be used to define the vector
*        magnitudes. It may be "Data", or "Variance". The vector
*        orientations are always defined by the "Data" component of
*        NDF2. ["Data"]
*     COSYS = LITERAL (Read)
*        The co-ordinate system to be used.  This can be either "WORLD"
*        or "DATA".  "WORLD" makes pixel co-ordinates to appear on axes.
*        If COSYS = "DATA" the axis information from NDF1 is used to
*        annotate axes (if it exists).  [Current co-ordinate system]
*     DEVICE = DEVICE (Read)
*        The plotting device. [Current image-display device]
*     FONT = LITERAL (Read)
*        The fount to be used for the line graphics.  It can be either
*        "NCAR" for the NCAR fancy characters and "GKS" for the standard
*        GKS san-serif fount.   The former is intended for hardcopy
*        publication-quality plots, since it is relatively slow; the
*        latter is intended for normal interactive graphics requiring
*        rapid plotting, and it is clearer on small plots. The
*        suggested default is the current value. ["GKS"]
*     JUST = LITERAL (Read)
*        The justification for each vector; can take any of the
*        following values:
*
*         "Centre" - The vectors are drawn centred on the
*                    corresponding pixel.
*
*         "Start"  - The vectors are drawn starting at the
*                    corresponding pixel
*
*         "End"    - The vectors are drawn ending at the corresponding
*                    pixel.  ["CENTRE"]
*     KEY = _LOGICAL (Read)
*        True if a key is to be produced. [TRUE]
*     KEYVEC = _REAL (Read)
*        Length of the vector to be displayed in the key, in data units.
*        A default value is generated based on the spread of vector
*        lengths in the plot. []
*     MAJTIC( 2 ) = _REAL (Read)
*        The parameter controlling the numbers of major tick marks
*        for the x and y axes.  (Number used is between MAJTIC+2 and
*        5*MAJTIC/2+4.) [3.,3.]
*     MINTIC( 2 ) = _REAL (Read)
*        The number of minor tick marks between each major tick mark
*        for the x and y axes.  A negative value forces the graphics
*        package to compute appropriate values. [-1.,-1.]
*     NDF1 = NDF (Read)
*        NDF structure containing the 2-d image giving the vector
*        magnitudes.
*     NDF2 = NDF (Read)
*        NDF structure containing the 2-d image giving the vector
*        orientations.The values are considered to be in units of
*        degrees unless the "UNITS" component of the NDF has the value
*        "Radians" (case insensitive). The positive y axis defines zero
*        orientation, and rotation from the x axis to the y axis is
*        considered positive.
*     ORDLAB  =  LITERAL (Read)
*        Label for the plot ordinate, in which NCAR fancy founts may be
*        embedded when FONT = "NCAR".  If axis information is present
*        in NDF1 the suggested default is the axis label from NDF1
*        followed by the units, in parentheses.  If an error occurs
*        obtaining the label the suggested default is "Y". []
*     OUTTIC = _LOGICAL (Read)
*        True if the axis tick marks are to appear on the outside of
*        the axes instead of inside. By default, the tick marks are
*        drawn outside the plotting region to eliminate
*        intersections of ticks with the vectors. [TRUE]
*     PLTITL = LITERAL (Read)
*        The title of the plot.  Up to about 40 characters can be
*        accommodated, and NCAR fancy founts may be embedded when FONT =
*        "NCAR". If an error occurs obtaining the title, it is
*        defaulted to "VECPLOT map".  [The title from NDF1]
*     PXSIZE = _REAL (Read)
*        The length (x axis) of the plot in metres. [Maximum that can
*        fit in the current picture whilst preserving square pixels]
*     PYSIZE = _REAL (Read)
*        The length (y axis) of the plot in metres. [Maximum that can
*        fit in the current picture whilst preserving square pixels]
*     STEP = _INTEGER (Read)
*        The number of pixels between adjacent displayed vectors (along
*        both axes). Increasing this value reduces the number of
*        displayed vectors. The default value gives about 30 vectors
*        along the longest axis of the plot. []
*     THICK = _REAL (Read)
*        The thickness of the axes and annotations in the plot, where
*        1.0 is the normal thickness. Currently, this is only available
*        on a few devices.  It must take a value in the range 0.5--5.0.
*        [1.0]
*     VECCOL = LITERAL (Read)
*        The colour for the vectors. The options are:
*          "MAX"          - The maximum colour index in the image
*                           display colour lookup table.
*          "MIN"          - The minimum (non-reserved) colour index in
*                           the image display colour lookup table.
*          An integer     - The actual colour index. It is constrained
*                           between 0 and the maximum colour index
*                           available on the device.
*          A named colour - Uses the named colour from the palette, and
*                           if it is not present, the nearest colour
*                           from the palette is selected.
*        The suggested default is the current value. [The current value,
*        but equals "MIN" if there is no current value.]
*     VSCALE = _REAL (Given)
*        The scale to be used for the vectors. The supplied value
*        should give the data value corresponding to a vector length of
*        one centimetre. The default makes 5% of all displayed vectors
*        larger than the interval between adjacent vectors. []
*     VTYPE = LITERAL (Read)
*        The type of vector to be plotted; can take the value "Arrow"
*        or "Line". Vectors are drawn as arrows or lines accordingly.
*        ["LINE"]

*  Examples:
*     VECPLOT POLINT POLANG
*        Produces a vector map on the current graphics device with
*        vector magnitude taken from POLINT and vector orientation
*        taken from POLANG.  All other settings are defaulted, so
*        for example about 20 vectors are displayed along the longest
*        axis, and a key is plotted.
*     VECPLOT POLINT POLANG ANGROT=23.4
*        Produces a vector map in which the primary axis of the vectors
*        (as defined by the value zero in the NDF "POLANG") is at the
*        position angle 23.4 degrees (measured anti-clockwise from the
*        positove Y axis) in the displayed map.
*     VECPLOT STACK(,,2) STACK(,,1) VTYPE=ARROW JUST=START NOKEY
*        Produces a vector map in which the vectors are defined by two
*        planes in the 3-d NDF "STACK". There is no need to copy the
*        two planes into two separate NDFs before running VECPLOT.
*        Each vector is represented by an arrow, starting at the
*        position of the corresponding pixel. No key to the vector scale
*        and justification is produced.

*  Notes:
*     -  The application stores a number of pictures in the graphics
*     database in the following order: a FRAME of the specified size
*     containing the title, annotated axes, vector map and key; a DATA
*     picture which is stored with world co-ordinates in units of data
*     pixels; and a KEY picture to store the key if present.  The DATA
*     picture may also have double-precision data co-ordinates derived
*     from the axis components of NDF1 provided these are linear and
*     different from pixel co-ordinates; the data co-ordinates are
*     stored via a linear transformation.  A reference to NDF1 is
*     stored with the DATA picture.  On exit the current database
*     picture for the chosen device reverts to the input picture.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-AUG-1993 (DSB):
*        Original version.
*     21-SEP-1993 (DSB):
*        Modified to overlay the vector plot on any existing DATA plot
*        contained within the current picture.
*     1-MAY-1995 (DSB):
*        ANGFAC set to DTOR if no UNITS component found in NDF2 (i.e.
*        assume angles are in degrees if units are unspecified). This
*        is what the docs says has always happened, but in fact it was
*        previously assumed that angles were in radians if units were
*        unspecified (i.e. ANGFAC was incorrrectly set to 1.0).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE            ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Standard SAE constants
      INCLUDE 'PRM_PAR'        ! VAL_ constants
      INCLUDE 'NDF_PAR'        ! NDF_ constants
      INCLUDE 'CTM_PAR'        ! CTM_ Colour Table Management constants
      INCLUDE 'GKS_PAR'        ! GKS constants (GSET)

*  Status:
      INTEGER STATUS

*  External References:
      LOGICAL CHR_SIMLR        ! Two strings equivalent apart from case?

*  Local Constants:
      REAL
     :           AHPAR,        ! Arrow head size normalised to long axis
     :           ANCLP1,       ! Fraction of the frame zone in which the
     :           ANCLP2,       ! image will appear when there are axes
     :           ANCLP3,
     :           ANCLP4,
     :           ASPKEY,       ! Fractional aspect ratio for the key
     :           DTOR,         ! Degrees to radians conversion factor
     :           NVEC0         ! Default no. of vectors along short axis

      PARAMETER( AHPAR = 0.01,
     :           ANCLP1 = 0.19,
     :           ANCLP2 = 0.95,
     :           ANCLP3 = 0.15,
     :           ANCLP4 = 0.91,
     :           ASPKEY = 0.4,
     :           DTOR = 1.7453293E-2,
     :           NVEC0 =  30.0 )

      INTEGER
     :           CUNITS,       ! Max. no. of characters in units string
     :           NDIM          ! Dimensionality of input array

      PARAMETER( CUNITS = 14,
     :           NDIM = 2 )

*  Local Variables:
      CHARACTER
     :        ABSLAB*72,       ! Label for the abscissa of the plot
     :        COMP*8,          ! Component to be displayed
     :        COSYS*5,         ! Co-ordinate system
     :        FOUNT*4,         ! Fount type
     :        JUST*6,          ! Vector justification; CENTRE or START
     :        ORDLAB*72,       ! Label for the ordinate of the plot
     :        PLTITL*72,       ! Title of the plot
     :        UNITS1*( CUNITS + 5 ),! Units of the data
     :        UNITS2*( CUNITS + 5 ),! Units of the data
     :        VTYPE*5          ! Vector type; arrow or line

      DOUBLE PRECISION
     :        OFFSET( NDIM ),  ! Offsets in world-to-data mapping
     :        SCALE( NDIM )    ! Scale factors in world-to-data mapping

      LOGICAL
     :        AXES,            ! Annotated axes to be drawn?
     :        BORDER,          ! Border to be drawn round the plot?
     :        CLEAR,           ! Graphics device to be cleared?
     :        DATAVL,          ! Usable data coordinates available?
     :        KEY,             ! A key to vector scale to be plotted?
     :        MONOTO,          ! Axes are monotonic?
     :        OUTTIC,          ! Tick marks to be put outside the box?
     :        THERE            ! Does the requested object exist?

      REAL
     :        AHSIZE,          ! Arrow head size in world coordinates
     :        AHSIZM,          ! Arrow head size in metres
     :        ALBND( NDIM ),   ! Axis lower bounds
     :        ANGFAC,          ! NDF2 data to radians conversion factor
     :        ANGROT,          ! Angle to add on to NDF2 values
     :        AUBND( NDIM ),   ! Axis upper bounds
     :        DEFSCA,          ! Default value for VSCALE
     :        DXW,             ! Size of a raster unit in x
     :        DYW,             ! Size of a raster unit in y
     :        GRID( 4 )        ! Current AUTOGRAPH grid offsets

      REAL
     :        MINTIC( 2 ),     ! No. of minor tick marks in x and y
     :        MAJTIC( 2 ),     ! Major tick mark parameters in x and y
     :        SXHI,            ! High X bound of data zone pixel coords
     :        SXLO,            ! Low X bound of data zone pixel coords
     :        SYHI,            ! High Y bound of data zone pixel coords
     :        SYLO,            ! Low Y bound of data zone pixel coords
     :        THICK,           ! The line thickness (standard is 1.0)
     :        TICDEF( 2 ),     ! Suggested default axis-tick values
     :        TYPDAT,          ! A typical data value from 1st input NDF
     :        VSCALE,          ! Vector scale, data units per centimetre
     :        X1, X2, Y1, Y2,  ! Zone size in world co-ordinates
     :        XM, YM           ! DATA zone size in metres

      INTEGER
     :        COLI,            ! GKS colour index for current SGS pen
     :        EL1,             ! Number of elements in the first NDF
     :        EL2,             ! Number of elements in the second NDF
     :        LBND1( NDF__MXDIM ),! Lower bounds of the first NDF
     :        LBND2( NDF__MXDIM ),! Lower bounds of the second NDF
     :        NCU1,            ! No. of characters in units of 1st NDF
     :        NCU2,            ! No. of characters in units of 2nd NDF
     :        I,               ! Loop count
     :        IERR,            ! GKS error code.
     :        INDF1,           ! Identifier for first NDF
     :        INDF2,           ! Identifier for second NDF
     :        IPIXX,           ! Max. no. of pixel columns
     :        IPIXY,           ! Max. no. of pixel lines
     :        IWKID            ! Current GKS workstation identifier

      INTEGER
     :        LNTYPE,          ! GKS line type
     :        LWIDTH,          ! GKS line width
     :        NDIMS1,          ! Total number of dimensions in 1st NDF
     :        NDIMS2,          ! Total number of dimensions in 2nd NDF
     :        NINTS,           ! No. of colour indices available
     :        PEN,             ! Current SGS pen number
     :        PICID0,          ! AGI identifier on input
     :        PICIDD,          ! AGI identifier for the DATA picture
     :        PICIDF,          ! AGI identifier for the frame picture
     :        PICIDK,          ! AGI identifier for the KEY picture
     :        PNTR1,           ! Pointer to 1st data array
     :        PNTR2,           ! Pointer to 2nd data array
     :        SDIM1( NDF__MXDIM ),! Significant dimensions of 1st NDF
     :        SDIM2( NDF__MXDIM ),! Significant dimensions of 2nd NDF
     :        SLBND( NDIM ),   ! Significant lower bounds of plot area
     :        SLBND1( NDIM ),  ! Significant lower bounds of 1st NDF
     :        SLBND2( NDIM ),  ! Significant lower bounds of 2nd NDF
     :        VCI              ! Vector colour index

      INTEGER
     :        STEP,            ! Pixel increment between vectors
     :        SUBND( NDIM ),   ! Significant upper bounds of plot area
     :        SUBND1( NDIM ),  ! Significant upper bounds of 1st NDF
     :        SUBND2( NDIM ),  ! Significant upper bounds of 2nd NDF
     :        UBND1( NDF__MXDIM ),! Upper bounds of the 1st NDF
     :        UBND2( NDF__MXDIM ),! Upper bounds of the 2nd NDF
     :        ZONE0,           ! Zone id. for current picture on entry
     :        ZONED,           ! Zone id. for DATA picture
     :        ZONEF,           ! Zone id. for new FRAME picture
     :        ZONEG,           ! Zone id. for the NCAR "graph window"
     :        ZONEK            ! Zone id. for the KEY picture
*.

*  Check inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Find which component of the first input NDF is to be used to define
*  the vector magnitudes (the vector orientations are always defined by
*  the DATA component of the second NDF).
      CALL PAR_CHOIC( 'COMP', 'Data', 'Data,Variance', .FALSE., COMP,
     :                STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain an identifier for the first NDF which defines the vector
*  magnitudes.
      CALL NDF_ASSOC( 'NDF1', 'READ', INDF1, STATUS )

*  Report an error if the required component does not exist.
      CALL NDF_STATE( INDF1, COMP, THERE, STATUS )
      IF( .NOT. THERE .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'COMP', COMP )
         CALL NDF_MSG( 'NDF', INDF1 )
         CALL ERR_REP( 'VECPLOT_NOCOMP', 'VECPLOT: ^NDF does not '//
     :                 'contain a ^COMP component.', STATUS )
         GO TO 999
      END IF

*  Ensure that there are just two significant dimensions in the first
*  NDF (i.e. dimensions spanning more than a single element). An error
*  is reported otherwise. The indices of the significant dimensions are
*  returned in SDIM1.
      CALL KPG1_SGDIM( INDF1, NDIM, SDIM1, STATUS )

*  Obtain the bounds of the first NDF.
      CALL NDF_BOUND( INDF1, NDF__MXDIM, LBND1, UBND1, NDIMS1, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Store the bounds of the significant dimensions of the first NDF.
      SLBND1( 1 ) = LBND1( SDIM1( 1 ) )
      SLBND1( 2 ) = LBND1( SDIM1( 2 ) )
      SUBND1( 1 ) = UBND1( SDIM1( 1 ) )
      SUBND1( 2 ) = UBND1( SDIM1( 2 ) )

*  Obtain the units of the requested component of the first NDF.
      CALL KPG1_DAUNI( INDF1, COMP, UNITS1, NCU1, STATUS )

*  Map the required component as an array of _REAL values.
      CALL NDF_MAP( INDF1, COMP, '_REAL', 'READ', PNTR1, EL1, STATUS )

*  Now do just the same for the second NDF which defines the vector
*  orientations. The DATA component is used.
      CALL NDF_ASSOC( 'NDF2', 'READ', INDF2, STATUS )
      CALL KPG1_SGDIM( INDF2, NDIM, SDIM2, STATUS )
      CALL NDF_BOUND( INDF2, NDF__MXDIM, LBND2, UBND2, NDIMS2, STATUS )
      IF( STATUS .NE. SAI__OK ) GO TO 999

      SLBND2( 1 ) = LBND2( SDIM2( 1 ) )
      SLBND2( 2 ) = LBND2( SDIM2( 2 ) )
      SUBND2( 1 ) = UBND2( SDIM2( 1 ) )
      SUBND2( 2 ) = UBND2( SDIM2( 2 ) )

      CALL KPG1_DAUNI( INDF2, 'DATA', UNITS2, NCU2, STATUS )
      CALL NDF_MAP( INDF2, 'Data', '_REAL', 'READ', PNTR2, EL2, STATUS )

*  Set up a factor which converts values stored in the DATA component
*  of the second NDF into units of radians. If the UNITS component does
*  not have the value "RADIANS" (case insensitive), then assume the
*  data values are in degrees.
      IF( NCU2 .GT. 0 ) THEN
         IF( CHR_SIMLR( UNITS2( : NCU2 ), 'RADIANS' ) ) THEN
            ANGFAC = 1.0
         ELSE
            ANGFAC = DTOR
         END IF
      ELSE
         ANGFAC = DTOR
      END IF

*  Compute the bounds of the significant dimensions within the region
*  of overlap between the two NDFs. Using this method allows greater
*  flexibility in the specification of the input NDFs than would be
*  allowed if NDF_MBND was used (for instance NDF_MBND would report an
*  error if the two NDFs were given as FRED(,,1) and FRED(,,2), where
*  FRED is a 3-d NDF).
      SLBND( 1 ) = MAX( SLBND1( 1 ), SLBND2( 1 ) )
      SLBND( 2 ) = MAX( SLBND1( 2 ), SLBND2( 2 ) )
      SUBND( 1 ) = MIN( SUBND1( 1 ), SUBND2( 1 ) )
      SUBND( 2 ) = MIN( SUBND1( 2 ), SUBND2( 2 ) )

*  Report an error if there is no overlap between the two NDFs.
      IF( ( SUBND( 1 ) .LT. SLBND( 1 ) .OR.
     :      SUBND( 2 ) .LT. SLBND( 2 ) ) .AND.
     :      STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF1', INDF1 )
         CALL NDF_MSG( 'NDF2', INDF2 )
         CALL ERR_REP( 'VECPLOT_NOOVER',
     :                 'VECPLOT: There are no pixels in common '//
     :                 'between ^NDF1 and ^NDF2.', STATUS )
         GO TO 999
      END IF

*  Store the corresponding pixel coordinate bounds. These go to the
*  outer edges of the pixels, not to the centres.
      SXLO = REAL( SLBND( 1 ) - 1 )
      SXHI = REAL( SUBND( 1 ) )
      SYLO = REAL( SLBND( 2 ) - 1 )
      SYHI = REAL( SUBND( 2 ) )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain a "typical" data value from the first input NDF. This will be
*  used to define the default vector scaling.
      CALL DTPCL( INDF1, SUBND, SLBND, SDIM1, TYPDAT, STATUS )

*  See if annotated axes are required, and if so, what sort of
*  coordinates are to be displayed. Store a blank value for the
*  coordinate system if no axes are required.
      CALL PAR_GTD0L( 'AXES', .TRUE., .TRUE., AXES, STATUS )
      IF( AXES ) THEN
         CALL PAR_CHOIC( 'COSYS', 'Data', 'Data,World', .FALSE., COSYS,
     :                   STATUS )
      ELSE
         COSYS = ' '
      END IF

*  See if there are usable axis coordinate structures within the first
*  NDF. These provide the data coordinates which are plotted on the
*  annotated axes (if required), and stored in the graphics database
*  with the newly created data picture. Axis coordinates must be
*  monotonic to be usable. The coefficients of the linear transformation
*  from world to data coordinates are returned. If no usable data
*  coordinates are available, then a unit transformation is returned.
*  If the axes are non-linear a warning message is issued and a linear
*  approximation to the axis coordinates is returned.
      CALL KPG1_CHAXD( INDF1, SDIM1, DATAVL, SCALE, OFFSET, STATUS )

*  If axes with data coordinates were requested but no usable data
*  coordinates are available, use world coordinates for the axes
*  instead.
      IF( COSYS .EQ. 'DATA' .AND. (.NOT. DATAVL ) ) COSYS = 'WORLD'

*  See if a key to the vector magnitude scale is required.
      CALL PAR_GTD0L( 'KEY', .TRUE., .TRUE., KEY, STATUS )

*  Now we set up the graphics system. First see whether the current
*  picture is to be cleared before creating the new plot.
      CALL PAR_GTD0L( 'CLEAR', .TRUE., .TRUE., CLEAR, STATUS )

*  Open the graphics database for the selected device, obtaining
*  identifiers for the current AGI picture and SGS zone.
      IF( CLEAR ) THEN
         CALL AGS_ASSOC( 'DEVICE', 'WRITE', ' ', PICID0, ZONE0, STATUS )
      ELSE
         CALL AGS_ASSOC( 'DEVICE', 'UPDATE', ' ', PICID0, ZONE0,
     :                   STATUS )
      END IF

*  Ensure that plotted lines will be solid.
      CALL KPG1_SOLIN( STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Create the zones required for the various parts of the plot (FRAME,
*  KEY, DATA and graph window). A picture corresponding to the new FRAME
*  zone is stored in the AGI database. If a DATA picture is contained
*  within the current picture then it is used to define the zone in
*  which to produce the vector plot. In this case, the bounds of the
*  displayed DATA picture (in pixel coordinates) are returned in SXLO,
*  etc. Otherwise, the user is asked for a FRAME picture size and the
*  DATA picture is created within it using the supplied values of SXLO,
*  etc. The current zone is unchanged.
      CALL ZOPIC( 'PXSIZE', 'PYSIZE', 'KAPPA_VECPLOT', ASPKEY, SXLO,
     :            SXHI, SYLO, SYHI, KEY, AXES, ZONEF, ZONED, ZONEK,
     :            ZONEG, PICIDF, XM, YM, STATUS )

*  Produce annotated axes if required.
      IF( AXES ) THEN

*  Obtain the labels for each axis. First deal with axes displaying
*  DATA coordinates...
         IF( COSYS .EQ. 'DATA' ) THEN

*  Get the abscissa and ordinate labels, suggesting the value in the
*  axis structure of INDF1, if present, as the default. Otherwise, use
*  "X" and "Y" as defaults.
            CALL KPG1_GAXLB( INDF1, SDIM1( 1 ), 'ABSLAB', 'X', ABSLAB,
     :                       STATUS )
            CALL KPG1_GAXLB( INDF1, SDIM2( 2 ), 'ORDLAB', 'Y', ORDLAB,
     :                       STATUS )

*  Now deal with axes displaying WORLD coordinates. Get the labels
*  without reference to any axis structures. The suggested defaults are
*  'X' and 'Y'.
         ELSE
            CALL PAR_DEF0C( 'ABSLAB', 'X', STATUS )
            CALL PAR_GET0C( 'ABSLAB', ABSLAB, STATUS )
            CALL PAR_DEF0C( 'ORDLAB', 'Y', STATUS )
            CALL PAR_GET0C( 'ORDLAB', ORDLAB, STATUS )
         END IF

*  Obtain a title for the plot, using the title from the first NDF as
*  the default. If NDF1 does not have a title, then use "VECPLOT map"
*  as the default.
         CALL KPG1_GNTIT( INDF1, 'PLTITL', 'VECPLOT map', PLTITL,
     :                    STATUS )

*  Get the number of minor ticks. Negative values for MINTIC causes NCAR
*  to calculate default values.
         TICDEF( 1 ) = -1.
         TICDEF( 2 ) = -1.
         CALL PAR_GDR1R( 'MINTIC', 2, TICDEF, -1., VAL__MAXR, .FALSE.,
     :                   MINTIC, STATUS )

*  Get the parameter controlling the number of major ticks per axis.
         TICDEF( 1 ) = 3.0
         TICDEF( 2 ) = 3.0
         CALL PAR_GDR1R( 'MAJTIC', 2, TICDEF, -1., VAL__MAXR, .FALSE.,
     :                   MAJTIC, STATUS )

*  See if the tick marks are to be drawn on the outside of the axes.
         CALL PAR_GTD0L( 'OUTTIC', .TRUE., .TRUE., OUTTIC, STATUS )

*  Get the line thickness for the axes.
         CALL PAR_GDR0R( 'THICK', 1.0, 0.5, 5.0, .TRUE., THICK, STATUS )

*  See which fount is required, and select it.  Although NCAR is the
*  default, one or the other must always be selected to prevent
*  persistence from earlier applications.
         CALL PAR_CHOIC( 'FONT', 'GKS', 'GKS,NCAR', .TRUE., FOUNT,
     :                   STATUS )

         IF( FOUNT .EQ. 'GKS ' ) THEN
            CALL AGPWRT( 0.0, 0.0, ' ', 0, 0, 0, -100 )

         ELSE IF( FOUNT .EQ. 'NCAR' ) THEN
            CALL AGPWRT( 0.0, 0.0, ' ', 0, 0, 0, 100 )
         END IF

*  Select the zone to be used as the AUTOGRAPH "graph window" (the
*  rectangle which contains labels, axes, grid, etc).
         CALL SGS_SELZ( ZONEG, STATUS )

*  Ensure that the graph window used by AUTOGRAPH corresponds to this
*  zone.
         CALL SNX_AGWV

*  Ensure that the graph window zone has world coordinate bounds of
*  [0,1] on each axis.
         CALL SGS_SW( 0.0, 1.0, 0.0, 1.0, STATUS )

*  Get the bounds of the DATA zone within the world coordinate system
*  of the graph window zone.
         CALL SGS_TPZ( ZONED, SXLO, SYLO, ZONEG, X1, Y1, STATUS )
         CALL SGS_TPZ( ZONED, SXHI, SYHI, ZONEG, X2, Y2, STATUS )

*  Get the current bounds of the AUTOGRAPH "grid window" (the rectanglar
*  section of the graph window along the edges of which axes are to be
*  drawn). These values are given as fractions of the graph window width
*  and height, starting from the bottom left corner of the graph window.
         CALL AGGETF( 'GRID/LEFT.', GRID( 1 ) )
         CALL AGGETF( 'GRID/RIGHT.', GRID( 2 ) )
         CALL AGGETF( 'GRID/BOTTOM.', GRID( 3 ) )
         CALL AGGETF( 'GRID/TOP.', GRID( 4 ) )

*  Store new values corresponding to the bounds of the DATA zone. The
*  original values are re-instated when the axes have been produced.
         CALL AGSETF( 'GRID/LEFT.', X1 )
         CALL AGSETF( 'GRID/RIGHT.', X2 )
         CALL AGSETF( 'GRID/BOTTOM.', Y1 )
         CALL AGSETF( 'GRID/TOP.', Y2 )

*  Store the bounds of the coordinate values (WORLD or DATA) used to
*  annotate the axes.
         IF( COSYS .EQ. 'DATA' ) THEN
            ALBND( 1 ) = REAL( SCALE( 1 )*DBLE( SXLO ) + OFFSET( 1 ) )
            AUBND( 1 ) = REAL( SCALE( 1 )*DBLE( SXHI ) + OFFSET( 1 ) )
            ALBND( 2 ) = REAL( SCALE( 2 )*DBLE( SYLO ) + OFFSET( 2 ) )
            AUBND( 2 ) = REAL( SCALE( 2 )*DBLE( SYHI ) + OFFSET( 2 ) )

         ELSE IF( COSYS .EQ. 'WORLD' ) THEN
            ALBND( 1 ) = SXLO
            AUBND( 1 ) = SXHI
            ALBND( 2 ) = SYLO
            AUBND( 2 ) = SYHI

         END IF

*  Draw annotated axes in the graph window with the grid positioned as
*  defined above.
         CALL NCRAXS( ALBND( 1 ), ALBND( 2 ), AUBND( 1 ), AUBND( 2 ),
     :                PLTITL, ABSLAB, ORDLAB, MINTIC, MAJTIC, OUTTIC,
     :                THICK, .FALSE., STATUS )

*  Restore the original bounds of the AUTOGRAPH grid window.
         CALL AGSETF( 'GRID/LEFT.', GRID( 1 ) )
         CALL AGSETF( 'GRID/RIGHT.', GRID( 2 ) )
         CALL AGSETF( 'GRID/BOTTOM.', GRID( 3 ) )
         CALL AGSETF( 'GRID/TOP.', GRID( 4 ) )

*  If no axes are required...
      ELSE

*  See if a border is to be produced.
         CALL PAR_GTD0L( 'BORDER', .TRUE., .TRUE., BORDER, STATUS )

*  If required, find the bounds of the DATA zone within the FRAME
*  coordinate system and draw a box around it. The box is drawn one
*  device unit inside the data box to avoid bits of it being clipped.
         IF( BORDER ) THEN
            CALL SGS_TPZ( ZONED, SXLO, SYLO, ZONEF, X1, Y1, STATUS )
            CALL SGS_TPZ( ZONED, SXHI, SYHI, ZONEF, X2, Y2, STATUS )
            CALL SGS_SELZ( ZONEF, STATUS )
            CALL SGS_IDUN( DXW, DYW )
            CALL SGS_BOX( X1 + DXW, X2 - DXW, Y1 + DYW, Y2 - DYW )
         ENDIF

      END IF

*  Get the angle (in degrees) which is to be added to the values stored
*  in NDF2, and convert to radians.
      CALL PAR_GET0R( 'ANGROT', ANGROT, STATUS )
      ANGROT = ANGROT*DTOR

*  Find the increment between plotted vectors, in pixels, ensuring that
*  it is not zero or negative. A default value which gives a reasonable
*  number of vectors along the shortest axis is used.
      STEP = MAX( 1, NINT( MAX( SXHI - SXLO, SYHI - SYLO )/NVEC0 ) )
      CALL PAR_DEF0I( 'STEP', STEP, STATUS )
      CALL PAR_GET0I( 'STEP', STEP, STATUS )
      STEP = MAX( 1, STEP )

*  Establish the default value for the vector scaling factor such that
*  a typical data value corresponds to a vector equal to the increment
*  size given by STEP, and then get a new (positive) value. If a value
*  of zero is supplied, use the default value.
      DEFSCA = ABS( TYPDAT*( SXHI - SXLO )/( 100.0*XM*REAL( STEP ) ) )
      CALL PAR_DEF0R( 'VSCALE', DEFSCA, STATUS )
      CALL PAR_GET0R( 'VSCALE', VSCALE, STATUS )
      VSCALE = ABS( VSCALE )
      IF( VSCALE .LE. VAL__SMLR ) VSCALE = DEFSCA

*  Get the type of vectors to be plotted, arrows or lines.
      CALL PAR_CHOIC( 'VTYPE', 'LINE', 'LINE,ARROW', .TRUE., VTYPE,
     :                STATUS )

*  Get the vector justification to be used.
      CALL PAR_CHOIC( 'JUST', 'CENTRE', 'CENTRE,START,END', .TRUE.,
     :                JUST, STATUS )

*  Set up the arrow head size in units of pixels ("Line" vectors are
*  considered to be "Arrow" vectors with zero sized arrow heads).
      IF( VTYPE .EQ. 'ARROW' ) THEN
         AHSIZE = AHPAR*MAX( SXHI - SXLO, SYHI - SYLO )
      ELSE
         AHSIZE = 0.0
      END IF

*  Get the arrow head size in metres.
      AHSIZM = AHSIZE*XM/( SXHI - SXLO )

*  Select the DATA zone as the current zone.
      CALL SGS_SELZ( ZONED, STATUS )

*  Obtain the maximum number of colour indices.
      CALL KPG1_QIDAT( 'DEVICE', 'SGS', NINTS, IPIXX, IPIXY, STATUS )

*  See what pen is to be used to draw the vectors.
      CALL KPG1_MACOL( 'VECCOL', CTM__RSVPN, NINTS - 1, VCI, STATUS )

*  Inquire the current SGS pen colour index.
      CALL SGS_IPEN( PEN )
      CALL SGS_ICURW( IWKID )
      CALL GQPLR( IWKID, PEN, GSET, IERR, LNTYPE, LWIDTH, COLI )

*  Store the new colour index for this pen.
      CALL GSPLR( IWKID, PEN, LNTYPE, LWIDTH, VCI )

*  See if a GKS error has occurred.
      CALL GKS_GSTAT( STATUS )

*  Plot the vectors.
      CALL VECPL( SLBND1( 1 ), SUBND1( 1 ), SLBND1( 2 ), SUBND1( 2 ),
     :            %VAL( PNTR1 ), SLBND2( 1 ), SUBND2( 1 ), SLBND2( 2 ),
     :            SUBND2( 2 ), %VAL( PNTR2 ), ANGFAC, ANGROT, STEP,
     :            VSCALE, AHSIZE, JUST, STATUS )

*  Flush all graphics.
      CALL SGS_FLUSH

*  Re-instate the original colour index for this pen.
      CALL GSPLR( IWKID, PEN, LNTYPE, LWIDTH, COLI )

*  Select the frame picture.
      CALL AGI_SELP( PICIDF, STATUS )

*  Record the data picture and a reference to the first NDF in the
*  database.
      CALL KPG1_SDTRN( 'KAPPA_VECPLOT', 'NDF1', PICIDD, STATUS )

*  If usable data coordinates are available, store a transformation
*  from data co-ordinates to world co-ordinates with the data picture in
*  the graphics database.
      IF( DATAVL ) CALL KPG1_LITRD( SCALE, OFFSET, STATUS )

*  Now produce the key if required.
      IF( KEY ) THEN

*  Select the key zone.
         CALL SGS_SELZ( ZONEK, STATUS )

*  Now produce the key.
         CALL VECKY( 'KEYVEC', VSCALE, AHSIZM, ABS( TYPDAT ), UNITS1,
     :               JUST, STATUS )

*  Switch back to the frame picture.
         CALL AGI_SELP( PICIDF, STATUS )

*  Create a new picture in the graphics database covering the key.
         CALL AGS_SZONE( 'KEY', 'KAPPA_VECPLOT', PICIDK, STATUS )
         IF( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'VECPLOT_DBSK', 'VECPLOT: Error while '//
     :                    'storing the key picture in the graphics '//
     :                    'database.', STATUS )
         END IF

      END IF

*  Arrive here if an error occurs.
 999  CONTINUE

*  Shut down the graphics database.
      CALL AGS_DEASS( 'DEVICE', .TRUE., STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'VECPLOT_ERR',
     :                 'VECPLOT: Error producing a vector map.',
     :                 STATUS )
      END IF

      END
