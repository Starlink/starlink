      SUBROUTINE VECPLOT( STATUS )
*+
*  Name:
*     VECPLOT

*  Purpose:
*     Plots a 2-dimensional vector map.

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
*     within a pair of 2-dimensional NDFs, the first holding the
*     magnitude of the vector quantity at each pixel, and the second
*     holding the corresponding vector orientations.  The number of
*     vectors in the plot is kept to a manageable value by only
*     plotting vectors for pixels on a sparse regular matrix.  The
*     increment (in pixels) between plotted vectors is given by
*     parameter STEP.  Zero orientation may be fixed at any position
*     angle within the plot by specifying an appropriate value for
*     parameter ANGROT.  Each vector may be represented either by an
*     arrow or by a simple line, as selected by parameter VTYPE.
*
*     The plot is situated within the current graphics-database
*     picture, and may reside within optional, annotated and enumerated
*     axes.  An optional, but recommended, key may be drawn to the
*     right of the plot.  It reports the data units if there are any
*     (taken from the NDF associated with parameter NDF1) and gives the
*     scale used for drawing the vectors in data units per centimetre.
*     It also displays a typical vector and the corresponding data
*     value.  The justification of the vector is indicated by a small
*     circle placed at the position of the corresponding pixel centre.
*
*  Usage:
*     vecplot ndf1 ndf2 [comp] [step] [vscale] [vtype] [just] [device]

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
*        It should be in the range 0--360. [0.0]
*     AXES = _LOGICAL (Read)
*        TRUE if labelled and annotated axes are to be drawn around the
*        plot.  The annotations are either the data co-ordinates from
*        the axis components of NDF1, provided these are present and
*        linear and COSYS = "Data"; otherwise pixel co-ordinates are
*        used. [TRUE]
*     BORDER = _LOGICAL (Read)
*        TRUE if a box is to be drawn about the plot.  This is only
*        accessed if no axes are drawn. [TRUE]
*     CLEAR = _LOGICAL (Read)
*        TRUE if the graphics device is to be cleared before display
*        of the array. [TRUE]
*     COMP = LITERAL (Read)
*        The component of NDF1 which is to be used to define the vector
*        magnitudes.  It may be "Data", "Error" or "Variance".  The
*        last two are not available if NDF1 does not contain a VARIANCE
*        component.  The vector orientations are always defined by the
*        "Data" component of NDF2. ["Data"]
*     COSYS = LITERAL (Read)
*        The co-ordinate system to be used.  This can be either "World"
*        or "Data".  "World" makes pixel co-ordinates to appear on axes.
*        If COSYS = "Data" the axis information from NDF1 is used to
*        annotate axes (if it exists).  [Current co-ordinate system]
*     DEVICE = DEVICE (Read)
*        The plotting device. [Current graphics device]
*     FONT = LITERAL (Read)
*        The fount to be used for the line graphics.  It can be either
*        "NCAR" for the NCAR fancy characters and "GKS" for the standard
*        GKS san-serif fount.  The former is intended for hardcopy
*        publication-quality plots, since it is relatively slow; the
*        latter is intended for normal interactive graphics requiring
*        rapid plotting, and it is clearer on small plots.  The
*        suggested default is the current value. ["GKS"]
*     JUST = LITERAL (Read)
*        The justification for each vector; it can take any of the
*        following values:
*
*         "Centre" - the vectors are drawn centred on the
*                    corresponding pixel,
*
*         "Start"  - the vectors are drawn starting at the
*                    corresponding pixel,
*
*         "End"    - the vectors are drawn ending at the corresponding
*                    pixel.

*        ["Centre"]
*     KEY = _LOGICAL (Read)
*        TRUE if a key is to be produced. [TRUE]
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
*        NDF structure containing the 2-dimensional image giving the
*        vector magnitudes.
*     NDF2 = NDF (Read)
*        NDF structure containing the 2-dimensional image giving the
*        vector orientations.  The values are considered to be in units
*        of degrees unless the UNITS component of the NDF has the value
*        "Radians" (case insensitive).  The positive y axis defines
*        zero orientation, and rotation from the x axis to the y axis
*        is considered positive.  The suggested default is the current
*        value.
*     ORDLAB  =  LITERAL (Read)
*        Label for the plot ordinate, in which NCAR fancy founts may be
*        embedded when FONT = "NCAR".  If axis information is present
*        in NDF1 the suggested default is the axis label from NDF1
*        followed by the units, in parentheses.  If an error occurs
*        obtaining the label the suggested default is "Y". []
*     OUTTIC = _LOGICAL (Read)
*        TRUE if the axis tick marks are to appear on the outside of
*        the axes instead of inside.  By default, the tick marks are
*        drawn outside the plotting region to eliminate
*        intersections of ticks with the vectors. [TRUE]
*     PLTITL = LITERAL (Read)
*        The title of the plot.  Up to about 40 characters can be
*        accommodated, and NCAR fancy founts may be embedded when FONT =
*        "NCAR".  If an error occurs obtaining the title, it is
*        defaulted to "VECPLOT map".  [The title from NDF1]
*     PXSIZE = _REAL (Read)
*        The length (x axis) of the plot in metres. [Maximum that can
*        fit in the current picture whilst preserving square pixels]
*     PYSIZE = _REAL (Read)
*        The length (y axis) of the plot in metres. [Maximum that can
*        fit in the current picture whilst preserving square pixels]
*     STEP = _INTEGER (Read)
*        The number of pixels between adjacent displayed vectors (along
*        both axes).  Increasing this value reduces the number of
*        displayed vectors.  The default value gives about 30 vectors 
*        along the longest axis of the plot. []
*     THICK = _REAL (Read)
*        The thickness of the axes and annotations in the plot, where
*        1.0 is the normal thickness.  Currently, this is only available
*        on a few devices.  It must take a value in the range 0.5--5.0.
*        [1.0]
*     VECCOL = LITERAL (Read)
*        The colour for the vectors.  The options are described below.
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
*
*        If the colour is to remain unaltered as the lookup table is
*        manipulated choose an integer between 0 and 15, or a named
*        colour.  The suggested default is the current value. [The
*        current value, but equals "MIN" if there is no current value.]
*     VSCALE = _REAL (Read)
*        The scale to be used for the vectors.  The supplied value
*        should give the data value corresponding to a vector length of
*        one centimetre.  The default makes 5% of all displayed vectors
*        larger than the interval between adjacent vectors. []
*     VTYPE = LITERAL (Read)
*        The type of vector to be plotted; it can take the value "Arrow"
*        or "Line".  Vectors are drawn as arrows or lines accordingly.
*        ["Line"]

*  Examples:
*     vecplot polint polang
*        Produces a vector map on the current graphics device with
*        vector magnitude taken from the NDF called polint and vector
*        orientation taken from NDF polang.  All other settings are
*        defaulted, so for example about 20 vectors are displayed along
*        the longest axis, and a key is plotted.
*     vecplot polint polang angrot=23.4 
*        Produces a vector map in which the primary axis of the vectors
*        (as defined by the value zero in the NDF polang) is at the
*        position angle 23.4 degrees (measured anti-clockwise from the
*        positive y axis) in the displayed map.
*     vecplot stack(,,2) stack(,,1) vtype=arrow just=start nokey
*        Produces a vector map in which the vectors are defined by two
*        planes in the 3-dimensional NDF called stack.  There is no
*        need to copy the two planes into two separate NDFs before
*        running VECPLOT.  Each vector is represented by an arrow,
*        starting at the position of the corresponding pixel.  No key
*        to the vector scale and justification is produced.

*  Notes:
*     -  If the current graphics-database picture is a DATA picture or
*     contains a DATA picture (such as created by the applications
*     DISPLAY, CONTOUR, etc.), then the vector plot is overlaid on top
*     of the existing DATA plot.  In this case any requested annotation
*     and key are drawn outside the DATA picture but within the current
*     picture.  If there is insufficient room within the current
*     picture, then the annotation and/or key may not be drawn.  You
*     are warned if this happens but the application continues.  If no
*     DATA picture can be found within the current picture then you
*     specify the total size of the plot frame using parameters PXSIZE
*     and PYSIZE, but the application itself chooses how to position
*     the vector plot and key within this frame.
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
*     -  The units string in the key may be truncated with an ellipsis
*     if it cannot be accommodated in full at the smallest allowed
*     character height.  Generally the maximum length will be between
*     18 and 27 characters, the exact value depending on the number of
*     digits in the scale factors.

*  Related Applications:
*     KAPPA: CALPOL.

*  Implementation Status:
*     -  Only real data can be processed directly.  Other non-complex
*     numeric data types will undergo a type conversion before the
*     vector plot is drawn.
*     -  Bad pixels and automatic quality masking are supported.

*  Authors:
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-AUG-1993 (DSB):
*        Original version.
*     21-SEP-1993 (DSB):
*        Modified to overlay the vector plot on any existing DATA plot
*        contained within the current picture. 
*     1995 April 12 (MJC):
*        Added Related Applications and Implementation Status, and a
*        further example.  Moved last paragraph of the long Description
*        to Notes.  Made Examples and Usage lowercase.  KEY is no longer
*        a position parameter.  Various tidying and stylistic changes,
*        and typo's corrected.  Called KPG1_GTNDF.  Constrained ANGROT.
*        Used modern-style variable declarations.  Added headings to
*        the commentary.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'NDF_PAR'          ! NDF_ constants
      INCLUDE 'CTM_PAR'          ! CTM_ Colour-Table Management
                                 ! constants
      INCLUDE 'GKS_PAR'          ! GKS constants (GSET)

*  Status:
      INTEGER STATUS

*  External References:
      LOGICAL CHR_SIMLR          ! Two strings equivalent apart from
                                 ! case?

*  Local Constants:
      REAL AHPAR                 ! Arrowhead size normalised to long
      PARAMETER( AHPAR = 0.01 )  ! axis

      REAL  ANCLP1               ! Fraction of the frame zone in which
      REAL  ANCLP2               ! the image will appear when there are
      REAL  ANCLP3               ! axes
      REAL  ANCLP4         
      PARAMETER ( ANCLP1 = 0.19 )
      PARAMETER ( ANCLP2 = 0.95 )
      PARAMETER ( ANCLP3 = 0.15 )
      PARAMETER ( ANCLP4 = 0.91 )

      REAL  ASPKEY               ! Fractional aspect ratio for the key
      PARAMETER ( ASPKEY = 0.4 )

      INTEGER CUNITS             ! Maximum number of characters in units
      PARAMETER( CUNITS = 30 )   ! string

      REAL  DTOR                 ! Degrees to radians conversion factor
      PARAMETER ( DTOR = 1.7453293E-2 )

      INTEGER NDIM               ! Dimensionality of input array
      PARAMETER( NDIM = 2 )

      REAL  NVEC0                ! Default no. of vectors along short
      PARAMETER ( NVEC0 = 30.0 ) ! axis

*  Local Variables:
      CHARACTER * ( 72 ) ABSLAB  ! Label for the abscissa of the plot
      REAL AHSIZE                ! Arrowhead size in world co-ordinates
      REAL AHSIZM                ! Arrowhead size in metres
      REAL ALBND( NDIM )         ! Axis lower bounds
      REAL ANGFAC                ! NDF2 data to radians conversion factor
      REAL ANGROT                ! Angle to add on to NDF2 values
      CHARACTER * ( NDF__SZTYP ) ATYPE ! Numeric type for mapping AXIS
                                 ! centres
      REAL AUBND( NDIM )         ! Axis upper bounds
      LOGICAL AXES               ! Annotated axes to be drawn?
      LOGICAL BORDER             ! Border to be drawn round the plot?
      LOGICAL CLEAR              ! Graphics device to be cleared?
      INTEGER COLI               ! GKS colour index for current SGS pen
      CHARACTER * ( 8 ) COMP     ! Component to be displayed
      CHARACTER * ( 5 ) COSYS    ! Co-ordinate system
      LOGICAL DATAVL             ! Usable data co-ordinates available?
      REAL DEFSCA                ! Default value for VSCALE
      DOUBLE PRECISION DOFSET( NDIM ) ! Axis co-ords at pixel co-ords
                                 ! (0,0)
      DOUBLE PRECISION DSCALE( NDIM ) ! Dimensions of a pixel in axis
                                 ! units
      REAL DXW                   ! Size of a raster unit in x
      REAL DYW                   ! Size of a raster unit in y
      INTEGER EL1                ! Number of elements in the first NDF
      INTEGER EL2                ! Number of elements in the second NDF
      CHARACTER * ( 4 ) FOUNT    ! Fount type
      REAL GRID( 4 )             ! Current AUTOGRAPH grid offsets
      INTEGER I                  ! Loop count
      INTEGER IERR               ! GKS error code
      INTEGER INDF1              ! Identifier for first NDF
      INTEGER INDF2              ! Identifier for second NDF
      INTEGER IPIXX              ! Maximum number of pixel columns   
      INTEGER IPIXY              ! Maximum number of pixel lines
      INTEGER IWKID              ! Current GKS workstation identifier
      CHARACTER * ( 6 ) JUST     ! Vector justification: CENTRE or START
      LOGICAL KEY                ! A key to vector scale to be plotted?
      INTEGER LNTYPE             ! GKS line type
      INTEGER LWIDTH             ! GKS line width
      REAL MAJTIC( 2 )           ! Major tick mark parameters in x and y
      CHARACTER * ( 8 ) MCOMP    ! Component to be mapped
      REAL MINTIC( 2 )           ! Number of minor tick marks in x and y
      INTEGER NCU1               ! No. of characters in units of 1st NDF
      INTEGER NCU2               ! No. of characters in units of 2nd NDF
      INTEGER NINTS              ! Number of colour indices available
      REAL OFFSET( NDIM )        ! Offsets in world-to-data mapping
      CHARACTER * ( 72 ) ORDLAB  ! Label for the ordinate of the plot
      LOGICAL OUTTIC             ! Tick marks to be put outside the box?
      INTEGER PEN                ! Current SGS pen number
      INTEGER PICID0             ! AGI identifier on input
      INTEGER PICIDD             ! AGI identifier for the DATA picture
      INTEGER PICIDF             ! AGI identifier for the frame picture
      INTEGER PICIDK             ! AGI identifier for the KEY picture
      CHARACTER * ( 72 ) PLTITL  ! Title of the plot
      INTEGER PNTR1              ! Pointer to 1st data array
      INTEGER PNTR2              ! Pointer to 2nd data array
      REAL SCALE( NDIM )         ! Scale factors in world-to-data
                                 ! mapping
      INTEGER SDIM1( NDF__MXDIM ) ! Significant dimensions of 1st NDF
      INTEGER SDIM2( NDF__MXDIM ) ! Significant dimensions of 2nd NDF
      INTEGER SLBND( NDIM )      ! Significant lower bounds of plot area
      INTEGER SLBND1( NDIM )     ! Significant lower bounds of 1st NDF
      INTEGER SLBND2( NDIM )     ! Significant lower bounds of 2nd NDF
      INTEGER STEP               ! Pixel increment between vectors
      INTEGER SUBND( NDIM )      ! Significant upper bounds of plot area
      INTEGER SUBND1( NDIM )     ! Significant upper bounds of 1st NDF
      INTEGER SUBND2( NDIM )     ! Significant upper bounds of 2nd NDF
      REAL SXHI                  ! High x bound of data-zone pixel
                                 ! coords
      REAL SXLO                  ! Low x bound of data-zone pixel coords
      REAL SYHI                  ! High y bound of data zone pixel
                                 ! coords
      REAL SYLO                  ! Low y bound of data zone pixel coords
      REAL THICK                 ! The line thickness (standard is 1.0)
      LOGICAL THERE              ! Does the requested object exist?
      REAL TICDEF( 2 )           ! Suggested default axis-tick values
      REAL TYPDAT                ! A typical data value from 1st input NDF
      CHARACTER * ( CUNITS ) UNITS1 ! Units of the data
      CHARACTER * ( CUNITS ) UNITS2 ! Units of the data
      INTEGER VCI                ! Vector colour index
      REAL VSCALE                ! Vector scale, viz. data units per cm
      CHARACTER * ( 5 ) VTYPE    ! Vector type: arrow or line
      REAL X1                    ! Lower x w.c. bound of SGS zone
      REAL X2                    ! Upper x w.c. bound of SGS zone
      REAL XM                    ! DATA zone x size in metres
      REAL Y1                    ! Lower y w.c. bound of SGS zone
      REAL Y2                    ! Upper y w.c. bound of SGS zone
      REAL YM                    ! DATA zone y size in metres
      INTEGER ZONE0              ! Zone id. for current picture on entry
      INTEGER ZONED              ! Zone id. for DATA picture
      INTEGER ZONEF              ! Zone id. for new FRAME picture
      INTEGER ZONEG              ! Zone id. for the NCAR "graph window"
      INTEGER ZONEK              ! Zone id. for the KEY picture

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find which component of the first input NDF is to be used to define
*  the vector magnitudes (the vector orientations are always defined by
*  the DATA component of the second NDF).
      CALL PAR_CHOIC( 'COMP', 'Data', 'Data,Error,Variance', .FALSE.,
     :                COMP, STATUS )

*  Most NDF routines with a component argument don't recognise 'ERROR',
*  so we need two variables.  Thus convert 'ERROR' into 'VARIANCE' in
*  the variable needed for such routines.  The original value is held
*  in a variable with the prefix M for mapping, as one of the few
*  routines that does support 'ERROR' is NDF_MAP; it is also needed for
*  plot annotations using any NDF units.
      MCOMP = COMP
      IF ( COMP .EQ. 'ERROR' ) COMP = 'VARIANCE'

*  Access the first input NDF.
*  ===========================

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain an identifier for the first NDF which defines the vector
*  magnitudes.  Ensure that it has just two significant dimensions
*  (i.e. dimensions spanning more than a single element).  An error
*  is reported otherwise. The indices of the significant dimensions are
*  returned in SDIM1.
      CALL KPG1_GTNDF( 'NDF1', NDIM, .TRUE., 'READ', INDF1, SDIM1,
     :                 SLBND1, SUBND1, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Report an error if the required component does not exist.
      CALL NDF_STATE( INDF1, COMP, THERE, STATUS )
      IF ( .NOT. THERE .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'COMP', COMP )
         CALL NDF_MSG( 'NDF', INDF1 )
         CALL ERR_REP( 'VECPLOT_NOCOMP', 'VECPLOT: ^NDF does not '/
     :                 /'contain a ^COMP component.', STATUS )
         GO TO 999
      END IF

*  Obtain the units of the requested component of the first NDF.
      CALL KPG1_DAUNI( INDF1, MCOMP, UNITS1, NCU1, STATUS )

*  Map the required component as an array of _REAL values.  Note that
*  the component may be 'Error'.
      CALL NDF_MAP( INDF1, MCOMP, '_REAL', 'READ', PNTR1, EL1, STATUS )

*  Access the first input NDF.
*  ===========================

*  Now do just the same for the second NDF which defines the vector
*  orientations.  The DATA component is used.  Access the NDF and its
*  significant bounds.
      CALL KPG1_GTNDF( 'NDF2', NDIM, .TRUE., 'READ', INDF2, SDIM2,
     :                 SLBND2, SUBND2, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain the units of the DATA component of the second NDF.
      CALL KPG1_DAUNI( INDF2, 'Data', UNITS2, NCU2, STATUS )

*  Map the DATA component as an array of _REAL values.
      CALL NDF_MAP( INDF2, 'Data', '_REAL', 'READ', PNTR2, EL2, STATUS )

*  Find the conversion factor for the angle data.
*  ==============================================
*  Set up a factor which converts values stored in the DATA component
*  of the second NDF into units of radians.  If the UNITS component does
*  not have the value "RADIANS" (case insensitive), then assume the
*  data values are in degrees.
      IF ( NCU2 .GT. 0 ) THEN
         IF ( CHR_SIMLR( UNITS2( : NCU2 ), 'RADIANS' ) ) THEN
            ANGFAC = 1.0
         ELSE
            ANGFAC = DTOR
         END IF
      ELSE
         ANGFAC = DTOR
      END IF

*  Find the overlap between the NDFs.
*  ==================================

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
      IF ( ( SUBND( 1 ) .LT. SLBND( 1 ) .OR.
     :       SUBND( 2 ) .LT. SLBND( 2 ) ) .AND.
     :       STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF1', INDF1 )
         CALL NDF_MSG( 'NDF2', INDF2 )
         CALL ERR_REP( 'VECPLOT_NOOVER',
     :                 'VECPLOT: There are no pixels in common '/
     :                 /'between ^NDF1 and ^NDF2.', STATUS )
         GO TO 999
      END IF

*  Store the corresponding pixel co-ordinate bounds.  These go to the
*  outer edges of the pixels, not to the centres.
      SXLO = REAL( SLBND( 1 ) - 1 )
      SXHI = REAL( SUBND( 1 ) )
      SYLO = REAL( SLBND( 2 ) - 1 )
      SYHI = REAL( SUBND( 2 ) )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Find representative data value.
*  ==============================
*  Obtain a "typical" data value from the first input NDF. This will be
*  used to define the default vector scaling.
      CALL KPS1_DTPCL( INDF1, SLBND, SUBND, SDIM1, TYPDAT, STATUS )

*  Define axis system.
*  ===================

*  See if annotated axes are required, and if so, what sort of
*  co-ordinates are to be displayed. Store a blank value for the
*  co-ordinate system if no axes are required.
      CALL PAR_GTD0L( 'AXES', .TRUE., .TRUE., AXES, STATUS )
      IF ( AXES ) THEN
         CALL PAR_CHOIC( 'COSYS', 'Data', 'Data,World', .FALSE., COSYS,
     :                   STATUS )
      ELSE
         COSYS = ' '
      END IF

*  Find the precision needed for the system of axis arrays.
      CALL NDF_ATYPE( INDF1, 'Centre', 0, ATYPE, STATUS )

*  See if there are usable axis co-ordinate structures within the first
*  NDF.  These provide the data co-ordinates which are plotted on the
*  annotated axes (if required), and stored in the graphics database
*  with the newly created data picture.  Axis co-ordinates must be
*  monotonic to be usable.  The coefficients of the linear
*  transformation from world to data co-ordinates are returned.  If no
*  usable data co-ordinates are available, then a unit transformation
*  is returned.  If the axes are non-linear a warning message is issued
*  and a linear approximation to the axis co-ordinates is returned.
*
*  Note that if we were to map a single-precision axis array as double
*  precision, the linearity might be violated merely because of the
*  increased sensitivity of the test.  Thus the testing is done with
*  the appropriate type.  Hereafter though the single-precision
*  transformation is adequate for the job apart from storing the
*  transformation in the AGI database.
      IF ( ATYPE .EQ. '_DOUBLE' ) THEN
         CALL KPG1_CHAXD( INDF1, NDIM, SDIM1, DATAVL, DSCALE, DOFSET,
     :                    STATUS )
         DO I = 1, NDIM
            SCALE( I ) = SNGL( DSCALE( I ) )
            OFFSET( I ) = SNGL( DOFSET( I ) )
         END DO

      ELSE
         CALL KPG1_CHAXR( INDF1, NDIM, SDIM1, DATAVL, SCALE, OFFSET,
     :                    STATUS )
      END IF

*  If axes with data co-ordinates were requested but no usable data
*  co-ordinates are available, use world co-ordinates for the axes
*  instead.
      IF ( COSYS .EQ. 'DATA' .AND. (.NOT. DATAVL ) ) COSYS = 'WORLD'

*  Start the graphics system.
*  ==========================

*  Now we set up the graphics system.  First see whether the current
*  picture is to be cleared before creating the new plot.
      CALL PAR_GTD0L( 'CLEAR', .TRUE., .TRUE., CLEAR, STATUS )

*  Open the graphics database for the selected device, obtaining
*  identifiers for the current AGI picture and SGS zone.
      IF ( CLEAR ) THEN
         CALL AGS_ASSOC( 'DEVICE', 'WRITE', ' ', PICID0, ZONE0, STATUS )
      ELSE
         CALL AGS_ASSOC( 'DEVICE', 'UPDATE', ' ', PICID0, ZONE0,
     :                   STATUS )
      END IF

*  Ensure that plotted lines will be solid. 
      CALL KPG1_SOLIN( STATUS )

*  Define the zones and pictures.
*  ==============================

*  See if a key to the vector magnitude scale is required.
      CALL PAR_GTD0L( 'KEY', .TRUE., .TRUE., KEY, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Create the zones required for the various parts of the plot (FRAME,
*  KEY, DATA and graph window). A picture corresponding to the new FRAME
*  zone is stored in the AGI database. If a DATA picture is contained
*  within the current picture then it is used to define the zone in
*  which to produce the vector plot. In this case, the bounds of the
*  displayed DATA picture (in pixel co-ordinates) are returned in SXLO,
*  etc.  Otherwise, the user is asked for a FRAME picture size and the
*  DATA picture is created within it using the supplied values of SXLO,
*  etc. The current zone is unchanged.
      CALL KPG1_ZOPIC( 'PXSIZE', 'PYSIZE', 'KAPPA_VECPLOT', ASPKEY, 
     :                 SXLO, SXHI, SYLO, SYHI, KEY, AXES, ZONEF, ZONED, 
     :                 ZONEK, ZONEG, PICIDF, XM, YM, STATUS )

*  Obtain the plotting style.
*  ==========================

*  Produce annotated axes if required.
      IF ( AXES ) THEN

*  Obtain the labels for each axis. First deal with axes displaying
*  DATA co-ordinates...
         IF ( COSYS .EQ. 'DATA' ) THEN

*  Get the abscissa and ordinate labels, suggesting the value in the 
*  axis structure of INDF1, if present, as the default.  Otherwise, use
*  "X" and "Y" as defaults.
            CALL KPG1_GAXLB( INDF1, SDIM1( 1 ), 'ABSLAB', 'X', ABSLAB,
     :                       STATUS )
            CALL KPG1_GAXLB( INDF1, SDIM2( 2 ), 'ORDLAB', 'Y', ORDLAB,
     :                       STATUS )

*  Now deal with axes displaying WORLD co-ordinates. Get the labels
*  without reference to any axis structures.  The suggested defaults
*  are 'X' and 'Y'.
         ELSE
            CALL PAR_DEF0C( 'ABSLAB', 'X', STATUS )
            CALL PAR_GET0C( 'ABSLAB', ABSLAB, STATUS )
            CALL PAR_DEF0C( 'ORDLAB', 'Y', STATUS )
            CALL PAR_GET0C( 'ORDLAB', ORDLAB, STATUS )
         END IF

*  Obtain a title for the plot, using the title from the first NDF as
*  the default.  If NDF1 does not have a title, then use "VECPLOT map"
*  as the default.
         CALL KPG1_GNTIT( INDF1, 'PLTITL', 'VECPLOT map', PLTITL,
     :                    STATUS )

*  Get the number of minor ticks.  Negative values for MINTIC causes
*  NCAR to calculate default values.
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

         IF ( FOUNT .EQ. 'GKS ' ) THEN
            CALL AGPWRT( 0.0, 0.0, ' ', 0, 0, 0, -100 )

         ELSE IF ( FOUNT .EQ. 'NCAR' ) THEN
            CALL AGPWRT( 0.0, 0.0, ' ', 0, 0, 0, 100 )
         END IF

*  Set up the graph co-ordinate system and draw the axes.
*  ======================================================

*  Select the zone to be used as the AUTOGRAPH "graph window" (the
*  rectangle which contains labels, axes, grid, etc).
         CALL SGS_SELZ( ZONEG, STATUS )

*  Ensure that the graph window used by AUTOGRAPH corresponds to this
*  zone.
         CALL SNX_AGWV

*  Ensure that the graph window zone has world co-ordinate bounds of
*  [0,1] on each axis.
         CALL SGS_SW( 0.0, 1.0, 0.0, 1.0, STATUS )

*  Get the bounds of the DATA zone within the world co-ordinate system
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

*  Store the bounds of the co-ordinate values (WORLD or DATA) used to
*  annotate the axes.
         IF ( COSYS .EQ. 'DATA' ) THEN
            ALBND( 1 ) = SCALE( 1 ) * SXLO + OFFSET( 1 )
            AUBND( 1 ) = SCALE( 1 ) * SXHI + OFFSET( 1 )
            ALBND( 2 ) = SCALE( 2 ) * SYLO + OFFSET( 2 )
            AUBND( 2 ) = SCALE( 2 ) * SYHI + OFFSET( 2 )

         ELSE IF ( COSYS .EQ. 'WORLD' ) THEN
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

*  Draw a border, if required.
*  ===========================
*  
*  See if a border is to be produced.
         CALL PAR_GTD0L( 'BORDER', .TRUE., .TRUE., BORDER, STATUS )

*  If required, find the bounds of the DATA zone within the FRAME
*  co-ordinate system and draw a box around it.  The box is drawn one
*  device unit inside the data box to avoid bits of it being clipped.
         IF ( BORDER ) THEN
            CALL SGS_TPZ( ZONED, SXLO, SYLO, ZONEF, X1, Y1, STATUS )
            CALL SGS_TPZ( ZONED, SXHI, SYHI, ZONEF, X2, Y2, STATUS )
            CALL SGS_SELZ( ZONEF, STATUS )
            CALL SGS_IDUN( DXW, DYW )
            CALL SGS_BOX( X1 + DXW, X2 - DXW, Y1 + DYW, Y2 - DYW )
         END IF

      END IF

*  Obtain the vector-plot characteristics.
*  =======================================

*  Get the angle (in degrees) which is to be added to the values stored
*  in NDF2, and convert to radians.  Do not set a dynamic default.
*  Constrain to 0 to 360 degrees.
      CALL PAR_GDR0R( 'ANGROT', -1.0, 0.0, 360.0, .FALSE., ANGROT,
     :                STATUS )
      ANGROT = ANGROT * DTOR

*  Find the increment between plotted vectors, in pixels, ensuring that
*  it is not zero or negative.  A default value which gives a reasonable
*  number of vectors along the shortest axis is used.
      STEP = MAX( 1, NINT( MAX( SXHI - SXLO, SYHI - SYLO ) / NVEC0 ) )
      CALL PAR_DEF0I( 'STEP', STEP, STATUS )
      CALL PAR_GET0I( 'STEP', STEP, STATUS )
      STEP = MAX( 1, STEP )

*  Establish the default value for the vector scaling factor such that
*  a typical data value corresponds to a vector equal to the increment
*  size given by STEP, and then get a new (positive) value.  If a value
*  of zero is supplied, use the default value.  XM is measured in metres
*  so 100 time converts to centimetres.
      DEFSCA = ABS( TYPDAT * ( SXHI - SXLO ) /
     :              ( 100.0 * XM * REAL( STEP ) ) )
      CALL PAR_DEF0R( 'VSCALE', DEFSCA, STATUS )
      CALL PAR_GET0R( 'VSCALE', VSCALE, STATUS )
      VSCALE = ABS( VSCALE )
      IF ( VSCALE .LE. VAL__SMLR ) VSCALE = DEFSCA

*  Get the type of vectors to be plotted, arrows or lines.
      CALL PAR_CHOIC( 'VTYPE', 'LINE', 'LINE,ARROW', .TRUE., VTYPE,
     :                STATUS )

*  Get the vector justification to be used.
      CALL PAR_CHOIC( 'JUST', 'CENTRE', 'CENTRE,START,END', .TRUE.,
     :                JUST, STATUS )

*  Set up the arrowhead size in units of pixels ("Line" vectors are
*  considered to be "Arrow" vectors with zero sized arrowheads).
      IF ( VTYPE .EQ. 'ARROW' ) THEN
         AHSIZE = AHPAR * MAX( SXHI - SXLO, SYHI - SYLO )
      ELSE
         AHSIZE = 0.0
      END IF

*  Get the arrowhead size in metres.
      AHSIZM = AHSIZE * XM / ( SXHI - SXLO )

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
*  =================
      CALL KPS1_VECPL( SLBND1( 1 ), SUBND1( 1 ), SLBND1( 2 ), 
     :                 SUBND1( 2 ), %VAL( PNTR1 ), SLBND2( 1 ), 
     :                 SUBND2( 1 ), SLBND2( 2 ), SUBND2( 2 ), 
     :                 %VAL( PNTR2 ), ANGFAC, ANGROT, STEP,
     :                 VSCALE, AHSIZE, JUST, STATUS )

*  Flush all graphics.
      CALL SGS_FLUSH

*  Re-instate the original colour index for this pen.
      CALL GSPLR( IWKID, PEN, LNTYPE, LWIDTH, COLI )

*  Record the pictures in the AGI database.
*  ========================================

*  Select the frame picture.
      CALL AGI_SELP( PICIDF, STATUS )

*  Record the data picture and a reference to the first NDF in the
*  database.
      CALL KPG1_SDTRN( 'KAPPA_VECPLOT', INDF1, PICIDD, STATUS )

*  If usable data co-ordinates are available, store a transformation
*  from data co-ordinates to world co-ordinates with the data picture in
*  the graphics database.
      IF ( DATAVL ) THEN
         IF ( ATYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG1_LITRD( DSCALE, DOFSET, STATUS )
         ELSE
            CALL KPG1_LITRR( SCALE, OFFSET, STATUS )
         END IF
      END IF

*  Plot and record the key.
*  ========================

*  Now produce the key if required.
      IF ( KEY ) THEN

*  Select the key zone.
         CALL SGS_SELZ( ZONEK, STATUS )

*  Now produce the key.
         CALL KPS1_VECKY( 'KEYVEC', VSCALE, AHSIZM, ABS( TYPDAT ), 
     :                    UNITS1, JUST, STATUS )

*  Switch back to the frame picture.
         CALL AGI_SELP( PICIDF, STATUS )

*  Create a new picture in the graphics database covering the key.
         CALL AGS_SZONE( 'KEY', 'KAPPA_VECPLOT', PICIDK, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'VECPLOT_DBSK', 'VECPLOT: Error while '/
     :        /'storing the key picture in the graphics database.',
     :        STATUS )
         END IF

      END IF

*  Closedown sequence.
*  ===================

*  Arrive here if an error occurs.
 999  CONTINUE
      
*  Shut down the graphics database.
      CALL AGS_DEASS( 'DEVICE', .TRUE., STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'VECPLOT_ERR',
     :     'VECPLOT: Error producing a vector map.', STATUS )
      END IF

      END
