      SUBROUTINE ZAPLIN( STATUS )
*+
*  Name:
*     ZAPLIN

*  Purpose:
*     Replaces regions in a 2-d NDF by bad values or by linear
*     interpolation.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ZAPLIN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine allows you to mask or clean whole columns or lines,
*     or regions from a 2-dimensional data and variance arrays in an
*     NDF structure.  The cleaned arrays are written to an output NDF.
*     At present the cleaning process is one of the following selection.

*     1. Flagging by substitution of the bad value.
*     2. Bi-linear interpolation across a region, using the nearest
*        non-bad pixels on each of the four sides.
*     3. Linear interpolation across whole lines or columns, using
*        the nearest non-bad pixels to either side.
*     The magic value is also substituted in processes 2. and 3. where
*     interpolation is not possible, such as at the edge of the array.

*     Co-ordinates may either be in pixels or in data co-ordinates.
*     Three methods are available for obtaining the lines or columns
*     or the region:

*     1. From the parameter system, usually in response to prompting.
*     2. By a placing a graphics cursor of a nominated device either
*        side of the defect.  If columns are being zapped then the
*        line position of the cursor is ignored, and vice versa.  To
*        use this mode the data array must already be displayed as an
*        image or contour plot and the picture stored in the graphics
*        database.
*     3. By reading a free-format text file in which each record
*        defines a zapping instruction.  Each record must contain
*        either a) a pair of column or line positions followed by "L" or
*        "C" to indicate whether it is lines or columns being specified
*        respectively; or b) the lower followed by the upper bound of
*        a region (i.e. a pair of x-y positions.)  There may be
*        commentary lines in the file beginning with "#" or "!".  For
*        example,

*           # University of Madrugada  CCD Mark III  defects
*           23  23  L
*           157 158 C
*           40 23 45 25
*           <EOF>

*       would zap line 23, columns 157 and 158, and a region from
*       (40,23) to (45,25).

*     In the first two modes the application loops asking for new
*     columns, lines or regions to zap, until told to quit or it
*     encounters an error.  An output co-ordinate-list file may also be
*     produced; it may be recycled in later processing as the input to
*     the third mode.  In the last mode processing stops when the end
*     of file is found.

*  Usage:
*     zaplin in out [title] colin=? lincol=? columns=? lines=?

*  ADAM Parameters:
*     COLIN =  FILENAME (Read)
*        Name of the text file containing the column and line bounds
*        of areas to be cleaned. It is only used when MODE = "File".
*     COLOUT =  FILENAME (Read)
*        Name of the file to store the areas cleaned.  It has the same
*        format as an input text file.  It may be used as input via
*        parameter COLIN for processing of other NDFs in the same way,
*        without the drudgery of repeating the commands by hand.
*        It is not available if MODE = "File".  If COLOUT is null (!),
*        there will be no logging to an output text file. [!]
*     COLUMNS( 2 ) = _DOUBLE (Read)
*        Columns that define the inclusive bounds of the region to be
*        zapped.  These are given either pixel indices if COSYS =
*        "World" or the NDF has no axis information, or data
*        co-ordinates if COSYS = "Data".  The application constrains
*        the column bounds to be within the bounds of the NDF.  This
*        parameter is only required when MODE = "Interface".
*     COSYS = LITERAL (Read)
*        The co-ordinate system to be used.  This can be either "World"
*        or "Data".  If COSYS = "Data" the input co-ordinates, either
*        in the text file (File mode) or parameter values (Interface
*        mode) are to be expressed in data co-ordinates, otherwise
*        pixel indices (the world co-ordinates) are used.  In all modes
*        the results are written in data co-ordinates.  The data values
*        are converted to and from pixel indices via the NDF's axis
*        values; if there is no axis information within the NDF, world
*        co-ordinates are then used, except in Cursor mode where the
*        transformation, if present, is taken from the last DATA
*        picture in the graphics database.  If COSYS = "World" pixel
*        co-ordinates are used throughout.  [Current co-ordinate
*        system]
*     DEVICE = DEVICE (Read)
*        The graphics device whose the cursor is used to select the
*        columns or lines that are to be zapped.  It is only used
*        when MODE = "Cursor".  [Current graphics device]
*     IN  =  NDF (Read)
*        Input NDF structure containing the 2-dimensional data array to
*        be cleaned.
*     LINCOL  =  LITERAL (Read)
*        The type of area is to be cleaned.  The options are "Lines",
*        "Columns" or a "Region".   "Lines" cleans all the columns
*        between two line limits; likewise "Columns" cleans all the
*        lines between two column limits; "Region" cleans an area
*        given by pairs of column and line limits.  This parameter is
*        not used if MODE = "File".  If it is specified on the command
*        line in interface mode only one zap operation will be
*        performed; otherwise a series of changes may be made until
*        terminated by setting LINCOL to null (!).
*     LINES( 2 ) =  _DOUBLE (Read)
*        Lines that define the inclusive bounds of the region to be
*        zapped.  These are given either pixel indices if COSYS =
*        "World" or the NDF has no axis information, or data
*        co-ordinates if COSYS = "Data".  The application constrains
*        the line bounds to be within the bounds of the NDF.  This
*        parameter is only required when MODE = "Interface".
*     MARK = _LOGICAL (Read)
*        If TRUE, each point selected by the cursor will be marked by a
*        cross when MODE = "Cursor".  [FALSE]
*     MODE  =  LITERAL (Read)
*        The mode by which the bounds of the region to be cleaned
*        are to be obtained.  The options are as follows: "Interface"
*        defines via the parameter system, "Cursor" enables selection
*        by graphics cursor, and "File" reads them from a text file.
*        [Current interaction mode]
*     NOISE  =  _LOGICAL (Read)
*        If NOISE is TRUE random noise is added to each substituted
*        pixel unless ZAPTYPE = "BAD".  The variance of the noise is
*        equal to that of the data variance of the substituted data
*        value.  If the data variance is bad for a pixel, no noise is
*        added to that pixel.  This facility is provided for cosmetic
*        use. [FALSE]
*     OUT  =  NDF (Write)
*        Output NDF structure containing cleaned version of the
*        input data and variance arrays.
*     TITLE  =  LITERAL (Read)
*        Title for the output NDF structure.  A null value (!)
*        propagates the title from the input NDF to the output NDF. [!]
*     ZAPTYPE  =  LITERAL (Read)
*        The type of the cleaning.  The options are "Linear" for linear
*        interpolation across the line or column using the values that
*        abut the pixels to be zapped, or "Bad" for substitution by the
*        bad-pixel value. ["Linear"]

*  Examples:
*     zaplin out=cleaned colout=fudge.dat
*        Assuming the current interaction mode is cursor this will copy
*        the NDF associated with the last DATA picture to an NDF called
*        cleaned, ready to be zapped interactively using the current
*        graphics device.  The cleaning is via linear interpolation.
*        A record of the areas cleaned will be stored in the text file
*        named fudge.dat.
*     zaplin grubby cleaned i cosys=w lincol=r columns=[188,190]
*       lines=[15,16]
*        This zaps a region from pixel (188,15) to (190,16) within the
*        NDF called grubby and stores the result in the NDF called
*        cleaned.  The zapping is via linear interpolation.
*     zaplin grubby(6,,) cleaned i cosys=w lincol=r columns=[188,190]
*        This zaps columns 188 to 190 in the 6th y-z plane region
*        within the NDF called grubby and stores the result in the NDF
*        called cleaned.  The zapping is via linear interpolation.
*     zaplin m42 m42c f colin=aaoccd1.dat zaptype=b
*        This flags with the bad pixel value the regions in the NDF
*        called m42 defined in the text file called aaoccd1.dat, and
*        stores the result in an NDF called m42c.
*     zaplin m42 m42c f colin=aaoccd1.dat noise
*        As above except that linear interpolation plus cosmetic noise
*        are used to replace the areas to be cleaned rather than bad
*        pixels.

*  Notes:
*     -  If there is no variance array in the NDF, the absolute data
*     value is used instead to apply noise.  This variance is not
*     written to the output NDF.
*     -  When using input files care should be taken to ensure that
*     the co-ordinate system used in the file matches that of the NDF
*     in the current co-ordinate system.
*     -  Data co-ordinates are stored and output in single precision
*     except when the axis array is type _DOUBLE or _INTEGER, or
*     in cursor mode when there is no axis information in the NDF.
*     -  If the input NDF is a section of an NDF with a higher
*     dimensionality, the "lines" and "columns" are with respect to the
*     2-dimensional section, and do not necessarily refer to the first
*     and second dimensions of the NDF as a whole.  See the "Examples".

*  Algorithm:
*     -  Find the mode of operation, the type of zapping operation and
*     whether noise is to be added.  Open the log and File-mode text
*     files as required.
*     -  Set up the cursor mode.  Associate the graphics device, obtain
*     picture and zone identifiers, and a locator to the reference data
*     for the last DATA picture within the current picture, find the
*     initial cursor position and whether or not points are to be
*     marked.
*     -  Obtain the NDF via the command line, the graphics database or
*     prompting.  Get its bounds and check the dimensionality.
*     Propagate the input NDF to an output NDF.  Check the whether
*     variance is present.  Map the output data array and variance.
*     See whether the components may have bad pixels.
*     -  Get the type of co-ordinate system, constrained by whether
*     there is an axis structure.
*     -  Map the axes for data co-ordinates in file or interface modes
*     or a logfile is being created.
*     -  Depending on the mode: a) cursor---prepare the cursor; b)
*     environment---define the bounds within which initial values must
*     lie.
*     -  Create variance in workspace when none exists in the NDF.  Use
*     absolute data value.
*     -  There is a long loop for each zap
*        o  Find the type of area to zap (excluding file mode).
*        o  Obtain the zap limits via the required method.  For
*        environment and file modes this may involve a conversion to
*        pixel indices.  Exit the loop when the end of the file is
*        encountered (file), or a null is given in response to a prompt
*        (environment), or the escape trigger is fired (cursor).
*        o  Validate the region limits, continuing in the loop even if
*        there is an error.  Report the chosen bounds.
*        o  Zap the region with the required method, flushing the
*        errors so that a further cleaning operation may be performed.
*        o  For data co-ordinates convert from the pixel (world)
*        co-ordinates used by the zapping routines.
*        o  Report the results and store them in the logfile converting
*        pixel indices to data co-ordinates if required determing the
*        output precision by inquiring the implementation type of the
*        axis centres.
*        o  Flush the errors
*     -  Tidy the graphics database, and the data; close any open text
*     files.

*  Related Applications:
*     KAPPA: ARDMASK, CHPIX, FILLBAD, GLITCH, NOMAGIC, SEGMENT,
*     SETMAGIC; Figaro: CSET, ICSET, NCSET, TIPPEX.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     VARIANCE, LABEL, TITLE, UNITS, WCS and HISTORY components of the
*     input NDF and propagates all extensions.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.
*     -  There could be a false precision in the data co-ordinates
*     when the transformation is obtained from the AGI database.  This
*     only occurs when there is no axis information in the NDF.

*  Implementation Deficiencies:
*     -  Needs more options for removing defects, such as bi-cubic
*     fitting the neighbourhood.
*     -  Immediate updating of a displayed image and rejection.

*  Authors:
*     MJM: Mark McCaughrean (UoE)
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     01-07-1985 (MJM):
*        First implementation, using line/column co-ordinates read from
*        the user directly, rather than by use of an interactive cursor
*        on a display screen.
*     09-12-1985 (MJM):
*        Added Poisson noise option and corrected error checking bug.
*     11-12-1985 (MJM):
*        Fixed bug caused by continuous updating of read only data -
*        changed program order.
*     06-07-1986 (MJM):
*        Tidied and more error checking.
*     1986 Aug 8 (MJC):
*        Added invocation. Renamed ZAPLINSUB to ZPLNSB.  Reordered
*        arguments in ZPLNSB (2nd to 6th). Added status check for
*        the output DATA_ARRAY component and associated indentation.
*     1986 Sep 1 (MJC):
*        Added arguments and deficiencies section to the prologue and
*        tidied. Used CHR_UCASE rather than UPCASE. Referred to lines
*        rather than rows hence some parameter names were changed.
*     1987 Oct 15 (MJC):
*        Reordered tidying and corrected method.
*     1988 Mar 17 (MJC):
*        Referred to `array' rather than `image'.
*     1988 Jun 5 (MJC):
*        More reporting of error context.
*     1989 Jun 13 (MJC):
*        Allow for processing primitive NDFs.
*     1989 Aug  7 (MJC):
*        Passed array dimensions as separate variables to COPY2D and
*        ZPLNSB.
*     1990 Mar 31 (MJC):
*        Added cursor and file modes.
*     1990 Apr 8 (MJC):
*        Added zapping type (i.e. bad option) and the region zapping.
*     1991 May 26 (MJC):
*        NDF version featuring co-ordinate systems.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1992 April 13 (MJC):
*        Made to handle significant dimensions for user-defined
*        sections.
*     1992 November 30 (MJC):
*        Does not use non-monotonic axis centres.
*     1993 December 13 (MJC):
*        Fixed bug in cursor mode where the region bounds were half a
*        pixel too small, i.e. in co-ordinates not pixel indices.
*     1995 September 20 (MJC):
*     	 Changed default of TITLE to null.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! no default typing allowed

*  Global Constants: 
      INCLUDE 'SAE_PAR'        ! Global SSE parameters 
      INCLUDE 'DAT_PAR'        ! Data-system constants
      INCLUDE 'NDF_PAR'        ! NDF definitions
      INCLUDE 'NDF_ERR'        ! NDF error constants
      INCLUDE 'SUBPAR_PAR'     ! SUBPAR constants
      INCLUDE 'PAR_ERR'        ! Parameter-system errors
      INCLUDE 'FIO_ERR'        ! FIO error definitions

*  Status:
      INTEGER STATUS

*  External References:
      EXTERNAL CHR_ISALF
      LOGICAL CHR_ISALF        ! True if a character is alphabetic

*  Local Constants:
      INTEGER NDIM             ! Array dimensionality
      PARAMETER ( NDIM = 2 )   ! 2-dimensional arrays only

      INTEGER NPTS             ! Number of x-y points to be measured
      PARAMETER ( NPTS = 2 )   ! by cursor for defining coloumn/line
                               ! limits

      INTEGER MXCHO            ! Maximum number of choices
      PARAMETER ( MXCHO = 3 )

*  Local Variables:
      INTEGER
     :  AEL( NDIM ),           ! Number of elements in a mapped axis
     :  AXPNTR( NDIM ),        ! Pointers to the mapped axes
     :  DIMS( NDIM ),          ! Dimensions of data and variance array
     :  EL,                    ! Number of elements in mapped array
     :  FD,                    ! File description of input text file
     :  FDO,                   ! File description of output text file
     :  I,                     ! Loop counter
     :  IERR,                  ! Position of first exception during
                               ! variance generation
     :  INDEXE,                ! Column index of the end of a word in
                               ! the buffer read from the input file
     :  INDEXS,                ! Column index of the start a word in
                               ! the buffer read from the input file
     :  LCSTA,                 ! The state of parameter LINCOL
     :  LBND( NDF__MXDIM )     ! Lower bounds of the NDF

      INTEGER
     :  NC,                    ! Character column counter
     :  NCHAR,                 ! Number of characters in buffer read
                               ! from the co-ordinate file
     :  NCO,                   ! Character column counter
     :  NDF,                   ! Input-NDF identifier
     :  NDFO,                  ! Output-NDF identifier
     :  NDIMS,                 ! Actual number of dimensions of the NDF
     :  NERR,                  ! Number of exceptions in variance
                               ! generation
     :  NP,                    ! Number of points obtained by the cursor
     :  PICID,                 ! Input picture identifier
     :  PICIDI,                ! Data image picture identifier
     :  PNTRO( 2 )             ! Pointer to output DATA_ARRAY component

      INTEGER
     :  SDIM( NDF__MXDIM ),    ! Significant dimensions of the NDF
     :  SLBND( NDIM ),         ! Significant lower bounds of the image
     :  SUBND( NDIM ),         ! Significant upper bounds of the image
     :  UBND( NDF__MXDIM ),    ! Upper bounds of the NDF
     :  ZLBND( NDIM ),         ! Lower bounds of the pixels to be
                               ! replaced
     :  ZUBND( NDIM ),         ! Upper bounds of the pixels to be
                               ! replaced
     :  ZONEO,                 ! SGS zone of the displayed image
     :  ZONEOV                 ! SGS zone of the input picture

      CHARACTER*( DAT__SZLOC ) ! Locator for :
     :  LOCI,                  ! Input data structure
     :  VARLOC                 ! Workspace for making variance

      CHARACTER
     :  ATYPE * ( NDF__SZTYP ),! Processing type of the axis centres
     :  BUFFER * 80,           ! Buffer for reading the x-y lists
     :  BUFOUT * 80,           ! Buffer for writing the logfile
     :  CHID( 2 ) * 80,        ! Commentary on use of image-display
                               ! cursor
     :  CHTERM( 2 ) * 80,      ! Commentary on use of terminal cursor
     :  COMP * 13,             ! Array components to process
     :  COSYS * 5,             ! Co-ordinate system
     :  DTYPE * ( NDF__SZFTP ),! Data type for output components
     :  ITYPE * ( NDF__SZTYP ),! Data type for processing
     :  LC * 1,                ! Whether it is line or columns to be
                               ! cleaned (read from the file).
     :  LINCOL * 7,            ! Type of area to zap
     :  LINDEF * 7,            ! Suggested default area to zap
     :  MODE * 10,             ! Mode in which line/column co-ordinates
                               ! are to be obtained
     :  REFNAM * 256,          ! Reference name
     :  ZAP * 10               ! Method of zapping

      REAL
     :  DELTA,                 ! Width of the point markers in cursor
                               ! mode
     :  X1, Y1,                ! World co-ordinates of the lower-left
                               ! corner of the image picture
     :  X2, Y2,                ! World co-ordinates of the upper-right
                               ! corner of the image picture
     :  XIN, YIN,              ! Co-ordinates of the centre of the image
                               ! picture
     :  XM, YM,                ! Size of the image zone
     :  XP( NPTS ), YP( NPTS ) ! Co-ordinates of the points selected by
                               ! a cursor.

      DOUBLE PRECISION
     :  DUMMY( 2 ),            ! Dummy co-ordinates for transforming
     :  INIMAX( NDIM ),        ! Co-ordinate upper bounds
     :  INIMIN( NDIM ),        ! Co-ordinate lower bounds
     :  DXP( NPTS ),           ! X co-ordinates of the points
     :  DYP( NPTS ),           ! Y co-ordinates of the points
     :  POS( 2 * NPTS )        ! Dummy co-ordinates for reading

      LOGICAL                  ! True if :
     :  ANOTHR,                ! User wants to do another zap
     :  BAD( 2 ),              ! Data and variance arays may contain
                               ! bad pixels 
     :  COLFIL,                ! The text input co-ordinate file option
                               ! was selected
     :  COLLOG,                ! The output co-ordinate list file is
                               ! open
     :  CURCHO,                ! Cursor is available with suitable
                               ! number of choices
     :  CURSOR,                ! The cursor option was selected
     :  DACOOR,                ! The NDF contains an axis structure
     :  DATACO,                ! The zapping bounds are given in data
                               ! co-ordinates
     :  DEVCAN,                ! The device parameter is to be
                               ! cancelled
     :  ENVIRO,                ! The environment option was selected
     :  GOTLOC,                ! A locator to the NDF has been obtained
     :  GOTNAM                 ! A reference name of the NDF has been
                               ! obtained

      LOGICAL                  ! True if:
     :  IMGDIS,                ! Device is nominally an image display
     :  LCDEF,                 ! Parameter LINCOL defined on the command
                               ! line
     :  LINE,                  ! Lines to be removed - else columns
     :  MARK,                  ! Mark the point defined by cursor by a
                               ! cross
     :  MONOTO,                ! Axis is monotonic
     :  NOISE,                 ! Poissonian noise to be added
     :  REGION,                ! A region is to be zapped
     :  VARNCE,                ! Variance is present in the NDF
     :  VWORK,                 ! Workspace obtained for variance
     :  XYOK,                  ! The column or line co-ordinates read
                               ! successfully from the file
     :  ZBAD                   ! Flagged values are to be substituted
                               ! into the bad region

*.

*    Check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

      DEVCAN =.FALSE.
      COLLOG = .FALSE.
      GOTLOC = .FALSE.
      GOTNAM = .FALSE.
      VWORK = .FALSE.

*    Find which mode of operation is to be employed.
*    ===============================================

      CALL PAR_CHOIC( 'MODE', 'Interface', 'Interface,Cursor,File',
     :                .TRUE., MODE, STATUS )

*    Abort if the answer is illegal.

      IF ( STATUS .NE. SAI__OK ) GOTO 999

      CURSOR = MODE .EQ. 'CURSOR'
      COLFIL = MODE .EQ. 'FILE'
      ENVIRO = MODE .EQ. 'INTERFACE'

*    Open text files.
*    =================

*    Option to output to an x-y list the selected columns and lines that
*    were processed.  This is not necessary when the input comes from
*    such a list.

      IF ( .NOT. COLFIL ) THEN
         CALL ERR_MARK
         CALL FIO_ASSOC( 'COLOUT', 'WRITE', 'LIST', 80, FDO, STATUS )

*       Null means no logfile is required, and it is handled invisibly.

         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
         ELSE IF ( STATUS .EQ. SAI__OK ) THEN
            COLLOG = .TRUE.
         END IF
         CALL ERR_RLSE

*    Read positions from an x-y list file.

      ELSE
         CALL FIO_ASSOC( 'COLIN', 'READ', 'LIST', 80, FD, STATUS )
      END IF

      IF ( STATUS .NE. SAI__OK ) GOTO 999

      IF ( COLLOG ) CALL MSG_OUT( 'LOG', 'Recording the co-ordinate '/
     :  /'list to $COLOUT.', STATUS )

*    Obtain the characteristics of the substitutions.
*    ================================================

*    Get the type of zap operation to perform.

      CALL PAR_CHOIC( 'ZAPTYPE', 'Linear', 'Linear,Bad', .TRUE., ZAP,
     :                STATUS )
      ZBAD = ZAP .EQ. 'BAD'

      IF ( ENVIRO ) THEN
         CALL NDG_STATE( 'LINCOL', LCSTA, STATUS )
         LCDEF = LCSTA .EQ. SUBPAR__ACTIVE
      END IF

*    Find out whether or not Poissonian noise is to be added.
 
      IF ( .NOT. ZBAD )
     :  CALL PAR_GTD0L( 'NOISE', .FALSE., .TRUE., NOISE, STATUS )

*    Start graphics system.
*    ======================

      IF ( CURSOR ) THEN

*       Associate image display and start database activity.
*       Update access is used to be able to mark the plot to
*       indicate which images have been measured.

         CALL AGS_ASSOC( 'DEVICE', 'UPDATE', ' ', PICID, ZONEOV,
     :                   STATUS )

*       Find the last DATA picture.

         CALL KPG1_AGFND( 'DATA', PICIDI, STATUS )

*       Obtain the SGS zone identifier for the current DATA picture.

         CALL AGS_NZONE( ZONEO, STATUS )

*       Report the name, comment, and label, if one exists, for the
*       current picture.

         CALL KPG1_AGATC( STATUS )

*       Obtain a reference to the NDF.
*       ==============================

         CALL KPG1_AGREF( PICIDI, 'READ', GOTNAM, REFNAM, STATUS )

*       See whether the reference is a name or locator.  The latter should be
*       phased out, but there may be some old databases and software
*       in circulation.

         CALL DAT_VALID( REFNAM, GOTLOC, STATUS )
         IF ( GOTLOC ) LOCI = REFNAM

*       End immediately if there an error.

         IF ( STATUS .NE. SAI__OK ) THEN
            DEVCAN = .TRUE.
            GOTO 960
         END IF

*       Obtain marker attributes.
*       =========================

*       Mark the selected points?

         CALL PAR_GTD0L( 'MARK', .FALSE., .TRUE., MARK, STATUS )

*       Get initial cursor position as the centre of the current
*       picture.

         CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )
         XIN = 0.5 * ( X1 + X2 )
         YIN = 0.5 * ( Y1 + Y2 )
         CALL SGS_SETCU( XIN, YIN )

*       Get the marker height.

         DELTA = 0.005 * MIN( X2 - X1, Y2 - Y1 )
      END IF

*    Obtain the input NDF.
*    =====================

*    Begin an NDF context.

      CALL NDF_BEGIN

*    Obtain the NDF.  If the name is given on the command line
*    it will be used.  If not, the database data reference is used,
*    if there is one.  Otherwise, the user is prompted.

      CALL KPG1_ASREF( 'IN', 'READ', GOTNAM, REFNAM, NDF, STATUS )

*    Find whether or not there are but two significant dimensions and
*    which ones they are.

      CALL KPG1_SGDIM( NDF, NDIM, SDIM, STATUS )

*    Obtain the bounds of the image.  These will be stored in the
*    graphics database once the cell-array is displayed.

      CALL NDF_BOUND( NDF, NDF__MXDIM, LBND, UBND, NDIMS, STATUS )

*    Must have a 2-d.  A bad status will be generated by NDF_BOUND
*    if there are greater than 2 significant dimensions.

      IF ( STATUS .EQ. NDF__XSDIM ) THEN
         CALL ERR_REP( 'ZAPLIN_IVDIM',
     :     'ZAPLIN: NDF ^NDF is not two-dimensional.', STATUS )
         GOTO 940
      END IF

*    Compute the dimensions and the significant bounds.

      DO I = 1, NDIM
         SLBND( I ) = LBND( SDIM( I ) )
         SUBND( I ) = UBND( SDIM( I ) )
         DIMS( I ) = SUBND( I ) - SLBND( I ) + 1
      END DO

*    Create the output NDF.
*    ======================

*    This is initially a copy of the input NDF, which will then be
*    altered.

      CALL NDG_PROPL( NDF, 'WCS,Data,Variance,Quality,Axis,Units', 'OUT',
     :               NDFO, STATUS )

*    The input NDF is no longer required so annul its identifier.

      CALL NDF_ANNUL( NDF, STATUS )

*    Obtain a new title for the output NDF.

      CALL NDF_CINP( 'TITLE', NDFO, 'Title', STATUS )
      
*    Check whether or not bad pixels may be present.

      CALL NDF_BAD( NDFO, 'Data', .FALSE., BAD( 1 ), STATUS )

*    See whether variance exists.

      VARNCE = .FALSE.
      CALL NDF_STATE( NDFO, 'Variance', VARNCE, STATUS )

      IF ( VARNCE ) THEN
         COMP = 'Data,Variance'
      ELSE
         COMP = 'Data'
      END IF

*    This application supports all the non-complex numeric types
*    directly.  Therefore for the given type of the image find in which
*    type it should be processed.

      CALL NDF_MTYPE( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_REAL,'/
     :                /'_DOUBLE', NDFO, NDFO, 'Data', ITYPE, DTYPE,
     :                STATUS )

*    Map the output data array and variance. Check whether or not bad
*    pixels may be present in the variance.

      CALL KPG1_MAP( NDFO, COMP, ITYPE, 'UPDATE', PNTRO, EL, STATUS )

      IF ( VARNCE ) THEN
         CALL NDF_BAD( NDFO, 'Variance', .FALSE., BAD( 2 ), STATUS )
      END IF

*    Get the type of co-ordinates to input and report.
*    =================================================

*    Is there an axis system?

      CALL NDF_STATE( NDFO, 'Axis', DACOOR, STATUS )

*    Obtain the desired co-ordinate system.

      CALL PAR_CHOIC( 'COSYS', 'World', 'Data,World', .FALSE., COSYS,
     :                STATUS )

*    Find the effective co-ordinate system.

      DATACO = DACOOR .AND. COSYS .EQ. 'DATA'

      IF ( STATUS .NE. SAI__OK ) GOTO 940

*    Map axes for data co-ordinates and test monotonicity.
*    =====================================================

      IF ( ( COLFIL .OR. ENVIRO .OR. COLLOG ) .AND. DATACO ) THEN

*       Obtain the axes.  Map in double precision to avoid loss of
*       accuracy where needed, i.e. the separations of pixels in data
*       co-ordinates are much smaller than their absolute values.

         DO  I = 1, NDIM
            CALL NDF_AMAP( NDFO, 'Centre', SDIM( I ), '_DOUBLE', 'READ',
     :                     AXPNTR( I ), AEL( I ), STATUS )

*          Are all the axes monotonic?  Start a new error context so
*          that the error reports concerning a non-monotonic axis may
*          be annulled.  Instead we issue a warning message so that the
*          application can continue by using world co-ordinates.

            CALL ERR_MARK
            CALL KPG1_MONOD( .TRUE., AEL( I ), %VAL( AXPNTR( I ) ),
     :                       MONOTO, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_ANNUL( STATUS )
               MONOTO = .FALSE.
            END IF
            CALL ERR_RLSE

*          Issue the warning.

            IF ( .NOT. MONOTO ) THEN
               CALL MSG_SETI( 'IAXIS', SDIM( I ) )
               CALL MSG_OUT( 'ZAPLIN_NOTMONO',
     :           'ZAPLIN: Axis ^IAXIS is not monotonic.  Will use '/
     :           /'world co-ordinates instead.', STATUS )

*             Reset the co-ordinate system and axis flags.

               DATACO = .FALSE.
               DACOOR = .FALSE.

*             Unmap the axis since we have finished with it.

               CALL NDF_AUNMP( NDF, 'Centre', SDIM( I ), STATUS )

            END IF
         END DO
      END IF

*    Define the bounds within which the initial values must lie.
*    ===========================================================

      IF ( ENVIRO ) THEN

*       Define the bounds within which the array must lie using pixel
*       or data co-ordinates as requested.

         IF ( DATACO ) THEN

*          Get the axis bounds for the data co-ordinates.

            DO  I = 1, NDIM
               CALL KPG1_AXBND( AEL( I ), %VAL( AXPNTR( I ) ),
     :                          INIMIN( I ), INIMAX( I ), STATUS )
            END DO
            
         ELSE

*          Use pixel indices.

            DO  I = 1, NDIM
               INIMIN( I ) = DBLE( SLBND( I ) )
               INIMAX( I ) = DBLE( SUBND( I ) )
            END DO
         END IF

*    Prepare the cursor.
*    ===================

      ELSE IF ( CURSOR ) THEN

*       Create some commentary describing how to specify points
*       either with an image display, or with a graphics terminal.

         CHID( 1 ) = 'To select a point press the left button '/
     :               /'on the mouse or trackerball.'
         CHID( 2 ) = 'To exit press the right button.'

         CHTERM( 1 ) = 'Type the spacebar to select a point.'
         CHTERM( 2 ) = 'Type . to exit.'

*       Set up the cursor for use.

         CALL KPG1_PRCUR( 1, CHTERM, 2, CHID, 2, '12 .', CURCHO,
     :                    IMGDIS, STATUS )

*       If there is no cursor, or some has gone wrong it is time
*       to abort.

         IF ( STATUS .NE. SAI__OK .OR. .NOT. CURCHO ) THEN
            DEVCAN = .TRUE.
            IF ( .NOT. CURCHO ) THEN

*             Report what is happening.

               STATUS = SAI__ERROR
               CALL ERR_REP( 'ZAPLIN_NOCURSOR',
     :           'ZAPLIN: There is no cursor available on the $DEVICE '/
     :           /'workstation.', STATUS )
            ELSE

*             Status is already set.

               CALL ERR_REP( 'ZAPLIN_CURSOR',
     :           'ZAPLIN: Error preparing the cursor.', STATUS )
            END IF
            GOTO 940
         END IF
      END IF

*    Create workspace for variance when none exists in the NDF.
*    ==========================================================

      IF ( .NOT. VARNCE ) THEN
         CALL AIF_GETVM( ITYPE, 1, EL, PNTRO( 2 ), VARLOC, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'ZAPLIN_WSP',
     :        'ZAPLIN: Error obtaining workspace.', STATUS )
         END IF

*       Note that there is workspace locator.

         VWORK = .TRUE.

*       Fill the workspace with variance data of the appropriate type.
*       Variance is taken to be the absolute data value.

         IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL VEC_ABSR( BAD( 1 ), EL, %VAL( PNTRO( 1 ) ),
     :                     %VAL( PNTRO( 2 ) ), IERR, NERR, STATUS )

         ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN
            CALL VEC_ABSB( BAD( 1 ), EL, %VAL( PNTRO( 1 ) ),
     :                     %VAL( PNTRO( 2 ) ), IERR, NERR, STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL VEC_ABSD( BAD( 1 ), EL, %VAL( PNTRO( 1 ) ),
     :                     %VAL( PNTRO( 2 ) ), IERR, NERR, STATUS )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
            CALL VEC_ABSI( BAD( 1 ), EL, %VAL( PNTRO( 1 ) ),
     :                     %VAL( PNTRO( 2 ) ), IERR, NERR, STATUS )

         ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
            CALL VEC_ABSUB( BAD( 1 ), EL, %VAL( PNTRO( 1 ) ),
     :                      %VAL( PNTRO( 2 ) ), IERR, NERR, STATUS )

         ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
            CALL VEC_ABSUW( BAD( 1 ), EL, %VAL( PNTRO( 1 ) ),
     :                      %VAL( PNTRO( 2 ) ), IERR, NERR, STATUS )

         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
            CALL VEC_ABSW( BAD( 1 ), EL, %VAL( PNTRO( 1 ) ),
     :                     %VAL( PNTRO( 2 ) ), IERR, NERR, STATUS )

         END IF
      END IF

*    End of the preliminaries.


*    Start the loop for deglitching.
*    ===============================

*    Initialise switches for the repeat loop.

      ANOTHR  =  .TRUE.
      LINDEF = 'Lines'

      DO WHILE ( ANOTHR .AND. STATUS .EQ. SAI__OK )

*       Obtain the type of area to zap.
*       ===============================

*       The x-y list has its own steering of the columns or lines.

         IF ( .NOT. COLFIL ) THEN

*          Find out whether a range of lines or columns, or a region is
*          to be cleaned.

            CALL MSG_OUT( 'BLANK', ' ', STATUS )
            CALL PAR_CHOIC( 'LINCOL', LINDEF, 'Lines,Columns,Region,'/
     :                      /'Exit', .FALSE., LINCOL, STATUS )
            LINDEF = LINCOL

*          Cancel the line/column switch so that it can be reprompted.

            CALL PAR_CANCL( 'LINCOL', STATUS )

*          Abort if there is an error.  No need to call ERR_MARK and
*          ERR_RLSE because the error can only have arisen in obtaining
*          the type of bounds.

            IF ( STATUS .NE. SAI__OK .OR. LINCOL( 1:1 ) .EQ. 'E' ) THEN
               IF ( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )
               GOTO 940
            END IF

*          Set the LINE logical accordingly.

            REGION = .FALSE.
            IF ( LINCOL( 1:1 ) .EQ. 'R' ) THEN
               REGION = .TRUE.
            ELSE IF ( LINCOL( 1:1 ) .EQ. 'L' ) THEN
               LINE  =  .TRUE.
            ELSE
               LINE  =  .FALSE.
            END IF
         END IF

*       Now get the actual areas to be replaced.

*       Obtain column/line limits via the graphics cursor.
*       ==================================================

*       An exact number of points is required.  Marked points will
*       be erased.  Points should be unconnected, and distinct.

         IF ( CURSOR ) THEN
            NP = 0
            CALL CURPTS( NPTS, .TRUE., MXCHO, MARK, .TRUE., DELTA,
     :                   .FALSE., .FALSE., X1, X2, Y1, Y2, XIN, YIN,
     :                   NP, XP, YP, STATUS )

*          Look out for the abort, i.e. the number of points is zero.

            IF ( NP .GE. NPTS ) THEN

*             Remember where to put the cursor next time.

               XIN = XP( 2 )
               YIN = YP( 2 )
            ELSE
               GOTO 940
            END IF

*       Obtain column/line limits from the text file.
*       ==============================================

         ELSE IF ( COLFIL ) THEN

            CALL ERR_MARK

*          Read from a file.  Note have to watch for comment and
*          blank lines, hence the loop.

            XYOK = .FALSE.

            DO WHILE ( ANOTHR .AND. .NOT. XYOK .AND.
     :                 STATUS .EQ. SAI__OK )

*             Read a line from the steering file.
*             ===================================

               CALL FIO_READ( FD, BUFFER, NCHAR, STATUS )

               IF ( STATUS .EQ. SAI__OK ) THEN

*                Parse the buffer read from the file.
*                ====================================

*                Check for comment or blank line.

                  IF ( BUFFER .EQ. ' ' .OR. BUFFER( 1:1 ) .EQ. '#'
     :                 .OR. BUFFER( 1:1 ) .EQ. '!' ) THEN
                     XYOK = .FALSE.

                  ELSE

*                   Find the third word in the line.

                     INDEXE = -1
                     DO I = 1, 3
                        INDEXS = INDEXE + 1
                        CALL CHR_FIWS( BUFFER, INDEXS, STATUS )
                        INDEXE = INDEXS
                        CALL CHR_FIWE( BUFFER, INDEXE, STATUS )
                     END DO

*                   If it is not numeric then a region has been given
*                   in the form x1,y1,x2,y2, otherwise it is a range
*                   of lines or columns.

                     REGION = .FALSE.
                     IF ( .NOT.
     :                    CHR_ISALF( BUFFER( INDEXS:INDEXS ) ) ) THEN

                        REGION = .TRUE.

*                      Extract the region co-ordinates from the
*                      string.

                        NCO = 1
                        CALL KPG1_FFRD( BUFFER, 4, NCO, POS, STATUS )

                        IF ( STATUS .EQ. SAI__OK ) THEN
                           XYOK = .TRUE.
                           DXP( 1 ) = POS( 1 )
                           DYP( 1 ) = POS( 2 )
                           DXP( 2 ) = POS( 3 )
                           DYP( 2 ) = POS( 4 )
                        END IF
                           
                     ELSE

*                      Extract the line or column co-ordinates from
*                      the string.  Assume they are columns.

                        NCO = 1
                        CALL KPG1_FFRD( BUFFER( :INDEXS-1 ), 2, NCO,
     :                                 DXP, STATUS )

                        IF ( STATUS .EQ. SAI__OK ) XYOK = .TRUE.

*                      We know the column numbers of the line or
*                      column marker.

                        LC = BUFFER( INDEXS:INDEXS )

*                      Find whether it is lines or columns.

                        CALL CHR_RMBLK( LC )
                        CALL CHR_UCASE( LC )
                        IF ( LC .EQ. 'L' ) THEN
                           LINE = .TRUE.

*                         So the x co-ordinates are really y's.

                           DYP( 1 ) = DXP( 1 )
                           DYP( 2 ) = DXP( 2 )
                        ELSE IF ( LC .EQ. 'C' ) THEN
                           LINE = .FALSE.
                        ELSE

*                         Start a new error context.

                           CALL ERR_MARK

*                         Ignore lines with incorrect format and
*                         warn the user immediately.

                           CALL MSG_SETC( 'BUF', BUFFER( 1:INDEXE ) )
                           STATUS = SAI__ERROR
                           CALL ERR_REP( 'ZAPLIN_INVINP',
     :                       'ZAPLIN: Input file contains invalid '/
     :                       /'format. Line was "^BUF".', STATUS )
                           CALL ERR_FLUSH( STATUS )
                           XYOK = .FALSE.

*                         End the error context.

                           CALL ERR_RLSE
                        END IF
                     END IF

*                End of check for comment or blank records in the file.

                  END IF

               ELSE

*                Report error context unless the end-of-file has been
*                reached.

                  IF ( STATUS .NE. FIO__EOF ) THEN
                     CALL ERR_REP( 'ZAPLIN_RDDATA',
     :                 'ZAPLIN: Error reading data '/
     :                 /'record from the file', STATUS )
                     CALL ERR_FLUSH( STATUS )
                  ELSE
                     CALL ERR_ANNUL( STATUS )
                  END IF
                  CALL ERR_RLSE

*                No more data to zap.

                  GOTO 940

*             End of no-error-reading-record-from-file check.

               END IF
            END DO
            CALL ERR_RLSE

*       Obtain column/line limits from the parameter system.
*       ====================================================

         ELSE IF ( ENVIRO ) THEN
            CALL ERR_MARK

*          Find the bounds for the chosen region.

            IF ( REGION .OR. LINE ) THEN

*             Get start and finish line co-ordinates to zap.

               CALL PAR_GDR1D( 'LINES', NDIM, INIMIN( 2 ), INIMIN( 2 ),
     :                         INIMAX( 2 ), .FALSE., DYP, STATUS )
            END IF

            IF ( REGION .OR. .NOT. LINE ) THEN

*             Get start and finish column co-ordinates to zap.

               CALL PAR_GDR1D( 'COLUMNS', NDIM, INIMIN( 1 ),
     :                         INIMIN( 1 ), INIMAX( 1 ), .FALSE., DXP,
     :                         STATUS )
            END IF

*          Null is recognised.

            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )
               ANOTHR = .FALSE.
            END IF
            CALL ERR_RLSE

*       End of section to obtain the line or column limits.

         END IF

*       Abort if requested.

         IF ( STATUS .EQ. PAR__ABORT .OR. .NOT. ANOTHR ) GOTO 940

*       Validate and transform the co-ordinates to pixel indices.
*       =========================================================

         IF ( DATACO .AND. .NOT. CURSOR .AND. STATUS .EQ. SAI__OK ) THEN

*          Convert from data to world co-ordinates.
*          ========================================

            CALL ERR_MARK

*          Convert the x co-ordinates.

            IF ( REGION .OR. .NOT. LINE ) THEN

*             Derive the world equivalent, i.e. pixel index, in situ via
*             dummy variable.

               DUMMY( 1 ) = DXP( 1 )
               DUMMY( 2 ) = DXP( 2 )
               CALL KPG1_AINDD( SLBND( 1 ), SUBND( 1 ),
     :                          %VAL( AXPNTR( 1 ) ), 2, DUMMY,
     :                          DXP, STATUS )

*             Now the co-ordinates are in pixels, single precision is
*             quite adequate since we want to obtain the nearest pixels.
*             If the co-ordinates have been obtained in cursor mode,
*             the co-ordinates are already in these variables.

               XP( 1 ) = REAL( DXP( 1 ) )
               XP( 2 ) = REAL( DXP( 2 ) )
            END IF

*          Convert the y co-ordinates.

            IF ( REGION .OR. LINE ) THEN

*             Derive the world equivalent, i.e. pixel index, in situ via
*             dummy variable.

               DUMMY( 1 ) = DYP( 1 )
               DUMMY( 2 ) = DYP( 2 )
               CALL KPG1_AINDD( SLBND( 2 ), SUBND( 2 ),
     :                          %VAL( AXPNTR( 2 ) ), 2, DUMMY,
     :                          DYP, STATUS )

*             Now the co-ordinates are in pixels, single precision is
*             quite adequate since we want to obtain the nearest pixels.

               YP( 1 ) = REAL( DYP( 1 ) )
               YP( 2 ) = REAL( DYP( 2 ) )
            END IF
            CALL ERR_RLSE

*       Copy the pixel indices obtained from the file or the environment
*       to the common single-precision variables for convenience.

         ELSE IF ( .NOT. CURSOR ) THEN
            XP( 1 ) = REAL( DXP( 1 ) )
            XP( 2 ) = REAL( DXP( 2 ) )
            YP( 1 ) = REAL( DYP( 1 ) )
            YP( 2 ) = REAL( DYP( 2 ) )

*       Convert the pixel co-ordinates obtained from the cursor into
*       pixel indices, so that all the XP, YP will be in the system
*       regardless of the selected mode or co-ordinate system.

         ELSE
            XP( 1 ) = XP( 1 ) + 0.5
            XP( 2 ) = XP( 2 ) + 0.5
            YP( 1 ) = YP( 1 ) + 0.5
            YP( 2 ) = YP( 2 ) + 0.5

         END IF

         IF ( STATUS .EQ. SAI__OK ) THEN

*          Define, validate and report the bounds of the region to zap.
*          ============================================================

*          Define the bounds of the region.

            ZLBND( 1 ) = NINT( MIN( XP( 1 ), XP( 2 ) ) )
            ZUBND( 1 ) = NINT( MAX( XP( 1 ), XP( 2 ) ) )
            ZLBND( 2 ) = NINT( MIN( YP( 1 ), YP( 2 ) ) )
            ZUBND( 2 ) = NINT( MAX( YP( 1 ), YP( 2 ) ) )

*          Constrain the bounds for line or column zapping.

            IF ( LINE .AND. .NOT. REGION ) THEN
               ZLBND( 1 ) = SLBND( 1 )
               ZUBND( 1 ) = SUBND( 1 )
            ELSE IF ( .NOT. LINE .AND. .NOT. REGION ) THEN
               ZLBND( 2 ) = SLBND( 2 )
               ZUBND( 2 ) = SUBND( 2 )
            END IF

*          There is a potential problem if the input text file uses
*          data co-ordinates, and world co-ordinates are selected or
*          the input NDF does not contain axis information, and vice
*          versa.  In fact the user could have completely the wrong NDF
*          or text file and the co-ordinates have no correspondence.

            IF ( COLFIL ) THEN
               IF ( ZUBND( 1 ) .GT. SUBND( 1 ) .OR.
     :              ZLBND( 1 ) .LT. SLBND( 1 ) .OR.
     :              ZUBND( 2 ) .GT. SUBND( 2 ) .OR.
     :              ZLBND( 2 ) .LT. SLBND( 2 ) ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETC( 'INBUF', BUFFER )
                  CALL ERR_REP( 'ZAPLIN_FILBOUNDS',
     :              'ZAPLIN: the bounds (^INBUF) in $COLIN lie beyond '/
     :              /'those of the NDF.', STATUS )
               END IF
            END IF
      
*          Make the reports of the pixel-index bounds.  Bad status is
*          returned when the line/column limits are invalid.

            CALL KPS1_ZPREP( REGION, LINE, ZLBND, ZUBND, SLBND, SUBND,
     :                       STATUS )
         END IF

*       Given no error so far, and a region, or valid lines or columns
*       were selected, we can continue.

*       Zap the region.
*       ===============

         IF ( STATUS .EQ. SAI__OK ) THEN

*          The subroutines do not use origin information, only the
*          displacement within the array.  Therefore, subtract the
*          origins.

            ZLBND( 1 ) = ZLBND( 1 ) - SLBND( 1 ) + 1
            ZUBND( 1 ) = ZUBND( 1 ) - SLBND( 1 ) + 1
            ZLBND( 2 ) = ZLBND( 2 ) - SLBND( 2 ) + 1
            ZUBND( 2 ) = ZUBND( 2 ) - SLBND( 2 ) + 1

*          Call appropriate routines depending on the implementation
*          type.  The routines are strictly 2-d for the time being.

            IF ( ITYPE .EQ. '_REAL' ) THEN
      
               IF ( ZBAD ) THEN

*                Replace the region by magic values.

                  CALL BAD2DR( DIMS( 1 ), DIMS( 2 ), ZLBND, ZUBND,
     :                         %VAL( PNTRO( 1 ) ), STATUS )

               ELSE 

*                Linearly or bi-linearly interpolate across the
*                pixels to be zapped.

                  CALL KPS1_ZPRGR( DIMS( 1 ), DIMS( 2 ), ZLBND, ZUBND,
     :                             NOISE, %VAL( PNTRO( 2 ) ),
     :                             %VAL( PNTRO( 1 ) ), STATUS )
               END IF

            ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN
      
               IF ( ZBAD ) THEN

*                Replace the region by magic values.

                  CALL BAD2DB( DIMS( 1 ), DIMS( 2 ), ZLBND, ZUBND,
     :                         %VAL( PNTRO( 1 ) ), STATUS )

               ELSE 

*                Linearly or bi-linearly interpolate across the
*                pixels to be zapped.

                  CALL KPS1_ZPRGB( DIMS( 1 ), DIMS( 2 ), ZLBND, ZUBND,
     :                             NOISE, %VAL( PNTRO( 2 ) ),
     :                             %VAL( PNTRO( 1 ) ), STATUS )
               END IF

            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
      
               IF ( ZBAD ) THEN

*                Replace the region by magic values.

                  CALL BAD2DD( DIMS( 1 ), DIMS( 2 ), ZLBND, ZUBND,
     :                         %VAL( PNTRO( 1 ) ), STATUS )

               ELSE 

*                Linearly or bi-linearly interpolate across the
*                pixels to be zapped.

                  CALL KPS1_ZPRGD( DIMS( 1 ), DIMS( 2 ), ZLBND, ZUBND,
     :                             NOISE, %VAL( PNTRO( 2 ) ),
     :                             %VAL( PNTRO( 1 ) ), STATUS )
               END IF

            ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
      
               IF ( ZBAD ) THEN

*                Replace the region by magic values.

                  CALL BAD2DI( DIMS( 1 ), DIMS( 2 ), ZLBND, ZUBND,
     :                         %VAL( PNTRO( 1 ) ), STATUS )

               ELSE 

*                Linearly or bi-linearly interpolate across the
*                pixels to be zapped.

                  CALL KPS1_ZPRGI( DIMS( 1 ), DIMS( 2 ), ZLBND, ZUBND,
     :                             NOISE, %VAL( PNTRO( 2 ) ),
     :                             %VAL( PNTRO( 1 ) ), STATUS )
               END IF

            ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
      
               IF ( ZBAD ) THEN

*                Replace the region by magic values.

                  CALL BAD2DUB( DIMS( 1 ), DIMS( 2 ), ZLBND, ZUBND,
     :                          %VAL( PNTRO( 1 ) ), STATUS )

               ELSE 

*                Linearly or bi-linearly interpolate across the
*                pixels to be zapped.

                  CALL KPS1_ZPRGUB( DIMS( 1 ), DIMS( 2 ), ZLBND, ZUBND,
     :                              NOISE, %VAL( PNTRO( 2 ) ),
     :                              %VAL( PNTRO( 1 ) ), STATUS )
               END IF

            ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
      
               IF ( ZBAD ) THEN

*                Replace the region by magic values.

                  CALL BAD2DUW( DIMS( 1 ), DIMS( 2 ), ZLBND, ZUBND,
     :                          %VAL( PNTRO( 1 ) ), STATUS )

               ELSE 

*                Linearly or bi-linearly interpolate across the
*                pixels to be zapped.

                  CALL KPS1_ZPRGUW( DIMS( 1 ), DIMS( 2 ), ZLBND, ZUBND,
     :                              NOISE, %VAL( PNTRO( 2 ) ),
     :                              %VAL( PNTRO( 1 ) ), STATUS )
               END IF

            ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
      
               IF ( ZBAD ) THEN

*                Replace the region by magic values.

                  CALL BAD2DW( DIMS( 1 ), DIMS( 2 ), ZLBND, ZUBND,
     :                         %VAL( PNTRO( 1 ) ), STATUS )

               ELSE 

*                Linearly or bi-linearly interpolate across the
*                pixels to be zapped.

                  CALL KPS1_ZPRGW( DIMS( 1 ), DIMS( 2 ), ZLBND, ZUBND,
     :                             NOISE, %VAL( PNTRO( 2 ) ),
     :                             %VAL( PNTRO( 1 ) ), STATUS )
               END IF

            END IF

*          Record the data for posterity and future processing.
*          ====================================================

            IF ( COLLOG .AND. STATUS .EQ. SAI__OK ) THEN

*             Restore the normalisation of the co-ordinates.  If data
*             co-ordinates are required then convert the zapping bounds
*             from pixel indices.
*             =========================================================

*             Use the AGI data co-ordinate system in cursor mode when
*             there is no axis structure in the NDF.

               IF ( CURSOR .AND. .NOT. DACOOR .AND.
     :              COSYS .EQ. 'DATA' ) THEN

*                Convert the column and line limits.

*                Replace the values in situ.  In this mode the cursor
*                returns world co-ordinates.  Allow for conversion to
*                pixel co-ordinates and rounding errors.

                  DUMMY( 1 ) = DBLE( XP( 1 ) - 0.49 )
                  DUMMY( 2 ) = DBLE( YP( 1 ) - 0.49 )
                  CALL AGI_TWTDD( PICIDI, 1, DUMMY( 1 ), DUMMY( 2 ),
     :                            DXP( 1 ), DYP( 1 ), STATUS )

                  DUMMY( 1 ) = DBLE( XP( 2 ) - 0.49 )
                  DUMMY( 2 ) = DBLE( YP( 2 ) - 0.49 )
                  CALL AGI_TWTDD( PICIDI, 1, DUMMY( 1 ), DUMMY( 2 ),
     :                            DXP( 2 ), DYP( 2 ), STATUS )

*             Treat all other modes requiring data co-ordinates and
*             where there is an NDF axis structure in cursor mode.

               ELSE IF ( DATACO ) THEN

*                The pixel indices are converted to pixel co-ordinates
*                and then to data co-ordinates.  First the columns then
*                the lines.  The deltas of 0.01 allows for rounding
*                errors so that an output text file will zap the same
*                region as an input file.  This is a fudge, because the
*                rounding depends on the axis value and the step size
*                between centres.

                  IF ( REGION .OR. .NOT. LINE ) THEN 
                     DUMMY( 1 ) = DBLE( ZLBND( 1 ) + SLBND( 1 ) - 1 )
     :                            - 0.49
                     DUMMY( 2 ) = DBLE( ZUBND( 1 ) + SLBND( 1 ) - 1 )
     :                            - 0.49
                     CALL KPG1_AXVLD( SLBND( 1 ), SUBND( 1 ),
     :                                %VAL( AXPNTR( 1 ) ), 2, DUMMY,
     :                                DXP, STATUS )
                  END IF

                  IF ( REGION .OR. LINE ) THEN 
                     DUMMY( 1 ) = DBLE( ZLBND( 2 ) + SLBND( 2 ) - 1 )
     :                            - 0.49
                     DUMMY( 2 ) = DBLE( ZUBND( 2 ) + SLBND( 2 ) - 1 )
     :                            - 0.49
                     CALL KPG1_AXVLD( SLBND( 2 ), SUBND( 2 ),
     :                                %VAL( AXPNTR( 2 ) ), 2, DUMMY,
     :                                DYP, STATUS )
                  END IF

*             Use pixel indices.

               ELSE

*                Restore the normalisation of the co-ordinates to the
*                true origin.

                  DXP( 1 ) = DBLE( ZLBND( 1 ) + SLBND( 1 ) - 1 )
                  DYP( 1 ) = DBLE( ZLBND( 2 ) + SLBND( 2 ) - 1 )
                  DXP( 2 ) = DBLE( ZUBND( 1 ) + SLBND( 1 ) - 1 )
                  DYP( 2 ) = DBLE( ZUBND( 2 ) + SLBND( 2 ) - 1 )
               END IF

*             Find the implementation type of the axis structure.

               IF ( DATACO ) THEN
                  CALL KPG1_AXTYP( NDFO, 'Centre', ATYPE, STATUS )
               ELSE
                  ATYPE = '_REAL'
               END IF

*             Create the record for the text file formatting in the
*             appropriate precision.  For AGI transformation the
*             required type is not known so double-precision must be
*             assumed.

               IF ( ( CURSOR .AND. .NOT. DACOOR .AND.
     :              COSYS .EQ. 'DATA' ) .OR. ATYPE .EQ. '_DOUBLE' ) THEN

*                Double precision.

                  CALL KPS1_ZPABD( REGION, LINE, DXP, DYP, BUFOUT, NC,
     :                             STATUS )
               ELSE

*                Single precision therefore convert.

                  XP( 1 ) = REAL( DXP( 1 ) )
                  XP( 2 ) = REAL( DXP( 2 ) )
                  YP( 1 ) = REAL( DYP( 1 ) )
                  YP( 2 ) = REAL( DYP( 2 ) )
                  CALL KPS1_ZPABR( REGION, LINE, XP, YP, BUFOUT, NC,
     :                             STATUS )
               END IF

*             Write the line to the output steering/log file.

               CALL FIO_WRITE( FDO, BUFOUT( :NC ), STATUS )
            END IF

*       End of status check before doing the zap operation.

         END IF

*       Handle errors encountered this time around the loop.
*       ====================================================

*       Find whether there are errors to flush.

         CALL ERR_STAT( STATUS )

*       Flush the errors in order to continue around the loop.

         IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )

*       More zapping?
*       =============

*       Zap area was given on the command line in interface mode so
*       exit.

         IF ( ENVIRO .AND. LCDEF ) ANOTHR = .FALSE.

*       File mode ends when EOF is encountered.  That leaves the
*       interface and cursor modes when another value may be required...

         IF ( .NOT. COLFIL .AND. ANOTHR ) THEN

*          Cancel previous values of the parameters in the main
*          loop to permit new values to be obtained.

            IF ( ENVIRO ) THEN
               IF ( REGION .OR. .NOT. LINE )

     :           CALL PAR_CANCL( 'COLUMNS', STATUS )
               IF ( REGION .OR. LINE )
     :           CALL PAR_CANCL( 'LINES', STATUS )
            END IF
         END IF

*    End of while-no-error-and-another-zap-wanted loop.

      END DO

  940 CONTINUE

*    Tidy NDF system and the workspace.
*    ==================================

      IF ( VWORK ) CALL AIF_ANTMP( VARLOC, STATUS )

*    Tidy up the input data structure.

      IF ( CURSOR ) THEN
         IF ( GOTLOC ) CALL REF_ANNUL( LOCI, STATUS )
         CALL DAT_VALID( LOCI, GOTLOC, STATUS )
         IF ( GOTLOC ) CALL DAT_ANNUL( LOCI, STATUS )
      END IF

*    End the NDF context.

      CALL NDF_END( STATUS )

  960 CONTINUE

*    AGI closedown sequence.
*    =======================

*    Need to tidy up the graphics database before exiting.

      IF ( CURSOR ) CALL AGS_DEASS( 'DEVICE', DEVCAN, STATUS )

  980 CONTINUE

*    Close the text files.
*    ======================

      IF ( COLFIL ) THEN
         CALL FIO_ANNUL( FD, STATUS )
      ELSE IF ( COLLOG ) THEN
         CALL FIO_ANNUL( FDO, STATUS )
      END IF

  999 CONTINUE

*    The End.

      END

