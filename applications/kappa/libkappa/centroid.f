      SUBROUTINE CENTROID ( STATUS )
*+
*  Name:
*     CENTROID

*  Purpose:
*     Finds the centroids of star-like features in an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CENTROID( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine takes an NDF and returns the co-ordinates of the
*     centroids of features in its data array given approximate initial
*     co-ordinates.  A feature is a set of connected pixels which are
*     above or below the surrounding background region.  For example, a
*     feature could be a star or galaxy on the sky.  The centroid is
*     obtained iteratively from computing marginal profiles in a
*     search region about the initial position, and subtracting the
*     background.  The loop is repeated up to a maximum number of
*     iterations, though it normally terminates when a desired accuracy
*     has been achieved.  Typically, for stars better than 0.1 pixel
*     is readily attainable, but the accuracy is affected by noise,
*     non-Gaussian and overlapping features.  The error in the centroid
*     position may be estimated by a Monte-Carlo method using the
*     data variance to generate realisations of the data about the
*     feature.  Each realisation is processed identically to the actual
*     data, and statistics are formed to derive the standard deviations.

*     Positions may be expressed in either pixel or data co-ordinates.
*     Three methods are available for obtaining the initial positions.

*     1. From the parameter system, usually in response to prompting.
*     2. By a placing a graphics cursor of a nominated device on the
*        feature.  To do this the data array must already be
*        displayed as an image, or a contour or line plot, and the
*        picture stored in the graphics database.
*     3. By reading a text file containing a list of co-ordinates
*        in free format, one object per record.  There may be
*        commentary lines in the file beginning with '#' or '!'.

*     In the first two modes the application loops asking for new
*     feature co-ordinates until told to quit or encounters an error.
*     Features within NDFs of dimension 1 to 7 may be located, except
*     in the second method, where the graphics interface limits
*     constrain the NDF to be one- or two-dimensional---the most common
*     cases.

*     The results may optionally be written to a log file that includes
*     details of the input parameters.  This is intended for the human
*     reader.  If the centroid positions are to be fed into another
*     application requiring a list of co-ordinates, e.g. for photometry,
*     a co-ordinate list file may also be written.

*  Usage:
*     centroid ndf [mode] init [search] [maxiter] [maxshift] [toler]

*  ADAM Parameters:
*     CENTRE()  = _DOUBLE (Write)
*        Co-ordinates of the centroid of the feature.  Note
*        that only the last centroid is recorded in this parameter.
*     CERROR = _LOGICAL (Read)
*        If TRUE, errors in the centroid position will be calculated.
*        The input NDF must contain the data's variance in order
*        to compute errors. [FALSE]
*     COIN =  FILENAME (Read)
*        Name of the text file containing the initial co-ordinates
*        of star-like images.   The co-ordinates should be arranged
*        in free-format columns x then y then z etc., one record per
*        image.  The file may contain comment lines with the first
*        character # or !.  (XYlist mode)
*     COOUT =  FILENAME (Read)
*        Name of the text file to contain the centroid co-ordinates
*        of the star-like images.  It is a co-ordinate file which can
*        be used by other applications, and contains the position of
*        one object per record.  If COOUT is null, !, no output
*        co-ordinate file will be created. [!]
*     COSYS = LITERAL (Read)
*        The co-ordinate system to be used.  This can be either "World"
*        or "Data".  If COSYS = "Data" the input co-ordinates and the
*        centroids are to be expressed in data co-ordinates, otherwise
*        pixel (world) co-ordinates are used.  The data co-ordinates
*        are converted to and from pixel indices via the NDF's axis
*        values; if there is no axis information within the NDF, world
*        co-ordinates are then used, except in Cursor mode where the
*        transformation, if present, is taken from the last DATA
*        picture in the graphics database.  If COSYS = "World" pixel
*        co-ordinates are used throughout.  [Current co-ordinate
*        system]
*     DEVICE = DEVICE (Read)
*        The graphics device whose the cursor is used to select the
*        images for which centroids are to be calculated. (Cursor
*        mode) [Current graphics device]
*     INIT()  = _DOUBLE (Read)
*        Guess co-ordinates of the feature to be centroided.  The
*        co-ordinates must lie within the bounds of the NDF.  If
*        the initial co-ordinates are supplied on the command line
*        only one centroid will be found; otherwise the application
*        will ask for further guesses, which may be terminated by
*        supplying the null value (!). (Interface mode)
*     LOGFILE  =  FILENAME (Read)
*        Name of the text file to log the results.  If null, there
*        will be no logging.  Note this is intended for the human reader
*        and is not intended for passing to other applications. [!]
*     MARK = _LOGICAL (Read)
*        If TRUE, the point selected by the cursor will be marked by a
*        cross. (Cursor mode) [FALSE]
*     MAXITER  =  _INTEGER (Read)
*        Maximum number of iterations to be used in the search.  It must
*        be in the range 1--9.  The dynamic default is 3. [9]
*     MAXSHIFT()  =  _REAL (Read)
*        Maximum shift in each dimension allowed between the guess and
*        output positions in pixels.  Each must lie in the range
*        0.0--26.0.  If only a single value is given, then it will be
*        duplicated to all dimensions. The dynamic default is half of
*        SEARCH + 1. [9.0]
*     MODE  =  LITERAL (Read)
*        The mode in which the initial co-ordinates are to be obtained.
*        "Interface" means from the parameter system, "Cursor"
*        enables selection by graphics cursor, and "File" reads
*        them from an text file. [Current interaction mode]
*     NDF = NDF (Read)
*        The NDF structure containing the data array to be analysed.
*     NSIM =  _INTEGER (Read)
*        The number of simulations or realisations using the variance
*        information in order to estimate the error in the centroid
*        position.  The uncertainty in the centroid error decreases
*        as one over the square root of NSIM. The range of acceptable
*        values is 3--10000. [100]
*     POSITIVE  =  _LOGICAL (Read)
*        TRUE, if array features are positive above the background.
*        [TRUE]
*     SEARCH()  =  _INTEGER (Read)
*        Size in pixels of the search box to be used. If only a single
*        value is given, then it will be duplicated to all dimensions
*        so that a square, cube or hypercube region is searched.
*        Each value must be odd and lie in the range 3--51.  [9]
*     TOLER  =  _REAL (Read)
*        Accuracy in pixels required in centroiding.  Iterations will
*        stop when the shift between successive centroid positions
*        is less than the accuracy.  The accuracy must lie in the range
*        0.0--2.0. [0.05]
*     XCEN  =  _DOUBLE (Write)
*         x co-ordinate of the centroid of the star-like feature.  Note 
*         that only the last centroid is recorded in this parameter. 
*         This provided in addition to CENTRE for convenience until ICL
*         permits arrays.
*     YCEN  =  _DOUBLE (Write)
*         y co-ordinate of the centroid of the star-like feature.  Note 
*         that only the last centroid is recorded in this parameter.
*         This provided in addition to CENTRE for convenience until ICL
*         permits arrays.

*  Examples:
*     centroid cluster cu
*        This finds the centroids in the NDF called cluster via the
*        graphics cursor on the current graphics device.
*     centroid cluster cu search=21
*        This finds the centroids in the NDF called cluster via the
*        graphics cursor on the current graphics device.  The search
*        box for the centroid is 21 pixels in each dimension.
*     centroid cluster i [21.7,5007.1] cosys=d
*        This finds the centroid of the object in the 2-dimensional NDF
*        called cluster around the data co-ordinate (21.7,5007.1).
*     centroid arp244(6,,) i [40,30] cosys=w toler=0.01
*        This finds the 2-dimensional centroid of the feature near
*        pixel (6,40,30) in the 3-dimensional NDF called arp244 via the
*        graphics cursor on the current graphics device.  The centroid
*        must be found to 0.01 pixels.
*     centroid cluster i [40,30] cosys=w xcen=(xp) ycen=(yp)
*        This finds the centroid of the object in the 2-dimensional NDF
*        called cluster around the pixel co-ordinate (40.0,30.0).  The
*        centroid co-ordinates are written to ICL variables XP and YP
*        for use in other applications.
*     centroid cluster mode=xy coin=objects.dat logfile=centroids.log
*        This finds the centroids in the NDF called cluster.  The
*        initial positions are given in the text file objects.dat in
*        the current co-ordinate system.  A log of the input parameter
*        values, initial and centroid positions is written to the text
*        file centroids.log.
*     centroid cluster mode=xy coin=objects.dat coout=centres.dat
*        As above, except there is no logfile; instead a co-ordinate
*        file called centres.dat is generated. 
*     centroid cluster i [40,30] cosys=w nopositive
*        This finds the centroid of the object in the 2-dimensional NDF
*        called cluster around the pixel co-ordinate (40.0,30.0).  The
*        feature must have values below that of the surrounding
*        background.

*  Algorithm:
*     -  Obtain the logfile if required.  Find the mode of operation
*     and the type of co-ordinate system.
*     -  Set up the different modes a) xyfile---open the file; b)
*     cursor---associate the graphics device, obtain picture and zone
*     identifiers, and a locator to the reference data for the last
*     DATA picture within the current picture, find the initial cursor
*     position and whether or not points are to be marked; c)
*     environment---find out whether the initial values are given on
*     the command line.
*     -  Obtain the NDF via the command line, the graphics database or
*     prompting.  Get its bounds and check the dimensionality.  Map the
*     data array.  Record the name the NDF in the log file.
*     -  Depending on the mode: a) cursor---prepare the cursor; b)
*     environment---define the bounds within which initial values must
*     lie.
*     -  Get the relevant parameters for centroiding.  Padding out to
*     the required number of dimensions when one search or max. shift
*     is given.  Record whatis going on in the log file.
*     -  Get workspace if errors are to be estimated.
*     -  There is a long loop for each centroid value 
*        o  Obtain the initial centroid co-ordinates via the required
*        method.  For environment and xyfile modes this may involve
*        a conversion to pixel co-ordinates.  Exit the loop when the
*        end of the file is encountered (xyfile), or a null is given in
*        response to a prompt (environment), or the escape trigger is
*        fired (cursor).
*        o  Evaluate the centroid, flushing the errors so that a further
*        centroid may be evaluated.
*        o  If errors are required, obtain a section of the NDF about
*        the centroid position of dimensions equal to the search sizes.
*        Initialise a summation of the deviations from the nominal
*        centroid in each dimension. For each simulation copy the
*        section to he workspace, add noise to the work array, find the
*        new centroid, and update the statistics. Find the standard
*        deviations .
*        o  For data co-ordinates convert from the pixel (world)
*        co-ordinates used by the centroid-finding routine.
*        o  Report the results and store them in the logfile.  For 2-d
*        the data are arranged in headed columns for consistency with
*        earlier versions.
*     -  Write the centroid position to the output parameters.
*     -  Tidy the graphics database, and the data; close any open text
*     files.

*  Implementation Status:
*     -  The processing of bad pixels and all non-complex numeric types
*     is supported.
*     -  Data co-ordinates are processed in double precision.  When data
*     co-ordinates are being used, and the cursor mode is selected or
*     the NDF contains axis information, the results appear in double
*     precision.  Double precision arithmetic is needed to prevent
*     a loss of precision for certain co-ordinate transformations stored
*     within the graphics database or for double-precision axis centres.
*     -  The format of the logfile is different for 2-d from other
*     dimensions for historical reasons.  The 2-d has a tabular layout
*     with headings.  Logging awaits a proper table system to make this
*     consistent.

*  Related Applications:
*     KAPPA: PSF.

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 March 24 (MJC):
*        Original NDF version based on the pre-0.8 version. 
*     1991 July 12 (MJC):
*        Added COOUT file.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1992 April 14 (MJC):
*        Handles arbitrary user-defined sections.
*     1992 November 30 (MJC):
*        Does not use non-monotonic axis centres.
*     5-JUN-1998 (DSB):
*        Report an error if an even value is supplied for SEARCH.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT  NONE           ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! SSE global definitions
      INCLUDE 'DAT_PAR'        ! Data-system constants
      INCLUDE 'FIO_ERR'        ! FIO error definitions
      INCLUDE 'NDF_PAR'        ! NDF definitions
      INCLUDE 'NDF_ERR'        ! NDF error constants
      INCLUDE 'SUBPAR_PAR'     ! SUBPAR constants
      INCLUDE 'PAR_ERR'        ! Parameter-system errors
      INCLUDE 'PRM_PAR'        ! Magic-value definitions

*  Status:
      INTEGER  STATUS

*  Local Constants:
      INTEGER NPTS             ! Number of x-y points to be measured
      PARAMETER ( NPTS = 1 )   ! by cursor for centroiding

      INTEGER MXCHO            ! Maximum number of choices
      PARAMETER ( MXCHO = 3 )  !

*  Local Variables:
      INTEGER
     :  AEL( NDF__MXDIM ),     ! Number of elements in a mapped axis
     :  AXPNTR( NDF__MXDIM ),  ! Pointers to the mapped axes
     :  DIMS( NDF__MXDIM ),    ! Dimensions of NDF
     :  EL,                    ! Number of mapped elements
     :  EPNTR( 2 ),            ! Pointers to the mapped sections of data
                               ! and variance
     :  ERPNTR,                ! Pointer to workspace for simulations
     :  FD,                    ! File description of input co-ord list
     :  FDL,                   ! File description of logfile
     :  FDO,                   ! File description of output co-ord list
     :  I, J,                  ! Loop counters
     :  IERR,                  ! Position of first conversion error
                               ! during copying
     :  ILBND( NDF__MXDIM ),   ! Lower bounds of NDF section about
                               ! initial guess
     :  ISLBND( NDF__MXDIM ),  ! Significant lower bounds of the region
                               ! for error estimation
     :  ISUBND( NDF__MXDIM ),  ! Significant upper bounds of the region
                               ! for error estimation
     :  IUBND( NDF__MXDIM ),   ! Upper bounds of NDF section about
                               ! initial guess
     :  LBND( NDF__MXDIM )     ! Lower bounds of NDF

      INTEGER
     :  MXDIM,                 ! Maximum number of dimensions
     :  MXITER,                ! Maximum number of iterations to be used
     :  NCHAR,                 ! Number of characters in buffer read
                               ! from the x-y file
     :  NC,                    ! Character column counter
     :  NCI,                   ! Character column counter of image names
     :  NCO,                   ! Character column counter
     :  NDIM,                  ! Effective number of dimensions of NDF
     :  NDIMS,                 ! Actual number of dimensions of the NDF
     :  NDF,                   ! NDF identifier
     :  NDFC,                  ! Identifier for input section
     :  NDFER,                 ! NDF identifier for simulation section
     :  NERR,                  ! Number of errors during copying
     :  NP,                    ! Number of points obtained by the cursor
     :  NSIM,                  ! Number of simulations
     :  NSIMOK,                ! Number simulations that found a
                               ! centroid
     :  NVAL                   ! Number of values return from a
                               ! parameter system

      INTEGER
     :  PICID,                 ! Input picture identifier
     :  PICIDI,                ! Data image picture identifier
     :  PNTRI( 1 ),            ! Pointer to input DATA_ARRAY component
     :  SDIM( NDF__MXDIM ),    ! Significant dimensions of the NDF
     :  SEARCH( NDF__MXDIM ),  ! Size of search region to be used
     :  SEL,                   ! Number of mapped elements in the
                               ! simulations section
     :  SLBND( NDF__MXDIM ),   ! Significant lower bounds of the image
     :  STATE,                 ! State of parameter INIT
     :  SUBND( NDF__MXDIM ),   ! Significant upper bounds of the image
     :  UBND( NDF__MXDIM ),    ! Upper bounds of NDF
     :  ZONEO,                 ! SGS zone of the displayed image
     :  ZONEOV                 ! SGS zone of the input picture

      REAL
     :  DELTA,                 ! Width of the point markers in cursor
                               ! mode
     :  EFINAL( NDF__MXDIM ),  ! Output calculated position in
                               ! simulations
     :  MXSHFT( NDF__MXDIM ),  ! Maximum shifts allowed between guess
                               ! and output
     :  PCINIT( NDF__MXDIM ),  ! Input co-ordinate guess position in
                               ! pixels
     :  PFINAL( NDF__MXDIM ),  ! Output calculated position in pixels
     :  TOLER,                 ! Accuracy required in centroiding
     :  X1, Y1,                ! World co-ordinates of the lower-left
                               ! corner of the image picture
     :  X2, Y2,                ! World co-ordinates of the upper-right
                               ! corner of the image picture
     :  XIN, YIN,              ! Co-ordinates of the centre of the image
                               ! picture
     :  XM, YM,                ! Size of the image zone
     :  XP( 1 ), YP ( 1 )      ! Co-ordinates of the point selected by
                               ! a cursor.

      CHARACTER*(DAT__SZLOC)   ! Locator for :
     :  ERLOC,                 ! Workspace for simulaitons
     :  LOCI                   ! Input data structure

      CHARACTER
     :  BUFFER * 132,          ! Buffer for reading the x-y lists
     :  BUFOUT * 132,          ! Buffer for writing the logfile
     :  CHID( 2 ) * 80,        ! Commentary on use of image-display
                               ! cursor
     :  CHTERM( 2 ) * 80,      ! Commentary on use of terminal cursor
     :  COSYS * 5,             ! Co-ordinate system
     :  DATNAM * 100,          ! Name of input IMAGE
     :  DTYPE * ( NDF__SZFTP ),! Data type for output components
     :  ITYPE * ( NDF__SZTYP ),! Data type for processing
     :  MODE * 10,             ! Mode in which initial co-ordinates are
                               ! to be obtained
     :  REFNAM * 256           ! Reference name

      LOGICAL                  ! True if:
     :  ANOTHR,                ! User wants to locate another feature
     :  BAD,                   ! Data array may contain bad pixels
     :  CENFND,                ! Centroid was found
     :  CERROR,                ! Errors to be calculated
     :  COK,                   ! The co-ordinates read successfully
     :  CURCHO,                ! Cursor is available with suitable
                               ! number of choices
     :  CURSOR,                ! The cursor option was selected
     :  DACOOR,                ! The NDF contains an axis structure
     :  DATACO,                ! Initial and centroid positions to be
                               ! given in data co-ordinates
     :  DEVCAN,                ! The device parameter is to be
                               ! cancelled
     :  DPOUT,                 ! Report co-ordinates in double precision
     :  ENVIRO,                ! The environment option was selected
     :  GOTLOC,                ! A locator to the NDF has been obtained
     :  GOTNAM                 ! A reference name of the NDF has been
                               ! obtained

      LOGICAL                  ! True if:
     :  IMGDIS,                ! Device is nominally an image display
     :  INCO,                  ! The input co-ordinate file was opened
                               ! successfully
     :  LOGPOS,                ! A log of the positions is written to
                               ! a text file
     :  MARK,                  ! Mark the point defined by cursor by a
                               ! cross
     :  MONOTO,                ! Axis is monotonic
     :  OUTCO,                 ! The output co-ordinate file was opened
                               ! successfully
     :  POSTVE,                ! Array features are positive in sign
     :  SINGLE,                ! A single centroid is required
     :  XYFILE                 ! The XY file option was selected

      DOUBLE PRECISION
     :  CINIT( NDF__MXDIM ),   ! Input co-ordinate guess position
     :  CINDEF( NDF__MXDIM ),  ! Suggested input co-ordinates
     :  CDINIT( NDF__MXDIM ),  ! Input data-co-ordinate guess position
     :  DUMMY( NDF__MXDIM ),   ! Used to convert from world to data
                               ! co-ordinates
     :  ERROR( NDF__MXDIM ),   ! Output calculated position errors
     :  FINAL( NDF__MXDIM ),   ! Output calculated position
     :  INIMAX( NDF__MXDIM ),  ! Co-ordinate upper bounds
     :  INIMIN( NDF__MXDIM ),  ! Co-ordinate lower bounds
     :  SUMS( NDF__MXDIM )     ! Sums of the squared deviations of the
                               ! centroids in the simulations

*.

*    Check the inherited status.

      IF ( STATUS .NE. SAI__OK ) RETURN

      DEVCAN =.FALSE.
      GOTLOC = .FALSE.
      GOTNAM = .FALSE.
      INCO = .FALSE.

*    Attempt to obtain and open a log file to output the results.  A
*    null value, meaning no logfile is required, is handled invisibly.

      LOGPOS = .FALSE.
      CALL ERR_MARK
      CALL FIO_ASSOC( 'LOGFILE', 'WRITE', 'LIST', 132, FDL, STATUS )

      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
         LOGPOS = .TRUE.
      END IF
      CALL ERR_RLSE
      IF ( LOGPOS ) CALL MSG_OUT( 'LOG', 'Logging to $LOGFILE', STATUS )

*    Attempt to obtain and open an output co-ordinate file.  A null
*    value, meaning no co-ordinate file is required, is handled
*    invisibly.

      OUTCO = .FALSE.
      CALL ERR_MARK
      CALL FIO_ASSOC( 'COOUT', 'WRITE', 'LIST', 132, FDO, STATUS )

      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
         OUTCO = .TRUE.
      END IF
      CALL ERR_RLSE

*    Find which mode of operation is to be employed.

      CALL PAR_CHOIC( 'MODE', 'Interface', 'Interface,Cursor,File',
     :               .TRUE., MODE, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

      CURSOR = MODE .EQ. 'CURSOR'
      XYFILE = MODE .EQ. 'FILE'
      ENVIRO = MODE .EQ. 'INTERFACE'

*    Read the positions from an x-y list file.

      IF ( XYFILE ) THEN
         CALL ERR_MARK
         CALL FIO_ASSOC( 'COIN', 'READ', 'LIST', 132, FD, STATUS )

         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
         ELSE IF ( STATUS .EQ. SAI__OK ) THEN
            INCO = .TRUE.
         END IF
         CALL ERR_RLSE
         IF ( STATUS .NE. SAI__OK ) GOTO 999

*       Maximum number of dimensions permitted in this mode is not 
*       limited to 2-d; it is the maximum possible.

         MXDIM = NDF__MXDIM

*    Cursor mode selected.

      ELSE IF ( CURSOR ) THEN

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
            GOTO 980
         END IF

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

*       Maximum number of dimensions permitted in this mode is 
*       limited to 2-d.

         MXDIM = 2

*    Looping in prompting mode?
*    ==========================

      ELSE IF ( ENVIRO ) THEN

*       Check whether or not the initial co-ordinates are supplied
*       on the command line.  It will be in the active state if it
*       has.  In this case we want to set a flag to indicate that only
*       one centroid is to be determined.

         CALL PAR_STATE( 'INIT', STATE, STATUS )
         IF ( STATE .EQ. SUBPAR__ACTIVE ) THEN
            SINGLE = .TRUE.
         ELSE
            SINGLE = .FALSE.
         END IF

*       Maximum number of dimensions permitted in this mode is not 
*       limited to 2-d; it is the maximum possible.

         MXDIM = NDF__MXDIM
      END IF

*    Obtain the NDF.
*    ===============

*    Begin an NDF context.

      CALL NDF_BEGIN

*    Obtain the NDF.  If the name is given on the command line
*    it will be used.  If not, the database data reference is used,
*    if there is one.  Otherwise, the user is prompted.

      CALL KPG1_ASREF( 'NDF', 'READ', GOTNAM, REFNAM, NDF, STATUS )

*    This application supports all the non-complex numeric types
*    directly.  Therefore for the given type of the image find in which
*    type it should be processed.

      CALL NDF_MTYPE( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_REAL,'/
     :                /'_DOUBLE', NDF, NDF, 'Data', ITYPE, DTYPE,
     :                STATUS )

*    Want to find the number of significant dimensions in non-cursor
*    mode.  In cursor mode there must be two significant dimensions.

      IF ( .NOT. CURSOR .AND. STATUS .EQ. SAI__OK ) THEN
         CALL ERR_MARK

*       Find the number of dimensions.

         CALL NDF_DIM( NDF, NDF__MXDIM, DIMS, NDIMS, STATUS )

*       Find the significant dimensions and which ones they are.

         CALL KPG1_SGDIM( NDF, NDIMS, SDIM, STATUS )

*       An error results if one or more of the dimensions is
*       insignificant, but this does not matter here as the application
*       is n-d for non-cursor mode.  Therefore, ignore an error unless
*       it is the one for no significant dimensions (indicated by the
*       first significant dimension being zero).  In this case the last
*       significant dimension needs to be found.  If current dimension
*       is assume previous is significant unless found otherwise.

         IF ( STATUS .NE. SAI__OK .AND. SDIM( 1 ) .NE. 0 ) THEN
            CALL ERR_ANNUL( STATUS )
            DO I = NDIMS, 1, -1
               IF ( SDIM( I ) .EQ. 0 ) NDIM = I - 1
            END DO

*       All the dimensions are significant.

         ELSE IF ( STATUS .EQ. SAI__OK ) THEN
            NDIM = NDIMS
         END IF

         CALL ERR_RLSE

*       Obtain the bounds of the image.

         CALL NDF_BOUND( NDF, NDF__MXDIM, LBND, UBND, NDIMS, STATUS )

*    Cursor mode so must be a 2-d image.

      ELSE IF ( CURSOR ) THEN

*       Find the significant dimensions and which ones they are.

         CALL KPG1_SGDIM( NDF, MXDIM, SDIM, STATUS )

*       Obtain the bounds of the image.

         CALL NDF_BOUND( NDF, NDF__MXDIM, LBND, UBND, NDIMS, STATUS )
         NDIM = MXDIM
      END IF

*    Note the differnce between NDIM and NDIMS.  NDIMS is used to define
*    sections of the NDF as it is the number of dimensions in the NDF.
*    NDIM is the effective number of dimensions, and is used except in
*    obtaining sections.

*    Set upper insignificant bounds to one.  We have to make a section
*    so that trailing insignificant bounds may be shifted when the
*    user has specified the whole NDF.  This cannot be done for the base
*    NDF.

      CALL NDF_SECT( NDF, NDIMS, LBND, UBND, NDFC, STATUS )
      CALL KPG1_SECSH( NDFC, SDIM( NDIM ), STATUS )

*    Check the dimensionality.  The only difficult case is for
*    cursor mode, which requires a 2-d NDF.  A bad status will be
*    generated by KPG1_SGDIM there are greater than MXDIM significant 
*    dimensions.

      IF ( CURSOR .AND. STATUS .EQ. NDF__XSDIM ) THEN
         CALL NDF_MSG( 'NDF', NDF )
         CALL MSG_SETI( 'NDIMS', NDIMS )
         CALL ERR_REP( 'CENTROID_IVDIM',
     :     'CENTROID: Input NDF, ^NDF, has an unacceptable '/
     :     /'dimensionality of ^NDIMS. Must be 2-d for cursor mode.',
     :     STATUS )
         GOTO 960
      END IF

*    Prevent access violation when the significant dimensions have not
*    be found.

      IF ( STATUS .NE. SAI__OK ) GOTO 960

*    Compute the dimensions.

      DO I = 1, NDIM
         SLBND( I ) = LBND( SDIM( I ) )
         SUBND( I ) = UBND( SDIM( I ) )
         DIMS( I ) = SUBND( I ) - SLBND( I ) + 1
      END DO

*    Check whether or not bad pixels may be present.

      CALL NDF_BAD( NDF, 'Data', .FALSE., BAD, STATUS )

*    Map the input data array.

      CALL NDF_MAP( NDF, 'Data', ITYPE, 'READ', PNTRI, EL, STATUS )

*    Obtain the file name of the NDF and record it in the log.
*    =========================================================

      IF ( LOGPOS ) THEN

*       Get the name in a token and convert it to a string.

         CALL NDF_MSG( 'NDF', NDF )
         CALL MSG_LOAD( 'DUMMY', '^NDF', DATNAM, NCI, STATUS )

*       Form the sentence.

         NC = 0
         BUFFER = ' '
         CALL CHR_PUTC( 'Input NDF is ', BUFFER, NC )
         CALL CHR_PUTC( DATNAM( :NCI ), BUFFER, NC )
         CALL CHR_PUTC( '.', BUFFER, NC )

*       Write the sentence to the log file.

         CALL FIO_WRITE( FDL, BUFFER( :NC ), STATUS )
      END IF

*    Get the type of co-ordinates to input and report.
*    =================================================

*    Is there an axis system?

      CALL NDF_STATE( NDF, 'Axis', DACOOR, STATUS )

*    Obtain the desired co-ordinate system.

      CALL PAR_CHOIC( 'COSYS', 'World', 'Data,World', .FALSE., COSYS,
     :                STATUS )

*    Find the effective co-ordinate system.

      DATACO = DACOOR .AND. COSYS .EQ. 'DATA'

*    Prevents incorrect error messages being added later.

      IF ( STATUS .NE. SAI__OK ) GOTO 960

*    Map axes for data co-ordinates and test monotonicity.
*    =====================================================

      IF ( DATACO ) THEN

*       Obtain the axes.  Map in double precision to avoid loss of
*       accuracy where needed, i.e. the separations of pixels in data
*       co-ordinates are much smaller than their absolute values.
*       For example, small regions of the sky in equatorial co-ordinates
*       or the wavelength from an echelle spectrogram.

         DO  I = 1, NDIM
            CALL NDF_AMAP( NDF, 'Centre', SDIM( I ), '_DOUBLE', 'READ',
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
               CALL MSG_OUT( 'CENTROID_NOTMONO',
     :           'CENTROID: Axis ^IAXIS is not monotonic.  Will use '/
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

*          Use pixel co-ordinates.

            DO  I = 1, NDIM
               INIMIN( I ) = DBLE( SLBND( I ) - 0.5 )
               INIMAX( I ) = DBLE( SUBND( I ) - 0.5 )
            END DO
         END IF

*       Set the default initial positions to be null, and therefore
*       there is no suggested default.

         DO  I = 1, NDIM
            CINDEF( I ) = VAL__BADD
         END DO

      ELSE IF ( CURSOR ) THEN

*       Prepare the cursor mode.
*       ========================

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
               CALL ERR_REP( 'CENTROID_NOCURSOR',
     :           'CENTROID: There is no cursor available on the '/
     :           /'$DEVICE workstation.', STATUS )
            ELSE
               CALL ERR_REP( 'CENTROID_CURSOR',
     :           'CENTROID: Error preparing the cursor.', STATUS )
            END IF
            GOTO 960
         END IF
      END IF

*    Get the relevant parameters for centroiding.
*    ============================================

*    Obtain the search region sizes, duplicating the value if only a
*    single value is given.  Set a default of 9. Each size must be a
*    positive odd number.

      CALL PAR_DEF1I( 'SEARCH', 1, 9, STATUS )
      CALL PAR_GDRVI( 'SEARCH', NDIM, 1, 51, SEARCH, NVAL, STATUS )
      IF ( NVAL .LT. NDIM ) THEN
         DO  I = NVAL + 1, NDIM
            SEARCH( I ) = SEARCH( 1 )
         END DO
      END IF

*    Constrain the search area to be odd and no bigger than the image.

      DO  I = 1, NDIM

         IF( MOD( SEARCH( I ), 2  ) .EQ. 0 .AND. 
     :      STATUS .EQ. SAI__OK ) THEN
            CALL MSG_SETI( 'S', SEARCH( I ) )
            CALL ERR_REP( 'CENTROID_EVN', 'CENTROID: Even value ^S '//
     :                    'supplied for parameter %SEARCH - must '//
     :                    'be odd.', STATUS )
            GO TO 960
         END IF

         SEARCH( I ) = MIN( DIMS( I ), SEARCH( I ) )

      END DO

      CALL PAR_GDR0I( 'MAXITER', 3, 1, 9, .TRUE., MXITER, STATUS )
      CALL PAR_GET0L( 'POSITIVE', POSTVE, STATUS )

*    Obtain the maximum shifts, duplicating the value if only a
*    single value is given.  Set a default of 9. Each size must be a
*    positive odd number.

      CALL PAR_DEF1R( 'MAXSHIFT', 1, REAL( SEARCH( 1 ) ) * 0.5 + 1.0,
     :                STATUS )
      CALL PAR_GDRVR( 'MAXSHIFT', NDIM, 0.0, 26.0, MXSHFT, NVAL,
     :                STATUS )
      IF ( NVAL .LT. NDIM ) THEN
         DO  I = NVAL + 1, NDIM
            MXSHFT( I ) = MXSHFT( 1 )
         END DO
      END IF

*    Constrain the maximum shifts.

      DO  I = 1, NDIM
         MXSHFT( I ) = MIN( MXSHFT( I ), REAL( DIMS( I ) ) )
      END DO

*    Obtain the tolerance of the iterations to find the centroid.

      CALL PAR_GDR0R( 'TOLER', 0.05, 0.0, 2.0, .TRUE., TOLER, STATUS )

*    Determine whether or not errors are to be estimated, and if so
*    how many simulations to perform.  Since a standard deviation is
*    to be calculated the minimum number of simulations is 3.  Check
*    that there is variance present in the NDF.

      CALL NDF_STATE( NDF, 'Variance', CERROR, STATUS )
      IF ( CERROR ) THEN
         CALL PAR_GET0L( 'CERROR', CERROR, STATUS )
         CALL PAR_GDR0I( 'NSIM', 100, 3, 10000, .TRUE., NSIM, STATUS )
      END IF

*    Record what is going on in the log file.
*    ========================================

      IF ( LOGPOS ) THEN

*       Form string for the search areas.

         BUFOUT = 'Search area in pixels = '
         NC = 25
         DO  J = 1, NDIM
            CALL CHR_PUTI( SEARCH( J ), BUFOUT, NC )
            IF ( J. LT. NDIM ) CALL CHR_PUTC( ', ', BUFOUT, NC )
         END DO
         CALL FIO_WRITE( FDL, BUFOUT( :NC ), STATUS )


         WRITE( BUFOUT, '(''Maximum number of iterations = '',I2)' )
     :          MXITER
         CALL FIO_WRITE( FDL, BUFOUT( :33), STATUS )

         IF ( POSTVE ) THEN
            CALL FIO_WRITE( FDL, 'Positive features', STATUS )
         ELSE
            CALL FIO_WRITE( FDL, 'Negative features', STATUS )
         END IF

*       Form string for the search areas.

         BUFOUT = 'Maximum shift in pixels = '
         NC = 27
         DO  J = 1, NDIM
            CALL CHR_PUTR( MXSHFT( J ), BUFOUT, NC )
            IF ( J. LT. NDIM ) CALL CHR_PUTC( ', ', BUFOUT, NC )
         END DO
         CALL FIO_WRITE( FDL, BUFOUT( :NC ), STATUS )


         WRITE( BUFOUT, '(''Tolerance in pixels = '', G11.5)' ) TOLER
         CALL FIO_WRITE( FDL, BUFOUT( :33 ), STATUS )
         CALL FIO_WRITE( FDL, ' ', STATUS )

         IF ( CERROR ) THEN
            WRITE( BUFOUT, '(''Number of simulations = '', I4)' ) NSIM
            CALL FIO_WRITE( FDL, BUFOUT, STATUS )
            CALL FIO_WRITE( FDL, ' ', STATUS )
         END IF

*       Put in some headings for the results to save space in
*       the output file and to enable easier reading by another
*       application.  At present this is limited to 2-d.  Awaiting
*       CHI_ to produce output table.

         IF ( NDIM .EQ. 2 ) THEN
            IF ( CERROR ) THEN
               BUFOUT = '  Initial position       Centroid position '/
     :                  /'           Centroid errors'
               CALL FIO_WRITE( FDL, BUFOUT( :70 ), STATUS )
               BUFOUT = '    x         y             x         y '/
     :                  /'              x              y'
               CALL FIO_WRITE( FDL, BUFOUT( :71 ), STATUS )
            ELSE
               BUFOUT = '  Initial position       Centroid position'
               CALL FIO_WRITE( FDL, BUFOUT( :42 ), STATUS )
               BUFOUT = '    x         y             x         y'
               CALL FIO_WRITE( FDL, BUFOUT( :39 ), STATUS )
            END IF
         END IF
      END IF

*    Get workspace for the simulations.
*    ==================================

      IF ( CERROR ) THEN
         SEL = 1
         DO  I = 1, NDIM
            SEL = SEL * SEARCH( I )
         END DO

*       Work space is needed for the simulations used to calculate the
*       errors.

         CALL AIF_GETVM( ITYPE, 1, SEL, ERPNTR, ERLOC, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'CENTROID_WSP',
     :        'CENTROID: error obtaining the workspace for the '/
     :        /'simulations.', STATUS )
         END IF
      END IF

*    The main loop starts here.
*    ==========================

*    Initialise the logical to say we want to obtain another centroid.

      ANOTHR = .TRUE.

*    Loop whilst the user wants to locate another star-like feature.

      DO WHILE ( ANOTHR .AND. STATUS .EQ. SAI__OK )

*       Obtain the initial-guess co-ordinates.
*       ======================================
*
*       The initial guess co-ordinates are obtained via one of three
*       methods.

         IF ( CURSOR ) THEN

*          An exact number of points is required.  Any marked point
*          will not be erased.

            CALL CURPTS( NPTS, .TRUE., MXCHO, MARK, .FALSE.,
     :                   DELTA, .FALSE., .FALSE., X1, X2, Y1,
     :                   Y2, XIN, YIN, NP, XP, YP, STATUS )

*          Look out for the abort, i.e. the number of points is zero.

            IF ( NP .EQ. 0 ) THEN
               ANOTHR = .FALSE.
            ELSE

*             Remember where the cursor was left. 

               XIN = XP( 1 )
               YIN = YP( 1 )

*             By convention these world co-ordinates will be pixel
*             co-ordinates.  Copy them to the standard array.

               CINIT( 1 ) = DBLE( XP( 1 ) )
               CINIT( 2 ) = DBLE( YP( 1 ) )
            END IF

         ELSE IF ( XYFILE ) THEN
            CALL ERR_MARK

*          Read from a file.  Note have to watch for initial
*          comment lines, hence the loop.

            COK = .FALSE.

            DO WHILE ( ANOTHR .AND. .NOT. COK .AND.
     :                 STATUS .EQ. SAI__OK )

*             Read an initial co-ordinate point.

               CALL FIO_READ( FD, BUFFER, NCHAR, STATUS )

               IF ( STATUS .EQ. SAI__OK ) THEN

*                Parse the buffer read from the file.
*                ====================================

*                Check for comment or blank line.

                  IF ( BUFFER .EQ. ' ' .OR. BUFFER( 1:1 ) .EQ. '#'
     :                 .OR. BUFFER( 1:1 ) .EQ. '!' ) THEN
                     COK = .FALSE.

                  ELSE

*                   Extract the position from the string.

                     NCO = 1
                     CALL KPG1_FFRD( BUFFER, NDIM, NCO, CINIT, STATUS )

                     IF ( STATUS .EQ. SAI__OK ) COK = .TRUE.
                  END IF

               ELSE

*                Report error context unless the end-of-file
*                has been reached.

                  IF ( STATUS .NE. FIO__EOF ) THEN
                     CALL ERR_REP( 'CENTROID_RDDATA',
     :                 'CENTROID: Error reading data record '/
     :                 /'from the file', STATUS )
                     CALL ERR_FLUSH( STATUS )
                  ELSE
                     CALL ERR_ANNUL( STATUS )
                  END IF

                  ANOTHR = .FALSE.

*                No more positions to be read so close the file and
*                record the fact.

                  CALL FIO_ANNUL( FD, STATUS )
                  INCO = .FALSE.

*             End of no-error-reading-record-from-file check.

               END IF
            END DO
            CALL ERR_RLSE

         ELSE

*          Start a new error context to handle a null meaning exit the
*          loop.

            CALL ERR_MARK
      
*          Get the initial guess co-ordinates from the environment.
*          Each value must lie within the co-ordinate bounds of its
*          associated dimension of the array.

            CALL PAR_GRM1D( 'INIT', NDIM, CINDEF, INIMIN, INIMAX,
     :                      .FALSE., CINIT, STATUS )

*          A null status means compute no more centroids.

            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )
               ANOTHR = .FALSE.
            END IF

            CALL ERR_RLSE
         END IF

         IF ( STATUS .EQ. SAI__OK .AND. ANOTHR ) THEN

*          Start a new error context.

            CALL ERR_MARK

*          Convert from data co-ordinates to world co-ordinates for the
*          centroid-finding routine.
*          ============================================================

            IF ( ( XYFILE .OR. ENVIRO ) .AND. DATACO ) THEN

               DO  I = 1, NDIM

*                Save the initial data co-ordinate for the reports. 
*                Derive its world equivalent, i.e. pixel index.

                  CDINIT( I ) = CINIT( I )
                  CALL KPG1_AINDD( SLBND( I ), SUBND( I ),
     :                             %VAL( AXPNTR( I ) ), 1, CDINIT( I ),
     :                             CINIT( I ), STATUS )
               END DO

*          Only the world co-ordinates from the input file have yet to
*          be validated.  Check that each in within range otherwise
*          construct an error message.

            ELSE IF ( XYFILE ) THEN

               DO  I = 1, NDIM
                  IF ( CINIT( I ) .LT. DBLE( SLBND( I ) ) - 0.5D0 .OR.
     :                 CINIT( I ) .GT. DBLE( SUBND( I ) ) - 0.5D0 ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'I', I )
                     CALL MSG_SETD( 'DC', CINIT( I ) )
                     CALL MSG_SETD( 'LB', DBLE( SLBND( I ) ) - 0.5D0 )
                     CALL MSG_SETD( 'UB', DBLE( SUBND( I ) ) - 0.5D0 )
                     CALL ERR_REP( 'CENTROID_FILEWBND',
     :                 'CENTROID: Input co-ordinate ^DC (dimension '/
     :                 /'^I) is out of bounds (^LB to ^UB).', STATUS )
                  END IF
               END DO
            END IF

*          Evaluate the centroid.
*          ======================

*          Now the co-ordinates are in pixels, single precision is
*          quite adequate since we want to obtain the nearest pixels.

            DO  I = 1, NDIM
               PCINIT( I ) = REAL( CINIT( I ) )
            END DO

*          Call the subroutine that does the actual work for the
*          required data type.

            IF ( ITYPE .EQ. '_INTEGER' ) THEN
               CALL KPG1_LOCTI( NDIM, SLBND, SUBND, %VAL( PNTRI( 1 ) ),
     :                          PCINIT, SEARCH, POSTVE, MXSHFT, MXITER,
     :                          TOLER, PFINAL, STATUS )
            ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
               CALL KPG1_LOCTR( NDIM, SLBND, SUBND, %VAL( PNTRI( 1 ) ),
     :                          PCINIT, SEARCH, POSTVE, MXSHFT, MXITER,
     :                          TOLER, PFINAL, STATUS )
            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
               CALL KPG1_LOCTD( NDIM, SLBND, SUBND, %VAL( PNTRI( 1 ) ),
     :                          PCINIT, SEARCH, POSTVE, MXSHFT, MXITER,
     :                          TOLER, PFINAL, STATUS )
            ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
               CALL KPG1_LOCTW( NDIM, SLBND, SUBND, %VAL( PNTRI( 1 ) ),
     :                          PCINIT, SEARCH, POSTVE, MXSHFT, MXITER,
     :                          TOLER, PFINAL, STATUS )
            ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
               CALL KPG1_LOCTUW( NDIM, SLBND, SUBND, %VAL( PNTRI( 1 ) ),
     :                           PCINIT, SEARCH, POSTVE, MXSHFT, MXITER,
     :                           TOLER, PFINAL, STATUS )
            ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN
               CALL KPG1_LOCTB( NDIM, SLBND, SUBND, %VAL( PNTRI( 1 ) ),
     :                          PCINIT, SEARCH, POSTVE, MXSHFT, MXITER,
     :                          TOLER, PFINAL, STATUS )
            ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
               CALL KPG1_LOCTUB( NDIM, SLBND, SUBND, %VAL( PNTRI( 1 ) ),
     :                           PCINIT, SEARCH, POSTVE, MXSHFT, MXITER,
     :                           TOLER, PFINAL, STATUS )
            END IF

*          The errors are not serious.  Usually an image cannot be
*          found and the user should be able to try again, therefore
*          report the error.  To continue the loop the status must be
*          annulled and the error flushed to the user.  The annulling
*          will occur later except for input from an x-y file.  This is
*          because the user will want to know which input values failed
*          to identify a Gaussian image.  Therefore, flush now so the
*          reporting and any logging can be undertaken.

            IF ( STATUS .NE. SAI__OK ) THEN
               IF ( XYFILE ) THEN
                  CALL ERR_FLUSH( STATUS )
               ELSE
                  CALL MSG_OUT( 'BLANK', ' ', STATUS )
                  CALL ERR_REP( 'CENTROID_NOIM',
     :              'CENTROID: No Gaussian feature found near '/
     :              /'the suggested position.', STATUS )
               END IF
               CENFND = .FALSE.

*             Flag the bad centroid in double precision in the
*             standard array.

               DO  I = 1, NDIM
                  FINAL( I ) = VAL__BADD
               END DO

            ELSE
               CENFND = .TRUE.

*             The centroids are in pixel co-ordinates.  Convert to
*             double precision for transformation.

               DO  I = 1, NDIM
                  FINAL( I ) = DBLE( PFINAL( I ) )
               END DO
            END IF

*          Initialise for the simulations to estimate the errors.
*          ======================================================

            IF ( CENFND .AND. CERROR .AND. STATUS .EQ. SAI__OK ) THEN

*             Define the search area bounds.  First just copy all the
*             bounds, then alter the significant ones.  Initialise the
*             number of centroids found and the squared deviations.

               DO  I = 1, NDIM
                  ILBND( I ) = LBND( I )
                  IUBND( I ) = UBND( I )
               END DO
     
               NSIMOK = 0
               DO  I = 1, NDIM
                  ILBND( SDIM( I ) ) = INT( FINAL( I ) + 0.5 ) -
     :                                 SEARCH( I ) / 2
                  IUBND( SDIM( I ) ) = INT( FINAL( I ) + 0.5 ) +
     :                                 SEARCH( I ) / 2
                  ISLBND( I ) = ILBND( SDIM( I ) )
                  ISUBND( I ) = IUBND( SDIM( I ) )
                  SUMS( I ) = 0.0D0
               END DO

*             Start a new NDF context.

               CALL NDF_BEGIN

*             Create a section in which the search with noise is to be
*             undertaken.

               CALL NDF_SECT( NDFC, NDIMS, ILBND, IUBND, NDFER, STATUS )

*             Map the section in the data and variance arrays.

               CALL NDF_MAP( NDFER, 'Data,Variance', ITYPE, 'READ',
     :                       EPNTR, SEL, STATUS )

               IF ( STATUS .EQ. SAI__OK ) THEN

*                Loop for each simulation.

                  DO  I = 1, NSIM

*                   Find the centroid for the data plus random noise.
*                   =================================================

*                   Call appropriate routines depending on the data
*                   type.

                     IF ( ITYPE .EQ. '_INTEGER' ) THEN

*                      Copy the input data section to the workspace.

                        CALL VEC_ITOI( BAD, SEL, %VAL( EPNTR( 1 ) ),
     :                                 %VAL( ERPNTR ), IERR, NERR,
     :                                 STATUS )
      
*                      Add Gaussian noise to the data section.

                        CALL KPG1_NOISI( BAD, SEL, %VAL( EPNTR( 2 ) ),
     :                                   %VAL( ERPNTR ), STATUS )

*                      Find the new centroid after noise applied.

                        CALL KPG1_LOCTI( NDIM, ISLBND, ISUBND,
     :                                   %VAL( ERPNTR ), PFINAL, SEARCH,
     :                                   POSTVE, MXSHFT, MXITER, TOLER,
     :                                   EFINAL, STATUS )

                     ELSE IF ( ITYPE .EQ. '_REAL' ) THEN

*                      Copy the input data section to the workspace.

                        CALL VEC_RTOR( BAD, SEL, %VAL( EPNTR( 1 ) ),
     :                                 %VAL( ERPNTR ), IERR, NERR,
     :                                 STATUS )
      
*                      Add Gaussian noise to the data section.

                        CALL KPG1_NOISR( BAD, SEL, %VAL( EPNTR( 2 ) ),
     :                                   %VAL( ERPNTR ), STATUS )

*                      Find the new centroid after noise applied.

                        CALL KPG1_LOCTR( NDIM, ISLBND, ISUBND,
     :                                   %VAL( ERPNTR ), PFINAL, SEARCH,
     :                                   POSTVE, MXSHFT, MXITER, TOLER,
     :                                   EFINAL, STATUS )

                     ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN

*                      Copy the input data section to the workspace.

                        CALL VEC_RTOD( BAD, SEL, %VAL( EPNTR( 1 ) ),
     :                                 %VAL( ERPNTR ), IERR, NERR,
     :                                 STATUS )
      
*                      Add Gaussian noise to the data section.

                        CALL KPG1_NOISD( BAD, SEL, %VAL( EPNTR( 2 ) ),
     :                                   %VAL( ERPNTR ), STATUS )

*                      Find the new centroid after noise applied.

                        CALL KPG1_LOCTD( NDIM, ISLBND, ISUBND,
     :                                   %VAL( ERPNTR ), PFINAL, SEARCH,
     :                                   POSTVE, MXSHFT, MXITER, TOLER,
     :                                   EFINAL, STATUS )

                     ELSE IF ( ITYPE .EQ. '_WORD' ) THEN

*                      Copy the input data section to the workspace.

                        CALL VEC_RTOW( BAD, SEL, %VAL( EPNTR( 1 ) ),
     :                                 %VAL( ERPNTR ), IERR, NERR,
     :                                 STATUS )
      
*                      Add Gaussian noise to the data section.

                        CALL KPG1_NOISW( BAD, SEL, %VAL( EPNTR( 2 ) ),
     :                                   %VAL( ERPNTR ), STATUS )

*                      Find the new centroid after noise applied.

                        CALL KPG1_LOCTW( NDIM, ISLBND, ISUBND,
     :                                   %VAL( ERPNTR ), PFINAL, SEARCH,
     :                                   POSTVE, MXSHFT, MXITER, TOLER,
     :                                   EFINAL, STATUS )

                     ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN

*                      Copy the input data section to the workspace.

                        CALL VEC_RTOUW( BAD, SEL, %VAL( EPNTR( 1 ) ),
     :                                  %VAL( ERPNTR ), IERR, NERR,
     :                                  STATUS )
      
*                      Add Gaussian noise to the data section.

                        CALL KPG1_NOISUW( BAD, SEL, %VAL( EPNTR( 2 ) ),
     :                                    %VAL( ERPNTR ), STATUS )

*                      Find the new centroid after noise applied.

                        CALL KPG1_LOCTUW( NDIM, ISLBND, ISUBND,
     :                                   %VAL( ERPNTR ), PFINAL, SEARCH,
     :                                   POSTVE, MXSHFT, MXITER, TOLER,
     :                                   EFINAL, STATUS )

                     ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN

*                      Copy the input data section to the workspace.

                        CALL VEC_RTOB( BAD, SEL, %VAL( EPNTR( 1 ) ),
     :                                 %VAL( ERPNTR ), IERR, NERR,
     :                                 STATUS )
      
*                      Add Gaussian noise to the data section.

                        CALL KPG1_NOISB( BAD, SEL, %VAL( EPNTR( 2 ) ),
     :                                   %VAL( ERPNTR ), STATUS )

*                      Find the new centroid after noise applied.

                        CALL KPG1_LOCTB( NDIM, ISLBND, ISUBND,
     :                                   %VAL( ERPNTR ), PFINAL, SEARCH,
     :                                   POSTVE, MXSHFT, MXITER, TOLER,
     :                                   EFINAL, STATUS )

                     ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN

*                      Copy the input data section to the workspace.

                        CALL VEC_RTOUB( BAD, SEL, %VAL( EPNTR( 1 ) ),
     :                                  %VAL( ERPNTR ), IERR, NERR,
     :                                  STATUS )
      
*                      Add Gaussian noise to the data section.

                        CALL KPG1_NOISUB( BAD, SEL, %VAL( EPNTR( 2 ) ),
     :                                    %VAL( ERPNTR ), STATUS )

*                      Find the new centroid after noise applied.

                        CALL KPG1_LOCTUB( NDIM, ISLBND, ISUBND,
     :                                   %VAL( ERPNTR ), PFINAL, SEARCH,
     :                                   POSTVE, MXSHFT, MXITER, TOLER,
     :                                   EFINAL, STATUS )
                     END IF

*                   Update the statistics.
*                   ======================

*                   Check that the centroid was found.

                     IF ( STATUS .EQ. SAI__OK ) THEN

*                      Form the summation of the deviations from the
*                      centroid of the raw data.

                        NSIMOK = NSIMOK + 1
                        DO  J = 1, NDIM
                           SUMS( J ) = SUMS( J ) + ( DBLE( EFINAL( J ) )
     :                                 - DBLE( PFINAL( J ) ) ) ** 2
                        END DO
                     END IF
                  END DO

*                Compute the error in the centroid.
*                ==================================

*                Keep single-precision copy for database transformation.

                  IF ( NSIMOK .GE. 3 ) THEN
                     DO  J = 1, NDIM
                        ERROR( J ) = SQRT( SUMS( J ) / 
     :                               DBLE( NSIMOK - 1 ) )
                     END DO
                  ELSE
                     ERROR( 1 ) = VAL__BADD
                  END IF
               END IF

*             End the NDF context.

               CALL NDF_END( STATUS )
            END IF

*          No error encountered by the centroid-finding routine,
*          therefore output the results.
*          =====================================================

            IF ( STATUS .EQ. SAI__OK ) THEN

*             If data co-ordinates are required then convert the initial
*             and centroid positions from pixel indices.
*             ==========================================================

*             Use the AGI data co-ordinate system in cursor mode when
*             there is no axis structure in the NDF.

               IF ( CURSOR .AND. .NOT. DACOOR .AND.
     :              COSYS .EQ. 'DATA' ) THEN

*                Replace the values in situ.  In this mode the cursor
*                returns world co-ordinates.  CINIT is not known for
*                data obtained via the cursor, hence the conversion
*                to double precision.  First convert the initial
*                values...

                  DUMMY ( 1 ) = DBLE( PCINIT( 1 ) )
                  DUMMY ( 2 ) = DBLE( PCINIT( 2 ) )
                  CALL AGI_TWTDD( PICIDI, 1, DUMMY( 1 ), DUMMY( 2 ),
     :                            CINIT( 1 ), CINIT( 2 ), STATUS )

*                and the centroids. Only transform the centroid and its
*                error if a centroid was actually found.

                  IF ( CENFND ) THEN
                     DUMMY ( 1 ) = FINAL( 1 )
                     DUMMY ( 2 ) = FINAL( 2 )
                     CALL AGI_TWTDD( PICIDI, 1, DUMMY( 1 ), DUMMY( 2 ),
     :                               FINAL( 1 ), FINAL( 2 ), STATUS )

*                   Compute errors in data co-ordinates.  Add the error
*                   to the centroid, compute it data value and subtract
*                   the centroid in data co-ordinates to give the error
*                   also in data co-ordinates.  This assumes a linear
*                   transformation.  Note the dummy is updated because
*                   the centroid is in data co-ordinates.

                     IF ( CERROR .AND. ERROR( 1 ) .NE. VAL__BADD ) THEN
                        DUMMY( 1 ) = DUMMY( 1 ) + ERROR( 1 )
                        DUMMY( 2 ) = DUMMY( 2 ) + ERROR( 2 )
                        CALL AGI_TWTDD( PICIDI, 1, DUMMY( 1 ),
     :                                  DUMMY( 2 ), ERROR( 1 ),
     :                                  ERROR( 2 ), STATUS )
                        ERROR( 1 ) = ABS( ERROR( 1 ) - FINAL( 1 ) )
                        ERROR( 2 ) = ABS( ERROR( 2 ) - FINAL( 2 ) )
                     END IF
                  END IF

*             Treat all other modes requiring data co-ordinates and
*             where there is an NDF axis structure in cursor mode.

               ELSE IF ( DATACO ) THEN

                  CALL ERR_MARK

                  DO  J = 1, NDIM

*                   The initial values are known except in cursor mode,
*                   therefore these merely need be copied.  In cursor
*                   mode the pixel co-ordinates must be converted to
*                   data co-ordinates.

                     IF ( CURSOR ) THEN
                        DUMMY( J ) = DBLE( PCINIT( J ) )
                        CALL KPG1_AXVLD( SLBND( J ), SUBND( J ),
     :                                   %VAL( AXPNTR( J ) ), 1,
     :                                   DUMMY( J ), CINIT( J ),
     :                                   STATUS )
                     ELSE
                        CINIT( J ) = CDINIT( J )
                     END IF
                  END DO

*                Only transform the centroid and its error if a centroid
*                was actually found.

                  IF ( CENFND ) THEN
                     DO  J = 1, NDIM

*                      The final values are converted to data
*                      co-ordinates.

                        DUMMY( J ) = FINAL( J )
                        CALL KPG1_AXVLD( SLBND( J ), SUBND( J ),
     :                                   %VAL( AXPNTR( J ) ), 1,
     :                                   DUMMY( J ), FINAL( J ),
     :                                   STATUS )

*                      Compute errors in data co-ordinates.  Add the
*                      error to the centroid, compute it data value and
*                      subtract the centroid in data co-ordinates to
*                      give the error also in data co-ordinates.  This
*                      assumes a linear transformation.

                        IF ( CERROR .AND.
     :                       ERROR( 1 ) .NE. VAL__BADD ) THEN
                           DUMMY( J ) = DUMMY( J ) + ERROR( J )
                           CALL KPG1_AXVLD( SLBND( J ), SUBND( J ),
     :                                      %VAL( AXPNTR( J ) ), 1,
     :                                      DUMMY( J ), ERROR( J ),
     :                                      STATUS )
                           ERROR( J ) = ABS( ERROR( J ) - FINAL( J ) )
                        END IF
                     END DO
                  END IF

*                Flush an error as we are in a loop.

                  IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
                  CALL ERR_RLSE
               END IF

*             Write to the co-ordinate file.
*             ==============================

               IF ( OUTCO .AND. FINAL( 1 ) .NE. VAL__BADD ) THEN
                  NC = 0
                  DO  J = 1, NDIM
                     CALL CHR_PUTD( FINAL( J ), BUFOUT, NC )
                     CALL CHR_PUTC( '  ', BUFOUT, NC )
                  END DO
                  CALL FIO_WRITE( FDO, BUFOUT( :NC ), STATUS )
               END IF

*             Write to the log file.
*             ======================
      
               CALL MSG_OUT( 'BLANK', ' ', STATUS )

*             Form string for the initial positions.
*             World co-ordinates only need single precision output.

               BUFOUT = 'Input guess position was    '
               NC = 29
               DPOUT = ( COSYS .EQ. 'DATA' .AND. CURSOR ) .OR. DATACO
               DO  J = 1, NDIM
                  IF ( DPOUT ) THEN
                     CALL CHR_PUTD( CINIT( J ), BUFOUT, NC )
                  ELSE
                     CALL CHR_PUTR( REAL( CINIT( J ) ), BUFOUT, NC )
                  END IF
                  IF ( J. LT. NDIM ) CALL CHR_PUTC( ', ', BUFOUT, NC )
               END DO
               CALL MSG_OUT( 'INPUT_POS', BUFOUT, STATUS )

*             Write to the log file as well except for the 2-d case
*             where the old format is preserved until it can be is
*             revamped via new database software.

               IF ( LOGPOS .AND. NDIM .NE. 2 )
     :           CALL FIO_WRITE( FDL, BUFOUT, STATUS )

*             Watch for the case when no centroid was found.

               IF ( FINAL( 1 ) .NE. VAL__BADD ) THEN

*                Form string for the centroid positions.

                  BUFOUT = 'Output centroid position is '
                  NC = 29
                  DO  J = 1, NDIM
                     IF ( DPOUT ) THEN
                        CALL CHR_PUTD( FINAL( J ), BUFOUT, NC )
                     ELSE
                        CALL CHR_PUTR( REAL( FINAL( J ) ), BUFOUT, NC )
                     END IF
                     IF ( J. LT. NDIM )
     :                  CALL CHR_PUTC( ', ', BUFOUT, NC )
                  END DO
                  CALL MSG_OUT( 'OUTPUT_POS', BUFOUT, STATUS )

*                Write to the log file as well except for the 2-d case
*                where the old format is preserved until can be revamped
*                via new database software.

                  IF ( LOGPOS .AND. NDIM .NE. 2 )
     :              CALL FIO_WRITE( FDL, BUFOUT, STATUS )

*                Form string for the centroid errors when they have
*                been calculated.

                  IF ( CERROR .AND. ERROR( 1 ) .NE. VAL__BADD ) THEN
                     BUFOUT = 'Centroid position error is '
                     NC = 29
                     DO  J = 1, NDIM
                        IF ( DPOUT ) THEN
                           CALL CHR_PUTD( ERROR( J ), BUFOUT, NC )
                        ELSE
                           CALL CHR_PUTR( REAL( ERROR( J ) ), BUFOUT,
     :                                    NC )
                        END IF
                        IF ( J. LT. NDIM )
     :                     CALL CHR_PUTC( ', ', BUFOUT, NC )
                     END DO
                     CALL MSG_OUT( 'OUTPUT_ERR', BUFOUT, STATUS )

*                   Write to the log file as well. 

                     IF ( LOGPOS .AND. NDIM .NE. 2 )
     :                 CALL FIO_WRITE( FDL, BUFOUT, STATUS )
                  END IF

*             Report the failure.

               ELSE
                  CALL MSG_OUT( 'OUTPUT_POS',
     :              'Centroid not found.', STATUS )

*                Write to the log file as well.

                  IF ( LOGPOS .AND. NDIM .NE. 2 )
     :              CALL FIO_WRITE( FDL, 'Centroid not found.', STATUS )

               END IF

*             Leave whitespace for clarity.

               CALL MSG_OUT( 'BLANK', ' ', STATUS )
               IF ( LOGPOS .AND. NDIM .NE. 2 )
     :           CALL FIO_WRITE( FDL, ' ', STATUS )

*             Now for the log file in the 2-d case.  The format of
*             output is not in neat columns, but can be read in free
*             format.

               IF ( LOGPOS .AND. NDIM .EQ. 2 ) THEN
                  BUFOUT = ' '
                  NCHAR = 1

                  CALL CHR_PUTD( CINIT( 1 ), BUFOUT( 1:9 ), NCHAR )

*                Fixed offset to improve the appearance of the layout.

                  NCHAR = 11
                  CALL CHR_PUTD( CINIT( 2 ), BUFOUT( 1:20 ), NCHAR )

*                Fixed offset to improve the appearance of the layout.

                  NCHAR = 25

*                Watch for the case when no centroid was found.

                  IF ( FINAL( 1 ) .NE. VAL__BADD ) THEN
                     CALL CHR_PUTD( FINAL( 1 ), BUFOUT( 1:34 ), NCHAR )
                     NCHAR = NCHAR + 2
                     CALL CHR_PUTD( FINAL( 2 ), BUFOUT( 1:NCHAR+9 ),
     :                              NCHAR )

*                   Append the error estimates again using fixed offset
*                   to improve the layout.

                     NCHAR = 48
                     IF ( CERROR .AND. ERROR( 1 ) .NE. VAL__BADD ) THEN
                        CALL CHR_PUTD( ERROR( 1 ), BUFOUT( 1:61 ),
     :                                 NCHAR )
                        NCHAR = NCHAR + 2
                        CALL CHR_PUTD( ERROR( 2 ),
     :                                 BUFOUT( :NCHAR+13 ), NCHAR )
                     END IF
                  ELSE
                     CALL CHR_PUTC( 'Centroid not found', BUFOUT,
     :                              NCHAR )
                  END IF

                  CALL FIO_WRITE( FDL, BUFOUT, STATUS )
               END IF

            ELSE

*             Report the bad status from centroid subroutine for modes
*             other than File, but continue.

               CALL ERR_FLUSH( STATUS )
               ANOTHR = .TRUE.

*          End of centroid-determination error checking and reporting.

            END IF

*          Release the new error context.

            CALL ERR_RLSE

*          Annul the parameters before looping.

            IF ( ENVIRO ) THEN
               CALL PAR_CANCL( 'INIT', STATUS )

*             If the initial co-ordinates have already been supplied,
*             the application will not loop.

               ANOTHR = .NOT. SINGLE
            END IF

*       End of if-no-error-after-getting-co-ordinates check.

         END IF

*    End of another-centroid loop.

      END DO

*    Write the last results to the parameter system for use by
*    other applications in procedures.  Until ICl can handle arrays
*    preserve x-y values as this will be by far the most commonly used.

      CALL PAR_PUT0D( 'XCEN', FINAL( 1 ), STATUS )
      IF ( NDIM .GT. 1 ) CALL PAR_PUT0D( 'YCEN', FINAL( 2 ), STATUS )
      CALL PAR_PUT1D( 'CENTRE', NDIM, FINAL, STATUS )

*    Tidy the workspace.
*    ===================

      IF ( CERROR ) CALL DAT_ANNUL( ERLOC, STATUS )

*    NDF closedown.
*    ==============

  960 CONTINUE

*    Unmap and annul the data explictely as NDF_ will be confused if
*    the locator by reference is annulled first.

      CALL NDF_ANNUL( NDFC, STATUS )
      CALL NDF_ANNUL( NDF, STATUS )

*    Tidy up the input data structure.

      IF ( CURSOR ) THEN
         IF ( GOTLOC ) CALL REF_ANNUL( LOCI, STATUS )
         CALL DAT_VALID( LOCI, GOTLOC, STATUS )
         IF ( GOTLOC ) CALL DAT_ANNUL( LOCI, STATUS )
      END IF

*    End the NDF context.

      CALL NDF_END( STATUS )

*    AGI closedown
*    =============

  980 CONTINUE

*    Need to tidy up the graphics database before exiting.

      IF ( MODE .EQ. 'CURSOR' )
     :  CALL AGS_DEASS( 'DEVICE', DEVCAN, STATUS )

  999 CONTINUE

      IF ( INCO ) CALL FIO_ANNUL( FD, STATUS )
      IF ( OUTCO ) CALL FIO_ANNUL( FDO, STATUS )
      IF ( LOGPOS ) CALL FIO_ANNUL( FDL, STATUS )

      END
