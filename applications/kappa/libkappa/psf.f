      SUBROUTINE PSF( STATUS )
*+
*  Name:
*     PSF

*  Purpose:
*     Determines the parameters of a model star profile by fitting star
*     images in a two-dimensional NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PSF( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application finds a set of parameters to describe a model
*     Gaussian star image.  It can be used for profile-fitting stellar
*     photometry, to evaluate correction terms to aperture
*     photometry, or for filtering.
*
*     The model has a radial profile:
*        D =  A exp(-0.5 * (r/sigma) ** gamma )
*     where r is calculated from the true radial distance from the star
*     centre allowing for image ellipticity, sigma is the Gaussian
*     precision constant or profile width.  The application combines a
*     number of star images you specify and determines a mean
*     seeing-disc size, radial fall-off parameter (gamma), axis ratio,
*     and orientation of a model star image.

*     A table, giving details of the seeing and ellipticity of each
*     star image used can be reported to an output text file.  This
*     table indicates if any star could not be used.  Reasons for 
*     rejecting stars are too-many bad pixels present in the image,
*     the star is too close to the edge of the data array, the
*     `star' is a poor fit to model or it could not be located.

*     An optional plot of the mean profile and the fitted function may
*     be produced.  The point-spread function may be stored in an NDF
*     for later use.

*  Usage:
*     psf in cofile device out [cut] [range] [isize] [poscols] [clear]

*  ADAM Parameters:
*     ABSLAB = LITERAL (Read)
*        Label for the plot abscissa, in which NCAR fancy founts may be
*        embedded when FONT = "NCAR".  The default is "^AXIS-axis
*        Distance (^RADUNITS)" where ^RADUNITS is replaced by the value
*        of parameter RADUNITS, and ^AXIS is "Minor" when MINOR=TRUE
*        and is "Major" when MINOR=FALSE.
*     AXISR = _REAL (Write)
*        The axis ratio of the star images: the ratio of the major
*        axis length to that of the minor axis.
*     CLEAR = _LOGICAL (Read)
*        Determines if the graphics workstation is to be cleared before
*        producing the plot.  It is ignored if no plotting is required
*        defined by DEVICE. [TRUE]
*     COFILE = FILENAME (Read)
*        Text file containing the x and y co-ordinates.  The data
*        should be in columns separated by spaces or tabs, however
*        precise alignment is not necessary.
*     COSYS = LITERAL (Read)
*        The co-ordinate system to be used.  This can be either "World"
*        or "Data".  If COSYS = "Data" the input co-ordinates are to be
*        expressed in data co-ordinates, otherwise pixel (world)
*        co-ordinates are used.  In order to compute the point-spread
*        function data co-ordinates are converted to pixel indices via
*        the NDF's axis values; if there is no axis information within
*        the NDF, world co-ordinates are then used.  If COSYS = "World"
*        pixel co-ordinates are used throughout.  [Current co-ordinate
*        system]
*     CUT = _REAL (Read)
*        This parameter controls the size of the output NDF.  If it is
*        null, !, the dimension of the square NDF will be the size of
*        the region used to calculate the radial profile, which usually
*        is given by RANGE * width in pixels * AXISR, unless truncated.
*        If CUT has a value it is the threshold which must be included
*        in the PSF NDF, and it is given as the fraction of the peak
*        amplitude of the PSF.  For example, if CUT=0.5 the NDF would
*        contain the point-spread function to half maximum.  CUT must
*        be greater than 0 and less than 1.  The suggested default is
*        0.0001 [!]
*     DEVICE = DEVICE (Read)
*        The graphics workstation on which to produce a plot of the
*        mean radial profile of the stars and the fitted function.  A
*        null (!) name indicates that no plot is required.  The
*        suggested default is the current graphics device.
*     FONT = LITERAL (Read)
*        The fount to be used for the line graphics.  It can be either
*        "NCAR" for the NCAR fancy characters and "GKS" for the standard
*        GKS san-serif fount.   The former is intended for hardcopy
*        publication-quality plots, since it is relatively slow; the
*        latter is intended for normal interactive graphics requiring
*        rapid plotting, and it is clearer on small plots. The
*        suggested default is the current value. ["GKS"]
*     FWHM = _REAL (Write)
*        The seeing-disc size: the full width at half maximum across the
*        minor axis of the stars.  It is in the units defined by
*        parameters SCALE and RADUNITS.  By default this will be in
*        pixels.
*     GAMMA = _REAL (Write)
*        The radial fall-off parameter of the star images. See the 
*        description for more details.  A gamma of two would be a
*        Gaussian.
*     IN = NDF (Read)
*        The NDF containing the star images to be fitted.
*     ISIZE = _INTEGER (Read)
*        The side of the square area to be used when forming the
*        marginal profiles for a star image.  It should be sufficiently
*        large to contain the entire star image.  It should be an odd
*        number and must lie in the range from 3 to 101.  If an even
*        value is given, the next largest odd number is used instead.
*        [15]
*     LOGFILE = FILENAME (Read)
*        Text file to contain the table of parameters for each star.  A
*        null (!) name indicates that no log file is required. [!]
*     MAJTIC( 2 ) = _REAL (Read)
*        The parameter controlling the numbers of major tick marks
*        for the x and y axes.  (Number used is between MAJTIC+2 and
*        5*MAJTIC/2+4.) [3.,3.]
*     MINOR = _LOGICAL (Read)
*        If MINOR is TRUE the plot abscissa is the distance along the
*        minor axis from the centre of the PSF.  If MINOR is FALSE, the
*        major axis is plotted.  [TRUE]
*     MINTIC( 2 ) = _REAL (Read)
*        The number of minor tick marks between each major tick mark
*        for the x and y axes.  A negative value forces the graphics
*        package to compute appropriate values. [-1.,-1.]
*     ORDLAB = LITERAL (Read)
*        Label for the plot ordinate, in which NCAR fancy founts may be
*        embedded when FONT = "NCAR".  ["Intensity"]
*     ORIENT = _REAL (Write)
*        The orientation of the major axis of the star images to the x
*        axis (increasing pixel-index direction).  This value is in
*        degrees, x through y being considered positive.
*     OUT = NDF (Write)
*        The NDF containing the fitted point-spread function evaluated
*        at each pixel. Its dimensions are always odd numbered and
*        the centre of the PSF is located at the centre of the image.
*        If null, !, is entered no output NDF will be created.  The
*        dimensions of the array are controlled by parameter CUT. [!]
*     OUTTIC = _LOGICAL (Read)
*        OUTTIC is TRUE if the axis tick marks are to appear on the
*        outside of the axes instead of inside. [FALSE]
*     PLTITL = LITERAL (Read)
*        The title of the plot.  Up to about 40 characters can be
*        accommodated, and NCAR fancy founts may be embedded when
*        FONT = "NCAR".  ["Mean Star Profile"]
*     POSCOLS = _INTEGER (Read)
*        Column positions of the co-ordinates in an input record of the
*        Text file, x then y.  The columns must be different amongst
*        themselves. If there is duplication new values will be
*        requested.  [1,2]
*     PXSIZE = _REAL (Read)
*        The horizontal size of the plot in metres. If a value less
*        than the default is requested, then the plot will appear at
*        the bottom left of the current picture. [The size of the
*        current picture]
*     PYSIZE = _REAL (Read)
*        The vertical size of the plot in metres. If a value less than
*        the default is requested, then the plot will appear at the
*        bottom left of the current picture. [The size of the current
*        picture]
*     RADUNITS = LITERAL (Read)
*        The units of the radial profile after applying parameter SCALE
*        to the pixels.  RADUNITS defaults to "pixels" when SCALE is
*        1.0. []
*     RANGE = _REAL (Read)
*        The number of image profile widths out to which the radial
*        star profile is to be fitted.  (There is an upper limit of 100
*        pixels to the radius at which data are actually used.) [4.0]
*     SCALE = _REAL (Read)
*        A scale factor to convert pixels to some physical units such
*        as arcseconds.  This factor is applied to the reported FWHM
*        and to the radial distances in the plotted profile. [1.0]
*     TITLE = LITERAL (Read)
*        The title for the NDF to contain the fitted point-spread
*        function.  If null, !, is entered the NDF will not contain a
*        title.  ["KAPPA - PSF"]

*  Examples:
*     psf ngc6405i starlist.dat \
*        Derives the mean point-spread function for the stars images
*        in the NDF called ngc6405i that are situated near the x-y
*        co-ordinates given in the first two columns of file starlist.
*        A plot of the profile is drawn on the current graphics device.
*        The results are stored in the parameter file psf.sdf.
*     psf ngc6405i starlist device=!
*        As above but there is no graphical output. 
*     psf cofile=starlist in=ngc6405i logfile=fit.log fwhm=(seeing) \
*        As the first example, but the results, including the fits to
*        each star, are written to the text file fit.log.  The
*        full-width half-maximum is written to the ICL variable SEEING
*        rather than the parameter file.
*     psf ngc6405i starlist isize=31 \
*        As the first example, but the area including a star image is
*        31 pixels square, say because the seeing is poor or the pixels
*        are smaller than normal.
*     psf ngc6405i starlist out=starpsf cut=1.0e-3 scale=0.52
*     radunits="arcseconds"
*        As the first example, but the resultant point-spread function
*        is stored in the NDF called starpsf, and will contain signals
*        as low as 1.0E-3.  The FWHM and plot abscissa are scaled to
*        arcseconds, where a pixel corresponds to 0.52 arcseconds.

*  Notes:
*     -  The stars used to determine the mean image parameters should
*     be chosen to represent those whose magnitudes are to be found
*     using a stellar photometry application, and to be sufficiently
*     bright, uncrowded, and noise-free to allow an accurate fit to be
*     made.
*     -  The method to calculate the fit is as follows.
*        o   Marginal profiles of each star image are formed in four
*        directions, inclined at 45-degree intervals.  A Gaussian curve
*        and background is fitted to each profile.  Using the resulting
*        four Gaussian centres, a mean centre is found for each star.
*        o  The four Gaussian widths of all the stars are combined,
*        using a weighted average with rejection of erroneous data, and
*        from the four average widths the seeing-disc size, axis ratio
*        and axis inclination are calculated.
*        o  The data surrounding each star is then binned into
*        isophotal zones which are elliptical annuli centred on the
*        star---the ellipse parameters being those just calculated.
*        The data in each zone is processed to remove erroneous points
*        and to find an average value.  A Gaussian profile is fitted to
*        these average values and the derived amplitude is used to
*        normalise the values to an amplitude of unity.  The normalised
*        values are put into bins together with the corresponding data
*        from all other stars and this binned data represents a
*        weighted average radial profile for the set of stars, with the
*        image ellipticity removed.  Finally a radial profile is fitted
*        to these data, giving the radial profile parameter gamma and a
*        final re-estimate of the seeing-disc size.
*     -  If a plot was requested the application stores two pictures in
*     the graphics database in the following order: a FRAME of the
*     specified size containing the title, annotated axes, and line
*     plot; and a DATA picture, which has world co-ordinates measured
*     in pixels along the x axis and normalized intensity values along
*     y.  The NDF associated with the plot is not stored by reference
*     with the DATA picture.  On exit the current database picture for
*     the chosen device reverts to the input picture.

*  Algorithm:
*     -  Obtain the column numbers for each co-ordinate, checking
*     that there are no duplications.
*     -  Get the input NDF and the implementation type.  Ensure that the
*     NDF is 2-dimensional, aborting if not.
*     -  Open and read the x-y file to find the number of stars.
*     -  Obtain workspace for the co-ordinates, and read them in.
*     -  Open the log file if required.
*     -  Get the search-area size and the fitting range.
*     -  Call the routine to do the individual and mean fit, recording
*     and displaying the results.  (See the Notes section for more
*     details.)
*     -  Obtain the cut for the output NDF.  Compute the dimensions of
*     the output NDF, and obtain the NDF (if not null).  Map its data
*     array and fill it with the evaluated PSF at each pixel.  Obtain
*     a title for the PSF NDF.
*     -  Store the results in parameters.
*     -  Tidy the workspace, close the log file and close the NDF
*     system.

*  Timing:
*     Approximately proportional to the number of stars used and the
*     image area which each occupies.

*  Related Applications:
*     PHOTOM; Starman.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     LABEL, and TITLE components of an NDF data structure.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.  The output
*     point-spread-function NDF has type _REAL.

*  Implementation Deficiencies:
*     -  The star images can only be defined via an x-y list.
*     -  There is only one way of computing the point-spread functions.
*     -  Bad pixels are always assumed to be present.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1990 Sep 25 (MJC):
*        Original version with commentary from Rodney Warren-Smith's
*        EDRS documentation.
*     1991 July 6 (MJC):
*        Added Usage, Notes on graphics pictures stored in the database,
*        and more on the Implementation Status in the documentation.
*     1991 July 9 (MJC):
*        An output NDF containing the PSF may now be created.  There are
*        three new parameters: CUT, OUT and TITLE.
*     1991 July 12 (MJC):
*     	 Support for data co-ordinates added via new parameter COSYS.
*     1991 August 20 (MJC):
*        Added FONT parameter.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1992 April 21 (MJC):
*        Made to handle significant dimensions for user-defined
*        sections.
*     1992 November 30 (MJC):
*        Does not use non-monotonic axis centres.
*     1993 August 27 (MJC):
*        Added ABSLAB, ORDLAB, PLTITL, RADUNITS, SCALE, and MINOR
*        parameters for adjustment of the plotting style, for scaling
*        from pixels to physical units, and for minor-axis profiles.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE            ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Standard SAE constants
      INCLUDE 'DAT_PAR'        ! DAT_ constants
      INCLUDE 'NDF_PAR'        ! NDF_ public constants
      INCLUDE 'PRM_PAR'        ! PRIMDAT public constants
      INCLUDE 'PAR_ERR'        ! Parameter-system error definitions
      INCLUDE 'FIO_ERR'        ! Fortran-I/O-system errors

*  Status:
      INTEGER STATUS           ! Global status

*  External references:
      INTEGER CHR_LEN          ! Length of string ignoring trailing
                               ! blanks

*  Local Constants:
      INTEGER MAXREC           ! Maximum number of values per record
      PARAMETER ( MAXREC = 65536 )

      INTEGER NDIM             ! 2-d data arrays only
      PARAMETER ( NDIM = 2 )

      INTEGER NCHLIN           ! Maximum number of characters in a
                               ! an input record
      PARAMETER ( NCHLIN = 132 )
 

*  Local Variables:
      INTEGER
     :  AEL( NDF__MXDIM ),     ! Number of elements in a mapped axis
     :  AXPNTR( NDF__MXDIM ),  ! Pointers to the mapped axes
     :  COUNT,                 ! Number of data points input
     :  DCPLCE,                ! Placeholder to temporay NDF for data
                               ! co-ordinates
     :  DCPNTR( 1 ),           ! Pointer to work array for data co-ords
     :  DIMS( NDIM ),          ! Dimensions of the NDF
     :  EL,                    ! Number of elements in the input array
                               ! and output array
     :  FDL,                   ! File description for log file
     :  FDXY,                  ! File description for file of x-y
                               ! positions
     :  I,                     ! Loop counter
     :  ISIZE,                 ! Pixel size of square about a star used
                               ! to form marginal profiles
     :  LBND( NDF__MXDIM ),    ! Lower bounds of the image
     :  LSTLEN                 ! Number of non-comment lines in the
                               ! x-y file

      INTEGER
     :  NC,                    ! Character column counter in output
                               ! buffer
     :  NCHAR,                 ! Number of characters in the buffer read
                               ! from the x-y file
     :  NCF,                   ! Character column counter of file names
     :  NDIMS,                 ! Actual number of dimensions of the NDF
     :  NDF,                   ! NDF identifier
     :  NDFDC,                 ! NDF identifier for data co-ordinates
     :  NDFDCS,                ! NDF identifier for data co-ordinates
                               ! along one axis
     :  NDFO,                  ! NDF identifier for output PSF
     :  NDFWC,                 ! NDF identifier for world co-ordinates
     :  NDFWCS                 ! NDF identifier for world co-ordinates
                               ! along one axis

      INTEGER
     :  PNTRI( 1 ),            ! Pointer to input data array
     :  POSCOD( NDIM ),        ! Positions of co-ordinates in records
     :  POSDEF( NDIM ),        ! Suggested default positions of
                               ! co-ordinates in records
     :  PSFDIM( NDIM ),        ! PSF dimensions
     :  PSFPTR( 1 ),           ! Pointer to output PSF data array
     :  PSFSIZ,                ! Dimension of region used to calculate
                               ! mean PSF
     :  SDIM( NDF__MXDIM ),    ! Significant dimensions of the NDF
     :  SLBND( NDIM ),         ! Significant lower bounds of the image
     :  SUBND( NDIM ),         ! Significant upper bounds of the image
     :  UBND( NDF__MXDIM )     ! Upper bounds of the image

      INTEGER
     :  WCPLCE,                ! Placeholder to temporay NDF for world
                               ! co-ordinates
     :  WCPNTR( 1 ),           ! Pointer to work array for world co-ords
     :  WLBND( 2 ),            ! Lower bounds of the work space
     :  WOBT,                  ! Number of workspace arrays obtained
                               ! (not temporary NDFs)
     :  WPNTR( 2 ),            ! Pointers to work array for width data
                               ! and for co-ordinates
     :  WUBND( 2 )             ! Upper bounds of the work space

      REAL
     :  AXISR,                 ! Axis ratio o fthe star images
     :  CUT,                   ! The threshold to which the output PSF
                               ! must extend
     :  FWHM,                  ! FWHM of the star images
     :  GAMMA,                 ! Radial fall-off parameter
     :  LBNDP( NDIM ),         ! Minimum co-ordinates input alias the
                               ! origin co-ordinates
     :  RANGE,                 ! Number of image profile widths to which
                               ! the radial profile is to be fitted
     :  SCALE,                 ! Scale factor to convert radial pixels
                               ! into physical units
     :  THETA,                 ! Orientation of the star images
     :  UBNDP( NDIM )          ! Maximum co-ordinates input

      DOUBLE PRECISION
     :  DLBNDP( NDIM ),        ! Minimum data co-ordinates input
     :  DUBNDP( NDIM )         ! Maximum data co-ordinates input

      CHARACTER
     :  BUFFER * ( NCHLIN ),   ! Buffer to store output string
     :  COSYS * 5,             ! Co-ordinate system
     :  DATNAM * 100,          ! Name of input NDF
     :  DTYPE * ( NDF__SZFTP ),! HDS type of the data values
     :  FILNAM * 100,          ! Name of input x-y file
     :  ITYPE * ( NDF__SZTYP ),! Implemention HDS type
     :  RUNITS * ( 48 )        ! Units of scaled radial distance (length
                               ! governed by output of results, 32
                               ! characters used)

      LOGICAL                  ! True if:
     :  CMPLET,                ! Completely read the text file
     :  DACOOR,                ! The NDF contains an axis structure
     :  DATACO,                ! Input positions are to be given in
                               ! data co-ordinates
     :  LOGFIL,                ! Log file is open
     :  MONOTO,                ! Axis is monotonic
     :  POSDUP                 ! Obtained different column positions for
                               ! co-ordinates

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      POSDUP = .TRUE.
      WOBT = 0

*  Get different column numbers for the x-y co-ordinates.
*  ======================================================
*    
*  Set dynamic defaults for position columns.
      POSDEF( 1 ) = 1
      POSDEF( 2 ) = 2

      DO WHILE ( POSDUP .AND. STATUS .EQ. SAI__OK )

*  Start a new error context.
         CALL ERR_MARK

*  Get the co-ordinates.  Must be two-dimensional.
         CALL PAR_GDR1I( 'POSCOLS', NDIM, POSDEF, 1, MAXREC, .FALSE.,
     :                   POSCOD, STATUS )

*  Check for good status before further validation.
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Assume no duplication for the moment.
            POSDUP = .FALSE.

*  Check for duplication of the  co-ordinate columns.
            IF ( POSCOD( 1 ) .EQ. POSCOD( 2 ) ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'PSF_CEQCP',
     :          'PSF: Duplication of co-ordinate columns.', STATUS )

*  Report the error immediately, and reset the status to OK.
               CALL ERR_FLUSH( STATUS )

*  There is duplication so try again.
               POSDUP = .TRUE.

*  Cancel the parameters so that the user can be re-prompted.
               CALL PAR_CANCL( 'POSCOLS', STATUS )
            END IF
         END IF

*  End the error context.
         CALL ERR_RLSE
      END DO

*  Report an error and abort.
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Get and map the NDF.
*  ====================
*    
*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain the identifier of the NDF to be displayed.
      CALL NDF_ASSOC( 'IN', 'READ', NDF, STATUS )

*  This application can only process non-complex types. Therefore for
*  the given type of the image find in which type it should be
*  processed.
      CALL NDF_MTYPE( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_REAL,'/
     :                /'_DOUBLE', NDF, NDF, 'Data', ITYPE, DTYPE,
     :                STATUS )

*  Find whether or not there are but two significant dimensions and
*  which ones they are.
      CALL KPG1_SGDIM( NDF, NDIM, SDIM, STATUS )

*  Exit if an error occurred.  This is needed because the significant
*  dimensions are used as array indices.
      IF ( STATUS .NE. SAI__OK ) GOTO 980

*  Obtain the bounds of the image.  These will be stored in the
*  graphics database once the cell-array is displayed.
      CALL NDF_BOUND( NDF, NDF__MXDIM, LBND, UBND, NDIMS, STATUS )

*  Compute the dimensions from the significant dimensions.
      DO I = 1, NDIM
         SLBND( I ) = LBND( SDIM( I ) )
         SUBND( I ) = UBND( SDIM( I ) )
         DIMS( I ) = SUBND( I ) - SLBND( I ) + 1
      END DO

*  Map the image.
      CALL NDF_MAP( NDF, 'Data', ITYPE, 'READ', PNTRI, EL, STATUS )

*  Get the type of co-ordinates to input.
*  ======================================

*  Is there an axis system?
      CALL NDF_STATE( NDF, 'Axis', DACOOR, STATUS )

*  Obtain the desired co-ordinate system.
      CALL PAR_CHOIC( 'COSYS', 'World', 'Data,World', .FALSE., COSYS,
     :                STATUS )

*  Find the effective co-ordinate system.
      DATACO = DACOOR .AND. COSYS .EQ. 'DATA'

*  Map axes for data co-ordinates and test monotonicity.
*  =====================================================
      IF ( DATACO ) THEN

*  Obtain the axes.  Map in double precision to avoid loss of accuracy
*  where needed, i.e. the separations of pixels in data co-ordinates
*  are much smaller than their absolute values.  For example, small
*  regions of the sky in equatorial co-ordinates or the wavelength from
*  an echelle spectrogram.
         DO  I = 1, NDIM
            CALL NDF_AMAP( NDF, 'Centre', SDIM( I ), '_DOUBLE', 'READ',
     :                     AXPNTR( I ), AEL( I ), STATUS )

*  Are all the axes monotonic?  Start a new error context so that the
*  error reports concerning a non-monotonic axis may be annulled.
*  Instead we issue a warning message so that the application can
*  continue by using world co-ordinates.
            CALL ERR_MARK
            CALL KPG1_MONOD( .TRUE., AEL( I ), %VAL( AXPNTR( I ) ),
     :                       MONOTO, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_ANNUL( STATUS )
               MONOTO = .FALSE.
            END IF
            CALL ERR_RLSE

*  Issue the warning.
            IF ( .NOT. MONOTO ) THEN
               CALL MSG_SETI( 'IAXIS', SDIM( I ) )
               CALL MSG_OUT( 'PSF_NOTMONO',
     :           'PSF: Axis ^IAXIS is not monotonic.  Will use '/
     :           /'world co-ordinates instead.', STATUS )

*  Reset the co-ordinate system flag.
               DATACO = .FALSE.

*  Unmap the axis since we have finished with it.
               CALL NDF_AUNMP( NDF, 'Centre', SDIM( I ), STATUS )

            END IF
         END DO
      END IF

*  Open the x-y file and find the number of positions.
*  ===================================================
*    
*  Open the file containing the list of initial star positions.
      CALL FIO_ASSOC( 'COFILE', 'READ', 'LIST', 0, FDXY, STATUS )

      IF ( STATUS .NE. SAI__OK ) GOTO 980

*  The x-y list was obtained successfully.
      LSTLEN = 0
      DO WHILE ( STATUS .EQ. SAI__OK )

*  Read a line of the file.
         CALL FIO_READ( FDXY, BUFFER, NCHAR, STATUS )

*  Count the number of points in the list excluding comment lines.
         IF ( BUFFER( 1:1 ) .NE. '#' .AND.
     :        BUFFER( 1:1 ) .NE. '!' ) LSTLEN = LSTLEN + 1
      END DO

*  An end-of-file error is expected.  The last read added a non-
*  existent line to the co-ordinate file, so subtract it to obtain the
*  actual number of objects.  Do not wish to record this file as the
*  current value when there was an unexpected error.
      IF ( STATUS .NE. FIO__EOF ) THEN
         CALL ERR_REP( 'PSF_XYFIL',
     :     'PSF: Error reading the x-y file', STATUS )
         CALL FIO_CANCL( 'COFILE', STATUS )
         GOTO 980
      ELSE
         LSTLEN = LSTLEN - 1
      END IF

*  There must be some data.  If there are no data, do not record this
*  file as the current value.
      IF ( LSTLEN .LT. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'PSF_XYREC',
     :     'PSF: Insufficient data records in the x-y file.', STATUS )
         CALL FIO_CANCL( 'COFILE', STATUS )
         GOTO 980
      END IF

*  Now rewind the file so the values can be extracted later.
      CALL ERR_ANNUL( STATUS )
      CALL FIO_RWIND( FDXY, STATUS )

*  Obtain the name of the file of x-y positions.
      CALL AIF_FLNAM( 'COFILE', FILNAM, STATUS )

*  Obtain workspace for the x-y positions.
*  =======================================
      CALL PSX_CALLOC( 5 * LSTLEN, '_REAL', WPNTR( 1 ), STATUS )
      IF ( STATUS .EQ. SAI__OK ) WOBT = WOBT + 1

*  More workspace is needed to read data co-ordinates into double
*  precision, and convert them to pixels co-ordinates. A temporary NDF
*  is used because slices can be obtained and can be easily remapped as
*  _REAL once the co-ordinates are converted to pixels.
      IF ( DATACO ) THEN
         WLBND( 1 ) = 1
         WLBND( 2 ) = 1
         WUBND( 1 ) = NDIM
         WUBND( 2 ) = LSTLEN
         CALL NDF_TEMP( DCPLCE, STATUS )
         CALL NDF_NEW( '_DOUBLE', 2, WLBND, WUBND, DCPLCE, NDFDC,
     :                 STATUS )
         CALL NDF_MAP( NDFDC, 'Data', '_DOUBLE', 'WRITE', DCPNTR, EL,
     :                 STATUS )

         CALL NDF_TEMP( WCPLCE, STATUS )
         CALL NDF_NEW( '_DOUBLE', 2, WLBND, WUBND, WCPLCE, NDFWC,
     :                 STATUS )

*  There is no co-ordinate conversion so just read in positions in
*  single precision.
      ELSE
         CALL PSX_CALLOC( LSTLEN * NDIM, '_REAL', WPNTR( 2 ), STATUS )
         IF ( STATUS .EQ. SAI__OK ) WOBT = WOBT + 1
      END IF

*  If space was not available, give error message and abort.
      IF ( STATUS .NE. SAI__OK ) THEN

         CALL ERR_REP( 'PSF_WSP',
     :     'PSF: Unable to get workspace to read the co-ordinates.',
     :     STATUS )

         CALL FIO_ANNUL( FDXY, STATUS )
         GOTO 960
      END IF

*  Read the file to copy identifiers and x-y positions to the
*  workspace.  The read will be complete because we've already counted
*  the data lines.  COUNT on exit is the actual number of star
*  positions read plus one, so use LSTLEN instead.  Read in double
*  precision when using data co-ordinates.  Unmap the NDF so that a
*  section may be mapped.  No need to initialise the bounds of the
*  co-ordinates as these are not subsequently used.
      COUNT = 0
      IF ( DATACO ) THEN
         CALL KPG1_RFCOD( FDXY, NDIM, LSTLEN, POSCOD, COUNT,
     :                    %VAL( DCPNTR( 1 ) ), DLBNDP, DUBNDP, CMPLET,
     :                    STATUS )
         CALL NDF_UNMAP( NDFDC, 'Data', STATUS )
      ELSE
         CALL KPG1_RFCOR( FDXY, NDIM, LSTLEN, POSCOD, COUNT,
     :                    %VAL( WPNTR( 2 ) ), LBNDP, UBNDP, CMPLET,
     :                    STATUS )
      END IF

*  Close the input file of x-y positions.
      CALL FIO_ANNUL( FDXY, STATUS )

*  Convert from data co-ordinates to world co-ordinates for the
*  PSF-finding routine.
*  ============================================================
      IF ( DATACO ) THEN
         DO  I = 1, NDIM

*  Obtain a slice along the axis from the work array holding world
*  co-ordinates.   Note we only want to read the data positions written
*  to the temporary NDF, but to write world co-ordinates.
            WLBND( 1 ) = I
            WUBND( 1 ) = I
            CALL NDF_SECT( NDFDC, 2, WLBND, WUBND, NDFDCS, STATUS )
            CALL NDF_MAP( NDFDCS, 'Data', '_DOUBLE', 'READ', DCPNTR, EL,
     :                    STATUS )
            CALL NDF_SECT( NDFWC, 2, WLBND, WUBND, NDFWCS, STATUS )
            CALL NDF_MAP( NDFWCS, 'Data', '_DOUBLE', 'WRITE', WCPNTR,
     :                    EL, STATUS )
      
*  Derive its world equivalent, i.e. pixel indices.
            CALL KPG1_AINDD( SLBND( I ), SUBND( I ),
     :                       %VAL( AXPNTR( I ) ), LSTLEN,
     :                       %VAL( DCPNTR( 1 ) ), %VAL( WCPNTR( 1 ) ),
     :                       STATUS )

*  Unmap the sections.
            CALL NDF_ANNUL( NDFWCS, STATUS )
            CALL NDF_ANNUL( NDFDCS, STATUS )
         END DO

*  Finished with the data co-ordinates so annul them.
         CALL NDF_ANNUL( NDFDC, STATUS )

*  Map the array of world co-ordinates as single precision.  Use the
*  same pointer as for world co-ordinates.
         CALL NDF_MAP( NDFWC, 'Data', '_REAL', 'READ', WPNTR( 2 ), EL,
     :                 STATUS )
      END IF

*  Obtain the search area size and, the range of radii to be used in
*  the profile fit, in units of sigma.  Note the upper limit of the
*  area size should be equal to twice MAXRAD in KPS1_RPRFx.
      CALL PAR_GODD( 'ISIZE', 15, 3, 101, .TRUE., ISIZE, STATUS )
      CALL PAR_GDR0R( 'RANGE', 4.0, 1.0, 10.0, .FALSE., RANGE, STATUS )

      IF ( STATUS .NE. SAI__OK ) GOTO 960
      
*  Open a log file for the tabulated results.
*  ==========================================
*
*  A null result indicates that no logging is to take place.
      LOGFIL = .FALSE.
      CALL ERR_MARK
      CALL FIO_ASSOC( 'LOGFILE', 'WRITE', 'LIST', 80, FDL, STATUS )

      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
         LOGFIL = .TRUE.
      END IF
      CALL ERR_RLSE
      IF ( STATUS .NE. SAI__OK ) GOTO 960

      IF ( LOGFIL ) CALL MSG_OUT( 'LOG', 'Logging to $LOGFILE.',
     :                            STATUS )

*  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

*  Record the data files used in the log.
      IF ( LOGFIL ) THEN

*  Get the name of the NDF to store in the log file.
         CALL AIF_FLNAM( 'IN', DATNAM, STATUS )

*  Construct the strings.
         NC = 3
         BUFFER = ' '
         NCF = CHR_LEN( DATNAM )
         CALL CHR_PUTC( 'Input NDF is ', BUFFER, NC )
         CALL CHR_PUTC( DATNAM( :NCF ), BUFFER, NC )
         CALL CHR_PUTC( '.', BUFFER, NC )
         CALL FIO_WRITE( FDL, BUFFER( :NC ), STATUS )
         CALL FIO_WRITE( FDL, ' ', STATUS )

         NC = 3
         BUFFER = ' '
         NCF = CHR_LEN( FILNAM )
         CALL CHR_PUTC( 'Input x-y list file is ', BUFFER, NC )
         CALL CHR_PUTC( FILNAM( :NCF ), BUFFER, NC )
         CALL CHR_PUTC( '.', BUFFER, NC )
         CALL FIO_WRITE( FDL, BUFFER( :NC ), STATUS )
      END IF

*  Obtain the scale factor and units.
*  ==================================

*  Get the scale factor from pixels to physical units.  A null value
*  gives 1.0.
      CALL PAR_GDR0R( 'SCALE', 1.0, 0.0, VAL__MAXR, .TRUE., SCALE,
     :                STATUS )

*  Only set a dynamic default for the scaled physical units when the
*  scale factor is unity.
      IF ( ABS( SCALE-1.0 ) .LT. 2.0 * VAL__EPSR ) THEN
         CALL PAR_DEF0C( 'RADUNITS', 'pixels', STATUS )
      END IF

*  Get the units of the scaled radial distances.
      CALL PAR_GET0C( 'RADUNITS', RUNITS, STATUS )

*  Find the mean star profile.
*  ===========================
*
*  Find the mean star profile parameters calling the routine of the
*  appropriate data type.  Plot the results as required.
      IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL KPS1_SPARR( DIMS( 1 ), DIMS( 2 ), %VAL( PNTRI( 1 ) ),
     :                    SLBND, ISIZE, RANGE, LSTLEN,
     :                    %VAL( WPNTR( 2 ) ), SCALE, RUNITS, LOGFIL,
     :                    FDL, 'CLEAR', 'DEVICE', 'MINOR', 'PXSIZE',
     :                    'PYSIZE', 'PLTITL', 'ABSLAB', 'ORDLAB',
     :                    'MINTIC', 'MAJTIC', 'OUTTIC', 'FONT',
     :                    'KAPPA - PSF', AXISR, THETA, FWHM, GAMMA,
     :                    PSFSIZ, %VAL( WPNTR( 1 ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN
         CALL KPS1_SPARB( DIMS( 1 ), DIMS( 2 ), %VAL( PNTRI( 1 ) ),
     :                    SLBND, ISIZE, RANGE, LSTLEN,
     :                    %VAL( WPNTR( 2 ) ), SCALE, RUNITS, LOGFIL,
     :                    FDL, 'CLEAR', 'DEVICE', 'MINOR', 'PXSIZE',
     :                    'PYSIZE', 'PLTITL', 'ABSLAB', 'ORDLAB',
     :                    'MINTIC', 'MAJTIC', 'OUTTIC', 'FONT',
     :                    'KAPPA - PSF', AXISR, THETA, FWHM, GAMMA,
     :                    PSFSIZ, %VAL( WPNTR( 1 ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL KPS1_SPARD( DIMS( 1 ), DIMS( 2 ), %VAL( PNTRI( 1 ) ),
     :                    SLBND, ISIZE, RANGE, LSTLEN,
     :                    %VAL( WPNTR( 2 ) ), SCALE, RUNITS, LOGFIL,
     :                    FDL, 'CLEAR', 'DEVICE', 'MINOR', 'PXSIZE',
     :                    'PYSIZE', 'PLTITL', 'ABSLAB', 'ORDLAB',
     :                    'MINTIC', 'MAJTIC', 'OUTTIC', 'FONT',
     :                    'KAPPA - PSF', AXISR, THETA, FWHM, GAMMA,
     :                    PSFSIZ, %VAL( WPNTR( 1 ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         CALL KPS1_SPARI( DIMS( 1 ), DIMS( 2 ), %VAL( PNTRI( 1 ) ),
     :                    SLBND, ISIZE, RANGE, LSTLEN,
     :                    %VAL( WPNTR( 2 ) ), SCALE, RUNITS, LOGFIL,
     :                    FDL, 'CLEAR', 'DEVICE', 'MINOR', 'PXSIZE',
     :                    'PYSIZE', 'PLTITL', 'ABSLAB', 'ORDLAB',
     :                    'MINTIC', 'MAJTIC', 'OUTTIC', 'FONT',
     :                    'KAPPA - PSF', AXISR, THETA, FWHM, GAMMA,
     :                    PSFSIZ, %VAL( WPNTR( 1 ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         CALL KPS1_SPARUW( DIMS( 1 ), DIMS( 2 ), %VAL( PNTRI( 1 ) ),
     :                     SLBND, ISIZE, RANGE, LSTLEN,
     :                     %VAL( WPNTR( 2 ) ), SCALE, RUNITS, LOGFIL,
     :                     FDL, 'CLEAR', 'DEVICE', 'MINOR', 'PXSIZE',
     :                     'PYSIZE', 'PLTITL', 'ABSLAB', 'ORDLAB',
     :                     'MINTIC', 'MAJTIC', 'OUTTIC', 'FONT',
     :                     'KAPPA - PSF', AXISR, THETA, FWHM, GAMMA,
     :                     PSFSIZ, %VAL( WPNTR( 1 ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
         CALL KPS1_SPARUB( DIMS( 1 ), DIMS( 2 ), %VAL( PNTRI( 1 ) ),
     :                     SLBND, ISIZE, RANGE, LSTLEN,
     :                     %VAL( WPNTR( 2 ) ), SCALE, RUNITS, LOGFIL,
     :                     FDL, 'CLEAR', 'DEVICE', 'MINOR', 'PXSIZE',
     :                     'PYSIZE', 'PLTITL', 'ABSLAB', 'ORDLAB',
     :                     'MINTIC', 'MAJTIC', 'OUTTIC', 'FONT',
     :                     'KAPPA - PSF', AXISR, THETA, FWHM, GAMMA,
     :                     PSFSIZ, %VAL( WPNTR( 1 ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL KPS1_SPARW( DIMS( 1 ), DIMS( 2 ), %VAL( PNTRI( 1 ) ),
     :                    SLBND, ISIZE, RANGE, LSTLEN,
     :                    %VAL( WPNTR( 2 ) ), SCALE, RUNITS, LOGFIL,
     :                    FDL, 'CLEAR', 'DEVICE', 'MINOR', 'PXSIZE',
     :                    'PYSIZE', 'PLTITL', 'ABSLAB', 'ORDLAB',
     :                    'MINTIC', 'MAJTIC', 'OUTTIC', 'FONT',
     :                    'KAPPA - PSF', AXISR, THETA, FWHM, GAMMA,
     :                    PSFSIZ, %VAL( WPNTR( 1 ) ), STATUS )

      END IF

*  Find the dimensions of the NDF to contain the PSF.
*  ==================================================

*  Obtain the level as a fraction of the central amplitude to which the
*  output PSF should extend.  This controls the dimensions of of the
*  PSF array.
      CALL ERR_MARK
      CALL PAR_GDR0R( 'CUT', 0.0001, VAL__SMLR, 1.0 - VAL__SMLR,
     :                .FALSE., CUT, STATUS )

*  A null value means just use the same dimensions as was used to
*  calculate the mean profile.
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         PSFDIM( 1 ) = PSFSIZ
         PSFDIM( 2 ) = PSFSIZ
      ELSE
         CALL KPS1_PSDIM( AXISR, THETA, FWHM, GAMMA, CUT, PSFDIM,
     :                    STATUS )
      END IF
      CALL ERR_RLSE

*  Create output NDF containing the PSF.
*  =====================================
      CALL ERR_MARK

*  Start a new NDF context.
      CALL NDF_BEGIN

*  Create a new primitive NDF.
      CALL NDF_CREP( 'OUT', '_REAL', NDIM, PSFDIM, NDFO, STATUS )

*  Map it for write access.
      CALL NDF_MAP( NDFO, 'Data', '_REAL', 'WRITE', PSFPTR, EL, STATUS )

*  Fill the data array with the evaluated point-spread function.
*  Exclude origin information. (Eventually would like to have the PSF
*  centre at (0,0).)  Protect against the case where the dimensions are
*  undefined.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL KPS1_PSEVL( AXISR, THETA, FWHM, GAMMA, PSFDIM( 1 ),
     :                    PSFDIM( 2 ), %VAL( PSFPTR( 1 ) ), STATUS )
      END IF

*  Obtain a title for the PSF NDF.
      CALL NDF_CINP( 'TITLE', NDFO, 'TITLE', STATUS )

*  A null status can be ignored.  This means that no output NDF was
*  required.
      IF ( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )
      
      CALL ERR_RLSE

*  Write the results to the environment.
*  =====================================
      THETA = THETA * 57.29578
      CALL PAR_PUT0R( 'FWHM', FWHM, STATUS )
      CALL PAR_PUT0R( 'AXISR', AXISR, STATUS )
      CALL PAR_PUT0R( 'ORIENT', THETA, STATUS )
      CALL PAR_PUT0R( 'GAMMA', GAMMA, STATUS )

*  Tidying sequence.
*  =================

*  Close the log file.
      IF ( LOGFIL ) CALL FIO_ANNUL( FDL, STATUS )

*  Tidy the workspace.
  960 CONTINUE
      IF ( WOBT .GE. 1 ) THEN
         DO I = 1, WOBT
            CALL PSX_FREE( WPNTR( I ), STATUS )
         END DO
      END IF

  980 CONTINUE

*  Close down the NDF system, unmapping the NDF.
      CALL NDF_END( STATUS )

  999 CONTINUE

      END
