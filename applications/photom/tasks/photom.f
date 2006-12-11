      SUBROUTINE PHOTOM ( STATUS )
*+
*  Name:
*     PHOTOM

*  Purpose:
*     Perform aperture photometry.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PHOTOM( STATUS )

*  Description:
*     PHOTOM performs aperture photometry. It has two basic modes of
*     operation; using an interactive display to specify the positions
*     for the measurements, or obtaining those positions from a file.
*     The aperture is circular or elliptical and the size and shape can
*     be varied interactively on the display, or by entering values from
*     the keyboard or parameter system. The background sky level can be
*     sampled interactively by manual positioning of the aperture, or
*     automatically from an annulus surrounding the object.
*
*     PHOTOM is a menu driven application. The menu has been designed
*     around single character entries, which hopefully have easily
*     remembered mnemonics. Many of the options have counterparts in the
*     parameter system, and so can be controlled outside the task by the
*     environment.

*  ADAM Parameters:
*     ANGLE = _REAL (Read)
*        The orientation of the ellipse defining the aperture. This is
*        defined in degrees going anti-clockwise from the positive
*        y-axis. This is equivalent to a position angle.
*
*     BIASLE = _REAL (Read)
*        The level in data units per pixel of any constant offset in
*        the image. This should be defined if the errors are to be
*        calculated from photon statistics. If the true value is unknown
*        use a value of 0.
*
*     CENTRO = _LOGICAL (Read)
*        Centre the object before measurement or accept the given
*        position as the centre of the aperture.
*
*        If this is true the aperture is centered around the object of
*        interest before the measurement is taken. The position supplied
*        to the program (interactively or from a file of positions) is
*        taken as a starting point and the position of maximum flux is
*        located within a search box of fixed size.
*
*        If this is false the position supplied to the program is used
*        as the centre of the aperture.
*
*     COMMAND = _CHAR (Read)
*        The next action. The options are defined by single letter
*        entries and should be one of the following:-
*
*        A(nnulus) --- This toggles between using an annular background
*             aperture and an interactive aperture.
*
*        C(entroid) --- This switches the centroiding of the object in
*             the aperture on and off.
*
*        E(xit) --- This command terminates the current PHOTOM session.
*
*        F(ile of positions) --- This command takes positions from a
*             file and performs photometry with the current aperture
*             parameters.
*
*        H(elp) --- This displays a brief line of help for each command.
*
*        I(nteractive shape) --- This allows the size and shape of the
*             aperture to be adjusted interactively on the screen.
*
*        M(easure) --- This performs interactive measurements of objects
*             individually selected from the screen.
*
*        N(on-interactive shape) --- The size and shape of the aperture
*             is entered from the keyboard.
*
*        O(ptions) --- This allows some of the defaulted parameters to
*             be changed from within the program.
*
*        P(hoton statistics) --- This selects the method for calculating
*             the errors; either from photon statistics, or from the
*             measured variance in the sky aperture or from the variance
*             component of the data array.
*
*        S(ky) --- This selects between the different methods of
*             estimating the background level in the sky aperture.
*
*        V(alues) --- This summarises the current settings of the
*             programs parameters.
*
*     CONCEN = _LOGICAL (Read)
*        Find the sky automatically from a concentric aperture or
*        select the sky regions interactively.
*
*        If this is true the sky level is estimated from an aperture
*        which is concentric about the object aperture. The shape and
*        orientation of the sky aperture is the same as the object
*        aperture and the size of the annular aperture is defined by the
*        INNER and OUTER parameters. This mode is used if the
*        measurement positions are being supplied from a file.
*
*        If this is false the sky level is estimated from an aperture
*        equal in size and shape to the object aperture, which is
*        positioned manually on the image display. In this mode several
*        consecutive sky measurements can be made around the object of
*        interest and these are averaged to give the final sky estimate.
*
*     DEVICE = DEVICE (Read)
*        The name of the device to be used for interactive measurements
*        on which the data has been displayed. If the device has an
*        overlay plane then this should be selected.
*
*     ECCEN = _REAL (Read)
*        The eccentricity of the ellipse defining the aperture. For a
*        circular aperture this should be set to 0.0.
*
*     EXSOURCE = LITERAL (Read)
*        The "source" of the image exposure time supplied via the ETIME
*        parameter. This can take one of the values:
*
*           - HDS
*           - CONSTANT
*           - HEADER
*
*        HDS: indicates that the exposure value is stored in an HDS
*        object somewhere in the image (this presumes that the image is
*        an NDF and corresponds to the original behaviour of PHOTOM,
*        prior to the introduction of this parameter).
*
*        CONSTANT: indicates that a simple floating point value will be
*        supplied for the image exposure time.
*
*        HEADER: indicates that the value to be used is stored in the
*        image header (i.e. FITS headers).
*
*        [HDS]
*
*     ETIME = LITERAL (Read)
*        A string that, according to the value returned for parameter
*        EXSOURCE, allows the exposure time of the image to be
*        determined. If EXSOURCE is defined as:
*
*        HDS: then a fully qualified HDS path to the required object
*        within the NDF should be given. For instance if the exposure
*        time is stored in the CCDPACK extension of an NDF, under the
*        item ETIME then a suitable return would be:
*
*           - more.ccdpack.etime
*
*        The HDS structure of an NDF can be viewed using the HDSTRACE
*        utility (see SUN/102).
*
*        CONSTANT: then a floating point value should be given.
*
*        HEADER: then the name of the associated item should be given
*        (e.g. the FITS item EXPOSURE).
*
*        [!]
*
*     IN = NDF (Read)
*        An NDF data structure containing the 2-dimensional image on
*        which aperture photometry will be performed.
*
*     INNER = _REAL (Read)
*        The radius of the inner edge of the annular sky aperture in
*        units of the object aperture size. The actual dimension in
*        pixels is obtained by multiplying this factor by the object
*        aperture semi-major axis in pixels.
*
*     MASKFILE = FILENAME (Read)
*        Name of the file containing the positions to be used as
*        centers for masking objects from the sky aperture. The file
*        should contain a minimum of three columns the first of which
*        contains an integer index number and the next two contain an
*        x and y position.
*
*     MASKRAD = _REAL (Read)
*        The radius in pixels of the circles used to mask out objects
*        from the background estimate. A pixel which is inside the sky
*        aperture and inside a masked region is not included in the
*        background estimate.
*
*     MAXITER = _INTEGER (Read)
*        The maximum number of iteration steps to be used in locating
*        the object centroid.
*
*     MAXSHIFT = _REAL (Read)
*        The maximum allowable shift in pixels between the initial
*        object position and the calculated centroid.
*
*     OUTER = _REAL (Read)
*        The radius of the outer edge of the annular sky aperture in
*        units of the object aperture size. The actual dimension in
*        pixels is obtained by multiplying this factor by the object
*        aperture semi-major axis in pixels.
*
*     PADU = _REAL (Read)
*        The number of photons for each interval of the data. If the
*        true value is unknown use a value of 1, in which case the
*        quoted measurement errors will be wrong by the unknown factor
*        SQRT(PADU).
*
*     PHOTON = _INTEGER (Read)
*        Select the method for calculating the measurement errors.
*        There are three possible choices selected by the integers 1 to 4
*        which have the following bindings:-
*        1 --- The errors are estimated from the photon statistics in the
*              sky and object apertures. The parameters PADU and BIASLE
*              should be set to their appropriate values to convert the
*              data units to photon numbers.
*        2 --- The errors are estimated from the measured variance in the
*              sky aperture. This method assumes that the measured variance
*              is due to photon statistics and estimates the error in the
*              object aperture accordingly. The PADU parameter should be
*              set to its appropriate value to convert the data units to
*              photon numbers.
*        3 --- The errors are estimated from the variance component of the
*              data array.
*        4 --- The errors are estimated from the measured variance in the
*              sky aperture. This method assumes that the errors are
*              Gaussian (same value per object and sky pixel), and thus
*              requires no knowledge of the values of PADU and BIASLE,
*              but can only be considered an upper limit on the error in
*              a measurement.
*
*     POSFILE = FILENAME (Read)
*        Name of the file containing a list of positions for
*        measurement. The file should contain a minimum of three columns
*        the first of which contains an integer index number and the
*        next two contain an x and y position.
*
*     POSITIVE = _LOGICAL (Read)
*        Find the object centroid for image features which are positive
*        or negative with respect to the background. This should be set
*        to true.
*
*     RESFILE = FILENAME (Write)
*        Name of the file to receive the results of the measurements.
*
*     SATURE = _REAL (Read)
*        The saturation level in data units for the image. If any pixels
*        in the object aperture have values greater than this then the
*        measurement is flagged with an 'S' in the output record.
*
*     SEARCH = _INTEGER (Read)
*        The size of the search box in pixels to be used in locating the
*        object centroid.
*
*     SEMIM = _REAL (Read)
*        The semi-major axis of the ellipse defining the aperture in
*        pixel units. For a circular aperture this corresponds to the
*        radius in pixel units.
*
*     SKY = _REAL (Read)
*        A constant value to be used as the sky esimate for subsequent
*        measurements. This defines the sky level in data units per
*        pixel. This value is used until another estimator is chosen.
*
*     SKYEST = _INTEGER (Read)
*        Select the estimator to be used to evaluate the background
*        level in the sky aperture. There are four possible choices
*        selected by the integers 1 to 4 which have the following
*        bindings:-
*        1 --- Mean. All pixels in the sky aperture are averaged.
*        2 --- Mean with 2 sigma rejection. All pixels with data values
*              within 2 standard deviations of the mean are averaged.
*        3 --- Mode. The peak of the histogram of pixel values (the most
*              likely value) in the sky aperture is estimated.
*        4 --- User supplied value. A constant value used for all
*              measurements. A sky variance (standard deviation) is also
*              requested so that a realistic error can be assigned to
*              the measurements if the error is calculated from the
*              variance in the sky aperture.
*
*     SKYMAG = _REAL (Read)
*        The magnitude assigned to the sky level when calculating the
*        magnitude of the object using the relation
*        OBJMAG = SKYMAG - 2.5 * LOG10( SIGNAL )
*        where SIGNAL is the brightness of the object minus sky in
*        photons.
*
*     SKYSIG = _REAL (Read)
*        A constant value for the sky variance. This is an estimate of
*        the standard deviation in the sky level in data units and is
*        used when SKYEST is 4.
*
*     TOLER = _REAL (Read)
*        The required positional accuracy in pixels to terminate the
*        centroiding iterations.
*
*     USEMAGS = _LOGICAL (Read)
*        If TRUE then the output values are converted into magnitudes.
*        If FALSE the output values MAG and MAGERR are modified to be
*        a mean photon count and the error in this count, the other
*        values remain the same, i.e. the sum of sky corrected photons
*        and the mean sky value. Note the SKYMAG value is not used
*        when this is FALSE. Note also that this value may only be
*        set once when PHOTOM is started and must be set either on the
*        command line (USEMAGS=TRUE or USEMAGS=FALSE) or in response to
*        a forced prompt (command line argument PROMPT).
*        [TRUE]
*
*     USEMASK = _LOGICAL (Read)
*        Define a mask to exclude regions from the background estimate.
*        If this is true a file of positions is requested which define
*        the centres of circles used to block regions from the sky
*        aperture. Contaminating objects, such as bright stars, can thus
*        be removed from the background estimate.
*
*     OPTIMA = _LOGICAL (Read)
*        Do optimal or aperture extraction
*
*     CLIP = REAL (Read)
*        Clipping Radius for weight mask in optimal extraction
*
*     SEE = REAL (Read)
*        Approximate seeing in pixels
*
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Examples:
*     PHOTOM ARP199
*        Performs aperture photometry on the 2-dimensional NDF data
*        structure ARP199.
*
*     PHOTOM SEARCH=5 MAXSHIFT=2.0
*        Defines the centroiding search box to be 5 pixels wide and the
*        maximum shift of the centroid from its initial, rough position
*        to be 2 pixels.
*
*     PHOTOM ETIME=MORE.EXP_TIME
*        An exposure time for the frame will be found in the primitive
*        component EXP_TIME which is a component of the structure MORE
*        in the data file.
*
*     PHOTOM USEMASK=T
*        A mask file will be used to define regions to be excluded from
*        the sky aperture.
*
*     PHOTOM USEMAGS=FALSE
*        This will output the photometry results in photon counts, so
*        the MAG field will now have a mean photon count and MAGERR the
*        error in this count (assuming poissonian statistics are valid).

*  Authors:
*     NE: Nick Eaton (University of Durham)
*     PWD: Peter W. Draper (Starlink, University of Durham)
*     AA: Alasdair Allan (Starlink, Keele University)
*     {enter_new_authors_here}

*  History:
*     30-OCT-1987 (NE):
*        Original version.
*     30-OCT-1989 (NE):
*        Get exposure time paramter.
*        Get workspace for sky mask.
*     30-JUL-1990 (NE):
*        Convert to NDF's.
*     20-JAN-1992 (NE):
*        Get variance component of NDF for error estimating.
*        Only get mask workspace if USEMASK parameter is set.
*        Remove request for grid array workspace. Now obtained in APTOP.
*     6-NOV-1996 (PWD):
*        Added USEMAGS parameters and all the associated changes.
*     18-MAY-1998 (PWD):
*        Added EXSOURCE parameter and associated changes.
*     3-DEC-1998 (AA):
*        Added OPTIMA parameter and associated changes
*        Added CLIP and SEE parameters and associated changes
*     10-DEC-1998 (AA)
*        CLIP and SEE parameters changed to _REAL from _INT
*     07-SEP-2004 (PWD):
*        Change to use CNF pointers.
*     11-DEC-2006 (PWD):
*        Introduce  Gaussian error analysis.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-

*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'CNF_PAR'

*    Status :
      INTEGER STATUS

*    Local variables :
      CHARACTER * ( 80 ) CETIME ! Exposure time qualifier as string
      CHARACTER * ( 80 ) EXSRC  ! Exposure time source
      CHARACTER * ( DAT__SZLOC ) MLOC

      INTEGER IDIMS( 2 ), IMASK, NE
      INTEGER INDF, IDATA, IVAR, LBND( 2 ), NDIM, NEL, UBND( 2 )

      LOGICAL ISVAR, USEMSK

*   Define the number of vertices in the polygonal aperture
      PARAMETER ( NE = 36 )

      REAL ELLIPS( 2, NE ), ETIME, INSL( 2, NE + 4 ), INSR( 2, NE + 4 ),
     :     L( 2, NE ), LYLIST( NE + 4 ), POLY( 2, 2 * NE + 8 ),
     :     R( 2, NE ), RYLIST( NE + 4 ), YLIST( NE + 6 )
*-

*   Check status on entry
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Start up NDF and get a NDF identifier for the data
      CALL NDF_BEGIN
      CALL NDF_ASSOC( 'IN', 'READ', INDF, STATUS )

*   Check the bounds of the input data
      CALL NDF_BOUND( INDF, 2, LBND, UBND, NDIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      IF ( NDIM .NE. 2 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'PHOTOM_BOUNDS', 'Data array not 2-dimensional',
     :                 STATUS )
         GOTO 99
      ENDIF

*   Put the pixel dimensions of the data array into the IDIMS array
      IDIMS( 1 ) = UBND( 1 ) - LBND( 1 ) + 1
      IDIMS( 2 ) = UBND( 2 ) - LBND( 2 ) + 1

*   Get a mapped pointer to the data array
      CALL NDF_MAP( INDF, 'DATA', '_REAL', 'READ', IDATA, NEL, STATUS )

*   Find out if the data has a variance component
      CALL NDF_STATE( INDF, 'VARIANCE', ISVAR, STATUS )

*   If so get a mapped pointer to the variance array
      IF ( ISVAR ) THEN
         CALL NDF_MAP( INDF, 'VARIANCE', '_REAL', 'READ', IVAR, NEL,
     :                 STATUS )
      ENDIF

*   Get the source for the image exposure time.
      CALL PAR_CHOIC( 'EXSOURCE', 'HDS', 'HDS,CONSTANT,HEADER', .FALSE.,
     :                EXSRC, STATUS )

*   Get the value that qualifies the exposure time (i.e. a FITS keyword,
*   HDS path name or a simple floating point number).
      CALL PAR_GET0C( 'ETIME', CETIME, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         ETIME = 1.0
      ELSEIF ( STATUS .NE. SAI__OK ) THEN
         GOTO 99
      ELSE
         CALL PHO1_ETIME( EXSRC, CETIME, INDF, ETIME, STATUS )
      ENDIF

*   Inquire if a mask is going to be used
      CALL PAR_GET0L( 'USEMASK', USEMSK, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         USEMSK = .FALSE.
      ELSEIF ( STATUS .NE. SAI__OK ) THEN
         GOTO 99
      ENDIF

*   Get some work space for the mask array
      IF ( USEMSK ) THEN
         CALL AIF_TEMP( '_REAL', 2, IDIMS, MLOC, STATUS )
         CALL DAT_MAPR( MLOC, 'WRITE', 2, IDIMS, IMASK, STATUS )
      ENDIF

*   Call the main routine
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL APTOP ( IDIMS( 1 ), IDIMS( 2 ), LBND,
     :                %VAL( CNF_PVAL( IDATA ) ), ISVAR,
     :                %VAL( CNF_PVAL( IVAR ) ), USEMSK,
     :                %VAL( CNF_PVAL( IMASK ) ), ETIME, NE, ELLIPS,
     :                L, R, YLIST, LYLIST, RYLIST, INSL, INSR, POLY,
     :                STATUS )
      ENDIF

*   Unmap the arrays and annul the locators
  99  CONTINUE
      IF ( USEMSK ) THEN
         CALL AIF_ANTMP( MLOC, STATUS )
      ENDIF
      CALL NDF_END( STATUS )

      END

