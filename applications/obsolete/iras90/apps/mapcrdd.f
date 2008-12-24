      SUBROUTINE MAPCRDD( STATUS )
*+
*  Name:
*     MAPCRDD

*  Purpose:
*     Maps a group of CRDD files into a two dimensional image.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL MAPCRDD( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine maps a group of CRDD files into a two dimensional
*     surface brightness image by coadding the data samples. The
*     algorithm used is described below, together with the extensive
*     facilities which exist for tailoring the behaviour of the
*     application.

*  Usage:
*     MAPCRDD IN PIXSIZE CENTRE_LON CENTRE_LAT BOXSIZE OUT 

*  ADAM Parameters:
*     BOXSIZE = _REAL (Read)
*        A pair of numbers giving the dimensions of the output image in
*        arc-minutes, along the two image axes. The default values for
*        CENTRE_LON, CENTRE_LAT and BOXSIZE offered to the user are
*        chosen so that all the input data (excluding the short
*        sections at the ends of each scan which have coverage by only
*        half of the detectors) just fits in the image.  If the image
*        is large, the distance between the sides (in terms of arc
*        minutes) will change due to projection effects.  The given
*        values are the distances between the centre points of opposite
*        sides of the image. If only one value is given, the created
*        image is square with the size of each side equal to the
*        supplied value.
*     CENTRE_LAT = LITERAL (Read)
*        The sky latitude of the centre of the output image, in the
*        coordinate system specified by the parameter COORDS (eg if
*        COORDS is EQUATORIAL then CENTRE_LAT should be given the
*        Declination of the image centre). See help on
*        "Sky_coordinates" for the formats allowed for this value.
*     CENTRE_LON = LITERAL (Read)
*        The sky longitude of the centre of the output image, in the
*        coordinate system specified by the parameter COORDS (eg if
*        COORDS is EQUATORIAL then CENTRE_LON should be given the Right
*        Ascension of the image centre).See help on "Sky_coordinates"
*        for the formats allowed for this value.
*     COORDS = LITERAL (Read)
*        Specifies the coordinate system used for referring to sky
*        positions. Valid values include ECLIPTIC, EQUATORIAL,
*        GALACTIC. See help on "Sky_coordinates" for more information
*        on available sky coordinate systems.
*                                        [current sky coordinate system]
*     EXCLUDE = LITERAL (Read)
*        A group of detector numbers, selected from those available in
*        the IRAS waveband of the data contained in the input CRDD
*        files. All data from the specified detectors are excluded from
*        the final map (dead detectors are automatically excluded). See
*        help on "Specifying_detectors" for more information on
*        specifying groups of detector numbers. The parameter INCLUDE
*        can alternatively be used to specify the detectors to use. The
*        default value of SMALL causes all the small detectors to be 
*        excluded.                                               [SMALL]
*     FWHM = _REAL (Read)
*        Two values specifying the full widths at half maximum of the
*        Gaussian weighting functions for a full-size detector (in
*        arc-minutes) in the cross-scan and in-scan directions.  The
*        values actually used for each detector are scaled in
*        proportion to the size of the detector. The weighting function
*        is truncated to zero at the edge of the detector mask. The
*        default values are the maximum cross-scan and in-scan detector
*        sizes for the relevant waveband, and result in the weighting
*        function falling to 0.5 before it is truncated at the edge of
*        the detector.  If a single value is given, the same value is
*        used for both cross-scan and in-scan widths.                 []
*     GAUSSIAN = _LOGICAL (Read)
*        If GAUSSIAN is set to a true value, then a Gaussian weighting
*        function is applied to all input CRDD samples (i.e. each CRDD
*        sample has a greater influence on pixels near the detector
*        centre than it does on the pixels near the edge). This can
*        decrease the noise and improve the resolution in the output
*        image, but at the cost of a slightly greater run-time.   [TRUE]
*     HISTORY = _LOGICAL (Read)
*        Determines if history information is to be stored within the
*        output NDF. See help on "History_in_IRAS90" for more
*        information on history. The history information will contain
*        the names of the input CRDD files, MAPCRDD parameter values
*        used to create the image, and the detector solid angles and
*        effective bandwidth used.             [current history setting]
*     IN = NDF (Read)
*        Specifies a group of input CRDD files. This should be in the
*        form of a group expression (see help on "Group_expressions").
*        All CRDD files must contain data from the same IRAS waveband.
*        There is no limit on the number of CRDD files which can be
*        specified.
*     INCLUDE = LITERAL (Read)
*        A group of detector numbers, selected from those available in
*        the IRAS waveband of the data contained in the input CRDD
*        files. Only data from the specified detectors are included in
*        the final map. The parameter EXCLUDE can alternatively be used
*        to specify the detectors to be omitted. Values specified for
*        the INCLUDE parameter take precidence over those specified for
*        the EXCLUDE parameter.  See help on "Specifying_detectors"
*        for more information on specifying groups of detector numbers.
*        The default group contains all the available detectors,
*        excluding those specified by the EXCLUDE parameter.          []
*     MSG_FILTER = LITERAL (Read)
*        The level of information displayed on the users screen. This
*        should take one of the values QUIET, NORMAL or VERBOSE (see
*        help on "Message_filtering").
*                                       [current message filter setting]
*     ORIENT = _REAL (Read)
*        The position angle of the second image axis( or "Y" axis), in
*        degrees.  That is, the angle from north (defined by the
*        coordinate system specified by parameter COORDS) to the image
*        "Y" axis ("upwards" if the image is displayed normally).
*        Positive angles are measured in the same sense as rotation
*        from north through east.  The default value of zero puts
*        north "upwards".                                          [0.0]
*     OUT = NDF (Write)
*        The name of the NDF to hold the output image. The output NDF
*        may have defined VARIANCE or HISTORY components depending on
*        the value of the VAROUT and HISTORY parameters, but never has
*        defined QUALITY or AXIS components.
*     PIXSIZE = _REAL (Read)
*        The dimensions of a pixel in the output image, in arc-minutes.
*        If a single value is given for PIXSIZE, then the output pixels
*        are square. If two values are given then the output pixels are
*        rectangular, having the specified dimensions. It should be
*        remembered that the actual pixel size may vary slightly across
*        the image depending on the type of projection. The values
*        specified by parameter PIXSIZE give the actual pixel
*        dimensions at the image centre.
*     PROJTYPE = LITERAL (Read)
*        Specifies the type of projection to use when creating the
*        output image. Valid values include GNOMONIC (i.e. tangent
*        plane), ORTHOGRAPHIC, LAMBERT and AITOFF.  See help on
*        "Map_projections" for more information on available
*        projections.                                         [GNOMONIC]
*     QEXP = LITERAL (Read)
*        A quality expression (see help on "Quality_in_IRAS90").  Only
*        those samples from the input CRDD files which have a QUALITY
*        satisfying the given quality expression will be included in
*        the mapping process. The default value of "ANY" causes all
*        data to be used regardless of QUALITY.  If any of the quality
*        names referenced in the quality expression are not defined for
*        an input CRDD file, then the entire CRDD file is included in
*        the map, irrespective of QUALITY. The user is warned if this
*        happens. Note, the output image has no QUALITY component.
*                                                                  [ANY]
*     SECSIZE = _REAL (Read)
*        The cross-scan and in-scan sizes of a full size detector
*        "sector" in arc-minutes.  Smaller values will produce a more
*        accurately Gaussian in-scan weighting function, but will
*        increase the run-time in proportion. The default values are
*        both equal to half of the minimum of the two pixel dimensions
*        (given by parameter PIXSIZE). If a single value is given then
*        the same value is used for both dimensions. The values used
*        for SECSIZE are internally limited to be less than or equal to
*        the pixel dimensions. Sector sizes for small detectors are
*        reduced in proportion to the detector size (the sector weights
*        are reduced by the same factor).                             []
*     TITLE = LITERAL (Read)
*        A title to be stored with the output image. The default is the
*        value of the TITLE component from the first input CRDD file. []
*     UNITS = LITERAL (Read)
*        The units in which the output image values are required.  See
*        help on "Data_units" for a list of the available units.  Note,
*        when the selected units represent flux (or flux density) per
*        pixel values, all pixels are considered to be the same size.
*        This may not be the case if a large area is being mapped (more
*        than a few degrees), resulting in erroneous output values. To
*        avoid this, users are recommended to use an area preserving
*        projection (such as AITOFF or LAMBERT, see parameter PROJTYPE)
*        in these cases.                                        [MJy/sr]
*     VAROUT = LITERAL (Read)
*        VAROUT specifies the type of output variances which are to be
*        calculated. These variances represent the uncertainty in each
*        pixel value in the output image, and are stored in the
*        VARIANCE component of the output NDF. Legal values for VAROUT
*        are INTERNAL, EXTERNAL or NONE. External is only allowed if
*        all the input CRDD files have associated VARIANCE components.
*        If EXTERNAL is specified, these externally calculated
*        variances are used as the basis for the calculation of output
*        variances.  If INTERNAL is specified, then the output
*        variances are estimated internally from the spread of CRDD
*        samples contributing to each output pixel. Users must be aware
*        of the warnings about such variances contained in the "Notes"
*        section below. A value of NONE for VAROUT causes no output
*        variances to be calculated, with a corresponding reduction in
*        run time.  If all input CRDD files have VARIANCES components,
*        then the run-time default for VAROUT is EXTERNAL. Otherwise
*        the run-time default is NONE.                                []
*     WEIGHT = _LOGICAL (Read)
*        If WEIGHT is set to a true value, then the total weight
*        assigned to each input data sample will be proportional to the
*        reciprocal of the sample variance value stored in the input
*        CRDD file. If WEIGHT is false, each full size detector sample
*        will have a total weight of unity (smaller detectors have
*        lower weight because they influence fewer pixels). If any
*        input CRDD files do not contain variance values then WEIGHT is
*        always given a false value.  Care should be taken when using
*        this option because bright samples usually have larger
*        variances.  Selecting this option would therefore cause small,
*        bright sources to be given lower weight than the surrounding
*        background regions, resulting in sources being made
*        systematically fainter.                                 [FALSE]

*  Examples:
*     MAPCRDD ^M51_SCANS M51_B1 COORDS=ECL ORIENT=10 HISTORY
*              QEXP=SOURCE.AND..NOT.SATURATED EXCLUDE=26-27
*
*        This will take the CRDD files listed in text file
*        M51_SCANS.DAT and produce an output map in M51_B1.SDF. The
*        output map will be oriented 10 degrees east of ecliptic north.
*        Only data from detector #47 with quality satisfying the
*        expression SOURCE.AND..NOT. SATURATED will be included in the
*        map. The quality names SOURCE and SATURATED must be set up by
*        the user using the application SETQUAL). The output MAP
*        includes a HISTORY component.

*  The Mapping Process:
*     A CRDD sample value represents the integrated flux over some
*     small area of the sky (defined by the detector Point Spread
*     Function).  When a sample value is divided by the detectors
*     effective solid angle, it becomes a surface brightness estimate.
*     Each pixel value in the output image is formed as the weighted
*     mean of all the sample surface brightness values in the
*     neighbourhood of the pixel. MAPCRDD provides two different
*     weighting schemes:
*
*     1) The sample has a constant weight at all pixels lying within
*     the area covered by the relevant detector mask at the moment the
*     sample was taken. The sample has zero weight at all other pixels.
*
*     2) The sample has a weight which varies as a Gaussian with the
*     distance between the detector centre and the pixel. The weights
*     are truncated to zero at the edges of the detector mask. The
*     Gaussian can have different widths in the in-scan and cross-scan
*     directions (see parameter FWHM).
*
*     The user can specify which scheme to use by means of the
*     parameter GAUSSIAN. Scheme 1 is very slightly faster, but scheme
*     2 results in less striping in the output image, and somewhat
*     higher resolution.
*
*     The algorithm proceeds for each data sample by first finding the
*     centre of the detector within the output image. The area covered
*     by the detector is then divided into rectangular "sectors" (see
*     parameter SECSIZE).  The sample weight within each sector is
*     found (it is considered to be constant across the sector), and
*     multiplied by the sample value. The output pixel closest to the
*     sector centre is found, and two images modified at that pixel.
*     The first image holds the sum of the weighted data samples
*     (expressed as surface brightness values), and the second hold the
*     sum of the weights. After this has been done for all data
*     samples, the first image is divided by the second.  Any pixels
*     which have zero weight are set to the "bad" value.  Each output
*     pixel value is thus the weighted mean of the neighbouring data
*     samples.

*  Reducing the Impact of Poor Data:
*     By default, each full size detector sample has a total weight of
*     unity (smaller detectors have lower weight because they influence
*     fewer pixels).  However, if all input data have associated
*     variance estimates, there is an option (see parameter WEIGHT) for
*     giving each sample a total weight proportional to the reciprocal
*     of the sample's variance. It should be borne in mind that
*     brighter samples usually have larger variances, so this option
*     will give lower weight to brighter samples, thus (for instance)
*     systematically lowering point source fluxes. Samples with zero
*     variance are treated like bad pixels if this option is selected.

*  Estimating the Noise in the Output Image:
*     If the input sample values have associated variance values, then
*     formal variance values can be defined for the output pixel values
*     (albeit these variance values will be correlated because of the
*     mapping algorithm). This is done by forming a third image holding
*     the sum of the weighted input variance values. The weights used
*     in this image are the square of the weights used in the first
*     image. The final variances are formed by dividing this third
*     image by the square of the second image. These variances represent
*     the expected error between the "true" sky AFTER CONVOLUTION WITH
*     THE DETECTOR PSFS, and the calculated surface brightness image.
*     It should be remembered that Starlink software assumes that the
*     NDF VARIANCE component holds variances related to independent,
*     Gaussian errors. This is not the case with IRAS data, so the
*     variance values should not be relied on too heavily.
*
*     There is an option for creating output variance values based on
*     the spread of input sample values at each pixel, rather than on
*     the variance value stored for each input sample (see parameter
*     VAROUT).  This facility allows output variances to be produced
*     even when the input CRDD files do not have any associated
*     variances. However, it must be emphasised that these variance
*     estimates include a contribution caused by spatial structure in
*     the image. For instance, the spread in input data values
*     contributing to an output pixel which is close to a bright point
*     source, will be completely dominated by the real variations
*     caused by the point source. They will have only a relatively
*     small contribution from the variations caused by noise. Caution
*     should therefore be shown when using these "internally
*     calculated" variance estimates.  Having said that, one benefit of
*     using internal variance values, is that the resulting output
*     variances will automatically include noise caused by residual
*     striping in the input data. This will not usually be the case if
*     input variance values are used. (This depends on how the input
*     variances were calculated.)

*  Colour Correction and Output Units:
*     The output image pixel values are not colour corrected and can be
*     in one of several different systems of units. The available units
*     are described in the help on "Data_units". The detector effective
*     solid angles and bandwidths used to produce the output values are
*     listed in the help on "Detector_solid_angles" and
*     "Detector_bandwidths". These values are also written to the
*     HISTORY component of the output NDF (see parameter HISTORY).

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-SEP-1990 (DSB):
*        Original version of header.
*     17-MAR-1992 (DSB):
*        IMAGE_INFO structure added to the output NDF's IRAS extension.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PAR_ERR'          ! PAR error values.
      INCLUDE 'MSG_PAR'          ! MSG parameters.
      INCLUDE 'I90_DAT'          ! IRAS90 constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRI_PAR'          ! IRI constants.
      INCLUDE 'NDF_PAR'          ! NDF constants.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER DD*2             ! Default detector number.
      CHARACTER DDLIST*(3*I90__DETS)! List of default detector numbers.
      CHARACTER OUNITS*20        ! Value of UNITS parameter.
      CHARACTER PLIST*(IRA__SZPLS)! List of supported projections.
      CHARACTER PROJ*(IRA__SZPRJ)! Requested projection.
      CHARACTER QEXP*254         ! Quality expression.
      CHARACTER SCS*(IRA__SZSCS)! Requested sky coordinate system.
      CHARACTER TITLE*50         ! Title from first good input CRDD
                                 ! file.
      CHARACTER VAROUT*8         ! Value of VAROUT parameter.


      DOUBLE PRECISION AC        ! Sky longitude of map centre.
      DOUBLE PRECISION AREF      ! Sky longitude of the reference point
                                 ! of the first good CRDD file.
      DOUBLE PRECISION BC        ! Sky latitude of map centre.
      DOUBLE PRECISION BREF      ! Sky latitude of the reference point
                                 ! of the first good CRDD file.
      DOUBLE PRECISION HEIGHT    ! "Height" of output image in
                                 ! arc-radians.
      DOUBLE PRECISION LPCBND(2) ! Lower pixel coordinate bounds on
                                 ! each image axis.
      DOUBLE PRECISION PIXSOL    ! Nominal pixel area in steradians.
      DOUBLE PRECISION PRJPAR(8) ! Projection parameters.
      DOUBLE PRECISION UPCBND(2) ! Upper pixel coordinate bounds on each
                                 ! image axis.
      DOUBLE PRECISION WIDTH     ! "Width" of output image in
                                 ! arc-radians.
      DOUBLE PRECISION XC        ! Pixel X coordinate of map centre.
      DOUBLE PRECISION YC        ! Pixel Y coordinate of map centre.


      INTEGER BAND0              ! IRAS waveband number (1-4) for
                                 ! the first good CRDD file.
      INTEGER CRDDF0             ! Index of first good CRDD file within
                                 ! the input group.
      INTEGER DETNO              ! Detector number.
      INTEGER DLIST( I90__MAXDT )! Detectors available in current wave
                                 ! band.
      INTEGER EXCLUD( I90__MAXDT + I90__NDEAD )! Detectors to exclude 
                                 ! from the map.
      INTEGER I                  ! Loop count.
      INTEGER IAT                ! Position of last non-blank character.
      INTEGER IDA                ! Identifier for Astrometry Info.
      INTEGER IDC0               ! IRC identifier for first good input
                                 ! CRDD file.
      INTEGER IGRP               ! Identifier for the group containing
                                 ! the names of the input CRDD files.
      INTEGER INCLUD( I90__MAXDT )! Detectors to include in the map.
      INTEGER INDF0              ! NDF identifier for first good input
                                 ! CRDD file.
      INTEGER IP                 ! Pointer to temporary workspace
                                 ! holding history text.
      INTEGER IPPWPS             ! Pointer to an array used to hold
                                 ! pointers to the pixel weight grids.
      INTEGER LBND(2)            ! Lower bounds on pixel indices.
      INTEGER MAXSOP             ! Max. SOP no. within output map.
      INTEGER MINSOP             ! Min. SOP no. within output map.
      INTEGER NCRDDF             ! No. of CRDD files in group identified
                                 ! by IGRP.
      INTEGER NDETAV             ! No. of live detectors in this
                                 ! waveband.
      INTEGER NDFOUT             ! Identifier for output NDF.
      INTEGER NEXCL              ! No. of detectors excluded from the
                                 ! map.
      INTEGER NINCL              ! No. of detectors included in the map.
      INTEGER NX                 ! No. of fractional X positions that
                                 ! the detector centre can take within
                                 ! the closest map pixel.
      INTEGER NY                 ! No. of fractional Y positions that
                                 ! the detector centre can take within
                                 ! the closest map pixel.
      INTEGER SIZE               ! No. of lines of history text to be
                                 ! added to the output NDF.
      INTEGER SOP                ! SOP no. of first usable CRDD file.
      INTEGER UBND(2)            ! Upper bounds on pixel indices.


      LOGICAL ALLVAR             ! True if all usable CRDD files have
                                 ! defined VARIANCE components.
      LOGICAL ANYVAR             ! True if any usable CRDD files have
                                 ! defined VARIANCE components.
      LOGICAL GAUSS              ! True if Gaussian weighting scheme is
                                 ! to be used.
      LOGICAL HIST               ! True if HISTORY is required for
                                 ! output NDF.
      LOGICAL WEIGHT             ! True if the input variances are to be
                                 ! used to modify each CRDD sample
                                 ! weight.


      REAL    BOXSIZ(2)          ! Map dimensions in radians.
      REAL    DEFPXS(4)          ! Default pixel size for each band.
      REAL    FWHM(2)            ! FWHMS of cross-scan and in-scan
                                 ! Gaussian weighting functions
                                 ! (radians).
      REAL    ORIENT             ! Position angle of second image axis
                                 ! in radians.
      REAL    PIXSIZ(2)          ! Pixel dimensions in radians.
      REAL    SECSIZ(2)          ! Cross- and in- scan sector sizes in
                                 ! radians.

*  Local Data:
      DATA DEFPXS / 0.25, 0.25, 0.5, 1.0 /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a value for parameter MSG_FILTER, and use it to establish the
*  conditional message filter level.
      CALL MSG_IFGET( 'MSG_FILTER', STATUS )

*  Get a value for parameter COORDS.
      CALL IRA_GTSCS( 'COORDS', .FALSE., SCS, STATUS )
      CALL MSG_BLANKIF( MSG__VERB, STATUS )
      CALL MSG_SETC( 'C', SCS )
      CALL MSG_OUTIF( MSG__VERB, 'MAPCRDD_MSG1',
     :               '  Using ^C sky coordinates.', STATUS )

*  Get a value for parameter ORIENT and convert to radians.
      CALL PAR_GDR0R( 'ORIENT', 0.0, 0.0, 360.0, .TRUE., ORIENT,
     :                STATUS )
      ORIENT = ORIENT*IRA__DTOR
      CALL MSG_SETR( 'O', ORIENT )
      CALL MSG_OUTIF( MSG__VERB, 'MAPCRDD_MSG2',
     :        '  Y axis is at a position angle of ^O degrees.', STATUS )

*  Get a value for parameter PROJTYPE.
      CALL IRA_IPROJ( PLIST, STATUS )
      CALL PAR_CHOIC( 'PROJTYPE', 'A bad projection name',  PLIST,
     :                .TRUE., PROJ, STATUS )
      CALL MSG_SETC( 'P', PROJ )
      CALL MSG_OUTIF( MSG__VERB, 'MAPCRDD_MSG3',
     :               '  Using ^P projection.', STATUS )

*  Get a value for parameter GAUSSIAN.
      CALL PAR_GET0L( 'GAUSSIAN', GAUSS, STATUS )
      IF( GAUSS ) THEN
         CALL MSG_OUTIF( MSG__VERB, 'MAPCRDD_MSG4',
     :                   '  Using Gaussian weights.', STATUS )
      ELSE
         CALL MSG_OUTIF( MSG__VERB, 'MAPCRDD_MSG5',
     :                   '  Using uniform weights.', STATUS )
      END IF

*  Get a value for parameter UNITS.
      CALL IRI_GETUN( 'UNITS', IRI__MJPS, OUNITS, STATUS )
      CALL MSG_SETC( 'U', OUNITS )
      CALL MSG_OUTIF( MSG__VERB, 'MAPCRDD_MSG6',
     :                '  Output map will be in units of ^U.', STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Initialise the IRC_ and IRA_ systems.
      CALL IRC_INIT( STATUS )
      CALL IRA_INIT( STATUS )

*  Get an NDF Group containing all the input CRDD files.
      CALL MSG_BLANKIF( MSG__NORM, STATUS )
      CALL IRM_RDNDF( 'IN', 0, 1, 'Give more CRDD file names',
     :                IGRP, NCRDDF, STATUS )

*  If the group contains no names, or if an error occured, abort.
      IF ( NCRDDF .EQ. 0 .OR. STATUS .NE. SAI__OK ) GO TO 999

*  Find the index of the first good CRDD file in the input group,
*  together with the IRAS band number (1-4) of the data within it, the
*  sky coordinates of it's reference point, it's title, an NDF
*  identifier, and an IRC identifier.
      CALL MAPCA0( IGRP, NCRDDF, SCS, CRDDF0, BAND0, AREF, BREF, TITLE,
     :             INDF0, IDC0, SOP, STATUS )
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Initialise the maximum and minimum SOP numbers included in the output
*  map.
      MAXSOP = SOP
      MINSOP = SOP

*  Conditionally display the waveband.
      CALL MSG_BLANKIF( MSG__VERB, STATUS )
      CALL MSG_SETI( 'WL', I90__WAVEL( BAND0 ) )
      CALL MSG_OUTIF( MSG__VERB, 'MAPCRDD_MSG7',
     :               '  Input CRDD files contain ^WL um data.', STATUS )

*  Produce a list of detectors available in the required waveband,
*  excluding any dead detectors.
      NDETAV = 0

      DO I = 1, I90__NDETS( BAND0 )
         DETNO = I90__BDETS( I, BAND0 )

         IF( I90__SOLAN( DETNO ) .NE. 0.0 ) THEN
            NDETAV = NDETAV + 1
            DLIST( NDETAV ) = DETNO
         END IF

      END DO

*  Get a list of detector numbers to exclude, selected from the list of
*  detectors available in the waveband of the first good CRDD file.
      CALL IRM_GTDET( 'EXCLUDE', NDETAV, DLIST, EXCLUD, NEXCL, STATUS )

*  Convert this into a list of detector numbers to include.
      CALL IRM_CVDET( NDETAV, DLIST, NEXCL, EXCLUD, INCLUD, NINCL,
     :                STATUS )

*  Get a list of detector numbers to include. The dynamic default is
*  the list calculated on the basis of the EXCLUDE parameter.
      IAT = 0
      DO I = 1, NINCL
         WRITE( DD, '(I2)' ) INCLUD( I )
         CALL CHR_APPND( DD//',', DDLIST, IAT )
      END DO

      CALL PAR_DEF0C( 'INCLUDE', DDLIST( : IAT - 1 ), STATUS )
      CALL IRM_GTDET( 'INCLUDE', NDETAV, DLIST, INCLUD, NINCL, STATUS )

*  Conditionally display the detector numbers being included.
      IF( NINCL .GT. 0 ) THEN
         CALL MSG_SETC( 'LIST', '#' )
         CALL MSG_SETI( 'LIST', INCLUD( 1 ) )

         DO I =2, NINCL
            CALL MSG_SETC( 'LIST', ', ' )
            CALL MSG_SETC( 'LIST', '#' )
            CALL MSG_SETI( 'LIST', INCLUD( I ) )
         END DO

         CALL MSG_OUTIF( MSG__VERB, 'MAPCRDD_MSG8',
     :             '  Data being used from detectors:- ^LIST.', STATUS )

*  If no detectors are to be included in the map, report an error.
      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'MAPCRDD_ERR1',
     :     'MAPCRDD: All available detectors have been excluded from '//
     :     'the map.', STATUS )
         GO TO 999
      END IF

*  Get the required pixel dimensions (in radians), using different
*  default values for each waveband and ensuring that each dimension is
*  greater than 0.02 arc-minutes (=5.8E-6 radians).
      CALL MSG_BLANKIF( MSG__VERB, STATUS )
      PIXSIZ( 1 ) = IRA__DTOR*DEFPXS( BAND0 )/60.0
      PIXSIZ( 2 ) = IRA__DTOR*DEFPXS( BAND0 )/60.0
      CALL IRM_DIMEN( 'PIXSIZE', .TRUE., 5.8E-6, PIXSIZ, STATUS )

*  Store the nominal pixel area, in steradians.
      PIXSOL = DBLE( PIXSIZ( 1 ) )*DBLE( PIXSIZ( 2 ) )

*  Create temporary mappings between image coordinates and sky
*  coordinates, using the requested projection and sky coordinate
*  system. The reference point for the image frame is taken to have the
*  same sky coordinates as the reference point for the first good input
*  CRDD file, and image coordinates (0,0).
      PRJPAR( 1 ) = AREF
      PRJPAR( 2 ) = BREF
      PRJPAR( 3 ) = 0.0D0
      PRJPAR( 4 ) = 0.0D0
      PRJPAR( 5 ) = DBLE( PIXSIZ( 1 ) )
      PRJPAR( 6 ) = DBLE( PIXSIZ( 2 ) )
      PRJPAR( 7 ) = DBLE( ORIENT )
      PRJPAR( 8 ) = DBLE( ORIENT )
      CALL IRA_CREAT( PROJ, 8, PRJPAR, SCS, IRA__IRJEP, NDF__NOID, IDA,
     :                STATUS )

*  Obtain the bounds of the image which will include all the data from
*  the good CRDD files in the input group. Any CRDD files which cannot
*  be accessed are removed from the group identified by the returned
*  value of IGRP.  Also see if all, or any, CRDD files contain defined
*  variance values.
      CALL MAPCA2( IDC0, INDF0, CRDDF0, BAND0, IDA, NINCL, INCLUD,
     :             IGRP, NCRDDF, MAXSOP, MINSOP, LPCBND, UPCBND, ALLVAR,
     :             ANYVAR, STATUS )

*  Conditionally display the number of CRDD files being mapped.
      CALL MSG_BLANKIF( MSG__NORM, STATUS )
      IF( NCRDDF .EQ. 1 ) THEN
         CALL MSG_OUTIF( MSG__NORM, 'MAPCRDD_MSG9',
     :                   '  Only 1 CRDD file being mapped.', STATUS )
      ELSE
         CALL MSG_SETI( 'NCRDDF', NCRDDF )
         CALL MSG_OUTIF( MSG__NORM, 'MAPCRDD_MSG10',
     :                   '  ^NCRDDF CRDD files being mapped.', STATUS )
      END IF
      CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Annul the NDF and IRC identifiers for the first good CRDD file.
      CALL IRC_ANNUL( IDC0, STATUS )
      CAll NDF_ANNUL( INDF0, STATUS )

*  Abort if an error has been reported.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Find the image coordinates of the centre of the image frame which
*  encloses all the input data.
      XC = 0.5*( LPCBND( 1 ) + UPCBND( 1 ) )
      YC = 0.5*( LPCBND( 2 ) + UPCBND( 2 ) )

*  Convert them to sky coordinates. These are the default image centre
*  coordinates.
      CALL IRA_TRANS( 1, XC, YC, .TRUE., SCS, IDA, AC, BC, STATUS )

*  Get values for the image centre sky coordinates using parameters
*  CENTRE_LON and CENTRE_LAT.
      CALL IRA_GETCO( 'CENTRE_LON', 'CENTRE_LAT',
     :                ' of the image centre', SCS, .TRUE., AC, BC,
     :                STATUS )

*  Calculate the image coordinates of the requested image centre.
      CALL IRA_TRANS( 1, AC, BC, .FALSE., SCS, IDA, XC, YC, STATUS )

*  Annul the IRA identifier for the temporary astrometry information.
      CALL IRA_ANNUL( IDA, STATUS )

*  Modify the projection parameters to describe mappings between image
*  and sky coordinates, which have the reference point (pixel (0,0))
*  at the requested image centre.
      PRJPAR( 1 )  = AC
      PRJPAR( 2 )  = BC

*  Create a new IRA identifier for the modified astrometry information.
      CALL IRA_CREAT( PROJ, 8, PRJPAR, SCS, IRA__IRJEP, NDF__NOID, IDA,
     :                STATUS )

*  Update the pixel bounds of the image frame so that all the data will
*  still lie within it, given that the image centre ( pixel (0,0) ) is
*  at the requested sky coordinates. Max and min bounds will be equal
*  and opposite since pixel (0,0) is at the centre.
      UPCBND( 1 ) = MAX( UPCBND( 1 ) - XC, XC - LPCBND( 1 ) )
      LPCBND( 1 ) = -UPCBND( 1 )
      UPCBND( 2 ) = MAX( UPCBND( 2 ) - YC, YC - LPCBND( 2 ) )
      LPCBND( 2 ) = -UPCBND( 2 )

*  Calculate the height and width (in arc-radians) of the image which
*  just encompasses all the input data.
      CALL MAPCA3( IDA, SCS, LPCBND, UPCBND, WIDTH, HEIGHT, STATUS )

*  Convert to single precision.
      BOXSIZ( 1 ) = REAL( WIDTH )
      BOXSIZ( 2 ) = REAL( HEIGHT )

*  Get the required image dimensions (in radians), using parameter
*  BOXSIZE, ensuring that each axis contains at least two pixels.
      CALL IRM_DIMEN( 'BOXSIZE', .TRUE.,
     :                2.0*MAX( PIXSIZ( 1 ), PIXSIZ( 2 ) ), BOXSIZ,
     :                STATUS )

*  Find the image bounds which produce an image with the requested
*  width and height.
      CALL IRA_XYLIM( IDA, AC, BC, DBLE( BOXSIZ( 1 ) ), 
     :                DBLE( BOXSIZ( 2 ) ), LPCBND, UPCBND, STATUS )

*  Convert the pixel coordinate image bounds to pixel indices.
      LBND( 1 ) = INT( LPCBND( 1 ) ) - 1
      UBND( 1 ) = INT( UPCBND( 1 ) ) + 1
      LBND( 2 ) = INT( LPCBND( 2 ) ) - 1
      UBND( 2 ) = INT( UPCBND( 2 ) ) + 1

*  Create an NDF to hold the output image.  The array components have
*  the pixel bounds found above.
      CALL NDF_CREAT( 'OUT', '_REAL', 2, LBND, UBND, NDFOUT, STATUS )

*  Conditionally display the the bounds of the output image.
      CALL MSG_BLANKIF( MSG__VERB, STATUS )
      CALL MSG_SETI( 'XLO', LBND( 1 ) )
      CALL MSG_SETI( 'YLO', LBND( 2 ) )
      CALL MSG_SETI( 'XHI', UBND( 1 ) )
      CALL MSG_SETI( 'YHI', UBND( 2 ) )
      CALL MSG_OUTIF( MSG__VERB, 'MAPCRDD_MSG11',
     : '  Output image has bounds ( ^XLO:^XHI, ^YLO:^YHI ).', STATUS )

*  Get a value for parameter TITLE and add it to the output NDF.
      IF( TITLE .NE. ' ' ) CALL PAR_DEF0C( 'TITLE', TITLE, STATUS )
      CALL NDF_CINP( 'TITLE', NDFOUT, 'TITLE', STATUS )

*  Set the LABEL component of the output NDF to "Surface Brightness"
      CALL NDF_CPUT( 'Surface Brightness', NDFOUT, 'LABEL', STATUS )

*  Create the IRAS NDF extension, and get an IRA identifier to the
*  astrometry information.
      CALL MAPCF2( NDFOUT, IDA, SCS, AREF, BREF, BAND0, MAXSOP,
     :             MINSOP, OUNITS, STATUS )

*  If required, get values for parameter FWHM in radians.
      IF( GAUSS ) THEN
         FWHM( 1 ) = IRA__DTOR*I90__DZ( BAND0 )/60.0
         FWHM( 2 ) = IRA__DTOR*I90__DY( BAND0 )/60.0
         CALL IRM_DIMEN( 'FWHM', .TRUE., 1.0E-5, FWHM, STATUS )

         CALL MSG_SETR( 'FC',REAL( FWHM( 1 )*IRA__RTOD*60.0 ) )
         CALL MSG_SETR( 'FI', REAL( FWHM( 2 )*IRA__RTOD*60.0 ) )
         CALL MSG_OUTIF( MSG__VERB, 'MAPCRDD_MSG12',
     : '  Gaussian weights have in- and cross- scan FWHMs of ^FI and '//
     :   '^FC arcmins.', STATUS )

*  If the Gaussian weighting scheme is not being used, set both FWHM
*  values to zero.
      ELSE
         FWHM( 1 ) = 0.0
         FWHM( 2 ) = 0.0

      END IF

*  Get values for parameter SECSIZE in radians.
      SECSIZ( 1 )  = 0.5*MIN( PIXSIZ( 1 ), PIXSIZ( 2 ) )
      SECSIZ( 2 ) = SECSIZ( 1 )
      CALL IRM_DIMEN( 'SECSIZE', .TRUE., 1.0E-5, SECSIZ, STATUS )

*  Limit sector dimensions to be smaller than the pixel dimensions.
      IF( SECSIZ( 1 ) .GT. PIXSIZ( 1 ) .OR.
     :    SECSIZ( 2 ) .GT. PIXSIZ( 2 ) ) THEN

         SECSIZ( 1 ) = PIXSIZ( 1 )
         SECSIZ( 2 ) = PIXSIZ( 2 )

         CALL MSG_OUTIF( MSG__QUIET, 'MAPCRDD_MSG13',
     :          'WARNING: Sectors must be smaller than pixels', STATUS )

         CALL MSG_SETR( 'SS1', REAL( SECSIZ( 1 )*IRA__RTOD*60.0 ) )
         CALL MSG_SETR( 'SS2', REAL( SECSIZ( 2 )*IRA__RTOD*60.0 ) )
         CALL MSG_OUTIF( MSG__QUIET, 'MAPCRDD_MSG14',
     :                '  Using sectors dimensions ^SS1 x ^SS2 arc-mins',
     :                   STATUS )

      ELSE
         CALL MSG_SETR( 'SS1', REAL( SECSIZ( 1 )*IRA__RTOD*60.0 ) )
         CALL MSG_SETR( 'SS2', REAL( SECSIZ( 2 )*IRA__RTOD*60.0 ) )
         CALL MSG_OUTIF( MSG__VERB, 'MAPCRDD_MSG15',
     :               '  Using sectors dimensions ^SS1 x ^SS2 arc-mins.',
     :                   STATUS )

      END IF

*  Determine the number of pixel weights grids to be used. This equals
*  the number of sectors in a pixel.
      NX = INT( PIXSIZ( 1 )/SECSIZ( 1 ) ) + 1
      NY = INT( PIXSIZ( 2 )/SECSIZ( 2 ) ) + 1

*  Conditionally display the number of pixel weight grids being used.
      CALL MSG_SETI( 'NX', NX )
      CALL MSG_SETI( 'NY', NY )
      CALL MSG_OUTIF( MSG__VERB, 'MAPCRDD_MSG16',
     :               '  Using ^NX by ^NY pixel weight grids.', STATUS )

*  Get memory to hold the pointers to the grids.
      CALL PSX_CALLOC( NX*NY, '_INTEGER', IPPWPS, STATUS )

*  If some but not all input CRDD files had VARIANCE components,
*  conditionally warn the user that no variance values will be used.
      IF( ANYVAR .AND. .NOT. ALLVAR ) THEN
         CALL MSG_BLANKIF( MSG__QUIET, STATUS )
         CALL MSG_OUTIF( MSG__QUIET, 'MAPCRDD_MSG17',
     :   'WARNING: Some input CRDD files did not contain VARIANCE '//
     :   'values.', STATUS )
         CALL MSG_OUTIF( MSG__QUIET, 'MAPCRDD_MSG18',
     :   '          No input VARIANCE values will be used.', STATUS )
         CALL MSG_BLANKIF( MSG__QUIET, STATUS )
      END IF

*  If all input CRDD files contained VARIANCE components, get a value
*  for the parameter WEIGHT.
      IF( ALLVAR ) THEN
         CALL PAR_GET0L( 'WEIGHT', WEIGHT, STATUS )

*  If all the input CRDD files had VARIANCE components, get a value for
*  parameter VAROUT suggesting a default of EXTERNAL.
         CALL PAR_CHOIC( 'VAROUT', 'EXTERNAL', 'EXTERNAL,NONE,INTERNAL',
     :                   .TRUE., VAROUT, STATUS )

*  If any of the input CRDD files did not have VARIANCE components, set
*  WEIGHT to FALSE, and then get a value for parameter VAROUT suggesting
*  a default of NONE (EXTERNAL variances cannot be created in this
*  case).
      ELSE
         WEIGHT = .FALSE.
         CALL PAR_CHOIC( 'VAROUT', 'NONE', 'NONE,INTERNAL', .TRUE.,
     :                   VAROUT, STATUS )

      END IF

*  Conditionally, display the weighting method.
      IF( WEIGHT ) THEN
         CALL MSG_OUTIF( MSG__VERB, 'MAPCRDD_MSG19',
     :              '  Giving greater weight to more accurate samples.',
     :                   STATUS )
      ELSE
         CALL MSG_OUTIF( MSG__VERB, 'MAPCRDD_MSG20',
     :        '  Giving total weight of unity to all samples.', STATUS )
      END IF

*  Conditionally, display the type of variances being produced.
      IF( VAROUT .EQ. 'NONE' ) THEN
         CALL MSG_OUTIF( MSG__VERB, 'MAPCRDD_MSG21',
     :        '  No output variances being calculated.', STATUS )
      ELSE
         CALL MSG_SETC( 'V', VAROUT )
         CALL MSG_OUTIF( MSG__VERB, 'MAPCRDD_MSG22',
     :        '  ^V output variances being calculated.', STATUS )
      END IF

*  Get a quality expression. The expression is checked for syntax errors
*  and converted to upper case, leading blanks are removed.
      CALL IRM_GETQX( 'QEXP', QEXP, STATUS )
      CALL MSG_SETC( 'Q', QEXP )
      CALL MSG_OUTIF( MSG__VERB, 'MAPCRDD_MSG23',
     : '  Samples which satsify quality expression "^Q" being mapped.',
     :    STATUS)

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Create the output DATA and (if required) VARIANCE arrays.
      CALL MAPCA5( IGRP, IDA, NCRDDF, WEIGHT, VAROUT, NDFOUT, LBND,
     :             UBND, BAND0, SECSIZ, FWHM, NINCL, INCLUD,
     :             QEXP, OUNITS, NX, NY, PIXSOL, %VAL( IPPWPS ),
     :             STATUS )

*  See if HISTORY is required in output NDF.
      CALL PAR_GET0L( 'HISTORY', HIST, STATUS )

*  If it is, get sufficient workspace to hold the history text.
      IF( HIST ) THEN
         CALL MSG_OUTIF( MSG__VERB, 'MAPCRDD_MSG24',
     :                  '  HISTORY being added to output NDF.', STATUS )

         SIZE = NCRDDF + NINCL + 20
         CALL PSX_CALLOC( 80*SIZE, '_CHAR', IP, STATUS )

*  Convert the point to the storage area into a pointer to a character
*  descriptor so that the mapped array can be passed using %VAL (on UNIX
*  systems this has no effect).
         CALL IRM_CDESC( 80, IP, STATUS )

*  Abort if an error has occured.
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Set up the text and add it to the output NDF. NB, the final argument
*  is needed to be consistent with the method used by UNIX for passing
*  pointers to mapped character arrays. There is no dummy argument
*  within MAPCC0 corresponding to this argument.
         CALL MAPCC0( NDFOUT, IGRP, NCRDDF, NINCL, INCLUD, BAND0, SIZE,
     :                %VAL( IP ), STATUS, %VAL( 80 ) )

*  Convert the pointer back into its original form so that the workspace
*  can be released.
         CALL IRM_CPOIN( IP, STATUS )

*  Release the workspace.
         CALL PSX_FREE( IP, STATUS )

      ELSE
         CALL MSG_OUTIF( MSG__VERB, 'MAPCRDD_MSG25',
     :               '  No HISTORY being added to output NDF.', STATUS )

      END IF

*  Close down the IRA, IRC, and IRQ systems.
 999  CONTINUE

      CALL IRA_CLOSE( STATUS )
      CALL IRC_CLOSE( STATUS )
      CALL IRQ_CLOSE( STATUS )

*  If an error has occured, attempt to delete the output NDF.
      IF( STATUS .NE. SAI__OK ) CALL NDF_DELET( NDFOUT, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  Free the memory used to store the pointers to the pixel weight grids.
      CALL PSX_FREE( IPPWPS, STATUS )

*  Delete the group used to hold the input CRDD file names.
      CALL GRP_DELET( IGRP, STATUS )

*  If a parameter null or abort error exists, annul it.
      IF( STATUS .EQ. PAR__NULL .OR. STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'MAPCRDD_ERR2',
     :   'MAPCRDD: Error mapping IRAS CRDD into an image.',
     :   STATUS )
      END IF

      END
