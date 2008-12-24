      SUBROUTINE SKYALIGN( STATUS )
*+
*  Name:
*     SKYALIGN

*  Purpose:
*     Re-map a group of images so that they are aligned.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SKYALIGN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine re-projects a group of images so that they are
*     aligned pixel-for-pixel with another specified reference image.
*     Alternatively, instead of giving a reference image, the user can
*     supply the parameters which explicitly describe the projection
*     required for the output images (see parameter REF).
*
*     Two methods exist for determining the bounds of the output
*     images.  Firstly, the user can give values for parameters XY1
*     and XY2 which are then used as the bounds for all output images.
*     Secondly, if a null value is given for XY1 or XY2 , default
*     values are generated separately for each output image so that the
*     output image just encloses the entire area covered by the
*     corresponding input image. Using the first method will ensure
*     that all output images have the same pixel origin, and so the
*     resulting images can be directly compared. However, this may
*     result in the output images being larger than necessary. In
*     general, the second method results in smaller images being
*     produced, in less time. However, the output images will have
*     differing pixel origins which need to be taken into account when
*     comparing the aligned images.
*     
*     The output image values are formed by re-sampling the input image
*     values using either nearest neighbour or bi-linear interpolation
*     (see parameter METHOD). If the former method is used, then any
*     QUALITY and VARIANCE values which are present in the input NDFs
*     are copied to the corresponding positions in the output NDFs. If
*     bi-linear interpolation is used, then the output NDFs contain no
*     QUALITY or VARIANCE values.

*  Usage:
*     SKYALIGN IN REF XY1 XY2 OUT

*  ADAM Parameters:
*     ACC = REAL (Read)
*        The positional accuracy required, as a a number of pixels. For
*        highly non-linear projections, a recursive algorithm is used in
*        which succesively smaller regions of the projection are fitted 
*        with a least squares linear transformation. If such a 
*        transformation results in a maximum positional error greater 
*        than the value supplied for ACC (in pixels), then a smaller 
*        region is used. High accuracy is paid for by larger run times.
*                                                                  [0.5]
*     CENTRE_LAT = LITERAL (Read)
*        The sky latitude of the projection centre for the output
*        images, in the coordinate system specified by the parameter
*        COORDS (eg if COORDS is EQUATORIAL then CENTRE_LAT should be
*        given the Declination of the image centre). See help on
*        "Sky_coordinates" for the formats allowed for this value. This
*        parameter is only used if the parameter REF is given a null
*        value.
*     CENTRE_LON = LITERAL (Read)
*        The sky longitude of the projection centre for the output
*        images, in the coordinate system specified by the parameter
*        COORDS (eg if COORDS is EQUATORIAL then CENTRE_LON should be
*        given the Right Ascension of the image centre).See help on
*        "Sky_coordinates" for the formats allowed for this value. This
*        parameter is only used if the parameter REF is given a null
*        value.
*     CENTRE_XY = _REAL (Read)
*        A pair of (X,Y) pixel coordinates giving the projection centre
*        for the output images. The default values will put the
*        projection centre at the origin of the pixel coordinate
*        system. This parameter is only used if the parameter REF is
*        given a null value.                                   [0.0,0.0]
*     COORDS = LITERAL (Read)
*        Specifies the coordinate system used for referring to sky
*        positions. Valid values include ECLIPTIC, EQUATORIAL,
*        GALACTIC. See help on "Sky_coordinates" for more information
*        on available sky coordinate systems. This parameter is only
*        used if the parameter REF is given a null value.
*                                        [current sky coordinate system]
*     EPOCH = DOUBLE PRECISION (Read)
*        The Julian epoch at which the input images were obtained. The
*        same value is used for all the input images.  This parameter
*        is only accessed if the parameter REF is given a null value.
*        A value of 1983.5 is acceptable for all IRAS data.
*     HISTORY = _LOGICAL (Read)
*        Determines if history information is to be stored within the
*        output NDFs. See help on "History_in_IRAS90" for more
*        information on history.               [current history setting]
*     IN = NDF (Read)
*        A group of input NDFs, each containing an image with
*        associated astrometry information. This should be in the form
*        of a group expression (see help on "Group_expressions").
*     METHOD = LITERAL (Read)
*        The method to use when sampling the input pixel values, in
*        order to find the corresponding output pixel value. It can
*        take the following values:
*
*        o  BILINEAR - This causes the output pixel values to be
*        calculated by bilinear interpolation among the four nearest
*        pixels values in the input image, resulting in smoother output
*        images. If this method is selected then there is no one-to-one
*        correspondance between input and output pixels and so no
*        QUALITY or VARIANCE components will be propagated from the
*        input to the output NDFs.
*
*        o  NEAREST - This causes the output pixel values to be
*        assigned the value of the single nearest input pixels If this
*        method is selected then QUALITY or VARIANCE components are
*        propagated from the input to the output NDFs. This method is
*        faster than BILINEAR.                                 [NEAREST]
*     MSG_FILTER = LITERAL (Read)
*        The level of information displayed on the users screen. This
*        should take one of the values QUIET, NORMAL or VERBOSE (see
*        help on "Message_filtering").
*                                       [current message filter setting]
*     PIXSIZE = _REAL (Read)
*        The dimensions of a pixel at the centre of the output
*        projection, in arc-minutes.  If a single value is given for
*        PIXSIZE, then the output pixels are square. If two values are
*        given then the output pixels are rectangular, having the
*        specified dimensions. It should be remembered that the actual
*        pixel size may vary slightly across the output projection
*        depending on the type of projection. The values specified by
*        parameter PIXSIZE give the actual pixel dimensions at the
*        projection centre. This parameter is only used if the
*        parameter REF is given a null value.
*     PROJTYPE = LITERAL (Read)
*        Specifies the type of projection to use when creating the
*        output images. Valid values include GNOMONIC (i.e. tangent
*        plane), ORTHOGRAPHIC, LAMBERT and AITOFF.  See help on
*        "Map_projections" for more information on available
*        projections. This parameter is only used if the parameter REF
*        is given a null value.
*     REF = NDF (Read)
*        An NDF containing an image to which all the images specified
*        by parameter IN are to be aligned. If a null value is supplied
*        for this parameter, then the values supplied for parameters
*        PROJTYPE, CENTRE_LON, CENTRE_LAT, CENTRE_XY, PIXSIZE and
*        COORDS are used to define the pixel grid to which output
*        images are aligned.
*     ORIENT = _REAL (Read)
*        The position angle of the second axis (or "Y" axis) of the
*        output image grid, in degrees.  That is, the angle from north
*        (defined by the coordinate system specified by parameter
*        COORDS) to the image "Y" axis ("upwards" if the image is
*        displayed normally).  Positive angles are measured in the same
*        sense as rotation from north through east.  The default value
*        of zero puts north "upwards". This parameter is only used if
*        the parameter REF is given a null value.                  [0.0]
*     OUT = NDF (Write)
*        A group of output NDFs corresponding one-for-one with the list
*        of input NDFs given for parameter IN.  This should be in the
*        form of a group expression (see help on "Group_expressions").
*        Expressions such as "*_AL" are expanded by replacing the "*"
*        character with each input NDF name in turn.
*     XY1 = INTEGER (Read)
*        A pair of values giving the pixel indices of the lower left
*        corner of the output images. The same values are used for all
*        output images.  NDFs.  If a null value is given then separate
*        default values are calculated for each output NDF.
*     XY2 = INTEGER (Read)
*        A pair of values giving the pixel indices of the upper right
*        corner of the output images. The same values are used for all
*        output images.  NDFs.  If a null value is given then separate
*        default values are calculated for each output NDF.

*  Examples:
*     SKYALIGN IMAGE1 IMAGE2 [-100,-10] [200,100] *_AL 
*        This example re-samples the image contained in IMAGE1 so that
*        it is aligned with the image contained in IMAGE2, putting the
*        output in IMAGE1_AL. The output image covers pixel coordinates
*        between -100 and 200 in X, and -10 and 100 in Y.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-FEB-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants
      INCLUDE 'NDF_PAR'          ! NDF_ constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants.
      INCLUDE 'IRA_PAR'          ! IRA_ constants.
      INCLUDE 'GRP_PAR'          ! GRP_ constants.

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Returns used length of a character string.

*  Local Variables:
      CHARACTER
     :          COMC*1,             ! Output group comment character.
     :          METHOD*8,           ! Interpolation method to use.
     :          NDFNAM*(GRP__SZNAM),! The name of an NDF.
     :          PLIST*(IRA__SZPLS), ! List of supported projections.
     :          PROJ*(IRA__SZPRJ),  ! Projection to use for output.
     :          SCS*(IRA__SZSCS),   ! Sky coordinate system to use.
     :          TEXT*(GRP__SZNAM)   ! Text to be stored in a group.


      DOUBLE PRECISION
     :          ACEN,            ! Sky longitude of projection centre.
     :          BCEN,            ! Sky latitude of projection centre.
     :          EPOCH,           ! Julian epoch of observations.
     :          P( 8 )           ! Projection parameters.

      INTEGER
     :          I,               ! Index into input and output groups.
     :          IDAR,            ! IRA identifier for reference
     :                           ! astrometry information.
     :          IGRP1,           ! Identifier for group holding input
     :                           ! NDFs.
     :          IGRP2,           ! Identifier for group holding all
     :                           ! candidate output NDFs.
     :          INDF1,           ! Identifier for the input NDF.
     :          INDF2,           ! Identifier for the output NDF.
     :          INDFR,           ! Identifier for the reference NDF.
     :          NOUT,            ! No. of good output NDFs.
     :          SIZE,            ! Total size of the input group.
     :          SIZEO,           ! Total size of the output group.
     :          TLEN,            ! Used length of text string.
     :          XY1( 2 ),        ! Indices of lower left corner of
     :                           ! outputs.
     :          XY2( 2 )         ! Indices of upper right corner of
                                 ! outputs.


      LOGICAL
     :          THERE            ! True if an object was found.


      REAL
     :          ERRLIM,          ! Positional accuracy in pixels.
     :          ORIENT,          ! Position angle of output Y axis.
     :          PIXSIZ( 2 ),     ! Pixel dimensions in output.
     :          XYCEN( 2 )       ! Image coordinates of projection
                                 ! centre.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Establish the conditional message filter level using parameter
*  MSG_LEVEL.
      CALL MSG_IFGET( 'MSG_FILTER', STATUS )

*  Get a group containing the names of the NDFs to be processed.
      CALL IRM_RDNDF( 'IN', 0, 1, '  Give more NDF names...', 
     :                IGRP1, SIZE, STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Start up the IRA astrometry package.
      CALL IRA_INIT( STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get the reference image.
      CALL NDF_ASSOC( 'REF', 'READ', INDFR, STATUS )

*  If an NDF was supplied, get an IRA identifier for the astrometry
*  information stored in it, and get the stored sky coordinate system.
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL IRA_IMPRT( INDFR, IDAR, STATUS )
         CALL IRA_SCSEP( IDAR, SCS, EPOCH, STATUS )

*  If a null value was supplied, annul the error 
      ELSE IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

*  Get values for the parameters which define the output projection...
*  Sky coordinate system.
         CALL IRA_GTSCS( 'COORDS', .FALSE., SCS, STATUS )

*  Sky coordinates of projection centre.
         CALL IRA_GETCO( 'CENTRE_LON', 'CENTRE_LAT',
     :                   ' of the projection centre', SCS, .TRUE., ACEN,
     :                   BCEN, STATUS )

*  Image coordinates of projection centre.
         CALL PAR_EXACR( 'CENTRE_XY', 2, XYCEN, STATUS )

*  Pixel dimensions.
         CALL IRM_DIMEN( 'PIXSIZE', .FALSE., 0.0, PIXSIZ, STATUS )

*  Position angle of Y axis.
         CALL PAR_GDR0R( 'ORIENT', 0.0, 0.0, 360.0, .TRUE., ORIENT,
     :                   STATUS )
         ORIENT = ORIENT*IRA__DTOR

*  Projection type.
         CALL IRA_IPROJ( PLIST, STATUS )
         CALL PAR_CHOIC( 'PROJTYPE', 'A bad projection name',  PLIST,
     :                   .TRUE., PROJ, STATUS )

*  Epoch of observations.
         CALL PAR_GET0D( 'EPOCH', EPOCH, STATUS )

*  Get an IRA identifier for this astrometry information.
         P( 1 ) = ACEN
         P( 2 ) = BCEN
         P( 3 ) = DBLE( XYCEN( 1 ) )
         P( 4 ) = DBLE( XYCEN( 2 ) )
         P( 5 ) = DBLE( PIXSIZ( 1 ) )
         P( 6 ) = DBLE( PIXSIZ( 2 ) )
         P( 7 ) = DBLE( ORIENT )
         P( 8 ) = 0.0D0
         CALL IRA_CREAT( PROJ, 8, P, SCS, EPOCH, NDF__NOID, IDAR,
     :                   STATUS )

      END IF

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get the bounds required for the output images.
      CALL PAR_EXACI( 'XY1', 2, XY1, STATUS )
      CALL PAR_EXACI( 'XY2', 2, XY2, STATUS )

*  If a null value was supplied for XY1 or XY2, annul the error and
*  put bad values in them.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         XY1( 1 ) = VAL__BADI
         XY1( 2 ) = VAL__BADI
         XY2( 1 ) = VAL__BADI
         XY2( 2 ) = VAL__BADI
      END IF

*  Get a group containing the names of the output NDFs.  Base
*  modification elements on the group containing the input NDFs.
      CALL IRM_WRNDF( 'OUT', IGRP1, SIZE, SIZE,
     :                '  Give more NDF names...',
     :                 IGRP2, SIZEO, STATUS )

*  Get the character used to introduce comments into group expressions
*  relating to the output group.
      CALL GRP_GETCC( IGRP2, 'COMMENT', COMC, STATUS )

*  Get the interpolation method to be used.
      CALL PAR_CHOIC( 'METHOD', 'NEAREST',  'NEAREST,BILINEAR', .TRUE.,
     :                METHOD, STATUS )

*  Get the positional accuracy required.
      CALL PAR_GET0R( 'ACC', ERRLIM, STATUS )      
      ERRLIM = MAX( 0.0001, ERRLIM )

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Loop round each NDF to be processed.
      NOUT = 0
      DO I = 1, SIZE
         CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Get an NDF identifier for the input NDF.
         CALL NDG_NDFAS( IGRP1, I, 'READ', INDF1, STATUS )

*  Tell the user which input NDF is currently being procesed.
         CALL NDF_MSG( 'NDF', INDF1 )
         CALL MSG_OUTIF( MSG__NORM, 'SKYALIGN_MSG1',
     :                   '  Processing ^NDF...', STATUS )

*  Create the output NDF by propagation from the input NDF. The default
*  components HISTORY, TITLE, LABEL and all extensions are propagated,
*  together with the UNITS component. The NDF is initially created with
*  the same bounds as the input NDF.
         CALL NDG_NDFPR( INDF1, 'UNITS', IGRP2, I, INDF2, STATUS )

*  Process this pair of input and output NDFs.
         CALL SALIA0( INDF1, INDF2, IDAR, SCS, METHOD, XY1, XY2,
     :                ERRLIM, STATUS )

*  Add a history record to the output NDF.
         CALL SALIA1( 'HISTORY', INDF2, INDF1, INDFR, SCS, P, PROJ,
     :                EPOCH, METHOD, XY1, XY2, STATUS )

*  Annul the input NDF identifier.
         CALL NDF_ANNUL( INDF1, STATUS )

*  If an error has occurred, delete the output NDF, otherwise just 
*  annul its identifier.
         IF( STATUS .NE. SAI__OK ) THEN
            CALL NDF_DELET( INDF2, STATUS )
         ELSE
            CALL NDF_ANNUL( INDF2, STATUS )
         END IF

*  If an error occured processing the current input NDF...
         IF( STATUS .NE. SAI__OK ) THEN

*  Flush the error.
            CALL ERR_FLUSH( STATUS )

*  Give a warning telling the user that no output NDF will be created 
*  for the current input NDF.
            CALL GRP_GET( IGRP2, I, 1, NDFNAM, STATUS )
            CALL MSG_SETC( 'NDF', NDFNAM )
            CALL MSG_OUTIF( MSG__QUIET, 'SKYALIGN_MSG2',
     :                      'WARNING: ^NDF cannot be produced',
     :                      STATUS )

*  Overwrite the current output NDF name with a string which will be
*  interpreted as a comment by later applications.
            TEXT = COMC//'          '//NDFNAM( : CHR_LEN( NDFNAM ) )//
     :             ' could not be produced'
            CALL GRP_PUT( IGRP2, 1, TEXT, I, STATUS )

*  If no error occurred, increment the number of good output NDFs.
         ELSE
            NOUT = NOUT + 1
         END IF

*  Process the next input NDF.
      END DO

*  Display a blank line.
      CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Assign a group expression to the output parameter NDFLIST which
*  specifies all the output NDFs. NDFLIST should normally be associated 
*  with a suitable global parameter to cause its value to be passed on 
*  to the next application.  The output parameter NDFLIST is not 
*  advertised as a user parameter since users will normally not be 
*  aware of the existence of global parameter, and so will not know 
*  how to assign a value to it.
      IF( NOUT .GT. 0 ) CALL IRM_LISTN( 'NDFLIST', IGRP2, 'SKYALIGN', 
     :                                   STATUS )

*  Annul the IRA identifier for the reference astrometry information.
 999  CONTINUE
      CALL IRA_ANNUL( IDAR, STATUS )

*  Close down the IRA package.
      CALL IRA_CLOSE( STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  Delete all groups.
      CALL GRP_DELET( IGRP1, STATUS )
      CALL GRP_DELET( IGRP2, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN

*  If a null parameter was given or a parameter abort was requested, 
*  annul the error.
         IF( STATUS .EQ. PAR__NULL .OR. STATUS .EQ. PAR__ABORT ) THEN
            CALL ERR_ANNUL( STATUS )

*  If any other error occurred, then report a contextual message.
         ELSE
            CALL ERR_REP( 'SKYALIGN_ERR1',
     :                   'SKYALIGN: Unable to align a group of images.',
     :                    STATUS )
         END IF

      END IF

      END
