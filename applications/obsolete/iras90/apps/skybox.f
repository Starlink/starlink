      SUBROUTINE SKYBOX( STATUS )
*+
*  Name:
*     SKYBOX

*  Purpose:
*     Define a rectangular region of sky.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SKYBOX( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine defines a rectangular region of an image, using
*     either a cursor or the parameter interface to define the region,
*     and reports the pixel coordinates and sky coordinates of the
*     corners of the region, together with the extent of the region in
*     arc-minutes and in pixels. The edges of the rectangular region are
*     parallel to the image X and Y axes.  The outline of the region
*     may optionally be drawn on a specified graphics device, and a
*     picture corresponding to the region may be added to the graphics
*     database.
*
*     By default, the region is specified by giving the positions of two
*     opposite corners. However, if parameter BOXSIZE is given a value
*     (on the command line for instance) then the region is specified by
*     giving the position of the centre, together with the size of the
*     region is the X and Y directions.

*  Usage:
*     SKYBOX DEVICE IN

*  ADAM Parameters:
*     BOXSIZE = REAL (Read)
*        A pair of values specifying the sizes of the rectangular region
*        parallel to the X and Y image axes. If a single value is
*        supplied, the same value is used for both axes. If a null
*        value is supplied, then the corners of the region are used to
*        specify the box. If parameter COSYS has the value SKY, then
*        BOXSIZE should be supplied in units of arc-minutes. Otherwise,
*        it should be supplied in units of pixels.                   [!]
*     CENTRE_LAT = LITERAL (Read)
*        The latitude of the box centre, in the coordinate system
*        specified by COORDS (eg if COORDS was EQUATORIAL, CENTRE_LAT
*        should be given a Declination value). CENTRE_LAT is only used
*        if parameter CURSOR has a false value, if a value is supplied
*        for parameter BOXSIZE, and if parameter COSYS is given the
*        value SKY.  See the help on "Sky_coordinates" for details of
*        the formats allowed for this value.
*     CENTRE_LON = LITERAL (Read)
*        The longitude of the box centre, in the coordinate system
*        specified by COORDS (eg if COORDS was EQUATORIAL, CENTRE_LON
*        should be given a Right Ascension value). CENTRE_LON is only
*        used if parameter CURSOR has a false value, if a value is
*        supplied for parameter BOXSIZE, and if parameter COSYS is
*        given the value SKY. See the help on "Sky_coordinates" for
*        details of the formats allowed for this value.
*     CENTRE_XY = REAL (Read)
*        The image coordinates (X and Y) of the box centre. CENTRE_XY
*        is only used if parameter CURSOR has a false value, if a value
*        is supplied for parameter BOXSIZE, and if parameter COSYS has
*        one of the values WORLD, IMAGE or PIXEL.
*     CLEAR = LOGICAL (Read)
*        True if the area of the graphics device over which the box is
*        to be drawn should be cleared before drawing it.           [NO]
*     COORDS = LITERAL (Read)
*        Specifies the sky coordinate system to use.  Valid values
*        include ECLIPTIC, EQUATORIAL, GALACTIC. See help on
*        "Sky_coordinates" for more information on available sky
*        coordinate systems.             [current sky coordinate system]
*     COSYS = LITERAL (Read)
*        Determines if the positions which specify the box are to be
*        given in sky coordinates or image coordinates. It can take any
*        of the values SKY, IMAGE, PIXEL or WORLD. The first is used to
*        indicate that sky coordinates will be supplied (in the system
*        specified by parameter COORDS). The other three are all
*        synonymous and indicate that image coordinates will be
*        supplied.                                                 [SKY]
*     CURSOR = LOGICAL (Read)
*        If true, a cursor on a graphics device is used to specify
*        positions. Otherwise, parameters CENTRE_LON, CENTRE_LAT,
*        CENTRE_XY, LAT1, LAT2, LON1, LON2, XY1 and XY2 are used.  [YES]
*     DEVICE = DEVICE (Read)
*        The plotting device.     [Current image-display-overlay device]
*     IN = NDF (Read)
*        The NDF to which image coordinates relate. In CURSOR mode, the
*        NDF will usually be identified using information stored in the
*        AGI database, without the user needing to specify a value for
*        parameter IN.
*     LAT1 = LITERAL (Read)
*        The latitude at a corner of the box, in the coordinate system
*        specified by COORDS. LAT1 is only used if parameter CURSOR has
*        a false value, if a null value is supplied for parameter
*        BOXSIZE, and if parameter COSYS is given the value SKY.  See
*        the help on "Sky_coordinates" for details of the formats
*        allowed for this value.
*     LAT2 = LITERAL (Read)
*        The latitude at the opposite corner of the box, in the
*        coordinate system specified by COORDS.  LAT2 is only used if
*        parameter CURSOR has a false value, if a null value is
*        supplied for parameter BOXSIZE, and if parameter COSYS is
*        given the value SKY. See the help on "Sky_coordinates" for
*        details of the formats allowed for this value.
*     LON1 = LITERAL (Read)
*        The longitude at a corner of the box, in the coordinate system
*        specified by COORDS. LON1 is only used if parameter CURSOR has
*        a false value, if a null value is supplied for parameter
*        BOXSIZE, and if parameter COSYS is given the value SKY. See
*        the help on "Sky_coordinates" for details of the formats
*        allowed for this value.
*     LON2 = LITERAL (Read)
*        The longitude at the opposite corner of the box, in the
*        coordinate system specified by COORDS. LON2 is only used if
*        parameter CURSOR has a false value, if a null value is
*        supplied for parameter BOXSIZE, and if parameter COSYS is
*        given the value SKY.  See the help on "Sky_coordinates" for
*        details of the formats allowed for this value.
*     MSG_FILTER = LITERAL (Read)
*        The level of information displayed on the users screen and
*        logged to the log file. This should take one of the values
*        QUIET, NORMAL or VERBOSE (see help on "Message_filtering").
*                                       [current message filter setting]
*     OUTLINE = LOGICAL (Read)
*        If true, a box is drawn on the graphics device specified by
*        parameter DEVICE, representing the rectangular region. The
*        run-time default for this parameter is YES if parameter CURSOR
*        has a true value, and NO otherwise.                          []
*     PEN = INTEGER (Read)
*        The SGS pen number used to draw the box on the displayed
*        image.  Only used if parameter OUTLINE is given a true value.
*                                                          [current pen]
*     PIC = LOGICAL (Read)
*        If true, a picture is added to the graphics data base
*        corresponding to the rectangular region. The new picture 
*        becomes the current picture. See parameters PICNAM and PICLAB.
*                                                                   [NO]
*     PICLAB = LITERAL (Read)
*        A Label to be given to the new picture. Only used if parameter
*        PIC has a true value. No label is added if a null value is
*        supplied.                                                   [!]
*     PICNAM = LITERAL (Read)
*        The name to associated with the new picture. Only used if parameter
*        PIC has a true value.                                    [DATA]
*     XY1 = REAL (Read)
*        The image coordinates (X and Y) at a corner of the box. XY1 is
*        only used if parameter CURSOR has a false value, if a null
*        value is supplied for parameter BOXSIZE, and if parameter
*        COSYS has one of the values WORLD, IMAGE or PIXEL.
*     XY2 = REAL (Read)
*        The image coordinates (X and Y) at the opposite corner of the 
*        box. XY2 is only used if parameter CURSOR has a false value, 
*        if a null value is supplied for parameter BOXSIZE, and if 
*        parameter COSYS has one of the values WORLD, IMAGE or PIXEL.

*  Examples:
*     SKYBOX
*        This command will display a cursor over the current image
*        overlay device, and ask the user to identify two opposite
*        corners of the box. An outline of the box will be drawn.  The
*        image coordinates and sky coordinates of the corners and the
*        centre of the box will then be displayed, together with the
*        extent of the box in the X and Y directions. No picture will be
*        added to the graphics database.
*     SKYBOX NOCURSOR IN=M51 COSYS=IMAGE XY1=[1,1] XY2=[50,50]
*        The box extends from pixel (1,1) to pixel (50,50) in the NDF
*        "M51". The sky coordinates of the corners and the centre of
*        the box will then be displayed, together with the extent of
*        the box in the X and Y directions. No picture will be added to
*        the graphics database, and no outline will de drawn.
*     SKYBOX BOXSIZE=[50,60] NOOUTLINE PIC PICNAM='FRAME'
*        A cursor is displayed and the user asked to identify the
*        centre of the box. The box extends for 25 arc-minutes on each
*        side of this centre position in the X direction, and for 30
*        arc-minutes in the Y direction. The coordinates of the corners
*        and centre, and the extent of the box are reported. No outline
*        of the box is drawn, but a new FRAME picture corresponding to
*        the box is added to the graphics data base.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-FEB-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants
      INCLUDE 'AGI_ERR'          ! AGI_ error constants
      INCLUDE 'AGI_PAR'          ! AGI_ constants
      INCLUDE 'IRA_PAR'          ! IRA_ constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER
     :        COSYS*5,            ! Coordinate system; image or sky.
     :        GDTYPE*13,          ! Graphics device type.
     :        PICCOM*(AGI__CMAX), ! Comment for a picture.
     :        PICLAB*(AGI__SZLAB),! Label for a picture.
     :        PICNAM*(AGI__SZNAM),! Name for a picture.
     :        SCS*(IRA__SZSCS)    ! Sky coordinate system to use.

      DOUBLE PRECISION
     :        AC( 3 ),           ! Longitude at corners and centre
     :        BC( 3 )            ! Latitude at corners and centre
      
      INTEGER
     :        ACTVAL,            ! Number of values supplied.
     :        IDA,               ! IRA identifier for astrometry info.
     :        INDF,              ! Identifier for input NDF.
     :        ISTAT,             ! SGS error status.
     :        LBND( 2 ),         ! Lower bounds of picture.
     :        PEN,               ! Upper bounds of picture.
     :        PICID1,            ! Identifier for original picture.
     :        PICID2,            ! Identifier for DATA picture.
     :        PICID3,            ! Identifier for new picture.
     :        UBND( 2 ),         ! Upper bounds of picture.
     :        ZONE1,             ! Zone identifier for original picture.
     :        ZONE2,             ! Zone identifier for DATA picture.
     :        ZONE3              ! Zone identifier for new picture.

      LOGICAL
     :        CLEAR,             ! True if display is to be cleared.
     :        CURSOR,            ! True if cursor is to be used.
     :        OUTLIN,            ! True if a box is to be drawn.
     :        PIC,               ! True if a new picture is to be made.
     :        SKYCO              ! True if sky coordinates are supplied.

      REAL
     :        ABSIZE( 2 ),       ! Box dimensions in radians.
     :        BOXSIZ( 2 ),       ! Box dimensions in pixels.
     :        XC( 3 ),           ! X coord.s at box corners and centre.
     :        XYSIZE( 2 ),       ! Supplied box dimensions.
     :        YC( 3 )            ! Y coord.s at box corners and centre.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the level of message reporting and set the report filtering
*  level accordingly.
      CALL MSG_IFGET( STATUS )

*  See if the cursor is to be used.
      CALL PAR_GET0L( 'CURSOR', CURSOR, STATUS )

*  See if an outline of the box is to be drawn.            
      CALL PAR_DEF0L( 'OUTLINE', CURSOR, STATUS )
      CALL PAR_GET0L( 'OUTLINE', OUTLIN, STATUS )

*  See if a picture is to be added to the AGI database.
      CALL PAR_GET0L( 'PIC', PIC, STATUS )

*  See if positions supplied through the parameter interface are to be
*  treated as sky coordinates or image coordinates.
      CALL PAR_CHOIC( 'COSYS', 'SKY', 'SKY,WORLD,IMAGE,PIXEL', .FALSE.,
     :                COSYS, STATUS )
      SKYCO = COSYS .EQ. 'SKY'

*  Get the sky coordinate system to be used when displaying the results.
      CALL IRA_GTSCS( 'COORDS', .FALSE., SCS, STATUS )

*  Start up the IRA astrometry package.
      CALL IRA_INIT( STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  If a graphics device is required, start up AGI and SGS, and open the
*  device.
      IF( PIC .OR. OUTLIN .OR. CURSOR ) THEN
         CALL AGS_ASSOC( 'DEVICE', 'UPDATE', ' ', PICID1, ZONE1,
     :                   STATUS )

*  Attempt to find a DATA picture.
         CALL IRM_AGFND( 'DATA', PICID2, STATUS )

*  If a DATA picture could not be found, add a context message.
         IF( STATUS .EQ. AGI__NONAM ) THEN
            CALL ERR_REP( 'SKYBOX_ERR1',
     :     'SKYBOX: Unable to find a DATA picture which is contained '//
     :     'entirely within the current picture.', STATUS )
         ENDIF

*  Create an SGS zone corresponding to the DATA picture.
         CALL AGS_NZONE( ZONE2, STATUS )

*  Describe the picture to the user.
         CALL MSG_BLANKIF( MSG__NORM, STATUS )

         CALL AGI_ICOM( PICCOM, STATUS )
         CALL AGI_ILAB( -1, PICLAB, STATUS )
         IF( PICLAB .NE. ' ' ) THEN
            CALL MSG_SETC( 'COM', PICCOM )
            CALL MSG_SETC( 'LAB', PICLAB )
            CALL MSG_OUTIF( MSG__NORM, 'SKYBOX_MSG1',
     :                   '  DATA picture ^LAB ("^COM") being used', 
     :                      STATUS )
         ELSE
            CALL MSG_SETC( 'COM', PICCOM )
            CALL MSG_OUTIF( MSG__NORM, 'SKYBOX_MSG2',
     :                   '  DATA picture "^COM" being used', STATUS )
         END IF   

*  If an outline is to be drawn, set up the pen number.
         IF( OUTLIN ) THEN
            CALL PAR_GET0I( 'PEN', PEN, STATUS )
            IF( STATUS .EQ. SAI__OK ) CALL SGS_SPEN( MAX( 0, PEN ) )
            CALL GKS_GSTAT( STATUS )
         END IF

*  Get the type of the graphic device.
         CALL IRM_GDTYP( 'SGS', GDTYPE, STATUS )

*  Set the dynamic default value for CLEAR according to the graphics
*  device type. If the device type is image_overlay, clear the picture
*  by default, otherwise don't.
         CALL PAR_DEF0L( 'CLEAR', GDTYPE .EQ. 'IMAGE_OVERLAY', STATUS )

*  See if the display is to be cleared.
         CALL PAR_GET0L( 'CLEAR', CLEAR, STATUS )

*  Abort if an error has occurred.
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Clear the display surface, if required.
         IF ( CLEAR ) THEN
            CALL SGS_CLRZ
            CALL GKS_GSTAT( STATUS )
         END IF

*  Attempt to get astrometry information from the AGI database.
         CALL IRM_GTAST( 'IN', PICID2, LBND, UBND, IDA, STATUS )

*  Ensure astrometry information is stored in the AGI data base.
         CALL IRM_PTAST( PICID2, IDA, STATUS )

*  If no graphics device is being used, get astrometry information from
*  a supplied NDF.
      ELSE
         CALL NDF_ASSOC( 'IN', 'READ', INDF, STATUS )

*  If a null value was obtained, annul the error and try once more.
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            CALL PAR_CANCL( 'IN', STATUS )
            CALL NDF_ASSOC( 'IN', 'READ', INDF, STATUS )
         END IF

*  Import the NDF into the IRA astrometry system, then annul the NDF 
*  identifier.
         CALL IRA_IMPRT( INDF, IDA, STATUS )
         CALL NDF_ANNUL( INDF, STATUS )            

      END IF

*  Abort if an error has occurred (this guards the following check for
*  a parameter null status).
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to get the dimensions of the box using the parameter .
*  If sky coordinates are being provided by the user, then BOXSIZE will
*  hold dimensions in radians, otherwise it will hold dimensions in
*  pixel coordinates.
      IF( SKYCO ) THEN
         CALL IRM_DIMEN( 'BOXSIZE', .FALSE., 0.0, BOXSIZ, STATUS )
      ELSE
         CALL PAR_GET1R( 'BOXSIZE', 2, BOXSIZ, ACTVAL, STATUS )
         IF( ACTVAL .EQ. 1 ) BOXSIZ( 2 ) = BOXSIZ( 1 )
      END IF

*  Ensure that the box size is not negative.
      BOXSIZ( 1 ) = MAX( 0.0, BOXSIZ( 1 ) )
      BOXSIZ( 2 ) = MAX( 0.0, BOXSIZ( 2 ) )

*  If a good value was supplied for BOXSIZE, the box will be specified
*  by its centre and dimensions.
      IF( STATUS .EQ. SAI__OK ) THEN

*  Obtain the centre position using cursor or parameter system, and find
*  the corresponding image coordinates at the bottom left and top right
*  corners of the box.
         CALL SBOXA0( 'CENTRE_LON', 'CENTRE_LAT', 'CENTRE_XY', BOXSIZ,
     :                SKYCO, CURSOR, IDA, SCS, XC, YC, STATUS )

*  If a null value was supplied for BOXSIZE, find the image coordinates
*  at the corners directly using the cursor or parameter system.
      ELSE IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL SBOXA1( 'LON1', 'LAT1', 'LON2', 'LAT2', 'XY1', 'XY2',
     :                SKYCO, CURSOR, OUTLIN, IDA, SCS, XC, YC, STATUS )

      END IF

*  Calculate and report the geometric properties of the box.
      CALL SBOXA2( IDA, SCS, XC, YC, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  If required, draw the box.
      IF( OUTLIN ) THEN
         CALL SGS_BOX( XC( 1 ), XC( 2 ), YC( 1 ), YC( 2 ) )
         CALL GKS_GSTAT( STATUS )

*  Abort if an error has occurred.
         IF ( STATUS .NE. SAI__OK ) GO TO 999

      END IF

*  If a picture is to be stored in the AGI database...
      IF( PIC ) THEN

*  Create a new zone covering the box, and set the world coordinate 
*  system for the new zone to be pixel coordinates.
         CALL SGS_ZONE( XC( 1 ), XC( 2 ), YC( 1 ), YC( 2 ), ZONE3,
     :                  ISTAT )
         CALL SGS_SW( XC( 1 ), XC( 2 ), YC( 1 ), YC( 2 ), ISTAT )
         CALL GKS_GSTAT( STATUS )

*  Get the name to give to the picture.
         CALL PAR_GET0C( 'PICNAM', PICNAM, STATUS )

*  Save the zone as a picture.
         CALL AGS_SZONE( PICNAM, 'IRAS90_SKYBOX', PICID3, STATUS )

*  Store an astrometry structure with the new picture in the AGI data
*  base.
         CALL IRM_PTAST( PICID3, IDA, STATUS )

*  Abort if an error has occured.
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get the label (if any) to give to the picture
         CALL PAR_GET0C( 'PICLAB', PICLAB, STATUS )

*  If a label was obtained, store it. Otherwise, annul the error.
         IF( STATUS .EQ. SAI__OK ) THEN
            CALL AGI_SLAB( PICID3, PICLAB, STATUS )

         ELSE IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )

         END IF

      END IF

*  Annul the IRA identifier and close down the IRA package.
 999  CONTINUE

      CALL IRA_ANNUL( IDA, STATUS )
      CALL IRA_CLOSE( STATUS )
      
*  If required, close down the graphics package, data base and device.
*  If a new picture has been created, leave it as the current picture.
      IF( PIC .OR. OUTLIN .OR. CURSOR ) THEN
         CALL AGS_DEACT( STATUS )

         IF( PIC ) THEN
            CALL AGI_END( PICID3, STATUS )
         ELSE
            CALL AGI_END( -1, STATUS )
         END IF

         CALL AGI_CANCL( 'DEVICE', STATUS )
      END IF

*  If a parameter null or abort error exists, annul it.
      IF( STATUS .EQ. PAR__NULL .OR. STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SKYBOX_ERR2',
     :           'SKYBOX: Error defining a rectangular region of sky.',
     :                  STATUS )
      END IF

      END
