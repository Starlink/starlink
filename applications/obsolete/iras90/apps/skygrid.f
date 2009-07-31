      SUBROUTINE SKYGRID( STATUS )
*+
*  Name:
*     SKYGRID

*  Purpose:
*     Overlay a sky coordinate grid on a displayed picture.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SKYGRID( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine overlays a sky coordinate grid on top of a specified
*     section of a previously displayed picture, using astrometry
*     information stored with the picture in the AGI database, or
*     stored in a specified NDF. If the current picture is a DATA
*     picture, then the coordinate grid is overlayed on top of the
*     current picture. Otherwise, it is overlayed on top of the most
*     recent DATA picture to be created within the current picture.
*
*     Many aspects of the display can be tailored using the parameters
*     described below. Particularly, the general plotting accuracy can
*     be controlled using parameter TOLERANCE. Greater accuracy is
*     bought at the cost of greater run-time, so in general only use
*     high accuracy if it is really necessary.
 
*  Usage:
*     SKYGRID DEVICE IN

*  ADAM Parameters:
*     CLEAR = _LOGICAL (Read)
*        True if the area of the graphics device over which the
*        coordinate grid is to be drawn should be cleared before
*        creating the grid.                                        [NO] 
*     COORDS = LITERAL (Read)
*        Specifies the sky coordinate system to use. Valid values
*        include ECLIPTIC, EQUATORIAL, GALACTIC. See help on
*        "Sky_coordinates" for more information on available sky
*        coordinate systems.
*                                        [current sky coordinate system]
*     COORDSIZE = REAL (Read)
*        The text size to be used when writing formatted coordinate
*        values, given as a fraction of the maximum dimension of the
*        plotting zone. A negative or zero value suppresses coordinate
*        labels.                                                  [0.01]
*     DEVICE = DEVICE (Read)
*        The plotting device.     [Current image-display-overlay device]
*     IN = NDF (Read)
*        The NDF from which to read the astrometry information. This
*        will usually be the NDF holding the displayed image. A null
*        value will cause the astrometry to be located using
*        information stored within the AGI database. If this cannot be
*        done, then the user will be re-prompted for an NDF using
*        parameter IN.  If a section of an NDF is specified, the
*        coordinate grid is only drawn over the specified section of
*        the picture.                                                [!]
*     LABEL = _LOGICAL (Read)
*        True if the sky coordinate grid is to be labelled.       [TRUE]
*     LATACC = LITERAL (Read)
*        The accuracy required for the displayed latitude labels, in
*        the coordinate system specified by the parameter COORDS (eg if
*        COORDS is EQUATORIAL then LATACC should be given the accuracy
*        as a declination value; for instance, a value of 1 arc-minute
*        will suppress the arc-seconds field in the displayed
*        declination values). See help on "Sky_coordinates" for the
*        formats allowed for this value.  If a value of zero is
*        supplied, a sensible default value will be used.            [0]
*     LATINT = LITERAL (Read)
*        The interval between lines of constant latitude (parallels),
*        in the coordinate system specified by the parameter COORDS (eg
*        if COORDS is EQUATORIAL then LATINT should be given the
*        interval in declination between parallels). See help on
*        "Sky_coordinates" for the formats allowed for this value.  If
*        a value of zero is supplied, a sensible default value will be
*        used.                                                       [0]
*     LINES = LOGICAL (Read)
*        True if lines of constant longitude and latitude should extend
*        across the entire image. Otherwise, short sections at the ends
*        of these lines are displayed (in the nature of tick marks).
*                                                                [FALSE]
*     LONACC = LITERAL (Read)
*        The accuracy required for the displayed longitude labels, in
*        the coordinate system specified by the parameter COORDS (eg if
*        COORDS is EQUATORIAL then LONACC should be given the accuracy
*        as a right ascension value; for instance, a value of 1 hour
*        will suppress the minutes and seconds fields in the displayed
*        RA values). See help on "Sky_coordinates" for the formats
*        allowed for this value. If a value of zero is supplied, a
*        sensible default value will be used.                        [0]
*     LONINT = LITERAL (Read)
*        The interval between lines of constant longitude (meridians),
*        in the coordinate system specified by the parameter COORDS (eg
*        if COORDS is EQUATORIAL then LONINT should be given the
*        interval in right ascension between meridians). See help on
*        "Sky_coordinates" for the formats allowed for this value.  If
*        a value of zero is supplied, a sensible default value will be
*        used.                                                       [0]
*     MSG_FILTER = LITERAL (Read)
*        The level of information displayed on the users screen. This
*        should take one of the values QUIET, NORMAL or VERBOSE (see
*        help on "Message_filtering").
*                                       [current message filter setting]
*     PENA = _INTEGER (Read)
*        Pen number to use when drawing the boundary round the
*        coordinate grid.                                            [3]
*     PENB = _INTEGER (Read)
*        Pen number to use when drawing the curves and ticks.        [3]
*     PENC = _INTEGER (Read)
*        Pen number to use when drawing text labels.                 [1]
*     PEND = _INTEGER (Read)
*        Pen number to use when drawing coordinate labels.           [1]
*     TEXTSIZE = REAL (Read)
*        The text size to be used when writing text labels, given as a
*        fraction of the maximum dimension of the plotting zone. A
*        negative or zero value suppresses text labels.           [0.02]
*     TOLERANCE = _INTEGER (Read)
*        The tolerance allowed when plotting the curves which make up 
*        the grid. The value should be between zero and ten. Values 
*        outside this range are take as being equal to the nearest end 
*        point of the range.  A value of zero gives minimum tolerance
*        (i.e. maximum accuracy), at the cost of increased plotting 
*        time. A value of ten gives poorer accuracy but is faster.   [6]
   
*  Examples:
*     SKYGRID XWINDOWS
*        Overlay a coordinate grid on top of the most recent DATA 
*        picture on the XWINDOWS image display. The AGI database will 
*        usually contain the information needed to define the 
*        coordinate grid. If it does not, then the user will be prompted
*        for parameter IN, and the name of the displayed NDF should be
*        given in response.

*  Notes:
*     -  The astrometry information used to define the sky coordinate
*     grid is located using the following search path:
*
*        o  Firstly, astrometry information is looked for in any NDF
*        specified on the command line using parameter IN.
*
*        o  Secondly, astrometry information is looked for in any MORE
*        structure associated with the AGI picture.
*
*        o  Thirdly, astrometry information is looked for in any
*        reference object associated with the AGI picture.
*
*        o  If all else fails, the value of the IN parameter is
*        cancelled, and the user is prompted for the NDF containing
*        relevant astrometry information.
*      
*     -  This routine can only be used to display coordinate grids
*     for NDFs which contain astrometry information in the form used by
*     the IRAS90 package. 

*  Authors:
*     WG: Wei Gong (IPMAF)
*     DSB: David Berry (Starlink)
*     {enter_new_authors_here}

*  History:
*     25-FEB-1992 (WG):
*        Original version.
*     18-JAN-1993 (DSB):
*        Modified for inclusion in the IRAS90 package.
*     10-MAY-1994 (DCP):
*        Release 1.0 Bugfix 9 - Entered DSB's modification to stop
*        annotation of thin images being truncated ( give x and y
*        margins a different width )
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'IRA_PAR'          ! IRA_ constants
      INCLUDE 'GNS_PAR'          ! GNS_ constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants
      INCLUDE 'AGI_ERR'          ! AGI_ error constants
                                
*  Status:
      INTEGER STATUS             ! Global status
                                
**** The next lines were removed for Release1.0 bugfix 9 ****
*  Local Constants:
*      REAL MARGIN                ! Width of margin for annotations, as a
*                                 ! fractionm of the grid size.
*      PARAMETER ( MARGIN = 0.2 )
*************

*  Local Variables:
      REAL BX1, BX2, BY1, BY2    ! Extension of base zone
      REAL BXM, BYM              ! Size of base zone 
      LOGICAL CLEAR              ! True if grid zone is to be cleared
      LOGICAL COLOUR             ! Graphic device colour flag 
      REAL COORSZ                ! Coordinate label size.
**** next variable inserted for release 1.0 bugfix 9
      REAL DIMMAX                ! Maximum dimension of plotting area
      DOUBLE PRECISION EPOCH     ! Epoch of the sky coord. system
      CHARACTER*( GNS__SZKEY ) GDTYPE
                                 ! Graphic device type
      REAL GX1, GX2, GY1, GY2    ! Extension of grid zone
      REAL GLPXL( 2 ), GUPXL( 2 ) ! Grid section ext. in picel indices
      INTEGER IDA                ! ID of IRA package
      LOGICAL LABEL              ! Sky grid annotation flag
      DOUBLE PRECISION LATINT, LONINT
                                 ! Grid line intervals
      DOUBLE PRECISION LATACC    ! Accuracy of displayed latitude
                                 ! values.
      DOUBLE PRECISION LONACC    ! Accuracy of displayed longitude
                                 ! values.
      INTEGER LBND( 2 ), UBND( 2 )
                                 ! Bound of specified NDF section
      LOGICAL LINES              ! Grid line flag
      INTEGER LPXL( 2 ), UPXL( 2 )
                                 ! Data picture ext. in pixels indices  
      INTEGER NEWPIC             ! AGI picture number for the created
                                 ! picture.
      INTEGER PENA               ! SGS pen number
      INTEGER PENB               ! SGS pen number
      INTEGER PENC               ! SGS pen number
      INTEGER PEND               ! SGS pen number
      CHARACTER PICCOM*80        ! Picture comment.
      INTEGER PICID1             ! AGI picture ID for original picture
      INTEGER PICID2             ! AGI picture ID for DATA picture
      INTEGER PICIDB             ! AGI picture ID for base picture
      INTEGER PICIDS             ! AGI picture ID for sky grid picture
      CHARACTER PICLAB*80        ! Picture label.
      REAL PTX1, PTX2, PTY1, PTY2! Ext. of temp. zone in picture coord.
      CHARACTER*( IRA__SZSCS ) SCS
                                 ! Name of the sky coord. system
      REAL TEXTSZ                ! Text size.
      INTEGER TOL                ! Tolerance for plotting.
      REAL TX1, TX2, TY1, TY2    ! Extension of temporary zone
      REAL TXM, TYM              ! Size of temporary zone
      LOGICAL VALID              ! Valid NDF flag
      REAL X1, X2, Y1, Y2        ! Extension of DATA picture zone
      REAL XM, YM                ! Size of DATA picture zone
**** next variable inserted for release 1.0 bugfix 9
      REAL XMAR, YMAR            ! Width of margins
      INTEGER ZONE1              ! SGS zone ID on entry
      INTEGER ZONEB              ! SGS zone ID for base zone 
      INTEGER ZONEG              ! ID of SGS grid zone, include annotation
      INTEGER ZONEGS             ! ID of SGS grid section zone
      INTEGER ZONEP              ! SGS zone ID for the DATA picture
      INTEGER ZONET              ! SGS zone ID for temporary zone
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a value for parameter MSG_FILTER, and use it to establish the
*  conditional message filter level.
      CALL MSG_IFGET( STATUS )

*  Open the graphic database and start up SGS on the requested graphics
*  device. Do not clear the display. 
      CALL AGS_ASSOC( 'DEVICE', 'UPDATE', ' ', PICID1, ZONE1, 
     :                STATUS )

*  Attempt to find a DATA picture.
      CALL IRM_AGFND( 'DATA', PICID2, STATUS )

*  If a DATA picture could not be found, add a context message.
      IF( STATUS .EQ. AGI__NONAM ) THEN
         CALL ERR_REP( 'SKYGRID_ERR1',
     : 'SKYGRID: Unable to find a DATA picture which is contained '//
     : 'entirely within the current picture.', STATUS )
      ENDIF

*  Get the type of the graphic device.
      CALL IRM_GDTYP( 'SGS', GDTYPE, STATUS )

*  Set the dynamic default value for CLEAR according to graphic device
*  type. If the device is an image_overlay, clear the picture zone by 
*  default.
      IF ( GDTYPE( : 13 ) .EQ. 'IMAGE_OVERLAY' ) THEN
         CALL PAR_DEF0L( 'CLEAR', .TRUE., STATUS )

*  Otherwise, leave the device un-cleared by default.
      ELSE
         CALL PAR_DEF0L( 'CLEAR', .FALSE., STATUS )

      END IF

*  See if the area of the display surface on which the sky coordinate
*  grid is to be drawn is to be cleared.
      CALL PAR_GET0L( 'CLEAR', CLEAR, STATUS )

*  Describe the picture to the user.
      CALL MSG_BLANKIF( MSG__NORM, STATUS )

      CALL AGI_ICOM( PICCOM, STATUS )
      CALL AGI_ILAB( -1, PICLAB, STATUS )
      IF( PICLAB .NE. ' ' ) THEN
         CALL MSG_SETC( 'COM', PICCOM )
         CALL MSG_SETC( 'LAB', PICLAB )
         CALL MSG_OUTIF( MSG__NORM, 'SKYGRID_MSG1',
     :                   '  DATA picture ^LAB ("^COM") being used', 
     :                      STATUS )
      ELSE
         CALL MSG_SETC( 'COM', PICCOM )
         CALL MSG_OUTIF( MSG__NORM, 'SKYGRID_MSG2',
     :                   '  DATA picture "^COM" being used', STATUS )
      END IF   

*  Get an SGS zone id for the DATA picture.
      CALL AGS_NZONE( ZONEP, STATUS )

*  Enquire the zone size of the DATA picture. 
      CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )

*  Enquire if GKS/SGS has reported an error.
      CALL GKS_GSTAT( STATUS )

*  Select the base picture as current, get an SGS zone ID for it.
      CALL AGI_IBASE( PICIDB, STATUS )
      CALL AGI_SELP( PICIDB, STATUS )
      CALL AGS_NZONE( ZONEB, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Find the size of the base zone.
      CALL SGS_IZONE( BX1, BX2, BY1, BY2, BXM, BYM )

*  Enquire if GKS/SGS has reported an error.
      CALL GKS_GSTAT( STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Create a temporary zone which is a little smaller than the base zone,
*  located in the centre of the base zone.
      TXM = 0.99999 * BXM
      TYM = 0.99999 * BYM
      CALL SGS_ZSIZE( TXM, TYM, 'CC', ZONET, STATUS )
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Find the world coordinate extent of the temporary zone.
      CALL SGS_IZONE( TX1, TX2, TY1, TY2, TXM, TYM )
      
*  Enquire if GKS/SGS has reported an error.
      CALL GKS_GSTAT( STATUS )

*  And its extent under the world coordinate system of the picture zone.
      IF( STATUS .EQ. SAI__OK ) CALL SGS_TPZ( ZONET, TX1, TY1, ZONEP,
     :                                       PTX1, PTY1, STATUS )

      IF( STATUS .EQ. SAI__OK ) CALL SGS_TPZ( ZONET, TX2, TY2, ZONEP,
     :                                        PTX2, PTY2, STATUS )

*  Set the world coordinate system of the temporary zone to be
*  continuous with that of the picture zone.
      IF( STATUS .EQ. SAI__OK ) CALL SGS_SW( PTX1, PTX2, PTY1, PTY2, 
     :                                       STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the extent in pixel indices of the DATA picture, assuming that
*  the world coordinates are in pixels.
      LPXL( 1 ) = NINT( X1 + 1.0 )
      UPXL( 1 ) = NINT( X2 )
      LPXL( 2 ) = NINT( Y1 + 1.0 )
      UPXL( 2 ) = NINT( Y2 )

*  Report the picture extent in pixel indices.
      CALL MSG_SETI( 'LX', LPXL( 1 ) )
      CALL MSG_SETI( 'UX', UPXL( 1 ) )
      CALL MSG_SETI( 'LY', LPXL( 2 ) )
      CALL MSG_SETI( 'UY', UPXL( 2 ) )
      CALL MSG_OUTIF( MSG__NORM, 'SKYGRID_MSG3',
     :  '  The picture covers the image section ( ^LX:^UX, ^LY:^UY )',
     :                STATUS )

*  Initialise the IRA astrometry package
      CALL IRA_INIT( STATUS )

*  Get the astrometry information associated with the DATA picture.
      CALL IRM_GTAST( 'IN', PICID2, LBND, UBND, IDA, STATUS )
      CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Get the bounds of the section of the DATA picture which is to be
*  covered by the sky coordinate grid (in pixel coordinates).
      GLPXL( 1 ) = MAX( X1, REAL( LBND( 1 ) - 1 ) )
      GUPXL( 1 ) = MIN( X2, REAL( UBND( 1 ) ) )
      GLPXL( 2 ) = MAX( Y1, REAL( LBND( 2 ) - 1 ) )
      GUPXL( 2 ) = MIN( Y2, REAL( UBND( 2 ) ) )

*  Abort if the area to be covered is too small (i.e. if either
*  dimension is less than 3 pixels).
      IF( ( GUPXL( 1 ) - GLPXL( 1 ) .LT. 3.0 .OR.
     :      GUPXL( 2 ) - GLPXL( 2 ) .LT. 3.0 ) .AND.
     :      STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SKYGRID_ERR2',
     :'SKYGRID: Area to be covered by the coordinate grid is too small',
     :                 STATUS )
         GO TO 999
      END IF

*  Get the sky coordinate system and epoch used in the astrometry
*  structure of the NDF.
      CALL IRA_SCSEP( IDA, SCS, EPOCH, STATUS )

*  See which sky coordinate system the user wants to use.
      CALL IRA_GTSCS( 'COORDS', .TRUE., SCS, STATUS )
       
*  Get values for the parameters describing the graphics required. First
*  see if the plot is to be annotated.
      CALL PAR_GET0L( 'LABEL', LABEL, STATUS )

*  If so, get the text height and coordinate value height, and limit to
*  unity.
      IF( LABEL ) THEN      
         CALL PAR_GET0R( 'TEXTSIZE', TEXTSZ, STATUS )
         CALL PAR_GET0R( 'COORDSIZE', COORSZ, STATUS )
         TEXTSZ = MIN( 0.99999, TEXTSZ )
         COORSZ = MIN( 0.99999, COORSZ )

*  If annotation is not required, set both to a negative value.
      ELSE
         TEXTSZ = -1.0
         COORSZ = -1.0
      END IF

*  Get the drawing tolerance and limit to the range [0,10]
      CALL PAR_GET0I( 'TOLERANCE', TOL, STATUS )
      TOL = MAX( 0, MIN( 10, TOL ) )

*  See if the grid should be the lines across the picture or just tick
*  marks around the bounds of the picture.
      CALL PAR_GET0L( 'LINES', LINES, STATUS )      

*  Get the longitude and latitude gaps of the grid lines.
      CALL IRA_GETCO( 'LONINT', 'LATINT',
     :                ' interval between grid marks', SCS, .FALSE.,
     :                LONINT, LATINT, STATUS )

*  Get the accuracy to which longitude and latitude values should be
*  displayed.
      CALL IRA_GETCO( 'LONACC', 'LATACC', ' accuracy', SCS, .FALSE.,
     :                LONACC, LATACC, STATUS )

*  Get the SGS pen number for the boundary.
      CALL PAR_GET0I( 'PENA', PENA, STATUS )

*  Get the SGS pen number for the curves and ticks.
      CALL PAR_GET0I( 'PENB', PENB, STATUS )

*  Get the SGS pen number for text.
      CALL PAR_GET0I( 'PENC', PENC, STATUS )

*  Get the SGS pen number for coordinate values.
      CALL PAR_GET0I( 'PEND', PEND, STATUS )

*  Store values for the IRA graphics options.
      CALL IRA_DROPT( 'TEXT_SIZE', DBLE( TEXTSZ ), STATUS )
      CALL IRA_DROPT( 'COORD_SIZE', DBLE( COORSZ ), STATUS )
      CALL IRA_DROPT( 'TOLERANCE', DBLE( TOL ), STATUS )

      IF( LINES ) THEN
         CALL IRA_DROPT( 'LINES', 1.0D0, STATUS )
      ELSE
         CALL IRA_DROPT( 'LINES', -1.0D0, STATUS )
      END IF
      
      CALL IRA_DROPT( 'LONG_GAP', LONINT, STATUS )
      CALL IRA_DROPT( 'LAT_GAP', LATINT, STATUS )
      CALL IRA_DROPT( 'LONG_ACC', LONACC, STATUS )
      CALL IRA_DROPT( 'LAT_ACC', LATACC, STATUS )
      CALL IRA_DROPT( 'PEN1', DBLE( PENA ), STATUS )
      CALL IRA_DROPT( 'PEN2', DBLE( PENB ), STATUS )
      CALL IRA_DROPT( 'PEN3', DBLE( PENC ), STATUS )
      CALL IRA_DROPT( 'PEN4', DBLE( PEND ), STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  If annotation is required, a zone larger than the picture is needed
*  to contain the annotations.
      IF ( LABEL ) THEN
                        
*  Find the boundary of a zone which is larger than the overlying
*  section to contain the annotation.

***** The next section removed for Release 1.0 bugfix 9
*         GX1 = MAX( PTX1, GLPXL( 1 ) - 
*     :                    MARGIN * ( GUPXL( 1 ) - GLPXL( 1 ) ) ) 
*         GX2 = MIN( PTX2, GUPXL( 1 ) + 
*     :                    MARGIN * ( GUPXL( 1 ) - GLPXL( 1 ) ) ) 
*         GY1 = MAX( PTY1, GLPXL( 2 ) -
*     :                    MARGIN * ( GUPXL( 2 ) - GLPXL( 2 ) ) ) 
*         GY2 = MIN( PTY2, GUPXL( 2 ) +
*     :                    MARGIN * ( GUPXL( 2 ) - GLPXL( 2 ) ) )
*****

***** The next section created for Release 1.0 bugfix 9
      DIMMAX = MAX( GUPXL( 1 ) - GLPXL( 1 ), GUPXL( 2 ) - GLPXL( 2 ) )
      XMAR = ( 2*TEXTSZ + 20*COORSZ ) * DIMMAX
      YMAR = ( 8*TEXTSZ +  6*COORSZ ) * DIMMAX

      GX1 = MAX( PTX1, GLPXL( 1 ) - XMAR )
      GX2 = MIN( PTX2, GUPXL( 1 ) + XMAR )
      GY1 = MAX( PTY1, GLPXL( 2 ) - YMAR )
      GY2 = MIN( PTY2, GUPXL( 2 ) + YMAR )
*****

*  Create a zone to contain the sky grid and labels. And set the
*  coordinate of the grid zone as that of the picture zone.
         CALL SGS_ZONE( GX1, GX2, GY1, GY2, ZONEG, STATUS )
         CALL SGS_SW( GX1, GX2, GY1, GY2, STATUS )  

*  If labels are not required, ...
      ELSE

*  Create an SGS zone to contain only the sky grid, and set the
*  coordinate of the zone as that of the picture zone.
         CALL SGS_ZONE( GLPXL( 1 ), GUPXL( 1 ), GLPXL( 2 ), GUPXL( 2 ), 
     :                  ZONEG, STATUS )
         CALL SGS_SW( GLPXL( 1 ), GUPXL( 1 ), GLPXL( 2 ), GUPXL( 2 ), 
     :                STATUS )
    
      END IF

*  Release the base zone and temporary zone.
      CALL SGS_RELZ( ZONET )
      CALL SGS_RELZ( ZONEB )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999
      
*  If the clear zone  is requested, clear the zone.
      IF ( CLEAR ) CALL SGS_CLRZ

*  If colour is available on the graphic device, use solid lines for all
*  plotting.
      CALL IRM_QCOL( COLOUR, STATUS )
      IF ( COLOUR ) CALL IRM_SOLIN( STATUS )

*  Draw the sky coordinate grid.
      CALL IRA_DRGRD( IDA, SCS, GLPXL, GUPXL, STATUS )

*  Save the astrometry information associated with the DATA picture.
      CALL IRM_PTAST( PICID2, IDA, STATUS )

*  Save the zone in which the grid will be drawn in the AGI database, as
*  a FRAME picture.
      CALL AGS_SZONE( 'FRAME', 'IRAS90_SKYGRID', NEWPIC, STATUS )
      
*  Save the astrometry information associated with the new picture.
      CALL IRM_PTAST( NEWPIC, IDA, STATUS )

*  Annul the IRA identifier and close down the IRA astrometry package.
 999  CONTINUE
      CALL IRA_ANNUL( IDA, STATUS )
      CALL IRA_CLOSE( STATUS )

*  Set the current picture on entry as current and close down the AGI
*  and SGS.
      CALL AGS_DEASS( 'DEVICE', .TRUE., STATUS )

*  If a parameter null or abort error exists, annul it.
      IF( STATUS .EQ. PAR__NULL .OR. STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SKYGRID_ERR3',
     :               'SKYGRID: Error displaying a sky coordinate grid.',
     :                 STATUS )
      END IF

      END
