      SUBROUTINE SKYMARK( STATUS )
*+
*  Name:
*     SKYMARK

*  Purpose:
*     Draw markers at specified positions.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SKYMARK( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine draws markers at a set of specified positions within
*     the most recently created DATA picture, or alternatively draws a 
*     polyline through them. The positions can be specified by using a 
*     graphics cursor, or by reading them from a text file, or by 
*     supplying them in response to parameter prompts.
*
*     There are 5 different markers available for selection: '.', '+',
*     '*',  'o' and 'x'. Except the '.', there is the option in which
*     the size of the markers are proportional to the image data value
*     at the selected position. In this case, a '.' will temporarily be
*     put at each of the selected positions until the full range of
*     image data values at all selected positions is known.  The
*     temporary markers will then be replaced by the selected marker,
*     with sizes ranging between the limits specified by parameter
*     SIZE.
*     
*     The sky coordinates of the marked positions can be logged into
*     a logging file, see parameter LOGFILE, which can be used in
*     future by other IRAS90 applications as well as this application
*     itself.
         
*  Usage:
*     SKYMARK DEVICE IN

*  ADAM Parameters:
*     CLEAR = LOGICAL (Read)
*        True if the area of the graphics device over which the markers
*        are to be drawn should be cleared before drawing any markers.
*                                                                   [NO]
*     COORDS = LITERAL (Read)
*        Specifies the sky coordinate system to use. Valid values
*        include ECLIPTIC, EQUATORIAL, GALACTIC. See help on
*        "Sky_coordinates" for more information on available sky
*        coordinate systems.
*                                        [current sky coordinate system]
*     DEVICE = DEVICE (Read)
*        The plotting device.     [Current image-display-overlay device]
*     EPOCH = DOUBLE PRECISION (Read)
*        The Julian epoch at which the supplied sky positions were
*        determined. This is only used if sky coordinates supplied in a
*        text file (in FILE mode) are stored in a coordinate system
*        different to that specified by the COORDS parameter. In this
*        case the epoch may be necessary to perform the conversion from
*        one coordinate system to the other (depending on what the
*        coordinate systems are). A value of 1983.5 is acceptable for
*        all IRAS data.
*     FILE = FILENAME (Read)
*        A file from which to read coordinates. This is only prompted
*        for if parameter MODE has the value FILE. See the help on
*        "Coordinate_files" for a description of the format required for
*        this file.
*     IN = NDF (Read)
*        The NDF to which image coordinates relate. In CURSOR mode, the
*        NDF will usually be identified using information stored in the
*        AGI database, without the user needing to specify a value for
*        parameter IN.
*     LAT = LITERAL (Read)
*        A latitude of a position to be marked, in the coordinate
*        system specified by COORDS (eg if COORDS was EQUATORIAL, LAT
*        should be given a Declination value). LAT is only prompted
*        for if parameter MODE has the value KEYBOARD. See the help on
*        "Sky_coordinates" for details of the formats allowed for this
*        value.
*     LOGFILE = LITERAL (Read)
*        The name of a text file to receive a copy of the displayed
*        information. The run time default is for no log file to be
*        produced. Log files are created in a format suitable for use
*        with parameter FILE.                                        [!]
*     LON = LITERAL (Read)
*        A longitude of a position to be marked, in the coordinate
*        system specified by COORDS (eg if COORDS was EQUATORIAL, LON
*        should be given a Right Ascension value). LON is only prompted
*        for if parameter MODE has the value KEYBOARD. See the help on
*        "Sky_coordinates" for details of the formats allowed for this
*        value.
*     LOOP = LOGICAL (Read)
*        If true then application does not exit when the final input
*        position has been given. Instead, the user is allowed to
*        change some of the parameter values and to continue to
*        mark more positions. See parameter OPTION.                 [NO]
*     MODE = LITERAL (Read)
*        Specifies the working mode of the application. It can take
*        following values:
*
*         CURSOR - The positions are specified by the cursor. This
*         option is only available when cursor is available on the
*         graphic device in use.
*
*         KEYBOARD - The positions are specified from the keyboard.
*
*         FILE - The positions are read from a text file.
*
*        The inputs can be abbrievated to one character and is case
*        insensitive.                                         [KEYBOARD]
*     MSG_FILTER = LITERAL (Read)
*        The level of information displayed on the users screen and
*        logged to the log file. This should take one of the values
*        QUIET, NORMAL or VERBOSE (see help on "Message_filtering").
*                                       [current message filter setting]
*     OPTION = LITERAL (Read)
*        The action to perform once all positions have been marked.
*        This is only prompted for if parameter LOOP is TRUE (otherwise
*        a value of EXIT is assumed). It can take the following values;
*        
*         CLEAR - Clear the marks drawn in previous loops. This option
*         is only available when the graphic device in use is the type
*         of IMAGE-OVERLAY.
*                 
*         TYPE - Select a new type of mark.
*
*         PEN - Select a new pen to draw the mark.
*         
*         POLY - Toggle between polyline/marker mode.
*         
*         SIZE - Select a new mark size.
*         
*         MODE - Select a new working mode for the application.
*
*         MARK - Perform the marking task with newly selected parameters
*
*         EXIT - Exit the application.
*
*        The application will keep prompt for a new value of this
*        parameter until 'MARK' or 'EXIT' is given. The input can be
*        abbrievated to an unambiguous length and is case insensitive.
*     PEN = INTEGER (Read)
*        The SGS pen number used to draw the markers on the displayed
*        image.                                                      [3]
*     POLYLINE = LOGICAL (Read)
*        True if a polygonal line is to be drawn connecting the supplied
*        positions, instead of the usual markers. The polyline is closed
*        by connecting the last position with the first position.   [NO]
*     SIZE = REAL (Read)
*        A pair of real value give the max. and min. size of the marks
*        to be draw on the image. Given as the scale of the nominal
*        size for the graphic device. The size of a mark is at somewhere
*        between this two values according to the image magnitude at the
*        mark position. Among all positions to be marked, the mark at
*        the position with the max. image magnitude will has max. size
*        and the mark at the position with the min. image magnitude will
*        has min. size. If only one value is obtained from the user, the
*        max size will be equal to the min szie, that is, all marks will
*        have the same size.                                       [1.0]
*     TYPE = LITERAL (Read)
*        The marker to be used. The following markers are currently
*        available ".", "+", "*", "o", "x"                           [+]

*  Notes:
*     - The displayed image should have name 'DATA' and be either the
*     current one of the most recent one with the name 'DATA' in the
*     AGI data base, that is, the picture should either be the most
*     recently displayed, or be made current by using KAPPA application
*     CURSOR or PICLIST etc (see SUN/95 for more details about the
*     usage of these KAPPA applictions).
*
*     - This application uses the astrometry information stored in the
*     Astrometry Extension, created by IRA_ package, of the underlying
*     NDF file of the displayed image. Therefore the Astrometry
*     Extension should be first created if it does not exist. For IRAS
*     images, it is to require the user applying the I90 application
*     PREPARE to the image NDFs right after extracting the images from
*     the FITS tape with KAPPA applciation FITSIN.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     3-JUN-1992 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants
      INCLUDE 'IRA_PAR'          ! IRA_ constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants
      INCLUDE 'AGI_PAR'          ! AGI_ constants
      INCLUDE 'AGI_ERR'          ! AGI_ error constants
                               
*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants: Max. number of positions
      INTEGER MAXPNT
      PARAMETER ( MAXPNT = 500 )

*  External References:
      INTEGER CHR_LEN

*  Local Variables:
      CHARACTER*( 10 ) MODE      ! Application working mode
      CHARACTER*( 10 ) PRMPDF    ! Prompt default for mode
      CHARACTER*( 14 ) GDTYPE    ! Graphic device type
      CHARACTER*( 45 ) OPTLOP    ! Option list for OPTION
      CHARACTER*( 40 ) OPTMOD    ! Option list for mode
      CHARACTER*( 6 )  OPT       ! Option
      CHARACTER*( 80 ) CMNT      ! Comment string in output file
      CHARACTER*( DAT__SZLOC ) LOC  ! Locator to the NDF 
      CHARACTER*( 1 )  MTYPE     ! Marker to be used.
      CHARACTER*( AGI__CMAX ) PICCOM  ! Comment of the displayed image
      CHARACTER*( AGI__SZLAB ) PICLAB ! Label of the displayed image
      CHARACTER*( IRA__SZSCS ) SCS ! Name of sky coordinate system


      DOUBLE PRECISION EPOCH     ! Epoch of the sky coordinate system
      DOUBLE PRECISION LAT( MAXPNT ), LON( MAXPNT ) ! Sky coordinates
                                                    ! of positions
      DOUBLE PRECISION LATTM, LONTM ! Temporary sky coordinates
      DOUBLE PRECISION X( MAXPNT ), Y( MAXPNT )! Image coordinates of
                                               ! positions

      INTEGER CLEN               ! Used length of a string.
      INTEGER I                  ! Do loop index
      INTEGER IRA                ! IRA system ID
      INTEGER MARKTY             ! Type of the marks
      INTEGER NDF                ! NDF id of the image
      INTEGER NKEY               ! Number of keys pressed
      INTEGER NPNT               ! Number of selected positions
      INTEGER NSIZE              ! Number of the sizes given by user
      INTEGER OUTFID             ! Logging file ID
      INTEGER PEN                ! SGS pen used to draw markers
      INTEGER PICID1             ! Original AGI picture ID 
      INTEGER PICID2             ! AGI DATA picture ID 
      INTEGER TYPE               ! Type of the marks
      INTEGER ZONE1              ! SGS zone ID for initial picture
      INTEGER ZONE2              ! SGS zone ID for DATA picture


      LOGICAL CLEAR              ! Clear display surface flag
      LOGICAL CURSOR             ! Cursor flag
      LOGICAL EXIT               ! Exit application flag
      LOGICAL FIRST              ! True if the first point is being 
                                 ! processed.
      LOGICAL LOGING             ! Logging flag
      LOGICAL LOOP               ! True if looping is required.
      LOGICAL NULL               ! Null response flag
      LOGICAL POLY               ! True if a polyline is required.
      LOGICAL REFOBJ             ! Reference object fla
      LOGICAL SIZCHG             ! Change size flag


      REAL CURSX, CURSY          ! Cursor position
      REAL MKSIZ( MAXPNT )       ! Size of the marks
      REAL MXMNSZ( 2 )           ! Max and min Marker sizes
      REAL TEMP                  ! A temporary variable
      REAL X1, X2, Y1, Y2, XM, YM ! Extension and size of picture zone

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Establish the conditional message filter level using parameter
*  MSG_LEVEL.
      CALL MSG_IFGET( 'MSG_FILTER', STATUS )

*  See if the user wants to loop round, performing multiple operations.
      CALL PAR_GET0L( 'LOOP', LOOP, STATUS )

*  See if a polyline is to be drawn instead of the markers.
      CALL PAR_GET0L( 'POLYLINE', POLY, STATUS )

*  Open the graphic database and get the graphic device. Since it is not
*  known whether the graphic device is image-desplay or image-overlay,
*  keep the graphic sorface uncleared at present.
      CALL AGS_ASSOC( 'DEVICE', 'UPDATE', ' ', PICID1, ZONE1, STATUS )

*  Attempt to find a DATA picture.
      CALL IRM_AGFND( 'DATA', PICID2, STATUS )

*  If a DATA picture could not be found, add a context message.
      IF( STATUS .EQ. AGI__NONAM ) THEN
         CALL ERR_REP( 'SKYMARK_ERR1',
     : 'SKYMARK: Unable to find a DATA picture which is contained '//
     : 'entirely within the current picture.', STATUS )
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
         CALL MSG_OUTIF( MSG__NORM, 'SKYMARK_MSG1',
     :                   '  DATA picture ^LAB ("^COM") being used', 
     :                      STATUS )
      ELSE
         CALL MSG_SETC( 'COM', PICCOM )
         CALL MSG_OUTIF( MSG__NORM, 'SKYMARK_MSG2',
     :                   '  DATA picture "^COM" being used', STATUS )
      END IF   

*  If the graphics device was not available, exit.
      IF ( STATUS .NE. SAI__OK ) GO TO 980

*  Get the type of the graphic device.
      CALL IRM_GDTYP( 'SGS', GDTYPE, STATUS )

*  Set the dynamic default value for CLEAR according to graphic device
*  type. If the device is image_overlay, clear the picture zone by
*  default.
      IF ( GDTYPE( : 13 ) .EQ. 'IMAGE_OVERLAY' ) THEN
         CALL PAR_DEF0L( 'CLEAR', .TRUE., STATUS )

*  If the device is anything else, unclear the picture zone by default.
      ELSE 
         CALL PAR_DEF0L( 'CLEAR', .FALSE., STATUS )
      END IF

*  Clear the display surface, if required.
      CALL PAR_GET0L( 'CLEAR', CLEAR, STATUS )
      IF ( CLEAR ) CALL SGS_CLRZ

*  Enquire the zone size of the DATA picture.
      CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )

*  See if there is a reference object associated with the picture.
      CALL IRM_AGREF( PICID2, 'READ', REFOBJ, LOC, STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 940
      
*  If reference object exits, report it to the user and import it to
*  the NDF system.
      IF ( REFOBJ ) CALL NDF_IMPRT( LOC, NDF, STATUS )
      
*  If reference object does not exit or the reference object is not
*  a valid NDF, get the NDF file name from the user.
      IF ( .NOT.REFOBJ .OR. STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL NDF_ASSOC( 'NDF',, 'READ', NDF, STATUS )

*  Or a valid NDF is obtained through reference, report to the user.
      ELSE
         CALL IRM_HMSG( 'OBJNM', LOC )
         CALL MSG_OUTIF( MSG__VERB, 'SKYMARK_MSG3',
     :                 '  NDF associated with image: ^OBJNM',
     :                 STATUS )      
      END IF

*  Initialise the IRA astrometry package, and inpurt astrometry
*  structure from the NDF into IRA system.
      CALL IRA_INIT( STATUS )
      CALL IRA_IMPRT( NDF, IRA, STATUS )

*  Get the sky coordinate system the user want to use, using the one
*  in IRA structure as default.
      CALL IRA_SCSEP( IRA, SCS, EPOCH, STATUS )
      CALL IRA_GTSCS( 'COORDS', .TRUE., SCS, STATUS )
      
*  Get the name of the file to log the output coordinates.
      CALL IRM_ASFIO( 'LOGFILE', 'WRITE', 'LIST', 80, OUTFID, LOGING,
     :                 STATUS )

*  If a logging file is obtained, get the label of the image to
*  form the first comment of logging file.
      IF ( LOGING ) THEN
         CALL AGI_ILAB( PICID2, PICLAB, STATUS )

         IF( PICLAB .NE. ' ' ) THEN
            CMNT = '# SKYMARK: Positions marked on the image '//PICLAB
         ELSE
            CMNT = '# SKYMARK: Positions marked'
         END IF

         CLEN = CHR_LEN( CMNT )
         CALL FIO_WRITE( OUTFID, CMNT( : MIN( 80, CLEN ) ), STATUS )

      END IF

*  Get the pen number used to draw markers.
      CALL PAR_GET0I( 'PEN', PEN, STATUS )
      
*  Get the type of the marker to be drawn.
      CALL PAR_CHOIC( 'TYPE', '+', '.,+,*,o,x', .FALSE., MTYPE, STATUS )
      CALL PAR_CANCL( 'TYPE', STATUS )
      CALL CHR_UCASE( MTYPE )
      MARKTY = MAX( INDEX( '.+*OX', MTYPE ), 1 )
      
*  Get the max and min size of the marks.
      CALL PAR_GET1R( 'SIZE', 2, MXMNSZ, NSIZE, STATUS )

*  Select the working mode according to the availability of the cursor.
      CALL SGS_ICUAV( CURSOR )
      IF ( CURSOR ) THEN
         PRMPDF = 'Cursor'
         OPTMOD = 'Cursor,Keyboard,File'
      ELSE
         PRMPDF = 'Keyboard'
         OPTMOD = 'Keyboard,File'
      END IF
      CALL PAR_CHOIC( 'MODE', PRMPDF, OPTMOD, .FALSE., MODE, STATUS )
      CALL CHR_UCASE( MODE )

*  Set the option list for OPTION according to the type of the graphic
*  device in use.
      IF ( GDTYPE( : 13 ) .EQ. 'IMAGE_OVERLAY' ) THEN
         OPTLOP = 'CLEAR,TYPE,PEN,POLY,SIZE,MODE,MARK,EXIT'
      ELSE
         OPTLOP = 'TYPE,PEN,POLY,SIZE,MODE,MARK,EXIT'
      END IF
      
*  Enter a loop to perform the marking until exit is requested.
      OPT = 'MARK'
      DO WHILE ( OPT( : 4 ) .NE. 'EXIT' .AND. STATUS .EQ. SAI__OK )

*  Initialise the number of marks drawn in this loop.
         NPNT = 0

*  Ask the value of OPTION until EXIT or MARK is specified.
         DO WHILE( OPT( : 4 ) .NE. 'EXIT' .AND. OPT( : 4 ) .NE. 'MARK'
     :            .AND. STATUS .EQ. SAI__OK )
            CALL PAR_CHOIC( 'OPTION', 'MARK', OPTLOP, .FALSE., OPT,
     :                       STATUS )

*  If select mark type is wanted, ...
            IF ( OPT( : 4 ) .EQ. 'TYPE' ) THEN
               CALL PAR_CANCL( 'TYPE', STATUS )
               CALL PAR_CHOIC( 'TYPE', '+', '.,+,*,o,x', .FALSE.,
     :                         MTYPE, STATUS )
               CALL PAR_CANCL( 'TYPE', STATUS )
               CALL CHR_UCASE( MTYPE )
               MARKTY = MAX( INDEX( '.+*OX', MTYPE ), 1 )

*  If select pen number is wanted, ...
            ELSE IF ( OPT( : 3 ) .EQ. 'PEN' ) THEN
               CALL PAR_CANCL( 'PEN', STATUS )
               CALL PAR_GET0I( 'PEN', PEN, STATUS )

*  If toggle polyline/marker mode is required, ...
            ELSE IF ( OPT( : 4 ) .EQ. 'POLY' ) THEN
               POLY = .NOT. POLY
               IF( POLY ) THEN
                  CALL MSG_OUTIF( MSG__NORM, 'SKYMARK_MSG4',
     :                            '  Ok. A polyline will be drawn.', 
     :                            STATUS )         
               ELSE
                  CALL MSG_OUTIF( MSG__NORM, 'SKYMARK_MSG5',
     :                            '  Ok. Markers will be drawn.', 
     :                            STATUS )         
               END IF

*  If the mark size is to be changed, ...
            ELSE IF ( OPT( : 4 ) .EQ. 'SIZE' ) THEN
               CALL PAR_CANCL( 'SIZE', STATUS )
               CALL PAR_GET1R( 'SIZE', 2, MXMNSZ, NSIZE, STATUS )
      
*  If the working mode is to be changed, ...
            ELSE IF ( OPT( : 4 ) .EQ. 'MODE' ) THEN
               CALL PAR_CANCL( 'MODE', STATUS )
               CALL PAR_CHOIC( 'MODE', PRMPDF, OPTMOD, .FALSE., MODE,
     :                          STATUS )

*  If clear the previous marks is requested, ...
            ELSE IF ( OPT( : 5 ) .EQ. 'CLEAR' ) THEN
               CALL SGS_CLRZ
      
            END IF

*  Cancel OPTION's value for getting a new value.
            CALL PAR_CANCL( 'OPTION', STATUS ) 
         END DO


*  Go on only If no error happened and no exit was requested.
         IF ( STATUS .EQ. SAI__OK .AND. OPT( : 4 ) .NE. 'EXIT' ) THEN
         
*  Set the pen number as selected.
            CALL SGS_SPEN( PEN )

*  If only one size is obtained, mark size is a constant and use the
*  mark type as selected.
            IF ( NSIZE .EQ. 1 ) THEN
               SIZCHG = .FALSE.
               TYPE = MARKTY
      
*  Otherwise, the size of the marks will change proportionally to the
*  magnitude of the image. Make sure the max size is greater than the
*  min. size.
            ELSE
               SIZCHG = .TRUE.
               IF ( MXMNSZ( 1 ) .LT. MXMNSZ( 2 ) ) THEN
                  TEMP = MXMNSZ( 1 )
                  MXMNSZ( 1 ) = MXMNSZ( 2 )
                  MXMNSZ( 2 ) = TEMP
               END IF
      
*  And a '.', instead of the selected type, will be draw immediately
*  after a position is specifed.
               TYPE = 1      
            END IF
      
*  If cursor mode is selected, enable the cursor and write message to
*  the user.
            IF ( MODE( : 6 ) .EQ. 'CURSOR' ) THEN
               CALL SGS_CUVIS( .TRUE. )

               CALL MSG_BLANKIF( MSG__NORM, STATUS )
               CALL MSG_OUTIF( MSG__NORM, 'SKYMARK_MSG6',
     :             '  Position the cursor and press any button '//
     :             '(position the cursor outside the image to exit).',
     :                         STATUS )
               CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Put the cursor at the centre of the image and then get the position
*  of the cursor.
               CALL SGS_SETCU( 0.5 * ( X1 + X2 ), 0.5 * ( Y1 + Y2 ) )

*  Get the first position.
               FIRST = .TRUE.
               CALL SGS_REQCU( CURSX, CURSY, NKEY )

*  Enter a do loop until the cursor is outside the image.
               DO WHILE ( STATUS .EQ. SAI__OK .AND.
     :                    CURSX .GE. X1 .AND. CURSX .LE. X2 .AND.
     :                    CURSY .GE. Y1 .AND. CURSY .LE. Y2 )

*  Record the selected position.
                  NPNT = NPNT + 1
                  X( NPNT ) = DBLE( CURSX )
                  Y( NPNT ) = DBLE( CURSY )
                  MKSIZ( NPNT ) = MXMNSZ( 1 )

*  And draw the mark.
                  CALL SMARA0( 1, X( NPNT ), Y( NPNT ), TYPE, MKSIZ,
     :                         X1, X2, Y1, Y2, POLY, .FALSE., FIRST, 
     :                         STATUS )

*  Get another cursor position.
                  CALL SGS_REQCU( CURSX, CURSY, NKEY )
               END DO

*  If logging is require, get the sky coordinates of the specified
*  positions (if any).
               IF ( NPNT .GT. 0 .AND. LOGING )
     :            CALL IRA_TRANS( NPNT, X, Y, .TRUE., SCS, IRA,
     :                            LON, LAT, STATUS )
      
*  If keyboard mode is selected, enter a loop to get sky positions from
*  the keyboard until a null response is obtained.
            ELSE IF ( MODE( : 8 ) .EQ. 'KEYBOARD' ) THEN
               FIRST = .TRUE.
               NULL = .FALSE.
               DO WHILE ( .NOT.NULL .AND. STATUS .EQ. SAI__OK )
                  CALL IRA_GETCO( 'LON', 'LAT',
     :                            ' of the position to be marked',
     :                            SCS, .FALSE., LONTM, LATTM, STATUS )

*  If a null is obtained, set the flag.
                  IF ( STATUS .EQ. PAR__NULL ) THEN
                     CALL ERR_ANNUL( STATUS )
                     NULL = .TRUE.

*  Or if a valid sky coordinate is obtained, find its image position.
                  ELSE IF ( STATUS .EQ. SAI__OK ) THEN
                     NPNT = NPNT + 1
                     LON( NPNT ) = LONTM
                     LAT( NPNT ) = LATTM
                     CALL IRA_TRANS( 1, LON( NPNT ), LAT( NPNT ),
     :                              .FALSE., SCS, IRA, X( NPNT ),
     :                               Y( NPNT ), STATUS )
      
*  And draw the mark.
                     MKSIZ( NPNT ) = MXMNSZ( 1 )
                     CALL SMARA0( 1, X( NPNT ), Y( NPNT ), TYPE,
     :                            MKSIZ( NPNT ), X1, X2, Y1, Y2, 
     :                            POLY, .FALSE., FIRST, STATUS )

*  For any other case, exit the application.
                  ELSE
                     GO TO 940  
                  END IF

*  Cancel the values of the parameters to get new values.
                  CALL PAR_CANCL( 'LON', STATUS )
                  CALL PAR_CANCL( 'LAT', STATUS )
               END DO
      
*  If file mode is selected, get a input file from the user and find the
*  sky and image coordinates.
            ELSE IF ( MODE( : 4 ) .EQ. 'FILE' ) THEN
               CALL SMARA1( 'FILE', 'EPOCH', SCS, IRA, MAXPNT, X, Y, 
     :                      LON, LAT, NPNT, STATUS )

*  If error happens, exit.
               IF ( STATUS .NE. SAI__OK ) GO TO 940

*  And draw the mark if get some positions.
               IF ( NPNT .GT. 0 ) THEN
                  DO I = 1, NPNT
                     MKSIZ( I ) = MXMNSZ( 1 )
                  END DO

                  FIRST = .TRUE.
                  CALL SMARA0( NPNT, X, Y, TYPE, MKSIZ, X1, X2, Y1, Y2, 
     :                         POLY, .FALSE., FIRST, STATUS )
               END IF

*  Cancel the value of the parameter for next use.
               CALL PAR_CANCL( 'FILE', STATUS )      
            END IF

*  If some position has been specified and the size of the marks on
*  these position should change, calculate the size of the marks. 
            IF ( NPNT .GT. 0 .AND. SIZCHG ) THEN
               CALL SMARA2( NPNT, X, Y, MXMNSZ, NDF, MKSIZ, STATUS )

*  Overlay the selected mark type with proper size on the '.'s
*  previous drawn.
               FIRST = .TRUE.
               CALL SMARA0( NPNT, X, Y, MARKTY, MKSIZ, X1, X2, Y1, Y2, 
     :                      POLY, .FALSE., FIRST, STATUS )
            END IF
     
*  If logging file was opened, write the sky coordinates of the marked
*  position to the file and then close the file, cancel parameter and
*  de-active FIO
            IF ( NPNT .GT. 0 .AND. LOGING ) THEN
               CALL SMARA3( NPNT, X, Y, LON, LAT, SCS, OUTFID, STATUS )
            END IF  

*  Delete the value of variable OPT and go be to get another value for
*  it.
            IF( LOOP ) THEN
               OPT = ' '
            ELSE
               OPT = 'EXIT'
            END IF

         END IF

*  Complete any outstanding polyline.
         CALL SMARA0( 1, X, Y, MARKTY, MKSIZ, X1, X2, Y1, Y2, POLY, 
     :                .TRUE., FIRST, STATUS )

      END DO

 940  CONTINUE

*  Close down the IRA astrometry package.
      CALL IRA_CLOSE( STATUS )

*   End the NDF context and release the locator to the ref. object.
      CALL NDF_END( STATUS )
      IF ( REFOBJ ) CALL REF_ANNUL( LOC, STATUS )
      
 980  CONTINUE

*  Set the current picture on entry as current and close down the AGI
*  and SGS.
      CALL AGS_DEASS( 'DEVICE', .TRUE., STATUS )

*  Close any log file and cancel the parameter.
      IF( LOGING ) CALL FIO_CANCL( 'LOGFILE', STATUS )

*  If a parameter null or abort value was given, annul the error.
      IF( STATUS .EQ. PAR__NULL .OR. STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SKYMARK_ERR1',
     :         'SKYMARK: Error drawing markers at specified positions.',
     :                 STATUS )
      END IF
      
      END
