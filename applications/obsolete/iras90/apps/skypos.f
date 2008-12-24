      SUBROUTINE SKYPOS( STATUS )
*+
*  Name:
*     SKYPOS

*  Purpose:
*     Find the sky coordinates of selected image positions, and
*     vice-verse.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SKYPOS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine returns the sky coordinates corresponding to
*     selected image positions, or the image coordinates corresponding
*     to selected sky positions, depending on the value of parameter
*     INVERSE. Positions may be specified using a graphics cursor if an
*     image is already displayed. Alternatively, coordinates may be
*     specified in response to parameter prompts, or they may be read
*     from a text file (see parameter MODE).

*  Usage:
*     SKYPOS 

*  ADAM Parameters:
*     COORDS = LITERAL (Read)
*        Specifies the coordinate system used for referring to sky
*        positions. Valid values include ECLIPTIC, EQUATORIAL,
*        GALACTIC. See help on "Sky_coordinates" for more information
*        on available sky coordinate systems.
*                                        [current sky coordinate system]
*     DEVICE = DEVICE (Read)
*        The graphics workstation.         [The current graphics device]
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
*     INSIDE =  (Write)
*        An output parameter which is set TRUE if the last supplied
*        position lies within the bounds of the NDF. The position must
*        also be within the bounds of the AGI DATA picture if CURSOR
*        mode is being used. It is set FALSE otherwise.
*     INVERSE = LOGICAL (Read)
*        SKYPOS usually expects the user to supply image coordinates as
*        input. The corresponding sky coordinates are then found and
*        displayed. However, if INVERSE is true then the user should
*        supply sky coordinates and the corresponding image coordinates
*        are found and displayed.                                   [NO]
*     LAT = LITERAL (Read)
*        A latitude of a position to be transformed, in the coordinate
*        system specified by COORDS (eg if COORDS was EQUATORIAL, LAT
*        should be given a Declination value). LAT is only prompted
*        for if parameter MODE has the value KEYBOARD. See help on 
*        "Sky_coordinates" for more information on available sky 
*        coordinate systems.
*     LATOUT = LITERAL (Write)
*        An output parameter to which is written the final displayed
*        latitude, in the coordinate system specified by COORDS.
*     LOGFILE = FILENAME (Write)
*        The name of a text file to receive a copy of the displayed
*        information. The run time default is for no log file to be
*        produced. Log files are created in a format suitable for use
*        with parameter FILE.                                        [!]
*     LON = LITERAL (Read)
*        A longitude of a position to be transformed, in the coordinate
*        system specified by COORDS (eg if COORDS was EQUATORIAL, LON
*        should be given a Right Ascension value). LON is only prompted
*        for if parameter MODE has the value KEYBOARD. See help on 
*        "Sky_coordinates" for more information on available sky 
*        coordinate systems.
*     LONOUT = LITERAL (Write)
*        An output parameter to which is written the final displayed
*        longitude, in the coordinate system specified by COORDS.
*     LOOP = LOGICAL (Read)
*        If true then application does not exit when the final input
*        position has been given. Instead, the user is allowed to
*        change some of the parameter values and to continue to
*        transform more positions. See parameter OPTION.            [NO]
*     MODE = LITERAL (Read)
*        MODE specifies the source of input coordinates. It can take the
*        following values;
*
*        CURSOR - The graphics cursor is used to specify the positions.
*        A DATA picture must previously have been created (eg a grey
*        scale image or contour plot). The most recently created DATA
*        picture contained within the current picture is used. The user
*        is given instructions on the use of the cursor.
*
*        FILE - The input positions are contained within the file
*        specified by parameter FILE.
*
*        KEYBOARD - The input positions are specified by parameters LON
*        and LAT (if INVERSE is FALSE), or X and Y (if INVERSE is TRUE).
*                                                               [CURSOR]
*     MSG_FILTER = LITERAL (Read)
*        The level of information displayed on the users screen and
*        logged to the log file. This should take one of the values
*        QUIET, NORMAL or VERBOSE (see help on "Message_filtering").
*                                       [current message filter setting]
*     OPTION = LITERAL (Read)
*        The action to perform once all positions have been processed.
*        This is only prompted for if parameter LOOP is TRUE (otherwise
*        a value of EXIT is assumed). It can take the following values;
*
*        CONTINUE - Continue to process more positions using the current
*        parameter settings.
*
*        EXIT - Exit the application without processing any more
*        positions.
*
*        MODE - Change the value of the MODE parameter.
*
*        MAPPING - Change the value of the INVERSE parameter.
*
*        COORDINATES - Change the value of the COORDS parameter.
*
*        PLOT - Change the value of the PLOT parameter.
*
*        PEN - Change the value of the PEN parameter.
*
*        Note, if MAPPING or COORDINATES is selected, then a new log
*        file will be created (if logging has been requested using
*        parameter LOGFILE) to ensure that each log file contains
*        coordinates of a single type.
*     ORIENT = REAL (Write)
*        An output parameter to which is written the position angle of
*        the Y axis (in degrees) at the last displayed position. This
*        may vary across an image due to projection effects.
*     PEN = INTEGER (Read)
*        The SGS pen number used to draw the graphics specified by 
*        parameter PLOT.                                             [3]
*     PIXSIZE = REAL (Write)
*        An output parameter to which is written a pair of values
*        giving the pixel dimensions (in arc-minutes) at the last
*        displayed position. These may vary across an image due to
*        projection effects.
*     PLOT = LITERAL (Read)
*        Specifies the sort of graphics which are to be used to mark the
*        positions selected in cursor mode. PLOT can take any of the 
*        values POLY,MARK,NONE. POLY causes a polygonal line to be drawn
*        between the selected points, MARK causes a cross to be drawn at
*        each point, NONE causes no graphics to be produced. The 
*        graphics are produced on the device specified by parameter 
*        DEVICE, using the pen specified by parameter PEN.        [MARK]
*     SINGLE = LOGICAL (Read)
*        If TRUE, then the application normally exits after the user
*        has given a single input position. If parameter LOOP is TRUE
*        then the user may continue to give further single positions.
*                                                                   [NO]
*     X = REAL (Read)
*        An X image coordinate of a position to be transformed. X is 
*        only prompted for if parameter MODE has the value KEYBOARD.
*     XOUT = REAL (Write)
*        An output parameter to which is written the final displayed
*        X image coordinate.
*     Y = REAL (Read)
*        A Y image coordinate of a position to be transformed. Y is 
*        only prompted for if parameter MODE has the value KEYBOARD.
*     YOUT = REAL (Write)
*        An output parameter to which is written the final displayed
*        Y image coordinate.

*  Examples:
*     SKYPOS LOGFILE=STARS.DAT
*        This displays a cursor on top of the most recently created
*        DATA picture (within the current picture). The user may then
*        repeatedly select points, and the image and sky coordinates of
*        each point are displayed on the terminal. Giving a point
*        outside the picture causes the application to terminate. The
*        coordinates are also recorded in a log file called STARS.DAT.
*     SKYPOS MODE=FILE IN=STARFIELD FILE=STARS.DAT LOOP INVERSE
*        This reads a list of sky coordinates from text file STAR.DAT
*        and displays the corresponding image coordinates (within NDF
*        STARFIELD) on the terminal. Once all the positions included in
*        STAR.DAT have been processed, the user is prompted for further
*        action using parameter OPTION. The user may choose (for
*        instance) to continue selecting more positions using the
*        cursor.
*     SKYPOS SINGLE MSG_FILTER=QUIET
*        This allows the user to indicate a single point on a displayed
*        DATA picture using the cursor, and the application then
*        immediately exits. No coordinates (or other information) are
*        displayed on the screen, but the sky coordinates of the
*        selected position are written to output parameters LONOUT and
*        LATOUT for use by subsequent applications.

*  Notes:
*     -  When converting from image to sky coordinates, the displayed 
*     sky coordinates correspond to the displayed image coordinates
*     which have been rounded to one decimal place, not to the original
*     supplied image coordinates before rounding.
*     -  If the current message filtering level (see parameter
*     MSG_FILTER) is VERBOSE, then additional information is displayed
*     on the screen for each position, describing the position angle of
*     the Y axis and the pixel dimensions. These can vary across an
*     image because of projection effects. The last displayed values
*     are written to the output parameters ORIENT and PIXSIZE.

*  Authors:
*     DSB: David Berry (STARLINK)
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     29-JAN-1993 (DSB):
*        Original version, based on SKYPOSITION by WG.
*     23-AUG-1993 (DSB):
*        Parameter XOUT and YOUT added.
*     22-SEP-1993 (DSB):
*        Parameters ORIENT and PIXSIZE added, and display of Y axis
*        position angle and pixel dimensions included.
*     1-OCT-1993 (DSB):
*        Parameter INSIDE added.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants
      INCLUDE 'IRA_PAR'          ! IRA_ constants
      INCLUDE 'GRP_PAR'          ! GRP_ constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants
                               
*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER COORDS*(IRA__SZSCS)! Input coordinate system; sky or 
                                  ! image.
      CHARACTER FILE*(GRP__SZFNM) ! Name of input text file.
      CHARACTER LSCS*(IRA__SZSCS) ! Value of SCS on previous operation.
      CHARACTER MODE*8            ! Source of input coordinates.
      CHARACTER OPTION*11         ! Option for next operation.
      CHARACTER PLOT*4            ! Type of graphics to produce.
      CHARACTER SCS*(IRA__SZSCS)  ! Sky coordinates system in which 
                                  ! keyboard input is expected, and in
                                  ! which output values are displayed.
      CHARACTER TEXT( 2 )*(GRP__SZNAM)! Text strings representing
                                  ! a pair of coordinates.

      DOUBLE PRECISION A         ! Longitude value.
      DOUBLE PRECISION B         ! Latitude value.
      DOUBLE PRECISION EPOCH     ! Epoch of observations.

      INTEGER FD                 ! FIO identifier for log file.
      INTEGER IDA                ! IRA identifier for astrometry info.
      INTEGER IGRP               ! GRP identifier for group holding
                                 ! input coordinates read from a file.
      INTEGER INDEX              ! Index into group identified by IGRP.
      INTEGER LFILE              ! Used length of FILE.
      INTEGER LBND( 2 )          ! Lower bounds of NDF.
      INTEGER NKEY               ! Choice device selected.
      INTEGER PEN                ! SGS pen for graphics.
      INTEGER PICID              ! AGI identifier for current picture.
      INTEGER SIZE               ! The size of the group identified by
                                 ! IGRP.
      INTEGER UBND( 2 )          ! Upper bounds of NDF.
      INTEGER ZONE               ! SGS zone identifier for current
                                 ! picture.

      LOGICAL ASTAVL             ! True if astrometry information is
                                 ! available.
      LOGICAL CURAVL             ! True if a cursor is available.
      LOGICAL DEVOPN             ! True if a graphics device is open.
      LOGICAL EXIT               ! True if no more operations are to be
                                 ! performed.
      LOGICAL GOTFIL             ! True if text file has been read.
      LOGICAL GOTSKY             ! True if the sky coordinates have been
                                 ! obtained from file, cursor or
                                 ! keyboard.
      LOGICAL INSIDE             ! True if the last position is inside
                                 ! both the NDF bounds and the AGI DATA
                                 ! picture (if appropriate).
      LOGICAL INVER              ! True if mapping from sky to image
                                 ! coordinates is to be performed.
      LOGICAL LINVER             ! Value of INVER on previous operation.
      LOGICAL LOGING             ! True if a log file is to be produced.
      LOGICAL LOOP               ! True if multiple operations are to be
                                 ! performed.
      LOGICAL MORE               ! True if more coordinates remain to be
                                 ! transformed in the current operation.
      LOGICAL NEWOP              ! True if a new oepration is being
                                 ! started.
      LOGICAL SINGLE             ! True if each operation is to be
                                 ! terminated after a single position.
      LOGICAL TITLE              ! True if titles for the output 
                                 ! columns are required.

      REAL ANGLE                 ! Position angle of Y axis 
      REAL FIRSTX                ! X coord. of first selected point.
      REAL FIRSTY                ! Y coord. of first selected point.
      REAL LASTX                 ! X coord. of previous selected point.
      REAL LASTY                 ! Y coord. of previous selected point.
      REAL PIXSIZ( 2 )           ! Pixel dimensions
      REAL X                     ! Image X coordinate value.
      REAL X1                    ! Lower X bound of picture zone.
      REAL X2                    ! Upper X bound of picture zone.
      REAL XLAST                 ! Last displayed X value
      REAL Y                     ! Image Y coordinate value.
      REAL Y1                    ! Lower Y bound of picture zone.
      REAL Y2                    ! Upper Y bound of picture zone.
      REAL YLAST                 ! Last displayed Y value

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      GOTFIL = .FALSE.
      DEVOPN = .FALSE.

*  Establish the conditional message filter level using parameter
*  MSG_LEVEL.
      CALL MSG_IFGET( 'MSG_FILTER', STATUS )

*  See where the input coordinates should be read from; cursor, file or
*  keyboard.
      CALL PAR_CHOIC( 'MODE', 'Cursor', 'Cursor,Keyboard,File',
     :                .FALSE., MODE, STATUS )
      CALL CHR_UCASE( MODE )

*  If cursor mode is selected, see what sort of graphics are to be 
*  produced, and what pen is to be used. Initialise the positions of 
*  the first and last points to be bad.
      IF( MODE .EQ. 'CURSOR' ) THEN
         CALL PAR_CHOIC( 'PLOT', 'Mark', 'Mark,Poly,None', .FALSE., 
     :                   PLOT, STATUS )
         CALL CHR_UCASE( PLOT )

         IF( PLOT .NE. 'NONE' ) THEN
            CALL PAR_GET0I( 'PEN', PEN, STATUS )

            FIRSTX = VAL__BADR
            FIRSTY = VAL__BADR
            LASTX = VAL__BADR
            LASTY = VAL__BADR

         END IF

      END IF

*  See if the user wants to loop round, performing multiple operations.
      CALL PAR_GET0L( 'LOOP', LOOP, STATUS )

*  See if the user wants to transform only one point per operation.
      CALL PAR_GET0L( 'SINGLE', SINGLE, STATUS )

*  See which direction the transformation should be applied in; forward
*  is from image to sky coordinate, inverse is from sky to image
*  coordinates.
      CALL PAR_GET0L( 'INVERSE', INVER, STATUS )

*  Get the sky coordinate system to use.
      CALL IRA_GTSCS( 'COORDS', .FALSE., SCS, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  If required open a log file. The file descriptor returned in FD is
*  used to access this file.
      CALL SPOSB0( 'LOGFILE', INVER, SCS, LOGING, FD, STATUS )
      LSCS = ' '

*  Flag that no graphics device has yet been opened.
      DEVOPN = .FALSE.

*  Flag that no input coordinates file has yet been obtained.
      GOTFIL = .FALSE.

*  Flag that no astrometry information is yet available.
      ASTAVL = .FALSE.

*  Initialise the IRA system.
      CALL IRA_INIT( STATUS )
                  
*  Loop round until all requested operations have been performed.
      EXIT = .FALSE.
      DO WHILE( .NOT. EXIT .AND. STATUS .EQ. SAI__OK )
         CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  If the coordinate system has changed, tell the user what sort of sky
*  coordinates will be displayed, and remember the current sky
*  coordinatye system.
         IF( SCS .NE. LSCS ) THEN
            CALL MSG_SETC( 'SCS', SCS )
            CALL MSG_OUTIF( MSG__NORM, 'SKYPOS_MSG1',
     :                      '  Displaying ^SCS sky coordinates',
     :                      STATUS )
            LSCS = SCS
         END IF

*  Indicate that this is the start of a new operation.
         NEWOP = .TRUE.
         TITLE = .TRUE.

*  Loop round until all positions in this operation have been
*  transformed and displayed.
         MORE = .TRUE.
         DO WHILE( MORE .AND. STATUS .EQ. SAI__OK )

*  If cursor mode is selected...
            IF( MODE .EQ. 'CURSOR' ) THEN
         
*  Initialise a flag to say that the next point is not inside the bounds
*  of the NDF or AGI picture.
               INSIDE = .FALSE.

*  If a graphics device has already been set up, use it. Otherwise,
*  open the AGI database and set up the specified device.
               IF( .NOT. DEVOPN ) THEN 
                  CALL SPOSB1( 'DEVICE', PICID, X1, X2, Y1, Y2, STATUS )

*  If the source of astrometry information is not yet defined, try to
*  get some astromety information from the AGI database. If none is
*  avialable, get it from the NDF specified by parameter IN
                  IF( .NOT. ASTAVL ) THEN
                     CALL IRM_GTAST( 'IN', PICID, LBND, UBND, IDA,
     :                               STATUS )

*  If succesful, indicate that astrometry information is now available.
                     IF( STATUS .EQ. SAI__OK ) ASTAVL = .TRUE.

                  END IF

*  If all has gone OK, indicate that the graphics device is now open.
                  IF( STATUS .EQ. SAI__OK ) THEN
                     DEVOPN = .TRUE.

*  Otherwise, try to close the graphics device, cancelling the parameter
*  in the process.
                  ELSE
                     CALL AGS_DEASS( 'DEVICE', .TRUE., STATUS )
                  END IF

               END IF

*  If this is a new operation ...
               IF( NEWOP ) THEN

*  Put the cursor at the centre of the picture.
                  CALL SGS_SETCU( 0.5 * ( X1 + X2 ), 0.5 * ( Y1 + Y2 ) )

*  Ensure the cursor is visible.
                  CALL SGS_CUVIS( .TRUE. )

*  Display instructions.
                  CALL MSG_BLANKIF( MSG__NORM, STATUS )
                  CALL MSG_OUTIF( MSG__NORM, 'SKYPOS_MSG2',
     :             '  Position the cursor and press any button '//
     :             '(position the cursor outside the image to exit).',
     :                            STATUS )
                  CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Indicate that the cursor returns image coordinates.
                  GOTSKY = .FALSE.

*  Record the source of input coordinates  in the log file.
                  IF( LOGING ) THEN
                     CALL FIO_WRITE( FD, ' ', STATUS )
                     CALL FIO_WRITE( FD,
     :                        '#  Positions specified using the cursor',
     :                               STATUS )
                  END IF

*  Set the pen if any graphics are to be produced.
                  IF( PLOT .NE. 'NONE' ) CALL SGS_SPEN( PEN )

               END IF

*  Get a cursor position in image coordinates.
               CALL SGS_REQCU( X, Y, NKEY )

*  If the cursor position is outside the area including the picture and
*  the safety margin, flag that the current operation is complete, 
*  switch off the cursor, complete any polyline, and set the first and 
*  last points bad.
               IF( X .LT. X1 .OR. X .GT. X2 .OR.
     :             Y .LT. Y1 .OR. Y .GT. Y2 ) THEN
                  MORE = .FALSE.
                  CALL SGS_CUVIS( .FALSE. )
                  
                  IF( FIRSTX .NE. VAL__BADR .AND.
     :                FIRSTY .NE. VAL__BADR .AND.
     :                LASTX .NE. VAL__BADR .AND.
     :                LASTY .NE. VAL__BADR .AND. 
     :                PLOT .EQ. 'POLY' ) THEN

                     CALL SGS_LINE( FIRSTX, FIRSTY, LASTX, LASTY )
                     CALL SGS_FLUSH

                     FIRSTX = VAL__BADR
                     FIRSTY = VAL__BADR
                     LASTX = VAL__BADR
                     LASTY = VAL__BADR
                  
                  END IF

*  If the position is within the usable area, see if any graphics are
*  to be produced.
               ELSE 

*  If a marker is to be produced, display a "+" at the cursor position.
                  IF( PLOT .EQ. 'MARK' ) THEN
                     CALL SGS_MARK( X, Y, 2 )

*  If a polyline is being produced...
                  ELSE IF( PLOT .EQ. 'POLY' ) THEN

*  If this is the first position, draw a dot, and store the starting 
*  coordinates.                     
                     IF( NEWOP ) THEN
                        CALL SGS_MARK( X, Y, 1 )
                        FIRSTX = X
                        FIRSTY = Y

*  Otherwise, draw a line between the current cursor position and the
*  last cursor position.
                     ELSE
                        CALL SGS_LINE( LASTX, LASTY, X, Y )
                     END IF

*  Save the current cursor position.
                     LASTX = X
                     LASTY = Y

*  Flush the graphics.
                     CALL SGS_FLUSH

                  END IF

               END IF

*  If file mode is selected...
            ELSE IF( MODE .EQ. 'FILE' ) THEN

*  If no file contents are available...
               IF( .NOT. GOTFIL ) THEN

*  Get the name of a file from the environment, read its contents into 
*  a GRP group, and close the file. The name of the coordinate system 
*  used by the data in the file is returned in COORDS.
                  CALL SPOSA0( 'FILE', IGRP, COORDS, GOTSKY, SIZE,
     :                         FILE, LFILE, STATUS )

*  Cancel the parameter.
                  CALL PAR_CANCL( 'FILE', STATUS )

*  If the input coordinates system is different to the requested output 
*  coordinate system, get the epoch of the obvservations. This need only
*  be done if the input coordinates are sky coordinates.
                  IF( COORDS .NE. SCS .AND. GOTSKY ) THEN
                     CALL PAR_DEF0D( 'EPOCH', IRA__IRJEP, STATUS ) 
                     CALL PAR_GET0D( 'EPOCH', EPOCH, STATUS )
                     CALL PAR_CANCL( 'EPOCH', STATUS )
                  END IF

*  If all has gone well, indicate that the contents of a file are now
*  available, and initialise the pointer to the first coordinate. 
*  Record the source of coordinates in the log file.
                  IF( STATUS .EQ. SAI__OK ) THEN
                     GOTFIL = .TRUE.
                     INDEX = 1

                     IF( LOGING ) THEN
                        CALL FIO_WRITE( FD, ' ', STATUS )
                        CALL FIO_WRITE( FD,
     :                              '#  Positions specified in file '//
     :                              FILE( : MIN( 49, LFILE ) ), STATUS )
                     END IF

*  Otherwise, delete the group.
                  ELSE
                     CALL GRP_DELET( IGRP, STATUS )

                  END IF               

               END IF

*  If the source of astrometry information is not yet defined, use
*  astrometry information stored in the NDF specified by parameter IN.
               CALL SPOSB2( 'IN', ASTAVL, LBND, UBND, IDA, STATUS )

*  If the group has been exhausted, flag that no more coordinates remain
*  to be transformed and delete the group.
               IF( INDEX .GE. SIZE ) THEN
                  MORE = .FALSE.
                  GOTFIL = .FALSE.
                  CALL GRP_DELET( IGRP, STATUS )

*  Otherwise, get the next pair of values from the group
               ELSE
                  CALL GRP_GET( IGRP, INDEX, 2, TEXT, STATUS )

*  Increment the index of the next pair of coordinates.               
                  INDEX = INDEX + 2

*  If the file contains sky coordinates, convert the text strings into
*  floating point sky coordinate values.
                  IF( GOTSKY ) THEN
                     CALL IRA_CTOD( TEXT( 1 ), TEXT( 2 ), COORDS, A, B,
     :                              STATUS )

*  Convert the input sky coordinates to the requested output 
*  coordinate system.
                     CALL IRA_CONVT( 1, A, B, COORDS, SCS, EPOCH, A, B,
     :                               STATUS )

*  Otherwise, convert the text strings into floating point image
*  coordinate values.
                  ELSE
                     CALL CHR_CTOR( TEXT( 1 ), X, STATUS )
                     CALL CHR_CTOR( TEXT( 2 ), Y, STATUS )
                  END IF

*  Initialise a flag to say that the next point is not inside the bounds
*  of the NDF.
                  INSIDE = .FALSE.

               END IF

*  If keyboard mode is selected...
            ELSE IF( MODE .EQ. 'KEYBOARD' ) THEN

*  If this is a new operation...
               IF( NEWOP ) THEN

*  If the source of astrometry information is not yet defined, use 
*  astrometry information stored in the NDF specified by parameter IN.
                  CALL SPOSB2( 'IN', ASTAVL, LBND, UBND, IDA, STATUS )

*  Record the sort of coordinates which are supplied; image or sky.
                  GOTSKY = INVER

*  Record the source of the coordinates in the log file.
                  IF( LOGING ) THEN
                     CALL FIO_WRITE( FD, ' ', STATUS )
                     CALL FIO_WRITE( FD,
     :                      '#  Positions specified using the keyboard',
     :                                  STATUS )
                  END IF

               END IF
 
*  If the inverse mapping is required, get a pair of sky coordinates
*  from the environment and then cancel the parameter values.
               CALL MSG_BLANKIF( MSG__NORM, STATUS )
               IF( INVER ) THEN
                  CALL IRA_GETCO( 'LON', 'LAT',
     :                            ' of the required position', SCS,
     :                            .FALSE., A, B, STATUS )

                  CALL PAR_CANCL( 'LON', STATUS )
                  CALL PAR_CANCL( 'LAT', STATUS )

*  If the forward mapping is required, get a pair of image coordinates
*  from the environment and then cancel the parameter values.
               ELSE
                  CALL PAR_GET0R( 'X', X, STATUS )
                  CALL PAR_GET0R( 'Y', Y, STATUS )

                  CALL PAR_CANCL( 'X', STATUS )
                  CALL PAR_CANCL( 'Y', STATUS )

               END IF
               CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  If a null value has been supplied, annul the error and flag that the
*  current operation is complete.
               IF( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL( STATUS )
                  MORE = .FALSE.

*  Otherwise, initialise a flag to say that the next point is not
*  inside the bounds of the NDF.
               ELSE
                  INSIDE = .FALSE.
               END IF

            END IF

*  Display a blank line before the first position in each operation.
            IF( NEWOP ) CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Now tranform and display the supplied coordinates. if necessary.
            IF( MORE ) THEN
               CALL SPOSB3( GOTSKY, INVER, SCS, IDA, LOGING, FD, TITLE, 
     :                      A, B, X, Y, STATUS )
               XLAST = X
               YLAST = Y

*  See if the position lies within the bounds of the NDF and set a flag
*  if it does.
               INSIDE = ( X .GT. REAL( LBND( 1 ) - 1 ) .AND.
     :                    X .LT. REAL( UBND( 1 ) ) .AND.
     :                    Y .GT. REAL( LBND( 2 ) - 1 ) .AND.
     :                    Y .LT. REAL( UBND( 2 ) ) )

*  Find the position angle of the Y axis and the pixel dimensions at
*  this position, and display them.
               CALL SPOSB4( X, Y, SCS, IDA, ANGLE, PIXSIZ, STATUS )

            END IF

*  If only one position per operation is allowed, ensure that no more
*  positions are transformed in this operation.
            IF( SINGLE ) MORE = .FALSE.

*  Indicate that this is no longer a new operation.
            NEWOP = .FALSE.

*  Go round for the next sky or image position.
         END DO

*  Separate operations with a blank line.
         CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  If the user has asked for mutiple operations to be performed, prepare
*  for the next one.
         IF( LOOP ) THEN

*  Remember the current mapping direction.
            LINVER = INVER

*  Loop round until an exit is requested, or a new operation is
*  requested or an error occurs.
            OPTION = ' '
            DO WHILE( OPTION .NE. 'CONTINUE' .AND. .NOT. EXIT .AND.
     :                STATUS .EQ. SAI__OK ) 

*  Get the next operation to perform, and then cancel the parameter.
               CALL PAR_CHOIC( 'OPTION', 'Continue', 'Continue,Exit,'//
     :                         'Mode,Mapping,Coordinates,Plot,Pen',
     :                         .FALSE., OPTION, STATUS )
               CALL CHR_UCASE( MODE )
               CALL PAR_CANCL( 'OPTION', STATUS )

*  If an exit has been requested, flag it.
               IF( OPTION .EQ. 'EXIT' ) THEN      
                  EXIT = .TRUE.

*  If the user has requested a change of mode, cancel the old value and
*  get a new value.
               ELSE IF( OPTION .EQ. 'MODE' ) THEN
                  CALL PAR_CANCL( 'MODE', STATUS )
                  CALL PAR_CHOIC( 'MODE', 'Cursor',
     :                            'Cursor,Keyboard,File', .FALSE.,
     :                            MODE, STATUS )
                  CALL CHR_UCASE( MODE )

*  If the user has requested a change in mapping, toggle the inverse
*  flag and report it.
               ELSE IF( OPTION .EQ. 'MAPPING' ) THEN
                  INVER = .NOT. INVER

                  IF( INVER ) THEN
                     CALL MSG_OUTIF( MSG__NORM, 'SKYPOS_MSG5',
     :                  '  OK. Sky to image mapping now being used...', 
     :                               STATUS )
                  ELSE
                     CALL MSG_OUTIF( MSG__NORM, 'SKYPOS_MSG6',
     :                  '  OK. Image to sky mapping now being used...', 
     :                               STATUS )

                  END IF

*  If the user has requested a change of sky coordinates, cancel the 
*  old value and get a new value.
               ELSE IF( OPTION .EQ. 'COORDINATES' ) THEN
                  CALL PAR_CANCL( 'COORDS', STATUS )
                  CALL IRA_GTSCS( 'COORDS', .TRUE., SCS, STATUS )

*  If the user has requested a change of graphics type, cancel the 
*  old value and get a new value.
               ELSE IF( OPTION .EQ. 'PLOT' ) THEN
                  CALL PAR_CANCL( 'PLOT', STATUS )
                  CALL PAR_CHOIC( 'PLOT', PLOT, 'Mark,Poly,None', 
     :                            .FALSE., PLOT, STATUS )
                  CALL CHR_UCASE( PLOT )

*  If the user has requested a change of graphics pen, cancel the 
*  old value and get a new value.
               ELSE IF( OPTION .EQ. 'PEN' ) THEN
                  CALL PAR_CANCL( 'PEN', STATUS )
                  CALL PAR_GET0I( 'PEN', PEN, STATUS )

*  Report an error for any option other than CONTINUE.
               ELSE IF( OPTION .NE. 'CONTINUE' .AND.
     :                  STATUS .EQ. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETC( 'O', OPTION )
                  CALL ERR_REP( 'SKYPOS_ERR1',
     :                          'SKYPOS: Unknown option - "^O"',
     :                          STATUS )
               END IF

            END DO

*  If the coordinate system or mapping has changed a new log file will 
*  be required.             
            IF( LOGING .AND. .NOT. EXIT. AND.
     :        ( INVER .NEQV. LINVER .OR. LSCS .NE. SCS ) ) THEN

*  Tell the user what is happening.      
               CALL MSG_OUTIF( MSG__NORM, 'SKYPOS_MSG7',
     :   '  A new log file is required because the output coordinate '//
     :   'system has changed', STATUS )

*  First close the old one.
               CALL FIO_CANCL( 'LOGFILE', STATUS )

*  Now open a new one
               CALL SPOSB0( 'LOGFILE', INVER, SCS, LOGING, FD, STATUS )

            END IF

*  If looping is not required, indicate an immediate exit.
         ELSE
            EXIT = .TRUE.            

         ENDIF

*  Do the next operation.
      END DO

*  Write the final longitude and latitude values to the output 
*  parameters.
      CALL IRA_DTOC( A, B, SCS, 0, TEXT( 1 ), TEXT( 2 ), STATUS )
      CALL PAR_PUT0C( 'LONOUT', TEXT( 1 ), STATUS )
      CALL PAR_PUT0C( 'LATOUT', TEXT( 2 ), STATUS )

*  Write the final (used) image coordinate values to the output 
*  parameters.
      CALL PAR_PUT0R( 'XOUT', XLAST, STATUS )
      CALL PAR_PUT0R( 'YOUT', YLAST, STATUS )

*  Write the final Y axis position angle and pixel dimensions to the
*  output parameters.
      CALL PAR_PUT0R( 'ORIENT', ANGLE, STATUS )
      CALL PAR_PUT1R( 'PIXSIZE', 2, PIXSIZ, STATUS )

*  Write the flag which indicates if the final position was inside the
*  bounds of the NDF (and AGI picture if it was specified using the
*  cursor) to an output parameter.
      CALL PAR_PUT0L( 'INSIDE', INSIDE, STATUS )

*  Tidy up.
      CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Tidy up (including saving the astrometry information associated with 
*  the DATA picture).
 999  CONTINUE
      
      IF( ASTAVL ) THEN
         IF( DEVOPN ) CALL IRM_PTAST( PICID, IDA, STATUS )
         CALL IRA_ANNUL( IDA, STATUS )
      END IF

      IF( DEVOPN ) CALL AGS_DEASS( 'DEVICE', .TRUE., STATUS )
      IF( GOTFIL ) CALL GRP_DELET( IGRP, STATUS )

      CALL IRA_CLOSE( STATUS )
      IF( LOGING ) CALL FIO_CANCL( 'LOGFILE', STATUS )

*  If a parameter null or abort value was given, annul the error.
      IF( STATUS .EQ. PAR__NULL .OR. STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SKYPOS_ERR2',
     :             'SKYPOS: Error displaying sky or image coordinates.',
     :                 STATUS )
      END IF
      
      END
