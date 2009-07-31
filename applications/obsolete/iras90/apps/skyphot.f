      SUBROUTINE SKYPHOT( STATUS )
*+
*  Name:
*     SKYPHOT

*  Purpose:
*     Calculate integrated fluxes in rectangular, elliptical or
*     polygonal regions.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SKYPHOT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine calculates the integrated flux within a rectangular,
*     elliptical or polygonal region of an image, optionally
*     subtracting a supplied background surface brightness. Central
*     positions and polygon vertices may be specified using a graphics
*     cursor if an image is already displayed. Alternatively, sky
*     coordinates may be specified in response to parameter prompts, or
*     they may be read from a text file (see parameter MODE). The
*     dimensions of rectangular or elliptical apertures are specified
*     using parameter SIZE.

*  Usage:
*     SKYPHOT 

*  ADAM Parameters:
*     BACKVAL = REAL (Read)
*        A background surface brightness to be subtracted from the data
*        before integrating over the selected region. The value should
*        be supplied in units of MJy/sr.                           [0.0]
*     COORDS = LITERAL (Read)
*        Specifies the coordinate system used for referring to sky
*        positions. Valid values include ECLIPTIC, EQUATORIAL,
*        GALACTIC. See help on "Sky_coordinates" for more information
*        on available sky coordinate systems.
*                                        [current sky coordinate system]
*     DEVICE = DEVICE (Read)
*        The graphics workstation.         [The current graphics device]
*     FILE = FILENAME (Read)
*        A file from which to read sky coordinates. This is only
*        prompted for if parameter MODE has the value FILE. See the
*        help on "Coordinate_files" for a description of the format
*        required for this file. Note, the file must contain sky
*        coordinates, not image coordinates.
*     FLUX = REAL (Write)
*        An output parameter holding the last displayed flux density
*        value.
*     IN = NDF (Read)
*        The input NDF.
*     LAT = LITERAL (Read)
*        A latitude of a position (either the centre of a rectangular
*        or elliptical region, or a vertex of a polygon), in the
*        coordinate system specified by COORDS (eg if COORDS was
*        EQUATORIAL, LAT should be given a Declination value). LAT is
*        only prompted for if parameter MODE has the value KEYBOARD.
*        See help on "Sky_coordinates" for more information on
*        available sky coordinate systems.
*     LOGFILE = FILENAME (Write)
*        The name of a text file to receive a copy of the displayed
*        information. The run time default is for no log file to be
*        produced.                                                   [!]
*     LON = LITERAL (Read)
*        The longitude corresponding to the latitude value given for
*        parameter LAT. LON is only prompted for if parameter MODE has
*        the value KEYBOARD. See help on "Sky_coordinates" for more
*        information on available sky coordinate systems.
*     LOOP = LOGICAL (Read)
*        If true then application does not exit when the final input
*        position has been given. Instead, the user is allowed to
*        change some of the parameter values and to continue to
*        perform more integrations. See parameter OPTION.           [NO]
*     MEAN = REAL (Write)
*        An output parameter holding the last displayed mean surface
*        brightness value.
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
*        and LAT.                                               [CURSOR]
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
*        PLOT - Change the value of the PLOT parameter.
*
*        PEN - Change the value of the PEN parameter.
*
*        SHAPE - Change the value of the SHAPE parameter.
*
*        SIZE - Change the value of the SIZE parameter.
*
*        BACKVAL - Change the value of the BACKVAL parameter.
*        
*     PEN = INTEGER (Read)
*        The SGS pen number used to draw the graphics specified by 
*        parameter PLOT.                                             [3]
*     PLOT = LOGICAL (Read)
*        If a true value is supplied, then the area over which the flux
*        is to be integrated is outline on the graphics device
*        specified by parameter DEVICE. The pen colour specified by
*        parameter PEN is used. This parameter is only used if
*        parameter MODE is given the value CURSOR.                 [YES]
*     SHAPE = LITERAL (Read)
*        The shape of the region to be integrated over. It can take the
*        values RECTANGLE, ELLIPSE or POLYGON.                 [ELLIPSE]
*     SIGMA = REAL (Write)
*        An output parameter holding the last displayed value for the
*        standard deviation of the surface brightness.
*     SINGLE = LOGICAL (Read)
*        If TRUE, then the application normally exits after the user
*        has given a single input position. If parameter LOOP is TRUE
*        then the user may continue to give further single positions.
*        The value of this parameter is ignored when integrating
*        polygonal regions.                                         [NO]
*     SIZE = REAL (Read)
*        A pair of values giving the dimensions of the aperture
*        parallel to X and Y, in arc-minutes. If a single value is
*        given then the supplied value is used for both axes.

*  Examples:
*     SKYPHOT LOGFILE=FLUXES.LIS SIZE=10
*        This displays a cursor on top of the most recently created
*        DATA picture (within the current picture). The user may then
*        repeatedly select points, and integrated flux within a
*        circular aperture of diameter 10 arc-minutes is displayed on
*        the terminal and written to text file FLUXES.LIS. Giving a
*        point outside the picture causes the application to terminate.
*     SKYPHOT SHAPE=RECT SIZE=[3,5] MODE=KEY SINGLE
*        This prompts the user for a single pair of sky coordinates. The
*        integrated flux within a rectangular aperture of 3 by 5
*        arc-minutes is displayed, and the application exits.

*  Notes:
*     - Flux density values are displayed in units of Janskys, and 
*     surface brightness values in mega-Janskys per steradian.
*     - The last displayed values are written to output parameters
*     FLUX, MEAN and SIGMA and can be accessed by subsequent
*     applications.
*     - The displayed total flux values include only whole pixel
*     values.  No attempt is made to perform any interpolation for
*     pixels which are only partly within the aperture. For this reason
*     the actual aperture shape can only be considered to be accurate
*     to one pixel.  If this is a problem, the image can be
*     re-projected using application SKYALIGN so that it has smaller
*     pixels before using SKYPHOT.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JUL-1993 (DSB):
*        Original version.
*     22-SEP-1993 (DSB):
*        Parameters FLUX, MEAN and SIGMA added. SIZE made into a 2 value
*        parameter holding X and Y dimensions.
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
      INCLUDE 'IRI_PAR'          ! IRI_ constants
      INCLUDE 'GRP_PAR'          ! GRP_ constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants
                               
*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXVERT             ! Max. no. of vertices per polygon
      PARAMETER ( MXVERT = 30 )

*  Local Variables:
      CHARACTER COORDS*(IRA__SZSCS)! Sky coordinaste system specified
                                  ! within the input text file.
      CHARACTER FILE*(GRP__SZFNM) ! Name of input text file.
      CHARACTER MODE*8            ! Source of input coordinates.
      CHARACTER OPTION*8          ! Option for next operation.
      CHARACTER SCS*(IRA__SZSCS)  ! Sky coordinates system in which 
                                  ! keyboard input is expected, and in
                                  ! which output values are displayed.
      CHARACTER SHAPE*9           ! Aperture shape.
      CHARACTER TEXT( 2 )*(GRP__SZNAM)! Text strings representing
                                  ! a pair of coordinates.

      DOUBLE PRECISION A         ! Longitude value.
      DOUBLE PRECISION B         ! Latitude value.
      DOUBLE PRECISION PIXSIZ( 2 )! Nominal pixel dimensions, in
                                 ! radians.
      DOUBLE PRECISION XX        ! Image X coordinate.
      DOUBLE PRECISION YY        ! Image Y coordinate.

      INTEGER BAND               ! Waveband index for input NDF.
      INTEGER FD                 ! FIO identifier for log file.
      INTEGER IAT                ! Position of last significant
                                 ! character.
      INTEGER IDA                ! IRA identifier for astrometry info.
      INTEGER IGRP               ! GRP identifier for group holding
                                 ! input coordinates read from a file.
      INTEGER INDEX              ! Index into group identified by IGRP.
      INTEGER INDF               ! Identifier for input NDF.
      INTEGER IPDATA             ! Pointer to mapped data array.
      INTEGER LFILE              ! Used length of FILE.
      INTEGER LBND( 2 )          ! Lower bounds of NDF.
      INTEGER NDIM               ! No. of dimensions in input NDF.
      INTEGER NEL                ! No. of elements in mapped array.
      INTEGER NKEY               ! Choice device selected.
      INTEGER NVERT              ! No. of polygon vertices.
      INTEGER PEN                ! SGS pen for graphics.
      INTEGER PICID              ! AGI identifier for current picture.
      INTEGER SIZE               ! The size of the group identified by
                                 ! IGRP.
      INTEGER UBND( 2 )          ! Upper bounds of NDF.
      INTEGER ZONE               ! SGS zone identifier for current
                                 ! picture.

      LOGICAL CURAVL             ! True if a cursor is available.
      LOGICAL DEVOPN             ! True if a graphics device is open.
      LOGICAL EXIT               ! True if no more operations are to be
                                 ! performed.
      LOGICAL GOTFIL             ! True if text file has been read.
      LOGICAL LOGING             ! True if a log file is to be produced.
      LOGICAL LOOP               ! True if multiple operations are to be
                                 ! performed.
      LOGICAL MORE               ! True if more coordinates remain to be
                                 ! transformed in the current operation.
      LOGICAL NEWOP              ! True if a new oepration is being
                                 ! started.
      LOGICAL PLOT               ! True if graphics are to be produced.
      LOGICAL SINGLE             ! True if each operation is to be
                                 ! terminated after a single position.

      REAL ASIZE(2)              ! Dimensions of aperature in radians.
      REAL BACK                  ! Background surface brightness value.
      REAL FIRSTX                ! X coord. of first selected point.
      REAL FIRSTY                ! Y coord. of first selected point.
      REAL FLUXD                 ! Displayed flux density
      REAL LASTX                 ! X coord. of previous selected point.
      REAL LASTY                 ! Y coord. of previous selected point.
      REAL MEAN                  ! Displayed mean surface brightness
      REAL PIXSOL                ! Nominal pixel area, in steradians.
      REAL SCALE                 ! Factor for converting i/p units to
                                 ! jy/pixel.
      REAL SIGMA                 ! Displayed standard deviation
      REAL X                     ! Image X coordinate value.
      REAL X1                    ! Lower X bound of picture zone.
      REAL X2                    ! Upper X bound of picture zone.
      REAL XSIZE                 ! X size of the aperature in pixels.
      REAL XVERT( MXVERT )       ! Polygon vertices X coordinates.
      REAL Y                     ! Image Y coordinate value.
      REAL Y1                    ! Lower Y bound of picture zone.
      REAL Y2                    ! Upper Y bound of picture zone.
      REAL YSIZE                 ! Y size of the aperature in pixels.
      REAL YVERT( MXVERT )       ! Polygon vertices Y coordinates.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      GOTFIL = .FALSE.
      DEVOPN = .FALSE.

*  Establish the conditional message filter level using parameter
*  MSG_LEVEL.
      CALL MSG_IFGET( STATUS )

*  Initialise the IRA package.
      CALL IRA_INIT( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Get an identifier for the input NDF.
      CALL NDF_ASSOC( 'IN', 'READ', INDF, STATUS )

*  Get an IRA identifier for the astrometry information.
      CALL IRA_IMPRT( INDF, IDA, STATUS )

*  Get the nominal pixel dimensions, in radians, and the corresponding
*  solid angle.
      CALL IRA_PIXSZ( IDA, PIXSIZ, STATUS )
      PIXSOL = REAL( PIXSIZ( 1 )*PIXSIZ( 2 ) )

*  Get a factor for converting input data values to units of JY/PIXEL.
      CALL IRM_UNTIM( INDF, IRI__JPP, SCALE, BAND, STATUS )

*  Map the data array of the NDF.
      CALL NDF_MAP( INDF, 'DATA', '_REAL', 'READ', IPDATA, NEL, STATUS )

*  Find the bounds of the input NDF.
      CALL NDF_BOUND( INDF, 2, LBND, UBND, NDIM, STATUS )

*  See where the input coordinates should be read from; cursor, file or
*  keyboard.
      CALL PAR_CHOIC( 'MODE', 'Cursor', 'Cursor,Keyboard,File',
     :                .FALSE., MODE, STATUS )
      CALL CHR_UCASE( MODE )

*  If cursor mode is selected, see if graphics are to be produced, and
*  what pen is to be used. Initialise the positions of the first and
*  last points to be bad.
      IF( MODE .EQ. 'CURSOR' ) THEN
         CALL PAR_GET0L( 'PLOT', PLOT, STATUS )

         IF( PLOT ) THEN
            CALL PAR_GET0I( 'PEN', PEN, STATUS )

            FIRSTX = VAL__BADR
            FIRSTY = VAL__BADR
            LASTX = VAL__BADR
            LASTY = VAL__BADR

         END IF

      END IF

*  See if the user wants to loop round, performing multiple operations.
      CALL PAR_GET0L( 'LOOP', LOOP, STATUS )

*  Get the sky coordinate system to use.
      CALL IRA_GTSCS( 'COORDS', .FALSE., SCS, STATUS )

*  Get the aperture shape to use.
      CALL PAR_CHOIC( 'SHAPE', 'Ellipse', 'Ellipse,Rectangle,Polygon',
     :                .FALSE., SHAPE, STATUS )
      CALL CHR_UCASE( SHAPE )

*  See if the user wants to integrate only one flux per operation
*  (unless a polygonal area is being integrated).
      IF( SHAPE .NE. 'POLYGON' ) THEN
         CALL PAR_GET0L( 'SINGLE', SINGLE, STATUS )
      ELSE
         SINGLE = .FALSE.
      END IF

*  Get the linear dimensions of the aperture, in radians, so long
*  as the shape is not polygonal.
      IF( SHAPE .NE. 'POLYGON' ) THEN
         CALL IRM_DIMEN( 'SIZE', .FALSE., 0.0, ASIZE, STATUS )
      ELSE
         ASIZE( 1 ) = 0.0
         ASIZE( 2 ) = 0.0
      END IF

*  Get the background surface brightness, in MJY/sr.
      CALL PAR_GET0R( 'BACKVAL', BACK, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Convert the background value into the same units as the input data.
      BACK = BACK*PIXSOL*1.0E6/SCALE

*  Convert the aperture size to half-aperture sizes in pixels, in the X
*  and Y directions.
      XSIZE = 0.5*ASIZE( 1 )/PIXSIZ( 1 )
      YSIZE = 0.5*ASIZE( 2 )/PIXSIZ( 2 )

*  If required open a log file. The file descriptor returned in FD is
*  used to access this file.
      CALL SPHOB0( 'LOGFILE', SCS, LOGING, FD, STATUS )

*  If required, write the background value, aperture shape and aperture
*  size to the log file.
      IF( LOGING ) THEN
         TEXT( 1 ) = 'Background: '
         IAT = 16
         CALL CHR_PUTR( BACK, TEXT( 1 ), IAT )
         CALL CHR_APPND( ' MJy/sr', TEXT( 1 ), IAT )
         CALL FIO_WRITE( FD, TEXT( 1 )( : IAT ), STATUS )

         TEXT( 1 ) = 'Aperture shape: '
         IAT = 19
         CALL CHR_APPND( SHAPE, TEXT( 1 ), IAT )
         CALL FIO_WRITE( FD, TEXT( 1 )( : IAT ), STATUS )

         IF( SHAPE .NE. 'POLYGON' ) THEN
            TEXT( 1 ) = 'Aperture size: ( '
            IAT = 20
            CALL CHR_PUTR( REAL( ASIZE( 1 )*IRA__R2AM ), TEXT( 1 ),
     :      IAT )
            CALL CHR_APPND( ',', TEXT( 1 ), IAT )
            IAT = IAT + 1
            TEXT( 1 )( IAT : IAT ) = ' '
            CALL CHR_PUTR( REAL( ASIZE( 2 )*IRA__R2AM ), TEXT( 1 ),
     :      IAT )
            CALL CHR_APPND( ') Arc-mins', TEXT( 1 ), IAT )
            CALL FIO_WRITE( FD, TEXT( 1 )( : IAT ), STATUS )
         END IF

      END IF

*  Display the sky coordinate system.
      CALL MSG_SETC( 'SCS', SCS )
      CALL MSG_OUTIF( MSG__NORM, 'SKYPHOT_MSG1',
     :                '  Displaying ^SCS sky coordinates.', STATUS )

*  Flag that no graphics device has yet been opened.
      DEVOPN = .FALSE.

*  Flag that no input coordinates file has yet been obtained.
      GOTFIL = .FALSE.

*  Loop round until all requested operations have been performed.
      EXIT = .FALSE.
      DO WHILE( .NOT. EXIT .AND. STATUS .EQ. SAI__OK )
         CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Indicate that this is the start of a new operation.
         NEWOP = .TRUE.

*  Initialise the number of polygon vertices found so far.
         NVERT = 0

*  Loop round until all positions in this operation have been
*  processed.
         MORE = .TRUE.
         DO WHILE( MORE .AND. STATUS .EQ. SAI__OK )

*  If cursor mode is selected...
            IF( MODE .EQ. 'CURSOR' ) THEN
         
*  If a graphics device has already been set up, use it. Otherwise,
*  open the AGI database and set up the specified device.
               IF( .NOT. DEVOPN ) THEN 
                  CALL SPHOB1( 'DEVICE', PICID, X1, X2, Y1, Y2, STATUS )

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

                  IF( SHAPE .EQ. 'POLYGON' ) THEN
                     CALL MSG_OUTIF( MSG__NORM, 'SKYPHOT_MSG2',
     :'  Position the cursor at each vertex of the polygonal aperture'//
     :' and press any button (position the cursor outside the image '//
     :'to exit).', STATUS )

                  ELSE
                     CALL MSG_OUTIF( MSG__NORM, 'SKYPHOT_MSG3',
     :'  Position the cursor at each aperture centre and press '//
     :'any button (position the cursor outside the image to exit).',
     :                               STATUS )
                  END IF
      
                  CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Record the source of input coordinates  in the log file.
                  IF( LOGING ) THEN
                     CALL FIO_WRITE( FD, ' ', STATUS )
                     CALL FIO_WRITE( FD,
     :                        'Positions specified using the cursor',
     :                               STATUS )
                  END IF

*  Set the pen if any graphics are to be produced.
                  IF( PLOT ) CALL SGS_SPEN( PEN )

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
     :                PLOT .AND. SHAPE .EQ. 'POLYGON' ) THEN

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
                  IF( PLOT ) THEN

*  If a polyline is being produced...
                     IF( SHAPE .EQ. 'POLYGON' ) THEN

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

*  If a rectangular area is being used, draw the box.
                     ELSE IF( SHAPE .EQ. 'RECTANGLE' ) THEN
                        CALL SGS_BOX( X - XSIZE, X + XSIZE,
     :                                Y - YSIZE, Y + YSIZE )

*  If an elliptical area is being used, draw the ellipse.
                     ELSE IF( SHAPE .EQ. 'ELLIPSE' ) THEN
                        CALL IRM_ELLIP( X, Y, XSIZE, YSIZE, STATUS )

                     END IF

*  Flush the graphics.
                     CALL SGS_FLUSH

                  END IF

               END IF

*  If file mode is selected...
            ELSE IF( MODE .EQ. 'FILE' ) THEN

*  If this is a new operation...
               IF( NEWOP ) THEN

*  If no file contents are available...
                  IF( .NOT. GOTFIL ) THEN

*  Get the name of a file from the environment, read its contents into 
*  a GRP group, and close the file. The name of the coordinate system 
*  used by the data in the file is returned in COORDS.
                     CALL SPHOA0( 'FILE', IGRP, COORDS, SIZE, FILE,
     :                            LFILE, STATUS )

*  Cancel the parameter.
                     CALL PAR_CANCL( 'FILE', STATUS )

*  If all has gone well, indicate that the contents of a file are now
*  available, and initialise the pointer to the first coordinate. 
*  Record the source of coordinates in the log file.
                     IF( STATUS .EQ. SAI__OK ) THEN
                        GOTFIL = .TRUE.
                        INDEX = 1

                        IF( LOGING ) THEN
                           CALL FIO_WRITE( FD, ' ', STATUS )
                           CALL FIO_WRITE( FD,
     :                              'Positions specified in file '//
     :                              FILE( : MIN( 49, LFILE ) ), STATUS )
                        END IF

*  Otherwise, delete the group.
                     ELSE
                        CALL GRP_DELET( IGRP, STATUS )

                     END IF               

                  END IF

               END IF

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

*  Convert the text strings into floating point sky coordinate values.
                  CALL IRA_CTOD( TEXT( 1 ), TEXT( 2 ), COORDS, A, B,
     :                           STATUS )

*  Convert the input sky coordinates to image coordinates.
                  CALL IRA_TRANS( 1, A, B, .FALSE., COORDS, IDA, XX,
     :                            YY, STATUS )
                  IF( XX .NE. VAL__BADD ) THEN
                     X = REAL( XX )
                  ELSE
                     X = VAL__BADR
                  END IF

                  IF( YY .NE. VAL__BADD ) THEN
                     Y = REAL( YY )
                  ELSE
                     Y = VAL__BADR
                  END IF

               END IF

*  If keyboard mode is selected...
            ELSE IF( MODE .EQ. 'KEYBOARD' ) THEN

*  If this is a new operation...
               IF( NEWOP ) THEN

*  Record the source of the coordinates in the log file.
                  IF( LOGING ) THEN
                     CALL FIO_WRITE( FD, ' ', STATUS )
                     CALL FIO_WRITE( FD,
     :                      'Positions specified using the keyboard',
     :                                  STATUS )
                  END IF

               END IF
 
*  Get the sky coordinates of the aperture centre from the environment
*  and then cancel the parameter values.
               CALL MSG_BLANKIF( MSG__NORM, STATUS )
               CALL IRA_GETCO( 'LON', 'LAT',
     :                         ' of the aperture centre', SCS,
     :                         .FALSE., A, B, STATUS )

               CALL PAR_CANCL( 'LON', STATUS )
               CALL PAR_CANCL( 'LAT', STATUS )

               CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  If a null value has been supplied, annul the error and flag that the
*  current operation is complete.
               IF( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL( STATUS )
                  MORE = .FALSE.

*  Otherwise, convert to image coordinates.
               ELSE
                  CALL IRA_TRANS( 1, A, B, .FALSE., SCS, IDA, XX,
     :                            YY, STATUS )
                  IF( XX .NE. VAL__BADD ) THEN
                     X = REAL( XX )
                  ELSE
                     X = VAL__BADR
                  END IF

                  IF( YY .NE. VAL__BADD ) THEN
                     Y = REAL( YY )
                  ELSE
                     Y = VAL__BADR
                  END IF

               END IF

            END IF

*  Display a blank line before the first position in each operation.
            IF( NEWOP ) CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Now process the new position, if required.
            IF( MORE ) THEN

*  Report an error if an invalid position has been obtained.
               IF( ( X .EQ. VAL__BADR .OR. Y .EQ. VAL__BADR ) .AND.
     :             STATUS .EQ. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'SKYPHOT_ERR1', 'SKYPHOT: Supplied '//
     :                          'position has no corresponding image '//
     :                          'coordinates.', STATUS )
               ELSE

*  If the aperture is a polygon, store the new coordinates. Report an
*  error if the coordinate list is full.
                  IF( SHAPE .EQ. 'POLYGON' ) THEN      
                     NVERT = NVERT + 1

                     IF( NVERT .GT. MXVERT .AND.
     :                   STATUS .EQ. SAI__OK ) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'MAX', MXVERT )
                        CALL ERR_REP( 'SKYPHOT_ERR2', 'SKYPHOT: '//
     :                                'Maximum number of vertices '//
     :                                '(^MAX) exceeded.', STATUS )
                     ELSE 
                        XVERT( NVERT ) = X
                        YVERT( NVERT ) = Y

                     END IF

*  If the aperture is not a polygon, integrate the flux within the
*  aperture, and display and log the results.
                  ELSE
                     CALL SPHOZ1( LBND( 1 ), UBND( 1 ), LBND( 2 ),
     :                            UBND( 2 ), %VAL( IPDATA ), SCALE,
     :                            PIXSOL, BACK, X, Y, XSIZE, YSIZE,
     :                            LOGING, FD, IDA, SCS, SHAPE, MEAN,
     :                            FLUXD, SIGMA, STATUS )

                  END IF

               END IF

            END IF

*  If only one position per operation is allowed, ensure that no more
*  positions are transformed in this operation.
            IF( SINGLE ) MORE = .FALSE.

*  Indicate that this is no longer a new operation.
            NEWOP = .FALSE.

*  Go round for the next sky or image position.
         END DO

*  The operation is complete. If a polygonal area has been defined
*  If the aperture is not a polygon, integrate the flux within the
*  aperture, and display and log the results.
         IF( SHAPE .EQ. 'POLYGON' ) THEN
            CALL SPHOZ2( LBND( 1 ), UBND( 1 ), LBND( 2 ), UBND( 2 ),
     :                   %VAL( IPDATA ), SCALE, PIXSOL, BACK, NVERT,
     :                   XVERT, YVERT, LOGING, FD, IDA, SCS, MEAN,
     :                   FLUXD, SIGMA, STATUS )

         END IF

*  Separate operations with a blank line.
         CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  If the user has asked for mutiple operations to be performed, prepare
*  for the next one.
         IF( LOOP ) THEN

*  Loop round until an exit is requested, or a new operation is
*  requested or an error occurs.
            OPTION = ' '
            DO WHILE( OPTION .NE. 'CONTINUE' .AND. .NOT. EXIT .AND.
     :                STATUS .EQ. SAI__OK ) 

*  Get the next operation to perform, and then cancel the parameter.
               CALL PAR_CHOIC( 'OPTION', 'Continue', 'Continue,Exit,'//
     :                         'Mode,Plot,Pen,Shape,Size,Backval',
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
                  CALL PAR_CHOIC( 'MODE', MODE, 'Cursor,Keyboard,File',
     :                            .FALSE., MODE, STATUS )
                  CALL CHR_UCASE( MODE )

*  If the user has requested a change of graphics status, toggle the
*  current value.
               ELSE IF( OPTION .EQ. 'PLOT' ) THEN
                  PLOT = .NOT. PLOT

                  IF( PLOT ) THEN
                     CALL MSG_OUT( 'SKYPHOT_MSG4',
     :                           '  OK. Graphics will now be produced.',
     :                             STATUS )
                  ELSE
                     CALL MSG_OUT( 'SKYPHOT_MSG5',
     :                        '  OK. No graphics will now be produced.',
     :                             STATUS )
                  END IF

*  If the user has requested a change of graphics pen, cancel the 
*  old value and get a new value.
               ELSE IF( OPTION .EQ. 'PEN' ) THEN
                  CALL PAR_CANCL( 'PEN', STATUS )
                  CALL PAR_DEF0I( 'PEN', PEN, STATUS )
                  CALL PAR_GET0I( 'PEN', PEN, STATUS )

*  If the user has requested a change of shape, canel the old value and
*  get a new value. Record the new value in the log file.
               ELSE IF( OPTION .EQ. 'SHAPE' ) THEN
                  CALL PAR_CANCL( 'SHAPE', STATUS )
                  CALL PAR_CHOIC( 'SHAPE', SHAPE, 'Ellipse,Rectangle,'//
     :                            'Polygon', .FALSE., SHAPE, STATUS )
                  CALL CHR_UCASE( SHAPE )

                  IF( LOGING ) THEN
                     TEXT( 1 ) = 'Aperture shape: '
                     IAT = 19
                     CALL CHR_APPND( SHAPE, TEXT( 1 ), IAT )
                     CALL FIO_WRITE( FD, TEXT( 1 )( : IAT ), STATUS )
                  END IF

*  If a non-polygonal shape is now being used, but the current aperture 
*  size is zero, get a value for the aperture size and log it.
                  IF( SHAPE .NE. 'POLYGON' .AND.
     :                ASIZE( 1 ) .EQ. 0.0 ) THEN
                     CALL PAR_CANCL( 'SIZE', STATUS )
                     CALL IRM_DIMEN( 'SIZE', .TRUE., 0.0, ASIZE,
     :                                STATUS )

                     XSIZE = 0.5*ASIZE( 1 )/PIXSIZ( 1 )
                     YSIZE = 0.5*ASIZE( 2 )/PIXSIZ( 2 )

                     IF( LOGING ) THEN
                        TEXT( 1 ) = 'Aperture size: ( '
                        IAT = 20
                        CALL CHR_PUTR( REAL( ASIZE( 1 )*IRA__R2AM ),
     :                                 TEXT( 1 ), IAT )
                        CALL CHR_APPND( ',', TEXT( 1 ), IAT )
                        IAT = IAT + 1
                        TEXT( 1 )( IAT : IAT ) = ' '
                        CALL CHR_PUTR( REAL( ASIZE( 2 )*IRA__R2AM ),
     :                                 TEXT( 1 ), IAT )
                        CALL CHR_APPND( ') Arc-mins', TEXT( 1 ), IAT )
                        CALL FIO_WRITE( FD, TEXT( 1 )( : IAT ), STATUS )
                     END IF

                  END IF

*  If the user has requested a change of aperture size, canel the old
*  value and get a new value. Record the new value in the log file.
               ELSE IF( OPTION .EQ. 'SIZE' ) THEN
                  IF( SHAPE .NE. 'POLYGON' ) THEN

                     CALL PAR_CANCL( 'SIZE', STATUS )
                     CALL IRM_DIMEN( 'SIZE', .TRUE., 0.0, ASIZE,
     :                                STATUS )

                     XSIZE = 0.5*ASIZE( 1 )/PIXSIZ( 1 )
                     YSIZE = 0.5*ASIZE( 2 )/PIXSIZ( 2 )

                     IF( LOGING ) THEN
                        TEXT( 1 ) = 'Aperture size: ( '
                        IAT = 20
                        CALL CHR_PUTR( REAL( ASIZE( 1 )*IRA__R2AM ),
     :                                 TEXT( 1 ), IAT )
                        CALL CHR_APPND( ',', TEXT( 1 ), IAT )
                        IAT = IAT + 1
                        TEXT( 1 )( IAT : IAT ) = ' '
                        CALL CHR_PUTR( REAL( ASIZE( 2 )*IRA__R2AM ),
     :                                 TEXT( 1 ), IAT )
                        CALL CHR_APPND( ') Arc-mins', TEXT( 1 ), IAT )
                        CALL FIO_WRITE( FD, TEXT( 1 )( : IAT ), STATUS )
                     END IF

                  ELSE
                     CALL MSG_OUT( 'SKYPHOT_MSG6',
     :               '  Cannot specify size of polygonal apertures.',
     :                              STATUS )
                  ENDIF

*  If the user has requested a change of background value, canel the
*  old value and get a new value.
               ELSE IF( OPTION .EQ. 'BACKVAL' ) THEN
                  CALL PAR_CANCL( 'BACKVAL', STATUS )
                  CALL PAR_DEF0R( 'BACKVAL', BACK, STATUS )
                  CALL PAR_GET0R( 'BACKVAL', BACK, STATUS )

                  IF( LOGING ) THEN
                     TEXT( 1 ) = 'Background: '
                     IAT = 16
                     CALL CHR_PUTR( BACK, TEXT( 1 ), IAT )
                     CALL CHR_APPND( ' MJy/sr', TEXT( 1 ), IAT )
                     CALL FIO_WRITE( FD, TEXT( 1 )( : IAT ), STATUS )
                  END IF

*  Report an error for any option other than CONTINUE.
               ELSE IF( OPTION .NE. 'CONTINUE' .AND.
     :                  STATUS .EQ. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETC( 'O', OPTION )
                  CALL ERR_REP( 'SKYPHOT_ERR3',
     :                          'SKYPHOT: Unknown option - "^O"',
     :                          STATUS )
               END IF

            END DO

*  If looping is not required, indicate an immediate exit.
         ELSE
            EXIT = .TRUE.            

         ENDIF

*  Put some blank lines in th log file to indicate the end of the
*  previous operation.
         IF( LOGING ) THEN
            CALL FIO_WRITE( FD, ' ', STATUS )
            CALL FIO_WRITE( FD, ' ', STATUS )
         END IF

*  Do the next operation.
      END DO

      CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Write out the final displayed values to the appropriate parameters.
      CALL PAR_PUT0R( 'MEAN', MEAN, STATUS )
      CALL PAR_PUT0R( 'FLUX', FLUXD, STATUS )
      CALL PAR_PUT0R( 'SIGMA', SIGMA, STATUS )

*  Tidy up.
 999  CONTINUE

      CALL NDF_END( STATUS )
      CALL IRA_ANNUL( IDA, STATUS )
      CALL IRA_CLOSE( STATUS )

      IF( DEVOPN ) CALL AGS_DEASS( 'DEVICE', .TRUE., STATUS )
      IF( GOTFIL ) CALL GRP_DELET( IGRP, STATUS )
      IF( LOGING ) CALL FIO_CANCL( 'LOGFILE', STATUS )

*  If a parameter null or abort value was given, annul the error.
      IF( STATUS .EQ. PAR__NULL .OR. STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SKYPHOT_ERR4',
     :                 'SKYPHOT: Error finding integrated flux values.',
     :                 STATUS )
      END IF
      
      END
