      SUBROUTINE PICCUR ( STATUS )
*+
*  Name:
*     PICCUR

*  Purpose:
*     Uses a cursor to select the current picture and to report the
*     co-ordinates of points.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PICCUR( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application allows you to select a new current picture in
*     the graphics database using the cursor.  The picture associated
*     with the last-selected point becomes the new current picture.

*     This task also uses the cursor to read Cartesian co-ordinates
*     from the chosen graphics device and displays them on your
*     terminal.  In addition if the co-ordinate frame changes between
*     selected positions, the comment, name, and any label associated
*     with the new picture are appended to the message.  There is an
*     option to let you store the co-ordinates, and their picture names
*     and labels in a text file.

*     There are three modes of operation to define which co-ordinate
*     system/picture is to be used.  These are ANCHOR, CURRENT and
*     DYNAMIC.  See the parameter MODE for details.

*     In ANCHOR or DYNAMIC modes there is an option to select only
*     pictures of a certain name in the database.  This is most useful
*     when DATA pictures are covered by transparent FRAME pictures.

*  Usage:
*     piccur [mode] [name] [logfile] [device]

*  ADAM Parameters:
*     COSYS = LITERAL (Read)
*        The co-ordinate system to be used.  This can be either "World"
*        or "Data".  "World" makes the world co-ordinates of the cursor
*        position to be reported.  World co-ordinates that relate to a
*        location in a data array will be in array pixels.  If COSYS =
*        "Data" the graphics database is examined for data co-ordinates
*        stored via a transformation.  Data co-ordinates are arbitrary
*        but most often they will be a linear or logarithmic
*        transformation of the world co-ordinates.  For example, the x
*        co-ordinate of a spectrum would be given in pixels if COSYS =
*        "World", but if COSYS = "Data" the x co-ordinate could be in
*        wavelength units, such as Angstroms.  If the database does not
*        have a world-to-data transformation for a given picture, the
*        value of this parameter is irrelevant and world co-ordinates
*        will be reported for that picture. [Current co-ordinate system]
*     DEVICE = DEVICE (Read)
*        The graphics workstation. [The current graphics device]
*     DOUBLE = _LOGICAL (Read)
*        If TRUE, co-ordinates will be reported, written to the output
*        parameters, and stored in the text file in double precision,
*        otherwise single precision is used.  [FALSE]
*     LOGFILE = FILENAME (Write)
*        The name of the text file in which the co-ordinates of points
*        selected with the cursor may be stored.  A null string (!)
*        means that no file is created.  The suggested default is the
*        current value. [!]
*     MODE = LITERAL (Read)
*        The mode defining the co-ordinate system/picture in which
*        cursor positions are returned.  There are three options.

*           "Current" selects the current picture in the AGI
*           database and reports the position of a point selected by
*           the cursor.  If the point does not lie within the picture,
*           an extrapolated position is reported.

*           "Dynamic" selects the topmost picture in the AGI
*           database which encompasses that position selected.  Thus
*           the second and subsequent cursor hits may result in the
*           selection of a new picture.  On exit the last picture
*           selected becomes the current picture.

*           "Anchor" lets the first cursor hit select a
*           picture which remains current throughout the running of
*           the application.  If subsequent cursor hits fall outside
*           the extent of this picture, a position extrapolated from
*           the picture's co-ordinate system is reported.  On exit the
*           anchor picture becomes the current picture.
*        ["Dynamic"]
*     NAME = LITERAL (Read)
*        Only pictures of this name are to be selected.  A null string
*        (!) or blanks means that pictures of all names may be selected.
*        [!]
*     XC = _DOUBLE (Write)
*        The x co-ordinate of the last point selected with the cursor.
*     YC = _DOUBLE (Write)
*        The y co-ordinate of the last point selected with the cursor.

*  Examples:
*     piccur
*        This obtains the co-ordinates of any visible picture for the
*        current graphics device by use of the cursor.  In this and all
*        the examples, the picture containing the last-selected point
*        becomes the new picture.
*     piccur cosys=w
*        This obtains the world co-ordinates of any visible picture for
*        the current graphics device by use of the cursor.
*     piccur current device=graphon
*        This obtains the co-ordinates of any visible picture in the
*        reference frame of the current picture of the Graphon device.
*     piccur logfile=stars.dat name=data
*        This obtains the co-ordinates of any visible DATA picture for
*        the current graphics device.  The x-y co-ordinates, and their
*        picture names and labels are stored in the text file called
*        stars.dat.

*  Notes:
*     -  Should an error occur trying to obtain the base picture for
*     ANCHOR or DYNAMIC modes, the current picture is unchanged.
*     -  In DYNAMIC and ANCHOR modes, if the cursor is situated at a
*     point where there are no pictures of the selected name, the
*     co-ordinates in the base picture are reported.

*  Algorithm:
*     -  Find the mode of operation, the name of valid pictures and
*     whether or not world co-ordinates are to be reported.
*     -  Associate the graphics device and activate SGS
*     -  If mode is dynamic or anchor then get and select the base
*     picture for the current workstation, and create an SGS zone for
*     the base.  For cursor mode create an SGS zone for the input
*     current picture.
*     -  Check there is a cursor and prepare it.  Set the initial cursor
*     position as the centre of the current picture.
*     -  Loop until status is bad on the escape button has been pressed.
*        o  Synchronise the cursor with the output.  Read the cursor
*        position.  If it is an acceptable choice then
*           -  If mode is dynamic or it is the first hit with anchor
*           mode then
*              o  Get the last picture of the chosen name which
*              encompasses that cursor position.  If there is no picture
*              of that name at the cursor position then use the base
*              zone instead.  Store the id of the last picture. Convert
*              the cursor position in base-zone co-ordinates to to those
*              in the selected picture.  In dynamic mode select the new
*              zone containing the cursor and release the old zone.  In
*              anchor set the cursor to its position in the anchor
*              zone's co-ordinates.
*           -  For other modes move the cursor to the new position if
*           it is outside the current zone.
*           -  Synchronise the output with the cursor. Report the cursor
*           position, and only report details of the current picture
*           when the picture has changed.  Write the co-ordinates to the
*           log file if required.  In dynamic mode reselect the base
*           picture and create a new zone from it.  Annul the redundant
*           picture identifiers.
*        o  Make the last picture the current picture on completion.
*     -  Put the last x-y co-ordinates into the output parameters.
*     -  Close an open log file
*     -  Deactivate SGS and close AGI workstation.

*  Related Applications:
*     KAPPA: CURSOR, PICBASE, PICDATA, PICEMPTY, PICENTIRE, PICFRAME,
*     PICLIST, PICSEL, PICVIS.

*  Authors:
*     JM: Jo Murray  (STARLINK)
*     MJC: Malcolm J. Currie  (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1995 August 25 (MJC):
*        Original version based on CURSOR by MJC and JM.
*     {enter_any_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'PAR_ERR'          ! Parameter-system errors
      INCLUDE 'AGI_ERR'          ! AGI error constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NCHLIN             ! Maximum number of characters in a
                                 ! an output record
      PARAMETER ( NCHLIN = 80 )

      INTEGER SZNAM              ! Length of picture name
      PARAMETER ( SZNAM = 15 )

*  Local Variables:
      CHARACTER * ( 256 ) COMENT ! Comment associated with the latest
                                 ! picture
      CHARACTER * ( 6 ) COSYS    ! Co-ordinate system
      LOGICAL CURCHO             ! Cursor is available with suitable
                                 ! number of choices?
      LOGICAL DEVCAN             ! The device parameter is to be
                                 ! cancelled?
      LOGICAL DOUBLE             ! Precision of co-ordinates is double?
      DOUBLE PRECISION DXD       ! x data co-ordinate to be reported
      DOUBLE PRECISION DYD       ! y data co-ordinate to be reported

      INTEGER FD                 ! File description
      LOGICAL FIRST              ! It is the first position to be read?
      LOGICAL HASLAB             ! The picture has a label?
      INTEGER HITVAL             ! The selected choice of the cursor
      LOGICAL IMGDIS             ! Device is nominally an image display?
      CHARACTER * ( 80 ) IMGMES( 4 ) ! Informational messages if device is
                                 ! an image display
      CHARACTER * ( SZNAM ) LABEL ! Picture label
      INTEGER LASPIC             ! Last picture's identifier
      CHARACTER *( NCHLIN ) LINE ! Line buffer which takes the image
                                 ! data and transfers it to the file
      LOGICAL LOGPOS             ! A log of the positions is written to
                                 ! a text file?
      CHARACTER * ( 10 ) MODE    ! The mode that defines which
                                 ! co-ordinate system is to be used
      CHARACTER * ( DAT__SZNAM ) NAME ! Only pictures of this name are
                                 !  to be selected
      INTEGER NCHAR              ! Number of characters in a record
                                 ! written to the log file
      LOGICAL NEWPIC             ! The latest cursor position lies in a
                                 ! different picture from the previous?
      INTEGER NEWZON             ! SGS zone identifier of the latest
                                 ! zone
      INTEGER NTERMS             ! Number of lines of terminal messages
      INTEGER NIMGMS             ! Number of lines of image-display
                                 ! messages
      LOGICAL NONAME             ! A picture of the chosen name could
                                 ! not be found at the cursor position?
      INTEGER NUPIC              ! The latest picture's identifier
      INTEGER PICID              ! Current (input) picture identifier
      INTEGER PICIDB             ! Base picture identifier
      INTEGER PICIDO             ! Picture identifier on exit
      CHARACTER * ( DAT__SZNAM ) PNAME ! Name associated with the latest
                                 ! picture
      CHARACTER * ( 80 ) TERMES( 4 ) ! Informational messages if
                                 ! device is a terminal
      REAL X1, Y1                ! Lower-left corner of the initial
                                 ! picture
      REAL X2, Y2                ! Upper-right corner of the initial
                                 ! picture
      REAL XD                    ! x data co-ordinate to be reported
      REAL XIN                   ! x co-ordinate as measured by the
                                 ! cursor
      REAL XM, YM                ! Size of the initial picture
      REAL XOUT                  ! x world co-ordinate
      REAL YD                    ! y data co-ordinate to be reported
      REAL YIN                   ! y co-ordinate as measured by the
                                 ! cursor
      REAL YOUT                  ! y world co-ordinate
      LOGICAL WORLD              ! Only world co-ordinates are to be
                                 ! reported?
      INTEGER ZONID              ! SGS zone identifier of the initial
                                 ! picture

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      DEVCAN = .FALSE.

*  Get the mode parameter.
      CALL PAR_CHOIC( 'MODE', 'Dynamic', 'Dynamic,Current,Anchor',
     :                .TRUE., MODE, STATUS )

*  Get the reporting precision.
      CALL PAR_GTD0L( 'DOUBLE', .FALSE., .TRUE., DOUBLE, STATUS )

*  Get the type of co-ordinates to report.
      CALL PAR_CHOIC( 'COSYS', 'Data', 'Data,World', .FALSE., COSYS,
     :                STATUS )
      WORLD = COSYS .EQ. 'WORLD'

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Get the name parameter.  A null value is made equivalent to a blank
*  string, i.e. all pictures of any name may be selected.  It cannot
*  apply to the CURRENT mode because there is only one current picture
*  and that is already defined.
      IF ( MODE( 1:3 ) .EQ. 'DYN' .OR. MODE( 1:3 ) .EQ. 'ANC' ) THEN
         CALL PAR_DEF0C( 'NAME', ' ', STATUS )
         CALL PAR_GET0C( 'NAME', NAME, STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            NAME = ' '
            CALL ERR_ANNUL( STATUS )
         END IF

         IF ( STATUS .NE. SAI__OK ) GOTO 999
      END IF

*  Attempt to obtain and open a log file to output the results.  A null
*  value, meaning no logfile is required, is handled invisibly.
      LOGPOS = .FALSE.
      CALL ERR_MARK
      CALL FIO_ASSOC( 'LOGFILE', 'WRITE', 'LIST', 80, FD, STATUS )

      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
         LOGPOS = .TRUE.
      END IF
      CALL ERR_RLSE
      IF ( STATUS .NE. SAI__OK ) GOTO 999

      IF ( LOGPOS ) CALL MSG_OUT( 'LOG', 'Logging to $LOGFILE', STATUS )

*  Start the graphics system.
*  ==========================

*  Start a new AGI context.
      CALL AGI_BEGIN

*  Get the graphics device, and open SGS.
      CALL AGI_ASSOC( 'DEVICE', 'UPDATE', PICID, STATUS )

*  Start a new AGI context.
      CALL AGI_BEGIN

*  Activate SGS.
      CALL AGS_ACTIV( STATUS )

*  If the graphics device was not available, report the error and leave
*  the programme.
      IF ( STATUS .NE. SAI__OK ) THEN

         IF ( STATUS .NE. PAR__ABORT ) THEN
            CALL ERR_REP( 'PICCUR__NID',
     :        'Graphics device not available or not recognised.',
     :        STATUS )
         END IF
         DEVCAN = .TRUE.
         GOTO 980
      END IF

*  Put out a blank line to ensure the commentary appears on the alpha
*  plane of the terminal.
      CALL MSG_BLANK( STATUS )

      IF ( MODE( 1:3 ) .EQ. 'DYN' .OR. MODE( 1:3 ) .EQ. 'ANC' ) THEN

*  Inquire the base picture for the current workstation.
         CALL AGI_IBASE( PICIDB, STATUS )

*  Select this as the current picture.
         CALL AGI_SELP( PICIDB, STATUS )

*  Create a new SGS zone from the current picture.
         CALL AGS_NZONE( ZONID, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'PICCUR__NOBAS',
     :        'Unable to get the base picture/zone from the database.',
     :        STATUS )

*  Reset current picture to the input picture.  (Actually happens when
*  AGI_END is called.)
            PICIDO = PICID
            DEVCAN = .TRUE.
            GOTO 980
         END IF
      ELSE

*  Create a new SGS_ZONE from current picture.
         CALL AGS_NZONE( ZONID, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'PICCUR__NCURP',
     :        'Unable to get the current zone from the database.',
     :        STATUS )
            DEVCAN = .TRUE.
            GOTO 980
         END IF
      END IF

*  Create informational messages for the interaction.
      IMGMES( 1 ) = 'Use the graphics cursor to define the next '/
     :              /'point or picture...'
      IMGMES( 2 ) = '   Press left button on mouse/trackerball to '/
     :              /'select a point.'
      IMGMES( 3 ) = '   Press right button on mouse/trackerball to '/
     :              /'end input.'
      IMGMES( 4 ) = ' '
      NIMGMS = 4

      TERMES( 1 ) = 'Use the graphics cursor to select the next '/
     :              /'point or picture...'
      TERMES( 2 ) = '   Press keyboard "1" or space key to select a '/
     :               /'point.'
      TERMES( 3 ) = '   Press keyboard "." to end input.'
      TERMES( 4 ) = ' '
      NTERMS = 4

*  Prepare the cursor.
      CALL KPG1_PRCUR( 1, TERMES, NTERMS, IMGMES, NIMGMS, '12 .',
     :                 CURCHO, IMGDIS, STATUS )
      IF ( .NOT. CURCHO .OR. STATUS .NE. SAI__OK ) GOTO 980

*  Get initial cursor position as the centre of the current picture.
      CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )
      XIN = 0.5 * ( X1 + X2 )
      YIN = 0.5 * ( Y1 + Y2 )
      CALL SGS_SETCU( XIN, YIN )

*  Initialise some values before the main loop is entered.
      HITVAL = 1
      LASPIC = PICID
      FIRST = .TRUE.
      NEWPIC = .TRUE.

*  Loop until the escape choice is selected
      DO WHILE ( HITVAL .GT. 0 .AND. HITVAL .LT. 4 .AND.
     :           STATUS .EQ. SAI__OK )

*  Start a new error context.
         CALL ERR_MARK

         NONAME = .FALSE.

*  If a message has already been displayed, and then the cursor is
*  used, the next message is no longer in synchronisation with the
*  cursor.  So synchronise the message system.
         CALL MSG_SYNC( STATUS )

*  Read the cursor position and button value.
         CALL SGS_REQCU( XIN, YIN, HITVAL )
         IF ( HITVAL .GT. 0 .AND. HITVAL .LT. 4 ) THEN

*  Dynamic or the first reading in anchor mode?
            IF ( MODE( 1:3 ) .EQ. 'DYN' .OR.
     :        ( ( MODE( 1:3 ) .EQ. 'ANC' ) .AND. FIRST ) ) THEN

*  Get the last picture of the chosen name which encompasses the cursor
*  position.
               CALL AGI_RCLP( NAME, XIN, YIN, NUPIC, STATUS )

*  Watch for the case when there is no picture of that name at the
*  selected point.
               IF ( STATUS .EQ. AGI__NONAM ) THEN
                  CALL ERR_ANNUL( STATUS )

*  Use the base zone in these cases.
                  NUPIC = PICIDB
                  CALL AGI_SELP( NUPIC, STATUS )
                  XOUT = XIN
                  YOUT = YIN

*  Set a flag to indicate what has happened.
                  NONAME = .TRUE.
               END IF

*  Decide if the cursor is situated in a different picture from last
*  time.
               CALL AGI_ISAMP( LASPIC, NEWPIC, STATUS )
               NEWPIC = .NOT. NEWPIC

*  The last picture is no long needed, so annul it unless it is the
*  input picture or the base picture.
               IF ( LASPIC .NE. PICID .AND.
     :            ( LASPIC .NE. PICIDB .AND. MODE( 1:3 ) .NE. 'CUR' ) )
     :           CALL AGI_ANNUL( LASPIC, STATUS )

*  Record the current picture as the new last picture.
               LASPIC = NUPIC

               IF ( .NOT. NONAME ) THEN

*  Create an SGS zone associated with the chosen picture.
                  CALL AGS_NZONE( NEWZON, STATUS )

*  Convert cursor position in base-zone co-ordinates to those of the
*  selected picture.
                  CALL SGS_TPZ( ZONID, XIN, YIN, NEWZON, XOUT, YOUT,
     :                          STATUS )
               END IF

*  Tidy up to avoid exhaustion of available SGS zones.
               IF ( MODE( 1:3 ) .EQ. 'DYN' )THEN
                  IF ( .NOT. NONAME ) THEN
                     CALL SGS_SELZ( ZONID, STATUS )
                     CALL SGS_RELZ( NEWZON )
                  END IF
               ELSE

*  Move the cursor to its position in the anchor zone's co-ordinates.
                  CALL SGS_SETCU( XOUT, YOUT )
               END IF

*  Non-dynamic modes.

            ELSE

*  In current mode the picture identifier will always be the input
*  identifier.  It is placed in the loop so that the picture updating
*  mechanism for dynamic modes is unaffected.
               NUPIC = PICID

*  Set for reporting later.
               XOUT = XIN
               YOUT = YIN

*  Move the cursor to the new position, which may be outside the
*  current zone.
               CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )
               IF ( XIN .LT. X1 .OR. XIN .GT. X2 .OR. YIN .LT. Y1 .OR.
     :              YIN .GT. Y2 ) THEN
                  CALL SETCR( XIN, YIN, STATUS )
               END IF
            END IF

*  Obtain details of the new picture.
            IF ( FIRST .OR. NEWPIC ) THEN
               CALL AGI_ICOM( COMENT, STATUS )
               CALL MSG_SETC( 'COMENT', COMENT )

               CALL AGI_INAME( PNAME, STATUS )
               CALL MSG_SETC( 'PNAME', PNAME )

*  Obtain the label associated with the picture, if a label exists.
               HASLAB = .FALSE.
               CALL AGI_ILAB( NUPIC, LABEL, STATUS )
               IF ( LABEL( 1:1 ) .NE. ' ' ) THEN
                  CALL MSG_SETC( 'LABEL', LABEL )
                  HASLAB = .TRUE.
               END IF

            END IF

*  Transform the world co-ordinates to data co-ordinates in the current
*  picture unless requested not to do so, when use the world
*  co-ordinates directly.  Often the conversion will make no difference
*  because there is no transformation in the database and the identity
*  transformation is performed.  Convert to the precision required.
            IF ( DOUBLE ) THEN

*  First double precision.
               IF ( WORLD ) THEN
                  DXD = DBLE( XOUT )
                  DYD = DBLE( YOUT )
               ELSE
                  CALL AGI_TWTDD( -1, 1, DBLE( XOUT ), DBLE( YOUT ),
     :                            DXD, DYD, STATUS )
               END IF

*  Put the co-ordinates into message tokens.
               CALL MSG_SETD( 'XVAL', DXD )
               CALL MSG_SETD( 'YVAL', DYD )

            ELSE

*  Alternatively, single precision.
               IF ( WORLD ) THEN
                  XD = XOUT
                  YD = YOUT
               ELSE
                  CALL AGI_TWTOD( -1, 1, XOUT, YOUT, XD, YD, STATUS )
               END IF

*  Put the co-ordinates into message tokens.
               CALL MSG_SETR( 'XVAL', XD )
               CALL MSG_SETR( 'YVAL', YD )
            END IF

*  There is now a change from the graphics cursor operation to report
*  values on the text screen (assuming the device is a terminal).  In
*  order for the message to appear in the correct plane, there must be
*  a delay, so that the graphics system can complete its work before
*  the (faster and independent) message system reports the cursor
*  position.  The following calls achieves this synchronisation.
            CALL MSG_SYNC( STATUS )

*  Only report the picture name, comment and any label when the current
*  picture has changed.
            IF ( FIRST .OR. NEWPIC ) THEN
               IF ( HASLAB ) THEN
                  CALL MSG_OUT( ' ', ' X = ^XVAL, Y = ^YVAL in '/
     :              /'^COMENT ( ^PNAME ), label = ^LABEL.', STATUS )
               ELSE
                  CALL MSG_OUT( ' ', ' X = ^XVAL, Y = ^YVAL in '/
     :              /'^COMENT ( ^PNAME )', STATUS )
               END IF

*  It is no longer a new picture.
               NEWPIC = .FALSE.

*  Picture has not changed.
            ELSE
               CALL MSG_OUT( ' ', ' X = ^XVAL, Y = ^YVAL ', STATUS )
            END IF

*  The next cursor position cannot be the first.
            FIRST = .FALSE.

            IF ( LOGPOS ) THEN

*  Write the co-ordinates in free format to a buffer using the desired
*  precision.
               NCHAR = 0
               IF ( DOUBLE ) THEN
                  CALL CHR_PUTD( DXD, LINE, NCHAR )
                  CALL CHR_PUTC( ' ', LINE, NCHAR )
                  CALL CHR_PUTD( DYD, LINE, NCHAR )
               ELSE
                  CALL CHR_PUTR( XD, LINE, NCHAR )
                  CALL CHR_PUTC( ' ', LINE, NCHAR )
                  CALL CHR_PUTR( YD, LINE, NCHAR )
               END IF

*  Append the name of the picture.
               CALL CHR_PUTC( ' ', LINE, NCHAR )
               CALL CHR_PUTC( PNAME, LINE, NCHAR )

*  Append the label, if there is one associated with the picture.
               IF ( HASLAB ) THEN
                  CALL CHR_PUTC( ' ', LINE, NCHAR )
                  CALL CHR_PUTC( LABEL, LINE, NCHAR )
               END IF

*  Write the buffer to the file.
               CALL FIO_WRITE( FD, LINE( :NCHAR ), STATUS )
            END IF

            IF ( MODE( 1:3 ) .EQ. 'DYN' ) THEN

*  Select base picture as current picture.
               CALL AGI_SELP( PICIDB, STATUS )

*  Create a new zone from the current picture.
               CALL SGS_SELZ( ZONID, STATUS )

            END IF
         ELSE

*  Make the last picture the current picture on completion (actually
*  performed by AGI_END).
            PICIDO = LASPIC
         END IF

*  Release the new error context.
         CALL ERR_RLSE
      END DO

*  Write the results to the output parameters.
      IF ( DOUBLE ) THEN
         CALL PAR_PUT0D( 'XC', DXD, STATUS )
         CALL PAR_PUT0D( 'YC', DYD, STATUS )
      ELSE
         CALL PAR_PUT0D( 'XC', DBLE( XD ), STATUS )
         CALL PAR_PUT0D( 'YC', DBLE( YD ), STATUS )
      END IF

*  Close the file storing positions if present.
      IF ( LOGPOS ) CALL FIO_ANNUL( FD, STATUS )

*  AGI closedown sequence.
*  =======================

  980 CONTINUE

*  Deactivate SGS and close the workstation.
      CALL AGS_DEACT( STATUS )

*  Close the AGI context.
      CALL AGI_END( PICIDO, STATUS )

*  Close the AGI database.  Record the name of the workstation only if
*  it was used successfully.
      IF ( DEVCAN ) THEN
         CALL AGI_CANCL( 'DEVICE', STATUS )
      ELSE
         CALL AGI_ANNUL( PICID, STATUS )
      END IF

*  Close the AGI context.
      CALL AGI_END( -1, STATUS )

*  Come here for any errors that occurred before the graphics device
*  was opened.

  999 CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PICCUR_ERR',
     :     'PICCUR: Error selecting a point or picture with a cursor.',
     :     STATUS )
      END IF

      END
