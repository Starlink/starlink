      SUBROUTINE CURSOR ( STATUS )
*+
*  Name:
*     CURSOR

*  Purpose:
*     Reports the co-ordinates of points selected using the cursor.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CURSOR( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This reads co-ordinates from the chosen graphics device and
*     displays them on your terminal.  There is commentary that
*     describes which buttons should be pressed to select or erase a
*     point, or exit.  Optionally, the co-ordinates may be stored in a
*     text file.

*     For each selected cursor position its Cartesian co-ordinates are
*     reported.  If the co-ordinate frame changes between selected
*     positions the comment, name and any label associated with the new
*     graphics-database picture are appended to the message.

*     There are three modes of operation to define which co-ordinate
*     system/picture is to be used.  These are ANCHOR, CURRENT and
*     DYNAMIC.  See the parameter MODE for details.

*     In ANCHOR or DYNAMIC modes there is an option to select only
*     pictures of a certain name in the database.  This is most useful
*     when DATA pictures are covered by transparent FRAME pictures.

*  Usage:
*     cursor [mode] [name] [logfile] [device]

*  ADAM Parameters:
*     COLOUR = LITERAL (Read)
*        The colour in which to draw any graphics specified by
*        parameter PLOT.  The options are described below.
*
*          "MAX"          - The maximum colour index used for the
*                           display of the image.
*          "MIN"          - The minimum colour index used for the
*                           display of the image.
*          An integer     - The actual colour index.  It is constrained
*                           between 0 and the maximum colour index
*                           available on the device. 
*          A named colour - Uses the named colour from the palette, and
*                           if it is not present, the nearest colour
*                           from the palette is selected.
*
*        If the colour is to remain unaltered as the lookup table is
*        manipulated choose an integer between 0 and 15, or a named
*        colour.  The suggested default is the current value.
*
*        This parameter is ignored on window overlays, where the
*        overlay colour is used.  (Use the PALENTRY command to change
*        this colour.)  An overlay has the advantage that the crosses
*        or polygon can be erased using OVCLEAR once this task is
*        completed.  The parameter is also ignored for character-cell
*        terminals.
*        [The current value, but equals "Green" if there is no current
*        value.]
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
*        The graphics workstation.  This device must support cursor
*        interaction, and belong to one of the following classes:
*        TERMINAL, IMAGE_DISPLAY, IMAGE_OVERLAY, WINDOW, and
*        WINDOW_OVERLAY.  [The current graphics device]
*     DOUBLE = _LOGICAL (Read)
*        If true co-ordinates will be reported, written to the output
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
*           selection of a new picture.  

*           "Anchor" lets the first cursor hit select a
*           picture which remains current throughout the running of
*           the application.  If subsequent cursor hits fall outside
*           the extent of this picture, a position extrapolated from
*           the picture's co-ordinate system is reported.
*        ["Dynamic"]
*     NAME = LITERAL (Read)
*        Only pictures of this name are to be selected.  A null string
*        (!) or blanks means that pictures of all names may be selected.
*        NAME is ignored when MODE = "Current".
*        [!]
*     NUMBER = _DOUBLE (Write)
*        The number of points selected with the cursor and stored in
*        output parameters XP and YP.
*     PLOT = LITERAL (Read)
*        The type of graphics to be used to mark the position of each
*        selected point.  PLOT can take any of the following values:
*
*          "Cross" - Each vertex is marked by a cross.
*
*          "None"  - No graphics are produced.
*       
*          "Poly"  - Causes each point to be joined by a straight line
*                    to the previous point.  The last point is joined
*                    to the first point.
*
*        The initial default is "None", then subsequently it is the
*        current value.  []
*     XC = _DOUBLE (Write)
*        The x co-ordinate of the last point selected with the cursor.
*        This is only written when parameter NUMBER is positive.
*     XP() = _DOUBLE (Write)
*        The x co-ordinates of the points selected with the cursor.
*        The number of values is given by parameter NUMBER, and
*        therefore this parameter is only written when parameter NUMBER
*        is positive.
*     YC = _DOUBLE (Write)
*        The y co-ordinate of the last point selected with the cursor.
*        This is only written when parameter NUMBER is positive.
*     YP() = _DOUBLE (Write)
*        The y co-ordinates of the points selected with the cursor.
*        The number of values is given by parameter NUMBER, and
*        therefore this parameter is only written when parameter NUMBER
*        is positive.

*  Examples:
*     cursor
*        This obtains the co-ordinates of any visible picture for the
*        current graphics device by use of the cursor.
*     cursor colour=blue plot=cross
*        As above except that the points are marked with blue crosses.
*     cursor xc=(xpos) yc=(ypos)
*        As the first example.  The x and y co-ordinates of the
*        last-selected point is written to ICL variables xpos and ypos.
*     cursor cosys=w
*        This obtains the world co-ordinates of any visible picture for
*        the current graphics device by use of the cursor.
*     cursor current device=graphon
*        This obtains the co-ordinates of any visible picture in the
*        reference frame of the current picture of the Graphon device.
*     cursor logfile=stars.dat name=data
*        This obtains the co-ordinates of any visible DATA picture
*        for the current graphics device.  The x-y co-ordinates are
*        stored in the text file called stars.dat.

*  Notes:
*     -  Should an error occur trying to obtain the BASE picture for
*     ANCHOR or DYNAMIC modes, the current picture is unchanged.
*     -  In DYNAMIC and ANCHOR modes, if the cursor is situated at a
*     point where there are no pictures of the selected name, the
*     co-ordinates in the BASE picture are reported.
*     -  The maximum number of points is 500.
*     -  Points can be removed (the instructions state how), starting
*     from the most-recent one.  For an overlay device, the plotted
*     points or polygon lines disappear.  For other devices, the
*     erroneous points and lines are plotted with pen 4 of the palette.

*  Algorithm:
*     -  Find the mode of operation, the name of valid pictures and
*     whether or not world co-ordinates are to be reported.
*     -  Associate the graphics device and activate SGS.
*     -  Get and select the BASE picture for the current workstation,
*     and create an SGS zone for the BASE.  Make this the current
*     picture and zone if mode is dynamic or anchor.  For cursor mode
*     create an SGS zone for the input current picture.
*     -  Set the graphics style.  Set the initial cursor position as
*     the centre of the current picture.
*     -  Check there is a cursor and prepare it.
*     -  Loop until status is bad on the escape button has been pressed.
*        o  Synchronise the cursor with the output.  Read the cursor
*        position (in base-zone co-ordinates).  If it is an acceptable
*        choice then
*           -  Plot the point or join the polygon segment if plotting
*           enabled.
*           -  If mode is dynamic or it is the first hit with anchor
*           mode then
*              o  Get the last picture of the chosen name which
*              encompasses that cursor position.  If there is no picture
*              of that name at the cursor position then use the base
*              zone instead.  Store the id of the last picture.  Convert
*              the cursor position in base-zone co-ordinates to to those
*              in the selected picture.  In dynamic mode select the new
*              zone containing the cursor and release the old zone.  In
*              anchor set the cursor to its position in the anchor
*              zone's co-ordinates.
*           -  For other modes move the cursor to the new position if
*           it is outside the current zone.  Convert the co-ordinates
*           from the base to those of the current picture.
*           -  Synchronise the output with the cursor.  Report the
*           cursor position, and only report details of the current
*           picture when the picture has changed.  Write the
*           co-ordinates to the log file if required.  In dynamic mode
*           reselect the BASE picture and create a new zone from it.
*           Annul the redundant picture identifiers.
*        o  Make the last picture the current picture on completion.
*     -  Close the polygon, if one has been plotted.
*     -  Put the last x-y co-ordinates into the output parameters.
*     -  Close an open log file
*     -  Deactivate SGS and close AGI workstation.

*  Related Applications:
*     KAPPA: CURSOR, INSPECT, PICCUR; Figaro: ICUR, IGCUR.

*  Authors:
*     JM: Jo Murray  (STARLINK)
*     MJC: Malcolm J. Currie  (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-MAY-1989 (JM):
*        Original version.
*     1989 Jun 28 (MJC):
*        Added error checking, some tidying, fixed several bugs, only
*        reports the picture name and comment when it changes.
*     1989 Jul 10 (MJC):
*        Fixed "SGS zone too small" bug by reordering the code so that
*        the MODE is obtained before the device; extended the prologue.
*     1989 Oct 19 (MJC):
*        Fixed the synchronisation for terminals, and added commentary
*        for both terminals and image displays.
*     1989 Oct 24 (MJC):
*        Added the log file and name options, and redesigned the
*        description.
*     1989 Nov 10 (MJC):
*        Calls a subroutine to prepare the cursor.
*     1990 Jan 9 (MJC):
*        Corrected SGS status.
*     1990 Apr 20:
*        Added output of the current picture's label, if it exists.
*     1991 February 8 (MJC):
*        Added AGI context control, and tidied unwanted picture
*        identifiers.
*     1991 March 19 (MJC):
*        Converted to SST prologue.
*     1991 April 9 (MJC):
*        Added AGI begin-and-end block.  Obtains data co-ordinates if
*        present.
*     1991 May 14 (MJC):
*        Added COSYS parameter.
*     1992 January 29 (MJC):
*        Fixed bug in list of options for MODE.
*     1992 February 19 (MJC):
*        AGI behaviour has changed, so call new AGI routine to test
*        whether or not the picture has changed between cursor
*        selections.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1993 May 24 (MJC):
*        Added XC and YC output parameters.
*     1995 August 24 (MJC):
*        Added the COLOUR and PLOT parameters and example of their use.
*        No longer selects a new current picture (use PICCUR instead).
*        Made usage and examples lowercase.  Added Related Applications.
*     1995 December 16 (MJC):
*        Added NUMBER, XP, and YP output parameters.  Allowed erasure
*        of previous point.
*     1996 May 31 (MJC):
*        Improved and simplified the graphics, so the marker has a
*        uniform size, and plotting can occur outside the current
*        SGS picture in ANCHOR and CURRENT modes.
*     1997 March 11 (MJC):
*        Fixed initialisation bug for parameter NUMBER, and hence XP and
*        YP.  Fixed bug where SGS pen was being set when PLOT="None".
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'GKS_PAR'          ! GKS constants (e.g. GSET)
      INCLUDE 'CTM_PAR'          ! KAPPA CTM_ constants
      INCLUDE 'GNS_PAR'          ! GNS constants 
      INCLUDE 'PAR_ERR'          ! Parameter-system errors
      INCLUDE 'AGI_ERR'          ! AGI error constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER EPEN               ! SGS pen number used to erase graphics
      PARAMETER ( EPEN = 4 )

      REAL MARKSZ                ! Fractional size of the markers
      PARAMETER ( MARKSZ = 0.015 )

      REAL MAXPTS                ! Maximum number of points
      PARAMETER ( MAXPTS = 500 )

      INTEGER MPEN               ! SGS pen number used to draw graphics
      PARAMETER ( MPEN = 3 )

      INTEGER NCHLIN             ! Maximum number of characters in a
                                 ! an output record
      PARAMETER ( NCHLIN = 80 )

      INTEGER SZNAM              ! Length of picture name
      PARAMETER ( SZNAM = 15 )

*  Local Variables:
      INTEGER BZONID             ! SGS zone identifier of the base
                                 ! picture
      INTEGER CI                 ! Colour index required for graphics
      INTEGER COLI               ! Original colour index of current pen
      CHARACTER * ( 256 ) COMENT ! Comment associated with the latest
                                 ! picture
      LOGICAL COLOUR             ! Workstation supports colour?
      CHARACTER * ( 6 ) COSYS    ! Co-ordinate system
      REAL CROSHT                ! Height of crosses
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
      INTEGER IERR               ! GKS error indicator
      LOGICAL IMGDIS             ! Device is nominally an image display?
      CHARACTER * ( 80 ) IMGMES( 5 ) ! Informational messages if device is
                                 ! an image display
      INTEGER IWKID              ! GKS workstation identifier
      CHARACTER * ( SZNAM ) LABEL ! Picture label
      INTEGER LASPIC             ! Last picture's identifier
      CHARACTER *( NCHLIN ) LINE ! Line buffer which takes the image
                                 ! data and transfers it to the file
      INTEGER LNTYPE             ! Line type for current SGS pen
      LOGICAL LOGPOS             ! A log of the positions is written to
                                 ! a text file?
      REAL LWIDTH                ! The width of the current SGS pen
      LOGICAL MARK               ! Crosses required at cursor
                                 ! positions?
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
      INTEGER NP                 ! The number of points marked
      INTEGER NUPIC              ! The latest picture's identifier
      LOGICAL OVRLAY             ! The device is an overlay?
      INTEGER PEN                ! Current SGS pen
      INTEGER PICID              ! Current (input) picture identifier
      INTEGER PICIDB             ! BASE picture identifier
      INTEGER PICIDO             ! Picture identifier on exit
      CHARACTER * ( 5 ) PLOT     ! Nature of required graphics
      CHARACTER * ( DAT__SZNAM ) PNAME ! Name associated with the latest
                                 ! picture
      LOGICAL POLY               ! Lines required to join cursor
                                 ! positions?
      LOGICAL SETPLR             ! Polyline representation to be reset?
      CHARACTER * ( 80 ) TERMES( 5 ) ! Informational messages if
                                 ! device is a terminal
      CHARACTER * ( GNS__SZKEY ) WKCLAS ! Workstation class
      REAL X1, Y1                ! Lower-left corner of the initial
                                 ! picture
      REAL X2, Y2                ! Upper-right corner of the initial
                                 ! picture
      DOUBLE PRECISION XA( MAXPTS ) ! Array of all x data co-ordinates
      REAL XCUR( MAXPTS )        ! X cursor co-ordinates
      REAL XD                    ! X data co-ordinate to be reported
      REAL XIN                   ! X co-ordinate as measured by the
                                 ! cursor
      REAL XM, YM                ! Size of the initial picture
      REAL XOCP                  ! Original cursor position in x
      REAL XOUT                  ! X world co-ordinate
      DOUBLE PRECISION YA( MAXPTS ) ! Array of all y data co-ordinates
      REAL YCUR( MAXPTS )        ! Y cursor co-ordinates
      REAL YD                    ! Y data co-ordinate to be reported
      REAL YIN                   ! Y co-ordinate as measured by the
                                 ! cursor
      REAL YOCP                  ! Original cursor position in y
      REAL YOUT                  ! y world co-ordinate
      LOGICAL WORLD              ! Only world co-ordinates are to be
                                 ! reported?
      INTEGER ZONID              ! SGS zone identifier of the initial
                                 ! picture

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      DEVCAN = .FALSE.
      NP = 0

*  Get the mode parameter.
      CALL PAR_CHOIC( 'MODE', 'Dynamic', 'Dynamic,Current,Anchor',
     :                .TRUE., MODE, STATUS )

*  Get the reporting precision.
      CALL PAR_GTD0L( 'DOUBLE', .FALSE., .TRUE., DOUBLE, STATUS )

*  Get the type of co-ordinates to report.
      CALL PAR_CHOIC( 'COSYS', 'Data', 'Data,World', .FALSE., COSYS,
     :                STATUS )
      WORLD = COSYS .EQ. 'WORLD'

*  See what type of graphics are required.
      CALL PAR_CHOIC( 'PLOT', 'None', 'Poly,Cross,None', .TRUE., 
     :                PLOT, STATUS )

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
            CALL ERR_REP( 'CURSOR__NID',
     :        'Graphics device not available or not recognised.',
     :        STATUS )
         END IF
         DEVCAN = .TRUE.
         GOTO 980
      END IF

*  Put out a blank line to ensure the commentary appears on the alpha
*  plane of the terminal.
      CALL MSG_BLANK( STATUS )

*  Access the current and base pictures.
*  =====================================

      IF ( MODE( 1:3 ) .EQ. 'CUR' ) THEN

*  Create a new SGS_ZONE from current picture.
         CALL AGS_NZONE( ZONID, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'CURSOR__NCURP',
     :        'Unable to get the current zone from the database.',
     :        STATUS )
            DEVCAN = .TRUE.
            GOTO 980
         END IF
      END IF

*  Inquire the BASE picture for the current workstation.
      CALL AGI_IBASE( PICIDB, STATUS )

*  Select this as the current picture.
      CALL AGI_SELP( PICIDB, STATUS )

*  Create a new SGS zone from the current picture.
      CALL AGS_NZONE( BZONID, STATUS )

      IF ( MODE( 1:3 ) .EQ. 'DYN' .OR. MODE( 1:3 ) .EQ. 'ANC' ) THEN

*  The base zone is treated as the input picture for these two modes.
         ZONID = BZONID

*  Reset current picture to the input picture.  (Actually happens when
*  AGI_END is called.)
         PICIDO = PICID

*  Reset the current picture to the input current picture for the
*  CURRENT mode.
      ELSE
         CALL AGI_SELP( PICID, STATUS )
      END IF

       IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( 'CURSOR__NOBAS',
     :      'Unable to get the BASE picture/zone from the database.',
     :      STATUS )

         DEVCAN = .TRUE.
         GOTO 980
      END IF

*  Define the graphics style.
*  ==========================
      SETPLR = .FALSE.
      OVRLAY = .FALSE.

*  If graphics are required...
      IF ( PLOT .NE. 'NONE' ) THEN

*  Find the class of the workstation.
         CALL KPG1_IWCG( 'CLASS', WKCLAS, STATUS )

*  Check for an unsupported device.
         IF ( WKCLAS .NE. 'IMAGE_DISPLAY' .AND.
     :        WKCLAS .NE. 'IMAGE_OVERLAY' .AND.
     :        WKCLAS .NE. 'TERMINAL' .AND. WKCLAS .NE. 'WINDOW' .AND.
     :        WKCLAS .NE. 'WINDOW_OVERLAY' ) THEN

            STATUS = SAI__ERROR
            CALL MSG_SETC( 'CLASS', WKCLAS )
            CALL ERR_REP( 'CURSOR_INVDEV',
     :        'The chosen device is of the wrong class (^CLASS).  '/
     :        /'It should be a TERMINAL, WINDOW, WINDOW_OVERLAY, '/
     :        /'IMAGE_DISPLAY, or IMAGE_OVERLAY.', STATUS )
            GOTO 980

         ELSE

*  Define whether or not the device is an overlay.
            OVRLAY = INDEX( WKCLAS, 'OVERLAY' ) .NE. 0

*  Want solid lines for the plotting.
            CALL KPG1_SOLIN( STATUS )

*  Inquire the current SGS pen, and then select the pen used to draw
*  markers by the KAPPA routine which gets cursor positions.
            CALL SGS_IPEN( PEN )
            CALL SGS_SPEN( MPEN )

            IF ( WKCLAS .NE. 'TERMINAL' ) THEN

*  Obtain the colour index for the graphics.
               CALL KPG1_IVCI( 'DEVICE', 'COLOUR', .FALSE., CI, STATUS )

*  Inquire the workstation identifier for GKS inquiries.
               CALL SGS_ICURW( IWKID )

*  Inquire the current colour index of this pen (it will be restored
*  after all plotting is complete).
               CALL GQPLR( IWKID, MPEN, GSET, IERR, LNTYPE, LWIDTH,
     :                     COLI )

*  Store the new colour index for this pen.
               CALL GSPLR( IWKID, MPEN, LNTYPE, LWIDTH, CI )
               SETPLR = .TRUE.

*  Assign the background colour to the erasure pen for overlays.
               IF ( OVRLAY ) THEN

*  Store the new colour index for this pen, using the same style as
*  the marking pen.
                  CALL GSPLR( IWKID, EPEN, LNTYPE, LWIDTH, 0 )
               END IF

*  See if a GKS error has occurred.
               CALL GKS_GSTAT( STATUS )
            END IF
         END IF
      END IF

*  Find the limits of the base zone.
      CALL SGS_SELZ( BZONID, STATUS )
      CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )

*  Calculate the initial marker height.  At the start this is the
*  current cross height (this might change in ANCHOR mode).
      CROSHT = MARKSZ * MIN( X2 - X1, Y2 - Y1 )

*  Set flags for the type of graphics required.
      MARK = PLOT .EQ. 'CROSS'
      POLY = PLOT .EQ. 'POLY'

*  Set up cursor interactions.
*  ===========================
*  
*  Create informational messages for the interaction.
      IMGMES( 1 ) = 'Use the graphics cursor to define the next '/
     :              /'point...'
      IMGMES( 2 ) = '   Press left button on mouse/trackerball to '/
     :              /'select a point.'
      IMGMES( 3 ) = '   Press middle button on mouse/trackerball to '/
     :              /'remove the last point.'
      IMGMES( 4 ) = '   Press right button on mouse/trackerball to '/
     :              /'end input.'
      IMGMES( 5 ) = ' '
      NIMGMS = 5

      TERMES( 1 ) = 'Use the graphics cursor to select the next '/
     :              /'point...'
      TERMES( 2 ) = '   Press keyboard "1" or space key to select a '/
     :               /'point.'
      TERMES( 3 ) = '   Press keyboard "2" to remove the last point.'
      TERMES( 4 ) = '   Press keyboard "." to end input.'
      TERMES( 5 ) = ' '
      NTERMS = 5

*  Prepare the cursor.
      CALL KPG1_PRCUR( 2, TERMES, NTERMS, IMGMES, NIMGMS, '12 .',
     :                 CURCHO, IMGDIS, STATUS )
      IF ( .NOT. CURCHO .OR. STATUS .NE. SAI__OK ) GOTO 980

*  Get initial cursor position as the centre of the current picture.
*  It will use the limits previously obtained unless it is in the
*  CURRENT mode.
      IF ( MODE .EQ. 'CUR' ) THEN
         CALL SGS_SELZ( ZONID, STATUS )
         CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )
      END IF

      XIN = 0.5 * ( X1 + X2 )
      YIN = 0.5 * ( Y1 + Y2 )
      CALL SGS_SETCU( XIN, YIN )

*  Return to the base zone for the interactions.
      IF ( MODE .EQ. 'CUR' ) THEN
         CALL SGS_SELZ( BZONID, STATUS )

*  In current mode the picture identifier will always be the input
*  identifier.
         NUPIC = PICID
      END IF

*  Store the original position of the cursor.  It may be needed in
*  anchor mode, if the first point is erased.
      XOCP = XIN
      YOCP = YIN

*  Main interaction loop.
*  ======================

*  Note that we shall use the base zone for the cursor and graphics.
*  This enables graphics to be plotted outside the current picture for
*  the ANCHOR and CURRENT modes, and a uniform marker size.  The
*  returned cursor positions are converted into co-ordinates in the
*  input picture (CURRENT mode), the first-selected picture (ANCHOR
*  mode), or the currently selected picture (DYNAMIC mode).

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

*  Set the cursor to the previous (or initial) position.
         CALL SGS_SETCU( XIN, YIN )

*  If a message has already been displayed, and then the cursor is
*  used, the next message is no longer in synchronisation with the
*  cursor.  So synchronise the message system.
         CALL MSG_SYNC( STATUS )

*  Read the cursor position and button value.
         CALL SGS_REQCU( XIN, YIN, HITVAL )

*  Convert the `break' on a mouse (the middle button) for image
*  displays to the GKS 7.2 behaviour, so that the interaction can be
*  fully under mouse control.
         IF ( HITVAL .EQ. 0 ) HITVAL = 2

*  Interaction: accept a position.
*  -------------------------------
  
*  A point is selected if choices 1 or 3 (1 or space or keyboard) is
*  selected.
         IF ( HITVAL .EQ. 1 .OR. HITVAL .EQ. 3 ) THEN

*  Increment the count of the points and record the point.
            NP = NP + 1
            XCUR( NP ) = XIN
            YCUR( NP ) = YIN

*  Plot the point or polygon border.
*  ---------------------------------

*  Polygon plotting...
            IF ( POLY ) THEN
               IF ( NP .GT. 1 ) THEN

*  Join two succesive points with a straight line if requested.
                  CALL SGS_LINE( XCUR( NP - 1 ), YCUR( NP - 1 ),
     :                           XCUR( NP ), YCUR( NP ) )
                  CALL SGS_FLUSH

*  Record the previous point in case the
               END IF
            END IF

*  Mark with a cross if requested.
            IF ( MARK ) THEN
               CALL KPG1_CROSS( XCUR( NP ), YCUR( NP ), CROSHT, STATUS )

*  Want to plot the cross immediately.
               CALL SGS_FLUSH
            END IF

*  Determine which picture to use.
*  -------------------------------

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

*  Use the base zone in these cases.  Thus the cursor co-ordinates can
*  be used directly.
                  NUPIC = PICIDB
                  CALL AGI_SELP( NUPIC, STATUS )
                  XOUT = XIN
                  YOUT = YIN

*  Set a flag to indicate what has happened.
                  NONAME = .TRUE.
               ELSE

*  There is a picture of the chosen name at the position.
                  NONAME = .FALSE.
               END IF

*  Decide if the cursor is situated in a different picture from last
*  time.
               CALL AGI_ISAMP( LASPIC, NEWPIC, STATUS )
               NEWPIC = .NOT. NEWPIC

*  The last picture is no long needed, so annul it unless it is the
*  input picture or the BASE picture.
               IF ( LASPIC .NE. PICID .AND.
     :            ( LASPIC .NE. PICIDB .AND. MODE( 1:3 ) .NE. 'CUR' ) )
     :           CALL AGI_ANNUL( LASPIC, STATUS )

*  Record the current picture as the new last picture.
               LASPIC = NUPIC

*  Transform co-ordinates into those of the new current picture.
*  -------------------------------------------------------------
               IF ( .NOT. NONAME ) THEN

*  Create an SGS zone associated with the chosen picture.
                  CALL AGS_NZONE( NEWZON, STATUS )

*  Convert cursor position in base-zone co-ordinates to those of the
*  selected picture.
                  CALL SGS_TPZ( BZONID, XIN, YIN, NEWZON, XOUT, YOUT,
     :                          STATUS )

               ELSE

*  By definition, when there is no pictures of the desired name, the
*  'new' zone is the base zone.  This is needed for the co-ordinate
*  transformation, albeit the identity matrix.
                  NEWZON = BZONID
               END IF

*  Manage the zones.
*  -----------------

*  Tidy up to avoid exhaustion of available SGS zones.
               IF ( MODE( 1:3 ) .EQ. 'DYN' ) THEN
                  IF ( .NOT. NONAME ) THEN
                     CALL SGS_SELZ( ZONID, STATUS )
                     CALL SGS_RELZ( NEWZON )
                  END IF

*  Store the new zone selected in ANCHOR mode as if it were the original
*  zone, thus the ANCHOR mode will now behave as if it were in the
*  CURRENT mode.
               ELSE
                  ZONID = NEWZON

*  Reinstate the base zone.
                  CALL SGS_SELZ( BZONID, STATUS )

               END IF

*  Non-dynamic modes.
*  ------------------
            ELSE

*  Ensure the the current picture is the input picture.
               CALL AGI_SELP( NUPIC, STATUS )

*  Convert the base-zone co-ordinates to those of the current or
*  anchored picture.  These will be use for reporting below.
               CALL SGS_TPZ( BZONID, XIN, YIN, ZONID, XOUT, YOUT,
     :                       STATUS )

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

*  Record the values for the output parameters.
            IF ( DOUBLE ) THEN
               XA( NP ) = DXD
               YA( NP ) = DYD
            ELSE
               XA( NP ) = DBLE( XD )
               YA( NP ) = DBLE( YD )
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

*  Write the buffer to the file.
               CALL FIO_WRITE( FD, LINE( :NCHAR ), STATUS )
            END IF

            IF ( MODE( 1:3 ) .EQ. 'DYN' ) THEN

*  Select BASE picture as current picture.
               CALL AGI_SELP( PICIDB, STATUS )

*  Create a new zone from the current picture.
               CALL SGS_SELZ( ZONID, STATUS )

            END IF

*  Interaction: erase the last point.
*  ----------------------------------
         ELSE IF ( HITVAL .EQ. 2 ) THEN

*  Only proceed if there is a point to remove.
            IF ( NP .GT. 0 ) THEN

*  Can erase the graphics too.  If the device is an overlay
*  can draw the line or cross with the background colour.
*  For non-overlays, plot in a different colour.
               IF ( PLOT .NE. 'NONE' ) CALL SGS_SPEN( EPEN )
               IF ( MARK ) THEN

*  Overwrite the cross in the new colour immediately.
                  CALL KPG1_CROSS( XCUR( NP ), YCUR( NP ), CROSHT,
     :                             STATUS )

               ELSE IF ( POLY .AND. NP .GT. 1 ) THEN

*  Overwrite the cross in the new colour immediately.
                  CALL SGS_LINE( XCUR( NP - 1 ), YCUR( NP - 1 ),
     :                           XCUR( NP ), YCUR( NP ) )
               END IF
               CALL SGS_FLUSH

*  Decrement the count of points.
               NP = NP - 1

*  Reset the FIRST flag if the first point was removed.  This affects
*  the mode.
               FIRST = NP .EQ. 0

*  Switch back to the original picture identifier, if the first point
*  was erased.
               IF ( FIRST ) THEN
                  LASPIC = PICID

*  In the anchor mode, the first point was erased, so lift the anchor
*  and release the new zone, and switch back to the base zone...
                  IF ( MODE( 1:3 ) .EQ. 'ANC' ) THEN

*  Select BASE picture as current picture.
                     CALL AGI_SELP( PICIDB, STATUS )

*  Select the BASE zone as the current zone
                     CALL SGS_SELZ( BZONID, STATUS )

*  Is the current picture the BASE picture?
                     CALL AGI_ISAMP( NUPIC, NEWPIC, STATUS )
                     IF ( .NOT. NEWPIC ) THEN

*  The anchor picture is no long needed, so annul it unless it is the
*  the BASE picture.  Release its associated zone.
                        CALL AGI_ANNUL( NUPIC, STATUS )
                        CALL SGS_RELZ( NEWZON )
                     END IF

*  Restore the original position of the cursor.
                     XIN = XOCP
                     YIN = YOCP
                  END IF
               END IF
                   
*  Reset the pen to mark a new point.
               IF ( PLOT .NE. 'NONE' ) CALL SGS_SPEN( MPEN )
            END IF

*  Exit was requested with the cursor.
         ELSE

*  Make the input picture the current picture on completion (actually
*  performed by AGI_END).
            PICIDO = PICID

*  Complete the polygon, if there is one.
            IF ( POLY .AND. NP .GT. 2 ) THEN
               CALL SGS_LINE( XCUR( NP ), YCUR( NP ), XCUR( 1 ),
     :                        YCUR( 1 ) )
               CALL SGS_FLUSH
            END IF
         END IF

*  Release the new error context.
         CALL ERR_RLSE
      END DO

*  Write the results to the output parameters.
      CALL PAR_PUT0I( 'NUMBER', NP, STATUS )

      IF ( NP .GT. 0 ) THEN
         CALL PAR_PUT1D( 'XP', NP, XA, STATUS )
         CALL PAR_PUT1D( 'YP', NP, YA, STATUS )

         IF ( DOUBLE ) THEN
            CALL PAR_PUT0D( 'XC', DXD, STATUS )
            CALL PAR_PUT0D( 'YC', DYD, STATUS )
         ELSE
            CALL PAR_PUT0D( 'XC', DBLE( XD ), STATUS )
            CALL PAR_PUT0D( 'YC', DBLE( YD ), STATUS )
         END IF
      END IF

*  Close the file storing positions if present.
      IF ( LOGPOS ) CALL FIO_ANNUL( FD, STATUS )

*  AGI closedown sequence.
*  =======================

  980 CONTINUE

*  If necessary, re-instate the original colour index for the marker
*  pen, and reinstate the original pen.
      IF ( PLOT .NE. 'NONE' ) THEN
         IF ( SETPLR ) CALL GSPLR( IWKID, MPEN, LNTYPE, LWIDTH, COLI )
         CALL SGS_SPEN( PEN )
      END IF

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
         CALL ERR_REP( 'CURSOR_ERR',
     :     'CURSOR: Error selecting a point or picture with a cursor.',
     :     STATUS )
      END IF

      END
