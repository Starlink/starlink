      SUBROUTINE PICDEF( STATUS )
*+
*  Name:
*     PICDEF

*  Purpose:
*     Defines a new graphics-database FRAME picture or an array of
*     FRAME pictures.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PICDEF( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates either one new graphics-database FRAME
*     picture or a grid of new FRAME pictures.  It offers a variety of
*     ways by which you can define a new picture's location and extent.
*     You may constrain a new picture to lie within either the current
*     or the BASE picture, and the new picture adopts the world
*     co-ordinate system of that reference picture.

*     You may specify a single new picture using one of three methods:
*       1.  moving a cursor to define the lower and upper bounds via
*           pressing choice number 1 (the application will instruct what
*           to do for the specific graphics device), provided a cursor
*           is available on the chosen graphics workstation;
*       2.  obtaining the bounds from the environment (in world
*           co-ordinates of the reference picture);
*       3.  or by giving a position code for the new picture, and
*           specifying its linear fractional size along each axis in
*           terms of the reference picture, and/or its aspect ratio.
*           The position code comprises two characters.  The first
*           controls the vertical location, and may be "T", "B", or "C"
*           to create the new picture at the top, bottom, or in the
*           centre respectively.  The second defines the horizontal
*           situation, and may be "L", "R", or "C" to define a new
*           picture to the left, right, or in the centre respectively.
*           Thus a code of "BR" will make a new picture in the
*           bottom-right corner.

*     The picture created becomes the current picture on exit.

*     Alternatively, you can create an array of n-by-m equal-sized
*     pictures by giving the number of pictures in the horizontal and
*     vertical directions.  These may or may not be abutted.  For easy
*     reference in later processing the pictures may be labelled
*     automatically.  The label consists of a prefix you define,
*     followed by the number of the picture.  The numbering starts at a
*     defined value, usually one, and increments by one for each new
*     picture starting from the bottom-left corner and moving from left
*     to right to the end of the line.  This is repeated in each line
*     until the top-right picture.  Thus if the prefix were "GALAXY",
*     the start number is one and the array comprises three pictures
*     horizontally and two vertically, the top-left picture would have
*     the label "GALAXY4".  On completion the bottom-left picture in
*     the array becomes the current picture.

*  Usage:
*     picdef [mode] [fraction]
*        { xpic ypic prefix=?
*        { lbound ubound
*       mode

*  ADAM Parameters:
*     ASPECT = _REAL (Read)
*        The aspect ratio (x/y) of the picture to be created in modes
*        other than Cursor, Array, and XY.  The new picture is the
*        largest possible with the chosen aspect ratio that will fit
*        within the part of the reference picture defined by the
*        fractional sizes (see parameter FRACTION).  The justification
*        comes from the value of MODE.  Thus to obtain the largest
*        picture parameter set FRACTION=1.0.  A null value (!) does not
*        apply an aspect-ratio constraint, and therefore the new
*        picture fills the part of the reference picture defined by the
*        fractional sizes. [!]
*     CURRENT = _LOGICAL (Read)
*        TRUE if the new picture is to lie within the current picture,
*        otherwise the new picture can lie anywhere within the BASE
*        picture.  In other words, when it is TRUE the current picture
*        is the reference picture, and when FALSE the base is the
*        reference picture. [FALSE]
*     DEVICE = DEVICE (Read)
*        The graphics device. [Current graphics device]
*     FILL = _REAL (Read)
*        The linear filling factor for the Array mode.  In other words
*        the fractional size (applied to both co-ordinates) of the new
*        picture within each of the XPIC * YPIC abutted sections of
*        the picture being sub-divided.  Each new picture is located
*        centrally within the section.  A filling factor of 1.0 means
*        that the pictures in the array abut.  Smaller factors permit a
*        gap between the pictures.  For example, FILL = 0.9 would give
*        a gap between the created pictures of 10 per cent of the
*        height and width of each picture, with exterior borders of 5
*        per cent.  FILL must lie between 0.1 and 1.0. [1.0]
*     FRACTION( ) = _REAL (Read)
*        The fractional size of the new picture along each axis,
*        applicable for modes other than Array, Cursor, and XY.  Thus
*        FRACTION controls the relative shape as well as the size of
*        the new picture.  If only a single value is given then it is
*        applied to both x and y axes, whereupon the new picture has
*        the shape of the reference picture.  So a value of 0.5 would
*        create a picture 0.25 the area of the current or BASE picture.
*        The default is 0.5, unless parameter ASPECT is not null, when
*        the default is 1.0. []
*     LABELNO = _INTEGER (Read)
*        The number used to form the label for the first (bottom-left)
*        picture in Array mode.  It cannot be negative. [1]
*     LBOUND( 2 ) = _REAL (Read)
*        Co-ordinates of the lower bound that defines the new picture.
*        The suggested default is the bottom-left of the current
*        picture.  (XY mode)
*     MODE = LITERAL (Read)
*        Method for selecting the new picture. The options are "Cursor"
*        for cursor mode (provided the graphics device has one), "XY"
*        to select x-y limits, and "Array" to create a grid of
*        equal-sized FRAME pictures.  The remainder are locations specified
*        by two characters, the first corresponding to the vertical
*        position and the second the horizontal.  For the vertical,
*        valid positions are T(op), B(ottom), or C(entre); and for the
*        horizontal the options are L(eft), R(ight), or C(entre). (It
*        is the same as the disposition code in SGS). ["Cursor"]
*     OUTLINE = _LOGICAL (Read)
*        If TRUE, a box that delimits the new picture is drawn. [TRUE]
*     PREFIX = LITERAL (Read)
*        The prefix to be given to the labels of picture created in
*        Array mode.  It should contain no more than twelve characters.
*        If the empty string "" is given, the pictures will have
*        enumerated labels.  Note that the database can contain only
*        one picture with a given label, and so merely numbering labels
*        increases the chance of losing existing labels.  A ! response
*        means no labelling is required.  The suggested default is the
*        last-used prefix.
*     UBOUND( 2 ) = _REAL (Read)
*        Co-ordinates of the upper bound that defines the new picture.
*        The suggested default is the top-right of the current picture.
*        (XY mode)
*     XPIC = _INTEGER (Read)
*        The number of new pictures to be formed horizontally in the
*        BASE or current picture in Array mode.  The total number of
*        new pictures is XPIC * YPIC.    The value must lie in the
*        range 1--20.  The suggested default is 2.
*     YPIC = _INTEGER (Read)
*        The number of new pictures to be formed vertically in the BASE
*        or current picture in Array mode.  The value must lie in the
*        range 1--20.  The suggested default is the value of parameter
*        XPIC.

*  Examples:
*     picdef tr
*        Creates a new FRAME picture in the top-right quarter of the
*        full display area on the current graphics device, and an
*        outline is drawn around the new picture.  This picture becomes
*        the current picture.
*     picdef bl aspect=1.0
*        Creates a new FRAME picture within the full display area on
*        the current graphics device, and an outline is drawn around
*        the new picture.  This picture is the largest square possible,
*        and it is justified to the bottom-left corner.  It becomes the
*        current picture.
*     picdef cc 0.7 current nooutline
*        Creates a new FRAME picture situated in the centre of the
*        current picture on the current graphics device.  The new
*        picture has the same shape as the current one, but it is
*        linearly reduced by a factor of 0.7.  No outline is drawn
*        around it.  The new picture becomes the current picture.
*     picdef cc [0.8,0.5] current nooutline
*        As above except that its height is half that of the current
*        picture, and its width is 0.8 of the current picture.
*     picdef cu device=graphon
*        Creates a new FRAME picture within the full display area of
*        the Graphon device.  The bounds of the new picture are defined
*        by cursor interaction.  An outline is drawn around the new
*        picture which becomes the current picture.
*     picdef mode=a prefix=M xpic=3 ypic=2
*        Creates six new equally sized and abutting FRAME pictures
*        within the full display area of the current graphics device.
*        All are outlined.  They have labels M1, M2, M3, M4, M5, and
*        M6.  The bottom-left picture (M1) becomes the current picture.
*     picdef mode=a prefix="" xpic=3 ypic=2 fill=0.8
*        As above except that the pictures do not abut since each is
*        0.8 times smaller in both dimensions, and the labels are 1,
*        2, 3, 4, 5, and 6.

*  Algorithm:
*     -  Open graphics device and start database activity.
*     -  Determine the mode required (including cursor), and whether or
*     not the current picture is to enclose the new picture. If not
*     get the BASE picture and zone, otherwise get the zone associated
*     with the current picture.
*     -  If mode is cursor prepare the cursor.  If a cursor and one
*     choice could not be obtained report what has happened and
*     determine a new mode (excluding cursor).
*     -  Determine whether or not an outline is required.
*     -  If the mode is cursor get the bounds of the current picture and
*     find its centre.  Then get two x-y points defining the new
*     picture via a cursor starting from the centre and checking the
*     points do not lie outside the current picture.
*     -  If the mode is xy get two x-y points defining the new picture
*     from the environment
*     -  If the mode is array then
*        o  Get the number of pictures horizontally and vertically.
*        o  Get the prefix, and if null switch off labelling
*        o  Find maximum label start number depending on prefix length
*        and array size.  Get the label start number.
*        o  Inquire the bounds of the existing picture.  For each
*        picture in the array:
*           -  Select the original picture identifier and zone. Compute
*           its bounds, create the new zone. Draw a box around the zone
*           if requested. Save the new zone in the database.  If
*           labelling required form the label from the prefix and
*           current label number, and then store in the database.
*        o  If an error occurred, report that the current picture has
*        not been switched.
*        o  Make the bottom-left picture in the array the current
*        picture.
*     -  If the mode is a position code get linear fractions of the
*     current zone, and the picture aspect ratio.  Obtain x-y limits
*     given the justification, fractions, and aspect ratio.
*     -  For non-array modes create the new zone, draw a box around it
*     if requested, and save the new zone in the database.
*     -  If an error occurred, report that the current picture has not
*     been switched.
*     -  Deactivate SGS and cancel AGI device

*  Related Applications:
*     KAPPA: PICBASE, PICCUR, PICDATA, PICFRAME, PICGRID, PICLABEL,
*            PICLIST, PICSEL, PICXY.

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1989 April 30 (MJC):
*        Original.
*     1989 June 26 (MJC):
*        Added positional code and fraction.
*     1989 July 11 (MJC):
*        Reordered code so as to get a zone once the reference picture
*        is known to avoid "SGS zone too small" problem, and cursor
*        only checked if cursor mode is selected.
*     1989 November 10 (MJC):
*        Added box option, and commentary for both terminals and image
*        displays.
*     1990 January 9 (MJC):
*        Corrected SGS status.
*     1990 January 14 (MJC):
*        Added array option.
*     1990 April 30 (MJC):
*        Improved the cursor mode.
*     1990 August 6 (MJC):
*        Used parameters for upper and lower bounds rather than x-y
*        limits.
*     1991 March 19 (MJC):
*        Converted to SST prologue.
*     1991 April 9 (MJC):
*        Added AGI begin-and-end block.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1992 March 24 (MJC):
*        Added FILL parameter for Array mode.
*     1992 July 30 (MJC):
*        Changed parameter CURPIC to CURRENT for consistency.
*     1993 August 18 (MJC):
*        Added ASPECT parameter, and permitted FRACTION to have
*        different values for x and y.
*     1994 February 8 (MJC):
*        Annulled SGS zone in array mode that prevented large arrays
*        from being created.
*     1995 August 21 (MJC):
*        Made usage and examples lowercase.  Removed DEVICE as a
*        positional parameter, and made XPIC and YPIC positional; this
*        is to enable PICGRID to operate.  Added Related Applications.
*     1995 August 23 (MJC):
*        Removed CURRENT as a positional parameter, and made LBOUND and
*        UBOUND positional; this is to enable PICXY to operate and to
*        allow both PICGRID and PICXY to inherit the CURRENT option.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Global SSE definitions
      INCLUDE 'DAT_PAR'        ! Data-system constants
      INCLUDE 'PAR_ERR'        ! Parameter-system errors
      INCLUDE 'PRM_PAR'        ! Extreme and magic-value constants.

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN          ! string length ignoring trailing blanks

*  Local Constants:
      INTEGER NPTS             ! Number of x-y points to be measured
      PARAMETER ( NPTS = 2 )   ! by cursor for defining the picture
      INTEGER MXCHO            ! Maximum number of choices
      PARAMETER ( MXCHO = 3 )  !
      INTEGER MAXPIC           ! Maximum number of pictures in each
                               ! direction of an array, so the
                               ! maximum total number of pictures is
                               ! its square.
      PARAMETER( MAXPIC = 20 )

*  Local Variables:
      INTEGER
     :  I, J,                  ! Loop counters
     :  LABNUM,                ! Label number
     :  MAXNO,                 ! Maximum starting number for the labels
     :  NCLBNO,                ! No. of characters in label number
     :  NCPRFX,                ! No. of characters in label prefix
     :  NFRAC,                 ! Number of fraction sizes supplied
     :  NIMGMS,                ! Number of lines of image-display
                               ! messages
     :  NP,                    ! Number of points obtained by the cursor
     :  NTERMS                 ! Number of lines of terminal messages

      INTEGER
     :  PICID,                 ! AGI input picture identifier
     :  PICIDB,                ! AGI base picture identifier
     :  PICIDO,                ! AGI identifier of the bottom-left
                               ! picture of an array
     :  PICIDP,                ! Picture identifier for new picture
     :  START,                 ! Label number of the bottom-left picture
                               ! in an array
     :  XPIC,                  ! Number of pictures in the horizontal
                               ! direction
     :  YPIC,                  ! Number of pictures in the vertical
                               ! direction
     :  ZONE,                  ! SGS current zone identifier
     :  ZONEB,                 ! SGS zone identifier --- BASE picture
     :  ZONEN                  ! SGS zone identifier --- new picture

      LOGICAL                  ! True if :
     :  BOX,                   ! A box is to be drawn around the new
                               ! picture
     :  CURPIC,                ! New picture is to lie within the
                               ! current picture
     :  CURSOR,                ! The graphics device has a cursor with
                               ! suitable number of choices
     :  DEVCAN,                ! Image-display parameter is to be
                               ! cancelled
     :  IMGDIS,                ! Device is nominally an image display
     :  LABELS                 ! Labels are to given to the array of
                               ! pictures

      CHARACTER
     :  CLBNUM*9,              ! Label number
     :  IMGMES( 2 )*80,        ! Informational messages if device is
                               ! an image display
     :  LABEL*(DAT__SZNAM),    ! AGI picture label
     :  MODE*6,                ! Mode of determining the position and
                               ! size of the new picture
     :  PREFIX*(DAT__SZNAM-3), ! Label prefix
     :  TERMES( 2 )*80         ! Informational messages if device is
                               ! a terminal

      REAL
     :  ASPECT,                ! Aspect ratio
     :  DELTA,                 ! Width of the point markers in cursor
                               ! mode
     :  FILL,                  ! Linear filling factor for the array of
                               ! pictures
     :  FRACT( 2 ),            ! Linear fraction of the current or base
                               ! picture to form the new picture
     :  FRADEF,                ! Fraction dynamic default
     :  LBND( 2 ),             ! Lower bounds of the new picture
     :  OFFS( 2 ),             ! Offsets of the array of pictures with
                               ! respect to the origin of abutted set
     :  SIZE( 2 ),             ! Sizes of each picture array element
     :  UBND( 2 )              ! Upper bounds of the new picture

      REAL
     :  X1, X2,                ! x bounds of the new picture in world
                               ! co-ordinates
     :  Y1, Y2,                ! y bounds of the new picture in world
                               ! co-ordinates
     :  X1E, X2E,              ! x bounds of the old picture
     :  XIN, YIN,              ! Co-ordinates of the centre of the image
                               ! picture
     :  XLIM( MAXPIC * 2 ),    ! x limits of the array pictures
     :  XM, YM,                ! size of the old picture (not used)
     :  XP( NPTS ), YP( NPTS ),! Co-ordinates obtained via a cursor
     :  XR,                    ! x extent of the old picture
     :  Y1E, Y2E,              ! y bounds of the old picture
     :  YR                     ! y extent of the old picture

*.

*    Check the inherited status.

      IF ( STATUS .NE. SAI__OK ) RETURN

      CURPIC = .FALSE.
      CURSOR = .FALSE.
      DEVCAN = .FALSE.

*    Start an AGI scope.

      CALL AGI_BEGIN

*    Open GKS workstation to reset device

      CALL AGI_ASSOC( 'DEVICE', 'UPDATE', PICID, STATUS )

*    Activate SGS

      CALL AGS_ACTIV( STATUS )

*    If the graphics device was not available, report the error and
*    leave the programme.

      IF ( STATUS .NE. SAI__OK ) THEN

         IF ( STATUS .NE. PAR__ABORT ) THEN
            CALL ERR_REP( 'ERR_PICDEF_NID',
     :        'PICDEF: Graphics device not available or not '/
     :        /'recognised.', STATUS )
         END IF
         DEVCAN = .TRUE.
         GOTO 999
      END IF

*    Get the mode of operation

      CALL PAR_CHOIC( 'MODE', 'Cursor', 'Cursor,XY,Array,TL,BL,CL,TR,'/
     :                /'BR,CR,TC,BC,CC', .FALSE., MODE, STATUS )

      CALL PAR_GTD0L( 'CURRENT', .FALSE., .TRUE., CURPIC, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

      IF ( .NOT. CURPIC ) THEN

*       Get the BASE picture

         CALL AGI_IBASE( PICIDB, STATUS )
         CALL AGI_SELP( PICIDB, STATUS )

*       Get associated zone

         CALL AGS_NZONE( ZONEB, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'PICDEF__NOBAS',
     :        'PICDEF: Unable to get the BASE picture/zone from the '/
     :        /'database.', STATUS )

            DEVCAN = .TRUE.
            GOTO 999
         END IF
      ELSE

*       Get zone corresponding to the current picture

         CALL AGS_NZONE( ZONE, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'PICDEF__NCURP',
     :        'PICDEF: Unable to get the current zone from the '/
     :        /'database.', STATUS )

            DEVCAN = .TRUE.
            GOTO 999
         END IF

*    End of current-picture-to-be-used check

      END IF

      IF ( MODE .EQ. 'CURSOR' ) THEN

*       Create informational messages.

         TERMES( 1 ) = 'Type the spacebar to select a point.'
         TERMES( 2 ) = 'Type . to exit.'
         NTERMS = 2

         IMGMES( 1 ) = 'To select a point press the left '/
     :     /'button on the mouse or trackerball.'
         IMGMES( 2 ) = 'To exit press the right button.'
         NIMGMS = 2

*       Prepare the cursor. Specifically, does it exist and have the
*       correct attributes?

         CALL KPG1_PRCUR( 1, TERMES, NTERMS, IMGMES, NIMGMS, '12 .',
     :                    CURSOR, IMGDIS, STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 999

*       Has cursor mode been requested, but there is no cursor or it
*       does not have any choices?

         IF ( .NOT. CURSOR ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'PICDEF_NOCURSOR',
     :        'The workstation does not have a suitable cursor. '/
     :        /'Continuing in no-cursor mode.', STATUS )
            CALL ERR_FLUSH( STATUS )

*          Give the user a second chance to select a mode other than
*          with the cursor

            CALL PAR_CANCL( 'MODE', STATUS )
            CALL PAR_CHOIC( 'MODE', 'XY', 'XY,Array,TL,BL,CL,TR,BR,CR,'/
     :                      /'TC,BC,CC', .FALSE., MODE, STATUS )
            IF ( STATUS .NE. SAI__OK ) GOTO 999
         END IF

*    End of cursor-required check

      END IF

*    Is a rectangular box to be drawn about the new picture?

      CALL PAR_GTD0L( 'OUTLINE', .FALSE., .TRUE., BOX, STATUS )
      IF ( STATUS .EQ. PAR__ABORT ) GOTO 999

      IF ( MODE .EQ. 'CURSOR' ) THEN

*       Get the picture limits.

         CALL SGS_IZONE( X1E, X2E, Y1E, Y2E, XM, YM )

*       The cursor will appear at the centre of the picture.

         XIN = 0.5 * ( X1E + X2E )
         YIN = 0.5 * ( Y1E + Y2E )

*       Get the marker height.

         DELTA = 0.005 * MIN( X2E - X1E, Y2E - Y1E )

*       Obtain the bounds of the new picture via the cursor. A pair
*       of points is required, the points will be marked then erased.
*       The points should be unconnected, and distinct.

         CALL CURPTS( NPTS, .TRUE., MXCHO, .TRUE., .TRUE., DELTA,
     :                .FALSE., .TRUE., X1E, X2E, Y1E, Y2E, XIN, YIN,
     :                NP, XP, YP, STATUS )

*       Look out for an abort, i.e. the number of points is not NPTS.
*       Copy from the arrays into the standard (to the rest of the
*       application) variables.

         IF ( NP .GE. NPTS ) THEN
            X1 = MIN( XP( 1 ), XP( 2 ) )
            X2 = MAX( XP( 1 ), XP( 2 ) )
            Y1 = MIN( YP( 1 ), YP( 2 ) )
            Y2 = MAX( YP( 1 ), YP( 2 ) )

*          There is now a change from the graphics cursor operation
*          to report values on the text screen (assuming the device is
*          a terminal).  In order for the message to appear in the
*          correct plane, there must be a delay, so that the graphics
*          system can complete its work before the (faster and
*          independent) message system reports the cursor position.
*          The following calls achieves this synchronisation.

            CALL MSG_SYNC( STATUS )

*          Report the co-ordinates of the new frame.

            CALL MSG_SETR( 'CUR_X1', X1 )
            CALL MSG_SETR( 'CUR_Y1', Y1 )
            CALL MSG_SETR( 'CUR_X2', X2 )
            CALL MSG_SETR( 'CUR_Y2', Y2 )
            CALL MSG_OUT( 'CUR_RES', 'Co-ordinates are ( ^CUR_X1, '/
     :        /'^CUR_Y1 ) and ( ^CUR_X2, ^CUR_Y2 )', STATUS )

         ELSE
            STATUS = SAI__ERROR
            CALL ERR_REP( 'PICDEF__TOOFEW',
     :        'PICDEF: Too few points were selected via the cursor.',
     :        STATUS )
         END IF

      ELSE IF ( MODE .EQ. 'XY' ) THEN

*       Get limits from the environment.

         CALL CURRE( .FALSE., 'LBOUND', 'UBOUND', LBND, UBND, STATUS )

*       Use the same variables as for other modes.

         X1 = MIN( LBND( 1 ), UBND( 1 ) )
         X2 = MAX( LBND( 1 ), UBND( 1 ) )
         Y1 = MIN( LBND( 2 ), UBND( 2 ) )
         Y2 = MAX( LBND( 2 ), UBND( 2 ) )

*    ARray mode has been selected.

      ELSE IF ( MODE .EQ. 'ARRAY' ) THEN

*       Get the dimensions of the grid of pictures.

         CALL PAR_GDR0I( 'XPIC', 2, 1, MAXPIC, .FALSE., XPIC, STATUS )
         CALL PAR_GDR0I( 'YPIC', XPIC, 1, MAXPIC, .FALSE., YPIC,
     :                   STATUS )

*       Get the filling factor.

         CALL PAR_GDR0R( 'FILL', 1.0, 0.1, 1.0, .TRUE., FILL, STATUS )

         IF ( STATUS .NE. SAI__OK ) GOTO 999

*       Get the labelling prefix.

         LABELS = .TRUE.
         CALL PAR_GET0C( 'PREFIX', PREFIX, STATUS )

*       A null label means labelling is not required.

         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            LABELS = .FALSE.

         ELSE IF ( STATUS .NE. SAI__OK ) THEN
            GOTO 999

*       Get the start label number.

         ELSE

*          Find the length of the prefix.  The AGI label is restricted
*          the length of an HDS object name, so the maximum starting
*          number in a label has to be derived.

            NCPRFX = CHR_LEN( PREFIX )
            MAXNO = INT( MIN( DBLE( VAL__MAXI ),
     :              1.D1**( DAT__SZNAM - NCPRFX - 1 ) ) )
     :               - XPIC * YPIC + 1
            CALL PAR_GDR0I( 'LABELNO', 1, 0, MAXNO, .TRUE., START,
     :                      STATUS )
            IF ( STATUS .EQ. PAR__ABORT ) GOTO 999
         END IF

*       Get the bounds of the zone to be broken into the array.
*       Note SGS_ZPART could have been used for much of the array
*       creation but for the small number of zones permitted and the
*       filling factor.

         CALL SGS_IZONE( X1E, X2E, Y1E, Y2E, XM, YM )

*       Derive the dimension of the current picture in world
*       co-ordinates.

         XR = X2E - X1E
         YR = Y2E - Y1E

*       Obtain the offsets of each picture.

         OFFS( 1 ) = ( 1.0 - FILL ) * 0.5 * XR / REAL( XPIC )
         OFFS( 2 ) = ( 1.0 - FILL ) * 0.5 * YR / REAL( YPIC )

*       Calculate the dimensions of each new picture.

         SIZE( 1 ) = FILL * XR / REAL( XPIC )
         SIZE( 2 ) = FILL * YR / REAL( YPIC )

*       Find the horizontal bounds of the new zones.

         DO  I = 1, XPIC
            XLIM( 2 * I - 1 ) = X1E + OFFS( 1 ) + XR * 
     :                          REAL( I - 1 ) / REAL( XPIC )
            XLIM( 2 * I ) = XLIM( 2 * I - 1 ) + SIZE( 1 )
         END DO

*       Ensure that rounding errors have not made any of the new
*       pictures exceed the bounds of the current picture.

         XLIM( 1 ) = MAX( X1E, XLIM( 1 ) )
         XLIM( 2 * XPIC ) = MIN( X2E, XLIM( 2 * XPIC ) )

*       Initialise the label numbering.

         IF ( LABELS ) LABNUM = START - 1

*       Loop for all the pictures in the y direction.

         DO  J = 1, YPIC

*          Define the vertical bounds of the new zones, ensuring that
*          the created picture does not lie outside the bounds of the
*          current picture.

            Y1 = MAX( Y1E, Y1E + OFFS( 2 ) + YR *
     :           REAL( J - 1 ) / REAL( YPIC ) )
            Y2 = MIN( Y2E, Y1 + SIZE( 2 ) )

*          Loop for each picture along the x direction, whose bounds
*          have already been computed.

            DO  I = 1, XPIC
               X1 = XLIM( 2 * I - 1 )
               X2 = XLIM( 2 * I )

*             Pictures must lie within the current picture, so the
*             original picture identifier and zone must be selected.
*             These are either the BASE picture or the current picture
*             on input.

               IF ( CURPIC ) THEN
                  CALL AGI_SELP( PICID, STATUS )
                  CALL SGS_SELZ( ZONE, STATUS )
               ELSE
                  CALL AGI_SELP( PICIDB, STATUS )
                  CALL SGS_SELZ( ZONEB, STATUS )
               END IF

*             Free up the last picture unless it was the first.  Also
*             free its SGS zone to prevent exhausting the available
*             zones.  These operations must be done once there is
*             a new current picture and zone, as you cannot delete
*             the current zone.

               IF ( I .GT. 2 .OR. J .GT. 1 ) THEN
                  CALL AGI_ANNUL( PICIDP, STATUS )
                  CALL SGS_RELZ( ZONEN )
               END IF

*             Create the new zone.

               CALL SGS_ZONE( X1, X2, Y1, Y2, ZONEN, STATUS )
               CALL SGS_SW( X1, X2, Y1, Y2, STATUS )

*             Optionally, draw a box around it.

               IF ( BOX ) CALL SGS_BOX( X1, X2, Y1, Y2 )

*             Store the new zone as a database picture.

               CALL AGS_SZONE( 'FRAME', 'KAPPA_PICDEF', PICIDP, STATUS )

               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL MSG_SETI( 'X', I )
                  CALL MSG_SETI( 'Y', J )
                  CALL ERR_REP( 'ERR_PICDEF_DBSP',
     :              'PICDEF: Error while storing the new picture '/
     :              /'(^X,^Y) in the graphics database.', STATUS )
                  CALL AGI_SELP( PICID, STATUS )
               END IF

*             Store the first picture's identifier.

               IF ( I .EQ. 1 .AND. J .EQ. 1 ) PICIDO = PICIDP

*             Label the picture if required.

               IF ( LABELS ) THEN

*                Derive the label from the prefix and element number.

                  LABNUM = LABNUM + 1
                  CALL CHR_ITOC( LABNUM, CLBNUM, NCLBNO )
                  LABEL = PREFIX( :NCPRFX )//CLBNUM( :NCLBNO )
                  CALL AGI_SLAB( -1, LABEL, STATUS )
               END IF

*          End of the loops used to form the array of pictures.

            END DO
         END DO

*       Make the bottom-left picture the current picture.

         CALL AGI_SELP( PICIDO, STATUS )
      ELSE

*       Start a new error context.

         CALL ERR_MARK

*       Obtain the aspect ratio, ensuring that it is positive, and no
*       dynamic default.

         CALL PAR_GDR0R( 'ASPECT', VAL__BADR, VAL__SMLR, VAL__MAXR,
     :                   .FALSE., ASPECT, STATUS )

*       Handle null transparently.  A negative value tells the
*       subroutine not to apply an aspect-ratio constraint.

         IF ( STATUS .EQ. PAR__NULL ) THEN
            ASPECT = -1.0
            CALL ERR_ANNUL( STATUS )

*       Set dynamic defaults for the fraction.

            FRADEF = 0.5
         ELSE
            FRADEF = 1.0
         END IF

*       Obtain the fractional sizes within limits.

         CALL PAR_DEF0R( 'FRACTION', FRADEF, STATUS )
         CALL PAR_GDRVR( 'FRACTION', 2, 0.10, 1.0, FRACT, NFRAC,
     :                   STATUS )

*       Handle null transparently.

         IF ( STATUS .EQ. PAR__NULL ) THEN
            FRACT( 1 ) = 1.0
            FRACT( 2 ) = 1.0
            CALL ERR_ANNUL( STATUS )
         END IF

*       Release the new error context.

         CALL ERR_RLSE

         IF ( STATUS .NE. SAI__OK ) GOTO 999

*       Duplicate fractional size if only one value is given.

         IF ( NFRAC .EQ. 1 ) FRACT( 2 ) = FRACT( 1 )

*       Get the limits of the zone using the fraction and position code.

         CALL KPS1_FRARE( MODE, FRACT, ASPECT, X1, X2, Y1, Y2, STATUS )
      END IF

      IF ( STATUS .EQ. SAI__OK .AND. MODE .NE. 'ARRAY' ) THEN

*       Create the new zone.

         CALL SGS_ZONE( X1, X2, Y1, Y2, ZONEN, STATUS )
         CALL SGS_SW( X1, X2, Y1, Y2, STATUS )
         IF ( BOX ) CALL SGS_BOX( X1, X2, Y1, Y2 )

*       Store the new zone as a database picture.

         CALL AGS_SZONE( 'FRAME', 'KAPPA_PICDEF', PICIDP, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'ERR_PICDEF_DBSP',
     :        'PICDEF: Error while storing the new picture in the '/
     :        /'graphics database.', STATUS )
            CALL AGI_SELP( PICID, STATUS )
         END IF
      END IF

 999  CONTINUE

*    Deactivate SGS and close AGI workstation.

      CALL AGS_DEACT( STATUS )
      IF ( DEVCAN ) THEN
         CALL AGI_CANCL( 'DEVICE', STATUS )
      ELSE
         CALL AGI_ANNUL( PICID, STATUS )
      END IF

*    End the AGI scope.

      CALL AGI_END( -1, STATUS )

      END
