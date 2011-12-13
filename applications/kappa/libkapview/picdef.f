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
*
*     You may specify a single new picture using one of three methods:
*       1.  moving a cursor to define the lower and upper bounds via
*           pressing choice number 1 (the application will instruct what
*           to do for the specific graphics device), provided a cursor
*           is available on the chosen graphics workstation;
*       2.  obtaining the bounds from the environment (in world
*           co-ordinates of the reference picture);
*       3.  or by giving a position and size for the new picture.  The
*           position is specified by a two-character code. The first
*           controls the vertical location, and may be "T", "B", or "C"
*           to create the new picture at the top, bottom, or in the
*           centre respectively.  The second defines the horizontal
*           situation, and may be "L", "R", or "C" to define a new
*           picture to the left, right, or in the centre respectively.
*           Thus a code of "BR" will make a new picture in the
*           bottom-right corner.  The size of the new picture along each
*           axis may be specified either in centimetres, or as a
*           fraction of the corresponding axis of the reference picture.
*           The picture may also be forced to have a specified aspect
*           ratio.
*
*     The picture created becomes the current picture on exit.
*
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
*        fractional sizes.  [!]
*     CURRENT = _LOGICAL (Read)
*        TRUE if the new picture is to lie within the current picture,
*        otherwise the new picture can lie anywhere within the BASE
*        picture.  In other words, when it is TRUE the current picture
*        is the reference picture, and when FALSE the base is the
*        reference picture.  [FALSE]
*     DEVICE = DEVICE (Read)
*        The graphics device.  [Current graphics device]
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
*        per cent.  FILL must lie between 0.1 and 1.0.  [1.0]
*     FRACTION( ) = _REAL (Read)
*        The fractional size of the new picture along each axis,
*        applicable for modes other than Array, Cursor, and XY.  Thus
*        FRACTION controls the relative shape as well as the size of
*        the new picture.  If only a single value is given then it is
*        applied to both x and y axes, whereupon the new picture has
*        the shape of the reference picture.  So a value of 0.5 would
*        create a picture 0.25 the area of the current or BASE picture.
*        The default is 0.5, unless parameter ASPECT is not null, when
*        the default is 1.0. This parameter is not used if the picture
*        size is specified in centimetres using parameter SIZE.  []
*     LABELNO = _INTEGER (Read)
*        The number used to form the label for the first (bottom-left)
*        picture in Array mode.  It cannot be negative. [1]
*     LBOUND( 2 ) = _REAL (Read)
*        BASEPIC co-ordinates of the lower bounds that defines the new
*        picture.  The BASEPIC co-ordinates of the bottom-left corner of
*        the BASE picture are (0,0).  The shorter dimension of the BASE
*        picture has length 1.0, and the other axis has a length
*        greater than 1.0.  The suggested default is the top-right of
*        the current picture. (XY mode)
*     MODE = LITERAL (Read)
*        Method for selecting the new picture. The options are "Cursor"
*        for cursor mode (provided the graphics device has one), "XY"
*        to select x-y limits, and "Array" to create a grid of
*        equal-sized FRAME pictures.  The remainder are locations
*        specified by two characters, the first corresponding to the
*        vertical position and the second the horizontal.  For the
*        vertical, valid positions are T(op), B(ottom), or C(entre);
*        and for the horizontal the options are L(eft), R(ight), or
*        C(entre).  ["Cursor"]
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
*     SIZE( 2 ) = _REAL (Read)
*        The size of the new picture along both axes, in centimetres,
*        applicable for modes other than Array, Cursor, and XY. If a
*        single value is given, it is used for both axes.  If a null
*        value (!) is given, then the size of the picture is determined
*        by parameter FRACTION. [!]
*     UBOUND( 2 ) = _REAL (Read)
*        BASEPIC co-ordinates of the upper bound that defines the new
*        picture.  The BASEPIC co-ordinates of the bottom-left corner of
*        the BASE picture are (0,0).  The shorter dimension of the BASE
*        picture has length 1.0, and the other axis has a length greater
*        than 1.0. The suggested default is the top-right of the current
*        picture. (XY mode)
*     XPIC = _INTEGER (Read)
*        The number of new pictures to be formed horizontally in the
*        BASE or current picture in Array mode.  The total number of
*        new pictures is XPIC * YPIC.  The value must lie in the
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
*     picdef bl size=[15,10]
*        Creates a new FRAME picture within the full display area on
*        the current graphics device, and an outline is drawn around
*        the new picture.  This picture is 15 by 10 centimetres in size
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

*  Related Applications:
*     KAPPA: PICBASE, PICCUR, PICDATA, PICFRAME, PICGRID, PICLABEL,
*            PICLIST, PICSEL, PICXY.

*  Copyright:
*     Copyright (C) 1989-1994 Science & Engineering Research Council.
*     Copyright (C) 1995, 1998, 2000, 2004 Central Laboratory of the
*     Research Councils. Copyright (C) 2006 Particle Physics &
*     Astronomy Research Council. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     DSB: David S. Berry (STARLINK)
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
*     21-AUG-1998 (DSB):
*        Converted to use PGPLOT.  Re-formatted code.  Added parameter
*        SIZE.
*     15-FEB-2000 (DSB):
*        Calls to KPG1_PGCUR modified for new argument list.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2006 April 12 (MJC):
*        Remove unused variables, correct punctuation, and wrapped
*        long lines.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Global SSE definitions
      INCLUDE 'DAT_PAR'        ! Data-system constants
      INCLUDE 'AST_PAR'        ! AST constants
      INCLUDE 'PAR_ERR'        ! Parameter-system errors
      INCLUDE 'PRM_PAR'        ! Extreme and magic-value constants.

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN          ! String length ignoring trailing blanks

*  Local Constants:
      INTEGER NPTS             ! Number of x-y points to be measured
      PARAMETER ( NPTS = 2 )   ! by cursor for defining the picture

      INTEGER MAXPIC           ! Maximum number of pictures in each
                               ! direction of an array, so the
                               ! maximum total number of pictures is
                               ! its square.
      PARAMETER( MAXPIC = 20 )

*  Local Variables:
      INTEGER
     :  ACT( NPTS ),           ! Cursor choices
     :  I, J,                  ! Loop counters
     :  LABNUM,                ! Label number
     :  MAXNO,                 ! Maximum starting number for the labels
     :  NCLBNO,                ! No. of characters in label number
     :  NCPRFX,                ! No. of characters in label prefix
     :  NFRAC,                 ! Number of fraction sizes supplied
     :  NP,                    ! Number of points obtained by the cursor
     :  NSIZE                  ! Number of SIZE values obtained

      INTEGER
     :  PICID,                 ! AGI input picture identifier
     :  PICIDB,                ! AGI base picture identifier
     :  PICIDO,                ! AGI identifier of the bottom-left
                               ! picture of an array
     :  PICIDP,                ! Picture identifier for new picture
     :  START,                 ! Label number of the bottom-left picture
                               ! in an array
     :  UP,                    ! Highest available colour index
     :  XPIC,                  ! Number of pictures in the horizontal
                               ! direction
     :  YPIC                   ! Number of pictures in the vertical
                               ! direction

      LOGICAL                  ! True if :
     :  BOX,                   ! A box is to be drawn around the new
                               ! picture
     :  CURPIC,                ! New picture is to lie within the
                               ! current picture
     :  CURSOR,                ! The graphics device has a cursor with
                               ! suitable number of choices
     :  DEVCAN,                ! Image-display parameter is to be
                               ! cancelled
     :  LABELS                 ! Labels are to given to the array of
                               ! pictures

      CHARACTER
     :  CLBNUM*9,              ! Label number
     :  AMES( 2 )*80,          ! Informational messages about use of
                               ! cursor
     :  LABEL*(DAT__SZNAM),    ! AGI picture label
     :  MODE*6,                ! Mode of determining the position and
                               ! size of the new picture
     :  PREFIX*(DAT__SZNAM-3)  ! Label prefix

      REAL
     :  ASPECT,                ! Aspect ratio
     :  FILL,                  ! Linear filling factor for the array of
                               ! pictures
     :  FRACT( 2 ),            ! Linear fraction of the current or base
                               ! picture to form the new picture
     :  FRADEF,                ! Fraction dynamic default
     :  LBND( 2 ),             ! Lower bounds of the new picture
     :  OFFS( 2 ),             ! Offsets of the array of pictures with
                               ! respect to the origin of abutted set
     :  SIZE( 2 ),             ! Sizes of each picture array element
     :  SZMAX( 2 ),            ! Max size of picture in cm
     :  SZMIN( 2 ),            ! Min size of picture in cm
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

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      CURPIC = .FALSE.
      CURSOR = .FALSE.
      DEVCAN = .FALSE.

*  Start an AGI scope.
      CALL AGI_BEGIN

*  Open AGI database.
      CALL AGI_ASSOC( 'DEVICE', 'UPDATE', PICID, STATUS )

*  Activate PGPLOT
      CALL AGP_ACTIV( STATUS )

*  If the graphics device was not available, report the error and
*  leave the programme.
      IF ( STATUS .NE. SAI__OK ) THEN

         IF ( STATUS .NE. PAR__ABORT ) THEN
            CALL ERR_REP( 'ERR_PICDEF_NID',
     :        'PICDEF: Graphics device not available or not '/
     :        /'recognised.', STATUS )
         END IF
         DEVCAN = .TRUE.
         GO TO 999
      END IF

*  Get the mode of operation
      CALL PAR_CHOIC( 'MODE', 'Cursor', 'Cursor,XY,Array,TL,BL,CL,TR,'/
     :                /'BR,CR,TC,BC,CC', .TRUE., MODE, STATUS )

      CALL PAR_GTD0L( 'CURRENT', .FALSE., .TRUE., CURPIC, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

      IF ( .NOT. CURPIC ) THEN

*  Get the BASE picture
         CALL AGI_IBASE( PICIDB, STATUS )
         CALL AGI_SELP( PICIDB, STATUS )

*  Get associated viewport
         CALL AGP_NVIEW( .FALSE., STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'PICDEF__NOBAS',
     :        'PICDEF: Unable to get the BASE picture from the '/
     :        /'database.', STATUS )

            DEVCAN = .TRUE.
            GO TO 999
         END IF
      ELSE

*  Get viewport corresponding to the current picture
         CALL AGP_NVIEW( .FALSE., STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'PICDEF__NCURP',
     :        'PICDEF: Unable to get the current picture from the '/
     :        /'database.', STATUS )

            DEVCAN = .TRUE.
            GO TO 999
         END IF

*  End of current-picture-to-be-used check
      END IF

*  PGPLOT resets the colour palette when the device is opened (which
*  occurs when AGI creates the first PGPLOT viewport - above).
*  Therefore we need to re-instate the colour palette set by the user,
*  reading it from the HDS file kappa-palette.sdf in the users
*  adam directory.
      CALL KPG1_PLLOD( STATUS )

*  Has cursor mode been requested, but there is no cursor?
      IF ( MODE .EQ. 'CURSOR' .AND. STATUS .EQ. SAI__OK ) THEN

         CALL KPG1_PQVID( 'DEVICE', ' ', 'CURSOR', 0, UP, STATUS )

         IF ( STATUS .EQ. SAI__ERROR ) THEN
            CALL ERR_REP( 'PICDEF_NOCURSOR', 'Continuing in no-cursor '/
     :        /'mode.', STATUS )
            CALL ERR_FLUSH( STATUS )

*  Give the user a second chance to select a mode other than
*  with the cursor
            CALL PAR_CANCL( 'MODE', STATUS )
            CALL PAR_CHOIC( 'MODE', 'XY', 'XY,Array,TL,BL,CL,TR,BR,CR,'/
     :                      /'TC,BC,CC', .FALSE., MODE, STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 999
         END IF

*  End of cursor-required check
      END IF

*  Is a rectangular box to be drawn about the new picture?
      CALL PAR_GTD0L( 'OUTLINE', .FALSE., .TRUE., BOX, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

      IF ( MODE .EQ. 'CURSOR' ) THEN

*  Get the bounds of the picture in millimetres, and convert to metres.
         CALL PGQVP( 2, X1E, X2E, Y1E, Y2E )
         XM = ( X2E - X1E )/1000.0
         YM = ( Y2E - Y1E )/1000.0

*  Get its bounds in world coordinates.
         CALL PGQWIN( X1E, X2E, Y1E, Y2E )

*  The cursor will appear at the centre of the picture.
         XIN = 0.5 * ( X1E + X2E )
         YIN = 0.5 * ( Y1E + Y2E )

*  Obtain the bounds of the new picture via the cursor.  A pair of
*  points is required, the points will be marked.
         AMES( 1 ) = 'select a point'
         AMES( 2 ) = 'quit'
         CALL KPG1_PGCUR( .TRUE., 'select 2 distinct points', 2, AMES,
     :                    ' .', X1E, X2E, Y1E, Y2E, 2, XIN, YIN, NPTS,
     :                    2, 0, 0, 1, AST__NULL, XP, YP, ACT, NP,
     :                    STATUS )

*  Look out for an abort, i.e. the number of points is not 2.  Copy from
*  the arrays into the standard (to the rest of the application)
*  variables.
         IF ( NP .GE. NPTS ) THEN
            X1 = MIN( XP( 1 ), XP( 2 ) )
            X2 = MAX( XP( 1 ), XP( 2 ) )
            Y1 = MIN( YP( 1 ), YP( 2 ) )
            Y2 = MAX( YP( 1 ), YP( 2 ) )

*  There is now a change from the graphics cursor operation to report
*  values on the text screen (assuming the device is a terminal).  In
*  order for the message to appear in the correct plane, there must be
*  a delay, so that the graphics system can complete its work before the
*  (faster and independent) message system reports the cursor position.
*  The following calls achieves this synchronisation.
            CALL MSG_SYNC( STATUS )

*  Report the co-ordinates of the new frame.
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

*  Get limits from the environment.
         CALL KPG1_GDBND( 'LBOUND', 'UBOUND', LBND, UBND, STATUS )

*  Use the same variables as for other modes.
         X1 = MIN( LBND( 1 ), UBND( 1 ) )
         X2 = MAX( LBND( 1 ), UBND( 1 ) )
         Y1 = MIN( LBND( 2 ), UBND( 2 ) )
         Y2 = MAX( LBND( 2 ), UBND( 2 ) )

*  Array mode has been selected.
      ELSE IF ( MODE .EQ. 'ARRAY' ) THEN

*  Get the dimensions of the grid of pictures.
         CALL PAR_GDR0I( 'XPIC', 2, 1, MAXPIC, .FALSE., XPIC, STATUS )
         CALL PAR_GDR0I( 'YPIC', XPIC, 1, MAXPIC, .FALSE., YPIC,
     :                   STATUS )

*  Get the filling factor.
         CALL PAR_GDR0R( 'FILL', 1.0, 0.1, 1.0, .TRUE., FILL, STATUS )

         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get the labelling prefix.
         LABELS = .TRUE.
         CALL PAR_GET0C( 'PREFIX', PREFIX, STATUS )

*  A null label means labelling is not required.
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            LABELS = .FALSE.

         ELSE IF ( STATUS .NE. SAI__OK ) THEN
            GO TO 999

*  Get the start label number.
         ELSE

*  Find the length of the prefix.  The AGI label is restricted to the
*  length of an HDS object name, so the maximum starting number in a
*  label has to be derived.
            NCPRFX = CHR_LEN( PREFIX )
            MAXNO = INT( MIN( DBLE( VAL__MAXI ),
     :              1.D1**( DAT__SZNAM - NCPRFX - 1 ) ) )
     :               - XPIC * YPIC + 1
            CALL PAR_GDR0I( 'LABELNO', 1, 0, MAXNO, .TRUE., START,
     :                      STATUS )
            IF ( STATUS .EQ. PAR__ABORT ) GO TO 999
         END IF

*  Get the bounds of the picture in millimetres, and convert to metres.
         CALL PGQVP( 2, X1E, X2E, Y1E, Y2E )
         XM = ( X2E - X1E )/1000.0
         YM = ( Y2E - Y1E )/1000.0

*  Get its bounds in world coordinates.
         CALL PGQWIN( X1E, X2E, Y1E, Y2E )

*  Derive the dimension of the current picture in world
*  co-ordinates.
         XR = X2E - X1E
         YR = Y2E - Y1E

*  Obtain the offsets of each picture.
         OFFS( 1 ) = ( 1.0 - FILL ) * 0.5 * XR / REAL( XPIC )
         OFFS( 2 ) = ( 1.0 - FILL ) * 0.5 * YR / REAL( YPIC )

*  Calculate the dimensions of each new picture.
         SIZE( 1 ) = FILL * XR / REAL( XPIC )
         SIZE( 2 ) = FILL * YR / REAL( YPIC )

*  Find the horizontal bounds of the new pictures.
         DO  I = 1, XPIC
            XLIM( 2 * I - 1 ) = X1E + OFFS( 1 ) + XR *
     :                          REAL( I - 1 ) / REAL( XPIC )
            XLIM( 2 * I ) = XLIM( 2 * I - 1 ) + SIZE( 1 )
         END DO

*  Ensure that rounding errors have not made any of the new
*  pictures exceed the bounds of the current picture.
         XLIM( 1 ) = MAX( X1E, XLIM( 1 ) )
         XLIM( 2 * XPIC ) = MIN( X2E, XLIM( 2 * XPIC ) )

*  Initialise the label numbering.
         IF ( LABELS ) LABNUM = START - 1

*  Pictures must lie within the current picture, so the original
*  picture identifier and viewport must be selected.  These are either
*  the BASE picture or the current picture on input.
         IF ( CURPIC ) THEN
            CALL AGI_SELP( PICID, STATUS )
            CALL AGP_NVIEW( .FALSE., STATUS )
         ELSE
            CALL AGI_SELP( PICIDB, STATUS )
            CALL AGP_NVIEW( .FALSE., STATUS )
         END IF

         CALL PGQWIN( X1, X2, Y1, Y2 )

*  Loop for all the pictures in the y direction.
         DO  J = 1, YPIC

*  Define the vertical bounds of the new pictures, ensuring that
*  the created picture does not lie outside the bounds of the
*  current picture.
            Y1 = MAX( Y1E, Y1E + OFFS( 2 ) + YR *
     :           REAL( J - 1 ) / REAL( YPIC ) )
            Y2 = MIN( Y2E, Y1 + SIZE( 2 ) )

*  Loop for each picture along the x direction, whose bounds
*  have already been computed.
            DO  I = 1, XPIC
               X1 = XLIM( 2 * I - 1 )
               X2 = XLIM( 2 * I )

*  Create the new viewport
               CALL KPG1_PGCUT( X1, X2, Y1, Y2, STATUS )

*  Store the new viewport as a database picture.
               CALL AGP_SVIEW( 'FRAME', 'KAPPA_PICDEF', PICIDP, STATUS )

               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL MSG_SETI( 'X', I )
                  CALL MSG_SETI( 'Y', J )
                  CALL ERR_REP( 'ERR_PICDEF_DBSP',
     :              'PICDEF: Error while storing the new picture '/
     :              /'(^X,^Y) in the graphics database.', STATUS )
                  CALL AGI_SELP( PICID, STATUS )
               END IF

*  Store the first picture's identifier.  Otherwise, annul the
*  identifier.
               IF ( I .EQ. 1 .AND. J .EQ. 1 ) THEN
                  PICIDO = PICIDP
               ELSE
                  CALL AGI_ANNUL( PICIDP, STATUS )
               END IF

*  Label the picture if required.
               IF ( LABELS ) THEN

*  Derive the label from the prefix and element number.
                  LABNUM = LABNUM + 1
                  CALL CHR_ITOC( LABNUM, CLBNUM, NCLBNO )
                  LABEL = PREFIX( :NCPRFX )//CLBNUM( :NCLBNO )
                  CALL AGI_SLAB( -1, LABEL, STATUS )
               END IF

*  Re-instate the reference picture and viewport.
               IF ( CURPIC ) THEN
                  CALL AGI_SELP( PICID, STATUS )
                  CALL AGP_NVIEW( .FALSE., STATUS )
               ELSE
                  CALL AGI_SELP( PICIDB, STATUS )
                  CALL AGP_NVIEW( .FALSE., STATUS )
               END IF

*  Optionally, draw a box around the new viewport. Do this once the
*  reference picture has been made current to avoid PGPLOT clipping.
               IF ( BOX ) THEN
                  CALL PGSFS( 2 )
                  CALL PGRECT( X1, X2, Y1, Y2 )
               END IF

*  End of the loops used to form the array of pictures.
            END DO
         END DO

*  Make the bottom-left picture the current picture.
         CALL AGI_SELP( PICIDO, STATUS )

      ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*  Get the bounds of the picture in millimetres, and convert to
*  centimetres.
         CALL PGQVP( 2, X1E, X2E, Y1E, Y2E )
         SZMAX( 1 ) = ( X2E - X1E )/10.0
         SZMAX( 2 ) = ( Y2E - Y1E )/10.0
         SZMIN( 1 ) = 0.5
         SZMIN( 2 ) = 0.5

*  Start a new error context.
         CALL ERR_MARK

*  Get the dimensions of the picture in centimetres.
         CALL PAR_GRMVR( 'SIZE', 2, SZMIN, SZMAX, SIZE, NSIZE, STATUS )

*  If values were obtained succesfully...
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Ensure we have 2 values.
            IF ( NSIZE .EQ. 1 ) SIZE( 2 ) = SIZE( 1 )

*  Convert these sizes to fractions of the reference picture.
            FRACT( 1 ) = SIZE( 1 ) / SZMAX( 1 )
            FRACT( 2 ) = SIZE( 2 ) / SZMAX( 2 )

*  Indicate that the aspect ratio is not to be changed.
            ASPECT = -1.0

*  If a null value (!) was given, annull the error.
         ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )

*  Obtain the aspect ratio, ensuring that it is positive, and no
*  dynamic default.
            CALL PAR_GDR0R( 'ASPECT', VAL__BADR, VAL__SMLR, VAL__MAXR,
     :                      .FALSE., ASPECT, STATUS )

*  Handle null transparently.  A negative value tells the
*  subroutine not to apply an aspect-ratio constraint.
            IF ( STATUS .EQ. PAR__NULL ) THEN
               ASPECT = -1.0
               CALL ERR_ANNUL( STATUS )

*  Set dynamic defaults for the fraction.
               FRADEF = 0.5
            ELSE
               FRADEF = 1.0
            END IF

*  Obtain the fractional sizes within limits.
            CALL PAR_DEF0R( 'FRACTION', FRADEF, STATUS )
            CALL PAR_GDRVR( 'FRACTION', 2, 0.10, 1.0, FRACT, NFRAC,
     :                      STATUS )

*  Handle null transparently.
            IF ( STATUS .EQ. PAR__NULL ) THEN
               FRACT( 1 ) = 1.0
               FRACT( 2 ) = 1.0
               CALL ERR_ANNUL( STATUS )
            END IF

*  Duplicate fractional size if only one value is given.
            IF ( NFRAC .EQ. 1 ) FRACT( 2 ) = FRACT( 1 )

         END IF

*  Release the new error context.
         CALL ERR_RLSE

         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get the limits of the viewport using the fraction and position code.
         CALL KPG1_GDARE( MODE, FRACT, ASPECT, X1, X2, Y1, Y2, STATUS )

      END IF

*  Create the viewport. This will already have been done for ARRAY mode.
      IF ( STATUS .EQ. SAI__OK .AND. MODE .NE. 'ARRAY' ) THEN

*  Optionally, draw a box around it. Do this before the new viewport is
*  created to avoid PGPLOT clipping.
         IF ( BOX ) THEN
            CALL PGSFS( 2 )
            CALL PGRECT( X1, X2, Y1, Y2 )
         END IF

*  Create the new viewport.
         CALL KPG1_PGCUT( X1, X2, Y1, Y2, STATUS )

*  Store the new viewport as a database picture.
         CALL AGP_SVIEW( 'FRAME', 'KAPPA_PICDEF', PICIDP, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'ERR_PICDEF_DBSP',
     :        'PICDEF: Error while storing the new picture in the '/
     :        /'graphics database.', STATUS )
            CALL AGI_SELP( PICID, STATUS )
         END IF

      END IF


 999  CONTINUE

*  Deactivate PGPLOT and close AGI workstation.
      CALL AGP_DEACT( STATUS )
      IF ( DEVCAN ) THEN
         CALL AGI_CANCL( 'DEVICE', STATUS )
      ELSE
         CALL AGI_ANNUL( PICID, STATUS )
      END IF

*  End the AGI scope.
      CALL AGI_END( -1, STATUS )

*  Add a context report if anything went wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PICDEF_ERR', 'PICDEF: Failed to define one '/
     :     /'or more new graphics pictures.', STATUS )
      END IF

      END
