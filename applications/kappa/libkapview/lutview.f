      SUBROUTINE LUTVIEW( STATUS )
*+
*  Name:
*     LUTVIEW

*  Purpose:
*     Draws a colour-table key.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation
*     CALL LUTVIEW( STATUS )

*  Usage:
*     lutview [mode] [low] [high] [annota] [curpic] [device] lbound=?
*       ubound=?

*  Description:
*     This application draws a key of the colour table at a location
*     you select.  You can constrain the key to lie within either the
*     current or the BASE picture.  The key may be annotated, in which
*     case you must allow sufficient room for the annotations.  For
*     oblate regions colour index increases from left to right, and for
*     prolate it increases from bottom to top; the annotations
*     appearing to the top and right respectively.  The annotations
*     scale linearly between the values corresponding to the lower and
*     upper indices of the colour table.

*     The situation of the key is defined by the co-ordinates of a pair 
*     of opposite corners of a rectangular region.  You may specify
*     these using one of the following methods:
*       1.  moving a cursor and pressing a button on the mouse or
*           trackerball;
*       2.  obtaining bounds from the environment (in normalised
*           co-ordinates of the reference picture).

*  ADAM Parameters:
*     ANNOTA = _LOGICAL (Read)
*        ANNOTA is TRUE if the colour table is to be annotated with the
*        pen numbers.  Note a squarer picture should be created so that
*        the annotations are legible. [FALSE]
*     CURPIC = _LOGICAL (Read)
*        If CURPIC is TRUE, the colour table key is to lie within the
*        current picture, otherwise the new picture can lie anywhere
*        within the BASE picture.  It is ignored when the
*        current-picture mode is selected. [FALSE]
*     DEVICE = DEVICE (Read)
*        The image-display device on which the colour table is to be
*        drawn.  The device must be in one of the following GNS
*        categories: IMAGE_DISPLAY, IMAGE_OVERLAY, MATRIX_PRINTER, or
*        WINDOW, and have at least 24 greyscale intensities or colour
*        indices.  It must also not reset when the device is opened
*        (since the colour table would be lost) unless parameter LUT
*        does not have the null value.  [Current image-display device]
*     HIGH = _REAL (Read)
*        The value corresponding to the maximum colour index.  It is
*        used to calculate the annotation scale for the key.  If it
*        is null (!) the maximum colour index is used.
*        [Current display linear-scaling maximum]
*     LBOUND( 2 ) = _REAL (Read)
*        Co-ordinates of the lower bound that defines the location of
*        the colour-table plot.  These are in the world system of the
*        BASE or current picture. (XY mode)
*     LOW = _REAL (Read)
*        The value corresponding to the minimum colour index.  It is
*        used to calculate the annotation scale for the key.  If it
*        is null (!) the minimum colour index is used.
*        [Current display linear-scaling minimum]
*     LUT = NDF (Read)
*        Name of the NDF containing a lookup table as its data array;
*        the lookup table is written to the image-display's colour
*        table.  The purpose of this parameter is to provide a means of
*        controlling the appearance of the image on certain devices,
*        such as colour printers, that do not have a dynamic colour
*        table, i.e. the colour table is reset when the device is
*        opened.  If used with dynamic devices, such as windows or
*        Ikons, the new colour table remains after this application has
*        completed.  A null, !, means that the existing colour table
*        will be used.
*
*        The LUT must be two-dimensional, the first dimension
*        being 3, and the second being arbitrary.  The method used to
*        compress or expand the colour table if the second dimension is
*        different from the number of unreserved colour indices is
*        controlled by parameter NN.  Also the LUT's values must lie in 
*        the range 0.0--1.0. [!]
*     MODE = LITERAL (Read)
*        Method for defining the position, size and shape of the
*        colour-table key.  The options are "Cursor" for cursor mode
*        (provided the graphics device has one), "XY" to select x-y
*        limits via the parameter system, and "Picture" where the
*        whole of the current picture is used.  Additional positioning
*        options are available by using other KAPPA applications to
*        create new pictures and then specifying the picture mode.
*        ["Cursor"]
*     NN = _LOGICAL (Read)
*        If NN is TRUE, the input lookup table is mapped to the colour
*        table by using the nearest-neighbour method.  This preserves
*        sharp edges and is better for lookup tables with blocks of
*        colour.  If NN is FALSE, linear interpolation is used, and
*        this is suitable for smoothly varying colour tables.  NN is
*        ignored unless LUT is not null. [FALSE]
*     OUTLINE = _LOGICAL (Read)
*        If OUTLINE is TRUE, a box that delimits the key is drawn.  A
*        box is always drawn when there are annotations. [TRUE]
*     UBOUND( 2 ) = _REAL (Read)
*        Co-ordinates of the upper bound that defines the location
*        of the colour-table plot.  These are in the world system of the
*        BASE or current picture. (XY mode)

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Examples:
*     lutview annota
*        Draws an annotated colour table at a position selected via
*        the cursor on the current image-display device.
*     lutview p
*        Draws a colour table that fills the current picture on the
*        current image-display device.
*     lutview curpic
*        Draws a colour table within the current picture positioned
*        via the cursor.
*     lutview mode=xy outline device=ikon \
*        Draws a outlined colour table within the BASE picture
*        on the Ikon, defined by x-y extents.
*     lutview xy lut=my_lut device=lj250_p lbound=[0.92,0.2]
*     ubound=[0.98,0.8]
*        Draws the colour table in the NDF called my_lut with an
*        outline within the BASE picture on the device lj250_p, defined
*        by the x-y bounds (0.92,0.2) and (0.98,0.8).  In other words
*        the plot is to the right-hand side with increasing colour
*        index with increasing y position.

*  Notes:
*     -  When annotations are selected their precise height and the
*     width of the colour table, depend on the largest number of
*     significant figures in an annotation.  The colour table will have 
*     an aspect ratio in the range 0.17--0.45, and the text height is
*     adjusted to fit the characters within the available room.  The
*     default aspect ratio is 0.275. 
*     -  The text has a maximum height as a fraction of width (if
*     oblate) or height of the initial picture (BASE or current) so
*     that ridiculously large characters are drawn for big keys.
*     However, this can result in characters which are too small if for
*     example a highly oblate colour table is plotted within a strongly
*     prolate current picture.
*     -  A FRAME picture (when there are annotations) and the
*     colour-table picture are stored in the graphics database. These
*     have names FRAME and KEY respectively.  On completion the
*     current picture is unchanged.
*     -  Parameters LOW and HIGH are single precision because they are
*     also required to define world co-ordinates of the graphics.  Thus
*     this application is not suitable for double-precision data that
*     have been scaled over a range near the precision of real values.

*  Algorithm:
*     -  Open graphics device, start database activity obtaining the
*     BASE or current picture as requested.
*     -  Get the colour table.
*     -  Check that the device is an image display and will not reset
*     when no LUT has been defined.  Read in the LUT to the colour table
*     if requested.
*     -  Determine mode required (including cursor), whether or not an
*     outline is to be drawn, or annotations plotted.
*     -  If the cursor is being used check it exists and prepare it.
*     -  Select the region and create a zone to contain the key table
*     and determine its shape.
*     -  Create a ramp in workspace of pixel indices, and allowing for
*     the GKS vertical inversion.
*     -  Obtain the values corresponding to the extreme non-reserved
*     pens.
*     -  The following is performed for both shapes of region though
*     the exact formulations are different due to aspect ratios and
*     cell-array orientations.
*       o  Define the text height and the position of the cell array
*       using defaults and then adjust the height so that both a colour 
*       table of an allowed width and annotations are accommodated.  
*       Also constrain the height to a maximum value.
*       o  Plot the cell array in a colour-table zone, draw a box around
*       it and tick marks.
*       o  Adjust the text attributes and write the annotations.
*       o  Store the frame containing the annotations in the database.
*     -  Otherwise no annotations just plot the cell array to fit the
*     region, and an outline if required.
*     -  Store the colour-table picture in the database
*     -  Tidy the workspace, select the input picture, deactivate SGS
*     and cancel AGI device.

*  Related Applications:
*     KAPPA: DISPLAY, LUTABLE; Figaro: COLOUR.

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1990 July 24 (MJC):
*        Original version.
*     1991 April 9 (MJC):
*        Added AGI begin-and-end block.  Added LOW and HIGH parameters
*        and made to display only the non-reserved pens.
*     1991 July 31 (MJC):
*        No longer redefines colours of SGS pens to predefined state if
*        workstation has dynamic colour representation, now there is
*        palette control.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1992 December 1 (MJC):
*        LOW and HIGH default to the current linear scaling limits.
*     1993 January 14 (MJC):
*        Added LUT and NN parameters.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! No implicit typing allowed

*  Global Constants:
      INCLUDE  'SAE_PAR'       ! Global SSE definitions
      INCLUDE  'PAR_ERR'       ! Parameter-system errors
      INCLUDE  'CTM_PAR'       ! Colour-table management constants

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER FOUNT            ! GKS fount for annotation
      PARAMETER ( FOUNT = 4 )

      INTEGER MXLUTE           ! Maximum lookup table entry
      PARAMETER ( MXLUTE = CTM__MXPEN )

      INTEGER NANNO            ! Number of annotations
      PARAMETER ( NANNO = 6 )

      INTEGER NDIM             ! Dimensionality of LUT array
      PARAMETER ( NDIM = 2 )

      INTEGER NPTS             ! Number of x-y points to be measured
                               ! by cursor for defining the picture
      PARAMETER ( NPTS = 2 )

      INTEGER MXCHO            ! Maximum number of choices
      PARAMETER ( MXCHO = 3 )
 
      INTEGER MINCOL           ! Minimum number of colours on device to
                               ! be classed as an image display
      PARAMETER ( MINCOL = 16 )

      INTEGER NPRICL           ! Number of primary colours
      PARAMETER ( NPRICL = 3 )

      REAL BORDER              ! Width of the border at the periphery of
                               ! the frame in which no plotting occurs
      PARAMETER ( BORDER = 0.01 )

      REAL CHMIN, CHMAX        ! Minimum and maximum height positions
                               ! of the colour table within the
                               ! annotation frame
      PARAMETER ( CHMIN = 0.05, CHMAX = 0.95 )

      REAL CTWDEF              ! Default width of the colour table
                               ! within the annotation frame
      PARAMETER ( CTWDEF = 0.25 )

      REAL CTWMIN, CTWMAX      ! Minimum and maximum width positions
                               ! of the colour table within the
                               ! annotation frame
      PARAMETER ( CTWMIN = 0.15, CTWMAX = 0.4 )

      REAL GAP                 ! Gap from the tick to the annotation
      PARAMETER ( GAP = 0.03 )

      REAL MAXHT               ! Maximum text height as a fraction of
                               ! the input picture width or height
                               ! (depending on the colour table's aspect
                               ! ratio
      PARAMETER ( MAXHT = 0.02 )

      REAL TICK                ! Tick mark length
      PARAMETER ( TICK = 0.04 )

      REAL TXTASP              ! Standard text aspect ratio
      PARAMETER ( TXTASP = 0.67 )

*  Local Variables:
      INTEGER
     :  CTDIMS( 1 ),           ! Colour table dimension
     :  CTPTR,                 ! Pointer to colour-table cell array
     :  EL,                    ! Number of elements in the input LUT
     :  I,                     ! Loop counter
     :  IPIXX,                 ! Maximum number of columns of pixels
                               ! of the image display
     :  IPIXY,                 ! Maximum number of lines of pixels
                               ! of the image display
     :  LDIMS( NDIM ),         ! Dimensions of LUT arrays
     :  LPNTR( 1 ),            ! Pointer to input colour table
     :  NC( NANNO ),           ! Number of characters in annotations
     :  NCMAX,                 ! Maximum number of characters in
                               ! annotation
     :  NDFL,                  ! NDF identifier for LUT
     :  NDIMS                  ! Total number of LUT NDF dimensions

      INTEGER
     :  NIMGMS,                ! Number of lines of image-display
                               ! messages
     :  NINTS,                 ! Number of greyscale intensities
                               ! available on the chosen device
     :  NP,                    ! Number of points obtained by the cursor
     :  NTERMS,                ! Number of lines of terminal messages
     :  PICID,                 ! AGI input picture identifier
     :  PICIDB,                ! AGI BASE picture identifier
     :  PICIDC,                ! Picture identifier for colour table
     :  PICIDF,                ! Picture identifier for colour frame
     :  WKID,                  ! GKS workstation identifier
     :  ZONE,                  ! SGS current zone identifier
     :  ZONEB,                 ! SGS zone identifier --- base
     :  ZONECT,                ! SGS zone identifier --- colour table
     :  ZONEF                  ! SGS zone identifier --- FRAME picture
                               ! or colour table when no annotations

      LOGICAL                  ! True if :
     :  ANNOTA,                ! The colour table is annotated
     :  BOX,                   ! A box is to be drawn around the new
                               ! picture
     :  CURPIC,                ! New picture is to lie within the
                               ! current picture
     :  CURSOR,                ! The graphics device has a cursor with
                               ! suitable number of choices
     :  DEVCAN,                ! Image-display parameter is to be
                               ! cancelled
     :  IMGDIS,                ! Device is nominally an image display
     :  LUTIN,                 ! A LUT has been read
     :  NN,                    ! Mapping the input LUT via
                               ! nearest-neighbour method
     :  OBLATE                 ! Region selected for colour table is
                               ! oblate

      CHARACTER
     :  CANNO( NANNO ) *10,    ! Annotations
     :  IMGMES( 4 )*80,        ! Informational messages if device is
                               ! an image display
     :  MODE*7,                ! Mode of determining the position and
                               ! size of the new picture
     :  TERMES( 2 )*80         ! Informational messages if device is
                               ! a terminal

      REAL
     :  ASP,                   ! Physical aspect ratio of the frame
                               ! picture
     :  CTW,                   ! Colour-table width
     :  CTMAX,                 ! Colour-table position, upper bound
     :  CTMIN,                 ! Colour-table position, lower bound
     :  DELTA,                 ! Width of the point markers in cursor
                               ! mode
     :  HEIGHT,                ! Text height
     :  HTIC,                  ! Position of the tick mark
     :  LBND( 2 ),             ! Lower bounds of the new picture
     :  LUT( NPRICL, 0:MXLUTE ), ! Lookup table
     :  OFFSET,                ! Offset to centre the plot within the
                               ! frame
     :  SMAX,                  ! Maximum data value to annotate the
                               ! colour table
     :  SMIN                   ! Minimum data value to annotate the
                               ! colour table

      REAL
     :  SPTICK,                ! Spacing between tick marks
     :  SPVAL,                 ! Spacing between annotation values
     :  TWIDTH,                ! Maximum width of the text
     :  UBND( 2 ),             ! Upper bounds of the new picture
     :  VALUE( NANNO ),        ! Annotation values
     :  X1, X2,                ! x bounds of the new picture in world
                               ! co-ordinates
     :  Y1, Y2,                ! y bounds of the new picture in world
                               ! co-ordinates
     :  X1E, X2E,              ! x bounds of the old picture
     :  XIN, YIN,              ! Co-ordinates of the centre of the image
                               ! picture
     :  XM, YM,                ! size of the current zone
     :  XP( NPTS ), YP( NPTS ),! Co-ordinates obtained via a cursor
     :  Y1E, Y2E               ! y bounds of the old picture

*.

*    Check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

      ANNOTA = .FALSE.
      CURPIC = .FALSE.
      CURSOR = .FALSE.
      DEVCAN = .FALSE.

*    Open GKS workstation to reset device

      CALL AGI_ASSOC( 'DEVICE', 'UPDATE', PICID, STATUS )

*    Start an AGI scope.

      CALL AGI_BEGIN

*    Activate SGS

      CALL AGS_ACTIV( STATUS )

*    If the graphics device was not available, report the error and
*    leave the programme.

      IF ( STATUS .NE. SAI__OK ) THEN

         IF ( STATUS .NE. PAR__ABORT ) THEN
            CALL ERR_REP( 'LUTVIEW_NID',
     :        'LUTVIEW: Graphics device not available or not '/
     :        /'recognised.', STATUS )
         END IF
         DEVCAN = .TRUE.
         GOTO 999
      END IF

*    Limit the colour table plot to the current picture?

      CALL PAR_GTD0L( 'CURPIC', .FALSE., .TRUE., CURPIC, STATUS )

*    Get the mode of operation.

      CALL PAR_CHOIC( 'MODE', 'Cursor', 'Cursor,XY,Picture', .FALSE.,
     :                MODE, STATUS )

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*    Current picture required if the mode is to fill the current
*    picture.

      CURPIC = CURPIC .OR. MODE .EQ. 'PICTURE'

*    Get the required picture.
*    =========================

      IF ( .NOT. CURPIC ) THEN

*       Get the BASE picture.

         CALL AGI_IBASE( PICIDB, STATUS )
         CALL AGI_SELP( PICIDB, STATUS )

*       Get associated zone.

         CALL AGS_NZONE( ZONEB, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'LUTVIEW__NOBAS',
     :        'LUTVIEW: Unable to get the BASE picture/zone from the '/
     :        /'database.', STATUS )

            DEVCAN = .TRUE.
            GOTO 999
         END IF
      ELSE

*       Get zone corresponding to the current picture.

         CALL AGS_NZONE( ZONE, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'LUTVIEW__NCURP',
     :        'LUTVIEW: Unable to get the current zone from the '/
     :        /'database.', STATUS )

            DEVCAN = .TRUE.
            GOTO 999
         END IF

*    End of current-picture-to-be-used check.

      END IF

*    Obtain a lookup table.
*    ======================

*    Start a new error context.

      CALL ERR_MARK

*    Start an NDF context.

      CALL NDF_BEGIN
      
*    Obtain the NDF identifier and pointer of the input lookup table.
*    Validate the LUT.

      LUTIN = .TRUE.
      CALL KPG1_AVLUT( 'LUT', NDFL, LPNTR, EL, STATUS )

*    Obtain the array dimensions.

      CALL NDF_DIM( NDFL, NDIM, LDIMS, NDIMS, STATUS )

*    Null status means do not read a lookup table.

      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         LUTIN = .FALSE.
      END IF

*    At this point we cannot read in the LUT as we do not know the
*    number of colour-table entries.   First we must validate the
*    device.

*    Validate the device.
*    ====================

*    Check whether chosen device is an 'image display' with a suitable
*    minimum number of colour indices, and will not reset when opened
*    unless a colour table has been read in.
      IF ( LUTIN ) THEN
         CALL KPG1_QVID( 'DEVICE', 'SGS', 'IMAGE_DISPLAY,'/
     :                   /'IMAGE_OVERLAY,WINDOW,MATRIX_PRINTER',
     :                   ' ', MINCOL, STATUS )
      ELSE
         CALL KPG1_QVID( 'DEVICE', 'SGS', 'IMAGE_DISPLAY,'/
     :                   /'IMAGE_OVERLAY,WINDOW,MATRIX_PRINTER',
     :                   'RESET', MINCOL, STATUS )
      END IF

*    Obtain the number of colour indices and the maximum display
*    surface.

      CALL KPG1_QIDAT( 'DEVICE', 'SGS', NINTS, IPIXX, IPIXY, STATUS )

*    Abort if the device is not suitable or something has gone wrong
*    associating the device.

      IF ( STATUS .NE. SAI__OK ) THEN

*       The device name is to be cancelled.

         DEVCAN = .TRUE.
         CALL NDF_END( STATUS )
         GOTO 999
      END IF

*    Load the colour table.
      IF ( LUTIN ) THEN

*       Read the LUT.
*       =============

*       Map the lookup table to the colour table by interpolation or by
*       nearest neighbour?

         CALL PAR_GTD0L( 'NN', .FALSE., .TRUE., NN, STATUS )

*       If the structure was found then read in the lookup table.

         CALL KPG1_LUTIN( LDIMS( 2 ), %VAL( LPNTR( 1 ) ),
     :                    NINTS - CTM__RSVPN, NN, LUT, STATUS )

*       Install the lookup table into image-display colour table.
*       =========================================================

*       The lookup table follows the palette, hence the offset in the
*       colour index.

         CALL SGS_ICURW( WKID )
         DO  I = 0, NINTS - 1 - CTM__RSVPN, 1
            CALL GSCR( WKID, I + CTM__RSVPN, LUT( 1, I ), LUT( 2, I ),
     :                 LUT( 3, I ) )
         END DO
      END IF

*    End the NDF context.

      CALL NDF_END( STATUS )

*    End error context.

      CALL ERR_RLSE
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*    Annotate the colour table?

      CALL PAR_GTD0L( 'ANNOTA', .FALSE., .TRUE., ANNOTA, STATUS )

*    Is a rectangular box to be drawn about the new picture?  A box is
*    always drawn when there are annotations.

      IF ( .NOT. ANNOTA ) CALL PAR_GTD0L( 'OUTLINE', .FALSE., .TRUE.,
     :  BOX, STATUS )

      IF ( STATUS .NE. SAI__OK ) GOTO 999

      IF ( MODE .EQ. 'CURSOR' ) THEN

*       Create informational messages.  Dummy for a terminal.

         TERMES( 1 ) = 'Type the spacebar to select a point.'
         TERMES( 2 ) = 'Type . to exit.'
         NTERMS = 2

         IMGMES( 1 ) = 'To select a point press the left button on '/
     :     /'the mouse or trackerball.'
         IMGMES( 2 ) = 'To exit press the right button.'
         IMGMES( 3 ) = 'Select opposite corners to define the '/
     :     /'position and shape of the '
         IMGMES( 4 ) = 'colour table.'
         NIMGMS = 4

*       Prepare the cursor. Specifically, does it exist and have the
*       correct attributes?

         CALL KPG1_PRCUR( 1, TERMES, NTERMS, IMGMES, NIMGMS, '12 .',
     :                    CURSOR, IMGDIS, STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 999

*       Is there no cursor or does it not have any choices?

         IF ( .NOT. CURSOR ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'LUTVIEW_NOCURSOR',
     :        'The workstation does not have a suitable cursor.',
     :        STATUS )

            DEVCAN = .TRUE.
            GOTO 999
         END IF
      END IF

*    Normalise the zone to make the calculations easier below.

      CALL SGS_SW( 0.0, 1.0, 0.0, 1.0, STATUS )

*    Get the picture limits for the centering of the cursor, and more
*    generally, to obtain the limits of the zone in which the key
*    is to be drawn.

      CALL SGS_IZONE( X1E, X2E, Y1E, Y2E, XM, YM )


*    Define and create the region to contain the colour-table key.
*    =============================================================

      IF ( MODE .EQ. 'CURSOR' ) THEN

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

         ELSE
            STATUS = SAI__ERROR
            CALL ERR_REP( 'LUTVIEW__TOOFEW',
     :        'LUTVIEW: Too few points were selected via the cursor.',
     :        STATUS )
            GOTO 999
         END IF

*    XY mode
*    =======

      ELSE IF ( MODE .EQ. 'XY' ) THEN

*       Get limits from the environment.

*       Get limits from the environment.

         CALL CURRE( .FALSE., 'LBOUND', 'UBOUND', LBND, UBND, STATUS )

*       Use the same variables as for other modes.

         X1 = MIN( LBND( 1 ), UBND( 1 ) )
         X2 = MAX( LBND( 1 ), UBND( 1 ) )
         Y1 = MIN( LBND( 2 ), UBND( 2 ) )
         Y2 = MAX( LBND( 2 ), UBND( 2 ) )

*    Current-picture mode
*    ====================

      ELSE IF ( MODE .EQ. 'PICTURE' ) THEN

*       Use the existing picture.

         CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )
      END IF

*    Create the new zone.

      CALL SGS_ZONE( X1, X2, Y1, Y2, ZONEF, STATUS )
      CALL SGS_SW( X1, X2, Y1, Y2, STATUS )

*     Determine the orientation via the shape in absolute size units.
*     The colour table will extend along the larger axis.

      CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )
      OBLATE = XM .GT. YM

*    Create the ramp for the colour table.
*    =====================================

*    There is one element in the per non-reserved pen.

      CTDIMS( 1 ) = NINTS - CTM__RSVPN

*    Create work space for the colour-table display.

      CALL PSX_CALLOC( CTDIMS( 1 ), '_INTEGER', CTPTR, STATUS )

*    Fill the array with element numbers less one to give colour
*    indices.  GKS's convention displays an inverted image compared
*    with the Starlink conventional orientation, therefore the ramp
*    must be inverted for prolate regions.  Omit the reserved pens.

      IF ( OBLATE ) THEN
         CALL ELNMBI( CTM__RSVPN, NINTS-1, CTDIMS( 1 ),
     :                %VAL( CTPTR ), STATUS )
      ELSE
         CALL ELNMBI( NINTS-1, CTM__RSVPN, CTDIMS( 1 ),
     :                %VAL( CTPTR ), STATUS )
      END IF

*    Obtain the values corresponding to the lower and upper colour
*    indices.
*    =============================================================

      CALL ERR_MARK

*    Obtain the values.

      CALL PAR_GET0R( 'LOW', SMIN, STATUS )
      CALL PAR_GET0R( 'HIGH', SMAX, STATUS )

*    Check whether or not either is null.

      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )   

*       Set the data values to the colour-index limits.

         SMIN = REAL( CTM__RSVPN )
         SMAX = REAL( NINTS - 1 )
      END IF

*    Release the error context needed for the error annul.

      CALL ERR_RLSE

*    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      IF ( STATUS .EQ. SAI__OK ) THEN

         IF ( ANNOTA ) THEN

*          Determine the aspect ratio of the plotting region using
*          physical units.

            ASP = XM / YM

*          Normalise the co-ordinate system for plotting annotations
*          and tick marks.

            CALL SGS_SW( 0.0, 1.0, 0.0, 1.0, STATUS )

*          Annotations require a frame in which to draw them.  This is
*          the region defined by the user.  Within this the colour
*          table can be drawn.  Store the frame in the database.

            CALL AGS_SZONE( 'FRAME', 'KAPPA_LUTVIEW', PICIDF, STATUS )

*          Create a border by making a new smaller zone and renormalise
*          the world co-ordinates

            CALL SGS_ZONE( BORDER, 1.0 - BORDER, BORDER, 1.0 - BORDER,
     :                     ZONEF, STATUS )
            CALL SGS_SW( 0.0, 1.0, 0.0, 1.0, STATUS )

*          The annotations are spaced to leave gaps of the text height
*          given a square FRAME picture.  Compute the height that gives 
*          this spacing.  This is the first guess at the height.  It 
*          will be optimised below.

            HEIGHT = ( CHMAX - CHMIN ) / ( REAL( NANNO-1 ) * 2 + 1. )

*          Select the fount.

            CALL SGS_SFONT( FOUNT )

*          Select no text spacing.

            CALL SGS_SSPTX( 0.0 )

*          Find the tick mark spacing.

            SPTICK = ( CHMAX - CHMIN ) / REAL( NANNO - 1 )

*          Find the value spacing.

            SPVAL = ( SMAX - SMIN ) / REAL( NANNO - 1 )

*          Find the maximum width of the annotations.
*          ==========================================

            NCMAX = 0
            DO  I = 1, NANNO

*            Get the value of the Ith annotation.

              VALUE( I ) = SMIN + REAL( I - 1 ) * SPVAL

*            Convert to characters.

              CALL CHR_RTOC( VALUE( I ), CANNO( I ), NC( I ) )

              NCMAX = MAX( NCMAX, NC( I ) )
            END DO

*          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


*          Now to the part where some plotting is performed...

*          Two cases depending on the shape of the region.
*          ===============================================

            IF ( OBLATE ) THEN

*             Define the text height and the position of the cell-array.
*             ==========================================================

*             a) Correct for shape and set a maximum height.
*             
*             Allow for the rectangular shape of the FRAME picture
*             when setting the text height.

               HEIGHT = HEIGHT / ASP /
     :                  ( 1.0 - CTWDEF - GAP - TICK )

*             Constrain the text height to a fraction of the width of
*             the input picture (effectively the height for text
*             orientation) to prevent enormous characters.

               HEIGHT = MIN( HEIGHT, MAXHT * TXTASP *
     :                  ( X2E - X1E ) / ( X2 - X1 ) )

*             Find width allowing given no spacing of the characters
*             and the text aspect ratio. 

               TWIDTH = NCMAX * HEIGHT * ASP

*             b) Find the room left for the cell array.

               CTW = 1.0 - TWIDTH - GAP - TICK

*             c) Adjust the text height if necessary.
*             There are a number of cases: i) the width of the colour
*             table falls between a range of acceptable values; ii) it
*             is smaller than the minimum acceptable; or iii) it is
*             larger than the maximum permitted.  No extra work is
*             required for case i).

*             Case ii) means the text is too big, and so should be
*             scaled to just fit the available space, keeping the
*             colour-table width at its minimum.

               IF ( CTW .LT. CTWMIN ) THEN
                  HEIGHT = ( 1.0 - CTWMIN - GAP - TICK ) / TWIDTH *
     :                     HEIGHT

*                Width of the colour table is the minimum so define
*                the limits of the cell array.

                  CTMIN = 0.0
                  CTMAX = CTWMIN

*                No centring is required.

                  OFFSET = 0.0

*             Case iii) is more tricky.

               ELSE IF ( CTW .GT. CTWMAX ) THEN

*                Do not want to increase the size of the text in case
*                runs into the text below, or simply looks ugly, but
*                mainly because it is too involved.  Instead the text
*                and cell array are centred within the frame.  The
*                width of the colour table is the maximum.  Find the
*                offset to centre the frame.

                  OFFSET = 0.5 * ( CTW - CTWMAX )
                  CTMIN = OFFSET
                  CTMAX = CTWMAX + OFFSET

*              Leave the height and positioning as the default.

                ELSE

*                Width of the colour table is the minimum so define
*                the limits of the cell array.

                  CTMIN = 0.0
                  CTMAX = CTWDEF

*                No centring is required.

                  OFFSET = 0.0
               END IF

*             Define a zone for the colour table itself.

               CALL SGS_ZONE( CHMIN, CHMAX, CTMIN, CTMAX, ZONECT,
     :                        STATUS )

*             Set its window to match the data limits.

               CALL SGS_SW( SMIN, SMAX, CTMIN, CTMAX, STATUS )

               IF ( STATUS .EQ. SAI__OK ) THEN

*                Plot the colour-table cell array.

                  CALL KPG1_GCA( SMIN, CTMIN, SMAX, CTMAX, CTDIMS( 1 ),
     :                           1, CTDIMS( 1 ), 1, %VAL( CTPTR ),
     :                           STATUS )

*                Plot a box around it.

                  CALL SGS_BOX( SMIN, SMAX, CTMIN, CTMAX )
               END IF

*             Return to the frame zone.

               CALL SGS_SELZ( ZONEF, STATUS )

*             Draw the tick marks to the top of the colour table.

               DO  I = 1, NANNO
                  HTIC = CHMIN + REAL( I - 1 ) * SPTICK
                  CALL SGS_LINE( HTIC, CTMAX, HTIC, CTMAX + TICK )
               END DO

*             Orient the text to read from top to bottom.

               CALL SGS_SUPTX( 1.0, 0.0 )

*             Right-centre justify the text.

               CALL SGS_STXJ( 'CR' )

*             Now actually set the text height.

               CALL SGS_SHTX( HEIGHT )

*             Set the aspect ratio of the text to allow for the
*             rectangular shape of the FRAME picture.

               CALL SGS_SARTX( TXTASP * ASP )

*             Write the annotations.
*             ======================

               DO  I = 1, NANNO

*                Get the position of the Ith annotation.

                  HTIC = CHMIN + REAL( I - 1 ) * SPTICK

*                Plot the annotation.

                  CALL SGS_BTEXT( HTIC, CTMAX + TICK + GAP )
                  CALL SGS_ATEXT( CANNO( I )( :NC( I ) ) )
                  CALL SGS_OTEXT
               END DO

*          Prolate region.

            ELSE

*             Define the text height and the position of the cell-array.
*             ==========================================================

*             a) Correct for shape and set a maximum height.
*             
*             Allow for the rectangular shape of the FRAME picture
*             when setting the text height.

               HEIGHT = HEIGHT * ASP / ( 1.0 - CTWDEF - GAP - TICK )

*             Constrain the text height to a fraction of the height of
*             the input picture to prevent enormous characters.

               HEIGHT = MIN( HEIGHT, MAXHT * ( Y2E - Y1E )/( Y2 - Y1 ) )

*             Find width allowing given no spacing of the characters
*             and the text aspect ratio. 

               TWIDTH = NCMAX * HEIGHT / ASP

*             b) Find the room left for the cell array.

               CTW = 1.0 - TWIDTH - GAP - TICK

*             c) Adjust the text height if necessary.
*             There are a number of cases: i) the width of the colour
*             table falls between a range of acceptable values; ii) it
*             is smaller than the minimum acceptable; or iii) it is
*             larger than the maximum permitted.  No extra work is
*             required for case i).

*             Case ii) means the text is too big, and so should be
*             scaled to just fit the available space, keeping the
*             colour-table width at its minimum.

               IF ( CTW .LT. CTWMIN ) THEN
                  HEIGHT = ( 1.0 - CTWMIN - GAP - TICK ) / TWIDTH *
     :                     HEIGHT

*                Width of the colour table is the minimum so define
*                the limits of the cell array.

                  CTMIN = 0.0
                  CTMAX = CTWMIN

*                No centring is required.

                  OFFSET = 0.0

*             Case iii) is more tricky.

               ELSE IF ( CTW .GT. CTWMAX ) THEN

*                Do not want to increase the size of the text in case
*                runs into the text below, or simply looks ugly, but
*                mainly because it is too involved.  Instead the text
*                and cell array are centred within the frame.  The
*                width of the colour table is the maximum.  Find the
*                offset to centre the frame.

                  OFFSET = 0.5 * ( CTW - CTWMAX )
                  CTMIN = OFFSET
                  CTMAX = CTWMAX + OFFSET

*              Leave the height and positioning as the default.

                ELSE

*                Width of the colour table is the minimum so define
*                the limits of the cell array.

                  CTMIN = 0.0
                  CTMAX = CTWDEF

*                No centring is required.

                  OFFSET = 0.0
               END IF

*             Define a zone for the colour table itself.

               CALL SGS_ZONE( CTMIN, CTMAX, CHMIN, CHMAX, ZONECT,
     :                        STATUS )

*             Set its window to match the data limits.

               CALL SGS_SW( CTMIN, CTMAX, SMIN, SMAX, STATUS )

               IF ( STATUS .EQ. SAI__OK ) THEN

*                Plot the colour-table cell array.

                  CALL KPG1_GCA( CTMIN, SMIN, CTMAX, SMAX, 1,
     :                           CTDIMS( 1 ), 1, CTDIMS( 1 ),
     :                           %VAL( CTPTR ), STATUS )

*                Plot a box around it.

                  CALL SGS_BOX( CTMIN, CTMAX, SMIN, SMAX )
               END IF

*             Return to the frame zone.

               CALL SGS_SELZ( ZONEF, STATUS )

*             Draw the tick marks to the right of the colour table.

               DO  I = 1, NANNO
                  HTIC = CHMIN + REAL( I - 1 ) * SPTICK
                  CALL SGS_LINE( CTMAX, HTIC, CTMAX + TICK, HTIC )
               END DO

*             Left-centre justify the text.

               CALL SGS_STXJ( 'CL' )

*             Now actually set the text height.

               CALL SGS_SHTX( HEIGHT )

*             Set the aspect ratio of the text to allow for the
*             rectangular shape of the FRAME picture.

               CALL SGS_SARTX( TXTASP / ASP )

*             Write the annotations.
*             ======================

               DO  I = 1, NANNO

*                Get the position of the Ith annotation.

                  HTIC = CHMIN + REAL( I - 1 ) * SPTICK

*                Plot the annotation.

                  CALL SGS_BTEXT( CTMAX + TICK + GAP, HTIC )
                  CALL SGS_ATEXT( CANNO( I )( :NC( I ) ) )
                  CALL SGS_OTEXT
               END DO
            END IF

*          Return to colour-table zone for storing in the database.

            CALL SGS_SELZ( ZONECT, STATUS )

*       Just new to draw the colour table.

         ELSE

*          Two cases depending on the shape of the region.
*          ===============================================

            IF ( STATUS .EQ. SAI__OK ) THEN
               IF ( OBLATE ) THEN

*                Plot the colour-table cell array.

                  CALL KPG1_GCA( X1, Y1, X2, Y2, CTDIMS( 1 ), 1,
     :                           CTDIMS( 1 ), 1, %VAL( CTPTR ),
     :                           STATUS )

*             Prolate region.

               ELSE

*                Plot the colour-table cell array.

                  CALL KPG1_GCA( X1, Y1, X2, Y2, 1, CTDIMS( 1 ), 1,
     :                           CTDIMS( 1 ), %VAL( CTPTR ), STATUS )

               END IF

*             Plot a box around the colour table if requested.  It has
*             normal line width rather than say a pixel border.

               IF ( BOX ) CALL SGS_BOX( X1, X2, Y1, Y2 )
            END IF
         END IF

*       Store the colour-table zone as a database picture.

         CALL AGS_SZONE( 'KEY', 'KAPPA_LUTVIEW', PICIDC, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'LUTVIEW_DBSP',
     :        'LUTVIEW: Error while storing the new picture in the '/
     :        /'graphics database.', STATUS )
         END IF
      END IF

*    Free the workspace.

      CALL PSX_FREE( CTPTR, STATUS )

 999  CONTINUE

*    AGI closedown sequence.
*    =======================

      CALL AGS_DEASS( 'DEVICE', DEVCAN, STATUS )

      END
