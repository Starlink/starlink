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
*     lutview [mode] [low] [high] [curpic] [device] 

*  Description:
*     This application displays a ramp of colours on the specified image
*     display device using the whole of the current colour table (excluding
*     the low 16 pens which are reserved for axis annotation ,etc). By 
*     default, numerical values are displayed along the long edge of the 
*     ramp. The values corresponding to the maximum and minimum colour 
*     index are supplied using parameters HIGH and LOW. Intermediate colour 
*     indices are labelled with values which are linearly interpolated 
*     between these two extreme values.
*
*     The rectangular area in which the ramp (plus annotations) is drawn
*     may be specified either using a graphics cursor, or by specifying the
*     co-ordinates of two corners using parameters LBOUND and UBOUND.
*     Additionally, there is an option to make the ramp fill the current
*     picture. See parameter MODE. The ramp may be constrained to the
*     current picture using parameter CURPIC.
*
*     The appearance of the annotation my be controlled in detail using
*     the STYLE parameter.

*  ADAM Parameters:
*     CURPIC = _LOGICAL (Read)
*        If CURPIC is TRUE, the colour table key is to lie within the
*        current picture, otherwise the new picture can lie anywhere
*        within the BASE picture.  This parameter ignored if the
*        current-picture mode is selected. [FALSE]
*     DEVICE = DEVICE (Read)
*        The image-display device on which the colour table is to be
*        drawn.  The device must be in one of the following GNS
*        categories: IMAGE_DISPLAY, IMAGE_OVERLAY, MATRIX_PRINTER, or
*        WINDOW, and have at least 24 greyscale intensities or colour
*        indices.  It must also not reset when the device is opened
*        (since the colour table would be lost) unless parameter LUT
*        does not have the null value.  [Current image-display device]
*     FRAME = LITERAL (Read)
*        Specifies the co-ordinate Frame of the positions supplied using 
*        parameters LBOUND and UBOUND. The following Frames will always
*        be available:
*
*        - GRAPHICS -- gives positions in millimetres from the bottom left 
*        corner of the plotting surface.
*
*        - BASEPIC -- gives positions in a normalised system in which the 
*        bottom left corner of the plotting surface is (0,0) and the 
*        shortest dimension of the plotting surface has length 1.0. The
*        scales on the two axes are equal.
*
*        - CURPIC -- gives positions in a normalised system in which the 
*        bottom left corner of the current picture is (0,0) and the 
*        shortest dimension of the current picture has length 1.0. The
*        scales on the two axes are equal.
*
*        There may be additional Frames available, describing previously 
*        displayed data. If a null value is supplied, the current Frame
*        associated with the displayed data (if any) is used. This parameter 
*        is only accessed if parameter MODE is set to "XY". [BASEPIC]
*     HIGH = _REAL (Read)
*        The value corresponding to the maximum colour index.  It is
*        used to calculate the annotation scale for the key.  If it
*        is null (!) the maximum colour index is used.
*        [Current display linear-scaling maximum]
*     LBOUND = LITERAL (Read)
*        Co-ordinates of the lower left corner of the rectangular region
*        containing the colour ramp and annotation, in the co-ordinate 
*        Frame specified by parameter FRAME (supplying a colon ":" will 
*        display details of the selected co-ordinate Frame). The position 
*        should be supplied as a list of formatted axis values separated 
*        by spaces or commas. A null (!) value causes the lower left corner 
*        of the BASE or (if CURPIC is TRUE) current picture to be used.
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
*        rectangular region containing the colour ramp and annotation.
*        The options are:
*
*        - "Cursor" --  The graphics cursor is used to supply two
*        diametrically opposite corners or the region.
*
*        - "XY" -- The parameters LBOUND and UBOUND are used to get the
*        limits.
*
*        - "Picture" -- The whole of the current picture is used.  
*        Additional positioning options are available by using other 
*        KAPPA applications to create new pictures and then specifying 
*        the picture mode. 
*
*        ["Cursor"]
*     NN = _LOGICAL (Read)
*        If NN is TRUE, the input lookup table is mapped to the colour
*        table by using the nearest-neighbour method.  This preserves
*        sharp edges and is better for lookup tables with blocks of
*        colour.  If NN is FALSE, linear interpolation is used, and
*        this is suitable for smoothly varying colour tables.  NN is
*        ignored unless LUT is not null. [FALSE]
*     STYLE = GROUP (Read)
*        A group of attribute settings describing the plotting style to use 
*        for the annotation.
*
*        A comma-separated list of strings should be given in which each
*        string is either an attribute setting, or the name of a text file
*        preceded by an up-arrow character "^". Such text files should
*        contain further comma-separated lists which will be read and 
*        interpreted in the same manner. Attribute settings are applied in 
*        the order in which they occur within the list, with later settings
*        over-riding any earlier settings given for the same attribute.
*
*        Each individual attribute setting should be of the form:
*
*           <name>=<value>
*        
*        where <name> is the name of a plotting attribute, and <value> is
*        the value to assign to the attribute. Default values will be
*        used for any unspecified attributes. All attributes will be
*        defaulted if a null value (!) is supplied. See section "Plotting
*        Attributes" in SUN/95 for a description of the available
*        attributes. Any unrecognised attributes are ignored (no error is
*        reported). 
*       
*        Axis 1 is always the "data value" axis, whether it is displayed 
*        horizontally or vertically. So for instance, to set the label
*        for the data value axis, assign a value to "Label(1)" in the 
*        supplied style. [current value] 
*     UBOUND = LITERAL (Read)
*        Co-ordinates of the upper right corner of the rectangular region
*        containing the colour ramp and annotation, in the co-ordinate 
*        Frame specified by parameter FRAME (supplying a colon ":" will 
*        display details of the selected co-ordinate Frame). The position 
*        should be supplied as a list of formatted axis values separated 
*        by spaces or commas. A null (!) value causes the lower left corner 
*        of the BASE or (if CURPIC is TRUE) current picture to be used.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Examples:
*     lutview 
*        Draws an annotated colour table at a position selected via
*        the cursor on the current image-display device.
*     lutview style="'edge(1)=right,label(1)=Data value in m31'"
*        As above, but the data values are labelled on the right edge of 
*        the box, and the values are labelled with the string "Data value
*        in m31".
*     lutview style='"textlab(1)=0,width(border)=3,colour(border)=white"'
*        No textual label is drawn for the data values, and a thicker than
*        usual white box is drawn around the colour ramp.
*     lutview style="'textlab(1)=0,numlab(1)=0,majticklen(1)=0'"
*        Only the border is drawn around the colour ramp.
*     lutview style="'textlab(1)=0,numlab(1)=0,majticklen(1)=0,border=0'"
*        No annotation at all is drawn.
*     lutview p
*        Draws a colour table that fills the current picture on the
*        current image-display device.
*     lutview curpic
*        Draws a colour table within the current picture positioned
*        via the cursor.
*     lutview xy lut=my_lut device=ps_p lbound=[0.92,0.2] ubound=[0.98,0.8]
*        Draws the colour table in the NDF called my_lut with an
*        outline within the BASE picture on the device ps_p, defined
*        by the x-y bounds (0.92,0.2) and (0.98,0.8).  In other words
*        the plot is to the right-hand side with increasing colour
*        index with increasing y position.

*  Related Applications:
*     KAPPA: DISPLAY, LUTABLE; Figaro: COLOUR.

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-OCT-1999 (DSB):
*        Original AST/PGPLOT version. Based on previous version by MJC.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Global SSE definitions
      INCLUDE 'NDF_PAR'        ! NDF_ constants
      INCLUDE 'AST_PAR'        ! AST_ constants
      INCLUDE 'PAR_ERR'        ! Parameter-system error definitions
      INCLUDE 'CTM_PAR'        ! Colour-table management constants

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER MINCOL           ! Minimum number of colour indices on
                               ! device to be classed as an image
                               ! display
      PARAMETER ( MINCOL = 8 + CTM__RSVPN )

      INTEGER MXLUTE           ! Maximum lookup table entry
      PARAMETER ( MXLUTE = CTM__MXPEN )

      INTEGER NPRICL           ! Number of primary colours
      PARAMETER ( NPRICL = 3 )

*  Local Variables:
      CHARACTER AMES(2)*30     ! Instructions on cursor use
      CHARACTER LABEL*15       ! Label for data axis
      CHARACTER MODE*7         ! How to get KEY Frame size and position
      DOUBLE PRECISION C1( 2 ) ! First corner
      DOUBLE PRECISION C2( 2 ) ! Second corner
      DOUBLE PRECISION CC( 2 ) ! Current Frame co-ords at corner
      DOUBLE PRECISION IN( 2, 2 )! GRAPHICS Frame corners
      DOUBLE PRECISION OUT( 2, NDF__MXDIM )! Current Frame corners
      INTEGER ACT( 2 )         ! Actions used to terminate input
      INTEGER I                ! General variable
      INTEGER IFRM             ! Frame index
      INTEGER INDF             ! NDF identifier for input LUT NDF
      INTEGER IPIC             ! AGI id. for original current picture
      INTEGER IPICB            ! AGI id. for Base picture
      INTEGER IPICK            ! AGI id. for new KEY picture
      INTEGER IPLOT            ! Pointer to AST Plot 
      INTEGER IPLUT            ! Pointer to LUT array
      INTEGER IPWORK           ! Pointer to work space
      INTEGER LDIMS( 2 )       ! Dimensions of input LUT array
      INTEGER LP               ! Lowest pen with which to display the image
      INTEGER NAX              ! No. of current Frame axes
      INTEGER NDIMS            ! Total number of NDF dimensions
      INTEGER NEL              ! Number of elements in the mapped LUT array
      INTEGER NP               ! Number of positions supplied
      INTEGER UP               ! Highest pen with which to display the image
      LOGICAL CURPIC           ! Constrain LUT key to current picture?
      LOGICAL NN               ! Map the LUT via nearest-neighbour method?
      LOGICAL SAME             ! Is the current picture the BASE picture?
      REAL HIGH                ! High data value for LUT key
      REAL LOW                 ! Low data value for LUT key
      REAL LUT( NPRICL, 0:MXLUTE ) ! Lookup table
      REAL RXC( 2 )            ! Single precision X valeus at corners
      REAL RYC( 2 )            ! Single precision Y valeus at corners
      REAL X1, X2, Y1, Y2      ! Bounds of current picture viewport in mm.
*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Open the graphics data base and PGPLOT workstation (without clearing it).
      CALL KPG1_PGOPN( 'DEVICE', 'UPDATE', IPIC, STATUS )

*  Get an AGI identifier for the Base picture.
      CALL AGI_IBASE( IPICB, STATUS ) 

*  Check whether the chosen device is an 'image display' with a suitable
*  minimum number of colour indices, and obtain the number of colour 
*  indices.
      CALL KPG1_PQVID( 'DEVICE', 'IMAGE_DISPLAY,IMAGE_OVERLAY,'//
     :                 'WINDOW,MATRIX_PRINTER', ' ', MINCOL, UP, 
     :                 STATUS )

*  Define the lowest pen number for LUTVIEW of the image.  0 is
*  reserved for the background.  Others are reserved for annotations.
      LP = CTM__RSVPN

*  Get the data values corresponding to the maximum and minimum colour
*  indices. Use defaults of the highest and lowest non-reserved pen indices.
      HIGH = REAL( UP )
      LOW = REAL( LP )
      CALL PAR_DEF0R( 'HIGH', HIGH, STATUS )
      CALL PAR_DEF0R( 'LOW', LOW, STATUS )

      IF( STATUS .EQ. SAI__OK ) THEN
         CALL PAR_GET0R( 'HIGH', HIGH, STATUS )
         CALL PAR_GET0R( 'LOW', LOW, STATUS )
         LABEL = 'data value'

         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )      
            HIGH = REAL( UP )
            LOW = REAL( LP )
            LABEL = 'colour index'
         END IF         
      END IF

*  See how the area to use for the LUT key is to be specified.
      CALL PAR_CHOIC( 'MODE', 'Cursor', 'Cursor,XY,Picture', .FALSE.,
     :                MODE, STATUS )

*  Unless the "Picture" mode has been selected, allow the user to choose
*  whether to confine the key to the current picture or not.
      IF( MODE .NE. 'PICTURE' ) THEN

*  See if the LUT key is to be confined within the current AGI picture.
         CALL PAR_GET0L( 'CURPIC', CURPIC, STATUS )

*  If not, make the Base picture current.
         IF( .NOT. CURPIC ) CALL AGI_SELP( IPICB, STATUS )

      END IF

*  Re-create the PGPLOT viewport for this picture, and obtain an AST
*  pointer to the Plot associated with the picture. World co-ordinates
*  in the viewport will be millimetres from the bottom left of the
*  view surface.
      CALL KPG1_GDGET( -1, AST__NULL, .FALSE., IPLOT, STATUS )

*  Get the bounds of the current picture. The values will be returned in 
*  millimetres from the bottom left corner of the view surface.
      CALL PGQWIN( X1, X2, Y1, Y2 )

*  First handle Cursor mode.
      IF( MODE .EQ. 'CURSOR' ) THEN

*  Store the actions which can be performed using the mouse.
         AMES( 1 ) = 'Select a corner position'
         AMES( 2 ) = 'Exit'

*  Get two corner positions using the cursor, in PGPLOT world co-ordinates. 
*  This corresponds to the Base (i.e. GRAPHICS) Frame of the Plot (millimetres 
*  from the bottom left corner of the view surface). The positions which 
*  may be selected are restricted to the current picture.
         CALL KPG1_PGCUR( .TRUE., 'select two opposite corners', 2, 
     :                   AMES, 'A.', X1, X2, Y1, Y2, 0, 0.5*( X1 + X2 ), 
     :                   0.5*( Y1 + Y2 ), 2, 2, 0, 0, -32, RXC, RYC, 
     :                   ACT, NP, STATUS )

*  Abort if less than two positions were supplied.
         IF( NP .LT. 2 .OR. STATUS .NE. SAI__OK ) GO TO 999

*  Copy the corners to double precision variables. These values are in the 
*  GRAPHICS Frame (millimetres from bottom left corner of view surface).
         C1( 1 ) = DBLE( RXC( 1 ) )
         C1( 2 ) = DBLE( RYC( 1 ) )
         C2( 1 ) = DBLE( RXC( 2 ) )
         C2( 2 ) = DBLE( RYC( 2 ) )

*  Now handle XY mode.
      ELSE IF( MODE .EQ. 'XY' ) THEN

*  Allow the user to specify the Frame in which LBOUND and UBOUND are to
*  be given. Any Frame in the Plot can be chosen. The selected Frame
*  becomes the Current Frame in the Plot.
         CALL MSG_SETC( 'PIC', 'picture' )
         CALL KPG1_ASFRM( 'FRAME', 'EPOCH', IPLOT, ' ', ' ', .TRUE.,
     :                    '^PIC', STATUS )

*  If the CURPIC Frame was selected, and the current picture is the BASE
*  picture, change the Current Frame to BASEPIC for clarity.
         IF( AST_GETC( IPLOT, 'DOMAIN', STATUS ) .EQ. 'CURPIC' ) THEN
            CALL AGI_ISAMP( IPICB, SAME, STATUS ) 
            IF( SAME ) THEN
               CALL KPG1_ASFFR( IPLOT, 'BASEPIC', IFRM, STATUS )
               IF( IFRM .NE. AST__NOFRAME ) THEN
                  CALL AST_SETI( IPLOT, 'CURRENT', IFRM, STATUS )
               END IF
            END IF
         END IF

*  Save the number of axes in the selected Frame.
         NAX = AST_GETI( IPLOT, 'NAXES', STATUS )

*  Transform the corners of the current picture into the Current Frame of
*  the Plot.
         IN( 1, 1 ) = X1
         IN( 1, 2 ) = Y1
         IN( 2, 1 ) = X2
         IN( 2, 2 ) = Y2
         CALL AST_TRANN( IPLOT, 2, 2, 2, IN, .TRUE., NAX, 2, OUT, 
     :                   STATUS )

*  Get the position of a corner, using a corner of the current picture as
*  the default. C1 is returned holding the corresponding GRAPHICS (Base 
*  Frame) position.
         DO I = 1, NAX
            CC( I ) = OUT( 1, I )
         END DO
         CALL KPG1_GTPOS( 'LBOUND', IPLOT, .TRUE., CC, C1, STATUS )

*  Get the position of a second corner, using the other corner of the 
*  current picture as the default. C2 is returned holding the corresponding 
*  GRAPHICS (Base Frame) position.
         DO I = 1, NAX
            CC( I ) = OUT( 2, I )
         END DO
         CALL KPG1_GTPOS( 'UBOUND', IPLOT, .TRUE., CC, C2, STATUS )

*  Now handle Picture mode.
      ELSE

*  Just store the bounds of the current picture.
         C1( 1 ) = DBLE( X1 )
         C1( 2 ) = DBLE( Y1 )
         C2( 1 ) = DBLE( X2 )
         C2( 2 ) = DBLE( Y2 )

      END IF

*  Set the PGPLOT viewport to these bounds.
      CALL  KPG1_PGCUT( REAL( C1( 1 ) ), REAL( C2( 1 ) ), 
     :                  REAL( C1( 2 ) ), REAL( C2( 2 ) ), STATUS )

*  Save the current viewport as a new KEY picture in the graphics database.
      CALL AGP_SVIEW( 'KEY', 'KAPPA_LUTVIEW', IPICK, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain the NDF identifier and pointer of the input lookup table.
*  Validate the LUT.
      CALL KPG1_AVLUT( 'LUT', INDF, IPLUT, NEL, STATUS )

*  Null status means do not read a lookup table.
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

*  Otherwise, obtain the array dimensions.
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
         CALL NDF_DIM( INDF, 2, LDIMS, NDIMS, STATUS )

*  Map the lookup table to the colour table by interpolation or by
*  nearest neighbour?
         CALL PAR_GTD0L( 'NN', .FALSE., .TRUE., NN, STATUS )

*  If the structure was found then read in the lookup table.
         CALL KPG1_LUTIN( LDIMS( 2 ), %VAL( IPLUT ),
     :                    UP - LP + 1, NN, LUT, STATUS )

*  Install the lookup table into image-LUTVIEW colour table.
*  The lookup table follows the palette, hence the offset in the
*  colour index.
         DO  I = 0, UP - LP 
            CALL PGSCR( I + LP, LUT( 1, I ), LUT( 2, I ),
     :                  LUT( 3, I ) )
         END DO

      END IF

*  Get work space to hold the array of colour indices. The number of
*  entries is equal to the number of pens.
      CALL PSX_CALLOC( UP - LP + 1, '_INTEGER', IPWORK, STATUS )

*  Create the LUT key within the KEY Picture.
      CALL KPG1_LUTKY( IPICK, 'STYLE', HIGH, LOW, LABEL, 
     :                 'KAPPA_LUTVIEW', LP, UP, 0.0, %VAL( IPWORK ), 
     :                 STATUS ) 

*  Free the memory.
      CALL PSX_FREE( IPWORK, STATUS )

*  Tidy up
 999  CONTINUE

*  Shutdown PGPLOT and the graphics database.
      CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Add a context report if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'LUTVIEW_ERR', 'LUTVIEW: Failed to display a '//
     :                 'colour table key.',  STATUS )
      END IF

      END
