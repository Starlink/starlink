      SUBROUTINE ARDGEN(STATUS )
*+
*  Name:
*     ARDGEN

*  Purpose:
*     Creates a text file describing selected regions of an image.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ARDGEN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This is an interactive tool for selecting regions of a displayed
*     image using a cursor, and then stores a description of the
*     selected regions in a text file in the form of an `ARD
*     Description' (see SUN/183).  This text file may subsequently be
*     used in conjunction with packages such as CCDPACK or ESP.
*
*     The application initially obtains a value for the SHAPE parameter
*     and then allows you to identify either one or many regions of the
*     specified shape, dependent on the value of parameter STARTUP.
*     When the required regions have been identified, a value is
*     obtained for parameter OPTION, and that value determines what
*     happens next.  Options include obtaining further regions,
*     changing the current region shape, listing the currently defined
*     regions, leaving the application, etc.  Once the selected action
*     has been performed, another value is obtained for OPTION, and
*     this continues until you choose to leave the application.
*
*     Instructions on the use of the cursor are displayed when the
*     application is run.  The points required to define a region of
*     the requested shape are described whenever the current region
*     shape is changed using parameter SHAPE.  Once the points required
*     to define a region have been given an outline of the entire
*     region is drawn on the graphics device using the pen specified by
*     parameter PALNUM.
*
*     In the absence of any other information, subsequent application
*     will use the union (i.e. the logical OR) of all the defined
*     regions.  However, regions can be combined in other ways using the
*     COMBINE option (see parameter OPTION).  For instance, two regions
*     originally defined using the cursor could be replaced by their
*     region of intersection (logical AND), or a single region could be
*     replaced by its own exterior (logical NOT).  Other operators can
*     also be used (see parameter OPERATOR).

*  Usage:
*     ardgen ardout shape option [device] [startup] [palnum] [poicol]
*        { operands=? operator=?
*        { regions=?
*        option
      
*  ADAM Parameters:
*     ARDOUT = FILENAME (Write)
*        Name of the text file in which to store the description of the
*        selected regions.
*     DEVICE = DEVICE (Read)
*        The graphics device on which the regions are to be selected.
*        [Current graphics device]
*     OPERANDS() = _INTEGER (Read)
*        A pair of indices for the regions which are to be combined
*        together using the operator specified by parameter OPERATOR.
*        If the operator is "NOT", then only one region index need be
*        supplied.  Region indices are displayed by the "List" option
*        (see parameter OPTION).
*     OPERATOR = LITERAL (Read)
*        The operator to use when combining two regions into a single
*        region.  The pixels included in the resulting region depend on
*        which of the following operators is selected.
*    
*        "AND" - Pixels are included if they are in both of the regions
*                specified by parameter OPERANDS.
*        "EQV" - Pixels are included if they are in both or neither of
*                the regions specified by parameter OPERANDS.
*        "NOT" - Pixels are included if they are not inside the region
*                specified by parameter OPERANDS.
*        "OR"  - Pixels are included if they are in either of the
*                regions specified by parameter OPERANDS.  Note, an OR
*                operator is implicitly assumed to exist between each
*                pair of adjacent regions unless some other operator is
*                specified.
*        "XOR" - Pixels are included if they are in one, but not both,
*                of the regions specified by parameter OPERANDS.
*
*     OPTION= LITERAL (Read)
*        A value for this parameter is obtained when you choose to end
*        cursor input (by pressing the relevant button as described
*        when the application starts up).  It determines what to do
*        next.  The following options are available:
*
*        "Combine" - Combine two previously defined regions into a
*                    single region using a Boolean operator, or invert
*                    a previously defined region using a Boolean .NOT.
*                    operator.  See parameters OPERANDS and OPERATOR.  The
*                    original regions are deleted and the new combined
*                    (or inverted) region is added to the end of the
*                    list of defined regions.
*        "Delete"  - Delete previously defined regions, see parameter
*                    REGIONS.
*        "Exit"    - Write out the currently defined regions to a text
*                    file and exit the application.
*        "List"    - List the textual descriptions of the currently
*                    defined regions on the screen.  Each region is
*                    described by an index value, a "keyword"
*                    corresponding to the shape, and various arguments
*                    describing the extent and position of the shape.
*                    These arguments are described in the "Notes"
*                    section below.
*        "Multi"   - The cursor is displayed and you can then identify
*                    multiple regions of the current shape, without
*                    being re-prompted for OPTION after each one.  These
*                    regions are added to the end of the list of
*                    currently defined regions.  If the current shape is
*                    "Polygon", "Frame" or "Whole" (see parameter SHAPE)
*                    then multiple regions cannot be defined and the
*                    selected option automatically reverts to "Single".
*        "Single"  - The cursor is displayed and you can then identify a
*                    single region of the current shape.  You are
*                    re-prompted for parameter OPTION once you have
*                    defined the region.  The identified region is
*                    added to the end of the list of currently defined
*                    regions.
*        "Shape"   - Change the shape of the regions created by the
*                    "Single" and "Multi" options.  This causes a new
*                    value for parameter SHAPE to be obtained.
*        "Quit"    - Quit the application without saving the currently
*                    defined regions.
*
*     PALNUM= _INTEGER (Read)
*        The pen number with which to outline the selected regions. [3]
*     POICOL = _INTEGER (Read)
*        The pen number used to mark the cursor positions which define
*        each region.  [2]
*     REGIONS() = LITERAL (Read)
*        The list of regions to be deleted.  Regions are numbered
*        consecutively from 1 and can be listed using the "List" option
*        (see parameter OPTION).  Single regions or a set of adjacent
*        regions may be specified, e.g. assigning [4,6-9,12,14-16] will
*        delete regions 4,6,7,8,9,12,14,15,16.  (Note that the brackets
*        are required to distinguish this array of characters from a
*        single string including commas.  The brackets are unnecessary
*        when there is only one item.)  The numbers need not be in
*        ascending order.
*
*        If you wish to delete all the regions enter the wildcard *.
*        5-* will delete from 5 to the last region.  
*     SHAPE = LITERAL (Read)
*        The shape of the regions to be defined using the cursor.
*        After selecting a new shape, you are immediately requested to
*        identify multiple regions as if "Multi" had been specified for
*        parameter OPTION.  The currently available shapes are listed
*        below.
*
*        "Box"       - A rectangular box with sides parallel to the
*                      image axes, defined by its centre and one of its
*                      corners.
*        "Circle"    - A circle, defined by its centre and radius.
*        "Column"    - A single column of pixels.
*        "Ellipse"   - An ellipse, defined by its centre, one end of
*                      the major axis, and one other point which can be
*                      anywhere on the ellipse.
*        "Frame"     - The whole image excluding a border of constant
*                      width, defined by a single point on the frame. 
*        "Point"     - A single pixel.
*        "Polygon"   - Any general polygonal region, defined by up to
*                      200 vertices.
*        "Rectangle" - A rectangular box with sides parallel to the
*                      image axes, defined by a pair of diagonally
*                      opposite corners.
*        "Rotbox"    - A rotated box, defined by both ends of an edge,
*                      and one point on the opposite edge.
*        "Row"       - A single row of pixels.
*        "Whole"     - The whole of the displayed image.
*
*     STARTUP = LITERAL (Read)
*        Determines if the application starts up in "Multi" or "Single"
*        mode (see parameter OPTION). ["Multi"]
*
*  Examples:
*     ardgen extract.txt circle exit startup=single 
*        This example allows you to create a text file (extract.txt)
*        describing a single circular region of the image displayed on
*        the current graphics device.  The application immediately exits
*        after the region has been identified.  This example may be
*        useful in scripts or command procedures since there is no
*        prompting.

*  Related Applications:
*     KAPPA: ARDMASK; CCDPACK; ESP.
      
*  Notes:
*     -  An image must previously have been displayed on the graphics
*     device.
*     -  The arguments for the textual description of each shape are as
*     follows :
*
*     "Box"       - The co-ordinates of the centre, followed by the
*                   lengths of the two sides.
*     "Circle"    - The co-ordinates of the centre, followed by the
*                   radius.
*     "Column"    - The x co-ordinate of the column.
*     "Ellipse"   - The co-ordinates of the centre, followed by the
*                   lengths of the semi-major and semi-minor axes,
*                   followed by the angle between the x axis and the
*                   semi-major axis (in radians).
*     "Frame"     - The width of the border.
*     "Point"     - The co-ordinates of the pixel.
*     "Polygon"   - The co-ordinates of each vertex in the order given.
*     "Rectangle" - The co-ordinates of two diagonally opposite corners.
*     "Rotbox"    - The co-ordinates of the box centre, followed by the
*                   lengths of the two sides, followed by the angle
*                   between the first side and the x axis (in radians).
*     "Row"       - The y co-ordinate of the row.
*     "Whole"     - No arguments.
*
*      All co-ordinates are pixel, i.e. world co-ordinates. These should
*      not be mistaken with pixel indices. For example, the pixel with
*      indices [1,1] extends from 0.0 to 1.0 in world co-ordinates on both 
*      axes. So the centre of pixel [1,1] is actually at (0.5,0.5). The 
*      interpretation of the supplied positions does not depend on the
*      presence of a decimal point (unlike NDF section specifiers).
      
*  Authors:
*     GJP: Grant Privett (STARLINK)
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:   
*     15-JUL-1994 (GJP)
*        Original version
*     11-NOV-1994 (GJP)
*        Added ARD further keywords.
*     30-NOV-1994 (DSB)
*        Prologue largely re-written.  Re-formatted the code to
*        EDSTAR-style.  Removed redundant checks on STATUS.  KEYWORD
*        parameter renamed SHAPE.  Button assignments changed.  PIXEL
*        removed as a shape option.  OPTION parameter introduced.
*        Re-structured.  Store ARD descriptions internally in a GRP
*        group instead of a text file.  LIST, DELETE, COMBINE and QUIT
*        options added.  STARTUP parameter added.
*     1995 March 15 (MJC):
*        Added commentary, and corrected typographical errors.
*        Standardised the variable declarations and style.  Removed
*        the impersonal "the user".
*     1995 December 16 (MJC):
*        Devices with a mouse can use it instead of the keyboard.
*     30-AUG-1999 (DSB):
*        Do not cancel DEVICE when the graphics system is closed down.
*     {enter_further_changes_here}
      
*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No default typing allowed

*  Global Constants: 
      INCLUDE 'SAE_PAR'          ! SSE constants
      INCLUDE 'DAT_PAR'          ! HDS public constants
      INCLUDE 'PAR_PAR'          ! Parameter system public constants
      INCLUDE 'PAR_ERR'          ! Parameter system error constants
      INCLUDE 'NDF_PAR'          ! NDF system public constants
      INCLUDE 'NDF_ERR'          ! NDF system error constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_ISALF
      LOGICAL CHR_ISALF          ! True if a character is alphabetic

*  Local Constants:
      INTEGER NDIM               ! Array dimensionality
      PARAMETER ( NDIM = 2 )     ! 2-d arrays only

      INTEGER MXPOL              ! Max. no. of vertices in a polygon
      PARAMETER ( MXPOL = 200 )     

*  Local Variables:
      CHARACTER * ( 80 ) CHID( 5 ) ! Commentary on use of image-display
                                 ! cursor
      CHARACTER * ( 80 ) CHTERM( 5 ) ! Commentary on use of terminal cursor
      LOGICAL CURCHO             ! Cursor is available with suitable
                                 ! number of choices
      INTEGER DIMS( NDIM )       ! Dimensions of data
      LOGICAL GOTLOC             ! Locator to the NDF has been obtained?
      INTEGER I                  ! Loop counter
      INTEGER IGRP               ! ID. for GRP group holding ARD desc
      LOGICAL IMGDIS             ! Device is nominally an image display
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of the NDF
      CHARACTER * ( DAT__SZLOC ) LOCI ! Locator for input data structure
      LOGICAL MORE               ! Should another option be obtained?
      INTEGER NDF                ! Input-NDF identifier
      INTEGER NDIMS              ! Actual number of dimensions of the NDF
      INTEGER NP                 ! Number of points obtained by the cursor
      INTEGER NPTS               ! Number of x-y points to be measured
      CHARACTER * ( 7 ) OPTION   ! User selected option
      INTEGER PALNUM             ! Pen number for drawing a region
      INTEGER PICID              ! Input picture identifier
      INTEGER POICOL             ! Pen number for showing defining points
      LOGICAL READY              ! Has current option been performed?
      CHARACTER * ( 256 ) REFNAM ! Reference dataset name from AGI
      LOGICAL REGION             ! Another region to be defined?
      INTEGER SDIM( NDF__MXDIM ) ! Significant dimensions of the NDF
      CHARACTER * ( 128 ) SHAPE  ! Current region shape
      INTEGER SIZE               ! No. of elements in ARD description
      INTEGER SLBND( NDIM )      ! Significant lower bounds of the image
      INTEGER SUBND( NDIM )      ! Significant upper bounds of the image
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of the NDF
      REAL X1,Y1                 ! World co-ordinates of the lower-left
                                 ! corner of the image picture
      REAL X2,Y2                 ! World co-ordinates of the upper-right
                                 ! corner of the image picture
      REAL XIN,YIN               ! Co-ordinates of the centre of the image
                                 ! picture
      REAL XM,YM                 ! Size of the image zone
      REAL XP( MXPOL ), YP( MXPOL ) ! Co-ordinates of the points
                                 ! selected by a cursor.
      INTEGER ZONEO              ! SGS zone of the displayed image
      INTEGER ZONEOV             ! SGS zone of the input picture

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Access the last DATA picture a locator to the associated NDF.
*  =============================================================
*  
*  Associate image display and start database activity.  Update access
*  is used to be able to mark the plot to show the regions defined.
      CALL AGS_ASSOC( 'DEVICE', 'UPDATE', ' ', PICID, ZONEOV, STATUS )

*  Find the last DATA picture.
      CALL KPG1_AGFND( 'DATA', PICID, STATUS )

*  Obtain the SGS zone identifier for the current DATA picture.
      CALL AGS_NZONE( ZONEO, STATUS )

*  Report the name, comment, and label, if one exists, for the current
*  picture.
      CALL MSG_BLANK( STATUS )
      CALL KPG1_AGATC( STATUS )

*  Get a locator to the NDF associated with the DATA picture.
      CALL AGI_GTREF( PICID, 'READ', REFNAM, STATUS )

*  See whether the reference is a name or locator.  The latter should
*  be phased out, but there may be some old databases and software in
*  circulation.
      CALL DAT_VALID( REFNAM, GOTLOC, STATUS )
      IF ( GOTLOC ) LOCI = REFNAM

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Set initial cursor position as the centre of the current
*  picture.
      CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )
      XIN = 0.5*( X1 + X2 )
      YIN = 0.5*( Y1 + Y2 )
      CALL SGS_SETCU( XIN, YIN  )

*  Find the significant dimensions of the NDF.
*  ===========================================

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain an NDF identifier for the NDF.  Either import the NDF by
*  locator or by name.
      IF ( GOTLOC ) THEN
         CALL NDF_FIND( LOCI, ' ', NDF, STATUS )
      ELSE
         CALL NDF_FIND( DAT__ROOT, REFNAM, NDF, STATUS )
      END IF

*  Find whether or not there are but two significant dimensions and
*  which ones they are.
      CALL KPG1_SGDIM( NDF, NDIM, SDIM, STATUS )

*  Display the name of the NDF.
      CALL NDF_MSG( 'NDF', NDF)
      CALL MSG_OUT( 'ARDGEN_MSG1', 'Using NDF ''^NDF''', STATUS )

*  Obtain the bounds of the image. The NDF must not have more than 2
*  dimensions.
      CALL NDF_BOUND( NDF, 2, LBND, UBND, NDIMS, STATUS )

*  The input NDF is no longer required so end the NDF context.
      CALL NDF_END( STATUS )

*  Tidy up the input data structure.
      IF ( GOTLOC ) CALL REF_ANNUL( LOCI, STATUS )
     
*  Determine the image dimensions and the significant bounds.
      DO I = 1, NDIM
         SLBND( I ) = LBND( SDIM( I ) )
         SUBND( I ) = UBND( SDIM( I ) )
         DIMS( I ) = SUBND( I ) - SLBND( I ) + 1
      END DO

*  Obtain the pen numbers.
*  =======================
*  
*  Set the pen required to draw the regions.
      CALL PAR_GET0I( 'PALNUM', PALNUM, STATUS )

*  Set the defining points pen number required.
      CALL PAR_GET0I( 'POICOL', POICOL, STATUS )
 
*  Prepare the cursor.
*  ===================

*  Create some commentary describing how to specify points either with
*  an image display, or with a graphics terminal.
      CHID( 1 ) = 'Instructions for using the cursor...'
      CHID( 2 ) = '   Press left button on mouse/trackerball to ' //
     :            'select a point.'
      CHID( 3 ) = '   Press middle button on mouse/trackerball to ' //
     :            'see current cursor co-ordinates.'
      CHID( 4 ) = '   Press right button on mouse/trackerball to ' //
     :            'end input.'
      CHID( 5 ) = ' '
      
      CHTERM( 1 ) = 'Instructions for using the cursor...'
      CHTERM( 2 ) = '   Press keyboard "1" or space key to select a ' //
     :              'point.'
      CHTERM( 3 ) = '   Press keyboard "." to end input.'
      CHTERM( 4 ) = '   Press keyboard "2" to see the current cursor '//
     :              'co-ordinates.'
      CHTERM( 5 ) = ' '

*  Set up the cursor for use. The number of choices is really only
*  two, though it appears as if there are three.
      CALL KPG1_PRCUR( 2, CHTERM, 5, CHID, 5, '12 .', CURCHO, 
     :                 IMGDIS, STATUS )

*  If there is no cursor it is time to abort.
      IF ( ( STATUS .NE. SAI__OK ) .OR. ( .NOT. CURCHO ) ) THEN

*  Display the message showing what went wrong.   
         IF ( .NOT. CURCHO .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'ARDGEN_ERR1', 'There is no cursor available'/
     :                    /' on the $DEVICE workstation.', STATUS )
         ELSE
            CALL ERR_REP( 'ARDGEN_ERR2', 'Error preparing the cursor.',
     :                     STATUS )
         END IF

*  Exit.
         GO TO 999

      END IF

*  Obtain a group, and initial option and shape.
*  =============================================
*  
*  Create a GRP group in which to store the ARD description. 
      CALL GRP_NEW( 'ARDGEN output', IGRP, STATUS )
      
*  Get the initial region shape to use.
      CALL PAR_CHOIC( 'SHAPE', 'CIRCLE', 'Circle,Box,Point,Row,'/
     :                /'Column,Ellipse,Line,Rectangle,Whole,Frame,'/
     :                /'Rotbox,Polygon', .FALSE., SHAPE, STATUS )

*  Get the start up option.  Flag that the user has not yet performed
*  this option.
      CALL PAR_CHOIC( 'STARTUP', 'Multi', 'Multi,Single', .FALSE., 
     :                OPTION, STATUS )
      READY = .FALSE.

*  Main loop.
*  ==========
      
*  Loop round obtaining options from the user.
      MORE = .TRUE.
      DO WHILE( MORE .AND. STATUS .EQ. SAI__OK )

*  If we are ready for the next action, see what the user wants to do
*  next.  Otherwise, perform the action required by the current
*  contents of the variable "Option".
         IF ( READY ) THEN
            CALL MSG_BLANK( STATUS )
            CALL PAR_CHOIC( 'OPTION', 'SHAPE', 'Multi,Single,Shape,'/
     :                      /'Delete,List,Combine,Exit,Quit', .FALSE., 
     :                      OPTION, STATUS )
            CALL PAR_CANCL( 'OPTION', STATUS )
            READY = .FALSE.
         END IF
         
*  If user wants to define more regions of the current shape...
         IF ( OPTION .EQ. 'SINGLE' .OR. 
     :        OPTION .EQ. 'MULTI' ) THEN          

*  Tell the user how to specify the region, and also find out how many
*  positions are needed.
            CALL MSG_BLANK( STATUS )
            CALL KPS1_AGNMS( SHAPE, MXPOL, NPTS, STATUS )

*  If the user asked to give multiple regions, change to SINGLE if the
*  current shape is POLYGON (because the "." button is used to mark the
*  end of a single polygon and so can't also be used to mark the end of
*  a series of multiple polygons), or WHOLE or FRAME (because users
*  won't want to give multiple regions of these types).
            IF ( OPTION .EQ. 'MULTI' .AND. (
     :           SHAPE .EQ. 'POLYGON' .OR.
     :           SHAPE .EQ. 'WHOLE' .OR.
     :           SHAPE .EQ. 'FRAME' ) ) OPTION = 'SINGLE'

*  Loop round until all regions have been given.  Only loop once if
*  SINGLE was selected for parameter OPTION.
            REGION = .TRUE.
            DO WHILE ( REGION .AND. STATUS .EQ. SAI__OK )

*  Initialise the number of cursor positions obtained.
               NP = 0

*  Obtain the required number of positions via the graphics cursor.  An 
*  exact number of points is required.  Marked points will be erased. 
*  Points should be unconnected, and distinct.
               CALL KPS1_AGNCP( PALNUM, POICOL, MXPOL, NPTS, SHAPE, 
     :                          X1, X2, Y1, Y2, XIN, YIN, NP, XP, YP, 
     :                          STATUS )

*  If the required number of positions were obtained (or at least 1
*  position was obtained in the case of shapes with variable number
*  of defining positions), store the ARD description for the region in
*  the GRP group.
               IF ( ( NP .EQ. NPTS ) .OR.
     :              ( NPTS .LT. 0 .AND. NP .GT. 0 ) ) THEN

                  CALL KPS1_AGNST( NP, SHAPE, IGRP, X1, X2, Y1,
     :                             Y2, XP, YP, STATUS )
               
*  Save the last cursor position.   
                  XIN = XP( NP )
                  YIN = YP( NP )

*  Issue a warning if sufficient positions were not obtained. Only do
*  this if the user is giving a single region.  Otherwise take it as an
*  indication that the user doesn't want to give any more regions.
               ELSE IF ( OPTION .EQ. 'SINGLE' ) THEN
                  CALL MSG_SETC( 'SH', SHAPE )
                  CALL MSG_SETI( 'NPTS', NPTS )
                  CALL MSG_OUT( 'ARDGEN_MSG', '^NPTS positions '/
     :              /'required to define a "^SH" region!', STATUS )
               END IF

*  If the user only wanted to give one region, or if the user gave an
*  incomplete region, leave the loop.
               IF ( ( OPTION .EQ. 'SINGLE' ) .OR. 
     :              ( NP .NE. NPTS ) ) THEN
                  REGION = .FALSE.

*  If another region is to be defined, tell the user.
               ELSE                
                  CALL MSG_SETC( 'SH', SHAPE )
                  CALL MSG_OUT( 'ARDGEN_MSG', 'Region completed. '/
     :              /'Identify another ''^SH'' region...', STATUS )
               END IF

            END DO

*  Indicate that we are ready for a new option.
            READY = .TRUE.

*  If user wants to change the current shape...
         ELSE IF ( OPTION .EQ. 'SHAPE' ) THEN          

*  Cancel the SHAPE parameter value.
            CALL PAR_CANCL( 'SHAPE', STATUS )            
            
*  Get a new value for the SHAPE parameter.
            CALL ERR_MARK
            CALL PAR_CHOIC( 'SHAPE', SHAPE, 'Circle,Box,Point,Row,'/
     :                      /'Column,Ellipse,Line,Rectangle,Whole,'/
     :                      /'Frame,Rotbox,Polygon,Quit', .FALSE.,
     :                      SHAPE, STATUS )
            IF ( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )
            CALL ERR_RLSE
            

*  The user must now identify multiple regions.  Change the value of
*  OPTION and leave the READY flag cleared so that the user will not be
*  re-prompted for 'OPTION'.
            OPTION = 'MULTI'

*  If user wants to delete an identified region...
         ELSE IF ( OPTION .EQ. 'DELETE' ) THEN          
            CALL KPS1_AGNDL( 'REGIONS', IGRP, STATUS )
            READY = .TRUE.
            
*  If user wants to list the currently defined region...
         ELSE IF ( OPTION .EQ. 'LIST' ) THEN          
            CALL KPS1_AGNLS( IGRP, STATUS )
            READY = .TRUE.

*  If user wants to list the combined regions together...
         ELSE IF ( OPTION .EQ. 'COMBINE' ) THEN
            CALL ERR_MARK
            CALL KPS1_AGNCM( 'OPERATOR', 'OPERANDS', IGRP, STATUS )
            IF ( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )
            CALL ERR_RLSE
            READY = .TRUE.
            
*  If user wants to exit the program...
         ELSE IF ( OPTION .EQ. 'EXIT' ) THEN          
            MORE = .FALSE.

*  If user wants to quit the program, throwing away the current
*  regions...
         ELSE IF ( OPTION .EQ. 'QUIT' ) THEN          
            CALL GRP_SETSZ( IGRP, 0, STATUS )
            MORE = .FALSE.

         END IF         

      END DO

*  Write the ARD file.
*  ===================

*  If the ARD description is not of zero size, write it out to a text
*  file, and then delete the group.  Otherwise, issue a warning
*  message.
      CALL GRP_GRPSZ( IGRP, SIZE, STATUS )
      IF ( SIZE .GT. 0 ) THEN
         CALL GRP_LIST( 'ARDOUT', 0, 0, '   ARD description generated '/
     :                  /'by ARDGEN', IGRP, STATUS )

      ELSE
         CALL MSG_BLANK( STATUS )
         CALL MSG_OUT( 'ARDGEN_MSG', 'No output file has been created.',
     :                 STATUS )

      END IF
      CALL MSG_BLANK( STATUS )

*   CLosedown sequence.
*   ===================
      CALL GRP_DELET( IGRP, STATUS )
         
 999  CONTINUE

*   Close down the graphics database before exiting.
      CALL AGS_DEASS( 'DEVICE', .FALSE., STATUS )

      END

