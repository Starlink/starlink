      SUBROUTINE LISTSHOW( STATUS )
*+
*  Name:
*     LISTSHOW

*  Purpose:
*     Reports the positions stored in a positions list.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL LISTSHOW( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application reports positions contained in a catalogue. The
*     catalogue should have the form of a positions list as produced (for
*     instance) by applications LISTMAKE and CURSOR. By default all positions 
*     in the catalogue are reported, but a subset may be reported by 
*     specifying a range of "position identifiers" (see parameters FIRST 
*     and LAST). 
*
*     Positions may be reported in a range of co-ordinate Frames dependant 
*     on the information stored in the supplied positions list (see parameter 
*     FRAME). The selected positions are written to an output parameter 
*     (parameter POSNS), and may also be written to an output positions
*     list (see parameter OUTCAT). The formatted screen output can be saved 
*     in a logfile (see parameter LOGFILE). The formats used to report the 
*     axis values can be controlled using parameter STYLE.
*
*     Graphics may also be drawn marking the selected positions (see
*     parameters PLOT and LABEL). If a DATA picture can be found on the 
*     specified graphics device, the supplied positions are aligned with the 
*     DATA picture. If no DATA picture is found, positions are aligned with 
*     the current picture. If possible, this alignment occurs within the 
*     co-ordinate Frame specified using parameter FRAME. If this is not 
*     possible, alignment may occur in some other suitable Frame. A message 
*     is displayed indicating the Frame in which alignment occurred.

*  Usage:
*     listshow incat [frame] [first] [last] [plot] [device]

*  ADAM Parameters:
*     CLOSE = LOGICAL (Read)
*        This parameter is only accessed if parameter PLOT is set to
*        "Chain" or "Poly". If TRUE, polgons will be closed by joining 
*        the first position to the last position. [Current value]
*     DESCRIBE = LOGICAL (Read)
*        If TRUE, a detailed description of the co-ordinate Frame in which 
*        the positions will be reported is displayed before the 
*        positions. [Current value]
*     DEVICE = DEVICE (Read)
*        The graphics workstation.  Only accessed if parameter PLOT
*        indicates that graphics are required. [The current graphics device]
*     DIM = _INTEGER (Write)
*        The number of axes for each position written to output parameter 
*        POSNS.
*     EPOCH = DOUBLE PRECISION (Read)
*        If an IRAS90 Sky Co-ordinate System specification is supplied
*        (using parameter FRAME) for a celestial co-ordinate system, 
*        then an epoch value is needed to qualify it. This is the epoch at 
*        which the supplied sky positions were determined. It should be
*        given as a decimal years value, with or without decimal places 
*        ("1996.8" for example). Such values are interpreted as a Besselian 
*        epoch if less than 1984.0 and as a Julian epoch otherwise. 
*     FIRST = INTEGER (Read)
*        The identifier for the first position to be displayed. Positions
*        are only displayed which have identifiers in the range given by
*        parameters FIRST and LAST. The dynamic default is the lowest
*        identifier value in the positions list. []
*     FRAME = LITERAL (Read)
*        A string determining the co-ordinate Frame in which positions are 
*        to be reported. This application can report positions in
*        any of the co-ordinate Frames stored with the positions list. The
*        string supplied for FRAME can be one of the following:
*
*        - A Domain name such as SKY, AXIS, PIXEL, etc. 
*
*        - An integer value giving the index of the required Frame.
*
*        - An IRAS90 Sky Co-ordinate System (SCS) values such as 
*        EQUAT(J2000) (see SUN/163).
*
*        If a null value (!) is supplied, positions are reported in the 
*        co-ordinate Frame which was current when the positions list was 
*        created. The user is re-prompted if the specified Frame is not
*        available within the positions list. [!]
*     GEODESIC = LOGICAL (Read)
*        This parameter is only accessed if parameter PLOT is set to
*        "Chain" or "Poly". It specifies whether the curves drawn between
*        positions should be stright lines, or should be geodesic curves.
*        In many co-ordinate Frames geodesic curves will be simple straight 
*        lines. However, in others (such as the majority of celestial 
*        co-ordinates Frames) geodesic curves will be more complex curves 
*        tracing the shortest path between two positions in a non-linear 
*        projection. [FALSE]
*     INCAT = FILENAME (Read)
*        A catalogue containing a positions list such as produced by 
*        applications LISTMAKE, CURSOR, etc. 
*     LABEL = LOGICAL (Read)
*        If TRUE the positions are labelled with the corresponding integer
*        identifiers on the graphics device specified by parameter DEVICE.
*        The offset from each position to the centre of the corresponding 
*        label is controlled using the "NumLabGap(1)" and "NumLabGap(2)"
*        plotting attributes, and the appearance of the labels is
*        controlled using attributes "Colour(NumLab)", "Size(NumLab)", etc.
*        These attributes may be specified using parameter STYLE. [FALSE]
*     LAST = INTEGER (Read)
*        The identifier for the last position to be displayed. Positions
*        are only displayed which have identifiers in the range given by
*        parameters FIRST and LAST. parameters FIRST and LAST. The dynamic 
*        default is the highest identifier value in the positions list. []
*     LOGFILE = FILENAME (Write)
*        The name of the text file in which the formatted co-ordinates of 
*        the selected positions may be stored. This is intended primarily 
*        for recording the screen output, and not for communicating 
*        positions to subsequent applications. A null string (!) means that 
*        no file is created.  [!]
*     MARKER = INTEGER (Read)
*        This parameter is only accessed if parameter PLOT is set to
*        "Chain" or "Mark". It specifies the symbol with which each
*        position should be marked, and should be given as an integer 
*        PGPLOT marker type. For instance, 0 gives a box, 1 gives a dot, 
*        2 gives a cross, 3 gives an asterisk, 7 gives a triangle. The 
*        value must be larger than or equal to -31. [current value]
*     NUMBER = _INTEGER (Write)
*        The number of positions selected.
*     OUTCAT = FILENAME (Write)
*        The output catalogue in which to store the selected positions. If a 
*        null value is supplied, no output catalogue is produced. [!]
*     PLOT = LITERAL (Read)
*        The type of graphics to be used to mark the positions on the
*        graphics device specified by parameter DEVICE. The appearance of 
*        these graphics (colour, size, etc ) is controlled by the STYLE 
*        parameter. PLOT can take any of the following values:
*
*        - "None" -- No graphics are produced.
*       
*        - "Mark" -- Each position is marked by the symbol specified
*        by parameter MARKER.
*
*        - "Poly" -- Causes each position to be joined by a line to the 
*        previous position.  These lines may be simple straight lines or
*        geodesic curves (see parameter GEODESIC). The polygons may
*        optionally be closed by joining the last position to the first (see
*        parameter CLOSE).
*
*        - "Chain" -- This is a combination of "Mark" and "Poly". Each 
*        position is marked by a symbol and joined by a line to the previous 
*        position. Parameters MARKER, GEODESIC and CLOSE are used to
*        specify the symbols and lines to use.
*
*        - "Box" -- A rectangular box with edges parallel to the edges of
*        the graphics device is drawn between each pair of positions.
*
*        - "Vline" -- A vertial line is drawn through each position, 
*        extending the entire height of the selected picture.
*
*        - "Hline" -- A horizontal line is drawn through each position, 
*        extending the entire width of the selected picture.
*
*        - "Cross" -- A combination of "Vline" and "Hline".
*
*        - "Text" -- A text string is used to mark each position. The string 
*        is drawn horizontally and is centred on the specified position.
*        The strings to use for each position are specified using parameter 
*        STRINGS.
*
*        Each position may also be separately labelled with its integer 
*        identifier value by giving a TRUE value for parameter LABEL. [None]
*     POSNS() = _DOUBLE (Write)
*        The unformatted co-ordinates of the positions selected by
*        parameters FIRST and LAST, in the co-ordinate Frame selected by
*        FRAME. The axis values are stored as a 1-dimensional vector. All
*        the axis 1 values for the selected positions are stored first, 
*        followed by the axis 2 values, etc. The number of positions in 
*        the vector is written to the output parameter NUMBER, and the
*        number of axes per position is written to the output parameter 
*        DIM. The axis values may not be in the same units as the 
*        formatted values shown on the screen. For instance, unformatted 
*        celestial co-ordinate values are stored in units of radians. 
*     QUIET = LOGICAL (Read)
*        If TRUE then nothing is displayed on the screen while the
*        application is running (other than error messages). Output 
*        parameters and files are still created. The dynamic default is
*        to run quietly if any graphics or labels are being plotted (see
*        parameters PLOT and LABEL). []
*     STRINGS = LITERAL (Read)
*        A group of text strings which are used to mark the supplied positions 
*        if parameter PLOT is set to "TEXT". The first string in the
*        group is used to mark the first position, the second string is
*        used to mark the second position, etc (note, these integers may
*        be different to the position identifiers in the supplied positions 
*        list - if you want to see the position identifiers, use parameter 
*        LABEL). If more positions are given than there are strings in the 
*        group, then the extra positions will be marked with an integer value 
*        indicating the index within the list of supplied positions. If a 
*        null value (!) is given for the parameter, then all positions will 
*        be marked with integer indices, starting at 1.
*
*        A comma-separated list should be given in which each element is
*        either a marker string, or the name of a text file preceded by an 
*        up-arrow character "^". Such text files should contain further 
*        comma-separated lists which will be read and interpreted in the 
*        same manner. Note, strings within text files can be separated by
*        new lines as well as commas.
*     STYLE = LITERAL (Read)
*        The name of a text file containing a description of a plotting 
*        style. It is used only to control the labels and format for the 
*        displayed axis values, by specifying suitable values for Frame 
*        attributes (eg  Digits(1), Digits(2), Symbol(1), Symbol(2), etc).
*
*        Each line in the file should contain a string of the form 
*        <name>=<value>, in which <name> is the name of an attribute, 
*        and <value> is the value to assign to the attribute. The file may 
*        contain blank lines and comment lines starting with a hash (#) sign. 
*        Default values will be used for any unspecified attributes. All
*        attributes will be defaulted if a null value (!) is supplied.
*        [Current value]

*  Examples:
*     listshow stars pixel
*        This displays the pixel co-ordinates of all the positions
*        stored in the FITS binary catalogue stars.fit. They are all written 
*        to the output parameter POSNS as well as to the screen.
*     listshow stars.fit equat(J2010) first=3 last=3 quiet
*        This extracts position 3 from the catalogue stars.fit transforming
*        it into FK5 equatorial RA/DEC co-ordinates (referenced to the
*        J2010 equinox) if possible. The RA/DEC values (in radians) are
*        written to the output parameter POSNS, but not to the screen.
*     listshow stars_2.txt style="digits(1)=5,digits(2)=7"
*        This lists the positions in the STL format catalogue contained
*        in text file stars_2.txt in their original co-ordinate Frame. By 
*        default, 5 digits are used to format axis 1 values, and 7 to format 
*        axis 2 values. These defaults are over-ridden if the attributes 
*        Format(1) and/or Format(2) are assigned values in the description 
*        of the current Frame stored in the positions list.
*     listshow stars_2.txt plot=3 style="colour=red,size=2"
*        This lists the positions in stars_2.txt on the screen in their 
*        original co-ordinate Frame, and also marks them on the currently 
*        selected graphics device using PGPLOT marker 3 (an asterisk). The 
*        markers are red and are twice the default size. 

*  Notes:
*     -  This application uses the conventions of the CURSA package (SUN/190)
*     for determining the formats of input and output catalogues. If a file 
*     type of .fits is given, then the catalogue is assumed to be a FITS 
*     binary table. If a file type of .txt is given, then the catalogue is 
*     assumed to be stored in a text file in "Small Text List" (STL) format. 
*     If no file type is given, then ".fit" is assumed. 

*  Related Applications:
*     KAPPA: CURSOR, LISTMAKE; CURSA: XCATVIEW, CATSELECT

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-SEP-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER PLOT*15          ! Nature of required graphics
      CHARACTER TEXT*(GRP__SZNAM)! Text buffer
      CHARACTER TITLE*80         ! Title from positions list
      INTEGER BFRM               ! Copy of original Frame 
      INTEGER F                  ! Index of first non-blank character
      INTEGER FIRST              ! Lowest position identifier to display
      INTEGER FSTPOS             ! Position of lowest identifier
      INTEGER I                  ! Loop count
      INTEGER IBASE              ! Index of Base Frame in IWCS
      INTEGER IFRM               ! Index of Frame  for positions 
      INTEGER IGRP1              ! GRP identifier for formatted co-ordinate values
      INTEGER IGRP2              ! GRP identifier for marker strings group
      INTEGER IMARK              ! PGPLOT marker type
      INTEGER IPIC               ! AGI id for selected picture
      INTEGER IPIC0              ! Current (input) picture identifier
      INTEGER IPID               ! Pointer to original identifiers
      INTEGER IPLOT              ! AST pointer to Plot 
      INTEGER IPPOS              ! Pointer to original positions
      INTEGER IPW0               ! Pointer to array holding selected identifiers 
      INTEGER IPW1               ! Pointer to array holding selected positions
      INTEGER IPW2               ! Pointer to array holding selected positions
      INTEGER IPW3               ! Pointer to array holding GRAPHICS positions
      INTEGER IWCS               ! AST pointer to FrameSet
      INTEGER L                  ! Length of a string
      INTEGER LAST               ! Highest position identifier to display
      INTEGER LSTPOS             ! Position of highest identifier
      INTEGER MAP                ! AST pointer to original -> requested mapping
      INTEGER NBAX               ! No. of axes in original Frame
      INTEGER NDISP              ! Number of selected positions
      INTEGER NINVAL             ! No. of invalid identifiers
      INTEGER NPOS               ! Total number of positions
      INTEGER NRAX               ! No. of axes in requested Frame
      INTEGER NSTR               ! No. of marker strings supplied
      INTEGER SIZE               ! No. of elements in group
      LOGICAL CLOSE              ! Close the polygon?
      LOGICAL DESC               ! Describe each Coordinate Frame?
      LOGICAL GEO                ! Draw geodesic polygon?
      LOGICAL LABEL              ! Label positions on graphics device?
      LOGICAL QUIET              ! Run quietly?

*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Initialise resource identifiers so that they are not inappropriately
*  released during the shutdown procedure.
      IPW0 = 0
      IPW1 = 0
      IPW2 = 0
      IGRP1 = GRP__NOID
      IGRP2 = GRP__NOID

*  Start a new AST context.
      CALL AST_BEGIN( STATUS )

*  See what type of graphics are required. 
      CALL PAR_CHOIC( 'PLOT', 'None', 'Poly,Mark,Chain,Box,None,'//
     :                'Vline,Hline,Cross,Text', .TRUE., PLOT, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the PGPLOT marker type for CHAIN and MARKER graphics.
      IF( PLOT .EQ. 'MARK' .OR. PLOT .EQ. 'CHAIN' ) THEN
         CALL PAR_GDR0I( 'MARKER', 2, -31, 10000, .FALSE., IMARK, 
     :                   STATUS )
      END IF

*  For POLY and CHAIN graphics, see if the polygons or chains should be
*  closed, and whether the lines segments should be geodesic curves.
      IF( PLOT .EQ. 'POLY' .OR. PLOT .EQ. 'CHAIN' ) THEN
         CALL PAR_GET0L( 'CLOSE', CLOSE, STATUS )
         CALL PAR_GET0L( 'GEODESIC', GEO, STATUS )
      END IF

*  For text graphics, get a group of strings to be displayed. If a null 
*  value is supplied, annul the error and store a null GRP identifier to
*  indicate that integers starting at 1 should be used to mark each position.
      IF( PLOT .EQ. 'TEXT' .AND. STATUS .EQ. SAI__OK ) THEN
         CALL KPG1_GTGRP( 'STRINGS', IGRP2, NSTR, STATUS )
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            IGRP2 = GRP__NOID
         END IF
      END IF

*  See if positions are to be labelled on the graphics device.
      CALL PAR_GET0L( 'LABEL', LABEL, STATUS )

*  Set the dynamic default for QUIET.
      IF( PLOT .NE. 'NONE' .OR. LABEL ) THEN
         CALL PAR_DEF0L( 'QUIET', .TRUE., STATUS )
      ELSE
         CALL PAR_DEF0L( 'QUIET', .FALSE., STATUS )
      END IF

*  See if we are to supress display all information on the screen.
      CALL PAR_GET0L( 'QUIET', QUIET, STATUS )

*  Get the input positions list. A pointer to a FrameSet is returned,
*  together with pointers to positions and identifiers, and a title.
*  The positions are returned in the Base Frame of this FrameSet.
      IWCS = AST__NULL
      CALL KPG1_RDLST( 'INCAT', .FALSE., IWCS, NPOS, NBAX, IPPOS, IPID,
     :                 TITLE, STATUS )

*  Give a suitable message, store zero for NUMBER, and abort if the file 
*  was empty.
      IF( NPOS .EQ. 0 ) THEN

         IF( .NOT. QUIET ) THEN
            CALL MSG_BLANK( STATUS )
            CALL MSG_OUT( 'LISTSHOW_MSG1', '  The supplied positions '//
     :                    'list is empty.', STATUS )
            CALL MSG_BLANK( STATUS )
         END IF

         CALL PAR_PUT0I( 'NUMBER', 0, STATUS )
         GO TO 999

      END IF

*  Take a copy of the Base Frame in which the positions are defined.
      BFRM = AST_COPY( AST_GETFRAME( IWCS, AST__BASE, STATUS ), STATUS )

*  Set the Current Frame in the FrameSet to the requested Frame. If the
*  requested Frame is not available in the positions list, re-prompt for 
*  a new FRAME until an available Frame is obtained. Note, if the Current
*  Frame is a SKY Frame, this call will change the Frame if the user requests
*  a different sky co-ordinate system. For this reason, we took a copy of
*  the Current Frame above, rather than just saving its index.
      CALL KPG1_ASFRM( 'FRAME', 'EPOCH', IWCS, 'PIXEL', 'AXIS', 
     :                 .TRUE., STATUS )

*  Get the number of axes in the requested Frame.
      NRAX = AST_GETI( IWCS, 'NAXES', STATUS )

*  Get the Mapping which produces positions in the required Frame. 
      MAP = AST_SIMPLIFY( AST_GETMAPPING( IWCS, AST__BASE, AST__CURRENT,
     :                                    STATUS ), STATUS )

*  Abort if an error occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Find the maximum and minimum position identifiers.
      CALL KPG1_MXMNI( .FALSE., NPOS, %VAL( IPID ), NINVAL, LAST, 
     :                 FIRST, LSTPOS, FSTPOS, STATUS )

*  Get the first position identifier to be displayed. Use a dynamic
*  default equal to the lowest identifier in the positions list.
      CALL PAR_GDR0I( 'FIRST', FIRST, FIRST, LAST, .FALSE., FIRST, 
     :                STATUS )

*  Get the last position identifier to be displayed. Use a dynamic
*  default equal to the highest identifier in the positions list.
      CALL PAR_GDR0I( 'LAST', LAST, FIRST, LAST, .FALSE., LAST, 
     :                STATUS )

*  Count the number of positions within the supplied range of position
*  identifiers.
      CALL KPS1_LSHCT( NPOS, %VAL( IPID ), FIRST, LAST, NDISP, STATUS )

*  Give a suitable message, store zero for NUMBER, and abort if no
*  positions were selected.
      IF( NDISP .EQ. 0 ) THEN

         IF( .NOT. QUIET ) THEN
            CALL MSG_BLANK( STATUS )
            CALL MSG_OUT( 'LISTSHOW_MSG2', '  No positions have been '//
     :                    'selected.', STATUS )
            CALL MSG_BLANK( STATUS )
         END IF
 
         CALL PAR_PUT0I( 'NUMBER', 0, STATUS )
         GO TO 999
 
      END IF

*  Allocate a work array to hold the identifiers for the selected positions.
      CALL PSX_CALLOC( NDISP, '_INTEGER', IPW0, STATUS )

*  Allocate a work array to hold the selected positions in the original
*  Frame.
      CALL PSX_CALLOC( NBAX*NDISP, '_DOUBLE', IPW1, STATUS )

*  Allocate a work array to hold the selected positions in the requested 
*  Frame.
      CALL PSX_CALLOC( NRAX*NDISP, '_DOUBLE', IPW2, STATUS )

*  Abort if an error occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Copy the selected positions and identifiers into the work arrays.
      CALL KPS1_LSHCP( FIRST, LAST, NPOS, NBAX, %VAL( IPPOS ), 
     :                 %VAL( IPID ), NDISP, %VAL( IPW0 ), %VAL( IPW1 ),
     :                 STATUS )

*  Map the positions from the original Frame into the requested Frame.
      CALL AST_TRANN( MAP, NDISP, NBAX, NDISP, %VAL( IPW1 ), .TRUE.,
     :                NRAX, NDISP, %VAL( IPW2 ), STATUS ) 

*  Create an output positions list if required.
      IF( TITLE .EQ. ' ' ) THEN
         CALL KPG1_WRLST( 'OUTCAT', NDISP, NDISP, NBAX, %VAL( IPW1 ),
     :                    AST__BASE, IWCS, 'Output from LISTSHOW', 
     :                    0, %VAL( IPW0 ), .TRUE., STATUS )
      ELSE
         CALL KPG1_WRLST( 'OUTCAT', NDISP, NDISP, NBAX, %VAL( IPW1 ),
     :                    AST__BASE, IWCS, TITLE, 
     :                    0, %VAL( IPW0 ), .TRUE., STATUS )
      END IF

*  Write the mapped values to the output parameter.
      CALL PAR_PUT1D( 'POSNS', NDISP*NRAX, %VAL( IPW2 ), STATUS )

*  Write the number of selected positions to NUMBER.
      CALL PAR_PUT0I( 'NUMBER', NDISP, STATUS )

*  Write the number of axes per position to DIM.
      CALL PAR_PUT0I( 'DIM', NRAX, STATUS )

*  Set the Style of the FrameSet using the STYLE parameter. This is done so
*  that the Format of each axis value (for instance) can be controlled 
*  using STYLE.
      CALL KPG1_ASSET( 'LISTSHOW', 'STYLE', IWCS, STATUS )

*  See if Frame descriptions are required.
      CALL PAR_GET0L( 'DESCRIBE', DESC, STATUS )

*  Create a GRP Group to hold the formatted positions.
      CALL GRP_NEW( 'Formatted positions', IGRP1, STATUS )

*  Format the positions and identifiers, and store them in the group.
      CALL KPS1_LSHFM( AST_GETFRAME( IWCS, AST__CURRENT, STATUS ), 
     :                 NDISP, NRAX, %VAL( IPW0 ), %VAL( IPW2 ), IGRP1,
     :                 STATUS )

*  Save the number of positions in the list.
      CALL GRP_GRPSZ( IGRP1, SIZE, STATUS )

*  Produce any requestd scren output.
      IF( .NOT. QUIET ) THEN

*  If required, describe the co-ordinate Frame.
         IF( DESC ) THEN
            CALL MSG_BLANK( STATUS )
            CALL KPG1_DSFRM( IWCS, 'Positions will be reported in '//
     :                       'the following co-ordinate Frame:', 
     :                       STATUS )
         END IF

*  Display any title read from the positions list.
         IF( TITLE .NE. ' ' ) THEN
            CALL MSG_BLANK( STATUS )
            CALL MSG_SETC( 'TTL', TITLE )
            CALL MSG_OUT( 'LISTSHOW_MSG3', '  Title: ^TTL', STATUS )
         END IF

*  Display each formatted position in turn.
         CALL MSG_BLANK( STATUS )

         DO I = 1, SIZE
            CALL GRP_GET( IGRP1, I, 1, TEXT, STATUS ) 
            CALL MSG_SETC( 'TEXT', TEXT )
            CALL MSG_OUT( 'LISTSHOW_MSG4', '  ^TEXT', STATUS )
         END DO

         CALL MSG_BLANK( STATUS )

      END IF

*  Produce any required graphics or labels.
      IF( ( PLOT .NE. 'NONE' .OR. LABEL ) .AND. 
     :    STATUS .EQ. SAI__OK ) THEN

*  Open the graphics device for plotting with PGPLOT, obtaining an
*  identifier for the current AGI picture.
         CALL AGP_ASSOC( 'DEVICE', 'UPDATE', ' ', .FALSE., IPIC0, 
     :                   STATUS )

*  If the device could not be opened, cancel the parameter association
*  so that a different device will be used next time. Otherwise, the
*  association is retained so that the same device will be used.
         IF( STATUS .NE. SAI__OK .AND. STATUS .NE. PAR__ABORT .AND.
     :      STATUS .NE. PAR__NULL ) THEN
            CALL ERR_BEGIN( STATUS )
            CALL AGP_DEASS( 'DEVICE', .TRUE., STATUS )
            CALL ERR_BEGIN( STATUS )
         END IF

*  PGPLOT resets the colour palette when the device is opened. Therefore we
*  need to re-instate the colour palette set by the user, reading it from
*  the HDS file kappa-palette.sdf in the users adam directory.
         CALL KPG1_PLLOD( STATUS )

*  KPS1_LSHPL may need work space to hold the GRAPHICS positions.
         CALL PSX_CALLOC( 2*NDISP, '_DOUBLE', IPW3, STATUS )

*  Abort the graphics section if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 998

*  See if the current picture is a DATA picture or contains a 
*  DATA picture. If found, the DATA picture becomes the current picture 
*  and its AGI id is returned. If not found an error will be reported.
         CALL KPG1_AGFND( 'DATA', IPIC, STATUS )

*  If no DATA picture was found, annul the error and use the original 
*  current picture.
         IF( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            IPIC = IPIC0
         END IF

*  Set the PGPLOT viewport and AST Plot for the current picture.
*  The PGPLOT viewport is set equal to the selected picture, with 
*  world co-ordinates giving millimetres from the bottom left corner 
*  of the view surface. The returned Plot may include a Frame with 
*  Domain AGI_DATA representing AGI DATA co-ordinates (defined by a 
*  TRANSFORM structure stored with the picture in the database).
         CALL KPG1_GDGET( -1, AST__NULL, .TRUE., IPLOT, STATUS )

*  Merge the FrameSet read from the positions list with the Plot 
*  obtained from the AGI database, aligning them in a suitable common 
*  Frame. By preference, the Current Frame in the positions list is
*  used. The Current Frame in the FrameSet becomes the Current 
*  Frame in the Plot
         CALL KPG1_ASMRG( IPLOT, IWCS, 'PIXEL', QUIET, 2, STATUS )

*  Set the plotting style.
         CALL KPG1_ASSET( 'LISTSHOW', 'STYLE', IPLOT, STATUS )

*  Produce the graphics.
         CALL KPS1_LSHPL( IPLOT, NDISP, NRAX, %VAL( IPW2 ), PLOT,
     :                    GEO, IMARK, CLOSE, LABEL, IGRP2, %VAL( IPID ), 
     :                    %VAL( IPW3 ), STATUS )

*  Free work space used by KPS1_LSHPL.
 998     CONTINUE
         IF( IPW3 .NE. 0 ) CALL PSX_FREE( IPW3, STATUS )

*  Shutdown PGPLOT and the graphics database.
         CALL ERR_BEGIN( STATUS )
         CALL AGP_DEASS( 'DEVICE', .FALSE., STATUS )
         CALL ERR_END( STATUS )

      END IF

*  Create a a logfile containing formatted values if required.
*  We first put a "#" at the start of all comment lines and 
*  remove the "#" from the position identifiers. Also add some leading
*  spaces.
      DO I = 1, SIZE
         CALL GRP_GET( IGRP1, I, 1, TEXT, STATUS ) 

         IF( TEXT .NE. ' ' ) THEN
            CALL CHR_FANDL( TEXT, F, L )

            IF( TEXT( F : F ) .EQ. '#' ) THEN
               TEXT( F : F ) = ' '
               CALL CHR_PREFX( '   ', TEXT, L ) 
            ELSE
               CALL CHR_PREFX( '#  ', TEXT, L ) 
            END IF

            CALL GRP_PUT( IGRP1, 1, TEXT( : L ), I, STATUS ) 

         END IF

      END DO

*  Now create the log file.      
      CALL GRP_LIST( 'LOGFILE', 0, 0, ' ', IGRP1, STATUS ) 

*  Shutdown procedure.
*  ===================
  999 CONTINUE

*  Release work arrays.
      IF( IPW0 .NE. 0 ) CALL PSX_FREE( IPW0, STATUS )
      IF( IPW1 .NE. 0 ) CALL PSX_FREE( IPW1, STATUS )
      IF( IPW2 .NE. 0 ) CALL PSX_FREE( IPW2, STATUS )

*  Delete the GRP groups.
      IF( IGRP1 .NE. GRP__NOID ) CALL GRP_DELET( IGRP1, STATUS )
      IF( IGRP2 .NE. GRP__NOID ) CALL GRP_DELET( IGRP2, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a contextual error message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'LISTSHOW_ERR', 'LISTSHOW: Failed to display '//
     :                 'a positions list.', STATUS )
      END IF

      END
