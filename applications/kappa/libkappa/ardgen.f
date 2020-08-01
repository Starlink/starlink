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
*     image using a cursor, and then storing a description of the
*     selected regions in a text file in the form of an
*     `ARD Description' (see SUN/183).  This text file may subsequently
*     be used in conjunction with packages such as CCDPACK or ESP.
*
*     The application initially obtains a value for the SHAPE parameter
*     and then allows you to identify either one or many regions of the
*     specified shape, dependent on the value of Parameter STARTUP.
*     When the required regions have been identified, a value is
*     obtained for Parameter OPTION, and that value determines what
*     happens next.  Options include obtaining further regions,
*     changing the current region shape, listing the currently defined
*     regions, leaving the application, etc.  Once the selected action
*     has been performed, another value is obtained for OPTION, and
*     this continues until you choose to leave the application.
*
*     Instructions on the use of the cursor are displayed when the
*     application is run.  The points required to define a region of
*     the requested shape are described whenever the current region
*     shape is changed using Parameter SHAPE.  Once the points required
*     to define a region have been given an outline of the entire
*     region is drawn on the graphics device using the pen specified by
*     Parameter PALNUM.
*
*     In the absence of any other information, subsequent application
*     will use the union (i.e. the logical OR) of all the defined
*     regions.  However, regions can be combined in other ways using
*     the COMBINE option (see Parameter OPTION).  For instance, two
*     regions originally defined using the cursor could be replaced by
*     their region of intersection (logical AND), or a single region
*     could be replaced by its own exterior (logical NOT).  Other
*     operators can also be used (see Parameter OPERATOR).

*  Usage:
*     ardgen ardout shape option [device] [startup]
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
*        together using the operator specified by Parameter OPERATOR.
*        If the operator is "NOT", then only one region index need be
*        supplied.  Region indices are displayed by the "List" option
*        (see Parameter OPTION).
*     OPERATOR = LITERAL (Read)
*        The operator to use when combining two regions into a single
*        region.  The pixels included in the resulting region depend on
*        which of the following operators is selected.
*
*        - "AND" -- Pixels are included if they are in both of the
*        regions specified by Parameter OPERANDS.
*
*        - "EQV" -- Pixels are included if they are in both or neither
*        of the regions specified by Parameter OPERANDS.
*
*        - "NOT" -- Pixels are included if they are not inside the
*        region specified by Parameter OPERANDS.
*
*        - "OR" -- Pixels are included if they are in either of the
*        regions specified by Parameter OPERANDS.  Note, an OR
*        operator is implicitly assumed to exist between each
*        pair of adjacent regions unless some other operator is
*        specified.
*
*        - "XOR" -- Pixels are included if they are in one, but not
*        both, of the regions specified by Parameter OPERANDS.
*
*     OPTION= LITERAL (Read)
*        A value for this parameter is obtained when you choose to end
*        cursor input (by pressing the relevant button as described
*        when the application starts up).  It determines what to do
*        next.  The following options are available:
*
*        - "Combine" -- Combine two previously defined regions into a
*        single region using a Boolean operator, or invert
*        a previously defined region using a Boolean .NOT.
*        operator.  See Parameters OPERANDS and OPERATOR.  The
*        original regions are deleted and the new combined
*        (or inverted) region is added to the end of the
*        list of defined regions.
*
*        - "Delete" -- Delete previously defined regions, see Parameter
*        REGIONS.
*
*        - "Draw" -- Draw the outline of the union of one or more
*        previously defined regions, see Parameter REGIONS.
*
*        - "Exit" -- Write out the currently defined regions to a text
*        file and exit the application.
*
*        - "List" -- List the textual descriptions of the currently
*        defined regions on the screen.  Each region is described by an
*        index value, a "keyword" corresponding to the shape, and
*        various arguments describing the extent and position of the
*        shape.  These arguments are described in the "Notes" section
*        below.
*
*        - "Multi" -- The cursor is displayed and you can then identify
*        multiple regions of the current shape, without being
*        re-prompted for OPTION after each one.  These regions are
*        added to the end of the list of currently defined regions.  If
*        the current shape is "Polygon", "Frame" or "Whole" (see
*        Parameter SHAPE) then multiple regions cannot be defined and
*        the selected option automatically reverts to "Single".
*
*        - "Single" -- The cursor is displayed and you can then identify
*        single region of the current shape.  You are re-prompted for
*        Parameter OPTION once you have defined the region.  The
*        identified region is added to the end of the list of currently
*        defined regions.
*
*        - "Shape" -- Change the shape of the regions created by the
*        "Single" and "Multi" options.  This causes a new value for
*        Parameter SHAPE to be obtained.
*
*        - "Style" -- Change the drawing style by providing a new value
*        for Parameter STYLE.
*
*        - "Quit" -- Quit the application without saving the currently
*        defined regions.
*
*        - "Undo" -- Undo the changes made to the list of ARD regions by
*        the previous option. Note, the undo list can contain up to 30
*        entries.  Entries are only stored for options which actually
*        produce a change in the list of regions.
*
*     REGIONS() = LITERAL (Read)
*        The list of regions to be deleted or drawn.  Regions are
*        numbered consecutively from 1 and can be listed using the
*        "List" option (see Parameter OPTION).  Single regions or a set
*        of adjacent regions may be specified, e.g. assigning
*        [4,6-9,12,14-16] will delete regions 4,6,7,8,9,12,14,15,16.
*        (Note that the brackets are required to distinguish this array
*        of characters from a single string including commas.  The
*        brackets are unnecessary when there is only one item.)  The
*        numbers need not be in ascending order.
*
*        If you wish to delete or draw all the regions enter the
*        wildcard *.  5-* will delete or draw from 5 to the last region.
*     SHAPE = LITERAL (Read)
*        The shape of the regions to be defined using the cursor.
*        After selecting a new shape, you are immediately requested to
*        identify multiple regions as if "Multi" had been specified for
*        Parameter OPTION.  The currently available shapes are listed
*        below.
*
*        - "Box" -- A rectangular box with sides parallel to the
*        co-ordinate axes, defined by its centre and one of its corners.
*
*        - "Circle" -- A circle, defined by its centre and radius.
*
*        - "Column" -- A single value on Axis 1, spanning all values on
*        Axis 2.
*
*        - "Ellipse" -- An ellipse, defined by its centre, one end of
*        the major axis, and one other point which can be anywhere on
*        the ellipse.
*
*        - "Frame" -- The whole image excluding a border of constant
*        width, defined by a single point on the frame.
*
*        - "Point" -- A single pixel.
*
*        - "Polygon" -- Any general polygonal region, defined by up to
*        200 vertices.
*
*        - "Rectangle" -- A rectangular box with sides parallel to the
*        co-ordinate axes, defined by a pair of diagonally
*        opposite corners.
*
*        - "Rotbox" -- A rotated box, defined by both ends of an edge,
*        and one point on the opposite edge.
*
*        - "Row" -- A single value on Axis 2, spanning all values on
*        Axis 1.
*
*        - "Whole" -- The whole of the displayed image.
*
*     STARTUP = LITERAL (Read)
*        Determines if the application starts up in "Multi" or "Single"
*        mode (see Parameter OPTION). ["Multi"]
*     STYLE = GROUP (Read)
*        A group of attribute settings describing the plotting style to
*        use when drawing the regions.
*
*        A comma-separated list of strings should be given in which each
*        string is either an attribute setting, or the name of a text
*        file preceded by an up-arrow character "^".  Such text files
*        should contain further comma-separated lists which will be read
*        and interpreted in the same manner.  Attribute settings are
*        applied in the order in which they occur within the list, with
*        later settings overriding any earlier settings given for the
*        same attribute.
*
*        Each individual attribute setting should be of the form:
*
*           <name>=<value>
*
*        where <name> is the name of a plotting attribute, and <value>
*        is the value to assign to the attribute.  Default values will
*        be used for any unspecified attributes.  All attributes will be
*        defaulted if a null value (!)---the initial default---is
*        supplied.  To apply changes of style to only the current
*        invocation, begin these attributes with a plus sign.  A mixture
*        of persistent and temporary style changes is achieved by
*        listing all the persistent attributes followed by a plus sign
*        then the list of temporary attributes.
*
*        See section "Plotting Attributes" in SUN/95 for a description
*        of the available attributes.  Any unrecognised attributes are
*        ignored (no error is reported).
*
*        The appearance of the lines forming the edges of each region
*        is controlled by the attributes Colour(Curves), Width(Curves),
*        etc.  The appearance of the vertex markers is controlled by the
*        attributes Colour(Markers), Size(Markers), etc.
*        [current value]
*     UNDO = _LOGICAL (Read)
*        Used to confirm that it is OK to proceed with an "Undo" option.
*        The consequences of proceeding are described before the
*        parameter is obtained.

*  Examples:
*     ardgen extract.txt circle exit startup=single
*        This example allows you to create a text file (extract.txt)
*        describing a single circular region of the image displayed on
*        the current graphics device.  The application immediately exits
*        after the region has been identified.  This example may be
*        useful in scripts or command procedures since there is no
*        prompting.

*  Notes:
*     -  An image must previously have been displayed on the graphics
*     device.
*     -  The arguments for the textual description of each shape are as
*     follows :
*
*        - "Box" -- The co-ordinates of the centre, followed by the
*        lengths of the two sides.
*
*        - "Circle" -- The co-ordinates of the centre, followed by the
*        radius.
*
*        - "Column" -- The Axis 1 co-ordinate of the column.
*
*        - "Ellipse" -- The co-ordinates of the centre, followed by the
*        lengths of the semi-major and semi-minor axes, followed by the
*        angle between Axis 1 and the semi-major axis (in radians).
*
*        - "Frame" -- The width of the border.
*
*        - "Point" -- The co-ordinates of the pixel.
*
*        - "Polygon" -- The co-ordinates of each vertex in the order
*        given.
*
*        - "Rectangle" -- The co-ordinates of two diagonally opposite
*        corners.
*
*        - "Rotbox" -- The co-ordinates of the box centre, followed by
*        the lengths of the two sides, followed by the angle between the
*        first side and Axis 1 (in radians).
*
*        - "Row" -- The Axis 2 co-ordinate of the row.
*
*        - "Whole" -- No arguments.
*
*      -  The shapes are defined within the current co-ordinate Frame of
*      the displayed NDF.  For instance, if the current co-ordinate
*      Frame of the displayed NDF is RA/DEC, then "COLUMN" regions will
*      be curves of constant DEC, "ROW" regions will be curves of
*      constant RA (assuming Axis 1 is RA and Axis 2 is DEC), straight
*      lines will correspond to geodesics, etc. Numerical values will be
*      stored in the output text file in the current coordinate Frame of
*      the NDF.  WCS information will also be stored in the output text
*      file allowing the stored positions to be converted to other
*      systems (pixel co-ordinates, for instance).

*  Related Applications:
*     KAPPA: ARDPLOT, ARDMASK; CCDPACK; ESP.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1995, 1999, 2001 Central Laboratory of the Research
*     Councils.
*     Copyright (C) 2010 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

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
*     30-AUG-2001 (DSB):
*        Modify to use AST/PGPLOT and ARD V2. Added options Draw.
*     18-JUL-2006 (DSB):
*        Ensure all GRP groups are deleted before returning.
*     2010 October 14 (MJC):
*        Added documentation of Parameter STYLE, and permit temporary
*        style attributes.
*     1-APR-2011 (DSB):
*        Use KPG_GDFND in place of KPG1_AGFND in case the most recent
*        data picture had no WCS.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE constants
      INCLUDE 'DAT_PAR'          ! HDS public constants
      INCLUDE 'PAR_ERR'          ! Parameter system error constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'GRP_PAR'          ! GRP constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_ISALF
      LOGICAL CHR_ISALF          ! True if a character is alphabetic

*  Local Constants:
      INTEGER NDIM               ! Array dimensionality
      PARAMETER ( NDIM = 2 )     ! 2-d arrays only

      INTEGER MXGRP              ! Maximum number of undo operations
      PARAMETER ( MXGRP = 30 )

      INTEGER MXPOL              ! Max. no. of vertices in a polygon
      PARAMETER ( MXPOL = 200 )

*  Local Variables:
      LOGICAL CHANGE             ! Has the group changed?
      INTEGER GRPS( MXGRP )      ! The undo list of groups
      INTEGER I                  ! Loop count
      INTEGER IGRP               ! ID. for GRP group holding ARD desc
      LOGICAL INFO               ! Display user information?
      INTEGER IPIC               ! AGI identifier for DATA picture
      INTEGER IPIC0              ! AGI identifier for current picture
      INTEGER IPLOT              ! Plot associated with current DATA picture
      LOGICAL MORE               ! Should another option be obtained?
      INTEGER NP                 ! No. of points obtained by the cursor
      INTEGER NPTS               ! Number of x-y points to be measured
      INTEGER NREG0              ! Original no. of regions in the group
      INTEGER NREG               ! No. of regions in the group
      INTEGER NREGS( MXGRP )     ! No. of regions in all groups on the undo list
      CHARACTER*7 OPTION         ! User selected option
      CHARACTER*80 OPTS( MXGRP ) ! Text describing all undo operations
      LOGICAL READY              ! Has current option been performed?
      LOGICAL REGION             ! Another region to be defined?
      CHARACTER*128 SHAPE        ! Current region shape
      INTEGER TOPGRP             ! Index of most recent group in undo list
      LOGICAL UNDO               ! Undo the previous action?
      CHARACTER*80 UNDOTX        ! Text describing undo operation
      INTEGER UNDOI              ! Length of UNDOTX
      REAL X1, X2, Y1, Y2        ! World co-ordinates bounds of PGPLOT window
      REAL XIN, YIN              ! Initial cursor position
      DOUBLE PRECISION XP( MXPOL ) ! Current Frame Axis-1 cursor positions
      DOUBLE PRECISION YP( MXPOL ) ! Current Frame Axis-2 cursor positions

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start a new AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Start up the graphics device and get information about the NDF.
*  ===============================================================
*
*  Open the graphics device for plotting with PGPLOT, obtaining an
*  identifier for the current AGI picture.
      CALL KPG1_PGOPN( 'DEVICE', 'UPDATE', IPIC0, STATUS )

*  Find the most recent DATA picture which has WCS.
      CALL KPG1_GDFND( 'DATA', IPIC, STATUS )

*  Report the name, comment, and label, if one exists, for the current
*  picture.
      CALL KPG1_AGATC( STATUS )

*  Set the PGPLOT viewport and AST Plot for this DATA picture.  The
*  PGPLOT viewport is set equal to the selected picture, with world
*  co-ordinates giving millimetres from the bottom-left corner of the
*  view surface.
      CALL KPG1_GDGET( IPIC, AST__NULL, .FALSE., IPLOT, STATUS )

*  Save the bounds of the DATA picture.
      CALL PGQWIN( X1, X2, Y1, Y2 )

*  Save the mid point.
      XIN = 0.5 * ( X1 + X2 )
      YIN = 0.5 * ( Y1 + Y2 )

*  Set the plotting style.  The plus prefix requests support of
*  temporary STYLE attributes.
      CALL KPG1_ASSET( 'KAPPA_ARDGEN', '+STYLE', IPLOT, STATUS )

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

*  Main loop
*  =========

*  Indicate that currently there are no groups in the undo list.
      TOPGRP = 0
      DO I = 1, MXGRP
         GRPS( I ) = GRP__NOID
      END DO

*  Loop round obtaining options from the user.
      NREG = 0
      INFO = .TRUE.
      MORE = .TRUE.
      DO WHILE ( MORE .AND. STATUS .EQ. SAI__OK )

*  Initialise the undo text for this action.
         UNDOTX = ' '
         UNDOI = 0

*  If we are ready for the next action, see what the user wants to do
*  next.  Otherwise, perform the action required by the current
*  contents of the variable "Option".
         IF ( READY ) THEN
            CALL MSG_BLANK( STATUS )
            CALL PAR_CHOIC( 'OPTION', 'SHAPE', 'Multi,Single,Shape,'/
     :                      /'Delete,List,Combine,Draw,Exit,Quit,'/
     :                      /'Style,Undo', .FALSE., OPTION, STATUS )
            CALL PAR_CANCL( 'OPTION', STATUS )
            READY = .FALSE.
            IF ( STATUS .EQ. PAR__NULL ) OPTION = 'QUIT'
         END IF

*  If user wants to define more regions of the current shape...
*  ------------------------------------------------------------
         IF ( OPTION .EQ. 'SINGLE' .OR.
     :        OPTION .EQ. 'MULTI' ) THEN

*  Save the original number of regions in the group.
            NREG0 = NREG

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
*  SINGLE was selected for Parameter OPTION.
            REGION = .TRUE.
            DO WHILE ( REGION .AND. STATUS .EQ. SAI__OK )

*  Initialise the number of cursor positions obtained.
               NP = 0

*  Obtain the required number of positions via the graphics cursor.  An
*  exact number of points is required.
               CALL KPS1_AGNCP( IPLOT, SHAPE, MXPOL, NPTS, X1, X2, Y1,
     :                          Y2, INFO, XIN, YIN, NP, XP, YP, STATUS )

*  If the required number of positions were obtained (or at least one
*  position was obtained in the case of shapes with variable number
*  of defining positions), store the ARD description for the region in
*  the GRP group.
               IF ( ( NP .EQ. NPTS ) .OR.
     :              ( NPTS .LT. 0 .AND. NP .GT. 0 ) ) THEN

*  Now append the ARD description for this region to the end of the
*  group.
                  CALL KPS1_AGNST( IPLOT, NP, SHAPE, IGRP, X1, X2, Y1,
     :                             Y2, XP, YP, NREG, STATUS )

*  Draw the region.
                  CALL KPS1_AGNDR( IPLOT, IGRP, NREG, STATUS )

*  Issue a warning if sufficient positions were not obtained. Only do
*  this if the user is giving a single region.  Otherwise take it as an
*  indication that the user doesn't want to give any more regions.
               ELSE IF ( OPTION .EQ. 'SINGLE' ) THEN
                  CALL MSG_SETC( 'SH', SHAPE )
                  CALL MSG_SETI( 'NPTS', NPTS )
                  CALL MSG_OUT( 'ARDGEN_MSG', '^NPTS positions '/
     :                          /'required to define a "^SH" region!',
     :                          STATUS )
               END IF

*  If the user only wanted to give one region, or if the user gave an
*  incomplete region, leave the loop.
               IF( ( OPTION .EQ. 'SINGLE' ) .OR.
     :              ( NP .NE. NPTS ) ) THEN
                  REGION = .FALSE.

*  If another region is to be defined, tell the user.
               ELSE
                  CALL MSG_SETC( 'SH', SHAPE )
                  CALL MSG_OUT( 'ARDGEN_MSG', 'Region completed. '/
     :                          /'Identify another ''^SH'' region...',
     :                          STATUS )
               END IF

            END DO

*  Indicate that we are ready for a new option.
            READY = .TRUE.
            CALL CHR_APPND( 'remove', UNDOTX, UNDOI )
            UNDOI = UNDOI + 1
            CALL CHR_PUTI( NREG - NREG0, UNDOTX, UNDOI )
            UNDOI = UNDOI + 1
            CALL CHR_APPND( SHAPE, UNDOTX, UNDOI )
            UNDOI = UNDOI + 1
            CALL CHR_APPND( 'region', UNDOTX, UNDOI )
            IF ( NREG - NREG0 .GT. 1 ) CALL CHR_APPND( 's', UNDOTX,
     :                                                 UNDOI )

*  If user wants to change the current shape...
*  --------------------------------------------
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
*  -----------------------------------------------
         ELSE IF ( OPTION .EQ. 'DELETE' ) THEN
            NREG0 = NREG
            CALL KPS1_AGNDL( 'REGIONS', IGRP, NREG, STATUS )
            READY = .TRUE.

            CALL CHR_APPND( 'reinstate ', UNDOTX, UNDOI )
            UNDOI = UNDOI + 1
            CALL CHR_PUTI( NREG - NREG0, UNDOTX, UNDOI )
            UNDOI = UNDOI + 1
            CALL CHR_APPND( 'deleted region', UNDOTX, UNDOI )
            IF ( NREG - NREG0 .GT. 1 ) CALL CHR_APPND( 's', UNDOTX,
     :                                                 UNDOI )

*  If user wants to draw a region...
*  ---------------------------------
         ELSE IF ( OPTION .EQ. 'DRAW' ) THEN
            CALL KPS1_AGNDW( 'REGIONS', IPLOT, IGRP, NREG, STATUS )
            READY = .TRUE.

*  If user wants to list the currently defined region...
*  -----------------------------------------------------
         ELSE IF ( OPTION .EQ. 'LIST' ) THEN
            CALL KPS1_AGNLS( IGRP, STATUS )
            READY = .TRUE.

*  If user wants to combined regions together...
*  ---------------------------------------------
         ELSE IF ( OPTION .EQ. 'COMBINE' ) THEN
            CALL ERR_MARK
            CALL KPS1_AGNCM( 'OPERATOR', 'OPERANDS', IGRP, NREG,
     :                       STATUS )
            IF ( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )
            CALL ERR_RLSE
            READY = .TRUE.
            UNDOTX = 'undo the effects of the previous COMBINE option'

*  If user wants to undo the previous change...
*  --------------------------------------------
         ELSE IF ( OPTION .EQ. 'UNDO' ) THEN
            IF ( TOPGRP .GT. 0 .AND.
     :           GRPS( TOPGRP ) .NE. GRP__NOID ) THEN

*  Obtain confirmation.
               CALL MSG_SETC( 'ACTION', OPTS( TOPGRP ) )
               CALL MSG_OUT( ' ', 'This will ^ACTION.', STATUS )
               CALL PAR_CANCL( 'UNDO', STATUS )
               CALL PAR_GET0L( 'UNDO', UNDO, STATUS )

*  Form new groups.
               IF ( UNDO .AND. STATUS .EQ. SAI__OK ) THEN
                  CALL GRP_DELET( IGRP, STATUS )
                  CALL GRP_DELET( GRPS( TOPGRP ), STATUS )
                  GRPS( TOPGRP ) = GRP__NOID
                  OPTS( TOPGRP ) = ' '
                  NREGS( TOPGRP ) = 0

                  TOPGRP = TOPGRP - 1
                  IF ( TOPGRP .EQ. 0 ) TOPGRP = MXGRP
                  IF ( GRPS( TOPGRP ) .NE. GRP__NOID ) THEN
                     CALL GRP_COPY( GRPS( TOPGRP ), 0, 0, .FALSE., IGRP,
     :                              STATUS )
                     NREG = NREGS( TOPGRP )
                  ELSE
                     CALL GRP_NEW( ' ', IGRP, STATUS )
                     NREG = 0
                  END IF

               END IF

            ELSE
               CALL MSG_OUT( ' ', 'There are no changes to undo.',
     :                       STATUS )
            END IF
            READY = .TRUE.

*  If user wants to change the drawing style...
*  --------------------------------------------

*  The plus prefix requests support of temporary STYLE attributes.
*  As the parameter is cancelled there is no need to write the full
*  current value to the parameter file.
         ELSE IF ( OPTION .EQ. 'STYLE' ) THEN
            CALL PAR_CANCL( 'STYLE', STATUS )
            CALL KPG1_ASSET( 'KAPPA_ARDGEN', '+STYLE', IPLOT, STATUS )
            READY = .TRUE.

*  If user wants to exit the program...
*  ------------------------------------
         ELSE IF ( OPTION .EQ. 'EXIT' ) THEN
            MORE = .FALSE.

*  If user wants to quit the program...
*  ------------------------------------

*  Quitting throws away the current regions.
         ELSE IF ( OPTION .EQ. 'QUIT' ) THEN
            NREG = 0
            MORE = .FALSE.

         END IF

*  Update the group.
*  -----------------

*  If the contents of the group has changed (except for the action of an
*  UNDO option), add a copy of the group into the undo list.
         CALL KPS1_AGNCH( IGRP, MXGRP, GRPS, TOPGRP, CHANGE, STATUS )
         IF ( CHANGE .AND. OPTION .NE. 'UNDO' ) THEN

*  Find the index at which to store the new group. Go back to the
*  beginning when the end is reached.
            IF ( TOPGRP .EQ. MXGRP ) THEN
               TOPGRP = 1
            ELSE
               TOPGRP = TOPGRP + 1
            END IF

*  If this array slot already has a group, delete it.
            IF ( GRPS( TOPGRP ) .NE. GRP__NOID ) THEN
               CALL GRP_DELET( GRPS( TOPGRP ), STATUS )
            END IF

* Create the copy, storing it in the selected array slot.
            CALL GRP_COPY( IGRP, 0, 0, .FALSE., GRPS( TOPGRP ), STATUS )
            OPTS( TOPGRP ) = UNDOTX
            NREGS( TOPGRP ) = NREG

         END IF

      END DO

*  Write the ARD file.
*  ===================

*  If the ARD description is not of zero size, write it out to a text
*  file.
      IF ( NREG .GT. 0 ) THEN

*  Add a WCS statement to the start of the group.
         CALL KPS1_AGNWC( IPLOT, IGRP, STATUS )

*  Write this new group out.
         CALL GRP_LIST( 'ARDOUT', 0, 0, '   ARD description generated '/
     :                  /'by ARDGEN', IGRP, STATUS )

*  Otherwise, issue a warning message.
      ELSE
         CALL MSG_BLANK( STATUS )
         CALL MSG_OUT( 'ARDGEN_MSG', 'No output file has been created.',
     :                 STATUS )

      END IF
      CALL MSG_BLANK( STATUS )

*   Closedown sequence.
*   ===================

*  Delete all groups.
      DO I = 1, MXGRP
         IF ( GRPS( I ) .NE. GRP__NOID ) THEN
            CALL GRP_DELET( GRPS( I ), STATUS )
         END IF
      END DO
      CALL GRP_DELET( IGRP, STATUS )

*  Close down AGI and PGPLOT.
      CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a contextual error message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARDGEN_ERR', 'ARDGEN: Failed to create an ARD '/
     :                 /'description file.', STATUS )
      END IF

      END
