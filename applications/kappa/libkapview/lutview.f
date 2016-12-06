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

*  Invocation:
*     CALL LUTVIEW( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application displays a key to the current colour table on the
*     specified image display device using the whole of the current
*     colour table (excluding the low 16 pens which are reserved for
*     axis annotation, etc.).  The key can either be a simple
*     rectangular block of colour which ramps through the colour table,
*     a histogram-style key in which the width of the block reflects the
*     number of pixels allocated to each colour index, or a set of RGB
*     intensity curves.  The choice is made using the STYLE parameter.
*
*     By default, numerical data values are displayed along the long
*     edge of the key.  The values corresponding to the maximum and
*     minimum colour index are supplied using Parameters HIGH and LOW.
*     Intermediate colour indices are labelled with values which are
*     linearly interpolated between these two extreme values.
*
*     The rectangular area in which the key (plus annotations) is drawn
*     may be specified either using a graphics cursor, or by specifying
*     the co-ordinates of two corners using Parameters LBOUND and
*     UBOUND.  Additionally, there is an option to make the key fill the
*     current picture.  See Parameter MODE.  The key may be constrained
*     to the current picture using Parameter CURPIC.
*
*     The appearance of the annotation my be controlled in detail using
*     the STYLE parameter.

*  Usage:
*     lutview [mode] [low] [high] [curpic] [device]

*  ADAM Parameters:
*     COMP = LITERAL (Read)
*        The component (within the NDF given by Parameter NDF) which is
*        currently displayed.  It may be "Data", "Quality", "Variance",
*        or "Error" (where "Error" is an alternative to "Variance" and
*        causes the square root of the variance values to be used).  If
*        "Quality" is specified, then the quality values are treated as
*        numerical values (in the range 0 to 255).  The dynamic default
*        is obtained from global Parameter COMP which is set by
*        applications such as KAPPA:DISPLAY.  []
*     CURPIC = _LOGICAL (Read)
*        If CURPIC is TRUE, the colour table key is to lie within the
*        current picture, otherwise the new picture can lie anywhere
*        within the BASE picture.  This parameter ignored if the
*        current-picture mode is selected.  [FALSE]
*     DEVICE = DEVICE (Read)
*        The image-display device on which the colour table is to be
*        drawn.  The device must be in one of the following GNS
*        categories: IMAGE_DISPLAY, IMAGE_OVERLAY, MATRIX_PRINTER, or
*        WINDOW, and have at least 24 greyscale intensities or colour
*        indices.  It must also not reset when the device is opened
*        (since the colour table would be lost) unless Parameter LUT
*        does not have the null value.  [Current image-display device]
*     FRAME = LITERAL (Read)
*        Specifies the co-ordinate Frame of the positions supplied using
*        Parameters LBOUND and UBOUND.  The following Frames will always
*        be available.
*
*        - "GRAPHICS" -- gives positions in millimetres from the
*        bottom-left corner of the plotting surface.
*
*        - "BASEPIC" -- gives positions in a normalised system in which
*        the bottom-left corner of the plotting surface is (0,0) and the
*        shortest dimension of the plotting surface has length 1.0.  The
*        scales on the two axes are equal.
*
*        - "CURPIC" -- gives positions in a normalised system in which
*        the bottom-left corner of the current picture is (0,0) and the
*        shortest dimension of the current picture has length 1.0.  The
*        scales on the two axes are equal.
*
*        - "NDC" -- gives positions in a normalised system in which the
*        bottom-left corner of the plotting surface is (0,0) and the
*        top-right corner is (1,1).
*
*        - "CURNDC" -- gives positions in a normalised system in which
*        the bottom-left corner of the current picture is (0,0) and the
*        top-right corner is (1,1).
*
*        There may be additional Frames available, describing previously
*        displayed data. If a null value is supplied, the current Frame
*        associated with the displayed data (if any) is used.  This
*        parameter is only accessed if Parameter MODE is set to "XY".
*        ["BASEPIC"]
*     HIGH = _REAL (Read)
*        The value corresponding to the maximum colour index.  It is
*        used to calculate the annotation scale for the key.  If it
*        is null (!) the maximum colour index is used, and histogram
*        style keys are not available.
*        [Current display linear-scaling maximum]
*     LBOUND = LITERAL (Read)
*        Co-ordinates of the lower-left corner of the rectangular region
*        containing the colour ramp and annotation, in the co-ordinate
*        Frame specified by Parameter FRAME (supplying a colon ":" will
*        display details of the selected co-ordinate Frame).  The
*        position should be supplied as a list of formatted axis values
*        separated by spaces or commas.  A null (!) value causes the
*        lower-left corner of the BASE or (if CURPIC is TRUE) current
*        picture to be used.
*     LOW = _REAL (Read)
*        The value corresponding to the minimum colour index.  It is
*        used to calculate the annotation scale for the key.  If it
*        is null (!) the minimum colour index is used, and histogram
*        style keys are not available.
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
*        controlled by Parameter NN.  Also the LUT's values must lie in
*        the range 0.0--1.0.  [!]
*     MODE = LITERAL (Read)
*        Method for defining the position, size and shape of the
*        rectangular region containing the colour ramp and annotation.
*        The options are:
*
*        - "Cursor" --  The graphics cursor is used to supply two
*        diametrically opposite corners or the region.
*
*        - "XY" -- The Parameters LBOUND and UBOUND are used to get the
*        limits.
*
*        - "Picture" -- The whole of the current picture is used.
*        Additional positioning options are available by using other
*        KAPPA applications to create new pictures and then specifying
*        the picture mode.
*
*        ["Cursor"]
*     NDF = NDF (Read)
*        The NDF defining the image values to be used if a
*        histogram-style key is requested. This should normally be the
*        NDF currently displayed in the most recently created DATA
*        picture.  If a value is supplied on the command line for this
*        parameter it will be used.  Otherwise, the NDF to used is found
*        by interrogating the graphics database (which contains
*        references to displayed images).  If no reference NDF can be
*        obtained from the graphics database, the user will be prompted
*        for a value.
*     NN = _LOGICAL (Read)
*        If NN is TRUE, the input lookup table is mapped to the colour
*        table by using the nearest-neighbour method.  This preserves
*        sharp edges and is better for lookup tables with blocks of
*        colour.  If NN is FALSE, linear interpolation is used, and
*        this is suitable for smoothly varying colour tables.  NN is
*        ignored unless LUT is not null.  [FALSE]
*     STYLE = GROUP (Read)
*        A group of attribute settings describing the plotting style to
*        use for the annotation.
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
*        Axis 1 is always the "data value" axis, whether it is displayed
*        horizontally or vertically.  So for instance, to set the label
*        for the data value axis, assign a value to "Label(1)" in the
*        supplied style.
*
*        To get a ramp key (the default), specify "form=ramp".  To
*        get a histogram key (a coloured histogram of pen indices),
*        specify "form=histogram".  To get a graph key (three curves of
*        RGB intensities), specify "form=graph".  If a histogram key
*        is produced, the population axis can be either logarithmic or
*        linear. To get a logarithmic population axis, specify
*        "logpop=1".  To get a linear population axis, specify
*        "logpop=0" (the default).  To annotate the long axis with pen
*        numbers instead of pixel value, specify "pennums=1" (the
*        default, "pennums=0", shows pixel values).  [current value]
*     UBOUND = LITERAL (Read)
*        Co-ordinates of the upper-right corner of the rectangular
*        region containing the colour ramp and annotation, in the
*        co-ordinate Frame specified by Parameter FRAME (supplying a
*        colon ":" will display details of the selected co-ordinate
*        Frame).  The position should be supplied as a list of formatted
*        axis values separated by spaces or commas.  A null (!) value
*        causes the lower-left corner of the BASE or (if CURPIC is TRUE)
*        the current picture to be used.

*  Examples:
*     lutview
*        Draws an annotated colour table at a position selected via
*        the cursor on the current image-display device.
*     lutview style="form=hist,logpop=1"
*        As above, but the key has the form of a coloured histogram of
*        the pen numbers in the most recently displayed image.  The
*        second axis displays the logarithm (base 10) of the bin
*        population.
*     lutview style="form=graph,pennums=1"
*        The key is drawn as a set of three (or one if a monochrome
*        colour table is in use) curves indicating the red, green and
*        blue intensity for each pen. The first axis is annotated with
*        pen numbers instead of data values.
*     lutview style="edge(1)=right,label(1)=Data value in m31"
*        As above, but the data values are labelled on the right edge of
*        the box, and the values are labelled with the string
*        "Data value in m31".
*     lutview style="textlab(1)=0,width(border)=3,colour(border)=white"
*        No textual label is drawn for the data values, and a thicker
*        than usual white box is drawn around the colour ramp.
*     lutview style="textlab(1)=0,numlab(1)=0,majticklen(1)=0"
*        Only the border is drawn around the colour ramp.
*     lutview style="textlab(1)=0,numlab(1)=0,majticklen(1)=0,border=0"
*        No annotation at all is drawn.
*     lutview p
*        Draws a colour table that fills the current picture on the
*        current image-display device.
*     lutview curpic
*        Draws a colour table within the current picture positioned
*        via the cursor.
*     lutview xy lut=my_lut device=ps_p lbound="0.92,0.2" ubound="0.98,0.8"
*        Draws the colour table in the NDF called my_lut with an
*        outline within the BASE picture on the device ps_p, defined
*        by the x-y bounds (0.92,0.2) and (0.98,0.8).  In other words
*        the plot is to the right-hand side with increasing colour
*        index with increasing y position.

*  Related Applications:
*     KAPPA: DISPLAY, LUTABLE; Figaro: COLOUR.

*  Copyright:
*     Copyright (C) 1999-2002, 2004 Central Laboratory of the Research
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
*     MJC: Malcolm J. Currie  (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     15-OCT-1999 (DSB):
*        Original AST/PGPLOT version. Based on previous version by MJC.
*     2-FEB-2000 (DSB):
*        Added code to ensure the supplied points are in the correct
*        order.
*     15-FEB-2000 (DSB):
*        Calls to KPG1_PGCUR modified for new argument list.
*     4-OCT-2001 (DSB):
*        Added graph and histogram-style keys.
*     13-AUG-2002 (DSB):
*        Added CURNDC Frame.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2010 October 14 (MJC):
*        Permit temporary style attributes.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Global SSE definitions
      INCLUDE 'NDF_PAR'        ! NDF_ constants
      INCLUDE 'AST_PAR'        ! AST_ constants
      INCLUDE 'PAR_ERR'        ! Parameter-system error definitions
      INCLUDE 'CTM_PAR'        ! Colour-table management constants
      INCLUDE 'CNF_PAR'        ! For CNF_PVAL function

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER MINCOL           ! Minimum number of colour indices on
                               ! device to be classed as an image
                               ! display
      PARAMETER ( MINCOL = 8 + CTM__RSVPN )

*  Local Variables:
      CHARACTER AMES(2)*30     ! Instructions on cursor use
      CHARACTER COMP*8         ! Component to be displayed
      CHARACTER LABEL*15       ! Label for data axis
      CHARACTER MCOMP*8        ! Component to be mapped
      CHARACTER MODE*7         ! How to get KEY Frame size and position
      CHARACTER REFNAM*256     ! Reference name
      DOUBLE PRECISION C1( 2 ) ! First corner
      DOUBLE PRECISION C2( 2 ) ! Second corner
      DOUBLE PRECISION CC( 2 ) ! Current Frame co-ords at corner
      DOUBLE PRECISION IN( 2, 2 )! GRAPHICS Frame corners
      DOUBLE PRECISION OUT( 2, NDF__MXDIM )! Current Frame corners
      DOUBLE PRECISION TEMP    ! Used for swapping values
      INTEGER ACT( 2 )         ! Actions used to terminate input
      INTEGER I                ! General variable
      INTEGER IFRM             ! Frame index
      INTEGER INDF             ! NDF identifier for input LUT NDF
      INTEGER INDF1            ! NDF identifier for input image NDF
      INTEGER IPCOL            ! Pointer to vector of colour indices
      INTEGER IPIC             ! AGI id. for original current picture
      INTEGER IPICB            ! AGI id. for Base picture
      INTEGER IPICD            ! AGI id. for most recent DATA picture
      INTEGER IPICK            ! AGI id. for new KEY picture
      INTEGER IPIN             ! Pointer to input pixel array
      INTEGER IPLOT            ! Pointer to AST Plot
      INTEGER IPLUT            ! Pointer to LUT array
      INTEGER LDIMS( 2 )       ! Dimensions of input LUT array
      INTEGER LP               ! Lowest pen with which to display the image
      INTEGER NAX              ! No. of current Frame axes
      INTEGER NCOL             ! No. of pixels in displayed image
      INTEGER NDIMS            ! Total number of NDF dimensions
      INTEGER NEL              ! Number of elements in array
      INTEGER NP               ! Number of positions supplied
      INTEGER UP               ! Highest pen with which to display the image
      LOGICAL CURPIC           ! Constrain LUT key to current picture?
      LOGICAL GOTLIM           ! Were high and low data limits supplied?
      LOGICAL GOTNAM           ! Reference name obtained for the NDF?
      LOGICAL NN               ! Map the LUT via nearest-neighbour method?
      LOGICAL SAME             ! Is the current picture the BASE picture?
      REAL HIGH                ! High data value for LUT key
      REAL LOW                 ! Low data value for LUT key
      REAL RXC( 2 )            ! Single precision X valeus at corners
      REAL RYC( 2 )            ! Single precision Y valeus at corners
      REAL X1, X2, Y1, Y2      ! Bounds of current picture viewport in mm.

*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Open the graphics data base and PGPLOT workstation (without clearing it).
      CALL KPG1_PGOPN( 'DEVICE', 'UPDATE', IPIC, STATUS )

*  Check whether the chosen device is an 'image display' with a suitable
*  minimum number of colour indices, and obtain the number of colour
*  indices.
      CALL KPG1_PQVID( 'DEVICE', 'IMAGE_DISPLAY,IMAGE_OVERLAY,'//
     :                 'WINDOW,MATRIX_PRINTER', ' ', MINCOL, UP,
     :                 STATUS )

*  Define the lowest pen number for the image.  0 is reserved for the
*  background.  Others are reserved for annotations.
      LP = CTM__RSVPN

*  Get the data values corresponding to the maximum and minimum colour
*  indices. Use defaults of the highest and lowest non-reserved pen indices.
      HIGH = REAL( UP ) + 0.5
      LOW = REAL( LP ) - 0.5
      CALL PAR_DEF0R( 'HIGH', HIGH, STATUS )
      CALL PAR_DEF0R( 'LOW', LOW, STATUS )
      GOTLIM = .FALSE.

      IF( STATUS .EQ. SAI__OK ) THEN
         CALL PAR_GET0R( 'HIGH', HIGH, STATUS )
         CALL PAR_GET0R( 'LOW', LOW, STATUS )
         LABEL = 'Data Value'
         GOTLIM = ( STATUS .EQ. SAI__OK )

         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            HIGH = REAL( UP ) + 0.5
            LOW = REAL( LP ) - 0.5
            LABEL = 'Pen Number'
         END IF
      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Indicate that we have no colour index data for the currently displayed
*  image.
      NCOL = 0

*  We can only produce a histogram if low and high data values were
*  supplied.
      IF( GOTLIM ) THEN

*  Find the most recent DATA picture.
         CALL KPG1_AGFND( 'DATA', IPICD, STATUS )

*  If a DATA picture was found...
         IF( STATUS .EQ. SAI__OK ) THEN

*  Re-instate the original current picture.
            CALL AGI_SELP( IPIC, STATUS )

*  Obtain a reference to the NDF.
            CALL KPG1_AGREF( IPICD, 'READ', GOTNAM, REFNAM, STATUS )

*  Annul the error if no DATA picture could be found.
         ELSE
            CALL ERR_ANNUL( STATUS )
            GOTNAM = .FALSE.
         END IF

*  Obtain the input NDF. If the name is given on the command line
*  it will be used.  If not, the database data reference is used,
*  if there is one.  Otherwise, the user is prompted.
         CALL KPG1_ASREF( 'NDF', 'READ', GOTNAM, REFNAM, INDF1, STATUS )

*  If a null parameter values was obtained, annul the error (histogram
*  keys will not be available).
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
         ELSE

*  Find which component of the NDF to use. MCOMP is for use with NDF_MAP and
*  may be set to 'Error'. COMP is for use with all other NDF routines (which
*  do not accept 'Error' as an NDF component name), and has 'Variance' in
*  place of 'Error'.
            CALL KPG1_ARCOG( 'COMP', INDF1, MCOMP, COMP, STATUS )

*  Map the required component.
            CALL NDF_MAP( INDF1, MCOMP, '_REAL', 'READ', IPIN, NCOL,
     :                    STATUS )

*  Allocate memory for an array of corresponding colour indices.
            CALL PSX_CALLOC( NCOL, '_INTEGER', IPCOL, STATUS )

*  Produce the colour index corresponding to each data value.
            CALL KPG1_ISCLR( .TRUE., NCOL, 1, %VAL( CNF_PVAL( IPIN ) ),
     :                       .FALSE.,
     :                       LOW, HIGH, LP, UP, 0,
     :                       %VAL( CNF_PVAL( IPCOL ) ),
     :                       STATUS )

* Annul the NDF identifier.
            CALL NDF_ANNUL( INDF1, STATUS )

         END IF

      END IF

*  Get an AGI identifier for the Base picture.
      CALL AGI_IBASE( IPICB, STATUS )

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
     :                   0.5*( Y1 + Y2 ), 2, 2, 0, 0, -32, AST__NULL,
     :                   RXC, RYC, ACT, NP, STATUS )

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

*  If the CURNDC Frame was selected, and the current picture is the BASE
*  picture, change the Current Frame to NDC for clarity.
         IF( AST_GETC( IPLOT, 'DOMAIN', STATUS ) .EQ. 'CURNDC' ) THEN
            CALL AGI_ISAMP( IPICB, SAME, STATUS )
            IF( SAME ) THEN
               CALL KPG1_ASFFR( IPLOT, 'NDC', IFRM, STATUS )
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

*  Ensure the values are in the correct order (C1 should be the lower
*  bounds, and C2 should be the upper bounds).
      IF( C1( 1 ) .GT. C2( 1 ) ) THEN
         TEMP = C1( 1 )
         C1( 1 ) = C2( 1 )
         C2( 1 ) = TEMP
      END IF

      IF( C1( 2 ) .GT. C2( 2 ) ) THEN
         TEMP = C1( 2 )
         C1( 2 ) = C2( 2 )
         C2( 2 ) = TEMP
      END IF

*  Set the PGPLOT viewport to these bounds.
      CALL KPG1_PGCUT( REAL( C1( 1 ) ), REAL( C2( 1 ) ),
     :                 REAL( C1( 2 ) ), REAL( C2( 2 ) ), STATUS )

*  Save the current viewport as a new KEY picture in the graphics database.
      CALL AGP_SVIEW( 'KEY', 'KAPPA_LUTVIEW', IPICK, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain the NDF identifier and pointer of the input lookup table.
*  Validate the LUT.
      CALL KPG1_AVLUT( 'LUT', INDF, IPLUT, NEL, STATUS )

*  Null status means do not read a new lookup table. Instead, we will use
*  the existing table.
      IF ( STATUS .EQ. PAR__NULL ) THEN

*  Annul the error.
         CALL ERR_ANNUL( STATUS )

*  If a new lookup table was supplied, load it instead of the users LUT.
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain the array dimensions.
         CALL NDF_DIM( INDF, 2, LDIMS, NDIMS, STATUS )

*  Map the lookup table to the colour table by interpolation or by
*  nearest neighbour?
         CALL PAR_GTD0L( 'NN', .FALSE., .TRUE., NN, STATUS )

*  Install the lookup table into image-display colour table.
         CALL KPG1_PGLUT( LDIMS( 2 ), %VAL( CNF_PVAL( IPLUT ) ),
     :                    LP, UP, NN,
     :                    STATUS )

      END IF

*  Create the LUT key within the KEY Picture.  The plus sign requests
*  that temporary attributes be recognised.
      CALL KPG1_LUTKY( IPICK, '+STYLE', HIGH, LOW, LABEL,
     :                 'KAPPA_LUTVIEW', LP, UP, 0.0, 0.0, 0.0, 'CC',
     :                 0.0, NCOL, %VAL( CNF_PVAL( IPCOL ) ), STATUS )

*  Tidy up
 999  CONTINUE

*  Deallocate any colour index array.
      IF( NCOL .GT. 0 ) CALL PSX_CALLOC( NCOL, '_INTEGER', IPCOL,
     :                                   STATUS )

*  Shutdown PGPLOT and the graphics database.
      CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Add a context report if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'LUTVIEW_ERR', 'LUTVIEW: Failed to display a '//
     :                 'colour table key.',  STATUS )
      END IF

      END

