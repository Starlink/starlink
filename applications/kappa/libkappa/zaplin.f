      SUBROUTINE ZAPLIN( STATUS )
*+
*  Name:
*     ZAPLIN

*  Purpose:
*     Replaces regions in a two-dimensional NDF by bad values or by
*     linear interpolation.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ZAPLIN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine replaces selected areas within a two-dimensional
*     input NDF (specified by Parameter IN), either by filling the
*     areas with bad values, or by linear interpolation between
*     neighbouring data values (see Parameter ZAPTYPE).  Each area to be
*     replaced can be either a range of pixel columns extending the
*     full height of the image, a range of pixel lines extending the
*     full width of the image, or a rectangular region with edges
*     parallel to the pixel axes (see Parameter LINCOL).
*
*     The bounds of the area to be replaced can be specified either by
*     using a graphics cursor, or directly in response to parameter
*     prompts, or by supplying a text file containing the bounds (see
*     Parameter MODE).  In the first two modes the application loops
*     asking for new areas to zap, until told to quit or an error is
*     encountered.  In the last mode processing stops when the end of
*     file is found.  An output text file may be produced containing a
*     description of the areas replaced (see Parameter COLOUT).  This
*     file may be used to specify the regions to be replaced in a
*     subsequent invocation of ZAPLIN.

*  Usage:
*     zaplin in out [title] { lincol=?
*                           { columns=? lines=?
*                           { colin=?
*                           mode

*  ADAM Parameters:
*     COLIN = FILENAME (Read)
*        The name of a text file containing the bounds of the areas to
*        be replaced.  This parameter is only accessed if Parameter MODE
*        is set to "File".  Each record in the file must be either a
*        blank line, a comment (indicated by a "!" or "#" in column 1 ),
*        or a definition of an area to be replaced, consisting of three
*        or four space-separated fields.  If a range of columns is to be
*        replaced, each of the first two fields should be a formatted
*        value for the first axis of the current co-ordinate Frame of
*        the input NDF, and the third field should be the single
*        character "C".  If a range of lines is to be replaced, each of
*        the first two fields should be a formatted value for the second
*        axis of the current co-ordinate Frame, and the third field
*        should be the single character "L".  If a rectangular region is
*        to be replaced, the first two fields should give the formatted
*        values on axes 1 and 2 at one corner of the box, and the second
*        two fields should give the formatted values on axes 1 and 2 at
*        the opposite corner of the box.
*     COLOUT = FILENAME (Read)
*        The name of an output text file in which to store descriptions
*        of the areas replaced by the current invocation of this
*        application.  It has the same format as the input file accessed
*        using Parameter COLIN, and so may be used as input on a
*        subsequent invocation.  This parameter is not accessed if
*        Parameter MODE is set to "File".  If COLOUT is null (!), no
*        file will be created.  [!]
*     COLUMNS = LITERAL (Read)
*        A pair of X values indicating the range of columns to be
*        replaced.  All columns between the supplied values will be
*        replaced.  This parameter is only accessed if Parameter LINCOL
*        is set to "Columns" or "Region", and Parameter MODE is set to
*        "Interface".  Each X value should be given as a formatted value
*        for axis 1 of the current co-ordinate Frame of the input NDF.
*        The two values should be separated by a comma, or by one or
*        more spaces.
*     DEVICE = DEVICE (Read)
*        The graphics device to use if Parameter MODE is set to
*        "Cursor".  [Current graphics device]
*     IN = NDF (Read)
*        The input image.
*     LINCOL = LITERAL (Read)
*        The type of area is to be replaced.  This Parameter is only
*        accessed if Parameter MODE is set to "Cursor" or "Interface".
*        The options are as follows.
*
*        - "Lines" -- Replaces lines of pixels between the Y values
*        specified by Parameter LINES.  Each replaced line extends the
*        full width of the image.
*
*        - "Columns" -- Replaces columns of pixels between the X values
*        specified by Parameter COLUMNS.  Each replaced column extends
*        the full height of the image.
*
*        - "Region" -- Replaces the rectangular region of pixels within
*        the X and Y bounds specified by Parameters COLUMNS and LINES.
*        The edges of the box are parallel to the pixel axes.
*
*        If this parameter is specified on the command line, and
*        Parameter MODE is set to "Interface", only one area will be
*        replaced; otherwise a series of areas will be replaced until a
*        null (!) value is supplied for this parameter.
*     LINES = LITERAL (Read)
*        A pair of Y values indicating the range of lines to be
*        replaced.  All lines between the supplied values will be
*        replaced.  This parameter is only accessed if Parameter LINCOL
*        is set to "Lines" or "Region", and Parameter MODE is set to
*        "Interface".  Each Y value should be given as a formatted
*        value for axis 2 of the current co-ordinate Frame of the input
*        NDF.  The two values should be separated by a comma, or by one
*        or more spaces.
*     MARKER = INTEGER (Read)
*        This parameter is only accessed if Parameter PLOT is set to
*        "Mark".  It specifies the type of marker with which each
*        cursor position should be marked, and should be given as an
*        integer PGPLOT marker type.  For instance, 0 gives a box, 1
*        gives a dot, 2 gives a cross, 3 gives an asterisk, 7 gives a
*        triangle.  The value must be larger than or equal to -31.
*        [current value]
*     MODE = LITERAL (Read)
*        The method used to obtain the bounds of the areas to be
*        replaced.  The supplied string can be one of the following
*        options.
*
*        - "Interface" -- bounds are obtained using Parameters COLUMNS
*        and LINES.  The type of area to be replaced is specified using
*        Parameter LINCOL.
*
*        - "Cursor" -- bounds are obtained using the graphics cursor of
*        the device specified by Parameter DEVICE.  The type of area to
*        be replaced is specified using Parameter LINCOL.  The WCS
*        information stored with the picture in the graphics database is
*        used to map the supplied cursor positions into the pixel
*        co-ordinate Frame of the input NDF.  A message is displayed
*        indicating the co-ordinate Frame in which the picture and the
*        output NDF were aligned.  Graphics may be drawn over the image
*        indicating the region to be replaced (see Parameter PLOT).
*
*        - "File" -- the bounds and type of each area to be replaced
*        are supplied in the text file specified by Parameter COLIN.
*
*        [current value]
*     NOISE = _LOGICAL (Read)
*        This parameter is only accessed if Parameter ZAPTYPE is set to
*        "Linear".  If a TRUE value is supplied, gaussian noise is added
*        to each interpolated pixel value.  The variance of the noise is
*        equal to the variance of the data value being replaced.  If the
*        data variance is bad, no noise is added.  If the input NDF has
*        no VARIANCE component, variances equal to the absolute data
*        value are used.  This facility is provided for cosmetic use.
*        [FALSE]
*     OUT = NDF (Write)
*        The output image.
*     PLOT = LITERAL (Read)
*        The type of graphics to be used to mark each cursor position.
*        The appearance of these graphics (colour, size, etc ) is
*        controlled by the STYLE parameter.  PLOT can take any of the
*        following values.
*
*        - "Adapt" -- Causes "Box" to be used if a region is being
*        replaced, "Vline" is a range of columns is being replaced, and
*        "Hline" if a range of lines is being replaced.
*
*        - "Box" -- A rectangular box with edges parallel to the edges
*        of the graphics device is drawn with the two specified
*        positions at opposite corners.
*
*        - "Mark" -- Each position is marked by the symbol specified
*        by Parameter MARKER.
*
*        - "None" -- No graphics are produced.
*
*        - "Vline" -- A vertial line is drawn through each specified
*        position, extending the entire height of the selected picture.
*
*        - "Hline" -- A horizontal line is drawn through each specified
*        position, extending the entire width of the selected picture.
*
*        [current value]
*     STYLE = LITERAL (Read)
*        A group of attribute settings describing the style to use when
*        drawing the graphics specified by Parameter PLOT.
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
*        The appearance of vertical and horizontal lines is controlled
*        by the attributes Colour(Curves), Width(Curves), etc. (the
*        synonym Lines may be used in place of Curves).  The appearance
*        of boxes is controlled by the attributes Colour(Border),
*        Size(Border), etc. (the synonym Box may be used in place of
*        Border).  The appearance of markers is controlled by
*        attributes Colour(Markers), Size(Markers), etc.
*        [current value]
*     TITLE = LITERAL (Read)
*        Title for the output image.  A null value (!) propagates the
*        title from the input image to the output image.  [!]
*     USEAXIS = GROUP (Read)
*        USEAXIS is only accessed if the current co-ordinate Frame of
*        the input NDF has more than two axes.  A group of two strings
*        should be supplied specifying the two axes spanning the plane
*        containing the areas to be replaced.  Each axis can be
*        specified using one of the following options.
*
*        - Its integer index within the current Frame of the output
*        NDF (in the range 1 to the number of axes in the current
*        Frame).
*        - Its symbol string such as "RA" or "VRAD".
*        - A generic option where "SPEC" requests the spectral axis,
*        "TIME" selects the time axis, "SKYLON" and "SKYLAT" picks the
*        sky longitude and latitude axes respectively.  Only those axis
*        domains present are available as options.
*
*        A list of acceptable values is displayed if an illegal value is
*        supplied.  If a null (!) value is supplied, the axes with the
*        same indices as the first two significant NDF pixel axes are
*        used.  [!]
*     ZAPTYPE = LITERAL (Read)
*        The method used to choose the replacement pixel values.  It
*        should be one of the options below.
*
*        - "Bad" -- Replace the selected pixels by bad values.
*
*        - "Linear" -- Replace the selected pixels using linear
*        interpolation.  If a range of lines is replaced, then the
*        interpolation is performed vertically between the first
*        non-bad pixels above and below the selected lines.  If a
*        range of columns is replaced, then the interpolation is
*        performed horizontally between the first non-bad pixels to
*        the left and right of the selected columns.  If a rectangular
*        region is replaced, then the interpolation is bi-linear
*        between the nearest non-bad pixels on all four edges of
*        the selected region.  If interpolation is not possible (for
*        instance, if the selected pixels are at the edge of the array)
*        then the pixels are replaced with bad values.  ["Linear"]

*  Examples:
*     zaplin out=cleaned colout=fudge.dat
*        Assuming the current value of Parameter MODE is "Cursor", this
*        will copy the NDF associated with the last DATA picture to an
*        NDF called "cleaned", interactively replacing areas using the
*        current graphics device.  Linear interpolation is used to
*        obtain the replacement values.  A record of the areas replaced
*        will be stored in a text file named "fudge.dat".
*     zaplin grubby cleaned i lincol=r columns="188,190" lines="15,16"
*        This replaces a region from pixel (188,15) to (190,16) within
*        the NDF called "grubby" and stores the result in the NDF called
*        "cleaned".  The current co-ordinate Frame in the input NDF
*        should be set to PIXEL first (using WCSFRAME).  The replacement
*        is performed using linear interpolation.
*     zaplin grubby(6,,) cleaned i lincol=r columns="188,190"
*        This replaces columns 188 to 190 in the 6th y-z plane region
*        within the NDF called "grubby" and stores the result in the NDF
*        called "cleaned".  The current co-ordinate Frame in the input
*        NDF should be set to PIXEL first (using WCSFRAME).  The
*        replacement is performed using linear interpolation.
*     zaplin m42 m42c f colin=aaoccd1.dat zaptype=b
*        This flags with bad values the regions in the NDF called "m42"
*        defined in the text file called "aaoccd1.dat", and stores the
*        result in an NDF called "m42c".
*     zaplin m42 m42c f colin=aaoccd1.dat noise
*        As above except that linear interpolation plus cosmetic noise
*        are used to replace the areas to be cleaned rather than bad
*        pixels.

*  Notes:
*     -  Bounds supplied in Interface and File mode are transformed into
*     the PIXEL Frame of the input NDF before being used.
*     -  Complicated results arise if the axes of the current Frame of
*     the input NDF are not parallel to the pixel axes.  In these cases
*     it is usually better to switch to the PIXEL Frame (using WCSFRAME)
*     prior to using ZAPLIN.  Roughly speaking, the range of pixel lines
*     and/or columns which are replaced will include any which
*     intersect the specified range on the current Frame axis.
*     -  When using input files care should be taken to ensure that
*     the co-ordinate system used in the file matches the current
*     co-ordinate Frame of the input NDF.
*     -  If the input NDF is a section of an NDF with a higher
*     dimensionality, the "lines" and "columns" are with respect to the
*     two-dimensional section, and do not necessarily refer to the first
*     and second dimensions of the NDF as a whole.  See the "Examples".

*  Related Applications:
*     KAPPA: ARDMASK, CHPIX, FILLBAD, GLITCH, NOMAGIC, REGIONMASK,
*     SEGMENT, SETMAGIC; Figaro: CSET, ICSET, NCSET, TIPPEX.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     VARIANCE, LABEL, TITLE, UNITS, WCS and HISTORY components of the
*     input NDF and propagates all extensions.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.

*  Implementation Deficiencies:
*     -  Needs more options for removing defects, such as bi-cubic
*     fitting the neighbourhood.
*     -  Immediate updating of a displayed image and rejection.

*  Copyright:
*     Copyright (C) 1985-1993 Science & Engineering Research Council.
*     Copyright (C) 1995, 1998, 2000, 2004 Central Laboratory of the
*     Research Councils.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2010, 2012 Science & Facilities Research Council.
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
*     MJM: Mark McCaughrean (UoE)
*     MJC: Malcolm J.  Currie (STARLINK)
*     DSB: David S.  Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     01-07-1985 (MJM):
*        First implementation, using line/column co-ordinates read from
*        the user directly, rather than by use of an interactive cursor
*        on a display screen.
*     09-12-1985 (MJM):
*        Added Poisson noise option and corrected error checking bug.
*     11-12-1985 (MJM):
*        Fixed bug caused by continuous updating of read only data -
*        changed program order.
*     06-07-1986 (MJM):
*        Tidied and more error checking.
*     1986 Aug 8 (MJC):
*        Added invocation.  Renamed ZAPLINSUB to ZPLNSB.  Reordered
*        arguments in ZPLNSB (2nd to 6th).  Added status check for
*        the output DATA_ARRAY component and associated indentation.
*     1986 Sep 1 (MJC):
*        Added arguments and deficiencies section to the prologue and
*        tidied.  Used CHR_UCASE rather than UPCASE.  Referred to lines
*        rather than rows hence some parameter names were changed.
*     1987 Oct 15 (MJC):
*        Reordered tidying and corrected method.
*     1988 Mar 17 (MJC):
*        Referred to `array' rather than `image'.
*     1988 Jun 5 (MJC):
*        More reporting of error context.
*     1989 Jun 13 (MJC):
*        Allow for processing primitive NDFs.
*     1989 Aug  7 (MJC):
*        Passed array dimensions as separate variables to COPY2D and
*        ZPLNSB.
*     1990 Mar 31 (MJC):
*        Added cursor and file modes.
*     1990 Apr 8 (MJC):
*        Added zapping type (i.e.  bad option) and the region zapping.
*     1991 May 26 (MJC):
*        NDF version featuring co-ordinate systems.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1992 April 13 (MJC):
*        Made to handle significant dimensions for user-defined
*        sections.
*     1992 November 30 (MJC):
*        Does not use non-monotonic axis centres.
*     1993 December 13 (MJC):
*        Fixed bug in cursor mode where the region bounds were half a
*        pixel too small, i.e. in co-ordinates not pixel indices.
*     1995 September 20 (MJC):
*         Changed default of TITLE to null.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     18-JAN-2000 (DSB):
*        Changed to use PGPLOT for graphics and to read and write
*        co-ordinates in the current Frame of the NDF.  Reformatted code
*        layout.
*     15-APR-2004 (DSB):
*        Correct use of LINCOL to suppress looping if supplied on
*        command line.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2006 April 12 (MJC):
*        Remove unused variables, restored Examples convention, and
*        wrapped long lines.
*     2010 August 28 (MJC):
*        Replace calls to BAD2Dx with KPG_FISEx.
*     2010 October 14 (MJC):
*        Allow temporary style attributes.
*     1-APR-2011 (DSB):
*        Use KPG_GDFND in place of KPG1_AGFND in case the most recent
*        data picture had no WCS.
*     2012 May 9 (MJC):
*        Add _INT64 support.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Global SSE parameters
      INCLUDE 'NDF_PAR'          ! NDF definitions
      INCLUDE 'SUBPAR_PAR'       ! SUBPAR constants
      INCLUDE 'PAR_ERR'          ! Parameter-system errors
      INCLUDE 'FIO_ERR'          ! FIO error definitions
      INCLUDE 'AST_PAR'          ! AST constants and function
                                 ! declarations
      INCLUDE 'PRM_PAR'          ! VAL__ contants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER ACTDES( 2 )*20   ! Cursor action descriptions
      CHARACTER ATTR*15          ! AST attribute name
      CHARACTER BUFFER*80        ! Buffer for reading the x-y lists
      CHARACTER BUFOUT*80        ! Buffer for writing the logfile
      CHARACTER COMP*13          ! Array components to process
      CHARACTER DOM*60           ! Domain name from NDF WCS FrameSet
      CHARACTER DTYPE*( NDF__SZFTP ) ! Data type for output components
      CHARACTER FNAME*256        ! Buffer for input file name
      CHARACTER ITYPE*( NDF__SZTYP ) ! Data type for processing
      CHARACTER LINCOL*7         ! Type of area to zap
      CHARACTER LINDEF*7         ! Suggested default area to zap
      CHARACTER MESS*120         ! Cursor purpose message
      CHARACTER MODE*10          ! Mode in which line/column
                                 ! co-ordinates are to be obtained
      CHARACTER OBJECT*30        ! Description of area being zapped
      CHARACTER PLOT*5           ! Nature of required graphics
      CHARACTER REFNAM*256       ! Reference name
      CHARACTER ZAP*10           ! Method of zapping
      DOUBLE PRECISION CLBND( 2 ) ! Double precision current Frame
                                 ! bounds
      DOUBLE PRECISION COLVAL( 2 ) ! Range of values on axis 1 to zap
      DOUBLE PRECISION CUBND( 2 ) ! Double precision current Frame
                                 ! bounds
      DOUBLE PRECISION DLBND( 2 ) ! Double precision GRID bounds
      DOUBLE PRECISION DP( 2, 2 ) ! A pair of double precision 2-d
                                 !positions
      DOUBLE PRECISION DUBND( 2 ) ! Double precision GRID bounds
      DOUBLE PRECISION LINVAL( 2 ) ! Range of values on axis 2 to zap
      DOUBLE PRECISION XL(2)     ! Double precision dummy argument
      DOUBLE PRECISION XU(2)     ! Double precision dummy argument
      INTEGER ACT( 2 )           ! Action associated with each vertex
      INTEGER BOX                ! Non-zero if a box is to be drawn
      INTEGER BPV                ! Bytes per value for selected data type
      INTEGER BYTES              ! No of bytes to allocate
      INTEGER CFRM               ! Current Frame from input NDF
      INTEGER CMAP               ! Mapping from PIXEL to Current Frame
      INTEGER DIMS( 2 )          ! Dimensions of data and variance array
      INTEGER EL                 ! Number of elements in mapped array
      INTEGER FD                 ! File description of input text file
      INTEGER FDO                ! File description of output text file
      INTEGER I                  ! Loop counter
      INTEGER IAT                ! Number  of characters in a string
      INTEGER IERR               ! Position of first exception during
                                 ! variance generation
      INTEGER IMARK              ! Index of PGPLOT marker symbol for
                                 ! this region
      INTEGER INDF1              ! Input-NDF identifier
      INTEGER INDF2              ! Output-NDF identifier
      INTEGER IPIC               ! AGI identifier for DATA picture
      INTEGER IPIC0              ! AGI identifier for current picture
      INTEGER IPIX               ! Index of PIXEL Frame within IWCS
      INTEGER IPLOT              ! Plot associated with current DATA
                                 ! picture
      INTEGER IPOUT( 2 )         ! Pointer to output DATA_ARRAY
                                 ! component
      INTEGER IWCS               ! WCS FrameSet for output NDF
      INTEGER LCSTA              ! The state of Parameter LINCOL
      INTEGER LINES              ! Non-zero if a line is to be drawn
      INTEGER MAP                ! Mapping from supplied Frame to PIXEL
                                 ! Frame
      INTEGER NAX                ! Number  of axes in CFRM
      INTEGER NC                 ! Character column counter
      INTEGER NCHAR              ! Number of characters in buffer read
                                 ! from the co-ordinate file
      INTEGER NERR               ! Number of exceptions in variance
                                 ! generation
      INTEGER NFRM               ! Number of Frames in original Plot
      INTEGER NP                 ! Number of points obtained by the
                                 ! cursor
      INTEGER NVAL               ! Number of values supplied
      INTEGER RBMODE             ! PGPLOT rubber-band mode
      INTEGER SDIM( NDF__MXDIM )! Significant dimensions of the NDF
      INTEGER SLBND( 2 )         ! Significant lower bounds of the image
      INTEGER SUBND( 2 )         ! Significant upper bounds of the image
      INTEGER ZLBND( 2 )         ! Lower bounds of pixels to be replaced
      INTEGER ZUBND( 2 )         ! Upper bounds of pixels to be replaced
      LOGICAL AGAIN              ! User wants to do another zap?
      LOGICAL BAD( 2 )           ! Arrays may contain bad pixels?
      LOGICAL COLLOG             ! Output co-ordinate list file is open?
      LOGICAL CURSOR             ! Cursor option was selected?
      LOGICAL FILE               ! Positions supplied in input text
                                 ! file?
      LOGICAL FIRST              ! Display instructions?
      LOGICAL GOOD               ! Usable positions supplied?
      LOGICAL GOTNAM             ! Reference name obtained for the NDF?
      LOGICAL INTERF             ! Environment option was selected?
      LOGICAL LCDEF              ! Parameter LINCOL defined on the
                                 ! command line?
      LOGICAL LINE               ! Lines to be removed? (else columns)
      LOGICAL NOISE              ! Poissonian noise to be added?
      LOGICAL REGION             ! A region is to be zapped?
      LOGICAL VARNCE             ! Variance is present in the NDF?
      LOGICAL VWORK              ! Workspace obtained for variance?
      LOGICAL XYOK               ! Positions read successfully from
                                 ! file?
      LOGICAL ZBAD               ! Fill zapped areas with bad values?
      REAL X1, X2, Y1, Y2        ! World co-ordinates bounds of PGPLOT
                                 ! window
      REAL XIN, YIN              ! Initial cursor position
      REAL XP( 2 )               ! X co-ordinates of the supplied points
      REAL YP( 2 )               ! Y co-ordinates of the supplied points

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise some flags used in the tidying up section at the end.
      COLLOG = .FALSE.
      VWORK = .FALSE.

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Find which mode of operation is to be employed.
      CALL PAR_CHOIC( 'MODE', 'Interface', 'Interface,Cursor,File',
     :                .TRUE., MODE, STATUS )

*  Set up convenience flags.
      CURSOR = MODE .EQ. 'CURSOR'
      FILE = MODE .EQ. 'FILE'
      INTERF = MODE .EQ. 'INTERFACE'

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Option to output to a text file the selected columns and lines that
*  were processed.  This is not necessary when the input comes from
*  such a list.
      IF ( .NOT. FILE ) THEN
         CALL FIO_ASSOC( 'COLOUT', 'WRITE', 'LIST', 80, FDO, STATUS )

*  Null means no logfile is required, and it is handled invisibly.
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )

         ELSE IF ( STATUS .EQ. SAI__OK ) THEN
            COLLOG = .TRUE.
            CALL MSG_OUT( 'LOG', 'Recording the co-ordinate '//
     :                    'list to $COLOUT.', STATUS )
         END IF

*  Read positions from a text list file.
      ELSE
         CALL FIO_ASSOC( 'COLIN', 'READ', 'LIST', 80, FD, STATUS )
         CALL FIO_FNAME( FD, FNAME, STATUS )
      END IF

*  Get the type of zap operation to perform.
      CALL PAR_CHOIC( 'ZAPTYPE', 'Linear', 'Linear,Bad', .TRUE., ZAP,
     :                STATUS )
      ZBAD = ZAP .EQ. 'BAD'

*  See if LINCOL was supplied on the command line.  If it was, then only
*  one zap operation is performed in interface mode.
      IF ( INTERF ) THEN
         CALL LPG_STATE( 'LINCOL', LCSTA, STATUS )
         LCDEF = LCSTA .EQ. SUBPAR__ACTIVE
      END IF

*  Find out whether or not Poissonian noise is to be added.
      IF ( .NOT. ZBAD ) CALL PAR_GTD0L( 'NOISE', .FALSE., .TRUE., NOISE,
     :                                  STATUS )

*  In "Cursor" mode, open and prepare the graphics device.
      IF ( CURSOR ) THEN

*  Open the graphics device for plotting with PGPLOT, obtaining an
*  identifier for the current AGI picture.
         CALL KPG1_PGOPN( 'DEVICE', 'UPDATE', IPIC0, STATUS )

*  Find the most recent DATA picture which has WCS.
         CALL KPG1_GDFND( 'DATA', IPIC, STATUS )

*  Report the name, comment, and label, if one exists, for the current
*  picture.
         CALL KPG1_AGATC( STATUS )

*  Obtain a reference to the NDF.
         CALL KPG1_AGREF( IPIC, 'READ', GOTNAM, REFNAM, STATUS )

*  Set the PGPLOT viewport and AST Plot for this DATA picture.  The
*  PGPLOT viewport is set equal to the selected picture, with world
*  co-ordinates giving millimetres from the bottom-left corner of the
*  view surface.
         CALL KPG1_GDGET( IPIC, AST__NULL, .FALSE., IPLOT, STATUS )

*  Save the bounds of the DATA picture.
         CALL PGQWIN( X1, X2, Y1, Y2 )

*  set the initial position of the cursor to the mid point.
         XIN = 0.5 * ( X1 + X2 )
         YIN = 0.5 * ( Y1 + Y2 )

*  See what type of graphics are required.
         CALL PAR_CHOIC( 'PLOT', 'Adapt', 'None,Adapt,Mark,Box,Vline,'/
     :                   /'Hline,', .TRUE., PLOT, STATUS )

*  Set the plotting style if required.
         IF ( PLOT .NE. 'NONE' ) THEN
            CALL KPG1_ASPSY( '(LIN*ES)', '(CURVES)', STATUS )
            CALL KPG1_ASPSY( '(BOX)', '(BORDER)', STATUS )
            CALL KPG1_ASSET( 'KAPPA_ZAPLIN', '+STYLE', IPLOT, STATUS )
            CALL KPG1_ASPSY( ' ', ' ', STATUS )

*  Set the values which indicate what KPG1_PGCUR is to draw.
            IMARK = 1
            BOX = 0
            LINES = 0

            IF ( PLOT .EQ. 'MARK' ) THEN
               CALL PAR_GDR0I( 'MARKER', 2, -31, 10000, .FALSE., IMARK,
     :                         STATUS )
            ELSE IF ( PLOT .EQ. 'BOX' ) THEN
               BOX = 1
            ELSE IF ( PLOT .EQ. 'VLINE' ) THEN
               LINES = 2
            ELSE IF ( PLOT .EQ. 'HLINE' ) THEN
               LINES = 3
            END IF

         END IF

*  Store the action descriptions.
         ACTDES( 1 ) = 'select a position'
         ACTDES( 2 ) = 'exit'

*  If we are not in cursor mode, we do not have a reference NDF.
      ELSE
         GOTNAM = .FALSE.
      END IF

*  Obtain the input NDF.  If the name is given on the command line
*  it will be used.  If not, the database data reference is used,
*  if there is one.  Otherwise, the user is prompted.
      CALL KPG1_ASREF( 'IN', 'READ', GOTNAM, REFNAM, INDF1, STATUS )

*  Now get the WCS FrameSet from the NDF, ensuring we have exactly
*  two axes in the Base and Current Frames.
      CALL KPG1_ASGET( INDF1, 2, .TRUE., .TRUE., .TRUE., SDIM,
     :                 SLBND, SUBND, IWCS, STATUS )

*  Compute the dimensions.
      DO I = 1, 2
         DIMS( I ) = SUBND( I ) - SLBND( I ) + 1
      END DO

*  Find the index of the PIXEL Frame in the WCS FrameSet.
      CALL KPG1_ASFFR( IWCS, 'PIXEL', IPIX, STATUS )

*  Save the Mapping from the PIXEL Frame to the current Frame.
      CMAP = AST_SIMPLIFY( AST_GETMAPPING( IWCS, IPIX, AST__CURRENT,
     :                                     STATUS ), STATUS )

*  Get a pointer to the current Frame.  This is the Frame in which
*  positions are supplied by the user or in the file, and in which they
*  are written to the output log file.
      CFRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )

*  Write a header to the output log file if one is being created.
      IF ( COLLOG ) THEN
         CALL FIO_WRITE( FDO, '# File created by KAPPA:ZAPLIN '//
     :                  'describing areas zapped in:', STATUS )

         CALL NDF_MSG( 'N', INDF1 )
         CALL MSG_LOAD( ' ', '#    ^N', BUFOUT, NC, STATUS )
         CALL FIO_WRITE( FDO, BUFOUT( : NC ), STATUS )
         CALL FIO_WRITE( FDO, '#', STATUS )
         CALL FIO_WRITE( FDO, '# Axis labels for co-ordinate Frame '//
     :                   'used in this file:', STATUS )
         BUFOUT = '#   '
         NC = 4

         NAX = AST_GETI( CFRM, 'NAXES', STATUS )
         DO I = 1, NAX
            ATTR = 'LABEL('
            IAT = 6
            CALL CHR_PUTI( I, ATTR, IAT )
            CALL CHR_APPND( ')', ATTR, IAT )
            CALL CHR_APPND( AST_GETC( CFRM, ATTR( : IAT ), STATUS ),
     :                      BUFOUT, NC )

            IF ( I .LT. NAX ) THEN
               CALL CHR_APPND( ',', BUFOUT, NC )
               NC = NC + 1
            END IF

         END DO

         CALL FIO_WRITE( FDO, BUFOUT( : NC ), STATUS )
         CALL FIO_WRITE( FDO, ' ', STATUS )

      END IF

*  In graphics mode, merge the WCS FrameSet read from the NDF with the
*  Plot read from the graphics database, aligning them in some suitable
*  Frame.
      IF ( CURSOR ) THEN

*  Record the original number of Frames in the Plot.
         NFRM = AST_GETI( IPLOT, 'NFRAME', STATUS )

*  Nwo merge the FrameSet into the Plot, aligning them in some suitable
*  Frame.  The current Frame in the resulting Plot will be the PIXEL
*  Frame from the original WCS FrameSet.  The Base Frame will be the
*  GRAPHICS Frame.
         CALL KPG1_ASMRG( IPLOT, IWCS, DOM, .FALSE., 0, STATUS )

*  Find the index of the NDFs PIXEL Frame in the merged Plot.
         IPIX = IPIX + NFRM

*  Get the simplified Mapping from the GRAPHICS Frame to the PIXEL
*  Frame of the NDF.
         MAP = AST_SIMPLIFY( AST_GETMAPPING( IPLOT, AST__BASE, IPIX,
     :                                       STATUS ), STATUS )

*  Report an error if the forward transformation is not defined.
         IF ( .NOT. AST_GETL( MAP, 'TRANFORWARD', STATUS ) .AND.
     :       STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'ZAPLIN_ERR3', 'The transformation from '//
     :                    'the GRAPHICS co-ordinate Frame to the '//
     :                    'PIXEL co-ordinate Frame is undefined.',
     :                    STATUS )
            GO TO 999
         END IF

*  If File or Interface mode has been selected...
      ELSE

*  Store a Mapping from Current Frame to PIXEL Frame (i.e. the inverse
*  of CMAP).
         MAP = AST_COPY( CMAP, STATUS )
         CALL AST_INVERT( MAP, STATUS )

*  Report an error if the forward transformation (i.e. from Current to
*  PIXEL Frame) is not defined.
         IF ( .NOT. AST_GETL( MAP, 'TRANFORWARD', STATUS ) .AND.
     :        STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'N', INDF1 )
            CALL ERR_REP( 'ZAPLIN_ERR3', 'The transformation from '//
     :                    'the current co-ordinate Frame in ^N to the'//
     :                    ' PIXEL co-ordinate Frame is undefined.',
     :                    STATUS )
            GO TO 999
         END IF

*  In these modes, the user can supply upper and lower ranges on a
*  single specific axis.  To transform these into pixel co-ordinates,
*  we need to specify some value for the "other" axis as well.  We use
*  the extreme values covered by the whole image.
         DLBND( 1 ) = DBLE( SLBND( 1 ) ) - 1.0D0
         DLBND( 2 ) = DBLE( SLBND( 2 ) ) - 1.0D0
         DUBND( 1 ) = DBLE( SUBND( 1 ) )
         DUBND( 2 ) = DBLE( SUBND( 2 ) )

         CALL AST_MAPBOX( CMAP, DLBND, DUBND, .TRUE., 1, CLBND( 1 ),
     :                    CUBND( 1 ), XL, XU, STATUS )
         CALL AST_MAPBOX( CMAP, DLBND, DUBND, .TRUE., 2, CLBND( 2 ),
     :                    CUBND( 2 ), XL, XU, STATUS )

      END IF

*  Create the output NDF. This is initially a copy of the input NDF,
*  which will then be altered.
      CALL LPG_PROP( INDF1, 'WCS,Data,Variance,Quality,Axis,Units',
     :               'OUT', INDF2, STATUS )

*  Obtain a new title for the output NDF.
      CALL NDF_CINP( 'TITLE', INDF2, 'Title', STATUS )

*  Check whether or not bad pixels may be present.
      CALL NDF_BAD( INDF2, 'Data', .FALSE., BAD( 1 ), STATUS )

*  See whether variance exists.
      VARNCE = .FALSE.
      CALL NDF_STATE( INDF2, 'Variance', VARNCE, STATUS )

      IF ( VARNCE ) THEN
         COMP = 'Data,Variance'
      ELSE
         COMP = 'Data'
      END IF

*  This application supports all the non-complex numeric types directly.
*  Therefore for the given type of the image find in which type it
*  should be processed.
      CALL NDF_MTYPE( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_INT64,'//
     :                '_REAL,_DOUBLE', INDF2, INDF2, 'Data', ITYPE,
     :                DTYPE, STATUS )

*  Map the output data array and variance.  Check whether or not bad
*  pixels  may be present in the variance.
      CALL NDF_MAP( INDF2, COMP, ITYPE, 'UPDATE', IPOUT, EL, STATUS )

      IF ( VARNCE ) CALL NDF_BAD( INDF2, 'Variance', .FALSE., BAD( 2 ),
     :                            STATUS )

*  Create workspace for variance when none exists in the NDF.
      IF ( .NOT. VARNCE ) THEN

*  Unbelievably, PSX_CALLOC seems only to be able to handle a subset of
*  the defined HDS data types.  Since the data type at this point could
*  be anything, we need to use PSX_MALLOC, first finding the total
*  number of bytes required.
         CALL KPG_TYPSZ( ITYPE, BPV, STATUS )
         BYTES = EL * BPV

         CALL PSX_MALLOC( BYTES, IPOUT( 2 ), STATUS )

*  Note that workspace has been allocated.
         VWORK = .TRUE.

*  Fill the workspace with variance data of the appropriate type.
*  Variance is taken to be the absolute data value.
         IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL VEC_ABSR( BAD( 1 ), EL, %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                     %VAL( CNF_PVAL( IPOUT( 2 ) ) ),
     :                     IERR, NERR, STATUS )

         ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN
            CALL VEC_ABSB( BAD( 1 ), EL, %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                     %VAL( CNF_PVAL( IPOUT( 2 ) ) ),
     :                     IERR, NERR, STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL VEC_ABSD( BAD( 1 ), EL, %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                     %VAL( CNF_PVAL( IPOUT( 2 ) ) ),
     :                     IERR, NERR, STATUS )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
            CALL VEC_ABSI( BAD( 1 ), EL, %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                     %VAL( CNF_PVAL( IPOUT( 2 ) ) ),
     :                     IERR, NERR, STATUS )

         ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
            CALL VEC_ABSK( BAD( 1 ), EL, %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                     %VAL( CNF_PVAL( IPOUT( 2 ) ) ),
     :                     IERR, NERR, STATUS )

         ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
            CALL VEC_ABSUB( BAD( 1 ), EL,
     :                      %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                      %VAL( CNF_PVAL( IPOUT( 2 ) ) ),
     :                      IERR, NERR, STATUS )

         ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
            CALL VEC_ABSUW( BAD( 1 ), EL,
     :                      %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                      %VAL( CNF_PVAL( IPOUT( 2 ) ) ),
     :                      IERR, NERR, STATUS )

         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
            CALL VEC_ABSW( BAD( 1 ), EL, %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                     %VAL( CNF_PVAL( IPOUT( 2 ) ) ),
     :                     IERR, NERR, STATUS )

         END IF
      END IF

*  Initialise switches for the repeat loop.
      AGAIN  =  .TRUE.
      LINDEF = 'Lines'
      FIRST = .TRUE.

*  Start the loop for deglitching.
      DO WHILE( AGAIN .AND. STATUS .EQ. SAI__OK )
         NP = 0

*  Obtain the type of area to zap.  In FILE mode, this is specified by
*  the contents of the supplied file.  In other modes, ask the user.
         IF ( .NOT. FILE ) THEN

*  Find out whether a range of lines or columns, or a region is to be
*  cleaned.
            CALL MSG_OUT( 'BLANK', ' ', STATUS )
            CALL PAR_CHOIC( 'LINCOL', LINDEF, 'Lines,Columns,Region,'/
     :                      /'Exit', .FALSE., LINCOL, STATUS )

*  Save the new value as the default for next time.
            LINDEF = LINCOL

*  Annul the error and assume "Exit" if a null value was given.
            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )
               LINCOL = 'EXIT'
            END IF

*  Cancel the line/column switch so that it can be reprompted.
            CALL PAR_CANCL( 'LINCOL', STATUS )

*  Set the LINE logical accordingly.  Also store a description of the
*  object which is to be zapped, and indicate the PGPLOT rubber-band
*  mode to be used when getting the points with a cursor.
            REGION = .FALSE.
            IF ( LINCOL( 1:1 ) .EQ. 'R' ) THEN
               REGION = .TRUE.
               OBJECT = 'rectangular region'
               RBMODE = 2
            ELSE IF ( LINCOL( 1:1 ) .EQ. 'L' ) THEN
               LINE  =  .TRUE.
               OBJECT = 'range of lines'
               RBMODE = 0
            ELSE IF ( LINCOL( 1:1 ) .EQ. 'C' ) THEN
               LINE  =  .FALSE.
               OBJECT = 'range of columns'
               RBMODE = 0
            ELSE
               AGAIN = .FALSE.
            END IF

         END IF

*  Abort if an error has occurred.
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain column/line limits via the graphics cursor, if required.
         IF ( CURSOR .AND. AGAIN ) THEN

*  If the graphics should adapt to the type of region, set the values
*  which indicate what KPG1_PGCUR is to draw.
            IF ( PLOT .EQ. 'ADAPT' ) THEN
               BOX = 0
               LINES = 0

               IF ( REGION ) THEN
                  BOX = 1
               ELSE IF ( LINE ) THEN
                  LINES = 3
               ELSE
                  LINES = 2
               END IF

            END IF

*  First set up the text indicating the purpose for using the cursor.
            MESS = ' '
            NC = 0
            CALL CHR_APPND( 'give 2 points bounding the required',
     :                      MESS, NC )
            NC = NC + 1
            CALL CHR_APPND( OBJECT, MESS, NC )

*  Now get the two required points.
            CALL KPG1_PGCUR( .TRUE., MESS( : NC ), 2, ACTDES, 'AX',
     :                       X1, X2, Y1, Y2, 0, XIN, YIN, 2, RBMODE,
     :                       LINES, BOX, IMARK, IPLOT, XP, YP, ACT, NP,
     :                       STATUS )

*  Copy the GRAPHICS positions into double precision.
            DP( 1, 1 ) = DBLE( XP( 1 ) )
            DP( 2, 1 ) = DBLE( XP( 2 ) )
            DP( 1, 2 ) = DBLE( YP( 1 ) )
            DP( 2, 2 ) = DBLE( YP( 2 ) )

*  Remember where to put the cursor next time.
            IF ( NP .EQ. 2 ) THEN
               XIN = XP( 2 )
               YIN = YP( 2 )
            ELSE IF ( NP .EQ. 1 ) THEN
               XIN = XP( 1 )
               YIN = YP( 1 )
            END IF

*  Leave if no points were supplied.
            AGAIN = NP .GT. 0

*  Obtain column/line limits from the text file.
         ELSE IF ( FILE ) THEN

*  Initialise the positions to be a region enclosing the entire image.
            DP( 1, 1 ) = CLBND( 1 )
            DP( 1, 2 ) = CLBND( 2 )
            DP( 2, 1 ) = CUBND( 1 )
            DP( 2, 2 ) = CUBND( 2 )

*  Read a line from the steering file, and then decode it.  Loop until a
*  line is found which contains usable information (i.e. ignore comments
*  etc.).
            XYOK = .FALSE.
            DO WHILE ( .NOT. XYOK .AND. STATUS .EQ. SAI__OK )
               CALL FIO_READ( FD, BUFFER, NCHAR, STATUS )
               CALL CHR_LDBLK( BUFFER )
               CALL KPS1_ZPDEC( FNAME, CFRM, BUFFER, DP, NP, REGION,
     :                          LINE, OBJECT, XYOK, STATUS )
            END DO

*  Obtain column/line limits from the parameter system.
         ELSE IF ( INTERF .AND. AGAIN ) THEN
            NP = 0

*  Ensure no dynamic default is used by KPG1_GTAXV below.
            COLVAL( 1 ) = AST__BAD
            LINVAL( 1 ) = AST__BAD

*  If Zapping a region, get a ranges on both axes.
            IF ( REGION ) THEN
               CALL KPG1_GTAXV( 'COLUMNS', 2, .TRUE., CFRM, 1, COLVAL,
     :                          NVAL, STATUS )
               CALL KPG1_GTAXV( 'LINES', 2, .TRUE., CFRM, 2, LINVAL,
     :                          NVAL, STATUS )

*  If zapping a range of lines, just get the range on axis 2, and use
*  the full range covered by the image for the other axis.
            ELSE IF ( LINE ) THEN
               CALL KPG1_GTAXV( 'LINES', 2, .TRUE., CFRM, 2, LINVAL,
     :                          NVAL, STATUS )
               COLVAL( 1 ) = CLBND( 1 )
               COLVAL( 2 ) = CUBND( 1 )

*  If zapping a range of columns, just get the range on axis 1, and use
*  the full range covered by the image for the other axis.
            ELSE
               CALL KPG1_GTAXV( 'COLUMNS', 2, .TRUE., CFRM, 1, COLVAL,
     :                          NVAL, STATUS )
               LINVAL( 1 ) = CLBND( 2 )
               LINVAL( 2 ) = CUBND( 2 )

            END IF

*  If a null value was supplied, annul the error.  Otherwise store the
*  supplied position and increment the number of supplied positions.
            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )

            ELSE IF ( STATUS .EQ. SAI__OK ) THEN
               DP( 1, 1 ) = COLVAL( 1 )
               DP( 1, 2 ) = LINVAL( 1 )
               DP( 2, 1 ) = COLVAL( 2 )
               DP( 2, 2 ) = LINVAL( 2 )
               NP = 2
            END IF

*  Cancel parameters for next time.
            CALL PAR_CANCL( 'COLUMNS', STATUS )
            CALL PAR_CANCL( 'LINES', STATUS )

*  Leave if no points were supplied.
            AGAIN = NP .GT. 0

         END IF

*  Assume that a good zap region has been supplied.
         GOOD = .TRUE.

*  If no positions have been given, we have no zap region to use.
         IF ( NP .EQ. 0 ) THEN
            GOOD = .FALSE.

*  If only one position has been given warn the user.
         ELSE IF ( NP .EQ. 1 ) THEN
            CALL MSG_OUT( 'ZAPLIN_MSG1', 'Only one position was '//
     :                    'supplied.', STATUS )
            GOOD = .FALSE.

*  If two positions were supplied, process them.
         ELSE

*  Transform the co-ordinates of the two positions to pixel
*  co-ordinates.
            CALL AST_TRANN( MAP, 2, 2, 2, DP, .TRUE., 2, 2, DP, STATUS )

*  Check the pixel co-ordinates are good.
            IF ( DP( 1, 1 ) .EQ. AST__BAD .OR.
     :          DP( 1, 2 ) .EQ. AST__BAD ) THEN
               CALL MSG_OUT( 'ZAPLIN_MSG1', 'The first supplied '//
     :                       'position does not correspond to a '//
     :                       'valid position in pixel co-ordinates.',
     :                       STATUS )
               GOOD = .FALSE.

            ELSE IF ( DP( 2, 1 ) .EQ. AST__BAD .OR.
     :                DP( 2, 2 ) .EQ. AST__BAD ) THEN
               CALL MSG_OUT( 'ZAPLIN_MSG1', 'The second supplied '//
     :                       'position does not correspond to a '//
     :                       'valid position in pixel co-ordinates.',
     :                       STATUS )
               GOOD = .FALSE.

            END IF

         END IF

*  If two good positions have been supplied...
         IF ( GOOD ) THEN

*  Convert to single precision floating point pixel 'indices'.
            XP( 1 ) = REAL( DP( 1, 1 ) ) + 0.5
            XP( 2 ) = REAL( DP( 2, 1 ) ) + 0.5
            YP( 1 ) = REAL( DP( 1, 2 ) ) + 0.5
            YP( 2 ) = REAL( DP( 2, 2 ) ) + 0.5

*  Define the bounds of the region.
            ZLBND( 1 ) = NINT( MIN( XP( 1 ), XP( 2 ) ) )
            ZUBND( 1 ) = NINT( MAX( XP( 1 ), XP( 2 ) ) )
            ZLBND( 2 ) = NINT( MIN( YP( 1 ), YP( 2 ) ) )
            ZUBND( 2 ) = NINT( MAX( YP( 1 ), YP( 2 ) ) )

*  Ignore these points if they are coincident.
            IF ( ZLBND( 1 ) .EQ. ZUBND( 1 ) .AND.
     :          ZLBND( 2 ) .EQ. ZUBND( 2 ) ) THEN
               CALL MSG_OUT( 'ZAPLIN_MSG1', 'The two supplied '//
     :                       'positions have the same pixel '//
     :                       'co-ordinates.', STATUS )
               GOOD = .FALSE.
            END IF

         END IF

*  If we still have two good positions...
         IF ( GOOD ) THEN

*  Constrain the bounds to the image bounds.
            ZLBND( 1 ) = MAX( ZLBND( 1 ), SLBND( 1 ) )
            ZLBND( 2 ) = MAX( ZLBND( 2 ), SLBND( 2 ) )
            ZUBND( 1 ) = MIN( ZUBND( 1 ), SUBND( 1 ) )
            ZUBND( 2 ) = MIN( ZUBND( 2 ), SUBND( 2 ) )

*  Check that the region has some overlap with the NDF.
            IF ( REGION ) THEN
               IF ( ZUBND( 1 ) .LT. ZLBND( 1 ) .OR.
     :             ZUBND( 2 ) .LT. ZLBND( 2 ) ) THEN
                  CALL NDF_MSG( 'N', INDF1 )
                  CALL MSG_OUT( 'ZAPLIN_MSG1', 'The region to be '//
     :                          'zapped does not have any overlap '//
     :                          'with ''^N''.', STATUS )
                  GOOD = .FALSE.
               END IF

*  Or check that the lines have some overlap with the NDF.
            ELSE IF ( LINE ) THEN
               IF ( ZUBND( 2 ) .LT. ZLBND( 2 ) ) THEN
                  CALL NDF_MSG( 'N', INDF1 )
                  CALL MSG_OUT( 'ZAPLIN_MSG1', 'The lines to be '//
     :                          'zapped do not have any overlap '//
     :                          'with ''^N''.', STATUS )
                  GOOD = .FALSE.
               ELSE
                  ZLBND( 1 ) = SLBND( 1 )
                  ZUBND( 1 ) = SUBND( 1 )
               END IF

*  Or check that the columns have some overlap with the NDF.
            ELSE
               IF ( ZUBND( 1 ) .LT. ZLBND( 1 ) ) THEN
                  CALL NDF_MSG( 'N', INDF1 )
                  CALL MSG_OUT( 'ZAPLIN_MSG1', 'The columns to be '//
     :                          'zapped do not have any overlap '//
     :                          'with ''^N''.', STATUS )
                  GOOD = .FALSE.
               ELSE
                  ZLBND( 2 ) = SLBND( 2 )
                  ZUBND( 2 ) = SUBND( 2 )
               END IF

            END IF

         END IF

*  If we still have two good positions...
         IF ( GOOD .AND. STATUS .EQ. SAI__OK ) THEN

*  Make the reports of the pixel-index bounds.  Bad status is returned
*  when  the line/column limits are invalid.
            CALL KPS1_ZPREP( REGION, LINE, ZLBND, ZUBND, SLBND, SUBND,
     :                       STATUS )

*  Flush any error.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_FLUSH( STATUS )
               GOOD = .FALSE.
            END IF

         END IF

*  If we still have two good positions...
         IF ( GOOD .AND. STATUS .EQ. SAI__OK ) THEN

*  Zap the region.  The subroutines do not use origin information, only
*  the displacement within the array.  Therefore, subtract the origins.
            ZLBND( 1 ) = ZLBND( 1 ) - SLBND( 1 ) + 1
            ZUBND( 1 ) = ZUBND( 1 ) - SLBND( 1 ) + 1
            ZLBND( 2 ) = ZLBND( 2 ) - SLBND( 2 ) + 1
            ZUBND( 2 ) = ZUBND( 2 ) - SLBND( 2 ) + 1

*  Call appropriate routines depending on the implementation type. The
*  routines are strictly two-dimensional for the time being.
            IF ( ITYPE .EQ. '_REAL' ) THEN

*  Replace the region by bad values, if required.
               IF ( ZBAD ) THEN
                  CALL KPG_FISER( VAL__BADR, 2, DIMS, ZLBND, ZUBND,
     :                            %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                            STATUS )

*  Or, linearly or bi-linearly interpolate across the pixels to be
*  zapped.
               ELSE
                  CALL KPS1_ZPRGR( DIMS( 1 ), DIMS( 2 ), ZLBND, ZUBND,
     :                             NOISE,
     :                             %VAL( CNF_PVAL( IPOUT( 2 ) ) ),
     :                             %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                             STATUS )
               END IF

*  Do the same for other data types.
            ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN
               IF ( ZBAD ) THEN
                  CALL KPG_FISEB( VAL__BADB, 2, DIMS, ZLBND, ZUBND,
     :                            %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                            STATUS )
               ELSE
                  CALL KPS1_ZPRGB( DIMS( 1 ), DIMS( 2 ), ZLBND, ZUBND,
     :                             NOISE,
     :                             %VAL( CNF_PVAL( IPOUT( 2 ) ) ),
     :                             %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                             STATUS )
               END IF

            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
               IF ( ZBAD ) THEN
                  CALL KPG_FISED( VAL__BADD, 2, DIMS, ZLBND, ZUBND,
     :                            %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                            STATUS )
               ELSE
                  CALL KPS1_ZPRGD( DIMS( 1 ), DIMS( 2 ), ZLBND, ZUBND,
     :                             NOISE,
     :                             %VAL( CNF_PVAL( IPOUT( 2 ) ) ),
     :                             %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                             STATUS )
               END IF

            ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
               IF ( ZBAD ) THEN
                  CALL KPG_FISEI( VAL__BADI, 2, DIMS, ZLBND, ZUBND,
     :                            %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                            STATUS )
               ELSE
                  CALL KPS1_ZPRGI( DIMS( 1 ), DIMS( 2 ), ZLBND, ZUBND,
     :                             NOISE,
     :                             %VAL( CNF_PVAL( IPOUT( 2 ) ) ),
     :                             %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                             STATUS )
               END IF

            ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
               IF ( ZBAD ) THEN
                  CALL KPG_FISEK( VAL__BADK, 2, DIMS, ZLBND, ZUBND,
     :                            %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                            STATUS )
               ELSE
                  CALL KPS1_ZPRGK( DIMS( 1 ), DIMS( 2 ), ZLBND, ZUBND,
     :                             NOISE,
     :                             %VAL( CNF_PVAL( IPOUT( 2 ) ) ),
     :                             %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                             STATUS )
               END IF

            ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
               IF ( ZBAD ) THEN
                  CALL KPG_FISEUB( VAL__BADUB, 2, DIMS, ZLBND, ZUBND,
     :                             %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                             STATUS )
               ELSE
                  CALL KPS1_ZPRGUB( DIMS( 1 ), DIMS( 2 ), ZLBND, ZUBND,
     :                              NOISE,
     :                              %VAL( CNF_PVAL( IPOUT( 2 ) ) ),
     :                              %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                              STATUS )
               END IF

            ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
               IF ( ZBAD ) THEN
                  CALL KPG_FISEUW( VAL__BADUW, 2, DIMS, ZLBND, ZUBND,
     :                             %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                             STATUS )
               ELSE
                  CALL KPS1_ZPRGUW( DIMS( 1 ), DIMS( 2 ), ZLBND, ZUBND,
     :                              NOISE,
     :                              %VAL( CNF_PVAL( IPOUT( 2 ) ) ),
     :                              %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                              STATUS )
               END IF

            ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
               IF ( ZBAD ) THEN
                  CALL KPG_FISEW( VAL__BADW, 2, DIMS, ZLBND, ZUBND,
     :                            %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                            STATUS )
               ELSE
                  CALL KPS1_ZPRGW( DIMS( 1 ), DIMS( 2 ), ZLBND, ZUBND,
     :                             NOISE,
     :                             %VAL( CNF_PVAL( IPOUT( 2 ) ) ),
     :                             %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                             STATUS )
               END IF

            END IF

*  Record the data for posterity and future processing.
            IF ( COLLOG .AND. STATUS .EQ. SAI__OK ) THEN

*  Convert the used pixel index bounds back to pixel co-ordinates.
               DP( 1, 1 ) = DBLE( ZLBND( 1 ) ) - 1.0D0
               DP( 1, 2 ) = DBLE( ZLBND( 2 ) ) - 1.0D0
               DP( 2, 1 ) = DBLE( ZUBND( 1 ) )
               DP( 2, 2 ) = DBLE( ZUBND( 2 ) )

*  Map these pixel co-ordinates into the current Frame of the NDF.
               CALL AST_TRANN( CMAP, 2, 2, 2, DP, .TRUE., 2, 2, DP,
     :                         STATUS )

*  Check the current Frame co-ordinates are good.
               IF ( DP( 1, 1 ) .EQ. AST__BAD .OR.
     :              DP( 1, 2 ) .EQ. AST__BAD ) THEN
                  CALL NDF_MSG( 'N', INDF1 )
                  CALL MSG_OUT( 'ZAPLIN_MSG1', 'The first supplied '//
     :                          'position does not correspond to a '//
     :                          'valid position in the current '//
     :                          'co-ordinate Frame of ''^N'' and so '//
     :                          'cannot be logged.', STATUS )

               ELSE IF ( DP( 2, 1 ) .EQ. AST__BAD .OR.
     :                   DP( 2, 2 ) .EQ. AST__BAD ) THEN
                  CALL NDF_MSG( 'N', INDF1 )
                  CALL MSG_OUT( 'ZAPLIN_MSG1', 'The first supplied '//
     :                          'position does not correspond to a '//
     :                          'valid position in the current '//
     :                          'co-ordinate Frame of ''^N'' and so '//
     :                          'cannot be logged.', STATUS )

*  If both are good create the text to be logged.
               ELSE
                  BUFOUT = ' '
                  NC = 0

*  Regions...
                  IF ( REGION ) THEN
                     CALL CHR_APPND( AST_FORMAT( CFRM, 1, DP( 1, 1 ),
     :                                           STATUS ), BUFOUT, NC )
                     NC = NC + 1
                     CALL CHR_APPND( AST_FORMAT( CFRM, 2, DP( 1, 2 ),
     :                                           STATUS ), BUFOUT, NC )
                     NC = NC + 1
                     CALL CHR_APPND( AST_FORMAT( CFRM, 1, DP( 2, 1 ),
     :                                           STATUS ), BUFOUT, NC )
                     NC = NC + 1
                     CALL CHR_APPND( AST_FORMAT( CFRM, 2, DP( 2, 2 ),
     :                                           STATUS ), BUFOUT, NC )
*  Lines...
                  ELSE IF ( LINE ) THEN
                     CALL CHR_APPND( AST_FORMAT( CFRM, 2, DP( 1, 2 ),
     :                                           STATUS ), BUFOUT, NC )
                     NC = NC + 1
                     CALL CHR_APPND( AST_FORMAT( CFRM, 2, DP( 2, 2 ),
     :                                           STATUS ), BUFOUT, NC )
                     CALL CHR_APPND( ' L', BUFOUT, NC )
*  Columns...
                  ELSE
                     CALL CHR_APPND( AST_FORMAT( CFRM, 1, DP( 1, 1 ),
     :                                           STATUS ), BUFOUT, NC )
                     NC = NC + 1
                     CALL CHR_APPND( AST_FORMAT( CFRM, 1, DP( 2, 1 ),
     :                                           STATUS ), BUFOUT, NC )
                     CALL CHR_APPND( ' C', BUFOUT, NC )

                  END IF

*  Write the text to the output text file.
                  CALL FIO_WRITE( FDO, BUFOUT( :NC ), STATUS )

               END IF

            END IF

         END IF

*  Add a context message if the supplied positions could not be used.
         IF ( AGAIN .AND. .NOT. GOOD ) THEN

            IF ( FILE ) THEN
               CALL MSG_SETC( 'FN', FNAME )
               CALL MSG_SETC( 'B', BUFFER )
               CALL MSG_OUT( 'ZAPLIN_MSG1', 'The following record '//
     :                       'in file ''^FN'' will ignored: ^B',
     :                       STATUS )

            ELSE
               IF ( NP .EQ. 1 ) THEN
                  CALL MSG_SETC( 'W', 'position' )
               ELSE
                  CALL MSG_SETC( 'W', 'positions' )
               END IF

               CALL MSG_OUT( 'ZAPLIN_MSG1', 'The supplied ^W will be '//
     :                       'ignored.', STATUS )
            END IF

         END IF

         FIRST = .FALSE.

*  If LINCOL was supplied on the command line, then quit the loop in
*  interface mode.
         IF ( INTERF .AND. LCDEF ) AGAIN = .FALSE.

      END DO

*  Annul the error caused by the end of file being reached.
      IF ( STATUS .EQ. FIO__EOF ) CALL ERR_ANNUL( STATUS )

*  Tidy up.
 999  CONTINUE

*  Close the text files.
      IF ( FILE ) THEN
         CALL FIO_ANNUL( FD, STATUS )

      ELSE IF ( COLLOG ) THEN
         CALL FIO_ANNUL( FDO, STATUS )
      END IF

*  Close the graphics database and device, if required.
      IF ( CURSOR ) CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )

*  If an error occurred, attempt to delete the output NDF.
      IF ( STATUS .NE. SAI__OK ) CALL NDF_DELET( INDF2, STATUS )

*  Free work space.
      IF ( VWORK ) CALL PSX_FREE( IPOUT( 2 ), STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Add a context report if anything went wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ZAPLIN_ERR', 'ZAPLIN: Failed to replace '//
     :                 'selected regions within a two-dimensional NDF.',
     :                 STATUS )
      END IF

      END
