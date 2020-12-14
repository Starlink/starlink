      SUBROUTINE POLPLOT( STATUS )
*+
*  Name:
*     POLPLOT

*  Purpose:
*     Plots a 2-dimensional vector map.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL POLPLOT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application plots vectors defined by the values contained
*     within four columns in a catalogue. These columns give the magnitude
*     and orientation of each vector, and the position of each vector (see
*     parameters COLMAG, COLANG, COLX and COLY). If the catalogue has a
*     third axis (spectral channel for instance), then only vectors with
*     a specified value on the third axis will be plotted (see parameter
*     ZAXVAL).
*
*     The plot is produced within the current graphics database picture,
*     and may be aligned with an existing DATA picture if the existing
*     picture contains suitable co-ordinate Frame information (see
*     parameter CLEAR).
*
*     Annotated axes can be produced (see parameter AXES), and the appearance
*     of the axes can be controlled in detail (see parameter STYLE). The
*     axes show co-ordinates in the co-ordinate Frame specified by
*     parameter FRAME.
*
*     A key to the vector scale can be displayed to the right of the
*     vector map (see parameter KEY). The appearance and position of this
*     key may be controlled using parameters KEYSTYLE and KEYPOS.

*  Usage:
*     polplot cat colx coly colmag colang [vscale] [arrow] [just] [device]

*  ADAM Parameters:
*     ANGROT = _REAL (Read)
*        A rotation angle in degrees to be added to each vector orientation
*        before plotting the vectors (see parameters COLANG and NEGATE). It
*        should be the range 0-360. Note, this parameter is named ANGROT
*        for historical reasons, and its use should not be confused with
*        the ANGROT extension item (see POLIMP) which gives the orientation
*        of the reference direction. [0.0]
*     ARROW = LITERAL (Read)
*        Vectors are drawn as arrows, with the size of the arrow head
*        specified by this parameter. Simple lines can be drawn by setting
*        the arrow head size to zero. The value should be expressed as a
*        fraction of the largest dimension of the vector map. [0.0]
*     AXES = _LOGICAL (Read)
*        TRUE if labelled and annotated axes are to be drawn around the
*        vector map, showing the coordinate Frame specified by parameter
*        FRAME. The appearance of the axes can be controlled using
*        the STYLE parameter. [TRUE]
*     CAT = LITERAL (Read)
*        The name of the input catalogue. This may be in any format
*        supported by the CAT library (see SUN/181). A file type of .FIT
*        is assumed if no file type is supplied.
*     CLEAR = _LOGICAL (Read)
*        TRUE if the graphics device is to be cleared before displaying
*        the vector map. If you want the vector map to be drawn over
*        the top of an existing DATA picture, then set CLEAR to FALSE. The
*        vector map will then be drawn in alignment with the displayed
*        data. If possible, alignment occurs within the co-ordinate Frame
*        specified by parameter FRAME. If this is not possible, (for instance
*        if suitable WCS information was not stored with the existing DATA
*        picture), then alignment is attempted in PIXEL co-ordinates. If this
*        is not possible, then alignment is attempted in GRID co-ordinates. If
*        this is not possible, then alignment is attempted in the first
*        suitable Frame found in the catalogue irrespective of its domain.
*        A message is displayed indicating the domain in which alignment
*        occurred. If there are no suitable Frames in the catalogue then an
*        error is reported. [TRUE]
*     COLANG = LITERAL (Read)
*        The name of the catalogue column holding the orientation of each
*        vector. The values are considered to be in units of degrees unless
*        the UNITS attribute of the column has the value "Radians" (case
*        insensitive).  The angles are assumed to be measured anti-clockwise
*        from the reference direction specified in the catalogue. A list
*        of available column names is displayed if a non-existent column name
*        is given. See also parameter NEGATE. [ANG]
*     COLMAG = LITERAL (Read)
*        The name of the catalogue column holding the magnitude of each
*        vector. A list of available column names is displayed if a
*        non-existent column name is given. [P]
*     COLX = LITERAL (Read)
*        The name of the catalogue column which gives the position of each
*        vector along the first axis. A list of available column names is
*        displayed if a non-existent column name is given. See the "Notes"
*        section below for further details of how these positions are
*        interpreted. [X]
*     COLY = LITERAL (Read)
*        The name of the catalogue column which gives the position of each
*        vector along the second axis. A list of available column names is
*        displayed if a non-existent column name is given. See the "Notes"
*        section below for further details of how these positions are
*        interpreted. [Y]
*     COLZ = LITERAL (Read)
*        The name of the catalogue column which gives the position of each
*        vector along a third axis. A list of available column names is
*        displayed if a non-existent column name is given. A null (!)
*        value should be supplied if no third axis is to be used. The dynamic
*        default is 'Z' if the catalogue contains a Z column, and null
*        (!) otherwise. See also parameter ZAXVAL. []
*     DEVICE = DEVICE (Read)
*        The plotting device. [Current graphics device]
*     EPOCH = _DOUBLE (Read)
*        If a "Sky Co-ordinate System" specification is supplied (using
*        parameter FRAME) for a celestial co-ordinate system, then an epoch
*        value is needed to qualify it. This is the epoch at which the
*        supplied sky positions were determined. It should be given as a
*        decimal years value, with or without decimal places  ("1996.8" for
*        example). Such values are interpreted as a Besselian epoch if less
*        than 1984.0 and as a Julian epoch otherwise.
*     FILL = _LOGICAL (Read)
*        The DATA picture containing the vector map is usually produced with
*        the same shape as the data. However, for maps with markedly different
*        dimensions this default behaviour may not give the clearest result.
*        When FILL is TRUE, the smaller dimension of the picture is expanded
*        to produce the largest possible picture within the current picture.
*        [FALSE]
*     FRAME = LITERAL (Read)
*        This gives the co-ordinate Frame to be displayed along the annotated
*        axes (see parameter AXES). If a null parameter value is supplied,
*        then the current Frame in the supplied catalogue is used. The
*        string can be one of the following:
*        - A domain name such as SKY, AXIS, PIXEL, etc. The two
*        "pseudo-domains" WORLD and DATA may be supplied and will be
*        translated into PIXEL and AXIS respectively, so long as the WCS
*        FrameSet in the catalogue does not contain Frames with these domains.
*
*        - An integer value giving the index of the required Frame within
*        the WCS component.
*
*        - A "Sky Co-ordinate System" (SCS) value such as EQUAT(J2000) (see
*        section "Sky Co-ordinate Systems" in SUN/95). [!]
*     JUST = LITERAL (Read)
*        The justification for each vector; it can take any of the
*        following values:
*           - CENTRE -- the vectors are drawn centred on the
*           corresponding pixel coordinates.
*           - START -- the vectors are drawn starting at the
*           corresponding pixel coordinates.
*           - END -- the vectors are drawn ending at the corresponding
*           pixel coordinates.
*        ["Centre"]
*     KEY = _LOGICAL (Read)
*        TRUE if a key indicating the vector scale is to be produced. [TRUE]
*     KEYPOS() = _REAL (Read)
*        Two values giving the position of the key. The first value gives
*        the gap between the right hand edge of the vector map and the left
*        hand edge of the key (0.0 for no gap, 1.0 for the largest gap). A
*        positive value will place the key to the right of (i.e. outside)
*        the vector map, and a negative value will place the key inside
*        the vector map. The second value gives the vertical position of the
*        top of the key (1.0 for the highest position, 0.0 for the lowest).
*        If the second value is not given, the top of the key is placed
*        level with the top of the vector map. Both values should be in the
*        range 0.0 to 1.0. If a key is produced, then the right hand margin
*        specified by parameter MARGIN is ignored. [current value]
*     KEYSTYLE = GROUP (Read)
*        A group of attribute settings describing the plotting style to use
*        for the key (see parameter KEY).
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
*        By default the key starts with two lines of text, the first
*        being "Vector scale:" and the second giving a numerical value
*        for the scale in units per centimetre. These two lines may be
*        replaced by assigning alternative text to the Title attribute
*        using this parameter. If no text is required, either assign a
*        blank value for Title, or set the DrawTitle attribute to zero.
*
*        The appearance of the text in the key is controlled using "String"
*        attributes (e.g. COLOUR(STRINGS), FONT(STRINGS), etc - the synonym
*        TEXT can be used in place of STRINGS). Note, the Size attribute
*        specifies the size of key text relative to the size of the numerical
*        labels on the vector map axes. Thus a value of 2.0 for Size will
*        result in text which is twice the size of the numerical axis labels.
*        The appearance of the example vector is controlled using "Curve"
*        attributes (e.g. COLOUR(CURVES), etc - the synonym VECTOR can be
*        used in place of CURVES). The numerical scale value is formatted as
*        an axis 1 value (using attributes FORMAT(1), DIGITS(1), etc - the
*        synonym SCALE can be used in place of the value 1). The length of
*        the example vector is formatted as an axis 2 value (using attribute
*        FORMAT(2), etc - the synonym VECTOR can be used in place of the
*        value 2). The vertical space between lines in the key can be
*        controlled using attribute TextLabGap. A value of 1.0 is used if
*        no value is set for this attribute, and produces default vertical
*        spacing. Values larger than 1.0 increase the vertical space, and
*        values less than 1.0 decrease the vertical space. If the key is
*        drawn over the top of the vector map, the key will ne opaque by
*        default. The key can be made transparent by including the setting
*        "Colour(Back)=clear". [current value]
*     KEYVEC = _REAL (Read)
*        Length of the vector to be displayed in the key, in data units.
*        A default value is generated based on the spread of vector
*        lengths in the plot. []
*     LBND(2) = _REAL (Read)
*        The coordinates to put at the lower left corner of the plotting
*        area, in the coordinates system specified by parameters COLX and
*        COLY. If a null value is supplied then an area is used which just
*        encloses all the data in the supplied catalogue. [!]
*     MARGIN( 4 ) = _REAL (Read)
*        The widths of the margins to leave around the vector map for axis
*        annotation. The widths should be given as fractions of the
*        corresponding dimension of the current picture.
*        The actual margins used may be increased to preserve the aspect
*        ratio of the DATA picture. Four values may be given, in the order;
*        bottom, right, top, left. If fewer than four values are given,
*        extra values are used equal to the first supplied value. If these
*        margins are too narrow any axis annotation may be clipped. The
*        dynamic default is 0.15 (for all edges) if annotated axes are being
*        produced, and zero otherwise. See also parameter KEYPOS. []
*     NEGATE = _LOGICAL (Read)
*        If a TRUE value is supplied, then the angles giving the
*        orientation of the polarization (i.e. the values in the column
*        specified by parameter COLANG) are negated before adding on any
*        value specified by parameter ANGROT. [FALSE]
*     STYLE = GROUP (Read)
*        A group of attribute settings describing the plotting style to use
*        for the contours and annotated axes.
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
*        The appearance of the vectors is controlled by the attributes
*        Colour(Curves), Width(Curves), etc (the synonym Vectors may be
*        used in place of Curves). [current value]
*     UBND(2) = _REAL (Read)
*        The coordinates to put at the top right corner of the plotting
*        area, in the coordinates system specified by parameters COLX and
*        COLY. If a null value is supplied then an area is used which just
*        encloses all the data in the supplied catalogue. [!]
*     VSCALE = _REAL (Read)
*        The scale to be used for the vectors.  The supplied value
*        should give the data value corresponding to a vector length of
*        one centimetre.  If a negative value is supplied, all vectors
*        will be drawn with the same length (the fixed vector length is
*        proportional to the supplied VSCALE value). []
*     ZAXVAL = LITERAL (Read)
*        Specifies the Z axis value for the vectors to be displayed. The
*        given value should be in the current coordinate Frame of the
*        supplied catalogue (see parameter COLZ). For instance, if the
*        current coordinate Frame contains a calibrated wavelength axis,
*        the value should be given in the units specified in that frame
*        (Angstroms, nanometres, etc.). If the wavelength axis has not been
*        calibrated, the value will probably need to be supplied in units
*        of pixels. Entering a colon (":") for the parameter will result in
*        a description of the current coordinate Frame being shown. This may
*        help to determine the units in which a value is expected. The
*        value actually used is the closest available value within the
*        catalogue. This value is displayed on the screen and included in
*        the default plot title. The ZAXVAL parameter is only accessed if a
*        null (!) value is supplied for parameter ZCOLVAL. See also
*        parameter COLZ.
*     ZCOLVAL = _REAL (Read)
*        Specifies the Z column value for the vectors to be displayed.
*        The given value should be in the same coordinate system as the
*        values stored in the Z column of the catalogue (usually pixels).
*        This parameter provides an alternative to the ZAXVAL parameter.
*        Use the ZCOLVAL parameter to specify the Z value in pixels, and
*        the ZAXVAL parameter to specify the Z value in Hertz, angstroms,
*        nanometres, etc (if the Z axis has been calibrated). If a null
*        value is supplied for ZCOLVAL, then ZAXVAL is used to determine
*        the Z value to display. [!]
*
*  Examples:
*     polplot poltab
*        Produces a vector map on the current graphics device with
*        vectors defined in the FITS binary table "poltab". The magnitudes
*        are taken from column P, the orientations from column ANG and
*        the coordinates of each vector from columns X and Y.
*     polplot poltab style=^mystyle.dat
*        As above, but the annotated axes and vectors are drawn according
*        to the description given in text file mystyle.dat. If this
*        files contains the following lines:
*
*           title = My favorite colours
*           grid = 1
*           minticklen = 0
*           colour(border) = green
*           colour(grid) = blue
*           colour(vec) = red
*           width(border) = 0.05
*
*        then the title is set to "My favourite colours"; a grid is drawn
*        across the plot instead of tick marks around the edge; the border,
*        grid and vectors are drawn in green, blue and red respectively,
*        and slightly thicker lines are used to draw the border.
*     polplot poltab ra dec noclear angrot=90 frame=eq(B1950)
*        Produces a vector map in which each vector is rotated by 90
*	 degrees from the orientation specified in the "poltab"
*	 catalogue. The position of each vector is specified by columns
*	 "ra" and "dec". The annotated axes give equatorial (RA/DEC)
*	 coordinates referred to the equinox of B1950. If the vector map
*	 is displayed over an existing DATA picture, then the vector map
*	 will be aligned on the sky with the previously displayed data if
*	 possible (i.e. the FrameSet associated with the existing picture
*	 in the AGI database contains a sky coordinate Frame). If this is
*	 not possible, then the vector map will be aligned in pixel or
*	 grid coordinates. A message is displayed indicating the domain
*	 in which alignment took place.
*     polplot poltab arrow=0.01 just=start nokey
*        Produces a vector map in which each vector is represented by an
*        arrow, starting at the position of the corresponding pixel.  No key
*        to the vector scale and justification is produced.
*     polplot ABEB.FIT clear=no colx=ra coly=dec colmag=l colang=b
*        Reads the non-POLPACK FITS table in file ABEB.FIT, and displays a
*        vector for every row defined by the catalogue columns "l" and "b".
*        The position of each vector is given by columns "ra" and "dec".
*        If a previous DATA picture has been displayed, and has a calibration
*        in terms of any of the common sky coordinate systems, then the
*        vectors are aligned with the existing DATA picture.

*  Notes:
*     -  The TITLE parameter in the supplied catalogue is used as the default
*     title for the annotated axes. If the catalogue does not have a TITLE
*     parameter (of it is blank), then the default title is taken from current
*     co-ordinate Frame stored in the WCS component of the catalogue. This
*     default may be over-ridden by specifying a value for the Title
*     attribute using the STYLE parameter.
*     -  The columns specified by parameters COLX and COLY should hold
*     coordinates in the "Base Frame" of the WCS information stored as
*     an AST FrameSet (see SUN/210) in the supplied catalogue. If the
*     catalogue has been produced by one of the POLPACK application polvec
*     or polbin, then the Base Frame will be pixel co-ordinates within the
*     aligned intensity images, and these will be stored in columns with
*     names "X" and "Y". If the catalogue was not created by POLPACK, it
*     may have no usable WCS information, in which case the supplied
*     positions are mapped linearly onto the screen. There is one
*     exception to this; if the columns have names RA and DEC then they
*     are assumed to be equatorial sky coordinates with epoch and equinox
*     specified by the optional catalogue parameters EPOCH and EQUINOX
*     (defaults are used for these parameters if they are not present in the
*     catalogue). If the vector map is displayed over an existing DATA
*     picture (i.e. if CLEAR=NO) then these RA/DEC positions will be aligned
*     with the existing DATA picture if possible (i.e. if the existing
*     picture has sky coordinate information stored with it).

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

*  Authors:
*     BC: Brad Cavanagh (JAC, Hawaii)
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     5-FEB-1998 (DSB):
*        Original version, derived from kappa:vecplot.
*     5-AUG-1998 (DSB):
*        Correct the DOMAIN passed to KPG1_PLOT so that it refers to the
*        Base Frame of the WCS info, instead of the Current Frame. Changed
*        the interpretation of parameters COLX and COLY accordingly. Defaults
*        for LBND and UBND changed.
*     9-NOV-1998 (DSB):
*        Modified to bring the handling of WCS and graphics into line with
*        KAPPA V0.13.
*     17-FEB-1999 (DSB):
*        Changed the estimation of the "typical" magnitude to be the 90%
*        percentile value. This is used as the default for the VSCALE
*        parameter.
*     22-MAR-1999 (DSB):
*        Get AXES *before* MARGIN.
*     3-NOV-1999 (DSB):
*        Margin changed to be a fraction of the current picture instead
*        of the DATA picture.
*     11-AUG-2000 (DSB):
*        Modified to allow negative KEYPOS values and use of Title
*        attribute in KEYSTYLE to set the key text.
*     22-FEB-2001 (DSB):
*        Modified to support 3D data.
*     11-DEC-2003 (DSB):
*        Clear the unnecessary Format attribute values set in KPG1_GDGET for
*        the key style.
*     22-SEP-2004 (TIMJ):
*        Use CNF_PVAL
*     16-AUG-2006 (BC):
*        Shut down the graphics database using KPG1_PGCLS instead of
*        AGP_DEASS, which doesn't seem to clear the AGI database properly.
*     13-APR-2007 (DSB):
*        Allow the key to be transparent.
*     14-DEC-2020 (DSB):
*        Allow negative VSCALE values to be used to indicate that all
*        vectors should be drawn with the same length.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'AST_PAR'          ! AST_ constants and function declarations
      INCLUDE 'CAT_PAR'          ! CAT_ constants
      INCLUDE 'NDF_PAR'          ! NDF_ constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:
      INTEGER CUNITS             ! Maximum number of characters in units
      PARAMETER( CUNITS = 30 )   ! string

      REAL  DTOR                 ! Degrees to radians conversion factor
      PARAMETER ( DTOR = 1.7453293E-2 )

      REAL  KW                   ! Width of key as a fraction of the
      PARAMETER ( KW = 0.2 )     ! current picture.

      INTEGER NBIN               ! No. of bins in histogram used to
      PARAMETER( NBIN = 1000 )   ! find typical magnitude value

      REAL  NVEC0                ! Default no. of vectors along short
      PARAMETER ( NVEC0 = 15.0 ) ! axis

*  Local Variables:
      CHARACTER COLNM*30         ! External column name
      CHARACTER DOMAIN*30        ! Domain containing vector positions
      CHARACTER FIELDS( 5 )*50   ! Individual fields of catalogue specification
      CHARACTER JUST*6           ! Vector justification: CENTRE or START
      CHARACTER NAME*(CAT__SZCMP)! CAT column name
      CHARACTER TITLE*80         ! Title from input catalogue
      CHARACTER UNITS1*( CUNITS )! Units of the data
      CHARACTER UNITS2*( CUNITS )! Units of the data
      CHARACTER ZTEXT*80         ! Text giving Z value
      CHARACTER ZBTEXT*80        ! Text giving base Frame Z value
      CHARACTER ZCTEXT*80        ! Text giving current Frame Z value
      DOUBLE PRECISION ATTRS( 20 )! Saved graphics attributes
      DOUBLE PRECISION BOX( 4 )  ! Bounds of used region of (X,Y) axes
      DOUBLE PRECISION CONST( 3 )! Constant axis values
      DOUBLE PRECISION DX        ! Extension of X axis
      DOUBLE PRECISION DY        ! Extension of Y axis
      DOUBLE PRECISION LBND      ! Lower axis bound
      DOUBLE PRECISION UBND      ! Upper axis bound
      DOUBLE PRECISION Z         ! Value of Z parameter
      INTEGER CI                 ! CAT catalogue identifier
      INTEGER FRM                ! Frame pointer
      INTEGER GI                 ! CAT column identifier
      INTEGER GIANG              ! CAT identifier for ANG column
      INTEGER GIMAG              ! CAT identifier for MAG column
      INTEGER GIS( 3 )           ! CAT identifiers for X,Y,Z catalogue columns
      INTEGER GTTL               ! CAT identifier for TITLE parameter
      INTEGER I                  ! Loop count
      INTEGER IAT                ! Used length of a string
      INTEGER ICAT               ! Index of (X,Y) Frame in IWCS and IPLOT
      INTEGER IFRM               ! Frame index
      INTEGER IPANG              ! Pointer to array holding vector angles
      INTEGER IPICD              ! AGI id. for DATA picture
      INTEGER IPICF              ! AGI id. for new FRAME picture
      INTEGER IPICK              ! AGI id. for the KEY picture
      INTEGER IPLOT              ! Pointer to AST Plot for DATA picture
      INTEGER IPLOTK             ! Pointer to AST Plot for KEY picture
      INTEGER IPMAG              ! Pointer to array holding vector magnitudes
      INTEGER IPX                ! Pointer to array holding vector X positions
      INTEGER IPX2               ! Pointer to array holding vector X positions
      INTEGER IPY                ! Pointer to array holding vector Y positions
      INTEGER IPY2               ! Pointer to array holding vector Y positions
      INTEGER IPZ                ! Pointer to array holding vector Z positions
      INTEGER IWCS               ! Pointer to AST FrameSet read from catalogue
      INTEGER MAP                ! Mapping pointer
      INTEGER MAPS( 3 )          ! Individual axis mappings
      INTEGER MAXPOS             ! Position of maximum value
      INTEGER MINPOS             ! Position of minimum value
      INTEGER NBAD               ! No. of bad values
      INTEGER NDIM               ! No. of input axes
      INTEGER NFRM               ! Frame index increment between IWCS and IPLOT
      INTEGER NGANG              ! No. of good vector angle values
      INTEGER NGMAG              ! No. of good vector magnitude values
      INTEGER NGX                ! No. of good vector X values
      INTEGER NGY                ! No. of good vector Y values
      INTEGER NGZ                ! No. of good vector Z values
      INTEGER NIN                ! No. of positions in user-specified bounds
      INTEGER NKP                ! No. of values supplied for parameter KEYPOS
      INTEGER NMARG              ! No. of margin values given
      INTEGER NVAL               ! No. of values obtained
      INTEGER NVEC               ! No. of vectors in catalogue
      LOGICAL ALIGN              ! Was DATA pic aligned with an old DATA pic?
      LOGICAL AXES               ! Annotated axes to be drawn?
      LOGICAL CLRKEY             ! Clear the key background?
      LOGICAL GOTZ               ! Was a Z column supplied?
      LOGICAL HINIT              ! Initialise histogram?
      LOGICAL KEY                ! A key to vector scale to be plotted?
      LOGICAL NEGATE             ! Negate supplied angles?
      LOGICAL V2PLUS             ! Catalogue created by POLPACK V2 or later?
      LOGICAL VERB               ! Verose errors required?
      LOGICAL ZCURR              ! Was the Z value given in the current Frame?
      REAL AHSIZE                ! Arrowhead size in world co-ordinates
      REAL AHSIZM                ! Arrowhead size in metres
      REAL ANGFAC                ! Supplied angles to radians conversion factor
      REAL ANGROT                ! Angle to add on to supplied angles
      REAL ASPECT                ! Aspect ratio for new DATA pictures
      REAL BHI( 2 )              ! Upper bounds of plotting space
      REAL BLO( 2 )              ! Lower bounds of plotting space
      REAL DEFSCA                ! Default value for VSCALE
      REAL DMAX                  ! Highest value in histogram
      REAL DMIN                  ! Lowest value in histogram
      REAL DSCALE                ! Vector scale, viz. data units per pixel
      REAL DUMMY                 ! Unused argument
      REAL HGT                   ! Character height scale factor
      REAL HIST( NBIN )          ! Histogram array
      REAL KEYOFF                ! Offset to top of key
      REAL KEYPOS( 2 )           ! Key position
      REAL MARGIN( 4 )           ! Margins round DATA picture
      REAL REFANG                ! ACW angle from +ve X to ref direction
      REAL SZHI                  ! Max Z value in catalogue
      REAL SZLO                  ! Min Z value in catalogue
      REAL TYPDAT                ! A typical vector data value
      REAL VSCALE                ! Vector scale, viz. data units per cm
      REAL X1                    ! Lower x w.c. bound of picture
      REAL X2                    ! Upper x w.c. bound of picture
      REAL XM                    ! DATA zone x size in metres
      REAL Y1                    ! Lower y w.c. bound of picture
      REAL Y2                    ! Upper y w.c. bound of picture
      REAL YM                    ! DATA zone y size in metres
      REAL ZUSE                  ! Z column value to display

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  See if the user wants verbose error messages.
      CALL KPG1_VERB( VERB, 'POLPACK', STATUS )

*  Open the input catalogue, and get its name.
      CALL CTG_ASSO1( 'CAT', VERB, 'READ', CI, FIELDS, STATUS )

*  Set up dynamic defaults for the column names.
      CALL POL1_COLNM( 'P', .FALSE., COLNM, STATUS )
      IF( COLNM .NE. ' ' ) CALL PAR_DEF0C( 'COLMAG', COLNM, STATUS )

      CALL POL1_COLNM( 'ANG', .FALSE., COLNM, STATUS )
      IF( COLNM .NE. ' ' ) CALL PAR_DEF0C( 'COLANG', COLNM, STATUS )

      CALL POL1_COLNM( 'X', .FALSE., COLNM, STATUS )
      IF( COLNM .NE. ' ' ) CALL PAR_DEF0C( 'COLX', COLNM, STATUS )

      CALL POL1_COLNM( 'Y', .FALSE., COLNM, STATUS )
      IF( COLNM .NE. ' ' ) CALL PAR_DEF0C( 'COLY', COLNM, STATUS )

*  Attempt to get an identifier for the Z column.
      CALL POL1_COLNM( 'Z', .FALSE., COLNM, STATUS )
      IF( COLNM .NE. ' ' .AND. STATUS .EQ. SAI__OK ) THEN
         CALL CAT_TIDNT( CI, COLNM, GIS( 3 ), STATUS )

*  If found, release the CAT identifier and use a dynamic default of COLNM
*  for parameter COLZ. Otherwise, annul the error and use no dynamic
*  default.
         IF( STATUS .EQ. SAI__OK ) THEN
            CALL CAT_TRLSE( GIS( 3 ), STATUS )
            CALL PAR_DEF0C( 'COLZ', COLNM, STATUS )
         ELSE
            CALL ERR_ANNUL( STATUS )
         END IF

      END IF

*  Get CAT identifiers for the columns which are to be used to define the
*  vector magnitudes, orientations, X and Y coordinates.
      CALL POL1_GTCTC( 'COLMAG', CI, CAT__FITYP, ' ', GIMAG, STATUS )
      CALL POL1_GTCTC( 'COLANG', CI, CAT__FITYP, ' ', GIANG, STATUS )
      CALL POL1_GTCTC( 'COLX', CI, CAT__FITYP, ' ', GIS( 1 ), STATUS )
      CALL POL1_GTCTC( 'COLY', CI, CAT__FITYP, ' ', GIS( 2 ), STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to get an identifier for the Z column. If a null (!) value is
*  given, annul the error and indicate no Z column is to be used.
      CONST( 1 ) = AST__BAD
      CONST( 2 ) = AST__BAD
      CALL POL1_GTCTC( 'COLZ', CI, CAT__FITYP, ' ', GIS( 3 ), STATUS )

      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         GOTZ = .FALSE.
         NDIM = 2
      ELSE
         GOTZ = .TRUE.
         NDIM = 3
      END IF

*  Find the number of rows in the catalogue. This is the number of
*  vectors to be plotted.
      CALL CAT_TROWS( CI, NVEC, STATUS )

*  Allocate workspace.
      CALL PSX_CALLOC( NVEC, '_REAL', IPMAG, STATUS )
      CALL PSX_CALLOC( NVEC, '_REAL', IPANG, STATUS )
      CALL PSX_CALLOC( NVEC, '_DOUBLE', IPX, STATUS )
      CALL PSX_CALLOC( NVEC, '_DOUBLE', IPY, STATUS )
      CALL PSX_CALLOC( NVEC, '_DOUBLE', IPX2, STATUS )
      CALL PSX_CALLOC( NVEC, '_DOUBLE', IPY2, STATUS )

      IF( GOTZ ) THEN
         CALL PSX_CALLOC( NVEC, '_REAL', IPZ, STATUS )
      ELSE
         IPZ = IPMAG
      END IF

*  Check the pointers can be used.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Copy each column into the corresponding array.
      CALL POL1_CPCTR( CI, GIMAG, NVEC, %VAL( CNF_PVAL( IPMAG ) ),
     :                 NGMAG, STATUS )
      CALL POL1_CPCTR( CI, GIANG, NVEC, %VAL( CNF_PVAL( IPANG ) ),
     :                 NGANG, STATUS )
      CALL POL1_CPCTD( CI, GIS( 1 ), NVEC, %VAL( CNF_PVAL( IPX ) ),
     :                 NGX, STATUS )
      CALL POL1_CPCTD( CI, GIS( 2 ), NVEC, %VAL( CNF_PVAL( IPY ) ),
     :                 NGY, STATUS )
      IF( GOTZ ) CALL POL1_CPCTR( CI, GIS( 3 ), NVEC,
     :                            %VAL( CNF_PVAL( IPZ ) ), NGZ,
     :                            STATUS )

*  Check the global status.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Report an error if any of the columns contain no good data.
      GI = CAT__NOID
      IF( NGMAG .EQ. 0 ) THEN
         GI = GIMAG
         CALL MSG_SETC( 'COL', 'vector magnitude' )

      ELSE IF( NGANG .EQ. 0 ) THEN
         GI = GIANG
         CALL MSG_SETC( 'COL', 'vector orientation' )

      ELSE IF( NGX .EQ. 0 ) THEN
         GI = GIS( 1 )
         CALL MSG_SETC( 'COL', 'X coordinate' )

      ELSE IF( NGY .EQ. 0 ) THEN
         GI = GIS( 2 )
         CALL MSG_SETC( 'COL', 'Y coordinate' )

      ELSE IF( NGZ .EQ. 0 ) THEN
         GI = GIS( 3 )
         CALL MSG_SETC( 'COL', 'Z coordinate' )

      END IF

      IF( GI .NE. CAT__NOID ) THEN
         CALL CAT_TIQAC( GI, 'NAME', NAME, STATUS )
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'NAME', NAME )
         CALL ERR_REP( 'POLPLOT_1', 'The ^COL column '//
     :                 '(^NAME) contains no data.', STATUS )
      END IF

*  Attempt to read an AST FrameSet from the catalogue. The Base Frame of
*  this FrameSet will be spanned by axes corresponding to the columns
*  in GIS.
      CALL POL1_GTCTA( CI, NDIM, GIS, IWCS, STATUS )

*  If required, get the Z column value to use. Only vectors with this
*  value of Z are displayed.
      IF( GOTZ .AND. STATUS .EQ. SAI__OK ) THEN

*  Get the name of the Z column.
         CALL CAT_TIQAC( GIS( 3 ), 'NAME', NAME, STATUS )

*  Split the Base Frame to Current Frame Mapping up into 3 separate
*  1D Mappings.
         CALL KPG1_ASSPL( IWCS, 3, MAPS, STATUS )

*  Find the max and min Z values in the Z column.
         CALL KPG1_MXMNR( .TRUE., NVEC, %VAL( CNF_PVAL( IPZ ) ),
     :                    NBAD, SZHI,
     :                    SZLO, MAXPOS, MINPOS, STATUS )

*  If there is only a single Z value available, use it.
         IF( SZHI .LE. SZLO ) THEN
            ZUSE = SZLO
            ZCURR = .TRUE.

*  Otherwise, allow the user to choose a Z value.
         ELSE IF( STATUS .EQ. SAI__OK ) THEN

*  First see if the user wants to give the Z value in pixels.
            CALL PAR_GET0D( 'ZCOLVAL', Z, STATUS )

*  If so, indicate that a base frame Z value has been given.
            IF( STATUS .EQ. SAI__OK ) THEN
               ZCURR = .FALSE.

*  Otherwise, annul the error and get a current Frame Z value.
            ELSE IF( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )

*  Get the required Z value in the current Frame.
               CALL KPG1_GTAXV( 'ZAXVAL', 1, .TRUE., IWCS, 3, Z,
     :                          NVAL, STATUS )

*  Use any third Mapping to transform the current Frame Z value into a
*  base Frame Z value.
               IF( MAPS( 3 ) .NE. AST__NULL ) THEN
                  CALL AST_TRAN1( MAPS( 3 ), 1, Z, .FALSE., Z, STATUS )
               ELSE
                  Z = AST__BAD
               END IF

*  If the result was undefined, we need to get the Z value again, this
*  time in the base Frame.
               IF( Z .EQ. AST__BAD ) THEN
                  ZCURR = .FALSE.
                  CALL PAR_GET0C( 'ZAXVAL', ZTEXT, STATUS )

                  CALL MSG_BLANK( STATUS )
                  CALL MSG_SETC( 'NAME', NAME )
                  CALL MSG_SETC( 'Z', ZTEXT )
                  CALL MSG_OUT( 'POLPLOT_MSG1', 'The supplied '//
     :                    'value for the ZAXVAL parameter (^Z) could '//
     :                    'not be converted into a value for the '//
     :                    '^NAME column in the catalogue. Please '//
     :                    'supply a value for the ZCOLVAL parameter '//
     :                    'in the same coordinate system as the '//
     :                    'values in the ^NAME column:', STATUS )

                  CALL PAR_CANCL( 'ZCOLVAL', STATUS )
                  CALL PAR_GET0D( 'ZCOLVAL', Z, STATUS )
               ELSE
                  ZCURR = .TRUE.
               END IF

            END IF

*  Find the closest available value to the requested Z value.
            CALL POL1_FCLOS( NVEC, %VAL( CNF_PVAL( IPZ ) ),
     :                       REAL( Z ), ZUSE,
     :                       STATUS )
         END IF

*  Produce some text describing the Z value. If possible, this includes
*  both the current Frame and the Base Frame values, the order being
*  chosen on the basis of whether a current or base Frame value was supplied
*  by the user.
         ZBTEXT = ' '
         IAT = 0
         CALL CHR_APPND( NAME, ZBTEXT, IAT )
         CALL CHR_APPND( ' =', ZBTEXT, IAT )
         IAT = IAT + 1
         CALL CHR_PUTR( ZUSE, ZBTEXT, IAT )

         IF( MAPS( 3 ) .NE. AST__NULL ) THEN
            CALL AST_TRAN1( MAPS( 3 ), 1, DBLE( ZUSE ), .TRUE., Z,
     :                      STATUS )
         ELSE
            Z = AST__BAD
         END IF

         ZCTEXT = ' '
         IAT = 0
         IF( Z .NE. AST__BAD ) THEN
            CALL CHR_APPND( AST_GETC( IWCS, 'SYMBOL(3)', STATUS ),
     :                      ZCTEXT, IAT )
            CALL CHR_APPND( ' =', ZCTEXT, IAT )
            IAT = IAT + 1
            CALL CHR_APPND( AST_FORMAT( IWCS, 3, Z, STATUS ), ZCTEXT,
     :                      IAT )
            IAT = IAT + 1
            CALL CHR_APPND( AST_GETC( IWCS, 'UNIT(3)', STATUS ),
     :                      ZCTEXT, IAT )
         END IF

         ZTEXT = ' '
         IAT = 0
         IF( ZCURR ) THEN
            IF( ZCTEXT .NE. ' ' ) THEN
               CALL CHR_APPND( ZCTEXT, ZTEXT, IAT )
               CALL CHR_APPND( ' (', ZTEXT, IAT )
               CALL CHR_APPND( ZBTEXT, ZTEXT, IAT )
               CALL CHR_APPND( ')', ZTEXT, IAT )
            ELSE
               ZTEXT = ZBTEXT
            END IF
         ELSE
            CALL CHR_APPND( ZBTEXT, ZTEXT, IAT )
            IF( ZCTEXT .NE. ' ' ) THEN
               CALL CHR_APPND( ' (', ZTEXT, IAT )
               CALL CHR_APPND( ZCTEXT, ZTEXT, IAT )
               CALL CHR_APPND( ')', ZTEXT, IAT )
            END IF
         END IF

*  Display the Z value being used.
         CALL MSG_BLANK( STATUS )
         CALL MSG_SETC( 'Z', ZTEXT )
         CALL MSG_OUT( 'POLPLOT_MSG', '  Using ^Z', STATUS )

*  Modify the WCS FrameSet so that the Base Frame has only two axes (the
*  original Base Frame may have 3 when dealing with spectropolarimtry data).
         CONST( 3 ) = DBLE( ZUSE )
         CALL POL1_PKAXS( NDIM, GIS, CONST, IWCS, STATUS )

*  If there is no Z column, store a blank string for the textual descxription
*  of the Z value being used.
      ELSE
         ZTEXT = ' '
         ZUSE = VAL__BADR
      END IF

*  Give the user the chance to change the Current Frame.
      CALL KPG1_ASFRM( 'FRAME', 'EPOCH', IWCS, 'PIXEL', 'AXIS', .TRUE.,
     :                 'catalogue', STATUS )

*  Obtain the units of the magnitude column.
      UNITS1 = ' '
      CALL POL1_TIQAC( GIMAG, 'UNITS', UNITS1, STATUS )

*  Obtain the units of the orientation column.
      UNITS2 = ' '
      CALL POL1_TIQAC( GIANG, 'UNITS', UNITS2, STATUS )

*  Set up a factor which converts values stored in the orientation column
*  into units of radians.  If the UNITS attribute does not have the value
*  "RADIANS" (case insensitive), then assume the data values are in degrees.
      IF ( UNITS2 .NE. ' ' ) THEN
         CALL CHR_RMBLK( UNITS2 )
         CALL CHR_UCASE( UNITS2 )
         IF ( UNITS2 .EQ. 'RADIANS' ) THEN
            ANGFAC = 1.0
         ELSE
            ANGFAC = DTOR
         END IF
      ELSE
         ANGFAC = DTOR
      END IF

*  Remove data outside user-specified bounds.
*  ==========================================

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the lower and upper bounds of the area to be included in the plot,
*  in terms of the COLX and COLY values.
      CALL PAR_EXACR( 'LBND', 2, BLO, STATUS )
      CALL PAR_EXACR( 'UBND', 2, BHI, STATUS )

*  If a null value was supplied, the entire data set is used...
      IF( STATUS .EQ. PAR__NULL ) THEN

*  Annul the error.
         CALL ERR_ANNUL( STATUS )

*  Find the maximum and minimum X value.
         CALL KPG1_MXMND( ( NGX .LT. NVEC ), NVEC,
     :                    %VAL( CNF_PVAL( IPX ) ), NBAD,
     :                    UBND, LBND, MAXPOS, MINPOS, STATUS )
         BHI( 1 ) = REAL( UBND )
         BLO( 1 ) = REAL( LBND )

*  Find the maximum and minimum Y value.
         CALL KPG1_MXMND( ( NGY .LT. NVEC ), NVEC,
     :                    %VAL( CNF_PVAL( IPY ) ), NBAD,
     :                    UBND, LBND, MAXPOS, MINPOS, STATUS )
         BHI( 2 ) = REAL( UBND )
         BLO( 2 ) = REAL( LBND )

*  Extend these bounds slightly.
         DX = 0.005*( BHI ( 1 ) - BLO( 1 ) )
         IF( DX .EQ. 0 ) DX = 0.005*BHI( 1 )
         IF( DX .EQ. 0 ) DX = 1.0
         BLO( 1 ) = BLO( 1 ) - DX
         BHI( 1 ) = BHI( 1 ) + DX

         DY = 0.005*( BHI ( 2 ) - BLO( 2 ) )
         IF( DY .EQ. 0 ) DY = 0.005*BHI( 2 )
         IF( DY .EQ. 0 ) DY = 1.0
         BLO( 2 ) = BLO( 2 ) - DY
         BHI( 2 ) = BHI( 2 ) + DY

*  Otherwise ensure the bounds are the correct way round.
      ELSE
         IF( BLO( 1 ) .GT. BHI( 1 ) ) THEN
            DUMMY = BLO( 1 )
            BLO( 1 ) = BHI( 1 )
            BHI( 1 ) = DUMMY
         END IF

         IF( BLO( 2 ) .GT. BHI( 2 ) ) THEN
            DUMMY = BLO( 2 )
            BLO( 2 ) = BHI( 2 )
            BHI( 2 ) = DUMMY
         END IF

      END IF

*  Remove any positions outside these bounds. This also shuffles bad
*  positions to the end, and counts the number of good positions.
      CALL POL1_RMBND( NVEC, BLO, BHI, ZUSE, %VAL( CNF_PVAL( IPZ ) ),
     :                 %VAL( CNF_PVAL( IPMAG ) ),
     :                 %VAL( CNF_PVAL( IPANG ) ),
     :                 %VAL( CNF_PVAL( IPX ) ), %VAL( CNF_PVAL( IPY ) ),
     :                 NIN,
     :                 STATUS )

*  Report an error if there are no vectors in the selected region.
      IF( NIN .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'POLPLOT_2', 'There are no vectors to plot.',
     :                 STATUS )
         GO TO 999
      END IF

*  Find a representative data value.
*  =================================
*  Find the approximate 90% percentile of the magnitude values.
      DMAX = 0.0
      DMIN = 0.0
      HINIT = .TRUE.
      CALL POL1_HIST( NIN, %VAL( CNF_PVAL( IPMAG ) ),
     :                0.9, NBIN, .TRUE., HIST,
     :                DMIN, DMAX, HINIT, TYPDAT, STATUS )

      IF( TYPDAT .EQ. VAL__BADR .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'POLPLOT_3a', 'All vector magnitudes are bad.',
     :                 STATUS )

      ELSE IF( TYPDAT .EQ. 0.0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'POLPLOT_3b', 'All vector magnitudes are zero.',
     :                 STATUS )
      END IF

*  Prepare to produce graphics using AST and PGPLOT.
*  =================================================

*  Get the index of the Frame corresponding to the catalogue columns
*  specifying the vector positions (i.e. the Base Frame in the above
*  FrameSet).
      ICAT = AST_GETI( IWCS, 'BASE', STATUS )

*  Look for a 2D PIXEL Frame in the FrameSet.
      IFRM = AST__NOFRAME
      DO I = 1, AST_GETI( IWCS, 'NFRAME', STATUS )
         FRM = AST_GETFRAME( IWCS, I, STATUS )
         IF( AST_GETI( FRM, 'NAXES', STATUS ) .EQ. 2 .AND.
     :       AST_GETC( FRM, 'DOMAIN', STATUS ) .EQ. 'PIXEL' ) THEN
            IFRM = I
         END IF
         CALL AST_ANNUL( FRM, STATUS )
      END DO

*  If a 2D PIXEL Frame was found...
      IF( IFRM .NE. AST__NOFRAME ) THEN

*  The extent of the Plot will be specified in the PIXEL Frame.
         DOMAIN = 'PIXEL'

*  We now need to find the extent of the data in the PIXEL Frame (BLO and
*  BHI currently contain the extent of the data in the Base Frame). Get
*  the Mapping from the Base Frame to the 2D PIXEL Frame.
         MAP = AST_GETMAPPING( IWCS, AST__BASE, IFRM, STATUS )

*  If this Mapping is not a UnitMap, transform the supplied catalogue
*  positions into PIXEL positions using this Mapping.
         IF( .NOT. AST_ISAUNITMAP( MAP, STATUS ) ) THEN
            CALL AST_TRAN2( MAP, NIN, %VAL( CNF_PVAL( IPX ) ),
     :                      %VAL( CNF_PVAL( IPY ) ), .TRUE.,
     :                      %VAL( CNF_PVAL( IPX2 ) ),
     :                      %VAL( CNF_PVAL( IPY2 ) ), STATUS )

*  Find the maximum and minimum X PIXEL value.
            CALL KPG1_MXMND( ( NIN .LT. NVEC ), NIN,
     :                       %VAL( CNF_PVAL( IPX2 ) ), NBAD,
     :                      UBND, LBND,  MAXPOS, MINPOS, STATUS )
            BHI( 1 ) = REAL( UBND )
            BLO( 1 ) = REAL( LBND )

*  Find the maximum and minimum Y PIXEL value.
            CALL KPG1_MXMND( ( NIN .LT. NVEC ), NIN,
     :                       %VAL( CNF_PVAL( IPY2 ) ), NBAD,
     :                      UBND, LBND, MAXPOS, MINPOS, STATUS )
            BHI( 2 ) = REAL( UBND )
            BLO( 2 ) = REAL( LBND )

*  Extend these bounds slightly.
            DX = 0.005*( BHI ( 1 ) - BLO( 1 ) )
            BLO( 1 ) = BLO( 1 ) - DX
            BHI( 1 ) = BHI( 1 ) + DX

            DY = 0.005*( BHI ( 2 ) - BLO( 2 ) )
            BLO( 2 ) = BLO( 2 ) - DY
            BHI( 2 ) = BHI( 2 ) + DY
         END IF

*  If no 2D PIXEL Frame was found...
      ELSE

*  The extent of the Plot will be specified in the BASE Frame. BLO and
*  BHI already contain the extent of the data in the Base Frame.
         DOMAIN = AST_GETC( AST_GETFRAME( IWCS, ICAT, STATUS ),
     :                      'DOMAIN', STATUS )
      END IF

*  Produce double precision versions of the bounds for use with AST.
      BOX( 1 ) = DBLE( BLO( 1 ) )
      BOX( 2 ) = DBLE( BLO( 2 ) )
      BOX( 3 ) = DBLE( BHI( 1 ) )
      BOX( 4 ) = DBLE( BHI( 2 ) )

*  Get the aspect ratio of the box (assuming equal scales on each axis).
      ASPECT = ( BOX( 4 ) - BOX( 2 ) )/( BOX( 3 ) - BOX( 1 ) )

*  Establish a synonym of VECTORS (minimum abbreviation VEC) for the AST
*  graphical element name CURVES.
      CALL KPG1_ASPSY( '(VEC*TORS)', '(CURVES)', STATUS )

*  See if axes are required
      CALL PAR_GET0L( 'AXES', AXES, STATUS )

*  Set the dynamic defaults for MARGIN.
      IF( AXES ) THEN
         CALL PAR_DEF1R( 'MARGIN', 1, 0.15, STATUS )
      ELSE
         CALL PAR_DEF1R( 'MARGIN', 1, 0.0, STATUS )
      END IF

      CALL PAR_GDRVR( 'MARGIN', 4, 0.0, 1.0, MARGIN, NMARG, STATUS )
      NMARG = MIN( 4, NMARG )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Use the first value for any unspecified edges.
      DO I = NMARG + 1, 4
         MARGIN( I ) = MARGIN( 1 )
      END DO

*  See if a key to the vector magnitude scale is required.
      CALL PAR_GTD0L( 'KEY', .TRUE., .TRUE., KEY, STATUS )

*  Start up the graphics system, and create an AST Plot. Leave room for a
*  key if required. A pointer to the Plot is returned, together with AGI
*  identifiers for the FRAME, DATA and KEY pictures. Note, to get the index of
*  a Frame (eg ICAT) in the returned Plot, you add the returned value of
*  NFRM onto its index in IWCS.  The PGPLOT viewport is set up so that it
*  corresponds to the DATA picture.
      CLRKEY = .FALSE.
      IF( KEY ) THEN

*  Get the position required for the key. The margin between DATA and KEY
*  Frames is determined by the horizontal position requested for the key.
         CALL PAR_GDRVR( 'KEYPOS', 2, -1.0, 1.0, KEYPOS, NKP, STATUS )
         IF( KEYPOS( 1 ) .GE. 0 ) THEN
            MARGIN( 2 ) = KEYPOS( 1 )
         ELSE
            MARGIN( 2 ) = KEYPOS( 1 ) - KW
            CLRKEY = .TRUE.
         END IF

*  Start up the graphics system, creating a KEY picture.
         CALL KPG1_PLOT( IWCS, 'UNKNOWN', 'POLPACK_POLPLOT', ' ',
     :                   MARGIN, 1, 'KEY', 'R', KW, ASPECT, DOMAIN,
     :                   BOX, IPICD, IPICF, IPICK, IPLOT, NFRM, ALIGN,
     :                   STATUS )

*  Otherwise, start up the graphics system, creating no KEY picture.
      ELSE
         CALL KPG1_PLOT( IWCS, 'UNKNOWN', 'POLPACK_POLPLOT', ' ',
     :                   MARGIN, 0, ' ', ' ', 0.0, ASPECT, DOMAIN, BOX,
     :                   IPICD, IPICF, IPICK, IPLOT, NFRM, ALIGN,
     :                   STATUS )
      END IF

*  Find the index of the Frame within the Plot corresponding to the
*  catalogue columns specifying the vector positions.
      ICAT = ICAT + NFRM

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the TITLE parameter (if any) from the input catalogue.
      CALL CAT_TIDNT( CI, 'TITLE', GTTL, STATUS )
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL CAT_TIQAC( GTTL, 'VALUE', TITLE, STATUS )
         CALL CAT_TRLSE( GTTL, STATUS )
         IAT = CHR_LEN( TITLE )
         CALL CHR_APPND( ':', TITLE, IAT )
         IAT = IAT + 1
         CALL CHR_APPND( ZTEXT, TITLE, IAT )
      ELSE
         TITLE = ZTEXT
         CALL ERR_ANNUL( STATUS )
      END IF

*  If the user did not specify a Plot title (as indicated by the Plot title
*  being the same as the WCS title), make the catalogue Title the default
*  Title for the Plot. We have to be careful about the timing of this change
*  to the Title. If we did it before KPG1_PLOT (i.e. if we set the Title in
*  IWCS) it may prevent alignment ocurring within KPG1_PLOT since alignment
*  fails if the Title of two Frames differ (??).
      IF( AST_GETC( IWCS, 'TITLE', STATUS ) .EQ.
     :    AST_GETC( IPLOT, 'TITLE', STATUS ) ) THEN

         IF( TITLE .NE. ' ' ) THEN
            CALL AST_SETC( IPLOT, 'TITLE', TITLE( : CHR_LEN( TITLE ) ),
     :                     STATUS )
         END IF

      END IF

*  Obtain the vector-plot characteristics.
*  =======================================

*  First find the ACW angle from the +ve X axis to the reference direction.
*  See if the input catalogue was created by V2.0 or later of POLPACK.
      CALL POL1_GTVRC( CI, '2.0', V2PLUS, STATUS )

*  If the catalogue was created before V2.0 of POLPACK, the reference
*  direction will be the +ve X axis.
      IF( .NOT. V2PLUS ) THEN
         REFANG = 0.0

*  Otherwise, for V2 catalogues (or later), get the angle from the POLANAL
*  Frame in the catalogues WCS FrameSet...
      ELSE
         REFANG = 0.0
         CALL POL1_GTANG( NDF__NOID, CI, IWCS, REFANG, STATUS )
      END IF

*  Convert to radians.
      REFANG = REFANG * DTOR

*  Get the angle (in degrees) which is to be added to the values stored
*  in the supplied catalogue. Do not set a dynamic default. Constrain to
*  0 to 360 degrees.
      CALL PAR_GDR0R( 'ANGROT', -1.0, 0.0, 360.0, .FALSE., ANGROT,
     :                STATUS )

*  Convert to radians.
      ANGROT = ANGROT * DTOR

*  See if the angles are clockwise (i.e. if they are to be negated before
*  being used).
      CALL PAR_GET0L( 'NEGATE', NEGATE, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the bounds of the viewport in millimetres, and convert to metres.
      CALL PGQVP( 2, X1, X2, Y1, Y2 )
      XM = ( X2 - X1 )/1000.0
      YM = ( Y2 - Y1 )/1000.0

*  Get the bounds of the PGPLOT window (this will also be in meillimetres).
      CALL PGQWIN( X1, X2, Y1, Y2 )

*  Establish the default value for the vector scaling factor such that
*  a typical data value corresponds to a vector equal to one 15th of
*  the smallest DATA zone dimension, and then get a new value. If a value of
*  zero is supplied, use the default value.  XM is measured in metres so 100
*  time converts to centimetres. If a negative scale is supplied, all
*  vectors are drawn with the same length.
      DEFSCA = ABS( NVEC0 * TYPDAT / ( 100.0 * MIN( XM, YM ) ) )
      CALL PAR_DEF0R( 'VSCALE', DEFSCA, STATUS )
      CALL PAR_GET0R( 'VSCALE', VSCALE, STATUS )
      IF ( VSCALE .GE. 0.0 .AND.
     :     VSCALE .LE. VAL__SMLR ) VSCALE = VAL__SMLR

*  Convert VSCALE so that it gives data units per unit distance in the
*  graphics world coordinate system, rather than data units per centimetre.
      IF( VSCALE .GE. 0.0 ) THEN
         DSCALE = VSCALE * 100.0 * XM / ( X2 - X1 )
      ELSE
         DSCALE = VSCALE
      END IF

*  Get the vector justification to be used.
      CALL PAR_CHOIC( 'JUST', 'CENTRE', 'CENTRE,START,END', .TRUE.,
     :                JUST, STATUS )

*  Get the arrow head size, and convert it to units of DATA zone world
*  coordinates.
      CALL PAR_GET0R( 'ARROW', AHSIZE, STATUS )
      AHSIZE = AHSIZE * MAX( X2 - X1, Y2 - Y1 )

*  Get the arrowhead size in metres.
      AHSIZM = AHSIZE * XM / ( X2 - X1 )

*  Produce the plot.
*  =================
*  Get the Mapping from the Frame in which catalogue positions are stored,
*  to graphics world coordinates.
      MAP = AST_GETMAPPING( IPLOT, ICAT, AST__BASE, STATUS )

*  Use this Mapping to transform the (x,y) positions in the catalogue
*  into graphics world coordinates.
      CALL AST_TRAN2( MAP, NIN, %VAL( CNF_PVAL( IPX ) ),
     :                %VAL( CNF_PVAL( IPY ) ), .TRUE.,
     :                %VAL( CNF_PVAL( IPX2 ) ),
     :                %VAL( CNF_PVAL( IPY2 ) ), STATUS )

*  Set the appearance of lines drawn using PGPLOT so that they mimic
*  curves produced using astCurves.
      CALL KPG1_PGSTY( IPLOT, 'CURVES', .TRUE., ATTRS, STATUS )

*  Plot the vectors.
      CALL POL1_VECPL( NIN, %VAL( CNF_PVAL( IPX2 ) ),
     :                 %VAL( CNF_PVAL( IPY2 ) ),
     :                 %VAL( CNF_PVAL( IPMAG ) ),
     :                 %VAL( CNF_PVAL( IPANG ) ),
     :                 ANGFAC, ANGROT, DSCALE, AHSIZE,
     :                 JUST, NEGATE, REFANG, STATUS )

*  Re-instate the previous PGPLOT attributes.
      CALL KPG1_PGSTY( IPLOT, 'CURVES', .FALSE., ATTRS, STATUS )

*  First draw the axes if required.
      IF ( AXES ) CALL KPG1_ASGRD( IPLOT, IPICF, .TRUE., STATUS )

*  Plot the key.
*  =============
*  Now produce the key if required.
      IF ( KEY .AND. STATUS .EQ. SAI__OK ) THEN

*  Report an error if there is insufficient room within the current
*  picture for the key.
         IF( IPICK .EQ. -1 .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'POLPLOT_4', 'There is insufficient '//
     :                    'room in the current picture for a key.',
     :                    STATUS )
            GO TO 999
         END IF

*  Get the PGPLOT character height scale factor used for numerical labels
*  in the main vector map area.
         CALL KPG1_PGSTY( IPLOT, 'NUMLAB', .TRUE., ATTRS, STATUS )
         CALL PGQCH( HGT )
         CALL KPG1_PGSTY( IPLOT, 'NUMLAB', .FALSE., ATTRS, STATUS )

*  If no value was supplied for the vertical position of the KEY using
*  parameter KEYPOS, find the value which puts the top of the key level
*  with the top of the DATA picture.
         IF( NKP .LT. 2 ) THEN

*  We need to know the position of the top of the DATA picture so that
*  the top of the key can be put at the same height on the screen. Get
*  the bounds of the current PGPLOT viewport, in mm. Only the vertical
*  position at the top is needed.
            CALL PGQVP( 2, DUMMY, DUMMY, DUMMY, KEYOFF )

*  Activate the KEY picture. This returns a pointer to an AST Plot which
*  can be used to draw in the KEY picture.
            CALL KPG1_GDGET( IPICK, AST__NULL, .FALSE., IPLOTK, STATUS )
            IF( STATUS .NE. SAI__OK ) GO TO 999

*  Find the vertical position in the key picture which corresponds to
*  the top of the DATA picture, as a fraction of the height of the key
*  picture.
            CALL PGQVP( 2, DUMMY, DUMMY, Y1, Y2 )
            KEYOFF = ( KEYOFF - Y1 )/( Y2 - Y1 )

*  Drop it a little if the key is being drawn over the vector map (so
*  that it does not obscure the top axis).
            IF( CLRKEY ) KEYOFF = 0.99*KEYOFF

*  If the horizontal positions was given using parameter KEYPOS, just
*  activate the KEY picture. This returns a pointer to an AST Plot which
*  can be used to draw in the KEY picture.
         ELSE
            KEYOFF = KEYPOS( 2 )
            CALL KPG1_GDGET( IPICK, AST__NULL, .FALSE., IPLOTK, STATUS )
            IF( STATUS .NE. SAI__OK ) GO TO 999
         END IF

         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Clear any title and formats in this plot.
         CALL AST_CLEAR( IPLOTK, 'TITLE', STATUS )
         CALL AST_CLEAR( IPLOTK, 'FORMAT(1)', STATUS )
         CALL AST_CLEAR( IPLOTK, 'FORMAT(2)', STATUS )

*  Ensure that previous synonyms are cleared.
         CALL KPG1_ASPSY( ' ', ' ', STATUS )

*  Establish some synonyms for AST attribute names.
         CALL KPG1_ASPSY( 'FORMAT(SCA*LE)', 'FORMAT(1)', STATUS )
         CALL KPG1_ASPSY( 'FORMAT(VEC*TOR)', 'FORMAT(2)', STATUS )
         CALL KPG1_ASPSY( 'COLOUR(BACK*GROUND)', 'TEXTBACKCOLOUR',
     :                    STATUS )
         CALL KPG1_ASPSY( '(VEC*TOR)', '(CURVES)', STATUS )
         CALL KPG1_ASPSY( '(TEXT)', '(STRINGS)', STATUS )

*  Set the style for plotting in the key picture.
         CALL KPG1_ASSET( 'POLPACK_POLPLOT', 'KEYSTYLE', IPLOTK,
     :                    STATUS )

*  Now produce the key.
         CALL POL1_VECKY( 'KEYVEC', IPLOTK, VSCALE, AHSIZM, KEYOFF,
     :                    ABS( TYPDAT ), UNITS1, JUST, HGT, CLRKEY,
     :                    STATUS )

      END IF

*  Closedown sequence.
*  ===================

*  Flush all graphics.
      CALL PGUPDT

*  Arrive here if an error occurs.
 999  CONTINUE

*  Release resources used to hold synonyms for AST attribute names.
      CALL KPG1_ASPSY( ' ', ' ', STATUS )

*  Release work space.
      CALL PSX_FREE( IPMAG, STATUS )
      CALL PSX_FREE( IPANG, STATUS )
      CALL PSX_FREE( IPX, STATUS )
      CALL PSX_FREE( IPY, STATUS )
      CALL PSX_FREE( IPX2, STATUS )
      CALL PSX_FREE( IPY2, STATUS )
      IF( GOTZ ) CALL PSX_FREE( IPZ, STATUS )

*  Shutdown PGPLOT and the graphics database.
      CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )

*  Release the catalogue identifier.
      CALL CAT_TRLSE( CI, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'POLPLOT_ERR', 'POLPLOT: Error producing a '//
     :                 'vector map.', STATUS )
      END IF

      END
