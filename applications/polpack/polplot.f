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
*     and orientation of each vector, and the position of each vector.
*
*     The plot is produced within the current graphics database picture. 
*     If there is an existing DATA picture within the current picture, then
*     the vector map can be aligned with the existing DATA picture (see 
*     parameter CLEAR). If no DATA picture exists within the current picture, 
*     then a new DATA picture is created.

*  Usage:
*     polplot cat colx coly colmag colang [vscale] [arrow] [just] [device]

*  ADAM Parameters:
*     ANGROT = _REAL (Read)
*        A rotation angle in degrees to be added to each vector
*        orientation before plotting the vectors (see parameters
*        COLANG and NEGATE). It should be in the range 0--360. [0.0]
*     ARROW = LITERAL (Read)
*        Vectors are drawn as arrows, with the size of the arrow head
*        specified by this parameter. Simple lines can be drawn by setting
*        the arrow head size to zero. The value should be expressed as a 
*        fraction of the largest dimension of the vector map. [0.0]
*     AXES = _LOGICAL (Read)
*        TRUE if labelled and annotated axes are to be drawn around the
*        vector map, showing the coordinate Frame specified by parameter
*        COSYS. The appearance of the axes can be controlled using
*        the STYLE parameter. [TRUE]
*     CAT = LITERAL (Read)
*        The name of the input catalogue. This may be in any format
*        supported by the CAT library (see SUN/181). A file type of .FIT
*        is assumed if no file type is supplied.
*     CLEAR = _LOGICAL (Read)
*        TRUE if the graphics device is to be cleared before displaying
*        the vector map. This will result in the vector map being drawn
*        in a new DATA picture. If you want the vector map to be drawn
*        on top of an existing DATA picture, then set CLEAR to FALSE. The
*        vector map will then be drawn in alignment with the displayed 
*        data. If possible, alignment occurs within the coordinate Frame 
*        specified using parameter COSYS. If this is not possible, (for 
*        instance if suitable WCS information was not available when the 
*        existing DATA picture was created), then an alignment is attempted 
*        in PIXEL coordinates. If this is not possible, then alignment is
*        attempted in GRID coordinates. If this is not possible, then
*        alignment is attempted in the first suitable Frame in the catalogue
*        irrespective of its Domain. A message is displayed indicating the 
*        Domain in which alignment occurred. If there are no suitable Frames 
*        in the catalogue then an error is reported. [TRUE]
*     COLANG = LITERAL (Read)
*        The name of the catalogue column holding the orientation of each 
*        vector. The values are considered to be in units of degrees unless 
*        the UNITS attribute of the column has the value "Radians" (case 
*        insensitive).  The positive X axis defines zero orientation, and 
*        rotation from the X axis to the Y axis is considered positive. 
*        A list of available column names is displayed if a non-existent
*        column name is given. [ANG]
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
*     COSYS = GROUP (Read)
*        This gives the co-ordinate Frame to be displayed along the annotated 
*        axes (see parameter AXES). The supplied value will usually be a 
*        Domain name such as SKY, AXIS, PIXEL, etc, but more specific Frame 
*        descriptions can also be given by supplying a group of "name=value" 
*        strings where "name" is the name of an AST Frame attribute (see 
*        SUN/210), and "value" is the value to assign to it. In addition, 
*        IRAS90 Sky Coordinate System (SCS) values (such as "EQUATORIAL(J2000)", 
*        "GALACTIC", etc - see SUN/163) can be given. Domains "WORLD" and 
*        "DATA" are taken as synonyms for "PIXEL" and "AXIS" (unless the 
*        catalogue contains explicit definitions for the "WORLD" and "DATA" 
*        Domains). The available coordinate Frames are defined by an AST 
*        FrameSet (see SUN/210) in the supplied catalogue. If the catalogue 
*        does not contain a FrameSet, then a default FrameSet is used 
*        containing a single 2-dimensional Frame spanned by the axes defined 
*        by parameters COLX and COLY. If a null (!) value is supplied for 
*        COSYS, then the Current Frame in the FrameSet is used. If the vector 
*        map is being drawn over an existing DATA picture (see parameter
*        CLEAR), then the Frame specified by COSYS must exist in the FrameSet 
*        associated with the existing picture, as well as in the supplied 
*        catalogue. [!]
*     DEVICE = DEVICE (Read)
*        The plotting device. [Current graphics device]
*     FILL = _LOGICAL (Read)
*        The DATA picture containing the vector map is usually produced with 
*        the same shape as the data. However, for maps with markedly different
*        dimensions this default behaviour may not give the clearest result. 
*        When FILL is TRUE, the smaller dimension of the picture is expanded
*        to produce the largest possible picture within the current picture.  
*        [FALSE]
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
*        TRUE if a key is to be produced. [TRUE]
*     KEYPOS() = _REAL (Read)
*        Two values giving the position of the key. The first value gives 
*        the gap between the right hand edge of the vector map and the left 
*        hand edge of the key. The second value gives the vertical position of
*        the top of the key. If the second value is not given, the top of 
*        the key is placed level with the top of the vector map. Both
*        values should be in the range 0.0 to 1.0. [0.03]
*     KEYSTYLE = GROUP (Read)
*        The plotting style to use for the key (see parameter KEY). This 
*        should be a group of comma-separated name=value strings where "name" 
*        is the name of a Plot attribute and "value" is the value to assign 
*        to the attribute. The text in the key is drawn as Plot "Strings"
*        (using attributes COLOUR(STRINGS), FONT(STRINGS), etc - the synonym
*        TEXT can be used in place of STRINGS). The example vector is drawn 
*        as a Plot "Curve" (using attributes COLOUR(CURVES), etc - the
*        synonym VECTOR can be used in place of CURVES). The numerical
*        scale value is formatted as an axis 1 value (using attributes 
*        FORMAT(1), DIGITS(1), etc - the synonym SCALE can be used in place 
*        of the value 1). The length of the example vector is formatted as an
*        axis 2 value (using attribute FORMAT(2), etc - the synonym VECTOR
*        can be used in place of the value 2). A null (!) value causes the 
*        key style used on the previous invocation of POLPLOT to be 
*        re-used. If no previous key style is available, or if the keyword 
*        RESET is supplied, then a default key style is used. [!]
*     KEYVEC = _REAL (Read)
*        Length of the vector to be displayed in the key, in data units.
*        A default value is generated based on the spread of vector
*        lengths in the plot. []
*     LBND(2) = _REAL (Read)
*        The coordinates to put at the lower left corner of the plotting 
*        area, in the coordinates system specified by parameters COLX and
*        COLY. If a null value is supplied then an area is used which just 
*        encloses all the data in the supplied catalogue. [!]
*     PXSIZE = _REAL (Read)
*        The length (x axis) of the plot in metres. [Maximum that can
*        fit in the current picture while preserving square pixels]
*     PYSIZE = _REAL (Read)
*        The length (y axis) of the plot in metres. [Maximum that can
*        fit in the current picture while preserving square pixels]
*     NEGATE = _LOGICAL (Read)
*        If a TRUE value is supplied, then the angles giving the
*        orientation of the polarization (i.e. the values in the column
*        specified by parameter COLANG) are negated before adding on any 
*        value specified by parameter ANGROT. [FALSE]
*     STYLE = GROUP (Read)
*        The plotting style to use for the annotated axes. This should be 
*        a group of comma-separated name=value strings where "name" is the 
*        name of a Plot attribute, and "value" is the value to assign to 
*        the attribute. Vectors are drawn as "Curves" but may also be 
*        referred to using the synonym "Vectors" when specifying Plot 
*        attributes. Default values are supplied for any attributes which 
*        are not specified. These are obtained from the WCS information 
*        in the supplied catalogue, or inherited from the previous 
*        invocation of POLPLOT. A null (!) value causes defaults to be used 
*        for all attributes. [!]
*     UBND(2) = _REAL (Read)
*        The coordinates to put at the top right corner of the plotting 
*        area, in the coordinates system specified by parameters COLX and
*        COLY. If a null value is supplied then an area is used which just 
*        encloses all the data in the supplied catalogue. [!]
*     VSCALE = _REAL (Read)
*        The scale to be used for the vectors.  The supplied value
*        should give the data value corresponding to a vector length of
*        one centimetre.  []

*  Examples:
*     polplot poltab 
*        Produces a vector map on the current graphics device with
*        vectors defined in the FITS binary table "poltab". The magnitudes
*        are taken from column P, the orientations from column ANG and
*        the coordinates of each vector from columns X and Y. 
*     polplot poltab style=^mystyle.dat
*        As above, but the annotated axes and vectors are drawn according
*        to the description given in text file \verb+mystyle.dat+. If this
*        files contains the following lines:
*    
*           title=My favorite colours
*           grid=1
*           minticklen=0
*           colour(border)=green
*           colour(grid)=blue
*           colour(vec)=red
*           width(border)=0.05
*
*        then the title is set to "My favourite colours"; a grid is drawn
*        across the plot instead of tick marks around the edge; the border,
*        grid and vectors are drawn in green, blue and red respectively,
*        and slightly thicker lines are used to draw the border.
*     polplot poltab ra dec noclear angrot=23.4 cosys=eq(B1950)
*        Produces a vector map in which the reference direction for the vectors
*        (as defined by the value zero in the column ANG) is at the
*        position angle 23.4 degrees (measured anti-clockwise from the
*        positive y axis) in the displayed map. The position of each vector
*        is specified by columns "ra" and "dec". The annotated axes give
*        equatorial (RA/DEC) coordinates referred to the equinox of B1950.
*        If the vector map is displayed over an existing DATA picture, then
*        the vector map will be aligned on the sky with the previously 
*        displayed data if possible (i.e. the FrameSet associated with the 
*        existing picture in the AGI database contains a sky coordinate
*        Frame). If this is not possible, then the vector map will be
*        aligned in pixel or grid coordinates. A message is displayed
*        indicating the domain in which alignment took place.
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
*     Copyright (C) 1998 Central Laboratory of the Research Councils
 
*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-FEB-1998 (DSB):
*        Original version, derived from kappa:vecplot.
*     5-AUG-1998 (DSB):
*        Correct the DOMAIN passed to KPG1_PLOT so that it refers to the 
*        Base Frame of the WCS info, instead of the Current Frame. Changed 
*        the interpretation of parameters COLX and COLY accordingly. Defaults 
*        for LBND and UBND changed.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'CTM_PAR'          ! CTM_ Colour-Table Management constants
      INCLUDE 'AST_PAR'          ! AST_ constants and function declarations
      INCLUDE 'CAT_PAR'          ! CAT_ constants 
      INCLUDE 'PAR_ERR'          ! PAR_ error constants 

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER CUNITS             ! Maximum number of characters in units
      PARAMETER( CUNITS = 30 )   ! string

      REAL  DTOR                 ! Degrees to radians conversion factor
      PARAMETER ( DTOR = 1.7453293E-2 )

      REAL  NVEC0                ! Default no. of vectors along short
      PARAMETER ( NVEC0 = 15.0 ) ! axis

*  Local Variables:
      CHARACTER DOMAIN*30        ! Domain containing vector positions
      CHARACTER JUST*6           ! Vector justification: CENTRE or START
      CHARACTER NAME*(CAT__SZCMP)! CAT column name
      CHARACTER UNITS1*( CUNITS )! Units of the data
      CHARACTER UNITS2*( CUNITS )! Units of the data
      DOUBLE PRECISION ATTRS( 20 )! Saved graphics attributes
      DOUBLE PRECISION BOX( 4 )  ! Bounds of used region of (X,Y) axes
      DOUBLE PRECISION DMAX      ! Max. value of pixels in array
      DOUBLE PRECISION DMAXC     ! Max. pixel value after clipping
      DOUBLE PRECISION DMIN      ! Min. value of pixels in array
      DOUBLE PRECISION DMINC     ! Min. pixel value after clipping
      DOUBLE PRECISION DX        ! Extension of X axis
      DOUBLE PRECISION DY        ! Extension of Y axis
      DOUBLE PRECISION LBND      ! Lower axis bound
      DOUBLE PRECISION MEAN      ! Mean of pixels in array
      DOUBLE PRECISION MEANC     ! Mean of pixels after clipping
      DOUBLE PRECISION STDEV     ! Standard devn. of pixels in array
      DOUBLE PRECISION STDEVC    ! Std. devn. of pixels after clipping
      DOUBLE PRECISION SUM       ! Sum of pixels in array
      DOUBLE PRECISION SUMC      ! Sum of pixels after clipping
      DOUBLE PRECISION UBND      ! Upper axis bound
      INTEGER CI                 ! CAT catalogue identifier
      INTEGER FRM                ! Frame pointer
      INTEGER GI                 ! CAT column identifier
      INTEGER GIANG              ! CAT identifier for ANG column
      INTEGER GIMAG              ! CAT identifier for MAG column
      INTEGER GIS( 2 )           ! CAT identifiers for X,Y catalogue columns
      INTEGER GIX                ! CAT identifier for X column
      INTEGER GIY                ! CAT identifier for X column
      INTEGER I                  ! Loop count
      INTEGER ICAT               ! Index of (X,Y) Frame in IWCS and IPLOT
      INTEGER IFRM               ! Frame index
      INTEGER IMAX               ! Vector index of max. pixel
      INTEGER IMAXC              ! Vector index of max. clipped pixel
      INTEGER IMIN               ! Vector index of min. pixel
      INTEGER IMINC              ! Vector index of min. clipped pixel
      INTEGER IPANG              ! Pointer to array holding vector angles
      INTEGER IPICD              ! AGI id. for DATA picture
      INTEGER IPICF              ! AGI id. for new FRAME picture
      INTEGER IPICK              ! AGI id. for the KEY picture
      INTEGER IPLOT              ! Pointer to AST Plot for DATA picture
      INTEGER IPLOTK             ! Pointer to AST Plot for KEY picture
      INTEGER IPMAG              ! Pointer to array holding vector magnitudes
      INTEGER IPX                ! Pointer to array holding vector X positions
      INTEGER IPY                ! Pointer to array holding vector Y positions
      INTEGER IPX2               ! Pointer to array holding vector X positions
      INTEGER IPY2               ! Pointer to array holding vector Y positions
      INTEGER IWCS               ! Pointer to AST FrameSet read from catalogue
      INTEGER MAP                ! Mapping pointer
      INTEGER MAXPOS             ! Position of maximum value
      INTEGER MINPOS             ! Position of minimum value
      INTEGER NBAD               ! No. of bad values
      INTEGER NFRM               ! Frame index increment between IWCS and IPLOT
      INTEGER NGANG              ! No. of good vector angle values
      INTEGER NGMAG              ! No. of good vector magnitude values
      INTEGER NGOOD              ! No. valid pixels in array
      INTEGER NGOODC             ! No. valid pixels after clipping
      INTEGER NGX                ! No. of good vector X values
      INTEGER NGY                ! No. of good vector Y values
      INTEGER NIN                ! No. of positions in user-specified bounds
      INTEGER NKP                ! No. of values supplied for parameter KEYPOS
      INTEGER NVEC               ! No. of vectors in catalogue
      INTEGER VCI                ! Vector colour index
      LOGICAL AXES               ! Annotated axes to be drawn?
      LOGICAL KEY                ! A key to vector scale to be plotted?
      LOGICAL NEGATE             ! Negate supplied angles?
      LOGICAL THERE              ! Was a FrameSet read fom the catalogue?
      REAL AHSIZE                ! Arrowhead size in world co-ordinates
      REAL AHSIZM                ! Arrowhead size in metres
      REAL ANGFAC                ! NDF2 data to radians conversion factor
      REAL ANGROT                ! Angle to add on to NDF2 values
      REAL ASPECT                ! Aspect ratio for new DATA pictures
      REAL BLO( 2 )              ! Lower bounds of plotting space
      REAL BHI( 2 )              ! Upper bounds of plotting space
      REAL CLIP( 4 )             ! Array of clipping limits
      REAL DEFSCA                ! Default value for VSCALE
      REAL DSCALE                ! Vector scale, viz. data units per pixel
      REAL DUMMY                 ! Unused argument
      REAL KEYOFF                ! Offset to top of key 
      REAL KEYPOS( 2 )           ! Key position
      REAL MARGIN( 4 )           ! Margins round DATA picture
      REAL TYPDAT                ! A typical data value from 1st input NDF
      REAL VECWID                ! Line thickness for vectors
      REAL VSCALE                ! Vector scale, viz. data units per cm
      REAL X1                    ! Lower x w.c. bound of picture
      REAL X2                    ! Upper x w.c. bound of picture
      REAL XM                    ! DATA zone x size in metres
      REAL Y1                    ! Lower y w.c. bound of picture
      REAL Y2                    ! Upper y w.c. bound of picture
      REAL YKEY                  ! Vertical position at top of DATA picture
      REAL YM                    ! DATA zone y size in metres

*  Local Data:
      DATA CLIP   / 4*4.0 /,
     :     MARGIN / 4*0.1 /

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Open the input catalogue, and get its name.
      CALL CAT_ASSOC( 'CAT', 'READ', CI, STATUS )

*  Get CAT identifiers for the columns which are to be used to define the 
*  vector magnitudes, orientations, X and Y coordinates.
      CALL KPG1_GTCTC( 'COLMAG', CI, CAT__FITYP, ' ', GIMAG, STATUS )
      CALL KPG1_GTCTC( 'COLANG', CI, CAT__FITYP, ' ', GIANG, STATUS )
      CALL KPG1_GTCTC( 'COLX', CI, CAT__FITYP, ' ', GIX, STATUS )
      CALL KPG1_GTCTC( 'COLY', CI, CAT__FITYP, ' ', GIY, STATUS )

*  Obtain the units of the magnitude column.
      UNITS1 = ' '
      CALL CAT_TIQAC( GIMAG, 'UNITS', UNITS1, STATUS )

*  Obtain the units of the orientation column.
      UNITS2 = ' '
      CALL CAT_TIQAC( GIANG, 'UNITS', UNITS2, STATUS )

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

*  Store the values of all 4 catalogue columns in 4 arrays.
*  ========================================================
*  Find the number of rows in the catalogue. This is the number of
*  vectors to be plotted.
      CALL CAT_TROWS( CI, NVEC, STATUS )

*  Allocate workspace for 4 _REAL arrays each with NVEC elements.
      CALL PSX_CALLOC( NVEC, '_REAL', IPMAG, STATUS )
      CALL PSX_CALLOC( NVEC, '_REAL', IPANG, STATUS )
      CALL PSX_CALLOC( NVEC, '_DOUBLE', IPX, STATUS )
      CALL PSX_CALLOC( NVEC, '_DOUBLE', IPY, STATUS )
      CALL PSX_CALLOC( NVEC, '_DOUBLE', IPX2, STATUS )
      CALL PSX_CALLOC( NVEC, '_DOUBLE', IPY2, STATUS )

*  Check the pointers can be used.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Copy each column into the corresponding array.
      CALL KPG1_CPCTR( CI, GIMAG, NVEC, %VAL( IPMAG ), NGMAG, STATUS )
      CALL KPG1_CPCTR( CI, GIANG, NVEC, %VAL( IPANG ), NGANG, STATUS )
      CALL KPG1_CPCTD( CI, GIX, NVEC, %VAL( IPX ), NGX, STATUS )
      CALL KPG1_CPCTD( CI, GIY, NVEC, %VAL( IPY ), NGY, STATUS )

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
         GI = GIX
         CALL MSG_SETC( 'COL', 'X coordinate' )

      ELSE IF( NGY .EQ. 0 ) THEN
         GI = GIY
         CALL MSG_SETC( 'COL', 'Y coordinate' )

      END IF

      IF( GI .NE. CAT__NOID ) THEN
         CALL CAT_TIQAC( GI, 'NAME', NAME, STATUS )
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'NAME', NAME )
         CALL ERR_REP( 'POLPLOT_1', 'The ^COL column '//
     :                 '(^NAME) contains no data.', STATUS )
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
         CALL KPG1_MXMND( ( NGX .LT. NVEC ), NVEC, %VAL( IPX ), NBAD, 
     :                    UBND, LBND, MAXPOS, MINPOS, STATUS )
         BHI( 1 ) = REAL( UBND )
         BLO( 1 ) = REAL( LBND )

*  Find the maximum and minimum Y value.
         CALL KPG1_MXMND( ( NGY .LT. NVEC ), NVEC, %VAL( IPY ), NBAD, 
     :                    UBND, LBND, MAXPOS, MINPOS, STATUS )
         BHI( 2 ) = REAL( UBND )
         BLO( 2 ) = REAL( LBND )

*  Extend these bounds slightly.
         DX = 0.005*( BHI ( 1 ) - BLO( 1 ) )         
         BLO( 1 ) = BLO( 1 ) - DX
         BHI( 1 ) = BHI( 1 ) + DX

         DY = 0.005*( BHI ( 2 ) - BLO( 2 ) )         
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
      CALL POL1_RMBND( NVEC, BLO, BHI, %VAL( IPMAG ), %VAL( IPANG ), 
     :                 %VAL( IPX ), %VAL( IPY ), NIN, STATUS )

*  Find a representative data value.
*  =================================
*  Obtain a "typical" data value from the magnitude column. This will be
*  used to define the default vector scaling. First, compute the
*  statistics. Remove abberant values by sigma clipping 4 times.
      CALL KPG1_STATR( ( NIN .LT. NVEC ), NIN, %VAL( IPMAG ), 4, 
     :                 CLIP, NGOOD, IMIN, DMIN, IMAX, DMAX, SUM, MEAN, 
     :                 STDEV, NGOODC, IMINC, DMINC, IMAXC, DMAXC, SUMC, 
     :                 MEANC, STDEVC, STATUS )

*  Report an error if all data values are effectively zero.
      IF ( ABS( DMAX ) .LE. VAL__SMLR .AND.
     :     ABS( DMIN ) .LE. VAL__SMLR .AND.
     :     STATUS .EQ. SAI__OK ) THEN 
         CALL CAT_TIQAC( GIMAG, 'NAME', NAME, STATUS )
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'NAME', NAME )
         CALL ERR_REP( 'POLPLOT_2', 'All magnitude values are zero '//
     :                 'in column ^NAME.', STATUS )
         GO TO 999
      END IF

*  Calculate the "typical value". This is the mean value after clipping,
*  plus one standard deviation.
      IF ( MEANC .NE. VAL__BADD .AND. STDEVC .NE. VAL__BADD ) THEN
         TYPDAT = MEANC + STDEVC
      ELSE
         TYPDAT = MEAN
      END IF

*  Prepare to produce graphics using AST and PGPLOT.
*  =================================================

*  Attempt to read an AST FrameSet from the catalogue. The Base Frame of 
*  this FrameSet will be spanned by axes corresponding to the X and Y 
*  catalogue columns. The Current Frame is set by the user, using
*  parameter COSYS.
      GIS( 1 ) = GIX
      GIS( 2 ) = GIY
      CALL KPG1_GTCTA( 'COSYS', CI, 2, GIS, IWCS, STATUS )

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
            CALL AST_TRAN2( MAP, NIN, %VAL( IPX ), %VAL( IPY ), .TRUE.,
     :                      %VAL( IPX2 ), %VAL( IPY2 ), STATUS ) 

*  Find the maximum and minimum X PIXEL value.
            CALL KPG1_MXMND( ( NIN .LT. NVEC ), NIN, %VAL( IPX2 ), NBAD, 
     :                      UBND, LBND,  MAXPOS, MINPOS, STATUS )
            BHI( 1 ) = REAL( UBND )
            BLO( 1 ) = REAL( LBND )

*  Find the maximum and minimum Y PIXEL value.
            CALL KPG1_MXMND( ( NIN .LT. NVEC ), NIN, %VAL( IPY2 ), NBAD, 
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

*  See if a key to the vector magnitude scale is required.
      CALL PAR_GTD0L( 'KEY', .TRUE., .TRUE., KEY, STATUS )

*  Start up the graphics system, and create an AST Plot. Leave room for a
*  key if required. A pointer to the Plot is returned, together with AGI
*  identifiers for the FRAME, DATA and KEY pictures. Note, to get the index of 
*  a Frame (eg ICAT) in the returned Plot, you add the returned value of 
*  NFRM onto its index in IWCS.  The PGPLOT viewport is set up so that it
*  corresponds to the DATA picture.
      IF( KEY ) THEN

*  Get the position required for the key. The margin between DATA and KEY 
*  Frames is determined by the horizontal position requested for the key.
         CALL PAR_GDRVR( 'KEYPOS', 2, 0.0, 1.0, KEYPOS, NKP, STATUS )
         MARGIN( 2 ) = KEYPOS( 1 )

*  Start up the graphics system, creating a KEY picture.
         CALL KPG1_PLOT( IWCS, 'UNKNOWN', 'POLPACK', 'POLPLOT', ' ', 
     :                   MARGIN, 1, 'KEY', 'R', 0.25, ASPECT, DOMAIN, 
     :                   BOX, IPICD, IPICF, IPICK, IPLOT, NFRM, STATUS )

*  Otherwise, start up the graphics system, creating no KEY picture.
      ELSE
         CALL KPG1_PLOT( IWCS, 'UNKNOWN', 'POLPACK', 'POLPLOT', ' ', 
     :                   MARGIN, 0, ' ', ' ', 0.0, ASPECT, DOMAIN, 
     :                   BOX, IPICD, IPICF, IPICK, IPLOT, NFRM, STATUS )
      END IF

*  Find the index of the Frame within the Plot corresponding to the 
*  catalogue columns specifying the vector positions. 
      ICAT = ICAT + NFRM

*  See if axes are required
      CALL PAR_GET0L( 'AXES', AXES, STATUS )

*  Obtain the vector-plot characteristics.
*  =======================================

*  Get the angle (in degrees) which is to be added to the values stored
*  in the supplied catalogue, and convert to radians.  Do not set a dynamic 
*  default. Constrain to 0 to 360 degrees.
      CALL PAR_GDR0R( 'ANGROT', -1.0, 0.0, 360.0, .FALSE., ANGROT,
     :                STATUS )
      ANGROT = ANGROT * DTOR

*  See if the angles are to be negated before being used.
      CALL PAR_GET0L( 'NEGATE', NEGATE, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Establish the default value for the vector scaling factor such that
*  a typical data value corresponds to a vector equal to one 15th of 
*  the smallest DATA zone dimension, and then get a new (positive) value.  
*  If a value of zero is supplied, use the default value.  XM is measured in 
*  metres so 100 time converts to centimetres.
      CALL KPG1_GDQPC( X1, X2, Y1, Y2, XM, YM, STATUS )
      DEFSCA = ABS( NVEC0 * TYPDAT / ( 100.0 * MIN( XM, YM ) ) )
      CALL PAR_DEF0R( 'VSCALE', DEFSCA, STATUS )
      CALL PAR_GET0R( 'VSCALE', VSCALE, STATUS )
      VSCALE = ABS( VSCALE )
      IF ( VSCALE .LE. VAL__SMLR ) VSCALE = DEFSCA

*  Convert VSCALE so that it gives data units per unit distance in the
*  graphics world coordinate system, rather than data units per centimetre.
      DSCALE = VSCALE * 100.0 * XM / ( X2 - X1 )

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
*  First draw the axes if required.
      IF ( AXES ) CALL AST_GRID( IPLOT, STATUS )

*  Get the Mapping from the Frame in which catalogue positions are stored,
*  to graphics world coordinates.
      MAP = AST_GETMAPPING( IPLOT, ICAT, AST__BASE, STATUS )

*  Use this Mapping to transform the (x,y) positions in the catalogue 
*  into graphics world coordinates.
      CALL AST_TRAN2( MAP, NIN, %VAL( IPX ), %VAL( IPY ), .TRUE.,
     :                %VAL( IPX2 ), %VAL( IPY2 ), STATUS ) 

*  Set the appearance of lines drawn using PGPLOT so that they mimic 
*  curves produced using astCurves.
      CALL KPG1_PGSTY( IPLOT, 'CURVES', .TRUE., ATTRS, STATUS )

*  Plot the vectors.
      CALL POL1_VECPL( NIN, %VAL( IPX2 ), %VAL( IPY2 ), %VAL( IPMAG ),
     :                 %VAL( IPANG ), ANGFAC, ANGROT, DSCALE, AHSIZE, 
     :                 JUST, NEGATE, STATUS )

*  Re-instate the previous PGPLOT attributes.
      CALL KPG1_PGSTY( IPLOT, 'CURVES', .FALSE., ATTRS, STATUS )

*  Plot the key.
*  =============
*  Now produce the key if required.
      IF ( KEY .AND. STATUS .EQ. SAI__OK ) THEN

*  Report an error if there is insufficient room within the current
*  picture for the key.
            IF( IPICK .EQ. -1 .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'POLPLOT_3', 'There is insufficient '//
     :                       'room in the current picture for a key.', 
     :                       STATUS )
               GO TO 999
            END IF

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
            CALL KPG1_GDACT( IPICK, IPLOTK, STATUS )
            IF( STATUS .NE. SAI__OK ) GO TO 999

*  Find the vertical position in the key picture which corresponds to
*  the top of the DATA picture, as a fraction of the height of the key
*  picture.
            CALL PGQVP( 2, DUMMY, DUMMY, Y1, Y2 )
            KEYOFF = ( KEYOFF - Y1 )/( Y2 - Y1 )

*  If the horizontal positions was given using parameter KEYPOS, just 
*  activate the KEY picture. This returns a pointer to an AST Plot which
*  can be used to draw in the KEY picture.
         ELSE
            KEYOFF = KEYPOS( 2 )
            CALL KPG1_GDACT( IPICK, IPLOTK, STATUS )
            IF( STATUS .NE. SAI__OK ) GO TO 999
         END IF

         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Find the corresponding world Y value in the key picture.
         CALL PGQWIN( X1, X2, Y1, Y2 )
         YKEY = Y1 + KEYOFF*( Y2 - Y1 ) 

*  Ensure that previous synonyms are cleared.
         CALL KPG1_ASPSY( ' ', ' ', STATUS )

*  Establish some synonyms for AST attribute names.
         CALL KPG1_ASPSY( 'FORMAT(SCA*LE)', 'FORMAT(1)', STATUS )
         CALL KPG1_ASPSY( 'FORMAT(VEC*TOR)', 'FORMAT(2)', STATUS )
         CALL KPG1_ASPSY( '(VEC*TOR)', '(CURVES)', STATUS )
         CALL KPG1_ASPSY( '(TEXT)', '(STRINGS)', STATUS )

*  Set the style for plotting in the key picture.
         CALL KPG1_GTSTY( 'KEYSTYLE', .FALSE., 
     :                    'POLPACK.POLPLOT.KEYSTYLE', IPLOTK, STATUS )

*  Now produce the key.
         CALL KPS1_VECKY( 'KEYVEC', IPLOTK, VSCALE, AHSIZM, YKEY,
     :                    ABS( TYPDAT ), UNITS1, JUST, STATUS )

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

*  Shut down the graphics database.
      CALL AGP_DEASS( 'DEVICE', .TRUE., STATUS )

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
