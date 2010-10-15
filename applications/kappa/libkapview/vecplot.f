      SUBROUTINE VECPLOT( STATUS )
*+
*  Name:
*     VECPLOT

*  Purpose:
*     Plots a two-dimensional vector map.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL VECPLOT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application plots vectors defined by the values contained
*     within a pair of two-dimensional NDFs, the first holding the
*     magnitude of the vector quantity at each pixel, and the second
*     holding the corresponding vector orientations.  It is assumed that
*     the two NDFs are aligned in pixel co-ordinates. The number of
*     vectors in the plot is kept to a manageable value by only
*     plotting vectors for pixels on a sparse regular matrix.  The
*     increment (in pixels) between plotted vectors is given by
*     Parameter STEP.  Zero orientation may be fixed at any position
*     angle within the plot by specifying an appropriate value for
*     Parameter ANGROT.  Each vector may be represented either by an
*     arrow or by a simple line, as selected by Parameter ARROW.
*
*     The plot is produced within the current graphics database picture,
*     and may be aligned with an existing DATA picture if the existing
*     picture contains suitable co-ordinate Frame information (see
*     Parameter CLEAR).
*
*     Annotated axes can be produced (see Parameter AXES), and the
*     appearance of these can be controlled in detail using Parameter
*     STYLE.  The axes show co-ordinates in the current co-ordinate
*     Frame of NDF1.
*
*     A key to the vector scale can be displayed to the right of the
*     vector map (see Parameter KEY).  The appearance and position of
*     this key may be controlled using Parameters KEYSTYLE and KEYPOS.

*  Usage:
*     vecplot ndf1 ndf2 [comp] [step] [vscale] [arrow] [just] [device]

*  ADAM Parameters:
*     ANGROT = _REAL (Read)
*        A rotation angle in degrees to be added to each vector
*        orientation before plotting the vectors (see Parameter NDF2).
*        It should be in the range 0--360.  [0.0]
*     ARROW = LITERAL (Read)
*        Vectors are drawn as arrows, with the size of the arrow head
*        specified by this parameter.  Simple lines can be drawn by
*        setting the arrow head size to zero.  The value should be
*        expressed as a fraction of the largest dimension of the vector
*        map.  [current value]
*     AXES = _LOGICAL (Read)
*        TRUE if labelled and annotated axes are to be drawn around the
*        vector map.  These display co-ordinates in the current
*        co-ordinate Frame NDF1, which may be changed using application
*        WCSFRAME (see also Parameter USEAXIS).  The width of the
*        margins left for the annotation may be controlled using
*        Parameter MARGIN.  The appearance of the axes (colours, founts,
*        etc.) can be controlled using the STYLE parameter.  [TRUE]
*     CLEAR = _LOGICAL (Read)
*        TRUE if the graphics device is to be cleared before displaying
*        the vector map.  If you want the vector map to be drawn over
*        the top of an existing DATA picture, then set CLEAR to FALSE.
*        The vector map will then be drawn in alignment with the
*        displayed data.  If possible, alignment occurs within the
*        current co-ordinate Frame of the NDF.  If this is not possible
*        (for instance, if suitable WCS information was not stored with
*        the existing DATA picture), then alignment is attempted in
*        PIXEL co-ordinates.  If this is not possible, then alignment is
*        attempted in GRID co-ordinates.  If this is not possible, then
*        alignment is attempted in the first suitable Frame found in the
*        NDF irrespective of its domain.  A message is displayed
*        indicating the domain in which alignment occurred.  If there
*        are no suitable Frames in the NDF then an error is reported.
*        [TRUE]
*     COMP = LITERAL (Read)
*        The component of NDF1 which is to be used to define the vector
*        magnitudes.  It may be "Data", "Error" or "Variance".  The
*        last two are not available if NDF1 does not contain a VARIANCE
*        component.  The vector orientations are always defined by the
*        "Data" component of NDF2.  ["Data"]
*     DEVICE = DEVICE (Read)
*        The plotting device.  [Current graphics device]
*     FILL = _LOGICAL (Read)
*        The DATA picture containing the vector map is usually produced
*        with the same shape as the data.  However, for maps with
*        markedly different dimensions this default behaviour may not
*        give the clearest result.  When FILL is TRUE, the smaller
*        dimension of the picture is expanded to produce the largest
*        possible picture within the current picture.  [FALSE]
*     JUST = LITERAL (Read)
*        The justification for each vector; it can take any of the
*        following values:
*
*         - "Centre" -- the vectors are drawn centred on the
*         corresponding pixel,
*
*         - "Start"  -- the vectors are drawn starting at the
*         corresponding pixel, and
*
*         - "End" -- the vectors are drawn ending at the corresponding
*         pixel.
*
*        ["Centre"]
*     KEY = _LOGICAL (Read)
*        TRUE if a key indicating the vector scale is to be produced.
*        [TRUE]
*     KEYPOS() = _REAL (Read)
*        Two values giving the position of the key.  The first value
*        gives the gap between the right-hand edge of the vector map and
*        the left-hand edge of the key (0.0 for no gap, 1.0 for the
*        largest gap).  The second value gives the vertical position of
*        the top of the key (1.0 for the highest position, 0.0 for the
*        lowest).  If the second value is not given, the top of the key
*        is placed level with the top of the vector map.  Both values
*        should be in the range 0.0 to 1.0.  If a key is produced, then
*        the right-hand margin specified by Parameter MARGIN is
*        ignored.  [current value]
*     KEYSTYLE = GROUP (Read)
*        A group of attribute settings describing the plotting style to
*        use for the key (see Parameter KEY).
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
*        The appearance of the text in the key is controlled using
*        "String" attributes (e.g. Colour(Strings), Font(Strings),
*        etc.; the synonym TEXT can be used in place of Strings).  Note,
*        the Size attribute specifies the size of key text relative to
*        the size of the numerical labels on the vector map axes.  Thus
*        a value of 2.0 for Size will result in text which is twice the
*        size of the numerical axis labels.  The appearance of the
*        example vector is controlled using "Curve" attributes (e.g.
*        Colour(Curves), etc.; the synonym Vector can be used in place
*        of Curves).  The numerical scale value is formatted as as
*        axis-1 value (using attributes Format(1), Digits(1), etc.; the
*        synonym Scale can be used in place of the value 1).  The length
*        of the example vector is formatted as an axis-2 value (using
*        attribute Format(2), etc.; the synonym Vector can be used in
*        place of the value 2).  The vertical space between lines in
*        the key can be controlled using attribute TextLabGap.  A value
*        of 1.0 is used if no value is set for this attribute, and
*        produces default vertical spacing.  Values larger than 1.0
*        increase the vertical space, and values less than 1.0 decrease
*        the vertical space.  [current value]
*     KEYVEC = _REAL (Read)
*        Length of the vector to be displayed in the key, in data units.
*        If a null (!) value is supplied, the value used is generated
*        on the basis of the spread of vector lengths in the plot. [!]
*     MARGIN( 4 ) = _REAL (Read)
*        The widths of the margins to leave around the vector map for
*        axis annotation.  The widths should be given as fractions of
*        the corresponding dimension of the current picture.  The actual
*        margins used may be increased to preserve the aspect ratio of
*        the DATA picture.  Four values may be given, in the order;
*        bottom, right, top, left.  If fewer than four values are given,
*        extra values are used equal to the first supplied value.  If
*        these margins are too narrow any axis annotation may be
*        clipped.  If a null (!) value is supplied, the value used is
*        0.15 (for all edges) if annotated axes are being produced, and
*        zero otherwise.  See also Parameter KEYPOS.  [current value]
*     NDF1 = NDF (Read)
*        NDF structure containing the two-dimensional image giving the
*        vector magnitudes.
*     NDF2 = NDF (Read)
*        NDF structure containing the two-dimensional image giving the
*        vector orientations.  The values are considered to be in units
*        of degrees unless the UNITS component of the NDF has the value
*        "Radians" (case insensitive).  The positive y pixel axis
*        defines zero orientation, and rotation from the x pixel axis
*        to the y pixel is considered positive.
*     STEP = _INTEGER (Read)
*        The number of pixels between adjacent displayed vectors (along
*        both axes).  Increasing this value reduces the number of
*        displayed vectors.  If a null (!) value is supplied, the value
*        used gives about thirty vectors along the longest axis of the
*        plot.  [!]
*     STYLE = GROUP (Read)
*        A group of attribute settings describing the plotting style to
*        use for the vectors and annotated axes.
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
*        The appearance of the vectors is controlled by the attributes
*        Colour(Curves), Width(Curves), etc. (the synonym Vectors may be
*        used in place of Curves).  [current value]
*     VSCALE = _REAL (Read)
*        The scale to be used for the vectors.  The supplied value
*        should give the data value corresponding to a vector length of
*        one centimetre.  If a null (!) value is supplied, a default
*        value is used.  [!]
*     USEAXIS = GROUP (Read)
*        USEAXIS is only accessed if the current co-ordinate Frame of
*        the NDF has more than two axes.  A group of two strings should
*        be supplied specifying the two axes which are to be used when
*        annotating and aligning the vector map.  Each axis can be
*        specified using one of the following options.
*
*        - Its integer index within the current Frame of the input
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
*        same indices as the two significant NDF pixel axes are used.
*        [!]

*  Examples:
*     vecplot polint polang
*        Produces a vector map on the current graphics device with
*        vector magnitude taken from the NDF called polint and vector
*        orientation taken from NDF polang.  All other settings are
*        defaulted, so for example about 20 vectors are displayed along
*        the longest axis, and a key is plotted.
*     vecplot polint polang angrot=23.4 clear=no
*        Produces a vector map in which the primary axis of the vectors
*        (as defined by the value zero in the NDF polang) is at the
*        position angle 23.4 degrees (measured anti-clockwise from the
*        positive y axis) in the displayed map.  The map is drawn over
*        the top of the previously drawn DATA picture, aligned in a
*        suitable co-ordinate Frame.
*     vecplot stack(,,2) stack(,,1) arrow=0.1 just=start nokey
*        Produces a vector map in which the vectors are defined by two
*        planes in the 3-dimensional NDF called stack.  There is no
*        need to copy the two planes into two separate NDFs before
*        running VECPLOT.  Each vector is represented by an arrow,
*        starting at the position of the corresponding pixel.  No key
*        to the vector scale and justification is produced.

*  Notes:
*     -  If no Title is specified via the STYLE parameter, then the
*     Title component in NDF1 is used as the default title for the
*     annotated axes.  If the NDF does not have a Title component, then
*     the default title is taken from current co-ordinate Frame in NDF1.
*     If this has not been set explicitly, then the name of NDF1 is used
*     as the default title.
*     -  The application stores a number of pictures in the graphics
*     database in the following order: a FRAME picture containing the
*     annotated axes, vectors, and key; a KEY picture to store
*     the key if present; and a DATA picture containing just the
*     vectors.  Note, the FRAME picture is only created if annotated
*     axes or a key has been drawn, or if non-zero margins were
*     specified using Parameter MARGIN.  The world co-ordinates in the
*     DATA picture will be pixel co-ordinates.  A reference to NDF1,
*     together with a copy of the WCS information in the NDF are stored
*     in the DATA picture.  On exit the current database picture for the
*     chosen device reverts to the input picture.

*  Related Applications:
*     KAPPA: CALPOL.

*  Implementation Status:
*     -  Only real data can be processed directly.  Other non-complex
*     numeric data types will undergo a type conversion before the
*     vector plot is drawn.
*     -  Bad pixels and automatic quality masking are supported.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 1995, 1999, 2004 Central Laboratory of the Research
*     Councils. Copyright (C) 2006 Particle Physics & Astronomy
*     Research Council.  Copyright (C) 2010 Science & Technology
*     Facilities Council.  All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     16-AUG-1993 (DSB):
*        Original version.
*     21-SEP-1993 (DSB):
*        Modified to overlay the vector plot on any existing DATA plot
*        contained within the current picture.
*     1995 April 12 (MJC):
*        Added Related Applications and Implementation Status, and a
*        further example.  Moved last paragraph of the long Description
*        to Notes.  Made Examples and Usage lowercase.  KEY is no longer
*        a position parameter.  Various tidying and stylistic changes,
*        and typo's corrected.  Called KPG1_GTNDF.  Constrained ANGROT.
*        Used modern-style variable declarations.  Added headings to
*        the commentary.
*     30-AUG-1999 (DSB):
*        Do not cancel the DEVICE parameter when closing the graphics
*        system.
*     4-OCT-1999 (DSB):
*        Modified to use AST/PGPLOT.
*     26-OCT-1999 (DSB):
*        Made MARGIN a fraction of the current picture, not the DATA
*        picture.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     27-JAN-2006 (DSB):
*        Ignore blank titles supplied in STYLE.
*     6-FEB-2006 (DSB):
*        Use KPG1_ASTTL to get the title.
*     2006 April 12 (MJC):
*        Remove unused variables, correct punctuation, remove references
*        to a contour map, and wrapped long lines.
*     2010 October 13 (MJC):
*        Permit temporary style attributes.
*     {enter_further_changes_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'NDF_PAR'          ! NDF_ constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants
      INCLUDE 'CTM_PAR'          ! Colour-Table Management constants
      INCLUDE 'AST_PAR'          ! AST_ constants and function
                                 ! declarations
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS

*  External References:
      LOGICAL CHR_SIMLR          ! Two strings equivalent apart from
                                 ! case?

*  Local Constants:
      INTEGER CUNITS             ! Maximum number of characters in units
      PARAMETER( CUNITS = 30 )   ! string

      REAL  DTOR                 ! Degrees-to-radians conversion factor
      PARAMETER ( DTOR = 1.7453293E-2 )

      REAL  KW                   ! Width of key as a fraction of the
      PARAMETER ( KW = 0.3 )     ! width of the current picture

      INTEGER NDIM               ! Dimensionality of input array
      PARAMETER( NDIM = 2 )

      REAL  NVEC0                ! Default number of vectors along short
      PARAMETER ( NVEC0 = 30.0 ) ! axis

*  Local Variables:
      CHARACTER COMP*( 8 )       ! Component to be displayed
      CHARACTER JUST*( 6 )       ! Vector justification: CENTRE or START
      CHARACTER MCOMP*( 8 )      ! Component to be mapped
      CHARACTER NDFNAM*( 255 )   ! Full NDF specification
      CHARACTER UNITS1*( CUNITS )! Units of the data
      CHARACTER UNITS2*( CUNITS )! Units of the data
      DOUBLE PRECISION ATTRS( 20 ) ! Saved graphics attributes
      DOUBLE PRECISION BOX( 4 )  ! Bounds of used region of (X,Y) axes
      INTEGER EL1                ! Number of elements in the first NDF
      INTEGER EL2                ! Number of elements in the second NDF
      INTEGER I                  ! Loop count
      INTEGER INDF1              ! Identifier for first NDF
      INTEGER INDF2              ! Identifier for second NDF
      INTEGER IPANG              ! Pointer to 2nd data array
      INTEGER IPICD              ! AGI identifier for the DATA picture
      INTEGER IPICF              ! AGI identifier for the frame picture
      INTEGER IPICK              ! AGI identifier for the KEY picture
      INTEGER IPIX               ! Index of PIXEL Frame within FrameSet
                                 ! or Plot
      INTEGER IPLOT              ! Pointer to AST Plot for DATA picture
      INTEGER IPLOTK             ! Pointer to AST Plot for KEY picture
      INTEGER IPMAG              ! Pointer to 1st data array
      INTEGER IPW1               ! Pointer to workspace
      INTEGER IPW2               ! Pointer to workspace
      INTEGER IWCS               ! AST pointer to WCS FrameSet from NDF1
      INTEGER MAP                ! Mapping pointer
      INTEGER NC                 ! Number of characters in NDFNAM
      INTEGER NCU1               ! No. of characters in units of 1st NDF
      INTEGER NCU2               ! No. of characters in units of 2nd NDF
      INTEGER NFRM               ! Frame index increment between IWCS
                                 ! and IPLOT
      INTEGER NKP                ! No. of values supplied for parameter
                                 ! KEYPOS
      INTEGER NMARG              ! No. of margin values given
      INTEGER NX                 ! No. of vectors along horizontal edge
      INTEGER NY                 ! No. of vectors along vertical edge
      INTEGER SDIM1( NDF__MXDIM ) ! Significant dimensions of 1st NDF
      INTEGER SDIM2( NDF__MXDIM ) ! Significant dimensions of 2nd NDF
      INTEGER SLBND( NDIM )      ! Significant lower bounds of plot area
      INTEGER SLBND1( NDIM )     ! Significant lower bounds of 1st NDF
      INTEGER SLBND2( NDIM )     ! Significant lower bounds of 2nd NDF
      INTEGER STEP               ! Pixel increment between vectors
      INTEGER SUBND( NDIM )      ! Significant upper bounds of plot area
      INTEGER SUBND1( NDIM )     ! Significant upper bounds of 1st NDF
      INTEGER SUBND2( NDIM )     ! Significant upper bounds of 2nd NDF
      LOGICAL ALIGN              ! DATA picture aligned with a previous
                                 ! picture?
      LOGICAL AXES               ! Annotated axes to be drawn?
      LOGICAL KEY                ! A key to vector scale to be plotted?
      REAL AHSIZE                ! Arrowhead size in world co-ordinates
      REAL AHSIZM                ! Arrowhead size in metres
      REAL ANGFAC                ! NDF2 data-to-radians conversion
                                 ! factor
      REAL ANGROT                ! Angle to add on to NDF2 values
      REAL ASPECT                ! Aspect ratio for new DATA pictures
      REAL DEFSCA                ! Default value for VSCALE
      REAL DSCALE                ! Vector scale, viz. data units per
                                 ! pixel
      REAL DUMMY                 ! Unused argument
      REAL HGT                   ! Character height scale factor
      REAL KEYOFF                ! Offset to top of key
      REAL KEYPOS( 2 )           ! Key position
      REAL MARGIN( 4 )           ! Margins round DATA picture
      REAL TYPDAT                ! A typical vector data value
      REAL VSCALE                ! Vector scale, viz. data units per cm
      REAL X1                    ! Lower x bound of PGPLOT viewport
      REAL X2                    ! Upper x bound of PGPLOT viewport
      REAL XM                    ! DATA picture x size in metres
      REAL Y1                    ! Lower y bound of PGPLOT viewport
      REAL Y2                    ! Upper y bound of PGPLOT viewport
      REAL YM                    ! DATA picture y size in metres
*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Access the first input NDF.
*  ===========================

*  Obtain the identifier of the NDF holding the vector magnitudes.
      CALL LPG_ASSOC( 'NDF1', 'READ', INDF1, STATUS )

*  Find which component to use.
      CALL KPG1_ARCOG( 'COMP', INDF1, MCOMP, COMP, STATUS )

*  Get an AST pointer to a FrameSet describing the co-ordinate Frames
*  present in the NDF's WCS component.  Modify it to ensure that the
*  Base, PIXEL and Current frames all have two dimensions.  The NDF must
*  have exactly two significant dimensions (i.e. axes spanning more
*  than one pixel).
      CALL KPG1_ASGET( INDF1, NDIM, .TRUE., .TRUE., .TRUE., SDIM1,
     :                 SLBND1, SUBND1, IWCS, STATUS )

*  Find the index of the PIXEL Frame.
      CALL KPG1_ASFFR( IWCS, 'PIXEL', IPIX, STATUS )

*  Obtain the units of the requested component of the first NDF.
      CALL KPG1_DAUNI( INDF1, MCOMP, UNITS1, NCU1, STATUS )

*  Map the required component as an array of _REAL values.  Note that
*  the component may be 'Error'.
      CALL KPG1_MAP( INDF1, MCOMP, '_REAL', 'READ', IPMAG, EL1, STATUS )

*  Access the second input NDF.
*  ===========================

*  Now get the second NDF which defines the vector orientations.  The
*  DATA component is used.  It is assumed that this NDF is aligned in
*  pixel co-ordinates with the first NDF, so no WCS FrameSet is
*  required.  Access the NDF and its significant bounds.
      CALL KPG1_GTNDF( 'NDF2', NDIM, .TRUE., 'READ', INDF2, SDIM2,
     :                 SLBND2, SUBND2, STATUS )
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain the units of the DATA component of the second NDF.
      CALL KPG1_DAUNI( INDF2, 'Data', UNITS2, NCU2, STATUS )

*  Map the DATA component as an array of _REAL values.
      CALL KPG1_MAP( INDF2, 'Data', '_REAL', 'READ', IPANG, EL2,
     :               STATUS )

*  Find the conversion factor for the angle data.
*  ==============================================
*  Set up a factor which converts values stored in the DATA component
*  of the second NDF into units of radians.  If the UNITS component does
*  not have the value "RADIANS" (case insensitive), then assume the
*  data values are in degrees.
      IF( NCU2 .GT. 0 ) THEN
         IF( CHR_SIMLR( UNITS2( : NCU2 ), 'RADIANS' ) ) THEN
            ANGFAC = 1.0
         ELSE
            ANGFAC = DTOR
         END IF
      ELSE
         ANGFAC = DTOR
      END IF

*  Find the overlap between the NDFs.
*  ==================================

*  Compute the bounds of the significant dimensions within the region
*  of overlap between the two NDFs.  Using this method allows greater
*  flexibility in the specification of the input NDFs than would be
*  allowed if NDF_MBND was used (for instance NDF_MBND would report an
*  error if the two NDFs were given as FRED(,,1) and FRED(,,2), where
*  FRED is a three-dimensional NDF).
      SLBND( 1 ) = MAX( SLBND1( 1 ), SLBND2( 1 ) )
      SLBND( 2 ) = MAX( SLBND1( 2 ), SLBND2( 2 ) )
      SUBND( 1 ) = MIN( SUBND1( 1 ), SUBND2( 1 ) )
      SUBND( 2 ) = MIN( SUBND1( 2 ), SUBND2( 2 ) )

*  Report an error if there is no overlap between the two NDFs.
      IF( ( SUBND( 1 ) .LT. SLBND( 1 ) .OR.
     :       SUBND( 2 ) .LT. SLBND( 2 ) ) .AND.
     :       STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF1', INDF1 )
         CALL NDF_MSG( 'NDF2', INDF2 )
         CALL ERR_REP( 'VECPLOT_ERR1', 'There are no pixels in '//
     :                 'common between ^NDF1 and ^NDF2.', STATUS )
         GO TO 999
      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Find representative data value.
*  ==============================
*  Obtain a "typical" data value from the first input NDF.  This will be
*  used to define the default vector scaling.
      CALL KPS1_DTPCL( INDF1, SLBND, SUBND, SDIM1, TYPDAT, STATUS )

*  Start the graphics system.
*  ==========================

*  See if annotated axes are required.
      CALL PAR_GET0L( 'AXES', AXES, STATUS )

*  See if a key to vector length is required.
      CALL PAR_GET0L( 'KEY', KEY, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Set the dynamic default for MARGIN.
      IF( AXES ) THEN
         MARGIN( 1 ) = 0.15
      ELSE
         MARGIN( 1 ) = 0.0
      END IF

      CALL PAR_DEF1R( 'MARGIN', 1, MARGIN( 1 ), STATUS )

*  Get new values.
      CALL PAR_GDRVR( 'MARGIN', 4, -0.49, 10.0, MARGIN, NMARG, STATUS )

*  Use the default if a null value was supplied.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         NMARG = 1
      END IF

*  Ignore any suplus values.
      NMARG = MIN( 4, NMARG )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Use the first value for any unspecified edges.
      DO I = NMARG + 1, 4
         MARGIN( I ) = MARGIN( 1 )
      END DO

*  Store the pixel co-ordinates bounds for the new DATA picture.  These
*  are only used if the new DATA picture is not based on an existing
*  DATA picture.  Note, the corresponding PGPLOT window created by
*  KPG1_PLOT will have world co-ordinates of millimetres from the
*  bottom-left corner of the view surface, NOT pixels.  This box is only
*  used to define the bounds of the picture within the AGI database for
*  the benefit of non-AST applications.
      BOX( 1 ) = DBLE( SLBND( 1 ) ) - 1.0D0
      BOX( 2 ) = DBLE( SLBND( 2 ) ) - 1.0D0
      BOX( 3 ) = DBLE( SUBND( 1 ) )
      BOX( 4 ) = DBLE( SUBND( 2 ) )

*  Store the aspect ratio of the data array, assuming square pixels.
      ASPECT = ( BOX( 4 ) - BOX( 2 ) ) / ( BOX( 3 ) - BOX( 1 ) )

*  Generate a reference for the NDF to be stored in the graphics
*  database.
      CALL NDF_MSG( 'NDF1', INDF1 )
      CALL MSG_LOAD( ' ', '^NDF1', NDFNAM, NC, STATUS )

*  Establish synonyms for AST graphical element names to be recognised
*  during the following call to KPG1_PLOT.
      CALL KPG1_ASPSY( '(VEC*TORS)', '(CURVES)', STATUS )

*  Start up the graphics system.  This stores a new DATA picture in the
*  AGI database with the given pixel co-ordinate bounds (a KEY picture
*  is also created if necessary, together with an enclosing FRAME
*  picture ).  The PGPLOT viewport is set so that it matches the area of
*  the DATA picture.  World co-ordinates within the PGPLOT window are
*  set to millimetres from the bottom-left corner of the view surface.
*  An AST Plot is returned for drawing in the DATA picture.  The Base
*  (GRAPHICS) Frame in the Plot corresponds to millimetres from the
*  bottom-left corner of the viewport, and the Current Frame is
*  inherited from the NDF's WCS FrameSet.

*  First deal with cases where a key is required...
      IF( KEY ) THEN

*  Get the position required for the key.  The margin between DATA and
*  KEY Frames is determined by the horizontal position requested for the
*  key.
         CALL PAR_GDRVR( 'KEYPOS', 2, -1.0, 1.0, KEYPOS, NKP, STATUS )
         IF( KEYPOS( 1 ) .GE. 0.0 ) THEN
            MARGIN( 2 ) = KEYPOS( 1 )
         ELSE
            MARGIN( 2 ) = KEYPOS( 1 ) - KW
         END IF

*  Start up the graphics system, creating a KEY picture.
         CALL KPG1_PLOT( IWCS, 'UNKNOWN', 'KAPPA_VECPLOT',
     :                   NDFNAM( : NC ), MARGIN, 1, 'KEY', 'R', KW,
     :                   ASPECT, 'PIXEL', BOX, IPICD, IPICF, IPICK,
     :                   IPLOT, NFRM, ALIGN, STATUS )

*  Otherwise, start up the graphics system, creating no KEY picture.
      ELSE
         CALL KPG1_PLOT( IWCS, 'UNKNOWN', 'KAPPA_VECPLOT',
     :                   NDFNAM( : NC ), MARGIN, 0, ' ', ' ', 0.0,
     :                   ASPECT, 'PIXEL', BOX, IPICD, IPICF, IPICK,
     :                   IPLOT, NFRM, ALIGN, STATUS )
      END IF

*  Find the index of the NDF PIXEL Frame within the Plot.
      IPIX = IPIX + NFRM

*  Ensure the Title attribute of the Plot has a useful value.
      CALL KPG1_ASTTL( IPLOT, IWCS, INDF1, STATUS )

*  Obtain the vector-plot characteristics.
*  =======================================

*  Get the angle (in degrees) which is to be added to the values stored
*  in NDF2, and convert to radians.  Do not set a dynamic default.
*  Constrain to 0 to 360 degrees.
      CALL PAR_GDR0R( 'ANGROT', -1.0, 0.0, 360.0, .FALSE., ANGROT,
     :                STATUS )
      ANGROT = ANGROT * DTOR

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Find the increment between plotted vectors, in pixels, ensuring that
*  it is not zero or negative.  A default value which gives a reasonable
*  number of vectors along the shortest axis is used.
      STEP = MAX( 1, NINT( MAX( SUBND( 1 ) - SLBND( 1 ),
     :                          SUBND( 2 ) - SLBND( 1 ) ) / NVEC0 ) )
      CALL PAR_DEF0I( 'STEP', STEP, STATUS )
      CALL PAR_GET0I( 'STEP', STEP, STATUS )
      IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )
      STEP = MAX( 1, STEP )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the bounds of the viewport in millimetres, and convert to metres.
      CALL PGQVP( 2, X1, X2, Y1, Y2 )
      XM = ( X2 - X1 )/1000.0
      YM = ( Y2 - Y1 )/1000.0

*  Get the bounds of the PGPLOT window (this will also be in
*  millimetres).
      CALL PGQWIN( X1, X2, Y1, Y2 )

*  Establish the default value for the vector scaling factor such that
*  a typical data value corresponds to a vector equal to one 15th of
*  the smallest DATA picture dimension, and then get a new (positive)
*  value.  If a value of zero is supplied, use the default value.  XM
*  is measured in metres so 100 times converts to centimetres.
      DEFSCA = ABS( NVEC0 * TYPDAT / ( 100.0 * MIN( XM, YM ) ) )
      CALL PAR_DEF0R( 'VSCALE', DEFSCA, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the new parameter value.
      CALL PAR_GET0R( 'VSCALE', VSCALE, STATUS )

*  Annul the error if a null value was given.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         VSCALE = DEFSCA
      END IF

*  Make sure the value is larger than zero.
      VSCALE = ABS( VSCALE )
      IF( VSCALE .LE. VAL__SMLR ) VSCALE = VAL__SMLR

*  Convert VSCALE so that it gives data units per unit distance in the
*  graphics world co-ordinate system, rather than data units per
*  centimetre.
      DSCALE = VSCALE * 100.0 * XM / ( X2 - X1 )

*  Get the vector justification to be used.
      CALL PAR_CHOIC( 'JUST', 'CENTRE', 'CENTRE,START,END', .TRUE.,
     :                JUST, STATUS )

*  Get the arrow head size, and convert it to units of DATA-picture
*  world co-ordinates.
      CALL PAR_GET0R( 'ARROW', AHSIZE, STATUS )
      AHSIZE = AHSIZE * MAX( X2 - X1, Y2 - Y1 )

*  Get the arrowhead size in metres.
      AHSIZM = AHSIZE * XM / ( X2 - X1 )

*  Get the Mapping from the PIXEL Frame in the input NDFs to graphics
*  world co-ordinates.
      MAP = AST_GETMAPPING( IPLOT, IPIX, AST__BASE, STATUS )

*  Calculate the number of vectors to be drawn along each axis.
      NX = ( SUBND( 1 ) - SLBND( 1 ) - STEP/2 + 1 )/STEP
      NY = ( SUBND( 2 ) - SLBND( 2 ) - STEP/2 + 1 )/STEP

*  Allocate work space.
      CALL PSX_CALLOC( 2*NX*NY, '_DOUBLE', IPW1, STATUS )
      CALL PSX_CALLOC( 2*NX*NY, '_DOUBLE', IPW2, STATUS )

*  Set the appearance of lines drawn using PGPLOT so that they mimic
*  curves produced using astCurves.
      CALL KPG1_PGSTY( IPLOT, 'CURVES', .TRUE., ATTRS, STATUS )

*  Plot the vectors.
      CALL KPS1_VECPL( MAP, SLBND, SUBND, NX*NY,
     :                 SLBND1( 1 ), SUBND1( 1 ), SLBND1( 2 ),
     :                 SUBND1( 2 ), %VAL( CNF_PVAL( IPMAG ) ),
     :                 SLBND2( 1 ), SUBND2( 1 ), SLBND2( 2 ),
     :                 SUBND2( 2 ), %VAL( CNF_PVAL( IPANG ) ),
     :                 STEP, ANGFAC, ANGROT, DSCALE, AHSIZE, JUST,
     :                 %VAL( CNF_PVAL( IPW1 ) ),
     :                 %VAL( CNF_PVAL( IPW2 ) ), STATUS )

*  Re-instate the previous PGPLOT attributes.
      CALL KPG1_PGSTY( IPLOT, 'CURVES', .FALSE., ATTRS, STATUS )

*  Draw the axes grid if required.
      IF( AXES ) CALL KPG1_ASGRD( IPLOT, IPICF, .TRUE., STATUS )

*  Plot the key.
*  =============
*  Now produce the key if required.
      IF( KEY .AND. STATUS .EQ. SAI__OK ) THEN

*  Report an error if there is insufficient room within the current
*  picture for the key.
         IF( IPICK .EQ. -1 .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'VECPLOT_ERR2', 'There is insufficient '//
     :                    'room in the current picture for a key.',
     :                    STATUS )
            GO TO 999
         END IF

*  Get the PGPLOT character-height scale factor used for numerical
*  labels in the main vector-map area.
         CALL KPG1_PGSTY( IPLOT, 'NUMLAB', .TRUE., ATTRS, STATUS )
         CALL PGQCH( HGT )
         CALL KPG1_PGSTY( IPLOT, 'NUMLAB', .FALSE., ATTRS, STATUS )

*  If no value was supplied for the vertical position of the KEY using
*  Parameter KEYPOS, find the value which puts the top of the key level
*  with the top of the DATA picture.
         IF( NKP .LT. 2 ) THEN

*  We need to know the position of the top of the DATA picture so that
*  the top of the key can be put at the same height on the screen.  Get
*  the bounds of the current PGPLOT viewport, in mm.  Only the vertical
*  position at the top is needed.
            CALL PGQVP( 2, DUMMY, DUMMY, DUMMY, KEYOFF )

*  Activate the KEY picture.  This returns a pointer to an AST Plot
*  which can be used to draw in the KEY picture.
            CALL KPG1_GDGET( IPICK, AST__NULL, .FALSE., IPLOTK, STATUS )
            IF( STATUS .NE. SAI__OK ) GO TO 999

*  Find the vertical position in the key picture which corresponds to
*  the top of the DATA picture, as a fraction of the height of the key
*  picture.
            CALL PGQVP( 2, DUMMY, DUMMY, Y1, Y2 )
            KEYOFF = ( KEYOFF - Y1 )/( Y2 - Y1 )

*  If the horizontal positions was given using Parameter KEYPOS, just
*  activate the KEY picture.  This returns a pointer to an AST Plot
*  which can be used to draw in the KEY picture.
         ELSE
            KEYOFF = KEYPOS( 2 )
            CALL KPG1_GDGET( IPICK, AST__NULL, .FALSE., IPLOTK, STATUS )
            IF( STATUS .NE. SAI__OK ) GO TO 999
         END IF

         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Ensure that previous synonyms are cleared.
         CALL KPG1_ASPSY( ' ', ' ', STATUS )

*  Establish some synonyms for AST attribute names.
         CALL KPG1_ASPSY( 'FORMAT(SCA*LE)', 'FORMAT(1)', STATUS )
         CALL KPG1_ASPSY( 'FORMAT(VEC*TOR)', 'FORMAT(2)', STATUS )
         CALL KPG1_ASPSY( '(VEC*TOR)', '(CURVES)', STATUS )
         CALL KPG1_ASPSY( '(TEXT)', '(STRINGS)', STATUS )

*  Set the style for plotting in the key picture.  The plus requests
*  support of temporary attributes.
         CALL KPG1_ASSET( 'KAPPA_VECPLOT', '+KEYSTYLE', IPLOTK,
     :                    STATUS )

*  Now produce the key.
         CALL KPS1_VECKY( 'KEYVEC', IPLOTK, VSCALE, AHSIZM, KEYOFF,
     :                    ABS( TYPDAT ), UNITS1, JUST, HGT, STATUS )

*  Free resources.
         CALL KPG1_ASPSY( ' ', ' ', STATUS )

      END IF

*  Arrive here if an error occurs.
 999  CONTINUE

*  Free work space.
      CALL PSX_FREE( IPW1, STATUS )
      CALL PSX_FREE( IPW2, STATUS )

*  Shutdown PGPLOT and the graphics database.
      CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'VECPLOT_ERR', 'VECPLOT: Error producing a '//
     :                 'vector map.', STATUS )
      END IF

      END
