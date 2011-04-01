      SUBROUTINE SEGMENT( STATUS )
*+
*  Name:
*     SEGMENT

*  Purpose:
*     Copies polygonal segments from one NDF into another.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SEGMENT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine copies one or more polygonal segments from the first
*     input NDF (Parameter IN1), and pastes them into the second input
*     NDF (Parameter IN2) at the same pixel co-ordinates.  The resulting
*     mosaic is stored in the output NDF (see OUT).  Either input NDF
*     may be supplied as null ("!") in which case the corresponding
*     areas of the output NDF are filled with bad values.  For instance,
*     supplying a null value for IN2 allows segments to be cut from IN1
*     and pasted on to a background of bad values.  Supplying a null
*     value for IN1 allows "holes" to be cut out of IN2 and filled with
*     bad values.
*
*     Each polygonal segment is specified by giving the positions of its
*     vertices. This may be done using a graphics cursor, by supplying
*     a positions list or text file containing the positions, or by
*     supplying the positions in response to a parameter prompt. The
*     choice is made by Parameter MODE.
*
*     This application may also be used to cut and paste cylinders with
*     polygonal cross-sections from NDFs with more than two dimensions.
*     See the Notes section below for further details.

*  Usage:
*     segment in1 in2 out { coords=?
*                         { incat1-incat20=?
*                         { poly1-poly20=?
*                         mode

*  ADAM Parameters:
*     COORDS = LITERAL (Read)
*        The co-ordinates of a single vertex for the current polygon.
*        If Parameter MODE is set to "Interface", this parameter is
*        accessed repeatedly to obtain the co-ordinates of all vertices
*        in the polygon.  A null value should be given when the final
*        vertex has been specified.  Each position should be supplied
*        within the current co-ordinate Frame of the output NDF (see
*        Parameter OUT).  Supplying a colon ":" will display details of
*        the required co-ordinate Frame.  No more than two formatted
*        axis values (separated by a comma or space) may be supplied.
*        If the co-ordinate Frame being used has more than two axes,
*        then the two axes to use must be specified using Parameter
*        USEAXIS.
*     DEVICE = DEVICE (Read)
*        The name of the graphics device on which an image is displayed.
*        Only used if Parameter MODE is given the value "Cursor".  Any
*        graphics specified by Parameter PLOT will be produced on this
*        device.  This device must support cursor interaction.  [Current
*        graphics device]
*     IN1 = NDF (Read)
*        The input NDF containing the data to be copied to the inside of
*        the supplied polygonal segments.  If a null value is supplied,
*        the inside of the polygonal segments will be filled with bad
*        values.
*     IN2 = NDF (Read)
*        The input NDF containing the data to be copied to the outside
*        of the supplied polygonal segments.  If a null value is
*        supplied, the outside of the polygonal segments will be filled
*        with bad values.
*     INCAT1-INCAT20 = FILENAME (Read)
*        If MODE is "Catalogue", each of the Parameters INCAT1 to
*        INCAT20 are used to access catalogues containing the
*        co-ordinates of the vertices of a single polygon.  Suitable
*        catalogues may be created using CURSOR, LISTMAKE, etc.  If a
*        value is assigned to INCAT1 on the command line, you are not
*        prompted for any of the remaining parameters in this group;
*        additional polygon catalogues must also be supplied on the
*        command line.  Otherwise, you are prompted for INCAT1, then
*        INCAT2, etc. until a null value is given or INCAT20 is reached.
*
*        The positions in each catalogue are mapped into the pixel
*        co-ordinate Frame of the output NDF by aligning the WCS
*        information stored in the catalogue with the WCS information in
*        the output NDF.  A message indicating the Frame in which the
*        positions were aligned with the output NDF is displayed.
*     LOGFILE = FILENAME (Write)
*        The name of a text file in which the co-ordinates of the
*        polygon vertices are to be stored.  A null value (!) means that
*        no file is created.  [!]
*     MARKER = INTEGER (Read)
*        This parameter is only accessed if Parameter PLOT is set to
*        "Chain" or "Mark". It specifies the type of marker with which
*        each cursor position should be marked, and should be given as
*        an integer PGPLOT marker type. For instance, 0 gives a box, 1
*        gives a dot, 2 gives a cross, 3 gives an asterisk, 7 gives a
*        triangle. The value must be larger than or equal to -31.
*        [current value]
*     MODE = LITERAL (Read)
*        The mode in which the co-ordinates of each polygon vertex are
*        to be obtained.  The supplied string can be one of the
*        following selection.
*
*        - "Interface" -- positions are obtained using Parameter COORDS.
*        These positions must be supplied in the current co-ordinate
*        Frame of the output NDF (see Parameter OUT).
*
*        - "Cursor" -- positions are obtained using the graphics cursor
*        of the device specified by Parameter DEVICE.  The WCS
*        information stored with the picture in the graphics database is
*        used to map the supplied cursor positions into the pixel
*        co-ordinate Frame of the output NDF.  A message is displayed
*        indicating the co-ordinate Frame in which the picture and the
*        output NDF were aligned.
*
*        - "Catalogue" -- positions are obtained from positions lists
*        using Parameters INCAT1 to INCAT20. Each catalogue defines a
*        single polygon. The WCS information in each catalogue is used
*        to map the positions in the catalogue into the pixel
*        co-ordinate Frame of the output NDF.  A message is displayed
*        for each catalogue indicating the co-ordinate Frame in which
*        the catalogue and the output NDF were aligned.
*
*        - "File" -- positions are obtained from text files using
*        Parameters POLY1 to POLY20.  Each file defines a single
*        polygon.  Each line in a file must contain two formatted axis
*        values in the current co-ordinate Frame of the output NDF (see
*        Parameter OUT), separated by white space or a comma.
*
*        [current value]
*     MAXPOLY = _INTEGER (Read)
*        The maximum number of polygons which can be used.  For
*        instance, this can be set to 1 to ensure that no more than one
*        polygon is used (this sort of thing can be useful when writing
*        procedures or scripts).  A null value causes no limit to be
*        imposed (unless MODE="File" or "Catalogue" in which case a
*        limit of 20 is imposed).  [!]
*     MINPOLY = _INTEGER (Read)
*        The minimum number of polygons which can be used.  For
*        instance, this can be set to 2 to ensure that at least 2
*        polygons are used.  The supplied value must be less than or
*        equal to the value given for MAXPOLY and must be greater than
*        zero.  [1]
*     OUT = NDF (Write)
*        The output NDF.  If only one input NDF is supplied (that is, if
*        one of IN1 and IN2 is assigned a null value), then the output
*        NDF has the same shape and size as the supplied input NDF.
*        Also ancillary data such as WCS information is propagated from
*        the supplied input NDF.  In particular, this means that the
*        current co-ordinate Frame of the output NDF (in which vertex
*        positions should be supplied if MODE is "File" or "Interface")
*        is inherited from the input NDF.  If two input NDFs are
*        supplied, then the shape and size of the output NDF
*        corresponds to the area of overlap between the two input NDFs
*        (in pixel space), and the WCS information and current Frame
*        are inherited from the NDF associated with Parameter IN1.
*     PLOT = LITERAL (Read)
*        The type of graphics to be used to mark the position of each
*        selected vertex.  It is only used if Parameter MODE is given
*        the value "Cursor".  The appearance of these graphics (colour,
*        size, etc ) is controlled by the STYLE parameter.  PLOT can
*        take any of the following values.
*
*        - "None" -- No graphics are produced.
*
*        - "Mark" -- Each position is marked with a marker of type
*        specified by Parameter MARKER.
*
*        - "Poly" -- Causes each position to be joined by a line to the
*        previous position.  Each polygon is closed by joining the last
*        position to the first.
*
*        - "Chain" -- This is a combination of "Mark" and "Poly".  Each
*        position is marked by a marker and joined by a line to the
*        previous position.  Parameter MARKER is used to specify the
*        marker  to use.  [current value]
*     POLY1-POLY20 = FILENAME (Read)
*        If MODE is "File", each of the Parameters POLY1 to POLY20 are
*        used to access text files containing the co-ordinates of the
*        vertices of a single polygon.  If a value is assigned to POLY1
*        on the command line, you are not prompted for any of the
*        remaining parameters in this group; additional polygon files
*        must also be supplied on the command line.  Otherwise, you are
*        prompted for POLY1, then POLY2, etc. until a null value is
*        given or POLY20 is reached.
*
*        Each position should be supplied within the current
*        co-ordinate Frame of the output NDF (see Parameter OUT).  No
*        more than two formatted axis values (separated by a comma or
*        space) may be supplied on each line.  If the co-ordinate Frame
*        being used has more than two axes, then the two axes to use
*        must be specified using Parameter USEAXIS.
*     QUALITY = _LOGICAL (Read)
*        If a TRUE value is supplied for Parameter QUALITY then quality
*        information is copied from the input NDFs to the output NDFs.
*        Otherwise, the quality information is not copied.  This
*        parameter is only accessed if all supplied input NDFs have
*        defined QUALITY components.  If any of the supplied input NDFs
*        do not have defined QUALITY components, then no quality is
*        copied.  Note, if a null input NDF is given then the
*        corresponding output QUALITY values are set to zero.  [TRUE]
*     STYLE = GROUP (Read)
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
*        The appearance of the lines forming the edges of each polygon
*        is controlled by the attributes Colour(Curves), Width(Curves),
*        etc. (either of the synonyms Lines and Edges may be used in
*        place of Curves).  The appearance of the vertex markers is
*        controlled by the attributes Colour(Markers), Size(Markers),
*        etc. (the synonyms Vertices may be used in place of Markers).
*        [current value]
*     USEAXIS = GROUP (Read)
*        USEAXIS is only accessed if the current co-ordinate Frame of
*        the output NDF has more than two axes.  A group of two strings
*        should be supplied specifying the two axes spanning the plane
*        in which the supplied polygons are defined.  Each axis can be
*        specified using one of the following options.
*
*        - An integer index of an axis within the current Frame of the
*        output NDF (in the range 1 to the number of axes in the current
*        Frame).
*
*        - An axis symbol string such as "RA" or "VRAD".
*
*        - A generic option where "SPEC" requests the spectral axis,
*        "TIME" selects the time axis, "SKYLON" and "SKYLAT" picks the
*        sky longitude and latitude axes respectively.  Only those axis
*        domains present are available as options.
*
*        A list of acceptable values is displayed if an illegal value is
*        supplied.  If a null (!) value is supplied, the axes with the
*        same indices as the first two significant NDF pixel axes are
*        used.  [!]
*     VARIANCE = _LOGICAL (Read)
*        If a TRUE value is supplied for Parameter VARIANCE then
*        variance information is copied from the input NDFs to the
*        output NDFs.  Otherwise, the variance information is not
*        copied.  This parameter is only accessed if all supplied input
*        NDFs have defined VARIANCE components.  If any of the supplied
*        input NDFs do not have defined VARIANCE components, then no
*        variances are copied.  Note, if a null input NDF is given then
*        the corresponding output variance values are set bad.  [TRUE]

*  Examples:
*     segment in1=m51a in2=m51b out=m51_comp incat1=coords mode=cat
*        Copies a region of the NDF m51a to the corresponding position
*        in the output NDF m51_comp.  The region is defined by the list
*        of vertex co-ordinates held in catalogue coords.FIT.  All
*        pixels in the output NDF which fall outside this region are
*        given the corresponding pixel values from NDF m51b.
*     segment in1=m51a out=m51_cut mode=cursor plot=poly accept
*        Copies a region of the NDF m51a to the corresponding position
*        in the output NDF m51_cut.  The region is defined by selecting
*        vertices using a graphics cursor.  The image m51a should
*        previously have been displayed.  Each vertex is joined to the
*        previous vertex by a line on the graphics device.  The
*        ACCEPT keyword causes the suggested null default value for IN2
*        to be accepted.  This means that all pixels outside the region
*        identified using the cursor will be set bad in the output NDF.

*  Notes:
*     -  Supplied positions are mapped into the pixel co-ordinate Frame
*     of the output NDF before being used. This means that the two input
*     NDFs (if supplied) must be aligned in pixel space before using
*     this application.
*     -  The routine can handle NDFs of arbitrary dimensionality.  If
*     either input has three or more dimensions then all planes in the
*     NDF pixel arrays are processed in the same way, that is the same
*     polygonal regions are extracted from each plane and copied to the
*     corresponding plane of the output NDF.  The plane containing the
*     polygons must be defined using Parameter USEAXIS.  This plane is a
*     plane within the current co-ordinate Frame of the output NDF
*     (which is inherited from the first supplied input NDF).  This
*     scheme will only work correctly if the selected plane in the
*     current co-ordinate Frame is parallel to one of the planes of the
*     pixel array.

*  Related Applications:
*     KAPPA: ARDMASK, ERRCLIP, FILLBAD, FFCLEAN, PASTE, REGIONMASK,
*     SETMAGIC, THRESH.

*  Implementation Status:
*     -  This routine will propagate VARIANCE component values so long
*     as all supplied input NDFs have defined VARIANCE components, and
*     Parameter VARIANCE is not FALSE.
*     -  This routine will propagate QUALITY component values so long
*     as all supplied input NDFs have defined QUALITY components, and
*     Parameter QUALITY is not FALSE.
*     -  The UNITS, AXIS, LABEL, TITLE, WCS and HISTORY components are
*     propagated from the first supplied input NDF, together with all
*     extensions.
*     -  All non-complex numeric types are supported.  The following
*     data types are processed directly: _WORD, _INTEGER, _REAL,
*     _DOUBLE.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 1995, 1997-1998, 2000, 2004 Central Laboratory of
*     the Research Councils.
*     Copyright (C) 2010 Science & Facilities Research Council.
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
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     9-NOV-1993 (DSB):
*        Original NDF version, based on MJC's IMAGE-format KAPPA
*        implementation.
*     1995 April 11 (MJC):
*        Added Notes and Related Applications, and more examples.
*        Moved old Notes to Implementation Status.  Made Examples and
*        Usage lowercase.  Various tidying and stylistic changes, and
*        typo's corrected.  Renamed Parameter XY to COORDS.  Revised
*        call to various routines for obtaining the co-ordinates
*        and KPG1_XYD2W.  Used modern-style variable declarations.
*        Added headings to the commentary.  Made the non-warning
*        messages conditional.  Used AGI_BEGIN/END to delimit picture
*        processing.
*     1997 July 11 (MJC):
*        When POLY1 is given on the command line in file mode, it is
*        now possible to supply additional polygons through Parameters
*        POLY2-POLY20 also given on the command line.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     15-JAN-2000 (DSB):
*        Big changes to use PGPLOT, and to allow positions to be
*        specified in the current Frame of the output NDF.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2010 October 14 (MJC):
*        Allow temporary style attributes.
*     1-APR-2011 (DSB):
*        Use KPG_GDFND in place of KPG1_AGFND in case the most recent
*        data picture had no WCS.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'PAR_PAR'          ! PAR_ constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants
      INCLUDE 'AST_PAR'          ! AST_ constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXPOLY             ! Max. no. of polygon files
      PARAMETER ( MXPOLY = 20 )

      INTEGER MXVERT             ! Max. no. of vertices in any polygon
      PARAMETER ( MXVERT = 1000 )

*  Local Variables:
      CHARACTER ACTDES( 2 )*20   ! Cursor action descriptions
      CHARACTER BUFOUT*80        ! Buffer for log file output
      CHARACTER DTYPE*( NDF__SZFTP ) ! Data type for an output NDF component
      CHARACTER IDTYPE*( NDF__SZTYP ) ! Numeric type for mapping DATA arrays
      CHARACTER IVTYPE*( NDF__SZTYP ) ! Numeric type for mapping VAR arrays
      CHARACTER MODE*9           ! Source of input co-ordinates
      CHARACTER PLOT*5           ! Nature of required graphics
      CHARACTER PNAME*6          ! Current polygon file parameter name
      CHARACTER TITLE*1          ! Catalogue title (unused)
      DOUBLE PRECISION BC( 2 )   ! Dummy argument
      DOUBLE PRECISION CC( 2 )   ! User supplied position
      DOUBLE PRECISION CVERT( MXVERT, 2 )! Current Frame vertex positions
      DOUBLE PRECISION GVERT( MXVERT, 2 )! GRAPHICS Frame vertex positions
      DOUBLE PRECISION PVERT( MXVERT, 2 )! PIXEL Frame vertex positions
      INTEGER ACT( MXVERT )      ! Action associated with each vertex
      INTEGER AXES( 2 )          ! Indices of Current Frame axes being supplied
      INTEGER CATNAX             ! No. of axes in IWCSC Base Frame
      INTEGER CMAP               ! Catalogue mapping
      INTEGER FDL                ! File identifier for log file
      INTEGER GMAP               ! Mapping from GRAPHICS to Current Frame
      INTEGER I                  ! Loop count
      INTEGER IAT                ! Used length of a string
      INTEGER IAX                ! Axis index
      INTEGER IDIM               ! Axis index
      INTEGER IFRM               ! Frame in which positions are supplied
      INTEGER IMARK              ! Index of PGPLOT marker symbol
      INTEGER INDF1              ! NDF identifier for first input NDF
      INTEGER INDF2              ! NDF identifier for second input NDF
      INTEGER INDF3              ! NDF identifier for output NDF
      INTEGER IPIC               ! AGI identifier for DATA picture
      INTEGER IPIC0              ! AGI identifier for current picture
      INTEGER IPID               ! Pointer to catalogue position identifiers
      INTEGER IPIN               ! Pointer to catalogue positions
      INTEGER IPIX               ! Index of PIXEL Frame in IWCS
      INTEGER IPLOT              ! Plot associated with current DATA picture
      INTEGER IPMASK             ! Pointer to mapped mask
      INTEGER IWCS               ! WCS FrameSet for output NDF
      INTEGER IWCSC              ! FrameSet read from input catalogue
      INTEGER LBND( NDF__MXDIM ) ! Lower pixel bounds of the output NDF
      INTEGER LINES              ! Lines to be drawn using cursor
      INTEGER LPNAME             ! Used length of parameter name
      INTEGER MAP                ! Mapping from current to PIXEL Frame
      INTEGER MAXPOL             ! Maximum number of polygons allowed
      INTEGER MINPOL             ! Minimum number of polygons allowed
      INTEGER NAX                ! No. of axis values supplied for each vertex
      INTEGER NAXC               ! No. of axes in current Frame of IWCS
      INTEGER NBAD               ! No. of empty polygons supplied in a row
      INTEGER NDIM               ! Number of dimensions in output NDF
      INTEGER NEWFRM             ! Frame in which vertices are supplied
      INTEGER NEWMAP             ! Another Mappng
      INTEGER NGOOD              ! No. of good pixel positions
      INTEGER NPARAM             ! Index of next parameter to access
      INTEGER NPOLY              ! Number of polygons processed so far
      INTEGER NVERT              ! Number of vertices in current polygon
      INTEGER PAX( 2 )           ! Pixel axes being used
      INTEGER PMAP               ! Pointer to a PermMap
      INTEGER PSTATE             ! State of POLY1 parameter
      INTEGER RBMODE             ! PGPLOT rubber-band mode
      INTEGER SLBND( 2 )         ! Lower pixel bounds of the select axes
      INTEGER SMAP               ! Simplified Mapping current to PIXEL Frame
      INTEGER SUBND( 2 )         ! Upper pixel bounds of the select axes
      INTEGER UBND( NDF__MXDIM ) ! Upper pixel bounds of the output NDF
      LOGICAL AGAIN              ! Get another vertex?
      LOGICAL CAT                ! In CATALOGUE mode?
      LOGICAL CURSOR             ! In CURSOR mode?
      LOGICAL FILE               ! In FILE mode?
      LOGICAL GOT1               ! Was the first NDF supplied?
      LOGICAL GOT2               ! Was the second NDF supplied?
      LOGICAL INFO               ! Display cursor usage instructions?
      LOGICAL INTERF             ! In INTERFACE mode?
      LOGICAL LOGPOS             ! Create an output log file?
      LOGICAL MORE               ! Do another polygon?
      LOGICAL PROMPT             ! Interactively get polygons from the user?
      LOGICAL QUAL1              ! Does the 1st NDF have a QUALITY comp?
      LOGICAL QUAL2              ! Does the 2nd NDF have a QUALITY comp?
      LOGICAL QUAL3              ! Does the o/p NDF have a QUALITY comp?
      LOGICAL VAR1               ! Does the 1st NDF have VARIANCE comp?
      LOGICAL VAR2               ! Does the 2nd NDF have VARIANCE comp?
      LOGICAL VAR3               ! Does the o/p NDF have VARIANCE comp?
      REAL X( MXVERT )           ! X pixel values
      REAL X1, X2, Y1, Y2        ! Bounds of PGPLOT window
      REAL Y( MXVERT )           ! Y pixel values
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain the actual or null input NDFs.
*  =====================================
*  Obtain an identifier for the first NDF, which defines the data to go
*  inside the polygonal segments.
      CALL LPG_ASSOC( 'IN1', 'READ', INDF1, STATUS )

*  If a null value was supplied, annul the error (a null NDF identifier
*  will have been returned by LPG_ASSOC in this case).
      IF ( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*  Set a flag indicating if an NDF was obtained.
      GOT1 = INDF1 .NE. NDF__NOID .AND. STATUS .EQ. SAI__OK

*  If an NDF was obtained, get the state of the VARIANCE and QUALITY
*  components.
      IF ( GOT1 ) THEN
         CALL NDF_STATE( INDF1, 'VARIANCE', VAR1, STATUS )
         CALL NDF_STATE( INDF1, 'QUALITY', QUAL1, STATUS )
      ELSE
         VAR1 = .FALSE.
         QUAL1 = .FALSE.
      END IF

*  Obtain an identifier for the second NDF, which defines the data to go
*  outside the polygonal segments.
      CALL LPG_ASSOC( 'IN2', 'READ', INDF2, STATUS )

*  If a null value was supplied, annul the error (a null NDF identifier
*  will have been returned by LPG_ASSOC in this case).
      IF ( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*  Set a flag indicating if an NDF was obtained.
      GOT2 = INDF2 .NE. NDF__NOID .AND. STATUS .EQ. SAI__OK

*  If an NDF was obtained, get the state of the VARIANCE and QUALITY
*  components.
      IF ( GOT2 ) THEN
         CALL NDF_STATE( INDF2, 'VARIANCE', VAR2, STATUS )
         CALL NDF_STATE( INDF2, 'QUALITY', QUAL2, STATUS )
      ELSE
         VAR2 = .FALSE.
         QUAL2 = .FALSE.
      END IF

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Report an error and abort if neither input NDF was specified.
      IF ( .NOT. ( GOT1 .OR. GOT2 ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SEGMENT_ERR1', 'No input NDFs specified.',
     :                 STATUS )
         GO TO 999
      END IF

*  If both NDFs were supplied, replace the identifiers for the NDFs by
*  identifiers for sections of the NDFs covering the region of overlap.
      IF ( GOT1 .AND. GOT2 ) CALL NDF_MBND( 'TRIM', INDF1, INDF2,
     :                                      STATUS )

*  Create the output NDF.
*  ======================
*  If the first input NDF was supplied, propagate the output NDF from
*  the first input NDF.
      IF ( GOT1 ) THEN
         CALL LPG_PROP( INDF1, 'UNITS,AXIS,WCS', 'OUT', INDF3, STATUS )

*  Otherwise, propagate the output NDF from the second input NDF.
      ELSE
         CALL LPG_PROP( INDF2, 'UNITS,AXIS,WCS', 'OUT', INDF3, STATUS )
      END IF

*  Get the bounds of the output NDF.
      CALL NDF_BOUND( INDF3, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Get an AST pointer to the NDFs WCS FrameSet.
      CALL KPG1_GTWCS( INDF3, IWCS, STATUS )

*  Get the number of axes in the Current Frame.
      NAXC = AST_GETI( IWCS, 'NAXES', STATUS )

*  Find the PIXEL Frame.
      CALL KPG1_ASFFR( IWCS, 'PIXEL', IPIX, STATUS )

*  Get the Mapping from Current Frame to PIXEL Frame.
      MAP = AST_GETMAPPING( IWCS, AST__CURRENT, IPIX, STATUS )

*  Simplify the Mapping.
      SMAP = AST_SIMPLIFY( MAP, STATUS )

*  Report an error if the forward transformation is not defined.
      IF( .NOT. AST_GETL( SMAP, 'TRANFORWARD', STATUS ) .AND.
     :    STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETC( 'DOM', AST_GETC( IWCS, 'DOMAIN', STATUS ) )
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SEGMENT_ERR2', 'The transformation from the '//
     :                 'Current (^DOM) co-ordinate Frame to the '//
     :                 'PIXEL co-ordinate Frame is undefined.', STATUS )
         GO TO 999
      END IF

*  Decide in which plane to specify polygonal segments.
*  ====================================================
*  In this section we obtain a FrameSet (pointed to by variable IWCS)
*  in which the current Frame is the Frame in which the user will supply
*  vertex co-ordinates. If the Current Frame in the output NDFs WCS
*  FrameSet has 1 or 2 axes, then this FrameSet will be used "as is".
*  Otherwise it will be modified by adding a new current Frame containing
*  exactly 2 axes, selected by the user form all the axes available in the
*  original current Frame).

*  If the Current WS Frame has only 1 axis, then the user specifies
*  1-dimensional positions.
      IF( NAXC .EQ. 1 ) THEN
         NAX = 1
         AXES( 1 ) = 1

*  If the Current WS Frame has exactly 2 axes, then these 2 axes are used.
      ELSE IF( NAXC .EQ. 2 ) THEN
         NAX = 2
         AXES( 1 ) = 1
         AXES( 2 ) = 2

*  If there are more than 2 axes, the user selects the two axes to use.
      ELSE
         NAX = 2

*  The dynamic default axes are choosen so that they corrrespond
*  numerically to the first two significant pixel axes.
         AXES( 1 ) = 0
         AXES( 2 ) = 0
         DO I = 1, MAX( 2, NDIM )
            IF( UBND( I ) .GT. LBND( I ) ) THEN
               IF( AXES( 1 ) .EQ. 0 ) THEN
                  AXES( 1 ) = I
               ELSE IF( AXES( 2 ) .EQ. 0 ) THEN
                  AXES( 2 ) = I
               END IF
            END IF
         END DO

*  Use the first insignificant axes if there were less than 2 significant axes,
*  ensuring that AXES(1) is less than AXES(2).
         IF( AXES( 1 ) .EQ. 0 ) AXES( 1 ) = 1
         IF( AXES( 2 ) .EQ. 0 ) THEN
            IF( AXES( 1 ) .GT. 1 ) THEN
               AXES( 2 ) = AXES( 1 )
               AXES( 1 ) = 1
            ELSE
               AXES( 2 ) = 2
            END IF
         END IF

*  If the Current Frame does not have sufficient axes to use these
*  defaults, use 1 and 2 as the default axes.
         IF( AXES( 1 ) .GT. NAXC .OR. AXES( 2 ) .GT. NAXC ) THEN
            AXES( 1 ) = 1
            AXES( 2 ) = 2
         END IF

*  Allow the user to choose the two Current Frame axes to use.
         CALL KPG1_GTAXI( 'USEAXIS', IWCS, 2, AXES, STATUS )

*  We now add a new Current Frame to the output NDFs WCS FrameSet.
*  This new Frame consists of the two selected axes. Other axes are
*  assigned an AST__BAD value. First create the new Frame and the PermMap
*  which joins it to the current Current Frame.
         NEWFRM = AST_PICKAXES( IWCS, 2, AXES, PMAP, STATUS )

*  Now add this new Frame into the FrameSet.
         CALL AST_ADDFRAME( IWCS, AST__CURRENT, PMAP, NEWFRM, STATUS )

*  We now add the inverted PermMap to the begining of the Mapping from
*  the original Current Frame to the PIXEL Frame.
         CALL AST_INVERT( PMAP, STATUS )
         NEWMAP = AST_CMPMAP( PMAP, SMAP, .TRUE., ' ', STATUS )
         CALL AST_ANNUL( SMAP, STATUS )
         SMAP = AST_SIMPLIFY( NEWMAP, STATUS )
      END IF

*  Get a pointer to the current Frame. This is the Frame in which positions
*  will be supplied.
      IFRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )

*  Prepare the graphics device if required.
*  ========================================
*  See whether the co-ordinates of the polygon vertices are to be
*  specified using the parameter system, graphics cursor, positions list,
*  or a text file.
      CALL PAR_CHOIC( 'MODE', 'Interface', 'Interface,Cursor,'//
     :                'Catalogue,File', .TRUE., MODE, STATUS )

*  Set convenience flags for the various values of MODE.
      CURSOR = MODE .EQ. 'CURSOR'
      CAT = MODE .EQ. 'CATALOGUE'
      FILE = MODE .EQ. 'FILE'
      INTERF = MODE .EQ. 'INTERFACE'

*  In "Cursor" mode, open and prepare the graphics device.
      IF( CURSOR ) THEN

*  Open the graphics device for plotting with PGPLOT, obtaining an
*  identifier for the current AGI picture.
         CALL KPG1_PGOPN( 'DEVICE', 'UPDATE', IPIC0, STATUS )

*  Find the most recent DATA picture which has WCS.
         CALL KPG1_GDFND( 'DATA', IPIC, STATUS )

*  Report the name, comment, and label, if one exists, for the current
*  picture.
         CALL KPG1_AGATC( STATUS )

*  Set the PGPLOT viewport and AST Plot for this DATA picture. The PGPLOT
*  viewport is set equal to the selected picture, with world co-ordinates
*  giving millimetres from the bottom left corner of the view surface.
         CALL KPG1_GDGET( IPIC, AST__NULL, .FALSE., IPLOT, STATUS )

*  Save the bounds of the DATA picture.
         CALL PGQWIN( X1, X2, Y1, Y2 )

*  Merge the FrameSet read from the AGI database with the output NDFs
*  FrameSet, aligning them in some suitable Frame.
         CALL KPG1_ASMRG( IPLOT, IWCS, ' ', .FALSE., 0, STATUS )

*  Get the simplified Mapping from the GRAPHICS Frame to the Current Frame
*  of the output NDF.
         GMAP = AST_SIMPLIFY( AST_GETMAPPING( IPLOT, AST__BASE,
     :                                        AST__CURRENT, STATUS ),
     :                        STATUS )

*  Report an error if the forward transformation is not defined.
         IF( .NOT. AST_GETL( GMAP, 'TRANFORWARD', STATUS ) .AND.
     :       STATUS .EQ. SAI__OK ) THEN
            CALL MSG_SETC( 'DOM', AST_GETC( IPLOT, 'DOMAIN', STATUS ) )
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SEGMENT_ERR3', 'The transformation from '//
     :                    'the GRAPHICS co-ordinate Frame to the ^DOM'//
     :                    ' co-ordinate Frame is undefined.', STATUS )
            GO TO 999
         END IF

*  See what type of graphics are required.
         CALL PAR_CHOIC( 'PLOT', 'Poly', 'Poly,Mark,Chain,None', .TRUE.,
     :                   PLOT, STATUS )

*  Set the rubber band mode to use (none, unless "Poly" or "Chain" graphics
*  are being produced, in which case use a straight line rubber band).
         IF( PLOT .EQ. 'POLY' .OR. PLOT .EQ. 'CHAIN' ) THEN
            RBMODE = 1
         ELSE
            RBMODE = 0
         END IF

*  Get the PGPLOT marker type for CHAIN and MARKER graphics.
         IF( PLOT .EQ. 'MARK' .OR. PLOT .EQ. 'CHAIN' ) THEN
            CALL PAR_GDR0I( 'MARKER', 2, -31, 10000, .FALSE., IMARK,
     :                      STATUS )
         ELSE
            IMARK = -1000
         END IF

*  Set the type of lines to drawn by KPG1_PGCUR
         IF( PLOT .EQ. 'POLY' .OR. PLOT .EQ. 'CHAIN' ) THEN
            LINES = 1
         ELSE
            LINES = 0
         END IF

*  Set the plotting style if anything is to be drawn.  The plus prefix
*  before the parameter name requests support of temporary STYLE
*  attributes.
         IF( PLOT .NE. 'NONE' ) THEN
            CALL KPG1_ASPSY( '(LIN*ES)', '(CURVES)', STATUS )
            CALL KPG1_ASPSY( '(EDGE*S)', '(CURVES)', STATUS )
            CALL KPG1_ASPSY( '(VERT*ICES)', '(MARKERS)', STATUS )
            CALL KPG1_ASSET( 'KAPPA_SEGMENT', '+STYLE', IPLOT, STATUS )
            CALL KPG1_ASPSY( ' ', ' ', STATUS )
         END IF

*  Store the action descriptions.
         ACTDES( 1 ) = 'select a vertex'
         ACTDES( 2 ) = 'exit'

      END IF

*  Do other preparation.
*  =====================
*  Indicate information explaining how to supply vertices should be
*  displayed before getting the first polygon.
      INFO = .TRUE.

*  If file or catalogue mode has been selected, see if a command-line value
*  has been supplied for the first polygon co-ordinate file.  If so, set a
*  flag to suppress prompting for extra polygons (i.e. only the polygons
*  supplied on the command line will be used).
      IF ( CAT ) THEN
         CALL LPG_STATE( 'INCAT1', PSTATE, STATUS )
         PROMPT = PSTATE .NE. PAR__ACTIVE

      ELSE IF ( FILE ) THEN
         CALL LPG_STATE( 'POLY1', PSTATE, STATUS )
         PROMPT = PSTATE .NE. PAR__ACTIVE

      ELSE
         PROMPT = .TRUE.
      END IF

*  The polygons defined by the user are initially stored in a
*  2-dimensional mask image which indicates which pixels are inside a
*  polygon and which are not. As yet we do not know which pixel axes will
*  be spanned by the supplied current Frame positions, and so we cannot
*  determine the required size for this mask. Set the pointer to the mask
*  to zero to indicate that memory for the mask has not yet been allocated.
*  This will be done once the first point has been transformed into pixel
*  co-ordinates.
      IPMASK = 0

*  Indicate that we do not yet know which two pixel axes to use.
      PAX( 1 ) = 0
      PAX( 2 ) = 0

*  Get the maximum number of polygons which may be given. There are only
*  a limited number of parameters for accessing input text files, so if
*  MODE=FILE or CATALOGUE, an absolute limit of MXPOLY has to be imposed.
      IF( FILE .OR. CAT ) THEN
         CALL PAR_GDR0I( 'MAXPOLY', MXPOLY, 1, MXPOLY, .TRUE., MAXPOL,
     :                    STATUS )
      ELSE
         CALL PAR_GDR0I( 'MAXPOLY', VAL__MAXI, 1, VAL__MAXI, .TRUE.,
     :                   MAXPOL, STATUS )
      END IF

*  Now get the minimum number of polygons which must be processed.
      CALL PAR_GDR0I( 'MINPOLY', 1, 1, MAXPOL, .FALSE., MINPOL, STATUS )

*  Initialise the number of valid polygons inserted into the mask so far.
      NPOLY = 0

*  Initialise the number of the next parameter to use.
      NPARAM = 1

*  Attempt to open a log file to store the results for human readers.
      CALL FIO_ASSOC( 'LOGFILE', 'WRITE', 'LIST', 80, FDL, STATUS )

*  Annul the error if a null value was given, and indicate that a log
*  file is not to be created.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         LOGPOS = .FALSE.

      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         LOGPOS = .TRUE.
         CALL FIO_WRITE( FDL, '# Log file created by SEGMENT', STATUS )
      END IF

*  Now loop round, obtaining polygons and adding them into a pixel mask.
*  =====================================================================
      NBAD = 0
      MORE = .TRUE.
      DO WHILE( MORE .AND. STATUS .EQ. SAI__OK )

*  Start a new AST context.
         CALL AST_BEGIN( STATUS )


*  Set NVERT to zero to indicate that no valid polygon has yet been
*  obtained.
         NVERT = 0

*  The following sections get a list of vertex positions, expressed in the
*  Current Frame of the IWCS FrameSet. There is a section for each mode
*  of operation.

*  First, deal with cursor mode.
*  -----------------------------
         IF( CURSOR ) THEN

*  On the second and subsequent passes, give an abbreviated set of
*  instructions.
            IF( .NOT. INFO ) THEN
               CALL MSG_OUT( 'SEGMENT_MSG1', '  Use the cursor to '//
     :                       'define another polygon, or press press '//
     :                       '"." (or the right mouse button) to exit.',
     :                       STATUS )
               CALL MSG_BLANK( STATUS )
            END IF

*  Get the vertex positions in GRAPHICS co-ordinates, using the graphics
*  cursor.
            CALL KPG1_PGCUR( INFO, 'define a polygon', 2, ACTDES, 'AX',
     :                       X1, X2, Y1, Y2, 0, 0.5*( X1 + X2 ),
     :                       0.5*( Y1 + Y2 ), MXVERT, RBMODE, LINES, 0,
     :                       IMARK, IPLOT, X, Y, ACT, NVERT, STATUS )

*  Copy any GRAPHICS positions to a DOUBLE PRECISION ARRAY and transform
*  them into Current Frame co-ordinates.
            IF( NVERT .GT. 0 ) THEN

               DO I = 1, NVERT
                  GVERT( I, 1 ) = DBLE( X( I ) )
                  GVERT( I, 2 ) = DBLE( Y( I ) )
               END DO

               CALL AST_TRANN( GMAP, NVERT, 2, MXVERT, GVERT, .TRUE.,
     :                         NAX, MXVERT, CVERT, STATUS )

*  We do not need to issue instructions on subsequent polygons.
               INFO = .FALSE.

*  If this polygon is null, and the previous one was null, indicate that
*  no more are to be obtained.
            ELSE IF( NBAD .EQ. 1 ) THEN
               PROMPT = .FALSE.

            END IF

*  Now, deal with interface mode.
*  ------------------------------
         ELSE IF( INTERF ) THEN

*  Give appropriate instructions.
            CALL MSG_BLANK( STATUS )
            IF( INFO ) THEN
               CALL MSG_OUT( 'SEGMENT_MSG2', 'Supply the positions '//
     :                       'of the vertices for the first polygon:',
     :                       STATUS )
            ELSE
               CALL MSG_OUT( 'SEGMENT_MSG3', 'Supply the positions '//
     :                       'of the vertices for another polygon '//
     :                       '(enter "!" if all polygons have been '//
     :                       'given):', STATUS )
            END IF

*  Loop round until no more points are given.
            AGAIN = .TRUE.
            DO WHILE( AGAIN .AND. STATUS .EQ. SAI__OK )

*  Get another position, expressed in the current Frame of the FrameSet.
*  Do not use a dynamic default.
               CC( 1 ) = AST__BAD
               CALL KPG1_GTPOS( 'COORDS', IFRM, .FALSE., CC, BC,
     :                          STATUS )

*  If a null value was supplied, annul the error, and indicate that the
*  loop should be left.
               IF( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL( STATUS )
                  AGAIN = .FALSE.

*  Otherwise, increment the number of supplied positions and store the
*  position in the work array.
               ELSE
                  NVERT = NVERT + 1
                  CVERT( NVERT, 1 ) = CC( 1 )
                  CVERT( NVERT, 2 ) = CC( 2 )

*  If the work array is now full, leave the loop.
                  IF( NVERT .EQ. MXVERT ) THEN
                     CALL MSG_SETI( 'MX', MXVERT )
                     CALL MSG_OUT( 'SEGMENT_MSG4', 'Maximum number of'//
     :                             ' vertices (^MX) reached.', STATUS )
                     AGAIN = .FALSE.
                  END IF

               END IF

*  Cancel the parameter.
               CALL PAR_CANCL( 'COORDS', STATUS )

            END DO

*  Change the instructions if we have succesfully got the first polygon
            IF( NVERT .GT. 0 ) INFO = .FALSE.

*  Now, deal with catalogue mode.
*  ------------------------------
         ELSE IF( CAT ) THEN

*  Construct the parameter name for the next catalogue.
            PNAME = 'INCAT '
            LPNAME = 5
            CALL CHR_PUTI( NPARAM, PNAME, LPNAME )

*  If prompting for catalogues is disabled, only use the next catalogue if
*  it was supplied on the command line (otherwise, leave the polygon loop).
            IF( .NOT. PROMPT ) THEN
               CALL LPG_STATE( PNAME( : LPNAME ), PSTATE, STATUS )
               MORE = PSTATE .EQ. PAR__ACTIVE
            END IF

*  Process the next catalogue if appropriate.
            IF( MORE .AND. STATUS .EQ. SAI__OK ) THEN

*  Open a positions list catalogue and read its contents. A pointer to a
*  FrameSet is returned, together with pointers to positions and identifiers,
*  and a title. The positions are returned in the Base Frame of this FrameSet.
               IWCSC = AST__NULL
               CALL KPG1_RDLST( PNAME( : LPNAME ), .FALSE., IWCSC,
     :                          NVERT, CATNAX, IPIN, IPID, TITLE, ' ',
     :                          STATUS )

*  If a null parameter value was supplied, annul the error.
               IF( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL( STATUS )

*  Otherwise, use the positions read from the catalogue.
               ELSE

*  Report an error if the catalogue contained too many vertices.
                  IF( NVERT .GT. MXVERT .AND. STATUS .EQ. SAI__OK ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'N', NVERT )
                     CALL MSG_SETI( 'M', MXVERT )
                     CALL MSG_SETC( 'P', PNAME( : LPNAME ) )
                     CALL ERR_REP( 'SEGMENT_ERR4', 'Too many vertices'//
     :                             ' (^N) specified by Parameter ^P. '//
     :                             'Each polygon must contain no more'//
     :                             ' than ^M vertices.', STATUS )
                  END IF

*  Align the FrameSet read from the catalogue with the FrameSet for the
*  output NDF.
                  CALL KPG1_ASMRG( IWCSC, IWCS, ' ', .FALSE., 0,
     :                             STATUS )

*  Get the simplified Mapping from the catalogue Base Frame to the
*  Current Frame of the output NDF.
                  CMAP = AST_SIMPLIFY( AST_GETMAPPING( IWCSC, AST__BASE,
     :                                           AST__CURRENT, STATUS ),
     :                                 STATUS )

*  Transform the supplied positions into the current Frame of the output
*  NDF.
                  CALL AST_TRANN( CMAP, NVERT, CATNAX, NVERT,
     :                            %VAL( CNF_PVAL( IPIN ) ),
     :                            .TRUE., NAX, MXVERT,
     :                            CVERT, STATUS )

*  Free the memory holding the positions and identifiers read from the
*  catalogue.
                  CALL PSX_FREE( IPIN, STATUS )
                  CALL PSX_FREE( IPID, STATUS )
               END IF

*  Use the next parameter on the next loop.
               NPARAM = NPARAM + 1

            END IF

*  Now, deal with file mode.
*  -------------------------
         ELSE

*  Construct the parameter name for the next file.
            PNAME = 'POLY  '
            LPNAME = 4
            CALL CHR_PUTI( NPARAM, PNAME, LPNAME )

*  If prompting for files is disabled, only use the next file if it was
*  supplied on the command line (otherwise, leave the polygon loop).
            IF( .NOT. PROMPT ) THEN
               CALL LPG_STATE( PNAME( : LPNAME ), PSTATE, STATUS )
               MORE = PSTATE .EQ. PAR__ACTIVE
            END IF

*  Process the next file if appropriate.
            IF( MORE ) THEN
               CALL KPG1_ASFIL( PNAME( : LPNAME ), ' ', IFRM, NVERT,
     :                          IPIN, ' ', STATUS )

*  If a null parameter value was supplied, annul the error.
               IF( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL( STATUS )

*  Otherwise, use the positions read from the file.
               ELSE

*  Report an error if the file contained too many vertices.
                  IF( NVERT .GT. MXVERT .AND. STATUS .EQ. SAI__OK ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'N', NVERT )
                     CALL MSG_SETI( 'M', MXVERT )
                     CALL MSG_SETC( 'P', PNAME( : LPNAME ) )
                     CALL ERR_REP( 'SEGMENT_ERR5', 'Too many vertices'//
     :                             ' (^N) specified by Parameter ^P. '//
     :                             'Each polygon must contain no more'//
     :                             ' than ^M vertices.', STATUS )
                  END IF

*  Copy the supplied positions into the work array.
                  CALL AST_TRANN( AST_UNITMAP( NAX, ' ', STATUS ),
     :                            NVERT, NAX, NVERT,
     :                            %VAL( CNF_PVAL( IPIN ) ),
     :                            .TRUE., NAX, MXVERT, CVERT, STATUS )

*  Free the memory holding the positions read from the file.
                  CALL PSX_FREE( IPIN, STATUS )
               END IF

*  Use the next parameter on the next loop.
               NPARAM = NPARAM + 1

            END IF

         END IF

*  Process the polygon obtained above.
*  -----------------------------------
*  If a polygon was obtained...
         IF( NVERT .GT. 0 ) THEN
            NBAD = 0

*  Map the positions obtained above into the PIXEL Frame of the output NDF.
            CALL AST_TRANN( SMAP, NVERT, NAX, MXVERT, CVERT, .TRUE.,
     :                      NDIM, MXVERT, PVERT, STATUS )

*  Abort if an error has occurred.
            IF( STATUS .NE. SAI__OK ) GO TO 999

*  If the pixel axes to be used have not yet been determined, find the
*  first transformed point with exactly two good axis values. These axes
*  are used as the required pixel axis from then on.
            I = 1
            DO WHILE( PAX( 1 ) .EQ. 0 .AND. I .LE. NVERT )

*  Store the indices of the first two axes which have good axis values.
               IAX = 0
               DO IDIM = 1, NDIM
                  IF( PVERT( I, IDIM ) .NE. AST__BAD ) THEN
                     IAX = IAX + 1
                     IF( IAX .LE. 2 ) THEN
                        PAX( IAX ) = IDIM
                     END IF
                  END IF
               END DO

*  If the number of good axis values for this position was not exactly two,
*  reset the indices of the axes to use, and look at the next transformed
*  position.
               IF( IAX .NE. 2 ) THEN
                  PAX( 1 ) = 0
                  PAX( 2 ) = 0
                  I = I + 1
               ELSE
                  SLBND( 1 ) = LBND( PAX( 1 ) )
                  SLBND( 2 ) = LBND( PAX( 2 ) )
                  SUBND( 1 ) = UBND( PAX( 1 ) )
                  SUBND( 2 ) = UBND( PAX( 2 ) )
               END IF

            END DO

*  Report an error if no points had exactly two good axis values.
            IF( PAX( 1 ) .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'SEGMENT_ERR6', 'Cannot determine which '//
     :                       'pixel axes correspond to the current '//
     :                       'Frame axes specified by Parameter '//
     :                       'USEAXIS.', STATUS )
               GO TO 999
            END IF

*  Copy the transformed points which are good on the two pixel axes
*  being used into separate X and Y arrays. Log them to the text file at
*  the same time.
            IF( LOGPOS ) THEN
               WRITE( BUFOUT, '(''# Polygon no: '', I4)' ) NPOLY + 1
               CALL FIO_WRITE( FDL, BUFOUT, STATUS )
            END IF

            NGOOD = 0
            DO I = 1, NVERT
               IF( PVERT( I, PAX( 1 ) ) .NE. AST__BAD .AND.
     :             PVERT( I, PAX( 2 ) ) .NE. AST__BAD ) THEN
                  NGOOD = NGOOD + 1
                  X( NGOOD ) = REAL( PVERT( I, PAX( 1 ) ) )
                  Y( NGOOD ) = REAL( PVERT( I, PAX( 2 ) ) )

                  IF( LOGPOS ) THEN
                     BUFOUT = ' '
                     IAT = 0
                     CALL CHR_APPND( AST_FORMAT( IFRM, PAX( 1 ),
     :                                   PVERT( I, PAX( 1 ) ), STATUS ),
     :                               BUFOUT, IAT )
                     IAT = IAT + 1
                     CALL CHR_APPND( AST_FORMAT( IFRM, PAX( 2 ),
     :                                   PVERT( I, PAX( 2 ) ), STATUS ),
     :                               BUFOUT, IAT )
                     CALL FIO_WRITE( FDL, BUFOUT( : IAT ), STATUS )
                  END IF

               END IF
            END DO

*  Warn the user about any bad points.
            IF( NGOOD .LT. 2 ) THEN
               CALL MSG_OUT( 'SEGMENT_MSG5', 'Less than two vertices '//
     :                       'could be transformed into pixel '//
     :                       'co-ordinates, so this polygon will be '//
     :                       'ignored.', STATUS )
               CALL MSG_BLANK( STATUS )

            ELSE IF( NGOOD .LT. NVERT ) THEN
               CALL MSG_SETI( 'N', NVERT - NGOOD )
               CALL MSG_OUT( 'SEGMENT_MSG6', '^N of the supplied '//
     :                       'vertices could not be transformed into '//
     :                       'pixel co-ordinates and so will be '//
     :                       'ignored.', STATUS )
               CALL MSG_BLANK( STATUS )
            END IF

*  Ignore this polygon if it has less than 2 vertices.
            IF( NGOOD .GE. 2 ) THEN

*  Allocate memory to hold the pixel mask unless this has already been
*  done.
               IF( IPMASK .EQ. 0 ) THEN
                  CALL PSX_CALLOC( ( SUBND( 1 ) - SLBND( 1 ) + 1 )*
     :                             ( SUBND( 2 ) - SLBND( 2 ) + 1 ),
     :                               '_LOGICAL', IPMASK, STATUS )
               END IF

*  Now insert this polygon into the mask.
               CALL KPS1_PLMSK( NPOLY, SLBND( 1 ), SUBND( 1 ),
     :                          SLBND( 2 ), SUBND( 2 ), NGOOD, X, Y,
     :                          %VAL( CNF_PVAL( IPMASK ) ), STATUS )

*  Increment the number of polygons accessed so far.
               NPOLY = NPOLY + 1

            END IF

*  If no polygon was obtained, check that the minimum number of
*  polygons has been processed.  If not, warn the user, and go back for
*  another polygon.
         ELSE
            NBAD = NBAD + 1

            IF ( NPOLY .LT. MINPOL ) THEN

               IF( PROMPT ) THEN
                  CALL MSG_BLANK( STATUS )
                  CALL MSG_SETI( 'MIN', MINPOL )
                  CALL MSG_OUT( 'SEGMENT_MSG7', 'Please give another '//
     :                          'polygon (at least ^MIN must be given'//
     :                          ' in total).', STATUS )
                  CALL MSG_BLANK( STATUS )
                  MORE = .TRUE.

               ELSE IF( STATUS .EQ. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'NP', NPOLY )
                  CALL MSG_SETI( 'MP', MINPOL )
                  CALL ERR_REP( 'SEGMENT_ERR7', 'The number of valid '//
     :                          'polygons obtained (^NP) was less '//
     :                          'than the minimum required (^MP).',
     :                          STATUS )
               END IF

*  Leave the loop if the minimum number of polygons has been obtained.
            ELSE
               MORE = .FALSE.
            END IF

         END IF

*  Leave the loop if the maximum number of polygons has been reached.
         IF( NPOLY .GE. MAXPOL ) MORE = .FALSE.

*  Close the AST context.
         CALL AST_END( STATUS )

      END DO

*  Now use the pixel mask to create the output NDF.
*  ================================================
*  If no polygons have been processed, report an error.
      IF ( NPOLY .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SEGMENT_ERR8','No valid polygons have been '//
     :                 'supplied.', STATUS )

*  Otherwise, Tell the user how many polygons are being copied.
      ELSE

         IF( NPOLY .EQ. 1 ) THEN
            CALL MSG_OUT( 'SEGMENT_MSG8', 'Copying data for a single '//
     :                    'polygon.', STATUS )
         ELSE
            CALL MSG_SETI( 'N', NPOLY )
            CALL MSG_OUT( 'SEGMENT_MSG9', 'Copying data for ^N '//
     :                    'polygons.', STATUS )
         END IF

*  If both input NDFs were supplied, find which numeric type should be
*  used to access the DATA arrays, and what numeric type the output
*  DATA component should be stored in.  The choice is made to avoid
*  unnecessary loss of precision.
         IF ( GOT1 .AND. GOT2 ) THEN
            CALL NDF_MTYPE( '_WORD,_INTEGER,_REAL,_DOUBLE', INDF1,
     :                      INDF2, 'DATA', IDTYPE, DTYPE, STATUS )

*  If only one input NDF was supplied, we still need to find out which
*  is the best data type to use from the ones available.
         ELSE
            IF ( GOT1 ) THEN
               CALL NDF_MTYPE( '_WORD,_INTEGER,_REAL,_DOUBLE', INDF1,
     :                         INDF1, 'DATA', IDTYPE, DTYPE, STATUS )

            ELSE
               CALL NDF_MTYPE( '_WORD,_INTEGER,_REAL,_DOUBLE', INDF2,
     :                         INDF2, 'DATA', IDTYPE, DTYPE, STATUS )

            END IF

         END IF

*  Set the numeric type of the output DATA array.
         CALL NDF_STYPE( DTYPE, INDF3, 'DATA', STATUS )

*  See if variance information is to be copied to the output NDF.  This
*  is true if all the supplied input NDFs have associated variance
*  values.
         VAR3 = .NOT. ( ( GOT1 .AND. (.NOT. VAR1 ) ) .OR.
     :                  ( GOT2 .AND. (.NOT. VAR2 ) ) )

*  Allow the user to suppress the copying of variance values.
         IF ( VAR3 ) CALL PAR_GET0L( 'VARIANCE', VAR3, STATUS )

*  If output variances are required, and both input NDFs have defined
*  variance values, match the numeric types of the VARIANCE arrays.
         IF ( VAR3 ) THEN
            IF ( VAR1 .AND. VAR2 ) THEN
               CALL NDF_MTYPE( '_WORD,_INTEGER,_REAL,_DOUBLE', INDF1,
     :                          INDF2, 'VARIANCE', IVTYPE, DTYPE,
     :                          STATUS )

*  If only one input NDF has defined variances, we still need to find
*  out which is the best data type to use from the ones available.
            ELSE
               IF ( VAR1 ) THEN
                  CALL NDF_MTYPE( '_WORD,_INTEGER,_REAL,_DOUBLE',
     :                            INDF1, INDF1, 'VARIANCE', IVTYPE,
     :                            DTYPE, STATUS )
               ELSE
                  CALL NDF_MTYPE( '_WORD,_INTEGER,_REAL,_DOUBLE', INDF2,
     :                            INDF2, 'VARIANCE', IVTYPE, DTYPE,
     :                            STATUS )
               END IF

            END IF

*  Set the numeric type of the output VARIANCE array.
            CALL NDF_STYPE( DTYPE, INDF3, 'VARIANCE', STATUS )

         END IF

*  See if quality information is to be copied to the output NDF.  This
*  is true if all the supplied input NDFs have associated quality
*  values.  Note, if only one input NDF was given, the undefined pixels
*  will be given a quality of zero in the output.  This may or may not
*  be appropriate!  Quality values are always mapped as unsigned bytes.
         QUAL3 = .NOT. ( ( GOT1 .AND. (.NOT. QUAL1 ) ) .OR.
     :                   ( GOT2 .AND. (.NOT. QUAL2 ) ) )

*  Allow the user to suppress the copying of quality values.
         IF ( QUAL3 ) CALL PAR_GET0L( 'QUALITY', QUAL3, STATUS )

*  Copy the input DATA values to the output, filling missing
*  values with bad values.  The mask just created is used to define
*  which pixels are inside a polygon and which are not.  If the NDF has
*  more than 2 dimensions, the mask is projected through the other
*  dimensions.
         CALL KPS1_PLCPY( INDF1, INDF2, INDF3, 'DATA', IDTYPE, GOT1,
     :                    GOT2, PAX, SLBND( 1 ), SUBND( 1 ),
     :                    SLBND( 2 ), SUBND( 2 ),
     :                    %VAL( CNF_PVAL( IPMASK ) ),
     :                    VAL__BADD, STATUS )

*  Now copy VARIANCE values if necessary, filling missing values with
*  bad values.
         IF ( VAR3 ) THEN
            CALL KPS1_PLCPY( INDF1, INDF2, INDF3, 'VARIANCE', IVTYPE,
     :                       VAR1, VAR2, PAX, SLBND( 1 ), SUBND( 1 ),
     :                       SLBND( 2 ), SUBND( 2 ),
     :                       %VAL( CNF_PVAL( IPMASK ) ),
     :                       VAL__BADD, STATUS )
         END IF

*  Now copy QUALITY values if necessary, filling missing values with
*  zeros.  QUALITY values are always accessed as unsigned byte values.
         IF ( QUAL3 ) THEN
            CALL KPS1_PLCPY( INDF1, INDF2, INDF3, 'QUALITY', '_UBYTE',
     :                       QUAL1, QUAL2, PAX, SLBND( 1 ), SUBND( 1 ),
     :                       SLBND( 2 ), SUBND( 2 ),
     :                       %VAL( CNF_PVAL( IPMASK ) ),
     :                       0.0D0, STATUS )
         END IF

      END IF

*  Tidy up.
*  ========
 999  CONTINUE

*  CLose the log file.
      IF( LOGPOS ) CALL FIO_ANNUL( FDL, STATUS )

*  Release work space.
      CALL PSX_FREE( IPMASK, STATUS )

*  Close the graphics database and device, if required.
      IF( CURSOR ) CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )

*  If an error occurred, attempt to delete the output NDF.
      IF ( STATUS .NE. SAI__OK ) CALL NDF_DELET( INDF3, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SEGMENT_ERR9', 'SEGMENT: Error copying a '//
     :                 'segment of an NDF into another NDF.', STATUS )
      END IF

      END
