      SUBROUTINE SEGMENT( STATUS )
*+
*  Name:
*     SEGMENT

*  Purpose:
*     Copies polygonal segments from one NDF to another.

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
*     This routine extracts polygonal segments from an NDF, and
*     optionally pastes them into the corresponding positions within
*     another NDF.  The application is intended to allow regions of an
*     NDF to be removed to another for separate processing.  It may
*     also be used to copy bad pixels into a NDF in order to delete a
*     region which is not required.
*
*     The vertices of polygonal segments are defined by lists of
*     co-ordinate pairs.  [For convenience these positions are denoted
*     x-y positions although they may not be literally x and y, for
*     example planes in a cube or hypercube.  See the "Notes" for more
*     details.]  Polygons are completed by connecting the last position
*     in the list to the first.  Pixels within each polygonal segment
*     are copied from the first NDF (IN1) to the corresponding position
*     in the second NDF (IN2).  A sequence of polygons may be supplied,
*     and each is copied in turn.  If a null value is given for either
*     NDF, the routine behaves as if an NDF full of bad pixels had been
*     specified.  Thus if IN1 is given a null value the inside of each
*     polygonal segment will be filled with bad values, and if IN2 is
*     given a null value the region outside the polygonal segments will
*     be filled with bad values.
*
*     The x-y positions may be specified in three ways:
*
*     1) from the parameter system, usually in response to prompting;
*
*     2) within text files (one for each polygon), where the files are
*     free format with x co-ordinates in column one and y co-ordinates
*     in column two (this is the format produced by other KAPPA
*     applications such as CURSOR); and
*
*     3) using a graphics cursor of a nominated device, for which an NDF
*     must already have been displayed on the device.
*
*     The x-y co-ordinates may be given as either data or pixel
*     (`world') co-ordinates.  If data co-ordinates are given, the input
*     NDFs must contain appropriate AXIS structures to allow the
*     corresponding pixel co-ordinates to be found.

*  Usage:
*     segment in1 in2 out

*  ADAM Parameters:
*     AXES( 2 ) = _INTEGER (Read)
*        The indices of the axes which span the plane containing the
*        polygon.  Two values should be given, each less than or equal
*        to the minimum of the number of dimensions in the two input
*        NDFs. [1,2]
*     CLEAR = _LOGICAL (Read)
*        Whether or not the image display device should be cleared
*        before opening it. [FALSE]
*     COLOUR = LITERAL (Read)
*        The colour in which to draw any graphics specified by
*        parameter PLOT.  The options are described below.
*
*          "MAX"          - The maximum colour index used for the
*                           display of the image.
*          "MIN"          - The minimum colour index used for the
*                           display of the image.
*          An integer     - The actual colour index.  It is constrained
*                           between 0 and the maximum colour index
*                           available on the device. 
*          A named colour - Uses the named colour from the palette, and
*                           if it is not present, the nearest colour
*                           from the palette is selected.
*
*        If the colour is to remain unaltered as the lookup table is
*        manipulated choose an integer between 0 and 15, or a named
*        colour.  The suggested default is the current value.
*
*        This parameter is ignored on window overlays, where the
*        overlay colour is used.  (Use the PALENTRY command to change
*        this colour.)  An overlay has the advantage that the crosses
*        or polygon can be erased using OVCLEAR once this task is
*        completed.  The parameter is also ignored for terminals.
*        [The current value, but equals "Green" if there is no current
*        value.]
*     COORDS( 2 ) = _REAL (Read)
*        A pair of x-y co-ordinates representing a single vertex.  It
*        is only used if parameter MODE is given the value "Interface".
*        A null value should be given when the final vertex has been
*        specified.
*     COSYS = LITERAL (Read)
*        The co-ordinate system in which the polygon vertices are
*        specified.  This can be either "WORLD" or "DATA".  If COSYS =
*        "DATA" is given, the input co-ordinates (however obtained) are
*        presumed to be data co-ordinates (as defined by AXIS structures
*        within the NDFs).  Otherwise, they are presumed to be world (or
*        `pixel') co-ordinates. [Current co-ordinate system]
*     DEVICE = DEVICE (Read)
*        The name of the graphics device on which an image is
*        displayed.  This is only used if parameter MODE is given the
*        value "Cursor".  Any graphics specified by parameter PLOT will
*        be produced on this device.  This device must support cursor
*        interaction, and belong to one of the following classes:
*        TERMINAL, IMAGE_DISPLAY, IMAGE_OVERLAY, WINDOW, and
*        WINDOW_OVERLAY.  [Current image-display-overlay device]
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
*     LOGFILE = FILENAME (Write)
*        The name of a text file in which the co-ordinates of the
*        polygon vertices are to be stored.  A null value (!) means that
*        no file is created. [!]
*     MODE = LITERAL (Read)
*        The mode by which the vertices of the polygonal segments are
*        to be obtained.  The options are as follows: "Interface"
*        defines via the parameter system, "Cursor" enables selection
*        by graphics cursor, and "File" reads them from a text file.
*        [Current interaction mode]
*     MAXPOLY = _INTEGER (Read)
*        The maximum number of polygons which can be used.  For
*        instance, this can be set to 1 to ensure that no more than 1
*        polygon is used (this sort of thing can be useful when writing
*        procedures or scripts).  A null value causes no limit to be
*        imposed (unless MODE="File" in which case a limit of 20 is
*        imposed). [!]
*     MINPOLY = _INTEGER (Read)
*        The minimum number of polygons which can be used.  For
*        instance, this can be set to 2 to ensure that at least 2
*        polygons are used.  The supplied value must be less than or
*        equal to the value given for MAXPOLY and must be greater than
*        zero. [1]
*     OUT = NDF (Write)
*        The output NDF.
*     PLOT = LITERAL (Read)
*        The type of graphics to be used to mark the position of each
*        selected vertex.  It is only used if parameter MODE is given
*        the value "Cursor".  PLOT can take any of the following values.
*
*          "Poly"  - Causes each vertex to be joined by a straight line
*                    to the previous vertex.  The last vertex is joined
*                    to the first vertex.
*
*          "Cross" - Each vertex is marked by a cross.
*
*          "None"  - No graphics are produced.
*       
*        The initial default is "Poly", then subsequently it is the
*        current value.  []
*     POLY1-POLY20 = FILENAME (Read)
*        Each of the parameters POLY1 to POLY20 are used to access text
*        files containing the x-y co-ordinates of the vertices of a
*        single polygon.  If a value is assigned to POLY1 on the
*        command line, you are not prompted for any of the remaining
*        parameters in this group; additional polygon files must also
*        be supplied on the command line.  Otherwise, you are prompted
*        for POLY1, then POLY2, etc. until a null value is given or
*        POLY20 is reached.
*     QUALITY = _LOGICAL (Read)
*        If a TRUE value is supplied for parameter QUALITY then quality
*        information is copied from the input NDFs to the output NDFs.
*        Otherwise, the quality information is not copied.  This
*        parameter is only accessed if all supplied input NDFs have
*        defined QUALITY components.  If any of the supplied input NDFs
*        do not have defined QUALITY components, then no quality is
*        copied.  Note, if a null input NDF is given then the
*        corresponding output QUALITY values are set to zero. [TRUE]
*     VARIANCE = _LOGICAL (Read) 
*        If a TRUE value is supplied for parameter VARIANCE then
*        variance information is copied from the input NDFs to the
*        output NDFs.  Otherwise, the variance information is not
*        copied.  This parameter is only accessed if all supplied input
*        NDFs have defined VARIANCE components.  If any of the supplied
*        input NDFs do not have defined VARIANCE components, then no
*        variances are copied.  Note, if a null input NDF is given then
*        the corresponding output VARIANCE values are set bad. [TRUE]

*  Examples:
*     segment in1=m51a in2=m51b out=m51_comp poly1=co-ords.lis mode=file
*        Copies a region of the NDF m51a to the corresponding position
*        in the output NDF m51_comp.  The region is defined by the list
*        of vertex co-ordinates held in text file co-ords.lis.  All
*        pixels in the output NDF which fall outside this region are
*        given the corresponding pixel values from NDF m51b.
*     segment in1=m51a out=m51_cut mode=cursor plot=poly accept
*        Copies a region of the NDF m51a to the corresponding position
*        in the output NDF m51_cut.  The region is defined by selecting
*        vertices using a graphics cursor.  The image m51a should
*        previously have been displayed.  Each vertex is joined to the
*        previous vertex by a green line on the graphics device.  The
*        ACCEPT keyword causes the suggested null default value for IN2
*        to be accepted.  This means that all pixels outside the region
*        identified using the cursor will be set bad in the output NDF.

*  Notes:
*     -  The routine can handle NDFs of arbitrary dimensionality.  If
*     either input has three or more dimensions then all planes in the
*     NDF are processed in the same way, that is the same polygonal
*     regions are extracted from each plane and copied to the
*     corresponding plane of the output NDF.  The polygon is usually
*     presumed to lie in the x-y plane (i.e. the plane spanned by the
*     first two axes of the NDF), but this can be changed by assigning
*     appropriate values to parameter AXES so that for instance the
*     polygon lies in the y-z plane (i.e. the plane spanned by axes 2
*     and 3).
*     -  The output NDF bounds are defined by the overlap region of the
*     input NDFs.
*     -  The log file has a comment line indicating whether the
*     co-ordinates are World or Data.

*  Related Applications:
*     KAPPA: ARDMASK, ERRCLIP, FILLBAD, FFCLEAN, PASTE, SETMAGIC,
*     THRESH.

*  Implementation Status:
*     -  This routine will propagate VARIANCE component values so long
*     as all supplied input NDFs have defined VARIANCE components, and
*     parameter VARIANCE is not FALSE.
*     -  This routine will propagate QUALITY component values so long
*     as all supplied input NDFs have defined QUALITY components, and
*     parameter QUALITY is not FALSE.
*     -  The UNITS, AXIS, LABEL, TITLE, WCS and HISTORY components are
*     propagated from the first supplied input NDF, together with all
*     extensions.
*     -  All non-complex numeric types are supported.  The following
*     data types are processed directly: _WORD, _INTEGER, _REAL,
*     _DOUBLE.

*  Authors:
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-NOV-1993 (DSB):
*        Original NDF version, based on MJC's IMAGE-format KAPPA
*        implementation.
*     1995 April 11 (MJC):
*        Added Notes and Related Applications, and more examples.
*        Moved old Notes to Implementation Status.  Made Examples and
*        Usage lowercase.  Various tidying and stylistic changes, and
*        typo's corrected.  Renamed parameter XY to COORDS.  Revised
*        call to various routines for obtaining the co-ordinates
*        and KPG1_XYD2W.  Used modern-style variable declarations.
*        Added headings to the commentary.  Made the non-warning
*        messages conditional.  Used AGI_BEGIN/END to delimit picture
*        processing.
*     1997 July 11 (MJC):
*        When POLY1 is given on the command line in file mode, it is
*        now possible to supply additional polygons through parameters
*        POLY2-POLY20 also given on the command line.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GKS_PAR'          ! GKS constants (e.g. GSET)
      INCLUDE 'NDF_PAR'          ! NDF_ constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'CTM_PAR'          ! KAPPA CTM_ constants
      INCLUDE 'GNS_PAR'          ! GNS constants 
      INCLUDE 'PAR_PAR'          ! PAR_ constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER IDIM               ! Number of dimensions for calculation
      PARAMETER ( IDIM = 2 )

      INTEGER MPEN               ! SGS pen number used to draw graphics
      PARAMETER ( MPEN = 3 )

      INTEGER MXPOLY             ! Max. no. of polygon files
      PARAMETER ( MXPOLY = 20 )

*  Local Variables:
      CHARACTER * ( NDF__SZTYP ) ATYPE ! Numeric type for mapping AXIS
                                 ! centres
      INTEGER AXES( IDIM )       ! Indices of axes spanning the plane
      CHARACTER * ( 80 ) BUF     ! Buffer for log file output
      INTEGER CI                 ! Colour index required for graphics
      LOGICAL CLEAR              ! Is graphics device to be cleared?
      INTEGER COLI               ! Original colour index of current pen
      REAL COLBND( IDIM )        ! Lower bounds of co-ordinates
      LOGICAL COLOUR             ! Workstation supports colour?
      CHARACTER * ( 5 ) COSYS    ! Nature of supplied co-ordinates
      REAL COUBND( IDIM )        ! Upper bounds of co-ordinates
      LOGICAL DATAVL             ! Are data co-ordinates available?
      INTEGER DEFAX( IDIM )      ! Default values for AXES
      DOUBLE PRECISION DOFSET( IDIM ) ! Axis co-ords at pixel co-ords
                                 ! (0,0)
      DOUBLE PRECISION DSCALE( IDIM ) ! Dimensions of a pixel in axis
                                 ! units
      CHARACTER * ( NDF__SZFTP ) DTYPE ! Data type for an output NDF
                                 ! component
      REAL DX( 4 )               ! Data x co-ordinates
      REAL DY( 4 )               ! Data y co-ordinates
      INTEGER FD                 ! FIO file descriptor for log file
      LOGICAL GOT1               ! Was the first NDF supplied?
      LOGICAL GOT2               ! Was the second NDF supplied?
      INTEGER I                  ! Loop count
      CHARACTER * ( NDF__SZTYP ) IDTYPE ! Numeric type for mapping DATA
                                 ! arrays
      INTEGER IERR               ! GKS error indicator
      INTEGER INDF1              ! NDF identifier for first input NDF
      INTEGER INDF2              ! NDF identifier for second input NDF
      INTEGER INDF3              ! NDF identifier for output NDF
      INTEGER IPIXX              ! Max. no. of columns in image display
      INTEGER IPIXY              ! Max. no. of lines in image display
      INTEGER IPMASK             ! Pointer to mapped mask
      INTEGER IPX                ! Pointer to array of x vertex co-ords
      INTEGER IPXY               ! Pointer to array of vertex co-ords
      INTEGER IPY                ! Pointer to array of y vertex co-ords
      INTEGER IWKID              ! GKS workstation identifier
      CHARACTER * ( NDF__SZTYP ) IVTYPE ! Numeric type for mapping VAR arrays
      INTEGER LBND( NDF__MXDIM ) ! Lower pixel bounds of the output NDF
      INTEGER LBUF               ! Used length of log file buffer
      INTEGER LNTYPE             ! Line type for current SGS pen
      LOGICAL LOGPOS             ! Create an output log file?
      LOGICAL LOOP               ! Should another polygon be processed?
      INTEGER LPNAME             ! Used length of parameter name
      REAL LWIDTH                ! The width of the current SGS pen
      INTEGER MAXPOL             ! Maximum number of polygons allowed
      INTEGER MINPOL             ! Minimum number of polygons allowed
      CHARACTER * ( 9 ) MODE     ! Source of input co-ordinates
      INTEGER NDIM               ! Number of dimensions in output NDF
      INTEGER NINTS              ! Number of greyscale levels available
      INTEGER NPOLY              ! Number of polygons processed so far
      INTEGER NVERT              ! Number of vertices in current polygon
      REAL OFFSET( IDIM )        ! Axis co-ords at pixel co-ords (0,0)
      INTEGER PEN                ! Current SGS pen
      INTEGER PICID0             ! AGI identifier for current picture
      INTEGER PICID1             ! AGI identifier for latest DATA pic.
      CHARACTER * ( 5 ) PLOT     ! Nature of required graphics
      CHARACTER * ( 6 ) PNAME    ! Current polygon file parameter name
      INTEGER POSCOD( IDIM )     ! Column locations of x and y in file
      INTEGER PSTATE             ! State of POLY1 parameter
      LOGICAL QUAL1              ! Does the 1st NDF have a QUALITY comp?
      LOGICAL QUAL2              ! Does the 2nd NDF have a QUALITY comp?
      LOGICAL QUAL3              ! Does the o/p NDF have a QUALITY comp?
      REAL SCALE( IDIM )         ! Dimensions of a pixel in axis units
      LOGICAL SETPLR             ! Polyline representation to be reset?
      INTEGER SLBND( IDIM )      ! Lower bounds of axes spanning polygon
      INTEGER SUBND( IDIM )      ! Upper bounds of axes spanning polygon
      INTEGER UBND( NDF__MXDIM ) ! Upper pixel bounds of the output NDF
      LOGICAL UNIT               ! Does AGI give a unit transformation?
      LOGICAL USE                ! Use the current polygon?
      CHARACTER * ( GNS__SZKEY ) WKCLAS ! Workstation class
      REAL WX( 4 )               ! World x co-ordinates
      REAL WY( 4 )               ! World y co-ordinates
      LOGICAL VAR1               ! Does the 1st NDF have VARIANCE comp?
      LOGICAL VAR2               ! Does the 2nd NDF have VARIANCE comp?
      LOGICAL VAR3               ! Does the o/p NDF have VARIANCE comp?
      INTEGER ZONE0              ! Identifier for current picture zone
      INTEGER ZONE1              ! Identifier for latest DATA picture
                                 ! zone

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the actual or null input NDFs.
*  =====================================

*  Ensure error reports are deferred.
      CALL ERR_MARK

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain an identifier for the first NDF, which defines the data to go
*  inside the polygonal segments.
      CALL NDG_ASSOCL( 'IN1', 'READ', INDF1, STATUS )

*  If a null value was supplied, annul the error (a null NDF identifier
*  will have been returned by NDG_ASSOCL in this case).
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
      CALL NDG_ASSOCL( 'IN2', 'READ', INDF2, STATUS )

*  If a null value was supplied, annul the error (a null NDF identifier
*  will have been returned by NDG_ASSOCL in this case).
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
         CALL NDG_PROPL( INDF1, 'UNITS,AXIS,WCS', 'OUT', INDF3, STATUS )

*  Otherwise, propagate the output NDF from the second input NDF.
      ELSE
         CALL NDG_PROPL( INDF2, 'UNITS,AXIS,WCS', 'OUT', INDF3, STATUS )
      END IF

*  Decide in which plane to specify polygonal segments.
*  ====================================================

*  Get the bounds of the output NDF.
      CALL NDF_BOUND( INDF3, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Set up the indices of the axes which span the plane within which the 
*  supplied polygon lies. 
      DEFAX( 1 ) = 1
      DEFAX( 2 ) = 2
      CALL PAR_GDR1I( 'AXES', 2, DEFAX, 1, NDIM, .FALSE., AXES, 
     :                 STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Store the bounds of the axes which span the plane containing the
*  polygon.
      SLBND( 1 ) = LBND( AXES( 1 ) )
      SUBND( 1 ) = UBND( AXES( 1 ) )
      SLBND( 2 ) = LBND( AXES( 2 ) )
      SUBND( 2 ) = UBND( AXES( 2 ) )

*  Find the data array's implementation and output-NDF data types.
*  ===============================================================

*  If both input NDFs were supplied, find which numeric type should be
*  used to access the DATA arrays, and what numeric type the output
*  DATA component should be stored in.  The choice is made to avoid
*  unnecessary loss of precision.
      IF ( GOT1 .AND. GOT2 ) THEN
         CALL NDF_MTYPE( '_WORD,_INTEGER,_REAL,_DOUBLE', INDF1, INDF2,
     :                   'DATA', IDTYPE, DTYPE, STATUS )

*  If only one input NDF was supplied, we still need to find out which
*  is the best data type to use from the ones available.
      ELSE
         IF ( GOT1 ) THEN
            CALL NDF_MTYPE( '_WORD,_INTEGER,_REAL,_DOUBLE', INDF1,
     :                      INDF1, 'DATA', IDTYPE, DTYPE, STATUS )

         ELSE
            CALL NDF_MTYPE( '_WORD,_INTEGER,_REAL,_DOUBLE', INDF2,
     :                      INDF2, 'DATA', IDTYPE, DTYPE, STATUS )

         END IF

      END IF

*  Set the numeric type of the output DATA array.
      CALL NDF_STYPE( DTYPE, INDF3, 'DATA', STATUS )

*  Determine if VARIANCE is to be propagated, and its data types.
*  ==============================================================

*  See if variance information is to be copied to the output NDF.  This
*  is true if all the supplied input NDFs have associated variance
*  values.
      VAR3 = .NOT. ( ( GOT1 .AND. (.NOT. VAR1 ) ) .OR.
     :               ( GOT2 .AND. (.NOT. VAR2 ) ) )

*  Allow the user to suppress the copying of variance values.
      IF ( VAR3 ) CALL PAR_GET0L( 'VARIANCE', VAR3, STATUS )

*  If output variances are required, and both input NDFs have defined
*  variance values, match the numeric types of the VARIANCE arrays.
      IF ( VAR3 ) THEN
         IF ( VAR1 .AND. VAR2 ) THEN
            CALL NDF_MTYPE( '_WORD,_INTEGER,_REAL,_DOUBLE', INDF1,
     :                       INDF2, 'VARIANCE', IVTYPE, DTYPE, STATUS )

*  If only one input NDF has defined variances, we still need to find
*  out which is the best data type to use from the ones available.
         ELSE
            IF ( VAR1 ) THEN
               CALL NDF_MTYPE( '_WORD,_INTEGER,_REAL,_DOUBLE', INDF1,
     :                         INDF1, 'VARIANCE', IVTYPE, DTYPE,
     :                         STATUS )
            ELSE
               CALL NDF_MTYPE( '_WORD,_INTEGER,_REAL,_DOUBLE', INDF2,
     :                         INDF2, 'VARIANCE', IVTYPE, DTYPE,
     :                         STATUS )
            END IF

         END IF

*  Set the numeric type of the output VARIANCE array.
         CALL NDF_STYPE( DTYPE, INDF3, 'VARIANCE', STATUS )

      END IF

*  Determine if QUALITY is to be propagated.
*  =========================================

*  See if quality information is to be copied to the output NDF.  This
*  is true if all the supplied input NDFs have associated quality
*  values.  Note, if only one input NDF was given, the undefined pixels
*  will be given a quality of zero in the output.  This may or may not
*  be appropriate!  Quality values are always mapped as unsigned bytes.
      QUAL3 = .NOT. ( ( GOT1 .AND. (.NOT. QUAL1 ) ) .OR.
     :                ( GOT2 .AND. (.NOT. QUAL2 ) ) )

*  Allow the user to suppress the copying of quality values.
      IF ( QUAL3 ) CALL PAR_GET0L( 'QUALITY', QUAL3, STATUS )

*  Find how and in what co-ordinate system the polygons will be defined.
*  =====================================================================

*  See if Data or World co-ordinates will be supplied, and tell the
*  user.
      CALL PAR_CHOIC( 'COSYS', 'World', 'Data,World', .FALSE., COSYS,
     :                STATUS )
      CALL MSG_SETC( 'SYS', COSYS )
      CALL MSG_OUTIF( MSG__NORM, 'SEGMENT_MSG1', 'Using ^SYS '/
     :              /'co-ordinates to define the polygon.', STATUS )

*  See whether the co-ordinates of the polygon vertices are to be
*  specified using the parameter system, graphics cursor, or a text
*  file.
      CALL PAR_CHOIC( 'MODE', 'File', 'Interface,Cursor,File',
     :                .TRUE., MODE, STATUS )

*  Set up graphics for cursor mode.
*  ================================

*  If cursor mode is selected, set up the graphics system (SGS).
      IF ( MODE .EQ. 'CURSOR' ) THEN

*  See whether the current picture is to be cleared before creating the
*  new plot.
         CALL PAR_GTD0L( 'CLEAR', .TRUE., .TRUE., CLEAR, STATUS )

*  Start a new AGI context.
         CALL AGI_BEGIN

*  Open the graphics database for the selected device, obtaining
*  identifiers for the current AGI picture and corresponding SGS zone.
         IF ( CLEAR ) THEN
            CALL AGS_ASSOC( 'DEVICE', 'WRITE', ' ', PICID0, ZONE0,
     :                      STATUS )
         ELSE
            CALL AGS_ASSOC( 'DEVICE', 'UPDATE', ' ', PICID0, ZONE0,
     :                      STATUS )
         END IF

*  Find the most recent DATA picture contained within the current
*  picture, and make it the current picture.
         CALL KPG1_AGFND( 'DATA', PICID1, STATUS )

*  Get an identifier for the corresponding SGS zone.
         CALL AGS_NZONE( ZONE1, STATUS )

*  Report the name, comment, and label, if one exists, for the current
*  picture.
         CALL KPG1_AGATC( STATUS )

*  See what type of graphics are required.
         CALL PAR_CHOIC( 'PLOT', 'Poly', 'Poly,Cross,None', .TRUE., 
     :                   PLOT, STATUS )

*  Define the graphics style.
*  ==========================
         SETPLR = .FALSE.

*  If graphics are required...
         IF ( PLOT .NE. 'NONE' ) THEN

*  Determine the type of the device.

*  Start the GNS system for GKS.
            CALL GNS_START( 'GKS', STATUS )

*  Inquire the workstation identifier for GKS inquiries.
            CALL SGS_ICURW( IWKID )

*  Find the class of the workstation.
            CALL GNS_IWCG( IWKID, 'CLASS', WKCLAS, STATUS )

*  Stop the GNS system for GKS.
            CALL GNS_STOP( 'GKS', STATUS )

*  Check for an unsupported device.
            IF ( WKCLAS .NE. 'IMAGE_DISPLAY' .AND.
     :           WKCLAS .NE. 'IMAGE_OVERLAY' .AND.
     :           WKCLAS .NE. 'TERMINAL' .AND. WKCLAS .NE. 'WINDOW' .AND.
     :           WKCLAS .NE. 'WINDOW_OVERLAY' ) THEN

               STATUS = SAI__ERROR
               CALL MSG_SETC( 'CLASS', WKCLAS )
               CALL ERR_REP( 'CURSOR_INVDEV',
     :           'The chosen device is of the wrong class (^CLASS).  '/
     :           /'It should be a TERMINAL, WINDOW, WINDOW_OVERLAY, '/
     :           /'IMAGE_DISPLAY, or IMAGE_OVERLAY.', STATUS )
               GOTO 999

            ELSE
     :
*  Want solid lines for the plotting.
               CALL KPG1_SOLIN( STATUS )

*  Inquire the current SGS pen, and then select the pen used to draw
*  markers by the KAPPA routine which gets cursor positions.
               CALL SGS_IPEN( PEN )
               CALL SGS_SPEN( MPEN )

*  Determine if the device supports colour.
               CALL KPG1_QCOL( COLOUR, STATUS )
      
               IF ( WKCLAS .NE. 'TERMINAL' ) THEN

*  For window overlay or monochrome, there is no choice of colour
*  index.  It must be 1.
                  IF ( WKCLAS .EQ. 'WINDOW_OVERLAY' .OR.
     :                 .NOT. COLOUR ) THEN
                     CI = 1

*  Obtain the maximum number of colour indices.
                  ELSE
                     CALL KPG1_QIDAT( 'DEVICE', 'SGS', NINTS, IPIXX,
     :                                IPIXY, STATUS )

*  See what pen is to be used to draw the graphics.  If there is only
*  one pen there is no choice.  Use index 1.
                     IF ( NINTS .EQ. 1 ) THEN
                        CI = 1

*  Device cannot have reserved pens.  Just select from what is
*  available.
                     ELSE IF ( NINTS .LT. CTM__RSVPN ) THEN
                        CALL KPG1_MACOL( 'COLOUR', 0, NINTS - 1, CI,
     :                                   STATUS )

*  Select from a palette and rest of the coluor indices.
                     ELSE
                        CALL KPG1_MACOL( 'COLOUR', CTM__RSVPN,
     :                                   NINTS - 1, CI, STATUS )
                     END IF
                  END IF

*  Inquire the current colour index of this pen (it will be restored
*  after all plotting is complete).
                  CALL GQPLR( IWKID, MPEN, GSET, IERR, LNTYPE, LWIDTH,
     :                        COLI )

*  Store the new colour index for this pen.
                  CALL GSPLR( IWKID, MPEN, LNTYPE, LWIDTH, CI )
                  SETPLR = .TRUE.

*  See if a GKS error has occurred.
                  CALL GKS_GSTAT( STATUS )
               END IF
            END IF
         END IF

*  Derive the actual co-ordinate system to be used.
*  ================================================

*  If the data co-ordinates selected by the user are to be used to
*  define the polygon (rather than the world co-ordinates), we need to
*  see if a `world-to-data' transformation is stored with the picture.
         IF ( COSYS .EQ. 'DATA' ) THEN

*  As yet there is no AGI inquiry for this.  If no such transform
*  exists, then AGI uses a unit transformation instead.  A test for a
*  unit transformation can thus give a warning that there may be no
*  data co-ordinates available (but of course the unit transformation
*  may actually have been specified as the world-to-data 
*  transformation).  To do the test, transform some arbitrary world 
*  co-ordinates to data co-ordinates. 
            WX( 1 ) = -100.0
            WX( 2 ) = +100.0
            WX( 3 ) = +100.0
            WX( 4 ) = -100.0
            WY( 1 ) = -100.0
            WY( 2 ) = -100.0
            WY( 3 ) = +100.0
            WY( 4 ) = +100.0
            CALL AGI_TWTOD( -1, 4, WX, WY, DX, DY, STATUS )

*  See if the data and world co-ordinates are the same.
            UNIT = .TRUE.
            DO I = 1, 4
               IF ( ABS( WX( I ) - DX( I ) ) .GT. VAL__SMLR .OR.
     :              ABS( WY( I ) - DY( I ) ) .GT. VAL__SMLR ) 
     :            UNIT = .FALSE.
            END DO

*  If there appears to be no world-to-data transformation stored with 
*  the picture, tell the user and use world co-ordinates instead of data
*  co-ordinates to define the polygonal region in the output NDF.
            IF ( UNIT ) THEN
               CALL MSG_BLANK( STATUS )
               CALL MSG_OUT( 'SEGMENT_MSG2', 'WARNING: The picture '/
     :           /'appears to have no data co-ordinate system.  World '/
     :           /'co-ordinates will be used to define the polygonal '/
     :           /'region.', STATUS )
               CALL MSG_BLANK( STATUS )
               COSYS = 'WORLD'
            END IF

         END IF

*  If no cursor is being used, no graphics can be produced.
      ELSE
         PLOT = 'NONE'
      END IF

*  Obtain the co-ordinate system.
*  ==============================

*  If Data co-ordinates are to be supplied, we need to find out how to
*  transform data co-ordinates into world co-ordinates within the output
*  NDF.
      IF ( COSYS .EQ. 'DATA' ) THEN

*  Find the precision needed for the system of axis arrays.
         CALL NDF_ATYPE( INDF3, 'Centre', 0, ATYPE, STATUS )

*  Obtain the scales and offsets of the linear transformation from
*  pixel co-ordinates to data co-ordinates.  Axis co-ordinates must be
*  monotonic to be usable.  If the axes are non-linear a warning
*  message is issued and a linear approximation to the axis
*  co-ordinates is returned.  Note that if we were to map a
*  single-precision axis array as double precision, the linearity might
*  be violated merely because of the increased sensitivity of the test.
*  Thus the testing is done with the appropriate type.  Hereafter though
*  the single-precision transformation is adequate for the job.
         IF ( ATYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG1_CHAXD( INDF3, IDIM, AXES, DATAVL, DSCALE, DOFSET,
     :                       STATUS )
            DO I = 1, IDIM
               SCALE( I ) = SNGL( DSCALE( I ) )
               OFFSET( I ) = SNGL( DOFSET( I ) )
            END DO
         ELSE
            CALL KPG1_CHAXR( INDF3, IDIM, AXES, DATAVL, SCALE, OFFSET,
     :                       STATUS )
         END IF

*  If no usable AXIS structures have been found, report an error and
*  abort.
         IF ( ( .NOT. DATAVL ) .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SEGMENT_ERR2', 'No usable Data '/
     :        /'co-ordinates can be found in the supplied NDFs.',
     :        STATUS )
            GO TO 999
         END IF
   
      END IF

*  Prepare File mode.
*  ==================

*  If file mode has been selected, see if a command-line value has
*  been supplied for the first polygon co-ordinate file.  If so, set a
*  flag to suppress looping.
      IF ( MODE .EQ. 'FILE' ) THEN
         CALL NDG_STATE( 'POLY1', PSTATE, STATUS )
         LOOP = PSTATE .NE. PAR__ACTIVE

      ELSE
         LOOP = .TRUE.
      END IF

*  Decides whether to process the first polygon (when given on the
*  command line), or all polygons otherwise.  For command-line use
*  this flag will change depending on whether or not the polygon file
*  is supplied on the command line or not.
      USE = .TRUE.

*  Obtain space to store the polygons.
*  ===================================

*  The polygons defined by the user are initially stored in a
*  2-dimensional mask image which indicates which pixels are inside a
*  polygon and which are not.  Get workspace to hold the mask.
      CALL PSX_CALLOC( ( SUBND( 1 ) - SLBND( 1 ) + 1 ) *
     :                 ( SUBND( 2 ) - SLBND( 2 ) + 1 ), '_LOGICAL',
     :                 IPMASK, STATUS )

*  Obtain a log file.
*  ==================

*  Attempt to obtain and open a log file to output the results.  A null
*  value, meaning no log file is required, is handled invisibly.
      LOGPOS = .FALSE.
      CALL ERR_MARK
      CALL FIO_ASSOC( 'LOGFILE', 'WRITE', 'LIST', 80, FD, STATUS )

      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
         LOGPOS = .TRUE.
      END IF
      CALL ERR_RLSE

*  If a log file is being created, write a descriptive comment to it and
*  tell the user.
      IF ( LOGPOS ) THEN
         CALL FIO_WRITE( FD, '# Log file created by SEGMENT', STATUS )

         IF ( COSYS .EQ. 'WORLD' ) THEN
            CALL FIO_WRITE( FD, '# The following are WORLD '//
     :                     'co-ordinates.', STATUS )
         ELSE
            CALL FIO_WRITE( FD, '# The following are DATA '//
     :                     'co-ordinates.', STATUS )
         END IF

         CALL MSG_OUTIF( MSG__NORM, 'SEGMENT_MSG3',
     :                   'Logging to $LOGFILE', STATUS )

      END IF

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain the minimum and maximum number of polygons.
*  ==================================================

*  Get the maximum number of polygons which may be given. There are only
*  a limited number of parameters for accessing input text files, so if
*  MODE=FILE, an absolute limit of MXPOLY has to be imposed.  If a null 
*  value is supplied, annul the error and use the maximum value (it is 
*  done this way instead of by setting the PAR_GDR0I argument NULL to 
*  .TRUE. to avoid the message produced by PAR_GDR0I).
      IF ( MODE .EQ. 'FILE' ) THEN 
         CALL PAR_GDR0I( 'MAXPOLY', MXPOLY, 1, MXPOLY, .FALSE., MAXPOL,
     :                    STATUS )

         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            MAXPOL = MXPOLY
         END IF

      ELSE
         CALL PAR_GDR0I( 'MAXPOLY', VAL__MAXI, 1, VAL__MAXI, .FALSE.,
     :                   MAXPOL, STATUS )

         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            MAXPOL = VAL__MAXI
         END IF

      END IF      

*  Now get the minimum number of polygons which must be processed.
      CALL PAR_GDR0I( 'MINPOLY', 1, 1, MAXPOL, .FALSE., MINPOL, STATUS )

*  Main loop for each polygon.
*  ===========================
*  ---------------------------

*  Initialise the number of polygons inserted into the mask so far.
      NPOLY = 0

*  Arrive here when a new polygon is to be added to the mask.
   10 CONTINUE

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain the co-ordinates in the file mode.
*  =========================================

*  Get the list of co-ordinates defining the vertices of the new
*  polygon.  First deal with file mode.
      IF ( MODE .EQ. 'FILE' ) THEN

*  Construct the name of the parameter used to access the next polygon 
*  file.
         PNAME = 'POLY  '
         LPNAME = 4
         CALL CHR_PUTI( NPOLY + 1, PNAME, LPNAME )

*  In no-prompt mode (i.e. when POLY1 is supplied on the command line)
*  check when the next polygon is supplied there too.
         IF ( NPOLY .GT. 0 .AND. .NOT. LOOP ) THEN
            CALL NDG_STATE( PNAME( :LPNAME ), PSTATE, STATUS )
            USE = PSTATE .EQ. PAR__ACTIVE
         END IF

*  Obtain the co-ordinate list.  Pointers to workspace holding the x
*  and y co-ordinate values are returned, together with the number of
*  vertices in the polygon.  Co-ordinates are stored in single
*  precision.
         IF ( USE ) THEN
            POSCOD( 1 ) = 1
            POSCOD( 2 ) = 2
            CALL KPG1_FLCOR( PNAME( :LPNAME ), IDIM, POSCOD, NVERT,
     :                       IPXY, COLBND, COUBND, STATUS )
         END IF

*  Obtain the co-ordinates in the cursor mode.
*  ===========================================

*  Now deal with cursor mode.  The returned co-ordinates are world
*  co-ordinates if COSYS is WORLD.  If COSYS is DATA then the returned
*  co-ordinates are in the data co-ordinate system of the displayed
*  picture. 
      ELSE IF ( MODE .EQ. 'CURSOR' ) THEN
         CALL KPS1_CUXYR( COSYS, PLOT, NVERT, IPX, IPY, STATUS )

*  Obtain the co-ordinates in the interface mode.
*  ==============================================
      ELSE 
         CALL KPG1_INCOR( 'COORDS', IDIM, NVERT, IPXY, STATUS )

      END IF

      IF ( USE ) THEN

*  Validate the polygon.
*  =====================

*  If no valid polygon was obtained, check that the minimum number of
*  polygons has been processed.  If not, warn the user, and go back for
*  another polygon.
         IF ( NVERT .EQ. 0 ) THEN

            IF ( NPOLY .LT. MINPOL ) THEN
               CALL MSG_BLANK( STATUS )
               CALL MSG_SETI( 'MIN', MINPOL )
               CALL MSG_OUT( 'SEGMENT_MSG4', 'Please give another '/
     :           /'polygon (at least ^MIN must be given in total).',
     :           STATUS )
               CALL MSG_BLANK( STATUS )
               GO TO 10
            END IF

*  If a valid polygon was obtained, process it.
         ELSE

*  Unzip the co-ordinate array into x and y co-ordinate arrays.
*  ============================================================

*  Some routines need x and y explicitly, though others use a single
*  array to work n-dimensionally.  Could have more specific routines
*  which are more work to maintain, or accept the differences and
*  use the appropriate storage for the job in hand.   The latter
*  approach is used.
            IF ( MODE .EQ. 'FILE' .OR. MODE .EQ. 'INTERFACE' ) THEN

*  Get two work arrays to hold the x and y co-ordinates of the vertices.
               CALL PSX_CALLOC( NVERT, '_REAL', IPX, STATUS )
               CALL PSX_CALLOC( NVERT, '_REAL', IPY, STATUS )

*  Split the 2-dimensional array into the x-y arrays.
               CALL KPG1_UNZ2R( NVERT, %VAL( IPXY ), %VAL( IPX ),
     :                          %VAL( IPY ), STATUS )

*  Free the original array.
               CALL PSX_FREE( IPXY, STATUS )
            END IF

*  Log the current polygon and co-ordinates.
*  =========================================

*  Write a comment to the log file identifying the current polygon.
            IF ( LOGPOS ) THEN
               CALL FIO_WRITE( FD, '#', STATUS )
               CALL MSG_SETI( 'NPOLY', NPOLY + 1 )
               CALL MSG_LOAD( ' ', '# Polygon no: ^NPOLY', BUF, LBUF,
     :                        STATUS )
               CALL FIO_WRITE( FD, BUF( : LBUF ), STATUS )

*  Write the positions in the input co-ordinate system to the log file.
               CALL KPS1_LOGXY( FD, NVERT, %VAL( IPX ), %VAL( IPY ),
     :                          STATUS )

            END IF

*  Convert data to world co-ordinates.
*  ===================================

*  If Data co-ordinates have been supplied, convert them to world
*  co-ordinates.  Note the other arrays are only working in single
*  precision (imposed by the cursor mode), the single-precision
*  conversion is used.  Also this is probably adequate as we only want
*  to find the next pixel.
            IF ( COSYS .EQ. 'DATA' ) THEN
               CALL KPG1_D2W2R( SCALE, OFFSET, NVERT, %VAL( IPX ),
     :                          %VAL( IPY ), STATUS )
            END IF

*  Add the current polygon it to the mask.
*  =======================================

*  Now insert this polygon into the mask.
            CALL KPS1_PLMSK( NPOLY, SLBND( 1 ), SUBND( 1 ), SLBND( 2 ),
     :                       SUBND( 2 ), NVERT, %VAL( IPX ),
     :                       %VAL( IPY ), %VAL( IPMASK ), STATUS )

*  Release the workspace used to hold the co-ordinates of the polygon
*  vertices.
            CALL PSX_FREE( IPX, STATUS )
            CALL PSX_FREE( IPY, STATUS )

*  Decide whether to loop or not.
*  ==============================

*  Increment the number of polygons in the mask.
            NPOLY = NPOLY + 1

*  If more polygons are to be defined, tell the user which polygon has
*  just been processed.
            IF ( LOOP ) THEN
               CALL MSG_OUTIF( MSG__NORM, 'BLANK', ' ', STATUS )
               CALL MSG_SETI( 'NPOLY', NPOLY )
               CALL MSG_OUTIF( MSG__NORM, 'SEGMENT_MSG5', 'Polygon '/
     :                         /'number ^NPOLY done.', STATUS )
            END IF

*  If the maximum number of polygons has not yet been reached loop back
*  to process another polygon.
            IF ( NPOLY .LT. MAXPOL ) GO TO 10

         END IF

      END IF

*  If no polygons have been processed, report an error.
      IF ( NPOLY .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SEGMENT_ERR3','No polygons have been '/
     :                 /'processed.', STATUS )

*  Copy the arrays within the polygon to the output NDF.
*  =====================================================

*  Otherwise, copy the input DATA values to the output, filling missing
*  values with bad values.  The mask just created is used to define
*  which pixels are inside a polygon and which are not.  If the NDF has
*  more than 2 dimensions, the mask is projected through the other
*  dimensions.
      ELSE
         CALL KPS1_PLCPY( INDF1, INDF2, INDF3, 'DATA', IDTYPE, GOT1,
     :                    GOT2, AXES, SLBND( 1 ), SUBND( 1 ),
     :                    SLBND( 2 ), SUBND( 2 ), %VAL( IPMASK ),
     :                    VAL__BADD, STATUS )

*  Now copy VARIANCE values if necessary, filling missing values with
*  bad values.
         IF ( VAR3 ) THEN
            CALL KPS1_PLCPY( INDF1, INDF2, INDF3, 'VARIANCE', IVTYPE,
     :                       VAR1, VAR2, AXES, SLBND( 1 ), SUBND( 1 ),
     :                       SLBND( 2 ), SUBND( 2 ), %VAL( IPMASK ),
     :                       VAL__BADD, STATUS )
         END IF

*  Now copy QUALITY values if necessary, filling missing values with
*  zeros.  QUALITY values are always accessed as unsigned byte values.
         IF ( QUAL3 ) THEN
            CALL KPS1_PLCPY( INDF1, INDF2, INDF3, 'QUALITY', '_UBYTE',
     :                       QUAL1, QUAL2, AXES, SLBND( 1 ), SUBND( 1 ),
     :                       SLBND( 2 ), SUBND( 2 ), %VAL( IPMASK ),
     :                       0.0D0, STATUS )
         END IF
      END IF

*  Closedown sequence.
*  ===================
*  
*  If necessary, re-instate the original colour index for the marker
*  pen, and reinstate the original pen.
      IF ( PLOT .NE. 'NONE' ) THEN
         IF ( SETPLR ) CALL GSPLR( IWKID, MPEN, LNTYPE, LWIDTH, COLI )
         CALL SGS_SPEN( PEN )
      END IF

*  Arrive here if an error has occurred.
 999  CONTINUE

*  Close the file storing positions if present.
      IF ( LOGPOS ) CALL FIO_ANNUL( FD, STATUS )

*  Release the workspace used to hold the mask.
      CALL PSX_FREE( IPMASK, STATUS )

*  If cursor mode was selected, select the original picture and shut
*  down the graphics device.  Use a new error reporting context to
*  ensure that the AGI picture is re-instated even if an error has
*  occurred.
      IF ( MODE .EQ. 'CURSOR' ) CALL AGI_END( -1, STATUS )

*  If an error occurred, attempt to delete the output NDF.
      IF ( STATUS .NE. SAI__OK ) CALL NDF_DELET( INDF3, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SEGMENT_ERR4',
     :   'SEGMENT: Error copying a segment of an NDF into another NDF.',
     :   STATUS )
      END IF

*  Release the error stack.
      CALL ERR_RLSE

      END
