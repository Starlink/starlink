      SUBROUTINE OUTLINE( STATUS )
*+
*  Name:
*     OUTLINE

*  Purpose:
*     Draws aligned outlines of multiple NDFs.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL OUTLINE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine draws on a graphics device the outlines of a set of
*     NDFs in their Current attached coordinate system.  This will
*     show their relative positions in their current coordinates,
*     and so can, for instance, be used to check that alignment looks
*     sensible prior to resampling and combining into a mosaic.
*     Each outline indicates the extent of the data of the 
*     corresponding NDF; no graphical indication is given of the value
*     or quality of pixels.  Each outline is therefore basically
*     rectangular in shape, though it may be distorted if the mapping
*     between pixel and Current coordinates is nonlinear.
*     The origin (minimum X,Y pixel value) of each outline is marked
*     by a dot, and by a label giving the NDF's name and/or index number
*     (as determined by the LABEL parameter).  This label is written
*     parallel to the X direction, or anti-parallel if the mapping
*     between pixel and Current coordinates includes a reflection.
*
*     The results are only likely to be sensible if the Current 
*     coordinate system of all the NDFs is one in which they are all
*     registered.  If they do not all have the same current Domain
*     name, a warning will be issued.
*
*     OUTLINE is aware of the AGI graphics database; if the CLEAR
*     parameter is set to false then it will attempt to align the
*     outline drawings with suitably registered images which are
*     already on the graphics device.  This allows one, for instance,
*     to overlay the outlines of a set of NDFs on a mosaic image
*     which has been constructed using those NDFs.
*
*     This routine is designed for use on two-dimensional NDFs; if any
*     of the NDFs presented has more than two dimensions, any extra
*     ones will be ignored.

*  Usage:
*     outline in [device]

*  ADAM Parameters:
*     AXES = _LOGICAL (Read)
*        True if labelled and annotated axes are to be drawn around the
*        outlines, showing the common Current coordinate frame of the
*        NDFs.  The appearance of the axes can be controlled using the
*        STYLE parameter.
*        [TRUE]
*     CLEAR = _LOGICAL (Read)
*        If true, the graphics device will be cleared before the plot
*        is made.  If you want the outlines to be drawn over the top
*        of an existing DATA picture, for instance one displayed with
*        KAPPA's DISPLAY application, then set CLEAR to false.  If
*        possible, alignment will occur within the Current coordinate
*        system of the NDF.  If this is not possible, an attempt is
*        made in SKY, PIXEL or GRID domains.  If the image cannot be
*        aligned in any suitable domain, then OUTLINE will terminate
*        with an error.
*        [TRUE]
*     DEVICE = DEVICE (Read)
*        The name of the device on which to draw the outlines.
*        [Current display device]
*     IN = LITERAL (Read)
*        A list of the NDFs whose outlines are to be drawn.
*     LABEL = LITERAL (Read)
*        This is a comma-separated list of keywords indicating what 
*        sort of label should be written at the origin of the outlines.
*        Any combination of the following keywords may be used:
*           - NAME   -- The name of the NDF is written
*           - INDEX  -- The index number of the NDF is written
*           - DOT    -- The origin of the NDF is marked by a dot
*
*        [NAME,DOT]
*     LOGFILE = FILENAME (Read)
*        Name of the CCDPACK logfile.  If a null (!) value is given for
*        this parameter then no logfile will be written, regardless of
*        the value of the LOGTO parameter.
*
*        If the logging system has been initialised using CCDSETUP
*        then the value specified there will be used. Otherwise, the
*        default is "CCDPACK.LOG".
*        [CCDPACK.LOG]
*     LOGTO = LITERAL (Read)
*        Every CCDPACK application has the ability to log its output
*        for future reference as well as for display on the terminal.
*        This parameter controls this process, and may be set to any
*        unique abbreviation of the following:
*           -  TERMINAL  -- Send output to the terminal only
*           -  LOGFILE   -- Send output to the logfile only (see the
*                           LOGFILE parameter)
*           -  BOTH      -- Send output to both the terminal and the
*                           logfile
*           -  NEITHER   -- Produce no output at all
*
*        If the logging system has been initialised using CCDSETUP
*        then the value specified there will be used. Otherwise, the
*        default is "BOTH".
*        [BOTH]
*     PENROT = _LOGICAL (Read)
*        If TRUE, each outline will be drawn with a different pen (colour).
*        Otherwise, they will all be drawn in the same pen.
*        [FALSE]
*     STYLE = LITERAL (Read)
*        A group of attribute settings describing the plotting style 
*        to use for the outlines and annotated axes.  This should be
*        a string consisting of comma-separated `attribute=value'
*        items, as explained in the `Plotting Styles and Attributes'
*        section of SUN/95.
*        []

*  Examples:
*     {routine_example_text}
*        {routine_example_description}
*     [routine_example]...

*  Notes:
*     {routine_notes}...

*  Behaviour of Parameters:
*     All parameters retain their current value as default. The
*     "current" value is the value assigned on the last run of the
*     application. If the application has not been run then the
*     "intrinsic" defaults, as shown in the parameter help, apply.
*
*     Retaining parameter values has the advantage of allowing you to
*     define the default behaviour of the application. The intrinsic
*     default behaviour of the application may be restored by using the
*     RESET keyword on the command line.
*
*     Certain parameters (LOGTO and LOGFILE) have global
*     values. These global values will always take precedence, except
*     when an assignment is made on the command line. Global values may
*     be set and reset using the CCDSETUP and CCDCLEAR commands.
*
*     The DEVICE parameter also has a global association. This is not
*     controlled by the usual CCDPACK mechanisms, instead it works in
*     co-operation with KAPPA (SUN/95) image display/control routines.

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-DEC-2000 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! Standard AST constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants
      INCLUDE 'GRP_PAR'          ! Standard GRP constants
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameterisations

*  Local Constants:
      INTEGER MAXLAB             ! Maximum number of labelling options
      PARAMETER ( MAXLAB = 8 )

*  External references:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN           ! Used length of string

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER BL                 ! Buffer length
      INTEGER DIMS( 2, CCD1__MXNDF ) ! Dimensions of NDFs
      INTEGER FRM                ! AST identifier for a frame
      INTEGER FSET               ! AST identifier for global frameset
      INTEGER GID                ! NDG group identifier for input NDFs
      INTEGER HICOL              ! Highest colour index available
      INTEGER I                  ! Loop variable
      INTEGER INDF               ! NDF identifier
      INTEGER IPEN               ! Current pen index
      INTEGER IWCS               ! AST identifier for WCS frameset
      INTEGER J                  ! Loop variable
      INTEGER JCOM               ! Index of common frame in global FSET
      INTEGER LOCOL              ! Lowest colour index available
      INTEGER LW                 ! Normal plotting line width
      INTEGER MAP                ! AST identifier for a mapping
      INTEGER MLW                ! Line width for drawing origin dot
      INTEGER NAXES              ! Number of axes (dimensions) in a frame
      INTEGER NDIM               ! Number of returned dimensions
      INTEGER NLAB               ! Number of labelling options
      INTEGER NNDF               ! Number of input NDFs in group
      INTEGER NPEN               ! Number of distinct pens for rotation
      INTEGER NSTY               ! Number of style elements in group
      INTEGER PAXES( 2 )         ! Map for picking two axes from many
      INTEGER PENGID             ! GRP identifier for pen style strings
      INTEGER PICID              ! AGI identifier for initial DATA picture
      INTEGER PICOD              ! AGI identifier for new picture
      INTEGER PLOT               ! AST identifier of plot
      INTEGER STYLEN             ! Length of style element
      LOGICAL AXES               ! Draw axes?
      LOGICAL CLEAR              ! Clear the graphics device before plotting?
      LOGICAL DIFERS             ! Not all the same domain?
      LOGICAL LABNAM             ! Use labelling NAME option?
      LOGICAL LABIND             ! Use labelling INDEX option?
      LOGICAL LABDOT             ! Use labelling DOT option?
      LOGICAL PENROT             ! Rotate pen styles for different outlines?
      REAL GBOX( 4 )             ! Graph box for AST_PLOT definition
      REAL XCH                   ! Vertical baseline text character height
      REAL YCH                   ! Horizontal baseline text character height
      REAL UP( 2 )               ! Up direction for text
      DOUBLE PRECISION BBOX( 4 ) ! Base box for AST_PLOT definition
      DOUBLE PRECISION DIMOD     ! Length of the unit diagonal vector
      DOUBLE PRECISION DOT       ! Dot product
      DOUBLE PRECISION GLBND( 2 ) ! Lower GRID-like coordinate bounds
      DOUBLE PRECISION GUBND( 2 ) ! Upper GRID-like coordinate bounds
      DOUBLE PRECISION IXUP( 4 ) ! Input X up-direction coordinate vector
      DOUBLE PRECISION IYUP( 4 ) ! Input Y up-direction coordinate vector
      DOUBLE PRECISION LPOS( 2 ) ! Dummy low position
      DOUBLE PRECISION OFS       ! Offset length for text label
      DOUBLE PRECISION OLBND( 2 ) ! Upper common coordinate bounds
      DOUBLE PRECISION OUBND( 2 ) ! Upper common coordinate bounds
      DOUBLE PRECISION OXUP( 4 ) ! Output X up-direction coordinate vector
      DOUBLE PRECISION OYUP( 4 ) ! Output X up-direction coordinate vector
      DOUBLE PRECISION TPOS( 2 ) ! Text reference position
      DOUBLE PRECISION UPOS( 2 ) ! Dummy high position
      DOUBLE PRECISION VERTEX( 5, 2 ) ! GRID-like coordinates of array corners
      DOUBLE PRECISION XADD      ! Padding in X direction
      DOUBLE PRECISION XHI       ! Upper X coordinate of outline
      DOUBLE PRECISION XLO       ! Lower X coordinate of outline
      DOUBLE PRECISION XMAX      ! Upper X coordinate of bounding box
      DOUBLE PRECISION XMIN      ! Lower X coordinate of bounding box
      DOUBLE PRECISION YHI       ! Upper Y coordinate of outline
      DOUBLE PRECISION YADD      ! Padding in Y direction
      DOUBLE PRECISION YLO       ! Lower Y coordinate of outline
      DOUBLE PRECISION YMAX      ! Upper Y coordinate of bounding box
      DOUBLE PRECISION YMIN      ! Lower Y coordinate of bounding box
      CHARACTER * ( 2 ) JUST     ! Justification for text placement
      CHARACTER * ( 16 ) LABOPT( MAXLAB ) ! Labelling mode strings
      CHARACTER * ( 16 ) LFMT    ! Format string for text labels
      CHARACTER * ( 80 ) BUFFER  ! Line buffer for output
      CHARACTER * ( AST__SZCHR ) DMN ! Current domain for this NDF
      CHARACTER * ( AST__SZCHR ) COMDMN ! Domain of common frame
      CHARACTER * ( GRP__SZNAM ) NDFNAM ! Name of the NDF
      CHARACTER * ( GRP__SZNAM ) STYEL ! Style element

*  Local data:
      DATA PAXES / 1, 2 /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start up CCDPACK.
      CALL CCD1_START( 'OUTLINE', STATUS )

*  Start an NDF context.
      CALL NDF_BEGIN

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the list of NDFs for display.
      CALL CCD1_NDFGL( 'IN', 1, CCD1__MXNDF, GID, NNDF, STATUS )

*  Determine what the labelling options are required.
      CALL PAR_CHOIV( 'LABEL', MAXLAB, 'NAME,INDEX,DOT', LABOPT, NLAB,
     :                STATUS )
      LABNAM = .FALSE.
      LABIND = .FALSE.
      LABDOT = .FALSE.
      DO I = 1, NLAB
         IF ( LABOPT( I ) .EQ. 'NAME' ) THEN
            LABNAM = .TRUE.
         ELSE IF ( LABOPT( I ) .EQ. 'INDEX' ) THEN
            LABIND = .TRUE.
         ELSE IF ( LABOPT( I ) .EQ. 'DOT' ) THEN
            LABDOT = .TRUE.
         END IF
      END DO

*  Construct a labelling format string accordingly.
      IF ( LABNAM .AND. LABIND ) THEN
         LFMT = '^INDEX: ^NDF'
      ELSE IF ( LABNAM ) THEN
         LFMT = '^NDF'
      ELSE IF ( LABIND ) THEN
         LFMT = '^INDEX'
      ELSE
         LFMT = ' '
      END IF

*  Determine whether we will be rotating pens for different outlines.
      CALL PAR_GET0L( 'PENROT', PENROT, STATUS )

*  Determine whether we should clear the display device before plotting.
      CALL PAR_GET0L( 'CLEAR', CLEAR, STATUS )

*  Determine whether we will draw axes.
      IF ( CLEAR ) THEN
         CALL PAR_GET0L( 'AXES', AXES, STATUS )
      ELSE
         AXES = .FALSE.
      END IF

*  Initialise the global frameset with the Current frame of the 
*  reference (first) NDF.  First get the NDF identifier.
      CALL NDG_NDFAS( GID, 1, 'READ', INDF, STATUS )

*  Get the WCS component of the reference NDF.
      CALL NDF_GTWCS( INDF, IWCS, STATUS )
      CALL NDF_ANNUL( INDF, STATUS )

*  Get the Current frame of the reference NDF, which we would like to 
*  use to represent the common coordinate system.  Save its domain too.
      FRM = AST_COPY( AST_GETFRAME( IWCS, AST__CURRENT, STATUS ), 
     :                STATUS )
      COMDMN = AST_GETC( FRM, 'Domain', STATUS )

*  Initialise the global frameset.
      FSET = AST_FRAMESET( FRM, ' ', STATUS )

*  Initialise differing-domains flag.
      DIFERS = .FALSE.

*  Identify the initial (and so far only) frame in the global frameset
*  as the common frame.
      JCOM = AST_GETI( FSET, 'Base', STATUS )

*  Prepare for summary output to the user.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      BUFFER = ' '
      BUFFER( 4: ) = 'Index'
      BUFFER( 12: ) = 'NDF'
      BUFFER( 40: ) = 'Domain'
      CALL CCD1_MSG( ' ', BUFFER( 1:CHR_LEN( BUFFER ) ), STATUS )
      BUFFER( 4: ) = '-----'
      BUFFER( 12: ) = '---'
      BUFFER( 40: ) = '------'
      CALL CCD1_MSG( ' ', BUFFER( 1:CHR_LEN( BUFFER ) ), STATUS )

*  Store the information we need from each of the NDFs.
      DO I = 1, NNDF

*  Get the NDF name.
         CALL GRP_GET( GID, I, 1, NDFNAM, STATUS )

*  Get the NDF identifier.
         CALL NDG_NDFAS( GID, I, 'READ', INDF, STATUS )

*  Get the WCS component from the NDF.
         CALL NDF_GTWCS( INDF, IWCS, STATUS )

*  Check that the Current frame has the right number of dimensions.
         NAXES = AST_GETI( IWCS, 'Naxes', STATUS )
         IF ( NAXES .NE. 2 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'NDF', NDFNAM )
            CALL MSG_SETI( 'NAXES', NAXES )
            CALL CCD1_ERREP( ' ', 'OUTLINE: NDF ''^NDF'' has ^NAXES '
     :                    // 'axes in Current coordinates.', STATUS )
            CALL CCD1_ERREP( ' ', '         It must have 2.', STATUS )
            GO TO 99
         END IF

*  Get the Base (GRID-domain) frame of the NDF, and the mapping into it
*  from the Current (common) coordinate system.
         FRM = AST_GETFRAME( IWCS, AST__BASE, STATUS )
         MAP = AST_GETMAPPING( IWCS, AST__CURRENT, AST__BASE, STATUS )

*  Check it has the right number of dimensions.  If it has too few, 
*  then bail out.  If it has too many, then generate a new frame and
*  mapping which just represents the first two.
         NAXES = AST_GETI( FRM, 'Naxes', STATUS )
         IF ( NAXES .LT. 2 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'NDF', NDFNAM )
            CALL CCD1_ERREP( ' ', 'OUTLINE: NDF ''^NDF'' must have at '
     :                    // 'least 2 dimensions.', STATUS )
            GO TO 99
         ELSE IF ( NAXES .GT. 2 ) THEN
            CALL CCD1_MSG( ' ', ' ', STATUS )
            CALL MSG_SETC( 'NDF', NDFNAM )
            CALL MSG_SETI( 'IG', NAXES - 2 )
            CALL CCD1_MSG( ' ', '  Warning: Ignoring ^IG axes in NDF '
     :                  // '^NDF', STATUS )
            FRM = AST_PICKAXES( FRM, 2, PAXES, MAP, STATUS )
         END IF

*  Change the Domain of the Base frame; this doesn't make any difference
*  to this application, but the resulting global frameset will get 
*  written into the AGI database, where having a lot of frames all 
*  called GRID is unlikely to be a good thing.
         CALL AST_SETC( FRM, 'Domain', 'CCD_GRID-' // NDFNAM, STATUS )

*  Add the Base (GRID-domain) frame of the NDF to the global frameset,
*  using the correct mapping from the common coordinate system.
*  Note the index of this frame within the frameset will be JCOM + I.
         CALL AST_ADDFRAME( FSET, JCOM, MAP, FRM, STATUS )

*  Get the NDF extent in its Base (GRID-domain) frame.
         CALL NDF_DIM( INDF, 2, DIMS( 1, I ), NDIM, STATUS )

*  Output the name of the NDF and its Current frame domain to the user.
         BUFFER = ' '
         CALL MSG_FMTI( 'INDEX', 'I3', I )
         CALL MSG_LOAD( ' ', '^INDEX)', BUFFER( 4: ), BL, STATUS )
         CALL MSG_SETC( 'NDF', NDFNAM )
         CALL MSG_LOAD( ' ', '^NDF', BUFFER( 12: ), BL, STATUS )
         IF ( CHR_LEN( BUFFER ) .GT. 38 ) BUFFER( 36: ) = '...'
         DMN = AST_GETC( IWCS, 'Domain', STATUS )
         DIFERS = DIFERS .OR. ( DMN .NE. COMDMN )
         BUFFER( 40: ) = DMN
         CALL CCD1_MSG( ' ', BUFFER( 1:CHR_LEN( BUFFER ) ), STATUS )

*  Release the NDF.
         CALL NDF_ANNUL( INDF, STATUS )
      END DO

*  Warn the user if not all the Current frames in the NDFs had the same
*  domain.
      IF ( DIFERS ) THEN
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ', '  Warning: Not all NDF Current coordinate'
     :               // ' frames have the same domain.', STATUS )
      END IF

*  Set the Current frame of the frameset to the common coordinate system.
      CALL AST_SETI( FSET, 'Current', JCOM, STATUS ) 

*  We now have a global frameset containing one common frame and a frame 
*  representing a two-dimensional GRID-frame-like coordinate system
*  corresponding to each of the input NDFs.  We can use this to construct
*  an AST Plot object with which to address the graphics device.
      PLOT = AST__NULL

*  If we have been asked not to clear the graphics device, attempt to 
*  construct a Plot object in conjunction with the current contents of
*  the AGI database.
      IF ( .NOT. CLEAR ) THEN

*  Open the graphics device and select the DATA picture.
         CALL AGP_ASSOC( 'DEVICE', 'UPDATE', 'DATA', .FALSE., PICID,
     :                   STATUS )
         CALL AGI_SELP( PICID, STATUS )

*  Attempt to get a plot aligned with the AGI current picture.
         IF ( STATUS .NE. SAI__OK ) GO TO 99
         CALL CCD1_APLOT( FSET, PICID, .TRUE., PLOT, STATUS )

*  It would probably be a good idea to deactivate clipping here, since
*  it may be useful to see outlines which go outside of the plotted
*  image.  However, the PGSCLP call is not available in the Starlink
*  version of PGPLOT.
c        CALL AST_CLIP( PLOT, AST__NOFRAME, GLBND, GUBND, STATUS )
c        CALL PGSCLP( 0 )
      END IF

*  If we have been asked to clear the device, we need to determine the 
*  boundaries of the area in which to plot.
      IF ( PLOT .EQ. AST__NULL ) THEN
         XMIN = VAL__MAXD
         YMIN = VAL__MAXD
         XMAX = VAL__MIND
         YMAX = VAL__MIND
         GLBND( 1 ) = 0.5D0
         GLBND( 2 ) = 0.5D0

*  Loop for each NDF.
         DO I = 1, NNDF

*  Find a bounding box which will contain the data array of this NDF in 
*  the common frame.
            MAP = AST_GETMAPPING( FSET, JCOM + I, JCOM, STATUS )
            GUBND( 1 ) = DBLE( DIMS( 1, I ) ) + 0.5D0
            GUBND( 2 ) = DBLE( DIMS( 2, I ) ) + 0.5D0
            DO J = 1, 2
               CALL AST_MAPBOX( MAP, GLBND, GUBND, .TRUE., J, 
     :                          OLBND( J ), OUBND( J ), LPOS, UPOS, 
     :                          STATUS )
            END DO

*  Update limits if necessary.
            IF ( OLBND( 1 ) .LT. XMIN ) XMIN = OLBND( 1 )
            IF ( OLBND( 2 ) .LT. YMIN ) YMIN = OLBND( 2 )
            IF ( OUBND( 1 ) .GT. XMAX ) XMAX = OUBND( 1 )
            IF ( OUBND( 2 ) .GT. YMAX ) YMAX = OUBND( 2 )
         END DO

*  Add a little to the limits so the outlines don't butt up to the edges.
         XADD = ( XMAX - XMIN ) * 0.03D0
         YADD = ( YMAX - YMIN ) * 0.03D0
         XMIN = XMIN - XADD
         XMAX = XMAX + XADD
         YMIN = YMIN - YADD
         YMAX = YMAX + YADD

*  Open the graphics device using AGI unless we have already done so.
*  Leave a gap round the outside only if we will be drawing axes.
         IF ( CLEAR ) THEN
            CALL AGP_ASSOC( 'DEVICE', 'WRITE', ' ', AXES, PICID, 
     :                      STATUS )
         ELSE
            IF ( AXES ) CALL AGP_NVIEW( .TRUE., STATUS )
         END IF
         IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Set the PGPLOT viewport and world coordinates to give a viewport
*  large enough to hold all the outlines, of the correct aspect ratio,
*  and with enough space for labelling round the outside.
         CALL PGWNAD( REAL( XMIN ), REAL( XMAX ), REAL( YMIN ),
     :                REAL( YMAX ) )

*  Turn the global frameset into an AST Plot.
         CALL CCD1_APLOT( FSET, PICID, .FALSE., PLOT, STATUS )
      END IF

*  Apply default and user-selected style settings to the plot.
      CALL CCD1_PLSTY( PLOT, 'STYLE', STATUS )

*  Save the PGPLOT viewport as a new picture in the AGI database.
      CALL AGP_SVIEW( 'DATA', 'CCDPACK_OUTLINE', PICOD, STATUS )

*  Save the Plot in the new picture.
      CALL CCD1_SPLOT( PICOD, PLOT, STATUS )

*  Set the common frame index to its correct value for the Plot object;
*  it will be the Current frame of the Plot object, since it was the
*  Current frame of the Frameset passed to CCD1_APLOT.  The relative
*  positions of the grid-like frames within the PLOT frameset will
*  be the same as those in the FSET frameset, so as long as we know
*  the position of the common one, we can easily get the positions of
*  the others.
      JCOM = AST_GETI( PLOT, 'Current', STATUS )

*  Plot the coordinate axes if required.
      IF ( AXES ) THEN
         CALL AST_GRID( PLOT, STATUS )
      END IF

*  If we are rotating pens between plots, generate a group of pen style
*  attributes to cycle through.  Doing it like this is slightly more 
*  involved than just calculating it within the plotting loop, but 
*  makes it easier to customise if more complicated changes of style
*  are required.
      PENGID = GRP__NOID
      IF ( PENROT ) THEN

*  Create a new group.
         CALL GRP_NEW( 'CCDPACK:PENS', PENGID, STATUS )

*  Use a semicolon as the separator character since the comma is used 
*  within the names themselves.
         CALL GRP_SETCC( PENGID, 'DELIMITER', ';', STATUS )

*  Get the range of available colours.
         CALL PGQCOL( LOCOL, HICOL )

*  Write a group entry for each of the available colours, or one for each
*  NDF, whichever is the fewer.
         NPEN = MIN( HICOL - LOCOL + 1, NNDF )
         DO I = 1, NPEN
            CALL MSG_SETI( 'IPEN', I )
            CALL MSG_LOAD( ' ', 'Colour=^IPEN', STYEL, STYLEN, STATUS )
            CALL GRP_PUT( PENGID, 1, STYEL( 1 : STYLEN ), I, STATUS )
         END DO
      END IF

*  Initialise pen number.
      IPEN = AST_GETI( PLOT, 'Colour(curves)', STATUS )

*  Initialise constant vertex positions.
      XLO = 0.5D0
      YLO = 0.5D0
      DO J = 1, 5
         VERTEX( J, 1 ) = XLO
         VERTEX( J, 2 ) = YLO
      END DO

*  Set the size of the marker to plot at the origin (a fraction of a
*  character height, specified in units of 1/200 inches).
      CALL PGQCS( 1, XCH, YCH )
      MLW = MAX( 1, MIN( 200, XCH * 2D2 * 0.2D0 ) )

*  Set the length of the offset vector for text labels.
      CALL PGQCS( 4, XCH, YCH )
      OFS = XCH * 0.5D0

*  Set up points allowing calculation of unit vectors in the plot Base
*  frame for positioning of the text labels.
      IXUP( 1 ) = 0D0
      IYUP( 1 ) = 0D0
      IXUP( 2 ) = IXUP( 1 )
      IYUP( 2 ) = IYUP( 1 ) + 1D0
      IXUP( 3 ) = IXUP( 1 ) + 1D0
      IYUP( 3 ) = IYUP( 1 ) + 1D0
      IXUP( 4 ) = IXUP( 1 ) + 1D0
      IYUP( 4 ) = IYUP( 1 )

*  Loop for each NDF.
      DO I = 1, NNDF

*  Set pen colour if required.
         IF ( PENGID .NE. GRP__NOID ) THEN
            IF ( IPEN .GT. NPEN ) IPEN = 1
            CALL GRP_GET( PENGID, IPEN, 1, STYEL, STATUS )
            CALL AST_SET( PLOT, STYEL, STATUS )
            IPEN = IPEN + 1
         END IF

*  Set up an array giving the GRID-like coordinates of the corners of 
*  the data array for this NDF.  The fifth point is a copy of the first
*  one, which makes it easier for plotting a closed loop.
         XHI = DIMS( 1, I ) + 0.5D0
         YHI = DIMS( 2, I ) + 0.5D0
         VERTEX( 2, 1 ) = XHI
         VERTEX( 3, 1 ) = XHI
         VERTEX( 3, 2 ) = YHI
         VERTEX( 4, 2 ) = YHI

*  Set the Current frame of the Plot object to the GRID-like coordinate 
*  system of this NDF.
         CALL AST_SETI( PLOT, 'Current', JCOM + I, STATUS )

*  Plot a marker at the origin of each outline.
         IF ( LABDOT ) THEN
            CALL PGQLW( LW )
            CALL PGSLW( MLW )
            CALL AST_MARK( PLOT, 1, 2, 5, VERTEX, -1, STATUS )
            CALL PGSLW( LW )
         END IF

*  We now want to plot the name of the NDF near the origin of its outline.
*  Calculate the origin position and 'up' direction in plot base 
*  coordinates.
         IF ( LFMT .NE. ' ' ) THEN
            CALL AST_TRAN2( PLOT, 4, IXUP, IYUP, .FALSE., OXUP, OYUP,
     :                      STATUS )
            UP( 1 ) = REAL( OXUP( 2 ) - OXUP( 1 ) )
            UP( 2 ) = REAL( OYUP( 2 ) - OYUP( 1 ) )

*  Get the position of the outline origin, plus a small offset in the 
*  direction of the plot Current coordinate (1,1) diagonal.
            DIMOD = SQRT( ( OXUP( 3 ) - OXUP( 1 ) ) ** 2 +
     :                    ( OYUP( 3 ) - OYUP( 1 ) ) ** 2 )
            TPOS( 1 ) = OXUP( 1 ) + OFS * ( OXUP( 3 ) - OXUP( 1 ) )
     :                / DIMOD
            TPOS( 2 ) = OYUP( 1 ) + OFS * ( OYUP( 3 ) - OYUP( 1 ) )
     :                / DIMOD

*  Arrange for the text to sit inside the outline regardless of which
*  direction the coordinates increase in.  This is done by rotating the
*  UP vector by 90 degrees in the graphics frame, and seeing if the
*  result is parallel or anti-parallel to the NDF's positive X direction
*  in the graphics frame.
            DOT = ( OYUP( 2 ) - OYUP( 1 ) ) * ( OXUP( 4 ) - OXUP( 1 ) )
     :          + ( OXUP( 1 ) - OXUP( 2 ) ) * ( OYUP( 4 ) - OYUP( 1 ) )
            IF ( DOT .GT. 0D0 ) THEN
               JUST = 'BL'
            ELSE
               JUST = 'BR'
            END IF

*  Construct the labelling string.
            CALL GRP_GET( GID, I, 1, NDFNAM, STATUS )
            CALL MSG_SETC( 'NDF', NDFNAM )
            CALL MSG_SETI( 'INDEX', I )
            CALL MSG_LOAD( ' ', LFMT, BUFFER, BL, STATUS )

*  Write the label in the correct position and orientation.
            CALL AST_SETI( PLOT, 'Current', AST__BASE, STATUS )
            CALL AST_TEXT( PLOT, BUFFER( :BL ), TPOS, UP, JUST, STATUS )
            CALL AST_SETI( PLOT, 'Current', JCOM + I, STATUS )
         END IF

*  Plot geodesics in the GRID-like coordinate system along the edges
*  of the data array.  These will appear as the correct outlines in the
*  common coordinate system.
         CALL AST_POLYCURVE( PLOT, 5, 2, 5, VERTEX, STATUS )
      END DO

*  Error exit label.
 99   CONTINUE

*  Close down AGI.
      CALL AGP_DEASS( 'DEVICE', .FALSE., STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Annul group resources.
      CALL CCD1_GRDEL( GID, STATUS )
      CALL CCD1_GRDEL( PENGID, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( 'OUTLINE_ERR',
     :                  'OUTLINE: Outline plotting failed.', STATUS )
      END IF

*  Close CCDPACK logging system.
      CALL CCD1_END( STATUS )
      
      END
* $Id$
