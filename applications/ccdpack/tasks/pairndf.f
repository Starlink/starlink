      SUBROUTINE PAIRNDF( STATUS )
*+
*  Name:
*     PAIRNDF

*  Purpose:
*     Aligns images graphically by drag and drop.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PAIRNDF( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine accepts a list of NDFs which may be aligned using
*     simple offsets in their Current coordinate frames.
*     By making use of a graphical user interface you
*     can indicate how pairs of NDFs are aligned with respect
*     to each other, and mark image features to allow accurate
*     alignments.  Once enough pairings have been specified to register
*     all frames completely a global merger of all the positions
*     for each NDF takes place. This results in the output of one list
*     of uniquely labelled positions for each NDF. These position lists
*     can then be used in a routine such as REGISTER to produce the
*     actual transformation between the NDFs.
*
*     The graphical interface consists of two parts: a chooser which
*     allows you to inspect pairs of NDFs to see whether they are 
*     able to be paired, and an aligner which allows you to move 
*     a pair of images around so that they are registered, and to
*     mark points in the overlapping region where the same centroidable 
*     features exist in both images.
*
*     Operation is as follows.  You must first use the chooser window
*     to select a pair of images which have a region in common.
*     Use the tabs at either side of the screen to pick the image to
*     appear on that side.  You can use the "Show FITS" button to
*     select one or more FITS headers to be displayed alongside
*     each NDF if this will make it easier to identify which is which.
*     You can use the "Display cutoff" menu to select the percentiles
*     controlling the brightness of each pixel; alignment is easier if
*     the same features are of a similar brightness in different images.
*     The images are displayed resampled into their Current attached 
*     coordinate system; you can only align them using this program if 
*     a simple offset (translation) maps one onto another in these 
*     coordinates (or very nearly does so).  If that is not the case,
*     you will have to set their Current coordinate system 
*     to a different value (see WCSEDIT) or align them using a 
*     different method.  The whole of each image will be displayed in
*     the chooser window, and you can resize it in the usual way to 
*     make the images appear bigger or smaller.  Select a pair with 
*     an overlapping region which you wish to align, and click the 
*     "Use this pair" button.  The aligner window will then appear, 
*     displaying the two images which you have selected.
*
*     In the aligner window you can drag either of these images around
*     the display region by holding down mouse button 1 (usually the 
*     left one) as you move the mouse; the easiest way to align the pair 
*     is to "pick up" one image by an identifiable feature and "drop" it
*     on the same feature in the other image.  Where the images overlap
*     their pixels will be averaged.  If they are not correctly 
*     positioned, you can move them again.  Once you are happy that
*     they are aligned about right, then click in the overlap region
*     to mark features which appear in both images - these will be
*     centroided to get an accurate alignment.  During this
*     part you mark points by clicking with mouse button 1 (usually
*     the left one) and you can remove them by clicking with button 3
*     (usually the right one).  The window can be resized, the 
*     magnification can be changed using the "Zoom" control, and the 
*     display region scrolled using the scrollbars.  When you have aligned 
*     the images and marked shared features, or if you decide that
*     the pair cannot be satisfactorily registered, click the "Done" 
*     button.  
*
*     You will then be returned to the chooser window to select another
*     pair and repeat the process.  After the first time however, 
*     you will only be allowed to select a pair of images to align
*     if at least one of them has already been aligned already.  Those
*     which have already been done are marked with a `+' sign on their
*     selection tabs.
*
*     Once you have made enough pairings to register the whole set, the
*     graphical windows will disappear and the program will complete
*     the global matching up of positions without any further user 
*     interaction.

*  Usage:
*     pairndf in outlist percentiles

*  ADAM Parameters:
*     IN = LITERAL (Read)
*        A list of NDF names whose data are to be transformed. The NDF
*        names should be separated by commas and may include wildcards.
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
*     MARKSTYLE = LITERAL (Read and Write)
*        A string indicating how markers are initially to be plotted in
*        the aligner widget.  It consists of a comma-separated list of
*        "attribute=value" type strings.  The available attributes are:
*           - colour     -- Colour of the marker in Xwindows format.
*           - size       -- Approximate height of the marker in pixels.
*           - thickness  -- Approximate thickness of lines in pixels.
*           - shape      -- One of Plus, Cross, Circle, Square, Diamond.
*           - showindex  -- 1 to show index numbers, 0 not to do so.
*
*        This parameter only gives the initial marker type; it can be
*        changed interactively while the program is running.
*        If specifying this value on the command line, it is not
*        necessary to give values for all the attributes; missing ones
*        will be given sensible defaults.
*        [""]
*     MAXCANV = INTEGER (Read and Write)
*        A value in pixels for the maximum initial X or Y dimension of 
*        the region in which the image is displayed.  Note this is the      
*        scrolled region, and may be much bigger than the sizes given
*        by WINX and WINY, which limit the size of the window on the
*        X display.  It can be overridden during operation by zooming
*        in and out using the GUI controls, but it is intended to
*        limit the size for the case when ZOOM is large (perhaps
*        because the last image was quite small) and a large image
*        is going to be displayed, which otherwise might lead to
*        the program attempting to display an enormous viewing region.
*        If set to zero, then no limit is in effect.
*        [1280]
*     OVERRIDE = LOGICAL (Read)
*        This parameter controls whether to continue and create an
*        incomplete solution. Such solutions will result when only a
*        subset of the input position lists have been paired.
*
*        In this case, any NDFs for which matching was not 
*        achieved will have their associated position lists removed
*        from their .MORE.CCDPACK extensions.  Thus after running 
*        PAIRNDF with OVERRIDE set to TRUE, any position list associated
*        with an NDF is guaranteed to be one which has been matched, and 
*        not just one left over from the previously associated unmatched 
*        list.
*        [TRUE]
*     OUTLIST = LITERAL (Read)
*        An expression which is either a list of names or expands to a
*        list of names for the output position lists.
*
*        These may be specified as list of comma separated names, 
*        using indirection if required, OR, as a single modification 
*        element (of the input names). The simplest modification
*        element is the asterisk "*" which means call each of the
*        output lists the same name as the corresponding input NDFs (but
*        without the ".sdf" extension).
*        So,
*           IN > *
*           OUTLIST > *
*        signifies that all the NDFs in the current directory should be
*        used and the output lists should have the same names. 
*
*        Other types of modification can also occur, such as,
*           OUTLIST > *_objs.dat
*        which means call the position lists the same as the input NDFs
*        but put "_objs.dat" after the names. Replacement of a specified
*        string with another in the output file names can also be used,
*           outlist > *|_debias|_images.dat|
*        this replaces the string "_debias" with "_images.dat" in any
*        of the output names.
*
*        If wildcarded names for the input NDFs are used then it is
*        recommended that wildcards are also used for the position list
*        names as the correspondence between these may be confusing.
*        [*.DAT]
*     PERCENTILES( 2 ) = _DOUBLE (Read)
*        The default low and high percentiles of the data range to use
*        when displaying the images; any pixels with a value lower than 
*        the first element will have the same colour, and any with a
*        value higher than the second will have the same colour.  This
*        parameter gives the default value - the percentile settings
*        can be set for each image individually from within the GUI
*        to accomodate the situation where images have different 
*        brightnesses.  Must be in the range 0 <= PERCENTILES( 1 ) 
*        <= PERCENTILES( 2 ) <= 100.
*        [2,98]
*     PREVX = INTEGER (Read and Write)
*        The initial width in pixels of the preview display for each image;
*        two images will be displayed side by side at any one time at
*        this size in the chooser window.  This can be effectively changed
*        by resizing the entire chooser window in the normal way using
*        the window manager while the program is running.
*        [350]
*     PREVY = INTEGER (Read and Write)
*        The initial height in pixels of the preview display for each image;
*        two images will be displayed side by side at any one time at
*        this size in the chooser window.  This can be effectively changed
*        by resizing the entire chooser window in the normal way using
*        the window manager while the program is running.
*        [350]
*     WINX = INTEGER (Read and Write)
*        The initial width in pixels of the aligner window, which contains
*        a space for dragging around a pair of images and associated
*        controls.  If the region required for the images is larger 
*        than the area allocated for display, it can be scrolled 
*        around within the window.  The window can be resized in the 
*        normal way using the window manager while the program is running.
*        [800]
*     WINY = INTEGER (Read and Write)
*        The initial height in pixels of the aligner window, which contains
*        space for dragging around a pair of images and associated
*        controls.  If the region required for the images is larger 
*        than the area allocated for display, it can be scrolled 
*        around within the window.  The window can be resized in the 
*        normal way using the window manager while the program is running.
*        [400]
*     ZOOM = DOUBLE (Read and Write)
*        A factor giving the initial level to zoom in to the images
*        displayed in the aligner window, that is the number of screen 
*        pixels to use for one image pixel.  It will be rounded to one 
*        of the values ... 3, 2, 1, 1/2, 1/3 ....  The zoom can be 
*        changed interactively from within the program.  The initial 
*        value may be limited by MAXCANV.
*        [1]

*  Examples:
*     pairndf * *.dat [1,99]
*        This example shows the positional nature of the parameters.
*        All the NDFs in the current directory are presented for 
*        alignment.  Their output position lists have the same name 
*        as the NDFs except that they have a file extension of .dat.
*        The default image display cutoff is between the 1st and 99th
*        percentile, which shows bright detail well.
*
*     pairndf in="data1,data2" outlist="d1-pos,d2-pos" zoom=2 maxcanv=0
*             markstyle="shape=circle,size=8,thickness=1,colour=HotPink"
*        Only the two images data1 and data2 will be aligned, and the 
*        corresponding sets of positions will be written to the
*        files d1-pos and d2-pos.  The images will initially be 
*        displayed for alignment at a magnification of two screen 
*        pixels to each data pixel, even if that results in a very 
*        large display area.  During alignment, marked points will be
*        shown as little pink circles.

*  Notes:
*     - NDF extension items. 
*
*       On exit the CURRENT_LIST items in the CCDPACK extensions
*       (.MORE.CCDPACK) of the input NDFs are set to the names of the 
*       appropriate output lists. These items will be used by other
*       CCDPACK position list processing routines to automatically 
*       access the lists.
*
*     - Output position list format.
*
*       CCDPACK format - Position lists in CCDPACK are formatted files
*       whose first three columns are interpreted as the following.
*
*          - Column 1: an integer identifier
*          - Column 2: the X position
*          - Column 3: the Y position
*
*       The column one value must be an integer and is used to identify
*       positions which may have different locations but are to be
*       considered as the same point. Comments may be included in the
*       file using the characters # and !. Columns may be separated by
*       the use of commas or spaces.
*
*       In all cases, the coordinates in position lists are pixel
*       coordinates.

*  Implementation Status:
*     - Supports Bad pixel values and all non-complex data types.

*  Behaviour of parameters:
*     All parameters retain their current value as default. The
*     "current" value is the value assigned on the last run of the
*     application. If the application has not been run then the
*     "intrinsic" defaults, as shown in the parameter help, apply.
*
*     Retaining parameter values has the advantage of allowing you to
*     define the default behaviour of the application.  The intrinsic
*     default behaviour of the application may be restored by using the
*     RESET keyword on the command line.
*
*     Certain parameters (LOGTO and LOGFILE) have global values. These
*     global values will always take precedence, except when an
*     assignment is made on the command line.  Global values may be set
*     and reset using the CCDSETUP and CCDCLEAR commands.
*
*     Some of the parameters (MARKSTYLE, MAXCANV, PERCENTILES, PREVX, 
*     PREVY, WINX, WINY)  give initial values for quantities which can 
*     be modified while the program is running.  Although these may be 
*     specified on the command line, it is normally easier to start 
*     the program up and modify them using the graphical user interface.
*     If the program exits normally, their values at the end of the run 
*     will be used as defaults next time the program starts up.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-FEB-1993 (PDRAPER):
*        Original version.
*     6-OCT-1995 (PDRAPER):
*        Updated for CCDPACK version 2.0.
*     3-MAR-1997 (PDRAPER):
*        Removed top-level locator control (foreign data access upgrade).
*     22-MAY-1997 (PDRAPER):
*        Now has better control of reserved pens.
*     22-FEB-1999 (PDRAPER):
*        Increased centroid control parameters to scale with 
*        image size. Large images generally have their initial centroids
*        badly positioned.
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     14-SEP-2000 (MBT):
*        Rewrote to use Tcl for the GUI instead of IDI.
*     16-JAN-2001 (MBT):
*        Added OVERRIDE parameter.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'CCD1_PAR'        ! CCDPACK parameterisations
      INCLUDE 'MSG_PAR'         ! Message system parameterisations
      INCLUDE 'AST_PAR'         ! Standard AST system declarations
      INCLUDE 'DAT_PAR'         ! Standard HDS constants
      INCLUDE 'GRP_PAR'         ! Standard GRP constants

*  Status:
      INTEGER STATUS            ! Global status

*  Local Variables:
      CHARACTER * ( 80 ) FNAME  ! Filename
      CHARACTER * ( 132 ) MSTYLE ! Marker style string
      CHARACTER * ( CCD1__BLEN ) LINE ! Buffer for writing output lines
      CHARACTER * ( DAT__SZLOC ) LOCEXT ! HDS locator for .MORE.CCDPACK ext
      CHARACTER * ( GRP__SZNAM ) NDFNAM ! Name of NDF
      DOUBLE PRECISION PERCNT( 2 ) ! Percentile values for display
      DOUBLE PRECISION XOFF( CCD1__MXNDF * CCD1__MXNDF ) ! Initial X offsets
      DOUBLE PRECISION XOFFN( CCD1__MXNDF * CCD1__MXNDF ) ! Absolute X offsets
      DOUBLE PRECISION YOFF( CCD1__MXNDF * CCD1__MXNDF ) ! Initial Y offsets
      DOUBLE PRECISION YOFFN( CCD1__MXNDF * CCD1__MXNDF ) ! Absolute Y offsets
      DOUBLE PRECISION ZOOM     ! Zoom factor for display
      INTEGER COUNT             ! Current intercomparison level
      INTEGER FDO( CCD1__MXNDF ) ! Output FIO descriptors
      INTEGER FRM( CCD1__MXNDF ) ! AST pointers to current frames
      INTEGER I                 ! Loop variable
      INTEGER IMAP              ! AST pointer to mapping
      INTEGER INDF( CCD1__MXNDF ) ! NDF identifiers
      INTEGER IPBEEN            ! Pointer to workspace
      INTEGER IPGRA             ! Pointer to graph
      INTEGER IPID( CCD1__MXNDF ) ! Pointer to output identifiers
      INTEGER IPQUE             ! Pointer to workspace
      INTEGER IPSUB             ! Pointer to sub-graph (spanning)
      INTEGER IPX1( CCD1__MXNDF * CCD1__MXNDF ) ! Pointers to X position lists
      INTEGER IPX2( CCD1__MXNDF * CCD1__MXNDF ) ! Pointers to X position lists
      INTEGER IPXO( CCD1__MXNDF ) ! Pointers to output X positions
      INTEGER IPXP              ! Pointer to X pixel coordinate list
      INTEGER IPYP              ! Pointer to Y pixel coordinate list
      INTEGER IPY1( CCD1__MXNDF * CCD1__MXNDF ) ! Pointers to Y position lists
      INTEGER IPY2( CCD1__MXNDF * CCD1__MXNDF ) ! Pointers to Y position lists
      INTEGER IPYO( CCD1__MXNDF ) ! Pointers to output Y positions
      INTEGER IWCS( CCD1__MXNDF ) ! AST pointers to WCS framesets
      INTEGER JPIX              ! Frame index of pixel frame
      INTEGER MAXCNV            ! Initial maximum dimension of display region
      INTEGER NDFGR             ! Input NDF group identifier
      INTEGER NEDGES            ! Number of edges in graph
      INTEGER NEWED             ! Number of edges in spanning graph
      INTEGER NMAT( CCD1__MXNDF * CCD1__MXNDF ) ! Number of position selected
      INTEGER NNDF              ! Number of input NDFs
      INTEGER NODES( 2, CCD1__MXNDF * CCD1__MXNDF ) ! Original node numbers
      INTEGER NOUT( CCD1__MXNDF ) ! Numbers of output positions
      INTEGER NRET              ! Dummy variable
      INTEGER OFFS( CCD1__MXNDF+ 1 ) ! Offsets into extended lists
      INTEGER OUTGRP            ! Output group identifier
      INTEGER PRVDIM( 2 )       ! Preview image dimensions for display
      INTEGER TOTNOD            ! Total number of nodes in graph
      INTEGER WINDIM( 2 )       ! Window dimensions for display
      LOGICAL COMPL             ! True if graph is complete
      LOGICAL CYCLIC            ! True if graph is cyclic
      LOGICAL OVERRD            ! True if continue with incomplete graph
                            
*.                             
                               
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start up CCDPACK.
      CALL CCD1_START( 'PAIRNDF', STATUS )

*  Start an NDF context.
      CALL NDF_BEGIN

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the values of display preferences from the parameter system.
      CALL PAR_GET0D( 'ZOOM', ZOOM, STATUS )
      CALL PAR_GET0I( 'MAXCANV', MAXCNV, STATUS )
      CALL PAR_GET0I( 'WINX', WINDIM( 1 ), STATUS )
      CALL PAR_GET0I( 'WINY', WINDIM( 2 ), STATUS )
      CALL PAR_GET0I( 'PREVX', PRVDIM( 1 ), STATUS )
      CALL PAR_GET0I( 'PREVY', PRVDIM( 2 ), STATUS )
      CALL PAR_GET0C( 'MARKSTYLE', MSTYLE, STATUS )

*  Get a list of NDF names.
      CALL CCD1_NDFGL( 'IN', 2, CCD1__MXNDF, NDFGR, NNDF, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Get the percentage histogram range for image display.
      CALL PAR_EXACD( 'PERCENTILES', 2, PERCNT, STATUS )

*  Get NDF identifiers.
      DO I = 1, NNDF
         CALL NDG_NDFAS( NDFGR, I, 'UPDATE', INDF( I ), STATUS )
      END DO

*  See if we should continue with registration if only a few of the
*  datasets have been paired.
      CALL PAR_GET0L( 'OVERRIDE', OVERRD, STATUS )

*  Write the Name of the positions lists and labels into the log.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ', '    Input NDFs:', STATUS )
      CALL CCD1_MSG( ' ', '    -----------', STATUS )
      DO I = 1, NNDF
         CALL NDF_MSG( 'NAME', INDF( I ) )
         CALL MSG_SETI( 'N', I )
         CALL CCD1_MSG( ' ', '  ^N) ^NAME', STATUS )
      END DO

*  Call the routine which does all the user interaction and obtains a
*  list of pairings with associated offsets.
      CALL CCD1_PNDF( NDFGR, PERCNT, COUNT, ZOOM, MAXCNV, WINDIM, 
     :                PRVDIM, MSTYLE, NODES, NMAT, XOFF, YOFF, 
     :                IPX1, IPY1, IPX2, IPY2, STATUS )

*=======================================================================
*  Spanning graph determination section
*=======================================================================
 97   CONTINUE
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ','    CONNECTIONS and OFFSETS', STATUS )
      CALL CCD1_MSG( ' ','    -----------------------', STATUS )
      CALL CCD1_MSG( ' ', ' ', STATUS )

*  Get workspace for the graph testing sections.
      CALL CCD1_MALL( NNDF * NNDF * 4, '_INTEGER', IPGRA, STATUS )
      CALL CCD1_MALL( NNDF * NNDF * 4, '_INTEGER', IPSUB, STATUS )
      CALL CCD1_MALL( NNDF * NNDF, '_INTEGER', IPQUE, STATUS )
      CALL CCD1_MALL( NNDF, '_LOGICAL', IPBEEN, STATUS )

*  Use a graph of all the connections given by the user. The first stage
*  is to change the format into one which is a recognisable graph with
*  edges of weight the number of common positions.
      CALL CCD1_CRGR2( NMAT, COUNT, NODES, %VAL( IPGRA ), NEDGES,
     :                 STATUS )

*  Write out what the graph actually is.
      CALL CCD1_WRGRA( %VAL( IPGRA ), NEDGES, STATUS )

*  Call routine to determine if the graph is complete.
      CALL CCD1_GRAPC( %VAL( IPGRA ), NEDGES, 1, %VAL( IPQUE ),
     :                 %VAL( IPBEEN ), COMPL, CYCLIC, %VAL( IPSUB ),
     :                 NEWED, TOTNOD, STATUS )

*  Decide whether the calculated subgraph is sufficiently complete to
*  continue, and bail out if not.
      IF ( STATUS .NE. SAI__OK ) GO TO 99 
      IF ( COMPL ) THEN
         IF ( TOTNOD .EQ. 0 ) THEN
            CALL CCD1_ERREP( 'PAIRNDF_NONE',
     :'  No pairings were made.', STATUS )
         ELSE IF ( TOTNOD .LT. NNDF ) THEN
            IF ( OVERRD ) THEN
               CALL CCD1_MSG( ' ', ' ', STATUS )
               CALL CCD1_MSG( ' ',
     :'  Warning - not all pairings were successfully made.'//
     :'  Continuing with those that have.', STATUS) 
               CALL CCD1_MSG( ' ', ' ', STATUS )
            ELSE
               STATUS = SAI__ERROR
               CALL CCD1_ERREP( 'PAIRNDF_NOTALL',
     :'  Not all datasets were successfully paired.', STATUS )
            END IF
         END IF
      ELSE
         STATUS = SAI__ERROR
         CALL CCD1_ERREP( 'PAIRNDF_NOTCOM',
     :'  Intercomparison of positions does not produce a complete'//
     :' registration of all frames -- graph incomplete', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99 
                           
*  The subgraph is complete -- all nodes connected.  Determine the 
*  `complete' solution.  Find the offsets of all positions to the 
*  `reference' set (first node of first edge of spanning graph is 
*  assumed to be the reference set).
      CALL CCD1_GROFF( %VAL( IPSUB ), NEWED, XOFF, YOFF,
     :                 NNDF, %VAL( IPBEEN ), %VAL( IPQUE ),
     :                 XOFFN, YOFFN, STATUS )

*  Get the WCS frameset and the current frame for each NDF.
      DO I = 1, NNDF
         CALL CCD1_GTWCS( INDF( I ), IWCS( I ), STATUS )
         FRM( I ) = AST_GETFRAME( IWCS( I ), AST__CURRENT, STATUS )
      END DO

*  Output offset information to the user.
      CALL CCD1_PROFF( NNDF, %VAL( IPBEEN ), XOFFN, YOFFN, FRM,
     :                 .TRUE., STATUS )

*  Initialise number of matched points.
      DO I = 1, NNDF
         NOUT( I ) = 0
      END DO

*  Generate the ID's for the output lists. Matching positions between
*  the lists and final merging all positions for each node.
      CALL CCD1_GMMP( %VAL( IPSUB ), NEWED, TOTNOD, IPX1, IPY1, IPX2,
     :                IPY2, NMAT, OFFS, IPXO, IPYO, IPID, NOUT,
     :                STATUS )


*=======================================================================
*   Writing output lists and updating NDF extensions
*=======================================================================
*  Get the output position lists file names. Use the input names
*  as possible modification elements.
*  Get the names of the corresponding lists of positions.
      CALL CCD1_STRGR( 'OUTLIST', NDFGR, NNDF, NNDF, OUTGRP, NRET,
     :                 STATUS )

*  Write a position list and store its name in the .MORE.CCDPACK
*  extension for each NDF.
      DO 12 I = 1, NNDF

*  Check whether there are any points to write in the list.
         IF ( NOUT( I ) .GT. 0 ) THEN 

*  Get temporary storage for list of points in pixel coordinates.
            CALL CCD1_MALL( NOUT( I ), '_DOUBLE', IPXP, STATUS )
            CALL CCD1_MALL( NOUT( I ), '_DOUBLE', IPYP, STATUS )

*  Convert the positions to Pixel coordinates.
            CALL CCD1_FRDM( IWCS( I ), 'Pixel', JPIX, STATUS )
            IMAP = AST_GETMAPPING( IWCS( I ), AST__CURRENT, JPIX,
     :                             STATUS )
            CALL AST_TRAN2( IMAP, NOUT( I ), %VAL( IPXO( I ) ), 
     :                      %VAL( IPYO( I ) ), 1, %VAL( IPXP ),
     :                      %VAL( IPYP ), STATUS )

*  Open the position list file.
            CALL GRP_GET( OUTGRP, I, 1, FNAME, STATUS )
            CALL CCD1_OPFIO( FNAME, 'WRITE', 'LIST', 0, FDO( I ),
     :                     STATUS )
            CALL CCD1_FIOHD( FDO( I ), 'Output from PAIRNDF', STATUS )

*  Write the data.
            CALL CCD1_WRIXY( FDO( I ), %VAL( IPID( I ) ), %VAL( IPXP ),
     :                       %VAL( IPYP ), NOUT( I ), LINE, CCD1__BLEN,
     :                       STATUS )

*  Close the file.
            CALL FIO_CLOSE( FDO( I ), STATUS )

*  Store the names of the position lists in the NDF extensions
            CALL CCG1_STO0C( INDF( I ), 'CURRENT_LIST', FNAME, STATUS )

*  If there are no points to write, then remove any existing list name
*  from the NDF extension, otherwise casual use of them by later 
*  applications may give the impression that the original position
*  lists are matched ones.
         ELSE
            IF ( STATUS .NE. SAI__OK ) GO TO 99
            CALL ERR_MARK
            CALL CCD1_CEXT( INDF( I ), .FALSE., 'UPDATE', LOCEXT,
     :                      STATUS )
            CALL DAT_ERASE( LOCEXT, 'CURRENT_LIST', STATUS )

*  If we succeeded, tell the user that the old list has been removed.
*  If we failed, it is presumably because there was no old list, which
*  is fine.
            IF ( STATUS .EQ. SAI__OK ) THEN
               CALL GRP_GET( NDFGR, I, 1, NDFNAM, STATUS )
               CALL MSG_SETC( 'NDF', NDFNAM )
               CALL CCD1_MSG( ' ', 
     :'  Removing associated list for unpaired NDF ^NDF', STATUS )
            ELSE
               CALL ERR_ANNUL( STATUS )
            END IF
            CALL ERR_RLSE
         END IF
 12   CONTINUE

*  Write display preference parameters back to the parameter system.
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      CALL PAR_PUT0D( 'ZOOM', ZOOM, STATUS )
      CALL PAR_PUT0I( 'MAXCANV', MAXCNV, STATUS )
      CALL PAR_PUT0I( 'WINX', WINDIM( 1 ), STATUS )
      CALL PAR_PUT0I( 'WINY', WINDIM( 2 ), STATUS )
      CALL PAR_PUT0I( 'PREVX', PRVDIM( 1 ), STATUS )
      CALL PAR_PUT0I( 'PREVY', PRVDIM( 2 ), STATUS )
      CALL PAR_PUT0C( 'MARKSTYLE', MSTYLE, STATUS )

*  Exit label.
 99   CONTINUE

*  Release all memory
      CALL CCD1_MFREE( -1, STATUS )
      CALL CCD1_FRTMP( -1, STATUS )

*  Release group resources.
      CALL CCD1_GRDEL( NDFGR, STATUS )
      CALL CCD1_GRDEL( OUTGRP, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL CCD1_ERREP( 'PAIRNDF_ERR',
     :   'PAIRNDF: Error performing NDF pairing.',
     :   STATUS )
      END IF

*  Finally close CCDPACK.
      CALL CCD1_END( STATUS )

      END
* $Id$
