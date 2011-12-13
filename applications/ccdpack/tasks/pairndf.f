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
*     can indicate how pairs of images are aligned with respect
*     to each other, and mark image features to allow accurate
*     alignments.  Once enough pairings have been specified to register
*     all frames completely a global merger of all the positions
*     for each image takes place. This results in the output of one list
*     of uniquely labelled positions for each NDF. These position lists
*     can then be used in a routine such as REGISTER to produce the
*     actual transformation between the NDFs.
*
*     If NDFs have been grouped into Sets for alignment purposes by
*     using MAKESET, and the USESET parameter is true, then the
*     program will treat each Set of NDFs as a single image to be
*     aligned.
*
*     The graphical interface consists of two parts: a chooser which
*     allows you to nominate pairs of images to be aligned,
*     and an aligner which allows you to move the pair around the
*     screen until they are registered, and to mark points in the
*     overlapping region where the same centroidable features exist
*     on both images.
*
*     Operation is as follows.  You must first use the chooser window
*     to select a pair of images which have a region in common
*     (if you only have two images this step may be skipped).
*     Use the tabs at either side of the screen to pick the image to
*     appear on that side.  You can use the "Show FITS" button to
*     select one or more FITS headers to be displayed alongside
*     each image if this will make it easier to identify which is which.
*     You can use the "Display cutoff" menu to select the percentiles
*     controlling the brightness of each pixel; alignment is easier if
*     the same features are of a similar brightness in different images.
*     The images are displayed resampled into their Current coordinates,
*     so that their orientation will be the same as in the aligner.
*     You can only align them using this program if a simple offset
*     (translation) maps one onto another in these
*     coordinates (or very nearly does so).  If that is not the case,
*     you will have to set their Current coordinate system
*     to a different value (see WCSEDIT) or align them using a
*     different method.  The whole of each image will be displayed in
*     the chooser window; select a pair with
*     an overlapping region which you wish to align, and click the
*     "Use this pair" button.  The aligner window will then appear,
*     displaying the two images which you have selected.
*     The chooser window can normally be resized in the normal way to
*     make the images bigger or smaller.  However there is currently
*     a bug which causes this to crash in some window managers which
*     use continuous resizing.  In this case you must use the PREVX
*     and PREVY parameters to change the image size.
*
*     In the aligner window you can drag either of these images around
*     the display region by holding down mouse button 1 (usually the
*     left one) as you move the mouse; the easiest way to align the pair
*     is to "pick up" one image by an identifiable feature and "drop" it
*     on the same feature in the other image.  Where the images overlap
*     their pixels will be averaged.  If they are not correctly
*     positioned, you can move them again.  Once you are happy that
*     they are aligned about right, then click in the overlap region
*     to mark features which appear in both images.  During this
*     part you mark points by clicking with mouse button 1 (usually
*     the left one) and you can remove them by clicking with button 3
*     (usually the right one).
*
*     When you add a point by clicking it will be centroided on both
*     images, and two markers plotted, one for each centroided position.
*     If a centroidable object near that point cannot be identified
*     on both images the program will not allow you to mark a point
*     there.  However, note that the centroiding algorithm is
*     capable of locating spurious objects from noise, so the fact
*     that a point can be marked does not prove that a real
*     feature exists on both images.  By looking at the two markers
*     it should be possible to see whether a real feature has been
*     located.  Though the two markers do not need to be exactly
*     concentric (REGISTER can take care of that later), the offset
*     between them should be similar to that of other marked objects
*     nearby in the overlap region.  If you do not think the same
*     object has been identified in both images, you should remove
*     this point (with mouse button 3).
*
*     The aligner window can be resized, the magnification changed
*     using the "Zoom" control, the display region scrolled using
*     the scrollbars, and the shape and colour of the point markers
*     selected.  When you have aligned the images and marked shared
*     features, or if you decide that the pair cannot be satisfactorily
*     registered, click the "Done" button.
*
*     You will then be returned to the chooser window to select another
*     pair and repeat the process.  After the first time however,
*     you will only be allowed to select a pair of images to align
*     if at least one of them has already been aligned.  Those
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
*     CHOOSER = _LOGICAL (Read)
*        If only two images are presented in the IN list, then this
*        parameter determines whether they should be previewed in
*        the chooser widget before they are presented for alignment.
*        With only two, the chooser is not normally necessary since
*        there is only one possible pair to select for alignment,
*        but if you want to equalise the image brightnesses using
*        the "Display cutoff" button or preview FITS headers you
*        may wish to respond true to this.
*        [FALSE]
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
*     MARKSTYLE1 = LITERAL (Read and Write)
*        A string indicating how markers are initially to be plotted in
*        the aligner widget to represent points on the left hand image.
*        It consists of a comma-separated list of "attribute=value"
*        type strings.  The available attributes are:
*           - colour     -- Colour of the marker in Xwindows format.
*           - size       -- Approximate height of the marker in pixels.
*           - thickness  -- Approximate thickness of lines in pixels.
*           - shape      -- One of Plus, Cross, Circle, Square, Diamond.
*
*        This parameter only gives the initial marker type; it can be
*        changed interactively while the program is running.
*        If specifying this value on the command line, it is not
*        necessary to give values for all the attributes; missing ones
*        will be given sensible defaults.
*        ["shape=plus"]
*     MARKSTYLE2 = LITERAL (Read and Write)
*        A string indicating how markers are initially to be plotted in
*        the aligner widget to represent points on the right hand image.
*        It consists of a comma-separated list of "attribute=value"
*        type strings.  The available attributes are:
*           - colour     -- Colour of the marker in Xwindows format.
*           - size       -- Approximate height of the marker in pixels.
*           - thickness  -- Approximate thickness of lines in pixels.
*           - shape      -- One of Plus, Cross, Circle, Square, Diamond.
*
*        This parameter only gives the initial marker type; it can be
*        changed interactively while the program is running.
*        If specifying this value on the command line, it is not
*        necessary to give values for all the attributes; missing ones
*        will be given sensible defaults.
*        ["shape=circle"]
*     MAXCANV = _INTEGER (Read and Write)
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
*     OVERRIDE = _LOGICAL (Read)
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
*           OUTLIST > *|_debias|_images.dat|
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
*     PREVX = _INTEGER (Read and Write)
*        The initial width in pixels of the preview display for each image;
*        two images will be displayed side by side at any one time at
*        this size in the chooser window.  This can be effectively changed
*        by resizing the entire chooser window in the normal way using
*        the window manager while the program is running.
*        [350]
*     PREVY = _INTEGER (Read and Write)
*        The initial height in pixels of the preview display for each image;
*        two images will be displayed side by side at any one time at
*        this size in the chooser window.  This can be effectively changed
*        by resizing the entire chooser window in the normal way using
*        the window manager while the program is running.
*        [350]
*     TOLER = _DOUBLE (Read)
*        The tolerance for deduplicating centroided points (in pixels).
*        If two centroided objects on the same image are within this
*        distance of each other they will be identified as the same
*        object.  For a bright elliptical object, centroiding arising
*        from any nearby point will normally arrive at the same
*        position, so this can be set to a small value (<1), but
*        if the objects being identified cover many pixels and are close
*        to the background noise level it may be advantageous to set
*        it to a larger value so that centroids near to each other
*        are identified as referring to the same object.
*        [0.5]
*     USESET = _LOGICAL (Read)
*        This parameter determines whether Set header information should
*        be used or not.  If USESET is true,
*        PAIRNDF will try to group images according to their Set Name
*        attribute.  All NDFs which share the same (non-blank) Set
*        Name attribute, and which have a CCD_SET attached coordinate
*        system, will be grouped together and treated as a single
*        image for alignment.  In the graphical part of the program you
*        will view and position this group of images as a single item.
*
*        If the input NDFs have no Set headers, or if they have no
*        Set alignment coordinate system (one with a Domain of CCD_SET)
*        the setting of USESET will make no difference.
*
*        If a global value for this parameter has been set using
*        CCDSETUP then that value will be used.
*        [FALSE]
*     WINX = _INTEGER (Read and Write)
*        The initial width in pixels of the aligner window, which contains
*        a space for dragging around a pair of images and associated
*        controls.  If the region required for the images is larger
*        than the area allocated for display, it can be scrolled
*        around within the window.  The window can be resized in the
*        normal way using the window manager while the program is running.
*        [800]
*     WINY = _INTEGER (Read and Write)
*        The initial height in pixels of the aligner window, which contains
*        space for dragging around a pair of images and associated
*        controls.  If the region required for the images is larger
*        than the area allocated for display, it can be scrolled
*        around within the window.  The window can be resized in the
*        normal way using the window manager while the program is running.
*        [400]
*     ZOOM = _DOUBLE (Read and Write)
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
*             markstyle1="shape=circle,size=8,thickness=1,colour=HotPink"
*        Only the two images data1 and data2 will be aligned, and the
*        corresponding sets of positions will be written to the
*        files d1-pos and d2-pos.  The images will initially be
*        displayed for alignment at a magnification of two screen
*        pixels to each data pixel, even if that results in a very
*        large display area.  During alignment, marked points on the
*        left hand image will be shown as little pink circles.

*  Implementation Status:
*     - Supports Bad pixel values and all non-complex data types.

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

*  Behaviour of Parameters:
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
*     Certain parameters (LOGTO, LOGFILE and USESET) have global values.
*     These global values will always take precedence, except when an
*     assignment is made on the command line.  Global values may be set
*     and reset using the CCDSETUP and CCDCLEAR commands.
*
*     Some of the parameters (MARKSTYLE1, MARKSTYLE2, MAXCANV,
*     PERCENTILES, PREVX, PREVY, WINX, WINY)  give initial values for
*     quantities which can be modified while the program is running.
*     Although these may be specified on the command line, it is
*     normally easier to start the program up and modify them using
*     the graphical user interface.  If the program exits normally,
*     their values at the end of the run will be used as defaults
*     next time the program starts up.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 1995, 1997, 1999-2002 Central Laboratory of the
*     Research Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

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
*     29-JAN-2001 (MBT):
*        Changed call parameters for modified CCD1_GMMP.
*     7-MAR-2001 (MBT):
*        Upgraded for use with Sets.
*     22-MAY-2001 (MBT):
*        Changed so that empty position lists are written rather than no
*        lists at all.
*     11-JUL-2001 (MBT):
*        Added new TOLER parameter.
*     18-JUL-2001 (MBT):
*        Changed the way that centroiding is done.
*     11-FEB-2002 (MBT):
*        Modified call to CCD1_GMMP to fix an uninitialised array bug.
*     {enter_changes_here}

*  Bugs:
*     A few window managers which allow continuous active resizing of
*     windows may cause the program to crash when the chooser window is
*     being resized.  In this case, the PREVX and PREVY parameters
*     must be used to control the size of previewed images.
*     {note_new_bugs_here}

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
      INCLUDE 'CNF_PAR'         ! For CNF_PVAL function

*  Status:
      INTEGER STATUS            ! Global status

*  Local Variables:
      CHARACTER * ( 80 ) FNAME  ! Filename
      CHARACTER * ( 132 ) MSTYA ! Marker style string A
      CHARACTER * ( 132 ) MSTYB ! Marker style string B
      CHARACTER * ( CCD1__BLEN ) LINE ! Buffer for writing output lines
      DOUBLE PRECISION PERCNT( 2 ) ! Percentile values for display
      DOUBLE PRECISION PSIZE    ! Pixel size in current coords
      DOUBLE PRECISION TOLER    ! Deduplicating tolerance in units of pixels
      DOUBLE PRECISION TOLS( CCD1__MXNDF ) ! Deduplicating tolerance by Set
      DOUBLE PRECISION XLO      ! Lower acceptable bound for X coordinate
      DOUBLE PRECISION XHI      ! Upper acceptable bound for X coordinate
      DOUBLE PRECISION XOFF( CCD1__MXNDF * CCD1__MXNDF ) ! Initial X offsets
      DOUBLE PRECISION XOFFN( CCD1__MXNDF * CCD1__MXNDF ) ! Absolute X offsets
      DOUBLE PRECISION YLO      ! Lower acceptable bound for Y coordinate
      DOUBLE PRECISION YHI      ! Upper acceptable bound for Y coordinate
      DOUBLE PRECISION YOFF( CCD1__MXNDF * CCD1__MXNDF ) ! Initial Y offsets
      DOUBLE PRECISION YOFFN( CCD1__MXNDF * CCD1__MXNDF ) ! Absolute Y offsets
      DOUBLE PRECISION ZOOM     ! Zoom factor for display
      INTEGER COUNT             ! Current intercomparison level
      INTEGER FDO               ! Output FIO descriptor
      INTEGER FRM( CCD1__MXNDF ) ! AST pointers to current frames
      INTEGER I                 ! Loop variable
      INTEGER IMAP              ! AST pointer to mapping
      INTEGER IMEM( CCD1__MXNDF ) ! Array of member indices in Set order
      INTEGER IMEMOF( CCD1__MXNDF + 1 ) ! Offsets into IMEM for each Set
      INTEGER INDF( CCD1__MXNDF ) ! NDF identifiers for each NDF
      INTEGER IPBEEN            ! Pointer to workspace
      INTEGER IPGRA             ! Pointer to graph
      INTEGER IPID( CCD1__MXNDF ) ! Pointer to output identifiers
      INTEGER IPIQ              ! Pointer to index values of chosen points
      INTEGER IPQUE             ! Pointer to workspace
      INTEGER IPRAN( CCD1__MXNDF ) ! Pointer to workspace
      INTEGER IPRAN1( CCD1__MXNDF * CCD1__MXNDF ) ! Pointer to workspace
      INTEGER IPRAN2( CCD1__MXNDF * CCD1__MXNDF ) ! Pointer to workspace
      INTEGER IPSUB             ! Pointer to sub-graph (spanning)
      INTEGER IPX1( CCD1__MXNDF * CCD1__MXNDF ) ! Pointers to X position lists
      INTEGER IPX2( CCD1__MXNDF * CCD1__MXNDF ) ! Pointers to X position lists
      INTEGER IPXO( CCD1__MXNDF ) ! Pointers to output X positions
      INTEGER IPXP              ! Pointer to X pixel coordinate list
      INTEGER IPXQ              ! Pointer to X pixels of chosen points
      INTEGER IPYP              ! Pointer to Y pixel coordinate list
      INTEGER IPYQ              ! Pointer to Y pixels of chosen points
      INTEGER IPY1( CCD1__MXNDF * CCD1__MXNDF ) ! Pointers to Y position lists
      INTEGER IPY2( CCD1__MXNDF * CCD1__MXNDF ) ! Pointers to Y position lists
      INTEGER IPYO( CCD1__MXNDF ) ! Pointers to output Y positions
      INTEGER IS                ! Set under consideration
      INTEGER ISET( CCD1__MXNDF ) ! Which Set each NDF is part of
      INTEGER IWCS( CCD1__MXNDF ) ! AST pointers to WCS frameset for each NDF
      INTEGER JPIX              ! Frame index of pixel frame
      INTEGER LBND( 2 )         ! Lower bounds of NDF
      INTEGER MAPSET( CCD1__MXNDF ) ! Workspace
      INTEGER MAXCNV            ! Initial maximum dimension of display region
      INTEGER NCOUT             ! Number of chosen points
      INTEGER NDFGR             ! Input NDF group identifier
      INTEGER NDIM              ! Number of dimensions of NDF
      INTEGER NEDGES            ! Number of edges in graph
      INTEGER NEWED             ! Number of edges in spanning graph
      INTEGER NMAT( CCD1__MXNDF * CCD1__MXNDF ) ! Number of position selected
      INTEGER NNDF              ! Number of input NDFs
      INTEGER NODES( 2, CCD1__MXNDF * CCD1__MXNDF ) ! Original node numbers
      INTEGER NOUT( CCD1__MXNDF ) ! Numbers of output positions
      INTEGER NRET              ! Dummy variable
      INTEGER NSET              ! Number of Sets
      INTEGER OFFS( CCD1__MXNDF+ 1 ) ! Offsets into extended lists
      INTEGER OUTGRP            ! Output group identifier
      INTEGER PRVDIM( 2 )       ! Preview image dimensions for display
      INTEGER SNAMGR            ! GRP identifier for Set Names
      INTEGER TOTNOD            ! Total number of nodes in graph
      INTEGER UBND( 2 )         ! Upper bounds of NDF
      INTEGER WINDIM( 2 )       ! Window dimensions for display
      LOGICAL CHOOSR            ! True if we definitely want a chooser widget
      LOGICAL COMPL             ! True if graph is complete
      LOGICAL CYCLIC            ! True if graph is cyclic
      LOGICAL OVERRD            ! True if continue with incomplete graph
      LOGICAL USESET            ! True if we are grouping NDFs by Set

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
      CALL PAR_GET0C( 'MARKSTYLE1', MSTYA, STATUS )
      CALL PAR_GET0C( 'MARKSTYLE2', MSTYB, STATUS )

*  Get a list of NDF names.
      CALL CCD1_NDFGL( 'IN', 2, CCD1__MXNDF, NDFGR, NNDF, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  See if we are using Sets.
      CALL PAR_GET0L( 'USESET', USESET, STATUS )

*  Get the percentage histogram range for image display.
      CALL PAR_EXACD( 'PERCENTILES', 2, PERCNT, STATUS )

*  Get the deduplication tolerance.
      CALL PAR_GET0D( 'TOLER', TOLER, STATUS )
      TOLER = MAX( 0D0, TOLER )

*  See if we should continue with registration if only a few of the
*  datasets have been paired.
      CALL PAR_GET0L( 'OVERRIDE', OVERRD, STATUS )

*  Group the NDFs into Sets.
      CALL CCD1_SETSW( NDFGR, NNDF, USESET, ISET, NSET, IMEM, IMEMOF,
     :                 SNAMGR, MAPSET, STATUS )

*  If there are only two NDFs, see whether we want to bother the user
*  with presenting a Chooser widget.
      CHOOSR = .FALSE.
      IF ( NSET .EQ. 2 ) THEN
         CALL PAR_GET0L( 'CHOOSER', CHOOSR, STATUS )
      END IF

*  Call the routine which does all the user interaction and obtains a
*  list of pairings with associated offsets.
      CALL CCD1_PNDF( NDFGR, NSET, IMEM, IMEMOF, SNAMGR, PERCNT,
     :                .NOT. CHOOSR, ZOOM, MAXCNV, WINDIM, PRVDIM,
     :                MSTYA, MSTYB, COUNT, NODES, NMAT, XOFF, YOFF,
     :                IPX1, IPY1, IPX2, IPY2, STATUS )

*  Release resources.
      CALL CCD1_GRDEL( SNAMGR, STATUS )

*=======================================================================
*  Spanning graph determination section
*=======================================================================
 97   CONTINUE
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ','    CONNECTIONS and OFFSETS', STATUS )
      CALL CCD1_MSG( ' ','    -----------------------', STATUS )
      CALL CCD1_MSG( ' ', ' ', STATUS )

*  Get workspace for the graph testing sections.
      CALL CCD1_MALL( NSET * NSET * 4, '_INTEGER', IPGRA, STATUS )
      CALL CCD1_MALL( NSET * NSET * 4, '_INTEGER', IPSUB, STATUS )
      CALL CCD1_MALL( NSET * NSET, '_INTEGER', IPQUE, STATUS )
      CALL CCD1_MALL( NSET, '_LOGICAL', IPBEEN, STATUS )

*  Use a graph of all the connections given by the user. The first stage
*  is to change the format into one which is a recognisable graph with
*  edges of weight the number of common positions.
      CALL CCD1_CRGR2( NMAT, COUNT, NODES, %VAL( CNF_PVAL( IPGRA ) ),
     :                 NEDGES,
     :                 STATUS )

*  Write out what the graph actually is.
      CALL CCD1_WRGRA( %VAL( CNF_PVAL( IPGRA ) ), NEDGES, STATUS )

*  Call routine to determine if the graph is complete.
      CALL CCD1_GRAPC( %VAL( CNF_PVAL( IPGRA ) ), NEDGES, 1,
     :                 %VAL( CNF_PVAL( IPQUE ) ),
     :                 %VAL( CNF_PVAL( IPBEEN ) ), COMPL, CYCLIC,
     :                 %VAL( CNF_PVAL( IPSUB ) ),
     :                 NEWED, TOTNOD, STATUS )

*  Decide whether the calculated subgraph is sufficiently complete to
*  continue, and bail out if not.
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      IF ( COMPL ) THEN
         IF ( TOTNOD .EQ. 0 ) THEN
            STATUS = SAI__ERROR
            CALL CCD1_ERREP( 'PAIRNDF_NONE',
     :'  No pairings were made.', STATUS )
         ELSE IF ( TOTNOD .LT. NSET ) THEN
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
      CALL CCD1_GROFF( %VAL( CNF_PVAL( IPSUB ) ),
     :                 NEWED, XOFF, YOFF, NSET,
     :                 %VAL( CNF_PVAL( IPBEEN ) ),
     :                 %VAL( CNF_PVAL( IPQUE ) ), XOFFN, YOFFN,
     :                 STATUS )

*  Initialise list of Set current Frame objects.
      DO I = 1, NSET
         FRM( I ) = AST__NULL
      END DO

*  Get NDF identifiers for each NDF and current Frame objects for each
*  Set.  The one for each Set is the Current frame of the WCS component
*  of the first NDF in the Set; for any sensible invocation of this
*  program, it will be similar to that for the other members of the
*  same Set.
      DO I = 1, NNDF
         IS = ISET( I )

*  Get the NDF identifier.
         CALL NDG_NDFAS( NDFGR, I, 'UPDATE', INDF( I ), STATUS )

*  Get the WCS component.
         CALL CCD1_GTWCS( INDF( I ), IWCS( I ), STATUS )

*  If this is the first member of this Set encountered so far, record
*  information to use for this Set (note it is arbitrary to use the
*  first one rather than others, but in all normal circumstances the
*  information should be similar for all members).
         IF ( FRM( IS ) .EQ. AST__NULL ) THEN

*  Store the current frame.
            FRM( IS ) = AST_GETFRAME( IWCS( I ), AST__CURRENT, STATUS )

*  Work out the deduplicating tolerance based on the pixel size.
            CALL CCD1_PSIZE( IWCS( I ), AST__CURRENT, PSIZE, STATUS )
            TOLS( IS ) = TOLER * PSIZE
         END IF
      END DO

*  Output offset information to the user.
      CALL CCD1_PROFF( NSET, %VAL( CNF_PVAL( IPBEEN ) ),
     :                 XOFFN, YOFFN, FRM,
     :                 .TRUE., STATUS )

*  Initialise number of matched points.
      DO I = 1, NSET
         NOUT( I ) = 0
      END DO

*  Create and fill auxiliary arrays for output list generation.
      DO I = 1, COUNT
         CALL CCD1_MALL( NMAT( I ), '_INTEGER', IPRAN1( I ), STATUS )
         CALL CCD1_MALL( NMAT( I ), '_INTEGER', IPRAN2( I ), STATUS )
         CALL CCD1_GISEQ( 1, 1, NMAT( I ),
     :                    %VAL( CNF_PVAL( IPRAN1( I ) ) ), STATUS )
         CALL CCD1_GISEQ( 1, 1, NMAT( I ),
     :                    %VAL( CNF_PVAL( IPRAN2( I ) ) ), STATUS )
      END DO

*  Generate the ID's for the output lists. Matching positions between
*  the lists and finally merging all positions for each node.
      CALL CCD1_GMMP( %VAL( CNF_PVAL( IPSUB ) ),
     :                NEWED, NSET, IPX1, IPY1, IPRAN1,
     :                IPX2, IPY2, IPRAN2, NMAT, TOLS, OFFS, IPXO, IPYO,
     :                IPRAN, IPID, NOUT, STATUS )

*  Release some memory.
      DO I = 1, COUNT
         CALL CCD1_MFREE( IPRAN1( I ), STATUS )
         CALL CCD1_MFREE( IPRAN2( I ), STATUS )
      END DO

*=======================================================================
*   Writing output lists and updating NDF extensions
*=======================================================================
*  Get the output position lists file names. Use the input names
*  as possible modification elements.
*  Get the names of the corresponding lists of positions.
      CALL CCD1_STRGR( 'OUTLIST', NDFGR, NNDF, NNDF, OUTGRP, NRET,
     :                 STATUS )

*  Write a position list and store its name in the .MORE.CCDPACK
*  extension for each NDF, if it has any entries.
      DO 12 I = 1, NNDF

*  Check whether there are any points in the Set to which this NDF
*  belongs.
         NCOUT = 0
         IS = ISET( I )
         IF ( NOUT( IS ) .GT. 0 ) THEN

*  Get temporary storage for list of all points in pixel coordinates,
*  and list of selected points in pixel coordinates.
            CALL CCD1_MALL( NOUT( IS ), '_DOUBLE', IPXP, STATUS )
            CALL CCD1_MALL( NOUT( IS ), '_DOUBLE', IPYP, STATUS )
            CALL CCD1_MALL( NOUT( IS ), '_INTEGER', IPIQ, STATUS )
            CALL CCD1_MALL( NOUT( IS ), '_DOUBLE', IPXQ, STATUS )
            CALL CCD1_MALL( NOUT( IS ), '_DOUBLE', IPYQ, STATUS )

*  Convert the positions to Pixel coordinates.
            CALL CCD1_FRDM( IWCS( I ), 'Pixel', JPIX, STATUS )
            IMAP = AST_GETMAPPING( IWCS( I ), AST__CURRENT, JPIX,
     :                             STATUS )
            CALL AST_TRAN2( IMAP, NOUT( IS ),
     :                      %VAL( CNF_PVAL( IPXO( IS ) ) ),
     :                      %VAL( CNF_PVAL( IPYO( IS ) ) ), .TRUE.,
     :                      %VAL( CNF_PVAL( IPXP ) ),
     :                      %VAL( CNF_PVAL( IPYP ) ), STATUS )

*  Get the bounds of the NDF.
            CALL NDF_BOUND( INDF( I ), 2, LBND, UBND, NDIM, STATUS )
            XLO = DBLE( LBND( 1 ) - 1 )
            YLO = DBLE( LBND( 2 ) - 1 )
            XHI = DBLE( UBND( 1 ) )
            YHI = DBLE( UBND( 2 ) )

*  Pick out only those points which fall in the area of the data array
*  of this NDF.
            CALL CCD1_CHUSB( %VAL( CNF_PVAL( IPID( IS ) ) ),
     :                       %VAL( CNF_PVAL( IPXP ) ),
     :                       %VAL( CNF_PVAL( IPYP ) ),
     :                       NOUT( IS ), XLO, XHI,
     :                       YLO, YHI, %VAL( CNF_PVAL( IPIQ ) ),
     :                       %VAL( CNF_PVAL( IPXQ ) ),
     :                       %VAL( CNF_PVAL( IPYQ ) ), NCOUT, STATUS )
         END IF

*  Open the position list file.
         CALL GRP_GET( OUTGRP, I, 1, FNAME, STATUS )
         CALL CCD1_OPFIO( FNAME, 'WRITE', 'LIST', 0, FDO, STATUS )
         CALL CCD1_FIOHD( FDO, 'Output from PAIRNDF', STATUS )

*  Write the data.
         CALL CCD1_WRIXY( FDO, %VAL( CNF_PVAL( IPIQ ) ),
     :                    %VAL( CNF_PVAL( IPXQ ) ),
     :                    %VAL( CNF_PVAL( IPYQ ) ),
     :                    NCOUT, LINE, CCD1__BLEN,
     :                    STATUS )

*  Close the file.
         CALL FIO_CLOSE( FDO, STATUS )

*  Store the names of the position lists in the NDF extensions
         CALL CCG1_STO0C( INDF( I ), 'CURRENT_LIST', FNAME, STATUS )

*  Release some resources.
         IF ( NOUT( IS ) .GT. 0 ) THEN
            CALL CCD1_MFREE( IPXP, STATUS )
            CALL CCD1_MFREE( IPYP, STATUS )
            CALL CCD1_MFREE( IPIQ, STATUS )
            CALL CCD1_MFREE( IPXQ, STATUS )
            CALL CCD1_MFREE( IPYQ, STATUS )
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
      CALL PAR_PUT0C( 'MARKSTYLE1', MSTYA, STATUS )
      CALL PAR_PUT0C( 'MARKSTYLE2', MSTYB, STATUS )

*  Exit label.
 99   CONTINUE

*  Release all memory
      CALL CCD1_MFREE( -1, STATUS )

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
