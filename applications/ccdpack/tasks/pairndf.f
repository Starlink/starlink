      SUBROUTINE PAIRNDF( STATUS )
*+
*  Name:
*     PAIRNDF

*  Purpose:
*     Displays and manipulates image pairs to allow easy registration.

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
*     simple offsets.  By making use of a graphical user interface you
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
*     able to be paired and an aligner which allows you to move 
*     a pair of images around so that they are registered, and to
*     mark points in the overlapping region where the same centroidable 
*     features exist in both images.
*
*     Operation is as follows.  You must first use the chooser window
*     to select a pair of images which have a region in common.
*     Use the tabs at each side of the screen to pick the image to
*     appear on that side.  You can use the "Show FITS" button to
*     select one or more FITS headers to be displayed alongside
*     each NDF if this will make it easier to identify which is which.
*     When you have selected a pair which you wish to register, 
*     click the "Use this pair" button.  The aligner window will
*     then appear, displaying the two images which you selected.
*     You can drag either of these images around the display region 
*     by holding the mouse button down as you move the mouse; the 
*     easiest way to align the pair is to "pick up" one image by an
*     identifiable feature and "drop" it on the same feature in 
*     the other image.  Where the images overlap their pixels are
*     averaged.  If you are not happy with the positioning, you can
*     move them again.  Once you are happy that they are aligned
*     about right, then click in the overlap region to mark bright
*     features which appear in both images - these will be centroided
*     to get an accurage alignment between the images.  During this
*     part you mark points by clicking with mouse button 1 (usually
*     the left one) and you can remove them by clicking with button 3
*     (usually the right one).  When you
*     have aligned the images and marked shared features, or if you
*     decide that the pair cannot be satisfactorily registered,
*     press the "Done" button on the aligner window.  You must then
*     select another pair and repeat the process.  Once you have
*     made enough pairings to register the whole set, the graphical
*     windows will disappear and the program will continue without
*     further graphical interaction.  Apart from the first pair, 
*     you will only be allowed to select a pair of images to align
*     if at least one of each pair has been aligned with another
*     image before these images are indicated by a highlighting mark 
*     in their chooser tabs.

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
*     MAXCANV = INTEGER (Read and Write)
*        A dimension in pixels for the maximum X or Y dimension of the
*        region in which the NDFs are displayed for registration.  Note 
*        this is the scrolled region, and may be much bigger than the 
*        sizes given by WINX and WINY, which limit the size of the window 
*        on the X display.  It can be overridden during operation by 
*        zooming in and out using the GUI controls, but it is intended
*        to limit the size for the case when ZOOM is large (perhaps
*        because the last image was quite small) and a large image
*        is going to be displayed, which otherwise might lead to
*        the program attempting to display an enormous viewing region.
*        If set to zero, then no limit is in effect.
*        [1280]
*     NAMELIST = LITERAL (Read)
*        The name of a file to contain the names of the output
*        position lists. The names written to this file are those
*        generated using the expression given to the OUTLIST parameter.
*        This file may be used in an indirection expression to input
*        all the position lists output from this routine into another
*        routine, if the associating position lists with NDFs option is
*        not being used.
*        [PAIRNDF.LIS]
*     OUTLIST = LITERAL (Read)
*        An expression which is either a list of names or expands to a
*        list of names for the output position lists.
*
*        These may be specified as list of comma separated names, 
*        using indirection if required, OR, as a single modification 
*        element (of the input names). The simplest modification
*        element is the asterisk "*" which means call each of the
*        output lists the same name as the corresponding input NDFs (but
*        without the ".SDF" extension).
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
*        The low and high percentiles of the data range to use when 
*        displaying the images; any pixels with a value lower than 
*        the first value will have the same colour, and any with a value
*        higher than the second will have the same colour.  Must be in
*        the range 0 <= PERCENTILES( 1 ) <= PERCENTILES( 2 ) <= 100.
*        [2,98]
*     WINX = INTEGER (Read and Write)
*        The width in pixels of the window to display the image and
*        associated controls in.  If the image is larger than the area
*        allocated for display, it can be scrolled around within the
*        window.  The window can be resized in the normal way using
*        the window manager while the program is running.
*        [200]
*     WINY = INTEGER (Read and Write)
*        The height in pixels of the window to display the image and
*        associated controls in.  If the image is larger than the area
*        allocated for display, it can be scrolled around within the
*        window.  The window can be resized in the normal way using
*        the window manager while the program is running.
*        [300]
*     ZOOM = DOUBLE (Read and Write)
*        A factor giving the initial level to zoom in to the image
*        displayed, that is the number of screen pixels to use for one
*        image pixel.  It will be rounded to one of the values
*        ... 3, 2, 1, 1/2, 1/3 ....  The zoom can be changed
*        interactively from within the program.  The initial value
*        may be limited by MAXCANV.
*        [1]

*  Examples:
*     pairndf '*'  '*.dat' '[1,99]'
*        In this routine the positional nature of the parameters is
*        shown. All the NDFs in the current directory are displayed.
*        Their output positions lists have the same name as the NDFs
*        except that they have a file type of .dat. The images are 
*        displayed using the percentile limits 1,99 which shows bright 
*        detail well.

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

*  Implementation Deficiencies:
*     - No support for coordinate systems other than pixel coordinates.

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
*     The DEVICE parameter also has a global association. This is not
*     controlled by the usual CCDPACK mechanisms, instead it works in
*     co-operation with KAPPA (SUN/95) image display/control routines.

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
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'CCD1_PAR'        ! CCDPACK parameterisations
      INCLUDE 'MSG_PAR'         ! Message system parameterisations
      INCLUDE 'AST_PAR'         ! Standard AST system declarations

*  Status:
      INTEGER STATUS            ! Global status

*  Local Variables:
      CHARACTER * ( 80 ) FNAME  ! Filename
      CHARACTER * ( CCD1__BLEN ) LINE ! Buffer for writing output lines
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
      INTEGER JCUR              ! Frame index of current frame
      INTEGER JPIX              ! Frame index of pixel frame
      INTEGER MAXCNV            ! Initial maximum dimension of display region
      INTEGER NDFGR             ! Input NDF group identifier
      INTEGER NEDGES            ! Number of edges in graph
      INTEGER NEWED             ! Number of edges in spanning graph
      INTEGER NMAT( CCD1__MXNDF * CCD1__MXNDF ) ! Number of position selected
      INTEGER NNDF              ! Number of input NDFs
      INTEGER NNODE             ! Number of nodes in spanning graph
      INTEGER NODES( 2, CCD1__MXNDF * CCD1__MXNDF ) ! Original node numbers
      INTEGER NOUT( CCD1__MXNDF ) ! Numbers of output positions
      INTEGER NRET              ! Dummy variable
      INTEGER OFFS( CCD1__MXNDF+ 1 ) ! Offsets into extended lists
      INTEGER OUTGRP            ! Output group identifier
      INTEGER TOTNOD            ! Total number of nodes in graph
      INTEGER WINDIM( 2 )       ! Window dimensions for display
      LOGICAL COMPL             ! True if graph is complete
      LOGICAL CYCLIC            ! True if graph is cyclic
                            
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

*  Get a list of NDF names.
      CALL CCD1_NDFGL( 'IN', 2, CCD1__MXNDF, NDFGR, NNDF, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Get the percentage histogram range for image display.
      CALL PAR_EXACD( 'PERCENTILES', 2, PERCNT, STATUS )

*  Get NDF identifiers.
      DO I = 1, NNDF
         CALL NDG_NDFAS( NDFGR, I, 'UPDATE', INDF( I ), STATUS )
      END DO

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
      CALL CCD1_PNDF( NDFGR, PERCNT, COUNT, ZOOM, MAXCNV, WINDIM, NODES, 
     :                NMAT, XOFF, YOFF, IPX1, IPY1, IPX2, IPY2, STATUS )

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
      CALL CCD1_MALL( NNDF * NNDF, '_LOGICAL', IPBEEN, STATUS )

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
      IF ( COMPL ) THEN    
                           
*  Graph is complete -- all nodes connected.  Determine the `complete'
*  solution.  Find the offsets of all positions to the `reference' set
*  (first node of first edge of spanning graph is assumed to be the
*  reference set).
         CALL CCD1_GROFF( %VAL( IPGRA ), NEDGES, XOFF, YOFF,
     :                    NNDF, %VAL( IPBEEN ), %VAL( IPQUE ),
     :                    XOFFN, YOFFN, STATUS )

*  Get the WCS frameset and the current frame for each NDF.
         DO I = 1, NNDF
            CALL NDF_GTWCS( INDF( I ), IWCS( I ), STATUS )
            FRM( I ) = AST_GETFRAME( IWCS( I ), AST__CURRENT, STATUS )
         END DO

*  Output offset information to the user.
         CALL CCD1_PROFF( NNDF, %VAL( IPBEEN ), XOFFN, YOFFN, FRM,
     :                    .TRUE., STATUS )

*  Generate the ID's for the output lists. Matching positions between
*  the lists and final merging all positions for each node.
         CALL CCD1_GMMP( %VAL( IPSUB ), NEWED, NNODE, IPX1, IPY1, IPX2,
     :                   IPY2, NMAT, OFFS, IPXO, IPYO, IPID, NOUT,
     :                   STATUS )

      ELSE

*  Set STATUS and issue error (may change this to cope with error
*  by forming most likely connection).
         STATUS = SAI__ERROR
         CALL CCD1_ERREP( 'PAIRNDF_NOTCON',
     :'  Intercomparison of positions does not produce a complete'//
     :' registration of all frames -- graph incomplete', STATUS )
         GO TO 99
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99 

*=======================================================================
*   Writing output lists and updating NDF extensions
*=======================================================================
*  Get the output position lists file names. Use the input names
*  as possible modification elements.
*  Get the names of the corresponding lists of positions.
      CALL CCD1_STRGR( 'OUTLIST', NDFGR, NNDF, NNDF, OUTGRP, NRET,
     :                 STATUS )

*  Write a position list for each NDF.
      DO 12 I = 1, NNDF
         IF ( NOUT( I ) .GT. 0 ) THEN 

*  Get temporary storage for list of points in pixel coordinates.
            CALL CCD1_MALL( NOUT( I ), '_DOUBLE', IPXP, STATUS )
            CALL CCD1_MALL( NOUT( I ), '_DOUBLE', IPYP, STATUS )

*  Convert the positions to Pixel coordinates.
            JCUR = AST_GETI( IWCS( I ), 'Current', STATUS )
            CALL CCD1_FRDM( IWCS, 'Pixel', JPIX, STATUS )
            IMAP = AST_GETMAPPING( IWCS( I ), JCUR, JPIX, STATUS )
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
         END IF
 12   CONTINUE

*  Write an output list of the names for other applications to use.
      IF ( STATUS .EQ. SAI__OK ) THEN 
         CALL CCD1_LNAM( 'NAMELIST', 1, NNDF,
     :                   'PAIRNDF - output position lists',
     :                   OUTGRP, .TRUE., STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            CALL CCD1_MSG( ' ', '  No namelist written ', STATUS )
         END IF
      END IF

*  Write display preference parameters back to the parameter system.
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      CALL PAR_PUT0D( 'ZOOM', ZOOM, STATUS )
      CALL PAR_PUT0I( 'MAXCANV', MAXCNV, STATUS )
      CALL PAR_PUT0I( 'WINX', WINDIM( 1 ), STATUS )
      CALL PAR_PUT0I( 'WINY', WINDIM( 2 ), STATUS )

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
