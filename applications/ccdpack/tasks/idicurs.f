      SUBROUTINE IDICURS( STATUS )
*+                   
*  Name:             
*     IDICURS        
                     
*  Purpose:          
*     Views and writes position lists interactively.
                     
*  Language:         
*     Starlink Fortran 77
                     
*  Type of Module:   
*     ADAM A-task    
                     
*  Invocation:       
*     CALL IDICURS( STATUS )
                     
*  Arguments:        
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This program displays an image or Set of images on the screen 
*     and provides a graphical user interface for marking points on it.
*     Points can be read in from a position list file at the start
*     (if READLIST is true) or written out to a position list file
*     at the end (if WRITELIST is true) or both.  If OVERWRITE is
*     true then a position list file can be viewed and edited in place.
*
*     The graphical interface used for marking features on the image
*     should be fairly self-explanatory.  The image can be scrolled
*     using the scrollbars, the window can be resized, and there are 
*     controls for zooming the image in or out, changing the style of 
*     display, and altering the percentile cutoff limits which control
*     the mapping of pixel value to displayed colour.  The position 
*     of the cursor is reported below the display using the coordinates 
*     of the selected coordinate frame for information, but the position 
*     list written out is always written in Pixel coordinates, since 
*     that is how all CCDPACK applications expect to find it written.
*     Points are marked on the image by clicking mouse button 1 
*     (usually the left one) and may be removed using mouse button 3
*     (usually the right one).  When you have marked all the points
*     that you wish to, click the 'Done' button.

*  Usage:
*     idicurs in

*  ADAM Parameters:
*     IN = LITERAL (Read)
*        Gives the name of the NDFs to display and get coordinates from.
*        If multiple NDFs are specified using wildcards or separating 
*        their names with commas, the program will run on each one in
*        turn, or on each Set in turn if applicable (see the USESET
*        parameter).
*     INEXT = _LOGICAL (Read)
*        If the READLIST parameter is true, then this parameter 
*        determines where the input position list comes from.  If it
*        is true, then the position list currently associated with the
*        NDF will be used.  If it is false, then the input position list
*        names will be obtained from the INLIST parameter.
*        [FALSE]
*     INLIST = FILENAME (Read)
*        If the READLIST parameter is true, and the INEXT parameter
*        is false, then this parameter gives the names of the files
*        in which the input position list is stored.  This parameter
*        may use modifications of the input NDF name.
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
*        A string indicating how markers are initially to be plotted on
*        the image.  It consists of a comma-separated list of 
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
*        ["showindex=1"]
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
*     READLIST = _LOGICAL (Read)
*        If this parameter is true, then the program will start up 
*        with with some positions already marked (where the points 
*        come from depends on the INEXT and INLIST parameters).
*        If it is  false, the program will start up with no points 
*        initially plotted.
*        [FALSE]
*     OUTLIST = FILENAME (Write)
*        If WRITELIST is true, and OVERWRITE is false, then this 
*        parameter determines the names of the files to use to write 
*        the position lists into.  It can be given as a comma-separated
*        list with the same number of filenames as there are IN files,
*        but wildcards can also be used to act as modifications of
*        the input NDF names.
*
*        This parameter is ignored if WRITELIST is false or READLIST 
*        and OVERWRITE are true.
*     OVERWRITE = _LOGICAL (Read)
*        If READLIST and WRITELIST are both true, then setting OVERWRITE
*        to true causes the input position list file to be used as
*        the output position list file as well.  Thus, setting this
*        parameter to true allows position list files to be edited in
*        place.
*        [FALSE]
*     PERCENTILES( 2 ) = _DOUBLE (Read and Write)
*        The initial values for the low and high percentiles of the data 
*        range to use when displaying the images; any pixels with a value
*        lower than the first element will have the same colour, and any 
*        with a value higher than the second will have the same colour.
*        Must be in the range 0 <= PERCENTILES( 1 ) <= PERCENTILES( 2 ) 
*        <= 100.  These values can be changed interactively while the
*        program runs.
*        [2,98]
*     USESET = _LOGICAL (Read)
*        This parameter determines whether Set header information
*        should be used in the object matching.  If USESET is true,
*        IDICURS will try to group images according to their Set
*        Name attribute before displaying them, rather than treating
*        them one by one.  All images which share the same (non-blank)
*        Set Name attribute, and which have a CCD_SET attached 
*        coordinate system, will be shown together in the viewer
*        resampled into their CCD_SET coordinates.
*
*        If USESET is false, then regardless of Set headers, each 
*        individual NDF will be displayed for marking separately.
*
*        If the input images have no Set headers, or if they have no
*        CCD_SET coordinates in their WCS components, the value of
*        USESET will make no difference.
*
*        If a global value for this parameter has been set using
*        CCDSETUP then that value will be used.
*        [TRUE]
*     VERBOSE = _LOGICAL (Read)
*        If this parameter is true, then at the end of processing all
*        the positions will be written through the CCDPACK log system.
*        [TRUE]
*     WINX = _INTEGER (Read and Write)
*        The width in pixels of the window to display the image and
*        associated controls in.  If the image is larger than the area
*        allocated for display, it can be scrolled around within the 
*        window.  The window can be resized in the normal way using 
*        the window manager while the program is running.
*        [450]
*     WINY = _INTEGER (Read and Write)
*        The height in pixels of the window to display the image and
*        associated controls in.  If the image is larger than the area
*        allocated for display, it can be scrolled around within the
*        window.  The window can be resized in the normal way using
*        the window manager while the program is running.
*        [600]
*     WRITELIST = _LOGICAL (Read)
*        This parameter determines whether an output position list
*        file will be written and associated with the input images.
*
*        If the program exits normally, there are points are marked 
*        on the image, and WRITELIST is true, then the points will be 
*        written to a position list file and that file will be associated 
*        with the image file.  The name of the position list file is
*        determined by the OUTLIST and OVERWRITE parameters.  The 
*        positions will be written to the file using the standard
*        CCDPACK format as described in the Notes section.
*
*        If this parameter is true and no points are marked on the
*        image when the program finishes, then the image file will be
*        left with no associated position list file - any pre-existing
*        one will be de-associated with it.
*
*        If WRITELIST is false, then no position lists are written and
*        no changes are made to the image associated position lists.
*        [FALSE]
*     ZOOM = _INTEGER (Read and Write)
*        A factor giving the initial level to zoom in to the image 
*        displayed, that is the number of screen pixels to use for one
*        image pixel.  It will be rounded to one of the values 
*        ... 3, 2, 1, 1/2, 1/3 ....  The zoom can be changed 
*        interactively from within the program.  The initial value 
*        may be limited by MAXCANV.
*        [1]

*  Examples:
*     idicurs mosaic mos.lis
*        This starts up the graphical user interface, allowing you to 
*        select a number of points which will be written to the 
*        position list file 'mos.lis', which will be associated with
*        the image file.
*
*     idicurs in=* out=*.pts percentiles=[10,90] useset=false
*        Each of the NDFs in the current directory will be displayed, 
*        and the positions marked on it written to a list with the same
*        name as the image but the extension '.pts', which will be 
*        associated with the image in question.  The display will
*        initially be scaled so that pixels with a value higher than
*        the 90th percentile will all be displayed as the brightest
*        colour and those with a value lower than the 10th percentile
*        as the dimmest colour, but this may be changed interactively
*        while the program is running.  Since USESET is explicitly
*        set to false, each input NDF will be viewed and marked 
*        separately, even if some they have Set headers and Set
*        alignment coordinates, 
*
*     idicurs in=gc6253 readlist inlist=found.lis outlist=out.lis
*             markstyle="colour=skyblue,showindex=0"
*        The image gc6253 will be displayed, with the points stored in
*        the position list 'found.lis' already plotted on it.  These 
*        may be added to, moved and deleted, and the resulting list
*        will be written to the file out.lis.  Points will initially
*        be marked using skyblue markers, and not labelled with index 
*        numbers.
*
*     idicurs * readlist writelist inext overwrite
*        All the images in the current directory will be displayed,
*        one after the other, with the points which are in their 
*        currently associated position lists already plotted.  
*        You can add and remove points, and the modified position 
*        lists will be written back into the same files.

*  Notes:
*     - Position list formats.
*
*       CCDPACK supports data in two formats.
*
*       CCDPACK format - the first three columns are interpreted as the
*       following.
*
*          - Column 1: an integer identifier
*          - Column 2: the X position
*          - Column 3: the Y position
*
*       The column one value must be an integer and is used to identify
*       positions which are the same but which have different locations
*       on different images. Values in any other (trailing) columns are
*       usually ignored.
*
*       EXTERNAL format - positions are specified using just an X and
*       a Y entry and no other entries.
*
*          - Column 1: the X position
*          - Column 2: the Y position
*
*       This format is used by KAPPA applications such as CURSOR.
*
*       Comments may be included in a file using the characters "#" and
*       "!". Columns may be separated by the use of commas or spaces.
*
*       Input position lists read when READLIST is true may be in either
*       of these formats.  The output list named by the OUTLIST 
*       parameter will be written in CCDPACK (3 column) format.
*
*       In all cases, the coordinates in position lists are pixel 
*       coordinates.
*
*     - NDF extension items. 
*
*       On normal exit, unless OUTLIST is set to null (!), the 
*       CURRENT_LIST items in the CCDPACK extensions (.MORE.CCDPACK) of 
*       the input NDFs are set to the name of the output list. These 
*       items will be used by other CCDPACK position list processing 
*       routines to automatically access the list.

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
*     Certain parameters (LOGTO, LOGFILE and USESET) have global values.
*     These global values will always take precedence, except when an
*     assignment is made on the command line.  Global values may be set
*     and reset using the CCDSETUP and CCDCLEAR commands.
*
*     Some of the parameters (MAXCANV, PERCENTILES, WINX, WINY, ZOOM,
*     MARKSTYLE) give initial values for quantities which can be modified 
*     while the program is running.  Although these may be specified on
*     the command line, it is normally easier to start the program up and
*     modify them using the graphical user interface.  If the program
*     exits normally, their values at the end of the run will be used
*     as defaults next time the program starts up.
      
*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-APR-2000 (MBT):
*        Original version (a previous file idicurs.f existed, but this 
*        is a rewrite from scratch).
*     9-APR-2001 (MBT):
*        Upgraded for use with Sets.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}
                     
*-                   
                     
*  Type Definitions: 
      IMPLICIT NONE              ! No implicit typing
                     
*  Global Constants: 
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'GRP_PAR'          ! GRP system constants
      INCLUDE 'AST_PAR'          ! AST system declarations
      INCLUDE 'CCD1_PAR'         ! Private CCDPACK constants

*  Local Constants:
      INTEGER MAXPOS             ! Maximum positions in the list
      PARAMETER ( MAXPOS = 999 )
                     
*  Status:           
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Length of string excluding trailing spaces
                     
*  Local Variables:  
      INTEGER FD                 ! FIO identifier of output file
      INTEGER I                  ! Loop counter
      INTEGER ID( MAXPOS )       ! Identifiers for position list
      INTEGER ILSTGR             ! GRP identifier for input position list files
      INTEGER IMEM( CCD1__MXNDF ) ! Input NDFs in Set order
      INTEGER IMEMOF( CCD1__MXNDF + 1 ) ! Indexes into IMEM
      INTEGER INDEX              ! Index in list of NDFs
      INTEGER INDF( CCD1__MXNDF ) ! NDF identifiers
      INTEGER IPDAT              ! Pointer to input position list coordinates
      INTEGER IPIND              ! Pointer to input position list indices
      INTEGER IPI2               ! Poiter to position list indices
      INTEGER IPX1               ! Pointer to X coordinates
      INTEGER IPX2               ! Pointer to X coordinates
      INTEGER IPY1               ! Pointer to Y coordinates
      INTEGER IPY2               ! Pointer to Y coordinates
      INTEGER IS                 ! Set loop counter
      INTEGER ISET( CCD1__MXNDF ) ! Index of Set for each NDF
      INTEGER IWCS               ! AST pointer to WCS frameset
      INTEGER JPIX               ! Frame index of Pixel frame
      INTEGER JSET               ! Frame index of CCD_SET frame
      INTEGER LBND( 2 )          ! Lower NDF bounds
      INTEGER MAP( CCD1__MXNDF ) ! AST pointers to PIXEL->CCD_SET mappings
      INTEGER MAPSET( CCD1__MXNDF ) ! Workspace
      INTEGER MAXCNV             ! Initial maximum canvas dimension
      INTEGER NDIM               ! Number of dimensions of NDF
      INTEGER NDFGR              ! NDG identifier of group of NDFs
      INTEGER NF                 ! Number of fields per line of input list
      INTEGER NFIELD             ! Number of fields in first line of input file
      INTEGER NMEM               ! Number of members in Set
      INTEGER NP                 ! Number of positions in this list
      INTEGER NPOS               ! Number of positions in total
      INTEGER NRET               ! Number of returns
      INTEGER NSET               ! Number of Sets
      INTEGER NNDF               ! Number of NDFs in group
      INTEGER SNAMGR             ! GRP identifier for Set names
      INTEGER OLSTGR             ! GRP identifier for output position list files
      INTEGER UBND( 2 )          ! Upper NDF bounds
      INTEGER WINDIM( 2 )        ! Dimensions of display window
      LOGICAL INEXT              ! True if input position list is in extension
      LOGICAL RDLIST             ! True if using initial position lists
      LOGICAL OVERWR             ! Overwrite input list file?
      LOGICAL USESET             ! Use Set alignment frame?
      LOGICAL VERBOS             ! Are we verbose?
      LOGICAL WRLIST             ! Will we write lists?
      DOUBLE PRECISION PERCNT( 2 ) ! Low and high percentiles for display
      DOUBLE PRECISION XLO       ! Lower X limit of NDF
      DOUBLE PRECISION XHI       ! Upper X limit of NDF
      DOUBLE PRECISION XPOS( MAXPOS ) ! X coordinates of positions in list
      DOUBLE PRECISION YLO       ! Lower Y limit of NDF
      DOUBLE PRECISION YHI       ! Upper Y limit of NDF
      DOUBLE PRECISION YPOS( MAXPOS ) ! Y coordinates of positions in list
      DOUBLE PRECISION ZOOM      ! Zoom factor for display
      CHARACTER * ( CCD1__BLEN ) LINE ! Buffer for line output to file
      CHARACTER * ( GRP__SZNAM ) NDFNMS( CCD1__MXNDF ) ! NDF names
      CHARACTER * ( GRP__SZNAM ) SNAME ! Set Name attribute
      CHARACTER * ( GRP__SZFNM ) FNAME ! Name of output list file
      CHARACTER * ( 8 ) ACMODE   ! File access mode
      CHARACTER * ( 8 ) DOMAIN ! Domain for resampling
      CHARACTER * ( 132 ) MSTYLE ! Marker style string

*.
                     
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Execute startup.
      CALL CCD1_START( 'IDICURS', STATUS )

*  Begin NDF context.
      CALL NDF_BEGIN
      NDFGR = GRP__NOID

*  Begin AST context.
      CALL AST_BEGIN( STATUS )

*  Get a group of NDFs.
      CALL CCD1_NDFGL( 'IN', 1, CCD1__MXNDF, NDFGR, NNDF, STATUS )

*  Determine if we will be using initial position lists.
      CALL PAR_GET0L( 'READLIST', RDLIST, STATUS )

*  Get the input list file names.
      INEXT = .FALSE.
      IF ( RDLIST ) THEN

*  Determine if we will be getting position lists from the NDF extensions
*  or directly from a parameter.
         CALL PAR_GET0L( 'INEXT', INEXT, STATUS )

*  Get the position list files.
         CALL CCD1_GTLIH( INEXT, NDFGR, 'CURRENT_LIST', 'INLIST', NNDF,
     :                    ILSTGR, STATUS )
      END IF

*  Determine if we will be writing position lists.
      CALL PAR_GET0L( 'WRITELIST', WRLIST, STATUS )

*  Get the output list file names.
      IF ( WRLIST ) THEN

*  Should we overwrite the input list files?
         OVERWR = .FALSE.
         IF ( RDLIST ) CALL PAR_GET0L( 'OVERWRITE', OVERWR, STATUS )

*  If we are just overwriting the original files then copy the input 
*  list group to the output list group.
         IF ( OVERWR ) THEN
            CALL GRP_COPY( ILSTGR, 1, NNDF, .FALSE., OLSTGR, STATUS )

*  Otherwise get the output list group from the OUTLIST parameter, 
*  using the input NDF group as a modification element.
         ELSE
            CALL CCD1_STRGR( 'OUTLIST', NDFGR, NNDF, NNDF, OLSTGR, NRET,
     :                       STATUS )
         END IF
      END IF

*  See if we are using Sets.
      CALL PAR_GET0L( 'USESET', USESET, STATUS )

*  If we are using Sets, group the NDFs accordingly.
      CALL CCD1_SETSW( NDFGR, NNDF, USESET, ISET, NSET, IMEM, IMEMOF,
     :                 SNAMGR, MAPSET, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Get display preference parameters from the parameter system.
      CALL PAR_EXACD( 'PERCENTILES', 2, PERCNT, STATUS )
      CALL PAR_GET0D( 'ZOOM', ZOOM, STATUS )
      CALL PAR_GET0I( 'MAXCANV', MAXCNV, STATUS )
      CALL PAR_GET0I( 'WINX', WINDIM( 1 ), STATUS )
      CALL PAR_GET0I( 'WINY', WINDIM( 2 ), STATUS )
      CALL PAR_GET0C( 'MARKSTYLE', MSTYLE, STATUS )

*  See if we are verbose.
      CALL PAR_GET0L( 'VERBOSE', VERBOS, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Issue instructions about how to interact with the GUI.
      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( ' ', '   Click on the image with mouse button 1 '
     :              // '(left) to mark a point', STATUS )
      CALL MSG_OUT( ' ', '                      with mouse button 3 '
     :              // '(right) to erase a point', STATUS )
      CALL MSG_OUT( ' ', '   and click the "Done" button '
     :              // 'when finished.', STATUS )

*  Loop over each selected NDF Set in turn.
      DO IS = 1, NSET
         IF ( STATUS .NE. SAI__OK ) GO TO 99
         CALL GRP_GET( SNAMGR, IS, 1, SNAME, STATUS )

*  See whether we will need to transform points.
         NMEM = IMEMOF( IS + 1 ) - IMEMOF( IS )
         IF ( NMEM .EQ. 1 ) THEN
            DOMAIN = 'PIXEL'
         ELSE
            DOMAIN = 'CCD_SET'
         END IF

*  Prepare for access to the NDFs.
         DO I = 1, NMEM
            INDEX = IMEM( IMEMOF( IS ) + I - 1 )

*  If we will write a list then access for update, otherwise just for
*  read.
            IF ( WRLIST ) THEN
               ACMODE = 'UPDATE'
            ELSE
               ACMODE = 'READ'
            END IF
            CALL NDG_NDFAS( NDFGR, INDEX, ACMODE, INDF( I ), STATUS )
            CALL GRP_GET( NDFGR, INDEX, 1, NDFNMS( I ), STATUS )

*  If we will have to transform points, get the correct mapping for
*  each NDF.
            IF ( DOMAIN .NE. 'PIXEL' ) THEN
               CALL CCD1_GTWCS( INDF( I ), IWCS, STATUS )
               CALL CCD1_FRDM( IWCS, 'PIXEL', JPIX, STATUS )
               CALL CCD1_FRDM( IWCS, 'CCD_SET', JSET, STATUS )
               MAP( I ) = AST_GETMAPPING( IWCS, JPIX, JSET, STATUS )
            END IF
         END DO

*  Write a short message to the user.
         CALL CCD1_MSG( ' ', ' ', STATUS )
         IF ( USESET ) THEN
            CALL MSG_SETI( 'NUM', IS )
            CALL CCD1_MSG( ' ', '  Displaying Set ^NUM)', STATUS )
         ELSE 
            CALL MSG_SETC( 'NAME', NDFNMS( 1 ) )
            CALL CCD1_MSG( ' ', '  Displaying NDF ^NAME', STATUS )
         END IF

*  Unless we read some in, there will be no positions in the initial 
*  position list.
         NPOS = 0

*  If we are using an initial position list, we will have to read the 
*  positions in.
         IF ( RDLIST ) THEN
            DO I = 1, NMEM
               INDEX = IMEM( IMEMOF( IS ) + I - 1 )
               CALL GRP_GET( ILSTGR, INDEX, 1, FNAME, STATUS )

*  Open the input file if there is one.
               IF ( FNAME .NE. ' ' ) THEN
                  CALL CCD1_OPFIO( FNAME, 'READ', 'LIST', 0, FD,
     :                             STATUS )
                  CALL CCD1_LTEST( FD, LINE, CCD1__BLEN, 2, 0, NFIELD,
     :                             STATUS )

*  Map in X and Y positions only (non-standard file).
                  IF ( NFIELD .EQ. 2 ) THEN
                     CALL CCD1_NLMAP( FD, LINE, CCD1__BLEN, IPDAT, NPOS,
     :                                NF, STATUS )

*  Standard file format map these in.
                  ELSE
                     CALL CCD1_LMAP( FD, LINE, CCD1__BLEN, IPIND, IPDAT,
     :                               NP, NF, STATUS )
                  END IF

*  Close the input file.
                  CALL FIO_CLOSE( FD, STATUS )

*  Log to user.
                  CALL MSG_SETI( 'NP', NP )
                  CALL MSG_SETC( 'ILIST', FNAME )
                  CALL CCD1_MSG( ' ', 
     :            '    Read ^NP positions from file ^ILIST.', STATUS )

*  If there are too many points in the input position list then prepare
*  to truncate it.
                  IF ( NPOS + NP .GT. MAXPOS ) THEN
                     CALL MSG_SETI( 'NUM', NPOS + NP - MAXPOS )
                     CALL CCD1_MSG( ' ', 
     :'    Warning - too many points in input list, discarding ^NUM',
     :                              STATUS )
                     NP = MAXPOS - NPOS
                  END IF

*  No input file - no points.
               ELSE
                  NP = 0
               END IF

*  See if we have a non-empty list.
               IF ( NP .GT. 0 ) THEN

*  Copy the points so they can be read by the GUI routine, and free the 
*  temporarily allocated memory.
                  IF ( NFIELD .EQ. 2 ) THEN
                     CALL CCD1_GISEQ( NPOS + 1, 1, NP, ID( NPOS + 1 ), 
     :                                STATUS )
                  ELSE
                     CALL CCG1_COPAI( NP, %VAL( IPIND ), ID( NPOS + 1 ),
     :                                STATUS )
                     CALL CCD1_MFREE( IPIND, STATUS )
                  END IF

*  Transform the points if necessary.
                  IF ( DOMAIN .EQ. 'PIXEL' ) THEN
                     CALL CCD1_LEXT( %VAL( IPDAT ), NP, NF, 1,
     :                               XPOS( NPOS + 1 ), STATUS )
                     CALL CCD1_LEXT( %VAL( IPDAT ), NP, NF, 2,
     :                               YPOS( NPOS + 1 ), STATUS )
                  ELSE
                     CALL CCD1_MALL( NP, '_DOUBLE', IPX1, STATUS )
                     CALL CCD1_MALL( NP, '_DOUBLE', IPY1, STATUS )
                     CALL CCD1_LEXT( %VAL( IPDAT ), NP, NF, 1,
     :                               %VAL( IPX1 ), STATUS )
                     CALL CCD1_LEXT( %VAL( IPDAT ), NP, NF, 2,
     :                               %VAL( IPY1 ), STATUS )
                     CALL AST_TRAN2( MAP( I ), NP, %VAL( IPX1 ),
     :                               %VAL( IPY1 ), .TRUE., 
     :                               XPOS( NPOS + 1 ), YPOS( NPOS + 1 ),
     :                               STATUS )
                     CALL CCD1_MFREE( IPX1, STATUS )
                     CALL CCD1_MFREE( IPY1, STATUS )
                  END IF

*  Release memory.
                  CALL CCD1_MFREE( IPDAT, STATUS )
               END IF

*  Increment the total number of points for this Set.
               NPOS = NPOS + NP
            END DO
         END IF
         
*  Invoke the Tcl code to do the work.
         CALL CCD1_TCURS( NDFNMS, NMEM, SNAME, DOMAIN, MAXPOS, PERCNT,
     :                    ZOOM, MAXCNV, WINDIM, MSTYLE, VERBOS, ID,
     :                    XPOS, YPOS, NPOS, STATUS )

*  If requested to do so, write the returned positions to an output 
*  position list file.
         IF ( WRLIST ) THEN
            IF ( NMEM .EQ. 1 ) THEN
               IF ( NPOS .GT. 0 ) THEN

*  Open the output position list file.
                  CALL GRP_GET( OLSTGR, IMEM( IMEMOF( IS ) ), 1, FNAME,
     :                          STATUS )
                  CALL CCD1_OPFIO( FNAME, 'WRITE', 'LIST', 0, FD,
     :                             STATUS )

*  Write a header to the file.
                  CALL CCD1_FIOHD( FD, 'Output from IDICURS', STATUS )

*  Write the positions to the output file.
                  CALL CCD1_WRIXY( FD, ID, XPOS, YPOS, NPOS, LINE, 
     :                             CCD1__BLEN, STATUS )

*  Report file used and number of entries.
                  CALL CCD1_MSG( ' ', ' ', STATUS )
                  CALL MSG_SETC( 'FNAME', FNAME )
                  CALL MSG_SETI( 'NPOS', NPOS )
                  CALL CCD1_MSG( ' ', 
     :            '    Wrote ^NPOS positions to file ^FNAME', STATUS )

*  Modify the CURRENT_LIST item of the CCDPACK extension accordingly.
                  CALL CCG1_STO0C( INDF( 1 ), 'CURRENT_LIST', FNAME,
     :                             STATUS )

*  If there were no points, ensure that the output position list is
*  not written into the extension.
               ELSE
                  CALL CCD1_RMIT( INDF( 1 ), 'CURRENT_LIST', .TRUE., 
     :                            STATUS )
               END IF

*  Multiple members of this Set.  In this case we have to transform
*  back from CCD_SET coordinates, and only write the appropriate 
*  points into each position list.
            ELSE

*  Allocate enough space to hold transformed coordinates, and a 
*  subset of this Set's position list.
               IF ( NPOS .GT. 0 ) THEN
                  CALL CCD1_MALL( NPOS, '_DOUBLE', IPX1, STATUS )
                  CALL CCD1_MALL( NPOS, '_DOUBLE', IPY1, STATUS )
                  CALL CCD1_MALL( NPOS, '_DOUBLE', IPX2, STATUS )
                  CALL CCD1_MALL( NPOS, '_DOUBLE', IPY2, STATUS )
                  CALL CCD1_MALL( NPOS, '_INTEGER', IPI2, STATUS )

*  Loop over each NDF in this Set.
                  DO I = 1, NMEM
                     INDEX = IMEM( IMEMOF( IS ) + I - 1 )

*  Transform the positions into pixel coordinates.
                     CALL AST_TRAN2( MAP( I ), NPOS, XPOS, YPOS,
     :                               .FALSE., %VAL( IPX1 ),
     :                               %VAL( IPY1 ), STATUS )

*  Get the NDF's bounds.
                     CALL NDF_BOUND( INDF( I ), 2, LBND, UBND, NDIM,
     :                               STATUS )
                     XLO = DBLE( LBND( 1 ) - 1 )
                     YLO = DBLE( LBND( 2 ) - 1 )
                     XHI = DBLE( UBND( 1 ) )
                     YHI = DBLE( UBND( 2 ) )

*  Select only the points in this position list which fall within the
*  bounds of this NDF.
                     CALL CCD1_CHUSB( ID, %VAL( IPX1 ), %VAL( IPY1 ),
     :                                NPOS, XLO, XHI, YLO, YHI, 
     :                                %VAL( IPI2 ), %VAL( IPX2 ), 
     :                                %VAL( IPY2 ), NP, STATUS )

*  If the subset contains some points, write a list and associate it
*  with this NDF.
                     IF ( NP .GT. 0 ) THEN

*  Open the position list file.
                        CALL GRP_GET( OLSTGR, INDEX, 1, FNAME, STATUS )
                        CALL CCD1_OPFIO( FNAME, 'WRITE', 'LIST', 0, FD,
     :                                   STATUS )

*  Write a header to the file.
                        CALL CCD1_FIOHD( FD, 'Output from IDICURS',
     :                                   STATUS )

*  Write the positions out.
                        CALL CCD1_WRIXY( FD, %VAL( IPI2 ), %VAL( IPX2 ),
     :                                   %VAL( IPY2 ), NP, LINE,
     :                                   CCD1__BLEN, STATUS )

*  Report file used and number of entries.
                        CALL MSG_SETC( 'FNAME', FNAME )
                        CALL MSG_SETI( 'NPOS', NP )
                        CALL CCD1_MSG( ' ',
     :'    Wrote ^NPOS positions to file ^FNAME', STATUS )

*  Modify the CURRENT_LIST item of the CCDPACK extension accordingly.
                        CALL CCG1_STO0C( INDF( I ), 'CURRENT_LIST',
     :                                   FNAME, STATUS )

*  If there were no points, ensure that the output position list is
*  not written into the extension.
                     ELSE
                        CALL CCD1_RMIT( INDF( I ), 'CURRENT_LIST',
     :                                  .TRUE., STATUS )
                     END IF
                  END DO

*  Free memory.
                  CALL CCD1_MFREE( IPX1, STATUS )
                  CALL CCD1_MFREE( IPY1, STATUS )
                  CALL CCD1_MFREE( IPX2, STATUS )
                  CALL CCD1_MFREE( IPY2, STATUS )
                  CALL CCD1_MFREE( IPI2, STATUS )

*  No points marked at all; just erase the CURRENT_LIST items.
               ELSE
                  DO I = 1, NMEM
                     CALL CCD1_RMIT( INDF( I ), 'CURRENT_LIST', .TRUE.,
     :                               STATUS )
                  END DO
               END IF
            END IF
         END IF
      END DO

*  Write display preference parameters back to the parameter system.
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      CALL PAR_PUT0D( 'ZOOM', ZOOM, STATUS )
      CALL PAR_PUT0I( 'MAXCANV', MAXCNV, STATUS )
      CALL PAR_PUT0I( 'WINX', WINDIM( 1 ), STATUS )
      CALL PAR_PUT0I( 'WINY', WINDIM( 2 ), STATUS )
      CALL PAR_PUT0C( 'MARKSTYLE', MSTYLE, STATUS )
      CALL PAR_PUT1D( 'PERCENTILES', 2, PERCNT, STATUS )

*  Exit on error label.
 99   CONTINUE

*  End NDF context.
      CALL NDF_END( STATUS )

*  End AST context.
      CALL AST_END( STATUS )

*  Release group resources.
      CALL CCD1_GRDEL( NDFGR, STATUS )
      CALL CCD1_GRDEL( ILSTGR, STATUS )
      CALL CCD1_GRDEL( OLSTGR, STATUS )
      CALL CCD1_GRDEL( SNAMGR, STATUS )

*  Ensure allocated memory is freed.
      CALL CCD1_MFREE( -1, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL CCD1_ERREP( 'IDICURS_ERR', 
     :                    'IDICURS: Error in cursor program', STATUS )
      END IF

*  Close the log file.
      CALL CCD1_END( STATUS )

      END
* $Id$
