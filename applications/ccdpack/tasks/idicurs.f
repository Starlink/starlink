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
*     CENTROID = _LOGICAL (Read and Write)
*        This parameter controls whether points marked on the image are
*        to be centroided.  If true, then when you click on the image to
*        add a new point IDICURS will attempt to find a centroidable
*        object near to where the click was made and add the point
*        there.  If no centroidable feature can be found nearby, you will
*        not be allowed to add a point.  Note that the centroiding
*        routine is capable of identifying spurious objects in noise,
*        but where a genuine feature is nearby this should find its
*        centre.
*
*        Having centroiding turned on does not guarantee that all points
*        on the image have been centroided, it only affects points
*        added by clicking on the image.  In particular any points read
*        from the INLIST file will not be automatically centroided.
*
*        This parameter only gives the initial centroiding state -
*        centroiding can be turned on and off interactively while the
*        program is running.
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
*        should be used or not.  If USESET is true,
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
*        [FALSE]
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
*     Some of the parameters (MAXCANV, PERCENTILES, WINX, WINY, ZOOM,
*     MARKSTYLE, CENTROID) give initial values for quantities which
*     can be modified while the program is running.  Although these
*     may be specified on the command line, it is normally easier to
*     start the program up and modify them using the graphical user
*     interface.  If the program exits normally, their values at the
*     end of the run will be used as defaults next time the program
*     starts up.

*  Copyright:
*     Copyright (C) 2000-2001 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-APR-2000 (MBT):
*        Original version (a previous file idicurs.f existed, but this
*        is a rewrite from scratch).
*     9-APR-2001 (MBT):
*        Upgraded for use with Sets.
*     22-MAY-2001 (MBT):
*        Changed so that an empty position list file is written when
*        there are no points rather than no file at all.
*     19-JUL-2001 (MBT):
*        Added CENTROID parameter.
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
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Length of string excluding trailing spaces

*  Local Variables:
      INTEGER FD                 ! FIO identifier of output file
      INTEGER I                  ! Loop counter
      INTEGER ILSTGR             ! GRP identifier for input position list files
      INTEGER IMEM( CCD1__MXNDF ) ! Input NDFs in Set order
      INTEGER IMEMOF( CCD1__MXNDF + 1 ) ! Indexes into IMEM
      INTEGER INDEX              ! Index in list of NDFs
      INTEGER INDF( CCD1__MXNDF ) ! NDF identifiers
      INTEGER IPDAT( CCD1__MXNDF ) ! Pointers to input position list coordinates
      INTEGER IPII               ! Pointer to input Set position list IDs
      INTEGER IPIND( CCD1__MXNDF ) ! Pointers to input position list indices
      INTEGER IPIO               ! Pointer to output Set position list IDs
      INTEGER IPI2               ! Poiter to position list indices
      INTEGER IPXI               ! Pointer to input Set position list X coords
      INTEGER IPXO               ! Pointer to output Set position list X coords
      INTEGER IPX1               ! Pointer to X coordinates
      INTEGER IPX2               ! Pointer to X coordinates
      INTEGER IPYI               ! Pointer to input Set position list Y coords
      INTEGER IPYO               ! Pointer to output Set position list Y coords
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
      INTEGER NF( CCD1__MXNDF )  ! Number of fields per line of input list
      INTEGER NFIELD             ! Number of fields in first line of input file
      INTEGER NMEM               ! Number of members in Set
      INTEGER NP( CCD1__MXNDF )  ! Number of positions in this list
      INTEGER NPA                ! Position list index counter
      INTEGER NPO                ! Selected number of output points
      INTEGER NPOSI              ! Total number of input positions
      INTEGER NPOSO              ! Total number of output positions
      INTEGER NRET               ! Number of returns
      INTEGER NSET               ! Number of Sets
      INTEGER NNDF               ! Number of NDFs in group
      INTEGER SNAMGR             ! GRP identifier for Set names
      INTEGER OLSTGR             ! GRP identifier for output position list files
      INTEGER UBND( 2 )          ! Upper NDF bounds
      INTEGER WINDIM( 2 )        ! Dimensions of display window
      LOGICAL CENTRD             ! Centroid points marked on image?
      LOGICAL INEXT              ! True if input position list is in extension
      LOGICAL RDLIST             ! True if using initial position lists
      LOGICAL OVERWR             ! Overwrite input list file?
      LOGICAL USESET             ! Use Set alignment frame?
      LOGICAL VERBOS             ! Are we verbose?
      LOGICAL WRLIST             ! Will we write lists?
      DOUBLE PRECISION PERCNT( 2 ) ! Low and high percentiles for display
      DOUBLE PRECISION XLO       ! Lower X limit of NDF
      DOUBLE PRECISION XHI       ! Upper X limit of NDF
      DOUBLE PRECISION YLO       ! Lower Y limit of NDF
      DOUBLE PRECISION YHI       ! Upper Y limit of NDF
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
      CALL PAR_GET0L( 'CENTROID', CENTRD, STATUS )

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
         NPOSI = 0

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

*  If there are any data in the file, read it.
                  IF ( NFIELD .GT. 0 ) THEN

*  Map in X and Y positions only (non-standard file).
*  Note: this could screw up (give duplicate ID values) in the event that
*  a Set contains some files with explicit ID values and some with
*  implicit ID values.  There would also be trouble in the case that
*  the concatenation of a Set's position list files contain duplicate
*  ID values themselves.  Under normal circumstances (if you have been
*  doing previous processing in a Set-aware way) neither case is likely.
*  If it happens, too bad (probably some positions will get lost).
                     IF ( NFIELD .EQ. 2 ) THEN
                        CALL CCD1_NLMAP( FD, LINE, CCD1__BLEN,
     :                                   IPDAT( I ), NP( I ), NF( I ),
     :                                   STATUS )
                        CALL CCD1_MALL( NP( I ), '_INTEGER', IPIND( I ),
     :                                  STATUS )
                        CALL CCD1_GISEQ( NPOSI + 1, 1, NP( I ),
     :                                   %VAL( CNF_PVAL( IPIND( I ) ) ),
     :                                   STATUS )

*  Standard file format map these in.
                     ELSE
                        CALL CCD1_LMAP( FD, LINE, CCD1__BLEN,
     :                                  IPIND( I ), IPDAT( I ), NP( I ),
     :                                  NF( I ), STATUS )
                     END IF

*  No data - set number of points to zero.
                  ELSE
                     NP( I ) = 0
                  END IF

*  Close the input file.
                  CALL FIO_CLOSE( FD, STATUS )

*  Log to user.
                  CALL MSG_SETI( 'NP', NP( I ) )
                  CALL MSG_SETC( 'ILIST', FNAME )
                  CALL CCD1_MSG( ' ',
     :            '    Read ^NP positions from file ^ILIST.', STATUS )

*  No input file - no points.
               ELSE
                  NP( I ) = 0
               END IF

*  Accumulate total points for this Set.
               NPOSI = NPOSI + NP( I )
            END DO
         END IF

*  Allocate memory for the Set position list.
         IF ( NPOSI .GT. 0 ) THEN
            CALL CCD1_MALL( NPOSI, '_INTEGER', IPII, STATUS )
            CALL CCD1_MALL( NPOSI, '_DOUBLE', IPXI, STATUS )
            CALL CCD1_MALL( NPOSI, '_DOUBLE', IPYI, STATUS )

*  Construct the Set position list from the lists read from each of
*  the member NDFs.
            NPA = 1
            DO I = 1, NMEM

*  See if we have a non-empty list.
               IF ( NP( I ) .GT. 0 ) THEN

*  Transform the points if necessary.
                  CALL CCD1_MALL( NP( I ), '_DOUBLE', IPX2, STATUS )
                  CALL CCD1_MALL( NP( I ), '_DOUBLE', IPY2, STATUS )
                  IF ( DOMAIN .EQ. 'PIXEL' ) THEN
                     CALL CCD1_LEXT( %VAL( CNF_PVAL( IPDAT( I ) ) ),
     :                               NP( I ),
     :                               NF( I ), 1,
     :                               %VAL( CNF_PVAL( IPX2 ) ), STATUS )
                     CALL CCD1_LEXT( %VAL( CNF_PVAL( IPDAT( I ) ) ),
     :                               NP( I ),
     :                               NF( I ), 2,
     :                               %VAL( CNF_PVAL( IPY2 ) ), STATUS )
                  ELSE
                     CALL CCD1_MALL( NP( I ), '_DOUBLE', IPX1, STATUS )
                     CALL CCD1_MALL( NP( I ), '_DOUBLE', IPY1, STATUS )
                     CALL CCD1_LEXT( %VAL( CNF_PVAL( IPDAT( I ) ) ),
     :                               NP( I ),
     :                               NF( I ), 1,
     :                               %VAL( CNF_PVAL( IPX1 ) ), STATUS )
                     CALL CCD1_LEXT( %VAL( CNF_PVAL( IPDAT( I ) ) ),
     :                               NP( I ),
     :                               NF( I ), 2,
     :                               %VAL( CNF_PVAL( IPY1 ) ), STATUS )
                     CALL AST_TRAN2( MAP( I ), NP( I ),
     :                               %VAL( CNF_PVAL( IPX1 ) ),
     :                               %VAL( CNF_PVAL( IPY1 ) ), .TRUE.,
     :                               %VAL( CNF_PVAL( IPX2 ) ),
     :                               %VAL( CNF_PVAL( IPY2 ) ),
     :                               STATUS )
                     CALL CCD1_MFREE( IPX1, STATUS )
                     CALL CCD1_MFREE( IPY1, STATUS )
                  END IF

*  Copy the points from their current arrays into the right part of
*  the Set array.
                  CALL CCG1_COPSI( 1, %VAL( CNF_PVAL( IPIND( I ) ) ),
     :                             NP( I ),
     :                             NPA, %VAL( CNF_PVAL( IPII ) ),
     :                             STATUS )
                  CALL CCG1_COPSD( 1, %VAL( CNF_PVAL( IPX2 ) ),
     :                             NP( I ), NPA,
     :                             %VAL( CNF_PVAL( IPXI ) ), STATUS )
                  CALL CCG1_COPSD( 1, %VAL( CNF_PVAL( IPY2 ) ),
     :                             NP( I ), NPA,
     :                             %VAL( CNF_PVAL( IPYI ) ), STATUS )

*  Increment the position in the Set array.
                  NPA = NPA + NP( I )

*  Release memory.
                  CALL CCD1_MFREE( IPDAT( I ), STATUS )
                  CALL CCD1_MFREE( IPIND( I ), STATUS )
                  CALL CCD1_MFREE( IPX2, STATUS )
                  CALL CCD1_MFREE( IPY2, STATUS )
               END IF
            END DO
         END IF

*  Invoke the Tcl code to do the work.
         CALL CCD1_TCURS( NDFNMS, NMEM, SNAME, DOMAIN,
     :                    %VAL( CNF_PVAL( IPII ) ),
     :                    %VAL( CNF_PVAL( IPXI ) ),
     :                    %VAL( CNF_PVAL( IPYI ) ), NPOSI, VERBOS,
     :                    PERCNT, ZOOM, MAXCNV, WINDIM, MSTYLE, CENTRD,
     :                    IPIO, IPXO, IPYO, NPOSO, STATUS )

*  If requested to do so, write the returned positions to an output
*  position list file.
         IF ( WRLIST ) THEN
            IF ( NMEM .EQ. 1 ) THEN

*  Open the output position list file.
               CALL GRP_GET( OLSTGR, IMEM( IMEMOF( IS ) ), 1, FNAME,
     :                       STATUS )
               CALL CCD1_OPFIO( FNAME, 'WRITE', 'LIST', 0, FD, STATUS )

*  Write a header to the file.
               CALL CCD1_FIOHD( FD, 'Output from IDICURS', STATUS )

*  Write the positions to the output file.
               CALL CCD1_WRIXY( FD, %VAL( CNF_PVAL( IPIO ) ),
     :                          %VAL( CNF_PVAL( IPXO ) ),
     :                          %VAL( CNF_PVAL( IPYO ) ), NPOSO, LINE,
     :                          CCD1__BLEN, STATUS )

*  Close the output file.
               CALL FIO_CLOSE( FD, STATUS )

*  Report file used and number of entries.
               CALL CCD1_MSG( ' ', ' ', STATUS )
               CALL MSG_SETC( 'FNAME', FNAME )
               CALL MSG_SETI( 'NPOS', NPOSO )
               CALL CCD1_MSG( ' ',
     :         '    Wrote ^NPOS positions to file ^FNAME', STATUS )

*  Modify the CURRENT_LIST item of the CCDPACK extension accordingly.
               CALL CCG1_STO0C( INDF( 1 ), 'CURRENT_LIST', FNAME,
     :                          STATUS )

*  Multiple members of this Set.  In this case we have to transform
*  back from CCD_SET coordinates, and only write the appropriate
*  points into each position list.
            ELSE

*  Loop over each NDF in this Set.
               DO I = 1, NMEM
                  INDEX = IMEM( IMEMOF( IS ) + I - 1 )

*  Open the position list file.
                  CALL GRP_GET( OLSTGR, INDEX, 1, FNAME, STATUS )
                  CALL CCD1_OPFIO( FNAME, 'WRITE', 'LIST', 0, FD,
     :                             STATUS )

*  Write a header to the file.
                  CALL CCD1_FIOHD( FD, 'Output from IDICURS', STATUS )

*  Allocate enough space to hold transformed coordinates, and a
*  subset of this Set's position list.
                  NPO = 0
                  IF ( NPOSO .GT. 0 ) THEN
                     CALL CCD1_MALL( NPOSO, '_DOUBLE', IPX1, STATUS )
                     CALL CCD1_MALL( NPOSO, '_DOUBLE', IPY1, STATUS )
                     CALL CCD1_MALL( NPOSO, '_DOUBLE', IPX2, STATUS )
                     CALL CCD1_MALL( NPOSO, '_DOUBLE', IPY2, STATUS )
                     CALL CCD1_MALL( NPOSO, '_INTEGER', IPI2, STATUS )

*  Transform the positions into pixel coordinates.
                     CALL AST_TRAN2( MAP( I ), NPOSO,
     :                               %VAL( CNF_PVAL( IPXO ) ),
     :                               %VAL( CNF_PVAL( IPYO ) ), .FALSE.,
     :                               %VAL( CNF_PVAL( IPX1 ) ),
     :                               %VAL( CNF_PVAL( IPY1 ) ),
     :                               STATUS )

*  Get the NDF's bounds.
                     CALL NDF_BOUND( INDF( I ), 2, LBND, UBND, NDIM,
     :                               STATUS )
                     XLO = DBLE( LBND( 1 ) - 1 )
                     YLO = DBLE( LBND( 2 ) - 1 )
                     XHI = DBLE( UBND( 1 ) )
                     YHI = DBLE( UBND( 2 ) )

*  Select only the points in this position list which fall within the
*  bounds of this NDF.
                     CALL CCD1_CHUSB( %VAL( CNF_PVAL( IPIO ) ),
     :                                %VAL( CNF_PVAL( IPX1 ) ),
     :                                %VAL( CNF_PVAL( IPY1 ) ),
     :                                NPOSO, XLO, XHI, YLO, YHI,
     :                                %VAL( CNF_PVAL( IPI2 ) ),
     :                                %VAL( CNF_PVAL( IPX2 ) ),
     :                                %VAL( CNF_PVAL( IPY2 ) ),
     :                                NPO, STATUS )

*  Write the positions out.
                     CALL CCD1_WRIXY( FD, %VAL( CNF_PVAL( IPI2 ) ),
     :                                %VAL( CNF_PVAL( IPX2 ) ),
     :                                %VAL( CNF_PVAL( IPY2 ) ),
     :                                NPO, LINE,
     :                                CCD1__BLEN, STATUS )

*  Free memory.
                     CALL CCD1_MFREE( IPX1, STATUS )
                     CALL CCD1_MFREE( IPY1, STATUS )
                     CALL CCD1_MFREE( IPX2, STATUS )
                     CALL CCD1_MFREE( IPY2, STATUS )
                     CALL CCD1_MFREE( IPI2, STATUS )
                  END IF

*  Close the output file.
                  CALL FIO_CLOSE( FD, STATUS )

*  Report file used and number of entries.
                  CALL MSG_SETC( 'FNAME', FNAME )
                  CALL MSG_SETI( 'NPOS', NPO )
                  CALL CCD1_MSG( ' ',
     :'    Wrote ^NPOS positions to file ^FNAME', STATUS )

*  Modify the CURRENT_LIST item of the CCDPACK extension accordingly.
                  CALL CCG1_STO0C( INDF( I ), 'CURRENT_LIST',
     :                             FNAME, STATUS )
               END DO
            END IF
         END IF

*  Release NDFs.
         DO I = 1, NMEM
            CALL NDF_ANNUL( INDF( I ), STATUS )
         END DO

*  Free memory.
         IF ( NPOSI .GT. 0 ) THEN
            CALL CCD1_MFREE( IPXI, STATUS )
            CALL CCD1_MFREE( IPYI, STATUS )
            CALL CCD1_MFREE( IPII, STATUS )
         END IF
         IF ( NPOSO .GT. 0 ) THEN
            CALL CCD1_MFREE( IPXO, STATUS )
            CALL CCD1_MFREE( IPYO, STATUS )
            CALL CCD1_MFREE( IPIO, STATUS )
         END IF
      END DO

*  Write display preference parameters back to the parameter system.
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      CALL PAR_PUT0D( 'ZOOM', ZOOM, STATUS )
      CALL PAR_PUT0I( 'MAXCANV', MAXCNV, STATUS )
      CALL PAR_PUT0I( 'WINX', WINDIM( 1 ), STATUS )
      CALL PAR_PUT0I( 'WINY', WINDIM( 2 ), STATUS )
      CALL PAR_PUT0C( 'MARKSTYLE', MSTYLE, STATUS )
      CALL PAR_PUT0L( 'CENTROID', CENTRD, STATUS )
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
