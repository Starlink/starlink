      SUBROUTINE CCDALIGN( PID, STATUS )
*+
*  Name:
*     CCDALIGN

*  Purpose:
*     Aligns images graphically by interactive object selection.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CCDALIGN( PID, STATUS )

*  Arguments:
*     PID = CHARACTER * ( * ) (Given)
*        A string to append to any task names used in this routine.
*        (this should be setup to allow attachment to existing monoliths).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This program aids the registration of NDFs which may not be
*     related by simple offsets (see FINDOFF and PAIRNDF if they are).
*     It also has the capability of dealing with groups of NDFs which
*     are almost registered (frames which have not been moved on the
*     sky) saving effort in re-identification of image features.
*
*     The basic method used is to supply a list of NDFs and an
*     optional reference NDF.  The first NDF or the reference
*     NDF is initially displayed and you are invited to mark
*     the positions of centroidable image features on it using a
*     graphical interface.  This window then remains on the screen for
*     reference while you identify the same features on each of the
*     other images in the same way.
*
*     After centroiding you are then given the option to stop. If
*     you decide to, then you will have labelled position lists to use
*     in the other CCDPACK routines (the labelled positions will be
*     called NDF_NAME.acc). If you choose the option to continue then
*     a full registration of the NDFs will be attempted. This may only
*     be performed for 'linear' transformations.
*
*     After choosing a transformation type the procedure will then go on
*     to calculate a transformation set between all the NDFs; this is
*     used (with the extended reference set from REGISTER) to
*     approximate the position of all possible image features, which are
*     then located by centroiding and a final registration of all NDFs
*     is performed.  The resultant NDFs then have associated lists of
*     labelled positions, and attached coordinate systems which may be
*     used to transform other position lists or when resampling the data.
*
*     If the EXTRAS parameter is true you may also enter, for each of
*     the original images, a group of images which is almost registered
*     with it (within the capabilities of centroiding, i.e. a few pixels).
*     In this way similar registration processes can be performed
*     on many almost-aligned images without additional work from the
*     user.
*
*     The graphical interface used for marking features on the image
*     should be fairly self-explanatory.  The image can be scrolled using
*     the scrollbars, the window can be resized, and there are controls
*     for zooming the image in or out, changing the style of display and
*     altering the percentile cutoff limits.  The displayed index numbers
*     of any identified features on each image must match those on the
*     reference image (though it is not necessary to identify all of
*     the features from the reference image on each one), and there is
*     also a control for selecting the number of the next point to mark.
*     Points are added by clicking mouse button 1 (usually the left one)
*     and may be removed by clicking mouse button 3 (usually the right
*     one).  It is possible to edit the points marked on the reference
*     image while you are marking points on the other images.  When
*     you have selected all the points you wish to on a given image,
*     click the 'Done' button and you will be presented with the next
*     one.

*  Usage:
*     ccdalign in

*  ADAM Parameters:
*     CONTINUE = _LOGICAL (Read)
*        If TRUE then this command will proceed to also work
*        out the registrations of your images. Note that this is
*        only possible if you are intending to use linear
*        transformations (this is the usual case).
*        [FALSE]
*     EXTRAS = _LOGICAL (Read)
*        If this parameter is true, then for each NDF (or Set of
*        NDFs, if USESET is true) from the IN list you will be
*        prompted to enter a group of corresponding names which
*        represent more files of the same type pointing at (almost)
*        the same sky position as the one in the IN list.  CCDALIGN
*        will then centroid the marked objects in all the images
*        in the same group so that multiple similar registrations
*        can be done at the same time.
*        [FALSE]
*     FITTYPE = _INTEGER (Read)
*        The type of fit which should be used when determining the
*        transformation between the input positions lists. This may take
*        the values
*           - 1 -- shift of origin
*           - 2 -- shift of origin and rotation
*           - 3 -- shift of origin and magnification
*           - 4 -- shift of origin, rotation and magnification (solid body)
*           - 5 -- a full six parameter fit
*           - 6 -- self defined function
*
*        [5]
*     IN = LITERAL (Read)
*        A list of the NDFs to be displayed in the GUI for interactive
*        marking of features.  The names should be separated by commas
*        and may include wildcards.
*     LOGFILE = FILENAME (Read)
*        Name of the CCDPACK logfile.  If a null (!) value is given for
*        this parameter then no logfile will be written, regardless of
*        the value of the LOGTO parameter.
*
*        If the logging system has been initialised using CCDSETUP
*        then the value specified there will be used. Otherwise, the
*        default is 'CCDPACK.LOG'.
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
*        default is 'BOTH'.
*        [BOTH]
*     MARKSTYLE = LITERAL (Read and Write)
*        A string indicating how markers are initially to be plotted on
*        the image.  It consists of a comma-separated list of
*        "attribute=value" type strings.  The available attributes are:
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
*     MORE = LITERAL (Read)
*        If EXTRAS is true, this parameter is used to get a list of
*        images corresponding to each one which is named by the IN
*        parameter.  These lists are always got interactively; MORE
*        values cannot be given on the command line.  For any given
*        response the null value (!) may be supplied, indicating that
*        there are no similarly aligned images.  If the original image
*        is included again in the supplied MORE value, it will be
*        ignored, since it already forms part of the group being
*        considered.
*        [!]
*     PERCENTILES( 2 ) = _DOUBLE (Read)
*        The initial low and high percentiles of the data range to use
*        when displaying the images; any pixels with a value lower than
*        the first element will have the same colour, and any with a value
*        higher than the second will have the same colour.  Must be in
*        the range 0 <= PERCENTILES( 1 ) <= PERCENTILES( 2 ) <= 100.
*        This can be changed from within the GUI.
*        [2,98]
*     REFNDF = LITERAL (Read)
*        The name of an additional reference image (or Set); this is the
*        first image displayed and the one which will be visible while
*        you are marking points on all the others.  If the null value
*        (!) is supplied then no additional reference image will be
*        used, and the first one in the IN list will be the first
*        displayed.
*        [!]
*     USESET = _LOGICAL (Read)
*        This parameter determines whether Set header information will
*        be used.  If USESET is true, then CCDALIGN will try to
*        group images according to their Set Name attribute before
*        displaying them, rather than treating them one by one.
*        All images in the IN list which share the same (non-blank)
*        Set Name attribute, and which have a CCD_SET attached
*        coordinate system, will be shown together as a single
*        image in the viewer for object marking, plotted in their
*        CCD_SET coordinates.
*
*        If USESET is false, then regardless of Set headers, each
*        individual NDF will be displayed for marking separately.
*        If the input images have no Set headers, or if they have
*        no CCD_SET coordinates in their WCS components, the value
*        of this parameter will make no difference.
*
*        If a global value for this parameter has been set using
*        CCDSETUP than that value will be used.
*        [FALSE]
*     WINX = INTEGER (Read and Write)
*        The width in pixels of the window to display the image and
*        associated controls in.  If the image is larger than the area
*        allocated for display, it can be scrolled around within the
*        window.  The window can be resized in the normal way using
*        the window manager while the program is running.
*        [450]
*     WINY = INTEGER (Read and Write)
*        The height in pixels of the window to display the image and
*        associated controls in.  If the image is larger than the area
*        allocated for display, it can be scrolled around within the
*        window.  The window can be resized in the normal way using
*        the window manager while the program is running.
*        [600]
*     ZOOM = DOUBLE (Read and Write)
*        A factor giving the initial level to zoom in to the image
*        displayed, that is the number of screen pixels to use for one
*        image pixel.  It will be rounded to one of the values
*        ... 3, 2, 1, 1/2, 1/3 ....  The zoom can be changed
*        interactively from within the program.  The initial value
*        may be limited by MAXCANV.
*        [1]

*  Examples:
*     ccdalign * continue=no
*        This will display all the images in the current directory and
*        invite you to mark corresponding image features on each one
*        in turn.  When you have done this, the centroids will be
*        calculated and you will be left with a position list with
*        the extension `.acc' associated with each one.
*
*     ccdalign "x1008,x1009,x1010" refndf=xmos extras=yes continue
*        Here the EXTRAS parameter is true, so for each of the named
*        images you will be prompted for a list of other images
*        which were taken pointing in the same direction.
*        The file `xmos' is being used as the reference image,
*        so that will be presented first for marking features.
*        When you have marked features on all four images, the
*        program will go on to match them all up and produce a
*        global registration, attaching a new coordinate system in
*        which they are all registered to each file.

*  Behaviour of Parameters:
*     All parameters retain their current value as default. The
*     'current' value is the value assigned on the last run of the
*     application. If the application has not been run then the
*     'intrinsic' defaults, as shown in the parameter help, apply.
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

*  Copyright:
*     Copyright (C) 1997-2001 Central Laboratory of the Research
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
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-MAY-1997 (PDRAPER):
*        Original version.
*     16-OCT-1998 (PDRAPER):
*        Added call to SLV_RESET to work around problems with
*        KAPPA:DISPLAY dynamic parameters (X/YMAGN and CENTRE) not
*        updating. This is a problem when using images of differing
*        sizes.
*     1-APR-1999 (MBT):
*        Modified to use WCS components.
*     19-MAY-2000 (MBT):
*        Added a call to IDI_ASSOC to ensure that no attempt is made to
*        use an unsupported visual.
*     25-AUG-2000 (MBT):
*        Removed the above IDI_ASSOC call since it causes obscure
*        problems on alpha_osf1 and sun4_solaris.
*     29-AUG-2000 (MBT):
*        Replaced use of CCDNDFAC A-task/routine, to try to fix weird
*        platform-dependent parameter-related problems, with the normal
*        routine CCD1_NGLIS.  Turned out to be an NDG bug, but it's
*        cleaner this way anyway.
*     11-OCT-2000 (MBT):
*        Rewrote using Tcl instead of IDI.
*     10-NOV-2000 (MBT):
*        Fixed a serious bug present since the WCS upgrade in 1999 which
*        meant that it simply didn't do the alignment properly, because
*        it was getting confused about what position lists were in
*        what coordinates.
*     5-APR-2001 (MBT):
*        Changed the parameter usage around quite a bit so that EXTRAS
*        and MORE are used rather than overloading the IN parameter for
*        everything which limits the potential use of wildcards.
*        Also upgraded for Sets.
*     22-MAY-2001 (MBT):
*        Changed so that an empty position list file is written instead
*        of no list file at all.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'FIO_ERR'         ! FIO error codes
      INCLUDE 'FIO_PAR'         ! FIO system constants
      INCLUDE 'GRP_PAR'         ! Standard GRP system constants
      INCLUDE 'DAT_PAR'         ! Standard HDS constants
      INCLUDE 'AST_PAR'         ! AST system declarations
      INCLUDE 'PSX_ERR'         ! PSX error codes
      INCLUDE 'PAR_ERR'         ! PAR system error codes
      INCLUDE 'CCD1_PAR'        ! CCDPACK private constants
      INCLUDE 'CNF_PAR'         ! For CNF_PVAL function

*  Arguments Given
      CHARACTER * ( * ) PID     ! Parent process id as string

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      EXTERNAL SLV_LOADW
      INTEGER SLV_LOADW

*  Local Constants:
      INTEGER TIMOUT
      PARAMETER ( TIMOUT = 30 ) ! Timeout when loading tasks
      INTEGER MAXGRP
      PARAMETER ( MAXGRP = 20 ) ! Maximum number of NDF groups for alignment

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN           ! Used length of string

*  Local Variables:
      CHARACTER * ( 1024 ) LINE ! Output message line (long)
      CHARACTER * ( 256 ) CMD   ! Command string
      CHARACTER * ( 132 ) MSTYLE ! Marker style string
      CHARACTER * ( 5 ) NULL    ! Parameter NULL symbol
      CHARACTER * ( 30 ) LISTID ! Identifier in filename for list
      CHARACTER * ( GRP__SZNAM ) SNAME ! Name of Set
      CHARACTER * ( GRP__SZNAM ) NDFNAM ! Name of NDF
      CHARACTER * ( GRP__SZNAM ) REGNAM ! Name of reference NDF for REGISTER
      CHARACTER * ( GRP__SZNAM ) NDFNMS( MAXGRP ) ! Names of NDFs
      CHARACTER * ( FIO__SZFNM ) FNAME ! Name of position list file
      CHARACTER * ( FIO__SZFNM ) NAMLST ! File name list
      CHARACTER * ( 30 ) CCDREG ! Message system name for ccdpack_reg
      INTEGER CCDGID            ! Id for CCDPACK_REG monolith
      INTEGER FD                ! File identifier
      INTEGER FDOUT             ! File identifier
      INTEGER FITTYP            ! Transformation type
      INTEGER GRGR( CCD1__MXNDF ) ! GRP identifiers for NDFs in each group
      INTEGER I                 ! Loop variable
      INTEGER INDF              ! NDF identifier
      INTEGER IPIND( CCD1__MXNDF ) ! Pointers to identifiers for list positions
      INTEGER IPI2              ! Pointer to subset of list position ids
      INTEGER IPXPOS( CCD1__MXNDF ) ! Pointers to X list pos coordinates
      INTEGER IPX1              ! Pointer to transformed list position X coords
      INTEGER IPX2              ! Pointer to subset of list position X coords
      INTEGER IPYPOS( CCD1__MXNDF ) ! Pointer to Y list pos coordinates
      INTEGER IPY1              ! Pointer to transformed list position Y coords
      INTEGER IPY2              ! Pointer to subset of list position X coords
      INTEGER ISET( CCD1__MXNDF ) ! Set membership of group leader NDFs
      INTEGER IWCS              ! Pointer to WCS frameset
      INTEGER J                 ! Loop variable
      INTEGER JPIX              ! Frame index of PIXEL frame
      INTEGER JSET              ! Frame index of CCD_SET frame
      INTEGER K                 ! Loop variable
      INTEGER LBND( 2 )         ! Lower bounds of NDF
      INTEGER LDRGR             ! GRP identifier for group leader NDFs
      INTEGER LENG              ! Returned length of string
      INTEGER LMEM( CCD1__MXNDF ) ! Leader NDF indices in Set order
      INTEGER LMEMOF( CCD1__MXNDF + 1 ) ! Pointers into LMEM
      INTEGER MAP               ! Pointer to AST mapping
      INTEGER MAPSET( CCD1__MXNDF ) ! Workspace
      INTEGER MAXCNV            ! Initial maximum dimension of display region
      INTEGER MORGR             ! GRP identifier for more NDF names
      INTEGER NDFLEN            ! length of NDF name
      INTEGER NDIM              ! Number of NDF dimensions
      INTEGER NGOT              ! Number of NDFs already got
      INTEGER NGR               ! Number of NDFs in a group
      INTEGER NL                ! Number in current leader group
      INTEGER NLDR              ! Number of NDFs in group leader group
      INTEGER NMEM( CCD1__MXNDF ) ! Number of members in each Set
      INTEGER NMOR              ! Number of extra NDFs in group
      INTEGER NNDF              ! Number of NDFs passed to GUI script
      INTEGER NPOINT( MAXGRP )  ! Number of positions marked for each NDF
      INTEGER NREF              ! Number of reference NDFs
      INTEGER NSET              ! Number of Sets represented in input
      INTEGER NTRY              ! Number of tries at getting variable
      INTEGER N2                ! Number of points in position list subset
      INTEGER OPLEN             ! String length
      INTEGER REFGR             ! GRP identifier for reference NDF names
      INTEGER REFSET            ! Position of the reference Set in the list
      INTEGER RISET( CCD1__MXNDF ) ! Set membership of group leader ref NDF
      INTEGER RMEM( CCD1__MXNDF ) ! Reference NDF indices in Set order
      INTEGER RMEMOF( CCD1__MXNDF + 1 ) ! Pointers into RMEM
      INTEGER RNAMGR            ! Reference NDF Set name group
      INTEGER RNSET             ! Number of reference NDF Sets
      INTEGER SNAMGR            ! Set Name group
      INTEGER UBND( 2 )         ! Upper bounds of NDF
      INTEGER WINDIM( 2 )       ! Window dimensions for display
      LOGICAL CONT              ! Continue processing
      LOGICAL DONE              ! Has job already been done?
      LOGICAL EXTRAS            ! Whether to solicit more NDF names per group
      LOGICAL HAVREF            ! Have a reference NDF
      LOGICAL US                ! Temporary USESET variable
      LOGICAL USESET            ! Use Set headers if available?
      DOUBLE PRECISION PERCNT( 2 ) ! The display percentiles
      DOUBLE PRECISION XLO      ! Lower bound of pixel X coordinates
      DOUBLE PRECISION XHI      ! Upper bound of pixel X coordinates
      DOUBLE PRECISION YLO      ! Lower bound of pixel Y coordinates
      DOUBLE PRECISION YHI      ! Upper bound of pixel Y coordinates
      DOUBLE PRECISION ZOOM     ! Zoom factor for display

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise some GRP identifiers to harmless values.
      SNAMGR = GRP__NOID
      RNAMGR = GRP__NOID
      LDRGR = GRP__NOID
      REFGR = GRP__NOID

*  Find out the environment we're running under. If it's IRAF then
*  INDEF is used instead of ! as a NULL symbol.
      CALL CCD1_SETEX( NULL, NL, STATUS )
      CCDGID = -1

*  Start the application and introduce ourselves.
      CALL CCD1_START( 'CCDALIGN', STATUS )
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ',
     :     '  An interactive aid for aligning groups of NDFs.', STATUS )
      CALL CCD1_MSG( ' ', ' ', STATUS )

*  Get a default percentile range for displaying the images.
      CALL PAR_EXACD( 'PERCENTILES', 2, PERCNT, STATUS )

*  Get a list of NDF group leaders.
      CALL CCD1_NDFGL( 'IN', 2, CCD1__MXNDF - 1, LDRGR, NLDR, STATUS )

*  See whether we are using Set headers.
      CALL PAR_GET0L( 'USESET', USESET, STATUS )

*  Sort the group leaders according to Set membership.
      CALL CCD1_SETSW( LDRGR, NLDR, USESET, ISET, NSET, LMEM, LMEMOF,
     :                 SNAMGR, MAPSET, STATUS )

*  Check that we do not have too many groups.
      IF ( NSET .GT. MAXGRP ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NUM', NSET )
         CALL MSG_SETI( 'MAX', MAXGRP )
         CALL ERR_REP( 'CCDALIGN_TOOMANY',
     :'CCDALIGN: ^NUM supplied groups exceeds maximum (^MAX)', STATUS )
         GO TO 99
      END IF

*  See whether we will solicit extra members of each group apart from the
*  leaders.
      CALL PAR_GET0L( 'EXTRAS', EXTRAS, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Write the names to lists.
      DO I = 1, NSET

*  Open a list file to contain the names of the NDFs in this group.
         CALL MSG_SETI( 'NGROUP', I )
         CALL MSG_LOAD( ' ', 'ccdalign_ndf^NGROUP.list', NAMLST,
     :                  OPLEN, STATUS )
         CALL CCD1_OPFIO( NAMLST, 'WRITE', 'LIST', 0, FD, STATUS )

*  Create a GRP group to store the members of this group of NDFs.
         CALL GRP_NEW( 'NDFs', GRGR( I ), STATUS )

*  Store the names of the group leader Set.
         NL = 0
         DO J = LMEMOF( I ), LMEMOF( I + 1 ) - 1
            NL = NL + 1
            CALL GRP_GET( LDRGR, LMEM( J ), 1, NDFNMS( NL ), STATUS )
         END DO

*  Write the names for the group leader Set to the file and store in
*  a group.
         DO J = 1, NL
            NDFNAM = NDFNMS( J )
            CALL FIO_WRITE( FD, NDFNAM( : CHR_LEN( NDFNAM ) ), STATUS )
            CALL GRP_PUT( GRGR( I ), 1, NDFNAM, 0, STATUS )
         END DO

*  If requested, get the extra members of the group.
         IF ( EXTRAS ) THEN

*  Inform the user what images the inputted NDFs should correspond to.
            CALL MSG_OUT( ' ', ' ', STATUS )
            IF ( USESET ) THEN

*  If we're using Sets, output the name of the Set and of the constituent
*  NDFs for the group leader.
               CALL GRP_GET( SNAMGR, I, 1, SNAME, STATUS )
               CALL MSG_SETC( 'SETNAM', SNAME )
               CALL MSG_OUT( ' ',
     :'Enter extra NDFs in the same sky position as Set "^SETNAM":',
     :                       STATUS )
               LINE = 'Member NDFs are:'
               DO J = 1, NL
                  LINE( 18: ) = NDFNMS( J )
                  CALL MSG_SETC( 'LINE', LINE )
                  CALL CCD1_MSG( ' ', '^LINE', STATUS )
                  LINE = ' '
               END DO
            ELSE

*  If not using Sets, just output the name of the group leader NDF.
               CALL GRP_GET( LDRGR, I, 1, NDFNAM, STATUS )
               CALL MSG_SETC( 'NAME', NDFNAM )
               CALL MSG_OUT( ' ',
     :'Enter extra NDFs in the same sky position as NDF "^NAME"',
     :                       STATUS )
            END IF

*  Now get the user to input extra NDFs for the group via the MORE
*  parameter.
            CALL PAR_CANCL( 'MORE', STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 99
            CALL CCD1_NDFGL( 'MORE', 0, CCD1__MXNDF - NL, MORGR, NMOR,
     :                       STATUS )

*  If a NULL response was given, annul the error and assume that there
*  are no extra NDFs in this group.
            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )
               NMOR = 0
            ELSE

*  If we do have extra NDFs, write them out to the list file and store
*  in the group.  If any are the same as group leader NDFs, don't
*  bother to write them again.
               DO J = 1, NMOR
                  CALL GRP_GET( MORGR, J, 1, NDFNAM, STATUS )
                  DONE = .FALSE.
                  DO K = 1, NL
                     DONE = DONE .OR. NDFNAM .EQ. NDFNMS( K )
                  END DO
                  IF ( .NOT. DONE ) THEN
                     CALL FIO_WRITE( FD, NDFNAM( : CHR_LEN( NDFNAM ) ),
     :                               STATUS )
                     CALL GRP_PUT( GRGR( I ), 1, NDFNAM, 0, STATUS )
                  END IF
               END DO
            END IF

*  Annul the group used to get the values from the MORE parameter.
            CALL CCD1_GRDEL( MORGR, STATUS )
         END IF

*  Close the list file.
         CALL FIO_CLOSE( FD, STATUS )
      END DO

*  See if the user is supplying a reference NDF.
      HAVREF = .FALSE.
      IF ( USESET ) THEN

*  If using Sets, then ensure that the user only enters members of a
*  single Set for the reference NDF.
         NTRY = 0
 4       CONTINUE
         CALL CCD1_NDFGL( 'REFNDF', 0, CCD1__MXNDF - NLDR, REFGR, NREF,
     :                    STATUS )
         US = .TRUE.
         RNSET = 0
         CALL CCD1_SETSW( REFGR, NREF, US, RISET, RNSET, RMEM, RMEMOF,
     :                    RNAMGR, MAPSET, STATUS )
         IF ( STATUS .EQ. SAI__OK .AND. RNSET .NE. 1 ) THEN
            NTRY = NTRY + 1
            IF ( NTRY .LT. 5 ) THEN
               CALL CCD1_MSG( ' ',
     :'The REFNDF parameter must only comprise members of a single Set',
     :                        STATUS )
               CALL PAR_CANCL( 'REFNDF', STATUS )
               GO TO 4
            ELSE
               STATUS = SAI__ERROR
               CALL ERR_REP( 'CCDALIGN_REFNDF',
     :'CCDALIGN: Failed to get value for REFNDF parameter', STATUS )
               GO TO 99
            END IF
         END IF
      ELSE

*  If not using Sets, get at most a single NDF for the reference NDF.
         CALL CCD1_NDFGL( 'REFNDF', 0, 1, REFGR, NREF, STATUS )
      END IF

*  See if we have a reference NDF or not.  A NULL response is OK, annul
*  the error and take it as having no reference NDF.
      IF ( STATUS .EQ. PAR__NULL ) THEN
         HAVREF = .FALSE.
         CALL ERR_ANNUL( STATUS )
      ELSE IF ( STATUS .EQ. SAI__OK .AND. NREF .GE. 1 ) THEN
         HAVREF = .TRUE.

*  I don't think this can happen, but if it does then bail out.
      ELSE
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCDALIGN_BADREF',
     :                 'CCDALIGN: Error getting REFNDF value', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  If there is a reference image group, write its name(s) to a file.
      IF ( HAVREF ) THEN

*  Open a list file to hold the name(s) of the reference image(s).
         CALL CCD1_OPFIO( 'ccdalign_ref.list', 'WRITE', 'LIST', 0, FD,
     :                    STATUS )

*  Create a GRP group to store the members of the reference group of NDFs.
         CALL GRP_NEW( 'Reference NDFs', GRGR( NSET + 1 ), STATUS )

*  Store the names of the reference leader Set.
         DO I = 1, NREF
            CALL GRP_GET( REFGR, I, 1, NDFNMS( I ), STATUS )
         END DO

*  Write the names for the group leader Set to the list file and store
*  in a group.
         DO I = 1, NREF
            NDFNAM = NDFNMS( I )
            CALL FIO_WRITE( FD, NDFNAM( : CHR_LEN( NDFNAM ) ), STATUS )
            CALL GRP_PUT( GRGR( NSET + 1 ), 1, NDFNAM, 0, STATUS )
         END DO

*  Close the list file.
         CALL FIO_CLOSE( FD, STATUS )
      END IF

*  Make sure we have all the NDFs to pass to the GUI routine.
      NNDF = NLDR

*  If there is a reference NDF or Set, then add it to the end of the list
*  of leaders, ensuring that all the variables keeping track of Set
*  ordering are updated accordingly.
      IF ( HAVREF ) THEN
         NSET = NSET + 1
         IF ( USESET ) THEN
            CALL GRP_GET( RNAMGR, 1, 1, SNAME, STATUS )
         ELSE
            SNAME = ' '
         END IF
         CALL GRP_PUT( SNAMGR, 1, SNAME, NSET, STATUS )
         DO I = 1, NREF
            NNDF = NNDF + 1
            ISET( NNDF ) = NSET
            LMEM( NNDF ) = NNDF
            CALL GRP_GET( REFGR, I, 1, NDFNAM, STATUS )
            CALL GRP_PUT( LDRGR, 1, NDFNAM, NNDF, STATUS )
         END DO
         LMEMOF( NSET + 1 ) = NNDF + 1
         REFSET = NSET

*  Otherwise, set the reference Set to that corresponding to the first
*  one in the list.
      ELSE
         REFSET = ISET( 1 )
      END IF

*  Calculate the number of members in each Set.
      DO I = 1, NSET
         NMEM( I ) = LMEMOF( I + 1 ) - LMEMOF( I )
      END DO

*  Get values of display preferences from parameter system.
      CALL PAR_GET0D( 'ZOOM', ZOOM, STATUS )
      CALL PAR_GET0I( 'MAXCANV', MAXCNV, STATUS )
      CALL PAR_GET0I( 'WINX', WINDIM( 1 ), STATUS )
      CALL PAR_GET0I( 'WINY', WINDIM( 2 ), STATUS )
      CALL PAR_GET0C( 'MARKSTYLE', MSTYLE, STATUS )

*  Call the routine which will do the graphical user interaction to
*  obtain the position lists for each group of NDFs.
      CALL CCD1_ALGN( LDRGR, NNDF, NSET, LMEM, LMEMOF, SNAMGR,
     :                REFSET, PERCNT, ZOOM, MAXCNV, WINDIM, MSTYLE,
     :                NPOINT, IPXPOS, IPYPOS, IPIND, STATUS )

*  Write display preference parameters back to the parameter system.
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      CALL PAR_PUT0D( 'ZOOM', ZOOM, STATUS )
      CALL PAR_PUT0I( 'MAXCANV', MAXCNV, STATUS )
      CALL PAR_PUT0I( 'WINX', WINDIM( 1 ), STATUS )
      CALL PAR_PUT0I( 'WINY', WINDIM( 2 ), STATUS )
      CALL PAR_PUT1D( 'PERCENTILES', 2, PERCNT, STATUS )
      CALL PAR_PUT0C( 'MARKSTYLE', MSTYLE, STATUS )

*  We are no longer interested in the reference NDF.
      IF ( HAVREF ) NSET = NSET - 1

*  Start up the CCDPACK_REG monolith which is used later in this task.
      CALL PSX_GETENV( 'CCDPACK_DIR', CMD, STATUS )
      CMD = CMD( :CHR_LEN( CMD ) )//'/ccdpack_reg'
      CCDREG = 'ccdpack_reg'//PID
      CCDGID = SLV_LOADW( CCDREG, CMD, .TRUE., TIMOUT, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'FAILED',
     :      'Sorry cannot proceed. Failed to load monolith '//
     :      '$CCDPACK_DIR/ccdpack_reg', STATUS )
         GO TO 99
      END IF

*  Now write the position lists corresponding to the marked images,
*  and associate the appropriate NDFs with those lists.
      DO I = 1, NSET

*  Get the name of the position list file and of the file containing
*  all the NDFs in this group.
         CALL MSG_SETI( 'LISTID', I )
         CALL MSG_LOAD( ' ', 'ndf^LISTID', LISTID, LENG, STATUS )
         NAMLST = 'ccdalign_' // LISTID( :LENG ) // '.list'

*  If this group does not represent a Set (i.e. it has only one member),
*  then assume that the same position list applies to all the NDFs in
*  the group.  The list will already be in Pixel coordinates.
         IF ( NMEM( I ) .EQ. 1 ) THEN

*  Open the position list file.
            FNAME = 'ccdalign_' // LISTID( :LENG ) // '.fea'
            CALL CCD1_OPFIO( FNAME, 'WRITE', 'LIST', 0, FD, STATUS )

*  Write a header.
            CALL CCD1_FIOHD( FD, 'Output from CCDALIGN', STATUS )

*  Write all the points in the position list to the file.
            CALL CCD1_WRIXY( FD, %VAL( CNF_PVAL( IPIND( I ) ) ),
     :                       %VAL( CNF_PVAL( IPXPOS( I ) ) ),
     :                       %VAL( CNF_PVAL( IPYPOS( I ) ) ),
     :                       NPOINT( I ), LINE, 1024, STATUS )

*  Close the position list file.
            CALL FIO_CLOSE( FD, STATUS )

*  Associate the position list file with all the NDFs in this group.
            CMD = 'logto=n ' //
     :            'mode=alist ' //
     :            'in=^' // NAMLST( :CHR_LEN( NAMLST ) ) // ' ' //
     :            'inlist=' // FNAME( :CHR_LEN( FNAME ) ) // ' accept'
            CALL SLV_OBEYW( CCDREG, 'ccdedit', CMD, ' ', STATUS )

*  If this is a Set, we have to be more careful.  For each NDF in
*  the group, construct an individual list file consisting of all the
*  points in the Set list which fall within the boundaries of the NDF
*  (since any given point may come from a different Set member of the
*  same Set).  In this case, the returned points will be in CCD_SET
*  coordinates.
         ELSE

*  Allocate enough space to hold transformed coordinates, and a subset
*  of this Set's position list.
            CALL CCD1_MALL( NPOINT( I ), '_DOUBLE', IPX1, STATUS )
            CALL CCD1_MALL( NPOINT( I ), '_DOUBLE', IPY1, STATUS )
            CALL CCD1_MALL( NPOINT( I ), '_DOUBLE', IPX2, STATUS )
            CALL CCD1_MALL( NPOINT( I ), '_DOUBLE', IPY2, STATUS )
            CALL CCD1_MALL( NPOINT( I ), '_INTEGER', IPI2, STATUS )

*  Loop over each NDF in the group.
            CALL GRP_GRPSZ( GRGR( I ), NGR, STATUS )
            DO J = 1, NGR

*  Get its NDF identifier.
               CALL NDG_NDFAS( GRGR( I ), J, 'UPDATE', INDF, STATUS )

*  Get the mapping from CCD_SET to PIXEL coordinates.
               CALL CCD1_GTWCS( INDF, IWCS, STATUS )
               CALL CCD1_FRDM( IWCS, 'CCD_SET', JSET, STATUS )
               CALL CCD1_FRDM( IWCS, 'PIXEL', JPIX, STATUS )
               MAP = AST_GETMAPPING( IWCS, JSET, JPIX, STATUS )

*  Transform the positions into pixel coordinates.
               CALL AST_TRAN2( MAP, NPOINT( I ),
     :                         %VAL( CNF_PVAL( IPXPOS( I ) ) ),
     :                         %VAL( CNF_PVAL( IPYPOS( I ) ) ), .TRUE.,
     :                         %VAL( CNF_PVAL( IPX1 ) ),
     :                         %VAL( CNF_PVAL( IPY1 ) ), STATUS )

*  Get its bounds.
               CALL NDF_BOUND( INDF, 2, LBND, UBND, NDIM, STATUS )
               XLO = DBLE( LBND( 1 ) )
               YLO = DBLE( LBND( 2 ) )
               XHI = DBLE( UBND( 1 ) )
               YHI = DBLE( UBND( 2 ) )

*  Select only those points in this Set's position list which fall
*  within the bounds of this NDF.
               CALL CCD1_CHUSB( %VAL( CNF_PVAL( IPIND( I ) ) ),
     :                          %VAL( CNF_PVAL( IPX1 ) ),
     :                          %VAL( CNF_PVAL( IPY1 ) ), NPOINT( I ),
     :                          XLO, XHI, YLO, YHI,
     :                          %VAL( CNF_PVAL( IPI2 ) ),
     :                          %VAL( CNF_PVAL( IPX2 ) ),
     :                          %VAL( CNF_PVAL( IPY2 ) ), N2, STATUS )

*  Now write a list and associate it with this NDF.  First construct
*  a name for the list file.
               CALL MSG_SETI( 'IX', J )
               CALL MSG_LOAD( ' ', 'ccdalign_' //
     :                        LISTID( :CHR_LEN( LISTID ) ) //
     :                        '_^IX.fea', FNAME, LENG, STATUS )

*  Open the list file and write a header.
               CALL CCD1_OPFIO( FNAME, 'WRITE', 'LIST', 0, FD, STATUS )
               CALL CCD1_FIOHD( FD, 'Output from CCDALIGN', STATUS )

*  Write the chosen points into it.
               CALL CCD1_WRIXY( FD, %VAL( CNF_PVAL( IPI2 ) ),
     :                          %VAL( CNF_PVAL( IPX2 ) ),
     :                          %VAL( CNF_PVAL( IPY2 ) ),
     :                          N2, LINE, 1024, STATUS )

*  Close the list file.
               CALL FIO_CLOSE( FD, STATUS )

*  Associate it with the NDF.
               CALL CCG1_STO0C( INDF, 'CURRENT_LIST', FNAME, STATUS )

*  Release the NDF.
               CALL NDF_ANNUL( INDF, STATUS )
            END DO

*  Release resources.
            CALL CCD1_MFREE( IPX1, STATUS )
            CALL CCD1_MFREE( IPY1, STATUS )
            CALL CCD1_MFREE( IPX2, STATUS )
            CALL CCD1_MFREE( IPX2, STATUS )
            CALL CCD1_MFREE( IPI2, STATUS )
         END IF
      END DO

*  Release memory used for position lists.
      DO I = 1, NSET
         CALL CCD1_MFREE( IPXPOS( I ), STATUS )
         CALL CCD1_MFREE( IPYPOS( I ), STATUS )
         CALL CCD1_MFREE( IPIND( I ), STATUS )
      END DO

*  Now centroid all the image feature positions to get accurate ones.
      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( ' ',  '  Centroiding the image feature positions.',
     :              STATUS )
      CALL MSG_BLANK( STATUS )
      DO I = 1, NSET
         IF ( STATUS .NE. SAI__OK ) GO TO 99
         CALL MSG_SETI( 'LISTID', I )
         CALL MSG_LOAD( ' ', 'ccdalign_ndf^LISTID.list', NAMLST, LENG,
     :                  STATUS )
         CMD = 'in=^'//NAMLST( :LENG )//' '//
     :         'ndfnames=true '//
     :         'outlist=*.acc '//
     :         'autoscale=true accept'
         CALL SLV_OBEYW( CCDREG, 'findcent', CMD, ' ', STATUS )
      END DO

*  See if the user wants to continue past this point.
      CALL MSG_BLANK( STATUS )
      LINE =
     :'You may stop processing at this point if all you require are '//
     :'labelled position lists associated with NDFs. If you want to '//
     :'determine the NDF registrations, then this procedure will aid '//
     :'this but only for linear transformations.'
      CALL CCD1_WRTPA( LINE, 72, 3, .FALSE., STATUS )
      CALL MSG_BLANK( STATUS )
      CALL PAR_GET0L( 'CONTINUE', CONT, STATUS )
      IF ( .NOT. CONT .OR. STATUS .NE. SAI__OK ) GO TO 99

*  Will continue get the type of transformation to use.
      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( ' ', '  Ok will continue processing, which type'//
     :              ' of transformation do you require?', STATUS )
      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( ' ', '     1 = shift of origin only', STATUS )
      CALL MSG_OUT( ' ', '     2 = shift of origin and rotation',
     :              STATUS )
      CALL MSG_OUT( ' ', '     3 = shift of origin and magnification',
     :              STATUS )
      CALL MSG_OUT( ' ', '     4 = shift of origin rotation and'//
     :              ' magnification', STATUS )
      CALL MSG_OUT( ' ', '     5 = full six parameter fit', STATUS )
      CALL MSG_BLANK( STATUS )
      CALL PAR_GET0I( 'FITTYPE', FITTYP, STATUS )

*  Need a list of all the input NDFs. To get this we create a new file
*  ccdalign_ndfs.list and copy the contents of all the groups files
*  into it.
      REGNAM = ' '
      CALL CCD1_OPFIO( 'ccdalign_ndfs.list', 'WRITE', 'LIST', 0, FDOUT,
     :                 STATUS )
      DO 6 I = 1, NSET
         CALL MSG_SETI( 'I', I )
         CALL MSG_LOAD( ' ', 'ccdalign_ndf^I.list', NAMLST,
     :                  OPLEN, STATUS )
         CALL CCD1_OPFIO( NAMLST, 'READ', 'LIST', 0, FD, STATUS )
         CALL ERR_MARK

*  While we can read this file, copy its contents into the total list.
*  Save the name of the very first NDF in the list for use later.
 7       CONTINUE
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL FIO_READ( FD, NDFNAM, NDFLEN, STATUS )
            CALL FIO_WRITE( FDOUT, NDFNAM, STATUS )
            IF ( REGNAM .EQ. ' ' ) REGNAM = NDFNAM
            GO TO 7
         END IF
         IF ( STATUS .EQ. FIO__EOF ) THEN
            CALL ERR_ANNUL( STATUS )
         END IF
         CALL ERR_RLSE
         CALL FIO_CLOSE( FD, STATUS )
 6    CONTINUE
      CALL FIO_CLOSE( FDOUT, STATUS )

*  Find the initial transformations, saving the extended reference set
*  to look for new positions on frames.
      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( ' ',  '   Determining initial transformations.',
     :              STATUS )
      CALL MSG_BLANK( STATUS )
      CALL MSG_SETI( 'FITTYP', FITTYP )
      CALL MSG_SETL( 'USESET', USESET )
      CALL MSG_LOAD( ' ', 'fittype=^FITTYP useset=^USESET ', CMD,
     :               OPLEN, STATUS )
      CMD( OPLEN + 1: ) = ' ndfnames=true '//
     :      'inlist=^ccdalign_ndfs.list '//
     :      'refpos=1 '//
     :      'usewcs=true '//
     :      'outdomain=ccd_reg1 '//
     :      'outref=ccdalign_ref.ext reset accept'
      CALL SLV_OBEYW( CCDREG, 'register', CMD, ' ', STATUS )

*  Now need to transform all the extended reference set to the coordinates of
*  all the other NDFs before re-centroiding to get accurate positions for any
*  image features beyond the initial reference set.
*  Associate extended reference set with all NDFs

*  Transform the reference list from pixel coordinates of the reference
*  NDF to CCD_REG1 coordinates, in which all the frames are registered.
      CALL MSG_SETC( 'REGNAM', REGNAM )
      CALL MSG_LOAD( ' ',
     :               'logto=n '//
     :               'ndfnames=false '//
     :               'inlist=ccdalign_ref.ext '//
     :               'trtype=wcs '//
     :               'framein=pixel '//
     :               'frameout=ccd_reg1 '//
     :               'outlist=ccdalign_reg.ext '//
     :               'wcsfile=^REGNAM '//
     :               'forward=true reset accept', CMD, OPLEN, STATUS )
      CALL SLV_OBEYW( CCDREG, 'tranlist', CMD, ' ', STATUS )

*  Associate this single position list, which corresponds to global
*  CCD_REG1 frame, with each of the NDFs.
      CMD = 'logto=n '//
     :      'mode=alist '//
     :      'in=^ccdalign_ndfs.list '//
     :      'inlist=ccdalign_reg.ext accept'
      CALL SLV_OBEYW( CCDREG, 'ccdedit', CMD, ' ', STATUS )

*  Now write a new position list for each NDF with the points from the
*  reference list but transformed into its own Pixel coordinates, which
*  is the more usual CCDPACK format.
      CMD = 'logto=n '//
     :      'ndfnames=true '//
     :      'inlist=^ccdalign_ndfs.list '//
     :      'outlist=*.ext '//
     :      'inext=true '//
     :      'forward=true '//
     :      'trtype=wcs '//
     :      'framein=ccd_reg1 '//
     :      'frameout=pixel '//
     :      'accept'
      CALL SLV_OBEYW( CCDREG, 'tranlist', CMD, ' ', STATUS )

*  Centroid the positions.
      CMD = 'logto=n '//
     :      'ndfnames=true '//
     :      'outlist=*.acc '//
     :      'in=^ccdalign_ndfs.list accept'
      CALL SLV_OBEYW( CCDREG, 'findcent', CMD, ' ', STATUS )

*  Now rework the transformations
      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( ' ',  '   Determining final transformations.',
     :              STATUS )
      CALL MSG_BLANK( STATUS )
      CALL MSG_SETI( 'FITTYP', FITTYP )
      CALL MSG_SETL( 'USESET', USESET )
      CALL MSG_LOAD( ' ', 'fittype=^FITTYP useset=^USESET ', CMD,
     :               OPLEN, STATUS )
      CMD( OPLEN + 1: ) = ' ndfnames=true '//
     :      'inlist=^ccdalign_ndfs.list '//
     :      'refpos=1 '//
     :      'usewcs=true '//
     :      'outdomain=ccd_reg '//
     :      'logto=both accept reset'
      CALL SLV_OBEYW( CCDREG, 'register', CMD, ' ', STATUS )

*  If an error occurred, then report a contextual message.
 99   CONTINUE

*  Clear up some groups.
      DO I = 1, NSET
         CALL CCD1_GRDEL( GRGR( I ), STATUS )
      END DO
      CALL CCD1_GRDEL( SNAMGR, STATUS )
      CALL CCD1_GRDEL( RNAMGR, STATUS )
      CALL CCD1_GRDEL( LDRGR, STATUS )
      CALL CCD1_GRDEL( REFGR, STATUS )

*  Free any memory which somehow has not been freed already.
      CALL CCD1_MFREE( -1, STATUS )

*  Stop any detached processes.
      IF ( CCDGID .GT. 0 ) CALL SLV_KILLW( CCDGID, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( 'CCDALIGN_ERR',
     :                  'CCDALIGN: failed to align CCD frames.',
     :                  STATUS )
      END IF

*  Close the logging system.
      CALL CCD1_END( STATUS )

      END
* $Id$
