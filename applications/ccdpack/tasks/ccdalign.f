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
*     CALL CCDALIGN( STATUS )

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
*     The basic method used is to access groups of NDFs (or a series
*     of single NDFs if all are moved between exposures) and an
*     optional reference NDF. The first NDF of the first group or the
*     reference NDF is initially displayed and you are invited to mark
*     the positions of centroidable image features on it using a 
*     graphical interface.  This window then remains on the screen for 
*     reference while you identify the same features on each of the 
*     other sets of images in the same way.
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
*     The graphical interface used for marking features on the image
*     should be fairly self-explanatory.  The image can be scrolled using
*     the scrollbars, the window can be resized, and there are controls
*     for zooming the image in or out, changing the style of display and 
*     altering the percentile cutoff limits.  The numbers of any 
*     identified features on each image must match those of the 
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
*     ccdalign

*  ADAM Parameters:
*     CONTINUE = _LOGICAL (Read)
*        If TRUE then this command will proceed to also work
*        out the registrations of your images. Note that this is
*        only possible if you are intending to use linear
*        transformations (this is the usual case).
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
*        A list of NDF names suitable to the current stage in
*        processing. The NDF names should be separated by commas
*        and may include wildcards.
*        [!]
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
*     PERCENTILES( 2 ) = _DOUBLE (Read)
*        The initial low and high percentiles of the data range to use
*        when displaying the images; any pixels with a value lower than
*        the first element will have the same colour, and any with a value
*        higher than the second will have the same colour.  Must be in
*        the range 0 <= PERCENTILES( 1 ) <= PERCENTILES( 2 ) <= 100.
*        This can be changed from within the GUI.
*        [2,98]
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
*     ccdalign
*        This runs the ccdalign program.

*  Behaviour of parameters:
*     All parameters retain their current value as default. The
*     'current' value is the value assigned on the last run of the
*     application. If the application has not been run then the
*     'intrinsic' defaults, as shown in the parameter help, apply.
*
*     As this application is a script some of the parameters are used
*     repeatedly and cannot be sensibly set on the command-line, indeed
*     it is not designed for this use and in general all parameters
*     should be responded to interactively.
*
*     Certain parameters (LOGTO and LOGFILE) have global values. These
*     global values will always take precedence, except when an
*     assignment is made on the command line.  Global values may be set
*     and reset using the CCDSETUP and CCDCLEAR commands.

*     Note - test it with NDFs in different directories.

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
      INCLUDE 'PSX_ERR'         ! PSX error codes
      INCLUDE 'PAR_ERR'         ! PAR system error codes
      INCLUDE 'CCD1_PAR'        ! CCDPACK private constants

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
      INTEGER MAXPOS
      PARAMETER ( MAXPOS = 99 ) ! Maximum number of positions in list
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
      CHARACTER * ( FIO__SZFNM ) FNAME ! Name of position list file
      CHARACTER * ( FIO__SZFNM ) NAMLST ! File name list
      CHARACTER * ( 30 ) CCDREG ! Message system name for ccdpack_reg
      CHARACTER * ( GRP__SZNAM ) NDFNMS( MAXGRP ) ! Names of lead NDFs in groups
      INTEGER CCDGID            ! Id for CCDPACK_REG monolith
      INTEGER FD                ! File identifier
      INTEGER FDOUT             ! File identifier
      INTEGER FITTYP            ! Transformation type
      INTEGER I                 ! Loop variable
      INTEGER INDEX( MAXPOS )   ! Identifiers for points in position list
      INTEGER IPIND             ! Pointer to identifiers for list positions
      INTEGER IPXPOS            ! Pointer to X coordinates of list positions
      INTEGER IPYPOS            ! Pointer to Y coordinates of list positions
      INTEGER ISET( CCD1__MXNDF ) ! Set membership of group leader NDFs
      INTEGER J                 ! Loop variable
      INTEGER LDRGR             ! GRP identifier for group leader NDFs
      INTEGER LENG              ! Returned length of string
      INTEGER LMEM( CCD1__MXNDF ) ! Leader NDF indices in Set order
      INTEGER LMEMOF( CCD1__MXNDF + 1 ) ! Pointers into LMEM
      INTEGER MAPSET( CCD1__MXNDF ) ! Workspace
      INTEGER MAXCNV            ! Initial maximum dimension of display region
      INTEGER MORGR             ! GRP identifier for more NDF names
      INTEGER NDFLEN            ! length of NDF name
      INTEGER NGOT              ! Number of NDFs already got
      INTEGER NL                ! Length of NULL string
      INTEGER NLDR              ! Number of NDFs in group leader group
      INTEGER NMOR              ! Number of extra NDFs in group
      INTEGER NNDF              ! Number of NDFs passed to GUI script
      INTEGER NPOINT( MAXGRP )  ! Number of positions marked for each NDF
      INTEGER NREF              ! Number of reference NDFs
      INTEGER NSET              ! Number of Sets represented in input
      INTEGER NTRY              ! Number of tries at getting variable
      INTEGER OPLEN             ! String length
      INTEGER REFGR             ! GRP identifier for reference NDF names
      INTEGER REFPOS            ! Position of the reference NDF in the list
      INTEGER RISET( CCD1__MXNDF ) ! Set membership of group leader ref NDF
      INTEGER RMEM( CCD1__MXNDF ) ! Reference NDF indices in Set order
      INTEGER RMEMOF( CCD1__MXNDF + 1 ) ! Pointers into RMEM
      INTEGER RNAMGR            ! Reference NDF Set name group
      INTEGER RNSET             ! Number of reference NDF Sets
      INTEGER SNAMGR            ! Set Name group
      INTEGER WINDIM( 2 )       ! Window dimensions for display
      LOGICAL CONT              ! Continue processing
      LOGICAL EXTRAS            ! Whether to solicit more NDF names per group
      LOGICAL HAVREF            ! Have a reference NDF
      LOGICAL US                ! Temporary USESET variable
      LOGICAL USESET            ! Use Set headers if available?
      DOUBLE PRECISION PERCNT( 2 ) ! The display percentiles
      DOUBLE PRECISION XPOS( MAXPOS ) ! X coordinates of positions in list
      DOUBLE PRECISION YPOS( MAXPOS ) ! Y coordinates of positions in list
      DOUBLE PRECISION ZOOM     ! Zoom factor for display

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

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
      CALL CCD1_NDFGL( 'IN', 2, CCD1__MXNDF, LDRGR, NLDR, STATUS )

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

*  See whether to solicit extra members of each group apart from the 
*  leaders.
      CALL PAR_GET0L( 'EXTRAS', EXTRAS, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Write the names to lists.
      DO I = 1, NSET

*  Write the names for this group to a file.
         CALL MSG_SETI( 'NGROUP', I )
         CALL MSG_LOAD( ' ', 'ccdalign_ndf^NGROUP.list', NAMLST,
     :                  OPLEN, STATUS )
         CALL CCD1_OPFIO( NAMLST, 'WRITE', 'LIST', 0, FD, STATUS )
         DO J = LMEMOF( I ), LMEMOF( I + 1 ) - 1
            CALL GRP_GET( LDRGR, LMEM( J ), 1, NDFNAM, STATUS )
            CALL FIO_WRITE( FD, NDFNAM( : CHR_LEN( NDFNAM ) ), STATUS )
         END DO
            
*  If requested, get the extra members.
         IF ( EXTRAS ) THEN

            CALL MSG_OUT( ' ', ' ', STATUS )
            IF ( USESET ) THEN
               CALL GRP_GET( SNAMGR, I, 1, SNAME, STATUS )
               CALL MSG_SETC( 'SETNAM', SNAME )
               CALL MSG_OUT( ' ', 
     :'Enter extra NDFs in the same sky position as Set "^SETNAM":',
     :                       STATUS )
               LINE = 'Member NDFs are:'
               DO J = 1, LMEMOF( I ), LMEMOF( I + 1 ) - 1
                  CALL GRP_GET( LDRGR, LMEM( J ), 1, LINE( 18: ),
     :                          STATUS )
                  CALL MSG_SETC( 'LINE', LINE )
                  CALL CCD1_MSG( ' ', '^LINE', STATUS )
                  LINE = ' '
               END DO
            ELSE
               CALL GRP_GET( LDRGR, I, 1, NDFNAM, STATUS )
               CALL MSG_SETC( 'NAME', NDFNAM )
               CALL MSG_OUT( ' ', 
     :'Enter extra NDFs in the same sky position as NDF "^NAME"',
     :                       STATUS )
            END IF
            CALL PAR_CANCL( 'MORE', STATUS )
            NGOT = LMEMOF( I + 1 ) - LMEMOF( I )
            IF ( STATUS .NE. SAI__OK ) GO TO 99
            CALL CCD1_NDFGL( 'MORE', 0, CCD1__MXNDF - NGOT, MORGR, NMOR,
     :                       STATUS )
            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )
               NMOR = 0
            END IF
            DO J = 1, NMOR
               CALL GRP_GET( MORGR, J, 1, NDFNAM, STATUS )
               CALL FIO_WRITE( FD, NDFNAM( : CHR_LEN( NDFNAM ) ),
     :                         STATUS )
            END DO
            CALL CCD1_GRDEL( MORGR, STATUS )
         END IF
         CALL FIO_CLOSE( FD, STATUS )
      END DO

*  See if the user is supplying a reference NDF.
      HAVREF = .FALSE.
      IF ( USESET ) THEN
         NTRY = 0
 4       CONTINUE
         CALL CCD1_NDFGL( 'REFNDF', 0, CCD1__MXNDF, REFGR, NREF,
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
         CALL CCD1_NDFGL( 'REFNDF', 0, 1, REFGR, NREF, STATUS )
      END IF
      IF ( STATUS .EQ. PAR__NULL ) THEN
         HAVREF = .FALSE.
         CALL ERR_ANNUL( STATUS )
      ELSE IF ( STATUS .EQ. SAI__OK .AND. NREF .GE. 1 ) THEN
         HAVREF = .TRUE.
      ELSE ! should not happen
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCDALIGN_BADREF', 
     :                 'CCDALIGN: Error getting REFNDF value', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  If there is a reference image group, write its name(s) to a file.
      IF ( HAVREF ) THEN
         CALL CCD1_OPFIO( 'ccdalign_ref.list', 'WRITE', 'LIST', 0, FD,
     :                    STATUS )
         CALL GRP_GET( REFGR, 1, 1, NDFNAM, STATUS )
         CALL FIO_WRITE( FD, NDFNAM( : CHR_LEN( NDFNAM ) ), STATUS )

         IF ( EXTRAS ) THEN
            CALL MSG_OUT( ' ', 
     :'Enter extra NDFs in the same sky position as the reference NDF',
     :                    STATUS )
            CALL PAR_CANCL( 'MORE', STATUS )
            CALL CCD1_NDFGL( 'MORE', 0, CCD1__MXNDF - NREF, MORGR, NMOR,
     :                       STATUS )
            DO J = 1, NMOR
               CALL GRP_GET( MORGR, J, 1, NDFNAM, STATUS )
               CALL FIO_WRITE( FD, NDFNAM( : CHR_LEN( NDFNAM ) ),
     :                         STATUS )
            END DO
            CALL CCD1_GRDEL( MORGR, STATUS )
         END IF
         CALL FIO_CLOSE( FD, STATUS )
      END IF

*  Get the number and list of NDFs.
      IF ( HAVREF ) THEN
         NNDF = NLDR + 1
         CALL GRP_GET( REFGR, 1, 1, NDFNAM, STATUS )
         CALL GRP_PUT( LDRGR, 1, NDFNAM, NNDF, STATUS )
         REFPOS = NNDF
      ELSE
         NNDF = NLDR
         REFPOS = 1
      END IF
      CALL GRP_GET( LDRGR, 1, NNDF, NDFNMS, STATUS )

*  Allocate memory for coordinates of position lists.
      CALL CCD1_MALL( MAXPOS * NNDF, '_INTEGER', IPIND, STATUS )
      CALL CCD1_MALL( MAXPOS * NNDF, '_DOUBLE', IPXPOS, STATUS )
      CALL CCD1_MALL( MAXPOS * NNDF, '_DOUBLE', IPYPOS, STATUS )

*  Get values of display preferences from parameter system.
      CALL PAR_GET0D( 'ZOOM', ZOOM, STATUS )
      CALL PAR_GET0I( 'MAXCANV', MAXCNV, STATUS )
      CALL PAR_GET0I( 'WINX', WINDIM( 1 ), STATUS )
      CALL PAR_GET0I( 'WINY', WINDIM( 2 ), STATUS )
      CALL PAR_GET0C( 'MARKSTYLE', MSTYLE, STATUS )

*  Call the routine which will do the graphical user interaction to
*  obtain the position lists for each group of NDFs.
      CALL CCD1_ALGN( NDFNMS, NNDF, REFPOS, MAXPOS, PERCNT, ZOOM, 
     :                MAXCNV, WINDIM, MSTYLE, NPOINT, %VAL( IPXPOS ),
     :                %VAL( IPYPOS ), %VAL( IPIND ), STATUS )

*  Write display preference parameters back to the parameter system.
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      CALL PAR_PUT0D( 'ZOOM', ZOOM, STATUS )
      CALL PAR_PUT0I( 'MAXCANV', MAXCNV, STATUS )
      CALL PAR_PUT0I( 'WINX', WINDIM( 1 ), STATUS )
      CALL PAR_PUT0I( 'WINY', WINDIM( 2 ), STATUS )
      CALL PAR_PUT1D( 'PERCENTILES', 2, PERCNT, STATUS )
      CALL PAR_PUT0C( 'MARKSTYLE', MSTYLE, STATUS )

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
      DO I = 1, NNDF

*  Get the name of the position list file and of the file containing all
*  the NDFs in this group.
         IF ( HAVREF .AND. I .EQ. NNDF ) THEN
            CALL MSG_SETC( 'LISTID', 'ref' )
            CALL MSG_LOAD( ' ', '^LISTID', LISTID, LENG, STATUS )
         ELSE
            CALL MSG_SETI( 'LISTID', I )
            CALL MSG_LOAD( ' ', 'ndf^LISTID', LISTID, LENG, STATUS )
         END IF
         FNAME = 'ccdalign_' // LISTID( :LENG ) // '.fea'
         NAMLST = 'ccdalign_' // LISTID( :LENG ) // '.list'

*  Open the position list file.
         CALL CCD1_OPFIO( FNAME, 'WRITE', 'LIST', 0, FD, STATUS )

*  Write a header.
         CALL CCD1_FIOHD( FD, 'Output from CCDALIGN', STATUS )

*  Copy the data for this position list into workspace arrays so that it
*  can be accessed.
         CALL CCG1_COPSI( MAXPOS * ( I - 1 ) + 1, %VAL( IPIND ), 
     :                    NPOINT( I ), 1, INDEX, STATUS )
         CALL CCG1_COPSD( MAXPOS * ( I - 1 ) + 1, %VAL( IPXPOS ),
     :                    NPOINT( I ), 1, XPOS, STATUS )
         CALL CCG1_COPSD( MAXPOS * ( I - 1 ) + 1, %VAL( IPYPOS ),
     :                    NPOINT( I ), 1, YPOS, STATUS )

*  Write all the points in the position list to the file.
         CALL CCD1_WRIXY( FD, INDEX, XPOS, YPOS, NPOINT( I ), LINE, 
     :                    1024, STATUS )

*  Close the position list file.
         CALL FIO_CLOSE( FD, STATUS )

*  Associate the position list file with all the NDFs in this group.
         CMD = 'logto=n ' //
     :         'mode=alist ' //
     :         'in=^' // NAMLST( :CHR_LEN( NAMLST ) ) // ' ' //
     :         'inlist=' // FNAME( :CHR_LEN( FNAME ) ) // ' accept'
         CALL SLV_OBEYW( CCDREG, 'ccdedit', CMD, ' ', STATUS )
      END DO

*  Release memory used for position lists.
      CALL CCD1_MFREE( IPYPOS, STATUS )
      CALL CCD1_MFREE( IPXPOS, STATUS )
      CALL CCD1_MFREE( IPIND, STATUS )

*  Now centroid all the image feature positions to get accurate ones.
      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( ' ',  '  Centroiding the image feature positions.',
     :              STATUS )
      CALL MSG_BLANK( STATUS )
      DO I = 1, NLDR
         IF ( STATUS .NE. SAI__OK ) GO TO 99
         CALL MSG_SETI( 'NGROUP', I ) 
         CALL MSG_LOAD( ' ', 'ccdalign_ndf^NGROUP.list', NAMLST,
     :                  OPLEN, STATUS )
         CMD = 'in=^'//NAMLST( :OPLEN )//' '//
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
      DO 6 I = 1, NNDF
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

*  Free any memory which somehow has not been freed already.
      CALL CCD1_MFREE( -1, STATUS )

*  Stop any detached processes.
      IF ( CCDGID .GT. 0 ) CALL SLV_KILLW( CCDGID, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( 'CCDALIGN_ERR',
     :                  'CCDALIGN: failed to align CCD frames.',
     :                  STATUS )
      END IF
      END
* $Id$
