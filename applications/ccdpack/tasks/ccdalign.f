      SUBROUTINE CCDALIGN( PID, STATUS )
*+
*  Name:
*     CCDALIGN

*  Purpose:
*     Interactive procedure to aid the alignment of NDFs.

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
*     This procedure aids the registration of NDFs which are not
*     related by simple offsets (see FINDOFF and PAIRNDF if they are).
*     It also has the capability of dealing with groups of NDFs which
*     are almost registered (frames which have not been moved on the
*     sky) saving effort in re-identification of image features.
*
*     The basic method used is to access groups of NDFs (or a series
*     of single NDFs if all are moved between exposures) and an
*     optional reference NDF. The first NDF of the first group or the
*     reference NDF is then displayed and a cursor application is used
*     to record the positions of centroidable image features. The
*     first NDFs of all the other groups are then displayed and you
*     are invited to identify the image features in the order which
*     corresponds to that used for the reference NDF. Missing image
*     features are identified as off the currently displayed image (so
*     the centroid routine will fail to find them). The reference set
*     of image features may be extended by identification after the
*     last reference feature has been marked.
*
*     After centroiding you are then given the option to stop. If
*     you decide to then you will have labelled position lists to use
*     in the other CCDPACK routines (the labelled positions will be
*     called NDF_NAME.acc). If you chose the option to continue then
*     a full registration of the NDFs will be attempted. This may only
*     be performed for 'linear' transformations.
*
*     After choosing a transformation type the procedure will then go on
*     to calculate a transformation set between all the NDFs, this is
*     then used (with the extended reference set from REGISTER) to
*     approximate the position of all possible image features, these are
*     then located by centroiding and a final registration of all NDFs
*     is performed. The resultant NDFs then have associated lists of
*     labelled positions and TRANSFORM structures which may be used to
*     transform other position lists or when resampling the data.

*  Usage:
*     ccdalign [device]

*  ADAM Parameters:
*     CONTINUE = _LOGICAL (Read)
*        If TRUE then this command will proceed to also work
*        out the registrations of your images. Note that this is
*        only possible if you are intending to use linear
*        transformations (this is the usual case).
*        [FALSE]
*     DEVICE = _CHAR (Read)
*        The graphics device to use when displaying images.
*        [Current image display device]
*     HARDCOPY = _LOGICAL (Read)
*        If TRUE then a hardcopy of the reference NDF will be made.
*        [TRUE]
*     HARDDEV = _CHAR (Read)
*        A graphics device that can be used to draw a copy of the 
*        displayed image into a file that can be subsequently printed.
*        A postscript device such as "ps_p" or "ps_l" is normal.
*        [ps_p]
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
*     PERCENTILES(2) = _REAL (Read)
*        Percentile scale used when displaying images. See the 
*        KAPPA command DISPLAY for more. The two values should be
*        in the range 0 to 100.
*        [2,98]
*     PRINTCMD = _CHAR (Read)
*        A command that will print the file "snapshot.ps". This file
*        is the result of printing a hardcopy of the image display
*        device. 
*        [lpr snapshot.ps]

*  Examples:
*     ccdalign device=xw
*        This starts the CCDALIGN script and displays all images
*        in a GWM xwindows window.

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
*
*     The DEVICE parameter also has a global association. This is not
*     controlled by the usual CCDPACK mechanisms, instead it works in
*     co-operation with KAPPA (SUN/95) image display/control routines.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     20-MAY-1997 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'MSG_PAR'         ! Message system constants
      INCLUDE 'FIO_ERR'         ! FIO error codes
      INCLUDE 'PSX_ERR'         ! PSX error codes

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

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN           ! Used length of string

*  Local Variables:
      CHARACTER * ( 1024 ) LINE ! Output message line (long)
      CHARACTER * ( 132 ) CMD   ! Command string
      CHARACTER * ( 30 ) DEVICE ! The display device
      CHARACTER * ( 30 ) HDEV   ! Hardcopy device
      CHARACTER * ( 30 ) NAMLST ! File name list
      CHARACTER * ( 5 ) NULL    ! Parameter NULL symbol
      CHARACTER * ( 30 ) PERC   ! Percentiles as string
      CHARACTER * ( MSG__SZMSG ) NDFNAM ! Name of NDF
      CHARACTER * ( MSG__SZMSG ) REFNAM ! Name of reference NDF
      CHARACTER * ( 30 ) CCDREG ! Message system name for ccdpack_reg
      CHARACTER * ( 30 ) CCDRES ! Message system name for ccdpack_res
      CHARACTER * ( 30 ) KAPVIE ! Message system name for kapview_mon
      INTEGER CCDGID            ! Id for CCDPACK_REG monolith
      INTEGER CCDRID            ! Id for CCDPACK_RES monolith
      INTEGER FD                ! File identifier
      INTEGER FDOUT             ! File identifier
      INTEGER FITTYP            ! Transformation type
      INTEGER GRPOFF            ! Offset in NDF list group
      INTEGER I                 ! Loop variable
      INTEGER KAPVID            ! Id for Kappa view monolith
      INTEGER NDFLEN            ! length of NDF name
      INTEGER NGROUP            ! Number of file groups
      INTEGER NL                ! Length of NULL string
      INTEGER NVAL              ! Dummy
      INTEGER OPLEN             ! String length
      INTEGER REFLEN            ! length of reference NDF name
      LOGICAL CONT              ! Continue processing
      LOGICAL EXISTS            ! File exists
      LOGICAL HAVREF            ! Have a reference NDF
      LOGICAL HCOPY             ! Print hardcopy
      LOGICAL OK                ! OK to loop
      REAL PERCEN( 2 )          ! The display percentiles

      INTEGER STATE             ! Debug

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find out the environment we're running under. If it's IRAF then
*  INDEF is used instead of ! as a NULL symbol. 
      CALL CCD1_SETEX( NULL, NL, STATUS )
      CCDRID = -1
      CCDGID = -1
      KAPVID = -1

*  Startup any monoliths that we need to use.
*  CCDPACK_RES (note hard-coded monolith).
      CALL PSX_GETENV( 'CCDPACK_DIR', CMD, STATUS )
      CMD = CMD( :CHR_LEN( CMD ) )//'/ccdpack_res'
      CCDRES = 'ccdpack_res'//PID
      CCDRID = SLV_LOADW( CCDRES, CMD, .TRUE., TIMOUT, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'FAILED',
     :'Sorry cannot proceed. Failed to load monolith '//
     :'$CCDPACK_DIR/ccdpack_res', STATUS )
         GO TO 99
      END IF

*   CCDPACK_REG
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

*  KAPVIEW_MON
      CALL PSX_GETENV( 'KAPPA_DIR', CMD, STATUS )
      CMD = CMD( :CHR_LEN( CMD ) )//'/kapview_mon'
      KAPVIE = 'kapview_mon'//PID
      KAPVID = SLV_LOADW( KAPVIE, CMD, .TRUE., TIMOUT, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'FAILED',
     :'Sorry cannot proceed. Failed to load monolith '//
     :'$KAPPA_DIR/kapview_mon', STATUS )
         GO TO 99
      END IF

*  Start the application and introduce ourselves.
      CALL CCD1_START( 'CCDALIGN', STATUS )
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ',
     :     '  An interactive aid for aligning groups of NDFs.', STATUS )
      CALL CCD1_MSG( ' ', ' ', STATUS )

*  Get the display device.
      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( ' ',
     :     '  Give the name of an image display device', STATUS )
      CALL MSG_BLANK( STATUS )
      CALL PAR_GET0C( 'DEVICE', DEVICE, STATUS )
      CMD = 'DEVICE='//DEVICE
      CALL SLV_OBEYW( KAPVIE, 'idset', CMD, ' ', STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Get a percentile range for displaying the images. Note we store these
*  as a string as KAPPA display tries to set up defaults and we
*  need to override this behaviour.
      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( ' ',
     :     '  What percentile range do you want to use when '//
     :     'displaying images?',
     :             STATUS )
      CALL MSG_BLANK( STATUS )
      CALL PAR_GET1R( 'PERCENTILES', 2, PERCEN, NVAL, STATUS )
      CALL MSG_SETR( 'LOW', PERCEN( 1 ) )
      CALL MSG_SETR( 'HIGH', PERCEN( 2 ) )
      CALL MSG_LOAD( ' ', '[^LOW,^HIGH]', PERC, OPLEN, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Write a message indicating that the user should return lists of
*  all the NDFs to process. Each group contains NDFs which have not
*  been moved on the sky.
      CALL MSG_BLANK( STATUS )
      LINE = 'Give the names of a series of groups of NDFs '//
     :       'whose positions have not been moved on the sky. '//
     :       'If all NDFs have been moved then give a single '//
     :       'NDF at each prompt. When all NDF groups/NDFs have '//
     :       'been given respond with a '//NULL(:NL)//' (null).'
      CALL CCD1_WRTPA( LINE, 72, 3, .FALSE., STATUS )
      CALL MSG_BLANK( STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Get the lists of NDFs. This continues until no output file list is
*  created.
      OK = .TRUE.
      NGROUP = 1
 1    CONTINUE
      IF ( OK .AND. STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETI( 'NGROUP', NGROUP )
         CALL MSG_LOAD( ' ', 'ccdalign_ndf^NGROUP.list', NAMLST,
     :                  OPLEN, STATUS )

*  If the output list for the filenames exists then delete it. We use
*  existence of this file to indicate that new NDFs are available.
         INQUIRE( FILE = NAMLST, EXIST = EXISTS )
         IF ( EXISTS ) THEN
            CMD = 'rm '//NAMLST
            CALL CCD1_EXEC( CMD, STATUS )
         END IF
C         CMD = 'namelist='//NAMLST//' '//
C     :         'echo=true '//
C     :         'maxndf=100 reset prompt'
C         CALL SLV_OBEYW( CCDRES, 'ccdndfac', CMD, 'IN<IN', STATUS )
         CALL CCD1_SETPA( 'NAMELIST', NAMLST, STATUS )
         CALL CCD1_SETPA( 'ECHO', 'TRUE', STATUS )
         CALL CCD1_SETPA( 'MAXNDF', '100', STATUS )
         CALL CCDNDFAC( STATUS )
         CALL PAR_CANCL( 'IN', STATUS )
         CALL PAR_CANCL( 'NAMELIST', STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  See if the file was created. If so we have some more NDFs and
*  need to ask for some more. Otherwise terminate this loop.
            INQUIRE( FILE = NAMLST, EXIST = EXISTS )
            IF ( EXISTS ) THEN
               NGROUP = NGROUP + 1
            ELSE
               OK = .FALSE.
            END IF

*  Trap the condition when no groups have been given.
            IF ( NGROUP .EQ. 1 ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'NOGROUPS', 'No groups of NDFs where given'
     :                     ,STATUS )
               GO TO 99
            END IF
         END IF

*  Cancel the parameter association to get a prompt on the next
*  loop.
         CALL PAR_CANCL( 'IN', STATUS )
         GO TO 1
      END IF

*  See if a reference NDF is available.
      CALL MSG_BLANK( STATUS )
      LINE = 'If you have a reference image to which the other NDFs '//
     :       'are to be aligned then give its name at the next '//
     :       'prompt. If no reference NDF is specified (signified '//
     :       'by a '//NULL(:NL)//' response) then the first NDF of '//
     :       'the first group will be used.'
      CALL CCD1_WRTPA( LINE, 72, 3, .FALSE., STATUS )
      CALL MSG_BLANK( STATUS )
      INQUIRE( FILE = 'ccdalign_ref.list', EXIST = EXISTS )
      IF ( EXISTS ) THEN
         CALL CCD1_EXEC( 'rm ccdalign_ref.list', STATUS )
      END IF
C      CMD = 'namelist=ccdalign_ref.list '//
C     :      'echo=true '//
C     :      'maxndf=100 reset'
C      CALL SLV_OBEYW( CCDRES, 'ccdndfac', CMD, 'IN<IN', STATUS )
      CALL CCD1_SETPA( 'NAMELIST', 'ccdalign_ref.list', STATUS )
      CALL CCD1_SETPA( 'ECHO', 'TRUE', STATUS )
      CALL CCD1_SETPA( 'MAXNDF', '100', STATUS )
      CALL CCDNDFAC( STATUS )
      CALL PAR_CANCL( 'IN', STATUS )
      CALL PAR_CANCL( 'NAMELIST', STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      INQUIRE( FILE = 'ccdalign_ref.list', EXIST = EXISTS )
      IF ( EXISTS ) THEN
         HAVREF = .TRUE.
      ELSE
         HAVREF = .FALSE.
      END IF

*  Now display the reference image or the very first NDF.
      IF ( HAVREF ) THEN
         CALL CCD1_OPFIO( 'ccdalign_ref.list', 'READ', 'LIST', 0,
     :                    FD, STATUS )
      ELSE
         CALL CCD1_OPFIO( 'ccdalign_ndf1.list', 'READ', 'LIST', 0,
     :               FD, STATUS )
      END IF
      CALL FIO_READ( FD, REFNAM, REFLEN, STATUS )
      CALL FIO_CLOSE( FD, STATUS )
      CALL CCD1_OPLOG( STATUS )
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL MSG_SETC( 'REFNDF', REFNAM( :REFLEN ) )
      CALL CCD1_MSG( ' ', '  Using reference NDF ^REFNDF', STATUS )
      CALL CCD1_MSG( ' ', ' ', STATUS )

*  Display this NDF.
      CALL MSG_BLANK( STATUS )
      CALL MSG_SETC( 'REFNDF', REFNAM( :REFLEN ) )
      CALL MSG_OUT( ' ', '  Displaying NDF ^REFNDF', STATUS )
      CALL MSG_BLANK( STATUS )
      CMD = 'in='//REFNAM( :REFLEN )//' '//
     :      'mode=percentiles '//
     :      'percentiles='//PERC//' accept'
      CALL SLV_OBEYW( KAPVIE, 'display', CMD, ' ', STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Now use the cursor routine to read the image feature positions.
      CALL MSG_BLANK( STATUS )
      LINE = 'Use the cursor to mark the image features. Remember '//
     :       'the order as this is important for later '//
     :       'identifications.'
      CALL CCD1_WRTPA( LINE, 72, 3, .FALSE., STATUS )
      CALL MSG_BLANK( STATUS )

*  Activate the cursor routine. If this is the first NDF of the first group
*  then use all the NDF names of this group as the IN parameter. This will
*  associate this position list with all the NDFs.
      IF ( HAVREF ) THEN
         CMD = 'in='//REFNAM( :REFLEN)//' '//
     :         'outlist='//REFNAM( :REFLEN)//'.fea accept reset'
      ELSE
         CMD = 'in=^ccdalign_ndf1.list'//' '//
     :         'outlist='//REFNAM( :REFLEN)//'.fea accept reset'
      END IF
      CALL SLV_OBEYW( CCDREG, 'idicurs', CMD, ' ', STATUS )

*  Now plot the identifiers of the image features.
      CMD = 'inlist='//REFNAM( :REFLEN)//'.fea '//
     :      'mtype=-1 '//
     :      'palnum=3 '//
     :      'ndfnames=false accept'
      CALL SLV_OBEYW( CCDREG, 'plotlist', CMD, ' ', STATUS )

*  See if user wants a hardcopy of the display.
      CALL PAR_GET0L( 'HARDCOPY', HCOPY, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Do the hardcopy if asked.
      IF ( HCOPY ) THEN
         CALL MSG_BLANK( STATUS )
         CALL MSG_OUT( ' ',
     :        '  Give the name of a device that can be printed to.',
     :                 STATUS )
         CALL MSG_BLANK( STATUS )
         CALL PAR_GET0C( 'HARDDEV', HDEV, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Get a snapshot.
         CALL MSG_OUT( ' ',
     :'  Capturing snapshot of display... Select portion of interest',
     :                 STATUS )
         INQUIRE( FILE='snapshot.ps', EXIST=EXISTS )
         IF ( EXISTS ) THEN
            CMD = 'rm snapshot.ps'
            CALL CCD1_EXEC( CMD, STATUS )
         END IF
         CMD = 'odevice='//HDEV( :CHR_LEN(HDEV) )//';snapshot.ps '//
     :         'whole=false '//
     :         'negative=true accept'
         CALL SLV_OBEYW( KAPVIE, 'snapshot', CMD, ' ', STATUS )

*  Need to print the output.
 2       CONTINUE
         CALL PAR_GET0C( 'PRINTCMD', CMD, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 99
         IF ( CMD .EQ. ' ' ) THEN
            CMD = 'lpr snapshot.ps'
         END IF
         IF ( INDEX( CMD, 'snapshot.ps' ) .EQ. 0 ) THEN 
            CALL MSG_OUT( ' ', 
     :'   You must give a command that will print the file snapshot.ps', 
     :                    STATUS )
            CALL PAR_CANCL( 'PRINTCMD', STATUS )
            GO TO 2
         END IF
         CALL CCD1_EXEC( CMD, STATUS )
      END IF

*  Set the number of NDF groups we need to process.
      IF ( HAVREF ) THEN 
         GRPOFF = 1
      ELSE
         GRPOFF = 2
      END IF

*  Introduction to next stage
      CALL MSG_BLANK( STATUS )
      LINE = 
     :'Now the first member of each NDF group or each NDF will be '//
     :'displayed. You will then be given the opportunity to use the '//
     :'cursor to mark the image features which correspond to those '//
     :'which you marked on the first (reference) NDF. The order in '//
     :'which you identify the image features must be the same. If an '//
     :'image feature does not exist mark a position off the frame. '//
     :'You may extend the complete set of positions by indicating '//
     :'image features after the last one in the reference set.'
      CALL CCD1_WRTPA( LINE, 72, 3, .FALSE., STATUS )
      CALL MSG_BLANK( STATUS )

*  Have used the first NDF group as reference set. Set start from next group.
 3    CONTINUE
      IF ( GRPOFF .NE. NGROUP .AND. STATUS .EQ. SAI__OK ) THEN 

*  Extract the name of the first member of this group.
         CALL MSG_SETI( 'NGROUP', GRPOFF ) 
         CALL MSG_LOAD( ' ', 'ccdalign_ndf^NGROUP.list', NAMLST,
     :                  OPLEN, STATUS )
         CALL CCD1_OPFIO( NAMLST, 'READ', 'LIST', 0, FD, STATUS )
         CALL FIO_READ( FD, NDFNAM, NDFLEN, STATUS )
         CALL FIO_CLOSE( FD, STATUS )
         
*  Display this NDF.
         CALL MSG_SETC( 'NDFNAM', NDFNAM( :NDFLEN ) )
         CALL CCD1_MSG( ' ',  '  Displaying NDF ^NDFNAM', STATUS )
         CALL MSG_BLANK( STATUS )
         CMD = 'in='//NDFNAM( :NDFLEN )//' '//
     :         'mode=percentiles '//
     :         'percentiles='//PERC//' accept'
         CALL SLV_OBEYW( KAPVIE, 'display', CMD, ' ', STATUS )

*  Activate the cursor routine. Use all the NDF names of this group as the IN
*  parameter. This will associate this position list with all the NDFs.
         CMD = 'outlist = '//NDFNAM( :NDFLEN )//'.fea '//
     :         'in='//NDFNAM( :NDFLEN )//' accept reset'
         CALL SLV_OBEYW( CCDREG, 'idicurs', CMD, ' ', STATUS )

*  Increment offset for next group.
         GRPOFF = GRPOFF + 1
         GO TO 3
      END IF

*  Now centroid all the image feature positions to get accurate ones.
      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( ' ',  '  Centroiding the image feature positions.',
     :              STATUS )
      CALL MSG_BLANK( STATUS )
      GRPOFF = 1
 5    CONTINUE
      IF ( GRPOFF .NE. NGROUP .AND. STATUS .EQ. SAI__OK ) THEN 
         CALL MSG_SETI( 'NGROUP', GRPOFF ) 
         CALL MSG_LOAD( ' ', 'ccdalign_ndf^NGROUP.list', NAMLST,
     :                  OPLEN, STATUS )
         CMD = 'in=^'//NAMLST( :OPLEN )//' '//
     :         'ndfnames=true '//
     :         'outlist=*.acc accept'
         CALL SLV_OBEYW( CCDREG, 'findcent', CMD, ' ', STATUS )
         GRPOFF = GRPOFF + 1
         GO TO 5
      END IF

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
      CALL CCD1_OPFIO( 'ccdalign_ndfs.list', 'WRITE', 'LIST', 0, FDOUT, 
     :                 STATUS )
      DO 6 I = 1, NGROUP - 1
         CALL MSG_SETI( 'I', I ) 
         CALL MSG_LOAD( ' ', 'ccdalign_ndf^I.list', NAMLST,
     :                  OPLEN, STATUS )
         CALL CCD1_OPFIO( NAMLST, 'READ', 'LIST', 0, FD, STATUS )
         CALL ERR_MARK

*  While we can read this file, copy its contents into the total list.
 7       CONTINUE
         IF ( STATUS .EQ. SAI__OK ) THEN 
            CALL FIO_READ( FD, NDFNAM, NDFLEN, STATUS )
            CALL FIO_WRITE( FDOUT, NDFNAM, STATUS )
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
      CALL MSG_LOAD( ' ', 'fittype=^FITTYP ', CMD, OPLEN, STATUS )
      CMD( OPLEN + 1: ) = ' ndfnames=true '//
     :      'inlist=^ccdalign_ndfs.list '//
     :      'refpos=1 '//
     :      'outref=ccdalign_ref.ext accept'
      CALL SLV_OBEYW( CCDREG, 'register', CMD, ' ', STATUS )

*  Now need to transform all the extended reference set to the coordinates of
*  all the other NDFs before re-centroiding to get accurate positions for any
*  image features beyond the initial reference set.
*  Associate extended reference set with all NDFs
      CMD = 'logto=n '//
     :      'mode=alist '//
     :      'in=^ccdalign_ndfs.list '//
     :      'inlist=ccdalign_ref.ext accept'
      CALL SLV_OBEYW( CCDREG, 'ccdedit', CMD, ' ', STATUS )
      CMD = 'logto=n '//
     :      'ndfnames=true '//
     :      'inlist=^ccdalign_ndfs.list '//
     :      'outlist=*.ext '//
     :      'inext=true '//
     :      'forward=false '//
     :      'trtype=struct accept'
      CALL SLV_OBEYW( CCDREG, 'tranlist', CMD, ' ', STATUS )
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
      CALL MSG_LOAD( ' ', 'fittype=^FITTYP ', CMD, OPLEN, STATUS )
      CMD( OPLEN + 1: ) = ' ndfnames=true '//
     :      'inlist=^ccdalign_ndfs.list '//
     :      'refpos=1 '//
     :      'outref=ccdalign_ref.ext '//
     :      'logto=both accept reset'
      CALL SLV_OBEYW( CCDREG, 'register', CMD, ' ', STATUS )

*  If an error occurred, then report a contextual message.
 99   CONTINUE

*  Stop any detached processes.
      IF ( CCDRID .GT. 0 ) CALL SLV_KILLW( CCDRID, STATUS )
      IF ( CCDGID .GT. 0 ) CALL SLV_KILLW( CCDGID, STATUS )
      IF ( KAPVID .GT. 0 ) CALL SLV_KILLW( KAPVID, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( 'CCDALIGN_ERR',
     :                  'CCDALIGN: failed to align CCD frames.',
     :                  STATUS )
      END IF
      END
