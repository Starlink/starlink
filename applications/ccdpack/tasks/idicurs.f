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
*     This program displays an image on the screen and provides a 
*     graphical user interface for marking points on it, which are then
*     written out to a file as a position list.
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
*
*     Optionally (if the READLIST parameter is set true), a list of 
*     points may be read at the start of the program; these will be 
*     marked on the image as an initial set which may be added to or
*     removed as if marked by the user in the first place.

*  Usage:
*     idicurs in outlist

*  ADAM Parameters:
*     IN = LITERAL (Read)
*        Gives the name of the NDF to display and get coordinates from.
*        If multiple NDFs are specified using wildcards or separating 
*        their names with commas, the program will run on each one in
*        turn.
*     INEXT = _LOGICAL (Read)
*        If the READLIST parameter is set true, then this parameter 
*        determines where the input position list comes from.  If set
*        true, then the position list currently associated with the
*        NDF will be used.  If set false, then the input position list
*        names will be obtained from the INLIST parameter.
*        [FALSE]
*     INLIST = FILENAME (Read)
*        If the READLIST parameter is set true, and the INEXT parameter
*        is set false, then this parameter gives the name of the file
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
*        If set true, then the program will start up with a set of points
*        displayed, read from the list specified by the INLIST parameter.
*        If set false, the program will start up with no points initially
*        plotted.
*        [FALSE]
*     OUTLIST = FILENAME (Write)
*        The name of the file which is to contain the selected
*        positions.  If the program exits normally the positions will
*        be written to this file using the standard CCDPACK format,
*        as described in the Notes section, and the list file will 
*        become associated with the input NDF.  The value of this 
*        parameter may use modifications of the input NDF name.
*        If set null (!), no output file will be written.  This
*        parameter is allowed to take the same value as INLIST, in
*        which case the list will be overwritten.
*        [*.lis]
*     PERCENTILES( 2 ) = _DOUBLE (Read and Write)
*        The initial values for the low and high percentiles of the data 
*        range to use when displaying the images; any pixels with a value
*        lower than the first element will have the same colour, and any 
*        with a value higher than the second will have the same colour.
*        Must be in the range 0 <= PERCENTILES( 1 ) <= PERCENTILES( 2 ) 
*        <= 100.  These values can be changed interactively while the
*        program runs.
*        [2,98]
*     SHOWIND = _LOGICAL (Read)
*        If true, then the index numbers will be displayed on the image
*        for each point marked, and it will be possible to choose 
*        the index number for each point.  If false, points will not
*        be numbered on the display, and will be written to the 
*        position list in (roughly) the order in which they are entered.
*        [FALSE]
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
*     idicurs in=* out=*.pts percentiles=[10,90]
*        Each of the NDFs in the current directory will be displayed, 
*        and the positions marked on it written to a list with the same
*        name as the image but the extension '.pts', which will be 
*        associated with the image in question.  The display will
*        initially be scaled so that pixels with a value higher than
*        the 90th percentile will all be displayed as the brightest
*        colour and those with a value lower than the 10th percentile
*        as the dimmest colour, but this may be changed interactively
*        while the program is running.
*
*     idicurs in=gc6253 showind readlist inlist=found.lis outlist=out.lis
*        The image gc6253 will be displayed, with the points stored in
*        the position list 'found.lis' already plotted on it.  These 
*        may be added to, moved and deleted, and the resulting list
*        will be written to the file out.lis.

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
*     Certain parameters (LOGTO and LOGFILE) have global values. These
*     global values will always take precedence, except when an
*     assignment is made on the command line.  Global values may be set
*     and reset using the CCDSETUP and CCDCLEAR commands.
*
*     Some of the parameters (MAXCANV, PERCENTILES, WINX, WINY, ZOOM)
*     give initial values for quantities which can be modified while
*     the program is running.  Although these may be specified on the
*     command line, it is normally easier to start the program up and
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
      INTEGER DUMGR              ! Dummy GRP identifier
      INTEGER FD                 ! FIO identifier of output file
      INTEGER ID( MAXPOS )       ! Identifiers for position list
      INTEGER ILSTGR             ! GRP identifier for input position list files
      INTEGER INDEX              ! Index in list of NDFs
      INTEGER INDF               ! NDF identifier
      INTEGER IPDAT              ! Pointer to input position list coordinates
      INTEGER IPIND              ! Pointer to input position list indices
      INTEGER MAXCNV             ! Initial maximum canvas dimension
      INTEGER NDFGR              ! NDG identifier of group of NDFs
      INTEGER NF                 ! Number of fields per line of input list
      INTEGER NFIELD             ! Number of fields in first line of input file
      INTEGER NPOS               ! Number of positions in list
      INTEGER NRET               ! Number of returns
      INTEGER NNDF               ! Number of NDFs in group
      INTEGER OLSTGR             ! GRP identifier for output position list files
      INTEGER WINDIM( 2 )        ! Dimensions of display window
      LOGICAL INEXT              ! True if input position list is in extension
      LOGICAL LOPEN              ! True if output file is open
      LOGICAL RDLIST             ! True if using initial position lists
      LOGICAL SHOIND             ! True if index numbers are to be plotted
      DOUBLE PRECISION PERCNT( 2 ) ! Low and high percentiles for display
      DOUBLE PRECISION XPOS( MAXPOS ) ! X coordinates of positions in list
      DOUBLE PRECISION YPOS( MAXPOS ) ! Y coordinates of positions in list
      DOUBLE PRECISION ZOOM      ! Zoom factor for display
      CHARACTER * ( CCD1__BLEN ) LINE ! Buffer for line output to file
      CHARACTER * ( GRP__SZNAM ) NDFNAM ! Name of NDF
      CHARACTER * ( GRP__SZFNM ) FNAME ! Name of output list file

*.
                     
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Execute startup.
      CALL CCD1_START( 'IDICURS', STATUS )
      LOPEN = .FALSE.

*  Begin NDF context.
      CALL NDF_BEGIN
      NDFGR = GRP__NOID

*  Determine if we will be using initial position lists.
      CALL PAR_GET0L( 'READLIST', RDLIST, STATUS )

*  If so, get a group of NDFs and a group of input position lists 
*  from the parameter system.
      IF ( RDLIST ) THEN

*  See if the input list is to be determined from the NDF extension.
         CALL PAR_GET0L( 'INEXT', INEXT, STATUS )

*  Get the NDFs and input position lists.
         IF ( INEXT ) THEN
            CALL CCD1_GTLIG( .TRUE., 'CURRENT_LIST', 'IN', 1, 
     :                       CCD1__MXNDF, NNDF, ILSTGR, NDFGR, STATUS )
         ELSE
            CALL CCD1_NDFGL( 'IN', 1, CCD1__MXNDF, NDFGR, NNDF, STATUS )
            CALL CCD1_GTLIG( .FALSE., ' ', 'INLIST', NNDF, NNDF, NRET,
     :                       ILSTGR, DUMGR, STATUS )
            CALL CCD1_GRDEL( DUMGR, STATUS )
         END IF

*  Otherwise, just get a group of NDFs.
      ELSE
         CALL CCD1_NDFGL( 'IN', 1, CCD1__MXNDF, NDFGR, NNDF, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Get a group of output lists from the parameter system.
      CALL CCD1_STRGR( 'OUTLIST', NDFGR, NNDF, NNDF, OLSTGR, NRET,
     :                 STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         OLSTGR = GRP__NOID
         CALL CCD1_MSG( ' ', '  Output lists will not be written', 
     :                  STATUS )
      END IF

*  Get display preference parameters from the parameter system.
      CALL PAR_GET0L( 'SHOWIND', SHOIND, STATUS )
      CALL PAR_EXACD( 'PERCENTILES', 2, PERCNT, STATUS )
      CALL PAR_GET0D( 'ZOOM', ZOOM, STATUS )
      CALL PAR_GET0I( 'MAXCANV', MAXCNV, STATUS )
      CALL PAR_GET0I( 'WINX', WINDIM( 1 ), STATUS )
      CALL PAR_GET0I( 'WINY', WINDIM( 2 ), STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Issue instructions about how to interact with the GUI.
      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( ' ', '   Click on the image with mouse button 1 '
     :              // '(left) to mark a point', STATUS )
      CALL MSG_OUT( ' ', '                      with mouse button 3 '
     :              // '(right) to erase a point', STATUS )
      CALL MSG_OUT( ' ', '   and click the "Done" button '
     :              // 'when finished.', STATUS )

*  Loop over each selected NDF in turn.
      DO 1 INDEX = 1, NNDF

*  Get the name of the NDF.
         CALL GRP_GET( NDFGR, INDEX, 1, NDFNAM, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Log to user.
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL MSG_SETC( 'NDF', NDFNAM )
         CALL CCD1_MSG( ' ', '  Displaying NDF ^NDF', STATUS )

*  Unless we read some in, there will be no positions in the initial 
*  position list.
         NPOS = 0

*  If we are using an initial position list, we will have to read the 
*  positions in.
         IF ( RDLIST ) THEN

*  Open the input file and test the number of entries.
            CALL GRP_GET( ILSTGR, INDEX, 1, FNAME, STATUS )
            CALL CCD1_OPFIO( FNAME, 'READ', 'LIST', 0, FD, STATUS )
            CALL CCD1_LTEST( FD, LINE, CCD1__BLEN, 2, 0, NFIELD, 
     :                       STATUS )

*  Map in X and Y positions only (non-standard file).
            IF ( NFIELD .EQ. 2 ) THEN
               CALL CCD1_NLMAP( FD, LINE, CCD1__BLEN, IPDAT, NPOS,
     :                          NF, STATUS )

*  Standard file format map these in.
            ELSE
               CALL CCD1_LMAP( FD, LINE, CCD1__BLEN, IPIND, IPDAT,
     :                         NPOS, NF, STATUS )
            END IF

*  Close the input file.
            CALL FIO_CLOSE( FD, STATUS )

*  Log to user.
            CALL MSG_SETI( 'NPOS', NPOS )
            CALL MSG_SETC( 'ILIST', FNAME )
            CALL CCD1_MSG( ' ', 
     :      '    Read ^NPOS positions from file ^ILIST.', STATUS )

*  If there are too many points in the input position list then prepare
*  to truncate it.
            IF ( NPOS .GT. MAXPOS ) THEN
               CALL CCD1_MSG( ' ', ' ', STATUS )
               CALL CCD1_MSG( ' ', 
     :         '    Warning - too many points in input list,', STATUS )
               CALL MSG_SETI( 'NPOS', NPOS )
               CALL MSG_SETI( 'MAX', MAXPOS )
               CALL CCD1_MSG( ' ',
     :        '               using only the first ^NPOS/^MAX', STATUS )
               NPOS = MAXPOS
            END IF

*  Copy the points so they can be read by the GUI routine, and free the 
*  temporarily allocated memory.
            IF ( NFIELD .EQ. 2 ) THEN
               CALL CCD1_GISEQ( 1, 1, NPOS, ID, STATUS )
            ELSE
               CALL CCG1_COPAI( NPOS, %VAL( IPIND ), ID, STATUS )
               CALL CCD1_MFREE( IPIND, STATUS )
            END IF
            CALL CCD1_LEXT( %VAL( IPDAT ), NPOS, NF, 1, XPOS, STATUS )
            CALL CCD1_LEXT( %VAL( IPDAT ), NPOS, NF, 2, YPOS, STATUS )
            CALL CCD1_MFREE( IPDAT, STATUS )
         END IF
         

*  Invoke the Tcl code to do the work.
         CALL CCD1_TCURS( NDFNAM( 1:CHR_LEN( NDFNAM ) ), MAXPOS, SHOIND, 
     :                    PERCNT, ZOOM, MAXCNV, WINDIM, ID, XPOS, YPOS,
     :                    NPOS, STATUS )

*  Access the output file in which to store the positions.  The name
*  of this file is stored in the OLSTGR list of names.
         IF ( OLSTGR .NE. GRP__NOID ) THEN
            CALL GRP_GET( OLSTGR, INDEX, 1, FNAME, STATUS )
            CALL CCD1_OPFIO( FNAME, 'WRITE', 'LIST', 0, FD, STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 99
            LOPEN = .TRUE.

*  Write header to output position list file.
            CALL CCD1_FIOHD( FD, 'Output from IDICURS', STATUS )

*  Write position data to output list file.
            CALL CCD1_WRIXY( FD, ID, XPOS, YPOS, NPOS, LINE, CCD1__BLEN,
     :                       STATUS )

*  Report file used and number of entries.
            CALL FIO_FNAME( FD, FNAME, STATUS )
            CALL MSG_SETC( 'FNAME', FNAME )
            CALL MSG_SETI( 'NPOS', NPOS )
            CALL CCD1_MSG( ' ', 
     :                     '    Wrote ^NPOS positions to file ^FNAME.',
     :                     STATUS )

            IF ( NPOS .GT. 0 .AND. STATUS .EQ. SAI__OK ) THEN

*  Get an identifier for the NDF.
               CALL NDG_NDFAS( NDFGR, INDEX, 'UPDATE', INDF, STATUS )

*  Update the extension with the name of the list file.
               CALL CCG1_STO0C( INDF, 'CURRENT_LIST', FNAME, STATUS )

*  Release the NDF.
               CALL NDF_ANNUL( INDF, STATUS )

*  If there was a problem (most likely that the NDF was not writable),
*  warn about this and continue.
               IF ( STATUS .NE. SAI__OK ) THEN 
                  CALL MSG_SETC( 'NDF', NDFNAM )
                  CALL MSG_SETC( 'FNAME', FNAME )
                  CALL CCD1_ERREP( 'IDICURS_NOUPD', 
     :'IDICURS: Failed to update ^NDF.MORE.CCDPACK.CURRENT_LIST ' //
     :'with value ''^FNAME''.', STATUS )
                  CALL ERR_ANNUL( STATUS )
               END IF
            END IF
         END IF
 1    CONTINUE

*  Write display preference parameters back to the parameter system.
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      CALL PAR_PUT0D( 'ZOOM', ZOOM, STATUS )
      CALL PAR_PUT0I( 'MAXCANV', MAXCNV, STATUS )
      CALL PAR_PUT0I( 'WINX', WINDIM( 1 ), STATUS )
      CALL PAR_PUT0I( 'WINY', WINDIM( 2 ), STATUS )
      CALL PAR_PUT1D( 'PERCENTILES', 2, PERCNT, STATUS )

*  Exit on error label.
 99   CONTINUE

*  Close output file.
      IF ( LOPEN ) CALL FIO_CLOSE( FD, STATUS )

*  End NDF context.
      CALL NDF_END( STATUS )

*  Release group resources.
      CALL CCD1_GRDEL( NDFGR, STATUS )
      CALL CCD1_GRDEL( ILSTGR, STATUS )
      CALL CCD1_GRDEL( OLSTGR, STATUS )

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
