      SUBROUTINE IDICURS( STATUS )
*+                   
*  Name:             
*     IDICURS        
                     
*  Purpose:          
*     Reads coordinates from an X display device.
                     
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
*     display, and altering the percentile cutoff limits.  The position 
*     of the cursor is reported below the display using the coordinates 
*     of the Current coordinate frame for information, but the position 
*     list written out is always written in Pixel coordinates, since 
*     that is how all CCDPACK applications expect to find it written.
*     Points are marked on the image by clicking mouse button 1 
*     (usually the left one) and may be removed using mouse button 3
*     (usually the right one).  When you have marked all the points
*     that you wish to, click the 'Done' button.

*  Usage:
*     idicurs in outlist

*  ADAM Parameters:
*     IN = LITERAL (Read)
*        Gives the name of the NDF to display and get coordinates from.
*        If multiple NDFs are using wildcards or separating their names
*        with commas, you will be invited to mark points on each one
*        in turn.
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
*        region in which the NDF is displayed.  Note this is the 
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
*     OUTLIST = FILENAME (Write)
*        The name of the file which is to contain the selected
*        positions. The positions are written using the standard 
*        format in CCDPACK which is described in the notes section.
*        This list will become associated with the named NDF, and
*        may use modifications of the input NDF name.
*        [*.lis]
*     PERCENTILES( 2 ) = DOUBLE (Read and Write)
*        The low and high percentiles of the data range to use when 
*        displaying the images; any pixels with a value lower than 
*        the first value will have the same colour, and any with a value
*        higher than the second will have the same colour.  Must be in
*        the range 0 <= PERCENTILES( 1 ) <= PERCENTILES( 2 ) <= 100.
*        [2,98]
*     SHOWIND = LOGICAL (Read)
*        If true, then the index numbers will be displayed on the image
*        for each point marked, and it will be possible to choose 
*        the index number for each point.  If false, points will not
*        be numbered on the display, and will be written to the 
*        position list in (roughly) the order in which they are entered.
*        [FALSE]
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
*     ZOOM = INTEGER (Read and Write)
*        A factor giving the initial level to zoom in to the image 
*        displayed, that is the number of screen pixels to use for one
*        image pixel.  It will be rounded to one of the values 
*        ... 3, 2, 1, 1/2, 1/3 ....  The zoom can be changed 
*        interactively from within the program.  The initial value 
*        may be limited by MAXCANV.
*        [1]

*  Examples:

*  Notes:
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
*
*     - NDF extension items. 
*
*       On exit the CURRENT_LIST items in the CCDPACK extensions
*       (.MORE.CCDPACK) of the input NDFs are set to the name of the 
*       output list. These items will be used by other CCDPACK position 
*       list processing routines to automatically access the list.

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
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-APR-2000 (MBT):
*        Original version.
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
      PARAMETER ( MAXPOS = 99 )
                     
*  Status:           
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Length of string excluding trailing spaces
                     
*  Local Variables:  
      INTEGER FD                 ! FIO identifier of output file
      INTEGER ID( MAXPOS )       ! Identifiers for position list
      INTEGER INDEX              ! Index in list of NDFs
      INTEGER INDF               ! NDF identifier
      INTEGER LISTGR             ! GRP identifier for group of output files
      INTEGER MAXCNV             ! Initial maximum canvas dimension
      INTEGER NDFGR              ! NDG identifier of group of NDFs
      INTEGER NPOS               ! Number of positions in list
      INTEGER NRET               ! Number of returns
      INTEGER NNDF               ! Number of NDFs in group
      INTEGER WINDIM( 2 )        ! Dimensions of display window
      LOGICAL LOPEN              ! True if output file is open
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

*  Get display preference parameters from the parameter system.
      CALL PAR_GET0L( 'SHOWIND', SHOIND, STATUS )
      CALL PAR_EXACD( 'PERCENTILES', 2, PERCNT, STATUS )
      CALL PAR_GET0D( 'ZOOM', ZOOM, STATUS )
      CALL PAR_GET0I( 'MAXCANV', MAXCNV, STATUS )
      CALL PAR_GET0I( 'WINX', WINDIM( 1 ), STATUS )
      CALL PAR_GET0I( 'WINY', WINDIM( 2 ), STATUS )

*  Get a group of NDFs from the parameter system.
      CALL CCD1_NDFGL( 'IN', 1, CCD1__MXNDF, NDFGR, NNDF, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Get a group of output lists from the parameter system.
      CALL CCD1_STRGR( 'OUTLIST', NDFGR, NNDF, NNDF, LISTGR, NRET,
     :                 STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL CCD1_MSG( ' ', ' Output lists will not be written', 
     :                  STATUS )
      END IF

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

*  Invoke the Tcl code to do the work.
         CALL CCD1_TCURS( NDFNAM( 1:CHR_LEN( NDFNAM ) ), MAXPOS, SHOIND, 
     :                    PERCNT, ZOOM, MAXCNV, WINDIM, ID, XPOS, YPOS,
     :                    NPOS, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Access the output file in which to store the positions.  The name
*  of this file is stored in the LISTGR list of names.
         CALL GRP_GET( LISTGR, INDEX, 1, FNAME, STATUS )
         CALL CCD1_OPFIO( FNAME, 'WRITE', 'LIST', 0, FD, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 99
         LOPEN = .TRUE.

*  Write header to output position list file.
         CALL CCD1_FIOHD( FD, 'Output from IDICURS', STATUS )

*  Write position data to output list file.
         CALL CCD1_WRIXY( FD, ID, XPOS, YPOS, NPOS, LINE, CCD1__BLEN,
     :                    STATUS )

*  Report file used and number of entries.
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL FIO_FNAME( FD, FNAME, STATUS )
         CALL MSG_SETC( 'FNAME', FNAME )
         CALL MSG_SETI( 'NPOS', NPOS )
         CALL CCD1_MSG( ' ', 
     :                  '  ^NPOS entries written to file ^FNAME',
     :                  STATUS )

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

*  Delete NDG group.
      CALL GRP_DELET( NDFGR, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL CCD1_ERREP( 'IDICURS_ERR', 
     :                    'IDICURS: Error in cursor program', STATUS )
      END IF

*  Close the log file.
      CALL CCD1_END( STATUS )

      END

* $Id$
