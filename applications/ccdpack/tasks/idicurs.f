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
*     This routine reports and records positions selected from an 
*     X display device. Before using this routine an image must 
*     have been displayed using a routine such as KAPPA DISPLAY (SUN/95). 
*     IDICURS allows displayed images and graphics to be zoomed 
*     and scrolled during the location operation.

*  Usage:
*     idicurs outlist device

*  ADAM Parameters:
*     ARROWS = _LOGICAL (Read)
*        Only used if SCROLL is TRUE. This parameter defines whether or
*        not the keyboard arrows are used to control the display scroll.
*        Using the keyboard increases the accuracy of positioning but is
*        slower. Pressing Shift-Arrow-Key increases the rate of scroll.
*        [FALSE]
*     COLOUR = _INTEGER (Read)
*        The colour of the position marker. This may take any value in
*        the range 0-4. 
*        [4]
*     DEVICE = DEVICE (Read)
*        The name of the image display device.
*        [Current-image display device]
*     IN = LITERAL (Read)
*        The names of a list of NDFs which are to be "associated" with
*        the output position list. Associating a list with an NDF means
*        that the item "CURRENT_LIST" in its CCDPACK extension will be
*        set to the name of the list. In future applications the NDF
*        name may then be used instead of the list name (any derived
*        lists will then be associated with the NDF instead), this is
*        the usual method in CCDPACK.
*
*        The names of the NDFs may be given as "!" in which case none
*        will be associated, otherwise the NDF names should be as a list 
*        separated by commas and may include wildcards.
*        [!]
*     KEEPLUT = _LOGICAL (Read)
*        If TRUE then the Look-Up-Table of the current device will be
*        used "as is". Otherwise a greyscale will be loaded. The
*        visibility of the cursor when using X windows and with
*        scrolling enabled is often a problem. To help with this the
*        primary colours are loaded into the first fews pens.
*        [TRUE]
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
*     MEMORY = _INTEGER (Read)
*        The memory of the device which is to be used. This can take
*        the values 0 and 1. 0 means the base memory and 1 the overlay.
*        [0]
*     MSIZE = _REAL (Read)
*        Determines the size of the position marker. This is set as a
*        fraction of the X dimension of the display.
*        [0.03]
*     OUTLIST = FILENAME (Write)
*        The name of the file which is to contain the selected
*        positions. The positions are written using the
*        standard format in CCDPACK which is described in the notes
*        section.
*        [IDICURS.LIS]
*     SCROLL = _LOGICAL (Read)
*        If TRUE then scrolling of the image display is enabled. This
*        may be very slow on devices with poor graphics facilities
*        and/or slow processors so is normally disabled.
*        [FALSE]
*     THICK = _INTEGER (Read)
*        The thickness of the position marker in device pixels.
*        [1]

*  Examples:
*     idicurs stars.pos
*        In this example IDICURS writes the output positions to the
*        formatted file stars.pos.
*
*     idicurs device=xw outlist=objects.dat memory=1
*        In this example the overlay plane of an xwindows display
*        can be manipulated and coordinates are read. The overlay
*        could contain a contour map as produced by KAPPA TURBOCONT.
*
*     idicurs outlist=group_positions in='*'
*        In this example an output position list group_positions is
*        associated with all the NDFs in the current directory. This
*        method could be useful if all the NDFs are not moved
*        significantly with respect to one another. This one list then
*        supplies initial positions for objects in all these NDFs (using
*        the centroid routine FINDCENT would then give each NDF an
*        accurate position list from these initial positions).
*
*     idicurs scroll arrows
*        In this example scrolling via the use of the keyboard arrows is
*        enabled.

*  Implementation Deficiencies:
*     -  Support is only given to coordinate systems which increase
*        linearly from left to right and bottom to top.  A method of
*        support for other system similar to that used in KAPPA should
*        be implemented.

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
*     - X windows display.
*
*       It is possible to scroll the memory using either the pointer or
*       keyboard arrows (if the keyboard arrows are chosen then holding
*       down the shift keys will accelerate the motion) parameters
*       SCROLL and ARROWS control these options. The C key performs a
*       quick re-centre, cancelling any zoom and scroll. The Q key
*       is used to exit from the routine.
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
*     NE: Nick Eaton  (Durham University)
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     10-JUL-1992 (NE + PDRAPER):
*        Original version, based on IDI example routine.
*     10-AUG-1992 (PDRAPER):
*        Added prologue, user parameters, markers & rearranged triggers.
*     10-MAR-1993 (PDRAPER):
*        Final version for NAM demo, incorporated X and IKON support
*        into one routine. Enabled scrolling and LUT control.
*     19-JUL-1993 (PDRAPER):
*        Updated prologue.
*     9-OCT-1995 (PDRAPER):
*        Updated for CCDPACK 2.0.
*     3-MAR-1997 (PDRAPER):
*        Removed top-level locator controls (foreign data access upgrade).
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}
                     
*-                   
                     
*  Type Definitions: 
      IMPLICIT NONE              ! No implicit typing
                     
*  Global Constants: 
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameterisations
      INCLUDE 'FIO_PAR'          ! FIO parameters
      INCLUDE 'IDI_ERR'          ! IDI error codes
      INCLUDE 'IDI_PAR'          ! IDI global constants
      INCLUDE 'PAR_ERR'          ! Parameter system codes
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameterisations
                     
*  Status:           
      INTEGER STATUS             ! Global status
                     
*  Local Variables:  
      CHARACTER * ( 10 ) PNAME   ! AGI database Picture name
      CHARACTER * ( 80 ) MESS    ! Buffer for output message string
      CHARACTER * ( FIO__SZFNM ) FNAME ! Output file name
      INTEGER BASEID             ! First picture identifier
      INTEGER CURSOR             ! Cursor shape parameter
      INTEGER EXTRN              ! Exit trigger number
      INTEGER FD                 ! FIO file descriptor
      INTEGER FRAC               ! Length of half-side of marker
      INTEGER I                  ! Loop variable
      INTEGER IAT                ! Position in string
      INTEGER ICOL               ! Marker colour index
      INTEGER ID                 ! Display identifier
      INTEGER IDIN               ! NDF identifier
      INTEGER INTID              ! Interactor identifier
      INTEGER INTOP              ! Interactive option
      INTEGER INTTY              ! Interactor type
      INTEGER ISTAT              ! IDI status
      INTEGER J                  ! Loop variable
      INTEGER MEMDEP( 2 )        ! Memory depths
      INTEGER MEMID              ! Memory plane identifier
      INTEGER MEMITT( 2 )        ! Memory ITT depths
      INTEGER MESLEN             ! Output message length
      INTEGER MIDS( 2 )          ! List of memory identifiers
      INTEGER MODCON             ! Configuration mode
      INTEGER MSIZEX( 2 )        ! Memory sizes in X
      INTEGER MSIZEY( 2 )        ! Memory sizes in Y
      INTEGER NDFGR              ! Input NDFs group identifier
      INTEGER NNDF               ! Number of input NDFs
      INTEGER NOBJ               ! Current location number (object identifier)
      INTEGER NOUT               ! Number of returns
      INTEGER NPRESS             ! Number of presses of unzoom trigger in zoom zero state
      INTEGER NRET               ! Number of returned values
      INTEGER NTRIGS             ! Number of triggers available
      INTEGER NUMCUR             ! Cursor identifier
      INTEGER NUMMEM             ! Number of memories
      INTEGER NVAL               ! Number of capabilities returned
      INTEGER OBJID              ! Object identifier (interactor)
      INTEGER OBJTY              ! Object type (interactor)
      INTEGER OUTMID             ! Output memory identifier
      INTEGER PENS( 1 )          ! Number of pens in LUT
      INTEGER PICID              ! AGI picture identifier
      INTEGER THICK              ! LIne thickness
      INTEGER TRIGS( IDI__MAXTR ) ! IDI triggers
      INTEGER USEMEM             ! Memory to use (from user)
      INTEGER XC                 ! X position of cursor (device pixels)
      INTEGER XOFF               ! X offset of initial image position
      INTEGER XORG               ! X origin of initial image position
      INTEGER XSIZE              ! X size of image (device pixels)
      INTEGER YC                 ! Y position of cursor (device pixels)
      INTEGER YOFF               ! Y offset of initial image position
      INTEGER YORG               ! Y origin of initial image position
      INTEGER YSIZE              ! Y size of image (device pixels)
      INTEGER ZOOM               ! Current zoom factor
      INTEGER ZRANGE( 2 )        ! Range of possible zooms 
      LOGICAL ARROWS             ! Use arrows keys for scrolling
      LOGICAL GOTNDF             ! Have input NDFs
      LOGICAL KEYOK              ! True if enough triggers present to allow keyboard
      LOGICAL LOPEN              ! Flag indicating that output file is opened
      LOGICAL SCRLOK             ! Can scroll (and have message)
      LOGICAL USELUT             ! Whether to use current LUT or load own
      REAL COLS( 3, 5 )          ! Available colours.
      REAL CRSIZE                ! Fraction of width (half marker size)
      REAL GREY( 3, 512 )        ! Look-up-table
      REAL SCALE                 ! Scale factor for levels to pens
      REAL WX1                   ! X World coordinates of bottom left-hand corner of image
      REAL WX2                   ! X World coordinates of top right-hand corner of image
      REAL WY1                   ! Y World coordinates of bottom left-hand corner of image
      REAL WY2                   ! Y World coordinates of top right-hand corner of image
      REAL XPOS                  ! Position of cursor in world coordinates
      REAL XSCALE                ! Scaling factor for device to world pixels
      REAL YPOS                  ! Position of cursor in world coordinates
      REAL YSCALE                ! Scaling factor for device to world coordinates

*  Data:          R    G    B
      DATA COLS/ 0.0, 0.0, 0.0,  ! Black
     :           1.0, 1.0, 1.0,  ! White
     :           1.0, 0.0, 0.0,  ! Red
     :           0.0, 1.0, 0.0,  ! Green
     :           0.0, 0.0, 1.0/  ! Blue

*.
                     
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Execute startup.
      CALL CCD1_START( 'IDICURS', STATUS )

*  Set the IDI status flag.
      ISTAT = IDI__OK

*  Initialise "exit" action flags.
      GOTNDF = .FALSE.
      LOPEN = .FALSE.
      XOFF = 0
      YOFF = 0
      ZOOM = 0

*  Start an NDF context.
      CALL NDF_BEGIN
                     
*  Open AGI, IDI and the device.
      CALL AGI_ASSOC( 'DEVICE', 'READ', BASEID, STATUS )
      CALL AGI_BEGIN 
      CALL AGD_ACTIV( STATUS )
      IF ( STATUS .NE. IDI__OK ) GOTO 99
                     
*  Look for a 'DATA' picture if the current one isn't.
      CALL AGI_INAME( PNAME, STATUS )
      IF ( PNAME .NE. 'DATA' ) THEN
                     
*  Find the most recently used one.
         CALL AGI_RCL( 'DATA', PICID, STATUS )

*  If a data picture cannot be found just use whatever we've been given.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            CALL MSG_OUT( 'NODATAPIC', '  Warning - Unable to locate'//
     :      ' a picture of type DATA within the current picture',
     :      STATUS )
            CALL AGI_SELP( BASEID, STATUS )
         END IF         
      END IF         

*  Set the initial IDI "viewport" for this picture.
      MEMID = 0      
      CALL AGD_NWIND( MEMID, ID, XSIZE, YSIZE, XORG, YORG, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Find which memory the user wants to manipulate. First get the number
*  of memories. Assuming only one configuration, memories may be image,
*  text or graphics, allowing only two memories (1+overlay).
      CALL IIDQDC( ID, 0, 7, 2, MODCON, MIDS, MSIZEX, MSIZEY, MEMDEP, 
     :             MEMITT, NUMMEM, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99
      IF ( NUMMEM .EQ. 2 ) THEN 

*  User may want to read from overlay.
         CALL PAR_GET0I( 'MEMORY', USEMEM, STATUS )
         USEMEM = MAX( 0, MIN( USEMEM, NUMMEM ) )
         IF ( USEMEM .NE. MEMID ) THEN 

*  Change of memory, get new AGI window.
            MEMID = USEMEM
            CALL AGD_NWIND( MEMID, ID, XSIZE, YSIZE, XORG, YORG, 
     :                      STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 99
         END IF
      END IF
                     
*  Trap bad picture sizes.
      IF ( XSIZE .EQ. 0 .OR. YSIZE .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'IDICURS_NOEX',
     :   '  The AGI DATA picture has no extent', STATUS )
         GO TO 99
      END IF

*  Get the colour of the cursor and markers.
      CALL PAR_GET0I( 'COLOUR', ICOL, STATUS )
      ICOL = MAX( 0, MIN( ICOL , 5 ) )

*  Find out if the user wants to keep the current look-up table.
      CALL PAR_GET0L( 'KEEPLUT', USELUT, STATUS )

*  Access the output positions list.
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      CALL CCD1_ASFIO( 'OUTLIST', 'WRITE', 'LIST', 0, FD, LOPEN,
     :                 STATUS )

*  Trap the occasion when the user doesn't want an output file. This
*  is indicated by a PAR__NULL return.
      IF ( STATUS .EQ. PAR__NULL ) THEN 
         CALL ERR_ANNUL( STATUS )
         LOPEN = .FALSE.
      END IF

*  Add the title etc.
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      IF ( LOPEN ) THEN
         CALL CCD1_FIOHD( FD, 'Output from IDICURS', STATUS )

*  Try to access an NDF if an output list is to be written. The CCDPACK 
*  extension of this will be updated to contain the name of the output file.
         CALL CCD1_NDFGL( 'IN', 'UPDATE', 1, CCD1__MXNDF, NDFGR, NNDF,
     :                    STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
         ELSE
            GOTNDF = .TRUE.
         END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  IDI setup............................................................
*  Load a Look-Up-Table to set suitable colours. Get the number of
*  pens in the LUT
      CALL IIDQCI( ID, INLUTC, 1, PENS, NRET, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99
      IF ( PENS( 1 ) .GT. 16 ) THEN 

*  Check the background colour.
         CALL IILRLT( ID, MEMID, 0, 1, COLS, ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
         IF ( COLS( 1, 1 ) .NE. 0.0 .OR. COLS( 2, 1 ) .NE. 0.0
     :       .OR. COLS( 3, 1 ) .NE. 0.0 ) THEN

*  First pen is not black so set second pen black.
            COLS( 1, 2 ) = 0.0
            COLS( 2, 2 ) = 0.0
            COLS( 3, 2 ) = 0.0
         END IF

*  Now load the primary colour LUT into the main LUT
         DO 23 I = 1, 5
            GREY( 1, I ) = COLS( 1, I ) 
            GREY( 2, I ) = COLS( 2, I ) 
            GREY( 3, I ) = COLS( 3, I ) 
 23      CONTINUE         
         IF ( ISTAT .NE. IDI__OK ) GO TO 99

*  Fill the rest with a greyscale if asked.
         IF ( .NOT. USELUT ) THEN
            SCALE = 256.0 / REAL( PENS( 1 ) - 6 )
            J = 0 
            DO 15 I = 6, PENS( 1 )
               GREY( 1, I ) = ( REAL( J ) * SCALE ) / 256.0
               GREY( 2, I ) = ( REAL( J ) * SCALE ) / 256.0
               GREY( 3, I ) = ( REAL( J ) * SCALE ) / 256.0
               J = J + 1
 15         CONTINUE
         ELSE
            PENS( 1 ) = 5
         END IF

*  And load the look-up-table.
         CALL IILWLT( ID, MEMID, 0, PENS( 1 ), GREY, ISTAT )
      ELSE

*  Not many pens in this display.
        IF ( PENS( 1 ) .GT. 1 ) THEN 

*  And fill with a greyscale if asked.
            IF ( .NOT. USELUT ) THEN
               SCALE = 256.0D0 / DBLE( PENS( 1 ) )
               J = 0
               DO 16 I = 1, PENS( 1 )
                  GREY( 1, I ) = ( DBLE( J ) * SCALE ) / 256.0D0
                  GREY( 2, I ) = ( DBLE( J ) * SCALE ) / 256.0D0
                  GREY( 3, I ) = ( DBLE( J ) * SCALE ) / 256.0D0
                  J = J + 1
 16            CONTINUE
               CALL IILWLT( ID, MEMID, 0, PENS( 1 ), GREY, ISTAT )
            END IF
         END IF
      END IF

*   Display the memory by setting its visibility to .TRUE.
      CALL IIMSMV( ID, MEMID, 1, 1, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99

*  Get the number of triggers which are available. At present Xwindows
*  is the only device with a keyboard extension of the trigger set.
      KEYOK = .FALSE.
      NVAL = 1
      CALL IIDQCI( ID, INTRIG, NVAL, NTRIGS, NOUT, ISTAT )
      IF ( NTRIGS .GT. 3 ) THEN

*  The keyboard is available.
         KEYOK = .TRUE.

*  Do we want to scroll when zoomed?
         CALL PAR_GET0L( 'SCROLL', SCRLOK, STATUS )

*  If we're going to scroll do we want to control it using the mouse
*  (locator) or the keyboard arrows.
         CALL PAR_GET0L( 'ARROWS', ARROWS, STATUS )
      END IF

*  Set the type of  required, cross hair full screen by default
*  unless we're using X and have been told not to scroll.
      IF ( KEYOK .AND. SCRLOK ) THEN
         CURSOR = 1              ! Cross hair full screen on X
      ELSE
         CURSOR = 0              ! Default
      END IF

*  Move the cursor to the middle of the screen. A memory id = -1 sets
*  the cursor position relative to the screen origin.
      XC = XORG + XSIZE / 2
      YC = YORG + YSIZE / 2
      NUMCUR = 0
      CALL IICINC( ID, -1, NUMCUR, CURSOR, ICOL, XC, YC, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99
                     
*  Display the cursor by setting its visibility to .TRUE.
      CALL IICSCV( ID, NUMCUR, 1, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99
                     
*  Inquire the range of zoom factors using Query Capabilities
*  Integer code 17 ( IZOOMR ) returns the zoom range
      CALL IIDQCI( ID, IZOOMR, 2, ZRANGE, NVAL, ISTAT )
      ZRANGE(1)=MIN( ZRANGE(1)+1, 0 )
      ZRANGE(2)=MAX( ZRANGE(2)-1, 0 )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99
                     
*  Inquire the zoom factor and current scroll.
      CALL IIZRSZ( ID, MEMID, XOFF, YOFF, ZOOM, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99

*  Set the exit trigger number. This must not exceed the maximum
*  number of triggers (IKONs).
      EXTRN = NTRIGS - 1

*  Start to Inform user of the choices that they have.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ',
     : '      Use of Buttons:', STATUS )

*  Sets left-hand button to execute application specific code
*  (read cursor).
      INTTY = 5
      INTID = 0
      OBJTY = 0
      OBJID = 0
      INTOP = 0
      CALL IIIENI( ID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :             ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99
      MESS = ' '
      CALL IIIQID( ID, INTTY, INTID, MESS, MESLEN, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99
      MESS( MAX( 23, MESLEN + 1 ): ) = ' - record position'
      CALL MSG_SETC( 'MESS', MESS )
      CALL CCD1_MSG( ' ', '         ^MESS', STATUS )
                      
*  Sets to execute application specific code (zoom in).
      INTTY = 5
      INTID = 1
      OBJTY = 0
      OBJID = 0
      INTOP = 0
      CALL IIIENI( ID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :             ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99
      MESS = ' '
      CALL IIIQID( ID, INTTY, INTID, MESS, MESLEN, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99
      MESS( MAX( 23, MESLEN + 1 ): ) = ' - increase zoom'
      CALL MSG_SETC( 'MESS', MESS )
      CALL CCD1_MSG( ' ', '         ^MESS', STATUS )
               
*  Sets right-hand button to execute application specific code
*  (unzoom).
      INTTY = 5
      INTID = 2
      OBJTY = 0
      OBJID = 0
      INTOP = 0
      CALL IIIENI( ID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :             ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99
      MESS = ' '
      CALL IIIQID( ID, INTTY, INTID, MESS, MESLEN, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99
      MESS( MAX( 23, MESLEN + 1 ): ) = ' - decrease zoom'
      CALL MSG_SETC( 'MESS', MESS )
      CALL CCD1_MSG( ' ', '         ^MESS', STATUS )

*  Message about other standard options (all driven from right button).
      MESS = ' '
      CALL IIIQID( ID, INTTY, INTID, MESS, MESLEN, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99
      MESS( MESLEN + 1: ) = ' once in unzoomed state to re-centre'
      CALL MSG_SETC( 'MESS', MESS )
      CALL CCD1_MSG( ' ', '      ^MESS', STATUS )

*  Disable standard option of exiting via multiple presses of the 
*  right-hand button, if the keyboard can be used to supply this 
*  functionality.
      IF ( .NOT. KEYOK ) THEN 
         CALL IIIQID( ID, INTTY, INTID, MESS, MESLEN, ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
         MESS( MESLEN + 1: ) = ' twice in unzoomed state to exit'
         CALL MSG_SETC( 'MESS', MESS )
         CALL CCD1_MSG( ' ', '      ^MESS', STATUS )
      END IF

      IF ( KEYOK ) THEN 
*  If available and requested set up an interaction to scroll the memory
*  and an interaction to exit.

*  Inform the user additional operations are available.
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ', '      The following options are also '//
     :   'available on this device:', STATUS )

*  If scrolling is allowed then.
         IF ( SCRLOK ) THEN

*  Set up an interaction to scroll the memory (over a fixed cursor -
*  this only works for the X implementation).
            INTTY = 0
            IF ( ARROWS ) THEN
               INTID = 1
            ELSE
               INTID = 0
            END IF
            OBJTY = 5
            OBJID = MEMID  
            INTOP = 1
            CALL IIIENI( ID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :                   ISTAT )
            IF ( ISTAT .NE. IDI__OK ) GO TO 99
            MESS = ' '
            CALL IIIQID( ID, INTTY, INTID, MESS, MESLEN, ISTAT )
            IF ( ISTAT .NE. IDI__OK ) GO TO 99
            MESS( MAX( 23, MESLEN + 1 ): ) = ' - scroll memory'
            CALL MSG_SETC( 'MESS', MESS )
            CALL CCD1_MSG( ' ', '         ^MESS', STATUS )
         END IF

*  Set up an interaction to unzoom and unscroll use a C keyboard press.
         INTTY = 5
         INTID = 16              ! C
         OBJTY = 0
         OBJID = 0
         INTOP = 0
         CALL IIIENI( ID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :                ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
         MESS = ' '
         CALL IIIQID( ID, INTTY, INTID, MESS, MESLEN, ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
         MESS( MAX( 23, MESLEN + 1 ): ) = ' - to cancel zoom and scroll'
         CALL MSG_SETC( 'MESS', MESS )
         CALL CCD1_MSG( ' ', '         ^MESS', STATUS )

*  Set up an interaction to exit quickly. Use a Q keyboard press.
         INTTY = 5
         INTID = 30              ! Q
         OBJTY = 0
         OBJID = 0
         INTOP = 0
         CALL IIIENI( ID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :                ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
         MESS = ' '
         CALL IIIQID( ID, INTTY, INTID, MESS, MESLEN, ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
         MESS( MAX( 23, MESLEN + 1 ): ) = ' - to exit '
         CALL MSG_SETC( 'MESS', MESS )
         CALL CCD1_MSG( ' ', '         ^MESS', STATUS )
      ELSE

*  Enable an iteraction to scroll the cursor.
         INTTY = 0
         INTID = 0
         OBJTY = 1
         OBJID = 0
         INTOP = 1
         CALL IIIENI( ID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :                ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
      END IF

*  And a couple of blanks.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ', ' ', STATUS )

*  End of IDI setup.....................................................

*  Get the world coordinates of the current picture and derive the
*  linear scales for coordinates. (Assumes GKS type coordinates only
*  will require use of transform a-la KAPPA CURSOR at some time.)
      CALL AGI_IWOCO( WX1, WX2, WY1, WY2, STATUS )
      XSCALE = ( WX2 - WX1 ) / REAL( XSIZE - 1 )
      YSCALE = ( WY2 - WY1 ) / REAL( YSIZE - 1 )

*  Get the cross size and thickness.
      CALL PAR_GET0R( 'MSIZE', CRSIZE, STATUS )
      CRSIZE = MAX( 0.001, MIN( CRSIZE , 1.0 ) )
      CALL PAR_GET0I( 'THICK', THICK, STATUS )
      THICK = MAX( 1, THICK )

*  Work out a number of device pixels that this fraction corresponds
*  too.
      FRAC = NINT( REAL( XSIZE ) / 2.0  * CRSIZE )

*  Set pressed counter for number of unzooms requested.
      NPRESS = 0

*  Set the object identifier count to zero.
      NOBJ = 0

*  Make sure we don't enter the loop when carry a bad status.
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Main loop............................................................
  10  CONTINUE

*  Execute the interactions.
      CALL IIIEIW( ID, TRIGS, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99
      
*  If the centre trigger has been fired then increase zoom
      IF ( TRIGS( 2 ) .NE. 0 ) THEN
*         ZOOM = ZOOM + 1
         ZOOM = ( ZOOM * 2 ) + 1
         IF ( ZOOM .GT. ZRANGE( 2 ) ) ZOOM = ZRANGE( 2 )
         CALL IIZWZM( ID, MEMID, 1, ZOOM, ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
      
*  Next push must be first of right-hand trigger
         NPRESS = 0
         GO TO 10
      
*  If the right-hand trigger has been fired then decrease the zoom,
*  re-centre the display or possibly exit.
      ELSE IF ( TRIGS( 3 ) .NE. 0 ) THEN
      
*  Increment NPRESS if this is zoom zero state.
         IF ( ZOOM .EQ. 0 ) NPRESS = NPRESS + 1
      
         IF ( NPRESS .EQ. 0 ) THEN 
*            ZOOM = ZOOM - 1
            ZOOM = ZOOM - 1
            IF ( ZOOM .GT. 0 ) ZOOM = INT( SQRT( REAL ( ZOOM ) ) )
            IF ( ZOOM .LT. 0 ) ZOOM = 0
            CALL IIZWZM( ID, MEMID, 1, ZOOM, ISTAT )
            IF ( ISTAT .NE. IDI__OK ) GO TO 99
      
*  Return for next interaction.
            GO TO 10
         ELSE IF ( NPRESS .EQ. 1 ) THEN
      
*  Re-centre display.
            CALL IIZWSC( ID, MEMID, 1, XOFF, YOFF, ISTAT )
            IF ( ISTAT .NE. IDI__OK ) GO TO 99

*  Inform user that this action has occurred if next press could
*  exit the routine.
            IF ( .NOT. KEYOK ) THEN
               CALL MSG_BELL( STATUS )
               CALL CCD1_MSG( ' ', 
     :'  Display now re-centred and unzoomed, next unzoom exits'//
     :' application', STATUS )
            END IF
       
*  Return for next interaction.
            GO TO 10
         ELSE
      
*  Time to exit second press in zoom zero state, but only if the
*  keyboard trigger to exit isn't available.
            IF ( KEYOK ) GO TO 10
         END IF      
      
*  If left-hand trigger has been pressed then mark position.
      ELSE IF ( TRIGS( 1 ) .NE. 0 ) THEN
      
*  Read the cursor position relative to the memory origin.
         CALL IICRCP( ID, MEMID, NUMCUR, XC, YC, OUTMID, ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
      
*  Mark the position.
         CALL CCD1_DRAWI( ID, MEMID, XC, YC, FRAC, THICK, ICOL, STATUS )
      
*  Increment the object count.
         NOBJ = NOBJ + 1
      
*  Output the cursor position correcting for origin and linear scaling
*  of axes.
         XPOS = REAL( XC - XORG ) * XSCALE + WX1 + 1 
         YPOS = REAL( YC - YORG ) * YSCALE + WY1 + 1
         CALL MSG_SETR( 'XPOS', XPOS )
         CALL MSG_SETR( 'YPOS', YPOS )
         CALL MSG_SETI( 'ID', NOBJ )
         CALL CCD1_MSG( ' ',
     :      '  Position ^ID: located at ^XPOS , ^YPOS', STATUS )
      
*  Output positions to output list, if one is opened.
         IF ( LOPEN ) THEN
            CALL MSG_SETR( 'XPOS', XPOS )
            CALL MSG_SETR( 'YPOS', YPOS )
            CALL MSG_SETI( 'ID', NOBJ )
            MESS = ' '
            CALL MSG_LOAD( ' ', ' ^ID ^XPOS  ^YPOS ', MESS, IAT,
     :                     STATUS )
            CALL FIO_WRITE( FD, MESS( : IAT ), STATUS )
         END IF

*  Re-centre the current position if requested. Note that this section
*  is commented out as a bug in IDI Xwindows driver stops this from
*  working. Use a DISPLAY zoom and pan as scrolling the memory doesn't
*  seem to work on the IKON either! If this every gets fixed need to re
*  think memory-v-display scroll and pan.
C         IF ( ATCEN ) THEN 
C            XSCROL = ( XSIZE / 2 ) - ( XC - XORG ) 
C            YSCROL = ( YSIZE / 2 ) - ( YC - YORG )
C
*  Pan and ZOOM the display.
C            CALL IIZWZP( ID, XSCROL, YSCROL, ZOOM, ISTAT )
C            CALL IIMSMV( ID, MEMID, 1, 1, STATUS )
C         END IF
      
*  Next push cannot be second of middle trigger.
         NPRESS = 0
         GO TO 10

      ELSE IF ( TRIGS( 31 ) .NE. 0 ) THEN 

*  This a call to get out quick, reset the zoom and scroll and exit.
         IF ( ZOOM .NE. 0 ) THEN 
            CALL IIZWZM( ID, MEMID, 1, 0, ISTAT )
            IF ( ISTAT .NE. IDI__OK ) GO TO 99
         END IF
         CALL IIZWSC( ID, MEMID, 1, XOFF, YOFF, ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99

      ELSE IF ( TRIGS( 17 ) .NE. 0 ) THEN 

*  This a call to reset the zoom and scroll only.
         IF ( ZOOM .NE. 0 ) THEN
            CALL IIZWZM( ID, MEMID, 1, 0, ISTAT )
            IF ( ISTAT .NE. IDI__OK ) GO TO 99
         END IF
         CALL IIZWSC( ID, MEMID, 1, XOFF, YOFF, ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99

*  Set zoom to ZOOM-1, this redisplays at the current magnification
*  next zoom-in press.
         ZOOM = MAX( 0, ZOOM - 1 )
         GO TO 10
      ELSE

*  Just return
         GO TO 10
      END IF

*  End of main loop.....................................................
*  Arrive here if normal exit. Undisplay the cursor by setting its
*  visibility to .FALSE.
      CALL IICSCV( ID, NUMCUR, 0, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99

*  Unzoom and scroll the image if necessary.
      IF ( ZOOM .NE. 0 ) THEN
         CALL IIZWZM( ID, MEMID, 1, 0, ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
      END IF
      CALL IIZWSC( ID, MEMID, 1, XOFF, YOFF, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99

*  Report file used and number of entries if an output list is written.
      IF ( LOPEN ) THEN 
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL FIO_FNAME( FD, FNAME, STATUS )
         CALL MSG_SETC( 'FNAME', FNAME )      
         CALL MSG_SETI( 'NOBJ', NOBJ )
         CALL CCD1_MSG( ' ',
     : '  ^NOBJ entries written to file ^FNAME', STATUS )
         CALL CCD1_MSG( ' ', ' ', STATUS )
      END IF

*  Update the CCDPACK extension of any NDFs which have been given.
      IF ( GOTNDF .AND. NOBJ .GT. 0 .AND. STATUS .EQ. SAI__OK ) THEN 
         DO 34 I = 1, NNDF

*  Access the NDFs and update their extensions.
            CALL IRG_NDFEX( NDFGR, I, IDIN, STATUS )
            CALL CCG1_STO0C( IDIN, 'CURRENT_LIST', FNAME, STATUS )

*  Close the NDFs.
            CALL NDF_ANNUL( IDIN, STATUS )
 34      CONTINUE
      END IF

*  Exit on error label.
 99   CONTINUE

*  Close IRH.
      IF ( GOTNDF ) CALL IRH_ANNUL( NDFGR, STATUS )

*  Report any IDI errors.
      IF ( ISTAT .NE. IDI__OK ) THEN
         STATUS = SAI__ERROR
         CALL IIDERR( ISTAT, MESS, MESLEN )
         CALL MSG_SETC( 'MESS', MESS( : MESLEN ) )
         CALL ERR_REP( ' ', '^MESS', STATUS )
      END IF

*  Close down IDI and cancel the parameter.
      CALL AGD_DEACT( STATUS )
      CALL AGI_CANCL( 'DEVICE', STATUS )
      CALL AGI_END( -1, STATUS )

*  Close position list.
      IF ( LOPEN ) CALL FIO_CLOSE( FD, STATUS )

*  Release and NDFs.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL CCD1_ERREP( 'IDICURS_ERR',
     :   'IDICURS: Error reading graphics device locations.',
     :   STATUS )
      END IF

*  Close log file.
      CALL CCD1_END( STATUS )

      END
* $Id$
