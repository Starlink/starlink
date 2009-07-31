      SUBROUTINE SKYLINE( STATUS )
*+
*  Name:
*     SKYLINE

*  Purpose:
*     Draw curves over a displayed sky image.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SKYLINE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine provides the user with facilities for drawing
*     curves over a previously displayed data picture. The curves
*     which can be drawn include:
*
*       1. Arcs of meridians (lines of constant longitude)
*
*       2. Arcs of parallels (lines of constant latitude)
*
*       3. Arcs of great circles
*
*       4. Poly-lines containing several sky positions. Each section of
*          the poly line is an arc of a great circle.
*
*     Many curves can be drawn by a single invocation of this routine
*     (see parameter OPTION). The curves to be drawn can be specified
*     either "interactively" (by positioning a graphics cursor or by
*     suppling values in response to parameter prompts), or
*     "non-interactively" (by supplying a text file containing the curve
*     specifications); see parameter MODE. A text file can be created
*     containing specifications of all the curves which have been
*     drawn. This file can be supplied as input to a subsequent run of
*     this routine.
*
*     The parameter OPTION provides more facilities for modifying and
*     editing the curves drawn by this routine, allowing astrometric
*     diagrams to be created.

*  Usage:
*     SKYLINE DEVICE IN

*  ADAM Parameters:
*     ANGLE = LITERAL
*        The position angle of an arc of a great circle (measured at
*        the start of the arc). The angle is measured from north to the
*        required direction with positive values in the sense of
*        rotation from north to east. The formats allowed for this
*        value are the same as those allowed for a declination value
*        (see help on "Sky_coordinates").
*     ARCLEN = LITERAL (Read)
*        Gives the length of the arc to be drawn. It can be positive or
*        negative. The latitude at the end of an arc of a meridian is
*        (LATITUDE + ARCLEN). The longitude at the end of an arc of a
*        parallel is (LONGITUDE + ARCLEN). The section will be clipped
*        by the boundary of the displayed image. The formats allowed
*        for this value are the same as those allowed for a declination
*        value (see help on "Sky_coordinates"). If the supplied value is
*        zero, the entire length of the arc visible within the picture
*        will be drawn.
*     CLEAR = LOGICAL (Read)
*        True if the area of the graphics device over which the curves
*        are to be drawn should be cleared before drawing commences.[NO]
*     COORDS = LITERAL (Read)
*        Specifies the sky coordinate system to use. Valid values
*        include ECLIPTIC, EQUATORIAL, GALACTIC. See help on
*        "Sky_coordinates" for more information on available sky
*        coordinate systems.
*                                        [current sky coordinate system]
*     DEVICE = DEVICE (Read)
*        The plotting device.     [Current image-display-overlay device]
*     ERASE = LITERAL (Read)
*        Erase a curve (or all curves ) just drawn. It can take the
*        following values:
*
*        MERIDIAN - Erase the last drawn meridian arc
*
*        PARALLEL - Erase the last drawn parallel arc
*
*        GREAT CIRCLE   - Erase the last drawn great circle arc
*
*        POLYLINE - Erase the last drawn poly-line
*
*        ALL - Erase all curves
*
*        Only the facility to erase the last drawn curve of each type
*        or to erase all curves is provided. After a curve is erased
*        the one drawn just before it becomes the last and hence can be
*        erased subsequently. If you have a complicated selection of
*        curves to be erased, the best way to achieve this is: save the
*        present drawing in a text file, erase all curves, edit the
*        text file with a text editor to delete the entries
*        corresponding to the curves to be erased and then draw the
*        remaining curves in the file by running SKYLINE again,
*        selecting non-interactive mode with the edited text file as
*        its input.
*     FILE = LITERAL (Read)
*        The name of the text file used to give the information about
*        the curves to be drawn in non-interactive mode. See the "Notes"
*        section below for a description of the format of this file
*     IN = NDF (Read)
*        The NDF from which to read the astrometry information. This
*        will usually be the NDF holding the displayed image. A null
*        value will cause the astrometry to be located using
*        information stored within the AGI database. If this cannot be
*        done, then the user will be re-prompted for an NDF using
*        parameter IN.  If a section of an NDF is specified, the curves
*        are only drawn over the specified section of the picture.   [!]
*     LAT = LITERAL (Read)
*        The latitude at the start of a curve, in the coordinate system
*        specified by COORDS (eg if COORDS was EQUATORIAL, LAT should
*        be given a Declination value).  See help on "Sky_coordinates"
*        for more information on available sky coordinate systems.
*     LOGFILE = LITERAL (Read)
*        The name of the log text file containing the information
*        about the present plotted curves. See the NOTES section about 
*        the format of this file. 
*     LON = LITERAL (Read)
*        A longitude at the start of a curve, in the coordinate system
*        specified by COORDS (eg if COORDS was EQUATORIAL, LON should
*        be given a Right Ascension value).  See help on
*        "Sky_coordinates" for more information on available sky
*        coordinate systems.
*     LOOP = LOGICAL (Read)
*        If true then the routine does not exit when the final curve of
*        the given type has been drawn. Instead, the user is allowed to
*        modify the current drawing by selecting from the options
*        provided by parameter OPTION.                              [NO]
*     MODE = LITERAL (Read)
*        Specifies the source from which curve specifications will be
*        obtained. It can be:
*
*        CURSOR - All positions are specified by cursor. This mode will
*        only be available on graphic devices which support cursors.
* 
*        KEYBOARD - Positions are specified in response to parameter
*        prompts, using the keyboard.
*
*        FILE - Curves are drawn according to the specifications
*        contained within a text file (see parameter FILE). This is
*        referred to as "non-interactive" mode. The run-time default is 
*        CURSOR if a cursor is available, and KEYBOARD otherwise.     []
*     MSG_FILTER = LITERAL (Read)
*        The level of information displayed on the users screen. This
*        should take one of the values QUIET, NORMAL or VERBOSE (see
*        help on "Message_filtering").  [current message filter setting]
*     OPTION = LITERAL (Read)
*        The action to take once all curves of the type specified by
*        parameter TYPE have been drawn. It can take following values:
*
*        CONTINUE - Continue to draw further curves.
*
*        ERASE - Select the curves to erase. This option will only
*        be available on image_overlay devices.
*         
*        SAVE - Save information describing the present drawing into a
*        log file.
*
*        MODE - Change the source from which curve specifications are
*        obtained.
*
*        PEN - Select a new SGS pen number. 
*
*        EXIT - Exit the application
*
*        This parameter is only prompted for if parameter LOOP has a
*        true value.                                              
*     PEN = INTEGER (Read)
*        The pen number used to draw the curves.                     [1]
*     TOLERANCE = INTEGER (Read)
*        The tolerance allowed when plotting the curves.  The value
*        should be between zero and ten. Values outside this range are
*        take as being equal to the nearest end point of the range.  A
*        value of zero gives minimum tolerance (i.e. maximum accuracy),
*        at the cost of increased plotting time. A value of ten gives
*        poorer accuracy but is faster.                              [6]
*     TYPE = LITERAL (Read)
*        Specifies the type of curve to be drawn. It takes the
*        following values (or any un-ambiguous abbreviation):
*
*        MERIDIAN - Draw meridian arcs
*
*        PARALLEL - Draw parallel arcs
*
*        GREAT CIRCLE - Draw great circle arcs
*
*        POLYLINE - Draw poly-lines connecting specified sky positions
*
*        A null response will result in the routine terminating if LOOP 
*        is false. If LOOP is true a value will be obtained for 
*        parameter OPTION and the corresponding action will be 
*        performed.

*  Examples:
*     SKYLINE MODE=KEY LON=0 LAT=0 TYPE=PAR COORDS=GAL ARCLEN=0
*        This causes a curve to be drawn over the displayed picture 
*        corresponding to the galactic equator. MODE=KEY causes the 
*        position supplied by parameters LON and LAT to be used in 
*        preference to the cursor. COORDS=GAL causes the values supplied
*        by LON and LAT to be interpreted as galactic coordinates. 
*        ARCLEN=0 causes the entire length of the intersection of the 
*        curve with the picture to be drawn (if any). TYPE=PAR specifies
*        that a parallel (i.e. a line of constant latitude) is to be 
*        drawn. LON=0 and LAT=0 causes the parallel to coincide with
*        the galactic equator. Once the curve is drawn, the user will 
*        be prompted for a new value for parameter LON. A null value
*        should be given to exit the routine, and then a null value 
*        should also be given for the subsequent prompt for TYPE.

*  Notes:
*     -  The curves are drawn within the current picture only if it is a
*     DATA picture, otherwise curves are drawn in the last DATA picture
*     to be created within the current picture.
*
*     -  The astrometry information used to draw the curves is located
*     using the following search path:
*
*        o  Firstly, astrometry information is looked for in any NDF
*        specified on the command line using parameter IN.
*
*        o  Secondly, astrometry information is looked for in any MORE
*        structure associated with the AGI picture.
*
*        o  Thirdly, astrometry information is looked for in any
*        reference object associated with the AGI picture.
*
*        o  If all else fails, the value of the IN parameter is
*        cancelled, and the user is prompted for the NDF containing
*        relevant astrometry information.
*      
*     -  This routine can only be used to display curves for NDFs which
*     contain astrometry information in the form used by the IRAS90
*     package.
*
*     - The format of input and output text files is:
*   
*        o The file is divided into comments and fields. Comments
*        consist of strings commencing with a "#" character, and are
*        considered to extend to the end of the line. Such comments are
*        ignored. Fields are strings which specify any of the items of
*        information described below. Each line in the file may contain
*        any number of fields, multiple fields being separated by
*        commas.
*
*        o The first field should give the name of the sky coordinate
*        system used (see help on "Sky_coordinates").
*
*        o Fields containing the key words MERIDIAN, PARALLEL, GREAT
*        CIRCLE or POLYLINE begin a block of specifications for arcs
*        of the corresponding type.
*
*        o The specification of an arc of a meridian or parallel takes
*        three consecutive fields; the longitude at the start of the
*        arc, the latitude at the start of the arc, and the arc-length.
*
*        o The specification of an arc of a great circle takes four
*        consecutive fields; the longitude at the start of the arc,
*        the latitude at the start of the arc, the position angle at
*        the start of the arc, and the arc-length.
*
*        o A poly-line is specified by giving the longitude and latitude
*        (in that order) of each vertex. A new POLYLINE keyword must be
*        specified for each poly-line which is to be drawn.
*
*        o Keywords can appear many times and in any order.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-JUN-1992 (WG):
*        Original version.
*     9-FEB-1993 (DSB):
*        Modified for inclusion in IRAS90.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'IRA_PAR'          ! IRA_ constants
      INCLUDE 'GNS_PAR'          ! GNS_ constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants
      INCLUDE 'AGI_PAR'          ! AGI_ constants
      INCLUDE 'AGI_ERR'          ! AGI_ error constants
              
*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:
      INTEGER MXNSCT             ! Max. number of each kind of sections
      PARAMETER ( MXNSCT = 50 )
      INTEGER MXVTCE             ! Max. No. of vertice of each polyline
      PARAMETER ( MXVTCE = 50 )

*  Local Variables:
      CHARACTER CMNT*120         ! Comment string
      CHARACTER DEFMOD*10        ! Default option for mode
      CHARACTER GDTYPE*(GNS__SZKEY) ! Graphic device type
      CHARACTER MODE*10          ! Working mode
      CHARACTER MODPRE*10        ! Previous working mode
      CHARACTER OPTION*10        ! Action to take in a loop
      CHARACTER OPTLOP*80        ! Loop option list
      CHARACTER OPTMOD*80        ! Mode option list
      CHARACTER PICCOM*(AGI__CMAX)! Comment of the picture
      CHARACTER PICLAB*(AGI__SZLAB)! Label of the picture
      CHARACTER SCS*(IRA__SZSCS)  ! Name of sky coordinate system


      DOUBLE PRECISION EPOCH     ! Epoch of sky coordinate system used
      DOUBLE PRECISION GLON( MXNSCT ),! Specifications for great circle
     :                 GLAT( MXNSCT ),! arc.
     :                 GANG( MXNSCT ),
     :                 GSCTLN( MXNSCT )
      DOUBLE PRECISION MLON( MXNSCT ),! Specifications for meridian arcs
     :                 MLAT( MXNSCT ),
     :                 MSCTLN( MXNSCT )
      DOUBLE PRECISION PLON( MXNSCT ),! Specifications for parallel arcs
     :                 PLAT( MXNSCT ),
     :                 PSCTLN( MXNSCT )
      DOUBLE PRECISION PLYLON( MXNSCT, MXVTCE ),! Poly-line vertices.
     :                 PLYLAT( MXNSCT, MXVTCE )


      INTEGER I                  ! Do loop index
      INTEGER IAT                ! Position of last non-blank character
      INTEGER INFID              ! Input text file ID
      INTEGER IRA                ! ID for IRA system
      INTEGER LBNDI( 2 ),        ! Bounds of the picture in integer
     :        UBNDI( 2 ) 
      INTEGER LOPLEN             ! Length of loop option list
      INTEGER NGCRL              ! Number of great circle section drawn
      INTEGER NMERD              ! Number of Meridian sections drawn
      INTEGER NPARL              ! Number of Parallel sections drawn
      INTEGER NPOLY              ! Number of polylines drawn
      INTEGER NVTCE( MXNSCT )    ! Number of vertices of each polyline
      INTEGER OUTFID             ! ID for output text file 
      INTEGER PEN                ! SGS pen number
      INTEGER PICID1             ! ID for the original picture 
      INTEGER PICID2             ! ID for the DATA picture 
      INTEGER TOL                ! Curve tolerance measurement
      INTEGER ZONE1              ! ID for original picture SGS zone.
      INTEGER ZONE2              ! ID for DATA picture SGS zone.


      LOGICAL CLEAR              ! Clear graphic device flag
      LOGICAL CURSOR             ! Cursor flag
      LOGICAL EXIT               ! Exit application flag
      LOGICAL LOOP               ! True if multiple operations are to
                                 ! be performed.
      LOGICAL OUTFIL             ! ID for output text file

      REAL  LBND( 2 ),           ! Bounds of the picture in pixels
     :      UBND( 2 ) 

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the level of message reporting and set the report filtering
*  level accordingly.
      CALL MSG_IFGET( STATUS )
            
*  See if the user wants to loop round, performing multiple operations.
      CALL PAR_GET0L( 'LOOP', LOOP, STATUS )

*  Open the graphic database and get the graphic device. Since it is not
*  known whether the graphic device is image-display or image-overlay,
*  keep the graphic surface uncleared at present.
      CALL AGS_ASSOC( 'DEVICE', 'UPDATE', ' ', PICID1, ZONE1, STATUS )

*  Attempt to find a DATA picture.
      CALL IRM_AGFND( 'DATA', PICID2, STATUS )

*  If a DATA picture could not be found, add a context message.
      IF( STATUS .EQ. AGI__NONAM ) THEN
         CALL ERR_REP( 'SKYLINE_ERR1',
     : 'SKYLINE: Unable to find a DATA picture which is contained '//
     : 'entirely within the current picture.', STATUS )
      ENDIF

*  Create an SGS zone corresponding to the DATA picture.
      CALL AGS_NZONE( ZONE2, STATUS )

*  Describe the picture to the user.
      CALL MSG_BLANKIF( MSG__NORM, STATUS )

      CALL AGI_ICOM( PICCOM, STATUS )
      CALL AGI_ILAB( -1, PICLAB, STATUS )
      IF( PICLAB .NE. ' ' ) THEN
         CALL MSG_SETC( 'COM', PICCOM )
         CALL MSG_SETC( 'LAB', PICLAB )
         CALL MSG_OUTIF( MSG__NORM, 'SKYLINE_MSG1',
     :                   '  DATA picture ^LAB ("^COM") being used', 
     :                      STATUS )
      ELSE
         CALL MSG_SETC( 'COM', PICCOM )
         CALL MSG_OUTIF( MSG__NORM, 'SKYLINE_MSG2',
     :                   '  DATA picture "^COM" being used', STATUS )
      END IF   

*  Get the type of the graphic device.
      CALL IRM_GDTYP( 'SGS', GDTYPE, STATUS )

*  Set the dynamic default value for CLEAR accordinag to graphic device
*  type. If the device is image_overlay, clear the picture zone by
*  default.
      IF ( GDTYPE( : 13 ) .EQ. 'IMAGE_OVERLAY' ) THEN
         CALL PAR_DEF0L( 'CLEAR', .TRUE., STATUS )

*  For another device type, do not clear the picture zone by default.
      ELSE
         CALL PAR_DEF0L( 'CLEAR', .FALSE., STATUS )
      END IF

*  See if the display surface is to be cleared.
      CALL PAR_GET0L( 'CLEAR', CLEAR, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Clear the display surface, if required.
      IF ( CLEAR ) CALL SGS_CLRZ

*  Initialise the IRA system.
      CALL IRA_INIT( STATUS )

*  Get the SGS pen used to draw the curves and then set the pen.
      CALL PAR_GET0I( 'PEN', PEN, STATUS )
      CALL IRA_DROPT( 'PEN2', DBLE( PEN ), STATUS )
      CALL PAR_CANCL( 'PEN', STATUS )

*  Get the curve tolerance and set accordingly.
      CALL PAR_GET0I( 'TOLERANCE', TOL, STATUS )
      CALL IRA_DROPT( 'TOLERANCE', DBLE( TOL ), STATUS )

*  Get the IRA astrometry information.
      CALL IRM_GTAST( 'IN', PICID2, LBNDI, UBNDI, IRA, STATUS )

*  Convert bounds from pixel indices to pixel coordinates.
      LBND( 1 ) = REAL( LBNDI( 1 ) - 1 )
      LBND( 2 ) = REAL( LBNDI( 2 ) - 1 )
      UBND( 1 ) = REAL( UBNDI( 1 ) )
      UBND( 2 ) = REAL( UBNDI( 2 ) )
      
*  Get the sky coordinate system the user want to use, using the one
*  in IRA structure as default.
      CALL IRA_SCSEP( IRA, SCS, EPOCH, STATUS )
      CALL IRA_GTSCS( 'COORDS', .TRUE., SCS, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Set the option list for the working mode according to the
*  availability of cursor on the graphic device.
      CALL SGS_ICUAV( CURSOR )
      IF ( CURSOR ) THEN
         OPTMOD = 'CURSOR,KEYBOARD,FILE'
         DEFMOD = 'CURSOR'
      ELSE
         OPTMOD = 'KEYBOARD,FILE'
         MODE = 'KEYBOARD'
      END IF
      
*  Get the working mode.
      CALL PAR_CHOIC( 'MODE', DEFMOD, OPTMOD, .FALSE., MODE, STATUS )
      CALL PAR_CANCL( 'MODE', STATUS )
           
*  Initialise the number of curves has been drawn.
      NMERD = 0
      NPARL = 0
      NGCRL = 0
      NPOLY = 0

      DO I = 1, MXNSCT
         NVTCE( I ) = 0
      END DO

*  If the interactively mode is selected, draw curve interactively.
      IF ( MODE( : 4 ) .NE. 'FILE' ) THEN
         CALL SLINA1( 'TYPE', 'LON', 'LAT', 'ARCLEN',
     :                'ANGLE', MODE, IRA, SCS, LBND, UBND, MXNSCT,
     :                 MXVTCE, NMERD, NPARL, NGCRL,
     :                 NPOLY, NVTCE, MLON, MLAT, MSCTLN, PLON, PLAT,
     :                 PSCTLN, GLON, GLAT, GANG, GSCTLN, PLYLON, PLYLAT,
     :                 STATUS )

*  Otherwise draw curves non-interactively.
      ELSE
         CALL SLINA2( 'FILE', IRA, LBND, UBND,  
     :                 MXNSCT, MXVTCE, NMERD, NPARL, NGCRL, NPOLY, 
     :                 NVTCE, MLON, MLAT, MSCTLN, PLON, PLAT, PSCTLN, 
     :                 GLON, GLAT, GANG, GSCTLN, PLYLON, PLYLAT, 
     :                 STATUS )
      END IF
      
*  Set up the option list for the loop according to the type of the
*  graphic device and the availability of cursor.
      OPTLOP = 'SAVE,PEN,CONTINUE,EXIT'
      LOPLEN = 22

      IF ( GDTYPE( : 13 ) .EQ. 'IMAGE_OVERLAY' )
     :             CALL CHR_APPND( ',ERASE', OPTLOP, LOPLEN )

      LOPLEN = CHR_LEN( OPTLOP )
      IF ( CURSOR ) CALL CHR_APPND( ',MODE', OPTLOP, LOPLEN ) 

*  Enter the loop until 'EXIT' is issued by the user or the number 
*  of all kinds of sections exceed the upper limit.
      EXIT = .NOT. LOOP
      DO WHILE ( MIN( NMERD, NPARL, NGCRL, NPOLY ) .LE. MXNSCT .AND. 
     :          .NOT.EXIT .AND. STATUS .EQ. SAI__OK )
      
*  See what to do next.
         CALL PAR_CHOIC( 'OPTION', 'EXIT', OPTLOP, .FALSE., OPTION,
     :                   STATUS )

*  If CONTINUE is selected, draw more curves specified by TYPE.
         IF ( OPTION( : 8 ) .EQ. 'CONTINUE' ) THEN

*  If the interactively mode is selected, draw curve interactively.
            IF ( MODE( : 4 ) .NE. 'FILE' ) THEN
               CALL SLINA1( 'TYPE', 'LON', 'LAT', 'ARCLEN',
     :                 'ANGLE', MODE, IRA, SCS, LBND, UBND, MXNSCT,
     :                 MXVTCE, NMERD, NPARL, NGCRL,
     :                 NPOLY, NVTCE, MLON, MLAT, MSCTLN, PLON, PLAT,
     :                 PSCTLN, GLON, GLAT, GANG, GSCTLN, PLYLON, PLYLAT,
     :                 STATUS )

*  Otherwise draw curves non-interactively.
            ELSE
               CALL SLINA2( 'FILE', IRA, LBND, UBND,  
     :                   MXNSCT, MXVTCE, NMERD, NPARL, NGCRL, NPOLY, 
     :                   NVTCE, MLON, MLAT, MSCTLN, PLON, PLAT, PSCTLN, 
     :                   GLON, GLAT, GANG, GSCTLN, PLYLON, PLYLAT, 
     :                   STATUS )

            END IF

*  If the working mode is to be changed, get the new work mode.
         ELSE IF ( OPTION( : 4 ) .EQ. 'MODE' ) THEN
            CALL PAR_CANCL( 'MODE', STATUS )
            CALL PAR_CHOIC( 'MODE', DEFMOD, OPTMOD, .FALSE., MODE,
     :                       STATUS )

*  If saving present drawing is required, get a file name for output
*  file.
         ELSE IF ( OPTION( : 4 ) .EQ. 'SAVE' ) THEN

*  Only create the file and write it when there is something to write
*  into.
            IF ( MAX( NMERD, NPARL, NGCRL, NPOLY ) .GT. 0 ) THEN
               CALL IRM_ASFIO( 'LOGFILE', 'WRITE', 'LIST', 80, OUTFID,
     :                          OUTFIL, STATUS )

*  Write the  head comments to the output text file after open the file
*  successfully.
               IF ( OUTFIL .AND. STATUS .EQ. SAI__OK ) THEN

*  Get the label of the displayed image and form the head comment.
                  CMNT = ' '
                  IAT = 0
                  CALL CHR_APPND( '# Curves drawn by SKYLINE over '//
     :                            'image ', CMNT, IAT )
                  CALL AGI_ILAB( PICID2, PICLAB, STATUS )
                  CALL CHR_APPND( PICLAB, CMNT, IAT )

                  CALL FIO_WRITE( OUTFID, CMNT( : MIN( 80, IAT ) ), 
     :                            STATUS )
                  CALL FIO_WRITE( OUTFID, ' ', STATUS )

*  Write the name of the sky coordinate system used.
                  CMNT = ' '
                  IAT = 0
                  CALL CHR_APPND( SCS, CMNT, IAT )
                  CALL CHR_APPND( '  # Sky Coordinate System', CMNT,
     :                            IAT )

                  CALL FIO_WRITE( OUTFID, CMNT( : MIN( 80, IAT ) ), 
     :                            STATUS )
                  CALL FIO_WRITE( OUTFID, ' ', STATUS )

*  Write the information about the present drawing into the output file.
                  CALL SLINA3( OUTFID, SCS, MXNSCT, MXVTCE, NMERD,
     :                         NPARL, NGCRL, NPOLY, NVTCE, MLON, MLAT,
     :                         MSCTLN, PLON, PLAT, PSCTLN, GLON, GLAT,
     :                         GANG, GSCTLN, PLYLON, PLYLAT, STATUS )
             
*  Cancel the value associated with the parameter for the later use
                  CALL FIO_CANCL( 'LOGFILE', STATUS )
               END IF

*  In case nothing has been draw, report to the user.
            ELSE
               CALL MSG_OUTIF( MSG__NORM, 'SKYLINE_MSG3',
     :                     '  Nothing has been drawn, nothing to save.',
     :                         STATUS )
               CALL MSG_BLANKIF( MSG__NORM, STATUS )
            END IF

*  If erase is specified, ...
         ELSE IF ( OPTION( : 5 ) .EQ. 'ERASE' ) THEN

*  and there are something have been drawn, see what to erase.
            IF ( MAX( NMERD, NPARL, NGCRL, NPOLY ) .GT. 0 ) THEN
               CALL SLINA4( 'ERASE', IRA, SCS, LBND, UBND, 
     :                       MXNSCT, MXVTCE, NMERD, NPARL, NGCRL, 
     :                       NPOLY, NVTCE, MLON, MLAT, MSCTLN, PLON, 
     :                       PLAT, PSCTLN, GLON, GLAT, GANG, GSCTLN, 
     :                       PLYLON, PLYLAT, STATUS )
      
*  If nothing has been drawn, report the user.
            ELSE
               CALL MSG_OUTIF( MSG__NORM, 'SKYLINE_MSG4',
     :               '  Nothing has been drawn, nothing can be erased.',
     :                         STATUS )
               CALL MSG_BLANKIF( MSG__NORM, STATUS )
            END IF

*  If pen number is to be changed, get new pen number set set the pen.
         ELSE IF ( OPTION( : 9 ) .EQ. 'PEN' ) THEN
            CALL PAR_GET0I( 'PEN', PEN, STATUS )
            CALL IRA_DROPT( 'PEN2', DBLE( PEN ), STATUS )
            CALL PAR_CANCL( 'PEN', STATUS )
      
*  If exit is required, set the exit flag.
         ELSE IF ( OPTION( : 4 ) .EQ. 'EXIT' ) THEN
            EXIT = .TRUE.  
         END IF

*  Cancel the value of parameter OPTION to get the next action.
         CALL PAR_CANCL( 'OPTION', STATUS )

      END DO

*  Store an astrometry structure in teh AGI data base.
      CALL IRM_PTAST( PICID2, IRA, STATUS )

*  Annul the IRA identifier and close down the IRA package.
 999  CONTINUE

      CALL IRA_ANNUL( IRA, STATUS )
      CALL IRA_CLOSE( STATUS )
      
*  Set the current picture on entry as current and Close down the AGI
*  and SGS.
      CALL AGS_DEASS( 'DEVICE', .TRUE., STATUS )

*  If a parameter null or abort error exists, annul it.
      IF( STATUS .EQ. PAR__NULL .OR. STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SKYLINE_ERR2',
     :               'SKYLINE: Error drawing curves in sky coordinates',
     :                 STATUS )
      END IF
      
      END
