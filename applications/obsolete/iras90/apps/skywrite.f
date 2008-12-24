      SUBROUTINE SKYWRITE( STATUS )
*+
*  Name:
*     SKYWRITE

*  Purpose:
*     Write text strings at specified positions.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SKYWRITE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine provides users with facilities to write text
*     strings at specified positions within the most recently created
*     DATA picture. Particularly, it can:
*
*        1. Write text strings at the sky positions specified by
*        parameters.
*
*        2. Write text strings at the sky positions specified within
*        a text file.
*
*        3. Write text strings at the cursor position.
*
*     The attributes of the text, that is, the direction, the height,
*     the aspect ratio, the justification, the font and the colour, can
*     be set using various parameters.
*
*     In interactive mode (cursor or keyboard), the application will
*     keep prompting the user for the specifications of the next text
*     string and/or position until a null response is obtained.
*
*     In non-interactive mode, an input text file is required which can
*     be the output of a previous run of this routine or can be created
*     by the user. The format of this file is described in the "Notes:"
*     section.
*
*     After getting a null reponse when in interactive mode, or
*     finishing writing in the non-interactive mode, the application
*     will exit by default. But users can choose to use the application
*     in a even greater loop, invisible to users by default, in which
*     users can interactively and/or non-interactively write texts,
*     change working mode, set new text attribute, save the information
*     of present writing into a text file for later use and erase the
*     last writing, etc., see parameter LOOP.

*  Usage:
*     SKYWRITE DEVICE IN

*  ADAM Parameters:
*     ATTRIBUTE = LITERAL (Read)
*        This parameter is used to specify which attribute of the text
*        is to be set. It takes the following values:
*
*         DIRECTION - Set the text 'up' direction.
*         
*         HEIGHT - Set the text height.
*
*         ASPECT RATIO - Set aspect ratio (width/height) of the
*         characters in the text.
*
*         JUSTIFICATION - Set the text justification.
*
*         SPACE - Set the space between characters in the text.
*
*         FONT - Set the font for the text.
*
*         PEN - Set the text colour.
*
*         DEFAULT - Select the default settings for all text attributes.
*
*         SHOW - Show the current text attribute settings.
*
*        The application will keep prompting for more attributes until
*        DEFAULT, or a null value is supplied.                 [DEFAULT]
*     CLEAR = LOGICAL (Read)
*        True if the area of the graphics device over which the text is
*        to be written should be cleared before writing commences.  [NO]
*     COORDS = LITERAL (Read)
*        Specifies the sky coordinate system to use. Valid values
*        include ECLIPTIC, EQUATORIAL, GALACTIC. See help on
*        "Sky_coordinates" for more information on available sky
*        coordinate systems.
*                                        [current sky coordinate system]
*     DEVICE = DEVICE (Read)
*        The plotting device.     [Current image-display-overlay device]
*     DIRECTION = Real (Read)
*        A pair of values specifing character orientation in terms of a
*        vector. Only the direction of the vector is significant. On
*        entry, the text up direction is set as upwards, that is (0,1).
*     EPOCH = DOUBLE PRECISION (Read)
*        The Julian epoch at which the supplied sky positions were
*        determined. This is only used if sky coordinates supplied in a
*        text file (in FILE mode) are stored in a coordinate system
*        different to that specified by the COORDS parameter. In this
*        case the epoch may be necessary to perform the conversion from
*        one coordinate system to the other (depending on what the
*        coordinate systems are). A value of 1983.5 is acceptable for
*        all IRAS data.                                         [1983.5]
*     FILE = LITERAL (Read)
*        The name of the text file containing specifications of the
*        text strings to be written.
*     FONT = INTEGER (Read)
*        Font number of the text string. See GKS user guide for details
*        about the available fonts.
*     HEIGHT = REAL (Read)
*        Specifies the height of the character in the text string. It
*        should be given as the fraction of the horizontal dimension of
*        the displayed image. On entry, it is 0.02, that is, 1/50 of
*        the width of the displayed image.
*     IN = NDF (Read)
*        The NDF from which to read the astrometry information. This
*        will usually be the NDF holding the displayed image. A null
*        value will cause the astrometry to be located using
*        information stored within the AGI database. If this cannot be
*        done, then the user will be re-prompted for an NDF using
*        parameter IN.  If a section of an NDF is specified, the curves
*        are only drawn over the specified section of the picture.   [!]
*     JUST = LITERAL (Read)
*        Specifies the disposition of the text string with respect to
*        the given position on the displayed image. The first character
*        of JUST should be one of B, C and T, specifies whether the
*        given position is to lie on the bottom, centre, or top edge of
*        the string. The second should be one of L, C and R and
*        specifies whether the given position is lie on the left,
*        centre or right edge of the string. Only first two characters
*        in JUST will be used. On entry it is set to 'BC'.
*     LAT = LITERAL (Read)
*        The latitude at which a text string is to be written, in the
*        coordinate system specified by COORDS (eg if COORDS was
*        EQUATORIAL, LAT should be given a Declination value).  See
*        help on "Sky_coordinates" for more information on available
*        sky coordinate systems.
*     LOGFILE = LITERAL (Read)
*        The name of the log text file containing the information
*        about the written text strings. See the NOTES section about 
*        the format of this file. 
*     LON = LITERAL (Read)
*        The longitude at which a text string is to be written, in the
*        coordinate system specified by COORDS (eg if COORDS was
*        EQUATORIAL, LON should be given a Right Ascension value).  See
*        help on "Sky_coordinates" for more information on available
*        sky coordinate systems.
*     LOOP = LOGICAL (Read)
*        If true then the routine does not exit when the final text
*        string has been written. Instead, the user is allowed to
*        modify the current drawing by selecting from the options
*        provided by parameter OPTION.                              [NO]
*     MODE = LITERAL (Read)
*        The source from which text string specifications are obtained.
*
*        CURSOR - All positions are specified by cursor. This mode will
*        only be available on graphic devices which support cursors.
* 
*        KEYBOARD - Positions are specified in response to parameter
*        prompts, using the keyboard.
*
*        FILE - Strings are drawn according to the specifications
*        contained within a text file (see parameter FILE). This is
*        referred to as "non-interactive" mode. The run-time default is 
*        CURSOR if a cursor is available, and KEYBOARD otherwise.     []
*     MSG_FILTER = LITERAL (Read)
*        The level of information displayed on the users screen. This
*        should take one of the values QUIET, NORMAL or VERBOSE (see
*        help on "Message_filtering").  [current message filter setting]
*     OPTION = LITERAL (Read)
*        The action to take once all strings have been written. It can
*        take following values:
*
*         CONTINUE - Continue to draw further text strings.
*
*         MODE - Change the source from which further text string
*         specifications will be obtained.
*
*         ATTRIBUTES - Set the attributes of any further text strings to
*         be written.
*
*         SAVE - Save information describing the text strings written so
*         far to a text file.
*
*         ERASE - Erase the last writen text string.
*         
*         EXIT - Exit the application.
*
*        This parameter is only prompted for if parameter LOOP has a
*        true value.                                              
*     PEN = INTEGER (Read)
*        Pen number used to write the texts. On entry the pen number is
*        set to 3.
*     RATIO = REAL (Read)
*        Specifies the aspect ration (Width/Height) of the charactors
*        in the text string. On entry it is set to 2/3.
*     SPACE = REAL (Read)
*        The space between characters in text strings, as a fraction of 
*        the width of a character. On entry it is set to 0 which means 
*        no extra space between characters and results in the text 
*        being of normal appearance. It can be negative which results 
*        in the characters overlapping.
*     TEXT = LITERAL (Read)
*       The text string to be written at a specified position.

*  Notes:
*     -  The strings are drawn within the current picture only if it is
*     a DATA picture, otherwise strings are drawn in the last DATA
*     picture to be created within the current picture.
*
*     -  The astrometry information used to position the strings is
*     located using the following search path:
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
*     -  This routine can only be used to display strings over NDFs
*     which contain astrometry information in the form used by the
*     IRAS90 package.
*
*     - The format of input and output text files is:
*   
*        o The file is divided into comments and fields. Comments
*        consist of strings commencing with a "#" character, and are
*        considered to extend to the end of the line. Such comments are
*        ignored. Fields are strings which specify any of the items of
*        information described below. Each line in the file may contain
*        any number of fields, multiple fields being separated by
*        commas. Blank lines or fields are ignored.
*
*        o The first field should give the name of the sky coordinate
*        system used (see help on "Sky_coordinates").
*
*        o There should then follow a block of fields defining the text
*        strings to be written and the positions at which to write
*        them. A position and text string takes three consecutive
*        fields. The first and second fields are the longitude and
*        latitude of the position. The third field is the string to be
*        written to the position. The string may be delimited by single
*        or double quote marks, which will be ignored. If a blank string
*        is to be written, it MUST be delimited by quote marks.
*      
*        o Fields containing the key words DIRECTION, HEIGHT, ASPECT
*        RATIO, JUSTIFICATION, FONT, SPACE, PEN and DEFAULT set up the
*        attributes to be used for writing any remaining text strings. 
*        Keywords can be abbreviated, and are case insensitive.
*
*        o A field containing the keyword DIRECTION should be followed
*        by two fields giving the direction vector of the text with X
*        component first, and Y component second.
*
*        o A field containing the keyword HEIGHT should be followed by
*        a single field giving the text height.
*
*        o A field containing the keyword ASPECT RATIO should be
*        followed by a single field giving the character aspect ration
*        (width/height).
*
*        o A field containing the keyword JUSTIFICATION should be
*        followed by a single field giving the disposition of the text
*        string with respect to the specified position.
*
*        o A field containing the keyword FONT should be followed by a
*        single field giving the font number.
*   
*        o A field containing the keyword SPACE should be followed by a
*        single field giving the space between characters.
*   
*        o A field containing the keyword PEN should be followed by a
*        single field giving the pen number.
*   
*        o A field containing the keyword DEFAULT causes the default 
*        text attributes to be re-established.
*   
*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     21-AUG-1992 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants
      INCLUDE 'AGI_ERR'          ! AGI_ error constants
      INCLUDE 'AGI_PAR'          ! AGI_ constants
      INCLUDE 'GNS_PAR'          ! GNS_ constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants
      INCLUDE 'IRA_PAR'          ! IRA_ constants
                                 
*  Status:
      INTEGER STATUS             ! Global status

*  External References:

*  Local Constants:
      INTEGER MXNTXT             ! Max. number of texts can be written
      PARAMETER ( MXNTXT = 100 )
      INTEGER MXTXLN             ! Max. length of text string
      PARAMETER ( MXTXLN = 120 )

*  Local Variables:
      LOGICAL CLEAR              ! Clear graphic device flag
      LOGICAL CURSOR             ! Cursor available flag
      CHARACTER*( 10 ) DEFMOD    ! Default option for MODE
      REAL DIR( 2 )              ! Direction setting of the texts
      DOUBLE PRECISION EPOCH     ! Epoch of sky coordinate system used
      LOGICAL EXIT               ! Exit application flag
      INTEGER FONT               ! Font setting of the texts 
      CHARACTER*( GNS__SZKEY ) GDTYPE
                                 ! Graphic device type
      REAL HEIGHT                ! Height setting of the texts
      INTEGER IRA                ! ID for IRA system
      CHARACTER*( 2 ) JSTFCT     ! Justification setting of the texts
      INTEGER LBND( 2 ), UBND( 2 ) ! Bounds of displayed picture
      DOUBLE PRECISION LON( MXNTXT ), LAT( MXNTXT )
                                 ! Sky coordinate of each text position
      LOGICAL LOOP               ! True if looping required.
      CHARACTER*( 16 ) OPTION    ! Action to take in a loop
      CHARACTER*( 10 ) MODE      ! Working mode
      INTEGER NTXT               ! Number of texts having been written
      CHARACTER*( 40 ) OPTMOD    ! Option list for MODE
      CHARACTER*( 45 ) OPTLOP    ! Option list for OPTION
      INTEGER PEN                ! SGS pen number
      INTEGER PICID1             ! ID for the picture on entry
      INTEGER PICID2             ! ID for the DATA picture 
      CHARACTER PICCOM*(AGI__CMAX)! Picture comment
      CHARACTER PICLAB*(AGI__SZLAB)! Picture label
      REAL RATIO                 ! Aspect Ratio of texts
      CHARACTER*( IRA__SZSCS ) SCS
                                 ! Name of sky coordinate system
      REAL SPACE                 ! Space setting of the texts
      CHARACTER*( MXTXLN ) TEXT( MXNTXT )
                                 ! Text written to the image.
      INTEGER TXFONT( MXNTXT ), TXPEN( MXNTXT )
                                 ! Font and pen of each written text
      CHARACTER*( 2 ) TXJSTF( MXNTXT )
                                 ! Justification of each written text
      REAL TXDIRX( MXNTXT ), TXDIRY( MXNTXT )
                                 ! Up direction vector of each text
      REAL TXHIGT( MXNTXT ), TXRTIO( MXNTXT )
                                 ! Height & aspect ratio of each text
      REAL TXSPAC( MXNTXT )      ! Space between characters
      REAL XW1, XW2, YW1, YW2, XMW, YMW
                                 ! Extension and size of SGS zone to be
                                 ! written in
      INTEGER ZONE1              ! ID for initial picture SGS zone
      INTEGER ZONE2              ! ID for DATA picture SGS zone
      INTEGER ZONEW              ! ID for the SGS zone to be written in
  
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the level of message reporting and set the report filtering
*  levevl accordingly.
      CALL MSG_IFGET( 'MSG_FILTER', STATUS )
            
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
         CALL ERR_REP( 'SKYWRITE_ERR1',
     : 'SKYWRITE: Unable to find a DATA picture which is contained '//
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
         CALL MSG_OUTIF( MSG__NORM, 'SKYWRITE_MSG1',
     :                   '  DATA picture ^LAB ("^COM") being used', 
     :                      STATUS )
      ELSE
         CALL MSG_SETC( 'COM', PICCOM )
         CALL MSG_OUTIF( MSG__NORM, 'SKYWRITE_MSG2',
     :                   '  DATA picture "^COM" being used', STATUS )
      END IF   

*  Get the type of the graphic device.
      CALL IRM_GDTYP( 'SGS', GDTYPE, STATUS )

*  Set the dynamic default value for CLEAR accordinag to graphic device
*  type. If the device is image_overlay, clear the picture zone by
*  default.
      IF ( GDTYPE( : 13 ) .EQ. 'IMAGE_OVERLAY' ) THEN
         CALL PAR_DEF0L( 'CLEAR', .TRUE., STATUS )

*  For another device type, unclear the picture zone by default.
      ELSE
         CALL PAR_DEF0L( 'CLEAR', .FALSE., STATUS )
      END IF

*  Since some text may need be written outside the image, create a zone
*  20% larger than the DATA picure.
      CALL SWRIF0( 20.0, ZONEW, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Inquire the size of the picture zone.
      CALL SGS_IZONE( XW1, XW2, YW1, YW2, XMW, YMW )      

*  See if the display is to be cleared.
      CALL PAR_GET0L( 'CLEAR', CLEAR, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Clear the display surface, if required.
      IF ( CLEAR ) CALL SGS_CLRZ

*  Cancel the value of the parameter for later use.
      CALL PAR_CANCL( 'CLEAR', STATUS )

*  Begine IRA context.
      CALL IRA_INIT( STATUS )

*  Get the IRA astrometry information about the image.
      CALL IRM_GTAST( 'IN', PICID2, LBND, UBND, IRA, STATUS )

*  Get the epoch at which coordinates stored in files are assumed to
*  have been determined.
      CALL PAR_GET0D( 'EPOCH', EPOCH, STATUS )

*  Get the initial text attributes from the environment.
      CALL SWRIA0( 'ATTRIBUTE', 'DIRECTION', 'HEIGHT', 'RATIO',
     :             'JUST', 'SPACE', 'FONT', 'PEN', DIR, HEIGHT,
     :              RATIO, JSTFCT, SPACE, FONT, PEN, STATUS )
 
*  Set text attributes accordingly.
      CALL SWRIA1( XW2 - XW1, DIR, HEIGHT, RATIO, JSTFCT, SPACE, FONT,
     :             PEN, STATUS )
      
*  Get the sky coordinate system to be used.
      CALL IRA_GTSCS( 'COORDS', .FALSE., SCS, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Set the option list and default choice for MODE according to
*  availability of cursor.
      CALL SGS_ICUAV( CURSOR )
      IF ( CURSOR ) THEN
         OPTMOD = 'CURSOR,KEYBOARD,FILE'
         DEFMOD = 'CURSOR'
      ELSE
         OPTMOD = 'KEYBOARD,FILE'
         DEFMOD = 'KEYBOARD'
      END IF

*  Get the required working mode.
      CALL PAR_CHOIC( 'MODE', DEFMOD, OPTMOD, .FALSE., MODE, STATUS )
      
*  Initialise the number of texts having been written.
      NTXT = 0

*  If cursor or keyboard mode is selected, write texts interactively.
      IF ( MODE( : 4 ) .NE. 'FILE' ) THEN
         CALL SWRIA2( 'TEXT', 'LON', 'LAT', MODE, IRA, SCS,
     :                XW1, XW2, YW1, YW2, MXNTXT, NTXT, LON, LAT, TEXT,
     :                TXDIRX, TXDIRY, TXHIGT, TXRTIO, TXJSTF, TXSPAC,
     :                TXFONT, TXPEN, STATUS )

*  Or if file mode is selected, write texts non-interactively.
      ELSE
         CALL SWRIA3( 'FILE', IRA, SCS, EPOCH, XW2 - XW1, MXNTXT, NTXT,
     :                 LON, LAT, TEXT, TXDIRX, TXDIRY, TXHIGT, TXRTIO,
     :                 TXJSTF, TXSPAC, TXFONT, TXPEN, STATUS )

*  Re-set the text attributes to their previous values.
         CALL SWRIA1( XW2 - XW1, DIR, HEIGHT, RATIO, JSTFCT, SPACE,
     :                FONT, PEN, STATUS )

      END IF
      
*  Set up the option list for OPTION according to the type of the
*  graphic device.
      IF ( GDTYPE( : 13 ) .EQ. 'IMAGE_OVERLAY' ) THEN
         OPTLOP = 'CONTINUE,MODE,ATTRIBUTES,SAVE,ERASE,EXIT'
      ELSE
         OPTLOP = 'CONTINUE,MODE,ATTRIBUTES,SAVE,EXIT'
      END IF
      
*  Enter a do loop until 'EXIT' is issued by the user or max number of
*  text has been reached, or anything goes wrong.
      EXIT = .NOT. LOOP
      DO WHILE ( .NOT.EXIT .AND. NTXT .LE. MXNTXT .AND.
     :            STATUS .EQ. SAI__OK )

*  Get the action to perform next.
         CALL PAR_CHOIC( 'OPTION', 'CONTINUE', OPTLOP, .FALSE., OPTION,
     :                    STATUS )

*  If CONTINUE is required...
         IF ( OPTION( : 8 ) .EQ. 'CONTINUE' ) THEN

*  If working mode is interactive, write text interactively.
*  If cursor or keyboard mode is selected, write texts interactively.
            IF ( MODE( : 4 ) .NE. 'FILE' ) THEN
               CALL SWRIA2( 'TEXT', 'LON', 'LAT', MODE,
     :                       IRA, SCS, XW1, XW2, YW1, YW2, MXNTXT,
     :                       NTXT, LON, LAT, TEXT, TXDIRX, TXDIRY,
     :                       TXHIGT, TXRTIO, TXJSTF, TXSPAC, TXFONT,
     :                       TXPEN, STATUS )

*  Or if file mode is selected, write texts non-interactively.
            ELSE
               CALL SWRIA3( 'FILE', IRA, SCS, EPOCH, XW2 - XW1, MXNTXT,
     :                       NTXT,
     :                       LON, LAT, TEXT, TXDIRX, TXDIRY, TXHIGT,
     :                       TXRTIO, TXJSTF, TXSPAC, TXFONT, TXPEN,
     :                       STATUS )

*  Re-set the text attributes to their previous values.
               CALL SWRIA1( XW2 - XW1, DIR, HEIGHT, RATIO, JSTFCT,
     :                      SPACE, FONT, PEN, STATUS )
            END IF
         
*  If change mode is required...
         ELSE IF ( OPTION( : 4 ) .EQ. 'MODE' ) THEN
            CALL PAR_CANCL( 'MODE', STATUS )
            CALL PAR_CHOIC( 'MODE', DEFMOD, OPTMOD, .FALSE., MODE,
     :                       STATUS )

*  If 'ATTRIBUTE' is selected, get the new text attributes from the
*  environment.
         ELSE IF ( OPTION( : 9 ) .EQ. 'ATTRIBUTE' ) THEN
            CALL SWRIA0( 'ATTRIBUTE', 'DIRECTION', 'HEIGHT', 'RATIO',
     :                   'JUST', 'SPACE', 'FONT', 'PEN', DIR,
     :                    HEIGHT, RATIO, JSTFCT, SPACE, FONT, PEN,
     :                    STATUS )

*  And then set the text attributes accordingly.
            CALL SWRIA1( XW2 - XW1, DIR, HEIGHT, RATIO, JSTFCT, SPACE,
     :                   FONT, PEN, STATUS )
       
*  If 'SAVE' is selected, save the present writting in a text file.
         ELSE IF ( OPTION( : 4 ) .EQ. 'SAVE' ) THEN

*  Do the save only when there is anything to save.
            IF ( NTXT .GT. 0 ) THEN
               CALL SWRIA4( 'LOGFILE', PICID2, SCS, NTXT, LON, LAT,
     :                       TEXT, TXDIRX, TXDIRY, TXHIGT, TXRTIO,
     :                       TXJSTF, TXSPAC, TXFONT, TXPEN, STATUS )
       
*  Report and do nothing if nothing has been written.
            ELSE
               CALL MSG_OUTIF( MSG__NORM, 'SKYWRITE_MSG3',
     :                   '  Nothing has been written, nothing to save.',
     :                         STATUS )
               CALL MSG_BLANKIF( MSG__NORM, STATUS )
            END IF
      
*  If erase is selected, ...
         ELSE IF ( OPTION( : 5 ) .EQ. 'ERASE' ) THEN

*  Abort if an error has occurred.
            IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Clear the display surface.
            CALL SGS_CLRZ

*  Re-write the texts written previously but last one.
            IF ( NTXT .GT. 0 ) NTXT = NTXT - 1
            IF ( NTXT .GT. 0 )
     :         CALL SWRIA5( NTXT, IRA, SCS, XW2 - XW1, LON, LAT, TEXT,
     :                      TXDIRX, TXDIRY, TXHIGT, TXRTIO, TXJSTF,
     :                      TXSPAC, TXFONT, TXPEN, STATUS )
      
*  Re-set the text attributes as before erasing.
            CALL SWRIA1( XW2 - XW1, DIR, HEIGHT, RATIO, JSTFCT, SPACE,
     :                   FONT, PEN, STATUS )

*  If 'EXIT' is selected, set exit flag.
         ELSE IF ( OPTION( : 4 ) .EQ. 'EXIT' ) THEN
            EXIT = .TRUE.  
         END IF

*  Cancel the value of parameter OPTION to get the next action.
         CALL PAR_CANCL( 'OPTION', STATUS )
      END DO

*  If the max. number of texts has been reached, report.
      IF ( NTXT .GT. MXNTXT .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'M', MXNTXT )
         CALL ERR_REP( 'SKYWRITE_ERR2',
     :'SKYWRITE: Maximum number of text strings (^M) has been reached.',
     :                 STATUS )
      END IF
      
*  Store an astrometry structure in the AGI data base.
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
         CALL ERR_REP( 'SKYWRITE_ERR3',
     :        'SKYWRITE: Error writing text strings at specified sky '//
     :        'coordinates', STATUS )
      END IF
      
      END
