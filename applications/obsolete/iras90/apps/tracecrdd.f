      SUBROUTINE TRACECRDD( STATUS )
*+
*  Name:
*     TRACECRDD

*  Purpose:
*     Display detector data streams from a CRDD file.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL TRACECRDD( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine displays a set of vertically displaced traces
*     representing the data from selected detectors in a specified CRDD
*     file. Many normally defaulted parameters provide control over the
*     content and layout of the display. Some of the other features
*     provided include:
*
*     -  The ability to loop round, re-drawing the display with
*     modified parameter values until the required effect is produced,
*     or using any of the other features listed below.
*
*     -  The ability to use a graphics cursor to read sample values and
*     positions from the display.
*
*     -  The ability to overlay standard point source profiles on top of
*     sources visible in the data traces.
*
*     -  The ability to assign a "quality" to samples selected using
*     a graphics cursor (see help on "Quality_in_IRAS90" for more
*     information about the use of quality in IRAS90).
*
*     Some of these features may be controlled using a primative 
*     "Graphical User Interface".

*  Usage:
*     TRACECRDD NDF DETS XLIMIT YLIMIT

*  ADAM Parameters:
*     COMMENT = LITERAL (Read)
*        A comment to store with a quality name. This parameter is used
*        if the quality specified by parameter QNAME is not currently
*        defined within the input NDF.
*     COORDS = LITERAL (Read)
*        Specifies the coordinate system used when displaying sample
*        positions. Valid values include ECLIPTIC, EQUATORIAL,
*        GALACTIC. See help on "Sky_coordinates" for more information
*        on available sky coordinate systems.
*                                        [current sky coordinate system]
*     CURSOR = _LOGICAL (Read)
*        If a true value is supplied, then the "Graphical User
*        Interface" will be used (if possible). Otherwise, a parameter 
*        interface will be used.                                 [FALSE]
*     DATA = _REAL (Write)
*        This is an output parameter to which is written the last data
*        value displayed using the "Get Data Value" option (selected
*        using parameter NEXT or through the "Graphical User
*        Interface").
*     DATDET = _INTEGER (Write)
*        This is an output parameter to which is written the last
*        detector number displayed using the "Get Data Value" or "Draw
*        Point Source" options (selected using parameter NEXT or
*        through the "Graphical User Interface").
*     DETS = LITERAL (Read)
*        A group of detector numbers, selected from those available in
*        the IRAS waveband of the data contained in the input CRDD
*        file. The display includes traces for each of these detectors.
*        See help on "Specifying_detectors" for more information on
*        specifying groups of detector numbers. The suggested default
*        consists of all the detectors which pass within the range of
*        cross scan distances given by parameter XSCAN (the cross scan
*        distance is the minimum distance between the detector track and
*        the CRDD file reference point). If no detectors fall in this
*        range then all detectors are included in the suggested default
*        value.
*     DEVICE = DEVICE (Read)
*        The plotting device.                  [Current graphics device]
*     HISTORY = _LOGICAL (Read)
*        Determines if history information is to be added to the NDF in
*        the event of the "Assign Quality" option being selected using
*        the parameter NEXT or through the "Graphical User Interface".
*        See help on "History_in_IRAS90" for more information on
*        history.                              [current history setting]
*     LOGFILE = LITERAL (Read)
*        The name of a text file to which the results of the "Get Data
*        Value" and "Draw Point Source" options (selected using
*        parameter NEXT or through the "Graphical User Interface") will
*        be written. No file is created if these options are not used,
*        or if a null value is supplied.                             [!]
*     LOOP = LOGICAL (Read)
*        If a false value is supplied, the application produces the
*        display and then returns immediately without allowing the user
*        to use any of the additional features. If a true value is
*        supplied, then the user may repeatedly re-draw the display
*        with different parameter values, or use any of the other
*        features selected by parameter NEXT, or through the "Graphical
*        User Interface".                                         [TRUE]
*     MSG_FILTER = INTEGER (Read)
*        The level of information displayed on the users screen. This
*        should take one of the values QUIET, NORMAL or VERBOSE (see
*        help on "Message_filtering").  [current message filter setting]
*     NDF = NDF (Update)
*        The NDF holding the CRDD to be displayed. This is updated by
*        any new quality assignments made using the "Assign Quality"
*        option selected using parameter NEXT or through the "Graphical
*        User Interface".
*     NEXT = LITERAL (Read)
*        Specifies the action to be performed once the initial display
*        has been produced. If parameter LOOP has a false value, this
*        parameter is ignored. It can take the following values (or any
*        un-ambiguous abbreviation):
*
*           QUIT - Leave the application.
*
*           REDRAW DISPLAY - Re-draw the display.
*
*           CHANGE PARAMETERS - Change some of the parameters which
*           determine the appearance of the display.
*
*           GET DATA VALUE - Get the values and positions of samples
*           selected using the graphics cursor.
*
*           DRAW POINT SOURCE - Display a standard point source template
*           overlayed on the display at a position specified either by
*           the graphics cursor or by parameters SRCDET, SRCPSN and
*           SRCPEAK.
*
*           ASSIGN QUALITY - Assign a nominated quality to samples
*           selected using the graphics cursor.
*
*        If the "Graphical User Interface" is being used (see parameter
*        CURSOR), then the parameter NEXT is not used, but these
*        options can be selected from a menu using the graphics cursor.
*     OFFSET = _REAL (Read)
*        This parameter specifies an array of offsets by which each
*        trace is to be vertically displaced in the display. It is only
*        used if the parameter SPACE is given the value FREE.
*     OMIT = LITERAL (Read)
*        A group expression specifying objects to be omitted from the
*        display. Any combination of the following items can be
*        specified:
*
*           IN-LINE - The in-line detector number labels plotted within
*           a gap in each data trace.
*
*           LABEL_X - The X axis label.
*
*           LABEL_Y - The Y axis label.
*
*           SCANDIR - The scan direction indicator.
*
*           MARKS_RHS - The right hand detector offset markers.
*
*           MARKS_LHS - The left hand detector offset markers
*           (including detector numbers).
*
*           TABLE - The table of detector offsets.
*
*           REF_POS - The description of the reference position.
*
*        In addition the string NOTHING can be given to indicate that
*        no sections should be omitted from the display (a null value
*        has the same effect).  Section names can be abbreviated. If an
*        ambiguous abbreviation is supplied, than all sections matching
*        the abbreviation are omitted from the display. Thus, for
*        instance, the string "M" would cause both left and right hand
*        markers to be omitted.                                [NOTHING]
*     PARAM = LITERAL (Read)
*        Specifies which aspect of the display is to be modified if the
*        "Change Parameters" option is selected using parameter NEXT.
*        It can take the following values (or any un-ambiguous
*        abbreviation):
*        
*           REDRAW DISPLAY - Redraw the display using the new parameter
*           values.
*
*           DET# - Change the detectors which are displayed by giving a
*           list of detector numbers. This also causes new Y limits and
*           trace offsets to be calculated and used. New detectors may
*           also be selected using the CROSS SCAN option.
*
*           DEVICE - Change the graphics device. The display is redrawn
*           immediately if this option is selected.
*
*           X LIMIT - Change the limits of the X axis of the display.
*           This also causes new Y limits and trace offsets to be
*           calculated and used.
*
*           Y LIMIT - Change the limits of the Y axis of the display.
*           This causes new trace offsets to be calculated and used.
*
*           TITLE - Change the title displayed at the top of the
*           display.
*
*           COMMAND SOURCE - Toggles between the "Graphical User
*           Interface" and the parameter interface as the means of
*           getting commands.
*
*           OFFSET METHOD - Change the offsets by which each trace is
*           displaced vertically in the display.
*
*           DISPLAY STRUCTURE - Select a list of features to be omitted
*           from the display.
*
*           QUALITY EXPRESSION - Only display samples which satisfy a
*           given quality expression.
*
*           CROSS SCAN - Change the detectors which are displayed by
*           giving a maximum cross scan distance. This also causes new
*           Y limits and trace offsets to be calculated and used. New
*           detectors may also be selected using the DET# option.
*           
*        If the "Graphical User Interface" is being used (see parameter
*        CURSOR), then the parameter PARAM is not used, but these
*        options can be selected from a menu using the graphics cursor.
*     PEAK = _REAL (Write)
*        This is an output parameter to which is written the last
*        source peak value displayed using the "Draw Point Source"
*        option (selected using parameter NEXT or through the
*        "Graphical User Interface").
*     PROFILES = NDF (Read)
*        An NDF holding in-scan point source profiles to be used by the
*        "Draw Point Source" option selected using parameter NEXT or
*        through the "Graphical User Interface". The default value is
*        the files "profiles.sdf" contained in the main IRAS90
*        directory, which contains profiles taken from the IRAS Catalogs
*        and Atlases Explanatory Supplement, page V-14.               []
*     PTITLE = LITERAL (Read)
*        A title for the top of the display. The run time default value
*        is the input NDF name.                                       []
*     PXSIZE = _REAL (Read)
*        The horizontal size of the display in metres. If a value less
*        than the default is requested, the the display will appear at
*        the bottom left of the current device.                       []
*     PYSIZE = _REAL (Read)
*        The vertical size of the display in metres. If a value less
*        than the default is requested, then the display will appear at
*        the bottom left of the current device.                       []
*     QEXP = LITERAL (Read)
*        A quality expression giving the quality of samples which are
*        to be included in the display. See help topic
*        "Quality_in_IRAS90" for information about the use of quality.
*        A value of "ANY" causes all samples to be used, without regard
*        to quality.                                               [ANY]
*     QNAME = LITERAL (Read)
*        The quality name to be assigned to the selected samples in the
*        event of the "Assign Quality" option being selected (by the
*        parameter NEXT or through the "Graphical User Interface").  If
*        the supplied name is not already defined within the input NDF,
*        then a definition of the name is added to the NDF. The user is
*        warned if the quality name is already defined within the NDF.
*     SCNPSN = _REAL (Write)
*        This is an output parameter consisting of an array of two 
*        values. The first is the last in-scan position and the second 
*        is the last cross-scan position displayed using the "Get Data 
*        Value" or "Draw Point Source" options (selected using parameter
*        NEXT or through the "Graphical User Interface").
*     SKYPSN = _DOUBLE (Write)
*        This is an output parameter consisting of an array of two 
*        values. The first is the last sky longitude and the second 
*        is the last sky latitude displayed using the "Get Data 
*        Value" or "Draw Point Source" options (selected using parameter
*        NEXT or through the "Graphical User Interface"). Both values 
*        are in radians.
*     SPACE = LITERAL (Read)
*        Specifies the method by which the vertical offset for each
*        detector trace is to be found. It can take any of the following
*        values (or any un-ambiguous abbreviation):
*
*           FREE - The user specifies the offsets explicitly using
*           parameter OFFSET.
*
*           CONSTANT - The offset markers are evenly spaced along the
*           display Y axis. This results in the actual traces not being
*           evenly spaced unless the input data has been destriped.
*
*           AVERAGE - The offset markers are spaced so that the average
*           data value in each traces are evenly spaced. Thus the traces
*           are evenly spaced but the offset markers may not be.
*                                                              [AVERAGE]
*     SRCDET = _INTEGER (Read)
*        The detector number specifying the trace on which a point
*        source template is to be overlayed. This parameter is only
*        used if no graphics cursor is available.
*     SRCPEAK = _REAL (Read)
*        The peak value of the point source template to be overlayed (in
*        the same units as the Y axis). This parameter is only used if
*        no graphics cursor is available.
*     SRCPSN = _REAL (Read)
*        The in-scan positions (in arc-minutes) at which a point source
*        template is to be overlayed. This parameter is only used if no
*        graphics cursor is available.
*     UNITS = LITERAL (Read)
*        The units in which the data values are to be displayed. See
*        help on "Data_units" for a list of the available units.    [Jy]
*     XLIMIT = _REAL (Read)
*        The upper and lower limits of the X axis, in arc-minutes. The
*        origin of the X axis is the position of closest approach to
*        the reference position specified within the input CRDD file.
*        The suggested default values include all the data in the input
*        NDF.
*     XNAME = LITERAL (Read)
*        If the NDF already contains any quality name definitions then
*        new quality names are put in the same extension as the old
*        names. If no previous quality names have been stored in the
*        NDF then parameter XNAME will be used to obtain the name of an
*        NDF extension in which to store new quality name. The
*        extension will be created if it does not already exist (see
*        parameter XTYPE).                               [QUALITY_NAMES]
*     XSCAN = REAL (Read)
*        A range of cross-scan distance (in arc-minutes) from the CRDD
*        file reference point to be used when selecting the list of
*        detectors. If only a single value is supplied, the limits are
*        considered to be equal and opposite.                     [-5,5]
*     XTYPE = LITERAL (Read)
*        If a new NDF extension is created to hold quality names (see
*        parameter XNAME), then parameter XTYPE is used to obtain the
*        HDS data type for the created extension. The run time default
*        is to give the extension a type identical to its name.       []
*     YLIMIT = _REAL (Read)
*        The upper and lower limits of the Y axis, in the units
*        specified by parameter UNITS. 

*  The Display:
*     The contents of the display can be controlled using the parameter
*     OMIT. This allows various parts of the display to be optionally
*     omitted.
*     
*     The main item in the display is the rectangular area containing
*     the actual data traces (the "data area"). The bottom edge of this
*     rectangle has a scale indicating distance along the scan in
*     arc-minutes. The value zero is assigned to the point in the scan
*     which is closest to the reference position of the input CRDD
*     file. This reference position is displayed to the left of the
*     data area. The extent of this scale is determined by parameter
*     XLIMIT.
*
*     The right hand edge of the data area has a scale indicating data
*     value. The units of this scale are specified by the parameter
*     UNITS, and its extent is specified by parameter YLIMIT.
*
*     The detectors from which data is to be displayed are specified
*     using parameter DETS. The data traces are offset vertically
*     within the data area, and are shifted horizontally so that the
*     "mis-alignment" caused by the detectors being at varying in-scan
*     positions within the focal plane, is removed. The detector
*     corresponding to each trace is indicated by means of an "in-line
*     label" towards the left end of the trace. If the default KAPPA
*     pallette is in effect, the in-line labels will be green (on
*     colour devices). Only those samples which satisfy the quality
*     expression given by parameter QEXP are included in the traces.
*     Bad or missing sample values are indicated by gaps in the traces.
*     The base level for each trace is indicated by a pair of "offset
*     markers", one on each of the vertical edges of the data area.
*     These offset markers are short green horizontal lines placed at
*     the position which corresponds to zero flux for the trace to
*     which they refer (the detector number to which the offset markers
*     refer is displayed just above the left hand offset marker).
*
*     The data area is surrounded by a "frame area" containing
*     annotation of various types. This includes a title above the data
*     area specified by parameter PTITLE. The X axis label is fixed, and
*     the Y axis label indicates the units used on the Y axis.
*
*     The traces are always drawn such that the end of the scan which is
*     closest to the southern ecliptic pole is at the left, and this is
*     indicated by annotations "South" and "North" at the left and right
*     ends of the bottom edge of the data area. An arrow placed between
*     these annotations indicates the direction in which the scan was
*     taken (south to north or north to south). The arrow indicates the
*     direction of increasing time (i.e a sample at the blunt end of
*     the arrow was taken earlier than one at the sharp end).
*
*     A table of detector offsets is displayed to the left of the data
*     area. This lists the offsets of each displayed trace. So to
*     determine a sample value by eye, one would read off the data value
*     from the scale on the left hand edge of the data area, and then
*     subtract the offset for the detector listed in the offset table.
*     In fact it is much easier to use the "Get Data Value" option to do
*     this! (see parameter NEXT). The table also lists the "cross scan
*     distance" of each trace. This is the offset (in arc-minutes) from 
*     the CRDD file reference position to the detector track (positive
*     in the same sense as the focal plane Z coordinate).
*     
*     All graphics are produced within the sub-region of the current
*     AGI picture, specified by parameters PXSIZE and PYSIZE (by default
*     the entire picture is used). Two new pictures are created within
*     the AGI database; a DATA picture corresponding to the data area,
*     and a FRAME picture corresponding to the frame area.

*  Modifying Parameter Values:
*     The default parameter values usually produce a reasonable
*     display.  However there may be some situations in which
*     parameters need to be changed, for instance to "zoom in" on a
*     particular feature of interest, to change the graphics device, or
*     to omit sections of the display prior to producing a hard copy.
*     If the parameter LOOP is given a true value (which is the
*     default), the user is asked to specify a further action once the
*     display has been produced (see parameter NEXT). One option is
*     "Change Parameters". This allows several of the parameters which
*     determine the appearance of the display to be changed before
*     redrawing the display. The parameter to change is specified by
*     giving an appropriate value for the parameter PARAM.

*  Getting Sample Values and Positions:
*     Sample values and positions can be estimated by eye from the
*     display, but it is generally easier and more accurate to use a
*     graphics cursor. This can be done by taking the "Get Data Value"
*     option (specified by the parameter NEXT or through the "Graphical
*     User Interface"). If the current graphics device has a cursor,
*     the user is asked to position the cursor at the point of
*     interest. The user does not need to position the cursor exactly
*     on a trace; the closest trace will always be used. Various items
*     of information are then displayed about the selected point; the
*     sample value (without the offset used to vertically displace the
*     traces), the sample coordinates (in the coordinate system
*     specified by parameter COORDS), the sample number, detector
*     number, NDF row number and in-scan position.  The user may then
*     give another position. Control returns to the application when
*     the user gives a position which lies outside the data area.
*
*     The information displayed by this option is logged to the text
*     file specified by parameter LOGFILE, and is also written to the
*     output parameters DATA, DATDET, SCNPSN and SKYPSN.

*  Overlaying Point Source Profiles:
*     An NDF containing four standard point source profiles (one for
*     each wave band) is distributed with IRAS90. These profiles are
*     taken from the IRAS Catalogs and Atlases Explanatory Supplement
*     (page V-14). They may be displayed at any point within the data
*     area in order to compare a feature in the data with a point
*     source. This is done by selecting the "Draw Point Source" option
*     for parameter NEXT or through the "Graphical User Interface". The
*     user is then requested to place the graphics cursor at the
*     position at which the peak of the profile should be put. A sloping
*     background is then estimated from the neighbouring data in the
*     closest trace, and the profile is drawn (in red assuming the
*     standard KAPPA pallette is in use) so that the peak is at the
*     requested position and the zero level corresponds to the local
*     background in the data. The position and peak value of the source
*     is then displayed, and the user is asked to repeat the process
*     until a position outside the data area is given.
*
*     The information displayed by this option is logged to the text
*     file specified by parameter LOGFILE, and is also written to the
*     output parameters PEAK, DATDET, SCNPSN and SKYPSN.
*
*     If the graphics device has no cursor, then the position of the
*     peak may be specified by parameters SRCDET, SRCPSN, and SRCPEAK.
*     If for any reason the user wishes to display profiles other than
*     those taken from the Explanatory Supplement, then a copy of the
*     file "profiles.sdf" (contained in the main IRAS90 directory)
*     should be taken and suitable modifications made.  The modified
*     file may then be given for parameter PROFILES.

*  Assigning a Quality to Selected Samples:
*     A general introduction to the use of quality is described in the
*     help topic "Quality_in_IRAS90". TRACECRDD provides a means of
*     assigning nominated qualities to samples in a CRDD file selected
*     using a graphical interface. To do this the user should select
*     the "Assign Quality" option for the parameter NEXT (or through
*     the "Graphical User Interface"). The user is then asked to
*     position the graphics cursor to indicate two diagonally opposite
*     corners of a box on the display. A quality name is then obtained
*     using parameter QNAME and this quality is assigned to all the
*     samples which fall within the box defined by the two corner
*     positions.  Note, the input CRDD file is updated rather than a
*     new output CRDD file being created). The samples to which the
*     quality is assigned are highlighted in the display by being
*     re-drawn in red (assuming the default KAPPA pallette is in use).
*     If the quality name is not defined in the CRDD file then a
*     definition is created, including a comment string given by
*     parameter COMMENT. If necessary, an extension is created within
*     the NDF to hold the quality name. The HDS name and type of this
*     extension are given by parameters XNAME and XTYPE.

*  The Graphical User Interface:
*     By default, the user selects options by assigning suitable values
*     to the parameters described in the "ADAM parameters" section.
*     Alternatively, a primative form of "Graphical User Interface" (GUI)
*     can be used for selecting the options normally determined by
*     parameters NEXT and PARAM. This alternative is only available if
*     the graphics workstation has a graphics cursor and also has the
*     facility for clearing sub-regions of the display. If this is the
*     case, the user may select the GUI either by giving the parameter
*     CURSOR a true value on the command line, or by selecting the
*     "Command Source" option for the parameter PARAM.
*
*     The GUI provides the user with menus of options displayed on the
*     graphics display as a series of boxes, each containing a
*     description of an option. The user selects an option by
*     positioning the cursor over a box and pressing any key. The menu
*     is erased once a valid choice has been made. The parameter
*     interface can be reinstated by selecting the "Command Source"
*     option from the menu produced by selecting the "Change Parameters"
*     option.

*  Examples:
*     TRACECRDD CENA_B1S3 
*        This command would display traces from the CRDD file CENA_B1S3.
*        The user is prompted for parameters DETS, XLIMIT and YLIMIT
*        before the traces are drawn. After the traces have been drawn
*        the user is prompted for parameter NEXT which gives options for
*        re-drawing the display with different parameter values, using
*        the cursor to get data values from the display, etc.
*     TRACECRDD CENA_B1S3 DEVICE=CANON NOLOOP \
*        This is like the previous example except that the NOLOOP
*        keyword causes the application to terminate as soon as the
*        display has been drawn, and the output goes to a Canon laser
*        printer. Note, the "\" character causes the suggested defaults
*        for parameters DETS, XLIMIT and YLIMIT to be accepted without
*        prompting the user. This sort of command is useful if a plot
*        is to be produced in a situation in which there is no user to
*        interact with (eg from a batch job). The statement NEXT=QUIT
*        is equivalent to the NOLOOP keyword.
*     TRACECRDD CENA_B1S3 CURSOR QEXP=FRED.AND.TOM
*        This causes the "Graphical User Interface" to be used for
*        obtaining values for the parameters NEXT and PARAM, rather than
*        the keyboard. In addition, only those samples which have both
*        the qualities FRED and TOM are displayed.
*        

*  Authors:
*     WG: Wei Gong (IPMAF)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-APR-1991 (WG):
*        Original version.
*        (Based on the INTERIM version CRDDTRACE by DSB ) 
*     18-NOV-1992 (DSB):
*        Modified for inclusion in the IRAS90 release (quality
*        assignment, OMIT parameter, re-write of prologue, etc).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'MSG_PAR'          ! MSG constants
      INCLUDE 'I90_DAT'          ! IRAS90 data.
      INCLUDE 'IRA_PAR'          ! IRA constants
      INCLUDE 'IRQ_PAR'          ! IRQ constants
      INCLUDE 'IRC_PAR'          ! IRC constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'PAR_ERR'          ! PAR error values.
                                
*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL AGCHAX            ! Substitution for standard NCAR routine
      EXTERNAL AGCHIL            ! Substitution for standard NCAR routine
      EXTERNAL AGCHCU            ! Substitution for standard NCAR routine

      INTEGER CHR_LEN            ! Used length of a character string
      INTEGER IRC_DETIN          ! The index of a detector

*  Local Variables:     
      CHARACTER COORDS*( IRA__SZSCS ) ! User specified sky coordinates
                                      ! system
      CHARACTER CRDUNT*( IRC__SZULS ) ! Units of data in CRDD NDF file
      CHARACTER DETLIS*( I90__MAXDT*3 )! List of default displayed
                                       ! detectors
      CHARACTER FILNAM*( GRP__SZFNM )! File name of logging file
      CHARACTER MENU*128         ! Option list of next action
      CHARACTER NAME*( GRP__SZFNM ) ! The full name of the input NDF.
      CHARACTER PTITLE*( GRP__SZFNM ) ! The title of the display    
      CHARACTER QEXP*(IRQ__SZQEX)! Current value of parameter QEXP.
      CHARACTER SELITM*20        ! Selected item
      CHARACTER UNITS*( IRC__SZUNI )  ! User specified display units


      DOUBLE PRECISION REFDEC    ! DEC of ref. position(equinox B1950.0)
      DOUBLE PRECISION REFRA     ! RA of ref. position (equinox B1950.0)


      INTEGER ACTVAL             ! Actual no. of values supplied.
      INTEGER ADET( I90__MAXDT ) ! Detector numbers in the CRDD file
      INTEGER BAND               ! Band number of the data in CRDD file
      INTEGER BDET               ! Begin detector index of data array
                                 ! of CRDD NDF file
      INTEGER BSMP               ! Begin sample index of data array of
                                 ! CRDD NDF file
      INTEGER DET( I90__MAXDT )  ! Detector numbers to be displayed
      INTEGER DETLN              ! Used length of DETCHR
      INTEGER DPNTR              ! Pointer to data array masked with
                                 ! quality expression.
      INTEGER DTINDX( I90__MAXDT ) !Detector index of displayed detector
      INTEGER EL                 ! Number of elements of a mapped array
      INTEGER EDET               ! End detector index of data array of
                                 ! CRDD NDF file
      INTEGER ESMP               ! End sample index of data array of
                                 ! CRDD NDF file 
      INTEGER FID                ! The identifier of logging file
      INTEGER FLNMLN             ! Used length of FILNAM string
      INTEGER I                  ! Do loop index
      INTEGER IDC                ! IRC identifier for CRDD NDF file    
      INTEGER IGRP               ! GRP identifier for group containing
                                 ! the input NDF name.
      INTEGER INDF               ! Identifier for CRDD NDF file
      INTEGER IPDATA             ! Pointer to mapped input DATA array.
      INTEGER IPNTR              ! Pointer to the temporary array
                                 ! containing in-scan distance
      INTEGER ITEMNO             ! Item number of selected item
      INTEGER LBND( 2 )          ! Lower bound of data array of CRDD NDF
      INTEGER LNAME              ! Used length of NAME.
      INTEGER LOG                ! =0 -- no logging is required
                                 ! >0 -- some data have been logged
                                 ! <0 -- nothing has been logged
      INTEGER NAVAIL             ! Number of available detectors.
      INTEGER NDIM               ! Dimension of data array of crdd NDF
      INTEGER NDISP              ! Number of detectors to be displayed
      INTEGER NIN                ! No. of NDFs to display (=1).
      INTEGER NROW( 6 )          ! Number of lines of each item in the
                                 ! displayed graphic menu
      INTEGER NVAL( I90__MAXDT ) ! Number of valid samples in each trace
      INTEGER OFFMTD             ! code of offset method:
                                 ! 0 -- 'FREE',
                                 ! 1 -- 'CONSTANT',
                                 ! 2 -- 'AVERAGE'.
      INTEGER OFFSET( I90__MAXDT ) ! Offset value for trace in display
      INTEGER PIC0               ! AGI picture ID for entry picture
      INTEGER PIC1               ! AGI picture ID for frame picture
      INTEGER PIC2               ! AGI picture ID for trace picture
      INTEGER PLOTZN             ! ID of SGS zone of NCAR grid window
      INTEGER UBND( 2 )          ! Upper bound of data array of CRDD NDF
      INTEGER ZONE               ! ID of SGS zone of NCAR graphic window


      LOGICAL CLRBLK             ! Block clear flag
      LOGICAL COLOUR             ! Colour available flag
      LOGICAL CURSOR             ! Cursor available flag
      LOGICAL EXIT               ! Flag of exiting do loop 2
      LOGICAL FLAGS(8)           ! Flags indicating which parts of the 
                                 ! display are required.
      LOGICAL LOGING             ! Logging flag
      LOGICAL LOOP               ! True if looping is selected.
      LOGICAL QUIT               ! Flag for quit the do loop 1
      LOGICAL SCNDIR             ! true -- scan is from north to south
                                 ! false -- scan is from south to north
      LOGICAL USECUR             ! Use cursor flag


      REAL AVERAG( I90__MAXDT )  ! Average of each data trace
      REAL COFST( 2 )            ! Coordinate offset
      REAL COSCL( 2 )            ! Coordinate scale 
      REAL SCALE( I90__MAXDT )   ! Scale factor to convert data from
                                 ! original units to user specified one
      REAL SGMA( I90__MAXDT )    ! Sigma of each data trace
      REAL TEMP                  ! Temporary REAL storage.
      REAL XLMT( 2 )             ! In-scan display limits
      REAL XSCAN( 2 )            ! Cross-scan limits.
      REAL XSCN( I90__MAXDT )    ! Cross-scan distance of each trace.
      REAL YLMT( 2 )             ! Vertical display limits
      REAL YMN( I90__MAXDT )     ! Min. value of each data trace
      REAL YMX( I90__MAXDT )     ! Max. value of each data trace

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Establish the conditional message filter level using parameter
*  MSG_LEVEL.
      CALL MSG_IFGET( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Get the input CRDD file. This is obtained as a group expression even
*  though only 1 NDF is required so that the user can use wild-cards to
*  complete long file names (for instance).
      CALL IRM_RDNDF( 'NDF',  1, 1, '  Give more NDF names...', 
     :                IGRP, NIN, STATUS )
      CALL NDG_NDFAS( IGRP, 1, 'UPDATE', INDF, STATUS )

*  Initialise the IRC system.
      CALL IRC_INIT( STATUS )

*  Attempt to import the NDF into the IRC system.
      CALL IRC_IMPRT( INDF, IDC, STATUS )

*  Get the full name of the input NDF.
      CALL NDF_MSG( 'NDF', INDF )
      CALL MSG_LOAD( ' ', '^NDF', NAME, LNAME, STATUS )

*  Remove the directory path from the NDF name. The resulting string
*  is used as the default value for the PTITLE parameter.
      CALL IRM_FILNM( NAME( : LNAME ), PTITLE, STATUS )

*  Set default value for the title of the display.
      CALL PAR_DEF0C( 'PTITLE', PTITLE, STATUS )

*  Get the title of the display.
      CALL PAR_GET0C( 'PTITLE', PTITLE, STATUS )

*  See if repeated plots are to be drawn.
      CALL PAR_GET0L( 'LOOP', LOOP, STATUS )

*  Get a valid sky coordinate system from the user.
      CALL IRA_GTSCS( 'COORDS', .FALSE., COORDS, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  If required open a log file. The file descriptor returned in FID is
*  used to access this file.
      CALL IRM_ASFIO( 'LOGFILE', 'WRITE', 'LIST', 80, FID, LOGING,
     :                 STATUS )
      IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*  Tell the user that output is being logged to the text file.
      IF( LOGING ) THEN
         CALL MSG_BLANKIF( MSG__NORM, STATUS )
         CALL MSG_OUTIF( MSG__NORM, 'TRACECRDD_MSG1',
     :           '  Logging displayed information to $LOGFILE', STATUS )
         CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Write a header to the log file.
         CALL FIO_WRITE( FID, ' ', STATUS )
         CALL FIO_WRITE( FID, '    *** TRACECRDD log file ***', STATUS )
         CALL FIO_WRITE( FID, ' ', STATUS )
         CALL FIO_WRITE( FID, ' Contains a log of all the values '//
     :                  'displayed with the "GET DATA VALUE" ', STATUS )
         CALL FIO_WRITE( FID, ' and "DRAW POINT SOURCE" options.', 
     :                   STATUS )
         CALL FIO_WRITE( FID, ' ', STATUS )
         CALL FIO_WRITE( FID, ' Input CRDD file:   '//
     :                   NAME( : MIN( 59, LNAME ) ), STATUS )

*  Set a flag to show that no data has yet been written to the log file.
         LOG = -1

*  If logging is not required, set the flag to indicate this.
      ELSE
         LOG = 0
      
      END IF

*  Inquire the bound of the data array of the input NDF.
      CALL NDF_BOUND( INDF, 2, LBND, UBND, NDIM, STATUS )

*  Store the indices of beggining and end samples, and beggining and end
*  detectors. 
      BSMP = LBND( 1 )
      ESMP = UBND( 1 )
      BDET = LBND( 2 )
      EDET = UBND( 2 )

*  Map the data array of the input CRDD file.
      CALL NDF_MAP( INDF, 'DATA', '_REAL', 'READ', IPDATA, EL, STATUS )

*  Get temporary work space to contain in-scan distance for each sample.
      CALL PSX_CALLOC( EL, '_REAL', IPNTR, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 998

*  Get the global properties, the in-scan distance and x-scan distance 
*  of the input CRDD file. 
      CALL TRACA1( IDC, BSMP, ESMP, BDET, EDET, %VAL( IPDATA ), BAND, 
     :             REFRA, REFDEC, ADET, SCNDIR, %VAL( IPNTR ), XSCN, 
     :             YMX, YMN, AVERAG, SGMA, NVAL, NAVAIL, STATUS )    

*  If there are no detectors available for selection, set status, report
*  the error and exit.
      IF ( NAVAIL .LE. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'TRACECRDD_ERR1',
     :   'TRACECRDD: No detectors are available for display', STATUS )
         GOTO 998
      END IF

*  Get workspace to hold a copy of the input DATA array in which samples
*  not satisfying the quality expression supplied by parameter QEXP are
*  set bad.
      CALL PSX_CALLOC( EL, '_REAL', DPNTR, STATUS )
  
*  Copy the required samples from the input DATA array to the temporary
*  array pointed to by DPNTR.
      CALL TRACC5( 'QEXP', INDF, EL, %VAL( IPDATA ), %VAL( DPNTR ),
     :              QEXP, STATUS )

*  Get the cross-scan range which defines the list of default detectors.
      CALL PAR_GET1R( 'XSCAN', 2, XSCAN, ACTVAL, STATUS )

* If only one value was supplied, use equal and opposite limits.      
      IF( ACTVAL .EQ. 1 ) THEN
         XSCAN( 1 ) = -ABS( XSCAN( 1 ) )
         XSCAN( 2 ) = -XSCAN( 1 )

*  Otherwise ensure that XSCAN(1) is less than XSCAN(2).
      ELSE

         IF( XSCAN( 1 ) .GT. XSCAN( 2 ) ) THEN
            TEMP = XSCAN( 1 )
            XSCAN( 1 ) = XSCAN( 2 )
            XSCAN( 2 ) = TEMP
         END IF

      END IF

*  Get the default detectors list to be displayed, selected from those
*  available in the input NDF.
      CALL TRACA2( IDC, NAVAIL, BDET, EDET, ADET, XSCN, XSCAN, DET,
     :             NDISP, DETLIS, DETLN, STATUS )
      
*  Establish the list as the default for parameter DETS.
      CALL PAR_DEF0C( 'DETS', DETLIS( : DETLN ), STATUS )

*  Get the user specified detector numbers from the environment.
      CALL IRM_GTDET( 'DETS', NAVAIL, ADET, DET, NDISP, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GOTO 998

*  If no detectors have been selected, report the error and exit.
      IF ( NDISP .LE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'TRACECRDD_ERR2',
     :                 'TRACECRDD: No detectors have been selected.',
     :                 STATUS )
         GOTO 998
      END IF

*  Get the indices of the detectors to be displayed.
      DO I = 1, NDISP
         DTINDX( I ) = IRC_DETIN( IDC, DET( I ), STATUS )
      END DO

*  Get the units of the input CRDD NDF file.
      CALL NDF_CGET( INDF, 'Units', CRDUNT, STATUS )

*  Get a system of units to display the data in.
      CALL IRM_GTCUN( 'UNITS', CRDUNT, UNITS, STATUS )

*  Find the scale factors needed to convert the data in original units
*  to the user specified units.
      CALL IRM_UNTCV( CRDUNT, UNITS, NDISP, DET, SCALE, STATUS )

*  Get the offset method and get offset for each data trace if the
*  method is 'FREE'.
      CALL TRACA4( 'SPACE', 'OFFSET', NDISP, OFFMTD, OFFSET, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 998

*  Get in-scan display limits.
      CALL TRACA5( BSMP, ESMP, BDET, EDET, %VAL( IPNTR ), NDISP, DTINDX,
     :             'XLIMIT', XLMT, STATUS )

*  Get vertical display limits.
      CALL TRACA6( IDC, BSMP, ESMP, BDET, EDET, %VAL( IPNTR ), 
     :             %VAL( DPNTR ), NDISP, DTINDX, SCALE, XLMT, 
     :             YMX, YMN, 'YLIMIT', YLMT, STATUS )

*  See which parts of the display are to be omitted. Use a dynamic 
*  default of "NOTHING".
      CALL PAR_DEF0C( 'OMIT', 'NOTHING', STATUS )
      CALL TRACC2( 'OMIT', FLAGS, STATUS )

*  Begin an AGI scope
      CALL AGI_BEGIN

*  Open a SGS workstation.
      CALL IRM_GROPN( 'DEVICE', 'PXSIZE', 'PYSIZE', .TRUE., 
     :             'IRAS90_TRACECRDD', PIC0, PIC1, ZONE, COLOUR, 
     :              CURSOR, CLRBLK, STATUS )

*  If cursor is available on the graphic device and the device can be 
*  partly cleared, see if user wants to use the cursor.
      IF ( CURSOR .AND. CLRBLK ) THEN
         CALL PAR_GET0L( 'CURSOR', USECUR, STATUS )

*  Otherwise, cursor can not be used.
      ELSE
         USECUR = .FALSE.

      END IF

*  Define the menu of options for the next action to be performed after
*  the current display is done.
      MENU = 'Quit,Redraw display,Change parameters,'/
     :      /'Get data value,Draw point source,Assign quality'
      NROW( 1 ) = 1
      NROW( 2 ) = 2
      NROW( 3 ) = 2
      NROW( 4 ) = 2
      NROW( 5 ) = 2
      NROW( 6 ) = 2

*  Enter do loop 1 to display the traces until QUIT is specified, or an
*  error occurs.
      QUIT = .FALSE.
      DO WHILE ( .NOT.QUIT .AND. STATUS .EQ. SAI__OK )

*  Display the data traces for the specified detectors.
         CALL TRACA7( BSMP, ESMP, BDET, EDET, %VAL( IPNTR ), XSCN,
     :               %VAL( DPNTR ), UNITS, REFRA, REFDEC, COORDS,
     :               NDISP, DET, DTINDX, SCALE, XLMT, YLMT, PTITLE,
     :               SCNDIR, COLOUR, AVERAG, OFFMTD, FLAGS, OFFSET, 
     :               STATUS )

*  If looping is selected, enter do loop 2 for further action.
         IF ( LOOP ) THEN
            EXIT = .FALSE.
            DO WHILE( .NOT.EXIT .AND. STATUS .EQ. SAI__OK )

*  If using cursor is specified, clear a block on the current SGS zone 
*  and display a menu for user to select an item
               ITEMNO = 0
               SELITM = ' '
               IF ( USECUR ) THEN
                  CALL SGS_CLRBL( -0.2, 1.2, -0.5, -0.13 )
                  CALL IRM_HMENU( 6, MENU, NROW, 2, -0.2, 1.2, -0.3,
     :                            -0.22, 'Select an item by '/
     :                          /'positioning cursor and pressing '/
     :                          /'any key.', COLOUR, 1, 0.02, .TRUE., 
     :                           ITEMNO, STATUS )

                  SELITM = ' '

*  Otherwise, select an item from the keyboard.
               ELSE
                  CALL PAR_CHOIC( 'NEXT', 'Redraw display', MENU,
     :                            .FALSE., SELITM, STATUS ) 
                  CALL PAR_CANCL( 'NEXT', STATUS )

               END IF

*  If 'Change parameters' is specified by user, ...
               IF ( ITEMNO .EQ. 3 .OR. SELITM( : 17 ) .EQ.
     :                                      'CHANGE PARAMETERS' ) THEN
                  CALL TRACA8( 'PARAM', 'DETS', 'DEVICE', 'PXSIZE',
     :                         'PYSIZE', 'XLIMIT', 'YLIMIT', 'PTITLE', 
     :                         'UNITS', 'SPACE', 'OFFSET', 'OMIT',
     :                         'QEXP', 'XSCAN', INDF, IDC, BSMP, ESMP, 
     :                         BDET, EDET, %VAL( IPNTR ), XSCN,
     :                         %VAL( IPDATA), %VAL( DPNTR ), CRDUNT,
     :                         YMX, YMN, NAVAIL, ADET, QEXP, DET,
     :                         DTINDX, NDISP, SCALE, PIC0, PIC1, ZONE,
     :                         COLOUR, CURSOR, CLRBLK, XLMT, YLMT,
     :                         USECUR, PTITLE, UNITS, OFFMTD, OFFSET,
     :                         FLAGS, STATUS )

*  If no error has occurred, clear the display surface.
                  IF( STATUS .EQ. SAI__OK ) CALL SGS_CLRZ

*  Exit do loop 2 to redraw the display with new parameters.
                  EXIT = .TRUE.

*  If 'Redraw display' is specified. 
               ELSE IF ( ITEMNO .EQ. 2 .OR. SELITM( : 14 ) .EQ.
     :                                      'REDRAW DISPLAY' ) THEN

*  Clear the display surface (so long as no error has occurred).
                  IF ( STATUS .EQ. SAI__OK ) CALL SGS_CLRZ

*  Exit do loop 2.
                  EXIT = .TRUE.      

*  If 'Quit' is specified, exit do loop 2 and quit do loop 1.
               ELSE IF ( ITEMNO .EQ. 1 .OR. SELITM( : 4 ) .EQ.
     :                                      'QUIT' ) THEN
                  EXIT = .TRUE.
                  QUIT = .TRUE.

*  If 'Get data value' is required, ...
               ELSE IF ( ITEMNO .EQ. 4 .OR. SELITM( : 14 ) .EQ.
     :                                      'GET DATA VALUE' ) THEN
                  CALL TRACA9( BSMP, ESMP, BDET, EDET, %VAL( IPNTR ),
     :                         XSCN, %VAL( IPDATA ), NDISP, OFFSET,
     :                         DTINDX, SCALE, XLMT, YLMT, IDC, 'DATDET',
     :                         'DATA', 'SCNPSN', 'SKYPSN',  COORDS,
     :                         UNITS, FID, CURSOR, CLRBLK, LOG, STATUS )

*  If graphic device can not be cleared partly, quit the application.
                  IF ( .NOT.CLRBLK ) THEN
                     QUIT = .TRUE.

*  Otherwise, clear the field used to display data values.
                  ELSE 
                     CALL SGS_CLRBL( -0.55, -0.1, -0.15, -0.05 )
                     CALL SGS_CLRBL( -0.55, 1.2, -0.5, -0.13 )
                     CALL SGS_FLUSH
                  END IF

*  If 'Draw point source' is specified, ...
               ELSE IF ( ITEMNO .EQ. 5 .OR. SELITM( : 17 ) .EQ.
     :                                      'DRAW POINT SOURCE' ) THEN
                  CALL TRACB0( BSMP, ESMP, BDET, EDET, %VAL( IPNTR ),
     :                         XSCN, %VAL( IPDATA ), SCNDIR,  NDISP, 
     :                         OFFSET, DET, DTINDX, SCALE, XLMT, YLMT, 
     :                         BAND, IDC, 'SRCDET', 'SRCPSN', 'SRCPEAK',
     :                        'PROFILES', 'DATDET', 'SCNPSN', 'SKYPSN', 
     :                        'PEAK', COORDS, UNITS, COLOUR, CURSOR,
     :                         CLRBLK, FID, LOG, STATUS )

*  If graphic device can not be cleared partly, quit the application.
                  IF ( .NOT.CLRBLK ) THEN
                     QUIT = .TRUE.

*  Otherwise, clear the field used to display data values.
                  ELSE 
                     CALL SGS_CLRBL( -0.55, -0.1, -0.15, -0.05 )
                     CALL SGS_CLRBL( -0.55, 1.2, -0.5, -0.13 )
                     CALL SGS_FLUSH
                  END IF

*  If 'Assign quality' is specified... 
               ELSE IF ( ITEMNO .EQ. 6 .OR. SELITM( : 14 ) .EQ.
     :                                      'ASSIGN QUALITY' ) THEN

                  CALL TRACC3( INDF, BSMP, ESMP, BDET, EDET,
     :                         %VAL( IPNTR ), %VAL( IPDATA ), NDISP,
     :                         OFFSET, DTINDX, SCALE, XLMT, YLMT, IDC,
     :                         'QNAME', 'XNAME', 'XTYPE', 'COMMENT',
     :                         'HISTORY', COLOUR, CURSOR, CLRBLK,
     :                         SCNDIR, STATUS )

*  If graphic device can not be cleared partly, quit the application.
                  IF ( .NOT.CLRBLK ) THEN
                     QUIT = .TRUE.

*  Otherwise, clear the field used to display instructions.
                  ELSE 
                     CALL SGS_CLRBL( -0.55, 1.2, -0.5, -0.13 )
                  END IF

               END IF

*  Go back for further action if neither 'Quit' nor 'Redraw display' is
*  specified.
            END DO

*  If looping was not requested, quit the loop.
         ELSE
            QUIT = .TRUE.

         END IF

*  Go back to redraw the display if no quit is specified.
      END DO

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO  997

*  Now save the data picture in the AGI data base. Select the 'FRAME' 
*  picture as current picture. 
      CALL AGI_SELP( PIC1, STATUS )

*  Set up a zone which covers the area used for the data curves.
      CALL SGS_ZONE( 0.0, 1.0, 0.0, 1.0, PLOTZN, STATUS )

*  Change the Y axis world coordinates to correspond to the displayed
*  data values. The X axis is left unchanged. This is because the
*  displayed units (in arc-mins) may decrease from left to right, and
*  AGI can only cope with coordinates which increase from left to right.
      CALL SGS_SW( 0.0, 1.0, YLMT( 1 ), YLMT( 2 ), STATUS ) 
 
*  Save the current SGS zone in the database.
      CALL AGS_SZONE( 'DATA', 'IRAS90_TRACECRDD', PIC2, STATUS )

*  A transformation is now stored with the picture which relates the
*  stored X axis values to the displayed values (in arc-mins). This
*  allows reversed X axis coordinates to be stored (i.e. coordinates
*  which decrease from left to right).
      IF ( SCNDIR ) THEN
         COSCL( 1 ) = XLMT( 1 ) - XLMT( 2 )
         COFST( 1 ) = XLMT( 2 )
      ELSE
         COSCL( 1 ) = XLMT( 2 ) - XLMT( 1 )
         COFST( 1 ) = XLMT( 1 )
      END IF

*  The scale and offset for the y coordinates transformation leaves the
*  stored values unchanged.
      COSCL( 2 ) = 1.0
      COFST( 2 ) = 0.0

*  Store the transformation in the data base.
      CALL IRM_LITRR( COSCL, COFST, STATUS )

*  Close SGS and AGI.
 997  CONTINUE

      CALL AGS_DEACT( STATUS )
      CALL AGI_END( PIC0, STATUS )
      CALL AGI_CANCL( 'DEVICE', STATUS )

*  If log file was opened, and some logging has been done, close the 
*  file and cancel the parameter.
 998  CONTINUE

      IF( LOGING ) THEN 
         IF ( LOG .GE. 0 ) THEN
            CALL FIO_CANCL( 'LOGFILE', STATUS )

*  Or if log file was opened, but no logging has been done, cancel the
*  parameter and then delete the file.
         ELSE 
            CALL FIO_FNAME( FID, FILNAM, STATUS )
            CALL FIO_CANCL( 'LOGFILE', STATUS )
            FLNMLN = CHR_LEN( FILNAM )
            CALL FIO_ERASE( FILNAM( : FLNMLN ), STATUS )
         END IF

      END IF

*  Release the temporary work space.
      CALL PSX_FREE( DPNTR, STATUS )
      CALL PSX_FREE( IPNTR, STATUS )

*  Close down the IRC_ package.
 999  CONTINUE
      CALL IRC_CLOSE( STATUS )

*  Annul the NDF identifier.
      CALL NDF_ANNUL( INDF, STATUS )

*  Store the processed file for use by later applications.
      CALL IRM_LISTN( 'NDFLIST', IGRP, 'TRACECRDD', STATUS )

*  Delete the group containing the input NDF name.
      CALL GRP_DELET( IGRP, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If a parameter null or abort value was given, annul the error.
      IF( STATUS .EQ. PAR__NULL .OR. STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'TRACECRDD_ERR3',
     :   'TRACECRDD: Error displaying CRDD traces.',
     :   STATUS )
      END IF

      END
