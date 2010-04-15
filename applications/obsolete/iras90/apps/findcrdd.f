
      SUBROUTINE FINDCRDD( STATUS )
*+
*  Name:
*     FINDCRDD

*  Purpose:
*     Accepts a list of sources, and prepares files specifying
*     corresponding CRDD data to be extracted.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL FINDCRDD( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     FINDCRDD enables the user to input and edit a list of source
*     positions which may be started as a new list, or initiated from
*     a previously prepared file. It then allows the user to input the
*     source size and waveband requirements associated with the
*     extraction of Survey CRDD data. It checks whether all the data for
*     any source can be provided from a single archive CRDD file, and if
*     not allows the user to return to the editing menu to put in dummy
*     sources to extract data from other archive files if required.
*     Finally it prepares files containing details of IRAS survey CRDD
*     data to be extracted by the succeeding program, EXCRDD.
*
*  ADAM Parameters:
*     AUGMENTNAME = _CHAR (Read)
*        Name of source to be augmented with size and wavebands.
*     BANDSREQ = LITERAL (Read)
*        Bands required entered as 12,25 etc.. [12,25,60,100].
*     CONFIRMADDEDIT = _LOGICAL (Read)
*        Confirmation that added or edited source details are correct
*        [Y].
*     CONFIRMDELETE = _LOGICAL (Read)
*        Confirmation that the source printed is to be deleted [N].
*     CONFIRMREQ = _LOGICAL (Read)
*        If .TRUE. added/ edited sources are to be confirmed.
*     CROSSCAN = _REAL (Read)
*        Size of required region in cross scan direction given in
*        arcmins. [0.0]
*     DATATYPE = LITERAL (Read)
*        (The facility that requires this is not available in FINDCRDD
*        version 1.0).
*
*        Type of survey data required: 0 = Don't know, 2 = PASS 2 CRDD
*        on tape, 3 = PASS 3 CRDD on exobyte.
*     DELETESONAME  = LITERAL (Read)
*        Name of the source to be deleted. If more than one source with
*        this name is present then each one is offered for deletion.
*
*        If the user enters ! to this prompt no more sources are to be
*        deleted.
*     DISPLAYORFILE = LITERAL (Read)
*        Controls whether certain information is displayed at the
*        terminal or written to a file.
*
*        The types of information affected are:-
*
*        Lists of sources prepared by selecting L from a menu,
*
*        Details of source regions which may fall partly outside plate
*           boundaries.
*
*        The options are:-
*        - D = Display on terminal
*        - F = File
*        - B = Both
*        - A = Ask user which he wants at time the display is prepared.

*        If the user enters ! to this prompt no display or file will be
*        prepared.
*        [A]
*     EDITSOURCENAME = LITERAL (Read)
*        Name of the source to be edited. If more than one source with
*        this name is present then each one is offered for editing.
*
*        If the user enters ! to this prompt no more sources are to be
*        edited.
*     INSCAN = _REAL (Read)
*        Size of required region in in_scan direction given in arcmins.
*        [120.0]
*     INSOURCEFILE1 = FILENAME (Read)
*        Name of a file from which source positions are to be read. It
*        will have been created in this or a previous run of FINDCRDD.
*
*        If the user enters ! to this prompt no file is read and the
*        user is returned to the menu.
*     MAINCHOICE = LITERAL (Read)
*        Main menu choice
*        - I = Input or edit source positions
*        - S = Find survey data
*        - Q = Exit FINDCRDD
*        [I]
*     MENU11CHOICE = LITERAL (Read)
*        Users choice from 'Select data to use' menu within the Input
*        Source Positions option:-
*        - N = Input new list of sources,
*        - C = Modify current list,
*        - F = Modify list from file,
*        - L = Display sources selected,
*        - P = Change number of lines displayed on a page
*        - R = Return to main menu,
*        - Y = Accept selected source list.
*
*        If the user enters ! to this prompt the menu is redisplayed.
*        [N].
*     MENU12CHOICE = LITERAL (Read)
*        Users choice from 'Edit source list' menu within the Input
*        source positions option:-
*        - A = Add new sources to current list,
*        - E = Edit data in current list of sources,
*        - D = Delete sources from current list,
*        - L = Display current list of sources,
*        - P = Change number of lines displayed on a page
*        - C = Change coordinate system in which position is entered,
*        - Y = Accept source list.
*
*        If the user enters ! to this prompt the menu is redisplayed.
*        [Y]
*     MENU2CHOICE = LITERAL (Read)
*        Option from input-size-and-waveband menu in survey data option.
*        - Z = Enter region size for all sources
*        - W = Enter wavebands required for all sources
*        - I = Enter region size and wavebands for individual sources
*        - L = Display current list of sources
*        - P = Change number of lines displayed on a page
*        - R = Return to main menu
*        - Y = Accept source list
*
*        If the user enters ! to this prompt the menu is redisplayed
*     MSG_FILTER = _INTEGER (Read)
*        Controls whether menus are displayed and updated source lists
*        are displayed after each change. Sources can be listed by
*        selecting L from a menu no matter what the value of MSG_FILTER.
*        - 1 = Neither source lists nor menus are shown.
*        - 2 = Source lists are shown automatically, menus are not.
*        - 3 = Menus are shown automatically, source lists are not.
*        - 4 = Both source lists and menus are shown automatically.
*        [3]
*     NEXTPAGE = _LOGICAL (Read)
*        Press return to display next page of source listing, scan
*        listing etc.
*
*        If the user enters N or ! to this prompt the next page will not
*        be displayed and the user will be returned to the menu.
*        [Y]
*     OFFEDGEFILE = FILENAME (Read)
*        Name of a file to contain details of sources which may have
*        some of the required region off the edge of the plate.
*
*        The default name of this file is generated automatically as
*        'source name'_OFFEDGE ( or the corresponding lower case on a
*        UNIX machine) and offered to the user.
*     OUTDESCFILE = FILENAME (Read)
*        Name of a file to contain messages about plate files created
*        and tapes to be used in subsequent EXCRDD runs.
*
*        The name of this file is generated automatically as
*        FINDCRDD_PLATES_REQ. ( or the corresponding lower case on a
*        UNIX machine)
*     OUTSOURCEFILE1 = FILENAME (Read)
*        Name of file in which to save source positions created in
*        input or edit source positions.
*
*        If the user enters ! to this prompt no file will be saved.
*     OUTSOURCEFILE2 = FILENAME (Read)
*        Name of file in which to save source positions created in
*        input size and wavebands part of the survey data option.
*     PAGELENGTH = INTEGER (Read)
*        Number of lines per page on display
*     RETURNMAIN = _LOGICAL (Read)
*        Option to return to main menu (.TRUE.) if at the end of the
*        subroutine which find what plates the sources are on, it has
*        found a part of a source region off the edge of the plate. It
*        allows the user to put in a dummy source in the next plate.
*     RETURNSWMENU = _LOGICAL (Read)
*        Set .TRUE. if on finding some sources without size or wavebands
*        the user wants to return to the Size and Wavebands Menu to add
*        them. If set .FALSE. those sources for which there is either no
*        inscan length, or no wavebands required will not be processed.
*     SOURCECOORD1 = _CHAR (Read)
*        First coordinate of source position in current coordinate
*        system. The formats available for both coordinates, are:-
*
*        Equatorial:-
*        -   Style 1 :- RA = 12hrs 3m 0.02s  DEC = -33deg 23m 0.0s
*        -   Style 2 :- 12h 3m 0.02s  -33d 23m 0.0s
*        -   Style 3 :- 120300.02   -332300.0  (hhmmss.ss and ddmmss.ss)
*        -   Style 4 :- 12 03 0.02  -33 23 00.0
*        -   Style 5 :- 12.050006  -33.383333  (fractional values in hrs
*                                              (RA) or deg (Dec))
*
*        Ecliptic:-
*        -   Style 1 :- ELONG= 12deg 3m 0.02s  ELAT= -33deg 23m 0.0s
*        -   Style 2 :- 12deg 3m 0.02s  -33d 23m 0.0s
*        -   Style 3 :- 0120300.02   -332300.0  (dddmmss.ss and
*                                                ddmmss.ss)
*        -   Style 4 :- 12 03 0.02  -33 23 00.0
*        -   Style 5 :- 12.050006  -33.383333  (fractional values in
*                                               degrees)
*
*        Galactic:-
*        -   Style 1 :- GLONG= 12deg 3m 0.02s  GLAT= -33deg 23m 0.0s
*        -   Style 2 :- 12deg 3m 0.02s  -33d 23m 0.0s
*        -   Style 3 :- 0120300.02   -332300.0  (dddmmss.ss and
*                                                ddmmss.ss)
*        -   Style 4 :- 12 03 0.02  -33 23 00.0
*        -   Style 5 :- 12.050006  -33.383333  (fractional values in
*                                               degrees)
*
*        See SOURCECOORDSYS for changing coordinate system
*     SOURCECOORD2 = _CHAR (Read)
*        Second coordinate of source position in current coordinate
*        system. See SOURCECOORD1 for the formats available.
*
*        See SOURCECOORDSYS for changing coordinate system
*     SOURCECOORDSYS = LITERAL (Read)
*        The identifier of the coordinate system the user wants to make
*        the current coordinate system. The options available are
*        Equatorial, Ecliptic, and Galactic. For Equatorial and
*        Ecliptic the user should also enter the equinox as (B1950) or
*        (J2000). If the equinox is ommitted (B1950) is assumed. If the
*        prefix B or J is ommitted B is assumed. Therefore the minimum
*        abreviations are EQ (Equatorial(B1950)), EQ(J2000), EC
*        (Ecliptic(B1950)), and GA. The default is the last coordinate
*        system used or Equatorial(B1950).
*     SOURCELISTFILE = FILENAME (Read)
*        The file name to which to file a source list as it would be
*        displayed on a terminal, for printing afterwards.
*     SOURCENAME = LITERAL (Read)
*        The source name for this source, may be up to eight characters
*        and must be a valid file name. Any characters input in lower
*        case will be translated to upper case automatically.
*        Since the source name used as an identification prefix for
*        all files relating to that source IT IS STRONGLY ADVISED THAT
*        EACH SOURCE NAME SHOULD BE UNIQUE.
*     SOURCETITLE = LITERAL (Read)
*        Full title of source for printout headings etc
*        (up to 40 chars). [SOURCENAME]
*     SPFARCHFILE = FILENAME (Read)
*        The name of the SPFARCH SDF file
*        [FINDCRDD_DIR:NEWSPFARCH]
*
*
*  Notes:
*     -  No ADAM parameter is given for the OUTPUT HDS FILE. An output
*     HDS file is generated for each plate, containing information on
*     the data EXCRDD needs to extract form that plate. This file is
*     automatically given the name "PLATExxxx" where xxxx is the plate
*     number, and therefore the program does not need to access the
*     parameter system. ( or the corresponding lower case name on a UNIX
*     machine)

*  [optional_A_task_items]...
*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     6-JUN-1991 (DCP):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_PAR'          ! IRAS 90 General constants
      INCLUDE 'IRA_PAR'          ! IRAS Astrometry constants
      INCLUDE 'IRA_ERR'          ! IRAS Astrometry errors
      INCLUDE 'MSG_PAR'          ! Message reporting constants
      INCLUDE 'MSG_ERR'          ! Message reporting errors
      INCLUDE 'ERR_PAR'          ! Error reporting constants
      INCLUDE 'ERR_ERR'          ! Error reporting errors
      INCLUDE 'PAR_ERR'          ! Parameter errors

*  Global Variables:
      INCLUDE 'FICOMN' ! Common blocks for FINDCRDD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL CONREQ             ! Returned confirmation required value
      CHARACTER * ( 1 ) DISFIL   ! Returned display or file value
      INTEGER ILEVEL             ! Returned interaction level
      INTEGER MAXLEN             ! Returned number of lines on page
      CHARACTER * ( 2 ) MMENU    ! Choice from main menu
      LOGICAL RETMAI             ! Set TRUE if the user wants to return
                                 ! to the main menu after off edge
                                 ! source in FIND16
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Display program identifying message
      CALL MSG_OUT( ' ', '                    FINDCRDD Version 1.0',
     : STATUS )

*  Initialisation of any systems that require initialisation, error
*  message contexts etc.
      CALL FIND24( 'CONFIRMREQ', 'DISPLAYORFILE', 'MSG_FILTER',
     : 'PAGELENGTH', CONREQ, DISFIL, ILEVEL, MAXLEN, STATUS )

*  Set MMENU to select_from_menu; M.
      MMENU = 'M'

*  Loop for choice from FINDCRDD main menu
 100    CONTINUE                   ! Start of 'DO WHILE' loop

*  *********************************************************************
*  If the option is select_from_menu, M.
*  *********************************************************************
      IF ( (MMENU .EQ. 'M') .AND. (STATUS .EQ. SAI__OK) ) THEN

         CALL FIND30( ILEVEL, 'MAINCHOICE', MMENU, STATUS )

      END IF

*  FIND30 displays a menu of:-
*      I = Input source positions [ Default ]
*      S = Find survey data
*      Q = Exit FINDCRDD
*  and prompts the user to select an option
*  The character representing the option selected is returned in MMENU.


************************************************************************
* MAIN MENU OPTION I
*    Input Source Positions
************************************************************************

      IF ( (MMENU .EQ. 'I') .AND. (STATUS .EQ. SAI__OK) ) THEN

         CALL FIND25( CONREQ, DISFIL, ILEVEL,
     :   'CONFIRMADDEDIT','CONFIRMDELETE', 'DELETESONAME',
     :   'DISPLAYORFILE', 'EDITSOURCENAME', 'INSOURCEFILE1',
     :   'PAGELENGTH', 'MENU11CHOICE', 'MENU12CHOICE', 'NEXTPAGE',
     :   'OUTSOURCEFILE1','SOURCECOORD1', 'SOURCECOORD2',
     :   'SOURCECOORDSYS',
     :   'SOURCELISTFILE', 'SOURCENAME', 'SOURCETITLE',
     :   MAXLEN, MMENU, STATUS )

      END IF

*  FIND25 allows the user to select a source list to modify and to
*  input, edit or delete source information.
*
*  FIND25 first asks the user to choose a source list to edit with the
*  menu:-
*       SELECT DATA TO USE MENU
*    N = Input new list of sources
*    C = Modify current list
*    F = Modify list from file
*    L = Display sources selected
*    P = Change number of lines displayed on a page
*    R = Return to main menu
*    Y = Accept selected source list
*
*  N, Y, and R will take you out of this menu
*  C, F, L, and P will return to this menu
*
*  If the user selects something other than a new source list, and
*  having done such checking as he needs, he accepts the source list,
*  He will be given the edit source list menu
*
*    EDIT SOURCE LIST MENU
*    A = Add new sources to current list
*    E = Edit data in current list of sources
*    D = Delete sources from current list
*    L = Display current list of sources
*    P = Change number of lines displayed on a page
*    C = Change coordinate system in which position is entered
*    R = Return to main menu
*    Y = Accept source list
*
*  If the user selects a new source list the options to change
*  coordinate system, and add new sources will be activated
*  automatically, before the user is offered the Edit source list menu.
*
*  The portions of the source data that are entered for each source
*  consist of source name, title, and position in the currently selected
*  coordinate system.
*  The source name is subsequently used to identify the source.
*  The source positions may be entered in equatorial, ecliptic, or
*  galactic coordinate system.
*  The positions of the sources in 1950 equatorial coordinates are
*  found. The details of the sources in both sets of coordinates, and
*  any other details input in, for example, the survey data option may
*  be stored in a file.


************************************************************************
* MAIN MENU OPTION S
*    Find Survey Data ( this prepares a set of files for EXCRDD )
************************************************************************

*  The stages in finding survey data consist of:-
*     Adding region size and waveband requirement information for each
*     source.
*     Finding the archive plate file in which each source's data is to
*     be found, and determining whether any data might be off the edge
*     of the file.
*     Examining each observation in the mission (as described in the
*     SPFARCH file) to determine if it is near enough to any source
*     for a scan to be needed from it.
*     Preparing an HDS file for each plate, to be read in by EXCRDD
*     telling it what data needs to be extracted from the plate.
*

************************************************************************
*  Add region size and wavebands required
************************************************************************

      IF ( (MMENU .EQ. 'S') .AND. (STATUS .EQ. SAI__OK) ) THEN

         CALL FIND26( DISFIL, ILEVEL, 'AUGMENTNAME', 'BANDSREQ',
     :   'CROSSCAN', 'DISPLAYORFILE', 'INSCAN',
     :   'PAGELENGTH', 'MENU2CHOICE',
     :   'NEXTPAGE', 'OUTSOURCEFILE2', 'RETURNSWMENU',
     :   'SOURCELISTFILE',
     :   MAXLEN, MMENU, STATUS )

      END IF


*  FIND26 allows the user to input or modify the size of the region, and
*  wavebands, he requires for each source.
*
*  The user is offered the menu:-
*  ADD REGION SIZE AND WAVEBANDS MENU
*    Z = Enter region size for all sources
*    W = Enter wavebands required for all sources
*    I = Enter region size and wavebands for individual sources
*    L = Display current list of sources
*    P = Change number of lines displayed on a page
*    R = Return to main menu
*    Y = Accept source list
*
*  After all editing is done the program checks that a valid region
*  size and waveband requirements have been entered for all sources.
*  If a source is found without valid parameters the user is offered
*  the choice of returning to input valid data or continuing with the
*  source being ommitted from further processing. The menu variable
*  is set to reflect the path the user wants to follow.


************************************************************************
* Find plate required for each source.
************************************************************************

      IF ( (MMENU .EQ. 'S') .AND. (STATUS .EQ. SAI__OK) ) THEN

         CALL FIND16( DISFIL, ILEVEL, 'DATATYPE', 'DISPLAYORFILE',
     :   'NEXTPAGE', 'OFFEDGEFILE', 'RETURNMAIN', MMENU, RETMAI,
     :   STATUS )

      END IF

*  IRAS boresight survey data is held in plates, each plate holding
*  all data for one region of the sky. This program calculates from
*  the position of each source the plate number required. It also
*  determines whether the area of source required is such that it
*  falls partly outside the region covered by the plate.
*
*  FIND16 finds the plate required for each source.
*  For each Source the program:-
*
*  Calculates the plate number for source.
*
*  Finds out whether details of the plate are already stored.
*  If they are not stored the program determines the identification
*  for the tape containing the plate, and the position of the plate
*  on the tape.
*
*  Sets up the cross linkages between the source record and the plate
*  record.
*
*  Determines whether the area of source required may fall partly
*  outside the region covered by the plate. This is not an accurate
*  check. The region size parameters define a box parallel to the
*  scan direction. We do not know the scan orientation which means
*  we do not know the orientation of the box. We make the worst
*  case assumption. If the required region could lie outside the plate
*  the program reports the fact and allows the user of obtaining more
*  detailed information on the size and position of the overflow.
*
*  If at the end of processing all sources, one or more are off the edge
*  of the plate the user is offered the option of returning to the main
*  menu. The menu variable reflects his choice.


************************************************************************
*  Find scans required
************************************************************************

      IF ( (MMENU .EQ. 'S') .AND. (STATUS .EQ. SAI__OK) ) THEN

         CALL FIND20( 'SPFARCHFILE', STATUS )

      END IF

*  FIND20 tests each observation to see whether it is within the
*  area required for each source position, and part of it called a scan
*  should be stored for extraction.
*
*  The subroutine uses the SPFARCH file which contains details of
*  position for each observation in the mission. The positions given
*  are estimates and therefore differ sightly from those of the Survey
*  Boresight file. This means that positions are sufficiently accurate
*  for scan selection but EXCRDD has to obtain more refined positions
*  for further processing.
*
*     For each SOP
*        For each source
*           Find whether the source was far away enough from the Sun
*           at the time of the SOP for the satellite possibly to have
*           looked close to the source.
*           If this criterion is passed then:-
*           For each observation within the SOP.
*              Find whether the observation passes through the required
*              region around the source position.
*              If the scan meets this criterion then store details of
*              the part of the observation required (scan) in a list
*              of scans.

************************************************************************
*  The following section is not yet implemented
************************************************************************
*
*      CALL IRCHSC( [arg]... )
*
*     The subroutine allows the user to
*     i)   Display storage reqirements for EXCRDD output
*     ii)  Divide a plate into two or more sections and allow the user
*     to partition sources between them
*     iii) Select all scans for a source
*     iv)  Review scans for a source and select those required.
*     v)   Draw maps of the source region showing the recgions covered
*     by all, or selected scans.
*     vi)  List scans for each source including cross scan distance from
*     source

************************************************************************
*  Prepare output HDS files
************************************************************************

      IF ( (MMENU .EQ. 'S') .AND. (STATUS .EQ. SAI__OK) ) THEN

         CALL FIND29( 'OUTDESCFILE', MMENU, STATUS )

      END IF
*
*     FIND29 creates the HDS output files which can be read by the
*     Boresight Survey Data Extraction Program, EXCRDD.
*
*     Each plate record is used to generate a separate output file.
*     The output file contains details of:-
*     The plate number, and details of the archive tape if this is
*     known.
*     A list of all the scans required from the plate sorted in start
*     time order, and marked where one scan obtains CRDD data that is
*     also required by the suceeding scan. The scan list contains
*     information about the start time and end time of each scan.
*     A list of all the sources for which data is to be extracted from
*     this plate. The list contains name, title, position, size and
*     waveband requirements for each source.
*
*     The menu variable is set to display the FINDCRDD main menu next.

* **********************************************************************
*  Return to select from FINDCRDD main menu
* **********************************************************************
      IF ( (MMENU .EQ. 'M') .AND. (STATUS .EQ. SAI__OK) ) THEN

         GO TO 100

      END IF

* **********************************************************************
*  If the menu variable is not set to 'S' or 'M' the the program
*  comes here to wind up
* **********************************************************************

* Tidying up of initialised systems etc.
      CALL FIND14( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'FINDCRDD_ERR',
     :   'FINDCRDD: Error in looking for IRAS data.',
     :   STATUS )
      END IF

      END
