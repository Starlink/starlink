      SUBROUTINE LUTHILITE( STATUS )
*+
*  Name:
*     LUTHILITE

*  Purpose:
*     Highlights a colour table of an image-display device.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL LUTHILITE( STATUS )

*  Description:
*     This routine adjusts the colour table of a nominated plane of
*     an IDI-supported image display, such as X-windows.  The adjustment
*     is like a highlight pen, only here it can traverse the colour
*     table, widen or thin is under mouse, joystick or trackerball
*     button control; and the colour of the highlight is arbitrary.
*     Thus particular features in an image may readily become visible.

*     For an Ikon or X-windows, moving the mouse left or right shifts
*     the highlight in the colour table towards lower and higher colour
*     indices respectively.  The highlight does not rotate around the
*     colour table.  Pressing the left button of the mouse reduces the
*     width of the highlight by one colour index.  Pressing the centre
*     button increases the width of the highlight by one colour index.
*     Hitting the right-hand button ends the modification of the colour
*     table.

*     The colour table may be viewed during its manipulation without
*     permanently altering the display memory.  The colour-table
*     display is situated via the cursor, and will disappear once the
*     highlighting is complete.

*  Usage:
*     luthilite colour [device] [plane] [view]

*  ADAM Parameters:
*     COLOUR() = LITERAL (Read)
*        The colour to be used as a highlight.  It is either of these
*        alternatives.
*
*          o  A named colour from the standard colour set, which may
*          be abbreviated.  If the abbreviated name is ambiguous the
*          first match is selected.  The case of the name is ignored.
*          Some examples are "Seagreen", "Violet", and "Orchid".
*
*          o  Normalised red, green, and blue intensities separated by
*          commas or spaces.  Each value must lie in the range 0.0--1.0.
*          For example, "1.0,1.0,0.5" would give a pale yellow.
*     DEVICE = DEVICE (Read)
*        The name of the image-display device whose lookup table is to
*        be adjusted.  The name of the base plane should be given even
*        if the overlay lookup table is to be adjusted.
*        [Current image display]
*     FULL = _LOGICAL (Read)
*        If FULL is TRUE, the whole colour-table for the device is
*        may be highlighted, including the reserved pens.  When FULL
*        is FALSE, the reserved pens in the palette are unaltered.
*        [FALSE]
*     PLANE = _INTEGER (Read)
*        The number of the memory plane whose lookup table is to be
*        manipulated.  If it is null the base (image) memory's lookup
*        table is adjusted.  The base memory is 0 and overlays are
*        numbered consecutively from 1.  For an Ikon the only overlay
*        plane is 1.  PLANE is only permitted to have a value in the
*        range 0 to the number of planes minus one. [0]
*     VIEW = _LOGICAL (Read)
*        If TRUE the colour table is displayed during its manipulation.
*        [FALSE]

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Examples:
*     luthilite red
*        Highlights the colour table on the current image-display
*        device with a red marker.
*     luthilite red full
*        Highlights the colour table and palette on the current
*        image-display device with a red marker.
*     luthilite skyblue xwindows
*        Highlights the colour table on the xwindows device with a
*        sky-blue marker.
*     luthilite [1.0,1.0,0.3] ikon 1
*        Highlights the colour table on the Ikon overlay plane in a
*        pale yellow.
*     luthilite red view
*        Highlights the colour table on the current image-display
*        device with a red marker.  The colour table is displayed
*        during the highlighting.

*  Notes:
*     -  Only Ikons and X-windows are supported.
*     -  Initially, the highlight has a width of two colour indices,
*     and it is located at the second lowest colour index.  The maximum
*     width of the highlight is the larger of six and a quarter of the
*     colour table, but may be narrower when there are less than 12
*     colour indices.  Should the highlight prove to be unsuitable, it
*     may be made invisible by reducing the width to zero.
*     -  The rate of motion of the highlight is a function of the
*     speed of cursor movement in addition to the cursor position.
*     For a given cursor displacement slow motion moves the highlight
*     more slowly, and faster motion moves it more rapidly.  This
*     permits both fine control and swift change in the highlight's
*     location.

*  Algorithm:
*     -  Determine whether or not the reserved pens are to be ignored.
*     -  Ensure the the clear flag is set to "no clear".
*     -  Associate the image display and get an identifier for it.
*     -  Check the attributes of the device, i.e. that it has at least
*     one cursor, at least two triggers and a locator.
*     -  Obtain the number of memory planes and the maximum number of
*        LUT entries.
*     -  Obtain the plane number whose LUT is to be highlight.
*     -  Read the input LUT.
*     -  Obtain the RGB intensities of the highlight colour.
*     -  Determine whether the colour table is to be displayed.  If it
*     is the following operations are performed.
*        o  Inquire the size of the display in pixels and the zoom.
*        o  Check whether there is room to draw the colour table.
*        o  Move the cursor to the middle of the display and make it
*        visible.
*        o  Set up interactions to move the cursor.  Loop until a
*        trigger is pressed.  Read the cursor position.  Make the cursor
*        invisible.  Cancel the interaction bindings.
*        o  Create workspace to store the existing memory.  Set up the
*        transfer window for this region.  Dump the existing data to the
*        work array.
*        o  Create a ramp in workspace for the colour table ignoring
*        the reserved pens.  Draw the colour-table as a series of the
*        ramps adjusting the transfer window accordingly.  Tidy the
*        ramp workspace.
*     -  Set up the interactions and tell the user what to do.
*     -  Loop doing the LUT-modification interaction. When the last
*     trigger is fired exit from the loop.  If the first trigger is
*     fired write the input LUT to the memory and restart the
*     interaction.  If the second trigger is fired switch the
*     padding-colour flag.  Find an effective displacement after
*     applying correction factors for the cursor speed and the number
*     of colour-table entries.  Call a routine to evaluate the new LUT.
*     Write the new LUT in the device's colour table.
*     -  If there is a colour table display, set up the original
*     transfer window, and write the captured data back to the memory.
*     Tidy the workspace.
*     -  Annul the device.

*  Related Applications:
*     KAPPA: CRELUT, LUTFLIP, LUTROT, LUTTWEAK.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 April 30 (MJC):
*        Original version.
*     1991 June 5 (MJC):
*        Altered for new, X-windows, IDI.  Capability 14 is 2**n rather
*        than n.
*     1991 July 22 (MJC):
*        Converted RGB parameter to the more-sophisticated COLOUR.
*     1991 November 15 (MJC):
*        Made exit trigger number 2.  Inceased maximum number of
*        triggers to 64.
*     1991 November 21 (MJC):
*        Fixed bug so that highlight may appear in but one colour index.
*     1992 February 10 (MJC):
*        Permit the highlight width to be zero.  Normalised the
*        highlight displacement by the effective number of colour
*        indices.
*     1992 February 29 (MJC):
*        Added fine tuning using the cursor speed, and computed the
*        displacements in floating point to give a smoother feel.
*     1992 March 26 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1992 August 5 (MJC):
*        Used new capability 18 to obtain the true number of
*        colour-table entries, which may not be a power of 2, as
*        returned by capability 14.
*     1992 November 24 (MJC):
*        Reinstated the one-to-one correspondence between the lookup
*        table and the intensity transformation table, lost by an
*        undocumented change to X-windows driver.
*     1994 April 29 (MJC):
*        Added FULL parameter.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'CTM_PAR'          ! Colour-table-management constants
      INCLUDE 'IDI_PAR'          ! IDI constants
      INCLUDE 'IDI_ERR'          ! IDI error codes
      INCLUDE 'PAR_ERR'          ! Parameter-system error codes

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER BORDER             ! Minimum border around colour table
      PARAMETER ( BORDER = 2 )

      INTEGER MXSPED             ! Number of speed correction factors
      PARAMETER ( MXSPED = 5 )

      INTEGER NMEMAX             ! Maximum number of memories
      PARAMETER ( NMEMAX = 10 )

*  Local Variables:
      INTEGER
     :  ACTVAL,                  ! Number of values returned
     :  CAPID,                   ! IDI capability-inquiry code
     :  BRUSH,                   ! Brush width in colour indices
     :  CTPTR,                   ! Pointer to workspace for colour-table
                                 ! display
     :  DEPTH,                   ! Depth of the data (bits/pixel)
     :  DID,                     ! Display identifier
     :  DSIZE( 2 ),              ! Display dimensions
     :  EMPNTR,                  ! Pointer to workspace for storing
                                 ! exisiting memory data
     :  EXTRN,                   ! Exit trigger for ending LUT
                                 ! highlighting
     :  HILICI,                  ! Colour index to be highlighted or
                                 ! made normal
     :  IDSTAT,                  ! IDI status
     :  INTID,                   ! Interactor identifier
     :  INTOP,                   ! Interactive operation number
     :  INTTY,                   ! Interactor type for zooming control
     :  IPENDI,                  ! Pen displacement
     :  ITTDEP( NMEMAX ),        ! ITT depths of available memories
     :  I, J,                    ! Loop counters
     :  LUTCAP                   ! Max. depth of LUTS capability code

      INTEGER
     :  MEMDEP( NMEMAX ),        ! Depths of available memories
                                 ! highlighted
     :  MEMID,                   ! Memory identifier whose LUT is to be
     :  MEMIDS( NMEMAX ),        ! Available memories
     :  MEMTYP,                  ! Memory type
     :  MODCON,                  ! Configuration mode
     :  MXBRSH,                  ! Maximum brush width
     :  NCHAR,                   ! Number of characters in IDI
                                 ! interaction instruction message
     :  NCHAR1,                  ! Number of characters in interaction
                                 ! message
     :  NCONF,                   ! Configuration number
     :  NDATA,                   ! Number of pixels overwritten by the
                                 ! colour-table display
     :  NINTS( 1 ),              ! Number of LUT entries
     :  NLOC( 1 ),               ! Number of locators
     :  NLUTE,                   ! Number of entries in the VLUT less
                                 ! the reserved pens.
     :  NMEM,                    ! Number of image memories
     :  NRESP,                   ! Number of reserved pens
     :  NTRIG( 1 )               ! Number of available triggers

      INTEGER
     :  NUMCUR,                  ! Cursor number
     :  NVAL,                    ! Number of values expected
     :  NXD,                     ! X size of displayed memory
     :  NYD,                     ! Y size of displayed memory

     :  OBJID,                   ! Object identifier
     :  OBJTY,                   ! Object type
     :  OUTMID,                  ! Memory pointed at by cursor
     :  OX,                      ! X co-ordinate for origin of transfer
                                 ! window
     :  OY,                      ! X co-ordinate for origin of transfer
                                 ! window
     :  PACK,                    ! Packing factor
     :  TRICAP,                  ! No. of triggers capability code
     :  TRIGS( IMAXTR ),         ! Trigger status
     :  WIDTH                    ! Width of the colour table display in
                                 ! pixels

      INTEGER
     :  XC,                      ! X display centre
     :  XCORR,                   ! X speed-factor index
     :  XDISP,                   ! Locator x displacement
     :  XOFF,                    ! X memory offset
     :  XP,                      ! X cursor position
     :  XSIZ( NMEMAX ),          ! X sizes of available memories
     :  YC,                      ! Y display centre
     :  YDISP,                   ! Locator y displacement
     :  YOFF,                    ! Y memory offset
     :  YP,                      ! Y cursor position
     :  YSIZ( NMEMAX ),          ! Y sizes of available memories
     :  ZOOM                     ! Zoom factor

      REAL
     :  HGLSCL,                  ! Scale factor to normalise the
                                 ! highlight displacements
     :  ITT( CTM__MXPEN ),       ! Intensity transformation table
     :  OLUT( 3 ),               ! Video lookup table entry
     :  PENDIS,                  ! Cursor/pen displacement
     :  RGB( 3 ),                ! RGB colour of the highlight brush
     :  TLUT( 3, CTM__MXPEN ),   ! Modified video lookup table
     :  VLUT( 3, CTM__MXPEN ),   ! Video lookup table on input
     :  XSPFAC( MXSPED )         ! X speed-related displacement-
                                 ! correction factors

      CHARACTER * ( DAT__SZLOC ) ! Locators to:
     :  CTLOC,                   ! Workspace for the colour-table
                                 ! display
     :  MEMLOC                   ! Workspace to save the memory
                                 ! overwritten by the colour table

      CHARACTER * 80
     :  BUFER1,                  ! Text message from IDI
     :  BUFFER * 256             ! Text messages from IDI

      LOGICAL                    ! True if:
     :  DEVCAN,                  ! Device is to be cancelled on exit
     :  FIRST,                   ! There has been no reading of the
                                 ! cursor position
     :  FULL,                    ! Ignore the reserved pens
     :  LOOP,                    ! The interaction is to continue
     :  VIEW                     ! A temporary colour-table display
                                 ! will be drawn

*  Local Data:
      DATA XSPFAC / 0.4, 0.7, 1.0, 1.2, 1.5 /

*.

*    Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Use reserved pens?
*    ==================
      CALL PAR_GET0L( 'FULL', FULL, STATUS )

*    Set the number of reserved pens.
      IF ( FULL ) THEN
         NRESP = 0
      ELSE
         NRESP = CTM__RSVPN
      END IF

*    Associate a device.
*    ===================

*    Assume that the application will work and the chosen device is
*    to be annulled rather than cancelled upon completion.

      DEVCAN = .FALSE.

*    Just want to highlight what is already there.  It may have
*    been produced by GKS without updating the IDI workstation
*    status table.  Without IDI context, IDI would normally
*    reset the device.  The following call prevents the reset.

      CALL IDI_CLRFG( 1 )

*    Open IDI for a device obtained through the parameter system.

      CALL IDI_ASSOC( 'DEVICE', 'READ', DID, STATUS )

      IF ( STATUS .NE. IDI__OK ) THEN

*       The image display should not be recorded.

         DEVCAN = .TRUE.
         GOTO 999
      END IF

*    Make sure we have selected the base configuration.
*    ==================================================

      NCONF = 0
      CALL IIDSEL( DID, NCONF, IDSTAT )

*    Abort if an error has occurred.  Note IDI does not use
*    inherited status.

      IF ( IDSTAT .NE. IDI__OK ) THEN

*       Obtain a meaningful IDI error message.

         CALL KPG1_IDERR( 'IIDSEL_ERR', IDSTAT, STATUS )
         GOTO 999
      END IF

*    Find which image plane is required.
*    ===================================

*    Find the number of image (MEMTYP=1) planes available for the
*    image-display device.  Only NMEM and MEMIDS are required.

      MEMTYP = 1
      CALL IIDQDC( DID, NCONF, MEMTYP, NMEMAX, MODCON, MEMIDS, XSIZ,
     :             YSIZ, MEMDEP, ITTDEP, NMEM, IDSTAT )

*    Abort if an error has occurred.  Note IDI does not use
*    inherited status.

      IF ( IDSTAT .NE. IDI__OK ) THEN

*       Obtain a meaningful IDI error message.

         CALL KPG1_IDERR( 'IIDQDC_ERR', IDSTAT, STATUS )
         GOTO 999
      END IF

*    Get the memory required with in the allowed range.
*    ==================================================

      CALL PAR_GDR0I( 'PLANE', 0, 0, NMEM-1, .FALSE., MEMID, STATUS )

*    A null status indicates that the base image plane is to be
*    used.

      IF ( STATUS .EQ. PAR__NULL ) THEN
         MEMID = 0
         CALL ERR_ANNUL( STATUS )
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
         GOTO 999
      END IF

*    Get the number of triggers.
*    ===========================

      TRICAP = 55
      NVAL = 1
      CALL IIDQCI( DID, TRICAP, NVAL, NTRIG, ACTVAL, IDSTAT )

*    Abort if an error has occurred.  Note IDI does not use
*    inherited status.

      IF ( IDSTAT .NE. IDI__OK ) THEN

*       Obtain a meaningful IDI error message.

         CALL KPG1_IDERR( 'IIDQCI_ERR', IDSTAT, STATUS )
         DEVCAN = .TRUE.
         GOTO 999
      END IF

*    Check that the image-display device has at least two triggers.
*    ==============================================================

*    These are numbered 0, 1, 2 etc.

      IF ( NTRIG( 1 ) .LT. 2 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'LUTHILITE_NOCURSOR',
     :     'LUTHILITE: The chosen device has insufficient triggers.',
     :     STATUS )

*       The image display should not be recorded.

         DEVCAN = .TRUE.
         GOTO 999
      END IF

*    Get the number of locators.
*    ===========================

      TRICAP = 50
      NVAL = 1
      CALL IIDQCI( DID, TRICAP, NVAL, NLOC, ACTVAL, IDSTAT )

*    Abort if an error has occurred.  Note IDI does not use
*    inherited status.

      IF ( IDSTAT .NE. IDI__OK ) THEN

*       Obtain a meaningful IDI error message.

         CALL KPG1_IDERR( 'IIDQCI_ERR', IDSTAT, STATUS )
         DEVCAN = .TRUE.
         GOTO 999
      END IF

*    Check that the image-display device has at least one locator.
*    =============================================================

*    These are numbered 0, 1, 2 etc.

      IF ( NLOC( 1 ) .LT. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'LUTHILITE_NOCURSOR',
     :     'LUTHILITE: The chosen device has no locator.', STATUS )

*       The image display should not be recorded.

         DEVCAN = .TRUE.
         GOTO 999
      END IF

*    Select memory visibility.
*    =========================

*    Select the current lookup table for the specified memory plane.
*    The use of VLUT = memory identifier is arbitrary.  It does work
*    for GKS, and will be changed once an IDI problem is resolved.

      CALL IIMSLT( DID, MEMID, MEMID, 0, IDSTAT )

*    Abort if an error has occurred.  Note IDI does not use
*    inherited status.

      IF ( IDSTAT .NE. IDI__OK ) THEN

*       Obtain a meaningful IDI error message.

         CALL KPG1_IDERR( 'IIMSLT_ERR', IDSTAT, STATUS )
         DEVCAN = .TRUE.
         GOTO 999
      END IF

*    Find the maximum number of lookup-table entries.
*    ================================================

      LUTCAP = 18
      NVAL = 1
      CALL IIDQCI( DID, LUTCAP, NVAL, NINTS, ACTVAL, IDSTAT )

*    Abort if an error has occurred.  Note IDI does not use
*    inherited status.

      IF ( IDSTAT .NE. IDI__OK ) THEN

*       Obtain a meaningful IDI error message.

         CALL KPG1_IDERR( 'IIDQCI_ERR', IDSTAT, STATUS )
         DEVCAN = .FALSE.
         GOTO 999
      END IF

*    Input the current LUT.
*    ======================

*    Read the current LUT, only storing what is possible.  The buffer
*    is generous as most devices have no more than 256 levels.

      NLUTE = MIN( NINTS( 1 ) - NRESP, CTM__MXPEN )
      CALL IILRLT( DID, MEMID, NRESP, NLUTE, VLUT, IDSTAT )

*    Abort if an error has occurred.  Note IDI does not use
*    inherited status.

      IF ( IDSTAT .NE. IDI__OK ) THEN

*       Obtain a meaningful IDI error message.

         CALL KPG1_IDERR( 'IILRLT_ERR', IDSTAT, STATUS )
         GOTO 999
      END IF

*    Obtain the highlight colour and maximum width.
*    ==============================================

      CALL KPG1_GPCOL( 'COLOUR', RGB, STATUS )
      MXBRSH = MAX( MIN( 6, NLUTE / 2 ), NLUTE / 4 )

*    Display the LUT.
*    ================

*    Find whether or not the LUT is to be displayed.

      CALL PAR_GTD0L( 'VIEW', .FALSE., .TRUE., VIEW, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

      IF ( VIEW ) THEN

*       Inquire the physical size of the display using Query
*       Capabilities Integer code 12 returns the screen size in pixels.

         CAPID = 12
         CALL IIDQCI( DID, CAPID, 2, DSIZE, NVAL, IDSTAT )

*       Abort if an error has occurred.  Note IDI does not use
*       inherited status.

         IF ( IDSTAT .NE. IDI__OK ) THEN

*          Obtain a meaningful IDI error message.

            CALL KPG1_IDERR( 'IIDQCI_ERR', IDSTAT, STATUS )
            GOTO 999
         END IF

*       Inquire the screen zoom and pan.
*       ================================

         CALL IIZRSZ( DID, MEMID, XOFF, YOFF, ZOOM, IDSTAT )

*       Abort if an error has occurred.  Note IDI does not use
*       inherited status.

         IF ( IDSTAT .NE. IDI__OK ) THEN

*          Obtain a meaningful IDI error message.

            CALL KPG1_IDERR( 'IIZRSZ_ERR', IDSTAT, STATUS )
            GOTO 999
         END IF

*       Check whether or not there is room to draw the colour-table.
*       ============================================================
*
*       Note that IDI only works in device pixels and does not resample
*       arbitrarily sized arrays to match the transfer window.

*       Find the number of pixels visible.

         NXD = DSIZE( 1 ) / ( ZOOM + 1 )
         NYD = DSIZE( 2 ) / ( ZOOM + 1 )

*       Define the width of the colour-table display.

         WIDTH = MAX( 8, MIN( 15, NLUTE / 8 ) )

*       Check there is room given a border around the ramp.

         IF ( MAX( NXD, NYD ) .LT. NLUTE + 2 * BORDER .OR.
     :        MIN( NXD, NYD ) .LT. WIDTH + 2 * BORDER ) THEN
            STATUS = SAI__ERROR
            IF ( ZOOM .GT. 0 ) THEN
               CALL ERR_REP( 'LUTHILITE_NOROOM',
     :           'LUTHILITE: No room to accommodate the colour-table '/
     :           /'display.  Reduce the zoom factor.', STATUS )
            ELSE
               CALL ERR_REP( 'LUTHILITE_NOROOM',
     :           'LUTHILITE: No room to accommodate the colour-table '/
     :           /'display.', STATUS )
            END IF
            GOTO 999
         END IF

*       Move the cursor to the middle of the screen.
*       ============================================

*       A memory id = -1 sets the cursor position relative to the screen
*       origin.

         XC = ( DSIZE( 1 ) / ( ZOOM + 1 ) ) / 2
         YC = ( DSIZE( 2 ) / ( ZOOM + 1 ) ) / 2

         NUMCUR = 0
         CALL IICWCP( DID, -1, NUMCUR, XC, YC, IDSTAT )

*       Abort if an error has occurred.  Note IDI does not use
*       inherited status.

         IF ( IDSTAT .NE. IDI__OK ) THEN

*          Obtain a meaningful IDI error message.

            CALL KPG1_IDERR( 'IICWCP_ERR', IDSTAT, STATUS )
            GOTO 999
         END IF

*       Display the cursor by setting its visibility to .TRUE..
*       =======================================================

         CALL IICSCV( DID, NUMCUR, .TRUE., IDSTAT )

*       Abort if an error has occurred.  Note IDI does not use
*       inherited status.

         IF ( IDSTAT .NE. IDI__OK ) THEN

*          Obtain a meaningful IDI error message.

            CALL KPG1_IDERR( 'IICSCV_ERR', IDSTAT, STATUS )
            GOTO 999
         END IF

*       Set up interactions to move the cursor.
*       =======================================

*       Set up the mouse (interactor type = 0, interactor id = 0) to
*       control the cursor (object type = 1, object id = 0) by moving
*       it (interactive operation = 1). End the interaction by pressing
*       the right hand button (exit trigger number = 2).

         INTTY = 0
         INTID = 0
         OBJTY = 1
         OBJID = 0
         INTOP = 1
         EXTRN = 2
         CALL IIIENI( DID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :                IDSTAT )

*       Inquire the interactive operation and instruct the user.

         CALL IIIQID( DID, INTTY, INTID, BUFFER, NCHAR, IDSTAT )
         BUFFER( NCHAR + 1 : ) = ' to control the cursor.'
         CALL MSG_OUT( ' ', BUFFER( :NCHAR + 25 ), STATUS )

*       Set up the first trigger (interactor type = 5, interactor id
*       = 0) to have no visible effect (object type = 0, object id = 0)
*       and to execute applications code (interactive operation = 0 ).
*       End the interaction by pressing the right-hand button (exit
*       trigger number = number of triggers less one).

         INTTY = 5
         INTID = 0
         OBJTY = 0
         OBJID = MEMID
         INTOP = 0
         EXTRN = 2
         CALL IIIENI( DID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :                IDSTAT )

*       Inquire the interactive operation text.

         CALL IIIQID( DID, INTTY, INTID, BUFFER, NCHAR, IDSTAT )

*       Set up the second trigger (interactor type = 5, interactor id
*       = 1) to have no visible effect (object type = 0, object id = 0)
*       and to execute applications code (interactive operation = 0 ).
*       End the interaction by pressing the right-hand button (exit
*       trigger number = number of triggers less one).

         INTTY = 5
         INTID = 1
         OBJTY = 0
         OBJID = MEMID
         INTOP = 0
         EXTRN = 2
         CALL IIIENI( DID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :                IDSTAT )

*       Inquire the interactive operation and instruct the user forming
*       a composite message since pressing either the first or second
*       trigger has the same effect.  The second message begins in
*       upercase so convert the string to lowercase.

         CALL IIIQID( DID, INTTY, INTID, BUFER1, NCHAR1, IDSTAT )
         CALL CHR_LCASE( BUFER1( :NCHAR1 ) )
         BUFFER( NCHAR + 1: ) = ' or '//BUFER1( :NCHAR1 )//' to '/
     :     /'situate the colour-table display at the cursor position.'
         CALL MSG_OUT( ' ', BUFFER( :64 + NCHAR + NCHAR1 ), STATUS )

*       Inquire the exit trigger operation and instruct the user.

         CALL IIIQID( DID, 5, EXTRN, BUFFER, NCHAR, IDSTAT )
         BUFFER( NCHAR + 1: ) = ' to exit.'
         CALL MSG_OUT( ' ', BUFFER( :NCHAR + 9 ), STATUS )
         CALL MSG_BLANK( STATUS )

*       Get the cursor position.
*       ========================

*       Loop until a cursor position is defined or the exit trigger has
*       been hit.

         LOOP = .TRUE.
         DO WHILE( LOOP )

*          Reset the relevant triggers.

            TRIGS( 1 ) = 0
            TRIGS( 2 ) = 0
            TRIGS( 3 ) = 0

*          Execute the interactions.

            CALL IIIEIW( DID, TRIGS, IDSTAT )

*          Abort if an error has occurred.

            IF ( IDSTAT .NE. IDI__OK ) THEN

*             Obtain a meaningful IDI error message.

               CALL KPG1_IDERR( 'IIIEIW_ERR', IDSTAT, STATUS )
               GOTO 999
            END IF

*          Exit the loop.
*          ==============

*          The exit trigger has been pressed. End the loop.

            IF ( TRIGS( 3 ) .NE. 0 ) THEN
               LOOP = .FALSE.

            ELSE IF ( TRIGS( 1 ) .NE. 0 .OR. TRIGS( 2 ) .NE. 0 ) THEN

*             Read the cursor position.

               CALL IICRCP( DID, MEMID, NUMCUR, XP, YP, OUTMID, IDSTAT )

*             Abort if an error has occurred.

               IF ( IDSTAT .NE. IDI__OK ) THEN

*                Obtain a meaningful IDI error message.

                  CALL KPG1_IDERR( 'IICRCP_ERR', IDSTAT, STATUS )
                  GOTO 999
               END IF

*             A cursor postion has been obtained so exit the loop.

               LOOP = .FALSE.
            END IF
         END DO

*       Make the cursor invisible.
*       ==========================

         CALL IICSCV( DID, NUMCUR, .FALSE., IDSTAT )

*       Abort if an error has occurred.

         IF ( IDSTAT .NE. IDI__OK ) THEN

*          Obtain a meaningful IDI error message.

            CALL KPG1_IDERR( 'IICSCV_ERR', IDSTAT, STATUS )
            GOTO 999
         END IF

*       Cancel all the interactive input bindings.
*       ==========================================

         CALL IIISTI( DID, IDSTAT )

*       Create work space to store the existing memory over which the
*       colour-table will be plotted.
*       =============================================================

         NDATA = NLUTE * WIDTH
         CALL AIF_GETVM( '_INTEGER', 1, NDATA, EMPNTR, MEMLOC, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'LUTHILITE_WSP',
     :        'LUTHILITE: Error obtaining workspace for storing the '/
     :        /'existing memory.', STATUS )
            CALL DAT_ANNUL( MEMLOC, STATUS )
            GOTO 980
         END IF

*       Set up the transfer window.
*       ===========================

*       Define the origins of the transfer window.

         OX = MIN( MAX( BORDER, XP - NLUTE / 2 ),
     :                  DSIZE( 1 ) + 1 - BORDER - NLUTE )
         OY = MIN( MAX( BORDER, YP - WIDTH / 2 ),
     :                  DSIZE( 2 ) + 1 - BORDER - WIDTH )

         DEPTH = 8
         CALL IIMSTW( DID, MEMID, 0, NLUTE, WIDTH, DEPTH, OX, OY,
     :                IDSTAT )

*       Abort if an error has occurred.  Note IDI does not use
*       inherited status.

         IF ( IDSTAT .NE. IDI__OK ) THEN

*          Obtain a meaningful IDI error message.

            CALL KPG1_IDERR( 'IIMSTW_ERR', IDSTAT, STATUS )
            GOTO 980
         END IF

*       Dump the existing data into the work array.
*       ===========================================

*       Use a one-to-one correspondence for the intensity
*       transformation table.
         NVAL = 2 ** ( ITTDEP( 1 ) - 1 )
         DO I = 1, NVAL
            ITT( I ) = REAL ( I - 1 ) / REAL( NVAL - 1 )
         END DO
         CALL IILWIT( DID, MEMID, MEMID, 0, NVAL, ITT, IDSTAT )

*       This may not be implemented so watch out for this particular
*       error message.  If it is not implemented there is already a
*       one-to-one correspondence. Abort if an error has occurred.
*       Note IDI does not use inherited status.
         IF ( IDSTAT .NE. IDI__OK .AND. IDSTAT .NE. IDI__NOTIM ) THEN

*          Obtain a meaningful IDI error message.
            CALL KPG1_IDERR( 'IILWIT_ERR', IDSTAT, STATUS )
            GOTO 980
         END IF

*       Dump the part of the memory that will show the colour table.
         PACK = 1
         CALL IIMRMY( DID, MEMID, NDATA, 0, 0, DEPTH, PACK, 0,
     :                %VAL( EMPNTR ), IDSTAT )

*       Abort if an error has occurred.  Note IDI does not use
*       inherited status.

         IF ( IDSTAT .NE. IDI__OK ) THEN

*          Obtain a meaningful IDI error message.

            CALL KPG1_IDERR( 'IIMRMY_ERR', IDSTAT, STATUS )
            GOTO 980
         END IF

*       Create the ramp for the colour table.
*       =====================================

*       Create work space for the colour-table display.

         CALL AIF_GETVM( '_INTEGER', 1, NLUTE, CTPTR, CTLOC, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN

*          Report the error and tidy.

            CALL ERR_REP( 'LUTHILITE_WSP2',
     :        'LUTHILITE: Unable to obtain workspace to display the '/
     :        /'the colour table.', STATUS )
            GOTO 960
         END IF

*       Fill the array with element numbers less one to give colour
*       indices.  Omit the reserved pens.

         CALL KPG1_ELNMI( NRESP, NINTS( 1 ) - 1, NLUTE, %VAL( CTPTR ),
     :                    STATUS )

*       Draw the colour-table display.
*       ==============================

*       Write the ramp repeatedly at one-pixel displacements in y until
*       the desired width is obtained.

         OY = OY - 1
         DO  I = 1, WIDTH
            OY = OY + 1

*          Set up the transfer window, NLUTE by one pixel.

            CALL IIMSTW( DID, MEMID, 0, NLUTE, 1, DEPTH, OX, OY,
     :                   IDSTAT )

*          Abort if an error has occurred.

            IF ( IDSTAT .NE. IDI__OK ) THEN

*             Obtain a meaningful IDI error message.

               CALL KPG1_IDERR( 'IIMSTW_ERR', IDSTAT, STATUS )
               GOTO 960
            END IF

*          Write the colour-table to the portion of the memory to the
*          screen starting at position OX, OY in the transfer window.
*          The data are taken from the lowest byte of each integer word
*          in the data array (defined by PACK = 1, DEPTH = 8 ).

            CALL IIMWMY( DID, MEMID, %VAL( CTPTR ), NLUTE, DEPTH, PACK,
     :                   0, 0, IDSTAT )

*          Intercept any error.

            IF ( IDSTAT .NE. IDI__OK ) THEN

*             Obtain a meaningful IDI error message.

               CALL KPG1_IDERR( 'IIMWMY_ERR', IDSTAT, STATUS )
               GOTO 960
            END IF
         END DO

*       Make the colour-table visible.
         CALL IIMSMV( DID, MEMID, 1, 1, IDSTAT )

*       Intercept any error.
         IF ( IDSTAT .NE. IDI__OK ) THEN

*          Obtain a meaningful IDI error message.
            CALL KPG1_IDERR( 'IIMWMV_ERR', IDSTAT, STATUS )
            GOTO 960
         END IF

*       Tidy the workspace.

         CALL DAT_ANNUL( CTLOC, STATUS )
      END IF

*    Set up the interactions to control the LUT manipulation.
*    ========================================================
*
*    Set up the mouse or trackerball (interactor type = 0, interactor
*    id = 0) to have no effect (object type = 0, object id = MEMID) by
*    executing some application-specific code (interactive operation =
*    0).  End the interaction by pressing the last trigger (exit trigger
*    number = number of triggers less 1).

      INTTY = 0
      INTID = 0
      OBJTY = 0
      OBJID = MEMID
      INTOP = 0
      EXTRN = 2

      CALL IIIENI( DID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :             IDSTAT )

*    Inquire the interactive operation and instruct the user.

      CALL IIIQID( DID, INTTY, INTID, BUFFER, NCHAR, IDSTAT )
      BUFFER( NCHAR + 1: ) = ' to manipulate the colour table.'
      CALL MSG_OUT( 'LUTMANIPULATE', BUFFER( :NCHAR + 32 ), STATUS )

*    Set up the first trigger (interactor type = 5, interactor id = 0)
*    to modify the LUT (object type = 3, object id = MEMID) by executing
*    some application-specific code (interactive operation = 0).  End
*    the interaction by pressing the last trigger (exit trigger number =
*    number of triggers less 1).

      INTTY = 5
      INTID = 0
      OBJTY = 3
      OBJID = MEMID
      INTOP = 0
      EXTRN = 2

      CALL IIIENI( DID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :             IDSTAT )

*    Inquire the interactive operation and instruct the user.

      CALL IIIQID( DID, INTTY, INTID, BUFFER, NCHAR, IDSTAT )
      BUFFER( NCHAR + 1: ) = ' to thin the highlight brush.'
      CALL MSG_OUT( 'LUTMANIPULATE', BUFFER( :NCHAR + 30 ), STATUS )

*    Set up the second trigger (interactor type = 5, interactor id = 1)
*    to have no effect (object type = 0, object id = MEMID) by
*    executing some application-specific code (interactive operation =
*    0).  End the interaction by pressing the last trigger (exit trigger
*    number = number of triggers less 1).

      INTTY = 5
      INTID = 1
      OBJTY = 0
      OBJID = MEMID
      INTOP = 0
      EXTRN = 2

      CALL IIIENI( DID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :             IDSTAT )

*    Inquire the interactive operation and instruct the user.

      CALL IIIQID( DID, INTTY, INTID, BUFFER, NCHAR, IDSTAT )
      BUFFER( NCHAR + 1: ) = ' to widen the highlight brush.'
      CALL MSG_OUT( 'LUTMANIPULATE', BUFFER( :NCHAR + 31 ), STATUS )

*    Inquire the exit trigger operation and instruct the user.

      CALL IIIQID( DID, INTTY, EXTRN, BUFFER, NCHAR, IDSTAT )
      BUFFER( NCHAR + 1: ) = ' to end the manipulation of the colour '/
     :                       /'table.'
      CALL MSG_OUT( 'END_LUTHILITE ', BUFFER( :NCHAR + 45 ), STATUS )

*    Draw the initial highlight.
*    ===========================

*    Initialise the highlight displacement.  Do not make it zero so
*    that it is more evident.  Make the initial width 2 rather than 1
*    for more clarity.

      PENDIS = 1.0
      IPENDI = NINT( PENDIS )
      BRUSH = 2

*    Copy the highlight brush to the colour table.

      DO  J = IPENDI, IPENDI + BRUSH - 1

*       Write the modified pen to the memory using the requested
*       highlight colour.  Colour-table entries start at 0 if
*       there were no reserved pens.

         CALL IILWLT( DID, MEMID, NRESP + J, 1, RGB, STATUS )

*       Intercept any error.

         IF ( IDSTAT .NE. IDI__OK ) THEN

*          Obtain a meaningful IDI error message.

            CALL KPG1_IDERR( 'IILGLD_ERR', IDSTAT, STATUS )
            GOTO 980
         END IF
      END DO

*    Perform the interactions.
*    =========================

*    Derive the normalisation.  Use 256-NRESP as the canonical value.
*    This is to prevent manipulations that are too sensitive to the
*    locator's movement.  Apply a bias to prevent the normalisation
*    going too far the other way.

      HGLSCL = 0.2 + 0.8 * REAL( NLUTE ) / REAL( 256 - NRESP )

*    Must loop to repeatedly call Enable_Interaction_and_Wait whenever
*    the first trigger is activated or the locator is moved.

      LOOP = .TRUE.
      FIRST = .TRUE.

      DO WHILE ( LOOP )

*       Reset the relevant triggers.

         TRIGS( 1 ) = 0
         TRIGS( 2 ) = 0
         TRIGS( 3 ) = 0

*       Execute the LUT-highlighting interactions.

         CALL IIIEIW( DID, TRIGS, IDSTAT )

*       Intercept any error.

         IF ( IDSTAT .NE. IDI__OK ) THEN

*          Obtain a meaningful IDI error message.

            CALL KPG1_IDERR( 'IIMEIW_ERR', IDSTAT, STATUS )
            GOTO 980
         END IF

*       Exit the loop.
*       ==============

*       The exit trigger has been pressed. End the loop.

         IF ( TRIGS( 3 ) .NE. 0 ) THEN
            LOOP = .FALSE.

*       Thin the highlight brush.
*       =========================
*
*       The trigger to make the highlight brush thinner has been given.

         ELSE IF ( TRIGS( 1 ) .NE. 0 ) THEN

*          Check that the brush can be made thinner.

            IF ( BRUSH .GT. 0 ) THEN

*             Set the the colour index to change.

               HILICI = IPENDI + BRUSH - 1

*             Thin from the higher colour-index side of the highlight
*             brush.  The temporary LUT will be modified once the
*             highlight position is changed.

               DO  I = 1, 3
                  OLUT( I ) = VLUT( I, HILICI + 1 )
               END DO

*             Write the modified pen to the memory.  Colour-table
*             entries start at 0 if there were no reserved pens.

               CALL IILWLT( DID, MEMID, NRESP + HILICI, 1,
     :                      OLUT, STATUS )

*             Intercept any error.

               IF ( IDSTAT .NE. IDI__OK ) THEN

*                Obtain a meaningful IDI error message.

                  CALL KPG1_IDERR( 'IILWLT_ERR', IDSTAT, STATUS )
                  GOTO 980
               END IF

*             Reduce the brush width.

               BRUSH = BRUSH - 1
            END IF

*       Broaden the highlight brush.
*       ============================
*
*       The trigger to make the highlight brush wider has been given.

         ELSE IF ( TRIGS( 2 ) .NE. 0 ) THEN

*          Check that the brush can be made wider.

            IF ( BRUSH .LT. MXBRSH ) THEN

*             Set the the colour index to change.

               HILICI = IPENDI + BRUSH

*             Normal procedure is to extend the brush to a higher colour
*             index.  However, there is a special case where this
*             cannot be done, namely where the highlight includes the
*             highest colour index.  Change the colour index to be
*             highlighted for the special case.  Note also that the
*             offset must be reduced by one.

               IF ( HILICI .GE. NLUTE ) THEN
                  HILICI = IPENDI
                  PENDIS = PENDIS - 1.0
                  IPENDI = IPENDI - 1
               END IF

*             Write the modified pen to the memory using the requested
*             highlight colour.  Colour-table entries start at 0 if
*             there were no reserved pens.

               CALL IILWLT( DID, MEMID, NRESP + HILICI, 1, RGB,
     :                      STATUS )

*             Intercept any error.

               IF ( IDSTAT .NE. IDI__OK ) THEN

*                Obtain a meaningful IDI error message.

                  CALL KPG1_IDERR( 'IILWLT_ERR', IDSTAT, STATUS )
                  GOTO 980
               END IF

*             Increase the brush width.

               BRUSH = BRUSH + 1
            END IF

*       Move the Highlight brush.
*       =========================

*       The locator has been moved.  Therefore move the highlight pens
*       in the colour table.

         ELSE

*          Find the position of the locator.

            CALL IIIGLD( DID, 0, XDISP, YDISP, IDSTAT )

*          First values are arbitrary so ignore them.

            IF ( FIRST ) XDISP = 0
            FIRST = .FALSE.

*          Intercept any error.

            IF ( IDSTAT .NE. IDI__OK ) THEN

*             Obtain a meaningful IDI error message.

               CALL KPG1_IDERR( 'IILGLD_ERR', IDSTAT, STATUS )
               GOTO 980
            END IF

*          Only re-write the colour table when the locator has moved in
*          the x direction.

            IF ( XDISP .NE. 0 ) THEN

*             Have x displacement adjustment via a lookup table.
*             ==================================================

               XCORR = MAX( 1, MIN( MXSPED, ABS( XDISP ) ) )

*             Find the total pen displacement.

               PENDIS = PENDIS + REAL( XDISP ) * XSPFAC( XCORR ) *
     :                  HGLSCL

*             Constrain the pen displacement so that the highlight brush
*             remains within the bounds of the colour table.

               PENDIS = MAX( MIN( REAL( NLUTE - BRUSH ), PENDIS ), 0.0 )
               IPENDI = NINT( PENDIS )

*             Write the modified VLUT.
*             =======================

*             First copy the input LUT to the colour table.

               DO  J = 1, NLUTE
                  DO  I = 1, 3
                     TLUT( I, J ) = VLUT( I, J )
                  END DO
               END DO

*             Copy the highlight brush to the colour table.

               DO  J = IPENDI + 1, IPENDI + BRUSH
                  DO  I = 1, 3
                     TLUT( I, J ) = RGB( I )
                  END DO
               END DO

*             Only write to the unreserved portion of the VLUT.

               CALL IILWLT( DID, MEMID, NRESP, NLUTE, TLUT, STATUS )

*             Intercept any error.

               IF ( IDSTAT .NE. IDI__OK ) THEN

*                Obtain a meaningful IDI error message.

                  CALL KPG1_IDERR( 'IILGLD_ERR', IDSTAT, STATUS )
                  GOTO 980
               END IF
            END IF
         END IF
      END DO

*    Restore the display to its former state, i.e. removing the colour-
*    table display.
*    ===================================================================

      IF ( VIEW ) THEN

*       Set up the transfer window.
*       ===========================

*       Redefine the origin of the transfer window.

         OY = OY - WIDTH + 1
         CALL IIMSTW( DID, MEMID, 0, NLUTE, WIDTH, DEPTH, OX, OY,
     :                IDSTAT )

*       Abort if an error has occurred.  Note IDI does not use
*       inherited status.

         IF ( IDSTAT .NE. IDI__OK ) THEN

*          Obtain a meaningful IDI error message.

            CALL KPG1_IDERR( 'IIMSTW_ERR', IDSTAT, STATUS )
            GOTO 980
         END IF

*       Write the captured portion of the memory to the screen starting
*       at position OX, OY in the transfer window.  The data are taken
*       from the lowest byte of each integer word in the data array
*       (defined by PACK = 1, DEPTH = 8 ).

         PACK = 1
         CALL IIMWMY( DID, MEMID, %VAL( EMPNTR ), NDATA, DEPTH, PACK,
     :                0, 0, IDSTAT )

*       Intercept any error.

         IF ( IDSTAT .NE. IDI__OK ) THEN

*          Obtain a meaningful IDI error message.

            CALL KPG1_IDERR( 'IIMWMY_ERR', IDSTAT, STATUS )
            GOTO 980
         END IF

*       Make the colour-table visible.
         CALL IIMSMV( DID, MEMID, 1, 1, IDSTAT )

*       Intercept any error.
         IF ( IDSTAT .NE. IDI__OK ) THEN

*          Obtain a meaningful IDI error message.
            CALL KPG1_IDERR( 'IIMWMV_ERR', IDSTAT, STATUS )
            GOTO 960
         END IF

*       Tidy the workspace.

         CALL DAT_ANNUL( MEMLOC, STATUS )
      END IF

*    Closedown sequence.
*    ===================

 960  CONTINUE
      IF ( VIEW .AND. STATUS .NE. SAI__OK )
     :   CALL DAT_ANNUL( CTLOC, STATUS )

 980  CONTINUE
      IF ( VIEW .AND. STATUS .NE. SAI__OK )
     :   CALL DAT_ANNUL( MEMLOC, STATUS )

*    Errors, either obtaining the device or because the device does not
*    support the LUT-rotation operation, fall to this point.

 999  CONTINUE

      IF ( DEVCAN ) THEN

*       Close down IDI using the parameter system.

         CALL IDI_CANCL( 'DEVICE', STATUS )
      ELSE
         CALL IDI_ANNUL( DID, STATUS )
      END IF

      END
