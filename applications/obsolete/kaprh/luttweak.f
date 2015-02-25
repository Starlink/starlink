      SUBROUTINE LUTTWEAK( STATUS )
*+
*  Name:
*     LUTTWEAK

*  Purpose:
*     Tweaks a colour table of an image-display device.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL LUTTWEAK( STATUS )

*  Description:
*     This routine adjusts the colour table of a nominated plane of
*     an IDI-supported image display, such as X-windows.  The adjustment
*     is under mouse, joystick or trackerball button control.
*
*     For an Ikon or X-windows, moving the mouse left or right shifts
*     the colour table towards lower and higher colour indices
*     respectively.  Moving the mouse up stretches the lookup table,
*     and moving it down squashes the lookup table until it disappears,
*     then the lookup table is flipped.  If the lookup table is
*     reversed, moving down stretches, and moving up squashes.
*     Pressing the left button of the mouse resets the colour table to
*     its input state.  Pressing the centre button alters the way in
*     which a squashed lookup table is padded.  The two states are
*     white or to use the first and last colours of the input lookup
*     table, the sense depending on whether the lookup table is
*     flipped.  Hitting the right-hand button ends the modification of
*     the colour table.

*     The colour table may be viewed during its manipulation without
*     permanently altering the display memory.  The colour-table
*     display is situated via the cursor, and will disappear once the
*     tweaking is complete.

*  Usage:
*     luttweak [device] [plane] [view]

*  ADAM Parameters:
*     DEVICE = DEVICE (Read)
*        The name of the image-display device whose lookup table is to
*        be adjusted.  The name of the base plane should be given even
*        if the overlay lookup table is to be adjusted.
*        [Current image-display device]
*     FULL = _LOGICAL (Read)
*        If FULL is TRUE, the whole colour-table for the device is
*        may be modified, including the reserved pens.  When FULL
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
*     luttweak
*        Tweaks the colour table on the current image-display device.
*     luttweak xwindows
*        Tweaks the colour table on the xwindows device.
*     luttweak xwindows full
*        Tweaks the colour table and palette on the xwindows device.
*     luttweak ikon 1
*        Tweaks the colour table on the Ikon overlay plane.
*     luttweak view
*        Tweaks the colour table on the current image-display device.
*        The colour table is displayed during the tweaking.

*  Notes:
*     -  Only Ikons and X-windows are supported.
*     -  The speed of the colour-table rotation is not linearly
*     proportional to the mouse displacement; the speed of displacement
*     tunes the effect so that slow motion makes a small change than
*     a faster motion.  The squashing and stretching factors are also
*     non-linear.

*  Algorithm:
*     -  Determine whether or not the reserved pens are to be ignored.
*     -  Ensure the the clear flag is set to "no clear".
*     -  Associate the image display and get an identifier for it.
*     -  Check the attributes of the device, i.e. that it has at least
*     one cursor, at least two triggers and a locator.
*     -  Obtain the number of memory planes and the maximum number of
*        LUT entries.
*     -  Obtain the plane number whose LUT is to be tweaked.
*     -  Read the input LUT.
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
*     -  Loop doing the LUT-modification interaction.  When the last
*     trigger is fired exit from the loop.  If the first trigger is
*     fired write the input LUT to the memory and restart the
*     interaction.  If the second trigger is fired switch the
*     padding-colour flag.  Find effective displacements after applying
*     correction factors for the cursor speed and the number of
*     colour-table entries.  Call a routine to evaluate the new LUT.
*     Write the new LUT in the device's colour table.
*     -  If there is a colour table display, set up the original
*     transfer window, and write the captured data back to the memory.
*     Tidy the workspace.
*     -  Annul the device.

*  Related Applications:
*     KAPPA: CRELUT, LUTFLIP, LUTHILITE, LUTROT.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 April 22 (MJC):
*        Original version.
*     1991 June 5 (MJC):
*        Altered for new, X-windows, IDI.  Capability 14 is 2**n rather
*        than n.
*     1991 November 15 (MJC):
*        Made exit trigger number 2.  Inceased maximum number of
*        triggers to 64.
*     1992 February 10 (MJC):
*        Normalised the rotation displacement by the effective number
*        of colour indices.
*     1992 February 19 (MJC):
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
     :  CTPTR,                   ! Pointer to workspace for colour-table
                                 ! display
     :  DEPTH,                   ! Depth of the data (bits/pixel)
     :  DID,                     ! Display identifier
     :  DSIZE( 2 ),              ! Display dimensions
     :  EMPNTR,                  ! Pointer to workspace for storing
                                 ! exisiting memory data
     :  EXTRN,                   ! Exit trigger for ending LUT tweaking
     :  IDSTAT,                  ! IDI status
     :  INTID,                   ! Interactor identifier
     :  INTOP,                   ! Interactive operation number
     :  INTTY,                   ! Interactor type for zooming control
     :  ITTDEP( NMEMAX )         ! ITT depths of available memories

      INTEGER
     :  I,                       ! Loop counters
     :  LUTCAP,                  ! Max. depth of LUTS capability code
     :  MEMDEP( NMEMAX ),        ! Depths of available memories
                                 ! tweaked
     :  MEMID,                   ! Memory identifier whose LUT is to be
     :  MEMIDS( NMEMAX ),        ! Available memories
     :  MEMTYP,                  ! Memory type
     :  MODCON,                  ! Configuration mode
     :  NCHAR,                   ! Number of characters in IDI
                                 ! interaction instruction message
     :  NCHAR1,                  ! Number of characters in interaction
                                 ! message
     :  NCONF,                   ! Configuration number
     :  NDATA,                   ! Number of pixels overwritten by the
                                 ! colour-table display
     :  NINTS( 1 ),              ! Number of LUT entries
     :  NLOC( 1 )                ! Number of locators

      INTEGER
     :  NLUTE,                   ! Number of entries in the VLUT less
                                 ! the reserved pens.
     :  NMEM,                    ! Number of image memories
     :  NRESP,                   ! Number of reserved pens
     :  NTRIG( 1 ),              ! Number of available triggers
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
     :  TRIGS( IMAXTR )          ! Trigger status

      INTEGER
     :  WIDTH,                   ! Width of the colour table display in
                                 ! pixels
     :  XC,                      ! X display centre
     :  XCORR,                   ! X speed-factor index
     :  XDISP,                   ! Locator x displacement
     :  XOFF,                    ! X memory offset
     :  XP,                      ! X cursor position
     :  XSIZ( NMEMAX ),          ! X sizes of available memories
     :  YC,                      ! Y display centre
     :  YCORR,                   ! Y speed-factor index
     :  YDISP,                   ! Locator y displacement
     :  YOFF,                    ! Y memory offset
     :  YP,                      ! Y cursor position
     :  YSIZ( NMEMAX ),          ! Y sizes of available memories
     :  ZOOM                     ! Zoom factor

      REAL
     :  ITT( CTM__MXPEN ),       ! Intensity transformation table
     :  ROTSCL,                  ! Scale factor to normalise the
                                 ! rotation displacements
     :  PENDIS,                  ! Pen displacement in x
     :  SOSDIS,                  ! Squash/stretch displacement in y
     :  TLUT( 3, CTM__MXPEN ),   ! Tweaked video lookup table
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
     :  VIEW,                    ! A temporary colour-table display
                                 ! will be drawn
     :  WHITE                    ! The padding of the lookup table is
                                 ! white

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

*    Just want to tweak what is already there.  It may have
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
         CALL ERR_REP( 'LUTTWEAK_NOCURSOR',
     :     'LUTTWEAK: The chosen device has insufficient triggers.',
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
         CALL ERR_REP( 'LUTTWEAK_NOCURSOR',
     :     'LUTTWEAK: The chosen device has no locator.', STATUS )

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
               CALL ERR_REP( 'LUTTWEAK_NOROOM',
     :           'LUTTWEAK: No room to accommodate the colour-table '/
     :           /'display.  Reduce the zoom factor.', STATUS )
            ELSE
               CALL ERR_REP( 'LUTTWEAK_NOROOM',
     :           'LUTTWEAK: No room to accommodate the colour-table '/
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
*       trigger number 2).
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
*       uppercase so convert the string to lowercase.
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
            CALL ERR_REP( 'LUTTWEAK_WSP',
     :        'LUTTWEAK: Error obtaining workspace for storing the '/
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
            CALL ERR_REP( 'LUTTWEAK_WSP2',
     :        'LUTTWEAK: Unable to obtain workspace to display the '/
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

*    Inquire the interactive operation and instruct the user
      CALL IIIQID( DID, INTTY, INTID, BUFFER, NCHAR, IDSTAT )
      BUFFER( NCHAR + 1: ) = ' to manipulate the colour table.'
      CALL MSG_OUT( 'LUTMANIPULATE', BUFFER, STATUS )

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
      BUFFER( NCHAR + 1: ) = ' to reset the colour table.'
      CALL MSG_OUT( 'RESETLUT', BUFFER, STATUS )

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
      BUFFER( NCHAR + 1: ) = ' to toggle the padding colours.'
      CALL MSG_OUT( 'PADDING', BUFFER, STATUS )

*    Inquire the exit trigger operation and instruct the user.
      CALL IIIQID( DID, INTTY, EXTRN, BUFFER, NCHAR, IDSTAT )
      BUFFER( NCHAR + 1: ) = ' to end the manipulation of the colour '/
     :                       /'table.'
      CALL MSG_OUT( 'END_LUTTWEAK ', BUFFER, STATUS )

*    Perform the interactions.
*    =========================

*    There is no current pen or stretch displacement.
      PENDIS = 0.0
      SOSDIS = 0.0


*    Derive the normalisation.  Use 256-NRESP as the canonical value.
*    This is to prevent manipulations that are too sensitive to the
*    locator's movement.  Apply a bias to prevent the normalisation
*    going too far the other way.
      ROTSCL = 0.2 + 0.8 * REAL( NLUTE ) / REAL( 256 - NRESP )

*    Initialise variables for the main loop.
      LOOP = .TRUE.
      FIRST = .TRUE.

*    Must loop to repeatedly call Enable_Interaction_and_Wait whenever
*    the first trigger is activated or the locator is moved.
      DO WHILE ( LOOP )

*       Reset the relevant triggers.
         TRIGS( 1 ) = 0
         TRIGS( 2 ) = 0
         TRIGS( 3 ) = 0

*       Execute the LUT-tweaking interactions.
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

*       Reset the LUT.
*       ==============

*       The reset trigger has been given, so write the input LUT to
*       the memory.  Colour-table entries start at 0 if there were
*       no reserved pens, therefore there is no plus-one term.
         ELSE IF ( TRIGS( 1 ) .NE. 0 ) THEN
            CALL MSG_OUT( 'LUT_RESET', 'Resetting the LUT.', STATUS )
            CALL IILWLT( DID, MEMID, NRESP, NLUTE, VLUT, STATUS )

*          Intercept any error.
            IF ( IDSTAT .NE. IDI__OK ) THEN

*             Obtain a meaningful IDI error message.
               CALL KPG1_IDERR( 'IILWLT_ERR', IDSTAT, STATUS )
               GOTO 980
            END IF

*          Following a reset there is no current pen or stretch
*          displacement.
            PENDIS = 0.0
            SOSDIS = 0.0

*       Tweak the LUT.
*       ==============

*       The locator has been moved.  Therefore tweak the LUT.
         ELSE

*          Find the position of the locator.
            CALL IIIGLD( DID, 0, XDISP, YDISP, IDSTAT )

*          First values are arbitrary so ignore them.
            IF ( FIRST ) THEN
               XDISP = 0
               YDISP = 0
            END IF

*          Intercept any error.
            IF ( IDSTAT .NE. IDI__OK ) THEN

*             Obtain a meaningful IDI error message.
               CALL KPG1_IDERR( 'IILGLD_ERR', IDSTAT, STATUS )
               GOTO 980
            END IF

*          Have x displacement adjustment via a lookup table.
*          ==================================================

            XCORR = MAX( 1, MIN( MXSPED, ABS( XDISP ) ) )
            YCORR = MAX( 1, MIN( MXSPED, ABS( YDISP ) ) )

*          Find the total pen and stretch/squash displacements.
            PENDIS = PENDIS + REAL( XDISP ) * XSPFAC( XCORR ) *
     :               ROTSCL
            SOSDIS = SOSDIS + REAL( YDISP ) * XSPFAC( YCORR )

*          Switch the padding colour.
*          ==========================

*          The trigger to switch the terminal colour has been given,
*          so toggle the flag for the tweaking routine.
            IF ( TRIGS( 2 ) .NE. 0 ) WHITE = .NOT. WHITE

*          Tweak the LUT.
*          ==============

*          Only tweak when the locator has moved or the padding toggle
*          has been switched.
            IF ( XDISP .NE. 0 .OR. YDISP .NE. 0 .OR.
     :           TRIGS( 2 ) .NE. 0 ) THEN

*             Set the padding colour, apply the shift, and stretch/
*             squash to the lookup table.
               CALL KPS1_LUTWK( NLUTE, VLUT, PENDIS,
     :                          SOSDIS, WHITE, TLUT, STATUS )

*             Write the tweaked VLUT.
*             =======================

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

*       Completed the loop so can next cycle can no longer be the first
*       pass.
         FIRST = .FALSE.

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
