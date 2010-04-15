      SUBROUTINE SNAPSHOT( STATUS )
*+
*  Name:
*     SNAPSHOT

*  Purpose:
*     Dumps an image-display memory to a graphics hardcopy and
*     optionally to an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task.

*  Invocation:
*     CALL SNAPSHOT( STATUS )

*  Description:
*     This routine captures the data in the memory of an image-display
*     device, and writes these data to a different GKS device.  For
*     example, the contents of an X-windows memory might be captured and
*     sent to a PostScript laser printer.
*
*     Various options are available:
*        o you may choose to capture a whole or part of what is visible
*          on the screen, or the entire contents of the memory.  For the
*          former you adjust a rubber-band region until the desired
*          area is enclosed. Instructions for controlling the
*          rubber-band are given at run time.
*        o A title may be included in the output.
*        o The array may be output to an NDF.

*  Usage:
*     snapshot odevice [out] [whole] [scale] [negativ] [title] [planes]

*  ADAM Parameters:
*     DEVICE = DEVICE (Read)
*        Input image-display device.  [Current image-display device]
*     NEGATIVE = _LOGICAL (Read)
*        If TRUE, the output hardcopy is a negative version of what is
*        stored in the image display.  On some output devices a
*        constant dark background can give a non-uniform result, and
*        so a negative representation is the default. [TRUE]
*     ODEVICE = DEVICE (Read)
*        Name of the output device.  The suggested default is the
*        graphics device last used in SNAPSHOT, and if there is not one,
*        the suggested default is the global current graphics device.
*        The device must be in the GNS category MATRIX_PRINTER, and
*        have at least 24 greyscale intensities.
*     OUT = NDF (Write)
*        Name given to the output NDF data structure used to store the
*        contents of the image-display memory.  If it is null (!) no
*        NDF will be created. [!]
*     PLANES() = _INTEGER (Read)
*        The numbers of the image memory planes not be output.  All
*        unspecified planes become visible.  If PLANES is null (!), all
*        memory planes will be used to form the snapshot.  The base
*        memory is 0 and overlays are numbered consecutively from 1.
*        The value must be between 0 and the number of image memories
*        minus 1.  For an Ikon the only overlay plane is 1.  [!]
*     SCALE = _REAL (Read)
*        Scale factor for output.  Unity gives the largest possible
*        output, but it takes longest to compute and print (goes as the
*        square of the scale factor).  On the other hand unity does
*        provide maximum resolution. SCALE must be between 0 and 1.
*        [0.707]
*     TITLE = LITERAL (Read)
*        Title of the plot and the output NDF.  There is only space on
*        the plot for about 25 characters in the title.  If it is null
*        (!) no title will be plotted, and the title in the output NDF
*        becomes "KAPPA - Snapshot". [!]
*     WHOLE = _LOGICAL (Read)
*        If TRUE, the whole image-display memory is recorded, otherwise
*        a selected region of what is visible on the screen is plotted.
*        Dumping the whole memory can require considerable disc space
*        for work arrays and the output NDF. [FALSE]

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Examples:
*     snapshot whole odevice=canon_l
*        This will dump the whole of the current image display's
*        memories to the canon_l device.  The plot will occupy half
*        of the maximum area available on the device, i.e. half of
*        root 2 magnification.
*     snapshot scale=1.0 \
*        This will capture a whole or part of what is visible on the
*        screen of the current image display, and dump it to the current
*        snapshot device at the largest magnification.
*     snapshot postscript views device=xw whole
*        This dumps the whole of the xw device's memories to the
*        postscript device, and also to a NDF called views.  The
*        magnification is a half of root 2.
*     snapshot ps_l device=xw whole planes=0 title="Hardcopy Base"
*        This dumps the whole of the xw device's base memory to the
*        ps_l device.  The plot is entitled "Hardcopy Base".  The
*        entire output plot occupies half of the maximum area available
*        on the device.

*  Notes:
*     -  The whole of the screen and the whole of the memory may be
*     different, for example, the image may have been zoomed or panned.
*     -  Files are not spooled to laserprinters.  They must be printed
*     outside this application.

*  Related Applications:
*     KAPPA: DISPLAY.

*  Implementation Status:
*     No origin information is passed to the output NDF.

*  Authors:
*     JM: Jo Murray (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     August 1989 (JM):
*        Original version.
*     March  1990 (JM):
*        Option added to write snapshot array to an NDF.
*     1990 May 2 (MJC):
*        Fixed bugs in obtaining depth capability, obtaining a null
*        title and ordering of closedown operations, corrected and
*        expanded the ADAM Parameters section, added a default title
*        for the output NDF and tidied.
*     1991 March 18 (MJC):
*        Region of interest added to obtain subsets.
*     1991 July 31 (MJC):
*        No longer redefines colours of SGS pens to predefined state if
*        workstation has dynamic colour representation, now there is
*        palette control.
*     1992 March 26 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1992 October 12 (MJC):
*        Added calls to make the memories visible on the Ikon.  To
*        enable planes not to be output, a new parameter, PLANES, has
*        been added.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type definitions:
      IMPLICIT NONE          ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'      ! Standard SAE constants
      INCLUDE 'DAT_PAR'      ! Data-system constants
      INCLUDE 'CTM_PAR'      ! Colour-table management constants
      INCLUDE 'PAR_ERR'      ! Parameter-system errors
      INCLUDE 'NDF_PAR'      ! NDF_ public constants
      INCLUDE 'IDI_PAR'      ! IDI constants
      INCLUDE 'IDI_ERR'      ! IDI error codes

*  External References:
      INTEGER CHR_LEN        ! String size ignoring trailing blanks

*  Status:
      INTEGER STATUS         ! Global status

*  Local Constants:
      INTEGER NMEMAX             ! Maximum number of memories
      PARAMETER ( NMEMAX = 10 )

      INTEGER NDIM           ! Number of dimensions
      PARAMETER ( NDIM = 2 )

      INTEGER MINCOL         ! Min. no. of colours required to do
                             ! greyplot
      PARAMETER ( MINCOL = 4 )

      INTEGER NPRICL         ! No. of primary colours
      PARAMETER ( NPRICL = 3 )

      CHARACTER * ( NDF__SZTYP ) TYPE ! Output image data type
      PARAMETER (TYPE = '_INTEGER')

*  Local Variables:
      INTEGER    ACTVAL      ! Number of values returned
      REAL       ASP         ! Aspect ratio of image display
      LOGICAL    BAD         ! Bad logical variable for VEC_ITOI
      CHARACTER*72 BUFFER    ! Error messages from IDI
      INTEGER    CAPID       ! IDI inquiry code
      INTEGER    DEPTH( 1 )  ! Data depth (bits/pixel)
      LOGICAL    DEVCAN      ! True if DEVICE has been opened
      LOGICAL    DEVCN1      ! True if ODEVICE has been opened
      INTEGER    DISPID      ! Display identifier
      INTEGER    DSIZE( NDIM ) ! Array to accommodate screen dimensions
      INTEGER    EL          ! Number of data elements mapped
      INTEGER    EXTRN       ! Exit trigger for ending pan/zoom
      REAL       GKSCOL( NPRICL ) ! Colour of a pen for transfer to
                             !lookup table
      INTEGER    I           ! Loop variable
      INTEGER    IDIMS( NDIM ) ! Image dimensions
      INTEGER    IDSTAT      ! IDI status
      INTEGER    IERR        ! Error no. in VEC_ITOI  (not used)
      INTEGER    INTID       ! Interactor identifier
      INTEGER    INTOP       ! Interactive operation number
      INTEGER    INTTY       ! Interactor type for zooming control
      INTEGER    LBND( NDIM ) ! Lower bounds of the ROI
      LOGICAL    INVERT      ! True if output image is to be inverted
      INTEGER    IPIXX       ! Max. no. of columns of pixels on image
                             ! display
      INTEGER    IPIXY       ! Max. no. of rows of pixels on image
                             ! display
      INTEGER    ITTDEP( NMEMAX ) ! ITT depths of available memories
      INTEGER    J           ! Loop variable
      INTEGER    LP          ! Lowest pen to use in the dumped cell
                             ! array
      INTEGER    MAXV        ! Maximum value for intensity
      INTEGER    MEMDEP( NMEMAX ) ! Depths of available memories
      INTEGER    MEMID( NMEMAX ) ! Identifiers of memories to be made
                             ! invisible
      INTEGER    MEMIDR      ! Memory identifier for ROI
      INTEGER    MEMIDS( NMEMAX ) ! Available memories
      INTEGER    MEMTYP      ! Memory type
      INTEGER    MINV        ! Minimum value for intensity
      INTEGER    MODCON      ! Configuration mode
      INTEGER    NARR        ! Number of dimensions to screen (2)
      INTEGER    NCHAR       ! Number of characters in text screen
      INTEGER    NCONF       ! Configuration number
      INTEGER    NDATA       ! Total number of data points
      INTEGER    NDF         ! Output NDF identifier
      INTEGER    NERR        ! No. of errors in VEC_ITOI (not used)
      INTEGER    NINTS       ! Number of intensity levels in output
                             ! device
      INTEGER    NMEM        ! Number of image memories
      INTEGER    NTITLE      ! Number characters in title string
      INTEGER    NTRIG( 1 )  ! Number of available triggers
      INTEGER    NVAL        ! Number of values
      INTEGER    NX          ! No of pixels in X-direction of image
                             ! display
      INTEGER    NX1         ! Number of visible pixels columns
      INTEGER    NY          ! No of pixels in Y-direction of image
                             ! display
      INTEGER    NY1         ! Number of visible pixels rows
      INTEGER    OBJID       ! Object identifier
      INTEGER    OBJTY       ! Object type
      INTEGER    OUTMID      ! Output memory identifier from reading
                             ! the region of interest
      INTEGER    OX          ! X co-ord for origin of transfer window
      INTEGER    OY          ! Y co-ord for origin of transfer window
      INTEGER    PACK        ! Packing factor
      INTEGER    PICID       ! AGI picture identifier
      LOGICAL    PLTITL      ! True if the plot has a title
      INTEGER    PNTR( 1 )   ! Pointer to mapped data array
      LOGICAL    NEGTIV      ! True if output image is to be negative
      INTEGER    ROIID       ! Region-of-interest identifier
      INTEGER    ROISIZ      ! Initial ROI size in pixels
      CHARACTER*(DAT__SZLOC) SCRLOC  ! Locator to scratch space
      CHARACTER*(DAT__SZLOC) SNPLOC  ! Locator to snapshot array
      REAL       SCALE       ! Scaling factor for output hardcopy
      INTEGER    SCRPNT      ! Pointer to scratch space
      INTEGER    SNPPNT      ! Locator to snapshot array
      INTEGER    SNPIMP( 1 ) ! 1=True if `create snapshot' is
                             ! implemented
      CHARACTER  TITLE*25    ! Title for output
      INTEGER    TRICAP      ! No. of triggers capability code
      INTEGER    TRIGS( IMAXTR ) ! Trigger status
      REAL       TXTHGT      ! Base text height
      INTEGER    UBND( NDIM ) ! Upper bounds of the ROI
      LOGICAL    WHOLE       ! True if whole image display memory is
                             ! captured
      INTEGER    WKID        ! Workstation identifer
      REAL       X1          ! Lower X co-ord of base zone
      REAL       X2          ! Upper X co-ord of base zone
      REAL       XL          ! Lower limit in X of SGS box
      REAL       XM          ! X-length in metres of base zone
      INTEGER    XOFF        ! Offset to data position in x
      INTEGER    XSIZ( NMEMAX ) ! X sizes of available memories
      REAL       XU          ! Upper limit in X of SGS box
      REAL       Y1          ! Lower Y co-ord of base zone
      REAL       Y2          ! Upper Y co-ord of base zone
      REAL       YL          ! Lower limit in Y of SGS box
      REAL       YM          ! Y-length in metres of base zone
      INTEGER    YOFF        ! Offset to data position in y
      INTEGER    YSIZ( NMEMAX ) ! Y sizes of available memories
      REAL       YU          ! Upper limit in Y of SGS box
      INTEGER    ZOOM        ! Zoom factor
      INTEGER    ZONE1       ! Zone number
      INTEGER    ZONE2       ! Zone number
      INTEGER    ZONEI       ! Zone number

*.

*   Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN
      DEVCAN = .FALSE.

*   Open image display, and check that it has suitable characteristics.
*   ===================================================================

*   Set IDI clear flag.

      CALL IDI_CLRFG( 1 )

*   Open IDI for a device obtained through the parameter system.

      CALL IDI_ASSOC( 'DEVICE', 'READ', DISPID, STATUS )

      IF ( STATUS .EQ. PAR__ABORT ) GOTO 999

*   Abort if an error has occured.

      IF ( STATUS .NE. SAI__OK ) THEN

*   Need to cancel the device.

         DEVCAN = .TRUE.
         GOTO 980
      END IF

*   Inquire whether the optional `create snapshot' is implemented
*   on this particular device.
*   Integer code CAPID = 100 is used with the `query capabilities'
*   routine to check this.

      CAPID = 100
      NARR = 1
      CALL IIDQCI( DISPID, CAPID, NARR, SNPIMP, NVAL, IDSTAT )

*   Abort if an error has occured.

      IF ( IDSTAT .NE. IDI__OK ) THEN

*   Obtain a meaningful IDI error message.

         CALL KPG1_IDERR( 'IIDQCI_ERR', IDSTAT, STATUS )
         DEVCAN = .TRUE.
         GOTO 980
      END IF

*   Abort if snapshot is not available.

      IF ( SNPIMP( 1 ) .EQ. 0 )THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SNAPSHOT_NOSNP',
     :     'Create Snapshot not implemented on this device.', STATUS )
         DEVCAN = .TRUE.
         GOTO 980
      END IF

*   Inquire the physical size of the display again using query
*   capabilities. Integer code CAPID = 12 is used to query the screen
*   size in pixels.

      CAPID = 12
      NARR = 2
      CALL IIDQCI( DISPID, CAPID, NARR, DSIZE, NVAL, IDSTAT )

*   Abort if an error has occurred.

      IF ( IDSTAT .NE. IDI__OK ) THEN

*   Obtain a meaningful IDI error message.

         CALL KPG1_IDERR( 'IIDQCI_ERR', IDSTAT, STATUS )
         DEVCAN = .TRUE.
         GOTO 980
      END IF

      NX = DSIZE( 1 )
      NY = DSIZE( 2 )

*   Inquire the depth of the display.
*   Integer code CAPID = 13 is used to query this.

      CAPID = 13
      NARR = 1
      CALL IIDQCI( DISPID, CAPID, NARR, DEPTH, NVAL, IDSTAT )

*   Abort if an error has occurred.

      IF ( IDSTAT .NE. IDI__OK ) THEN

*   Obtain a meaningful IDI error message.

         CALL KPG1_IDERR( 'IIDQCI_ERR', IDSTAT, STATUS )
         DEVCAN = .TRUE.
         GOTO 980
      END IF

      MINV = CTM__RSVPN
      MAXV = 2**DEPTH( 1 ) - 1

*    Obtain the number of memories.
*    ==============================

*    Find the number of image (MEMTYP=1) planes available for the
*    image-display device.  Only NMEM and MEMIDS are required.

      NCONF = 0
      MEMTYP = 1
      CALL IIDQDC( DISPID, NCONF, MEMTYP, NMEMAX, MODCON, MEMIDS, XSIZ,
     :             YSIZ, MEMDEP, ITTDEP, NMEM, IDSTAT )

*    Abort if an error has occurred.  Note IDI does not use
*    inherited status.

      IF ( IDSTAT .NE. IDI__OK ) THEN

*      Obtain a meaningful IDI error message.

         CALL KPG1_IDERR( 'IIDQDC_ERR', IDSTAT, STATUS )
         GOTO 999
      END IF

*    Get the memories required within the allowed range.
*    ===================================================

*    Default is the base plane.  There is no guarantee that the chosen
*    planes are not obscured by another overlay plane.

      NVAL = 1
      CALL PAR_GDRVI( 'PLANES', NMEM, 0, NMEM-1, MEMID, NVAL, STATUS )

*    A null status indicates that all planes are to be used.

      IF ( STATUS .EQ. PAR__NULL ) THEN
         DO I = 1, NMEM
           MEMID( I ) = MEMIDS( I )
         END DO
         NVAL = NMEM
         CALL ERR_ANNUL( STATUS )
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
         GOTO 999
      END IF

*    Set all the memories to be invisible.
*    =====================================

*    Note there is no checking that the ITT and VLUT are suitable.

      CALL IIMSMV( DISPID, MEMIDS, NMEM, 0, IDSTAT )

*    Intercept any error.

      IF ( IDSTAT .NE. IDI__OK ) THEN

*       Obtain a meaningful IDI error message.

         CALL KPG1_IDERR( 'IIMSMV_ERR', IDSTAT, STATUS )
         GOTO 999
      END IF

*    Make the nominated planes visible.
*    ==================================

      CALL IIMSMV( DISPID, MEMID, NVAL, 1, IDSTAT )

*    Intercept any error.

      IF ( IDSTAT .NE. IDI__OK ) THEN

*       Obtain a meaningful IDI error message.

         CALL KPG1_IDERR( 'IIMSMV_ERR', IDSTAT, STATUS )
         GOTO 999
      END IF

*   Define the memory on which to have the ROI.  Choose the highest
*   memory.
      MEMIDR = MEMID( NVAL )

*   Type of snapshot.
*   =================

*   See if the whole memory or only what is visible on the screen
*   is to be captured.

      CALL PAR_GTD0L( 'WHOLE', .FALSE., .TRUE., WHOLE, STATUS )
      IF ( STATUS .EQ. PAR__ABORT ) GOTO 980

*   Obtain the bounds of the region to be dumped.
*   =============================================

*   First set up coordinates to use for the transfer window.

      IF ( WHOLE ) THEN
         NX1 = NX
         NY1 = NY
         OX = 0
         OY = 0

      ELSE

*   Inquire screen zoom and pan if only dumping the visible display.

         CALL IIZRSZ( DISPID, MEMIDR, XOFF, YOFF, ZOOM, STATUS )
         OX = -XOFF
         OY = -YOFF
         NX1 = NX / ( ZOOM + 1 )
         NY1 = NY / ( ZOOM + 1 )

*    Now set up a region of interest.
*    ================================

*    Get the number of triggers.

         TRICAP = 55
         NVAL = 1
         CALL IIDQCI( DISPID, TRICAP, NVAL, NTRIG, ACTVAL, IDSTAT )

*    Abort if an error has occurred.  Note IDI does not use
*    inherited status.

         IF ( IDSTAT .NE. IDI__OK ) THEN

*    Obtain a meaningful IDI error message.

            CALL KPG1_IDERR( 'IIDQCI_ERR', IDSTAT, STATUS )
            DEVCAN = .TRUE.
            GOTO 980
         END IF

*    Check that the image-display device has at least two triggers.
*    These are numbered 0, 1, 2 etc.

         IF ( NTRIG( 1 ) .LT. 2 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SNAPSHOT_NOCURSOR',
     :        'SNAPSHOT: The chosen device has insufficient triggers.',
     :        STATUS )

*    The image display should not be recorded.

            DEVCAN = .TRUE.
            GOTO 980
         END IF

*    Define the initial ROI extent, which is symmetric about the centre
*    of the screen.

         ROISIZ = 64 / ( ZOOM + 1 )
         LBND( 1 ) = ( NX - ROISIZ ) / 2 + OX
         LBND( 2 ) = ( NY - ROISIZ ) / 2 + OY
         UBND( 1 ) = LBND( 1 ) + ROISIZ - 1 + OX
         UBND( 2 ) = LBND( 2 ) + ROISIZ - 1 + OY

*    Initialise the rectangular region of interest drawn in the highest
*    colour index.

         CALL IIRINR( DISPID, MEMIDR, MAXV, LBND( 1 ), LBND( 2 ),
     :                UBND( 1 ), UBND( 2 ), ROIID, IDSTAT )

*    Abort if an error has occurred.  Note IDI does not use
*    inherited status.

         IF ( IDSTAT .NE. IDI__OK ) THEN

*    Obtain a meaningful IDI error message.

            CALL KPG1_IDERR( 'IIRINR_ERR', IDSTAT, STATUS )
            DEVCAN = .TRUE.
            GOTO 980
         END IF

*    Set up the interactions to control the region of interest.
*    ==========================================================
*
*    Set up the mouse or trackerball (interactor type = 0, interactor
*    id = 0) to control the ROI (object type = 4, object id = ROIID)
*    by modifying it (interactive operation = 7). End the interaction by
*    pressing the right-hand button (exit trigger number = 2).

         INTTY = 0
         INTID = 0
         OBJTY = 4
         OBJID = ROIID
         INTOP = 7
         EXTRN = 2

         CALL IIIENI( DISPID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :                IDSTAT )

*    Inquire the interactive operation and instruct the user

         CALL IIIQID( DISPID, INTTY, INTID, BUFFER, NCHAR, IDSTAT )
         BUFFER( NCHAR + 1: ) = ' to control the region of interest.'
         CALL MSG_OUT( 'ROICONTROL', BUFFER, STATUS )

*    Set up the first trigger (interactor type = 5, interactor id = 0)
*    to toggle the ROI active corner (object type = 4, object id =
*    ROIID) while modifying it (interactive operation = 7).  End the
*    interaction by pressing the right-hand button (exit trigger
*    number 2).

         INTTY = 5
         INTID = 0
         OBJTY = 4
         OBJID = ROIID
         INTOP = 7
         EXTRN = 2

         CALL IIIENI( DISPID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :                IDSTAT )

*    Inquire the interactive operation and instruct the user.

         CALL IIIQID( DISPID, INTTY, INTID, BUFFER, NCHAR, IDSTAT )
         BUFFER( NCHAR + 1: ) = ' to toggle between the moving corners.'
         CALL MSG_OUT( 'TOGGLE', BUFFER, STATUS )

*    Inquire the exit trigger operation and instruct the user.

         CALL IIIQID( DISPID, INTTY, EXTRN, BUFFER, NCHAR, IDSTAT )
         BUFFER( NCHAR + 1: ) = ' to end the definition of the region '/
     :                          /'of interest.'
         CALL MSG_OUT( 'END_IDROI ', BUFFER, STATUS )

*    Display the ROI by setting its visibility to .TRUE.

         CALL IIRSRV( DISPID, ROIID, .TRUE., IDSTAT )

*    Intercept any error.

         IF ( IDSTAT .NE. IDI__OK ) THEN

*    Obtain a meaningful IDI error message.

            CALL KPG1_IDERR( 'IIRSRV_ERR', IDSTAT, STATUS )
            GOTO 980
         END IF

*    Execute the ROI interactions.

         CALL IIIEIW( DISPID, TRIGS, IDSTAT )

*    Intercept any error.

         IF ( IDSTAT .NE. IDI__OK ) THEN

*    Obtain a meaningful IDI error message.

            CALL KPG1_IDERR( 'IIIEIW_ERR', IDSTAT, STATUS )
            GOTO 980
         END IF

*    Read the position of the ROI now the interaction has completed.

         CALL IIRRRI( DISPID, MEMIDR, ROIID, LBND( 1 ), LBND( 2 ),
     :                UBND( 1 ), UBND( 2 ), OUTMID, IDSTAT )

*    Intercept any error.

         IF ( IDSTAT .NE. IDI__OK ) THEN

*    Obtain a meaningful IDI error message.

            CALL KPG1_IDERR( 'IIRRRI_ERR', IDSTAT, STATUS )

*    Make the ROI invisible.
            CALL IIRSRV( DISPID, ROIID, .FALSE., IDSTAT )
            GOTO 980
         END IF

*    Derive the new offsets and dimensions of the ROI.

         OX = OX + LBND( 1 ) - 1
         OY = OY + LBND( 2 ) - 1
         NX1 = UBND( 1 ) - LBND( 1 ) + 1
         NY1 = UBND( 2 ) - LBND( 2 ) + 1

      END IF

*   Now set up the transfer window.
*   ===============================

      CALL IIMSTW( DISPID, 0, 0, NX1, NY1, DEPTH( 1 ), OX, OY, IDSTAT )
      NDATA = NX1 * NY1
      PACK = 1
      IDIMS( 1 ) = NX1
      IDIMS( 2 ) = NY1

*   Abort if an error has occurred.

      IF ( IDSTAT .NE. IDI__OK ) THEN

*    Obtain a meaningful IDI error message.

         CALL KPG1_IDERR( 'IIMSTW_ERR', IDSTAT, STATUS )
         GOTO 980
      END IF

*    Make the ROI invisible.
*    =======================

      IF ( .NOT. WHOLE ) THEN
         CALL IIRSRV( DISPID, ROIID, .FALSE., IDSTAT )

*    Intercept any error.

         IF ( IDSTAT .NE. IDI__OK ) THEN

*    Obtain a meaningful IDI error message.

            CALL KPG1_IDERR( 'IIRSRV_ERR', IDSTAT, STATUS )
            GOTO 980
         END IF
      END IF

*   Create the snapshot.
*   ====================

*   Create a scratch area in which to put the snapshot array.

      SNPLOC = ' '
      CALL AIF_GETVM( '_INTEGER', 2, IDIMS, SNPPNT, SNPLOC, STATUS )

*   If an error occurred, report context information and then abort.

      IF ( STATUS .NE. SAI__OK ) THEN

*   Obtain and output a meaningful error message.

         CALL ERR_REP( 'SNAPSHOT_WSP',
     :     'SNAPSHOT: Error creating scratch space for the snapshot.',
     :     STATUS )
         GOTO 970
      END IF

*   Keep the user informed that something is happening at is may be a
*   lengthy process.

      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( 'CREATESNAP', 'Exposing the snapshot.', STATUS )

*   Create the snapshot.

      CALL IIDSNP( DISPID, 0, NDATA, 0, 0, DEPTH( 1 ), PACK,
     :             %VAL( SNPPNT ), IDSTAT )

*   Abort if an error has occurred.

      IF ( IDSTAT .NE. IDI__OK ) THEN

*    Obtain a meaningful IDI error message.

         CALL KPG1_IDERR( 'IIDSNP_ERR', IDSTAT, STATUS )
         GOTO 970
      END IF

*   Open the output device and create a zone for the snapshot.
*   ==========================================================

*   Associate graphics device.

      DEVCN1 = .FALSE.
      CALL AGS_ASSOC( 'ODEVICE', 'WRITE', ' ', PICID, ZONE1, STATUS )

*    Check whether chosen device is a hardcopy device with imaging
*    and with a suitable minimum number of colour indices.

      CALL KPG1_QVID( 'ODEVICE', 'SGS', 'MATRIX_PRINTER', ' ', MINCOL,
     :                STATUS )

*    Obtain the number of colour indices and the maximum display
*    surface.

      CALL KPG1_QIDAT( 'ODEVICE', 'SGS', NINTS, IPIXX, IPIXY, STATUS )

*    Abort if the device is not suitable or something has gone wrong
*    associating the device.

      IF ( STATUS .NE. SAI__OK ) THEN

*       The device name is to be cancelled.

         DEVCN1 = .TRUE.
         GOTO 960
      END IF

*   Obtain GKS workstation identifier.

      CALL SGS_ICURW( WKID )

*   Inquire zone size.

      CALL SGS_IZONE( X1, Y1, X2, Y2, XM, YM )

*   Find the SCALE factor to use.

      CALL PAR_GDR0R( 'SCALE', 0.707, 0.1, 1.0, .TRUE., SCALE, STATUS )

*   Find whether image is to be negative or not.

      CALL PAR_GTD0L( 'NEGATIVE', .TRUE., .TRUE., NEGTIV, STATUS )

*   Start a new error context.

      CALL ERR_MARK

*   Obtain title.

      PLTITL = .TRUE.
      CALL PAR_GET0C( 'TITLE', TITLE, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN

*   A null value indicates no title is required.  The previous two
*   parameters will not return a null status.

         CALL ERR_ANNUL( STATUS )
         PLTITL = .FALSE.
         TITLE = 'KAPPA - Snapshot'
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_RLSE
         GOTO 960
      END IF

*   Release the new error context.

      CALL ERR_RLSE

*   Create a zone of the specified size.  The 0.9 factor is
*   introduced to allow room for the title.

      IF ( PLTITL ) SCALE = SCALE * 0.9
      CALL SGS_ZSIZE( XM*SCALE, YM*SCALE, 'CC', ZONE2, STATUS )

*   Define aspect ratio of array and display.

      ASP = REAL( IDIMS( 1 ) ) / REAL( IDIMS( 2 ) )
      CALL SGS_ZSHAP( ASP, 'CC', ZONEI, STATUS )

*   Obtain new zone of the appropriate shape.

      CALL SGS_SELZ( ZONEI, STATUS )
      CALL SGS_SW( 0.0, REAL( IDIMS( 1 ) ) - 1.0, 0.0,
     :             REAL( IDIMS( 2 ) ) - 1.0, STATUS )

*   If an error occurred, report context information and then abort.

      IF ( STATUS .NE. SAI__OK ) THEN

*   Obtain and output a meaningful error message.

         CALL ERR_REP( 'SNAPSHOT_SGS',
     :   ' SGS error during zone manipulation.', STATUS )
         GOTO 960
      END IF

*   Create the greyscale.
*   =====================

*   Set up a greyscale lookup table in the specified device, leaving
*   the reserved pens.

      LP = CTM__RSVPN
      DO  I = LP, NINTS-1, 1
         DO  J = 1, NPRICL, 1
            GKSCOL( J ) = REAL( I - LP ) / REAL( NINTS - LP )
         END DO

         CALL GSCR( WKID, I, GKSCOL( 1 ), GKSCOL( 2 ), GKSCOL( 3 ) )
      END DO

*   Define default limiting positions of cell array.

      XL = 0.0
      YL = 0.0
      XU = REAL( IDIMS( 1 ) ) - 1.0
      YU = REAL( IDIMS( 2 ) ) - 1.0

*   Scale the captured data.
*   ========================

*   Create a scratch area in which to put the scaled array.

      SCRLOC = ' '
      CALL AIF_GETVM( '_INTEGER', 2, IDIMS, SCRPNT, SCRLOC, STATUS )

*   If an error occurred, report context information and then abort.

      IF ( STATUS .NE. SAI__OK ) THEN

*   Obtain and output a meaningful error message.

         CALL ERR_REP( 'SNAPSHOT_WSP2',
     :     'SNAPSHOT: Error obtaining scratch space for output data.',
     :     STATUS )
         GOTO 950
      END IF

*   Scale the screen SNAPSHOT array to a range suitable for the output
*   device.

      INVERT = .TRUE.
      CALL KPG1_ISCLI( .FALSE., IDIMS( 1 ), IDIMS( 2 ), %VAL( SNPPNT ),
     :                 INVERT, MINV, MAXV, LP, NINTS-1, 0,
     :                 %VAL( SCRPNT ), STATUS )

*   Display the array.
*   ==================

*   Keep the user informed that something is happening.

      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( 'DEVPRINT', 'Developing the print.', STATUS )

      CALL SGS_BOX( XL, XU, YL, YU )
      CALL KPG1_GCA( 0.0, 0.0, REAL( IDIMS( 1 ) - 1 ),
     :               REAL( IDIMS( 2 ) - 1 ), IDIMS( 1 ), IDIMS( 2 ),
     :               IDIMS( 1 ), IDIMS( 2 ), %VAL( SCRPNT ), STATUS )

*   Write the title.
*   ================

*   Set font attributes: height and bold.

      CALL SGS_SPEN( 1 )

      IF ( PLTITL ) THEN

*   Adjust font for title.

         NTITLE = CHR_LEN( TITLE )
         TXTHGT = 0.04

         IF ( NTITLE .NE. 0 ) THEN

            CALL SGS_SELZ( ZONE1, STATUS )
            CALL SGS_SW( 0.0, 1.0, 0.0, 1.0, STATUS )
            CALL SGS_SHTX( SCALE * TXTHGT )
            CALL SGS_SFONT( 1 )
            CALL SGS_BTEXT( 0.5 - NTITLE * SCALE * TXTHGT * 0.5,
     :                      0.5 + 0.475 * SCALE + TXTHGT *0.75 )
            CALL SGS_ATEXT( TITLE )
            CALL SGS_OTEXT
         END IF
      END IF

*   Create the output NDF.
*   ======================

*   Begin an NDF context.

      CALL NDF_BEGIN
      EL = IDIMS( 1 ) * IDIMS( 2 )

*   Begin a new error context.

      CALL ERR_MARK

*   Create the output NDF structure.

      CALL LPG_CREP( 'OUT', TYPE, NDIM, IDIMS, NDF, STATUS )

      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
      ELSE

*   Map the NDF's data component for WRITE access.

         CALL KPG1_MAP( NDF, 'DATA', '_INTEGER', 'WRITE', PNTR( 1 ), EL,
     :                STATUS )

*   Move the contents from the snapshot call to the NDF data component.
*   There are no bad values.

         BAD = .FALSE.
         CALL VEC_ITOI( BAD, EL, %VAL( SNPPNT ), %VAL( PNTR( 1 ) ),
     :                  IERR, NERR, STATUS )

*   Unmap the data component.

         CALL NDF_UNMAP( NDF, 'DATA', STATUS )

*   Store the title in the NDF.

         CALL NDF_CPUT( TITLE, NDF, 'TITLE', STATUS )
      END IF

*   End the error context.

      CALL ERR_RLSE

*   End the NDF context.

      CALL NDF_END( STATUS )

*   If an error occurred, report context information and then abort.

      IF ( STATUS .NE. SAI__OK ) THEN

         IF ( STATUS .NE. PAR__ABORT .AND. STATUS .NE. PAR__NULL ) THEN

*   Make an error report.

           CALL ERR_REP( 'SNAPSHOT_NDF',
     :      'Error creating output NDF.', STATUS )
         END IF
      END IF

  950 CONTINUE

*   Unmap and annul workspace for scratch array.

      CALL AIF_ANTMP( SCRLOC, STATUS )

*    AGI closedown sequence.
*    =======================

  960 CONTINUE

      CALL AGS_DEASS( 'ODEVICE', DEVCN1, STATUS )

  970 CONTINUE

*   Unmap and annul workspace for snapshot array.

      CALL AIF_ANTMP( SNPLOC, STATUS )

  980 CONTINUE

*   Close down the IDI device, cancelling its associated parameter
*   where there was a problem with the device.

      IF ( DEVCAN ) THEN
         CALL IDI_CANCL( 'DEVICE', STATUS )
      ELSE
         CALL IDI_ANNUL( DISPID, STATUS )
      END IF

  999 CONTINUE

      END
