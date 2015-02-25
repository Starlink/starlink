      SUBROUTINE LUTROT( STATUS )
*+
*  Name:
*     LUTROT

*  Purpose:
*     Rotates the colour table of an image-display device.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL LUTROT( STATUS )

*  Description:
*     This routine rotates the colour table of a nominated plane of
*     an IDI-supported image display, such as X-windows.  The rotation
*     is under mouse, joystick or trackerball button control.
*
*     For an Ikon or X-windows, moving the mouse left or right
*     rotates the colour table towards lower and higher pen numbers
*     respectively.  Pressing the left button of the mouse resets the
*     colour table to its input state, and hitting the right-hand
*     button ends the rotation.

*  Usage:
*     lutrot [device] [plane]

*  ADAM Parameters:
*     DEVICE = DEVICE (Read)
*        The name of the image-display device whose colour table is to
*        be rotated.  The name of the base plane should be given even if
*        the overlay colour table is to be rotated.
*        [Current image-display device]
*     FULL = _LOGICAL (Read)
*        If FULL is TRUE, the whole colour-table for the device is
*        rotated, including the reserved pens.  When FULL is FALSE, the
*        reserved pens in the palette are unaltered.  [FALSE]
*     PLANE = _INTEGER (Read)
*        The number of the memory plane whose colour table is to be
*        rotated.  If it is null the base (image) memory's colour table
*        is rotated.  The base memory is 0 and overlays are numbered
*        consecutively from 1.  For an Ikon the only overlay plane is 1.
*        PLANE is only permitted to have a value in the range 0 to the
*        number of planes minus one. [0]

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Examples:
*     lutrot
*        This enables rotation of the colour table on the current
*        image-display device.
*     lutrot xwindows
*        This enables rotation of the colour table on the xwindows
*        device.
*     lutrot full
*        This enables rotation of the colour table and palette on the
*        current image-display device.
*     lutrot ikon 1
*        This enables rotation of the colour table on the Ikon overlay
*        plane.

*  Notes:
*     -  Only Ikons and X-windows are supported.
*     -  The rate of motion of the colour table is a function of the
*     speed of cursor movement in addition to the cursor position.
*     For a given cursor displacement slow motion rotates the colour
*     table more slowly, and faster motion moves it more rapidly.  This
*     permits both fine control and swift rotation.

*  Algorithm:
*     -  Determine whether or not the reserved pens are to be ignored.
*     -  Ensure the the clear flag is set to "no clear".
*     -  Associate the image display and get an identifier for it.
*     -  Check the attributes of the device, i.e. that it has at least
*        one cursor, at least two triggers and a locator.
*     -  Obtain the number of memory planes and the maximum number of
*        LUT entries.
*     -  Obtain the plane number whose LUT is to be rotated.
*     -  Read the input LUT.
*     -  Set up the interactions and tell the user what to do.
*     -  Loop doing the LUT-rotation interaction. When the last trigger
*     is fired exit from the loop.  If the first trigger is fired write
*     the input LUT to the memory and restart the interaction.  Find an
*     effective displacement after applying correction factors for the
*     cursor speed and the number of colour-table entries.  Write the
*     rotated lookup table to the device's colour table.
*     -  Annul the device.

*  Related Applications:
*     KAPPA: CRELUT, LUTFLIP, LUTHILITE, LUTTWEAK.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1990 Apr 15 (MJC):
*        Original version.
*     1991 April 10 (MJC):
*        Modified for colour-table management, specifically the reserved
*        pens are not rotated.
*     1991 June 5 (MJC):
*        Altered for new, X-windows, IDI.  Capability 14 is 2**n rather
*        than n.
*     1991 November 15 (MJC):
*        Made exit trigger number 2.  Inceased maximum number of
*        triggers to 64.
*     1992 February 10 (MJC):
*        Normalised the rotation displacement by the effective number
*        of colour indices.
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
      INTEGER MXSPED             ! Number of speed correction factors
      PARAMETER ( MXSPED = 5 )

      INTEGER NMEMAX             ! Maximum number of memories
      PARAMETER ( NMEMAX = 10 )

*  Local Variables:

      INTEGER
     :  ACTVAL,                  ! Number of values returned
     :  DID,                     ! Display identifier
     :  EXTRN,                   ! Exit trigger for ending LUT rotation
     :  IDSTAT,                  ! IDI status
     :  INTID,                   ! Interactor identifier
     :  INTOP,                   ! Interactive operation number
     :  INTTY,                   ! Interactor type for zooming control
     :  ITTDEP( NMEMAX ),        ! ITT depths of available memories
     :  I, J,                    ! Loop counters
     :  JNEW,                    ! Colour index after rotation
     :  LUTCAP,                  ! Max. depth of LUTS capability code
     :  MEMDEP( NMEMAX ),        ! Depths of available memories
     :  MEMID,                   ! Memory identifier whose LUT is to be
                                 ! rotated
     :  MEMIDS( NMEMAX ),        ! Available memories
     :  MEMTYP,                  ! Memory type
     :  MODCON                   ! Configuration mode

      INTEGER
     :  NCHAR,                   ! Number of characters in IDI error
                                 ! message text
     :  NCONF,                   ! Configuration number
     :  NINTS( 1 ),              ! Number of LUT entries
     :  NLOC( 1 ),               ! Number of locators
     :  NLUTE,                   ! Number of entries in the VLUT less
                                 ! the reserved pens.
     :  NMEM,                    ! Number of image memories
     :  NRESP,                   ! Number of reserved pens
     :  NTRIG( 1 ),              ! Number of available triggers
     :  NVAL,                    ! Number of values expected
     :  OBJID,                   ! Object identifier
     :  OBJTY,                   ! Object type
     :  TRICAP,                  ! No. of triggers capability code
     :  TRIGS( IMAXTR ),         ! Trigger status
     :  XCORR,                   ! X speed-factor index
     :  XDISP,                   ! Locator x displacement
     :  XSIZ( NMEMAX ),          ! X sizes of available memories
     :  YDISP,                   ! Locator y displacement
     :  YSIZ( NMEMAX )           ! Y sizes of available memories

      REAL
     :  PENDIS,                  ! Pen displacement
     :  ROTSCL,                  ! Scale factor to normalise the
                                 ! rotation displacements
     :  TLUT( 3, CTM__MXPEN ),   ! Rotated video lookup table
     :  VLUT( 3, CTM__MXPEN ),   ! Video lookup table on input
     :  XSPFAC( MXSPED )         ! X speed-related displacement-
                                 ! correction factors

      CHARACTER*72
     :  BUFFER                   ! IDI informational messages

      LOGICAL                    ! True if:
     :  DEVCAN,                  ! Device is to be cancelled on exit
     :  FIRST,                   ! There has been no reading of the
                                 ! cursor position
     :  FULL,                    ! Ignore the reserved pens
     :  LOOP                     ! The interaction is to continue

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

*    Just want to rotate what is already there.  It may have
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

*    Get the memory required within the allowed range.
*    =================================================

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
         CALL ERR_REP( 'LUTROT_NOCURSOR',
     :     'LUTROT: The chosen device has insufficient triggers.',
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
         CALL ERR_REP( 'LUTROT_NOCURSOR',
     :     'LUTROT: The chosen device has no locator.', STATUS )

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

*    Set up the interactions to control the LUT rotation.
*    ====================================================
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
      BUFFER( NCHAR + 1: ) = ' to rotate the colour table.'
      CALL MSG_OUT( 'LUTROTATION', BUFFER, STATUS )

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

*    Inquire the exit trigger operation and instruct the user.

      CALL IIIQID( DID, INTTY, EXTRN, BUFFER, NCHAR, IDSTAT )
      BUFFER( NCHAR + 1: ) = ' to end the rotation of the colour table.'
      CALL MSG_OUT( 'END_LUTROT ', BUFFER, STATUS )

*    Perform the interactions.
*    =========================

*    There is no current pen displacement.

      PENDIS = 0.0

*    Derive the normalisation.  Use 256-NRESP as the canonical value.
*    This is to prevent manipulations that are too sensitive to the
*    locator's movement.  Apply a bias to prevent the normalisation
*    going too far the other way.

      ROTSCL = 0.2 + 0.8 * REAL( NLUTE ) / REAL( 256 - NRESP )

*    Must loop to repeatedly call Enable_Interaction_and_Wait whenever
*    the first trigger is activated or the locator is moved.

      LOOP = .TRUE.
      FIRST = .TRUE.

      DO WHILE ( LOOP )

*       Reset the relevant triggers.

         TRIGS( 1 ) = 0
         TRIGS( 2 ) = 0
         TRIGS( 3 ) = 0

*       Execute the LUT-rotation interactions.

         CALL IIIEIW( DID, TRIGS, IDSTAT )

*       Intercept any error.

         IF ( IDSTAT .NE. IDI__OK ) THEN

*          Obtain a meaningful IDI error message.

            CALL KPG1_IDERR( 'IIMEIW_ERR', IDSTAT, STATUS )
            GOTO 999
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
               GOTO 999
            END IF

*          Following a reset there is no current pen displacement.

            PENDIS = 0.0

*       Rotate the LUT.
*       ===============

*       The locator has been moved.  Therefore rotate the LUT.

         ELSE

            CALL IIIGLD( DID, 0, XDISP, YDISP, IDSTAT )
            IF ( FIRST ) XDISP = 0
            FIRST = .FALSE.

*          Intercept any error.

            IF ( IDSTAT .NE. IDI__OK ) THEN

*             Obtain a meaningful IDI error message.

               CALL KPG1_IDERR( 'IILGLD_ERR', IDSTAT, STATUS )
               GOTO 999
            END IF

*          Have x displacement adjustment via a lookup table.
*          ==================================================

            XCORR = MAX( 1, MIN( MXSPED, ABS( XDISP ) ) )

*          Find the total pen displacement.

            PENDIS = PENDIS + REAL( XDISP ) * XSPFAC( XCORR ) * ROTSCL

*          Rotate the LUT.
*          ===============

*          Only rotate when the locator has moved.

            IF ( XDISP .NE. 0 ) THEN
               DO  J = 1, NLUTE

*                Find the new colour index for the Jth colour.  Note the
*                total displacement is used.  Ensure that colour index
*                lies in the range 1 to NLUTE.

                  JNEW = MOD( J + NINT( PENDIS ) - 1, NLUTE ) + 1
                  IF ( JNEW .LE. 0 ) JNEW = JNEW + NLUTE

*                Create the revised LUT from the input LUT.

                  DO  I = 1, 3
                     TLUT( I, JNEW ) = VLUT( I, J )
                  END DO
               END DO

*             Write the rotated VLUT.
*             =======================

*             Only write to the unreserved portion of the VLUT.

               CALL IILWLT( DID, MEMID, NRESP, NLUTE, TLUT, STATUS )

*             Intercept any error.

               IF ( IDSTAT .NE. IDI__OK ) THEN

*                Obtain a meaningful IDI error message.

                  CALL KPG1_IDERR( 'IILWLT_ERR', IDSTAT, STATUS )
                  GOTO 999
               END IF
            END IF
         END IF
      END DO

*    Closedown sequence.
*    ===================

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
