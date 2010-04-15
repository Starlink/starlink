      SUBROUTINE IDPAZO( STATUS )
*+
*  Name:
*     IDPAZO

*  Purpose:
*     Pans and zooms an image-display device.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL IDPAZO( STATUS )

*  Description:
*     This routine pans all planes of an IDI-supported image display,
*     such as X-windows.  The zoom factor is controlled by the mouse
*     or trackerball buttons.
*
*     For an X-windows device, pressing the left button of the mouse
*     increases the zooming, the centre button reduces the zoom factor,
*     and the right-hand button ends the pan and zoom operation.

*  ADAM Parameters:
*     DEVICE = DEVICE (Read)
*        The name of the image-display device to be panned and zoomed.
*        The name of the base plane should be given.
*        [Current image display]

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Examples:
*     idpazo xwindows
*        Pans and zooms the xwindows device
*     idpazo
*        Pans and zooms the current image-display device.

*  Algorithm:
*     -  Ensure the the clear flag is set to "no clear".
*     -  Associate the image display and get an identifier for it.
*     -  Check the attributes of the device, i.e. that it has at least
*        three triggers and zooming is available.
*     -  Set up the interactions and tell the user what to do.
*     -  Find the size of the memory and make the cursor visible at the
*        display centre.
*     -  Perform the zoom and pan.
*     -  Make the cursor invisible.
*     -  Annul the device.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1990 Apr 11 (MJC):
*        Original version.
*     1991 October 28 (MJC):
*        Made exit trigger number 2.  Inceased maximum number of
*        triggers to 64.  Added a memory-visibility call.
*     1992 February 19 (MJC):
*        Removed the memory-visibility call.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IDI_PAR'          ! IDI constants
      INCLUDE 'IDI_ERR'          ! IDI error codes

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:

      INTEGER
     :  ACTVAL,                  ! Number of values returned
     :  DID,                     ! Display identifier
     :  EXTRN,                   ! Exit trigger for ending pan/zoom
     :  IDSTAT,                  ! IDI status
     :  INTID,                   ! Interactor identifier
     :  INTOP,                   ! Interactive operation number
     :  INTTY,                   ! Interactor type for zooming control
     :  MEMID,                   ! Base memory identifier
     :  NCHAR,                   ! Number of characters in IDI error
                                 ! message text
     :  NTRIG( 1 ),              ! Number of available triggers
     :  NVAL,                    ! Number of values expected
     :  OBJID,                   ! Object identifier
     :  OBJTY,                   ! Object type
     :  TRICAP,                  ! No. of triggers capability code
     :  TRIGS( IMAXTR ),         ! Trigger status
     :  ZOMCAP,                  ! Zooming capability code
     :  ZOMRNG( 2 )              ! Maximum and minimum zoom factors

      CHARACTER*72
     :  BUFFER                   ! Informational messages from IDI

      LOGICAL                    ! True if:
     :  DEVCAN                   ! Device is to be cancelled on exit

*.

*    Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Obtain the image-display device.
*    ================================

*    Assume that the application will work and the chosen device is
*    to be annulled rather than cancelled upon completion.

      DEVCAN = .FALSE.

*    Just want to pan and zoom what is already there.  It may have
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

*    Check that the image-display device supports zooming.
*    =====================================================

      ZOMCAP = 17
      NVAL = 2
      CALL IIDQCI( DID, ZOMCAP, NVAL, ZOMRNG, ACTVAL, IDSTAT )

*    Abort if an error has occurred.  Note IDI does not use
*    inherited status; it resets the status in each subroutine,
*    therefore it is necessary to check the status explicitly.

      IF ( IDSTAT .NE. IDI__OK ) THEN

*      Obtain a meaningful IDI error message.

         CALL KPG1_IDERR( 'IIDQCI_ERR', IDSTAT, STATUS )
         GOTO 999
      END IF

*    Check that zooming is possible by looking at the maximum zoom
*    factor.
*    =============================================================

      IF ( ZOMRNG( 2 ) .LT. 2 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'IDPAZO_NOAVA',
     :     'IDPAZO: Zooming not available on the chosen device.',
     :     STATUS )

*       The image display should not be recorded.

         DEVCAN = .TRUE.
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

*      Obtain a meaningful IDI error message.

         CALL KPG1_IDERR( 'IIDQCI_ERR', IDSTAT, STATUS )
         DEVCAN = .TRUE.
         GOTO 999
      END IF

*    Check that the image-display device has at least three triggers.
*    ================================================================

*    These are numbered 0, 1, 2 etc.

      IF ( NTRIG( 1 ) .LT. 3 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'IDPAZO_NOCURSOR',
     :     'IDPAZO: The chosen device has insufficient triggers.',
     :     STATUS )

*       The image display should not be recorded.

         DEVCAN = .TRUE.
         GOTO 999
      END IF

*    Use the base plane.

      MEMID = 0

*    Set up the interactions to move the cursor and zoom the memory.
*    ===============================================================
*
*    Set up the mouse or trackerball (interactor type = 0, interactor
*    id = 0) to control the memory (object type = 5, object id = MEMID)
*    by moving it (interactive operation = 1). End the interaction by
*    pressing the last trigger (exit trigger number = number of triggers
*    less 1).

      INTTY = 0
      INTID = 0
      OBJTY = 5
      OBJID = MEMID
      INTOP = 1
      EXTRN = 2

      CALL IIIENI( DID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :             IDSTAT )

*    Inquire the interactive operation and instruct the user

      CALL IIIQID( DID, INTTY, INTID, BUFFER, NCHAR, IDSTAT )
      BUFFER( NCHAR + 1: ) = ' to control the pan position.'
      CALL MSG_OUT( 'PANPOS ', BUFFER, STATUS )

*    Set up the first trigger (interactor type = 5, interactor id = 0)
*    to control the memory (object type = 5, object id = MEMID) by
*    increasing the zoom (interactive operation = 3).  End the
*    interaction by pressing the last trigger (exit trigger number =
*    number of triggers less 1).

      INTTY = 5
      INTID = 0
      OBJTY = 5
      OBJID = MEMID
      INTOP = 3
      EXTRN = 2

      CALL IIIENI( DID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :             IDSTAT )

*    Inquire the interactive operation and instruct the user.

      CALL IIIQID( DID, INTTY, INTID, BUFFER, NCHAR, IDSTAT )
      BUFFER( NCHAR + 1: ) = ' to increase the zoom.'
      CALL MSG_OUT( 'INC_ZOOM ', BUFFER, STATUS )

*    Set up the second trigger (interactor type = 5, interactor id = 1)
*    to control the memory (object type = 5, object id = MEMID) by
*    decreasing the zoom (interactive operation = 4).  End the
*    interaction by pressing the last trigger (exit trigger number =
*    number of triggers less 1).

      INTTY = 5
      INTID = 1
      OBJTY = 5
      OBJID = MEMID
      INTOP = 4
      EXTRN = 2

      CALL IIIENI( DID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :             IDSTAT )

*    Inquire the interactive operation and instruct the user.

      CALL IIIQID( DID, INTTY, INTID, BUFFER, NCHAR, IDSTAT )
      BUFFER( NCHAR + 1: ) = ' to decrease the zoom.'
      CALL MSG_OUT( 'DEC_ZOOM', BUFFER, STATUS )

*    Inquire the exit trigger operation and instruct the user.

      CALL IIIQID( DID, INTTY, EXTRN, BUFFER, NCHAR, IDSTAT )
      BUFFER( NCHAR + 1: ) = ' to end the pan and zoom.'
      CALL MSG_OUT( 'END_IDPAZO ', BUFFER, STATUS )

*    Execute the pan and zoom interactions.

      CALL IIIEIW( DID, TRIGS, IDSTAT )

*    Intercept any error.

      IF ( IDSTAT .NE. IDI__OK ) THEN

*      Obtain a meaningful IDI error message.

         CALL KPG1_IDERR( 'IIMEIW_ERR', IDSTAT, STATUS )
      END IF

*    Errors, either obtaining the device or because the device does not
*    support the pan-and-zoom operation, fall to this point.

 999  CONTINUE

      IF ( DEVCAN ) THEN

*       Close down IDI using the parameter system.

         CALL IDI_CANCL( 'DEVICE', STATUS )
      ELSE
         CALL IDI_ANNUL( DID, STATUS )
      END IF

      END
