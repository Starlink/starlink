      SUBROUTINE OVCLEAR( STATUS )
*+
*  Name:
*     OVCLEAR

*  Purpose:
*     Clears an image-display overlay.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL OVCLEAR( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Usage:
*     ovclear [device] [current]

*  Description:
*     This application clears an overlay device, but without purging
*     the graphics-database entries for the device.  Optionally, only
*     the current picture is cleared.

*  ADAM Parameters:
*     CURRENT = _LOGICAL (Read)
*        If TRUE, then only the current picture is cleared. [FALSE]
*     DEVICE = DEVICE (Read)
*        The graphics device to be cleared.  It must be in GNS classes
*        IMAGE_OVERLAY or WINDOW_OVERLAY. [Current image-display-overlay
*        device]

*  Examples:
*     ovclear
*        Clears the current image-overlay device.
*     ovclear current
*        Clears the current picture on the current image-overlay device.
*     ovclear xoverlay
*        Clears the xoverlay device.

*  Algorithm:
*     -  Find out whether the device is to be reset.  Open the SGS
*     workstation for required graphics device via AGI to get GNS
*     names, using update access when clearing the current picture, and
*     write access when resetting the device.
*     -  If the current picture alone is to be cleared then get the
*     zone associated with the current picture, and clear the zone.
*     Otherwise, get the base picture and base zone.  Clear the base
*     zone.  Delete all the pictures associated with the device in the
*     database.
*     -  Close SGS and the AGI device.

*  Related Applications:
*     KAPPA: GDCLEAR, IDCLEAR, OVSET.

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1992 November 30 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! no implicit typing allowed

*  Global Constants:
      INCLUDE  'SAE_PAR'       ! global SSE definitions
      INCLUDE  'PAR_ERR'       ! parameter-system errors

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER
     :    PICID,               ! AGI input picture identifier
     :    PICIDB,              ! AGI base picture identifier
     :    ZONE                 ! Workstation zone identifier

      LOGICAL                  ! True if:
     :    CURRNT               ! Only the current picture is to be
                               ! cleared

      REAL
     :    X1, X2,              ! X bounds of the current zone
     :    Y1, Y2,              ! Y bounds of the current zone
     :    XM, YM               ! Size the current zone

*.

*  Check the inherited status on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find whether the device is to be reset or just the current picture
*  is to be cleared.
      CALL PAR_GTD0L( 'CURRENT', .FALSE., .TRUE., CURRNT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Start an AGI scope.
      CALL AGI_BEGIN

*  If only the current picture is to be cleared, then the graphics
*  device is accessed in update mode.
      IF ( CURRNT ) THEN
         CALL AGI_ASSOC( 'DEVICE', 'UPDATE', PICID, STATUS )
      ELSE

*  Open the AGI workstation to reset the device.
         CALL AGI_ASSOC( 'DEVICE', 'WRITE', PICID, STATUS )
      END IF

*  Initialise SGS.
      CALL AGS_ACTIV( STATUS )

*  If the graphics device was not available, report the error and leave
*  the programme.
      IF ( STATUS .NE. SAI__OK ) THEN

         IF ( STATUS .NE. PAR__ABORT ) THEN
            CALL ERR_REP( 'OVCLEAR_NID',
     :        'OVCLEAR: Graphics device not available or not '/
     :        /'recognised.', STATUS )
         END IF
         GOTO 980
      END IF

      IF ( CURRNT ) THEN

*  Only the current picture is to be cleared.  First get the SGS zone
*  associated with the current picture.
         CALL AGS_NZONE( ZONE, STATUS )

         IF ( STATUS .NE. SAI__OK ) GOTO 980

*  Now clear the zone.  Note this will not work on all devices.  For
*  some the whole display surface may be cleared.  See the SGS
*  documentation for further details.
         CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )
         CALL SGS_BOX( X1, X2, Y1, Y2 )
         CALL SGS_CLRBL( X1, X2, Y1, Y2 )

      ELSE

*  Get the base picture.
         CALL AGI_IBASE( PICIDB, STATUS )
         CALL AGI_SELP( PICIDB, STATUS )

*  Get the SGS zone associated with the base picture to effect the
*  clear.
         CALL AGS_NZONE( ZONE, STATUS )

      END IF

 980  CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( CURRNT ) THEN
            CALL MSG_SETC( 'PIC', 'current picture' )
         ELSE
            CALL MSG_SETC( 'PIC', 'display surface' )
         END IF
         CALL ERR_REP( 'OVCLEAR_NOTCLEAR',
     :      'OVCLEAR: Unable to get to clear the ^PIC.', STATUS )
      END IF

*  Close the AGI workstation.
      CALL AGS_DEACT( STATUS )
      CALL AGI_ANNUL( PICID, STATUS )

*  End the AGI scope.
      CALL AGI_END( -1, STATUS )

 999  CONTINUE

      END
