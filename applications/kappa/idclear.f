      SUBROUTINE IDCLEAR( STATUS )
*+
*  Name:
*     IDCLEAR

*  Purpose:
*     Clears an image display and purges its database entries.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL IDCLEAR( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Usage:
*     idclear [device] [current]

*  Description:
*     This application software resets an image-display device. In effect
*     the device is cleared.  It purges the graphics-database entries
*     for the device.  Optionally, only the current picture is cleared
*     and the database unchanged.  (Note that the clearing of the current
*     picture may not work on some image-display devices.)

*  ADAM Parameters:
*     CURRENT = _LOGICAL (Read)
*        If true then only the current picture is cleared. [FALSE]
*     DEVICE = DEVICE (Read)
*        The graphics device to be cleared. [Current image-display
*        device]

*  Examples:
*     idclear
*        Clears the current image display and purges its graphics
*        database entries.
*     idclear current
*        Clears the current picture on the current image display.
*     idclear xwindows
*        Clears the xwindows device and purges its graphics-database
*        entries.

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
*     KAPPA: GDCLEAR, IDINVISIBLE, IDSTATE, OVCLEAR.

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 Apr 30 (MJC):
*        Original version.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! No implicit typing allowed

*  Global Constants:
      INCLUDE  'SAE_PAR'       ! Global SSE definitions
      INCLUDE  'PAR_ERR'       ! Parameter-system errors

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

*    Check the inherited status on entry.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Find whether the device is to be reset or just the current picture
*    is to be cleared.

      CALL PAR_GTD0L( 'CURRENT', .FALSE., .TRUE., CURRNT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*    Start an AGI scope.

      CALL AGI_BEGIN

*    If only the current picture is to be cleared, then the graphics
*    device is accessed in update mode.

      IF ( CURRNT ) THEN
         CALL AGI_ASSOC( 'DEVICE', 'UPDATE', PICID, STATUS )
      ELSE

*       Open the AGI workstation to reset the device.

         CALL AGI_ASSOC( 'DEVICE', 'WRITE', PICID, STATUS )
      END IF

*    Initialise SGS.

      CALL AGS_ACTIV( STATUS )

*    If the graphics device was not available, report the error and
*    leave the programme.

      IF ( STATUS .NE. SAI__OK ) THEN

         IF ( STATUS .NE. PAR__ABORT ) THEN
            CALL ERR_REP( 'IDCLEAR_NID',
     :        'IDCLEAR: Graphics device not available or not '/
     :        /'recognised.', STATUS )
         END IF
         GOTO 980
      END IF

      IF ( CURRNT ) THEN

*       Only the current picture is to be cleared.
*       First get the SGS zone associated with the current picture.

         CALL AGS_NZONE( ZONE, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'IDCLEAR_CZONE',
     :        'IDCLEAR: Unable to get the current zone.', STATUS )
            GOTO 980
         END IF

*       Now clear the zone.  Note this will not work on all devices.
*       For some the whole display surface may be cleared. See the
*       SGS documentation for further details.

         CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )
         CALL SGS_BOX( X1, X2, Y1, Y2 )
         CALL SGS_CLRBL( X1, X2, Y1, Y2 )
*         CALL SGS_CLRZ

      ELSE

*       In order to clear the database
*       Get the base picture.

         CALL AGI_IBASE( PICIDB, STATUS )
         CALL AGI_SELP( PICIDB, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'IDCLEAR_NBASE',
     :        'IDCLEAR: Unable to get the base picture.', STATUS )
            GOTO 980
         END IF

*       Get the SGS zone associated with the base picture to effect the
*       clear.

         CALL AGS_NZONE( ZONE, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'IDCLEAR_NCLER',
     :        'IDCLEAR: Unable to get the base zone to clear the '/
     :        /'display surface.', STATUS )
            GOTO 980
         END IF

*       Remove pictures associated with the device from the database.

         CALL AGI_PDEL( STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'IDCLEAR_DLAGI',
     :        'IDCLEAR: Unable to delete pictures from the database.',
     :        STATUS )
         END IF
      END IF

 980  CONTINUE

*    Close the AGI workstation.

      CALL AGS_DEACT( STATUS )
      CALL AGI_ANNUL( PICID, STATUS )

*    End the AGI scope.

      CALL AGI_END( -1, STATUS )

 999  CONTINUE

      END
