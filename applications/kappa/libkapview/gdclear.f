      SUBROUTINE GDCLEAR( STATUS )
*+
*  Name:
*     GDCLEAR

*  Purpose:
*     Clears a graphics device and purges its database entries.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL GDCLEAR( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Usage:
*     gdclear [device] [current]

*  Description:
*     This application software resets an SGS graphics device. In effect
*     the device is cleared.  It purges the graphics-database entries
*     for the device.  Optionally, only the current picture is cleared
*     and the database unchanged. (Note the clearing of the current
*     picture may not work on some graphics devices.)

*  ADAM Parameters:
*     CURRENT = _LOGICAL (Read)
*        If TRUE then only the current picture is cleared. [FALSE]
*     DEVICE = DEVICE (Read)
*        The graphics device to be cleared. [Current graphics device]

*  Examples:
*     gdclear
*        Clears the current graphics device and purges its graphics
*        database entries.
*     gdclear current
*        Clears the current picture on the current graphics device.
*     gdclear xw
*        Clears the xw device and purges its graphics database entries.

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
*     KAPPA: GDSET, GDSTATE, IDCLEAR, OVCLEAR.

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1989 Apr 13 (MJC):
*        Original version.
*     1989 Jul 24 (MJC):
*        Uses SGS to get device names consistent with the rest of KAPPA
*        in the partial GNS era.
*     1990 Jan 12 (MJC):
*        Option to clear the current picture added.
*     1991 March 24 (MJC):
*        Converted to SST prologue.
*     1991 April 9 (MJC):
*        Added AGI begin-and-end block.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     {enter_further_changes_here}

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
            CALL ERR_REP( 'GDCLEAR_NID',
     :        'GDCLEAR: Graphics device not available or not '/
     :        /'recognised.', STATUS )
         END IF
         GOTO 980
      END IF

      IF ( CURRNT ) THEN

*       Only the current picture is to be cleared.
*       First get the SGS zone associated with the current picture.

         CALL AGS_NZONE( ZONE, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'GDCLEAR_CZONE',
     :        'GDCLEAR: Unable to get the current zone.', STATUS )
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
            CALL ERR_REP( 'GDCLEAR_NBASE',
     :        'GDCLEAR: Unable to get the base picture ', STATUS )
            GOTO 980
         END IF

*       Get the SGS zone associated with the base picture to effect the
*       clear.

         CALL AGS_NZONE( ZONE, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'GDCLEAR_NCLER',
     :        'GDCLEAR: Unable to get the base zone to clear the '/
     :        /'display surface.', STATUS )
            GOTO 980
         END IF

*       Remove pictures associated with the device from the database.

         CALL AGI_PDEL( STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'GDCLEAR_DLAGI',
     :        'GDCLEAR: Unable to delete pictures from the database',
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
