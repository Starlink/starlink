      SUBROUTINE OVCLEAR( STATUS )
*+
*  Name:
*     OVCLEAR

*  Purpose:
*     Clears a graphics overlay device and purges its database entries.

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
*     This application software resets a graphics overlay device. In effect
*     the device is cleared.  It purges the graphics-database entries
*     for the device.  Optionally, only the current picture is cleared
*     and the database unchanged. (Note the clearing of the current
*     picture may not work on some graphics overlay devices.)

*  ADAM Parameters:
*     CURRENT = _LOGICAL (Read)
*        If TRUE then only the current picture is cleared. [FALSE]
*     DEVICE = DEVICE (Read)
*        The device to be cleared. [Current graphics overlay device]

*  Examples:
*     ovclear
*        Clears the current graphics overlay device and purges its graphics
*        database entries.
*     ovclear current
*        Clears the current picture on the current graphics overlay device.
*     ovclear xw
*        Clears the xw device and purges its graphics database entries.

*  Related Applications:
*     KAPPA: GDSET, GDSTATE, IDSET, IDCLEAR, OVSET, OVCLEAR.

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     TDCA: Tim Ash (STARLINK)
*     DSB: David S. Berry (STARLINK)
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
*     29-SEP-1999 (TDCA):
*        Converted to PGPLOT.
*     30-SEP-1999 (DSB):
*        Re-written to use KPG1_PGCLR.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! no implicit typing allowed


*  Global Constants:
      INCLUDE  'SAE_PAR'       ! global SSE definitions

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER IPIC             ! AGI input picture identifier
      INTEGER IPICB            ! AGI Base picture identifier
      LOGICAL CURRNT           ! Only the current picture is to be cleared?
*.

*  Check the inherited status on entry.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Find whether the device is to be reset or just the current picture
*  is to be cleared.
      CALL PAR_GTD0L( 'CURRENT', .FALSE., .TRUE., CURRNT, STATUS )

*  Open the graphics overlay device in update mode. This does not clear the current
*  picture. The PGPLOT viewport is set so that it corresponds to the current
*  picture.
      CALL KPG1_PGOPN( 'DEVICE', 'WRITE', IPIC, STATUS )

*  If the whole display is to be cleared. Select the Base picture as the
*  current picture, and create a PGPLOT viewport from it.
      IF( .NOT. CURRNT ) THEN
         CALL AGI_IBASE( IPICB, STATUS )
         CALL AGI_SELP( IPICB, STATUS )
         CALL AGP_NVIEW( .FALSE., STATUS )
      END IF

*  Attempt to clear the viewport. This will only do anything if
*  the device allows us to draw in the background colour.
      CALL KPG1_PGCLR( STATUS )

*  If the whole screen was cleared, empty the database of all pictures
*  (except the Base picture).
      IF( .NOT. CURRNT ) CALL AGI_PDEL( STATUS )

*  Shut down the workstation and database, retaining the original current
*  picture only if the whole screen has not been cleared.
      CALL KPG1_PGCLS( 'DEVICE', .NOT.CURRNT, STATUS )

*  If an error occurred, add a context message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'OVCLEAR_ERR', 'OVCLEAR: Unable to clear '//
     :                 'current graphics overlay device.', STATUS )
      END IF

      END
