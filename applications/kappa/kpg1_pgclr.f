      SUBROUTINE KPG1_PGCLR( STATUS )
*+
*  Name:
*     KPG1_PGCLR

*  Purpose:
*     Clear current PGPLOT viewport.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PGCLR( STATUS )

*  Description:
*     This routine clears the current PGPLOT viewport if possible. If it
*     is not possible (eg for a printer) it does nothing.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     - A PGPLOT image-display workstation must be open and active.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-SEP-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER CI                 ! Original colour index
      INTEGER CI2                ! Highest available colour index
      INTEGER CI1                ! Lowest available colour index
      INTEGER FS                 ! Original fill style
      REAL X1, X2, Y1, Y2        ! Bounds of PGPLOT viewport
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if we can write in the background colour on the current device.
      CALL PGQCOL( CI1, CI2 ) 

*  Do nothing if we can't.
      IF( CI1 .EQ. 0 ) THEN

*  Save the current fill area attributes.
         CALL PGQFS( FS )
         CALL PGQCI( CI )

*  Set "solid fill in background colour (pen 0)".
         CALL PGSFS( 1 )
         CALL PGSCI( 0 )

*  Get the extent in world co-ordinates of the current viewport.
         CALL PGQWIN( X1, X2, Y1, Y2 )

*  Draw a filled rectangle covering the current viewport.
         CALL PGRECT( X1, X2, Y1, Y2 )

*  Re-instate the original fill area attributes.
         CALL PGSFS( FS )
         CALL PGSCI( CI )

      END IF

      END
