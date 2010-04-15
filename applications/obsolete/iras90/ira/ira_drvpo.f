      SUBROUTINE IRA_DRVPO( X1, Y1, X2, Y2, H, STATUS )
*+
*  Name:
*     IRA_DRVPO

*  Purpose:
*     Return information about a plotted coordinate value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_DRVPO( X1, Y1, X2, Y2, H, STATUS )

*  Description:
*     This routine returns items of information describing the area in
*     which the last value plotted by IRA_DRVAL appeared.

*  Arguments:
*     X1 = REAL (Returned)
*        The X coordinate at the centre of the left hand edge of the
*        box in which the text was written.
*     Y1 = REAL (Returned)
*        The Y coordinate at the centre of the left hand edge of the
*        box in which the text was written.
*     X2 = REAL (Returned)
*        The X coordinate at the centre of the right hand edge of the
*        box in which the text was written.
*     Y2 = REAL (Returned)
*        The Y coordinate at the centre of the right hand edge of the
*        box in which the text was written.
*     H = REAL (Returned)
*        The height of the box in which the text was written,
*        perpendicular to the line joining (X1,Y1) and (X2,Y2).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-NOV-1992 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_PAR'          ! IRA_ constants.

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common blocks.
*        ACM_DRVPO( 5 ) = REAL (Read)
*           Values defining the area occupied by the text, in the
*           order; (X,Y) at start of box, (X,Y) at end of box, height
*           of box (perpendicular to the line joing start and end of
*           box).

*  Arguments Returned:
      REAL X1
      REAL Y1
      REAL X2
      REAL Y2
      REAL H

*  Status:
      INTEGER STATUS             ! Global status

*  External Routines:
      EXTERNAL IRA1_INIT         ! Initialise IRA common blocks.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that a value has been written. If not, report an error.
      IF( ACM_DRVPO( 1 ) .EQ. VAL__BADR ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'IRA_DRVPO_ERR1',
     :             'IRA_DRVPO: No coordinate value has yet been drawn.',
     :                 STATUS )
         GO TO 999
      END IF

*  Copy the values stored in common to the supplied arguments.
      X1 = ACM_DRVPO( 1 )
      Y1 = ACM_DRVPO( 2 )
      X2 = ACM_DRVPO( 3 )
      Y2 = ACM_DRVPO( 4 )
      H = ACM_DRVPO( 5 )

*  If an error occurred, give a context message.
 999  CONTINUE

      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRA_DRVPO_ERR2',
     :        'IRA_DRVPO: Error returning area covered by a plotting '//
     :        'coordinate value', STATUS )

      END IF

      END
